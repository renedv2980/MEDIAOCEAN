*          DATA SET ACREPTL02  AT LEVEL 016 AS OF 08/16/00                      
*PHASE ACTL02A,+0                                                               
*INCLUDE DATVAL                                                                 
*INCLUDE CASHVAL                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'TRANSACTION FILE UPLOAD PROGRAM'                                
ACTL02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACTL**,R9,R8    BASE REGISTERS 11, 9 , 8                     
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING ACTLD,RC            RC=A(SAVE W/S)                               
         L     R7,VBIGPRNT                                                      
         USING BIGPRNTD,R7                                                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
         CLI   MODE,RUNLAST        RUN LAST                                     
         BE    RUNL                                                             
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
*                                                                               
RUNF     L     RF,=A(BOXRC)        SET UP BOX ROUTINE                           
         ST    RC,0(RF)                                                         
         L     RF,=A(BXHOOK)                                                    
         ST    RF,HEADHOOK                                                      
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     RF,ADMASTD                                                       
         USING MASTD,RF                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
         GOTO1 DATCON,DMCB,(5,0),(1,TODAYP)                                     
         GOTO1 (RF),(R1),(5,0),(2,TODAYC)                                       
         GOTO1 (RF),(R1),(5,0),(0,TODAY)                                        
         BAS   RE,MAIN                                                          
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
*                                                                               
REQF     MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,0                                                       
         MVC   S9OFFICE,=C'BZ'     S9 POSTING OFFICE                            
         CLI   RCCOMPFL,X'94'      YBMSFS                                       
         BE    REQF0                                                            
         CLI   RCCOMPFL,X'6C'      YNBM  TEST FILE                              
         BE    *+6                                                              
         DC    H'0'                RUNNING OFF UNKNOWN FILE                     
         MVC   S9OFFICE,=C'00'                                                  
*                                                                               
REQF0    CLI   QOPT2,C'W'          WORKER FILE OUTPUT                           
         BNE   REQF1                                                            
         CLI   WRKSW,C'O'          WORKER ALREADY OPENED                        
         BE    REQF2                                                            
         MVI   RCWRITE,C'N'        INSURE WRITE=NO                              
         MVI   WRKSW,C'O'                                                       
         ZAP   POSTREC,=P'0'                                                    
         ZAP   POSTCASH,=P'0'                                                   
         XC    ID,ID                                                            
         MVC   ID(2),ORIGINUM                                                   
         MVC   ID+2(3),=C'ATL'                                                  
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'P'                                                        
         OI    ID+13,X'01'         ALLOW DUPLICATE FILES                        
         BAS   RE,OPNPOST                                                       
         B     REQF2                                                            
REQF1    CLI   QOPT2,C'T'          TAPE OUTPUT                                  
         BNE   REQF2                                                            
         CLI   TAPSW,C'O'          TAPE ALREADY OPENED                          
         BE    REQF2                                                            
         MVI   RCPOSTNG,C'N'       INSURE POSTING=NO                            
         MVI   TAPSW,C'O'                                                       
         L     R3,AOUTFIL                                                       
         OPEN  ((R3),(OUTPUT))                                                  
*                                                                               
REQF2    DS    0H                                                               
         L     R2,AINFIL           SET FILE DCB                                 
         OPEN  ((R2),(INPUT))      OPEN INPUT FILE                              
*                                                                               
         LA    R1,SRTRLEN          RECORD LENGTH                                
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+21(3),DUB+6(2)                                           
         LA    R1,SRTKLEN           LENGTH OF KEY                               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD                                   
         MVC   DATADIS2,=Y(ACCRFST-ACCRECD)                                     
         LA    R5,INREC                                                         
         USING INFD,R5                                                          
*                                                                               
REQF3    GET   (R2),(R5)                                                        
         OC    INFD(200),XSPACES                                                
         OC    INFD+200(INFLNQ-200),XSPACES                                     
         CLC   INFRTYP(3),=C'TLR'       SKIP THE TRAILER                        
         BE    REQF3                                                            
*        CLC   INFRTYP(3),=C'   '       SKIP BLANK RECORDS                      
*        BE    REQF3                                                            
*                                                                               
         CLC   INFRTYP(3),=C'HDR'                                               
         BNE   REQF4                                                            
         MVC   UPTYPE,INFHTYP           SAVE UPDATE TYPE(IN61 ETC)              
         MVC   BATBREF,INFHBREF         BATCH REFERENCE                         
         L     R4,ABILDTAB                                                      
         USING BTABD,R4                                                         
REQF3B   CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   UPTYPE,BTYPE                                                     
         BE    *+12                                                             
         AH    R4,BTYPLEN                                                       
         B     REQF3B                                                           
         MVC   TTYPE,BTYPCODE           SAVE THE TRANSACTION TYPE               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'INFHMOA),INFHMOA                                          
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   MOS,WORK+6                                                       
         MVC   MOST(1),INFHMOA+1        MOS                                     
         MVC   MOST+1(1),INFHMOA+3                                              
         CLI   INFHMOA+2,C'1'                                                   
         BNE   REQF3C                                                           
         MVI   MOST+1,C'A'                                                      
         CLI   INFHMOA+3,C'0'                                                   
         BE    REQF3C                                                           
         MVI   MOST+1,C'B'                                                      
         CLI   INFHMOA+3,C'1'                                                   
         BE    REQF3C                                                           
         MVI   MOST+1,C'C'                                                      
*                                                                               
REQF3C   L     RE,ABTOTTAB             TABLE OF BATCH HEADER TOTALS             
         LA    R1,BTOTMAX                                                       
         USING BTOTD,RE                                                         
         OC    BTOTBTCH,BTOTBTCH       LOOK FOR EMPTY SLOT                      
         BZ    *+14                                                             
         LA    RE,BTOTLEN(RE)                                                   
         BCT   R1,*-14                                                          
         DC    H'0'                    TOO MANY HDR RECORDS                     
         MVI   BTOTSTAT,X'00'                                                   
         MVC   BTOTMOS,MOST            SAVE BATCH REFERENCE                     
         MVC   BTOTBREF,BATBREF                                                 
         ZAP   BTOTAMNT,ZEROS          CLEAR ACCUM                              
*                                                                               
         USING SRTRECD,R6                                                       
         LA    R6,SORTREC                                                       
         MVC   SRTKEY(SRTRLEN),XSPACES                                          
         MVC   SRTKUNT(4),=C'S91J'     ACCOUNT ALWAYS IN S9J                    
         MVC   SRTKULC(4),=C'S91J'     CONTRA ACCOUNT ALWAYS IN S9J             
         MVC   SRTKOFF,S9OFFICE        S9 OFFICE                                
         MVC   SRTKDATE,TODAY          TODAY'S DATE                             
         MVC   SRTKREF(L'UPTYPE),UPTYPE    IE IN61                              
         MVC   SRTRMOS,MOST            MOS                                      
         MVC   SRTRBREF,BATBREF        BATCH REFERENCE                          
         MVC   SRTRTYPE,TTYPE          TRANSACTIION TYPE                        
         XC    SRTRSTAT,SRTRSTAT       CREDIT                                   
         MVC   SRTRAMNT,SPACES         AMOUNT FILLED IN AT THE END              
         B     REQF5                   PUT BALANCING ENTRY TO SORT              
*                                                                               
REQF4    CLC   INFRTYP,=C'ITEM'        BATCH ITEM                               
         BE    *+6                                                              
         DC    H'0'                    UNKNOWN RECORD TYPE                      
         USING SRTRECD,R6                                                       
         LA    R6,SORTREC                                                       
         MVC   SRTKEY(SRTRLEN),XSPACES                                          
         MVC   SRTRMOS,MOST            MOS                                      
         MVC   SRTRBREF,BATBREF        BATCH REFERENCE                          
         MVC   SRTRTYPE,TTYPE          TRANSACTIION TYPE                        
         MVI   SRTRSTAT,X'00'          TRANSACTION STATUS                       
         OI    SRTRSTAT,SRTRDEB        DEBIT                                    
         MVC   SRTKUNT(2),=C'SJ'       ACCOUNT ALWAYS IN SJ                     
         SR    R0,R0                                                            
         IC    R0,BTYPNUM              NUMBER OF SUBENTRIES                     
         LA    R3,BTENTRY                                                       
         USING BTENTRY,R3                                                       
REQF4A   LA    RE,INFD                                                          
         AH    RE,BTFROM               DISP OF MOVE FIELD FROM TAPE             
         LA    RF,SORTREC                                                       
         AH    RF,BTTO                 DISP OF RECEIVE FIELD ON SORTREC         
         SR    R1,R1                                                            
         IC    R1,BTFROMLN             LENGTH OF THE MOVE                       
         SH    R1,=H'1'                                                         
         LTR   R1,R1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RF),0(RE)                                                    
         LA    R3,BTENTRLN(R3)         NEXT FIELD TO MOVE                       
         BCT   R0,REQF4A                                                        
*                                                                               
         CLC   BTYPE,=C'IN61'          TYPE 61 SPECIALS                         
         BNE   REQF4B                                                           
         LA    R1,INFI1VEN             CONTRA                                   
         CLI   INFI1VEN,C'*'                                                    
         BNE   *+8                                                              
         LA    R1,INFI1VEN+1           BUMP PAST THE *                          
         MVC   SRTKULC,0(R1)           THEN MOVE IN CONTRA ACCOUNT              
         CLC   SRTKWORK,SPACES         DID YOU PICK UP COMMISH W/C?             
         BH    REQF5                   YES- SKIP THIS NON COMM STUFF            
         CLC   INFI1WCN,SPACES                                                  
         BNH   REQF5                                                            
         MVC   SRTKWORK,INFI1WCN                                                
         OI    SRTRSTAT,SRTRNOCM       MARK AS NON COMMISSIONABLE               
         B     REQF5                                                            
*                                                                               
REQF4B   CLC   BTYPE,=C'IN49'          TYPE 49 SPECIALS                         
         BNE   REQF4C                                                           
         MVC   SRTKCUNT(2),=C'1R'      DEFAULT CONTRA U/L                       
         MVC   WORK,SPACES             BURSON 1R STRUCTURE IS 2/2/2/6           
         MVC   WORK(4),INFI2ODP                                                 
         MVC   WORK+4(2),INFI2SUB                                               
         MVC   WORK+6(6),INFI2PER                                               
         MVC   SRTKCACT,WORK                                                    
         B     REQF5                                                            
*                                                                               
REQF4C   CLC   BTYPE,=C'IN08'          TYPE 08 SPECIALS                         
         BE    *+6                                                              
         DC    H'0'                    DIE FOR NOW                              
         MVC   SRTKCUNT(2),=C'SI'      DEFAULT CONTRA U/L                       
         CLC   INFI3CON(2),=C'SI'      SOMETIMES THEY PASS U/L                  
         BNE   *+14                                                             
         MVC   SRTKCACT(10),INFI3CON+2                                          
         B     REQF5                                                            
         MVC   SRTKCACT,INFI3CON                                                
REQF5    GOTO1 ADSORTER,DMCB,=C'PUT',(R6)                                       
         B     REQF3                                                            
*                                                                               
REQF7    CLOSE ((R2))                                                           
         L     R4,ADCMPNAM                                                      
         SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+4                                                           
         MVC   CMPNAME(0),2(R4)    COMPANY NAME                                 
         EJECT                                                                  
***********************************************************************         
* PROCESS SORTED TAPE RECORDS                                         *         
***********************************************************************         
*                                                                               
PROC00   DS    0H                                                               
         LA    R0,WRKNUM           NUMBER OF BUCKET TO CLEAR                    
         LA    R1,WRKACM           1ST ONE                                      
         ZAP   0(WRKABKLN,R1),ZEROS                                             
         LA    R1,WRKABKLN(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         MVC   ACCKEYSV,SPACES                                                  
         MVC   TRNKEYSV,SPACES                                                  
         MVC   CONTRASV,SPACES                                                  
         MVC   CONNAMSV,SPACES                                                  
*                                                                               
PROC02   DS    0H                                                               
         GOTO1 ADSORTER,DMCB,=C'GET'                                            
         ICM   RE,15,DMCB+4        GET SORTED TRANSACTIONS                      
         BZ    PROC10              ALL DONE                                     
         AP    CNTIN,=P'1'                                                      
         LA    R0,SORTREC          SAVE CURRENT SORT RECORD                     
         LA    R1,SRTRLEN                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         BAS   RE,VERIFY           VERIFY ALL RECORD FIELDS                     
         BNE   PROC02              BAD DATA RECORD , GET NEXT RECORD            
         BAS   RE,TRNBILD          BUILD TRANSACTION RECORD                     
         L     R6,ATRNREC                                                       
         USING TRNRECD,R6                                                       
*                                  SAME CONTRA HEADER?                          
         L     RE,ACHDREC                                                       
         CLC   TRNKEY(L'TRNKCULA+L'TRNKWORK+L'TRNKCULC),0(RE)                   
         BE    PROC06              YES - CONTINUE                               
         BAS   RE,CACUP            NO -  PROCESS PREV HEADER/HISTORY            
         BAS   RE,CHDBILD          BUILD NEW HEADER RECORD                      
         BAS   RE,CACBILD          BUILD NEW HISTORY RECORD                     
*                                                                               
PROC06   CLC   TRNKCULA,ACCKEYSV   SAME ACCOUNT?                                
         BE    *+8                 YES - CONTINUE                               
         BAS   RE,BALUP            NO -  UPDATE PREV ACCOUNTS X'32'             
         BAS   RE,TRNUP            UPDATE TRANSACTION TO FILE/TAPE              
         B     PROC02                                                           
*                                                                               
PROC10   DS    0H                  LAST RECORD PROCESSING                       
         BAS   RE,CACUP            PROCESS LAST HEADER/HISTORY                  
         BAS   RE,BALUP            UPDATE LAST ACCOUNTS X'32'                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
*                                                                               
RUNL     CLI   TAPSW,C'O'          TEST OUTPUT TAPE OPENED                      
         BNE   RUNL2                                                            
         L     R3,AOUTFIL                                                       
         CLOSE ((R3))                                                           
         B     RUNL4                                                            
RUNL2    CLI   WRKSW,C'O'          WORKER FILE OPTION ON?                       
         BNE   RUNL4                                                            
         ZAP   DUB,PDEBITS                                                      
         SP    DUB,PCREDITS                                                     
         CP    DUB,=P'1000'                                                     
         BNH   *+6                                                              
         DC    H'0'                                                             
         CP    DUB,=P'-1000'                                                    
         BNL   *+6                                                              
         DC    H'0'                ROUNDING MORE THAN 10 DOLLARS                
         LA    RE,T                RECEIVING FIELD                              
         LH    RF,=Y(L'T)          RECEIVING FIELD LENGTH                       
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   T-4(2),=X'0021'                                                  
         MVC   T(2),=X'521D'                                                    
         MVC   T+2(15),=C'TRANS UPLOAD   '                                      
         ZAP   T+17(6),POSTREC                                                  
         ZAP   T+23(6),POSTCASH                                                 
         GOTO1 ACREPORT                                                         
         BAS   RE,ADDPOST                                                       
         BAS   RE,CLOSPOST                                                      
*                                                                               
RUNL4    LA    R6,XP                                                            
         USING PLD,R6                                                           
         MVC   PLDEBIT,=C'--------------'                                       
         MVC   PLCREDIT,=C'--------------'                                      
         LA    R6,XPSECOND                                                      
         MVC   PLNME(24),=C'*** TOTAL FOR REPORT ***'                           
         EDIT  (P6,PDEBITS),(14,PLDEBIT),2,MINUS=YES                            
         EDIT  (P6,PCREDITS),(14,PLCREDIT),2,MINUS=YES                          
         LA    R6,XPTHIRD                                                       
         MVC   PLNME(24),=C'*** TOTAL HIST BUCKETS**'                           
         EDIT  (P6,BUKDRSV),(14,PLDEBIT),2,MINUS=YES                            
         EDIT  (P6,BUKCRSV),(14,PLCREDIT),2,MINUS=YES                           
         BAS   RE,ACRPT                                                         
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,CNTS                                                          
         LA    R6,XP                                                            
         LA    R3,PLCACC                                                        
RUNL8    EDIT  (P5,0(R2)),(6,PLDEBIT)                                           
         MVC   PLNME(20),5(R2)                                                  
         BAS   RE,ACRPT                                                         
         LA    R2,L'CNTS(R2)                                                    
         CLI   0(R2),X'FF'                                                      
         BNE   RUNL8                                                            
*                                                                               
         CLI   WRKSW,C'O'          WORKER FILE OPTION ON?                       
         BNE   RUNL10                                                           
         LA    R6,XP                                                            
         MVC   PLNME(14),=C'WORKER RECORDS'                                     
         EDIT  (P6,POSTREC),(14,PLDEBIT)                                        
         LA    R6,XPSECOND                                                      
         MVC   PLNME(14),=C'WORKER DOLLARS'                                     
         EDIT  (P6,POSTCASH),(14,PLDEBIT),2,MINUS=YES                           
         BAS   RE,ACRPT                                                         
*                                                                               
RUNL10   BAS   RE,ERPRT            ERROR REPORT                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VERIFY ALL RECORD INFO                                              *         
***********************************************************************         
*                                                                               
VERIFY   NTR1  ,                                                                
         USING SRTRECD,R6                                                       
         MVI   ERRFLG,0            RESET ERRORS TO NONE                         
         XC    ERRLST,ERRLST                                                    
         LA    R6,SORTREC                                                       
         LA    R2,DKEY                                                          
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL    COMPANY                                      
         MVC   ACTKULA,SRTKULA     SJ ACCOUNT                                   
         BAS   RE,DMRD                                                          
         LA    R2,DIR                                                           
         CLC   DKEY(L'ACTKEY),DIR                                               
         BNE   *+12                                                             
         TM    ACTKSTAT,ACTSDELT+ACTSCLOS+ACTSLOCK                              
         BZ    VER01               MARK DEL CLOSED OR LOCKED AS ERROR           
         MVI   ERRNUM,ERRACC       BAD ACCOUNT                                  
         BAS   RE,ERROR                                                         
         B     VER02                                                            
*                                                                               
VER01    CLC   ACTKULA,SJACCT     DID WE ALREADY LOOKUP THE NAME                
         BE    VER02                                                            
         MVC   SJACCT,ACTKULA                                                   
         MVC   SJNAME,SPACES                                                    
         MVC   BILLPRNT,SPACES                                                  
         BAS   RE,DMGETR                                                        
         L     R3,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                         MUST HAVE A NAME                     
         DC    H'0'                                                             
         USING NAMELD,R3                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   SJNAME(0),NAMEREC                                                
         L     R3,AIO                                                           
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL                                                         
         BNE   VER02                       PRINT ON BILLS INFO                  
         USING PPRELD,R3                                                        
         MVC   BILLPRNT,PPRBILLP                                                
*                                                                               
VER02    LA    R2,DKEY                                                          
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL    COMPANY                                      
         MVC   ACTKULA,SRTKULC     CONTRA                                       
         BAS   RE,DMRD                                                          
         LA    R2,DIR                                                           
         CLC   DKEY(L'ACTKEY),DIR                                               
         BNE   *+12                                                             
         TM    ACTKSTAT,ACTSDELT+ACTSCLOS+ACTSLOCK                              
         BZ    VER04               MARK DELETED CLOSED OR LOCKED AS ERR         
         MVI   ERRNUM,ERRCACC      BAD CONTRA                                   
         BAS   RE,ERROR                                                         
*                                                                               
VER04    CLC   SRTKUNT(2),=C'S9'                                                
         BE    VER06                                                            
         LA    R2,DKEY                                                          
         USING WCORECD,R2                                                       
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,RCCOMPFL    COMPANY                                      
         MVC   WCOKUNT(2),SRTKUNT  U/L                                          
         MVC   WCOKWRK,SRTKWORK    WORKCODE                                     
         BAS   RE,DMRD                                                          
         CLC   DKEY(L'WCOKEY),DIR                                               
         BE    VER06                                                            
         MVI   ERRNUM,ERRWORK      WORKCODE NOT FOUND                           
         BAS   RE,ERROR                                                         
*                                                                               
VER06    MVC   WORK,SPACES                                                      
         MVC   WORK(8),=C'  /  /  '                                             
         MVC   WORK(2),SRTKDATE+2                                               
         MVC   WORK+3(2),SRTKDATE+4                                             
         MVC   WORK+6(2),SRTKDATE                                               
         GOTO1 DATVAL,DMCB,(0,WORK),WORK+20                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+12                                                             
         MVI   ERRNUM,ERRDATE      BAD DATE PASSED                              
         BAS   RE,ERROR                                                         
*                                                                               
         CLC   SRTKREF,SPACES                                                   
         BH    *+12                                                             
         MVI   ERRNUM,ERRREF       BAD REF                                      
         BAS   RE,ERROR                                                         
*                                                                               
         CLC   SRTKUNT(2),=C'S9'                                                
         BNE   VER08                                                            
         TM    SRTRSTAT,SRTRDEB            SHOULD ALWAYS BE A CREDIT            
         BNO   *+6                                                              
         DC    H'0'                                                             
         CLC   SRTRAMNT,SPACES             AND AMOUNT SHOULD BE BLANK           
         BNE   VER10                                                            
         USING BTOTD,RE                                                         
         L     RE,ABTOTTAB                 TABLE OF BATCH TOTALS                
         LA    R1,BTOTMAX                  MAX NUMBER OF ENTRIES                
         CLC   SRTRBTCH,BTOTBTCH           MATCH BATCH REFERENCE                
         BE    *+14                                                             
         LA    RE,BTOTLEN(RE)                                                   
         BCT   R1,*-14                                                          
         DC    H'0'                                                             
         TM    BTOTSTAT,BTOTUSED           IF USED ALREADY TROUBLE              
         BNO   *+6                                                              
         DC    H'0'                                                             
         OI    BTOTSTAT,BTOTUSED           MARK USED                            
         CP    BTOTAMNT,ZEROS              DON'T MAKE ZERO POSTING              
         BE    VER10                                                            
         EDIT  (P6,BTOTAMNT),(12,SRTRAMNT),0,FLOAT=-                            
*                                                                               
VER08    MVC   WORK,SPACES                                                      
         MVC   WORK(L'SRTRAMNT),SRTRAMNT                                        
         ZAP   AMOUNT,ZEROS                                                     
         GOTO1 ADSQUASH,DMCB,WORK,L'WORK                                        
         ICM   R0,15,DMCB+4                                                     
         GOTO1 CASHVAL,DMCB,(C'0',WORK),(R0)                                    
         CLI   0(R1),0                                                          
         BNE   VER10                                                            
         ZAP   AMOUNT,4(8,R1)                                                   
         B     *+12                                                             
VER10    MVI   ERRNUM,ERRAMNT      BAD TRANS AMOUNT                             
         BAS   RE,ERROR                                                         
*                                                                               
         CLI   SRTRTYPE,49         EXPECT RATES AND HOURS ON 49'S               
         BNE   VER14                                                            
         CLC   SRTKUNT(2),=C'S9'   BUT NOT FOR S9                               
         BE    VER14                                                            
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'SRTRRATE),SRTRRATE                                        
         ZAP   RATE,ZEROS                                                       
         GOTO1 ADSQUASH,DMCB,WORK,L'WORK                                        
         ICM   R0,15,DMCB+4                                                     
         GOTO1 CASHVAL,DMCB,(C'0',WORK),(R0)                                    
         CLI   0(R1),0                                                          
         BNE   VER12                                                            
         CP    4(8,R1),=P'999999'                                               
         BH    VER12                                                            
         CP    4(8,R1),=P'-999999'                                              
         BL    VER12                                                            
         ZAP   RATE,4(8,R1)                                                     
         B     *+12                                                             
VER12    MVI   ERRNUM,ERRRATE      BAD RATE AMOUNT                              
         BAS   RE,ERROR                                                         
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'SRTRHOUR),SRTRHOUR                                        
         ZAP   HOURS,ZEROS                                                      
         GOTO1 ADSQUASH,DMCB,WORK,L'WORK                                        
         ICM   R0,15,DMCB+4                                                     
         GOTO1 CASHVAL,DMCB,(X'80',WORK),(R0)                                   
         CLI   0(R1),0                                                          
         BNE   VER13                                                            
         CP    4(8,R1),=P'99975'                                                
         BH    VER13                                                            
         CP    4(8,R1),=P'-99975'                                               
         BL    VER13                                                            
         ZAP   HOURS,4(8,R1)                                                    
         B     *+12                                                             
VER13    MVI   ERRNUM,ERRHOURS     BAD HOURS                                    
         BAS   RE,ERROR                                                         
*                                                                               
         ZAP   DUB,RATE            ON A 49 THE AMOUNT=RATE X HOURS              
         MP    DUB,HOURS                                                        
         SRP   DUB,64-2,5                                                       
         CP    AMOUNT,DUB                                                       
         BE    *+12                                                             
         MVI   ERRNUM,ERRAMNT2     BAD TRANS AMOUNT                             
         BAS   RE,ERROR                                                         
*                                                                               
         CLI   SRTRTTIM,C'B'                                                    
         BE    VER14                                                            
         CLI   SRTRTTIM,C' '                                                    
         BH    *+12                                                             
         MVI   SRTRTTIM,C'B'       B TIME IS DEFAULT                            
         B     VER14                                                            
         MVI   ERRNUM,ERRTYPE      BAD TYPE OF TIME                             
         BAS   RE,ERROR                                                         
*                                                                               
VER14    CLI   ERRFLG,0            CHECK FOR ERRORS                             
         BE    VER99Y                                                           
*                                                                               
         AP    CNTERRI,=P'1'                                                    
         L     R2,ATSRREC                                                       
         USING TSRRECD,R2                                                       
         MVC   TSRKBREF,SRTRBREF   PUT OUT ERROR BY BATCH REFERENCE             
         ZAP   TSRKNUMB,CNTERRI    USE ERROR NUMBER TO MAKE UNIQUE              
         MVC   TSRKEY2(TSRRLEN2),SRTKEY                                         
         MVC   TSRRERNM,ERRFLG                                                  
         MVC   TSRRERTB,ERRLST     LIST OF ERRORS                               
         L     R1,ATSRBLK          R1 IS A(TSAR BLOCK)                          
         USING TSARD,R1                                                         
         ST    R2,TSAREC           RECORD TO BE WRITTEN                         
         MVI   TSOFFACT,TSAADD     ACTION ADD                                   
         GOTO1 ATSAROFF                                                         
         BE    *+6                                                              
         DC    H'0'                TABLE FULL                                   
VER99N   LTR   RB,RB               NOT EQUAL CONDITION ON EXIT                  
         B     XIT                                                              
VER99Y   CR    RB,RB               EQUAL CONDITION ON EXIT                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD A NEW TRANSACTION RECORD                                      *         
***********************************************************************         
*                                                                               
TRNBILD  NTR1  ,                                                                
         L     RE,ATRNREC          RECEIVING FIELD                              
         LH    RF,=Y(MXRLNQ)       RECEIVING FIELD LENGTH                       
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
         USING SRTRECD,R6                                                       
         LA    R6,SORTREC                                                       
         L     R2,ATRNREC                                                       
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,RCCOMPFL            COMPANY                              
         MVC   TRNKULA,SRTKULA             ACCOUNT                              
         MVC   TRNKWORK,SRTKWORK           WORKCODE/OFFICE                      
         MVC   TRNKCCPY,TRNKCPY                                                 
         MVC   TRNKULC,SRTKULC             CONTRA                               
         GOTO1 DATCON,DMCB,(0,SRTKDATE),(1,TRNKDATE)                            
         MVC   TRNKREF,SRTKREF             REFERENCE                            
         XC    TRNKSBR,TRNKSBR             SET SUB REF TO X'00'                 
         MVC   TRNRSTYP,SRTRTYPE           TRANSACTION TYPE                     
         MVC   TRNRSMOS,MOS                MOS                                  
         LH    R1,=Y(TRNRFST-TRNRECD+1)                                         
         STCM  R1,3,TRNRLEN                RECORD LENGTH                        
         CLI   WRKSW,C'O'                  WORKER FILE OPTION ON?               
         BE    TRNB08                      YES - SKIP SUBREF STUFF              
*                                                                               
         XC    SUBREF,SUBREF               CLEAR SUBREF WORK AREA               
         CLC   TRNKEY(TRNKEND-1),TRNKEYSV  SAME KEY AS LAST UPDATED TRN         
         BNE   TRNB02                      NO - READ FILE FOR NEXT SEQ#         
         SR    R1,R1                                                            
         IC    R1,TRNKEYSV+(TRNKSBR-TRNRECD) YES- LAST UPDATED ASHIGH           
         STH   R1,SUBREF                   SEQUENCE NUMBER                      
         B     TRNB05                                                           
TRNB02   MVC   DKEY,TRNKEY                 READ FILE FOR HIGHEST SEQ #          
         BAS   RE,DMHGH                                                         
         CLC   DKEY(TRNKSBR-TRNRECD),DIR   NOT FOUND MEANS SEQ=0                
         BNE   TRNB06                                                           
         B     TRNB04A                                                          
TRNB04   BAS   RE,DMSEQ                                                         
         CLC   DKEY(TRNKSBR-TRNRECD),DIR                                        
         BNE   TRNB05                                                           
TRNB04A  XC    SUBREF,SUBREF                                                    
         LA    R2,DIR                                                           
         MVC   SUBREF+1(1),TRNKSBR                                              
         B     TRNB04                                                           
TRNB05   LH    R1,SUBREF                   BUMP SEQUENCE # BY 1                 
         LA    R1,1(R1)                                                         
         STH   R1,SUBREF                                                        
         SRA   R1,8                        TEST OVERFLOW                        
         BZ    *+6                                                              
         DC    H'0'                        DIE FOR NOW                          
*                                                                               
TRNB06   L     R2,ATRNREC                  RESET R2 TO NEW TRANS RECORD         
         LH    R1,SUBREF                   SEQUENCE #                           
         STC   R1,TRNKSBR                  STORE IN RECORD KEY                  
*                                                                               
TRNB08   XC    ELEMENT,ELEMENT             BUILD  44 ELEMENT                    
         LA    R3,ELEMENT                                                       
         USING TRNELD,R3                                                        
         MVI   TRNEL,TRNELQ                X'44'                                
         LH    R1,=Y(TRNLN1Q+1)            ASSUME A 1 BYTE NARRATIVE            
         STC   R1,TRNLN                                                         
         MVC   TRNDATE,TRNKDATE            DATE                                 
         MVC   TRNREF,TRNKREF              REFERENCE                            
         MVC   TRNSUB,TRNKSBR              SUB REF SAME AS KEY                  
         MVC   TRNMOS,SRTRMOS              MOS AS PART OF BATCH REF             
         MVC   TRNANAL,TRNKWORK            WORKCODE                             
         MVC   TRNBTCH,SRTRBTCH            BATCH REFERENCE                      
         MVC   TRNTYPE,TRNRSTYP            TRANSACTION TYPE                     
         MVC   TRNSTAT,SRTRSTAT            TRANSACTION STATUS                   
*                                                                               
TRNB10   MVC   WORK,SPACES                                                      
         MVC   WORK(L'SRTRAMNT),SRTRAMNT                                        
         GOTO1 ADSQUASH,DMCB,WORK,L'WORK                                        
         ICM   R0,15,DMCB+4                                                     
         GOTO1 CASHVAL,DMCB,(C'0',WORK),(R0)                                    
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   TRNAMNT,4(8,R1)                                                  
TRNB12   MVI   TRNNARR,SPACE               DEFAULT IS 1 BYTE NARRATIVE          
         CLC   SRTRNAR,XSPACES             DO WE HAVE ANY?                      
         BNH   TRNB14                      NO -GO ADD ELEMENT NOW               
         GOTO1 ADSQUASH,DMCB,SRTRNAR,L'SRTRNAR                                  
         ICM   R1,15,DMCB+4                                                     
         SH    R1,=H'1'                                                         
         EX    R1,*+4                                                           
         MVC   TRNNARR(0),SRTRNAR          NARRATIVE                            
         ICM   R1,15,DMCB+4                                                     
         LA    R1,TRNLN1Q(R1)                                                   
         STC   R1,TRNLN                    UPDATE ELEMENT LENGTH                
TRNB14   L     R2,ATRNREC                                                       
         BAS   RE,ADDL2                                                         
*                                                                               
*        XC    ELEMENT,ELEMENT             BUILD  DUMMY 4B ELEMENT              
*        LA    R3,ELEMENT                                                       
*        USING BNDELD,R3                                                        
*        MVI   BNDEL,BNDELQ                                                     
*        MVI   BNDLN,BNDLN2Q                                                    
*        ZAP   BNDCMP,ZEROS                COMMISSION RATE                      
*        L     R2,ATRNREC                                                       
*        BAS   RE,ADDL2                                                         
*                                                                               
         CLI   TRNRSTYP,49                                                      
         BNE   TRNB18                                                           
         CLC   TRNKULA(2),=C'S9'           IS THIS THE S9 POSTING?              
         BE    TRNB18                      YES - SKIP 40 EL                     
         XC    ELEMENT,ELEMENT             BUILD 40 ELEMENT                     
         LA    R3,ELEMENT                                                       
         USING PRTELD,R3                                                        
         MVI   PRTEL,PRTELQ                                                     
         MVI   PRTLN,PRTLNQ                                                     
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'SRTRRATE),SRTRRATE                                        
         GOTO1 ADSQUASH,DMCB,WORK,L'WORK                                        
         ICM   R0,15,DMCB+4                                                     
         GOTO1 CASHVAL,DMCB,(C'0',WORK),(R0)                                    
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   PRTRATE,4(8,R1)                                                  
         MVC   WORK,SPACES                                                      
         MVC   WORK(L'SRTRHOUR),SRTRHOUR                                        
         GOTO1 ADSQUASH,DMCB,WORK,L'WORK                                        
         ICM   R0,15,DMCB+4                                                     
         GOTO1 CASHVAL,DMCB,(X'80',WORK),(R0)                                   
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   PRTHOUR,4(8,R1)                                                  
         CLI   SRTRTTIM,C'B'                                                    
         BNE   *+12                                                             
         OI    PRTSTAT,PRTSBILQ            B TIME                               
         B     TRNB16                                                           
         CLI   SRTRTTIM,C'R'                                                    
         BNE   *+12                                                             
         OI    PRTSTAT,PRTSRTEQ            R TIME                               
         B     TRNB16                                                           
         CLI   SRTRTTIM,C'N'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    PRTSTAT,PRTSNOTQ            N TIME                               
TRNB16   L     R2,ATRNREC                                                       
         BAS   RE,ADDL2                                                         
*                                                                               
TRNB18   XC    ELEMENT,ELEMENT             BUILD  60 ELEMENT                    
         LA    R3,ELEMENT                                                       
         USING TRSELD,R3                                                        
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSDATE,TODAYC              ACTIVITY DATE                        
         MVC   TRSEFDT,TODAYC              EFFECTIVE DATE                       
         MVC   TRSPMOS,MOS                 MONTH OF SERVICE                     
         MVC   TRSUSER,ORIGINUM            USER ID NUMBER                       
         CLI   TRNRSTYP,49                                                      
         BNE   *+8                                                              
         OI    TRSSTAT2,TRSSTIME           SET 49 TO REGULAR TIMESHEET          
         L     R2,ATRNREC                                                       
         BAS   RE,ADDL2                                                         
*                                                                               
         CLC   TRNKULA(2),=C'S9'           IS THIS THE S9 POSTING?              
         BE    XIT                         YES - SKIP PTA                       
         CLI   TRNRSTYP,49                 IS THIS A TYPE 49?                   
         BNE   TRNB20                      NO - SKIP X'75' EL                   
         XC    ELEMENT,ELEMENT             BUILD  60 ELEMENT                    
         LA    R3,ELEMENT                                                       
         USING TRXELD,R3                                                        
         MVI   TRXEL,TRXELQ                                                     
         MVI   TRXLN,TRXLN1Q                                                    
         OI    TRXSTA2,TRXSNTMS            MARK AS NO TMS RECORD                
         L     R2,ATRNREC                                                       
         BAS   RE,ADDL2                                                         
*                                                                               
TRNB20   LA    R3,ELEMENT                                                       
         USING PTAELD,R3                   BUILD DUMMY PTA ELEMENT              
         XC    PTAEL(PTARLN1Q),PTAEL                                            
         MVI   PTAEL,PTAELQ                                                     
         MVI   PTALN,PTARLN1Q                                                   
         ZAP   PTANET,ZEROS                                                     
         ZAP   PTANETF,ZEROS                                                    
         ZAP   PTACDSC,ZEROS                                                    
         ZAP   PTARCORT,ZEROS                                                   
         ZAP   PTARCOM,ZEROS                                                    
         L     R2,ATRNREC                                                       
         BAS   RE,ADDL2                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD VIRGIN CONTRA HEADER                                          *         
***********************************************************************         
*                                                                               
CHDBILD  NTR1  ,                                                                
         L     RE,ACHDREC                                                       
         LH    RF,=Y(MXRLNQ)                                                    
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
         L     R2,ACHDREC                                                       
         USING CHDRECD,R2                                                       
         L     RE,ATRNREC                                                       
         MVC   CHDKEY,SPACES               CLEAR HEADER TO SPACES               
         MVC   CHDKEY(L'CHDKCULA+L'CHDKWRK+L'CHDKCULC),0(RE)                    
         XC    CHDKNULL,CHDKNULL                                                
         LH    R1,=Y(CHDRFST-CHDRECD)                                           
         STCM  R1,3,CHDRLEN                RECORD LENGTH                        
*                                                                               
         CLC   CHDKCULC,CONTRASV           SAME CONTRA AS PREVIOUS?             
         BE    CHD4                                                             
         LA    RE,DKEY                                                          
         USING ACTRECD,RE                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,CHDKCULC                                                
         BAS   RE,DMRD                                                          
         CLC   DKEY(L'CACKEY),DIR                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DMGETR                   GET CURRENT FILE RECORD              
         L     R3,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                         MUST HAVE A NAME                     
         DC    H'0'                                                             
         USING NAMELD,R3                                                        
         MVC   CONNAMSV,SPACES             CLEAR NAME SAVE AREA                 
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   CONNAMSV(0),NAMEREC                                              
         MVC   CONTRASV,CHDKCULC                                                
CHD4     XC    ELEMENT,ELEMENT             BUILD  43 ELEMENT                    
         LA    R3,ELEMENT                                                       
         USING CACELD,R3                                                        
         MVI   CACEL,CACELQ                X'43'                                
         MVI   CACLN,CACLN1Q                                                    
         MVC   CACCNT,CHDKCULC                                                  
         LA    R1,CONNAMSV+L'CONNAMSV-1                                         
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,CONNAMSV                                                      
         SR    R1,R0                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   CACNAME,CONNAMSV                                                 
         LA    R1,CACLN1Q+1(R1)                                                 
         STC   R1,CACLN                                                         
         BAS   RE,ADDL                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD VIRGIN CONTRA HISTORY RECORD                                  *         
***********************************************************************         
*                                                                               
CACBILD  NTR1  ,                                                                
         L     RE,AHISREC                                                       
         LH    RF,=Y(MXRLNQ)                                                    
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
         L     R2,AHISREC                                                       
         USING CACRECD,R2                                                       
         L     RE,ATRNREC                                                       
         MVC   CACKEY,SPACES               CLEAR HISTORY KET TO SPACES          
         MVC   CACKEY(L'CACKCULA+L'CACKWRK+L'CACKCULC),0(RE)                    
         LH    R1,=Y(CACRFST-CACRECD)                                           
         STCM  R1,3,CACRLEN                RECORD LENGTH                        
*                                                                               
         XC    ELEMENT,ELEMENT             BUILD  45 ELEMENT                    
         LA    R3,ELEMENT                                                       
         USING BUKELD,R3                                                        
         MVI   BUKEL,BUKELQ                X'45'                                
         MVI   BUKLN,BUKLNQ                                                     
         MVC   BUKMOS,MOS                                                       
         ZAP   BUKDR,ZEROS                                                      
         ZAP   BUKCR,ZEROS                                                      
         BAS   RE,ADDL                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE TRANSACTION RECORD TO TAPE OR FILE                           *         
***********************************************************************         
*                                                                               
TRNUP    NTR1  ,                                                                
         L     R2,ATRNREC                                                       
         USING TRNRECD,R2                                                       
         LR    RE,R2               WRITE RECORDS FROM AIO2                      
         L     R0,AIO2             ADDR OF RECEIVING                            
         LH    R1,=Y(MXRLNQ)       RECEIVING FIELD LENGTH                       
         LR    RF,R1               SENDING FIELD LENGTH                         
         MVCL  R0,RE                                                            
         CLI   WRKSW,C'O'                                                       
         BNE   TRNUP2                                                           
         BAS   RE,WORKIT           PUT TRANSACTION TO WORKER FILE               
         B     TRNUP4                                                           
TRNUP2   MVC   MSG,MSG1            TRANSACTION ADDED MESSAGE                    
         BAS   RE,ADDR             ADD RECORD TO FILE OR TAPE                   
         BE    TRNUP4                                                           
         MVC   MSG,MSG5            DUMP BAD TRANSACTION                         
         BAS   RE,DUMPER                                                        
         B     XIT                 ERROR ON ADD                                 
TRNUP4   AP    CNTTRAN,=P'1'                                                    
         MVC   TRNKEYSV,TRNKEY     SAVE LAST UPDATED KEY                        
         BAS   RE,REPT             PRINT REPORT LINE                            
         LA    R4,TRNRFST                                                       
         CLI   0(R4),X'44'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TRNELD,R4                                                        
         CLC   TRNKCULA,ACCKEYSV   MAKE SURE TIMING IS RIGHT                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TRNKULA(2),=C'S9'           IS THIS THE S9 POSTING?              
         BE    TRNUP8                      YES -                                
         TM    TRNSTAT,TRNSDR              SHOULD ALWAYS BE A DEBIT             
         BO    *+6                                                              
         DC    H'0'                                                             
         USING BTOTD,RE                                                         
         L     RE,ABTOTTAB                 TABLE OF BATCH TOTALS                
         LA    R1,BTOTMAX                  MAX NUMBER OF ENTRIES                
         CLC   TRNBTCH,BTOTBTCH            MATCH BATCH REFERENCE                
         BE    *+14                                                             
         LA    RE,BTOTLEN(RE)                                                   
         BCT   R1,*-14                                                          
         DC    H'0'                                                             
         AP    BTOTAMNT,TRNAMNT            ADD TO BATCH TOTAL                   
TRNUP8   LA    R1,ACC32CR                  UPDATE BALANCE ACCUMS                
         LA    RE,PCREDITS                 KEEP TRACK OF TOTAL DR CR            
         TM    TRNSTAT,TRNSDR                                                   
         BNO   TRNUP10                                                          
         AP    POSTCASH,TRNAMNT            UPDATE TOTAL FOR WORKFILE            
         LA    R1,ACC32DR                                                       
         LA    RE,PDEBITS                                                       
TRNUP10  AP    0(L'ACC32DR,R1),TRNAMNT                                          
         AP    0(L'PDEBITS,RE),TRNAMNT                                          
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING BUKELD,R3                                                        
         MVI   BUKEL,BUKELQ        SET UP HISTORY BUCKET ELEMENT                
         MVI   BUKLN,BUKLNQ        LENGTH                                       
         MVC   BUKMOS,MOS          MONTH OF SERVICE                             
*                                                                               
         L     R5,AHISREC          MAKE SURE TIMING IS RIGHT                    
         CLC   TRNKEY(L'TRNKCULA+L'TRNKWORK+L'TRNKCULC),0(R5)                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 HELLO,ELIST,(C'G',ACCMST),('BUKELQ',(R5)),              X        
               (L'BUKMOS,BUKMOS)                                                
         CLI   ELERR,0                                                          
         BE    *+6                 NO MATCHING BUCKET ELEMENT                   
         DC    H'0'                                                             
         L     R3,ELADDR                                                        
         LA    R1,BUKDR            UPDATE BUCKET ELEMENT                        
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R1,BUKCR                                                         
         AP    0(L'BUKDR,R1),TRNAMNT                                            
         B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* ADD A CONTRA HEADER IF NOT ON FILE                                  *         
* NOTE: DO NOT PUT OUT HISTORY IF ANY PROBLEM WITH CHEADER            *         
***********************************************************************         
*                                                                               
CACUP    NTR1  ,                                                                
         CLI   WRKSW,C'O'          SKIP IF WRITING TO WORKER FILE               
         BE    XIT                                                              
         MVI   BYTE,C'Y'           CONTRA HEADER "GOOD" SWITCH                  
         L     R2,ACHDREC          CONTRA HEADER                                
         USING CHDRECD,R2                                                       
         CLC   CHDKEY,SPACES                                                    
         BNH   XIT                                                              
         MVC   DKEY,CHDKEY                                                      
         BAS   RE,DMRD             TEST CHEADER RECORD ALREADY ON FILE          
         CLC   DKEY(L'CHDKEY),DIR                                               
         BE    CACUP4              YES - GO ADD HISTORY RECORD                  
*                                  NO -  ADD CHEADER TO FILE/TAPE               
         LR    RE,R2               WRITE RECORDS FROM AIO2                      
         L     R0,AIO2             ADDR OF RECEIVING                            
         LH    R1,=Y(MXRLNQ)       RECEIVING FIELD LENGTH                       
         LR    RF,R1               SENDING FIELD LENGTH                         
         MVCL  R0,RE                                                            
         MVC   MSG,MSG2            CONTRA HEADER ADDED MESSAGE                  
         BAS   RE,ADDR             ADD RECORD TO FILE OR TAPE                   
         BNE   *+14                                                             
         AP    CNTCHED,=P'1'                                                    
         B     CACUP4                                                           
         MVC   MSG,MSG6            DUMP BAD CHEADER RECORD                      
         BAS   RE,DUMPER                                                        
         MVI   BYTE,C'N'           DON'T ADD HISTORY                            
         B     CACUP6              ERROR ON ADD                                 
*                                                                               
CACUP4   L     R2,AHISREC          HISTORY BUCKET RECORD                        
         USING CACRECD,R2                                                       
         CLC   CACKEY,SPACES                                                    
         BH    *+6                                                              
         DC    H'0'                                                             
         L     RE,ACHDREC          IN SYNC WITH CH?                             
         CLC   CACKEY(L'CACKCULA+L'CACKWRK+L'CACKCULC),0(RE)                    
         BE    *+6                 YES - CONTINUE                               
         DC    H'0'                                                             
         CLC   CACKULA(2),=C'S9'   IS THIS THE S9 POSTING?                      
         BNE   *+10                YES - REMOVE WC/OFFICE                       
         MVC   CACKWRK,SPACES                                                   
         MVC   DKEY,CACKEY                                                      
         BAS   RE,DMRD             TEST RECORD ON FILE                          
         CLC   DKEY(L'CACKEY),DIR                                               
         BNE   CACUP6              NO - ADD HISTORY RECORD TO FILE/TAPE         
         BAS   RE,BUCKIT           YES - ADD TO CURRENT BUCKET RECORD           
         B     XIT                                                              
*                                                                               
CACUP6   L     R2,AHISREC          HISTORY BUCKET RECORD                        
         LR    RE,R2               WRITE RECORDS FROM AIO2                      
         L     R0,AIO2             ADDR OF RECEIVING                            
         LH    R1,=Y(MXRLNQ)       RECEIVING FIELD LENGTH                       
         LR    RF,R1               SENDING FIELD LENGTH                         
         MVCL  R0,RE                                                            
         CLI   BYTE,C'N'           IF CHEADER ERROR DON'T PUT HISTORY           
         BE    CACUP12                                                          
         MVC   MSG,MSG3            HISTORY ADDED MESSAGE                        
         BAS   RE,ADDR             ADD RECORD TO FILE OR TAPE                   
         BNE   CACUP12                                                          
         AP    CNTBUCK,=P'1'                                                    
         L     R3,AHISREC                                                       
         AH    R3,DATADIS2                                                      
         USING BUKELD,R3                                                        
CACUP08  CLI   BUKEL,0                                                          
         BE    XIT                                                              
         CLI   BUKEL,BUKELQ                                                     
         BE    CACUP10                                                          
CACUP09  SR    R1,R1                                                            
         IC    R1,BUKLN                                                         
         AR    R3,R1                                                            
         B     CACUP08                                                          
CACUP10  CLC   BUKMOS,MOS                                                       
         BNE   CACUP09                                                          
         AP    BUKDRSV,BUKDR      KEEP TRACK OF TOTAL BUCKETS                   
         AP    BUKCRSV,BUKCR                                                    
         B     XIT                                                              
CACUP12  MVC   MSG,MSG7            DUMP BAD HISTORY                             
         BAS   RE,DUMPER                                                        
         B     XIT                 ERROR ON ADD                                 
         EJECT                                                                  
***********************************************************************         
* UPDATE TRANSACTION RECORD TO TAPE OR FILE                           *         
***********************************************************************         
*                                                                               
BALUP    NTR1  ,                                                                
         LA    R2,ACCKEYSV                                                      
         USING ACTRECD,R2                                                       
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(L'ACTKCULA),ACTKCULA                                        
         CLC   DKEY,SPACES                                                      
         BNH   BALUPX                                                           
         CP    ACC32DR,ZEROS       DON'T BOTHER IF AMT IS ZERO                  
         BNZ   *+14                                                             
         CP    ACC32CR,ZEROS                                                    
         BZ    BALUPX                                                           
         CLI   WRKSW,C'O'          SKIP IF WRITING TO WORKER FILE               
         BE    BALUP4                                                           
         BAS   RE,DMRD             TEST RECORD ON FILE                          
         CLC   DKEY(L'ACTKEY),DIR                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DMGETR           GET CURRENT FILE RECORD                      
         L     R3,AIO                                                           
         MVI   ELCODE,X'32'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ABLELD,R3                                                        
         AP    ABLDR,ACC32DR                                                    
         AP    ABLCR,ACC32CR                                                    
         L     R2,AIO                                                           
         LR    RE,R2               WRITE RECORDS FROM AIO2                      
         L     R0,AIO2             ADDR OF RECEIVING                            
         LH    R1,=Y(MXRLNQ)       RECEIVING FIELD LENGTH                       
         LR    RF,R1               SENDING FIELD LENGTH                         
         MVCL  R0,RE                                                            
         BAS   RE,DMPUT            PUT RECORD BACK                              
         CLI   QOPT1,C'D'          DUMP OUTPUT                                  
         BNE   BALUP2                                                           
         MVC   MSG,MSG9            BALANCE UPDATED                              
         MVI   DUMPSW,C'Y'         DUMP SWITCH TO INDICATE PUT TO FILE          
         BAS   RE,DUMP                                                          
         MVI   DUMPSW,C'N'                                                      
*                                                                               
BALUP2   LA    R2,ACCKEYSV                                                      
         USING OFARECD,R2                                                       
         CLC   OFAKULA(2),=C'S9'   IS THIS THE S9 POSTING?                      
         BNE   BALUP4              NO- -                                        
         CLC   OFAKOFF,S9OFFICE    S9 OFFICE                                    
         BNE   BALUPX                                                           
         MVC   DKEY,SPACES                                                      
         MVC   DKEY(OFAKEND),OFAKCULA                                           
         BAS   RE,DMRD             TEST RECORD ON FILE                          
         CLC   DKEY(L'ACTKEY),DIR                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DMGETR           GET CURRENT FILE RECORD                      
         L     R3,AIO                                                           
         MVI   ELCODE,X'32'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ABLELD,R3                                                        
         AP    ABLDR,ACC32DR                                                    
         AP    ABLCR,ACC32CR                                                    
         L     R2,AIO                                                           
         LR    RE,R2               WRITE RECORDS FROM AIO2                      
         L     R0,AIO2             ADDR OF RECEIVING                            
         LH    R1,=Y(MXRLNQ)       RECEIVING FIELD LENGTH                       
         LR    RF,R1               SENDING FIELD LENGTH                         
         MVCL  R0,RE                                                            
         BAS   RE,DMPUT            PUT RECORD BACK                              
         CLI   QOPT1,C'D'          DUMP OUTPUT                                  
         BNE   BALUP4                                                           
         MVC   MSG,MSG10           OFFICE BALANCE UPDATED                       
         MVI   DUMPSW,C'Y'         DUMP SWITCH TO INDICATE PUT TO FILE          
         BAS   RE,DUMP                                                          
         MVI   DUMPSW,C'N'                                                      
*                                                                               
BALUP4   LA    R6,XP                                                            
         USING PLD,R6                                                           
         MVC   PLDEBIT,=C'--------------'                                       
         MVC   PLCREDIT,=C'--------------'                                      
         LA    R6,XPSECOND                                                      
         MVC   PLNME(24),=C'*** TOTAL FOR ACCOUNT **'                           
         EDIT  (P6,ACC32DR),(14,PLDEBIT),2,MINUS=YES                            
         EDIT  (P6,ACC32CR),(14,PLCREDIT),2,MINUS=YES                           
         CLI   QOPT1,C'E'                                                       
         BE    BALUPX                                                           
         BAS   RE,ACRPT                                                         
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
BALUPX   L     R1,ATRNREC          SAVE CURRENT ACCOUNT                         
         MVC   ACCKEYSV,SPACES                                                  
         MVC   ACCKEYSV,0(R1)                                                   
         ZAP   ACC32DR,ZEROS       CLEAR BALANCE ACCUMS                         
         ZAP   ACC32CR,ZEROS                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE FILE BUCKET ELEMENT                                          *         
* ADD OR UPDATE THE BUCKET ELEMENT THAT I BUILT TO THE CORRESPONDING  *         
* BUCKET FOR THE SAME MONTH ON THE FILE RECORD.                       *         
* I BUKEL DOES NOT EXIST THEN ADD THE ELEMENT                         *         
***********************************************************************         
*                                                                               
BUCKIT   NTR1  ,                                                                
         L     R2,AHISREC          HISTORY RECORD I BUILT                       
         USING CACRECD,R2                                                       
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING BUKELD,R3                                                        
         MVI   BUKEL,BUKELQ        SET UP HISTORY BUCKET ELEMENT                
         MVI   BUKLN,BUKLNQ        LENGTH                                       
         MVC   BUKMOS,MOS          MONTH OF SERVICE                             
         GOTO1 HELLO,ELIST,(C'G',ACCMST),('BUKELQ',CACRECD),           X        
               (L'BUKMOS,BUKMOS)                                                
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ELADDR                                                        
         CP    BUKDR,ZEROS       DON'T BOTHER IF AMT IS ZERO                    
         BNZ   *+14                                                             
         CP    BUKCR,ZEROS                                                      
         BZ    BUCKX                                                            
         XC    ELEMENT,ELEMENT                                                  
         SR    R1,R1                                                            
         IC    R1,BUKLN                                                         
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   ELEMENT(0),BUKEL    SAVE X'45' THAT I BUILT                      
*                                                                               
         MVC   DKEY,CACKEY                                                      
         BAS   RE,DMRD             TEST RECORD ON FILE                          
         CLC   DKEY(L'CACKEY),DIR                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DMGETR           GET CURRENT FILE RECORD                      
         L     R2,AIO                                                           
         GOTO1 HELLO,ELIST,(C'G',ACCMST),('BUKELQ',(R2)),              X        
               (L'BUKMOS,BUKMOS)                                                
         CLI   ELERR,0             BUCKET EL ON FILE?                           
         BE    BUCK2               YES - UPDATE FILE BUCKET                     
         L     R2,AIO              NO -  ADD A NEW BUCKET EL                    
         LA    R3,ELEMENT                                                       
         BAS   RE,ADDL                                                          
         B     BUCK4                                                            
*                                                                               
BUCK2    L     R3,ELADDR           CURRENT FILE BUCKET ELEMENT                  
         LA    R1,ELEMENT                                                       
         AP    BUKDR,BUKDR-BUKELD(L'BUKDR,R1)                                   
         AP    BUKCR,BUKCR-BUKELD(L'BUKCR,R1)                                   
*                                                                               
BUCK4    L     R2,AIO                                                           
         LR    RE,R2               WRITE RECORDS FROM AIO2                      
         L     R0,AIO2             ADDR OF RECEIVING                            
         LH    R1,=Y(MXRLNQ)       RECEIVING FIELD LENGTH                       
         LR    RF,R1               SENDING FIELD LENGTH                         
         MVCL  R0,RE                                                            
         BAS   RE,DMPUT            PUT RECORD BACK                              
         LA    R1,ELEMENT          SAVE TOTAL BUCKETS ADDED                     
         AP    BUKDRSV,BUKDR-BUKELD(L'BUKDR,R1)                                 
         AP    BUKCRSV,BUKCR-BUKELD(L'BUKCR,R1)                                 
         CLI   QOPT1,C'D'          DUMP OUTPUT                                  
         BNE   BUCKX                                                            
         MVC   MSG,MSG4            HISTORY UPDATED                              
         MVI   DUMPSW,C'Y'         DUMP SWITCH TO INDICATE PUT TO FILE          
         BAS   RE,DUMP                                                          
         MVI   DUMPSW,C'N'                                                      
BUCKX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD RECORD TO WORKER FILE                                           *         
***********************************************************************         
*                                                                               
WORKIT   NTR1  ,                                                                
         USING TRNRECD,R2                                                       
         L     R2,AIO2                                                          
         LA    R3,TRNRFST                                                       
         CLI   0(R3),X'44'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PSHEADD,R4          BUILD POSTING ELEMENT                        
         LA    R4,T                                                             
         XC    T-4(4),T-4                                                       
         MVI   PSHDEL,PSHDELQ                                                   
         MVI   PSHDLEN,PSHEADL                                                  
         MVC   PSHDACC,TRNKCULA    ACCOUNT                                      
         MVC   PSHDANAL,TRNKWORK                                                
         MVC   PSHDSBAC,TRNKCULC   CONTRA                                       
         CLC   TRNKCULC,CONTRASV                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PSHDSBNM,CONNAMSV   CONTRA NAME                                  
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
*                                                                               
WORKIT2  SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         SH    R1,=H'1'                                                         
         LTR   R1,R1                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)      MOVE ELEMENTS TO AREA ONE AT A TIME           
         LA    R4,1(R1,R4)        R4 TO NEXT OUTPUT AREA                        
         MVI   0(R4),0            END OF RECORD                                 
         LA    R3,1(R1,R3)        R3 TO NEXT ELEMENT                            
         CLI   0(R3),0                                                          
         BNE   WORKIT2                                                          
         BAS   RE,PUTIT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD RECORD TO THE FILE                                              *         
***********************************************************************         
*                                                                               
ADDR     NTR1  ,                                                                
         USING ACTRECD,R2                                                       
         L     R2,AIO2                                                          
         SR    R3,R3                                                            
         ICM   R3,3,ACTRLEN        SET LENGTH FOR TAPE                          
         LA    R3,4(R3)                                                         
         L     R5,AIO2                                                          
         SH    R5,=H'4'                                                         
         XC    0(4,R5),0(R5)                                                    
         STCM  R3,3,0(R5)                                                       
*                                                                               
         CLI   QOPT1,C'D'          DUMP OUTPUT                                  
         BNE   *+8                                                              
         BAS   RE,DUMP                                                          
         CLI   QOPT2,C'T'          OUTPUT TAPE                                  
         BNE   ADDR2                                                            
         MVC   DKEY,ACTKEY                                                      
         BAS   RE,DMRD             TEST RECORD ON FILE                          
         CLC   DKEY(L'ACTKEY),DIR                                               
         BE    ADDRBAD             ERROR                                        
         L     R3,AOUTFIL                                                       
         PUT   (R3),(R5)                                                        
         B     ADDRGOOD                                                         
*                                                                               
ADDR2    MVC   DKEY,ACTKEY                                                      
         BAS   RE,DMRD             TEST RECORD ON FILE                          
         CLC   DKEY(L'ACTKEY),DIR                                               
         BE    ADDRBAD                                                          
         BAS   RE,DMADDR           ADD NEW RECORD                               
*                                                                               
ADDRGOOD CR    RB,RB               EQUAL CONDITION ON EXIT                      
         B     XIT                                                              
ADDRBAD  AP    CNTERRO,=P'1'                                                    
         LTR   RB,RB               NOT EQUAL CONDITION ON EXIT                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                        *         
***********************************************************************         
*                                                                               
REPT     NTR1  ,                                                                
         LA    R6,XP                                                            
         USING PLD,R6                                                           
         L     R2,AIO2                                                          
         USING TRNRECD,R2                                                       
         LA    R4,TRNRFST                                                       
         USING TRNELD,R4                                                        
         CLI   0(R4),X'44'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TRNELD,R4                                                        
         MVC   XHEAD3(L'TRNKULA),TRNKULA                                        
         MVC   XHEAD3+L'TRNKULA+2(L'SJNAME),SJNAME                              
         MVC   XHEAD4(L'BILLPRNT),BILLPRNT                                      
         MVC   PLWORK,TRNKWORK                                                  
         MVC   PLCACC,TRNKULC                                                   
         MVC   PLNME,CONNAMSV                                                   
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(0,PLDATE)                              
         MVC   PLREF,TRNKREF                                                    
         EDIT  (B1,TRNRSTYP),(2,PLBTYPE)                                        
         EDIT  (B1,TRNKSBR),(2,PLSEQ),ZERO=NOBLANK                              
         MVC   PLBREF,TRNBTCH                                                   
         LA    R3,PLDEBIT                                                       
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R3,PLCREDIT                                                      
         EDIT  (P6,TRNAMNT),(14,(R3)),2,MINUS=YES                               
         CLC   TRNKULA(2),=C'S9'           IS THIS THE S9 POSTING?              
         BE    REPTXX                                                           
         CLI   TRNRSTYP,49                                                      
         BNE   REPTXX                                                           
         L     R3,AIO2                                                          
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
         BNE   REPTXX                                                           
         USING PRTELD,R3                                                        
         EDIT  (P4,PRTRATE),(10,PLRATE),2,MINUS=YES                             
         EDIT  (P3,PRTHOUR),(10,PLHOURS),2,MINUS=YES                            
         MVI   PLTTYPE+1,C'B'                                                   
         TM    PRTSTAT,PRTSBILQ                                                 
         BO    REPTXX                                                           
         MVI   PLTTYPE+1,C'R'                                                   
         TM    PRTSTAT,PRTSRTEQ                                                 
         BO    REPTXX                                                           
         MVI   PLTTYPE+1,C'N'                                                   
         TM    PRTSTAT,PRTSNOTQ                                                 
         BO    REPTXX                                                           
         MVI   PLTTYPE+1,C' '                                                   
REPTXX   CLI   QOPT1,C'E'                                                       
         BE    XIT                                                              
         BAS   RE,ACRPT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SET UP HEADLINES AND GO TO ACREPORT                                 *         
***********************************************************************         
*                                                                               
ACRPT    NTR1  ,                                                                
         MVC   XHEAD1+75(L'CMPNAME),CMPNAME                                     
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD ERROR ITEM TO TABLE                                             *         
***********************************************************************         
*                                                                               
ERROR    NTR1  ,                                                                
         SR    R1,R1                                                            
         IC    R1,ERRFLG           NUMBER SO FAR                                
         LA    RF,1(R1)                                                         
         STC   RF,ERRFLG           UPDATE COUNT                                 
         CH    RF,=Y(MXERR)                                                     
         BNH   *+6                                                              
         DC    H'0'                ERROR LIST IS FULL                           
         LA    R1,ERRLST(R1)                                                    
         MVC   0(1,R1),ERRNUM      SAVE ERROR NUMBER IN LIST                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD ERROR MESSAGES TO PRINT LINE                                    *         
***********************************************************************         
*                                                                               
ERPRT    NTR1  ,                                                                
         L     R2,ATSRREC                                                       
         USING TSRRECD,R2                                                       
         MVC   TSRKEY(L'TSRRLEN),XSPACES                                        
         L     R1,ATSRBLK          R1 IS A(TSAR BLOCK)                          
         USING TSARD,R1                                                         
         OC    TSPRECN(4),TSPRECN  ANY ERROR RECORDS IN TABLE?                  
         BZ    XIT                 NO -                                         
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVI   RCSUBPRG,1                                                       
*                                                                               
         MVC   BATBREFS,SPACES                                                  
         ST    R2,TSAREC           KEY OF RECORD FOR LOOKUP                     
         MVI   TSOFFACT,TSARDH     ACTION READ HIGH                             
         GOTO1 ATSAROFF                                                         
         TM    TSERRS,ALL-TSERNF   ANY ERRORS EXCEPT REC NOT FOUND?             
         BZ    ERPT2                                                            
         DC    H'0'                                                             
ERPT1    L     R1,ATSRBLK          R1 IS A(TSAR BLOCK)                          
         USING TSARD,R1                                                         
         MVI   TSOFFACT,TSANXT     ACTION READ NEXT                             
         GOTO1 ATSAROFF                                                         
         BE    ERPT2                                                            
         TM    TSERRS,TSEEOF       END OF BUFFER?                               
         BO    XIT                                                              
         DC    H'0'                                                             
*                                                                               
ERPT2    DS    0H                                                               
         LA    R6,XP                                                            
         USING PLD,R6                                                           
         MVC   XHEAD3(L'TSRKBREF),TSRKBREF                                      
         CLC   TSRKBREF,BATBREFS   NEW PAGE PER BATCH HEADER                    
         MVC   BATBREFS,TSRKBREF                                                
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   PLWORK,TSRKWORK     PRINT ERROR DETAIL LINE                      
         MVC   PLCACC,TSRKULA                                                   
         MVC   PLCACC2,TSRKULC                                                  
         MVC   PLDATE,TSRKDATE                                                  
         MVC   PLREF,TSRKREF                                                    
         EDIT  (B1,TSRRTYPE),(2,PLBTYPE)                                        
         MVC   PLBREF(L'TSRRBREF),TSRRBREF                                      
         MVC   PLRATE(L'TSRRRATE),TSRRRATE                                      
         MVC   PLHOURS(L'TSRRHOUR),TSRRHOUR                                     
         MVC   PLTTYPE+1(1),TSRRTTIM                                            
         MVC   PLDEBIT(L'TSRRAMNT),TSRRAMNT                                     
*                                                                               
         SR    R0,R0                                                            
         SR    R3,R3                                                            
         IC    R3,TSRRERNM         NUMBER OF ERRORS                             
         LH    R0,=H'4'            MAXIMUM 4 PER PRINT                          
         LA    R4,XP+(PLERR-PLD)                                                
         LA    R1,TSRRERTB         TABLE OF ERROR NUMBERS                       
*                                                                               
ERPT8    CH    R0,=H'0'                                                         
         BH    ERPT10                                                           
         BAS   RE,ACRPT                                                         
         LH    R0,=H'4'            MAXIMUM 4 PER PRINT                          
         LA    R4,XP+(PLERR-PLD)                                                
ERPT10   SR    RE,RE                                                            
         IC    RE,0(R1)            ERROR NUMBER                                 
         BCTR  RE,0                                                             
         MH    RE,=Y(ERRLNQ)                                                    
         L     RF,AERRS                                                         
         AR    RF,RE                                                            
         MVC   0(L'PLERR,R4),0(RF) CONVERT ERROR NUMBER TO DESCRIPTION          
         LA    R4,L'XP(R4)                                                      
         LA    R1,1(R1)                                                         
         SH    R0,=H'1'            KEEP TRACK OFF XP1,XPSECOND,ETC.             
         BCT   R3,ERPT8                                                         
         BAS   RE,ACRPT                                                         
         B     ERPT1                                                            
         EJECT                                                                  
***********************************************************************         
*              PUT WORKER RECORD TO ACPOST                                      
***********************************************************************         
*                                                                               
PUTIT    NTR1  ,                                                                
         AP    POSTREC,=P'1'                                                    
         LA    R2,T                                                             
         SR    R3,R3                                                            
         IC    R3,1(R2)                                                         
PUT2     AR    R2,R3                                                            
         CLI   0(R2),0                                                          
         BE    PUT4                                                             
         SR    R3,R3                                                            
         IC    R3,1(R2)                                                         
         LTR   R3,R3                                                            
         BNZ   PUT2                                                             
         MVI   0(R2),0                                                          
PUT4     LA    R2,1(R2)                                                         
         LA    R3,T-4                                                           
         SR    R2,R3                                                            
         STH   R2,T-4                                                           
         BAS   RE,ADDPOST                                                       
         CLI   QOPT1,C'D'          DUMPS                                        
         BNE   XIT                                                              
         MVC   MSG,MSG8            WORKER ADD MESSAGE                           
         BAS   RE,DUMP                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              WORKER INTERFACE                                                 
***********************************************************************         
*                                                                               
*                                                                               
OPNPOST  MVC   COMMAND,=CL6'OPEN'                                               
         B     FILE                                                             
*                                                                               
ADDPOST  MVC   COMMAND,=CL6'ADD'                                                
         B     FILE                                                             
*                                                                               
CLOSPOST MVC   COMMAND,=CL6'CLOSE'                                              
         B     FILE                                                             
*                                                                               
FILE     NTR1  ,                                                                
         CLI   RCPOSTNG,C'N'                                                    
         BE    XIT                                                              
         LA    R3,T-4                                                           
         L     R4,POSTBUFF                                                      
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ACQUIRE CORE FROM TSAR                                                 
***********************************************************************         
*                                                                               
MAIN     NTR1 ,                                                                 
         MVC   DUB,=CL8'T00A7D'    LOAD IN TSAROFF                              
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         OC    DMCB+4(4),DMCB+4    TEST LOAD WAS OK                             
         BNZ   *+6                 YES                                          
         DC    H'0'                CAN'T LOAD TSAROFF                           
         MVC   ATSAROFF,4(R1)      SAVE A(TSAROFF)                              
         L     R0,=A(TSRSIZE)      ACQUIRE ABOVE THE LINE BUFFER                
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF               TEST IF STORAGE ACQUIRED                     
         BZ    *+6                 YES                                          
         DC    H'0'                                                             
         ST    R1,ATSRBUFF         SAVE BEGINNING A(ERROR BUFFER)               
*        L     RE,ADMASTC                                                       
*        USING MASTD,RE                                                         
*        STCM  R1,15,MCUSRDMP      PRINT THE BUFFER IN A DUMP                   
*        LR    RF,R1                                                            
*        A     RF,=A(TSRSIZE)                                                   
*        STCM  RF,15,MCUSRDMP+4    END OF DUMP AREA                             
*                                                                               
         L     R1,ATSRBLK          R1 IS A(TSAR BLOCK)                          
         USING TSARD,R1                                                         
         XC    TSARD(TSARDL),TSARD CLEAR TSAR BLOCK                             
         MVC   TSACOM,ADCOMFAC         ADDR COMFACS                             
         MVC   TSABUF,ATSRBUFF         ADDR OF TSAR BUFFER                      
         MVC   TSAREC,=A(TSRSIZE)      SIZE OF BUFFER                           
         MVC   TSKEYL,=AL1(TSRKLEN)    KEY LENGTH                               
         MVC   TSRECL,=AL2(TSRRLEN)    RECORD LENGTH                            
         MVI   TSOFFACT,TSAINI         ACTION (INIT) IS HOB OF BUFFER           
         OI    TSIND2,TSI2MANY         USE FULL WORD COUNTERS                   
         GOTO1 ATSAROFF                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
*                                                                               
*                                                                               
DMRD     LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMHGH    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMSEQ    LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGETR   LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,AIO,DMWORK                         
         B     DMERR                                                            
*                                                                               
DMWRTR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,DMWRTR,ACCDIR,DIR,DIR                               
         B     DMERR                                                            
*                                                                               
DMADDR   CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,AIO2,DMWORK                        
         B     DMERR                                                            
*                                                                               
DMPUT    CLI   RCWRITE,C'N'                                                     
         BER   RE                                                               
         LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,AIO2,DMWORK                        
*                                                                               
DMERR    MVC   BYTE,8(R1)                                                       
         NI    BYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED               
         CLI   BYTE,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
*                                                                               
DUMP     CP    DMPCNT,DMPMAX       TEST MAXIMUM DUMPS                           
         BHR   RE                                                               
         AP    DMPCNT,=P'1'                                                     
DUMPER   DS    0H                                                               
         NTR1  ,                                                                
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,AIO2                                                          
         SR    R4,R4                                                            
         ICM   R4,3,ACCRLEN-ACCRECD(R3)                                         
         CLI   QOPT2,C'T'          OUTPUT TAPE                                  
         BNE   DUMP2                                                            
         CLI   DUMPSW,C'Y'         IT'S A "PUT" TO FILE/DONT ADJ LENGTH         
         BE    DUMP3                                                            
         SH    R3,=H'4'                                                         
         ICM   R4,3,0(R3)                                                       
         B     DUMP3                                                            
DUMP2    CLI   QOPT2,C'W'          WORKER FILE                                  
         BNE   DUMP3                                                            
         LA    R3,T-4                                                           
         ICM   R4,3,T-4                                                         
*                                                                               
DUMP3    LA    R5,=C'2D'                                                        
         GOTO1 ,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),(C'P',PRINT)            
         GOTO1 PRNTBL                                                           
         MVC   MSG,SPACES                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ADD AND ELEMENT TO THE RECORD    R3 = ELEMENT    R2 = RECORD        *         
* ADDL SORTS THE ELEMENTS                                             *         
* ADDL2 ADDS ELEMENTS TO END OF RECORD                                *         
***********************************************************************         
*                                                                               
ADDL     LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),(R3)                               
         LR    RE,R0                                                            
         BR    RE                                                               
ADDL2    LR    R0,RE                                                            
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),(R3),=C'ADD=END'                   
         LR    RE,R0                                                            
         BR    RE                                                               
         GETEL R3,DATADIS2,ELCODE                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
*                                                                               
MXRLNQ   EQU   2000                MAX RECORD SIZE                              
SPACE    EQU   X'40'               SPACE                                        
ALL      EQU   X'FF'                                                            
*                                                                               
DATVAL   DC    V(DATVAL)           DATVAL                                       
CASHVAL  DC    V(CASHVAL)          CASHVAL                                      
HELLO    DC    V(HELLO)            HELLO                                        
PRNTBL   DC    V(PRNTBL)           PRNTBL                                       
*                                                                               
AIO      DC    A(IO)               IO AREA                                      
AIO2     DC    A(IO2)              IO AREA 2                                    
ACHDREC  DC    A(CHDREC)           SAVED CONTRA HEADER RECORD                   
AHISREC  DC    A(HISREC)           SAVED HISTORY RECORD                         
ATRNREC  DC    A(TRNREC)           SAVED TRANSACTION RECORD                     
ATSRREC  DC    A(TSRREC)           SAVED TSAR BLOCK                             
ATSRBLK  DC    A(TSRBLK)           SAVED TSAR BLOCK                             
POSTBUFF DC    A(POSTBUF)          BUFFER FOR WORKER                            
ABTOTTAB DC    A(BTOTTAB)          TABLE FOR BATCH TOTALS                       
AERRS    DC    A(ERRS)             A(ERROR MESSAGES)                            
AINFIL   DC    A(INFIL)            A(INPUT FILE DCB)                            
AOUTFIL  DC    A(OUTFIL)           A(INPUT FILE DCB)                            
ABILDTAB DC    A(BILDTAB)          A(TABLE TO BUILD SORTREC)                    
TAPSW    DC    C'C'                OUTPUT TAPE C(LOSED) O(PENED)                
WRKSW    DC    C'C'                WORKER FILE C(LOSED) O(PENED)                
DUMPSW   DC    C'N'                SUMP SWITCH                                  
*                                                                               
ACCDIR   DC    CL8'ACCDIR  '                                                    
ACCMST   DC    CL8'ACCMST  '                                                    
GETREC   DC    CL8'GETREC  '                                                    
ADDREC   DC    CL8'ADDREC  '                                                    
PUTREC   DC    CL8'PUTREC  '                                                    
*                                                                               
*                                                                               
RECCARD  DC    C'RECORD TYPE=F,LENGTH=000 '                                     
SORTCARD DC    C'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1 '                       
*                                                                               
MSG1     DC    C'TRANS   ADD   '                                                
MSG2     DC    C'CHEADER ADD   '                                                
MSG3     DC    C'HISTORY ADD   '                                                
MSG4     DC    C'HISTORY UPDATE'                                                
MSG5     DC    C'***BAD TRAN***'                                                
MSG6     DC    C'***BAD CHED***'                                                
MSG7     DC    C'***BAD HIST***'                                                
MSG8     DC    C'**WORKER ADD**'                                                
MSG9     DC    C'BALANCE UPDATE'                                                
MSG10    DC    C'OF BAL UPDATED'                                                
DMPCNT   DC    PL2'0'              DUMP COUNT - ERRORS                          
DMPMAX   DC    PL2'50'             MAX DUMP COUNT                               
ZEROS    DC    PL8'0'                                                           
*                                                                               
CNTS     DS    0XL25               RECORD COUNTS                                
CNTIN    DC    PL5'0',CL20'RECORDS IN'                                          
CNTTRAN  DC    PL5'0',CL20'TRANSACTIONS ADDED'                                  
CNTBUCK  DC    PL5'0',CL20'BUCKETS ADDED'                                       
CNTCHED  DC    PL5'0',CL20'CHEADERS ADDED'                                      
CNTERRI  DC    PL5'0',CL20'RECS IN WITH ERRORS'                                 
CNTERRO  DC    PL5'0',CL20'RECS OUT WITH ERRORS'                                
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
*                                                                               
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
*                                                                               
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
*        MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS+(PLL-PLD),C'L'                                           
         MVI   BOXCOLS+(PLC1-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC2-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC3-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC4-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC5-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC6-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC7-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC8-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC9-PLD),C'C'                                          
         MVI   BOXCOLS+(PLC10-PLD),C'C'                                         
         MVI   BOXCOLS+(PLC11-PLD),C'C'                                         
         MVI   BOXCOLS+(PLC12-PLD),C'C'                                         
         MVI   BOXCOLS+(PLR-PLD),C'R'                                           
BX200    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
BXXIT    XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUFFERS                                                             *         
***********************************************************************         
*                                                                               
         DC    C'**IO****'                                                      
         DC    F'0'                      IOAREA #1                              
IO       DC    (MXRLNQ)X'00'                                                    
*                                                                               
         DC    C'**IO2***'                                                      
         DC    F'0'                      IOAREA #2                              
IO2      DC    (MXRLNQ)X'00'                                                    
*                                                                               
         DC    C'*CHDREC*'                                                      
         DC    F'0'                      CONTRA HEADER RECORD                   
CHDREC   DC    (MXRLNQ)X'00'                                                    
*                                                                               
         DC    C'*HISREC*'                                                      
         DC    F'0'                      HISTORY RECORD                         
HISREC   DC    (MXRLNQ)X'00'                                                    
*                                                                               
         DC    C'*TRNREC*'                                                      
         DC    F'0'                      TRANSACTION RECORD                     
TRNREC   DC    (MXRLNQ)X'00'                                                    
*                                                                               
         DC    C'*TSRREC*'                                                      
         DC    F'0'                      TSAR RECORD                            
TSRREC   DC    (TSRRLEN)X'00'                                                   
*                                                                               
         DC    C'*TSRBLK*'                                                      
         DC    F'0'                      TSAR BLOCK                             
TSRBLK   DC    (TSARDL)X'00'                                                    
*                                                                               
         DC    C'*PSTBUF*'                                                      
         DC    F'0'                      BUFFER FOR WORKER FILE                 
POSTBUF  DC    4500X'00'                                                        
*                                                                               
         DC    C'*BTOTAL*'                                                      
         DC    F'0'                      BATCH TOTAL TABLE                      
BTOTTAB  DC    (BTOTSIZE)X'00'                                                  
*                                                                               
ERRS     DS    CL20                                                             
ERRLNQ   EQU   *-ERRS                    SIZE OF ERROR ENTRY                    
         ORG   ERRS                                                             
ERRACC   EQU   1                                                                
         DC    CL(ERRLNQ)'ACCOUNT'                                              
ERRCACC  EQU   2                                                                
         DC    CL(ERRLNQ)'CONTRA ACCOUNT'                                       
ERRDATE  EQU   3                                                                
         DC    CL(ERRLNQ)'INVALID DATE'                                         
ERRWORK  EQU   4                                                                
         DC    CL(ERRLNQ)'WORKCODE'                                             
ERRREF   EQU   5                                                                
         DC    CL(ERRLNQ)'DOC NUMBER'                                           
ERRBREF  EQU   6                                                                
         DC    CL(ERRLNQ)'BATCH REF'                                            
ERRAMNT  EQU   7                                                                
         DC    CL(ERRLNQ)'AMOUNT'                                               
ERRRATE  EQU   8                                                                
         DC    CL(ERRLNQ)'RATE'                                                 
ERRHOURS EQU   9                                                                
         DC    CL(ERRLNQ)'HOURS'                                                
ERRAMNT2 EQU   10                                                               
         DC    CL(ERRLNQ)'RATE X HRS  NOT =AMT'                                 
ERRTYPE  EQU   11                                                               
         DC    CL(ERRLNQ)'TYPE OF TIME'                                         
MXERR    EQU   (*-ERRS)/ERRLNQ           MAX ERROR TABLE ENTRIES                
         SPACE 2                                                                
***********************************************************************         
* DCB'S                                                               *         
***********************************************************************         
*                                                                               
INFIL    DCB   DDNAME=INFIL,DSORG=PS,MACRF=(GM),                       X        
               RECFM=FB,LRECL=259,BLKSIZE=2590,EODAD=REQF7                      
*                                                                               
OUTFIL   DCB   DDNAME=OUTFIL,DSORG=PS,MACRF=(PM),                      X        
               RECFM=VB,LRECL=4004,BLKSIZE=32760                                
*                                                                               
         EJECT                                                                  
BILDTAB  DS    0F                                                               
*                                                                               
BILD08A  DC    CL4'IN08',X'08',AL1(BILD08N),AL2(BILD08L)                        
BILD08B  DC    Y(INFI3REF-INFD),Y(SRTKREF-SRTRECD)                              
         DC    AL1(L'INFI3REF),AL1(0)                                           
         DC    Y(INFI3DAT-INFD),Y(SRTKDATE-SRTRECD)                             
         DC    AL1(L'INFI3DAT),AL1(0)                                           
         DC    Y(INFI3CLI-INFD),Y(SRTKCLT-SRTRECD)                              
         DC    AL1(L'INFI3CLI),AL1(0)                                           
         DC    Y(INFI3PRD-INFD),Y(SRTKPRD-SRTRECD)                              
         DC    AL1(L'INFI3PRD),AL1(0)                                           
         DC    Y(INFI3JOB-INFD),Y(SRTKJOB-SRTRECD)                              
         DC    AL1(L'INFI3JOB),AL1(0)                                           
         DC    Y(INFI3WC-INFD),Y(SRTKWORK-SRTRECD)                              
         DC    AL1(L'INFI3WC),AL1(0)                                            
         DC    Y(INFI3AMT-INFD),Y(SRTRAMNT-SRTRECD)                             
         DC    AL1(L'INFI3AMT),AL1(0)                                           
         DC    Y(INFI3NAR-INFD),Y(SRTRNAR-SRTRECD)                              
         DC    AL1(L'INFI3NAR),AL1(0)                                           
BILD08N  EQU   (*-BILD08B)/BTENTRLN                # OF SUB ENTRIES             
BILD08L  EQU   (*-BILD08A)                         LENGTH                       
*                                                                               
BILD49A  DC    CL4'IN49',X'31',AL1(BILD49N),AL2(BILD49L)                        
BILD49B  DC    Y(INFI2REF-INFD),Y(SRTKREF-SRTRECD)                              
         DC    AL1(L'INFI2REF),AL1(0)                                           
         DC    Y(INFI2DAT-INFD),Y(SRTKDATE-SRTRECD)                             
         DC    AL1(L'INFI2DAT),AL1(0)                                           
         DC    Y(INFI2CLI-INFD),Y(SRTKCLT-SRTRECD)                              
         DC    AL1(L'INFI2CLI),AL1(0)                                           
         DC    Y(INFI2PRD-INFD),Y(SRTKPRD-SRTRECD)                              
         DC    AL1(L'INFI2PRD),AL1(0)                                           
         DC    Y(INFI2JOB-INFD),Y(SRTKJOB-SRTRECD)                              
         DC    AL1(L'INFI2JOB),AL1(0)                                           
         DC    Y(INFI2WC-INFD),Y(SRTKWORK-SRTRECD)                              
         DC    AL1(L'INFI2WC),AL1(0)                                            
         DC    Y(INFI2AMT-INFD),Y(SRTRAMNT-SRTRECD)                             
         DC    AL1(L'INFI2AMT),AL1(0)                                           
         DC    Y(INFI2RAT-INFD),Y(SRTRRATE-SRTRECD)                             
         DC    AL1(L'INFI2RAT),AL1(0)                                           
         DC    Y(INFI2HRS-INFD),Y(SRTRHOUR-SRTRECD)                             
         DC    AL1(L'INFI2HRS),AL1(0)                                           
         DC    Y(INFI2TIM-INFD),Y(SRTRTTIM-SRTRECD)                             
         DC    AL1(L'INFI2TIM),AL1(0)                                           
         DC    Y(INFI2NAR-INFD),Y(SRTRNAR-SRTRECD)                              
         DC    AL1(L'INFI2NAR),AL1(0)                                           
BILD49N  EQU   (*-BILD49B)/BTENTRLN                # OF SUB ENTRIES             
BILD49L  EQU   (*-BILD49A)                         LENGTH                       
*                                                                               
BILD61A  DC    CL4'IN61',X'3D',AL1(BILD61N),AL2(BILD61L)                        
BILD61B  DC    Y(INFI1REF-INFD),Y(SRTKREF-SRTRECD)                              
         DC    AL1(L'INFI1REF),AL1(0)                                           
         DC    Y(INFI1DAT-INFD),Y(SRTKDATE-SRTRECD)                             
         DC    AL1(L'INFI1DAT),AL1(0)                                           
         DC    Y(INFI1CLI-INFD),Y(SRTKCLT-SRTRECD)                              
         DC    AL1(L'INFI1CLI),AL1(0)                                           
         DC    Y(INFI1PRD-INFD),Y(SRTKPRD-SRTRECD)                              
         DC    AL1(L'INFI1PRD),AL1(0)                                           
         DC    Y(INFI1JOB-INFD),Y(SRTKJOB-SRTRECD)                              
         DC    AL1(L'INFI1JOB),AL1(0)                                           
         DC    Y(INFI1WC-INFD),Y(SRTKWORK-SRTRECD)                              
         DC    AL1(L'INFI1WC),AL1(0)                                            
         DC    Y(INFI1AMT-INFD),Y(SRTRAMNT-SRTRECD)                             
         DC    AL1(L'INFI1AMT),AL1(0)                                           
         DC    Y(INFI1NAR-INFD),Y(SRTRNAR-SRTRECD)                              
         DC    AL1(L'INFI1NAR),AL1(0)                                           
BILD61N  EQU   (*-BILD61B)/BTENTRLN                # OF SUB ENTRIES             
BILD61L  EQU   (*-BILD61A)                         LENGTH                       
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT FOR WORKING STORAGE                                           *         
***********************************************************************         
*                                                                               
ACTLD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
ATSAROFF DS    A                   ADDRESS OF TSAROFF                           
ATSRBUFF DS    A                   ADDRESS OF ACQUIRED BUFFER                   
*                                                                               
COMMAND  DS    CL8                 USED IN DATAMGR IO                           
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DIR      DS    CL64                DIRECTORY RECORD                             
DA       DS    F                                                                
*                                                                               
ELIST    DS    3A                  HELLO PARM LIST                              
ELERR    DS    0XL1                HELLO ERROR RETURN BYTE                      
ELADDR   DS    A                   HELLO ELEMENT ADDRESS (GET)                  
ELADDR2  DS    A                   HELLO ELEMENT ADDRESS (ADD)                  
*                                                                               
CMPNAME  DS    CL36                                                             
ACCKEYSV DS    CL(L'ACCKEY)        SAVE GOOD ACCOUNT                            
TRNKEYSV DS    CL(L'ACCKEY)        SAVE TRANS KEY                               
CONTRASV DS    CL15                SAVED CONTRA ACCOUNT                         
CONNAMSV DS    CL36                SAVED CONTRA NAME                            
SJACCT   DS    CL15                SAVED SJ ACCOUNT CODE                        
SJNAME   DS    CL36                SAVED SJ NAME                                
BILLPRNT DS    CL(L'PPRBILLP)      PRINT ON BILL INFO FROM JOB                  
*                                                                               
TODAY    DS    CL6                 TODAY'S DATE YYMMDD                          
TODAYP   DS    PL3                 TODAY'S DATE PACKED                          
TODAYC   DS    XL2                 TODAY'S DATE COMPRESSED FOR X'60'            
MOST     DS    CL2                 TRANSACTION MNTH OF SERICE(BAT REF)          
MOS      DS    XL2                 PACKED MONTH OF SERVICE                      
MSG      DS    CL14                                                             
S9OFFICE DS    CL2                 THE S9 POSTING OFFICE                        
ID       DS    CL16                WORKER FILE ID AREA                          
*                                                                               
ELEMENT  DS    XL255                                                            
ERRNUM   DS    X                   CURRENT ERROR NUMBER                         
ERRFLG   DS    X                   NUMBER OF ERRORS                             
ERRLST   DS    XL(MXERR)           LIST OF ERROR NUMBERS                        
*                                                                               
UPTYPE   DS    CL(L'INFHTYP)       UPDATE TYPE                                  
BATBREF  DS    CL(L'INFHBREF)      BATCH REFERENCE                              
TTYPE    DS    CL(L'TRNTYPE)       TRANSACTION TYPE                             
SUBREF   DS    H                                                                
DATADIS2 DS    H                                                                
BATBREFS DS    CL4                 BATCH REFERENCE SAVE                         
*                                                                               
WRKACM   DS    PL6                                                              
WRKABKLN EQU   *-WRKACM                                                         
         ORG   WRKACM                                                           
ACC32DR  DS    PL(WRKABKLN)        ACCOUNT DR BALANCE                           
ACC32CR  DS    PL(WRKABKLN)        ACCOUNT CR BALANCE                           
PDEBITS  DS    PL(WRKABKLN)        TOTAL DR BALANCE                             
PCREDITS DS    PL(WRKABKLN)        TOTAL CR BALANCE                             
POSTCASH DS    PL(WRKABKLN)        TOTAL DR POSTED FOR WORKER TOTALS            
POSTREC  DS    PL(WRKABKLN)        NUMBER OF RECORDS ADDED TO WORKER            
BUKDRSV  DS    PL(WRKABKLN)        TOTAL DR BUCKETS POSTED                      
BUKCRSV  DS    PL(WRKABKLN)        TOTAL CR BUCKETS POSTED                      
WRKNUM   EQU   (*-WRKACM)/WRKABKLN                                              
*                                                                               
AMOUNT   DS    PL8                                                              
RATE     DS    PL4                                                              
HOURS    DS    PL3                                                              
INREC    DS    CL(INFLNQ)          INPUT RECORD                                 
SORTREC  DS    CL(SRTRLEN)         SORT RECORD                                  
ELCODE   DS    X                   ELEMENT CODE                                 
         DS    F                                                                
T        DS    CL600               WORKER FILE BUILD AREA                       
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER THE INPUT FILE RECORD                                *         
***********************************************************************         
*                                                                               
INFD     DSECT                                                                  
INFRTYP  DS    CL4         RECORD TYPE (HDR* TLR* OR ITEM)                      
*                                                                               
*                                                                               
         ORG   INFRTYP                                                          
INFHEAD  DS    CL4         (HDR*) **HEADER RECORD******************             
INFHSYS  DS    CL1         SYSTEM (ALWAYS A)                                    
INFHTYP  DS    CL4         UPLOAD TYPE (IN61 OR IN49)                           
INFHBREF DS    CL4         BATCH REFERENCE (1BBF,2BBF,4BBF ETC)                 
INFHMOA  DS    CL4         YYMM MOA FOR POSTING                                 
INFHEFF  DS    CL6         EFFECTIVE DATE             (NOT NEEDED)              
INFHERR  DS    CL4         ERROR COUNT                (NOT NEEDED)              
         DS    CL230                                                            
         DS    CL2         CR/LF                                                
INFHLNQ  EQU   *-INFD                                                           
*                                                                               
*                                                                               
*                                                                               
         ORG   INFRTYP                                                          
INFITEM  DS    CL4         (ITEM)  **ITEM RECORD******************              
INFITRAN DS    0C          BEGINNING OF ITEM                                    
*                          * * * * *  T Y P E  6 1 * * * * * * * *              
INFI1REF DS    CL6         TRANS REFERENCE                                      
         DS    CL1         ,                          (NOT NEEDED)              
         DS    CL6         2ND TRANS REFERENCE        (NOT NEEDED)              
INFI1DAT DS    CL6         TRANS DATE YYMMDD                                    
         DS    CL1         URGENT (U OR SPACE)        (NOT NEEDED)              
         DS    CL15        CASH ACCOUNT               (NOT NEEDED)              
INFI1VEN DS    CL15        VENDOR ACCOUNT SY SV                                 
         DS    CL1         CD (Y OR SPACE)            (NOT NEEDED)              
         DS    CL15        EXPENSE VENDOR             (NOT NEEDED)              
         DS    CL1         CD (Y OR SPACE)            (NOT NEEDED)              
INFI1CLI DS    CL3         SJ CLIENT                                            
INFI1PRD DS    CL3         SJ PRODUCT                                           
INFI1JOB DS    CL6         SJ JOB                                               
         DS    CL15        SE EXPENSE ACCOUNT         (NOT NEEDED)              
INFI1WC  DS    CL2         WORKCODE (COMM)                                      
INFI1WCN DS    CL2         WORKCODE (NON-COMM)                                  
INFI1AMT DS    CL12        TRANS AMOUNT (-12345678900)                          
         DS    CL2         DR OFFICE                  (NOT NEEDED)              
         DS    CL2         CR OFFICE                  (NOT NEEDED)              
         DS    CL2         ANALYSIS OFFICE            (NOT NEEDED)              
         DS    CL3         DEPARTMENT                 (NOT NEEDED)              
         DS    CL8         PERSON                     (NOT NEEDED)              
INFI1NAR DS    CL55        NARRATIVE                                            
         DS    CL6         RESERVED????               (NOT NEEDED)              
         DS    CL6         UPLOAD DATE                (NOT NEEDED)              
         DS    CL4         UPLOAD TIME                (NOT NEEDED)              
         DS    CL55        SPARE                      (NOT NEEDED)              
         DS    CL2         CR/LF ?                    (NOT NEEDED)              
INF1LNQ  EQU   *-INFD                                                           
         ORG   INFITRAN                                                         
*                          * * * * *  T Y P E  4 9 * * * * * * * *              
INFI2ODP DS    CL4         1R OFFICE DEPT                                       
INFI2SUB DS    CL2         1R SUB DEPT                                          
INFI2PER DS    CL6         1R PERSON                                            
INFI2DAT DS    CL6         TRANS DATE YYMMDD                                    
INFI2REF DS    CL6         TRANS REFERENCE                                      
         DS    CL1         A=ADJUSTMENT M=MISSING OR SPACE   NEED?              
         DS    CL1         DUP (+ OR SPACE)                  NEED?              
INFI2TIM DS    CL1         B,R OR N                                             
INFI2HRS DS    CL6         HOURS 2.25                                           
INFI2CLI DS    CL3         SJ CLIENT                                            
INFI2PRD DS    CL3         SJ PRODUCT                                           
INFI2JOB DS    CL6         SJ JOB                                               
INFI2WC  DS    CL2         WORKCODE (TASK)                                      
INFI2TAX DS    CL1         Y OR BLANK                                           
INFI2RAT DS    CL8         RATE (-1234567)                                      
INFI2AMT DS    CL10        TRANS AMOUNT (-123456789)                            
INFI2INC DS    CL15        INCOME ACCOUNT BEGINS WITH SIXXXXXX                  
INFI2NAR DS    CL75        NARRATIVE                                            
         DS    CL6         RESERVED????               (NOT NEEDED)              
         DS    CL6         UPLOAD DATE                (NOT NEEDED)              
         DS    CL4         UPLOAD TIME                (NOT NEEDED)              
         DS    CL81        SPARE                      (NOT NEEDED)              
         DS    CL2         CR/LF ?                    (NOT NEEDED)              
INF2LNQ  EQU   *-INFD                                                           
         ORG   INFITRAN                                                         
*                          * * * * *  T Y P E  0 8 * * * * * * * *              
INFI3REF DS    CL6         TRANS REFERENCE                                      
INFI3DAT DS    CL6         TRANS DATE YYMMDD                                    
INFI3CLI DS    CL3         SJ CLIENT                                            
INFI3PRD DS    CL3         SJ PRODUCT                                           
INFI3JOB DS    CL6         SJ JOB                                               
INFI3WC  DS    CL2         WORK CODE                                            
INFI3CON DS    CL12        SI ACCOUNT                                           
         DS    CL1         COMMISSION                 (NOT NEEDED)              
INFI3AMT DS    CL10        TRANS AMOUNT (-123456789)                            
         DS    CL15        EXPENSE VENDOR             (NOT NEEDED)              
         DS    CL2         FINANCIAL OFFICE(XJOBS)    (NOT NEEDED)              
         DS    CL2         ANALYSIS OFFICE(XJOBS)     (NOT NEEDED)              
         DS    CL3         DEPARTMENT(XJOBS)          (NOT NEEDED)              
         DS    CL6         PERSON(XJOBS)              (NOT NEEDED)              
         DS    CL12        SALES TAX ACCT             (NOT NEEDED)              
         DS    CL2         SALES TAX WC               (NOT NEEDED)              
         DS    CL10        SALESTAX AMNT              (NOT NEEDED)              
         DS    CL12        SALES TAX ACCT2            (NOT NEEDED)              
         DS    CL2         SALES TAX WC2              (NOT NEEDED)              
         DS    CL10        SALESTAX AMNT2             (NOT NEEDED)              
INFI3NAR DS    CL55        NARRATIVE                                            
         DS    CL6         RESERVED????               (NOT NEEDED)              
         DS    CL6         UPLOAD DATE                (NOT NEEDED)              
         DS    CL4         UPLOAD TIME                (NOT NEEDED)              
         DS    CL57        SPARE                      (NOT NEEDED)              
         DS    CL2         CR/LF ?                    (NOT NEEDED)              
INF3LNQ  EQU   *-INFD                                                           
*                                                                               
*                                                                               
*                                                                               
         ORG   INFRTYP                                                          
INFTRAIL DS    CL4         (TLR*) **TRAILER RECORD*****************             
         DS    CL253                                                            
         DS    CL2         CR/LF                                                
INFTLNQ  EQU   *-INFD                                                           
INFLNQ   EQU   *-INFD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER SORT RECORD                                          *         
***********************************************************************         
*                                                                               
SRTRECD  DSECT                     ** SORT TRANSACTION RECORD **                
SRTKEY   DS    0X                                                               
SRTKULA  DS    0XL14                                                            
SRTKUNT  DS    CL1                 UNIT CODE                                    
SRTKLDG  DS    CL1                 LEDGER CODE                                  
SRTKACT  DS    0CL12               ACCOUNT CODE                                 
SRTKCLT  DS    CL3                 CLIENT                                       
SRTKPRD  DS    CL3                 PRODUCT                                      
SRTKJOB  DS    CL6                 JOB                                          
SRTKWORK DS    0CL2                WORK CODE OR SPACES                          
SRTKOFF  DS    CL2                 OFFICE CODE OR SPACES                        
SRTKULC  DS    0XL14                                                            
SRTKCUNT DS    CL1                 CONTRA-UNIT CODE                             
SRTKCLDG DS    CL1                 CONTRA-LEDGER CODE                           
SRTKCACT DS    CL12                CONTRA-ACCOUNT CODE                          
SRTKDATE DS    CL6                 TRANSACTION DATE YYMMDD                      
SRTKREF  DS    CL6                 TRANSACTION REFERENCE                        
SRTKLEN  EQU   *-SRTKEY                                                         
SRTRBTCH DS    CL6                                                              
         ORG   SRTRBTCH                                                         
SRTRMOS  DS    CL2                 MONTH-OF-SERVICE                             
SRTRBREF DS    CL4                 TRANSACTION BATCH REFERENCE                  
SRTRTYPE DS    XL1                 TRANSACTION TYPE                             
SRTRAMNT DS    CL12                TRANSACTION AMOUNT                           
SRTRRATE DS    CL8                 RATE                                         
SRTRHOUR DS    CL6                 HOURS                                        
SRTRTTIM DS    CL1                 TYPE OF TIME                                 
SRTRSTAT DS    XL1                 SORT TRANSACTION STATUS 1                    
SRTRDEB  EQU   TRNSDR              TRANSACTION IS DEBIT                         
SRTRURGE EQU   TRNSURG             TRANSACTION IS URGENT                        
SRTRNOCM EQU   TRNSNOCM            TRANSACTION IS NON COMMISH                   
SRTRNAR  DS    CL75                NARRATIVE                                    
SRTRLEN  EQU   *-SRTRECD                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TSAR ERROR RECORD                                    *         
***********************************************************************         
*                                                                               
TSRRECD  DSECT                     ** TSAR ERROR RECORD **                      
TSRKEY   DS    0C                                                               
TSRKBREF DS    CL4                 TRANSACTION BATCH REFERENCE                  
TSRKNUMB DS    PL5                 UNIQUE ERROR COUNTER NUMBER                  
TSRKEY2  DS    0C                                                               
TSRKULA  DS    0CL14                                                            
TSRKUNT  DS    CL1                 UNIT CODE                                    
TSRKLDG  DS    CL1                 LEDGER CODE                                  
TSRKACT  DS    0CL12               ACCOUNT CODE                                 
TSRKCLT  DS    CL3                 CLIENT                                       
TSRKPRD  DS    CL3                 PRODUCT                                      
TSRKJOB  DS    CL6                 JOB                                          
TSRKWORK DS    0CL2                WORK CODE OR SPACES                          
TSRKOFF  DS    CL2                 OFFICE CODE OR SPACES                        
TSRKULC  DS    0XL14                                                            
TSRKCUNT DS    CL1                 CONTRA-UNIT CODE                             
TSRKCLDG DS    CL1                 CONTRA-LEDGER CODE                           
TSRKCACT DS    CL12                CONTRA-ACCOUNT CODE                          
TSRKDATE DS    CL6                 TRANSACTION DATE YYMMDD                      
TSRKREF  DS    CL6                 TRANSACTION REFERENCE                        
TSRKLEN  EQU   *-TSRKEY                                                         
TSRRBTCH DS    CL6                 BATCH REFERENCE                              
         ORG   TSRRBTCH                                                         
TSRRMOS  DS    CL2                 MONTH-OF-SERVICE                             
TSRRBREF DS    CL4                 TRANSACTION BATCH REFERENCE                  
TSRRTYPE DS    XL1                 TRANSACTION TYPE                             
TSRRAMNT DS    CL12                TRANSACTION AMOUNT                           
TSRRRATE DS    CL8                 RATE                                         
TSRRHOUR DS    CL6                 HOURS                                        
TSRRTTIM DS    CL1                 TYPE OF TIME                                 
TSRRSTAT DS    XL1                 SORT TRANSACTION STATUS 1                    
TSRRDEB  EQU   X'80'               DEBIT                                        
TSRRURGE EQU   X'40'               URGENT                                       
TSRRNOCM EQU   X'20'               TRANSACTION IS NON COMMISH                   
TSRRLEN2 EQU   *-TSRKEY2                                                        
TSRRERNM DS    XL(L'ERRFLG)        NUMBER OF ERROR ENTRIES                      
TSRRERTB DS    XL(MXERR)           ERROR TABLE                                  
TSRRLEN  EQU   *-TSRRECD                                                        
TSRMAX   EQU   30000                                                            
TSRSIZE  EQU   TSRMAX*TSRRLEN                                                   
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER BATCH REFERENCE TOTAL ACCUMS                         *         
***********************************************************************         
*                                                                               
BTOTD    DSECT                                                                  
BTOTBTCH DS    CL6                                                              
         ORG   BTOTBTCH                                                         
BTOTMOS  DS    CL2                 MONTH-OF-SERVICE                             
BTOTBREF DS    CL4                 TRANSACTION BATCH REFERENCE                  
BTOTSTAT DS    XL1                 STATUS                                       
BTOTUSED EQU   X'80'               AMOUNT HAS BEEN USED                         
BTOTAMNT DS    PL6                                                              
BTOTLEN  EQU   *-BTOTD                                                          
BTOTMAX  EQU   15                                                               
BTOTSIZE EQU   BTOTMAX*BTOTLEN                                                  
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER COMMON TRANSACTION DISPLACEMENTS                     *         
***********************************************************************         
*                                                                               
BTABD    DSECT                                                                  
BTYPE    DS    CL4                 TRANSACTION TYPE ON TAPE IE "IN61"           
BTYPCODE DS    XL1                 TRANSACTION TYPE FOR TRANSACTION             
BTYPNUM  DS    AL1                 NUMBER OF MINI DISP ENTRIES                  
BTYPLEN  DS    AL2                 LENGTH OF ENTIRE TABLE ENTRY                 
BTYPKLN  EQU   *-BTABD                                                          
BTENTRY  DS    0C                                                               
BTFROM   DS    Y                   DISP FROM TAPE RECORD                        
BTTO     DS    Y                   DISP TO SORT RECORD                          
BTFROMLN DS    AL1                 LENGTH OF FROM FIELD                         
         DS    AL1                 ALIGNMENT                                    
BTENTRLN EQU   *-BTENTRY                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER REPORT PRINT LINE                                    *         
***********************************************************************         
*                                                                               
PLD      DSECT                                                                  
PLL      DS    X                                                                
PLWORK   DS    CL2                 WORKCODE                                     
PLC1     DS    X                                                                
PLCACC   DS    CL14                ACCT OR CONTRA U/L ACCOUNT CODE              
PLC2     DS    X                                                                
PLNME    DS    CL36                NAME                                         
         ORG   PLNME                                                            
PLCACC2  DS    CL14                CONTRA U/L ACCOUNT CODE(FOR ERROR)           
         ORG   PLNME+L'PLNME                                                    
PLC3     DS    X                                                                
PLDATE   DS    CL6                 YYMMDD                                       
PLC4     DS    X                                                                
PLREF    DS    CL6                 REFERENCE                                    
PLC5     DS    X                                                                
PLSEQ    DS    CL2                 SEQUENCE                                     
PLC6     DS    X                                                                
PLBREF   DS    CL6                 BATCH REFERENCE                              
PLC7     DS    X                                                                
PLBTYPE  DS    CL2                 BATCH TYPE                                   
PLC8     DS    X                                                                
PLTTYPE  DS    CL2                 TYPE OF TIME                                 
PLC9     DS    X                                                                
PLRATE   DS    CL10                RATE                                         
PLC10    DS    X                                                                
PLHOURS  DS    CL10                HOURS                                        
PLC11    DS    X                                                                
PLDEBIT  DS    CL14                DEBIT                                        
PLC12    DS    X                                                                
PLCREDIT DS    CL14                CREDIT                                       
PLERR    DS    CL20                                                             
         ORG   PLD+163                                                          
PLR      DS    X                                                                
         EJECT                                                                  
         DCBD  DSORG=PS,DEVD=DA                                                 
* ACBIGPRNTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDTSARD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACREPTL02 08/16/00'                                      
         END                                                                    
