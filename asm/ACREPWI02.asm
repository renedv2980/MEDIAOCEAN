*          DATA SET ACREPWI02  AT LEVEL 018 AS OF 01/03/13                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 041321.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
****** WARNING: DEIS SAYS THIS LOAD MODULE IS GETTING TOO LARGE *****           
*                                                                               
* AS OF JAN/2013, THIS PROGRAM PRODUCES THE LARGEST LOAD MODULE ON              
* 'DDS.LOADLIB' BY A VERY SIGNIFICANT MARGIN. IF THE LOAD MODULE GETS           
* MUCH LARGER, IT WILL BE UNPROMOTABLE VIA PANAPT, BECAUSE IT WON'T BE          
* LOADABLE INTO LOW CORE TO VALIDATE THE LEVEL STAMP. THE ONLY REASON           
* THE LOAD MODULE IS SO LARGE IS THAT IT CONTAINS ENORMOUS STATIC               
* STORAGE AREAS THAT COULD EASILY BE ACQUIRED VIA GETMAIN MACRO CALLS.          
* IF THIS MODULE IS DEAD, THEN THERE IS NO REASON TO CHANGE IT. BUT IF          
* IT IS BEING MODIFIED FOR PRODUCTION, THEN PLEASE CONSIDER DEALING             
* WITH THIS ISSUE!                                                              
*                                                                               
***********************************************************************         
*             QOPT1:' '= FORMAT   - DATE RANGE MUST BE SPECIFIED      *         
*                   '1'= FORMAT 1 - DAILY (TODAY ONLY)                *         
*                   '2'= FORMAT 2 - WEEKLY MONDAY THROUGH SUNDAY      *         
*                   '3'= FORMAT 3 - YTD (DATE RANGE MAY BE SPECIFIED) *         
*                   '4'= FORMAT 4 - MOA (CLIENT LEVEL)                *         
*                   '5'= FORMAT 5 - MOA (OFFICE SUMMARY)              *         
*             QOPT2:'Y'= DOWNLOAD                                     *         
*             QOPT3:'Y'= PRINT CLIENT DETAIL (W/O CODES)              *         
*                   'A'= PRINT CLIENT DETAILS AND CODES               *         
*             QOPT4:'Y'= A/P BREAKOUT BY LEDGER                       *         
*             QOPT5:'Y'= RANK BY NET CASH POSITION                    *         
*             QOPT6:'Y'= SR ACCOUNT BREAKDOWN                         *         
*                   '1'= RUN ON SINGLE, SPECIFIED LEDGER              *         
*                   'A'= BOTH SINGLE LEDGER/SR ACCOUNT BREAKDOWN      *         
*             QOPT7:'Y'= RUN PRINTABLE                                *         
*                   'C'= LIMIT SR LDGR TO THOSE ACTS THAT START W/C   *         
*                   'E'= EXCLUDE NON SJ CLIENT CODES                  *         
*                   'A'= LIMIT SR LEDGER AND EXCLUDE NON SJ CLI CODES *         
*             QOPT8:   = WIGROUP RECORDS LEDGER (NEED FOR READING)    *         
*                                                                     *         
*              QSEL:   = U/L TO RUN INDIVIDUALLY                      *         
*             QAPPL:   = GROUP CODE                                   *         
***********************************************************************         
*PHASE ACWI02A                                                                  
*INCLUDE DLFLD                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE XSORT                                                                  
         TITLE 'WORKING INVESTMENT REPORT'                                      
ACWI02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACWI**,R7,R9    BASE REGISTERS 11,9                          
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA = A(GLOBAL W/S)                           
         LA    RC,SPACEND                                                       
         USING ACWID,RC            RC = A(SAVE W/S)                             
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQUEST FIRST                                
         BE    REQF                                                             
         CLI   MODE,LEDGFRST       LEDGER FIRST                                 
         BE    LDGF                                                             
         CLI   MODE,PROCLEVA       PROCESS LEVEL A                              
         BE    PLEVA                                                            
         CLI   MODE,PROCACC        PROCESS ACCOUNT                              
         BE    PACC                                                             
         CLI   MODE,PROCTRNS       PROCESS TRANSACTIONS                         
         BE    PTRN                                                             
         CLI   MODE,REQLAST        REQUEST LAST                                 
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST                                                           *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
*                                                                               
         L     R2,ABOXRC           SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,ABXHOOK                                                       
         ST    R2,HEADHOOK                                                      
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY)                                      
         GOTO1 AINIT,DMCB,(RC)     INITIALIZE STORAGE                           
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         LA    R1,PKFLDS           R1 = A(PACKED FIELDS)                        
         LA    R0,PKFLDLNQ         R0 = # OF PACKED FIELDS                      
         ZAP   0(L'PKFLDS,R1),=P'0'                                             
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         TM    FLAG,FLGREQ         IS THIS AN ACTUAL REQF?                      
         BO    REQFX               NO EXIT                                      
*                                                                               
         CLC   ALPHAID,=C'YP'      ARE WE RUNNING SUDLER AND HENNESEY?          
         BNE   *+8                                                              
         MVI   QFILTER5,X'86'      EXCLUDE FILTER5=F ACCOUNTS                   
*                                                                               
         MVC   SVCPYNM,SPACES                                                   
         USING CPYTBD,R2                                                        
         L     R2,ACPYTAB          CUTOFF DATE TABLE                            
         LA    R0,CPYTABQ                                                       
REQF05   CLC   CPYID,ALPHAID                                                    
         BNE   *+14                                                             
         MVC   SVCPYNM,CPYNME      SAVE OFF COMPANY NAME                        
         B     *+12                                                             
         LA    R2,CPYLNQ(R2)                                                    
         BCT   R0,REQF05                                                        
*                                                                               
* READ FOR LEDGER AND UPDATE LEDGER TABLE                                       
*                                                                               
         USING LDGRECD,R2                                                       
         USING LDGTBD,R3                                                        
         L     R3,ALDGTAB          R3=A(LEDGER TABLE)                           
         LA    R0,LDGTABQ          # OF ENTRIES IN TABLE                        
REQF10   XC    SVKEY,SVKEY                                                      
         LA    R2,SVKEY                                                         
         MVC   LDGKCPY,RCCOMPFL    COMPANY CODE                                 
         MVI   LDGKUNT,C'S'        ALWAYS READ UNIT S                           
         MVC   LDGKLDG,LDGLDG      MOVE IN CURRENT LEDGER                       
         GOTO1 =A(DMHIGHDR),DMCB,(RC)       READ HIGH                           
         CLC   SVKEY(3),IOKEY      SAME KEY??                                   
         BNE   *+8                                                              
         OI    LDGST,LDGACT        SHOW LEDGER IS ACTIVE                        
         LA    R3,LDGTBLNQ(R3)                                                  
         BCT   R0,REQF10                                                        
         DROP  R2,R3                                                            
*                                                                               
         CLI   QOPT2,C'Y'          NO BOXES FOR DOWNLOADING                     
         BNE   REQF20                                                           
         MVI   RCSUBPRG,9          SET PRINT RTE AS INVALID                     
         GOTO1 ADWNL,DMCB,(RC),DWNINIT      INITIALIZE DOWNLOAD RTE             
*                                                                               
REQF20   BAS   RE,BLDGRP                    BUILD GROUP TABLE                   
*                                                                               
         USING CPYRECD,RF                                                       
         L     RF,ADCOMP           ADDRESS OF COMPANY RECORD                    
         LA    RE,ACCORFST(RF)     BUMP TO ELEMENT                              
         USING CPYELD,RE                                                        
REQF30   CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),CPYELQ        X'10' COMPANY ELEMNT                         
         BE    REQF40                                                           
         SR    R1,R1                                                            
         IC    R1,CPYLN                                                         
         AR    RF,R1                                                            
         B     REQF30                                                           
*                                                                               
REQF40   TM    CPYSTAT4,CPYSOFF2   TEST FOR NEW OFFICE                          
         BZ    *+8                                                              
         OI    FLAG,FLGNUOFF       TURN ON NEW OFFICE FLAG                      
*                                                                               
         MVC   REQOFF,QOFFICE      SAVE REQUEST OFFICE - IF ANY                 
         MVC   REQUL,QSELECT       SAVE REQUEST U/L    - IF ANY                 
         MVC   HDLNFRM,QOPT1                                                    
         MVI   HDLNFRM+2,C':'                                                   
*                                                                               
         CLI   QOPT1,C' '          ANY FORMAT SPECIFIED                         
         BE    *+12                                                             
         CLI   QOPT1,C'3'          YTD FORMAT                                   
         BNE   REQF70                                                           
         MVC   HDLNDSC,=CL16'YEAR TO DATE'                                      
         CLC   QSTART(L'QSTART+L'QEND),SPACES                                   
         BE    REQF60                                                           
         CLC   QSTART,SPACES                                                    
         BE    REQF50                                                           
         CLC   QSTART+4(2),SPACES  IF NO DAY GIVEN START AT 1ST                 
         BNE   REQF130                                                          
         MVC   QSTART+4(2),=C'01'                                               
         B     REQF130                                                          
REQF50   MVC   QSTART,QEND         FORCE TO YTD FOR BEGINNING OF END YR         
         MVC   QSTART+2(4),=C'0101'                                             
         B     REQF130                                                          
*                                                                               
REQF60   MVC   QSTART,TODAY        FORCE TO YTD                                 
         MVC   QSTART+2(4),=C'0101'                                             
         MVC   QEND,TODAY                                                       
         B     REQF130                                                          
*                                                                               
REQF70   CLI   QOPT1,C'1'          DAILY FORMAT                                 
         BNE   REQF80                                                           
         MVC   HDLNDSC,=CL16'DAILY'                                             
         MVC   QSTART,TODAY        FORCE TO DAILY                               
         MVC   QEND,TODAY                                                       
         B     REQF130                                                          
*                                                                               
REQF80   CLI   QOPT1,C'2'          WEEKLY FORMAT                                
         BNE   REQF90                                                           
         MVC   HDLNDSC,=CL16'WEEKLY'                                            
         GOTO1 GETDAY,DMCB,TODAY,WKDAY                                          
         SR    R2,R2                                                            
         IC    R2,0(R1)            DAY NUMBER RETURNED BY GETDAY CALL           
         LA    R0,7                SUNDAY IS NUMBERED 7                         
         SR    R0,R2               DIFFERENCE IS NUMBER TO ADD TO TODAY         
         GOTO1 ADDAY,DMCB,TODAY,WORK,(R0)                                       
         LA    R0,6                START DATE WILL BE 6 DAYS PREVIOUS           
         LNR   R0,R0                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         MVC   QSTART,WORK+6       SET START DATE                               
         MVC   QEND,WORK           SET END   DATE                               
         B     REQF130                                                          
*                                                                               
REQF90   CLI   QOPT1,C'4'          FOR MOA SET DATE FOR HEADINGS                
         BNE   *+14                                                             
         MVC   HDLNDSC,=CL16'MOA BY CLIENT'                                     
         B     *+18                                                             
         CLI   QOPT1,C'5'                                                       
         BNE   REQF130                                                          
         MVC   HDLNDSC,=CL16'MOA BY OFFICE'                                     
*                                                                               
         CLC   QMOSSTRT(L'QMOSSTRT+L'QMOSEND),SPACES                            
         BE    REQF160             CLEARED OUT ON ORIGINAL ENTRY                
*                                                                               
         BAS   RE,BLDOFF           BUILD SJ CLIENT OFF TAB FIRST                
*                                                                               
         USING ACMD,R1                                                          
         L     R1,AMONACC                                                       
         MVC   MOASTR,ACMMSTR      SAVE OFF MOA START/END DATES                 
         MVC   MOAEND,ACMMEND                                                   
         XC    ACMMSTR,ACMMSTR     CLEAR START DATE                             
         MVC   ACMMEND,=X'FFFF'    SET END DATE - INFINITY                      
         DROP  R1                                                               
*                                                                               
         CLC   QMOSSTRT,QMOSEND                                                 
         BNE   REQF100                                                          
*                                                                               
         MVC   WORK(4),QMOSEND     MOA START DATE                               
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(14,WORK+6)                                 
         MVC   HDLINE2(16),=C'FOR THE MONTH OF'                                 
         MVC   HDLINE2+17(6),WORK+6                                             
         B     REQF110                                                          
*                                                                               
REQF100  CLC   QMOSEND,SPACES                                                   
         BH    *+10                                                             
         MVC   QMOSEND,TODAY       FORCE TO TODAY                               
         MVC   WORK(4),QMOSEND     MOA END DATE                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(14,WORK+6)                                 
         MVC   HDLINE2+12(4),=C'THRU'                                           
         MVC   HDLINE2+17(6),WORK+6                                             
*                                                                               
         CLC   QMOSSTRT,SPACES     ANY START MOA?                               
         BH    *+14                                                             
         MVC   HEDSTDTE,SPACES     IF NO START MOA-NO COLUMN START DATE         
         B     REQF120                                                          
*                                                                               
         MVC   WORK(4),QMOSSTRT    MOA START DATE                               
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(14,WORK+6)                                 
         MVC   HDLINE2(4),=C'FROM'                                              
         MVC   HDLINE2+5(6),WORK+6                                              
*                                                                               
REQF110  MVC   WORK(4),QMOSSTRT    SET UP OPENING AND ENDING DATES              
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(5,HEDSTDTE)                                
REQF120  MVC   WORK(4),QMOSEND     MOA END DATE                                 
         MVC   WORK+4(2),=C'01'                                                 
         LA    R0,32               NUMBER OF DAYS TO BUMP                       
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         MVC   WORK(4),WORK+6                                                   
         MVC   WORK+4(2),=C'01'                                                 
         LA    R0,1                                                             
         LNR   R0,R0                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(5,HEDENDTE)                              
         B     REQF160                                                          
*                                                                               
REQF130  GOTO1 DATCON,DMCB,(0,QSTART),(2,STDTE)                                 
         GOTO1 DATCON,DMCB,(0,QSTART),(5,HEDSTDTE)                              
*                                                                               
         MVC   ENDTE,=X'FFFF'                      END DATE IS INFINITY         
         GOTO1 DATCON,DMCB,(0,TODAY),(5,HEDENDTE)         NO END DATE           
         CLC   QEND,SPACES                                                      
         BE    REQF150                                                          
         CLC   QEND+4(2),SPACES    IF NO DAY GIVEN FIX THRU ADDAY               
         BNE   REQF140                PUT IN LAST DAY OF MONTH                  
         MVC   WORK(6),QEND                                                     
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 ADDAY,DMCB,(C'M',WORK),(X'80',WORK+6),0                          
         MVC   QEND,WORK+6                                                      
REQF140  GOTO1 DATCON,DMCB,(0,QEND),(2,ENDTE)                                   
         GOTO1 DATCON,DMCB,(0,QEND),(5,HEDENDTE)                                
*                                                                               
REQF150  MVC   HDLINE2(L'HEDSTDTE),HEDSTDTE                                     
         MVC   HDLINE2+9(4),=C'THRU'                                            
         MVC   HDLINE2+14(L'HEDENDTE),HEDENDTE                                  
*                                                                               
REQF160  MVC   QMOSSTRT(L'QMOSSTRT+L'QMOSEND),SPACES     DON'T FILTER           
*                                                                               
         MVC   QSTART(L'QSTART+L'QEND),SPACES                                   
*                                                                               
         CLI   QOPT4,C'Y'                                                       
         BNE   *+10                                                             
         MVC   HDLINE3,=CL25'A/P BREAKDOWN'                                     
*                                                                               
         CLC   QACCOUNT,SPACES     IF ACCOUNT SPECIFIED                         
         BNH   *+8                                                              
         OI    FLAG,FLGACCT        SET FLAG TO FILTER ON THAT ACCOUNT           
*                                                                               
         MVC   SVACCT,QACCOUNT                                                  
*                                                                               
         MVC   QOFFICE,SPACES      SPACE OUT OFFICE - IF ANY                    
         MVC   QSELECT,SPACES      SPACE OUT SELECT FIELD - IF ANY              
*                                                                               
REQFX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LEDGER FIRST                                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGRECD,R2                                                       
LDGF     DS    0H                                                               
         L     R2,ADLEDGER         R2=A(LEDGER RECORD)                          
         MVC   SVLDG,LDGKLDG       SAVE LEDGER FOR NEXT RUN                     
*                                                                               
* FIND OUT AT WHAT LEVEL I SHOULD USE SJ OFFICE NAMES                           
*                                                                               
LDGF20   XC    OFFPOS,OFFPOS       ONLY DONE FOE SJ LEDGER                      
         XC    CLIPOS,CLIPOS       ONLY USED FOR SR LEDGER                      
         BAS   RE,GETLEVS                                                       
         CLC   LDGKUNT(2),=C'SJ'   ONLY DO THE FOLLOWING FOR SJ/SR              
         BE    *+14                                                             
         CLC   LDGKUNT(2),=C'SR'                                                
         BNE   LDGFX                                                            
*                                                                               
         USING LDGELD,R2                                                        
         L     R2,ADLDGEL          ADDRESS LEDGER ELEMENT                       
         MVC   OFFPOS,LDGOPOS      LOCATION OF OFFICE                           
         MVC   CLIPOS,LDGCPOS      DISPLACEMENT TO CLIENT IN ACCT               
*                                                                               
LDGFX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS LEVEL A                                                     *         
***********************************************************************         
         SPACE 1                                                                
PLEVA    DS    0H                                                               
         MVC   SVCLI,SPACES        CLEAR SAVED AREA FOR CLIENT CODE             
         MVC   SVCLINM,SPACES      CLEAR SAVED AREA FOR CLIENT NAME             
         MVC   SVOFF,SPACES                                                     
         USING ACTRECD,R2                                                       
         L     R2,ADHEIRA          R2=A(LEVAL A ACCOUNT RECORD)                 
         CLC   ACTKUNT(2),=C'SJ'   ONLY RUN FOR SJ                              
         BNE   PLEVAX                                                           
*                                                                               
         CLC   LSTCLI,ACTKACT      SAME CLIENT CODE AS BEFORE?                  
         BE    PLEVAX              IF IT IS SKIP PUTTING TO BIN TABLE           
*                                                                               
         USING CLID,R3                                                          
         LA    R3,CLIWRK                                                        
         MVC   CLIWRK,SPACES                                                    
*                                                                               
         MVC   SVCLI,ACTKACT       SAVE CLIENT CODE                             
         USING NAMELD,R2                                                        
         L     R2,ADLVANAM         R2=A(CLIENT NAME)                            
         CLI   NAMEL,NAMELQ        X'20' - ELEMENT                              
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVCLINM(0),NAMEREC  SAVE CLIENT NAME                             
*                                                                               
         TM    OFFPOS,LDGOPROF     OFFICE IN PRODUCTION PROFILE????             
         BNO   PLEVA10             IF NOT - DONT ADD OFFICE                     
*                                                                               
         USING PPRELD,R2                                                        
         L     R2,ADLVASUP         R2=A(PRODUCTION PROFILE ELEMENT)             
         MVC   SVOFF,PPRGAOFF      SAVE LEVA OFFICE                             
         MVC   CLIOFF,SVOFF        PUT LEVA OFFICE INTO TABLE                   
*                                                                               
PLEVA10  MVC   LSTCLI,SVCLI        SAVE CLIENT CODE FOR ABOVE COMPARE           
         MVC   CLICDE,SVCLI        PUT CLIENT CODE INTO TABLE                   
         MVC   CLINME,SVCLINM      PUT CLIENT NAME INTO TABLE                   
         GOTO1 ABINADD,DMCB,(RC),CLIWRK,ACLITAB       ADD TABLE ENTRY           
*                                                                               
PLEVAX   B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT                                                     *         
***********************************************************************         
         SPACE 1                                                                
PACC     DS    0H                                                               
         MVC   SVTRNKEY,SPACES                                                  
         MVI   FCRDTRNS,C'Y'       READ TRANSACTIONS                            
         BAS   RE,SETCDE           SET LEVEL CODES                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
         USING TRNELD,R4                                                        
PTRN     DS    0H                                                               
         L     R4,ADTRANS          R5=A(TRANSACTION ELEMENT)                    
         LR    R2,R4                                                            
         SH    R2,DATADISP         R2=A(TRANSACTION RECORD)                     
*                                                                               
         USING NCPD,R3                                                          
         LA    R3,NCPWRK           CLIENT TABLE ENTRY WORK AREA                 
         XC    NCPWRK,NCPWRK       CLEAR BINTABLE WORK AREA                     
         MVC   0(NCPKLNQ,R3),SPACES      CLEAR 1ST FIELDS TO SPACES             
         MVC   NCPCLTNM,SPACES     CLEAR CLIENT NAME TO SPACES                  
         MVC   NCPOFFNM,SPACES     CLEAR GROUP NAME TO SPACES                   
*                                                                               
         LA    R0,NCPBKCT          3 OF BUCKETS                                 
         LA    R1,NCPBKT           ZAP BUCKETS                                  
         ZAP   0(L'NCPBKT,R1),=P'0'                                             
         LA    R1,L'NCPBKT(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
* FIND CLIENT CODE IN EACH RECORD                                               
* CLIENT CODE IN SR LEDGER IS FOUND IN AT LEAST ONE OF THE FOLLOWING:           
* X'4F' - CLI/PRO/JOB ELEM - THE X'1A' - MEDIA TRANSFER ELEM OR THE KEY         
* LEVEL C OF KEY                                                                
*                                                                               
         CLC   TRNKUNT(2),=C'SR'   LOOK FOR MEDIA TRANSFER ELEMENT              
         BNE   PTRN30                                                           
*                                                                               
         CLI   QOPT7,C'C'          DO THEY WANT TO LIMIT SR LEDGER?             
         BE    *+12                YES - LIMIT IT                               
         CLI   QOPT7,C'A'          C OR A WILL LIMIT SR LEDGER!                 
         BNE   *+12                NO  -  SKIP IT                               
         CLI   TRNKACT,C'C'        YES -  ONLY RUN ON 1ST LEVEL C               
         BNE   PTRNX                                                            
*                                                                               
         CLC   SVTRNKEY,TRNKEY     IF CRD-IF KEY IS SAME AS PREV DEB            
         BE    *+10                                                             
         MVC   LSTCLI,SPACES                                                    
*                                                                               
         LR    R5,R4               R5=A(TRANSACTION ELEMENT)                    
         MVI   ELCODE,MDTELQ       X'1A' - MEDIA TRANSFER ELEMENT               
         BAS   RE,NEXTEL                                                        
         BNE   *+12                                                             
         USING MDTELD,R5                                                        
         LA    R1,MDTCLI           POINT R1 AT CLIENT CODE                      
         B     PTRN20                                                           
*                                                                               
         LR    R5,R4               R5=A(TRANSACTION ELEMENT)                    
         MVI   ELCODE,CPJELQ       X'4F' - CLI/PRO/JOB ELEMENT                  
         BAS   RE,NEXTEL                                                        
         BNE   PTRN10              IF NOT THERE - CHECK IF KEYS MATCH           
*                                                                               
         USING CPJELD,R5                                                        
         CLI   CPJTYPE,CPJTJOB     PRODUCTION JOB?                              
         BNE   PTRN10                                                           
         LA    R1,CPJCLI           POINT R1 AT CLIENT CODE                      
         B     PTRN20                                                           
*                                                                               
* IF NOT FOUND IN EITHER ELEMENT CHECK IF KEY MATCHES LAST KEY                  
*    IF IT MATCHES, USE PREVIOUS KEY.  OTHERWISE CHECK 4TH LEVEL                
*    IF IT MATCHES A CLIENT IN CLITAB.  IF IT DOES, USE IT ELSE USE             
*    WHATEVER POSITION THE LEDGER IS SET UP FOR.  (CLIPOS)                      
*                                                                               
PTRN10   CLC   LSTCLI,SPACES       NOT THE SAME KEY CLEAR LSTCLI                
         BNH   *+20                                                             
         MVC   SVCLI,LSTCLI                                                     
         MVC   SVTRNKEY,TRNKEY     SAVE KEY FOR COMPARE IF CRDS NEXT            
         B     PTRN120                                                          
*                                                                               
         LA    R1,LEVDCDE          FIRST TRY 4TH LEVEL                          
         MVC   SVCLI,LEVDCDE                                                    
         BAS   RE,GETCLI                                                        
         BNE   *+14                NOT FOUND USE 3RD LEVEL                      
         CLC   SVCLINM,=CL36'*NOT AN SJ CODE*'   IF THIS MATCHES                
         BNE   PTRN20                            USE 3RD LEVEL                  
*                                                                               
         LA    R1,TRNKACT          POINT R1 TO BEGINNING OF ACCOUNT             
         SR    R0,R0                                                            
         IC    R0,CLIPOS           R0=DISPLACEMENT INTO ACCT FOR CLI            
         AR    R1,R0                                                            
*                                                                               
PTRN20   MVC   SVCLI,0(R1)         CLIENT CODE - FROM C/P/J OR MEDIA            
         MVC   SVTRNKEY,TRNKEY     SAVE KEY FOR COMPARE IF CRDS NEXT            
         CLC   LSTCLI,SPACES       IF LSTCLI IS BLANK - UPDATE IT               
         BH    PTRN120                                                          
         MVC   LSTCLI,SVCLI                                                     
         B     PTRN120                                                          
*                                                                               
* CLIENT CODE IN SJ LEDGER RECORDS ARE FOUND IN LEVA OF ACCT                    
*                                                                               
PTRN30   CLC   TRNKUNT(2),=C'SJ'   FOR SJ CLI CODE AND NAME FROM LEVA           
         BNE   PTRN50                                                           
         CLC   ALPHAID,=C'WW'      EXCLUDE TYPE 45 FOR WW                       
         BNE   *+12                                                             
         CLI   TRNTYPE,45                                                       
         BE    PTRNX                                                            
*                                                                               
         CLI   QOPT6,C'1'          RUN ON SINGLE LEDGER?                        
         BE    *+12                                                             
         CLI   QOPT6,C'A'          RUN ON SINGLE LEDGER/AR ACCOUNT              
         BNE   PTRN140                                                          
         CLC   REQUL,SPACES        ANY LEDGER OTHER THAN SJ SELCTED?            
         BE    PTRN140                                                          
         CLC   REQUL,=C'SJ'        WAS SJ SELECTED?                             
         BE    PTRN140                                                          
         CLI   REQUL,C'S'          VALIDATE UNIT                                
         BNE   PTRN140             IF NOT VALID IGNORE - REGULAR RUN            
*                                                                               
         USING LDGTBD,R1                                                        
         LA    R0,LDGTABQ          R0=# OF ENTRIES IN LDGTAB                    
         L     R1,ALDGTAB          R1=A(LEDGER TABLE)                           
PTRN40   CLC   REQUL+1(1),LDGLDG                                                
         BNE   *+12                                                             
         MVI   FCRDTRNS,C'N'       SJ LEDGER NOT REQUESTED DONT READ            
         B     PTRNX                                                            
         LA    R1,LDGTBLNQ(R1)                                                  
         BCT   R0,PTRN40                                                        
         B     PTRN140             NO MATCH - REGULAR SINGLE SJ RUN             
         DROP  R1                                                               
*                                                                               
* FOR PAYABLES LEDGERS CHECK CONTRA U/U, THEN CONTRA ACCT THEN ELMS.            
*     IF CONTR U/L IS SJ TAKE FROM 1ST LEVEL OF CONTRA ACCOUNT. ELSE,           
*     CLIENT CODE IN SS,SP,SU LEDGER RECORDS ARE FOUND IN THE LAST 3            
*     3 POSITIONS OF THE CONTRA ACCOUNT.  ON THE SZ/SV LEDGER - IT IS           
*     FOUND IN THE FIRST 3 BYTES.  IF THESE LEAD TO SPACES CHECK ELEMS          
*                                                                               
PTRN50   DS    0H                                                               
         CLC   TRNKUNT(2),=C'SV'   IF LEDGER SV EXCLUDE ACCTS THAT              
         BNE   PTRN60              BEGIN WITH I                                 
         CLC   ALPHAID,=C'YE'      FOR YF/YE                                    
         BNE   PTRN60                                                           
         CLI   TRNKACT,C'I'                                                     
         BE    PTRNX                                                            
*                                                                               
PTRN60   LA    R1,TRNKCACT         CLIENT CODE FOUND IN CONTRA ACCOUNT          
         CLC   TRNKCUNT(2),=C'SJ'  IF CONTRA IS SJ CLIENT IN FIRST 3            
         BE    PTRN70                                                           
         CLC   TRNKUNT(2),=C'SZ'   SZ CLI FOUND AT START OF CONTRA              
         BE    PTRN70                                                           
         CLC   TRNKUNT(2),=C'SV'   SV CLI FOUND AT START OF CONTRA              
         BE    PTRN70                                                           
         LA    R1,TRNKCACT+9       FOR ALL OTHER AP LDG CLI AT END              
*                                                                               
PTRN70   CLC   TRNKUNT(2),=C'SZ'   LIMIT SZ ACCOUNT TO TABLE ENTRIES            
         BNE   PTRN100                                                          
*                                                                               
         USING SZTABD,R6                                                        
         L     R6,ASZTAB           SZ ACCOUNT TABLE                             
PTRN80   CLI   0(R6),EOF           NO ENTRY FOUND                               
         BE    PTRNX                                                            
         CLC   ALPHAID,SZID        MATCH ON ALPHAID                             
         BNE   *+14                                                             
         CLC   TRNKACT,SZACCT                                                   
         BE    *+12                                                             
         LA    R6,SZTABLNQ(R6)                                                  
         B     PTRN80                                                           
*                                                                               
         TM    SZSTAT,SZTRN        INCLUDE ALL TRANSACTIONS                     
         BO    PTRN100                                                          
         DROP  R6                                                               
*                                                                               
         L     R6,ATRNTAB          SZ TRNS TYPE TABLE                           
PTRN90   CLI   0(R6),EOF           NO ENTRY FOUND                               
         BE    PTRNX                                                            
         CLC   TRNTYPE,0(R6)                                                    
         BE    PTRN100                                                          
         LA    R6,L'TRNTAB(R6)                                                  
         B     PTRN90                                                           
*                                                                               
PTRN100  DS    0H                                                               
         CLC   0(3,R1),SPACES      COULD BE A TYPE 45                           
         BNE   PTRN110                                                          
*                                                                               
         LR    R5,R4               R5=A(TRANSACTION ELEMENT)                    
         MVI   ELCODE,MDTELQ       X'1A' - MEDIA TRANSFER ELEMENT               
         BAS   RE,NEXTEL                                                        
         BNE   *+12                                                             
         USING MDTELD,R5                                                        
         LA    R1,MDTCLI           POINT R1 AT CLIENT CODE                      
         B     PTRN110                                                          
*                                                                               
         LR    R5,R4               R5=A(TRANSACTION ELEMENT)                    
         MVI   ELCODE,CPJELQ       X'4F' - CLI/PRO/JOB ELEMENT                  
         BAS   RE,NEXTEL                                                        
         BNE   PTRN110             IF NOT THERE - LEAVE AS SPACES               
*                                                                               
         USING CPJELD,R5                                                        
         CLI   CPJTYPE,CPJTJOB     PRODUCTION JOB?                              
         BNE   PTRN110                                                          
         LA    R1,CPJCLI           POINT R1 AT CLIENT CODE                      
*                                                                               
PTRN110  MVC   SVCLI,0(R1)                                                      
*                                                                               
PTRN120  CLC   SVCLI,SPACES        IF NO CLIENT CODE - EXIT                     
         BH    PTRN130                                                          
         TM    FLAG,FLGACCT        FILTERING ON CLIENT CODE?                    
         BO    PTRNX               YES - SKIP IT                                
         MVC   SVOFF,=X'FFFFFF'    HEX OUT OFFICE AS WELL                       
         MVC   SVCLINM,SPACES      CLEAR NAME FIELD                             
         B     PTRN140                                                          
*                                                                               
PTRN130  DS    0H                                                               
         BAS   RE,GETCLI           GET CLI NAME FROM TABLE (NOT SJ)             
         BNE   PTRNX                                                            
*                                                                               
PTRN140  DS    0H                                                               
         CLI   QOPT7,C'E'          THEY WANT TO EXCLUDE NON SJ CODES?           
         BE    *+12                YES - TEST  IT                               
         CLI   QOPT7,C'A'          E OR A WILL EXCLUDE SJ CODES                 
         BNE   PTRN150                                                          
         TM    FLAG,FLGCLI         DO WE HAVE A CLIENT?                         
         BO    PTRNX               NO - EXCLUDE IT                              
         CLC   SVCLI,SPACES                                                     
         BNH   PTRNX               EXCLUDE ANYTHING WITHOUT CLIENTS             
*                                                                               
PTRN150  CLC   REQOFF,SPACES       WAS THERE AN OFFICE SELECTED?                
         BE    *+14                                                             
         CLC   REQOFF,SVOFF        SAME OFFICE?                                 
         BNE   PTRNX               NO - SKIP IT                                 
*                                                                               
         BAS   RE,GETGRP           GET GRP FROM TABLE                           
         CLC   QAPPL,SPACES        ANY GROUP SPECIFIED?                         
         BE    *+14                                                             
         CLC   SVGRP,QAPPL         SAME GROUP?                                  
         BNE   PTRNX               NO - DON'T ADD TO TABLE                      
*                                                                               
         MVC   NCPGRP,SVGRP        MOVE IN SAVED GROUP CODE                     
         MVC   NCPCATNM,SVCATNM    MOVE IN SAVED CATEGORY CODE                  
         MVC   NCPCLI,SVCLI        MOVE IN SAVED CLIENT CODE                    
         MVC   NCPDIVNM,SVDIVNM    MOVE IN SAVED DIVISION CODE                  
         MVC   NCPCLTNM,SVCLINM    MOVE IN SAVED CLIENT CODE                    
         OC    NCPCATNM,NCPCATNM   IF NO GROUP CODE/NAME FOUND                  
         BNZ   *+10                                                             
         MVC   NCPCATNM,SVCLINM    MOVE IN SAVED CLIENT CODE INSTEAD            
         MVC   NCPUL,TRNKUNT       SAVE U/L FOR FUTURE BREAKDOWN                
         MVC   NCPOFF,SVOFF        MOVE IN SAVED CLIENT OFFICE                  
         MVI   NCPTYP,NCPTCLT      CLIENT DETAIL RECORD                         
*                                                                               
         BAS   RE,GETOFF           GET OFFICE NAME                              
         MVC   NCPOFFNM,SVOFFNM    MOVE IN SAVED CLIENT OFFICE NAME             
*                                                                               
         CLI   QOPT1,C'4'          IF EITHER MOA OPTIONS ARE CHOSEN             
         BE    *+12                DON'T BOTHER READING X'60' ELM               
         CLI   QOPT1,C'5'                                                       
         BNE   PTRN160                                                          
*                                                                               
         USING ACMD,R1                                                          
         L     R1,AMONACC                                                       
         CLC   ACMMDTE,MOAEND      END DATE - WITHIN RANGE                      
         BH    PTRNX                                                            
         CLC   ACMMDTE,MOASTR      START DATE/ACTIVITY DATE                     
         BL    PTRN170                                                          
         B     PTRN180                                                          
         DROP  R1                                                               
*                                                                               
         USING TRSELD,R5                                                        
PTRN160  LR    R5,R4               R5=A(TRANSACTION ELEMENT)                    
         MVI   ELCODE,TRSELQ       X'60' - TRANSACTION STATUS ELEMENT           
         BAS   RE,NEXTEL                                                        
         BNE   PTRNX                                                            
*                                                                               
         CLC   TRSDATE,ENDTE       END DATE - WITHIN RANGE                      
         BH    PTRNX                                                            
         CLC   TRSDATE,STDTE       START DATE/ACTIVITY DATE                     
         BNL   PTRN180                                                          
*                                                                               
PTRN170  LA    R1,NCPOBDEB         OPENING BALANCE DEBIT                        
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R1,NCPOBCRD         OPENING BALANCE CREDIT                       
         ZAP   0(L'NCPBKT,R1),TRNAMNT                                           
         B     PTRN200                                                          
*                                                                               
         USING LDGTBD,R1                                                        
PTRN180  LA    R0,LDGTABQ          R0=# OF ENTRIES IN TABLE                     
         L     R1,ALDGTAB          R3=A(LEDGER TABLE)                           
         CLC   TRNKLDG,LDGLDG      FIND THE CORRECT LEDGER                      
         BE    PTRN190                                                          
         LA    R1,LDGTBLNQ(R1)                                                  
         BCT   R0,*-14                                                          
         DC    H'0'                LEDGER  MUST BE IN TABLE                     
*                                                                               
PTRN190  SR    R0,R0                                                            
         IC    R0,LDGDISP          DISP TO BUCKETS                              
         LA    R1,NCPBKT           R3=A(BEGINNING OF BUCKET AREA)               
         AR    R1,R0               R1=A(ACCUMULATOR DEBIT BUCKET)               
         DROP  R1                                                               
*                                                                               
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R1,L'NCPBKT(R1)     R1=A(ACCUMULATOR CREDIT BUCKET)              
         ZAP   0(L'NCPBKT,R1),TRNAMNT                                           
*                                                                               
         CLI   QOPT6,C'Y'          DO THEY WANT AN A/R TABLE                    
         BE    *+12                                                             
         CLI   QOPT6,C'A'          DO THEY WANT AN A/R TABLE                    
         BNE   PTRN200             NO - DON'T BUILD TABLE                       
*                                                                               
         CLC   TRNKUNT(2),=C'SR'                                                
         BNE   *+8                                                              
         BAS   RE,BLDSRA           BUILD SR ACCOUNT TABLE                       
*                                                                               
PTRN200  DS    0H                                                               
         XC    NCPREC,NCPREC       CLEAR RECORD TYPE                            
         OC    NCPCAT,NCPCAT       DO WE HAVE A CATEGORY RECORD?                
         BZ    *+8                 NO-JUST ADD RECORD AS IS                     
         OI    NCPREC,NCPRGRP      SHOW GROUP RECORD STATUS                     
         GOTO1 ABINADD,DMCB,(RC),NCPWRK,ANCPTAB                                 
*                                                                               
         OC    NCPGRP,NCPGRP       WAS THERE ANY GROUP?                         
         BZ    PTRNX                                                            
         BAS   RE,ADDGRP           YES-GET HIGHER LEVELS FROM TABLE             
*                                                                               
PTRNX    B     EXIT                                                             
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
**********************************************************************          
* REQUEST LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
         OI    FLAG,FLGREQ         SET FLAG FOR EACH REQUEST                    
         CLI   QOPT6,C'1'          ONE LEDGER SPECIFIED                         
         BE    *+12                                                             
         CLI   QOPT6,C'A'          RUN ON 1 LEDGER AND AR ACCT BREAK            
         BNE   REQL40              NO-REGULAR RUN GET NEXT LEDGER               
         CLC   REQUL,SPACES        ANY LEDGER SPECIFIED                         
         BE    REQL30              NO-DO AS A NORMAL SINGLE LEDGER RUN          
         CLC   REQUL,=C'SJ'        WAS SJ SPECIFIED?                            
         BE    REQL30              YES-DO AS A NORMAL SINGLE LEDGER RUN         
         CLI   REQUL,C'S'          VALIDATE UNIT                                
         BNE   REQL30              IF NOT VALID IGNORE                          
         MVC   SVLDG,REQUL+1       VALIDATE LEDGER IS IN TABLE?                 
*                                                                               
         USING LDGTBD,R1                                                        
         LA    R0,LDGTABQ          R0=# OF ENTRIES IN LDGTAB                    
         L     R1,ALDGTAB          R1=A(LEDGER TABLE)                           
REQL10   CLC   SVLDG,LDGLDG                                                     
         BNE   REQL20                                                           
         TM    LDGST,LDGACT        IS LEDGER ACTIVE??                           
         BNO   REQL30              NO-SKIP IT                                   
         MVC   QLEDGER,SVLDG       UPDATE LEDGER                                
         MVI   RCREQREP,C'N'       DONT PRINT ADDITIONAL REQUEST PAGES          
         B     REQL110                                                          
REQL20   LA    R1,LDGTBLNQ(R1)                                                  
         BCT   R0,REQL10                                                        
         DROP  R1                                                               
*                                                                               
REQL30   MVC   REQUL,SPACES        LEDGER NOT VALID SPACE OUT/IGNORE            
         SR    R0,R0                                                            
         B     REQL60                                                           
*                                                                               
         USING LDGTBD,R2                                                        
REQL40   LA    R0,LDGTABQ          R0=# OF ENTRIES IN LDGTAB                    
         L     R2,ALDGTAB          R1=A(LEDGER TABLE)                           
REQL50   CLC   SVLDG,LDGLDG                                                     
         BE    REQL60              NO-SKIP IT                                   
         LA    R2,LDGTBLNQ(R2)                                                  
         BCT   R0,REQL50                                                        
         DC    H'0'                LEDGER MUST BE IN TABLE                      
*                                                                               
REQL60   CH    R0,=H'1'            IF NOT GREATER THAN 1                        
         BH    REQL90              LAST LEDGER IN TABLE ALREADY DONE            
         MVI   RCREQREP,C'Y'       PRINT NEXT REQUEST PAGE                      
         CLI   QOPT5,C'Y'                                                       
         BNE   REQL65                                                           
         GOTO1 AGETRNK,DMCB,(RC)   FIGURE OUT NCP FOR RANKING                   
REQL65   BAS   RE,XSRT             SORT BY NET AND OFFICE                       
         CLI   QOPT1,C'5'          BREAKDOWN BY OFFICE                          
         BE    REQL70                                                           
         BAS   RE,PRNT             PRINT OUT REPORT                             
         NI    DWNSTAT,ALL-DWNHDLN                                              
         CLI   QOPT4,C'Y'          BREAKDOWN A/P BY LEDGER                      
         BNE   *+12                                                             
         BAS   RE,APPRNT           PRINT A/P BY LEDGER                          
         NI    DWNSTAT,ALL-DWNHDLN                                              
         CLI   QOPT6,C'Y'          BREAKDOWN A/R BY ACCOUNT                     
         BE    *+12                                                             
         CLI   QOPT6,C'A'          RUN ON 1 LEDGER AND AR ACCT BREAK            
         BNE   *+8                                                              
         BAS   RE,ARPRNT           PRINT A/R BY ACCOUNT                         
         B     REQL80              INITIALIZE AND EXIT                          
*                                                                               
REQL70   BAS   RE,PRNOFF           PRINT OUT REPORT - BY OFFICE                 
         NI    DWNSTAT,ALL-DWNHDLN                                              
         CLI   QOPT4,C'Y'          BREAKDOWN A/P BY LEDGER                      
         BNE   *+8                                                              
         BAS   RE,APPROFF          PRINT A/P BY LEDGER - BY OFFICE              
REQL80   GOTO1 AINIT,DMCB,(RC)     INITIALIZE STORAGE FOR EACH REQ              
         B     REQLX               AND EXIT                                     
*                                                                               
REQL90   LA    R2,LDGTBLNQ(R2)     BUMP TO NEXT LEDGER                          
         BCTR  R0,0                                                             
         TM    LDGST,LDGACT        IS LEDGER ACTIVE??                           
         BO    REQL100             NO-SKIP IT                                   
         LA    R2,LDGTBLNQ(R2)                                                  
         BCT   R0,*-12                                                          
         B     REQL60                                                           
*                                                                               
REQL100  MVI   RCREQREP,C'N'       DONT PRINT ADDITIONAL REQUEST PAGES          
         MVC   QLEDGER,LDGLDG      PUT THE NEXT LEDGER INTO REQUEST             
         MVC   SVLDG,LDGLDG        SAVE LEDGER FOR LATER USE                    
         DROP  R2                                                               
*                                                                               
REQL110  MVC   REQUL,SPACES        LEDGER NOT VALID SPACE OUT/IGNORE            
         MVC   QFILTER5,SPACES     DO NOT FILTER BEYOND SJ                      
         MVC   QACCOUNT,SPACES     DO NOT FILTER ON ACCT AFTER SJ               
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         MVI   ACMMODE,REQFRST     SIGNAL CONTROLLER TO RESET REQFRST           
*                                                                               
REQLX    B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT INFORMATION FROM TABLE                                       *          
**********************************************************************          
         SPACE 1                                                                
PRNT     NTR1                                                                   
         MVI   RCSUBPRG,4          REGULAR P/O W/O CODES                        
         CLI   QOPT3,C'A'          PRINT ALL INCLUDING CODES                    
         BNE   *+8                                                              
         MVI   RCSUBPRG,0          REGULAR RUN W/ CODES                         
*                                                                               
         CLI   QOPT2,C'Y'          NO BOXES FOR DOWNLOADING                     
         BNE   *+12                                                             
         MVI   RCSUBPRG,9          SET PRINT RTE AS INVALID                     
         MVI   DWNHDOPT,DWNRG      SET DOWNLOAD OPT TO SHOW REG RUN             
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
         USING BIND,R1                                                          
         L     R1,ANCPTAB          R1=A(CLIENT TABLE)                           
         ICM   R0,15,BININ                                                      
         BZ    PRNTX                                                            
         USING NCPD,R2                                                          
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         USING PLINED,R4                                                        
         BAS   RE,CLRALL           ZERO OUT ALL ACCUMULATORS                    
         XC    LSTGCO,LSTGCO       CLEAR SAVED ARE FOR PREVIOUS GCO             
         MVC   XP,XSPACES                                                       
         NI    FLAG,ALL-FLGPRT     TURN OFF PRINTED FLAG                        
         NI    FLAG,ALL-FLGGRP     GROUP DONE - TURN OFF FLAG                   
*                                                                               
PRNT10   DS    0H                                                               
         CLC   NCPCAT,LSTCAT                                                    
         BE    PRNT30              CHECK IF DIVISION CHANGE?                    
         OC    LSTCAT,LSTCAT       1ST TIME THROUGH?                            
         BZ    PRNT15                                                           
         GOTO1 TOTAL,DMCB,PKCLTS,PKDOS,NCPTCLT      DO CLIENT   TOTALS          
         GOTO1 TOTAL,DMCB,PKDOS,PKCATS,NCPTDIV      DO DIVISION TOTALS          
         GOTO1 TOTAL,DMCB,PKCATS,PKTOTS,NCPTCAT     DO CATEGORY TOTALS          
         NI    FLAG,ALL-FLGGRP     GROUP DONE - TURN OFF FLAG                   
*                                                                               
PRNT15   CLI   NCPTYP,NCPTCAT      IS THIS A CATEGORY RECORD?                   
         BNE   PRNT30                                                           
         OC    LSTCAT,LSTCAT       ANYTHING IN CATEGORY PREVIOUSLY?             
         BNZ   PRNT20                                                           
         OC    LSTCLI,LSTCLI       WAS THERE A CLIENT PREVIOUSLY?               
         BZ    PRNT20              IF NOT DON'T PRINT CLIENT TOTALS             
         GOTO1 TOTAL,DMCB,PKCLTS,PKTOTS,NCPTCLT    CLI TOTS ADD TO RUN          
         TM    FLAG,FLGPRT         WAS SOMETHING PRINTED?                       
         BNO   PRNT20              NO DON'T SKIP LINE                           
         GOTO1 ACREPORT                                                         
         NI    FLAG,ALL-FLGPRT     TURN OFF PRINTED FLAG                        
*                                                                               
PRNT20   OI    FLAG,FLGGRP         THERE IS A GROUP - SET FLAG                  
         LA    R4,SVPRNT1          R4=A(SAVED AREA FOR CATEGORY)                
         MVC   SVPRNT1,SPACES                                                   
         CLI   QOPT3,C'A'          PRINT ALL INCLUDING CODES                    
         BNE   *+10                                                             
         MVC   PCAT,NCPCAT         CATEGORY CODE                                
         MVC   PCATNM,NCPCATNM     CATEGORY NAME                                
         B     PRNT80                                                           
*                                                                               
PRNT30   CLC   NCPDIV,LSTDIV       SAME DIVISION AS BEFORE?                     
         BE    PRNT50                                                           
         OC    LSTDIV,LSTDIV       1ST TIME THROUGH?                            
         BZ    PRNT40                                                           
         GOTO1 TOTAL,DMCB,PKCLTS,PKDOS,NCPTCLT      DO CLIENT   TOTALS          
         GOTO1 TOTAL,DMCB,PKDOS,PKCATS,NCPTDIV      DO DIVISION TOTALS          
         TM    FLAG,FLGPRT         WAS SOMETHING PRINTED?                       
         BNO   PRNT40              NO DON'T SKIP LINE                           
         GOTO1 ACREPORT                                                         
         NI    FLAG,ALL-FLGPRT     TURN OFF PRINTED FLAG                        
*                                                                               
PRNT40   CLI   NCPTYP,NCPTDIV      IS THIS A DIVISION RECORD?                   
         BNE   PRNT50                                                           
         LA    R4,SVPRNT2                                                       
         MVC   SVPRNT2,SPACES                                                   
         CLI   QOPT3,C'A'          PRINT ALL INCLUDING CODES                    
         BNE   *+10                                                             
         MVC   PDIV,NCPDIV         DIVISION CODE                                
         MVC   PDIVNM,NCPDIVNM     DIVISION NAME                                
         B     PRNT80                                                           
*                                                                               
PRNT50   CLC   NCPCLI,LSTCLI       SAME CLIENT AS BEFORE?                       
         BE    PRNT70                                                           
         OC    LSTCLI,LSTCLI       1ST TIME THROUGH?                            
         BZ    PRNT70                                                           
         TM    FLAG,FLGGRP         WAS THERE A GRP ASSOCIATED W/CLI             
         BNO   PRNT60              NO - DO CLI TOTS AND ADD TO RUN              
         GOTO1 TOTAL,DMCB,PKCLTS,PKDOS,NCPTCLT     CLI TOTS ADD TO DIV          
         B     PRNT70                                                           
*                                                                               
PRNT60   MVC   SVPRNT1(L'SVPRNT1+L'SVPRNT2),SPACES                              
         GOTO1 TOTAL,DMCB,PKCLTS,PKTOTS,NCPTCLT    CLI TOTS ADD TO RUN          
         TM    FLAG,FLGPRT         WAS SOMETHING PRINTED?                       
         BNO   PRNT70              NO DON'T SKIP LINE                           
         GOTO1 ACREPORT                                                         
         NI    FLAG,ALL-FLGPRT     TURN OFF PRINTED FLAG                        
*                                                                               
PRNT70   ZAP   PKAMT,NCPOBCRD      OPENING BALANCE - CREDIT                     
         SP    PKAMT,NCPOBDEB      SUBTRACT DEBITS                              
         MP    PKAMT,=P'-1'                                                     
         AP    PKOB,PKAMT                                                       
         ZAP   PKNET,PKAMT         BEGINNING FIGURE FOR NET CP                  
*                                                                               
         ZAP   PKAMT,NCPARDEB      A/R - DEBITS                                 
         SP    PKAMT,NCPARCRD      SUBTRACT CREDITS                             
         AP    PKARCOL,NCPARCRD    A/R - CREDT                                  
         AP    PKARBIL,NCPARDEB    A/R DEBITS                                   
         AP    PKARNET,PKAMT       ADD NET A/R AMOUNT TO TOTAL                  
         AP    PKNET,PKAMT         SUBTRACT TO GET NET CP                       
*                                                                               
         ZAP   PKAMT,NCPAPCRD      A/P - CREDT                                  
         SP    PKAMT,NCPAPDEB      SUBTRACT DEBITS                              
         AP    PKAP,PKAMT          ADD AMOUNT TO CLIENT TOTAL                   
         SP    PKNET,PKAMT         ADD TO GET NET CP                            
*                                                                               
         ZAP   PKAMT,NCPPIDEB      PRODUCT INVENTORY - DEBITS                   
         SP    PKAMT,NCPPICRD      SUBTRACT CREDITS                             
         AP    PKPI,PKAMT          ADD AMOUNT TO CLIENT TOTAL                   
         AP    PKNET,PKAMT         SUBTRACT TO GET NET CP                       
         AP    PKNTCSH,PKNET       ADD TO CLIENT NET TOTAL                      
*                                                                               
PRNT80   MVC   LSTGCO,NCPGRP       LAST GROUP/CLIENT                            
         MVC   LSTCATNM,NCPCATNM   LAST CATEGORY NAME FOR DWNLD                 
         MVC   LSTDIVNM,NCPDIVNM   LAST DIVISION NAME FOR DWNLD                 
         MVC   LSTCLINM,NCPCLTNM   SAVE CLIENT NAME FOR PRINTING                
         LA    R2,NCPLNQ(R2)                                                    
         BCT   R0,PRNT10                                                        
*                                                                               
         TM    FLAG,FLGGRP         WAS THERE A GRP ASSOCIATED W/CLI             
         BNO   PRNT90              NO - DO CLI TOTS AND ADD TO RUN              
*                                                                               
         GOTO1 TOTAL,DMCB,PKCLTS,PKDOS,NCPTCLT      DO CLIENT   TOTALS          
         GOTO1 TOTAL,DMCB,PKDOS,PKCATS,NCPTDIV      DO DIVISION TOTALS          
         GOTO1 TOTAL,DMCB,PKCATS,PKTOTS,NCPTCAT     DO CATEGORY TOTALS          
         B     PRNT100                                                          
*                                                                               
PRNT90   MVC   SVPRNT1(L'SVPRNT1+L'SVPRNT2),SPACES                              
         GOTO1 TOTAL,DMCB,PKCLTS,PKTOTS,NCPTCLT    CLI TOTS ADD TO RUN          
PRNT100  GOTO1 TOTAL,DMCB,PKTOTS,0,0                DO RUN      TOTALS          
*                                                                               
PRNTX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT BREAKDOWN OF PAYABLES BY LEDGER                              *          
**********************************************************************          
         SPACE 1                                                                
APPRNT   NTR1                                                                   
         MVI   RCSUBPRG,5          A/P BREAKDOWN W/O CODES                      
         CLI   QOPT3,C'A'          PRINT ALL INCLUDING CODES                    
         BNE   *+8                                                              
         MVI   RCSUBPRG,1          A/P BREAKDOWN W/ CODES                       
*                                                                               
         CLI   QOPT2,C'Y'          NO BOXES FOR DOWNLOADING                     
         BNE   *+12                                                             
         MVI   RCSUBPRG,9          SET PRINT RTE AS INVALID                     
         MVI   DWNHDOPT,DWNAP      SET DOWNLOAD OPT TO SHOW A/P RUN             
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
         USING BIND,R1                                                          
         L     R1,ANCPTAB          R2=A(CLIENT TABLE)                           
         ICM   R0,15,BININ                                                      
         BZ    APPX                                                             
         USING NCPD,R2                                                          
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
* RE-SORT TABLE BY ORIGINAL KEY (IN CASE PREVIOUS OPTIONS SORTED TABLE)         
*                                                                               
         LA    R3,NCPLNQ           R3 = LENGTH OF RECORD                        
         LA    R5,NCPKLNQ          R5 = LENGTH OF FIELD TO XSORT                
         LA    R6,0                R6 = DISPLACEMENT TO FIELD                   
         GOTO1 =V(XSORT),DMCB,(0,(R2)),(R0),(R3),(R5),(R6)                      
*                                                                               
         USING PLINED,R4                                                        
         LA    R4,SVPRNT1          R4=A(PRINT LINE)                             
         MVC   SVPRNT1(L'SVPRNT1+L'SVPRNT2),SPACES                              
         MVC   XP,XSPACES                                                       
         OI    FLAG,FLGAP          SET FLAG FOR AP BREAKDOWN                    
         BAS   RE,CLRALL           ZERO OUT ALL ACCUMULATORS                    
         XC    LSTGCO,LSTGCO       CLEAR SAVED AREA FOR GRP/CLI/OFF             
         NI    FLAG,ALL-FLGPRT     TURN OFF PRINT FLAG                          
         NI    FLAG,ALL-FLGGRP     GROUP DONE - TURN OFF FLAG                   
*                                                                               
APP10    CLC   NCPCAT,LSTCAT       SAME CATEGORY AS BEFORE?                     
         BE    APP30                                                            
         OC    LSTCAT,LSTCAT       1ST TIME THROUGH?                            
         BZ    APP15                                                            
         GOTO1 TOTAL,DMCB,PKCLTS,PKDOS,NCPTCLT      DO CLIENT   TOTALS          
         GOTO1 TOTAL,DMCB,PKDOS,PKCATS,NCPTDIV      DO DIVISION TOTALS          
         GOTO1 TOTAL,DMCB,PKCATS,PKTOTS,NCPTCAT     DO CATEGORY TOTALS          
         NI    FLAG,ALL-FLGGRP     GROUP DONE - TURN OFF FLAG                   
*                                                                               
APP15    CLI   NCPTYP,NCPTCAT      IS THIS A CATEGORY RECORD?                   
         BNE   APP30                                                            
         OC    LSTCAT,LSTCAT       ANYTHING IN CATEGORY PREVIOUSLY?             
         BNZ   APP20                                                            
         OC    LSTCLI,LSTCLI       WAS THERE A CLIENT PREVIOUSLY?               
         BZ    APP20               IF NOT DON'T PRINT TOTALS                    
         GOTO1 TOTAL,DMCB,PKCLTS,PKTOTS,NCPTCLT    CLI TOTS ADD TO RUN          
         TM    FLAG,FLGPRT         WAS SOMETHING PRINTED?                       
         BNO   APP20               NO DON'T SKIP LINE                           
         GOTO1 ACREPORT                                                         
         NI    FLAG,ALL-FLGPRT     TURN OFF PRINTED FLAG                        
*                                                                               
APP20    OI    FLAG,FLGGRP         THERE IS A GROUP - SET FLAG                  
         LA    R4,SVPRNT1          R4=A(SAVED AREA FOR CATEGORY)                
         MVC   SVPRNT1,SPACES                                                   
         CLI   QOPT3,C'A'          PRINT ALL INCLUDING CODES                    
         BNE   *+10                                                             
         MVC   PCAT,NCPCAT         CATEGORY CODE                                
         MVC   PCATNM,NCPCATNM     CATEGORY NAME                                
         B     APP100                                                           
*                                                                               
APP30    CLC   NCPDIV,LSTDIV       SAME DIVISION AS BEFORE?                     
         BE    APP50                                                            
         OC    LSTDIV,LSTDIV       1ST TIME THROUGH?                            
         BZ    APP40                                                            
         GOTO1 TOTAL,DMCB,PKCLTS,PKDOS,NCPTCLT      DO CLIENT   TOTALS          
         GOTO1 TOTAL,DMCB,PKDOS,PKCATS,NCPTDIV      DO DIVISION TOTALS          
         TM    FLAG,FLGPRT         WAS SOMETHING PRINTED?                       
         BNO   APP40               NO DON'T SKIP LINE                           
         GOTO1 ACREPORT                                                         
         NI    FLAG,ALL-FLGPRT     TURN OFF PRINTED FLAG                        
*                                                                               
APP40    CLI   NCPTYP,NCPTDIV      IS THIS A DIVISION RECORD?                   
         BNE   APP50                                                            
         LA    R4,SVPRNT2                                                       
         MVC   SVPRNT2,SPACES                                                   
         CLI   QOPT3,C'A'          PRINT ALL INCLUDING CODES                    
         BNE   *+10                                                             
         MVC   PDIV,NCPDIV         DIVISION CODE                                
         MVC   PDIVNM,NCPDIVNM     DIVISION NAME                                
         B     APP100                                                           
*                                                                               
APP50    CLC   NCPCLI,LSTCLI       SAME CLIENT AS BEFORE?                       
         BE    APP70                                                            
         OC    LSTCLI,LSTCLI       1ST TIME THROUGH?                            
         BZ    APP70                                                            
         TM    FLAG,FLGGRP         WAS THERE A GRP ASSOCIATED W/CLI             
         BNO   APP60               NO - DO CLI TOTS AND ADD TO RUN              
         GOTO1 TOTAL,DMCB,PKCLTS,PKDOS,NCPTCLT     CLI TOTS ADD TO DIV          
         B     APP70                                                            
*                                                                               
APP60    MVC   SVPRNT1(L'SVPRNT1+L'SVPRNT2),SPACES                              
         GOTO1 TOTAL,DMCB,PKCLTS,PKTOTS,NCPTCLT    CLI TOTS ADD TO RUN          
         TM    FLAG,FLGPRT         WAS SOMETHING PRINTED?                       
         BNO   APP70               NO DON'T SKIP LINE                           
         GOTO1 ACREPORT                                                         
         NI    FLAG,ALL-FLGPRT     TURN OFF PRINTED FLAG                        
*                                                                               
APP70    L     R1,APAYTAB          R1=A(PAYABLE LEDGERS)                        
         LA    R3,PAYTABQ          R3=# OF PAYABLE LEDGERS                      
*                                                                               
APP80    CLC   NCPUL,0(R1)         TRY AND MATCH ON UNIT/LEDGERS                
         BE    APP90                                                            
         LA    R1,PAYDLNQ(R1)                                                   
         BCT   R3,APP80                                                         
         B     APP100              BUMP AND LOOP                                
*                                                                               
         USING PAYD,R1                                                          
APP90    MVC   SVPK,PAYPK          SAVE DISPLACEMENT IN PK STORAGE              
         DROP  R1                                                               
*                                                                               
         ZAP   PKAMT,NCPAPCRD      A/P - CREDT                                  
         SP    PKAMT,NCPAPDEB      SUBTRACT DEBITS                              
*                                                                               
         LA    R3,PKCLTS             R3=A(PACKED STORAGE LOCATION)              
         SR    R1,R1                                                            
         IC    R1,SVPK               R1=OFFSET INTO ACCUMULATOR                 
         AR    R3,R1                 BUMP                                       
         AP    0(L'PKFLDS,R3),PKAMT  ADD AMOUNT TO STORAGE                      
*                                                                               
APP100   MVC   LSTGCO,NCPGRP       SAVE OFF LAST GRP/CLI/OFF                    
         MVC   LSTCATNM,NCPCATNM   LAST CATEGORY NAME FOR DWNLD                 
         MVC   LSTDIVNM,NCPDIVNM   LAST DIVISION NAME FOR DWNLD                 
         MVC   LSTCLINM,NCPCLTNM   CLIENT NAME FOR PRINTING                     
         LA    R2,NCPLNQ(R2)                                                    
         BCT   R0,APP10                                                         
*                                                                               
APP110   TM    FLAG,FLGGRP         WAS THERE A GRP ASSOCIATED W/CLI             
         BNO   APP120              NO - DO CLI TOTS AND ADD TO RUN              
*                                                                               
         GOTO1 TOTAL,DMCB,PKCLTS,PKDOS,NCPTCLT      DO CLIENT   TOTALS          
         GOTO1 TOTAL,DMCB,PKDOS,PKCATS,NCPTDIV      DO DIVISION TOTALS          
         GOTO1 TOTAL,DMCB,PKCATS,PKTOTS,NCPTCAT     DO CATEGORY TOTALS          
         B     APP130                                                           
*                                                                               
APP120   MVC   SVPRNT1(L'SVPRNT1+L'SVPRNT2),SPACES                              
         GOTO1 TOTAL,DMCB,PKCLTS,PKTOTS,NCPTCLT    CLI TOTS ADD TO RUN          
APP130   GOTO1 TOTAL,DMCB,PKTOTS,0,0                DO RUN      TOTALS          
*                                                                               
APPX     B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* PRINT BREAKDOWN OF RECEIVABLES BY ACCOUNT                          *          
**********************************************************************          
         SPACE 1                                                                
ARPRNT   NTR1                                                                   
         MVI   RCSUBPRG,8          A/R BREAKDOWN BY ACCOUNT                     
         CLI   QOPT2,C'Y'          NO BOXES FOR DOWNLOADING                     
         BNE   *+12                                                             
         MVI   RCSUBPRG,9          SET PRINT RTE AS INVALID                     
         MVI   DWNHDOPT,DWNAR      SET DOWNLOAD OPT TO SHOW A/R RUN             
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVC   HDLINE3,=CL25'A/R BREAKDOWN BY ACCOUNT'                          
*                                                                               
         USING BIND,R1                                                          
         L     R1,ASRATAB          R2=A(SR ACCOUNT TABLE)                       
         ICM   R0,15,BININ                                                      
         BZ    ARPX                                                             
         USING SRAD,R2                                                          
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         USING PLINED,R4                                                        
         LA    R4,XP               R4=A(PRINT LINE)                             
         MVC   XP,XSPACES                                                       
         MVI   BYTE,ALL            SET BYTE TO X'FF' FOR CURRENT                
         BAS   RE,CLRALL           ZERO OUT ALL ACCUMULATORS                    
         ZAP   PKSRCNT,=P'0'                                                    
*                                                                               
         XC    LSTCLI,LSTCLI       CLEAR SAVED AREA FOR GRP/CLI/OFF             
         OI    FLAG,FLGAR          TURN ON BIT TO SAY RUNNING AR                
         MVC   PSRCLI,SRACLI       MOVE CLIENT CODE TO PRINT LINE               
         MVC   PSRCLINM,SRACLINM   MOVE CLIENT NAME TO PRINT LINE               
         B     ARP30                                                            
*                                                                               
ARP10    CLC   SRACLI,LSTCLI       SAME CLIENT AS BEFORE?                       
         BE    ARP30                                                            
         CP    PKSRCNT,=P'1'       IF ONLY ONE ACCOUNT NO TOTS                  
         BH    ARP20                                                            
*                                                                               
         LA    R1,PKLNQ            CLEAR CLIENT TOTALS                          
         LA    R3,PKCLTS                                                        
         ZAP   0(L'PKCLTS,R3),=P'0'                                             
         LA    R3,L'PKCLTS(R3)                                                  
         BCT   R1,*-10                                                          
*                                                                               
         CP    PKSRCNT,=P'1'       IF ONE ACCOUNT SKIP LINES                    
         BNE   ARP25                                                            
         GOTO1 ACREPORT            IF ONLY ONE ENTRY PRINTED SKIP LINES         
         GOTO1 ACREPORT              AFTER IT                                   
         B     ARP25                                                            
*                                                                               
ARP20    GOTO1 ARTOT,DMCB,PKCLTS,NCPTCLT   DO CLIENT   TOTALS                   
ARP25    ZAP   PKSRCNT,=P'0'                                                    
         MVC   PSRCLI,SRACLI       MOVE CLIENT CODE TO PRINT LINE               
         MVC   PSRCLINM,SRACLINM   MOVE CLIENT NAME TO PRINT LINE               
*                                                                               
ARP30    CP    SRADEB,=P'0'        DONT PRINT ZERO LINES                        
         BNE   ARP40                                                            
         CP    SRACRD,=P'0'                                                     
         BE    ARP120                                                           
*                                                                               
ARP40    AP    PKSRCNT,=P'1'       ADD TO COUNTER                               
         MVC   PSRACCT,SRACCT      ACCOUNT CODE                                 
         ZAP   PKAMT,SRADEB        A/R - DEBITS                                 
         SP    PKAMT,SRACRD        SUBTRACT CREDITS                             
*                                                                               
         CLI   QOPT2,C'Y'          ARE WE DOWNLOADING?                          
         BNE   ARP50                                                            
         GOTO1 ADWNRTE,DMCB,(RC)                                                
         B     ARP110                                                           
*                                                                               
ARP50    CP    SRADEB,=P'0'                                                     
         BNL   ARP60                                                            
         CURED (P8,SRADEB),(16,PSRDEB),2,ZERO=NOBLANK,BRACKET=YES               
         B     ARP70                                                            
ARP60    CURED (P8,SRADEB),(16,PSRDEB),2,ZERO=NOBLANK                           
*                                                                               
ARP70    CP    SRACRD,=P'0'                                                     
         BNL   ARP80                                                            
         CURED (P8,SRACRD),(16,PSRCRD),2,ZERO=NOBLANK,BRACKET=YES               
         B     ARP90                                                            
ARP80    CURED (P8,SRACRD),(16,PSRCRD),2,ZERO=NOBLANK                           
*                                                                               
ARP90    CP    PKAMT,=P'0'                                                      
         BNL   ARP100                                                           
         CURED (P8,PKAMT),(16,PSRNET),2,ZERO=NOBLANK,BRACKET=YES                
         B     ARP110                                                           
ARP100   CURED (P8,PKAMT),(16,PSRNET),2,ZERO=NOBLANK                            
*                                                                               
ARP110   AP    PKSRCRD,SRACRD      ADD CREDIT TO CLIENT ACCUMULATOR             
         AP    PKSRDEB,SRADEB      ADD BEBIT TO CLIENT ACCUMULATOR              
         AP    PKSRNET,PKAMT       ADD NET TO CLIENT ACCOUMULATOR               
*                                                                               
* ADD CLIENT LEVEL TO TOTAL                                                     
*                                                                               
         AP    PKSRCTOT,SRACRD     ADD CREDIT TO CLIENT ACCUMULATOR             
         AP    PKSRDTOT,SRADEB     ADD BEBIT TO CLIENT ACCUMULATOR              
         AP    PKSRNTOT,PKAMT      ADD NET TO CLIENT ACCOUMULATOR               
*                                                                               
         CLI   QOPT2,C'Y'          ARE WE DOWNLOADING?                          
         BE    ARP120                                                           
         GOTO1 ACREPORT                                                         
ARP120   MVC   LSTCLI,SRACLI       SAVE OFF LAST CLIENT CODE                    
         LA    R2,SRALNQ(R2)                                                    
         BCT   R0,ARP10                                                         
*                                                                               
         CP    PKSRCNT,=P'1'       IF ONLY ONE ACCOUNT NO CLI TOTS              
         BH    ARP130                                                           
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         B     ARP140                                                           
*                                                                               
ARP130   GOTO1 ARTOT,DMCB,PKCLTS,NCPTCLT   DO CLIENT   TOTALS                   
ARP140   GOTO1 ARTOT,DMCB,PKTOTS,0         DO RUN    TOTALS                     
*                                                                               
ARPX     B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* AR ACCOUNT TOTALS                                                  *          
*    R4 - PRINT LINE                                                 *          
**********************************************************************          
         SPACE 1                                                                
         USING PACKD,R2            CLIENT LEVEL                                 
         USING PLINED,R4                                                        
ARTOT    NTR1                                                                   
         L     R2,0(R1)                                                         
         MVC   BYTE,7(R1)          LEVEL SWITCH                                 
*                                                                               
         CLI   QOPT2,C'Y'          ARE WE DOWNLOADING?                          
         BNE   ART10                                                            
         GOTO1 ADWNRTE,DMCB,(RC)                                                
         B     ARTX                                                             
*                                                                               
ART10    MVC   PSRCLI,=C'RUN'      FOR RUN TOTALS                               
         CLI   BYTE,NCPTCLT        CLIENT TOTALS?                               
         BNE   *+10                                                             
         MVC   PSRCLI,LSTCLI       LAST CLIENT CODE                             
         MVC   PSRCLINM,=CL36'******TOTAL******'                                
*                                                                               
         CP    PSRDB,=P'0'                                                      
         BNL   ART20                                                            
         CURED (P8,PSRDB),(16,PSRDEB),2,ZERO=NOBLANK,BRACKET=YES                
         B     ART30                                                            
ART20    CURED (P8,PSRDB),(16,PSRDEB),2,ZERO=NOBLANK                            
*                                                                               
ART30    CP    PSRCR,=P'0'                                                      
         BNL   ART40                                                            
         CURED (P8,PSRCR),(16,PSRCRD),2,ZERO=NOBLANK,BRACKET=YES                
         B     ART50                                                            
ART40    CURED (P8,PSRCR),(16,PSRCRD),2,ZERO=NOBLANK                            
*                                                                               
ART50    CP    PSRNT,=P'0'                                                      
         BNL   ART60                                                            
         CURED (P8,PSRNT),(16,PSRNET),2,ZERO=NOBLANK,BRACKET=YES                
         B     ART70                                                            
ART60    CURED (P8,PSRNT),(16,PSRNET),2,ZERO=NOBLANK                            
*                                                                               
ART70    GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
*                                                                               
ARTX     MVI   BYTE,ALL            SET BYTE TO X'FF' FOR CURRENT                
         LA    R0,PCKQ             NUMBER OF PACKED FIELDS                      
         ZAP   0(L'PKFLDS,R2),=P'0'                                             
         LA    R2,L'PKFLDS(R2)                                                  
         BCT   R0,*-10                                                          
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* PRINT INFORMATION FROM TABLE - BY OFFICE                           *          
**********************************************************************          
         SPACE 1                                                                
PRNOFF   NTR1                                                                   
         MVI   RCSUBPRG,6          REGULAR P/O BY OFFICE W/O CODES              
         CLI   QOPT3,C'A'          PRINT ALL INCLUDING CODES                    
         BNE   *+8                                                              
         MVI   RCSUBPRG,2          REGULAR P/O BY OFFICE W/ CODES               
*                                                                               
         CLI   QOPT2,C'Y'          NO BOXES FOR DOWNLOADING                     
         BNE   *+12                                                             
         MVI   RCSUBPRG,9          SET PRINT RTE AS INVALID                     
         MVI   DWNHDOPT,DWNRGOFF   SET DOWNLOAD OPT TO SHOW REG OFF RUN         
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
         USING BIND,R1                                                          
         L     R1,ANCPTAB          R2=A(CLIENT TABLE)                           
         ICM   R0,15,BININ                                                      
         BZ    PRNOX                                                            
         USING NCPD,R2                                                          
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         USING PLINED,R4                                                        
         LA    R4,SVPRNT2                                                       
         BAS   RE,CLRALL           ZERO OUT ALL ACCUMULATORS                    
         XC    LSTGCO,LSTGCO       CLEAR SAVED ARE FOR PREVIOUS GCO             
         NI    FLAG,ALL-FLGPRT     TURN OFF PRINTED FLAG                        
         MVC   XP,XSPACES                                                       
         MVC   SVPRNT1(L'SVPRNT1+L'SVPRNT2),SPACES CLEAR SAVED AREA             
         B     PRNO20                                                           
*                                                                               
PRNO10   CLC   NCPOFF,LSTOFF                                                    
         BE    PRNO30                                                           
         GOTO1 TOTAL,DMCB,PKCLTS,PKDOS,NCPTCLT      DO CLIENT TOTALS            
         GOTO1 TOTAL,DMCB,PKDOS,PKTOTS,NCPTDIV      DO OFFICE TOTALS            
         TM    FLAG,FLGPRT         WAS SOMETHING PRINTED?                       
         BNO   PRNO20              NO DON'T SKIP LINE                           
         GOTO1 ACREPORT                                                         
         NI    FLAG,ALL-FLGPRT     TURN OFF PRINTED FLAG                        
*                                                                               
PRNO20   MVC   SVPRNT2,SPACES                                                   
         CLI   QOPT3,C'A'          PRINT ALL INCLUDING CODES                    
         BNE   *+10                                                             
         MVC   POFF,NCPOFF         CLIENT OFFICE                                
         MVC   POFFNM,NCPOFFNM     CLIENT OFFICE NAME                           
*                                                                               
         CLC   NCPOFF,SPACES       DO WE HAVE A OFFICE CODE                     
         BH    PRNO40              YES - PRINT IT                               
         MVC   POFFNM(8),=C'**NONE**'                                           
         B     PRNO40                                                           
*                                                                               
PRNO30   CLC   NCPCLI,LSTCLI       ARE WE DOING THE SAME CLIENT?                
         BE    PRNO40              NO - PRINT LINE ELSE ADD AMOUNTS             
         GOTO1 TOTAL,DMCB,PKCLTS,PKDOS,NCPTCLT      DO CLIENT   TOTALS          
*                                                                               
PRNO40   ZAP   PKAMT,NCPOBCRD      OPENING BALANCE - CREDIT                     
         SP    PKAMT,NCPOBDEB      SUBTRACT DEBITS                              
         MP    PKAMT,=P'-1'                                                     
         AP    PKOB,PKAMT                                                       
         ZAP   PKNET,PKAMT         BEGINNING FIGURE FOR NET CP                  
*                                                                               
         ZAP   PKAMT,NCPARDEB      A/R - DEBITS                                 
         SP    PKAMT,NCPARCRD      SUBTRACT CREDITS                             
         AP    PKARCOL,NCPARCRD    A/R - CREDT                                  
         AP    PKARBIL,NCPARDEB    A/R DEBITS                                   
         AP    PKARNET,PKAMT       ADD NET A/R AMOUNT TO TOTAL                  
         AP    PKNET,PKAMT         SUBTRACT TO GET NET CP                       
*                                                                               
         ZAP   PKAMT,NCPAPCRD      A/P - CREDT                                  
         SP    PKAMT,NCPAPDEB      SUBTRACT DEBITS                              
         AP    PKAP,PKAMT          ADD AMOUNT TO CLIENT TOTAL                   
         SP    PKNET,PKAMT         ADD TO GET NET CP                            
*                                                                               
         ZAP   PKAMT,NCPPIDEB      PRODUCT INVENTORY - DEBITS                   
         SP    PKAMT,NCPPICRD      SUBTRACT CREDITS                             
         AP    PKPI,PKAMT          ADD AMOUNT TO CLIENT TOTAL                   
         AP    PKNET,PKAMT         SUBTRACT TO GET NET CP                       
         AP    PKNTCSH,PKNET       ADD TO CLIENT NET TOTAL                      
*                                                                               
PRNO99   MVC   LSTGCO,NCPGRP       SAVE OFF LAST GRP/CLI/OFF                    
         MVC   LSTOFFNM,NCPOFFNM   CLIENT OFFICE NAME                           
         MVC   LSTCLINM,NCPCLTNM   SAVE CLIENT NAME FOR PRINTING                
         LA    R2,NCPLNQ(R2)                                                    
         BCT   R0,PRNO10                                                        
*                                                                               
         GOTO1 TOTAL,DMCB,PKCLTS,PKDOS,NCPTCLT      DO CLIENT TOTALS            
         GOTO1 TOTAL,DMCB,PKDOS,PKTOTS,NCPTDIV      DO OFFICE TOTALS            
         GOTO1 ACREPORT                                                         
         GOTO1 TOTAL,DMCB,PKTOTS,0,0                DO RUN    TOTALS            
*                                                                               
PRNOX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT BREAKDOWN OF PAYABLES BY LEDGER - BY OFFICE                  *          
**********************************************************************          
         SPACE 1                                                                
APPROFF  NTR1                                                                   
         MVI   RCSUBPRG,7          A/P BREAKDOWN BY OFFICE W/O CODES            
         CLI   QOPT3,C'A'          PRINT ALL INCLUDING CODES                    
         BNE   *+8                                                              
         MVI   RCSUBPRG,3          A/P BREAKDOWN BY OFFICE W/ CODES             
*                                                                               
         CLI   QOPT2,C'Y'          NO BOXES FOR DOWNLOADING                     
         BNE   *+12                                                             
         MVI   RCSUBPRG,9          SET PRINT RTE AS INVALID                     
         MVI   DWNHDOPT,DWNAPOFF   SET DOWNLOAD OPT TO SHOW A/P OFF RUN         
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
*                                                                               
         USING BIND,R1                                                          
         L     R1,ANCPTAB          R2=A(CLIENT TABLE)                           
         ICM   R0,15,BININ                                                      
         BZ    APPOX                                                            
         USING NCPD,R2                                                          
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         USING PLINED,R4                                                        
APPO10   LA    R4,SVPRNT2                                                       
         OI    FLAG,FLGAP          SET FLAG FOR AP BREAKDOWN                    
         BAS   RE,CLRALL           ZERO OUT ALL ACCUMULATORS                    
         MVC   SVPRNT1(L'SVPRNT1+L'SVPRNT2),SPACES CLEAR SAVED AREA             
         XC    LSTGCO,LSTGCO       CLEAR SAVED AREA FOR GRP/CLI/OFF             
         NI    FLAG,ALL-FLGPRT                                                  
*                                                                               
APPO30   L     R1,APAYTAB          R1=A(PAYABLE LEDGERS)                        
         LA    R3,PAYTABQ          R3=# OF PAYABLE LEDGERS                      
*                                                                               
APPO40   CLC   NCPUL,0(R1)         TRY AND MATCH ON UNIT/LEDGERS                
         BE    APPO50                                                           
         LA    R1,PAYDLNQ(R1)                                                   
         BCT   R3,APPO40                                                        
*                                                                               
         LA    R2,NCPLNQ(R2)       SKIP IT                                      
         BCT   R0,APPO30                                                        
         B     APPO120             PRINT TOTALS AND EXIT                        
*                                                                               
         USING PAYD,R1                                                          
APPO50   MVC   SVPK,PAYPK          SAVE DISPLACEMENT IN PK STORAGE              
         DROP  R1                                                               
*                                                                               
         OC    LSTGCO,LSTGCO       FIRST TIME THROUGH?                          
         BZ    APPO60                                                           
*                                                                               
         CLC   NCPOFF,LSTOFF                                                    
         BE    APPO90                                                           
         GOTO1 TOTAL,DMCB,PKCLTS,PKDOS,NCPTCLT      DO CLIENT TOTALS            
         GOTO1 TOTAL,DMCB,PKDOS,PKTOTS,NCPTDIV      DO OFFICE TOTALS            
         TM    FLAG,FLGPRT         WAS SOMETHING PRINTED?                       
         BNO   APPO60                                                           
         GOTO1 ACREPORT                                                         
         NI    FLAG,ALL-FLGPRT                                                  
*                                                                               
APPO60   MVC   SVPRNT2,SPACES                                                   
         CLI   QOPT3,C'A'          PRINT ALL INCLUDING CODES                    
         BNE   *+10                                                             
         MVC   POFF,NCPOFF         CLIENT OFFICE                                
         MVC   POFFNM,NCPOFFNM     CLIENT OFFICE NAME                           
         B     APPO100                                                          
*                                                                               
APPO90   CLC   NCPCLI,LSTCLI       ARE WE DOING THE SAME CLIENT?                
         BE    APPO100             NO - PRINT LINE ELSE ADD AMOUNTS             
         GOTO1 TOTAL,DMCB,PKCLTS,PKDOS,NCPTCLT      DO CLIENT TOTALS            
*                                                                               
APPO100  ZAP   PKAMT,NCPAPCRD      A/P - CREDT                                  
         SP    PKAMT,NCPAPDEB      SUBTRACT DEBITS                              
*                                                                               
         LA    R3,PKCLTS             R3=A(PRINTLINE)                            
         SR    R1,R1                                                            
         IC    R1,SVPK               R1=OFFSET INTO ACCUMULATOR                 
         AR    R3,R1                 BUMP                                       
         AP    0(L'PKFLDS,R3),PKAMT  ADD AMOUNT TO STORAGE                      
*                                                                               
APPO110  MVC   LSTGCO,NCPGRP       SAVE OFF LAST GRP/CLI/OFF                    
         MVC   LSTOFFNM,NCPOFFNM   OFFICE NAME                                  
         MVC   LSTCLINM,NCPCLTNM   CLIENT NAME FOR PRINTING                     
         LA    R2,NCPLNQ(R2)                                                    
         BCT   R0,APPO30                                                        
*                                                                               
APPO120  GOTO1 TOTAL,DMCB,PKCLTS,PKDOS,NCPTCLT      DO CLIENT TOTALS            
         GOTO1 TOTAL,DMCB,PKDOS,PKTOTS,NCPTDIV      DO OFFICE TOTALS            
         GOTO1 ACREPORT                                                         
         GOTO1 TOTAL,DMCB,PKTOTS,0,0                DO RUN    TOTALS            
*                                                                               
APPOX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* SORT BY GROUP/CLI CODE AMD OFFICE CODE (OFF ONLY IF REQUESTED)     *          
**********************************************************************          
         SPACE 1                                                                
XSRT     NTR1                                                                   
         USING BIND,R1                                                          
         L     R1,ANCPTAB          R1=A(NET CASH POSITION TABLE)                
         ICM   R0,15,BININ                                                      
         BZ    XSRTX                                                            
         USING NCPD,R2                                                          
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         CLI   QOPT1,C'5'          DO WE WANT TO SORT ON OFFICE?                
         BNE   XSRT10              NO - EXIT                                    
*                                                                               
* SORT BY OFFICE                                                                
*                                                                               
         LA    R3,NCPLNQ           R3 = LENGTH OF RECORD                        
         LA    R5,NCPOFLNQ         R5 = LENGTH OF FIELD TO XSORT                
         LA    R6,NCPOFDSP         R6 = DISPLACEMENT TO FIELD                   
         GOTO1 =V(XSORT),DMCB,(0,(R2)),(R0),(R3),(R5),(R6)                      
*                                                                               
XSRT10   CLI   QOPT5,C'Y'          DO WE WANT TO SORT ON RANK?                  
         BNE   XSRTX               NO - SKIP                                    
*                                                                               
* SORT BY RANK (NET CASH POSITION)                                              
*                                                                               
         LA    R3,NCPLNQ           R2 = LENGTH OF RECORD                        
         LA    R5,NCPNTLNQ         R4 = LENGTH OF FIELD TO XSORT                
         LA    R6,NCPNTDSP         R5 = DISPLACEMENT TO FIELD                   
         GOTO1 =V(XSORT),DMCB,(0,(R2)),(R0),(R3),(R5),(R6)                      
*                                                                               
XSRTX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* ZERO OUT ALL ACCUMULATORS                                          *          
**********************************************************************          
         SPACE 1                                                                
CLRALL   NTR1                                                                   
         LA    R0,PKFLDLNQ         # OF PACKED ACCUMULATORS                     
         LA    R1,PKFLDS           CLIENT AND GROUP/OFFICE LEVEL                
         ZAP   0(L'PKFLDS,R1),=P'0'                                             
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
CLRX     B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT LINE TOTALS                                                  *          
*       PARM1 - BUCKETS TO BE PRINTED AND CLEARED                    *          
*       PARM2 - BUCKETS TO BE ACCUMULATED                            *          
**********************************************************************          
         SPACE 1                                                                
         USING PACKD,R2                                                         
         USING PLINED,R4                                                        
TOTAL    NTR1                                                                   
         L     R2,0(R1)            ADDRESS OF BUCKETS TO BE PRINTED             
         L     R3,4(R1)            ADDRESS OF BUCKETS TO BE ACCUMULATED         
         LA    R4,XP               R4=A(PRINT LINE)                             
         MVC   BYTE,11(R1)         TOTAL SWITCH                                 
         LR    R5,R2               USE R5 FOR NEXT LOOP                         
*                                                                               
         MVC   PRTLNE,XSPACES                                                   
         LA    R0,PCKQ             FILTER OUT ZEROES                            
         CP    0(L'POB,R5),=P'0'                                                
         BNE   TOT05                                                            
         LA    R5,L'POB(R5)                                                     
         BCT   R0,*-14                                                          
         B     TOTX                                                             
*                                                                               
TOT05    L     R1,AREPTAB          R1=A(REPORT TABLE TO EXCLUDE 0 BAL)          
TOT10    CLI   0(R1),EOF                                                        
         BE    TOT20                                                            
         CLC   RCSUBPRG,0(R1)      ONLY TEST FOR REG PRINT (0,4)                
         BE    *+12                AND REG OFFICE PRINT (2,6)                   
         LA    R1,1(R1)                                                         
         B     TOT10                                                            
         CP    PKNTCSH-PKCLTS(L'PKCLTS,R2),=P'0'  DONT PRINT W/O A BAL          
         BE    TOT430                                                           
*                                                                               
TOT20    CLI   QOPT2,C'Y'          ARE WE DOWNLOADING?                          
         BNE   TOT30                                                            
         GOTO1 ADWNRTE,DMCB,(RC)   DO DOWNLOAD                                  
         B     TOT430              ADD TOTALS AND EXIT                          
*                                                                               
TOT30    OI    FLAG,FLGPRT         SET FLAG TO SHOW THERE WAS PRINTING          
         CLI   BYTE,NCPTCLT        CLIENT TOTALS?                               
         BNE   TOT100                                                           
         CLC   SVPRNT1,SPACES          ANYTHING TO PRINT?                       
         BNH   TOT40                                                            
         MVC   XP(L'SVPRNT1),SVPRNT1   MOVED SAVED DATA INTO PRINT LINE         
         GOTO1 ACREPORT                                                         
*                                                                               
TOT40    CLI   QOPT3,C'Y'          PRINT CLIENT INFO?                           
         BE    TOT50                                                            
         CLI   QOPT3,C'A'          PRINT ALL CLIENT INFO?                       
         BE    TOT50                                                            
         MVC   SVPRNT1,SPACES      CLEAR SAVED AREA                             
         MVC   PRTLNE,XSPACES      CLEAR PRINT LINE                             
         B     TOT70                                                            
*                                                                               
TOT50    CLC   SVPRNT2,SPACES          ANYTHING TO PRINT?                       
         BNH   TOT60                                                            
         MVC   XP(L'SVPRNT2),SVPRNT2   MOVED SAVED DATA INTO PRINT LINE         
         GOTO1 ACREPORT                                                         
*                                                                               
TOT60    MVC   SVPRNT1(L'SVPRNT1+L'SVPRNT2),SPACES CLEAR SAVED AREA             
         MVC   PRTLNE,XSPACES      CLEAR PRINT LINE                             
*                                                                               
TOT70    CLI   QOPT3,C'Y'          PRINT CLIENT TOTALS                          
         BE    TOT90                                                            
         CLI   QOPT3,C'A'          PRINT ALL CLIENT NAMES AND CODES             
         BE    TOT80                                                            
         TM    FLAG,FLGGRP         ONLY CLIENTS IN GROUPS DON'T PRINT           
         BO    TOT410                                                           
         CLI   QOPT1,C'5'          AS WELL AS OFFICE RUNS                       
         BE    TOT410                                                           
         B     TOT90                                                            
*                                                                               
TOT80    MVC   PCLT,LSTCLI         LAST CLIENT                                  
TOT90    MVC   PCLTNM,LSTCLINM     LAST CLIENT NAME                             
         B     TOT150                                                           
*                                                                               
TOT100   CLI   BYTE,NCPTDIV        DIVISION TOTALS?                             
         BNE   TOT130                                                           
         CLI   QOPT1,C'5'                                                       
         BNE   TOT110                                                           
         CLI   QOPT3,C'A'          PRINT ALL INCLUDING CODES                    
         BNE   *+10                                                             
         MVC   POFF,LSTOFF                                                      
         MVC   POFFNM(9),=C'**TOTAL**'                                          
         B     TOT120                                                           
*                                                                               
TOT110   CLI   QOPT3,C'A'          PRINT ALL INCLUDING CODES                    
         BNE   *+10                                                             
         MVC   PDIV,LSTDIV                                                      
         MVC   PDIVNM(9),=C'**TOTAL**'                                          
TOT120   CLI   QOPT3,C'Y'          IF PRINTING CLIENT TOTALS                    
         BE    TOT150              PRINT *TOTAL* IN DESC NAME                   
         CLI   QOPT3,C'A'                                                       
         BE    TOT150                                                           
         MVC   XP,XSPACES                                                       
         MVC   XP(L'SVPRNT2),SVPRNT2   MOVE SAVED DATA TO PRINT LINE            
         MVC   SVPRNT1(L'SVPRNT1+L'SVPRNT2),SPACES CLEAR SAVED AREA             
         B     TOT150                                                           
*                                                                               
TOT130   CLI   BYTE,NCPTCAT        CATEGORY TOTALS?                             
         BNE   TOT140                                                           
         CLI   QOPT3,C'Y'          IF PRINTING CLIENT TOTALS                    
         BE    *+12                PRINT *TOTAL* IN DESC NAME                   
         CLI   QOPT3,C'A'          PRINT ALL INCLUDING CODES                    
         BNE   *+10                                                             
         MVC   PCAT,LSTCAT                                                      
         MVC   PCATNM(9),=C'**TOTAL**'                                          
         B     *+10                                                             
*                                                                               
TOT140   MVC   PCATNM(13),=C'**RUN TOTAL**'    MUST RUN TOTALS?                 
*                                                                               
TOT150   TM    FLAG,FLGAP          IS THIS AN A/P RUN?                          
         BO    TOT290                                                           
*                                                                               
         CP    POB,=P'0'                                                        
         BNL   TOT160                                                           
         CURED (P8,POB),(16,POPNBAL),2,ZERO=NOBLANK,BRACKET=YES                 
         B     TOT170                                                           
TOT160   CURED (P8,POB),(16,POPNBAL),2,ZERO=NOBLANK                             
*                                                                               
TOT170   CP    PARC,=P'0'                                                       
         BNL   TOT180                                                           
         CURED (P8,PARC),(16,PARCOL),2,ZERO=NOBLANK,BRACKET=YES                 
         B     TOT190                                                           
TOT180   CURED (P8,PARC),(16,PARCOL),2,ZERO=NOBLANK                             
*                                                                               
TOT190   CP    PARB,=P'0'                                                       
         BNL   TOT200                                                           
         CURED (P8,PARB),(16,PARBIL),2,ZERO=NOBLANK,BRACKET=YES                 
         B     TOT210                                                           
TOT200   CURED (P8,PARB),(16,PARBIL),2,ZERO=NOBLANK                             
*                                                                               
TOT210   CP    PARN,=P'0'                                                       
         BNL   TOT220                                                           
         CURED (P8,PARN),(16,PARNET),2,ZERO=NOBLANK,BRACKET=YES                 
         B     TOT230                                                           
TOT220   CURED (P8,PARN),(16,PARNET),2,ZERO=NOBLANK                             
*                                                                               
TOT230   CP    PAP,=P'0'                                                        
         BNL   TOT240                                                           
         CURED (P8,PAP),(16,PPAY),2,ZERO=NOBLANK,BRACKET=YES                    
         B     TOT250                                                           
TOT240   CURED (P8,PAP),(16,PPAY),2,ZERO=NOBLANK                                
*                                                                               
TOT250   CP    PPI,=P'0'                                                        
         BNL   TOT260                                                           
         CURED (P8,PPI),(16,PPROD),2,ZERO=NOBLANK,BRACKET=YES                   
         B     TOT270                                                           
TOT260   CURED (P8,PPI),(16,PPROD),2,ZERO=NOBLANK                               
*                                                                               
TOT270   CP    PNCP,=P'0'                                                       
         BNL   TOT280                                                           
         CURED (P8,PNCP),(16,PNETCP),2,ZERO=NOBLANK,BRACKET=YES                 
         B     TOT410                                                           
TOT280   CURED (P8,PNCP),(16,PNETCP),2,ZERO=NOBLANK                             
         B     TOT410                                                           
*                                                                               
* A/P BREAKDOWN SECTION                                                         
*                                                                               
TOT290   ZAP   PKTOTAL,PSP                                                      
         CP    PSP,=P'0'                                                        
         BNL   TOT300                                                           
         CURED (P8,PSP),(16,PPAYSP),2,ZERO=NOBLANK,BRACKET=YES                  
         B     TOT310                                                           
TOT300   CURED (P8,PSP),(16,PPAYSP),2,ZERO=NOBLANK                              
*                                                                               
TOT310   AP    PKTOTAL,PSS                                                      
         CP    PSS,=P'0'                                                        
         BNL   TOT320                                                           
         CURED (P8,PSS),(16,PPAYSS),2,ZERO=NOBLANK,BRACKET=YES                  
         B     TOT330                                                           
TOT320   CURED (P8,PSS),(16,PPAYSS),2,ZERO=NOBLANK                              
*                                                                               
TOT330   AP    PKTOTAL,PSU                                                      
         CP    PSU,=P'0'                                                        
         BNL   TOT340                                                           
         CURED (P8,PSU),(16,PPAYSU),2,ZERO=NOBLANK,BRACKET=YES                  
         B     TOT350                                                           
TOT340   CURED (P8,PSU),(16,PPAYSU),2,ZERO=NOBLANK                              
*                                                                               
TOT350   AP    PKTOTAL,PSV                                                      
         CP    PSV,=P'0'                                                        
         BNL   TOT360                                                           
         CURED (P8,PSV),(16,PPAYSV),2,ZERO=NOBLANK,BRACKET=YES                  
         B     TOT370                                                           
TOT360   CURED (P8,PSV),(16,PPAYSV),2,ZERO=NOBLANK                              
*                                                                               
TOT370   AP    PKTOTAL,PSZ                                                      
         CP    PSZ,=P'0'                                                        
         BNL   TOT380                                                           
         CURED (P8,PSZ),(16,PPAYSZ),2,ZERO=NOBLANK,BRACKET=YES                  
         B     TOT390                                                           
TOT380   CURED (P8,PSZ),(16,PPAYSZ),2,ZERO=NOBLANK                              
*                                                                               
TOT390   CP    PKTOTAL,=P'0'                                                    
         BNL   TOT400                                                           
         CURED (P8,PKTOTAL),(16,PPAYTOT),2,ZERO=NOBLANK,BRACKET=YES             
         B     TOT410                                                           
TOT400   CURED (P8,PKTOTAL),(16,PPAYTOT),2,ZERO=NOBLANK                         
*                                                                               
* PRINT (IF NECESSARY), ADD TOTALS AND EXIT                                     
*                                                                               
TOT410   CLI   BYTE,NCPTCLT        CLIENT TOTALS?                               
         BNE   TOT420                                                           
         TM    FLAG,FLGGRP         ONLY CLIENTS IN GROUPS DON'T PRINT           
         BO    *+12                                                             
         CLI   QOPT1,C'5'          AND OFFICE RUNS DON'T PRINT                  
         BNE   TOT420                                                           
         CLI   QOPT3,C'Y'          PRINT CLIENT TOTALS                          
         BE    *+12                                                             
         CLI   QOPT3,C'A'                                                       
         BNE   TOT430                                                           
*                                                                               
TOT420   GOTO1 ACREPORT                                                         
         CLI   BYTE,NCPTCAT        IF CATEGORY TOTALS SKIP EXTRA LINES          
         BNE   TOT430                                                           
         GOTO1 ACREPORT                                                         
*                                                                               
* ADD TO TOTALS - ADDRESSED BY R3                                               
*                                                                               
TOT430   LTR   R3,R3               IS THIS RUN TOTAL?                           
         BZ    TOTX                                                             
         LR    R5,R2               R5 FOR ACCUMULATING NEXT LEVEL               
         LA    R0,PCKQ             # OF ACCUMULATORS                            
         AP    0(L'PKDOS,R3),0(L'PKCLTS,R5)                                     
         LA    R5,L'PKDOS(R5)                                                   
         LA    R3,L'PKCLTS(R3)                                                  
         BCT   R0,*-14                                                          
*                                                                               
TOTX     MVC   PRTLNE(PRLNQ),XSPACES     CLEAR PRINT LINE                       
         LA    R0,PCKQ                                                          
         ZAP   0(L'PKFLDS,R2),=P'0'                                             
         LA    R2,L'PKFLDS(R2)                                                  
         BCT   R0,*-10                                                          
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD SR ACCOUNT TABLE                                              *         
*       R2 - POINTS TO TRANSACTION RECORD                             *         
*       R4 - POINTS TO TRANSACTION ELEMENT                            *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2                                                       
         USING SRAD,R3                                                          
         USING TRNELD,R4                                                        
BLDSRA   NTR1                                                                   
         LA    R3,SRAWRK           SR ACCOUNT WORK AREA                         
         XC    SRAWRK,SRAWRK       CLEAR STORAGE                                
         MVC   SRACLI,SVCLI        CLIENT CODE                                  
         MVC   SRACLINM,SVCLINM    CLIENT NAME                                  
         MVC   SRACCT,TRNKACT      SR ACCOUNT CODE                              
*                                                                               
         LA    R0,SRABKCT          # OF BUCKETS TO ZAP                          
         LA    R1,SRABKT                                                        
         ZAP   0(SRABKLN,R1),=P'0'                                              
         LA    R1,SRABKLN(R1)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         LA    R1,SRADEB           DEBITS                                       
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R1,SRACRD           CREDITS                                      
         ZAP   0(L'SRABKT,R1),TRNAMNT                                           
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),SRAWRK,ASRATAB    ADD TO SR ACCT TABLE         
*                                                                               
BLDSX    B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD GROUP TABLE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING GRPD,R2                                                          
BLDGRP   NTR1                                                                   
         LA    R2,GRPWRK           GROUP WORK AREA                              
*                                                                               
         USING ACTRECD,R4                                                       
         LA    R4,SVKEY            ELSE - READ 2D LEDGER                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL    COMPANY CODE                                 
         MVI   ACTKUNT,C'F'                                                     
         MVC   ACTKLDG,QOPT8       LEDGER SPECIFIED IN REQUEST                  
         MVC   ACTKACT(4),QAPPL    READ ONLY FOR CAT SPECIFIED(IF ANY)          
         GOTO1 =A(DMHIGHDR),DMCB,(RC)       READ HIGH                           
         B     BLDG20                                                           
*                                                                               
BLDG10   GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
BLDG20   CLC   SVKEY(ACTKACT-ACTKEY),IOKEY                                      
         BNE   BLDGX                                                            
         LA    R4,IO                                                            
         CLC   QAPPL,SPACES        ANY GROUP SPECIFIED?                         
         BE    BLDG30                                                           
         CLC   ACTKACT(4),QAPPL    DID WE GET BACK THE RIGHT RECORD?            
         BNE   BLDGX                                                            
         CLC   ACTKACT+4(3),SPACES  CAT ONLY IS ACCEPTABLE                      
         BE    BLDG30                                                           
         CLC   ACTKACT,QAPPL        FULL ACCOUNT MUST MATCH                     
         BNE   BLDGX                                                            
*                                                                               
BLDG30   CLC   ACTKACT,SPACES                                                   
         BNH   BLDG10                                                           
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
*                                                                               
         XC    GRPWRK,GRPWRK                                                    
         MVC   GRPCDE,ACTKACT      MOVE IN CODE TO BINTABLE                     
         MVC   GRPCDCNM,SPACES     SPACE FILL NAME FIELD                        
*                                                                               
         USING NAMELD,R5                                                        
         LR    R5,R4               R5=A(IO)                                     
         MVI   ELCODE,NAMELQ       X'20' - NAME ELEMENT                         
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GRPCDCNM(0),NAMEREC                                              
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),GRPWRK,AGRPTAB ADD TO GROUP TABLE              
*                                                                               
         USING FFTELD,R5                                                        
*        LR    R5,R4               R5=A(IO)                                     
         MVI   ELCODE,FFTELQ       X'DB' - FREEFORM TEXT ELEMENT                
BLDG40   BAS   RE,NEXTEL2                                                       
         BNE   BLDG10                                                           
         CLI   FFTTYPE,FFTTCLIC    ARE THESE WORK GROUP ELEMENTS?               
         BNE   BLDG40                                                           
*                                                                               
         XC    GRPWRK,GRPWRK                                                    
         MVC   GRPCDE,ACTKACT                  MOVE IN GROUP CODE               
         MVC   GRPCDCNM,=CL36'CLIENT CODES'    CONSTANT                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,FFTDLEN          ACTUAL TEXT LENGTH                           
         LA    R3,FFTDATA          TEXT DATA - CLIENT CODES                     
*                                                                               
BLDG50   LTR   R0,R0                                                            
         BZ    BLDG40              END OF ELEMENT READ FOR NEXT                 
         MVC   GRPCLT,0(R3)        MOVE IN CLIENT CODES TO TABLE                
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),GRPWRK,AGRPTAB ADD TO GROUP TABLE              
*                                                                               
         LA    R3,L'GRPCLT(R3)     BUMP TO NEXT CLIENT CODE                     
         LA    R1,L'GRPCLT                                                      
         SR    R0,R1                                                            
         B     BLDG50                                                           
*                                                                               
BLDGX    B     EXIT                                                             
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD SJ OFFICE TABLE                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING OFFD,R2                                                          
BLDOFF   NTR1                                                                   
         LA    R2,OFFWRK                                                        
         XC    OFFWRK,OFFWRK                                                    
*                                                                               
         TM    FLAG,FLGNUOFF       IF ON NEW OFFICE READ OFFICE RECORDS         
         BO    BLDO30                                                           
*                                                                               
         USING ACTRECD,R4                                                       
         LA    R4,SVKEY            ELSE - READ 2D LEDGER                        
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,RCCOMPFL    COMPANY CODE                                 
         MVC   ACTKUNT(2),=C'2D'                                                
         MVC   ACTKACT(L'SVOFF),SVOFF                                           
         GOTO1 =A(DMHIGHDR),DMCB,(RC)       READ HIGH                           
         B     BLDO20                                                           
*                                                                               
BLDO10   GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
BLDO20   CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         BNE   BLDOX                                                            
         LA    R4,IO                                                            
         CLC   ACTKACT,SPACES                                                   
         BNH   BLDO10                                                           
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
         MVC   OFFWRK,SPACES            CLEAR WORK AREA                         
         MVC   OFFCDE(1),ACTKACT        MOVE OFFICE CODE INTO WORK AREA         
         B     BLDO60                                                           
*                                                                               
         USING OFFRECD,R4                                                       
BLDO30   LA    R4,SVKEY                                                         
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ    X'01' - OFFICE RECORD TYPE                   
         MVC   OFFKCPY,RCCOMPFL    COMPANY CODE                                 
         GOTO1 =A(DMHIGHDR),DMCB,(RC)       READ HIGH                           
         B     BLDO50                                                           
*                                                                               
BLDO40   GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
BLDO50   CLC   OFFKEY(OFFKOFF-OFFKEY),IOKEY                                     
         BNE   BLDOX                                                            
         LA    R4,IO                                                            
         CLC   OFFKOFF,SPACES                                                   
         BNH   BLDO40                                                           
         GOTO1 =A(DMGETREC),DMCB,(RC)       GET RECORD                          
         MVC   OFFWRK,SPACES       CLEAR WORK AREA                              
         MVC   OFFCDE,OFFKOFF      MOVE OFFICE CODE INTO WORK AREA              
*                                                                               
         USING ACTRECD,R4                                                       
BLDO60   LA    R4,IO                                                            
         USING NAMELD,R3                                                        
         LA    R3,ACTRFST                                                       
BLDO70   CLI   0(R3),0                                                          
         BE    BLDOX               IF NO NAME ELEMENT DON'T ADD NAME            
         CLI   NAMEL,NAMELQ        X'20' - NAME ELEMENT                         
         BE    BLDO80                                                           
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         AR    R3,R1                                                            
         B     BLDO70                                                           
*                                                                               
BLDO80   SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OFFNME(0),NAMEREC                                                
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),OFFWRK,AOFFTAB   ADD TO SJ CLI OFF TAB         
*                                                                               
         USING ACTRECD,R4                                                       
         LA    R4,SVKEY                                                         
         TM    FLAG,FLGNUOFF       IF ON NEW OFFICE READ OFFICE RECORDS         
         BO    BLDO40              2 BYTE OFFICE - READ SEQ                     
                                                                                
         MVC   ACTKEY(ACTKEND),IOKEY      UPDATE KEY                            
         MVI   ACTKACT+1,X'FF'            BUMP KEY                              
         B     BLDO10                     1 BYTE OFFICE - READ HIGH             
                                                                                
*                                                                               
BLDOX    B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
**********************************************************************          
* GET HEIRARCHY LEVEL STRUCTURE                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING ACLELD,R5                                                        
GETLEVS  NTR1                                                                   
         L     R5,ADLDGHIR         GET HEIRARCHY LEVELS                         
         MVC   LEVELS(LEVLNQ),SPACES                                            
         SR    R1,R1                                                            
         IC    R1,ACLLN            R1 = LENGTH OF ELEMENT                       
         SH    R1,=Y(ACLLN1Q+1)    SUBTRACT OVERHEAD                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LEVELS(0),ACLVALS   MVC LEVEL LENGTHS/NAMES INTO STORAGE         
*                                                                               
* CONVERT 1,3,5,12 -> 1,2,2,7                                                   
*                                                                               
         LA    R0,LEVELQ           R0 = MAXIMUM NUMBER OF LEVELS                
         STC   R0,LEVNUM           ASSUME 4 LEVEL STRUCTURE                     
         LA    R1,LEVELS           R1 = FIRST LEVEL LENGTH                      
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    R4,R4                                                            
GLEV10   ICM   R4,1,0(R1)          CURRENT LEVEL LENGTH                         
         BZ    GLEV20              NO MORE LEVELS - ADJUST LEVNUM               
         SR    R4,R3               SUBTRACT CURRENT FROM PREVIOUS               
         STC   R4,0(R2)            CURRENT INDIVIDUAL LENGTH                    
         IC    R3,0(R1)            UPDATE R3                                    
         LA    R1,L'ACLVALS(R1)                                                 
         LA    R2,1(R2)                                                         
         BCT   R0,GLEV10                                                        
         B     GLEVX                                                            
*                                                                               
GLEV20   LA    R1,LEVELQ           R1 = MAXIMUM NUMBER OF LEVELS                
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GLEVX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GET CLIENT NAME FROM CLIENT TABLE                                   *         
*     CLIENT CODE IS PASSED IN FIELD SVCLI                            *         
***********************************************************************         
         SPACE 1                                                                
GETCLI   NTR1                                                                   
         MVC   SVCLINM,SPACES      CLEAR CLIENT NAME FIELD                      
         MVC   SVOFF,SPACES        CLEAR CLIENT OFFICE IF NOT FOUND             
         NI    FLAG,X'FF'-FLGCLI   TURN OFF CLI NOT FOUND FLAG                  
*                                                                               
         USING BIND,R5                                                          
         L     R5,ACLITAB          R5=A(CLIENT CODE TABLE)                      
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         GOTO1 BINSRCH,DMCB,(X'00',SVCLI),(R6)                                  
         CLI   DMCB,1              RECORD WAS NOT FOUND                         
         BNE   GETC10                                                           
         OI    FLAG,FLGCLI         SHOW THAT CLIENT WASN'T FOUND                
         TM    FLAG,FLGACCT        ONLY RUN ON SJ CLIENT CODES                  
         BO    GETCXNO             YES-EXIT W/UNEQUAL                           
         MVC   SVCLINM,=CL36'*NOT AN SJ CODE*'                                  
         B     GETCXYES                                                         
*                                                                               
         USING CLID,R3                                                          
GETC10   L     R3,0(R1)                                                         
         MVC   SVCLINM,CLINME      PUT NAME FROM TABLE INTO SVCLINM             
         MVC   SVOFF,CLIOFF        SAVE CLIENT OFFICE                           
*                                                                               
GETCXYES SR    RC,RC                                                            
GETCXNO  LTR   RC,RC                                                            
         B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
**********************************************************************          
* GET GROUP CODE FROM GROUP RECORDS BY MATCHING CLIENT CODES         *          
*     CLIENT CODE IS PASSED IN FIELD SVCLI                           *          
*     GROUP CODE WILL BE RETURNED IN FIELD SVGRP                     *          
*     NAME OF LEV1 OF GRP(CATEGORY) WILL BE RETURNED IN SVCATNM FIELD*          
**********************************************************************          
         SPACE 1                                                                
         USING NCPD,R3                                                          
GETGRP   NTR1                                                                   
         LA    R3,GRPWRK                                                        
         XC    GRPWRK,GRPWRK                                                    
         XC    SVGRP,SVGRP                                                      
         XC    SVCATNM,SVCATNM                                                  
         XC    SVDIVNM,SVDIVNM                                                  
*                                                                               
         CLC   SVCLI,SPACES        IF CLIENT IS NOT SET EXIT                    
         BE    GETGX                                                            
*                                                                               
         USING BIND,R5                                                          
         L     R5,AGRPTAB          R2=A(GROUP TABLE)                            
         ICM   R0,15,BININ                                                      
         BZ    GETGX                                                            
         USING GRPD,R2                                                          
         LA    R2,BINTAB                                                        
*                                                                               
GETG10   CLC   GRPCLT,SVCLI                                                     
         BE    *+16                                                             
         LA    R2,GRPLNQ(R2)                                                    
         BCT   R0,GETG10                                                        
         B     GETGX                                                            
*                                                                               
         MVC   SVGRP,GRPCDE        SAVE GROUP CODE AND GET LEVS                 
         LA    R2,BINTAB                                                        
*                                                                               
         CLC   GRPCAT,SVCAT        MATCH ON GROUP CATEGORY                      
         BE    *+12                                                             
         LA    R2,GRPLNQ(R2)                                                    
         B     *-14                                                             
*                                                                               
         MVC   SVCATNM,GRPCDCNM    SAVE CATEGORY NAME                           
*                                                                               
         XC    GRPWRK,GRPWRK                                                    
         LA    R2,BINTAB                                                        
*                                                                               
         CLC   GRPCDE,SVGRP        MATCH ON GROUP CODE                          
         BE    *+12                                                             
         LA    R2,GRPLNQ(R2)                                                    
         B     *-14                                                             
*                                                                               
         MVC   SVDIVNM,GRPCDCNM    DIVISION NAME                                
*                                                                               
GETGX    B     EXIT                                                             
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
**********************************************************************          
* ADD HIGHER LEVELS OF GROUP RECORDS IF NEEDED                       *          
*     CLIENT CODE IS PASSED IN FIELD SVCLI                           *          
**********************************************************************          
         SPACE 1                                                                
         USING NCPD,R3                                                          
ADDGRP   NTR1                                                                   
         LA    R3,NCPWRK                                                        
         XC    NCPWRK,NCPWRK                                                    
*                                                                               
         CLC   SVCLI,SPACES        IF CLIENT IS NOT SET EXIT                    
         BE    ADDGX                                                            
*                                                                               
         USING BIND,R5                                                          
         L     R5,AGRPTAB          R2=A(GROUP TABLE)                            
         ICM   R0,15,BININ                                                      
         BZ    ADDGX                                                            
         USING GRPD,R2                                                          
         LA    R2,BINTAB                                                        
*                                                                               
ADDG10   CLC   GRPCLT,SVCLI                                                     
         BE    *+16                                                             
         LA    R2,GRPLNQ(R2)                                                    
         BCT   R0,ADDG10                                                        
         B     ADDGX                                                            
*                                                                               
         MVC   SVGRP,GRPCDE        SAVE GROUP CODE AND GET LEVS                 
         LA    R2,BINTAB                                                        
*                                                                               
         CLC   GRPCAT,SVCAT        MATCH ON GROUP CATEGORY                      
         BE    *+12                                                             
         LA    R2,GRPLNQ(R2)                                                    
         B     *-14                                                             
*                                                                               
         MVC   NCPCAT,GRPCAT       CATEGORY CODE                                
         MVC   NCPCATNM,GRPCDCNM   CATEGORY NAME                                
         MVI   NCPTYP,NCPTCAT      SET SWITCH FOR CATEGORY RECORD               
*                                                                               
         LA    R0,NCPBKCT          3 OF BUCKETS                                 
         LA    R1,NCPBKT           ZAP BUCKETS                                  
         ZAP   0(L'NCPBKT,R1),=P'0'                                             
         LA    R1,L'NCPBKT(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         OI    NCPREC,NCPRGRP      SHOW GROUP RECORD STATUS                     
         GOTO1 ABINADD,DMCB,(RC),NCPWRK,ANCPTAB                                 
         XC    NCPWRK,NCPWRK                                                    
         LA    R2,BINTAB                                                        
*                                                                               
         CLC   GRPCDE,SVGRP        MATCH ON GROUP CODE                          
         BE    *+12                                                             
         LA    R2,GRPLNQ(R2)                                                    
         B     *-14                                                             
*                                                                               
         MVC   NCPGRP,GRPCDE       DIVISION CODE                                
         MVC   NCPCATNM,SVCATNM    CATEGORY NAME                                
         MVC   NCPDIVNM,GRPCDCNM   DIVISION NAME                                
         MVI   NCPTYP,NCPTDIV      SET SWITCH FOR DIVISION RECORD               
*                                                                               
         LA    R0,NCPBKCT          3 OF BUCKETS                                 
         LA    R1,NCPBKT           ZAP BUCKETS                                  
         ZAP   0(L'NCPBKT,R1),=P'0'                                             
         LA    R1,L'NCPBKT(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         OI    NCPREC,NCPRGRP      SHOW GROUP RECORD STATUS                     
         GOTO1 ABINADD,DMCB,(RC),NCPWRK,ANCPTAB                                 
*                                                                               
ADDGX    B     EXIT                                                             
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
***********************************************************************         
* GET OFFICE NAME AND SV IN SVOFFNM FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
GETOFF   NTR1                                                                   
         MVC   SVOFFNM,SPACES      CLEAR OUT SAVED FIELD                        
*                                                                               
         L     R5,AOFFTAB          R5=A(OFFICE CODE TABLE)                      
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         GOTO1 BINSRCH,DMCB,(X'00',SVOFF),(R6)                                  
         CLI   DMCB,1              RECORD WAS NOT FOUND                         
         BE    GETOX                                                            
*                                                                               
         USING OFFD,R3                                                          
         L     R3,0(R1)            ADDRESS OF FOUND RECORD                      
         MVC   SVOFFNM,OFFNME      PLUG IN OFFICE NAME FROM TABLE               
*                                                                               
GETOX    B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
**********************************************************************          
* SET LEVEL CODES                                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,R5                                                       
SETCDE   NTR1                                                                   
         L     R5,ADACC            A(ACCOUNT RECORD)                            
         MVC   LEVSCDE(LEVSLNQ),SPACES     CLEAR LEVEL CODES AREA               
         SR    R0,R0                                                            
         IC    R0,LEVNUM           NUMBER OF LEVELS/LOOPS                       
         LA    R1,ACTKACT          FULL ACCOUNT CODE                            
         LA    R2,LEVSCDE          FIRST LEVEL CODE                             
         LA    R3,LEVLNQS          FIRST LEVEL LENGTHS                          
*                                                                               
SETCDE10 SR    R4,R4                                                            
         IC    R4,0(R3)            CURRENT INDIVIDUAL LEV LENGTH                
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R1)                                                    
         LA    R1,1(R4,R1)         BUMP TO NEXT LEVEL IN ACCOUNT CODE           
         LA    R2,L'LEVSCDE(R2)    BUMP TO NEXT LEVEL CODE AREA                 
         LA    R3,1(R3)            BUMP TO NEXT INDIVIUAL LEV LENGTH            
         BCT   R0,SETCDE10                                                      
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R5,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* GETEL#2                                                             *         
***********************************************************************         
         SPACE 1                                                                
         GETELN R5,DISP2,ELCODE,2                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
ABOXRC   DC    A(BOXRC)                                                         
ABXHOOK  DC    A(BXHOOK)                                                        
ADUMP    DC    A(DUMP)             PRINTABLE ROUTINE                            
*                                                                               
ANCPTAB  DC    A(NCPTAB)           NET CASH POSITION BINTABLE                   
ACLITAB  DC    A(CLITAB)           CLIENT INFO BINTABLE                         
AGRPTAB  DC    A(GRPTAB)           GROUP INFO BINTABLE                          
AOFFTAB  DC    A(OFFTAB)           OFFICE INFO BINTABLE                         
ARNKTAB  DC    A(RNKTAB)           RANKING BINTABLE                             
ASRATAB  DC    A(SRATAB)           SR ACCOUNT TABLE                             
AHD1TAB  DC    A(HD1TAB)           FIRST HEADLINE DWNLD FIELDS TABLE            
AHD2TAB  DC    A(HD2TAB)           SECOND HEADLINE DWNLD FIELDS TABLE           
ACPYTAB  DC    A(CPYTAB)           COMPANY INFO TABLE                           
ALDGTAB  DC    A(LDGTAB)           ALL LEDGER/BUCKETS INFO TABLE                
APAYTAB  DC    A(PAYTAB)           PAYABLES LEDGER/BUCKETS INFO TABLE           
AREPTAB  DC    A(REPTAB)           REPORT TABLE TO EXCLUDE ZERO BAL             
*                                                                               
RNKMSK   DC    PL8'999999999999999'                                             
         EJECT                                                                  
***********************************************************************         
* RELOCATABLES                                                        *         
***********************************************************************         
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(BINADD)           ADD TO BIN TABLE                             
         DC    A(GETRNK)           GET RANK FROM NCP TABLE                      
         DC    A(DWNL)             DOWNLOAD ROUTINE                             
         DC    A(DWNBUF)           DOWNLOAD BUFFER                              
         DC    A(DWNRTE)           DOWNLOAD TOTALS ROUTINE                      
         DC    A(INIT)             INITIALIZE STORAGE ROUTINE                   
         DC    A(SZTAB)            SZ ACCOUNT TABLE                             
         DC    A(TRNTAB)           SZ TRANSACTION TYPE TABLE                    
*                                                                               
         DC    V(PRNTBL)           PRINT DATA                                   
         DC    V(DLFLD)            DOWNLOAD MODULE                              
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
         SPACE 1                                                                
CPYTAB   DS    0C                       COMPANY TABLE                           
         DC    C'YN',CL30'YOUNG AND RUBICAM, L.P.'                              
         DC    C'YP',CL30'SUDLER AND HENNESSEY'                                 
         DC    C'YE',CL30'CHAPMAN DIRECT ADVERTISING'                           
         DC    C'WW',CL30'WUNDERMAN CATO JOHNSON'                               
CPYTABQ  EQU   (*-CPYTAB)/CPYLNQ                                                
*                                                                               
LDGTAB   DS    0CL3   1 BYTES LEDGER-1 BYTE DISP TO BUCKETS                     
         DC    C'J',X'00',AL1(NCPPIBKT-NCPBKT)     PRODUCTION                   
         DC    C'P',X'00',AL1(NCPAPBKT-NCPBKT)     PRINT PAYEES                 
         DC    C'R',X'00',AL1(NCPARBKT-NCPBKT)     RECEIVABLES                  
         DC    C'S',X'00',AL1(NCPAPBKT-NCPBKT)     SPOT PAYEES                  
         DC    C'U',X'00',AL1(NCPAPBKT-NCPBKT)     NETWORK PAYEES               
         DC    C'V',X'00',AL1(NCPAPBKT-NCPBKT)     PRODUCTION VENDORS           
         DC    C'Z',X'00',AL1(NCPAPBKT-NCPBKT)     MEDIA CONTROL                
LDGTABQ  EQU   (*-LDGTAB)/LDGTBLNQ                                              
*                                                                               
PAYTAB   DS    0C                                                               
         DC    C'SP',AL1(PKSP-PKCLTS)                                           
         DC    C'SS',AL1(PKSS-PKCLTS)                                           
         DC    C'SU',AL1(PKSU-PKCLTS)                                           
         DC    C'SV',AL1(PKSV-PKCLTS)                                           
         DC    C'SZ',AL1(PKSZ-PKCLTS)                                           
PAYTABQ  EQU   (*-PAYTAB)/PAYDLNQ                                               
*                                                                               
REPTAB   DS    0CL1                                                             
         DC    XL1'00'                                                          
         DC    XL1'02'                                                          
         DC    XL1'04'                                                          
         DC    XL1'06'                                                          
         DC    AL1(EOF)                                                         
*                                                                               
* FIRST HEADLINE TABLE                                                          
*                                                                               
HD1TAB   DC    XL1'A0',CL20'GROUP LEVEL 1'       FIRST FIELD                    
         DC    XL1'50',CL20'OFFICE'                                             
         DC    XL1'08',CL20'CLIENT'                                             
*                                                                               
         DC    XL1'A0',CL20'GROUP LEVEL 2'       SECOND FIELD                   
         DC    XL1'50',CL20' '                                                  
         DC    XL1'08',CL20'CLIENT'                                             
*                                                                               
         DC    XL1'F0',CL20'CLIENT'              THIRD FIELD                    
         DC    XL1'08',CL20' '                                                  
*                                                                               
         DC    XL1'A0',CL20'GROUP LEVEL 1'       FOURTH FIELD                   
         DC    XL1'50',CL20'OFFICE'                                             
         DC    XL1'08',CL20' '                                                  
*                                                                               
         DC    XL1'A0',CL20'GROUP LEVEL 2'       FIFTH FIELD                    
         DC    XL1'58',CL20' '                                                  
*                                                                               
         DC    XL1'E0',CL20'CLIENT'              SIXTH FIELD                    
         DC    XL1'08',CL20' '                                                  
*                                                                               
         DC    XL1'C0',CL20'OPENING BALANCE'     SEVENTH FIELD                  
         DC    XL1'30',CL20'SP'                                                 
         DC    XL1'08',CL20' '                                                  
*                                                                               
         DC    XL1'C8',CL20' '                   EIGTH FIELD                    
         DC    XL1'30',CL20'SS'                                                 
*                                                                               
         DC    XL1'C8',CL20' '                   NINTH FIELD                    
         DC    XL1'30',CL20'SU'                                                 
*                                                                               
         DC    XL1'C8',CL20' '                   TENTH FIELD                    
         DC    XL1'30',CL20'SV'                                                 
*                                                                               
         DC    XL1'C8',CL20' '                   ELEVENTH FIELD                 
         DC    XL1'30',CL20'SZ'                                                 
*                                                                               
         DC    XL1'C0',CL20'PRODUCTION'          TWELFTH FIELD                  
         DC    XL1'30',CL20'LEDGERS'                                            
         DC    XL1'08',CL20' '                                                  
*                                                                               
         DC    XL1'C0',CL20'NET CASH POSITION'   THIRTEENTH FIELD               
         DC    XL1'38',CL20' '                                                  
HD1LNQ   EQU   (*-HD1TAB)/HDLNQ                                                 
         EJECT                                                                  
*                                                                               
* SECOND HEADLINE TABLE                                                         
*                                                                               
HD2TAB   DC    XL1'F8',CL20'NAME'                FIRST FIELD                    
*                                                                               
         DC    XL1'A0',CL20'NAME'                SECOND FIELD                   
         DC    XL1'08',CL20'CODE'                                               
         DC    XL1'50',CL20' '                                                  
*                                                                               
         DC    XL1'F0',CL20'NAME'                THIRD FIELD                    
         DC    XL1'08',CL20'SR ACCOUNT'                                         
*                                                                               
         DC    XL1'F0',CL20'CODE'                FOURTH FIELD                   
         DC    XL1'08',CL20'BILLED'                                             
*                                                                               
         DC    XL1'A0',CL20'CODE'                FIFTH FIELD                    
         DC    XL1'08',CL20'COLLECTED'                                          
         DC    XL1'50',CL20' '                                                  
*                                                                               
         DC    XL1'F0',CL20'CODE'                SIXTH FIELD                    
         DC    XL1'08',CL20'NET'                                                
*                                                                               
         DC    XL1'C0',CL20'PRIOR TO'            SEVENTH FIELD                  
         DC    XL1'30',CL20'LEDGER'                                             
         DC    XL1'08',CL20' '                                                  
*                                                                               
         DC    XL1'C0',CL20'A/R BILLED'          EIGTH FIELD                    
         DC    XL1'30',CL20'LEDGER'                                             
         DC    XL1'08',CL20' '                                                  
*                                                                               
         DC    XL1'C0',CL20'A/R COLLECTED'       NINETH FIELD                   
         DC    XL1'30',CL20'LEDGER'                                             
         DC    XL1'08',CL20' '                                                  
*                                                                               
         DC    XL1'C0',CL20'A/R NET'             TENTH FIELD                    
         DC    XL1'30',CL20'LEDGER'                                             
         DC    XL1'08',CL20' '                                                  
*                                                                               
         DC    XL1'C0',CL20'A/P'                 ELEVENTH FIELD                 
         DC    XL1'30',CL20'LEDGER'                                             
         DC    XL1'08',CL20' '                                                  
*                                                                               
         DC    XL1'C0',CL20'INVENTORY'           TWELFTH FIELD                  
         DC    XL1'30',CL20'TOTAL'                                              
         DC    XL1'08',CL20' '                                                  
*                                                                               
         DC    XL1'C0',CL20'AS OF'               THIRTEENTH FIELD               
         DC    XL1'38',CL20' '                                                  
HD2LNQ   EQU   (*-HD2TAB)/HDLNQ                                                 
*                                                                               
SZTAB    DC    C'YF',CL12'PM',X'00'                                             
         DC    C'YF',CL12'PN',X'00'                                             
         DC    C'YF',CL12'PO',X'00'                                             
         DC    C'YF',CL12'PT',X'00'                                             
         DC    C'YP',CL12'PM',X'00'                                             
         DC    C'YP',CL12'PN',X'00'                                             
         DC    C'YP',CL12'PO',X'00'                                             
         DC    C'YP',CL12'PT',X'00'                                             
         DC    C'YE',CL12'NC',X'00'                                             
         DC    C'YE',CL12'PM',X'00'                                             
         DC    C'YE',CL12'PN',X'00'                                             
         DC    C'YE',CL12'PO',X'00'                                             
         DC    C'YE',CL12'PT',X'00'                                             
         DC    C'YE',CL12'SN',X'00'                                             
         DC    C'YE',CL12'SR',X'00'                                             
         DC    C'YE',CL12'ST',X'00'                                             
         DC    C'WW',CL12'NC',X'00'                                             
         DC    C'WW',CL12'NS',X'00'                                             
         DC    C'WW',CL12'PM',X'00'                                             
         DC    C'WW',CL12'PN',X'00'                                             
         DC    C'WW',CL12'PO',X'00'                                             
         DC    C'WW',CL12'PS',X'00'                                             
         DC    C'WW',CL12'PT',X'00'                                             
         DC    C'WW',CL12'SN',X'00'                                             
         DC    C'WW',CL12'ST',X'00'                                             
         DC    C'YN',CL12'MGAA',X'80'                                           
         DC    C'YN',CL12'MGA8',X'80'                                           
         DC    C'YN',CL12'PM',X'00'                                             
         DC    C'YN',CL12'PN',X'00'                                             
         DC    C'YN',CL12'PO',X'00'                                             
         DC    C'YN',CL12'PT',X'00'                                             
         DC    C'YN',CL12'SN',X'00'                                             
         DC    C'YN',CL12'SR',X'00'                                             
         DC    C'YN',CL12'ST',X'00'                                             
         DC    C'YN',CL12'SX',X'00'                                             
         DC    C'YN',CL12'UA',X'00'                                             
         DC    C'YN',CL12'UN',X'00'                                             
         DC    C'YN',CL12'UG',X'00'                                             
         DC    AL1(EOF)                                                         
*                                                                               
TRNTAB   DS    0XL1                TRANSACTION TYPE TABLE                       
         DC    XL1'21'             ONLY INCLUDE THESE TRNS FOR SZ               
         DC    XL1'22'                                                          
         DC    XL1'31'                                                          
         DC    XL1'32'                                                          
         DC    XL1'09'                                                          
         DC    XL1'00'                                                          
         DC    AL1(EOF)                                                         
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   DS    0D                                                               
         NMOD1 0,**BINA**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R5,8(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,4(R1)            A(ITEM)                                      
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
BINA10   AP    0(L'NCPBKT,R4),0(L'NCPBKT,R3)   ADD TO BUCKET                    
         LA    R3,L'NCPBKT(R3)     BUMP TO NEXT ENTRY IN NEW ITEM               
         LA    R4,L'NCPBKT(R4)     BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,BINA10                                                        
*                                                                               
BINXIT   XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* READ THROUGH TABLE AND FIGURE OUT NET CASH POSITION FOR RANKING    *          
**********************************************************************          
         SPACE 1                                                                
         USING BIND,R1                                                          
GETRNK   DS    0D                                                               
         NMOD1 0,**RANK**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R1,ANCPTAB          R1=A(NET CASH POSITION TABLE)                
         ICM   R0,15,BININ                                                      
         BZ    GETRX                                                            
         USING NCPD,R2                                                          
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         USING RNKD,R3                                                          
         LA    R3,RNKWRK           RANK TABLE WORK AREA                         
GETR10   XC    RNKWRK,RNKWRK       CLEAR WORK AREA                              
*                                                                               
         MVC   RNKCAT,NCPCAT       KEY=CAT OR CLIENT CODE                       
         CLC   RNKCAT,SPACES                                                    
         BH    *+16                                                             
         MVC   RNKCLI,NCPCLI       IF NO CAT USE CLIENT CODE                    
         MVC   RNKOFF,NCPOFF       OFFICE CODE                                  
*                                                                               
         CLI   QOPT1,C'5'          ARE WE DOING OFFICES?                        
         BNE   *+16                                                             
         XC    RNKKEY,RNKKEY       CLEAR KEY                                    
         MVC   RNKOFF,NCPOFF       OFFICE CODE (NO NEED FOR CLI/CAT)            
*                                                                               
         ZAP   PKAMT,NCPOBCRD      OPENING BALANCE - CREDIT                     
         SP    PKAMT,NCPOBDEB      SUBTRACT DEBITS                              
         MP    PKAMT,=P'-1'                                                     
         ZAP   RNKBKT,PKAMT        ADD TO RANK BUCKET                           
*                                                                               
         ZAP   PKAMT,NCPARDEB      A/R - DEBITS                                 
         SP    PKAMT,NCPARCRD      SUBTRACT CREDITS                             
         AP    RNKBKT,PKAMT        SUBTRACT TO GET NET CP                       
*                                                                               
         ZAP   PKAMT,NCPAPCRD      A/P - CREDT                                  
         SP    PKAMT,NCPAPDEB      SUBTRACT DEBITS                              
         SP    RNKBKT,PKAMT        ADD TO GET NET CP                            
*                                                                               
         ZAP   PKAMT,NCPPIDEB      PRODUCT INVENTORY - DEBITS                   
         SP    PKAMT,NCPPICRD      SUBTRACT CREDITS                             
         AP    RNKBKT,PKAMT        SUBTRACT TO GET NET CP                       
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),RNKWRK,ARNKTAB     ADD TO GROUP TABLE          
*                                                                               
         LA    R2,NCPLNQ(R2)                                                    
         BCT   R0,GETR10                                                        
*                                                                               
* READ BACK TOTALS AND PLUG THEM INTO NET CASH POSITION TABLE                   
*                                                                               
         USING BIND,R1                                                          
         L     R1,ANCPTAB          R1=A(NET CASH POSITION TABLE)                
         ICM   R0,15,BININ                                                      
         BZ    GETRX                                                            
         USING NCPD,R2                                                          
         LA    R2,BINTAB                                                        
*                                                                               
GETR20   XC    SVRNKKEY,SVRNKKEY   CLEAR OUT FIELD FOR TABLE LOOKUP             
         MVC   SVRNKCAT,NCPCAT                                                  
         CLC   NCPCAT,SPACES       USE CLIENT CODE IF NO CATEGORY               
         BH    *+16                                                             
         MVC   SVRNKCLI,NCPCLI                                                  
         MVC   SVRNKOFF,NCPOFF                                                  
*                                                                               
         CLI   QOPT1,C'5'          ARE WE DOING OFFICES?                        
         BNE   *+16                                                             
         XC    SVRNKKEY,SVRNKKEY                                                
         MVC   SVRNKOFF,NCPOFF                                                  
*                                                                               
         L     R1,ARNKTAB          R1=A(RANK TABLE)                             
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         GOTO1 BINSRCH,DMCB,(X'00',SVRNKKEY),(R6)                               
         CLI   DMCB,1              RECORD WAS NOT FOUND                         
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RNKD,R3                                                          
         L     R3,0(R1)                                                         
         ZAP   NCPNET,RNKBKT       PUT AMNT INTO NET CASH POS TAB               
*                                                                               
         CP    NCPNET,=P'0'        SEE IF POSITIVE                              
         BL    *+16                NO SO SKIP                                   
         MP    NCPNET,=P'-1'       MAKE NEG. TO FORCE SUBTRACTION               
         AP    NCPNET,RNKMSK       ADD TO MASK TO CORRECT SORT ORDER            
*                                                                               
         XC    NCPNETPR,NCPNETPR   CLEAR POSITIVE/NEGATIVE INDICATOR            
         CP    NCPNET,=P'0'                                                     
         BNL   *+8                                                              
         MVI   NCPNETPR,X'FF'      INDICATE AMOUNT IS NEGATIVE                  
*                                                                               
         LA    R2,NCPLNQ(R2)                                                    
         BCT   R0,GETR20                                                        
*                                                                               
GETRX    XIT1                                                                   
         DROP  R1,R2,R3                                                         
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD TOTALS                                                    *          
*          R2   - A(PACKED FIELDS FOR ALL SECTIONS EXCEPT A/R)       *          
*          R2   - FOR A/R A(PACKED FIELDS OR SRA BINTABLE IF BYTE=FF)*          
*          R4   - A(PRINT LINE - A/R SECTION ONLY)                   *          
*          BYTE - TYPE OF TOTAL (CLI/DIV/CAT/RUN)                    *          
**********************************************************************          
         SPACE 1                                                                
         USING PACKD,R2                                                         
DWNRTE   DS    0D                                                               
         NMOD1 0,**DWNR**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         USING BOXD,R3                                                          
         L     R3,ADBOX                                                         
         MVI   BOXOFF,C'Y'         NO BOXES IF DOWNLOADING                      
         DROP  R3                                                               
*                                                                               
         TM    DWNSTAT,DWNHDLN     WERE THE HEADLINES DOWNLOADED?               
         BO    *+8                                                              
         BAS   RE,DWNHEAD          DOWNLOAD HEADLINES                           
*                                                                               
         TM    FLAG,FLGAR                                                       
         BO    DWNR60                                                           
*                                                                               
         CLI   BYTE,NCPTCLT        CLIENT   TOTALS?                             
         BNE   DWNR10                                                           
         TM    FLAG,FLGGRP         IF CLIENT IS NOT PART OF GROUP               
         BO    DWNR20              CLEAR OUT CAT/DIV FIELDS                     
         MVC   LSTCATNM(L'LSTCATNM+L'LSTDIVNM),SPACES     NAMES                 
         MVC   LSTCAT(L'LSTCAT+L'LSTDIV),SPACES           CODES                 
         B     DWNR20                                                           
*                                                                               
DWNR10   CLI   BYTE,NCPTDIV        DIVISION TOTALS?                             
         BNE   *+20                                                             
         MVC   LSTDIVNM,=CL36'***DIV TOTAL***'                                  
         MVC   LSTCLINM(L'LSTCLINM+L'LSTGCO),SPACES                             
         B     DWNR20                                                           
*                                                                               
         MVC   LSTDIVNM(L'LSTDIVNM+L'LSTCLINM+L'LSTGCO),SPACES                  
         MVC   LSTCATNM,=CL36'***CAT TOTAL***'                                  
         CLI   BYTE,NCPTCAT        CATEGORY TOTALS?                             
         BE    DWNR20                                                           
         MVC   LSTCATNM,=CL36'***RUN TOTAL***'                                  
*                                                                               
DWNR20   MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD,LSTCATNM            MOVE CAT NAME TO DWN FLD              
         CLI   QOPT1,C'5'                 IF OFFICE RUN OVERWRITE               
         BNE   *+10                          WITH OFFICE NAME                   
         MVC   DWNFLD,LSTOFFNM            MOVE OFF NAME TO DWN FLD              
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         CLI   QOPT1,C'5'                 IF OFFICE RUN NO DIV NAME             
         BNE   *+10                                                             
         MVC   DWNFLD,LSTDIVNM            MOVE DIV NAME TO DWN FLD              
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD,LSTCLINM            MOVE CLI NAME TO DWN FLD              
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         CLI   QOPT3,C'A'                 PRINT ALL CLIENT INFO?                
         BE    DWNR30                                                           
         MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD                      
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CAT CODE              
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - DIV CODE              
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CLI CODE              
         B     DWNR40                                                           
*                                                                               
DWNR30   MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD                      
         MVC   DWNFLD(L'LSTCAT),LSTCAT    MOVE IN LAST CAT FOR DWNLD            
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CAT CODE              
*                                                                               
         MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD                      
         MVC   DWNFLD(L'LSTDIV),LSTDIV    MOVE IN LAST DIV FOR DWNLD            
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - DIV CODE              
*                                                                               
         MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD                      
         MVC   DWNFLD(L'LSTCLI),LSTCLI    MOVE IN LAST CLI FOR DWNLD            
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CLI CODE              
*                                                                               
DWNR40   TM    FLAG,FLGAP          IS THIS AN A/P RUN?                          
         BO    DWNR50                                                           
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,POB),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                    
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,PARB),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                   
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,PARC),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                   
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,PARN),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                   
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,PAP),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                    
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,PPI),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                    
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,PNCP),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                   
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
         B     DWNR130                                                          
*                                                                               
* A/P BREAKDOWN SECTION                                                         
*                                                                               
DWNR50   ZAP   PKTOTAL,PSP                ADD TO TOTAL                          
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,PSP),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                    
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         AP    PKTOTAL,PSS                ADD TO TOTAL                          
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,PSS),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                    
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         AP    PKTOTAL,PSU                ADD TO TOTAL                          
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,PSU),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                    
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         AP    PKTOTAL,PSV                ADD TO TOTAL                          
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,PSV),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                    
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         AP    PKTOTAL,PSZ                ADD TO TOTAL                          
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,PSZ),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                    
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,PKTOTAL),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD                      
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - NO FIELD              
         B     DWNR130                                                          
*                                                                               
* A/R BREAKDOWN SECTION                                                         
*                                                                               
DWNR60   CLI   BYTE,X'FF'                                                       
         BE    DWNR90                                                           
*                                                                               
         MVC   LSTCLINM,=CL36'******TOTAL******'                                
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD,LSTCLINM            MOVE CLI NAME TO DWN FLD              
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         CLI   BYTE,NCPTCLT               CLIENT TOTALS?                        
         BE    *+14                                                             
         MVC   LSTCLI,=C'RUN'             FOR RUN TOTALS                        
         B     DWNR70                                                           
*                                                                               
         CLI   QOPT3,C'A'                 PRINT ALL CLIENT INFO?                
         BE    DWNR70                                                           
         MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD                      
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CLI CODE              
         B     DWNR80                                                           
*                                                                               
DWNR70   MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD                      
         MVC   DWNFLD(L'LSTCLI),LSTCLI    MOVE CLI CODE TO DWN FLD              
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CAT CODE              
*                                                                               
DWNR80   MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD - SRACT FLD          
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CLI CODE              
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,PSRDB),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                  
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,PSRCR),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                  
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,PSRNT),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                  
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
         B     DWNR120             ALL THE REST ARE SPACE COLUMNS               
         DROP  R2                                                               
*                                                                               
         USING SRAD,R2                                                          
DWNR90   MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD,SRACLINM            MOVE CLI NAME TO DWN FLD              
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         CLI   QOPT3,C'A'                 PRINT ALL CLIENT INFO?                
         BE    DWNR100                                                          
         MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD                      
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CLI CODE              
         B     DWNR110                                                          
*                                                                               
DWNR100  MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD                      
         MVC   DWNFLD(L'SRACLI),SRACLI    MOVE CLI CODE TO DWN FLD              
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CAT CODE              
*                                                                               
DWNR110  MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD - SRACT FLD          
         MVC   DWNFLD(L'SRACCT),SRACCT    MOVE SR ACCT TO DWN FLD               
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CLI CODE              
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,SRADEB),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                 
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,SRACRD),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                 
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
         XC    DWNFLD,DWNFLD              CLEAR DOWNLOAD FIELD                  
         CURED (P8,PKAMT),(16,DWNFLD),2,ZERO=NOBLANK,MINUS=YES                  
         GOTO1 ADWNL,DMCB,(RC),DWNNUM     DOWNLOAD FIELD                        
*                                                                               
DWNR120  MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD - SRACT FLD          
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CLI CODE              
*                                                                               
         MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD - SRACT FLD          
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CLI CODE              
*                                                                               
         MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD - SRACT FLD          
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CLI CODE              
*                                                                               
         MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD - SRACT FLD          
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CLI CODE              
*                                                                               
         MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD - SRACT FLD          
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CLI CODE              
*                                                                               
         MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD - SRACT FLD          
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CLI CODE              
*                                                                               
         MVC   DWNFLD,SPACES              CLEAR OUT DWNFLD - SRACT FLD          
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT - CLI CODE              
*                                                                               
         CLI   BYTE,X'FF'          X'FF' MEANS WE ARE DWNLDING A LINE           
         BE    DWNR140                    THEN DOWNLOAD EOL                     
         CLI   BYTE,NCPTCLT               IF RUNNING CLIENT TOTALS              
         BE    DWNR140                    THEN DOWNLOAD EOL                     
         MVC   DWNFLD,SPACES              A/R IS LAST REPORT-IF RUN TOT         
         B     DWNR150                                                          
*                                                                               
DWNR130  CLI   QOPT6,C'Y'          SR REPORT IS THE LAST TO RUN                 
         BE    DWNR140               SO IF SR WAS SELECTED PUT EOR              
         CLI   QOPT6,C'A'            ONLY AFTER SR REPORT COMPLETES             
         BE    DWNR140                                                          
         CLC   LSTCATNM,=CL36'***RUN TOTAL***'                                  
         BE    DWNR150                                                          
DWNR140  MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOL     DOWNLOAD EOL MARKER                   
         B     DWNXIT                                                           
*                                                                               
DWNR150  MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOR     DOWNLOAD EOR MARKER                   
*                                                                               
DWNXIT   XMOD1                                                                  
         DROP  R2                                                               
                                                                                
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD HEADLINES (ONCE PER REPORT)                               *          
**********************************************************************          
         SPACE 1                                                                
DWNHEAD  NTR1                                                                   
         OI    DWNSTAT,DWNHDLN     SET SWITCH TO SHOW HDLNS WERE DWNLD          
*                                                                               
         LA    R0,HD1LNQ                 NUMBER OF HEADINGS IN LINE 1           
         USING HDLND,R2                                                         
         L     R2,AHD1TAB                FIRST HEADLINE TABLE                   
DWNH10   SR    R1,R1                     FIND MATCH IN TABLE                    
         IC    R1,DWNHDOPT               R1=(DWNLD OPT REG/OFF/AP/AR)           
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    HDNUM,0                   TEST IF OPT IS VAL FOR HEADING         
         BNO   DWNH20                                                           
         MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         MVC   DWNFLD(L'HDLAB),HDLAB     FIRST HEADLINE FIELDS                  
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                          
DWNH20   LA    R2,HDLNQ(R2)                                                     
         BCT   R0,DWNH10                                                        
*                                                                               
         MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         GOTO1 ADWNL,DMCB,(RC),DWNEOL    DOWNLOAD EOL MARKER                    
*                                                                               
         LA    R0,HD2LNQ                 NUMBER OF HEADINGS IN LINE 2           
         L     R2,AHD2TAB                SECOND HEADLINE TABLE                  
DWNH30   SR    R1,R1                     FIND MATCH IN TABLE                    
         IC    R1,DWNHDOPT               R1=(DWNLD OPT REG/OFF/AP/AR)           
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    HDNUM,0                   TEST IF OPT IS VAL FOR HEADING         
         BNO   DWNH40                                                           
         MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         MVC   DWNFLD(L'HDLAB),HDLAB     FIRST HEADLINE FIELDS                  
*                                                                               
         CLC   HDLAB,=CL20'PRIOR TO'     PUT IN REQUIRED DATES                  
         BNE   *+10                                                             
         MVC   DWNFLD+10(L'HEDSTDTE),HEDSTDTE                                   
*                                                                               
         CLC   HDLAB,=CL20'AS OF'        PUT IN REQUIRED DATES                  
         BNE   *+10                                                             
         MVC   DWNFLD+10(L'HEDENDTE),HEDENDTE                                   
*                                                                               
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT   DOWNLOAD TEXT                          
DWNH40   LA    R2,HDLNQ(R2)                                                     
         BCT   R0,DWNH30                                                        
*                                                                               
         MVC   DWNFLD,SPACES             SPACE OUT DOWNLOAD FIELD               
         GOTO1 ADWNL,DMCB,(RC),DWNEOL    DOWNLOAD EOL MARKER                    
*                                                                               
DWNHX    B     DWNXIT                                                           
         EJECT                                                                  
***********************************************************************         
* INIT - INITIALIZE STORAGE FOR EACH REQUEST                          *         
***********************************************************************         
         SPACE 1                                                                
INIT     DS    0D                                                               
         NMOD1 0,**INIT**                                                       
         L     RC,0(R1)                                                         
         LA    RE,IO               RE=A(IO AREA)                                
         LA    RF,IOLNQ            RF=(LENGTH OF IO AREA)                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         LA    RE,WISTR            RE=A(STORAGE AREA)                           
         LA    RF,WISTRLNQ         RF=(LENGTH OF STORAGE AREA)                  
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR STORAGE AREA                           
*                                                                               
         USING BOXD,R3                                                          
         L     R3,ADBOX                                                         
         MVI   BOXOFF,C'N'         TURN ON BOXES FOR EVERY REQ                  
         DROP  R3                                                               
*                                                                               
         MVC   HDLINE1(L'HDLINE1+L'HDLINE2+L'HDLINE3),SPACES                    
         MVC   MOASTR(L'MOASTR+L'MOAEND),SPACES                                 
*                                                                               
         USING BIND,R2                                                          
         L     R2,ANCPTAB          NET CASH POSITION TABLE                      
         XC    BININ,BININ         CLEAR BIN TABLE                              
         L     R2,ACLITAB          CLIENT TABLE                                 
         XC    BININ,BININ         CLEAR BIN TABLE                              
         L     R2,AGRPTAB          GROUP  TABLE                                 
         XC    BININ,BININ         CLEAR BIN TABLE                              
         L     R2,AOFFTAB          OFFICE TABLE                                 
         XC    BININ,BININ         CLEAR BIN TABLE                              
         L     R2,ARNKTAB          RANK TABLE                                   
         XC    BININ,BININ         CLEAR BIN TABLE                              
         L     R2,ASRATAB          SR LEDGER TABLE                              
         XC    BININ,BININ         CLEAR BIN TABLE                              
*                                                                               
         L     RF,ADWNBUF             RF=A(DOWNLOAD BUFFER)                     
         XC    0(L'DWNBUF,RF),0(RF)   CLEAR DOWNLOAD BUFFER                     
         XC    DWNSTAT,DWNSTAT        CLEAR DOWNSTAT BYTE                       
*                                                                               
         XMOD1                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD MODULE                                                    *          
*          PARM1 - RC                                                *          
*          PARM2 - ACTION                                            *          
**********************************************************************          
         SPACE 1                                                                
DWNL     DS    0D                                                               
         NMOD1 0,**DOWN**                                                       
         L     RC,0(R1)                                                         
         L     RF,4(R1)                                                         
         STC   RF,DWNMODE          SAVE CURRENT MODE                            
         USING DLCBD,R5                                                         
         L     R5,ADWNBUF                                                       
*                                                                               
         CLI   DWNMODE,DWNINIT     INITIALIZE                                   
         BE    DWNL10                                                           
         CLI   DWNMODE,DWNTEXT     DOWN-LOAD TEXT                               
         BE    DWNL20                                                           
         CLI   DWNMODE,DWNNUM      DOWN-LOAD NUMBER                             
         BE    DWNL30                                                           
         CLI   DWNMODE,DWNPACK     DOWN-LOAD NUMBER (PACKED)                    
         BE    DWNL40                                                           
         MVI   DLCBACT,DLCBEOL                                                  
         CLI   DWNMODE,DWNEOL      END OF LINE                                  
         BE    DWNL50                                                           
         MVI   DLCBACT,DLCBEOR                                                  
         CLI   DWNMODE,DWNEOR      END OF REPORT                                
         BE    DWNL50                                                           
         DC    H'0'                                                             
*                                                                               
* INITIALIZATION                                                                
*                                                                               
DWNL10   TM    DWNSTAT,DWNINTZ     HAS IT ALREADY BEEN INITIALIZED?             
         BO    DWNLX               YES - EXIT                                   
         MVI   DLCBACT,DLCBINIT    DOWN LOAD ACTION IS START                    
         LA    RE,XP               PRINT LINE                                   
         ST    RE,DLCBAPL                                                       
         LA    RE,DWNHOOK          POINT TO HOOK FOR APPLICATION                
         ST    RE,DLCBAPR                                                       
         MVC   DLCXMAXL,=Y(L'XP)                                                
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
         GOTO1 DLFLD,(R5)                                                       
         MVI   FORCEHED,C'Y'       EXCEPT FIRST TIME IN                         
         GOTO1 ACREPORT                                                         
         MVC   DLCBFLD,SPACES      MUST CLEAR FIRST TIME IN                     
*                                  TURN OFF DOWN-LOAD ROW FLDS AS C' '          
         OI    DWNSTAT,DWNINTZ     TURN ON INITIALIZED BYTE                     
         B     DWNLX               EXIT                                         
*                                                                               
* DOWNLOAD A RECORD - TEXT                                                      
*                                                                               
DWNL20   MVI   DLCBLEN,0           LEN OF FIELD-SET TO 0 (NO PADDING)           
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBTXT     TYPE   IS TEXT                               
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER                                                    
*                                                                               
DWNL30   MVI   DLCBLEN,0           LEN OF FIELD-SET TO 0 (NO PADDING)           
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBNUM     TYPE   IS NUMBER                             
         MVI   DLCBLEN,16          YES, USE MAXIMUM LENGTH OF NUMERICS          
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER (PACKED)                                           
*                                                                               
DWNL40   MVI   DLCBTYP,DLCBPACF    PACKED DATA                                  
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBLEN,L'PKFLDS    YES, USE MAXIMUM LENGTH OF NUMERICS          
         XC    DLCBFLD,DLCBFLD     CLEAN DWNLOAD FIELD TO 0'S                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         NC    DLCBFLD,DLCBFLD     YES, MAKE SURE NUMERIC FLD NOT ZEROS         
         BNZ   DWNL50              NOT  ZERO, DOWN-LOAD FIELD                   
         MVI   DLCBLEN,1           ZERO, SET LENGTH 1 TO DOWN-LOAD A 0          
*                                                                               
* END OF LINE/END OF RECORD                                                     
*                                                                               
DWNL50   GOTO1 DLFLD,(R5)          DOWN-LOAD FIELD                              
*                                                                               
DWNLX    XMOD1                                                                  
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD HOOK                                                      *          
**********************************************************************          
         SPACE 1                                                                
DWNHOOK  MVI   FORCEHED,C'N'       NEVER HEAD UP                                
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         L     RF,ACREPORT                                                      
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',ACCKDA,IO,DMWORK                  
DMX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R8                                                      
         USING BOXD,R4                                                          
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R8,VBIGPRNT                                                      
         L     R4,ADBOX                                                         
*                                                                               
         MVC   XHEAD1+136(L'HDLINE1),HDLINE1                                    
         MVC   XHEAD2+1(L'SVCPYNM),SVCPYNM                                      
         MVC   XHEAD2+69(L'HDLINE2),HDLINE2                                     
*                                                                               
         CLI   RCSUBPRG,0          REGULAR P/O                                  
         BE    *+12                                                             
         CLI   RCSUBPRG,4          REGULAR P/O WITHOUT CODES                    
         BNE   BXHK10                                                           
*                                                                               
         MVC   XHEAD8+47(8),HEDSTDTE               OPENING DATE                 
         MVC   XHEAD8+149(8),HEDENDTE              ENDING DATE                  
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
         CLI   RCSUBPRG,0          IF NOT REGULAR P/O                           
         BNE   *+16                DO NOT PRINT 1ST THREE COLUMNS               
         MVI   BOXCOLS+(PCAT-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PDIV-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PCLT-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(POPNBAL-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PARBIL-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PARCOL-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PARNET-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PPAY-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PPROD-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+(PNETCP-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+PRLNQ,C'R'                                               
*                                                                               
         BAS   RE,ARCOLS           SUB-DIVIDE A/R COLUMNS                       
*                                                                               
         CLI   RCSUBPRG,0          IF NOT REGULAR P/O                           
         BNE   BXXIT               THESE COLUMNS AREN'T PRINTING                
         BAS   RE,GRPCOLS          SUB-DIVIDE GROUP CODE COLUMN                 
*                                                                               
BXHK10   CLI   RCSUBPRG,1          A/P BREAKDOWN                                
         BE    *+12                                                             
         CLI   RCSUBPRG,5          A/P BREAKDOWN WITHOUT CODES                  
         BNE   BXHK20                                                           
*                                                                               
         MVC   XHEAD3+75(L'HDLINE3),HDLINE3                                     
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
         CLI   RCSUBPRG,1          IF NOT A/P BREAKDOWN W/ALL CLI INFO          
         BNE   *+16                DO NOT PRINT 1ST THREE COLUMNS               
         MVI   BOXCOLS+(PCAT-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PDIV-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PCLT-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PPAYSV-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PPAYSP-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PPAYSS-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PPAYSU-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PPAYSZ-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PPAYTOT-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+PPAYLNQ,C'R'                                             
*                                                                               
         CLI   RCSUBPRG,1          IF NOT A/P BREAKDOWN W/ALL CLI INFO          
         BNE   BXXIT               THESE COLUMNS AREN'T PRINTING                
         BAS   RE,GRPCOLS          SUB-DIVIDE GROUP CODE COLUMN                 
*                                                                               
BXHK20   CLI   RCSUBPRG,2          REGULAR P/O BY OFFICE                        
         BE    *+12                                                             
         CLI   RCSUBPRG,6          REGULAR P/O BY OFFICE WITHOUT CODES          
         BNE   BXHK30                                                           
*                                                                               
         MVC   XHEAD8+47(8),HEDSTDTE               OPENING DATE                 
         MVC   XHEAD8+149(8),HEDENDTE              ENDING DATE                  
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
         CLI   RCSUBPRG,2          IF NOT REGULAR BY OFFICE                     
         BNE   *+12                DO NOT PRINT 1ST THREE COLUMNS               
         MVI   BOXCOLS+(POFF-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PCLT-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(POPNBAL-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PARBIL-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PARCOL-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PARNET-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PPAY-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PPROD-PRTLNE-1),C'C'                                    
         MVI   BOXCOLS+(PNETCP-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+PRLNQ,C'R'                                               
*                                                                               
         BAS   RE,ARCOLS           SUB-DIVIDE A/R COLUMNS                       
*                                                                               
BXHK30   CLI   RCSUBPRG,3          A/P BREAKDOWN BY OFFICE                      
         BE    *+12                                                             
         CLI   RCSUBPRG,7          A/P BREAKDOWN BY OFFICE W/O CODES            
         BNE   BXHK40                                                           
*                                                                               
         MVC   XHEAD3+75(L'HDLINE3),HDLINE3                                     
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
         CLI   RCSUBPRG,3          IF NOT A/P BREAKDOWN BY OFFICE               
         BNE   *+12                DO NOT PRINT 1ST THREE COLUMNS               
         MVI   BOXCOLS+(POFF-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PCLT-PRTLNE-1),C'C'                                     
         MVI   BOXCOLS+(PPAYSV-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PPAYSP-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PPAYSS-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PPAYSU-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PPAYSZ-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PPAYTOT-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+PPAYLNQ,C'R'                                             
*                                                                               
BXHK40   CLI   RCSUBPRG,8          A/R BREAKDOWN BY ACCOUNT                     
         BNE   BXXIT                                                            
*                                                                               
         MVC   XHEAD3+69(L'HDLINE3),HDLINE3                                     
*                                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
         MVI   BOXCOLS+(PSRCLI-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PSRACCT-PRTLNE-1),C'C'                                  
         MVI   BOXCOLS+(PSRDEB-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PSRCRD-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+(PSRNET-PRTLNE-1),C'C'                                   
         MVI   BOXCOLS+PSRLNQ,C'R'                                              
*                                                                               
BXXIT    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
* SUB-DIVIDE A/R COLUMNS                                              *         
***********************************************************************         
         SPACE 1                                                                
ARCOLS   NTR1                                                                   
         MVI   XHEAD5+75,X'BF'            DASH                                  
         MVI   XHEAD5+92,X'BF'            DASH                                  
         MVI   XHEAD7+58,X'EB'            LEFT  T DASH                          
         MVI   XHEAD7+109,X'EC'           RIGHT T DASH                          
*                                                                               
         MVI   XHEAD7+59,X'BF'            PROPAGATE DASHES                      
         MVC   XHEAD7+60(49),XHEAD7+59                                          
         MVI   XHEAD7+75,X'CC'            TOP   T DASH                          
         MVI   XHEAD7+92,X'CC'            TOP   T DASH                          
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SUB-DIVIDE GROUP CODE COLUMN                                        *         
***********************************************************************         
         SPACE 1                                                                
GRPCOLS  NTR1                                                                   
         MVI   XHEAD5+32,X'BF'            DASH                                  
         MVI   XHEAD7+26,X'EB'            LEFT  T DASH                          
         MVI   XHEAD7+37,X'EC'            RIGHT T DASH                          
*                                                                               
         MVI   XHEAD7+27,X'BF'            PROPAGATE DASHES                      
         MVC   XHEAD7+28(9),XHEAD7+27                                           
         MVI   XHEAD7+32,X'CC'            TOP   T DASH                          
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
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
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUFFERS                                                             *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                  DOWNLOAD BUFFER                              
DWNBUF   DS    CL250                                                            
*                                                                               
* BINTABLE 1 - CLIENT TABLE                                                     
*                                                                               
         DC    C'**CLIENT**'                                                    
CLITAB   DS    0D                  BINTABLE CONSTANTS FOR TABLE 1               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(CLILNQ)             LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(CLIKLNQ)            KEY LENGTH                               
         DC    AL4(CLIMAX)             MAX IN TABLE                             
         DC    AL1(0)                  NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(0)                  DISPLACEMENT TO FIRST BUCKET             
         DS    (CLIMAX*CLILNQ)XL1      TABLE                                    
*                                                                               
* BINTABLE 2 - GROUP TABLE                                                      
*                                                                               
         DC    C'**GROUP**'                                                     
GRPTAB   DS    0D                  BINTABLE CONSTANTS FOR TABLE 2               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(GRPLNQ)             LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(GRPKLNQ)            KEY LENGTH                               
         DC    AL4(GRPMAX)             MAX IN TABLE                             
         DC    AL1(0)                  NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(0)                  DISPLACEMENT TO FIRST BUCKET             
         DS    (CLIMAX*CLILNQ)XL1      TABLE                                    
*                                                                               
* BINTABLE 3 - OFFICE TABLE                                                     
*                                                                               
         DC    C'**OFFICE**'                                                    
OFFTAB   DS    0D                  BINTABLE CONSTANTS FOR TABLE 3               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(OFFLNQ)             LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(OFFKLNQ)            KEY LENGTH                               
         DC    AL4(OFFMAX)             MAX IN TABLE                             
         DC    AL1(0)                  NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(0)                  DISPLACEMENT TO FIRST BUCKET             
         DS    (OFFMAX*OFFLNQ)XL1      TABLE                                    
*                                                                               
* BINTABLE 4 - NET CASH POSITION TABLE                                          
*                                                                               
         DC    C'**NCP**'                                                       
NCPTAB   DS    0D                  BINTABLE CONSTANTS FOR TABLE 4               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(NCPLNQ)             LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(NCPKLNQ)            KEY LENGTH                               
         DC    AL4(NCPMAX)             MAX IN TABLE                             
         DC    AL1(NCPBKCT)            NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(NCPBKT-NCPD)        DISPLACEMENT TO FIRST BUCKET             
         DS    (NCPMAX*NCPLNQ)XL1      TABLE                                    
*                                                                               
* BINTABLE 5 - RANK TABLE                                                       
*                                                                               
         DC    C'***RANK***'                                                    
RNKTAB   DS    0D                  BINTABLE CONSTANTS FOR TABLE 5               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(RNKLNQ)             LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(RNKKLNQ)            KEY LENGTH                               
         DC    AL4(RNKMAX)             MAX IN TABLE                             
         DC    AL1(RNKBKCT)            NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(RNKBKT-RNKD)        DISPLACEMENT TO FIRST BUCKET             
         DS    (RNKMAX*RNKLNQ)XL1      TABLE                                    
*                                                                               
* BINTABLE 6 - SR ACCOUNT TABLE                                                 
*                                                                               
         DC    C'**SRACCT**'                                                    
SRATAB   DS    0D                  BINTABLE CONSTANTS FOR TABLE 6               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(SRALNQ)             LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(SRAKLNQ)            KEY LENGTH                               
         DC    AL4(SRAMAX)             MAX IN TABLE                             
         DC    AL1(SRABKCT)            NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(SRABKT-SRAD)        DISPLACEMENT TO FIRST BUCKET             
         DS    (SRAMAX*SRALNQ)XL1      TABLE                                    
*                                                                               
NCPMAX   EQU  16000                                                             
RNKMAX   EQU  16000                                                             
SRAMAX   EQU  16000                                                             
CLIMAX   EQU   5000                                                             
GRPMAX   EQU   2000                                                             
OFFMAX   EQU   2000                                                             
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
ACWID    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
*                                                                               
VTYPES   DS    0A                                                               
ABINADD  DS    A                   BINADD ROUTINE                               
AGETRNK  DS    A                   GET RANK ROUTINE                             
ADWNL    DS    A                   DOWNLOAD ROUTINE                             
ADWNBUF  DS    A                   DOWNLOAD BUFFER                              
ADWNRTE  DS    A                   DOWNLOAD TOTALS ROUTINE                      
AINIT    DS    A                   INITIALIZE STORAGE ROUTINE                   
ASZTAB   DS    A                   SZ ACCOUNT TABLE                             
ATRNTAB  DS    A                   SZ TRANSACTION TYPE TABLE                    
*                                                                               
PRNTBL   DS    V                   PRINT DATA                                   
DLFLD    DS    V                   DOWNLOAD MODULE                              
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
DISP2    DS    H                   DISPLACEMENT TO ELEMENTS                     
TODAY    DS    CL6                 TODAY'S DATE PACKED                          
REQOFF   DS    CL2                 QOFFICE FROM REQUEST CARD                    
REQUL    DS    CL2                 QSELECT FROM REQUEST CARD UNIT/LDG           
*                                                                               
WISTR    DS    0C                  STORAGE TO BE CLEARED WITH EACH REQ          
PKSRCNT  DS    PL4                 SR ACCOUNT COUNTER                           
PKFLDS   DS    0PL8                PACKED FIELDS                                
PKAMT    DS    PL8                 TEMP STORAGE AMOUNT FOR BUCKETS              
PKNET    DS    PL8                 NET CASH POSITION                            
*                                                                               
PKCLTS   DS    0PL8          CLIENT LEVEL ACCUMULATORS                          
PKOB     DS    PL8                 OPENING BALANCE CLIENT TOTALS                
PKARCOL  DS    PL8                 RECEIVABLES - COLLECTED CLIENT TOTAL         
PKARBIL  DS    PL8                 RECEIVABLES - BILLED CLIENT TOTAL            
PKARNET  DS    PL8                 RECEIVABLES - NET CLIENT TOTAL               
PKAP     DS    PL8                 PAYABLES CLIENT TOTAL                        
PKPI     DS    PL8                 PRODUCTION INVENTORY CLIENT TOTAL            
PKNTCSH  DS    PL8                 NET CASH POSITION CLIENT TOTAL               
         ORG   PKOB          CLI LEV ACCUMULATORS FOR PAYABLES TOTS             
PKSP     DS    PL8                 PRINT PAYEES ACCUMULATOR                     
PKSS     DS    PL8                 SPOT PAYEES ACCUMULATOR                      
PKSU     DS    PL8                 NETWORK PAYEES ACCUMULATOR                   
PKSV     DS    PL8                 PROD VENDORS ACCUMULATOR                     
PKSZ     DS    PL8                 MEDIA CONTROL ACCUMULATOR                    
         DS    PL16                SPARE                                        
         ORG   PKOB          CLI LEV ACCUMULATORS FOR SR ACCOUNT TOTS           
PKSRCRD  DS    PL8                 SR ACCOUNT CREDIT                            
PKSRDEB  DS    PL8                 SR ACCOUNT DEBIT                             
PKSRNET  DS    PL8                 SR ACCOUNT NET                               
         DS    PL16                SPARE                                        
         DS    PL16                SPARE                                        
PKCLTLNQ EQU   *-PKCLTS                                                         
PKLNQ    EQU   (*-PKCLTS)/L'PKCLTS                                              
*                                                                               
PKDOS    DS    0PL8          DIVISION/OFFICE LEVEL ACCUMULATORS                 
PKOBDO   DS    PL8                 OPENING BALANCE CLIENT TOTALS                
PKARCDO  DS    PL8                 RECEIVABLES - COLLECTED CLIENT TOTAL         
PKARBDO  DS    PL8                 RECEIVABLES - BILLED CLIENT TOTAL            
PKARNDO  DS    PL8                 RECEIVABLES - NET CLIENT TOTAL               
PKAPDO   DS    PL8                 PAYABLES CLIENT TOTAL                        
PKPIDO   DS    PL8                 PRODUCTION INVENTORY CLIENT TOTAL            
PKNETDO  DS    PL8                 NET CSH POSITION CLIENT TOTAL                
         ORG   PKOBDO        DIV/OFF LEVEL AP BREAKDOWN ACCUMULATORS            
PKSPDO   DS    PL8                 PRINT PAYEES ACCUMULATOR BY CLIENT           
PKSSDO   DS    PL8                 SPOT PAYEES ACCUMULATOR BY CLIENT            
PKSUDO   DS    PL8                 NETWORK PAYEES ACCUMULATOR BY CLIENT         
PKSVDO   DS    PL8                 PROD VENDORS ACCUMULATOR BY CLIENT           
PKSZDO   DS    PL8                 MEDIA CONTROL ACCUMULATOR BY CLIENT          
         DS    PL16                    SPARE                                    
*                                                                               
PKCATS   DS    0PL8          CATEGORY LEVEL ACCUMULATORS                        
PKOBCT   DS    PL8                 OPENING BALANCE CLIENT TOTALS                
PKARCCT  DS    PL8                 RECEIVABLES - COLLECTED CLIENT TOTAL         
PKARBCT  DS    PL8                 RECEIVABLES - BILLED CLIENT TOTAL            
PKARNCT  DS    PL8                 RECEIVABLES - NET CLIENT TOTAL               
PKAPCT   DS    PL8                 PAYABLES CLIENT TOTAL                        
PKPICT   DS    PL8                 PRODUCTION INVENTORY CLIENT TOTAL            
PKNETCT  DS    PL8                 NET CSH POSITION CLIENT TOTAL                
         ORG   PKOBCT        CATEGORY LEVEL AP BREAKDOWN ACCUMULATORS           
PKSPCT   DS    PL8                 PRINT PAYEES ACCUMULATOR BY CLIENT           
PKSSCT   DS    PL8                 SPOT PAYEES ACCUMULATOR BY CLIENT            
PKSUCT   DS    PL8                 NETWORK PAYEES ACCUMULATOR BY CLIENT         
PKSVCT   DS    PL8                 PROD VENDORS ACCUMULATOR BY CLIENT           
PKSZCT   DS    PL8                 MEDIA CONTROL ACCUMULATOR BY CLIENT          
         DS    PL16                    SPARE                                    
*                                                                               
PKTOTS   DS    0PL8          OVERALL TOTAL ACCUMULATORS                         
PKOBTOT  DS    PL8                 OPENING BALANCE TOTALS                       
PKARCTOT DS    PL8                 RECEIVABLES - COLLECTED TOTAL                
PKARBTOT DS    PL8                 RECEIVABLES - BILLED TOTAL                   
PKARNTOT DS    PL8                 RECEIVABLES - NET TOTAL                      
PKAPTOT  DS    PL8                 PAYABLES TOTAL                               
PKPITOT  DS    PL8                 PRODUCTION INVENTORY TOTAL                   
PKNETOT  DS    PL8                 NET CASH POSITION TOTAL                      
         ORG   PKOBTOT       OVERALL TOTAL AP BREAKDOWN ACCUMULATORS            
PKSPTOT  DS    PL8                 PRINT PAYEES TOTAL                           
PKSSTOT  DS    PL8                 SPOT PAYEES TOTAL                            
PKSUTOT  DS    PL8                 NETWORK PAYEES TOTAL                         
PKSVTOT  DS    PL8                 PROD VENDORS TOTAL                           
PKSZTOT  DS    PL8                 MEDIA CONTROL TOTAL                          
         DS    PL8                 SPARE                                        
PKTOTAL  DS    PL8                 SPARE                                        
         ORG   PKOBTOT       OVERALL TOTAL AR BREAKDOWN ACCUMULATORS            
PKSRCTOT DS    PL8                 SR ACCOUNT CREDIT TOTALS                     
PKSRDTOT DS    PL8                 SR ACCOUNT DEBIT  TOTALS                     
PKSRNTOT DS    PL8                 SR ACCOUNT NET    TOTALS                     
         DS    PL16                SPARE                                        
         DS    PL16                SPARE                                        
PKTOTSQ  EQU   (*-PKTOTS)/L'PKTOTS                                              
*                                                                               
SVPKFLDS DS    CL(PKCLTLNQ)        SAVED AREA FOR PACKED TOTALS                 
PKFLDLNQ EQU   (*-PKFLDS)/L'PKFLDS                                              
*                                                                               
* THE FOLLOWING FIELDS (LSTXXX) MUST MATCH SORT RECORD (NCPD)                   
*                                                                               
LSTFLDS  DS    0C                                                               
LSTCATNM DS    CL36                LAST CATAGORY NAME                           
LSTDIVNM DS    0CL36               LAST DIVISION NAME                           
LSTOFFNM DS    CL36                LAST OFFICE   NAME                           
LSTCLINM DS    CL36                LAST CLIENT   NAME                           
LSTGCO   DS    0CL12                                                            
LSTGRP   DS    0CL7                LAST GROUP                                   
LSTCAT   DS    CL4                 LAST CATEGORY                                
LSTDIV   DS    CL3                 LAST DIVISION                                
LSTCLI   DS    CL3                 LAST CLIENT                                  
LSTOFF   DS    CL2                 LAST OFFICE                                  
LSTLNQ   EQU   *-LSTFLDS                                                        
*                                                                               
SVRNKKEY DS    0CL6                                                             
SVRNKCLI DS    0CL3                SAVED AREA FOR RANK CLIENT                   
SVRNKCAT DS    CL4                 SAVED AREA FOR RANK CATEGORY                 
SVRNKOFF DS    CL2                 SAVED AREA FOR RANK OFFICE                   
*                                                                               
SVCPYNM  DS    CL30                SAVED AREA FOR COMPANY                       
SVTRNKEY DS    CL(TRNKSBR-TRNKEY)  SAVED AREA FOR TRANS KEY(CRD ONLY)           
SVPK     DS    XL1                 SAVED AREA FOR PRINT FIELD OFFSET            
SVLDG    DS    CL1                 SAVED AREA FOR LEDGER RUN                    
SVGRP    DS    0CL7                GROUP CODE                                   
SVCAT    DS    CL4                 GROUP CODE - CATEGORY                        
SVDIV    DS    CL3                 GROUP CODE - DIVISION                        
SVCATNM  DS    CL36                GROUP - CATEGORY NAME                        
SVDIVNM  DS    CL36                GROUP - DIVISION NAME                        
SVCLI    DS    CL3                 CLIENT CODE                                  
SVCLINM  DS    CL36                CLIENT NAME                                  
SVOFF    DS    CL2                 CLIENT OFFICE                                
SVOFFNM  DS    CL36                CLIENT OFFICE NAME                           
SVDSC    DS    CL6                 SAVED AREA FOR DESCRIPTION GROUP/OFF         
SVKEY    DS    CL49                SAVED AREA FOR KEY                           
SVPRNT1  DS    CL50                SAVED AREA FOR HEADINGS 1                    
SVPRNT2  DS    CL50                SAVED AREA FOR HEADINGS 2                    
*                                                                               
SVACCT   DS    CL12                SAVED AREA FOR ACCOUNT IF ANY                
DWNFLD   DS    CL36                SAVED AREA FOR FIELD TO BE DWNLOADED         
*                                                                               
PRTSIZE  DS    XL1                 PRINT AREA LENGTH                            
*                                                                               
HDLINE1  DS    0CL20               HEADLINE # 1 INFO (FORMAT)                   
HDLNFRM  DS    CL1                 FORMAT OF REPORT - QOPT1                     
         DS    CL3                 SPACE                                        
HDLNDSC  DS    CL16                FORMAT DESCRIPTION                           
HDLINE2  DS    CL30                HEADLINE # 2 INFO DATES                      
HDLINE3  DS    CL25                HEADLINE # 3 INFO (LEDGER BREAKDOWN)         
*                                                                               
DWNSTAT  DS    XL1                 DOWNLOAD STATUS                              
DWNINTZ  EQU   X'80'               DOWNLOAD INITIALIZED                         
DWNHDLN  EQU   X'40'               DOWNLOAD HEADLINES                           
*                                                                               
DWNMODE  DS    XL1                 DOWNLOAD MODE                                
DWNINIT  EQU   1                      DOWN-LOAD INITIALIZATION                  
DWNEOL   EQU   2                      MARK END OF LINE                          
DWNEOR   EQU   3                      MARK END OF REPORT                        
DWNTEXT  EQU   4                      DOWN-LOAD TEXT                            
DWNNUM   EQU   5                      DOWN-LOAD NUMBER                          
DWNPACK  EQU   6                      DOWN-LOAD NUMBER (PACKED)                 
*                                                                               
COMPLEN  DS    XL1                 COMPARE LENGTH                               
ELCODE   DS    XL1                 ELEMENT CODE FOR GETEL                       
OFFPOS   DS    XL1                 SJ CLIENT OFFICE POSITION                    
CLIPOS   DS    XL1                 SR CLIENT POSITION                           
*                                                                               
FLAG     DS    XL1                 SPECIFIC TO EACH REQUEST                     
FLGACCT  EQU   X'80'               ACCOUNT HAS BEEN SPECIFIED - FILTER          
FLGPRT   EQU   X'40'               SOMETHING WAS PRINTED                        
FLGAP    EQU   X'20'               TOTAL FOR AP BREAKDOWN                       
FLGNUOFF EQU   X'10'               COMPANY IS ON NEW OFFICE                     
FLGREQ   EQU   X'08'               SET AT EACH NEW REQUEST                      
FLGGRP   EQU   X'04'               CLIENT IS PART OF GROUP                      
FLGAR    EQU   X'02'               TOTAL FOR AR BREAKDOWN                       
FLGCLI   EQU   X'01'               CLIENT NOT FOUND                             
*                                                                               
DWNHDOPT DS    XL1                 DOWNLOAD ROUTINES HEADING OPTION             
DWNRG    EQU   X'80'               REGULAR RUN                                  
DWNRGOFF EQU   X'40'               REGULAR RUN BY OFFICE                        
DWNAP    EQU   X'20'               A/P BREAKDOWN                                
DWNAPOFF EQU   X'10'               A/P BREAKDOWN BY OFFICE                      
DWNAR    EQU   X'08'               A/R BREAKDOWN                                
*                                                                               
MSG      DS    CL10                                                             
HEDSTDTE DS    CL8                 REQUEST START DATE FOR HEADLINES             
HEDENDTE DS    CL8                 REQUEST END   DATE FOR HEADLINES             
WKDAY    DS    CL3                 WEEK DAY RETURNED BY GETDAY CALL             
STDTE    DS    XL2                 REQUEST START DATE                           
ENDTE    DS    XL2                 REQUEST END   DATE                           
MOASTR   DS    XL2                 REQUEST MOA START DATE                       
MOAEND   DS    XL2                 REQUEST MOA END   DATE                       
*                                                                               
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DA       DS    F                   DISK ADDRESS                                 
*                                                                               
EOF      EQU   X'FF'               END OF FILE MARKER                           
ALL      EQU   X'FF'               EVERYTHING                                   
*                                                                               
LEVELS   DS    0XL16                                                            
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVADSC  DS    CL15                LEVEL A NAME                                 
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVBDSC  DS    CL15                LEVEL B NAME                                 
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVCDSC  DS    CL15                LEVEL C NAME                                 
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVDDSC  DS    CL15                LEVEL D NAME                                 
LEVLNQ   EQU   *-LEVELS                                                         
*                                                                               
LEVLNQS  DS    0XL1                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
LEVSCDE  DS    0CL12               LEVEL CODES                                  
LEVACDE  DS    CL12                LEVEL A CODE                                 
LEVBCDE  DS    CL12                LEVEL B CODE                                 
LEVCCDE  DS    CL12                LEVEL C CODE                                 
LEVDCDE  DS    CL12                LEVEL D CODE                                 
LEVSLNQ  EQU   *-LEVSCDE                                                        
*                                                                               
OFFSET   DS    XL1                 OFFSET TO LOWEST LEVEL                       
LOWLVLN  DS    XL1                 LOWEST LEVEL LENGTH                          
*                                                                               
NCPWRK   DS    CL(NCPLNQ)          BINSEARCH WORK AREA - NCP     TABLE          
CLIWRK   DS    CL(CLILNQ)          BINSEARCH WORK AREA - CLIENT  TABLE          
GRPWRK   DS    CL(NCPLNQ)          BINSEARCH WORK AREA - GROUP   TABLE          
OFFWRK   DS    CL(OFFLNQ)          BINSEARCH WORK AREA - OFFICE  TABLE          
RNKWRK   DS    CL(RNKLNQ)          BINSEARCH WORK AREA - RANK    TABLE          
SRAWRK   DS    CL(SRALNQ)          BINSEARCH WORK AREA - SR ACCT TABLE          
WISTRLNQ EQU   *-WISTR             STORAGE LENGTH                               
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* SZTAB DSECT                                                         *         
***********************************************************************         
         SPACE 1                                                                
SZTABD   DSECT                                                                  
SZID     DS    CL2                 ALPHAID                                      
SZACCT   DS    CL12                ACCOUNT                                      
SZSTAT   DS    XL1                 STATUS BYTE                                  
SZTRN    EQU   X'80'               INCLUDE ALL TRANSACTIONS                     
SZTABLNQ EQU   *-SZID                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT TO PACKED FIELDS (CAT/DIV/CLI LEVELS)                         *         
***********************************************************************         
         SPACE 1                                                                
PACKD    DSECT                                                                  
POB      DS    CL8                 OPENING BALANCE                              
PARC     DS    CL8                 RECEIVABLES - COLLECTED                      
PARB     DS    CL8                 RECEIVABLES - BILLED                         
PARN     DS    CL8                 RECEIVABLES - NET                            
PAP      DS    CL8                 PAYABLES TOTAL                               
PPI      DS    CL8                 PRODUCTION INVENTORY TOTAL                   
PNCP     DS    CL8                 NET CASH POSTION TOTAL                       
         ORG   POB                 PAYABLES BREAKDOWN                           
PSP      DS    CL8                 PRINT PAYEES                                 
PSS      DS    CL8                 SPOT PAYEES                                  
PSU      DS    CL8                 NETWORK PAYEES                               
PSV      DS    CL8                 PRODUCTION VENDORS                           
PSZ      DS    CL8                 MEDIA CONTROL                                
         DS    CL16                                                             
         ORG   POB                 RECEIVABLES BREAKDOWN                        
PSRCR    DS    CL8                 SR ACCOUNT CREDITS                           
PSRDB    DS    CL8                 SR ACCOUNT DEBITS                            
PSRNT    DS    CL8                 SR ACCOUNT NET (DB-CR)                       
         DS    CL32                PRODUCTION VENDORS                           
PACKLNQ  EQU   *-POB                                                            
PCKQ     EQU   (*-POB)/L'POB                                                    
         EJECT                                                                  
***********************************************************************         
* DSECT TO PACKED FIELDS (TOTAL LEVEL)                                *         
***********************************************************************         
         SPACE 1                                                                
PCKTOTD  DSECT                                                                  
POBTOT   DS    CL16                OPENING BALANCE                              
PARCTOT  DS    CL16                RECEIVABLES - COLLECTED                      
PARBTOT  DS    CL16                RECEIVABLES - BILLED                         
PARNTOT  DS    CL16                RECEIVABLES - NET                            
PAPTOT   DS    CL16                PAYABLES TOTAL                               
PPITOT   DS    CL16                PRODUCTION INVENTORY TOTAL                   
PNCPTOT  DS    CL16                NET CASH POSTION TOTAL                       
         ORG   POBTOT              PAYABLES BREAKDOWN                           
PSPTOT   DS    CL16                PRINT PAYEES                                 
PSSTOT   DS    CL16                SPOT PAYEES                                  
PSUTOT   DS    CL16                NETWORK PAYEES                               
PSVTOT   DS    CL16                PRODUCTION VENDORS                           
PSZTOT   DS    CL16                MEDIA CONTROL                                
         DS    CL32                                                             
         ORG   POBTOT              RECEIVABLES BREAKDOWN                        
PSRCTOT  DS    CL16                SR ACCOUNT CREDITS                           
PSRDTOT  DS    CL16                SR ACCOUNT DEBITS                            
PSRNTOT  DS    CL16                SR ACCOUNT NET (DB-CR)                       
         DS    CL64                PRODUCTION VENDORS                           
PCKTLNQ  EQU   *-POBTOT                                                         
PCKTOTQ  EQU   (*-POBTOT)/L'POBTOT                                              
         EJECT                                                                  
***********************************************************************         
* DSECT TO CUTOFF DATE TABLE                                          *         
***********************************************************************         
         SPACE 1                                                                
CPYTBD   DSECT                                                                  
CPYID    DS    CL2                 ALPHA ID                                     
CPYNME   DS    CL30                COMPANY NAME                                 
CPYLNQ   EQU   *-CPYTBD                                                         
*                                                                               
* DSECT TO FILTER TABLE                                                         
*                                                                               
FLTTBD   DSECT                                                                  
FLTID    DS    CL2                 ALPHA ID                                     
FLTLDG   DS    CL1                 LEDGER                                       
FLTLNQ   EQU   *-FLTTBD                                                         
*                                                                               
* DSECT TO COVER LEDGER TABLE                                                   
*                                                                               
LDGTBD   DSECT                                                                  
LDGLDG   DS    C                   LEDGER                                       
LDGST    DS    X                   LEDGER STATUS                                
LDGACT   EQU   X'80'               LEDGER IS ACTIVE                             
LDGDISP  DS    AL1                 DISPLACEMENT INTO PACKED FIELDS              
LDGTBLNQ EQU   *-LDGTBD                                                         
*                                                                               
* DSECT TO COVER PAYABLES LEDGER TABLE                                          
*                                                                               
PAYD     DSECT                                                                  
PAYUL    DS    CL2                 UNIT/LEDGER OF PAYABLES                      
PAYPK    DS    XL1                 DISPLACEMENT IN PRINT LINE                   
PAYDLNQ  EQU   *-PAYD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN NET CASH POSITION TABLE                          *         
***********************************************************************         
         SPACE 1                                                                
NCPD     DSECT                                                                  
NCPCATNM DS    CL36                GROUP CATEGORY NAME                          
NCPREC   DS    XL1                                                              
NCPRGRP  EQU   X'FF'               RECORD IS A GROUPED RECORD                   
NCPDIVNM DS    CL36                GROUP DIVISION NAME                          
NCPCLTNM DS    CL36                CLIENT NAME                                  
NCPGKEY  DS    0CL10               GROUP KEY-GROUP CODE AND CLIENT CODE         
NCPGRP   DS    0CL7                CLIENT GROUP                                 
NCPCAT   DS    CL4                 CLIENT GROUP - CATEGORY                      
NCPDIV   DS    CL3                 CLIENT GROUP - DIVISION                      
NCPCLI   DS    CL3                 CLIENT CODE                                  
NCPOFF   DS    CL2                 CLIENT OFFICE                                
NCPUL    DS    CL2                 ORIGINATING UNIT AND LEDGER                  
NCPKLNQ  EQU   *-NCPD              LENGTH OF KEY                                
NCPTYP   DS    CL1                 RECORD TYPE                                  
NCPTCAT  EQU   1                   GROUP RECORD - CATEGORY                      
NCPTDIV  EQU   2                   GROUP RECORD - DIVISION                      
NCPTCLT  EQU   3                   CLIENT DETAIL RECORD                         
NCPOFDSP EQU   *-NCPD              DISPLACEMENT TO XSORT FIELD-OFF NAM          
NCPOFFNM DS    CL36                CLIENT OFFICE NAME                           
NCPOFLNQ EQU   *-NCPOFFNM          LENGTH OF XSORT FIELD-OFFICE NAME            
NCPNTDSP EQU   *-NCPD              DISPLACEMENT TO XSORT FIELD-NET POS          
NCPNETPR DS    XL1                 NET CASH PREFIX X'00'-POS X'FF'-NEG          
NCPNET   DS    PL8                 NET CASH FOR RANKING                         
NCPNTLNQ EQU   *-NCPNETPR          LENGTH OF XSORT FIELD-NET POS                
*                                                                               
NCPBKT   DS    0PL8                START OF BUCKETS                             
NCPOBDEB DS    PL8                 OPENING BALANCE DEBITS                       
NCPBKLN  EQU   *-NCPBKT            BUCKET LENGTH                                
NCPOBCRD DS    PL8                 OPENING BALANCE CREDITS                      
*                                                                               
NCPARBKT DS    0PL8                A/R BUCKETS                                  
NCPARDEB DS    PL8                 A/R DEBITS                                   
NCPARCRD DS    PL8                 A/R CREDITS                                  
*                                                                               
NCPAPBKT DS    0PL8                A/P BUCKETS                                  
NCPAPDEB DS    PL8                 A/P DEBITS                                   
NCPAPCRD DS    PL8                 A/P CREDITS                                  
*                                                                               
NCPPIBKT DS    0PL8                PRODUCTION INVENTORY BUCKETS                 
NCPPIDEB DS    PL8                 PRODUCTION INVENTORY DEBITS                  
NCPPICRD DS    PL8                 PRODUCTION INVENTORY CREDITS                 
NCPBKCT  EQU   (*-NCPBKT)/NCPBKLN  NUMBER OF BUCKETS                            
NCPLNQ   EQU   *-NCPD              LENGTH                                       
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN CLIENT TABLE                                     *         
***********************************************************************         
         SPACE 1                                                                
CLID     DSECT                                                                  
CLICDE   DS    CL3                 CLIENT CODE                                  
CLIKLNQ  EQU   *-CLID              LENGTH OF KEY                                
CLIOFF   DS    CL2                 CLIENT OFFICE                                
CLINME   DS    CL36                CLIENT NAME                                  
CLILNQ   EQU   *-CLID              LENGTH OF ENTRY                              
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER GROUP TABLE                                          *         
***********************************************************************         
         SPACE 1                                                                
GRPD     DSECT                                                                  
GRPCDE   DS    0CL7                GROUP CODE                                   
GRPCAT   DS    CL4                 LEVEL 1 - CATEGORY                           
GRPDIV   DS    CL3                 LEVEL 2 - DIVISION                           
GRPCLT   DS    CL3                 CLIENT CODE                                  
GRPKLNQ  EQU   *-GRPD              LENGTH OF KEY                                
GRPCDCNM DS    CL36                GROUP CAT/DIV/CLIENT NAME                    
GRPLNQ   EQU   *-GRPD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN OFFICE TABLE                                     *         
***********************************************************************         
         SPACE 1                                                                
OFFD     DSECT                                                                  
OFFCDE   DS    CL2                 OFFICE CODE                                  
OFFKLNQ  EQU   *-OFFD              LENGTH OF KEY                                
OFFNME   DS    CL36                OFFICE NAME                                  
OFFLNQ   EQU   *-OFFD              LENGTH OF ENTRY                              
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN RANK TABLE (NET POSITIONING)                     *         
***********************************************************************         
         SPACE 1                                                                
RNKD     DSECT                                                                  
RNKKEY   DS    0CL6                RANK KEY (CAT OR CLI/OFF OR OFF)             
RNKCLI   DS    0CL3                CLIENT CODE (IF NO GROUP)                    
RNKCAT   DS    CL4                 CATEGORY CODE                                
RNKOFF   DS    CL2                 OFFICE CODE (W/CAT/CLI ONLY)                 
RNKKLNQ  EQU   *-RNKD              LENGTH OF KEY                                
RNKBKT   DS    PL8                 BUCKET                                       
RNKBKLN  EQU   *-RNKBKT            BUCKET LENGTH                                
RNKBKCT  EQU   (*-RNKBKT)/RNKBKLN  NUMBER OF BUCKETS                            
RNKLNQ   EQU   *-RNKD              LENGTH OF ENTRY                              
         EJECT                                                                  
***********************************************************************         
* DSECT FOR ENTRY IN SR ACCOUNT TABLE                                 *         
***********************************************************************         
         SPACE 1                                                                
SRAD     DSECT                                                                  
SRACLINM DS    CL36                CLIENT NAME                                  
SRACLI   DS    CL3                 CLIENT CODE                                  
SRACCT   DS    CL12                SR ACCOUNT CODE                              
SRAKLNQ  EQU   *-SRAD              LENGTH OF KEY                                
SRABKT   DS    0PL8                BUCKET                                       
SRACRD   DS    PL8                 CREDIT                                       
SRABKLN  EQU   *-SRABKT            BUCKET LENGTH                                
SRADEB   DS    PL8                 DEBIT                                        
SRABKCT  EQU   (*-SRABKT)/SRABKLN  NUMBER OF BUCKETS                            
SRALNQ   EQU   *-SRAD              LENGTH OF ENTRY                              
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
***********************************************************************         
* DSECT TO COVER DOWNLOAD HEADLINES TABLE                             *         
***********************************************************************         
         SPACE 1                                                                
HDLND    DSECT                                                                  
HDNUM    DS    X                   EQUATED NUMBER FOR OUTPUT                    
HDLAB    DS    CL20                HEADLINE LABEL                               
HDLNQ    EQU   *-HDLND             THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
* PRINT DESCT                                                         *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLNE   DS    0C                  PRINT LINE # CLIENT CODES                    
         DS    CL2                                                              
PCATNM   DS    CL24                GROUP CATEGORY NAME                          
         DS    CL1                                                              
         ORG   PCATNM                                                           
         DS    CL1                                                              
PDIVNM   DS    CL23                DIVISION NAME                                
         DS    CL1                                                              
         ORG   PCATNM                                                           
         DS    CL2                                                              
PCLTNM   DS    CL22                CLIENT NAME                                  
         DS    CL1                                                              
PCAT     DS    CL4                 GROUP CATEGORY                               
         DS    CL2                                                              
PDIV     DS    CL3                 GROUP DIVISION                               
         DS    CL2                                                              
PCLT     DS    CL3                 GROUP CLIENT                                 
         DS    CL1                                                              
         ORG   PCATNM                                                           
POFFNM   DS    CL30                CLIENT OFFICE NAME                           
         DS    CL2                                                              
POFF     DS    CL2                 CLIENT OFFICE                                
         DS    CL6                                                              
POPNBAL  DS    CL16                OPENING BALANCE                              
         DS    CL1                                                              
PREC     DS    0CL50               RECEIVABLES                                  
PARBIL   DS    CL16                RECEIVABLES - BILLED                         
         DS    CL1                                                              
PARCOL   DS    CL16                RECEIVABLES - COLLECTED                      
         DS    CL1                                                              
PARNET   DS    CL16                RECEIVABLES - NET                            
         DS    CL1                                                              
PPAY     DS    CL16                PAYABLES                                     
         DS    CL1                                                              
PPROD    DS    CL16                PRODUCTION INVENTORY                         
         DS    CL1                                                              
PNETCP   DS    CL16                NET CASH POSITION                            
         DS    CL2                                                              
PRLNQ    EQU   *-PRTLNE                                                         
         ORG   POPNBAL                                                          
PPAYSP   DS    CL16                PRINT PAYEES PAYABLES                        
         DS    CL3                                                              
PPAYSS   DS    CL16                SPOT PAYEES PAYABLES                         
         DS    CL3                                                              
PPAYSU   DS    CL16                NETWORK PAYEES PAYABLES                      
         DS    CL3                                                              
PPAYSV   DS    CL16                PROD VENDORS PAYABLES                        
         DS    CL3                                                              
PPAYSZ   DS    CL16                MEDIA CONTROL PAYABLES                       
         DS    CL3                                                              
PPAYTOT  DS    CL16                TOTAL OF ALL LEDGERS FOR CLIENT              
         DS    CL3                                                              
PPAYLNQ  EQU   *-PRTLNE                                                         
         ORG   PRTLNE              SR ACCOUNT BREAKDOWN                         
         DS    CL2                                                              
PSRCLINM DS    CL36                SR CLIENT NAME                               
         DS    CL3                                                              
PSRCLI   DS    CL3                 SR ACCOUNT CLIENT                            
         DS    CL4                                                              
PSRACCT  DS    CL12                SR ACCOUNT CODE                              
         DS    CL3                                                              
PSRDEB   DS    CL16                SR DEBIT                                     
         DS    CL3                                                              
PSRCRD   DS    CL16                SR CREDIT                                    
         DS    CL3                                                              
PSRNET   DS    CL16                SET NET                                      
         DS    CL3                                                              
PSRLNQ   EQU   *-PRTLNE                                                         
         EJECT                                                                  
***********************************************************************         
*              ++INCLUDES                                             *         
***********************************************************************         
         SPACE 1                                                                
* DDCNTRL                                                                       
*                                                                               
       ++INCLUDE DDCNTRL                                                        
*                                                                               
* DMWRKRK                                                                       
*                                                                               
       ++INCLUDE DMWRKRK                                                        
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* ACBIGPRINTD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDDLCB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018ACREPWI02 01/03/13'                                      
         END                                                                    
