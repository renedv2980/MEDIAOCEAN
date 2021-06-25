*          DATA SET RECNT63    AT LEVEL 127 AS OF 08/15/13                      
*PHASE T80263A,*                                                                
*INCLUDE OUTDAY                                                                 
*INCLUDE REPRTCOV                                                               
*INCLUDE REGENPBY                                                               
*INCLUDE REGENBUF                                                               
*INCLUDE REGENTL2                                                               
*INCLUDE UNTIME                                                                 
         TITLE 'T80263 - RECNT63 - ELECTRONIC CONTRACT'                         
*********************************************************************           
*                                                                   *           
*        RECNT63 --- CONTROL PROCESSING FOR SENDING AN ORDER        *           
*                    !!! OVERLAY IS CORE RESIDENT !!!               *           
* ----------------------------------------------------------------- *           
* REFER TO RECNTHIST FOR PAST HISTORY                               *           
*                                                                   *           
* 18FEB13 BOB ADD DEMOS AND GRPS TO REPORTS                         *           
* 03JUL12 SKU REMOVE KSL HARDCODE (0370775N)                        *           
* 04MAY11 SKU SUPPORT FORCED OCM NONDISCRIMINATION CLAUSE           *           
* 28JUN07 HQ  HARD CODE KSL TO ALWAYS PRINT RECAP                   *           
* 17MAY05 BU  WO /COL. TO GET 'E' FORMAT WORKSHEET                  *           
* 28MAR05 BU  VCI/W.O. TO GET 'U' FORMAT WORKSHEET                  *           
* 05MAR05 BU  VSS/IBS  TO GET 'J' FORMAT WORKSHEET                  *           
* 02FEB05 BU  W.O. (N) TO GET ENTERPRISE (KAMAN) WORKSHEET          *           
* 18OCT04 BU  MARKETRON WORKSHEET FORMAT                            *           
* 11AUG04 HQ  XML COMMENT                                           *           
* 10FEB04 HQ  FIX 8 DIGIT CONTRACT # COMBO ORDER WORKSHEET          *           
* 10FEB04 BU  VSS TO RECEIVE ENTERPRISE (KAMAN) WORKSHEET           *           
* 07JAN04 BU  OSI TO RECEIVE ENTERPRISE (KAMAN) WORKSHEET           *           
* 02SEP03 BU  OSI TO FOLLOW WIDE ORBIT PATH                         *           
* 22MAY03 SKU SELF APPLY                                            *           
* 04JUN03 SKU ADD MAKEGOOD PROGRAM NAME                             *           
* 01JUL02 HQ  DEMO CAT AND VALUE DISPLAY                            *           
* 21MAR02 SKU SUPPORT REPLACEMENT OFFERS PRINTING                   *           
* 06NOV02 HQ  FIX TRADE ORDER WEEKLY TOTAL                          *           
* 29AUG02 SKU FIX MG RANGE TOTAL                                    *           
* 22AUG02 HQ  FIX MG GROUP OFFER END DATE                           *           
* 15JUL02 BU  WEB STATION: CHANGES ONLY                             *           
* 11JUL02 BU  VCI/BDE                                               *           
* 25OCT01 RHV EOP FOR VCI                                           *           
* 22AUG01 SKU PREVENT RUNAWAY LOOP IN TOTLTOTL ROUTINE              *           
* 20JUL01 SKU MAKEGOOD TOTAL BUG FIX                                *           
* 06OCT00 SKU MAKEGOOD FORMAT CHANGES                               *           
* 27JUL00 BU  TRADE PROCESSING                                      *           
* 05MAY00 SKU NEW MULTI-MAKEGOOD PRINTING SUPPORT                   *           
* 13MAR00 SCH CHANGE 'S' TYPE REPORT TO VCI STAR II REPORT          *           
* 01NOV99 SKU SKIP MAKEGOOD PRINTING IF GROUP RECORD MISSING        *           
* 11MAR99 RHV REPL AGY ADDR FROM ADV REC                            *           
* 13JAN99 RHV ENTERPRISE II (H) TRAFFIC FORMAT                      *           
* 30DEC98 RHV HOME MARKET - STA WORKSHEET TO REP'S PQ               *           
* 22DEC98 RHV SUPPORT NON-DELETED CANCELLED LINES                   *           
* 04DEC98 RHV SENDPASS=4 - FAX WORKSHEET TO STATION                 *           
* 10SEP98 JRD UPDATE SONNET PRINTING                                *           
* 24JUN98 SKU ADD FORMAT O FOR PAXON                                *           
* 01JUN98 RHV PRINT COVERSHEET ON ORDER WORKSHEETS                  *           
* 06JAN97 SKU DDS ONLY DARE AGENCY LINK DISPLAY                     *           
* 23JUL97 SKU 4K CONTRACT SUPPORT                                   *           
* 19DEC97 JRD CARE OF AGENCY FOR REP, A & G FORMATS (RADIO)         *           
* 07JUL97 RHV SUPPORT AGY COMMENTS                                  *           
* 03JUN97 RHV SUPPORT CFC COMMENTS                                  *           
* 15MAY97 RHV DON'T DIE ON ENCOUNTERING ERRONEOUS 3RD CMT LINE      *           
* 14APR97 RHV REVISED CF ACTION LOGIC                               *           
* 27MAR97 SKU VARIOUS/BRAND SUPPORT                                 *           
* 17JAN97 SKU MAKEGOOD FOR MAKEGOOD                                 *           
* 05NOV96 SKU SUPPORT PREEMPT AND BONUS                             *           
* 10OCT96 RHV PRINT K ORD CMT BEFORE BUYLINES                       *           
* 04OCT96 SKU SUPPORT LOW POWER STATION                             *           
* 13SEP96 SKU FIX MAKEGOOD APPLIED PRINTING                         *           
* 05SEP96 SKU FIX DR PRINTING. FIX COMBO PARENT CHECK DUMP          *           
* 28JUN96 SKU FIX COMBO TOTAL PRINT                                 *           
* 24JUN96 SKU HIDE MG OFFER FROM OPPOSITE PARTY IF NOT YET SENT     *           
* 07JUN96 SKU ADD FORMAT K AND C TO SUPPORT EOP CODES               *           
* 05JUN96 RHV DISPLAY COMPETITIVE INFO ON ALL WORKSHEET VERSIONS    *           
* 03JUN96 RHV DISPLAY SONNET INFO ON WORKSHEETS                     *           
* 14MAY96 WSB DISP NON-DARE HIATUS INFO IF CON NOT LINKED TO DARE   *           
* 29APR96 RHV CONTYPE RECORD CONTROLLED WORKSHEET FORMATTING        *           
* 08APR96 RHV SUPPORT 34 BYTE AGY ADDRESS FIELDS FOR ALL USERS      *           
* 01APR96 SKU BOUND CHECKING FOR ORDER COMMENTS                     *           
* 25MAR96 RHV REVISED DISPLAY OF SAR BOOK NAMES&RENAMES             *           
* 21MAR96 RHV SUPPORT DISPLAY OF 12 SPL COMMENT LINES               *           
* 22MAR96 RHV RECAP/CHANGES ONLY PER NEW RULES & PROFILES           *           
* 29FEB96 RHV SUPPORT PETRY 34 BYTE AGY ADDRESS FIELDS              *           
* 29FEB96 RHV READING OF OFFICE REC FOR PROFILE FOR RECAP/CHANGES   *           
* 29FEB96 SKU PROFILE 25 TYPE D SEND WORKSHEET TO REP               *           
* 06FEB96 SKU CFX SUPPORT                                           *           
* 06JAN96 SKU PROFILE 24 TYPE D PRINT PTP OVER SALESPERSON          *           
* 13DEC95 SKU 2K CONTRACT SUPPORT                                   *           
*                                                                   *           
*********************************************************************           
*                                                                   *           
*                                                                   *           
* SENDPASS -- PASS NUMBER INDICATOR.  MEANING FOR SENDER:           *           
*                                                                   *           
* SENDER = REP ('SEND' ACTION)                                      *           
*                                                                   *           
*        1 = REP COPY. SEND ID = TWAUSRID. REP FORMAT.              *           
*                                                                   *           
*        2 = STATION WORKSHEET.  SEND ID = STA RECEIVE ID           *           
*            FORMAT = STATION TRAFFIC SYSTEM                        *           
*                                                                   *           
*        3 = OTHER DESTINATION. SEND ID = STA DEST. ID              *           
*            FORMAT = STATION DESTINATION FORMAT (1ST BYTE)         *           
*            SEND CONTROL = STA DEST FORMAT (2ND BYTE)              *           
*             -- (U = SEND UNCONFIRMED; C = CONFIRMED; B = BOTH)    *           
*                                                                   *           
* SENDER = REP ('CF' ACTION)                                        *           
*                                                                   *           
*        1 = ZERO OUT SEND ID.  PREVENT SPOOLED OUPUT (NOPQCLOS)    *           
*                                                                   *           
*        2 = SPECIAL                                                *           
*            IF AGENCY PROFILE 3 = Y ** AND **                      *           
*            IF ACTION = CF                                        *            
*               SEND ID = REP ID FROM CONTRACT SEND ELEMENT         *           
*               FORMAT = REP FORMAT.                                *           
*                                                                   *           
*        3 = OTHER DESTINATION. SEND ID = STA DEST. ID              *           
*            FORMAT = STATION DESTINATION FORMAT (1ST BYTE)         *           
*            SEND CONTROL = STA DEST FORMAT (2ND BYTE)              *           
*             -- (U = SEND UNCONFIRMED; C = CONFIRMED; B = BOTH)    *           
*                                                                   *           
* SENDER = STATION  ('SEND' OR 'CF' ACTIONS)                        *           
*                                                                   *           
*        1 = STATION WORKSHEET.  SEND ID = TWAUSRID.                *           
*            FORMAT = STATION TRAFFIC SYSTEM                        *           
*            IF ACTION = CF                                         *           
*               IF STATION OPTION 1 = C'Y'  (NO COPY ON CF)       *             
*                  DO NOT PRINT STATION COPY.                       *           
*        2 = SPECIAL                                                *           
*            IF ACTION = SEND  (REP COPY)                           *           
*               SEND ID = REP ID FROM CONTRACT SEND ELEMENT         *           
*               FORMAT = REP FORMAT.                                *           
*            IF ACTION = CF (OR CONX)  (POINT ID TO REP)            *           
*               SEND ID = REP ID FROM CONTRACT                      *           
*               IF AGENCY PROFILE 3 = N                             *           
*                  EXIT WITHOUT PRINTING.  (CHANGE ACTION TO CONX)  *           
*                     ELSE PRINT A COPY TO THE REP                  *           
*               FORMAT = REP FORMAT.                                *           
*                                                                   *           
*        3 = OTHER DESTINATION. SEND ID = STA DEST. ID              *           
*            FORMAT = STATION DESTINATION FORMAT (1ST BYTE)         *           
*            SEND CONTROL = STA DEST FORMAT (2ND BYTE)              *           
*             -- (U = SEND UNCONFIRMED; C = CONFIRMED; B = BOTH)    *           
*                                                                   *           
* SENDPASS 4 = FAX WORKSHEET FORMAT G TO STATION                    *           
*                                                                   *           
* ----------------------------------------------------------------  *           
* MEMORY MAP:                                                       *           
* ENTD (FOR SORTED FLIGHTS)                                         *           
*     SPOOLAR+4000 ===> SPOOLAR+6041 (255X7)=(MAX BUYS X LEN(ENTD)) *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
T80263   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYDX-MYD,*T80263*,R9,RR=R8,CLEAR=YES                             
         LR    R7,RC                                                            
         USING MYD,R7                                                           
         ST    R8,RELO                                                          
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,VTWA                                                          
         USING TWAD,RA                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*********************************************************************           
*        MAIN LINE PROCESSING                                                   
*********************************************************************           
         BAS   RE,INIT             SET INITAL ADDRS AND VALUES                  
         BAS   RE,GETCON           READ CONTRACT RECORD INTO AIO                
         BNZ   MAIN300                                                          
*                                                                               
         BAS   RE,GETSTA           READ STATION INTO IO3                        
         BNZ   MAIN300                                                          
*                                                                               
         GOTO1 =A(FINDID),RR=RELO  FIND SEND ID AND FORMAT.                     
         BNZ   MAIN300                                                          
*                                                                               
         BAS   RE,GETOFF                                                        
*                                                                               
         MVC   DUB(1),FORMAT       TO PASS FORMAT TO PQ OPEN ROUTINE            
         GOTO1 PQOPEN              OPEN PRINT QUEUE                             
*                                                                               
         BAS   RE,PCON             PROCESS THE CONTRACT RECORD                  
*                                                                               
         LR    R4,RA                                                            
         AH    R4,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R4                                                       
*                                                                               
         TM    TWACFFLG,X'08'      CFC COMMENT PRESENT?                         
         BZ    *+8                 NO                                           
         MVI   FORMAT,X'FF'        YES - FORCE REP FORMAT TO STA                
*                                                                               
         XC    XWORK5,XWORK5       INIT CASH TOTAL BUFFER                       
         XC    XWORK6,XWORK6       INIT TRADE TOTAL BUFFER                      
         XC    TWAEOPSL,TWAEOPSL                                                
         XC    TWAEOPOF,TWAEOPOF                                                
         MVI   TWAWSFLG,0          RESET WORKSHEET PRINTING FLAGS               
         OI    TWAWSFLG,X'02'      PRINT 1ST BUYLINE HEADER                     
*                                                                               
         CLI   FORMAT,C'B'         FOR BIAS, RETRIEVE EOP CODES                 
         BE    MAIN90              FOR SALESPERSON OR OFFICE, IF ANY            
         CLI   FORMAT,C'W'         AND W FORMAT, TOO                            
         BE    MAIN90                                                           
         CLI   FORMAT,C'J'         AND J FORMAT, TOO                            
         BE    MAIN90                                                           
         CLI   FORMAT,C'C'         AND C FORMAT, TOO                            
         BE    MAIN90                                                           
         CLI   FORMAT,C'H'         AND H FORMAT, TOO                            
         BE    MAIN90                                                           
         CLI   FORMAT,C'K'         AND K FORMAT, TOO                            
         BE    MAIN90                                                           
         CLI   FORMAT,C'N'         AND N FORMAT, TOO                            
         BE    MAIN90                                                           
         CLI   FORMAT,C'Z'         AND Z FORMAT, TOO (VSS)                      
         BE    MAIN90                                                           
         CLI   FORMAT,C'X'         AND X FORMAT, TOO (VSS/IBS)                  
         BE    MAIN90                                                           
         CLI   FORMAT,C'S'         AND S FORMAT, TOO                            
         BNE   MAIN100                                                          
*                                                                               
MAIN90   DS    0H                                                               
         GOTO1 =A(GETEOP),DMCB,(RC),(R7),(X'1E',TWAEOPSL),(3,RCONSAL), X        
               (16,0),RR=Y                                                      
         GOTO1 =A(GETEOP),DMCB,(RC),(R7),(X'1D',TWAEOPOF),(2,RCONKOFF),X        
               (17,0),RR=Y                                                      
         DROP  R4                                                               
*                                                                               
MAIN100  DS    0H                                                               
         GOTOR PROCSTAT            PROCESS THE STATION RECORD                   
*                                                                               
         MVI   CMNTPRTD,C'Y'       DONT' PRINT COMMENTS UNTIL THE END           
*                                                                               
* READ POINT PERSON RECORD FOR NAME                                             
*                                                                               
         XC    WPTPEXP,WPTPEXP     INIT POINT PERSON EXPANSION                  
         XC    WPTPPH#,WPTPPH#     INIT POINT PERSON EXPANSION                  
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         OC    TWAPDPTP,TWAPDPTP   POINT PERSON CODE PRESENT?                   
         BZ    MAIN130                                                          
         DROP  RF                                                               
*                                                                               
         GOTO1 =A(PPTP),DMCB,(RC),(R7),RR=Y                                     
*                                  GET POINT PERSON NAME                        
*                                                                               
*            READ PRODUCT RECORD FOR CATEGORY CODE                              
*                                                                               
MAIN130  DS    0H                                                               
         CLI   FORMAT,X'FF'        ONLY NEED IT FOR REP WORKSHEET               
         BE    MAIN140                                                          
         CLI   FORMAT,C'R'                                                      
         BE    MAIN140                                                          
         CLI   FORMAT,C'P'         AND WPVI FORMAT                              
         BNE   MAIN150                                                          
MAIN140  EQU   *                                                                
         BAS   RE,PPROD            PROCESS PRODUCT                              
MAIN150  EQU   *                                                                
*                                                                               
*  FOR ACTION CONFIRM (ACE), JUST PRINT HEADLINES, & CONTRACT $ TOTALS          
*                                                                               
         GOTO1 =A(GAGY),DMCB,(RC),RR=Y                                          
         GOTO1 =A(FMT),DMCB,(RC),(R7),RR=YES                                    
         BAS   RE,GOSPUL                                                        
*                                                                               
         CLC   =C'CF',CONACT       CONFIRM ACTION?                              
         BNE   MAIN152             NO                                           
         CLI   FORMAT,X'FF'        REP COPY?                                    
         BNE   MAIN160             NO - SKIP COVER ON STATION COPY              
*                                                                               
MAIN152  DS    0H                                                               
         MVI   P,0                                                              
         BAS   RE,GOSPUL                                                        
*                                                                               
         MVC   WORK+0(4),ASPOOLD   PRINT COVERSHEET                             
         MVC   WORK+4(4),DATAMGR                                                
         MVC   WORK+8(4),VREPFACS                                               
         MVC   WORK+12(4),AIO4                                                  
         MVC   WORK+16(4),SPOOL                                                 
         STCM  RC,15,WORK+20                                                    
         MVC   WORK+24,VREGENSC                                                 
         MVC   WORK+28,GETTXT                                                   
         LA    R1,RCONREC                                                       
         ST    R1,DMCB                                                          
         MVI   DMCB,80                                                          
         CLI   FORMAT,C'A'                                                      
         BE    MAIN155                                                          
         CLI   FORMAT,C'G'                                                      
         BE    MAIN155                                                          
         MVI   DMCB,108                                                         
MAIN155  GOTOX APRTCOV,DMCB,,(SVHEADCD,WORK)                                    
***>     GOTOX APRTCOV,DMCB,(108,RCONREC),(SVHEADCD,WORK)                       
*                                                                               
MAIN160  DS    0H                                                               
         CLC   =C'MGS',CONACT                                                   
         BE    MAIN180                                                          
*                                                                               
         LR    RF,RA               SPECIAL ROUTINE IF COMBO ORDER               
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0                                                       
         BE    MAIN170                                                          
         DROP  RF                                                               
*                                                                               
         GOTO1 =A(PRTCOMBO),DMCB,(RC),(R7),RR=Y                                 
         B     MAIN190                                                          
*                                                                               
MAIN170  DS    0H                                                               
*&&DO                                                                           
* FOR DARE AGENCY NO K - ASTE                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
                                                                                
         TM    TWACFFLG,X'20'      CONFIRMED ORDER WORKSHEET                    
         BZ    MAIN172                                                          
         CLI   TWADCFPT,C'Y'       DARE AGENCY NO CONTRACT FLAG ON??            
         BNE   MAIN172                                                          
         CLI   FORMAT,X'FF'        YES, REP??                                   
         BNE   MAIN172             NO, SKIP                                     
         BAS   RE,GOSPUL           PRINT BLANK LINE                             
*&&                                                                             
*                                                                               
MAIN172  BAS   RE,DOBUYS              PROCESS BUY LINES                         
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'80' DON'T PRINT BUY HEADR ON OTHER PGS          
         TM    TWAWSFLG,X'40'      K ORD COMMENT PRINTED YET?                   
         BO    MAIN175             YES - SKIP                                   
         DROP  RF                                                               
         GOTO1 =A(ORDCMT),DMCB,(RC),RR=Y   NO - PRINT IT NOW                    
MAIN175  GOTO1 =A(PTOTAL),RR=Y                                                  
*                                                                               
MAIN180  DS    0H                                                               
         GOTO1 =A(DOMKGS),DMCB,(RC),(R7),RR=Y                                   
*&&DO                                                                           
* FOR DARE AGENCY NO K - ASTE                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
                                                                                
         TM    TWACFFLG,X'20'      CONFIRMED ORDER WORKSHEET                    
         BZ    MAIN185                                                          
         CLI   TWADCFPT,C'Y'       DARE AGENCY NO CONTRACT FLAG ON??            
         BNE   MAIN185                                                          
         CLI   FORMAT,X'FF'        YES, REP??                                   
         BNE   MAIN185             NO, SKIP                                     
         BAS   RE,GOSPUL           PRINT BLANK LINE                             
         MVI   CMNTPRTD,C'N'       PRINT COMMENTS NOW                           
         BAS   RE,GOSPUL                                                        
         DROP  RF                                                               
*&&                                                                             
*                                                                               
MAIN185  CLC   =C'MGS',CONACT                                                   
         BE    MAIN300             SKIP CONTRACT COMMENTS                       
*                                                                               
MAIN190  CLC   CONACT,=C'CF  '     FOR ACTION CONFIRM,                          
         BNE   MAIN200             DON'T PRINT ANY OF THIS OTHER JUNK           
*                                                                               
* FORCED OCM (NON-DISCRIMINATION CLAUSE)                                        
*                                                                               
         GOTO1 =A(PRINT),RR=RELO   SKIP A LINE                                  
         GOTO1 =A(PSTCMT),DMCB,(3,=C'SC=*'),(RC),RR=RELO                        
         B     MAIN300                                                          
*                                                                               
*        CLI   SVVER,1                                                          
*        BE    MAIN200                                                          
*        TM    SVSENF,X'04'                                                     
*        BZ    MAIN210                                                          
MAIN200  EQU   *                                                                
         GOTO1 =A(DOSPL),RR=RELO                                                
MAIN210  EQU   *                                                                
*                                                                               
         CLI   RCONKSTA+4,C' '                                                  
         BE    MAIN225                                                          
         CLI   RCONKSTA+4,C'T'                                                  
         BE    MAIN225                                                          
         CLI   RCONKSTA+4,C'L'                                                  
         BE    MAIN225                                                          
         GOTO1 =A(DOBOP),RR=RELO                                                
         B     MAIN230                                                          
MAIN225  DS    0H                                                               
         GOTO1 =A(DOSAR),RR=RELO                                                
         BZ    MAIN230                                                          
         MVC   P,SPACES            CLEAR P, DON'T PRINT ANY SAR DATA            
         B     MAIN240                                                          
MAIN230  EQU   *                                                                
* PRINT BLOCK OF STORED COMMENTS                                                
         GOTO1 SPOOL,DMCB,(R8)     FLUSH OUT P BEFORE COMMENTS                  
MAIN240  DS    0H                                                               
         CLI   SVVER,1             VERSION 1?                                   
         BE    MAIN250             YES                                          
         GOTO1 =A(MODCODES),DMCB,(RC),RR=Y  NO - MOD CODE GLOSSARY              
MAIN250  DS    0H                                                               
         MVI   CMNTPRTD,C'N'       PRINT COMMENTS NOW                           
         BAS   RE,GOSPUL                                                        
*                                                                               
*        MAIN CONTROL END                                                       
*                                                                               
MAIN300  EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*        PPROD --- PRCESS PRODUCT RECORD                                        
*********************************************************************           
PPROD    NTR1                                                                   
*                                                                               
         CLC   SVCONPRD,SPACES     IF SPACES, NO CATEGORY CODE                  
         BE    PPRDGOOD                                                         
         SPACE 1                                                                
         LA    R5,KEY                                                           
         USING RPRDKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,SVCONADV                                                
         MVC   RPRDKPRD,SVCONPRD                                                
         MVC   RPRDKREP,REPALPHA                                                
         MVI   ION,3                                                            
         GOTO1 AHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    PPRD10                                                           
         DC    H'0'                                                             
PPRD10   GOTO1 AGETREC                                                          
         SPACE 1                                                                
         L     R6,AIO3                                                          
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         USING RPRDELEM,R6                                                      
         MVC   SVPRDCTG,RPRDCATG   CATEGORY CODE                                
         DROP  R5,R6                                                            
*                                                                               
*        PPROD EXIT                                                             
*                                                                               
PPRDGOOD B     YES                                                              
         EJECT                                                                  
*********************************************************************           
*        INIT --- SET INITAL ADDRS AND VALUES                                   
*********************************************************************           
INIT     NTR1                                                                   
*                                                                               
         L     RF,=V(OUTDAY)                                                    
         A     RF,RELO                                                          
         ST    RF,OUTDAY                                                        
*                                                                               
         L     RF,=V(REPRTCOV)                                                  
         A     RF,RELO                                                          
         ST    RF,APRTCOV                                                       
*                                                                               
         L     RF,=V(REGENPBY)                                                  
         A     RF,RELO                                                          
         ST    RF,REGENPBY                                                      
*                                                                               
* NOTE: REGENBUC USED TO BE SET HERE BUT NOW HANDLED IN RECNT00                 
*                                                                               
         L     RF,=V(REGENTL2)                                                  
         A     RF,RELO                                                          
         ST    RF,REGENTL2                                                      
*                                                                               
         L     RF,=V(UNTIME)                                                    
         A     RF,RELO                                                          
         ST    RF,UNTIME                                                        
*                                                                               
         MVI   SVDEMFLG,0          INIT DEMO PRINT FLAG                         
*                                                                               
         SR    RF,RF               BE SAFE                                      
*                                                                               
         LA    R1,HDS              RESOLVE A(WORK4)                             
         A     R1,=AL4(WRKDSP)                                                  
         ST    R1,AWORK4                                                        
         SPACE 1                                                                
         LA    R1,HDS              RESOLVE A(ORD CMT STORAGE AREAS)             
         A     R1,=AL4(ORDDSP)                                                  
         ST    R1,ASVRCOC          REP ORDER COMMENT AREA                       
         LA    R1,L'SVRCOC(R1)                                                  
         ST    R1,ASVSCOC          STATION ORDER COMMENT AREA                   
         LA    R1,L'SVSCOC(R1)                                                  
         STCM  R1,15,ASVACMT       AGENCY COMMENT SAVE AREA                     
*                                                                               
         MVI   SKIP,C'N'           TOP OF PAGE, NO SPACING NEEDED               
*                                                                               
         MVI   NOPQCLOS,0          ASSUME WE HAVE OUTPUT                        
         B     YES                                                              
         EJECT                                                                  
*********************************************************************           
*                                                                               
*        PCON --- PROCESS THE CONTRACT RECORD, ELEMENT AT A TIME                
*                                                                               
*                                                                               
*---> PROGRAMMER'S NOTE:  REGISTER USAGE IS MOST PECULIAR IN THIS               
*                         ROUTINE.  BE VERY CAREFUL WHEN MODIFYING!             
*                                                                               
*     NOTE:  PECULIAR USAGE IS SIMPLIFIED BY ISOLATING REGISTER                 
*            CONTENTS WITHIN ROUTINES IN WHICH IT IS MODIFIED,                  
*            SETTING INITIAL VALUE AT ENTRY, AND STORING FINAL VALUE            
*            UPON LEAVING ROUTINE.    BILL UHR  (30MAR93).                      
*                                                                               
*                                                                               
*                                                                               
*********************************************************************           
PCON     NTR1                                                                   
*                                                                               
         L     R4,AIO              CONTRACT RECORD                              
         USING RCONKEY,R4                                                       
         MVC   SVOFF,RCONKOFF                                                   
         LA    R2,RCONELEM                                                      
         DROP  R4                                                               
                                                                                
         L     R3,ASVSCOC          STATION CON ORD COMMENT                      
         ST    R3,PCONSCOC         SAVE IT OFF                                  
         LA    R3,SVSPLCOM         SPL COMMENT                                  
         ST    R3,PCONSPLC         SAVE IT OFF                                  
         LA    R3,SVCONCOM         CONTRACT COMMENT                             
         ST    R3,PCONCONC         SAVE IT OFF                                  
         L     R3,ASVRCOC          REP CON ORD COMMENT                          
         ST    R3,PCONRCOC         SAVE IT OFF                                  
         SPACE 1                                                                
         CLI   0(R2),X'01'         CONTRACT DESCRIPTION ELEMENT                 
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         MVC   SVCONPRD,22(R2)     PRODUCT                                      
         MVC   SVCONDTE,30(R2)     DATES                                        
         MVC   SVCONMOD,36(R2)     MODIFICATION NUMBER                          
         MVC   SVCONTP,41(R2)      ACE/GRAPHNET TYPE                            
         MVC   SVWKS,50(R2)        WEEKS                                        
         SPACE 1                                                                
PCON50   EQU   *                                                                
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    PCON90                                                           
         LA    RE,PCONTAB                                                       
PCON60   EQU   *                                                                
         CLI   0(RE),0                                                          
         BE    PCON50                                                           
         CLC   0(1,RE),0(R2)                                                    
         BE    PCON70                                                           
         LA    RE,L'PCONTAB(RE)                                                 
         B     PCON60                                                           
PCON70   EQU   *                                                                
         ZICM  R0,1(RE),3                                                       
         LR    RE,R0                                                            
         A     RE,RELO             RELOCATE TABLE BRANCH ADDRESS                
         BR    RE                                                               
*                                                                               
*        PCON EXIT                                                              
*                                                                               
PCON90   EQU   *                                                                
         B     YES                                                              
         SPACE 5                                                                
PCON100  EQU   *                   CONTRACT COMMENT ELEMENT X'02'               
         L     R3,PCONCONC         RESET A(CONTRACT COMMENT)                    
         LA    RE,SVCONCOM+L'SVCONCOM  A(END OF K CMT AREA)                     
         CR    R3,RE               DO WE HAVE ROOM FOR THIS CMT LINE?           
         BNL   PCON50              NO - SKIP IT                                 
         ZIC   RE,1(R2)                                                         
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),2(R2)                                                    
         LA    R3,60(R3)                                                        
         ST    R3,PCONCONC         SAVE A(CONTRACT COMMENT)                     
         B     PCON50                                                           
         SPACE 2                                                                
PCON150  EQU   *                   SPL ELEMENT X'06'                            
         OC    5(3,R2),5(R2)       NO SPL ENTRY DATE INDICATES                  
         BZ    PCON50              NO SPL DOLLARS                               
         TM    RCONSPES-RCONSPEL(R2),X'04'                                      
*                                  DOLLARS OR PERCENTS?                         
         BNO   PCON152             NOT ON = DOLLARS                             
*                                     TREAT AS ORIGINALLY                       
         GOTO1 =A(NEWSPL),DMCB,(RC),RR=Y                                        
*                                  ON  =  PERCENTS                              
*                                        ISOLATE DOLLARS                        
PCON152  EQU   *                                                                
         SR    R6,R6                                                            
         LA    R5,STALST                                                        
         MVI   BYTE2,0                                                          
         TM    4(R2),X'80'         STA WON 100% OF MKT?                         
         BZ    *+8                                                              
         MVI   BYTE2,1             SAVE FOR DOSPL (RADIO PART)                  
         SR    RE,RE                                                            
         CLI   8(R2),6             MORE THAN 6 STAS?                            
         BNH   *+12                NO, USE ACTUAL                               
         LA    RE,6                ELSE LIMIT TO SIX                            
         B     *+8                                                              
         IC    RE,8(R2)            NUMBER OF MINI ELEMENTS                      
         LR    R4,R2                                                            
         LA    R4,9(R2)            POINT TO MINI ELEMENT                        
PCON160  MVC   0(5,R5),0(R4)       MOVE IN STATION                              
         OC    0(5,R5),SPACES                                                   
         MVC   5(4,R5),5(R4)       MOVE IN AMOUNT                               
         L     R0,5(R4)                                                         
         AR    R6,R0               ADD STATION AMT TO MKT TOTAL                 
         LA    R4,9(R4)                                                         
         LA    R5,9(R5)                                                         
         BCT   RE,PCON160                                                       
*                                                                               
         ST    R6,MKTOT            TOTAL DOLLARS IN MARKET                      
         TM    RCONSPES-RCONSPEL(R2),X'04'                                      
*                                  DOLLARS OR PERCENTS?                         
         BNO   PCON162             NOT ON = DOLLARS                             
*                                     TREAT AS ORIGINALLY                       
         MVC   MKTOT,EST$$$        ON  = PERCENTS:                              
*                                     USE ESTIMATE DOLLARS                      
PCON162  EQU   *                                                                
         B     PCON50                                                           
         SPACE 2                                                                
PCON200  EQU   *                   SPL COMMENT X'07'                            
         L     R3,PCONSPLC         RESET A(SPL COMMENT)                         
         ZIC   RE,1(R2)                                                         
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),2(R2)                                                    
         LA    R3,65(R3)                                                        
         ST    R3,PCONSPLC         SAVE A(SPL COMMENT)                          
         B     PCON50                                                           
         SPACE 2                                                                
PCON225  EQU   *                   BOP ELEMENT X'10'                            
         CLI   RCONKSTA+4,C'T'     ONLY RADIO DO THIS                           
         BE    PCON50                                                           
         CLI   RCONKSTA+4,C' '                                                  
         BE    PCON50                                                           
         CLI   RCONKSTA+4,C'L'                                                  
         BE    PCON50                                                           
PCON230  DS    0H                                                               
         MVC   SVDEMO(20),49(R2)   DEMOS                                        
         MVC   SVSRC,RCONRTGS      RATING SERVICE                               
         XC    SVBOOK,SVBOOK                                                    
         MVC   SVBOOK(06),82(R2)   BOOKS                                        
         MVC   SVMKT(3),69(R2)     MARKET                                       
         OC    SVBOOK(9),SPACES                                                 
*                                                                               
         CLC   SVBOOK(6),SPACES   NOW SUFFIX BOOK W/BOOK TYPE                   
         BE    PCON235                                                          
         CLI   89(R2),0                                                         
         BE    PCON235                                                          
         CLI   89(R2),C' '                                                      
         BE    PCON235                                                          
         LA    RF,SVBOOK+6                                                      
         BCTR  RF,0                                                             
         CLI   0(RF),C' '                                                       
         BE    *-6                                                              
         MVC   1(3,RF),=C'( )'                                                  
         MVC   2(1,RF),89(R2)                                                   
PCON235  DS    0H                                                               
         B     PCON50                                                           
         SPACE 2                                                                
PCON250  EQU   *                   SAR ELEMENT X'12'                            
         CLI   RCONKSTA+4,C'T'     ONLY TV DO THIS                              
         BE    PCON260                                                          
         CLI   RCONKSTA+4,C'L'     ONLY TV DO THIS                              
         BE    PCON260                                                          
         CLI   RCONKSTA+4,C' '     ONLY TV DO THIS                              
         BNE   PCON50                                                           
PCON260  DS    0H                                                               
         MVC   SVDEMO,5(R2)        DEMOS                                        
         MVC   SVSRC,29(R2)        RATING SERVICE                               
         MVC   SVBOOK,30(R2)       BOOKS                                        
         B     PCON50                                                           
         SPACE 2                                                                
PCON300  EQU   *                   EXTENDED DESCRIPTION X'1F'                   
         MVC   SVCONF,6(R2)        X'80'- NOT CONFIRMED                         
         MVC   SVTRAF,8(R2)        TRAFFIC NUMBER                               
         B     PCON50                                                           
         SPACE 2                                                                
PCON350  EQU   *                   SEND ELEMENT X'20'                           
         CLC   5(1,R2),14(R2)      COMPARE REP & STA VER. NO.                   
         BH    *+14                USE HIGHER NUMBER                            
         MVC   SVVER,14(R2)        STATION VERSION NUMBER                       
         B     *+10                                                             
         MVC   SVVER,5(R2)         REP VERSION NUMBER                           
         B     PCON50                                                           
         SPACE 2                                                                
PCON400  EQU   *                   REP CON ORD CMT X'82'                        
*                                                                               
         L     R3,PCONRCOC         BOUND CHECKING, MAKE SURE WE DON'T           
         L     RE,ASVRCOC          HAVE MORE COMMENTS THAN WE CAN HOLD          
         LA    RE,L'SVRCOC(RE)                                                  
         CR    R3,RE               IF WE DO, SKIP THE REST!                     
         BNL   PCON50                                                           
*                                                                               
         L     R3,PCONRCOC         RESET A(REP CON ORD COMMENT)                 
         ZIC   RE,1(R2)                                                         
         SH    RE,=H'3'                                                         
*                                                                               
         CH    RE,=H'60'                                                        
         BL    *+8                                                              
         LA    RE,59               MAX LENGTH                                   
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),2(R2)                                                    
         LA    R3,60(R3)                                                        
         ST    R3,PCONRCOC         SAVE A(REP CON ORD COMMENT)                  
         B     PCON50                                                           
         SPACE 2                                                                
PCON450  EQU   *                   STA CONTRACT ORDER CMT X'92'                 
*                                                                               
         L     R3,PCONSCOC         BOUND CHECKING, MAKE SURE WE DON'T           
         L     RE,ASVSCOC          HAVE MORE COMMENTS THAN WE CAN HOLD          
         LA    RE,L'SVSCOC(RE)                                                  
         CR    R3,RE               IF WE DO, SKIP THE REST!                     
         BNL   PCON50                                                           
*                                                                               
         L     R3,PCONSCOC         RESET A(STA ORDER COMMENT)                   
         ZIC   RE,1(R2)                                                         
         SH    RE,=H'3'                                                         
*                                                                               
         CH    RE,=H'60'                                                        
         BL    *+8                                                              
         LA    RE,59               MAX LENGTH                                   
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),2(R2)                                                    
         LA    R3,60(R3)                                                        
         ST    R3,PCONSCOC         SAVE A(STATION ORDER COMMENT)                
         B     PCON50                                                           
         SPACE 2                                                                
PCON500  EQU   *                   EXTRA DESCRIPTION ELEMENT X'9F'              
         MVC   SVADVC,2(R2)        ADVERTISER CODE                              
         MVC   SVAGYC,12(R2)       AGENCY CODE                                  
         MVC   SVSASST,22(R2)      SALES ASSISTANT                              
         B     PCON50                                                           
         SPACE 2                                                                
PCON550  EQU   *                  UNI SUPPLEMENTARY WORKSHEET EL X'A1'          
         MVC   SVSWE(18),2(R2)                                                  
         B     PCON50                                                           
PCON600  EQU   *         CHECK '1E' ELEM FOR REVISED FLIGHT DATES               
         OC    RCONRFLT-RCONRFEL(6,R2),RCONRFLT-RCONRFEL(R2)                    
         BZ    PCON50                                                           
         MVC   SVCONDTE,RCONRFLT-RCONRFEL(R2)                                   
         B     PCON50                                                           
*                                                                               
*********************************************************************           
*        PCON CONTROL TABLE                                                     
*********************************************************************           
PCONTAB  DS    0CL4                                                             
         DC    X'02',AL3(PCON100)                                               
         DC    X'06',AL3(PCON150)                                               
*        DC    X'07',AL3(PCON200) THIS WILL BE HANDLED ELSEWHERE                
         DC    X'10',AL3(PCON225)                                               
         DC    X'12',AL3(PCON250)                                               
         DC    X'1E',AL3(PCON600)                                               
         DC    X'1F',AL3(PCON300)                                               
         DC    X'20',AL3(PCON350)                                               
         DC    X'82',AL3(PCON400)                                               
         DC    X'92',AL3(PCON450)                                               
         DC    X'9F',AL3(PCON500)                                               
         DC    X'A1',AL3(PCON550)                                               
         DC    X'00',AL3(0)                                                     
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
*********************************************************************           
*        GETCON --- READ CONTRACT RECORD INTO AIO OPEN PRINTQUEUE               
*********************************************************************           
GETCON   NTR1                                                                   
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   KEY+28(4),TWAKADDR                                               
         DROP  RF                                                               
         MVI   ION,1                                                            
         GOTO1 AGETREC                                                          
         L     R4,AIO              CONTRACT RECORD                              
         USING RCONKEY,R4                                                       
         MVC   ACTSTAT,RCONKSTA    STATION                                      
         MVC   SVCONADV,RCONKADV   ADVERTISER                                   
         MVC   SVCNTL,29(R4)                                                    
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*********************************************************************           
*- GETSTA -- READ STATION RECORD INTO IO3.                                      
*********************************************************************           
GETSTA   NTR1                                                                   
         GOTO1 =A(CHKPREF),DMCB,(RC),(R7),RR=Y                                  
*                                                                               
GSTA05   DS    0H                                                               
         LA    R5,KEY                                                           
         USING RSTAKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,ACTSTAT                                                 
         MVI   ION,3                                                            
         GOTO1 AHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    GSTA10                                                           
         DC    H'0'                                                             
GSTA10   GOTO1 AGETREC                                                          
*                                                                               
*- PICK UP RECORD DATA                                                          
GSTA20   DS    0H                                                               
         MVI   BYTE,C'W'           PARAM TO PQOPEN (ASSUME W.U.)                
         MVI   SVSTAOP2,0          SPACING OPTION                               
*                                                                               
         L     R6,AIO3             STATION RECORD                               
         MVI   ELCODE,X'08'        EXTRA DESCRIPTION ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   GSTA55              NOT THERE                                    
*                                                                               
         USING RSTAXXEL,R6                                                      
*                                                                               
GSTA50   MVC   SVSTAOP2,RSTAOPT2     SPACING OPTION                             
*                                       Y FOR GRAPHNET = SINGLE SPACE           
*                                       Y FOR NON-GRAPH = TRIPLE SPACE          
         DROP  R6                                                               
GSTA55   CLI   RSTAKSTA+4,C'C'                                                  
         BNE   YES                                                              
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'0A'        COMBO STA ELEM                               
         BRAS  RE,GETEL                                                         
         BE    GSTA60                                                           
         DC    H'0'                                                             
         USING RSTACSEL,R6                                                      
*                                                                               
GSTA60   DS    0H                                                               
         CLI   RSTACS+4,C'A'                                                    
         BE    GSTA70                                                           
         MVC   SVCFM,RSTACS                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   YES                                                              
         MVC   SVCAM,RSTACS                                                     
         B     YES                 EXIT WITH GOOD CC                            
*                                                                               
GSTA70   DS    0H                                                               
         MVC   SVCAM,RSTACS                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   YES                                                              
         MVC   SVCFM,RSTACS                                                     
         B     YES                 EXIT WITH GOOD CC                            
         DROP  R5,R6                                                            
         EJECT                                                                  
*********************************************************************           
*        PSTA --- NOW READ STATION RECORD FOR REPORT FORMAT & WU INFO           
*********************************************************************           
PROCSTAT NTR1                                                                   
*                                                                               
         LR    R5,RA                                                            
         AH    R5,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R5                                                       
*                                                                               
         L     R4,AIO                                                           
         USING RCONKEY,R4                                                       
*                                                                               
         L     R6,AIO3             STATION RECORD                               
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         USING RSTAELEM,R6                                                      
         MVC   SVSTAMKT,RSTAMKT    STATION MARKET                               
         MVC   SVAFFL,RSTAAFFL     AFFILIATE                                    
*                                                                               
*        CLC   RCONKSTA,=C'KSL  '  KSL ALWAYS PRINTS RECAP                      
*        BE    PSTA0060                                                         
*                                                                               
         CLI   TWAACCS,C'$'        STATION SIDE SEND?                           
         BNE   PSTA0020            NO  - FOLLOW STANDARD PROFILES               
*                                  YES - CHECK FOR USING WEB CONFIRM            
         TM    TWASTAOB,X'08'      YES - VIA WEB CONFIRM?                       
*                                    (STN OPT BIT SET IN 'RSTAOPTB')            
         BO    PSTA0040            YES - FORCE TO 'CHANGES ONLY'                
*                                                                               
PSTA0020 EQU   *                                                                
         TM    RSTASTAT,X'04'      STATION SAYS CHANGES ONLY?                   
         BZ    PSTA0060                                                         
*                                                                               
         CLC   =C'RSND',CONACT     ALWAYS RECAP FOR ACTION RSND                 
         BE    PSTA0080                                                         
*                                                                               
PSTA0040 EQU   *                                                                
         OI    SVSTAT,X'04'             CHANGES ONLY                            
PSTA0060 CLI   FORMAT,X'FF'             REP FORMAT                              
         BNE   PSTA0080                                                         
         TM    TWAOFFPR,X'10'          OFFICE PROFILE SAYS RECAP?               
         BZ    *+8                                                              
         NI    SVSTAT,X'FF'-X'04'       RECAP                                   
         TM    TWAOFFPR,X'08'          OFFICE PROFILE SAYS CHNG ONLY?           
         BZ    *+8                                                              
         OI    SVSTAT,X'04'             CHANGES ONLY                            
* IF PROFILE SET, SEND RECAP ALWAYS                                             
PSTA0080 TM    PROFILES+CNTBDELB,CNTBDELA                                       
         BZ    *+8                                                              
         NI    SVSTAT,X'FF'-X'04'       RECAP                                   
*                                                                               
         TM    TWACFFLG,X'08'      CFC?                                         
         BZ    *+8                                                              
         NI    SVSTAT,X'FF'-X'04'  YES- RECAP                                   
*                                                                               
         TM    RSTASTAT,X'02'      DON'T SEND "#DS ' LINES                      
         BZ    PSTA0100                                                         
         OI    SVSTAT,X'02'                                                     
         SPACE 1                                                                
*                                                                               
*- SEND ID AND FORMAT SELECTED IN FINDID ROUTINE.                               
PSTA0100 EQU   *                                                                
         LA    R6,WKFORMAT         POINT TO THE WORKSHEET FORMAT TABLE          
TYPE95   CLI   0(R6),C' '          BLANK IS DEFAULT FORMAT                      
         BE    TYPE100                                                          
         CLC   0(1,R6),FORMAT                                                   
         BE    TYPE100                                                          
         LA    R6,3(R6)                                                         
         B     TYPE95                                                           
TYPE100  EQU   *                                                                
         MVC   RCSUBPRG(1),1(R6)                                                
         MVC   BYTE(1),2(R6)                                                    
*                                                                               
* WU & RADIO GET DIFFERENT HEADINGS FOR VER 1...                                
         CLI   SVVER,1                                                          
         BE    TYPE110                                                          
         TM    PROFILES+CNTAADDB,CNTAADDA PRINT AGY ADDRESS ON ALL VER?         
         BZ    GETFMT                                                           
*                                                                               
TYPE110  CLI   FORMAT,C'A'                                                      
         BE    *+12                                                             
         CLI   FORMAT,C'G'                                                      
         BNE   *+8                                                              
         MVI   RCSUBPRG,5                                                       
*                                                                               
GETFMT   DS    0H                                                               
         MVC   SVHEADCD,RCSUBPRG       SAVE CODE FOR LATER ALSO                 
         GOTO1 CALLOV,DMCB,(BYTE,0),0                                           
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   AFMT,0(R1)          SAVE OVERLAY ADDRESS                         
         BAS   RE,SETFMT           SET HEADHOOK AND SPECS                       
*                                                                               
         CLC   SENDID,=X'0406'     CLASS G REPORT?                              
         BNE   PSTA0120            NO - SKIP                                    
         GOTO1 =A(WUINFO),DMCB,(RC),(R7),RR=Y  YES - NEEDS EDICT HEADER         
         SPACE 1                                                                
PSTA0120 MVI   CODESW,C'N'         REMOVED MOD CODES 5/87                       
         XC    WORK3X,WORK3X                                                    
         MVC   WORK3X(2),=H'2'                                                  
         XC    XWORK4,XWORK4                                                    
         MVC   XWORK4(2),=H'2'                                                  
*                                                                               
* CHECK STATION OPTION FOR DDS CODES                                            
*                                                                               
         L     R6,AIO3             STATION RECORD                               
         MVI   ELCODE,X'08'        EXTRA DESCRIPTION ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   PSTA0140            NOT THERE -                                  
         USING RSTAXXEL,R6                                                      
         CLI   RSTAOPT5,C'Y'       TEST FOR DDS CODES OPTION                    
         BNE   PSTA0140                                                         
         XC    SVADVC,SVADVC                                                    
         XC    SVAGYC,SVAGYC                                                    
         XC    SVSALC,SVSALC                                                    
         MVC   SVADVC(4),SVCONADV                                               
***>     MVC   SVAGYC(4),RCONKAGY                                               
         GOTO1 (RFAGYOUT,VREPFACS),DMCB,RCONKAGY,SVAGYC,0                       
         MVC   SVSALC(3),RCONSAL                                                
         DROP  R6                                                               
*                                                                               
PSTA0140 B     YES                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
*********************************************************************           
* GET OFFICE RECORD PROFILE                                                     
*********************************************************************           
GETOFF   NTR1                                                                   
         LR    R5,RA                                                            
         AH    R5,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R5                                                       
         XC    TWAOFFPR,TWAOFFPR                                                
         XC    KEY,KEY                                                          
         MVI   ION,4                                                            
         MVI   KEY,ROFF2TYQ                                                     
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),RCONKOFF                                               
         GOTO1 AHIGH                      READ OFFICE RECORD FOR K              
         CLC   KEY(L'ROFF2KEY),KEYSAVE                                          
         BNE   GOFFX                      IF NO RECORD, DONT SET PROF           
         GOTO1 AGETREC                                                          
         L     R6,AIO4                                                          
         USING ROFF2REC,R6                                                      
         MVC   TWAOFFPR,ROFF2PRF                                                
GOFFX    B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
*********************************************************************           
*        DOBUYS --- PROCESS BUY LINES                                           
* UNFLIGHTED BUYS ARE PRINTED FIRST WHILE FLIGHTED BUYS ARE KEPT AND            
* SORTED IN AREA AT SPOOLAR+4000.  THE SORTED FLIGHTED BUYS ARE THEN            
* PRINTED IN FLIGHT ORDER.                                                      
*********************************************************************           
DOBUYS   NTR1  BASE=*,LABEL=*                                                   
         XC    STATFLAG,STATFLAG   INIT STATUS FLAG                             
         XC    TEMP(4),TEMP                                                     
*                                                                               
         MVI   FLTNDX,0            ZERO FLIGHTED BUY COUNTER                    
         MVI   FLTNOW,0            ZERO CURRENT FLIGHT                          
         MVI   FLTCTRL,0           ZERO FLIGHT CONTROL INDICATOR                
*                                                                               
         L     R6,ASPULAR          AREA FOR FLIGHT SORT ENTRIES                 
         A     R6,=AL4(4000)                                                    
         USING ENTD,R6                                                          
*                                                                               
         L     R4,AIO                                                           
         USING RCONKEY,R4                                                       
*        - BUILD BUY KEY                                                        
         LA    R5,KEY                                                           
         USING RBUYKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   RBUYKCON,TWACNUM                                                 
         DROP  R5,RF                                                            
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 AHIGH                                                            
         B     DBUY0040                                                         
*                                                                               
DBUY0020 OI    DMINBTS,X'08'                                                    
         GOTO1 ASEQ                                                             
*                                                                               
DBUY0040 CLC   KEY(22),KEYSAVE                                                  
         BE    DBUY0100                                                         
         CLI   FLTNDX,0            ARE THERE ANY FLIGHTED BUYS                  
         BE    DBUY0580            NO, PRINT CONTRACT TOTAL                     
*                                                                               
         BAS   RE,SORTFLT                                                       
*                                                                               
DBUY0060 DS    0H                                                               
         MVC   KEY+28(4),ENTDA     FETCH DISK ADDRESS                           
*                                                                               
         DROP  R6                                                               
*                                                                               
         OI    DMINBTS,X'08'                                                    
         MVI   ION,2                                                            
         GOTO1 AGETREC                                                          
*                                                                               
         L     R5,AIO2             BUY RECORD                                   
         USING RBUYREC,R5                                                       
         CLC   FLTNOW,RBUYFLT      HAS FLIGHT NUMBER CHANGED                    
         BE    DBUY0080                                                         
         MVC   FLTNOW,RBUYFLT      REPLACE CURRENT FLIGHT                       
         OI    FLTCTRL,FLTH        TURN ON FLIGHT HEADER INDICATOR              
         TM    FLTCTRL,FLTWT                                                    
         BZ    *+8                                                              
*        BAS   RE,UPWKTOT          UPDATE WEEKLY BUCKETS                        
         BAS   RE,WKTOT            PUT OUT WEEKLY TOTALS FOR LAST FLT           
         DROP  R5                                                               
*                                                                               
DBUY0080 BAS   RE,UPWKTOT          UPDATE WEEKLY BUCKETS                        
         B     DBUY0120                                                         
*                                                                               
DBUY0100 TM    KEY+27,X'C0'        VOID                                         
         BO    DBUY0020                                                         
         SPACE 1                                                                
         OI    DMINBTS,X'08'                                                    
         MVI   ION,2                                                            
         GOTO1 AGETREC                                                          
*                                                                               
         L     R5,AIO2             BUY RECORD                                   
         USING RBUYREC,R5                                                       
*                                  CHECK IF BUYLINE A COMBO                     
         TM    RBUYCOMB,X'80'      PLACE HOLDER (IE N/A RATE)                   
         BO    DBUY0020            YES, SKIP IT                                 
*                                                                               
         CLI   RBUYFLT,0           DOES BUY BELONG TO A FLIGHT                  
         BE    DBUY0120                                                         
         DROP  R5                                                               
*                                                                               
         BAS   RE,BLDENTRY         YES, BUILD AN ENTRY FOR XSORT                
         B     DBUY0020            GO BACK AND READ NEXT BUY                    
*                                                                               
DBUY0120 DS    0H                                                               
         MVC   WORK(4),OUTDAY                                                   
         MVC   WORK+4(4),UNTIME                                                 
         MVC   WORK+8(4),DATCON                                                 
         MVC   WORK+12(4),ADDAY                                                 
         MVC   WORK+16(4),VREPFACS                                              
         MVC   WORK+20(4),DEMCON                                                
*                                                                               
         XC    BYTE,BYTE                                                        
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         OI    BYTE,X'80'                                                       
*                                                                               
         CLI   FORMAT,X'FF'        REP?                                         
         BE    DBUY0160                                                         
         CLI   FORMAT,C' '         REP?                                         
         BE    DBUY0160                                                         
*                                                                               
         GOTOR CKSTPROF                                                         
         BE    DBUY0130                                                         
         OI    BYTE,X'02'          NO, DO NOT PRINT DEMO                        
DBUY0130 DS    0H                                                               
         XC    WORK+12(4),WORK+12                                               
         CLI   FORMAT,C'M'         FOR MARKETRON AND                            
         BE    DBUY0140                                                         
         CLI   FORMAT,C'H'         ENTERPRISE II AND                            
         BE    DBUY0140                                                         
         CLI   FORMAT,C'Z'         VSS           AND                            
         BE    DBUY0140                                                         
         CLI   FORMAT,C'X'         VSS/IBS       AND                            
         BE    DBUY0140                                                         
         CLI   FORMAT,C'N'         FOR W.O./KAMAN, DON'T EXPAND                 
         BE    DBUY0140            ALTERNATE WEEKS                              
         CLI   FORMAT,C'K'         FOR KAMAN, DON'T EXPAND                      
         BNE   DBUY0160            ALTERNATE WEEKS                              
DBUY0140 XC    WORK+12(4),WORK+12                                               
DBUY0160 DS    0H                                                               
         MVC   SVDEMFLG,BYTE       SAVE DEMO PRINT OPTION                       
         NI    SVDEMFLG,X'02'      SAVE ONLY DEMO PRINT OPTION                  
         L     R5,AIO2             BUY RECORD                                   
         USING RBUYREC,R5                                                       
         GOTO1 REGENPBY,DMCB,(BYTE,RBUYREC),(32,AWORK4),WORK,RCONREC,  X        
               PROFILES,ACOMFACS                                                
*                                                                               
         MVC   BUYLIN,DMCB+4                                                    
         CLI   BUYLIN,0                                                         
         BE    DBUY0560                                                         
*                                                                               
         MVC   BYTE,DMCB+8         DON'T SEND INDICATOR                         
*                                                                               
         TM    FLTCTRL,FLTH        PRINT FLIGHT HEADER BEFORE BUY               
         BZ    DBUY0200            LINE DATA WHEN THE INDICATOR IS ON           
         NI    FLTCTRL,X'FF'-FLTH  TURN OFF INDICATOR                           
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'40'      ORD CMT ALREADY PRINTED ON WRKSHT?           
         BO    DBUY0180            YES - SKIP FORCING IT TO PRINT NOW           
         OI    TWAWSFLG,X'01'      FORCE WRKSHT HEADER TO PRINT BEFORE          
         DROP  RF                                                               
         BAS   RE,GOFMT            ALL THIS FLIGHT STUFF                        
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'01'                                             
         DROP  RF                                                               
*                                                                               
         CLI   TWAOFFC,C'*'        DDS ONLY??                                   
         BNE   DBUY0180                                                         
         OC    RBUYAGBL,RBUYAGBL                                                
         BZ    DBUY0180                                                         
         MVC   P(25),=C'*** DARE AGENCY BUY LINK:'                              
         EDIT  RBUYAGBL,(3,P+26),ALIGN=LEFT                                     
*                                                                               
         GOTOR XMLBUYLK                                                         
*                                                                               
         BAS   RE,GOSPUL                                                        
         BAS   RE,GOSPUL                                                        
*                                                                               
DBUY0180 OI    FLTCTRL,FLTWT       TURN ON WEEK TOTAL INDICATOR                 
         MVC   P(14),=C'FLIGHT NUMBER '                                         
         EDIT  RBUYFLT,(2,P+14),ALIGN=LEFT                                      
         BAS   RE,GOSPUL                                                        
         BAS   RE,GOSPUL                                                        
*                                                                               
DBUY0200 DS    0H                                                               
         L     R3,AWORK4           POINT TO REGENPBY OUTPUT                     
         USING PD,R3               CHECK IF BUYLINE CANCELLED                   
         CLI   PCHG,C'C'                                                        
         BE    DBUY0220                                                         
         CLI   PCHG+1,C'C'                                                      
         BNE   DBUY0240                                                         
*                                                                               
DBUY0220 DS    0H                  SET FLAG TO PRINT A LINE STATING             
         OI    STATFLAG,HASCANLN   ORDER CONTAINS CANCELLED BUYLINES            
         DROP  R3                                                               
*                                                                               
DBUY0240 DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWACFFLG,X'08'      CFC?                                         
         BO    DBUY0260            FORCE BUYS TO PRINT                          
         CLC   =C'CF',CONACT       FOR ACTION CONFIRM                           
         BE    DBUY0420            DON'T PRINT DETAILS OF BUY LINES             
*                                                                               
*        CLC   =C'CF',CONACT       FOR ACTION CONFIRM                           
*        BNE   DBUY0260                                                         
*        CLI   TWADCFPT,C'Y'       DARE AGENCY NO CONTRACT FLAG ON??            
*        BNE   DBUY0420            NO, DON'T PRINT BUY DETAIL                   
*        CLI   FORMAT,X'FF'        YES, REP?? PRINT BUY DETAIL                  
*        BNE   DBUY0420            ALL ELSE (STATION) DON'T PRINT BUY           
         DROP  RF                                                               
*                                                                               
DBUY0260 CLC   CONACT,=C'LAST'     FOR ACTION LAST                              
         BNE   DBUY0280                                                         
         NI    SVSTAT,X'F9'  TURN OFF CHANGES ONLY & DON'T SEND SWITCH          
         B     DBUY0380            AND PRINT ALL LINES                          
         SPACE 1                                                                
DBUY0280 CLC   CONACT,=C'RSND'     FOR ACTION RESEND                            
         BNE   DBUY0300                                                         
         NI    SVSTAT,X'F9'  TURN OFF CHANGES ONLY & DON'T SEND SWITCH          
         B     DBUY0380            AND PRINT ALL LINES                          
         SPACE 1                                                                
DBUY0300 CLI   FORMAT,X'FF'        REP FORMAT GETS CHANGES ONLY                 
         BNE   DBUY0320            UNLESS PROFILE IS SET                        
*                                                                               
* IF PROFILE SET, SEND RECAP ALWAYS                                             
*                                                                               
         TM    PROFILES+CNTBDELB,CNTBDELA                                       
         BZ    DBUY0320                                                         
         NI    SVSTAT,X'FF'-X'04'  RECAP                                        
         B     DBUY0380                                                         
DBUY0320 DS    0H                                                               
         TM    SVSTAT,X'04'        PRINT CHANGES ONLY?                          
         BZ    DBUY0360            NO                                           
DBUY0340 CLI   SVVER,1             PRINT ALL FOR VERSION 1                      
         BE    DBUY0380                                                         
         L     R3,AWORK4           POINT TO REGENPBY OUTPUT                     
         USING PD,R3                                                            
         CLC   =X'0000',PCHG       IF NO CHANGE CODES                           
         BE    DBUY0420        DON'T PRINT LINE (BUT DO PRINT TOTALS)           
*                                                                               
         CLI   PCHG,C'C'           CANCELLED LINE                               
         BE    *+12                                                             
         CLI   PCHG+1,C'C'                                                      
         BNE   DBUY0360                                                         
         CLC   RBUYVER,SVVER       MAKE SURE CANCELLED IN THIS VER              
         BL    DBUY0420            ELSE, DON'T PRINT                            
         DROP  R3                                                               
*                                                                               
DBUY0360 CLI   FORMAT,X'FF'        REP FORMAT IGNORES DON'T SEND                
         BE    DBUY0380                                                         
         TM    SVSTAT,X'02'        IS IT A DON'T SEND STATION?                  
         BZ    DBUY0380                                                         
         CLI   BYTE,C'D'           IS IT A DON'T SEND BUYLINE?                  
         BE    DBUY0420            YES, BUT STILL INCLUDE IN TOTALS             
*                                                                               
DBUY0380 DS    0H                                                               
         BAS   RE,GOFMT            GO TO FORMAT ROUTINE - PRINT BUYLINE         
*                                                                               
         CLI   TWAOFFC,C'*'        DDS ONLY??                                   
         BNE   DBUY0400                                                         
         OC    RBUYAGBL,RBUYAGBL                                                
         BZ    DBUY0400                                                         
         MVC   P(25),=C'*** DARE AGENCY BUY LINK:'                              
         EDIT  RBUYAGBL,(3,P+26),ALIGN=LEFT                                     
*                                                                               
         GOTOR XMLBUYLK                                                         
*                                                                               
         BAS   RE,GOSPUL                                                        
         BAS   RE,GOSPUL                                                        
*                                                                               
DBUY0400 DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         OI    TWAWSFLG,X'80'      ON FURTHER PGS, PRINT BUYLINE HEADR          
         DROP  RF                                                               
*                                                                               
DBUY0420 TM    RBUYCNTL,X'80'      CANCELLED OR DELETED?                        
         BO    DBUY0560            DON'T ADD INTO TOTALS                        
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BE    DBUY0560                                                         
*                                                                               
* ADD TO TOTAL BUCKETS                                                          
         XC    WORK2X,WORK2X                                                    
         MVC   WORK(4),VGTBROAD                                                 
         MVC   WORK+4(4),GETDAY                                                 
         MVC   WORK+8(4),ADDAY                                                  
         MVC   WORK+12(4),DATCON                                                
*                                                                               
* GET BUY BUCKETS WITH GRPS                                                     
*                                                                               
         GOTO1 REGENBUC,DMCB,RBUYREC,WORK2X,(X'40',WORK)                        
******                                                                          
******   LLC   RF,BYTE4                                                         
******   AHI   RF,1                                                             
******   CHI   RF,2                                                             
******   BNE   *+6                                                              
******   DC    H'0'                                                             
******   STC   RF,BYTE4                                                         
*                                                                               
         CLC   WORK2X(2),=H'2'                                                  
         BE    DBUY0560                                                         
         SR    R0,R0                                                            
* ADD BUY BUCKETS TO TOTAL CONTRACT BUCKETS                                     
         LA    R3,WORK2X+2          FIRST BUCKET                                
DBUY0440 LA    R2,WORK3X+2          TOTAL                                       
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BNO   DBUY0460            NO                                           
*                                                                               
         LR    R2,RA                                                            
         AH    R2,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R2                                                       
         LA    R2,XWORK4+2         YES - TRADE BUCKET                           
         DROP  R2                                                               
*                                                                               
DBUY0460 CLI   0(R2),0             LAST?                                        
         BE    DBUY0480                                                         
         CLC   2(2,R3),2(R2)       COMPARE BUCKET DATES                         
         BL    DBUY0480                                                         
         BE    DBUY0540                                                         
* GET NEXT TOTAL BUCKET                                                         
         IC    R0,1(R2)            LEN                                          
         AR    R2,R0                                                            
         B     DBUY0460                                                         
*                                                                               
* ADD NEW TOTAL BUCKET                                                          
DBUY0480 DS    0H                                                               
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BO    DBUY0500            YES                                          
         GOTO1 VRECUP,DMCB,(X'FF',WORK3X),(R3),(R2)                             
         B     DBUY0520                                                         
DBUY0500 DS    0H                                                               
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RE                                                       
         LA    RE,XWORK4           YES - TRADE BUCKET                           
         DROP  RE                                                               
*                                                                               
         ST    RE,DMCB                                                          
         MVI   DMCB,X'FF'                                                       
*                                                                               
         GOTO1 VRECUP,DMCB,,(R3),(R2)                                           
*                                                                               
* GET NEXT BUY BUCKET                                                           
DBUY0520 IC    R0,1(R3)            LENGTH                                       
         AR    R3,R0                                                            
         CLI   0(R3),0             LAST?                                        
         BE    DBUY0560                                                         
         B     DBUY0440                                                         
* ADD TO TOTAL BUCKET                                                           
DBUY0540 MVC   DUB(8),6(R3)                                                     
         LM    RE,RF,DUB                                                        
         MVC   DUB(8),6(R2)                                                     
         A     RE,DUB                                                           
         A     RF,DUB+4                                                         
         STM   RE,RF,DUB                                                        
         MVC   6(8,R2),DUB                                                      
*                                                                               
         CLI   1(R3),14            SKIP IF NO GRPS IN ELEMENT                   
         BNH   DBUY0545                                                         
*                                                                               
         ICM   RF,15,14(R3)        ADD TO TOTAL GRPS                            
         ICM   RE,15,14(R2)                                                     
         AR    RF,RE                                                            
         STCM  RF,15,14(R2)                                                     
*                                                                               
DBUY0545 DS    0H                                                               
*                                                                               
         B     DBUY0520                                                         
*                                                                               
DBUY0560 TM    FLTCTRL,FLT         IS FLIGHT INDICATOR ON?                      
         BZ    DBUY0020            NO, GO TO SEQUENTIAL READ                    
         LA    R6,L'ENTREC(R6)     INCREMENT POINTER                            
         ZIC   R3,FLTNDX           INSERT COUNTER OF FLIGHTED BUYS              
         BCTR  R3,0                DECREMENT IT                                 
         STC   R3,FLTNDX                                                        
         LTR   R3,R3               KEEP PROCESSING UNTIL NO MORE                
         BNZ   DBUY0060            ENTRIES IN SORT BUFFER                       
*                                  FOR CANCELLED FLIGHTS                        
         TM    FLTCTRL,FLTWT       IF FLIGHT HEADER NOT PRINTED                 
         BZ    *+8                 SHOULD NOT PRINT WEEK TOTALS                 
         BAS   RE,WKTOT            PUT OUT TOTALS FOR LAST FLIGHT               
*                                                                               
*        DOBUYS EXIT                                                            
*                                                                               
DBUY0580 EQU   *                                                                
*                                                                               
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
         DROP  R4,R5                                                            
         EJECT                                                                  
*********************************************************************           
* UPDATE WEEKLY BUCKET TOTALS                                                   
*********************************************************************           
         USING RBUYREC,R5                                                       
UPWKTOT  ST    RE,FULL                                                          
         TM    RBUYCNTL,X'80'      DO NOT INCLUDE DELETED LNE                   
         BOR   RE                                                               
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BER   RE                                                               
         ZIC   RE,RBUYNW           NUMBER PER WEEK                              
         LR    R0,RE                                                            
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BO    UPWKT10             YES                                          
         AH    R0,HALF2            UPDATE CASH SPOTS/WK BUCKET                  
         STH   R0,HALF2                                                         
         B     UPWKT20                                                          
UPWKT10  AH    R0,TEMP+4           UPDATE TRADE SPOTS/WK BUCKET                 
         STH   R0,TEMP+4                                                        
UPWKT20  ICM   RF,15,RBUYCOS                                                    
         MR    RE,RE               COST/SPOT X SPOTS/WK                         
         TM    RBUYFLG2,X'02'      TRADE BUY?                                   
         BO    UPWKT30             YES                                          
         A     RF,SAVETOT          UPDATE CASH COST/WK BUCKET                   
         ST    RF,SAVETOT                                                       
         B     UPWKTX                                                           
UPWKT30  A     RF,TEMP             UPDATE TRADE COST/WK BUCKET                  
         ST    RF,TEMP                                                          
UPWKTX   L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO PRINT WEEKLY TOTAL LNE                                             
*********************************************************************           
WKTOT    ST    RE,FULL                                                          
         MVC   P+25(24),=C'**WEEKLY FLIGHT TOTALS**'                            
*                                                                               
         EDIT  (2,HALF2),(3,P+52)               CASH                            
         EDIT  (4,SAVETOT),(10,P+57),2,FLOAT=-                                  
         OC    HALF2,HALF2                                                      
         BZ    WKTOT05                                                          
         BAS   RE,GOSPUL                                                        
*                                                                               
WKTOT05  DS    0H                                                               
         OC    TEMP+4(2),TEMP+4                                                 
         BZ    WKTOT08                                                          
         EDIT  (2,TEMP+4),(3,P+52)              TRADE                           
         MVI   P+56,C'T'                                                        
         EDIT  (4,TEMP),(10,P+57),2,FLOAT=-                                     
         BAS   RE,GOSPUL                                                        
WKTOT08  DS    0H                                                               
         BAS   RE,GOSPUL                                                        
WKTOT10  XC    HALF2,HALF2         ZERO BUCKETS FOR SPOTS                       
         XC    SAVETOT,SAVETOT     AND DOLLARS                                  
         XC    TEMP(6),TEMP        AND TRADE DOLLARS&SPOTS                      
*                                                                               
WKTOTEX  DS    0H                                                               
         NI    FLTCTRL,X'FF'-FLTWT RESET WEEK TOTALS                            
         NI    FLTCTRL,X'FF'-FIRST TURN OFF INDICATOR                           
         L     RE,FULL                                                          
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO BUILD A FLIGHT ENTRY FOR LATER XSORT CALL                          
*********************************************************************           
BLDENTRY DS    0H                                                               
         L     R5,AIO2                                                          
         USING RBUYREC,R5                                                       
         TM    RBUYCNTL,X'80'      BUILD A FLIGHT ENTRY FOR                     
         BZ    *+10                CANCELLED, BUT NOT DELETED BUYS.             
         CLI   RBUYCHGI,C'X'                                                    
         BER   RE                                                               
*                                                                               
*        L     R6,ASPULAR          AREA FOR FLIGHT SORT ENTRIES                 
*        A     R6,=AL4(4000)                                                    
         USING ENTD,R6                                                          
*                                                                               
         MVC   ENTFLT,RBUYFLT      FLIGHT NUMBER                                
         MVC   ENTMAS,RBUYKMLN     MASTER LNE                                   
         MVC   ENTLIN,RBUYKLIN     LNE                                          
         MVC   ENTDA,KEY+28        DISK ADDRESS                                 
         ZIC   R1,FLTNDX                                                        
         LA    R1,1(R1)            INCREMENT COUNTER                            
         STC   R1,FLTNDX                                                        
         LA    R6,L'ENTREC(R6)     INCREMENT POINTER TO XSORT ENTRIES           
         BR    RE                                                               
         DROP  R5,R6                                                            
         EJECT                                                                  
*********************************************************************           
* SUB-ROUTINES TO SUPPORT THE SORTING OF FLIGHTED BUYS, PRINTING A              
* LABEL ABOVE BUYS OF THE SAME FLIGHT, MAINTAINING BUCKETS OF WEEKLY            
* SPOTS AND DOLLARS FOR THE FLIGHT, AND PRINTING THE TOTALS                     
*********************************************************************           
SORTFLT  EQU   *                   SORT ENTRIES OF FLIGHTED BUYS                
         ST    RE,FULL                                                          
         OI    FLTCTRL,FLT+FIRST   TURN ON INDICATORS FOR ENTRY TO FLT          
*                                                                               
         L     R6,ASPULAR          AREA FOR FLIGHT SORT ENTRIES                 
         A     R6,=AL4(4000)                                                    
*                                                                               
         ZIC   R3,FLTNDX           INSERT COUNTER OF FLIGHTED BUYS              
         XC    DMCB+16(4),DMCB+16                                               
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CXSORT                                                        
         DROP  RE                                                               
*                                                                               
         GOTO1 (RF),DMCB,(R6),(R3),L'ENTREC,3                                   
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*********************************************************************           
* GO TO FORMAT OVERLAY                                                          
*********************************************************************           
SETFMT   MVI   DMCB+8,1                                                         
         B     *+8                                                              
GOFMT    MVI   DMCB+8,0                                                         
         LR    R0,RE                                                            
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         OI    TWAWSFLG,X'04'           REQUEST SEQ LOOP BE RESTORED            
         DROP  RF                                                               
         GOTO1 AFMT,DMCB,(RC),(R7)                                              
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         OI    TWAWSFLG,X'04'           REQUEST SEQ LOOP BE RESTORED            
         NI    TWAWSFLG,X'FF'-X'04'     RESTORE DONE                            
         DROP  RF                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*********************************************************************           
* SAVE SOME OVERHEAD GOTO1 CALLS                                                
*********************************************************************           
XMLBUYLK NTR1                                                                   
         L     R6,AIO2                                                          
         MVI   ELCODE,X'1D'                                                     
         BRAS  RE,GETEL                                                         
         BNE   XMLBX                                                            
         USING RBUYDREL,R6                                                      
         OC    RBUYDRXL,RBUYDRXL                                                
         BZ    XMLBX                                                            
         EDIT  RBUYDRXL,(4,P+26),ALIGN=LEFT                                     
XMLBX    DS    0H                                                               
         XIT1                                                                   
         DROP  R6                                                               
*********************************************************************           
* SAVE SOME OVERHEAD GOTO1 CALLS                                                
*********************************************************************           
GOSPUL   NTR1                                                                   
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
         XIT1                                                                   
*********************************************************************           
*        TEST EXIT, CONSTANTS, LITERAL POOL, ETC.                               
*********************************************************************           
         PRINT GEN                                                              
         ANSR                                                                   
*                                     YES, NO OR XIT                            
         PRINT NOGEN                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DASH     DC    51C'-'                                                           
         SPACE 2                                                                
***********************************************************************         
*        WORKSHEET FORMAT CONTROL TABLE                                         
*                                                                               
*        -CL1  FORMAT CODE                                                      
*        -XL1  RCSUBPRG CODE                                                    
*        -XL1  OV NUMBER TO PRODUCE THE WORKSHEET                               
***********************************************************************         
WKFORMAT EQU   *                                                                
         DC    C'B',X'01',X'66'    BIAS FORMAT                                  
         DC    C'C',X'05',X'66'    COLUMBINE FORMAT                             
         DC    C'E',X'05',X'66'    WIDE ORBIT COLUMBINE FORMAT                  
         DC    C'O',X'09',X'66'    WIDE ORBIT (O) EC - USE REP FMT              
         DC    C'N',X'01',X'68'    WIDE ORBIT (N) EC - USE KAMAN FMT            
****     DC    C'L',X'09',X'66'    WIDE ORBIT EC - USE REP FMT                  
         DC    C'L',X'01',X'68'    MARKETRON/WIDE ORBIT - USE KAMAN             
         DC    C'I',X'01',X'68'    OSI           - USE KAMAN FMT                
         DC    C'V',X'05',X'66'    VCI TRAFFIC/COLUMBINE PAPER FORMAT           
         DC    C'T',X'01',X'66'    VCI/BDE TRAF/BIAS PAPER FORMAT               
*!!!!    DC    C'S',X'07',X'61'    VCI STAR PLUS                                
*                                                                               
* VCI STAR PLUS REPORT IS IN RECNT61 ALONG WITH ENTERPRISE II                   
* A NEW CODE NEEDS TO BE DECIDED UPON FOR THE VCI STAR PLUS                     
* SINCE THE VCI STAR II WILL NOW USE CODE 'S'                                   
* ONCE A NEW CODE IS DECIDED, CORRESPONDING CHANGES HAVE TO                     
* BE MADE IN RECNT61 FOR THE FIELD 'FORMAT'                                     
* - 13MAR00 SCH                                                                 
*                                                                               
***>>    DC    C'S',X'07',X'79'    VCI STAR II                                  
         DC    C'S',X'01',X'79'    VCI STAR III                                 
         DC    C'U',X'01',X'79'    VCI/BDE STAR III                             
         DC    C'D',X'01',X'79'    VCI/W.O. STAR III                            
         DC    C'G',X'01',X'6C'    GRAPHNET FORMAT                              
         DC    C'J',X'07',X'67'    JEFFERSON FORMAT                             
         DC    C'K',X'01',X'68'    KAMAN FORMAT                                 
         DC    C'Z',X'01',X'68'    VSS   FORMAT                                 
         DC    C'H',X'01',X'61'    ENTERPRISE II FORMAT                         
         DC    C'M',X'03',X'68'    MARKETRON FORMAT                             
         DC    C'P',X'03',X'6C'    WPVI FORMAT                                  
         DC    C'A',X'01',X'69'    RADIO FORMAT                                 
         DC    C'W',X'01',X'6D'    BIAS COPY DOWN FORMAT (WUSA)                 
         DC    C'X',X'07',X'67'    VSS/IBS   FORMAT                             
*                                     PRODUCES 'J' WORKSHEET                    
         DC    C' ',X'03',X'67'    *DEFAULT FORMAT = REP*                       
         DC    X'00'                                                            
         SPACE 3                                                                
*                                                                               
*        WORKSHEET FORMAT CONTROL TABLE FOR STORED COMMENTS PAGES               
*                                                                               
*        -CL1  FORMAT CODE                                                      
*        -XL1  RCSUBPRG CODE                                                    
*        -XL1  RCSUBPRG CODE FOR AGY ADDRESS IF PROFILE ON                      
*                                                                               
SCPAGES  DS    0CL2                                                             
         DC    C'B',X'03'          BIAS FORMAT                                  
         DC    C'C',X'07'          COLUMBINE FORMAT                             
         DC    C'E',X'07'          WIDE ORBIT / COLUMBINE FORMAT                
         DC    C'G',X'01'          GRAPHNET FORMAT                              
         DC    C'J',X'09'          JEFFERSON FORMAT                             
         DC    C'X',X'09'          VSS/IBS   FORMAT                             
         DC    C'K',X'05'          KAMAN FORMAT                                 
         DC    C'N',X'05'          KAMAN FORMAT (WIDE ORBIT USE)                
         DC    C'Z',X'05'          VSS   FORMAT                                 
         DC    C'H',X'05'          ENTERPRISE II FORMAT                         
         DC    C'M',X'06'          MARKETRON FORMAT                             
         DC    C'P',X'02'          WPVI FORMAT                                  
         DC    C'A',X'01'          RADIO FORMAT                                 
         DC    C'W',X'03'          BIAS COPY DOWN FORMAT (WUSA)                 
         DC    C'R',X'05'          *DEFAULT FORMAT = REP*                       
         DC    C'O',X'03'          WIDE ORBIT USES BIAS FORMAT                  
         DC    C'L',X'03'          WIDE ORBIT USES BIAS FORMAT                  
         DC    C' ',X'05'          *DEFAULT FORMAT = REP*                       
         DC    X'FF',X'05'         *DEFAULT FORMAT = REP*                       
         DC    X'00'                                                            
         SPACE 3                                                                
MONTBL   DS    0CL4                                                             
         DC    X'01',C'JAN'                                                     
         DC    X'02',C'FEB'                                                     
         DC    X'03',C'MAR'                                                     
         DC    X'04',C'APR'                                                     
         DC    X'05',C'MAY'                                                     
         DC    X'06',C'JUN'                                                     
         DC    X'07',C'JUL'                                                     
         DC    X'08',C'AUG'                                                     
         DC    X'09',C'SEP'                                                     
         DC    X'0A',C'OCT'                                                     
         DC    X'0B',C'NOV'                                                     
         DC    X'0C',C'DEC'                                                     
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE RERISKTAB                                                      
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         ORG   LOCALTWA                                                         
XWORK4   DS    CL240                                                            
XWORK5   DS    CL240                                                            
XWORK6   DS    CL240                                                            
XWORKTWA DS    CL200                                                            
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE REGENPBYD                                                      
         EJECT                                                                  
       ++INCLUDE RECNTFMTD                                                      
       ++INCLUDE REGENDAR                                                       
       ++INCLUDE DDCOMFACS                                                      
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDPERVALD                                                      
*                                                                               
* DSECT TO COVER FLIGHTED BUY ENTRIES                                           
*                                                                               
ENTD     DSECT                                                                  
ENTREC   DS    0CL7                                                             
ENTFLT   DS    X                                                                
ENTMAS   DS    X                                                                
ENTLIN   DS    X                                                                
ENTDA    DS    XL4                                                              
         TITLE 'T80263 - RECNT63 - ELECTRONIC CONTRACT'                         
                                                                                
***********************************************************************         
* PRINT TOTAL                                                                   
***********************************************************************         
T80263   CSECT                                                                  
PTOTAL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*GET CONTRACT TOTALS                                                            
         BAS   RE,GOSPUL                                                        
         TM    SVCNTL,X'80'        DELETED K                                    
         BZ    PTOT0040                                                         
         MVC   P+10(24),=C'*** CONTRACT DELETED ***'                            
         BAS   RE,GOSPUL                                                        
         B     PTOT0660            EXIT, DON'T WORRY ABOUT STORED CMT           
*                                                                               
PTOT0040 DS    0H                                                               
         TM    RCONMODR+1,X'20'    MON???                                       
         BZ    PTOT0220                                                         
*                                                                               
*******************************************************************             
*                                                                 *             
* WORK3X IS THE STARTING BUCKET TOTAL   (BUILT IN PTOT0140)       *             
*    AREA+0(2)=YYMM                                               *             
*    AREA+2(4)=DOLLARS                                            *             
*                                                                 *             
*******************************************************************             
*                                                                               
         BAS   RE,GOSPUL                                                        
* BUILD TOTAL BUCKETS                                                           
PTOT0060 DS    0H                                                               
         LA    R5,WORK3X                                                        
         XCEF  (R5),240                                                         
         XR    R3,R3               KEEP TOTAL $ COUNTER                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PTOT0120            NO BUCKETS YET                               
PTOT0080 MVC   0(2,R5),2(R6)       SAVE BUCK YYMM                               
PTOT0100 L     R1,6(R6)            R1=BUCK AMT                                  
         AR    R3,R1               TOTAL TOTAL (ALL MOS)                        
         A     R1,2(R5)            ADD RUNNING TOTAL                            
         ST    R1,2(R5)                                                         
         BRAS  RE,NEXTEL                                                        
         BNE   PTOT0120                                                         
         CLC   0(2,R5),2(R6)       SAME BDCST MON?                              
         BE    PTOT0100                                                         
         LA    R5,6(R5)            NEXT TOTAL BUCKET                            
         B     PTOT0080                                                         
PTOT0120 DS    0H                                                               
         ST    R3,WORK2X                                                        
PTOT0140 DS    0H                                                               
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK,0,DUB                        
*                                                                               
*    WORK HAS EBCDIC START AND WORK+6 HAS END DATE                              
*                                                                               
         GOTO1 VGTBROAD,DMCB,(1,WORK),TEMP,GETDAY,ADDAY                         
         GOTO1 VGTBROAD,DMCB,(1,WORK+6),TEMP+12,GETDAY,ADDAY                    
*                                                                               
*    TEMP+6 HAS END DATE OF BROADCAST MONTH OF K START DATE                     
*    TEMP+18 HAS END DATE OF BROADCAST MONTH OF K END DATE                      
*                                                                               
         GOTO1 DATCON,DMCB,(0,TEMP+6),(3,WORK)                                  
         GOTO1 DATCON,DMCB,(0,TEMP+18),(3,WORK+3)                               
*                                                                               
*    WORK NOW HAS START BROADCAST MONTH & YEAR AND WORK+3 HAS END               
*    BROADCAST MONTH AND YEAR (YMD - BINARY)                                    
*                                                                               
         LA    R5,WORK3X                                                        
         MVC   YEAR,TEMP+6         K START YEAR                                 
         MVC   BYEAR,WORK          SAVE BIN YEAR                                
         ZIC   R4,WORK+1           START MON                                    
         ZIC   R3,WORK+4           END MON                                      
         CLC   WORK(1),WORK+3      SAME YEAR?                                   
         BE    *+8                                                              
         LA    R3,12(R3)           ADJUST FOR NEXT YEAR                         
         SR    R3,R4                                                            
         LA    R3,1(R3)            NUM MONTHS                                   
         LR    R1,R3               GET NUM OF LINES                             
         SRL   R1,2                DIV BY 4, FUCK THE REMAINDER                 
         LA    R1,1(R1)                                                         
         LR    R0,R1               CAN'T 'LA' R0...                             
         BCTR  R4,0                MONTHS ARE 0 RELATIVE                        
         MH    R4,=Y(L'MONTBL)     R4 NOW AT STARTING MONTH                     
         LA    R4,MONTBL(R4)       IT HELPS TO POINT IT AT THE TABLE            
         L     R2,AWORK4           OUTPUT AREA                                  
*                                                                               
         XCEF  (R2),2592           CLEAR FOR COMBO MULTIPLE USE                 
*                                                                               
PTOT0160 DS    0H                                                               
         MVC   0(3,R2),1(R4)       MMM                                          
         MVC   3(2,R2),YEAR        YY                                           
         LA    R2,7(R2)                                                         
*                                                                               
* DISPLAY $ (IF ANY)                                                            
         CLC   0(1,R5),BYEAR       THIS YEAR?                                   
         BNE   PTOT0180                                                         
         CLC   1(1,R5),0(R4)       THIS MONTH?                                  
         BNE   PTOT0180                                                         
         LR    R1,R0               EDIT KILLS R0                                
         EDIT  (4,2(R5)),(10,0(R2)),2                                           
         LR    R0,R1                                                            
         LA    R5,6(R5)            SET NEXT TOTAL BUCKET                        
PTOT0180 LA    R2,13(R2)                                                        
         LA    R4,L'MONTBL(R4)     NEXT MON                                     
         CLI   0(R4),X'FF'         END OF TABLE?                                
         BNE   PTOT0200            NO - CONTINUE                                
         LA    R4,MONTBL           BACK TO START OF TBL                         
         MVC   YEAR,TEMP+18        K END YEAR (EBCDIC)                          
         ZIC   R1,BYEAR                                                         
         LA    R1,1(R1)                                                         
         STC   R1,BYEAR                                                         
PTOT0200 BCT   R3,PTOT0160                                                      
         LR    R5,R0               EDIT KILLS R0                                
         MVC   0(05,R2),=C'TOTAL'                                               
         EDIT  (4,WORK2X),(10,7(R2)),2,FLOAT=$                                  
         LR    R0,R5                                                            
         L     R5,AWORK4                                                        
         B     PTOT0300                                                         
PTOT0220 EQU   *                                                                
*                                                                               
         GOTO1 REGENTL2,DMCB,(SVDEMFLG,WORK3X),AWORK4                           
         ZIC   R0,DMCB+4                                                        
         L     R5,AWORK4                                                        
*                                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RE                                                       
         CLC   =X'0002',XWORK4     ANY TRADE DOLLARS IN ARRAY?                  
         BE    PTOT0240            NO  - PROCESS AS REGULAR ORDER &             
         DROP  RE                                                               
*                                     DON'T SHOW 'CASH' BANNER                  
*                                  YES - SHOW 'CASH' BANNER IF THERE            
*                                     IS ACTUALLY CASH                          
         CLC   =X'0002',WORK3X     ANY CASH DOLLARS IN ARRAY?                   
         BE    PTOT0320            NO  - SKIP CASH DISPLAY TOTALLY              
*                                  YES - DISPLAY 'CASH' BANNER                  
         MVC   P(08),=C'**CASH**'                                               
         MVC   P+58(08),=C'**CASH**'                                            
         BAS   RE,GOSPUL                                                        
PTOT0240 EQU   *                                                                
         CLI   FORMAT,C'G'         GRAPHNET NEEDS ITS OWN FORMAT                
         BNE   PTOT0300            'CAUSE IT'S ONLY 69 CHARACTERS WIDE          
*                                  SO PRINT TOTALS FIRST                        
         CLI   60(R5),C'T'         TOTAL STARTS IN COL 59 OR 60                 
         BE    PTOT0260                                                         
         MVC   P(21),59(R5)        COL 59                                       
         MVC   P+16(6),139(R5)                                                  
*                                                                               
         CLI   219(R5),C'G'        IF GRPS PRESENT                              
         BNE   *+10                                                             
         MVC   P+25(16),219(R5)    GRPS                                         
*                                                                               
         B     *+16                                                             
PTOT0260 MVC   P(20),60(R5)        COL 60                                       
         MVC   P+16(6),140(R5)                                                  
*                                                                               
         CLI   220(R5),C'G'        IF GRPS PRESENT                              
         BNE   *+10                                                             
         MVC   P+25(16),220(R5)    GRPS                                         
*                                                                               
         BAS   RE,GOSPUL                                                        
         BAS   RE,GOSPUL                                                        
PTOT0280 MVC   P(59),0(R5)         THEN PRINT THE MONTHS                        
         BAS   RE,GOSPUL                                                        
         LA    R5,80(R5)                                                        
         BCT   R0,PTOT0280                                                      
         B     PTOT0320                                                         
         SPACE 1                                                                
PTOT0300 MVC   P(80),0(R5)                                                      
         BAS   RE,GOSPUL                                                        
         LA    R5,80(R5)                                                        
         BCT   R0,PTOT0300                                                      
PTOT0320 EQU   *                                                                
         LR    R2,RA                                                            
         AH    R2,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R2                                                       
*                                                                               
         CLC   =X'0002',XWORK4     ANY TRADE DOLLARS IN ARRAY?                  
         BE    PTOT0360            NO                                           
*                                  YES - ACCUM   DOLLAR TYPE                    
         OC    XWORK5,XWORK5       ANY VALUE IN 'CASH TOTAL'                    
         BNZ   PTOT0340            YES - ACCUMULATE, DON'T LOAD                 
         MVC   XWORK5,WORK3X       NO  - LOAD, DON'T ACCUMULATE                 
         B     PTOT0360                                                         
PTOT0340 EQU   *                                                                
         GOTO1 =A(GENLTOTL),DMCB,XWORK5,WORK3X,RR=Y                             
PTOT0360 EQU   *                                                                
*                                                                               
         GOTO1 REGENTL2,DMCB,(SVDEMFLG,XWORK4),(R5)                             
         ZIC   R0,DMCB+4           TOTAL LINES                                  
*                                                                               
PTOT0380 DS    0H                                                               
*                                  YES - DISPLAY DOLLAR TYPE                    
         CLC   =X'0002',XWORK4     ANY TRADE DOLLARS IN ARRAY?                  
         BE    PTOT0520            NO  - ONLY CASH FIGURES SHOWN                
*                                  YES - DISPLAY DOLLAR TYPE                    
         MVC   P(09),=C'**TRADE**'                                              
         MVC   P+58(09),=C'**TRADE**'                                           
         BAS   RE,GOSPUL                                                        
*   *PRINT LOOP 1                                                               
         CLI   FORMAT,C'G'         GRAPHNET NEEDS ITS OWN FORMAT                
         BNE   PTOT0440            'CAUSE IT'S ONLY 69 CHARACTERS WIDE          
*                                  SO PRINT TOTALS FIRST                        
         CLI   60(R5),C'T'         TOTAL STARTS IN COL 59 OR 60                 
         BE    PTOT0400                                                         
         MVC   P(21),59(R5)        COL 59                                       
         MVC   P+16(6),139(R5)                                                  
         B     *+16                                                             
PTOT0400 MVC   P(20),60(R5)        COL 60                                       
         MVC   P+16(6),140(R5)                                                  
         BAS   RE,GOSPUL                                                        
         BAS   RE,GOSPUL                                                        
PTOT0420 MVC   P(59),0(R5)         THEN PRINT THE MONTHS                        
         BAS   RE,GOSPUL                                                        
         LA    R5,80(R5)                                                        
         BCT   R0,PTOT0420                                                      
         B     PTOT0460                                                         
PTOT0440 MVC   P(80),0(R5)                                                      
         BAS   RE,GOSPUL                                                        
         LA    R5,80(R5)                                                        
         BCT   R0,PTOT0440                                                      
PTOT0460 EQU   *                                                                
**PRINT LOOP 1                                                                  
PTOT0480 DS    0H                                                               
*                                                                               
         CLC   =X'0002',XWORK4     ANY TRADE DOLLARS IN ARRAY?                  
         BE    PTOT0520            NO  - ONLY CASH FIGURES SHOWN                
*                                  YES - DISPLAY DOLLAR TYPE                    
         OC    XWORK6,XWORK6       ANY VALUE IN 'TRADE TOTAL'                   
         BNZ   PTOT0500            YES - ACCUMULATE, DON'T LOAD                 
         MVC   XWORK6,XWORK4       NO  - LOAD, DON'T ACCUMULATE                 
         B     PTOT0520                                                         
PTOT0500 EQU   *                                                                
         GOTO1 =A(GENLTOTL),DMCB,XWORK6,XWORK4,RR=Y                             
*                                  TRADE FIGURES (XWORK4) ADDED IN              
PTOT0520 DS    0H                                                               
         CLC   =X'0002',WORK3X     ANY CASH  DOLLARS IN ARRAY?                  
         BE    PTOT0660            NO  - TOTALS NOT NEEDED                      
*                                                                               
         CLC   =X'0002',XWORK4     YES - ANY TRADE DOLLARS IN ARRAY?            
         BE    PTOT0660            NO  - TOTALS NOT NEEDED                      
*                                                                               
         MVC   XWORKTWA,XWORK4     YES - LOAD TRADE TOTALS                      
         GOTO1 =A(TOTLTOTL),DMCB,(RC),XWORKTWA,RR=Y                             
*                                  WILL ADD IN CASH TOTALS                      
         GOTO1 REGENTL2,DMCB,(SVDEMFLG,XWORKTWA),(R5)                           
         ZIC   R0,DMCB+4           TOTAL LINES                                  
*                                                                               
PTOT0540 DS    0H                                                               
*                                  YES - DISPLAY DOLLAR TYPE                    
         MVC   P(14),=C'**CASH+TRADE**'                                         
         MVC   P+58(14),=C'**CASH+TRADE**'                                      
         BAS   RE,GOSPUL                                                        
PTOT0560 DS    0H                                                               
*   *PRINT LOOP 2                                                               
         CLI   FORMAT,C'G'         GRAPHNET NEEDS ITS OWN FORMAT                
         BNE   PTOT0620            'CAUSE IT'S ONLY 69 CHARACTERS WIDE          
*                                  SO PRINT TOTALS FIRST                        
         CLI   60(R5),C'T'         TOTAL STARTS IN COL 59 OR 60                 
         BE    PTOT0580                                                         
         MVC   P(21),59(R5)        COL 59                                       
         MVC   P+16(6),139(R5)                                                  
         B     *+16                                                             
PTOT0580 MVC   P(20),60(R5)        COL 60                                       
         MVC   P+16(6),140(R5)                                                  
         BAS   RE,GOSPUL                                                        
         BAS   RE,GOSPUL                                                        
PTOT0600 MVC   P(59),0(R5)         THEN PRINT THE MONTHS                        
         BAS   RE,GOSPUL                                                        
         LA    R5,80(R5)                                                        
         BCT   R0,PTOT0600                                                      
         B     PTOT0640                                                         
PTOT0620 MVC   P(80),0(R5)                                                      
         BAS   RE,GOSPUL                                                        
         LA    R5,80(R5)                                                        
         BCT   R0,PTOT0620                                                      
PTOT0640 EQU   *                                                                
**PRINT LOOP 2                                                                  
*                                                                               
PTOT0660 DS    0H                                                               
         BAS   RE,GOSPUL                                                        
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*- FINDID -- SELECT SEND ID AND FORMAT BASED ON SEND PASS NUMBER                
*            AND CALLER.  (SEE CHART AT BEGINNING OF SOURCE CODE)               
*                                                                               
*  INPUT                                                                        
*        AIO   A(CONTRACT RECORD)                                               
*        AIO3  A(STATION RECORD)                                                
*        SENDPASS  1, 2, OR 3 (BINARY)                                          
*                  (1 ALWAYS DENOTES ORIGINATOR)                                
*                                                                               
*  RETURN                                                                       
*        BYTE  PARAMETER INTO PQOPEN                                            
*              0 = GRAPHNET                                                     
*              'W' = WESTERN UNION                                              
*        CC    ZERO  = GOOD EXIT.  SEND ID AND FORMAT FILLED IN                 
*        CC    NON-0 = COPY NOT REQUIRED.  EXIT ASAP.                           
*                                                                               
*********************************************************************           
*********************************************************************           
*  ONLY PRINT SPL DATA ON VER 1, OR IF IT'S UPDATED SINCE LAST SEND             
*********************************************************************           
DOSPL    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BAS   RE,GOSPUL                                                        
         SPACE 1                                                                
         GOTO1 =A(DOXSPL),DMCB,(RC),RR=Y                                        
         SPACE 1                                                                
* IS THERE SPL DATA TO PRINT?                                                   
         OC    STALST,STALST                                                    
         BNZ   DSPL10                                                           
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWAPRFK,X'20'  SKIP LINE WHEN CONTYPE K OPTION#3 ON              
         BO    DSPL80                                                           
         MVC   P(29),=C'COMPETITIVE NOT YET AVAILABLE'                          
         B     DSPL80                                                           
         DROP  RF                                                               
         SPACE 1                                                                
* PRINT SPL DATA                                                                
DSPL10   LA    R4,P+17                                                          
         L     R2,MKTOT                                                         
*                                                                               
         CLI   FORMAT,C'A'         PRINT COMPETITIVE ONLY FOR                   
         BE    DSPL15              FORMAT A AND RADIO REP WORKSHEETS            
         CLI   FORMAT,C' '         REP FORMAT?                                  
         BE    DSPL13                                                           
         CLI   FORMAT,X'FF'        REP FORMAT?                                  
         BNE   DSPL14                                                           
*                                                                               
DSPL13   DS    0H                  BUT IS IT RADIO REP?                         
         CLI   RCONKSTA+4,C' '                                                  
         BE    DSPL14                                                           
         CLI   RCONKSTA+4,C'L'                                                  
         BNE   DSPL15                                                           
*                                                                               
DSPL14   DS    0H                  NO, PRINT TOTAL AND PERCENT MARKET           
         MVC   P(13),=C'MARKET TOTALS'                                          
*                                                                               
         OC    MKTOT,MKTOT                                                      
         BNZ   *+14                                                             
         MVC   7(2,R4),=C'$0'      PRINT ZERO DOLLARS                           
         B     DSPL18                                                           
         EDIT  (R2),(10,0(R4)),FLOAT=$,COMMAS=YES    OR EDIT AMOUNT             
         B     DSPL18                                                           
*                                                                               
DSPL15   DS    0H                  YES, PRINT THIS FOR FORMAT A AND             
         MVC   P(11),=C'COMPETITIVE' RADIO REP WORKSHEETS                       
*                                                                               
DSPL18   DS    0H                                                               
         LR    RF,RA               IF COMBO PRINT THE COMBO STATIONS            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0                                                       
         BE    DSPL50                                                           
*                                                                               
         LA    R5,TWACMBS1         LOOP TO PRINT COMBO STATIONS                 
DSPL20   OC    0(L'TWACMBS1,R5),0(R5)                                           
         BZ    DSPL30                                                           
         MVC   0(L'TWACMBS1,R4),0(R5)                                           
         LA    R1,TWACMBS4                                                      
         CR    R5,R1                                                            
         BNL   DSPL30                                                           
         LA    R5,9(R5)                                                         
         LA    R4,10(R4)                                                        
         B     DSPL20                                                           
         DROP  RF                                                               
*                                                                               
DSPL30   DS    0H                                                               
         BAS   RE,GOSPUL                                                        
*                                                                               
DSPL50   DS    0H                                                               
         CLI   FORMAT,C'A'         RADIO?                                       
         BE    DSPL75              DIFFERENT DISPLAY (EPL)                      
         LA    R4,P+29                                                          
         CLI   FORMAT,C'G'         GRAPHNET NEEDS ITS OWN FORMAT                
         BNE   DSPL60              'CAUSE IT'S ONLY 69 CHARACTERS WIDE          
*                                  SO PRINT MARKET TOTALS FIRST                 
         BAS   RE,GOSPUL                                                        
         LA    R4,P+2              THEN PRINT THE STATIONS                      
*                                                                               
DSPL60   DS    0H                                                               
         L     R6,AIO              PRINT STATIONS FROM EPL MINI ELEMENT         
         USING RCONSPEL,R6                                                      
         MVI   ELCODE,6                                                         
         BRAS  RE,GETEL                                                         
         BNE   DSPL80                                                           
         ST    R6,SAVASPL          PASS A(RCONSPEL) THROUGH                     
         CLI   RCONSPNU,0          ANY STATIONS??                               
         BE    DSPL80                                                           
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0          IF COMBO, WE'VE ALREADY PRINTED              
         BNE   DSPL62              THE MOTHER STATION.  SO, SKIP IT             
*                                                                               
         DROP  RF                                                               
*                                                                               
         ZIC   R5,RCONSPNU         NUMBER OF STATIONS                           
         LA    R6,RCONSPST         POINT TO MINI ELEMENT                        
         B     DSPL63                                                           
*                                                                               
DSPL62   CLI   RCONSPNU,1          THIS MEANS IT DOESN'T HAVE ANY               
         BE    DSPL80              STATIONS                                     
         ZIC   R5,RCONSPNU         NUMBER OF STATIONS                           
         BCTR  R5,0                SUBTRACT 1 FROM NUMBER OF STATIONS           
         LA    R6,RCONSPST         POINT TO MINI ELEMENT                        
         LA    R6,L'RCONSPST+L'RCONSPAM(R6) SKIP FIRST                          
         DROP  R6                                                               
*                                                                               
DSPL63   LA    R3,5                5 STATIONS PER LINE                          
*                                                                               
DSPL65   DS    0H                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVC   0(5,R4),0(R6)       LOAD STATION CALL LETTERS                    
         OC    0(5,R4),SPACES                                                   
         CLI   RCONKSTA+4,C' '                                                  
         BE    DSPL68              NO % FOR RADIO                               
         CLI   RCONKSTA+4,C'L'                                                  
         BNE   DSPL72              NO % FOR RADIO                               
DSPL68   DS    0H                                                               
         OC    5(4,R6),5(R6)       THIS STATION HAS 0 DOLLARS                   
         BZ    DSPL70                                                           
         L     R1,5(R6)                                                         
         L     RF,SAVASPL          RESTORE A(SPL ELEMENT)                       
         TM    RCONSPES-RCONSPEL(RF),X'04'                                      
*                                  DOLLARS OR PERCENT?                          
         BNO   DSPL70              OFF  =  DOLLARS                              
*                                  ON   =  PERCENT                              
*                                                                               
*   PERCENTS:  ROUND TO WHOLE NUMBER.  POSSIBLE THAT TOTAL WILL                 
*     BE 99%, BECAUSE OF ROUNDING                                               
*                                                                               
         A     R1,=F'50'           ADD FOR ROUNDING                             
         D     R0,=F'100'          DIVIDE BY 100                                
         SR    R0,R0               RESET R0                                     
*                                                                               
         EDIT  (R1),(7,5(R4)),TRAIL=C'%',ALIGN=LEFT,ZERO=NOBLANK                
         B     DSPL72                                                           
DSPL70   EQU   *                                                                
         M     R0,=F'200'                                                       
         L     R2,MKTOT                                                         
*                                                                               
         LTR   R2,R2               CHECK FOR DIVISION BY ZERO                   
         BNZ   DSPL71              NO MARKET TOTAL                              
         SR    R1,R1                                                            
         B     DSPL71A                                                          
*                                                                               
DSPL71   DS    0H                                                               
         DR    R0,R2                                                            
         AH    R1,=H'1'                                                         
         SRL   R1,1                                                             
*                                                                               
DSPL71A  DS    0H                                                               
         EDIT  (R1),(4,5(R4)),TRAIL=C'%',ALIGN=LEFT,ZERO=NOBLANK                
*                                                                               
DSPL72   DS    0H                                                               
         LA    R6,9(R6)                                                         
         LA    R4,13(R4)                                                        
*                                                                               
         BCTR  R5,0                ANYMORE STATIONS??                           
         LTR   R5,R5                                                            
         BZ    DSPL80              NO, DONE                                     
*                                                                               
         BCT   R3,DSPL65                                                        
         BAS   RE,GOSPUL                                                        
         LA    R4,P+29                                                          
         CLI   FORMAT,C'G'         GRAPHNET NEEDS ITS OWN FORMAT                
         BNE   *+8                 'CAUSE IT'S ONLY 69 CHARACTERS WIDE          
         LA    R4,P+2              THEN PRINT THE STATIONS                      
         B     DSPL63                                                           
*                                                                               
DSPL75   DS    0H                                                               
         LA    R2,STALST           FIND STATION PERCENTAGES                     
         CLI   BYTE2,1             STA WON 100% OF MKT?                         
         BNE   DSPL76                                                           
         MVC   P+29(5),0(R2)                                                    
         OC    P+29(5),SPACES                                                   
         EDIT  (R1),(4,5(R4)),TRAIL=C'%',ALIGN=LEFT,ZERO=NOBLANK                
         B     DSPL80                                                           
*                                                                               
DSPL76   L     R6,AIO              CONTRACT RECORD                              
         USING RCONSPEL,R6                                                      
         MVI   ELCODE,6            GET EPL STATIONS                             
         BRAS  RE,GETEL                                                         
         BNE   DSPL80                                                           
         CLI   RCONSPNU,0          ANY STATIONS??                               
         BE    DSPL80                                                           
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         CLI   TWACOMBO,0          IF COMBO, WE'VE ALREADY PRINTED              
         BNE   DSPL77              THE MOTHER STATION.  SO, SKIP IT             
         ZIC   R5,RCONSPNU         NUMBER OF STATIONS                           
         LA    R6,RCONSPST         POINT TO MINI ELEMENT                        
         B     DSPL77A                                                          
*                                                                               
DSPL77   CLI   RCONSPNU,1          THIS MEANS IT DOESN'T HAVE ANY               
         BE    DSPL80              STATIONS                                     
         ZIC   R5,RCONSPNU         NUMBER OF STATIONS                           
         BCTR  R5,0                SUBTRACT 1 FROM NUMBER OF STATIONS           
         LA    R6,RCONSPST         POINT TO MINI ELEMENT                        
         LA    R6,L'RCONSPST+L'RCONSPAM(R6) SKIP FIRST                          
         DROP  R6,RF                                                            
*                                                                               
DSPL77A  LA    R3,5                5 STATIONS PER LINE                          
         LA    R4,P+17                                                          
*                                                                               
DSPL78   DS    0H                                                               
         MVC   0(5,R4),0(R6)       MOVE IN STATION                              
         OC    0(5,R4),SPACES                                                   
         LA    R6,9(R6)            BUMP TO NEXT STATION                         
         LA    R4,10(R4)                                                        
*                                                                               
         BCTR  R5,0                ANYMORE STATIONS??                           
         LTR   R5,R5                                                            
         BZ    DSPL80              NO, DONE                                     
*                                                                               
         BCT   R3,DSPL78                                                        
*                                  PRINT A LINE OF STATIONS                     
         BAS   RE,GOSPUL                                                        
         B     DSPL77A                                                          
*                                                                               
DSPL80   DS    0H                                                               
         BAS   RE,GOSPUL                                                        
*                                  PRINT A BLANK LINE                           
         BAS   RE,GOSPUL                                                        
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'07'        SPL/EPL COMMENT ELEMENT                      
         BRAS  RE,GETEL                                                         
DSPL90   BNE   DSPL100             ELEMENT FOUND?                               
         ZIC   R5,1(R6)                                                         
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P(0),2(R6)          EX MOVE OF COMMENT TEXT                      
         OC    P,SPACES            ALL CAPS                                     
         BAS   RE,GOSPUL                                                        
         MVI   ELCODE,X'07'        SPL/EPL COMMENT ELEMENT                      
         BRAS  RE,NEXTEL           NEXT ELEMENT                                 
         B     DSPL90                                                           
DSPL100  EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
T80263   CSECT                                                                  
FINDID   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*- SENDPASS MUST BE DEFINED.  (ELSE CALLING ERROR)                              
         CLI   SENDPASS,1          MIN SENDPASS #                               
         BL    FIDPASRR                                                         
         CLI   SENDPASS,4          MAX SENDPASS #                               
         BNH   FID0020                                                          
FIDPASRR DC    H'0'                SENDPASS ERROR - INVALID PASS.               
         SPACE 2                                                                
*                                                                               
*- BRANCH TO RTN TO SELECT ID.                                                  
*  TABLE LAYOUT:  AL4(REP RTN),AL4(STATION RTN)                                 
*  1ST ENTRY FOR SENDPASS = 1, ETC.                                             
FID0020  EQU   *                                                                
         ZIC   RF,SENDPASS                                                      
         BCTR  RF,R0               LESS 1                                       
         SLL   RF,3                TIMES 8 (8 BYTES/ENTRY)                      
         LA    RE,FIDTBL                                                        
         AR    RF,RE               A(REP TABLE ENTRY)                           
         CLC   =C'DONE',CONACT     TREAT DONE AS IF STATION DID A CF            
         BE    *+12                                                             
         CLI   TWAACCS,C'$'        STATIONS START WITH '$'                      
         BNE   FID0040                                                          
         LA    RF,4(RF)            POINT TO STATION ENTRY                       
*                                                                               
FID0040  L     RF,0(RF)            PICK UP RTN ADDRESS FROM TABLE               
         A     RF,RELO             RELOCATE                                     
         BR    RF                  AND GO.                                      
         SPACE 2                                                                
FIDTBL   DS    0F                             A(REP RTN), A(STA RTN)            
         DC    A(FID1000),A(FID1500) PASS 1                                     
         DC    A(FID2000),A(FID2500) PASS 2                                     
         DC    A(FID3000),A(FID3000) PASS 3                                     
         DC    A(FID4000),A(FID4000) PASS 4                                     
         SPACE 2                                                                
FIDGOOD  B     YES                                                              
FIDBAD   B     NO                                                               
         EJECT                                                                  
*********************************************************************           
*- SENDER = REP. PASS = 1.                                                      
*                                                                               
*  IF ACTION = 'CF'                                                             
*     SET SEND ID TO 0 (SEE RECNT17 -- CONTRACT PRINTING PHASE)                 
*     SET 'NOPQCLOS' SWITCH  (AVOID PQCLOSE IN RECNT60)                         
*     EXIT W/NON-0 CC                                                           
*  ELSE                                                                         
*     ID = TERMINAL.   FORMAT = REP FORMAT                                      
*********************************************************************           
FID1000  EQU   *                                                                
         CLC   =C'CF',CONACT                                                    
         BNE   FID1020                                                          
         XC    SENDID,SENDID                                                    
         MVI   NOPQCLOS,X'1'                                                    
         B     FIDBAD                                                           
*                                                                               
FID1020  MVC   SENDID,TWAUSRID     ID = TERMINAL                                
         MVI   FORMAT,X'FF'        REP FORMAT                                   
         B     FIDGOOD                                                          
         EJECT                                                                  
*********************************************************************           
*- SENDER = STA.  PASS = 1                                                      
*                                                                               
*  ID = TERMINAL.  FORMAT = STATION TRAFFIC SYSTEM CODE.                        
*                                                                               
*  IF ACTION = 'CF' AND STA OPT 1 = 'Y'                                         
*     SUPPRESS STATION WORKSHEET (CHANGES ACTION TO 'CONX')                     
*     EXIT WITH ^0 CC.                                                          
*********************************************************************           
FID1500  EQU   *                                                                
         CLC   =C'CF',CONACT       ACTION NOT DEFINED FOR STA                   
         BNE   FID1510             IF NOT ACE ORDER                             
         TM    RCONMODR+1,X'80'                                                 
         BZ    FIDBAD                                                           
*                                                                               
FID1510  L     R6,AIO3             STATION RECORD                               
         USING RSTAREC,R6                                                       
*                                                                               
         MVC   SENDID,TWAUSRID                                                  
         MVC   FORMAT,RSTATRAF                                                  
         DROP  R6                                                               
*                                                                               
         CLC   =C'CF',CONACT     SUPPRESS STATION COPY?                         
         BNE   FIDGOOD                                                          
*                                                                               
         MVI   ELCODE,X'08'        EXTRA DESCRIPTION ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   FIDGOOD             NOT THERE.  PRINT.                           
*                                                                               
         USING RSTAXXEL,R6                                                      
*                                                                               
         CLI   RSTAOPT1,C'Y'       SUPPRESS?                                    
         BNE   FIDGOOD             NO. PRINT IT.                                
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWACFFLG,X'08'      CFC?                                         
         BO    FIDGOOD             YES - PRINT ANYWAY                           
         DROP  RF                                                               
*                                                                               
         MVC   CONACT,=C'CONX'     CHANGE ACTION.  (SEE RECNT60 PHASE)          
         DROP  R6                                                               
         B     FIDBAD                                                           
         EJECT                                                                  
*********************************************************************           
*- SENDER = REP. PASS = 2.                                                      
*                                                                               
*  ID = STATION RECEIVING ID.   FORMAT = STATION TRAFFIC FORMAT                 
*********************************************************************           
FID2000  EQU   *                                                                
         CLC   =C'CF',CONACT       SPECIAL FOR CF ACTION                        
         BE    FID2100                                                          
         CLC   CONACT,=C'CFX '     SPECIAL FOR CFX ACTION                       
         BE    FID2100                                                          
*                                                                               
         L     R6,AIO3             STATION RECORD                               
         USING RSTAREC,R6                                                       
         MVC   FORMAT,RSTATRAF                                                  
         DROP  R6                                                               
*                                                                               
         TM    TWAFLAGS,TWAFLHMQ   HOME MARKET?                                 
         BZ    FID2005             NO - PROCEED AS USUAL                        
         MVC   SENDID,TWAUSRID     ID = TERMINAL                                
         B     FIDGOOD                                                          
*                                                                               
FID2005  DS    0H                                                               
         MVI   ELCODE,X'05'        EXTENDED DESCRIPTION ELEMENT                 
         BRAS  RE,GETEL                                                         
         BE    FID2010                                                          
         LA    R2,CONCACTH                                                      
         LA    R3,275              NO 05 ELEM ON ACE STATION?                   
         B     ERROR                                                            
*                                                                               
FID2010  EQU   *                                                                
         USING RSTAXEL,R6                                                       
         MVC   SENDID,RSTARID      RECEIVING ID                                 
         OC    SENDID,SENDID                                                    
         BNZ   FIDGOOD                                                          
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
FID2100  EQU   *                                                                
         MVC   SENDID,TWAUSRID                                                  
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWACFFLG,X'20'      REP CONFIRMED ORD WRKSHT?                    
         BZ    FIDBAD                                                           
         DROP  RF                                                               
         MVI   FORMAT,X'FF'        REP FORMAT                                   
         B     FIDGOOD                                                          
         EJECT                                                                  
*********************************************************************           
*- SENDER = STA.  PASS = 2                                                      
*                                                                               
*  ID = REP ID FROM LAST CONTRACT SEND. FORMAT = REP FORMAT.                    
*                                                                               
*  IF ACTION = CF OR CONX, EXIT WITH ^0 CC (NO PRINTING)                        
*     EXCEPT IF AGENCY PROFILE 3 = Y, IN WHICH CASE AN ORDER WORKSHEET          
*     GET GENERATED TO THE REP                                                  
*                                                                               
*********************************************************************           
FID2500  EQU   *                                                                
         CLC   =C'CF',CONACT       ACTION NOT DEFINED FOR STA                   
         BNE   FID2505             IF NOT ACE ORDER                             
         TM    RCONMODR+1,X'80'                                                 
         BZ    FIDBAD                                                           
*                                                                               
FID2505  L     R6,AIO              CONTRACT RECORD                              
         MVI   ELCODE,X'20'        SEND ELEMENT                                 
         BRAS  RE,GETEL                                                         
         BE    FID2510                                                          
         DC    H'0'                                                             
FID2510  MVC   SENDID,2(R6)                                                     
         MVI   FORMAT,X'FF'                                                     
*                                                                               
         CLC   =C'CF',CONACT     EXIT ^0 CC FOR CF/CONX                         
         BE    FID2520                                                          
         CLC   =C'CONX',CONACT                                                  
         BE    FID2520                                                          
         CLC   =C'DONE',CONACT                                                  
         BE    FID2520                                                          
         B     FIDGOOD                                                          
*                                                                               
FID2520  DS    0H                                                               
         LR    RF,RA               IF AGENCY PROFILE 3 = Y                      
         AH    RF,=Y(TWAWORKQ)     CONTRACT SUPPRESSES CONFIRMATION(17)         
         USING TWAWORK,RF          PRINTING AND GENERATES A CONFIRMED           
         TM    TWACFFLG,X'20'      REP CONFIRMED ORD WRKSHT?                    
         BO    FIDGOOD             YES - GEN WORKSHEET                          
         B     FIDBAD              NO - DON'T GEN WORKSHEET                     
         DROP  RF                                                               
         EJECT                                                                  
*********************************************************************           
*- SENDER STATION OR REP, PASS 3                                                
*                                                                               
*  IF EXTRA DESTINATION ID FOUND ON STATION, SEND EXTRA WORKSHEET               
*  SUBJECT TO CONTRACT STATUS (CONF/UNCONF/BOTH) FILTER.                        
*********************************************************************           
FID3000  EQU   *                                                                
*                                                                               
FID3010  L     R6,AIO3                                                          
         MVI   ELCODE,X'08'        EXTRA DESC ELEMENT                           
         BRAS  RE,GETEL                                                         
         BNE   FIDBAD              NO ELEMENT, NO PRINT                         
*                                                                               
         USING RSTAXXEL,R6                                                      
         OC    RSTAORID,RSTAORID   ANY DEST ID?                                 
         BZ    FIDBAD              NO ID, NO PRINT.                             
*                                                                               
         MVC   SENDID,RSTAORID                                                  
*                                                                               
         MVI   FORMAT,X'FF'        ASSUME REP FORMAT                            
         CLI   RSTARWS,0                                                        
         BE    FID3020                                                          
         MVC   FORMAT,RSTARWS      USE DEST WKSHEET FORMAT                      
         DROP  R6                                                               
*                                                                               
*- CHECK DEST FILTERS VS CONTRACT STATUS                                        
FID3020  EQU   *                                                                
         CLI   RSTAWSCF,C'B'       BOTH?                                        
         BE    FIDGOOD                                                          
*                                                                               
         LR    R4,R6               SAVE STATION ELEMENT ADDRESS.                
*                                                                               
*- GET EXTENDED DESC ELEMENT FROM CONTRACT.                                     
         LA    R6,CONIO           (REAL CONTRACT RECORD)                        
         MVI   ELCODE,X'1F'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO EXTENDED DESC ELEMENT ON CONTRACT         
*                                                                               
         LR    R1,R6               A(CONTRACT X'1F' ELEM)                       
         LR    R6,R4               PUT BACK R6 TRASHED BY GETEL                 
*                                                                               
         USING RCONXEL,R1                                                       
         USING RSTAXXEL,R6                                                      
*                                                                               
         CLI   RSTAWSCF,C'C'       SEND CONFIRMED ONLY?                         
         BNE   FID3100                                                          
*                                                                               
         TM    RCONCONF,X'40'      CONTRACT CONFIRMED?                          
         BO    FIDGOOD                                                          
         B     FIDBAD                                                           
*                                                                               
FID3100  CLI   RSTAWSCF,C'U'       SEND UNCONFIRMED ONLY?                       
         BNE   FID3150                                                          
*                                                                               
         TM    RCONCONF,X'80'      CONTRACT UNCONFIRMED                         
         BO    FIDGOOD                                                          
         B     FIDBAD                                                           
*                                                                               
FID3150  B     FIDGOOD             WHEN IN DOUBT, PRINT.                        
         DROP  R1,R6                                                            
         EJECT                                                                  
*********************************************************************           
*- SENDER = REP. PASS = 4.                                                      
*                                                                               
*  ID = EASYLINK                FORMAT = G                                      
*********************************************************************           
FID4000  EQU   *                                                                
         CLC   =C'SEND',CONACT     ONLY FOR ACTION SEND                         
         BE    *+14                                                             
         CLC   =C'RSND',CONACT     OR RSND                                      
         BNE   FIDBAD                                                           
*                                                                               
         L     R6,AIO3             STATION RECORD                               
         MVI   ELCODE,X'08'        XTRA DESCRIPTION ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   FIDBAD                                                           
         USING RSTAXXEL,R6                                                      
         OC    RSTAOFAX,RSTAOFAX   HAVE A FAX #?                                
         BZ    FIDBAD              NO - CAN'T FAX                               
         DROP  R6                                                               
*                                                                               
         MVI   FORMAT,C'G'                                                      
         MVC   SENDID,=X'0406'     EASYLINK                                     
         B     FIDGOOD                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*  ONLY PRINT BOP DATA IF VER 1 OR BOP UPDATED SINCE LAST SEND                  
*********************************************************************           
T80263   CSECT                                                                  
DOBOP    NTR1  BASE=*,LABEL=*                                                   
*                                    PRINT A BLANK LINE                         
         BAS   RE,GOSPUL                                                        
         MVC   P(4),=C'SVC-'                                                    
         LA    RF,RTGS                                                          
DBOP5    CLC   SVSRC,0(RF)                                                      
         BE    DBOP10                                                           
         LA    RF,L'RTGS(RF)                                                    
         CLI   0(RF),0                                                          
         BNE   DBOP5                                                            
         B     *+10                                                             
DBOP10   MVC   P+5(3),0(RF)                                                     
         MVC   P+11(6),=C'BOOKS-'                                               
         MVC   P+18(9),SVBOOK                                                   
         BAS   RE,GOSPUL                                                        
         MVC   P(6),=C'DEMOS-'                                                  
         CLI   SVDEMO,X'FF'        VALIDATED BY DEMOVAL?                        
         BNE   DBOP50                                                           
         LA    R3,SVDEMO+1         DEMOS                                        
         LA    R4,P+7                                                           
         LA    R5,2                MAXIMUM 2 DEMOS                              
         L     R6,AWORK4           AREA FOR DBLOCK                              
         USING DBLOCKD,R6                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'R'                                                    
*                                                                               
DBOP20   OC    0(3,R3),0(R3)                                                    
         BZ    DBOP45                                                           
         CLI   1(R3),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R3),C'I'                                                       
         SPACE 1                                                                
*******NOTE - DEMCON IS REALLY DEMOCON - SEE T80260*************                
         SPACE 1                                                                
         GOTO1 DEMCON,DMCB,(0,(R3)),(6,0(R4)),(0,DBLOCKD)                       
         DROP  R6                                                               
         CLI   1(R3),C'I'                                                       
         BNE   *+8                                                              
         MVI   1(R3),C'T'                                                       
         LA    R4,8(R4)                                                         
*        - IF PRIMARY DEMO, PUT P AT END OF DEMO EXPRESSION                     
         TM    0(R3),X'40'         PRIMARY DEMO                                 
         BNO   DBOP40                                                           
         LR    R1,R4                                                            
DBOP30   BCTR  R1,0                                                             
         CLI   0(R1),C' '                                                       
         BE    DBOP30                                                           
         MVI   1(R1),C'P'                                                       
DBOP40   LA    R3,3(R3)                                                         
         BCT   R5,DBOP20                                                        
DBOP45   MVC   P+29(3),SVMKT                                                    
         B     BBOPX                                                            
DBOP50   MVC   P+7(20),SVDEMO                                                   
         MVC   P+29(3),SVMKT                                                    
         B     BBOPX                                                            
*                                                                               
BBOPX    DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
* RATING SERVICE TABLE                                                          
RTGS     DS    0CL3                                                             
         DC    C'ARBNSISRCBIRTRCMTDRAM'                                         
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
* CHECK STATION PROFILES TO SEE IF WE NEED TO DISPLAY DEMO OR NOT               
*                                                                               
CKSTPROF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
*                                                                               
         TM    PROFILES+CNTDEMOB,CNTDEMOA                                       
         BO    CKST0100            DISALLOW STATION TO SEE DEMO                 
*                                                                               
         TM    TWASTAOB,X'02'                                                   
         BO    CKSTPRN             DO NOT PROCESS DEMO                          
         B     CKSTPRY                                                          
CKST0100 DS    0H                                                               
         TM    TWASTAOB,X'02'                                                   
         BZ    CKSTPRN                                                          
         DROP  RF                                                               
*                                                                               
CKSTPRY  SR    RC,RC                                                            
CKSTPRN  LTR   RC,RC                                                            
CKSTPRX  DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*********************************************************************           
*  ONLY PRINT SAR DATA IF VER 1 OR SAR UPDATED SINCE LAST SEND                  
*********************************************************************           
DOSAR    NTR1  BASE=*,LABEL=*                                                   
*                                  PRINT A BLANK LINE                           
         MVC   P,SPACES            CLEAR PRINT LINE                             
         BAS   RE,GOSPUL                                                        
         MVC   P(4),=C'SVC-'                                                    
         CLI   SVSRC,C'A'                                                       
         BNE   DSAR10                                                           
         MVC   P+5(3),=C'ARB'                                                   
         B     DSAR30                                                           
DSAR10   CLI   SVSRC,C'N'                                                       
         BNE   DSAR20                                                           
         MVC   P+5(3),=C'NSI'                                                   
         B     DSAR30                                                           
DSAR20   CLI   SVSRC,C'S'                                                       
         BNE   DSAR30                                                           
         MVC   P+5(3),=C'SRC'                                                   
DSAR30   MVC   P+11(6),=C'BOOKS-'                                               
*                                                                               
         GOTO1 =A(SARBKS),DMCB,(RC),(R7),RR=Y                                   
         BO    DSARNO                                                           
*                                                                               
DSAR70   DS    0H                                                               
         BAS   RE,GOSPUL                                                        
         MVC   P(6),=C'DEMOS-'                                                  
         LA    R3,SVDEMO           DEMOS                                        
         LA    R4,P+7                                                           
         LA    R5,8                MAXIMUM 8 DEMOS                              
         L     R6,AWORK4           AREA FOR DBLOCK                              
         USING DBLOCKD,R6                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
*                                                                               
DSAR100  OC    0(3,R3),0(R3)                                                    
         BZ    DSARYES                                                          
         CLI   1(R3),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R3),C'I'                                                       
         SPACE 1                                                                
*******NOTE - DEMCON IS REALLY DEMOCON - SEE T80260*************                
         SPACE 1                                                                
         GOTO1 DEMCON,DMCB,(0,(R3)),(6,0(R4)),(0,DBLOCKD)                       
         DROP  R6                                                               
         CLI   1(R3),C'I'                                                       
         BNE   *+8                                                              
         MVI   1(R3),C'T'                                                       
         LA    R4,8(R4)                                                         
*        - IF PRIMARY DEMO, PUT P AT END OF DEMO EXPRESSION                     
         TM    0(R3),X'40'         PRIMARY DEMO                                 
         BNO   DSAR120                                                          
         LR    R1,R4                                                            
DSAR110  BCTR  R1,0                                                             
         CLI   0(R1),C' '                                                       
         BE    DSAR110                                                          
         MVI   1(R1),C'P'                                                       
DSAR120  LA    R3,3(R3)                                                         
         BCT   R5,DSAR100                                                       
         B     DSARYES                                                          
*                                                                               
DSARYES  SR    RC,RC                                                            
DSARNO   LTR   RC,RC                                                            
         B     EXXMOD                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*  WUINFO --- GRAPHNET GETS A PRINT LINE WITH SOME FIXED INFO, AND              
*               EITHER THE WU NUMBER AND ANSWER BACK CODE, OR A WU              
*               WU STA ID WESTERN UNION GETS CALL LETTERS, NO WU INFO           
*********************************************************************           
T80263   CSECT                                                                  
WUINFO   NMOD1 0,*WUINFO*                                                       
         L     RC,0(R1)                                                         
         L     R7,4(R1)                                                         
*                                                                               
         L     R4,AIO                                                           
         USING RCONKEY,R4                                                       
*                                                                               
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,0                                                           
         MVC   P+4(5),=C'*HDR*'                                                 
         SPACE 1                                                                
         L     R6,AIO3             STATION RECORD                               
         USING RSTAKEY,R6                                                       
         MVC   P+9(5),RSTAKSTA     DEFAULT STATION CALL LETTERS                 
         DROP  R6                                                               
                                                                                
         MVI   ELCODE,X'08'        XTRA DESCRIPTION ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   WUI30                                                            
         USING RSTAXXEL,R6                                                      
*                                                                               
         XC    WORK2X,WORK2X                                                    
         CLI   SENDPASS,3          COPY TO DEST ID?                             
         BNE   WUI05               NO                                           
         OC    RSTAOFX2,RSTAOFX2   YES, DO WE HAVE A 2ND FAX #?                 
         BZ    WUI30                                                            
         MVC   WORK2X(L'RSTAOFX2),RSTAOFX2  USE IT                              
         B     WUI06                                                            
WUI05    DS    0H                                                               
         OC    RSTAOFAX,RSTAOFAX   IF FAX# VALID, USE IT INSTEAD OF             
         BZ    WUI30               STATION CALL LETTERS                         
         MVC   WORK2X(L'RSTAOFAX),RSTAOFAX   USE FAX#                           
*                                                                               
* MAIL BOX                                                                      
*                                                                               
WUI06    DS    0H                                                               
         CLC   =C'MB=',WORK2X                                                   
         BNE   WUI10                                                            
         MVC   P+9(8),WORK2X+3                                                  
         B     WUI30                                                            
*                                                                               
* INTERNATINOAL FAX NUMBER                                                      
*                                                                               
WUI10    DS    0H                                                               
         CLI   WORK2X,0            INTERNATIONAL?                               
         BNE   WUI20                                                            
         CLI   WORK2X+1,0          HUH? NO LENGTH? SKIP IT!                     
         BE    WUI30                                                            
         MVC   P+9(4),=C'FAX '                                                  
*                                                                               
         ZIC   R3,WORK2X+1         INTERNATIONAL CODE                           
         EDIT  (R3),(3,P+13),FILL=0                                             
*                                                                               
         UNPK  WORK(16),WORK2X+3(8)                                             
         ZIC   RF,WORK2X+2         LENGTH OF SIGNIFICANT DIGITS                 
         LA    RE,16               MAXIMUM LENGTH OF FIELD                      
         SR    RE,RF               GET SIGNIFICANT OFFSET                       
         LA    RF,WORK             A(UNPACKED NUMBER)                           
         AR    RF,RE               ADD OFFSET                                   
         ZIC   RE,WORK2X+2         GET LENGTH OF FAX# FIELD AGAIN               
         BCTR  RE,0                DECREMENT FOR EX                             
         EX    RE,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   P+16(0),0(RF)                                                    
*                                                                               
         EX    RE,*+8              DISPLAY NUMBER IN ETI ALSO                   
         B     *+10                                                             
         MVC   P+41(0),0(RF)                                                    
         ZIC   R3,WORK2X+1                                                      
         EDIT  (R3),(3,P+38),FILL=0                                             
*                                                                               
         MVI   P+35,C'P'           REPL X'89' IN REP W/EASYLINK /PAGE           
         B     WUI30                                                            
*                                                                               
* NORMAL FAX NUMBER                                                             
*                                                                               
WUI20    DS    0H                                                               
         LA    RE,WORK2X                                                        
                                                                                
WUI23    DS    0H                                                               
         CLI   0(RE),0             PADDED WITH NULLS OR                         
         BE    WUI24                                                            
         CLI   0(RE),C' '          IT MIGHT BE PADDED WITH SPACES               
         BE    WUI24                                                            
         CLI   0(RE),C'0'          MUST BE NUMERIC                              
         BL    WUI30                                                            
         CLI   0(RE),C'9'                                                       
         BH    WUI30                                                            
         LA    RF,WORK2X                                                        
         LA    RF,L'WORK2X(RF)                                                  
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BNH   WUI23                                                            
                                                                                
WUI24    DS    0H                  FAX NUMBER MUST BE EXACTLY 10 CHARS          
         LA    RF,WORK2X                                                        
         SR    RE,RF                                                            
         CH    RE,=H'10'                                                        
         BNE   WUI30                                                            
                                                                                
         MVC   P+9(4),=C'FAX '                                                  
         MVC   P+13(10),WORK2X                                                  
         MVI   P+35,C'P'           REPL X'89' IN REP W/EASYLINK /PAGE           
                                                                                
         MVI   P+38,C'('           FORMATTED FAX# FOR $ETI                      
         MVC   P+39(3),WORK2X      AREA CODE                                    
         MVI   P+42,C')'                                                        
         MVC   P+44(3),WORK2X+3    PREFIX                                       
         MVI   P+47,C'-'                                                        
         MVC   P+48(4),WORK2X+6    SUFFIX                                       
         DROP  R6                                                               
                                                                                
WUI30    DS    0H                                                               
         MVC   P+54(2),RCONTEM     TEAM                                         
*                                  SEND SPECIAL PRINT LINE                      
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
*                                                                               
* PRINT A ++DDS CARD WITH 58 BYTES OF DATA FOR EDICT XACTION PROG               
*                                                                               
         LA    R5,P                                                             
         USING EDICTD,R5                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST,=C'RE'      FOR SYSTEM REP                               
         MVC   EDIPROG,=C'ORD'     FOR TYPE ORD                                 
         MVC   EDIIDEN,=C'TRN'     XACTION FILE DATA                            
*                                                                               
         MVC   EDIRCNRP,RCONKREP   REP CODE                                     
         MVC   EDIRCNOF,RCONKOFF   OFF CODE                                     
         MVC   EDIRCNSP,RCONSAL    SALESPERSON CODE                             
         MVC   EDIRCNAG,RCONKAGY   AGENCY CODE                                  
         MVC   EDIRCNAO,RCONKAOF   CITY CODE                                    
         MVC   EDIRCNAD,RCONKADV   ADVERTISER CODE                              
         MVC   EDIRCNCT,RCONTYPE   CONTRACT TYPE                                
* FLIGHT START AND END DATES                                                    
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFKFLT,VREPFACS),DMCB,RCONREC,WORK,0,DUB                        
         GOTO1 DATCON,DMCB,(0,WORK),(5,EDIRCNFS)                                
         GOTO1 DATCON,DMCB,(0,WORK+6),(5,EDIRCNFE)                              
* LATEST VERSION NUMBER                                                         
         L     R6,AIO                                                           
         USING RCONSEND,R6                                                      
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   WUI50                                                            
         CLC   RCONSRV,RCONSSV     SHOW LATEST VERSION                          
         BH    WUI40                                                            
         EDIT  (1,RCONSSV),(3,EDIRCNVN),ALIGN=LEFT                              
         B     WUI50                                                            
WUI40    EDIT  (1,RCONSRV),(3,EDIRCNVN),ALIGN=LEFT                              
*                                                                               
WUI50    MVC   EDIRCNST,RCONKSTA   STATION CALLS                                
* CONTRACT NUMBER                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
         EDIT  (P5,WORK),(8,EDIRCNHN),ALIGN=LEFT                                
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWASPREF,PREFSENT   CHECK IF A PREFERRED STATION                 
         BZ    WUI60               IS SELECTED                                  
         DROP  RF                                                               
*                                                                               
         L     R6,AIO3             IF PREFERRED STATION,                        
         USING RSTAKEY,R6          TELL EDICT WHERE WE SENT THE REPORT          
         MVC   EDIRCNST,RSTAKSTA                                                
         DROP  R5,R6                                                            
*                                                                               
WUI60    DS    0H                  SEND SPECIAL PRINT LINE                      
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         XMOD1                                                                  
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*        FMT  ---  ROUTINE TO HANDLE FORMATTING OF WORKSHEET                    
*********************************************************************           
FMT      NMOD1 0,**FMT***                                                       
         L     RC,0(R1)                                                         
         L     R7,4(R1)                                                         
         LR    R3,RA                                                            
         AH    R3,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,R3                                                       
*                                                                               
         MVI   TWAFMTFL,0               INITIALIZE FLAGS                        
*                                                                               
*  PUT FIELDS ONTO TWA FOR WORKSHEETS                                           
*                                                                               
         MVC   TWABUYER,CONBUY          BUYER NAME                              
         XC    TWAADVNM,TWAADVNM                                                
         MVC   TWAADVNM(20),CONADVN     ADVERTISER NAME                         
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   KEY+22(2),RCONKREP                                               
         MVC   KEY+24(3),RCONSAL                                                
         XC    DUB,DUB                                                          
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFGETREC,VREPFACS),DMCB,KEY,AIO3,0,DUB                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO3                                                          
         USING RSALREC,R6                                                       
         MVC   TWASALNM(20),RSALNAME    SALESMAN NAME                           
         DROP  R6                                                               
*                                                                               
*  SPECIAL CASE - KATZ CONVERTED CONTRACT - AGY NAME & ADDR IN CONREC           
*                                                                               
         TM    RCONMODR+1,X'10'    KATZ CONVERTED CONTRACT?                     
         BNO   FMT080                                                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'70'        AGENCY NAME ELEMENT                          
         BRAS  RE,GETEL                                                         
         BNE   FMT050              NOT FOUND - EXIT                             
         XC    TWAAGNM2,TWAAGNM2   SET A(PRINT FIELD)                           
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   TWAAGNM2(0),2(R6)                                                
FMT050   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'71'        FIRST ADDRESS ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   FMT060              NOT FOUND - EXIT                             
         XC    TWAAGAD1,TWAAGAD1                                                
         XC    TWAAGAD2,TWAAGAD2                                                
         XC    TWAAGAD3,TWAAGAD3                                                
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   TWAAGAD1(0),2(R6)                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'72'        SECOND ADDRESS ELEMENT                       
         BRAS  RE,GETEL                                                         
         BNE   FMT060              NOT FOUND - EXIT                             
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   TWAAGAD2(0),2(R6)                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'73'        MAY BE A THIRD ELEMENT                       
         BRAS  RE,GETEL                                                         
         BNE   FMT060              NOT FOUND - EXIT                             
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   TWAAGAD3(0),2(R6)                                                
FMT060   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'6F'        ADVERTISER NAME ELEMENT                      
         BRAS  RE,GETEL                                                         
         BNE   FMT070              NOT FOUND - EXIT                             
         XC    TWAADVNM,TWAADVNM                                                
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   TWAADVNM(0),2(R6)                                                
FMT070   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'74'        SALESPERSON NAME ELEMENT                     
         BRAS  RE,GETEL            (PUT INTO POINT PERSON)                      
         BNE   FMT080              NOT FOUND - EXIT                             
         XC    WPTPEXP,WPTPEXP                                                  
         XC    WPTPPH#,WPTPPH#                                                  
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   WPTPEXP(0),2(R6)                                                 
FMT075   DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'75'        SALESPERSON PHONE# ELEMENT                   
         BRAS  RE,GETEL            (PUT INTO POINT PERSON PHONE)                
         BNE   FMT078              NOT FOUND - EXIT                             
         ZIC   RF,1(R6)            DERIVE LENGTH                                
         LA    RE,3                                                             
         SR    RF,RE               SUBTRACT CONTROL + 1 FOR EX                  
         EX    RF,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   WPTPPH#(0),2(R6)                                                 
FMT078   DS    0H                       SPECIAL CASE - FOR CONVERTED            
         CLI   RCONTYPE,C'N'            TYPE N CONTRACTS, ALWAYS REPL           
         BNE   FMT080                   SAL NAME & PHONE                        
         MVC   TWASALNM,WPTPEXP         REPL SAL NAME W/PT PERS NAME            
         MVC   TWASALTL,WPTPPH#         REPL SAL PH# W/PT PERS PHONE            
         XC    TWASALFX,TWASALFX                                                
         XC    TWAOFFFX,TWAOFFFX                                                
FMT080   DS    0H                                                               
*                                                                               
*  LOOKUP CONTYPE RECORD TO GET FORMAT INFO                                     
*                                                                               
         LA    R6,RCONREC                                                       
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         USING RCTYREC,R5                                                       
         USING RCONREC,R6                                                       
         MVI   RCTYKTYP,RCTYKTYQ     REC TYPE                                   
         MVC   RCTYKREP,RCONKREP     REP CODE                                   
         MVC   RCTYKCTY,RCONTYPE     CON TYPE                                   
         DROP  R6                                                               
         MVI   ION,3                                                            
         GOTO1 AHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   FMT650                                                           
         GOTO1 AGETREC                                                          
         L     R6,AIO3                                                          
*                                                                               
         MVI   ELCODE,X'10'             FORMAT ELEMENT                          
         BRAS  RE,GETEL                                                         
         BNE   FMT700                                                           
         LR    R4,R6                                                            
         USING RCTYFEL,R4                                                       
         MVC   TWAPRFK,RCTYFPRC         SAVE OFF PROFILE BYTES                  
         MVC   TWAPRFW,RCTYFPRW                                                 
*                                                                               
         TM    RCTYFPRA,X'08'           CARE OF AGENCY OVERRIDE?                
         BNZ   FMT180                   YES                                     
*                                                                               
         BAS   RE,COAGY                 CHECK FOR CARE OF AGENCY                
         BNE   FMT180                   NOT A CARE OF AGENCY                    
*                                                                               
         OI    TWAFMTFL,X'08'           SET C/O AGENCY FLAG OVERRIDE            
         B     FMT260                   SKIP CONTYPE AGENCY OVERRIDES           
*                                                                               
FMT180   DS    0H                                                               
         TM    RCTYFA1S,X'40'           REPLACE AGY ADDRESS 1?                  
         BNO   FMT200                   NO - NEXT FIELD                         
         XC    TWAAGAD1,TWAAGAD1                                                
         MVI   HALF,C'G'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT200                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAAGAD1(0),3(R6)                                                
FMT200   DS    0H                                                               
         TM    RCTYFA2S,X'40'           REPLACE AGY ADDRESS 2?                  
         BNO   FMT230                   NO - NEXT FIELD                         
         XC    TWAAGAD2,TWAAGAD2                                                
         MVI   HALF,C'H'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT230                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAAGAD2(0),3(R6)                                                
FMT230   DS    0H                                                               
         TM    RCTYFA3S,X'40'           REPLACE AGY ADDRESS 3?                  
         BNO   FMT240                   NO - NEXT FIELD                         
         XC    TWAAGAD3,TWAAGAD3                                                
         MVI   HALF,C'I'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT240                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAAGAD3(0),3(R6)                                                
FMT240   DS    0H                                                               
         TM    RCTYFANS,X'40'           REPLACE AGY NAME?                       
         BNO   FMT250                   NO - NEXT FIELD                         
         XC    TWAAGNM2,TWAAGNM2                                                
         MVI   HALF,C'E'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT250                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAAGNM2(0),3(R6)                                                
FMT250   DS    0H                                                               
         TM    RCTYFABS,X'40'           REPLACE BUYER NAME?                     
         BNO   FMT260                   NO - NEXT FIELD                         
         XC    TWABUYER,TWABUYER                                                
         MVI   HALF,C'F'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   FMT260                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWABUYER(0),3(R6)                                                
*                                                                               
FMT260   DS    0H                                                               
         DROP  R4,R5                                                            
*                                                                               
*  HANDLE FORMAT OPTION BITS HERE                                               
*                                                                               
         TM    TWAPRFW,X'80'            OPTION #1                               
         BNO   FMT650                                                           
         MVC   TWASALNM,WPTPEXP         REPL SAL NAME W/PT PERS NAME            
         MVC   TWASALTL,WPTPPH#         REPL SAL PH# W/PT PERS PHONE            
         XC    TWASALFX,TWASALFX                                                
         XC    TWAOFFFX,TWAOFFFX                                                
FMT650   DS    0H                       OPTION #2 X'40' IS HANDLED              
         B     FMT702                   ELSEWHERE IN THIS MODULE                
*                                                                               
FMT700   DS    0H                                                               
         BAS   RE,COAGY                 CHECK FOR CARE OF AGENCY                
         BNE   FMT702                   NOT A CARE OF AGENCY                    
*                                                                               
         OI    TWAFMTFL,X'08'           SET C/O AGENCY FLAG OVERRIDE            
*                                                                               
FMT702   DS    0H                                                               
         BAS   RE,REPLADDR              REPLACE AGY ADDRESS?                    
                                                                                
FMTYES   CR    RB,RB                                                            
         B     FMTX                                                             
FMTNO    DS    0H                                                               
         LTR   RB,RB                                                            
FMTX     DS    0H                                                               
         XIT1                                                                   
*----------------------------                                                   
TXTSEEK  DS    0H                                                               
         LR    R0,RE                                                            
         MVI   ELCODE,X'12'                                                     
         L     R6,AIO3                                                          
         BRAS  RE,GETEL                                                         
TS010    BNE   TSNO                                                             
         CLC   2(1,R6),HALF                                                     
         BE    TSYES                                                            
         BRAS  RE,NEXTEL                                                        
         B     TS010                                                            
*                                                                               
TSNO     SR    R1,R1                                                            
         CR    R1,RB                                                            
         B     *+6                                                              
TSYES    CR    R1,R1                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*----------------------------                                                   
* CHECK ADV REC FOR REPLACEMENT AGY ADDRESS                                     
*----------------------------                                                   
REPLADDR NTR1                                                                   
         XC    KEY,KEY                                                          
K        USING RADVKEY,KEY                                                      
         MVI   K.RADVKTYP,X'08'                                                 
         MVC   K.RADVKADV,RCONKADV                                              
         MVC   K.RADVKREP,RCONKREP                                              
         DROP  K                                                                
*                                                                               
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTOX (RFGETREC,VREPFACS),DMCB,KEY,AIO3,0,DUB                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FULL(1),RCONTYPE                                                 
         MVC   FULL+1(2),RCONKOFF                                               
         GOTO1 HELLO,DMCB,(C'G',=C'REPFIL'),(X'20',AIO3),(3,FULL),0             
         CLI   12(R1),0            GOT IT?                                      
         BNE   FMTYES              NO ADDRESS                                   
*                                                                               
         ZICM  R6,13(R1),3         ELEMENT                                      
R        USING RADVAGEL,R6                                                      
         XC    TWAAGAD1,TWAAGAD1                                                
         XC    TWAAGAD2,TWAAGAD2                                                
         XC    TWAAGAD3,TWAAGAD3                                                
         MVC   TWAAGAD1(34),R.RADVAGA1                                          
         MVC   TWAAGAD2(34),R.RADVAGA2                                          
         MVC   TWAAGAD3(36),R.RADVAGA3                                          
         B     FMTYES                                                           
         DROP  R                                                                
*                                                                               
         DROP  R3                                                               
*                                                                               
*----------------------------                                                   
* ROUTINE READS AGENCY RECORD AND RETURNS CC EQUAL IF THE IN CARE OF            
*  FLAG IS ON                                                                   
*                                                                               
*  ROUTINE GETS AN AIO AREA IN WORKING STORAGE AND POINTS R6 AT IT              
*----------------------------                                                   
COAGY    NTR1  WORK=(R6,2000/8)                                                 
         XC    KEY,KEY                                                          
K        USING RAGYKEY,KEY                                                      
         MVI   K.RAGYKTYP,X'0A'                                                 
         MVC   K.RAGYKAGY,RCONKAGY                                              
         MVC   K.RAGYKAOF,RCONKAOF                                              
         MVC   K.RAGYKREP,RCONKREP                                              
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RAGYKEY),KEYSAVE                                           
         BNE   FMTNO                                                            
         GOTO1 VGETREC,DMCB,(R6)                                                
*                                                                               
R        USING RAGYREC,R6                                                       
         TM    R.RAGYFLAG,X'20'    CARE OF AGENCY?                              
         BZ    FMTNO               NO                                           
         B     FMTYES              YES                                          
         DROP  R                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*********************************************************************           
*        SARBKS  ---  ROUTINE TO HANDLE PRINTING OF SAR BOOKS                   
*********************************************************************           
SARBKS   NMOD1 0,*SARBKS*                                                       
         L     RC,0(R1)                                                         
         L     R7,4(R1)                                                         
*                                                                               
         LA    R6,RCONREC               CONTRACT RECORD                         
         MVI   ELCODE,X'12'                                                     
         BRAS  RE,GETEL2                                                        
         BE    SB03                                                             
         LA    R6,SPACES                IF NO ELEM, TEST AGAINST SPACES         
         B     SB08                                                             
*                                                                               
SB03     DS    0H                                                               
         USING RSARXEL,R6                                                       
         CLI   RSARXLEN,RSARXLTH        NEW ELEMENT?                            
         BNE   SB05                     NO!!!!!                                 
         TM    RSARXFLG,X'04'           PROPOSAL EXPANSION USED?                
         BO    SB40                                                             
SB05     LA    R6,RCONREC                                                       
         MVI   ELCODE,X'0B'             BOOK RENAME ELEMENT                     
         BRAS  RE,GETEL2                                                        
         BE    *+8                                                              
         LA    R6,SPACES                IF NO ELEM, TEST AGAINST SPACES         
         LA    R6,2(R6)                                                         
*                                                                               
SB08     DS    0H                                                               
         LA    R3,SVBOOK                                                        
         LA    R4,P+18                                                          
         LA    R5,6                                                             
SB10     OC    0(3,R3),0(R3)                                                    
         BZ    SBX                                                              
         CLC   0(2,R3),=C'DR'      DIRECT RESPONSE HAS NO                       
         BNE   *+14                BOOKS OR DEMOS                               
         MVC   0(2,R4),=C'DR'                                                   
         B     SBNO                                                             
         SPACE 1                                                                
         CLC   0(5,R6),SPACES                                                   
         BE    SB20                                                             
         MVC   0(5,R4),0(R6)                                                    
         LA    R4,6(R4)                                                         
         B     SB30                                                             
SB20     ZIC   R1,2(R3)            MONTH                                        
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS-3(R1)                                                  
         MVC   0(3,R4),0(R1)                                                    
         CLI   2(R3),0                                                          
         BNE   *+10                                                             
         MVC   0(3,R4),=C'EST'                                                  
         MVI   3(R4),C'/'                                                       
         EDIT  (1,1(R3)),(2,4(R4)) YEAR                                         
         SPACE 1                                                                
         LA    R4,7(R4)                                                         
SB30     LA    R3,3(R3)                                                         
         LA    R6,5(R6)                                                         
         BCT   R5,SB10                                                          
         B     SBX                                                              
*                                                                               
SB40     LA    R6,RCONREC                                                       
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RCPRBKEL,R6                                                      
         SR    R0,R0                                                            
         ZIC   R1,RCPRBKLN                                                      
         SH    R1,=AL2(RCPRBKOQ)                                                
         SR    RE,RE                                                            
         LA    RE,L'RCPRBKBK                                                    
         DR    R0,RE                                                            
         LTR   R1,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LR    R5,R1                                                            
         DROP  R6                                                               
         LA    R3,2(R6)                                                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'0B'             BOOK RENAME ELEMENT                     
         BRAS  RE,GETEL2                                                        
         BE    *+8                                                              
         LA    R6,SPACES                IF NO ELEM, TEST AGAINST SPACES         
         LA    R6,2(R6)                                                         
         LA    R4,P+18                                                          
SB50     OC    0(5,R3),0(R3)                                                    
         BZ    SBX                                                              
*                                                                               
         CLC   2(2,R3),=C'DR'      DIRECT RESPONSE HAS NO                       
         BNE   *+14                BOOKS OR DEMOS                               
         MVC   0(2,R4),=C'DR'                                                   
         B     SBX                                                              
*                                                                               
         OC    0(2,R3),0(R3)                                                    
         BZ    SB60                                                             
         MVC   0(5,R4),0(R3)                                                    
         LA    R4,6(R4)                                                         
         B     SB80                                                             
SB60     CLC   0(5,R6),SPACES                                                   
         BE    SB70                                                             
         MVC   0(5,R4),0(R6)                                                    
         LA    R4,6(R4)                                                         
         B     SB80                                                             
SB70     ZIC   R1,4(R3)            MONTH                                        
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS-3(R1)                                                  
         MVC   0(3,R4),0(R1)                                                    
         CLI   4(R3),0                                                          
         BNE   *+10                                                             
         MVC   0(3,R4),=C'EST'                                                  
         MVI   3(R4),C'/'                                                       
         EDIT  (1,3(R3)),(2,4(R4)) YEAR                                         
         LA    R4,7(R4)                                                         
SB80     LA    R3,5(R3)                                                         
         LA    R6,5(R6)                                                         
         BCT   R5,SB50                                                          
         B     SBX                                                              
*                                                                               
SBNO     LA    R1,1                                                             
         B     *+8                                                              
SBX      SR    R1,R1                                                            
         LTR   R1,R1                                                            
         XMOD1                                                                  
         LTORG                                                                  
         GETEL2 R6,34,ELCODE                                                    
         EJECT                                                                  
*                                                                               
***********************************************************************         
* PURPOSE:                                                                      
*     PRINT STORED COMMENTS IF ANY, ELSE IT WILL PRINT FREE                     
*     FORM COMMENTS                                                             
*                                                                               
* INPUT: PARAMETER 1: BYTE 1    = MODE                                          
*                     BYTE 2-4  = A(COMMENT CODE)                               
*                                                                               
* OUTPUT: NONE                                                                  
***********************************************************************         
PSTCMT   NMOD1 0,*PSTCMT*                                                       
         L     RC,4(R1)                                                         
         L     R3,0(R1)                                                         
         MVC   RMSCMODE(1),0(R1)                                                
*                                                                               
         LA    R2,IOAREA                                                        
         GOTO1 VREGENSC,DMCB,(RMSCMODE,0(R3)),(R2),DATAMGR,RCONREC,    X        
               GETTXT                                                           
         BNZ   PSTCMTX             COMMENT NOT FOUND, PRINT NOTHING             
         CLI   0(R2),X'FF'         IF X'FF', PRINT NOTHING                      
         BE    PSTCMTX                                                          
         CLI   0(R2),0             IF NULL, PRINT FREE FORM COMMENT             
         BE    PSTCMT20                                                         
*                                                                               
         CLI   RMSCMODE,4          PRINT IF LIABILITY POSITION                  
         BNE   PSTCMT10                                                         
         MVC   P+6(15),=C'*AGENCY POLICY*'                                      
*                                                                               
PSTCMT10 ZIC   R4,0(R2)            GET LENGTH OF COMMENT                        
         BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+30(0),1(R2)                                                    
         GOTO1 =A(PRINT),RR=Y                                                   
*                                                                               
         ZIC   R4,0(R2)            BUMP TO NEXT COMMENT ENTRY                   
         AR    R2,R4                                                            
         CLI   0(R2),X'FF'         IF X'FF', DONE                               
         BE    PSTCMTX                                                          
*                                                                               
         LR    R4,RC               BOUNDARY CHECK FOR R2                        
         A     R4,=AL4(IOAREA-GENOLD+1001)                                      
         CR    R4,R2                                                            
         BH    PSTCMT10                                                         
         B     PSTCMTX                                                          
*                                                                               
PSTCMT20 DS    0H                  PRINT FREE FORM COMMENTS                     
         CLI   RMSCMODE,3          K CMTS OR ORD CMTS??                         
         BNE   PSTCMTX             ANYTHING ELSE, DON'T PRINT                   
*                                                                               
         MVC   P+30(60),0(R3)                                                   
         OC    P+30(60),SPACES                                                  
*                                                                               
         GOTO1 =A(PRINT),RR=Y                                                   
*                                                                               
PSTCMTX  XMOD1                                                                  
         EJECT                                                                  
*********************************************************************           
*        GOSPOOL ---  ROUTINE TO HANDLE SPOOL INTERFACE                         
*********************************************************************           
GOSPOOL  NMOD1 0,*GSPOOL*                                                       
         L     RC,0(R1)                                                         
         L     R7,4(R1)                                                         
*                                                                               
         CLI   CMNTPRTD,C'N'                                                    
         BE    GOSPL5                                                           
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         CLI   MYHEDSW,C'Y'        DID WE JUST PRINT HEADLINES                  
         BNE   GOSPLX                                                           
         B     GOSPL30                                                          
*                                                                               
GOSPL5   DS    0H                                                               
         BAS   RE,COMMENT          PRINT STORED COMMENTS                        
*                                                                               
         BAS   RE,AGYRISK          PRINT AGENCY CREDIT RISK                     
*                                                                               
         BAS   RE,AGYLIAB          PRINT AGENCY LIABILITY POSITION              
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'10'      DO WE NEED TO CLOSE CMT SECTION              
         BZ    GOSPL25             NO - SKIP THIS                               
         NI    TWAWSFLG,X'FF'-X'10'   RESET                                     
         DROP  RF                                                               
*                                                                               
         MVI   P,C'*'                                                           
         CLI   FORMAT,C'A'         FOR FAX, LIMIT COMMENTS MARKER LINE          
         BE    GOSPL10               TO 65 CHARS.                               
         CLI   FORMAT,C'G'                                                      
         BNE   GOSPL20                                                          
*                                                                               
GOSPL10  MVC   P+1(65),P                                                        
         B     *+10                                                             
*                                                                               
GOSPL20  MVC   P+1(107),P                                                       
         MVC   P+3(17),=C' END OF COMMENTS '                                    
GOSPL25  GOTO1 =A(PRINT),RR=Y                                                   
         B     GOSPLX                                                           
*                                                                               
GOSPL30  CLI   XTRHED,0            ANY EXTRA HEADLINES??                        
         BE    GOSPL50                                                          
         LA    R4,H15                                                           
         ZIC   R3,XTRHED                                                        
GOSPL40  MVC   P,0(R4)             PRINT THE EXTRA HEADLINES                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,132(R4)                                                       
         BCT   R3,GOSPL40                                                       
*                                                                               
GOSPL50  DS    0H                                                               
         MVC   P,SVPRNT            NOW PRINT THE LINE OF DATA                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
GOSPLX   MVI   MYHEDSW,C'N'                                                     
         XMOD1                                                                  
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO PRINT CONTRACT COMMENTS                                            
*********************************************************************           
COMMENT  NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAPRFW,X'40'       CONTYPE RECORD OPTION #2                     
         BZ    CMT63               PROFILE TO CON TYPE/DEV TYPE                 
         DROP  RF                                                               
         BAS   RE,CMTSECT                                                       
         BAS   RE,TYPEDESC         DESCRIPTION                                  
*                                                                               
CMT63    DS    0H                                                               
         CLI   TWAACCS,C'$'        DISP SONNET INFO FOR STATIONS ONLY           
         BNE   CMT63C                                                           
         LA    R1,SONTAB                                                        
CMT63A   CLI   0(R1),0             END OF TABLE?                                
         BE    CMT63C                                                           
         CLC   0(1,R1),FORMAT      MATCH FORMAT TO TABLE ENTRY?                 
         BE    CMT63B              YES - GO DISPLAY SONNET INFO                 
         LA    R1,1(R1)            NO - BUMP TO NEXT TABLE ENTRY                
         B     CMT63A              LOOP BACK                                    
CMT63B   DS    0H                                                               
         BAS   RE,CMTSECT                                                       
         BAS   RE,DOSONNET                                                      
CMT63C   CLI   RCONTYPE,0                                                       
         BE    CMT64                                                            
         BAS   RE,TYPECMT          TYPE STANDDARD COMMENT SUPPORT               
         B     CMT64                                                            
*                                                                               
SONTAB   DC    C'BCVJKMPWR',X'00' LIST OF FORMATS FOR SONNET INFO               
*                                                                               
CMT64    DS    0H                                                               
         TM    STATFLAG,HASCANLN   IF ORDER CONTAINS CANCELLED BUYLINES         
         BZ    CMT65               SAY SO                                       
         BAS   RE,CMTSECT                                                       
         MVC   P+6(48),=C'*** THIS ORDER CONTAINS CANCELLED BUYLINE(S) X        
               ***'                                                             
         GOTO1 =A(PRINT),RR=Y                                                   
*                                                                               
CMT65    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        IS DARE AGY ORDER ELEM THERE?                
         BRAS  RE,GETEL                                                         
         BNE   CMT67               NO, CONTINUE                                 
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'+X'01' YES, IS CONTR LINKED TO AGY ORDER?          
         BZ    CMT67               OR TAKEOVER??                                
*                                                                               
         BAS   RE,CMTSECT                                                       
         MVI   P+6,C'*'                                                         
         LA    R2,P+7                                                           
         TM    RCONDRFG,X'04'                                                   
         BO    CMT65A50                                                         
*                                                                               
         MVC   0(4,R2),=C'XML '                                                 
         LA    R2,4(R2)                                                         
         TM    RCONDRF2,X'01'                                                   
         BO    CMT65A50                                                         
         SHI   R2,4                BACK THE PRINT CURSOR UP                     
*                                                                               
         MVC   0(5,R2),=C'DARE '                                                
         LA    R2,5(R2)                                                         
*                                                                               
CMT65A50 DS    0H                                                               
         MVC   0(14,R2),=C'AGENCY ORDER #'                                      
         LA    R2,14(R2)                                                        
         GOTO1 HEXOUT,DMCB,RCONDRLK,0(R2),4                                     
         MVI   8(R2),C'*'                                                       
*                                                                               
*&&DO                                                                           
* FOR DARE AGENCY NO K - ASTE                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
*                                                                               
         CLC   =C'PCF',BUYACT      SKIP IF BUYACT IS PCF                        
         BE    CMT65AA                                                          
         TM    TWACFFLG,X'20'      CONFIRMED ORDER WORKSHEET?                   
         BZ    CMT65AA                                                          
         CLI   TWADCFPT,C'Y'       DARE AGENCY NO CONTRACT FLAG ON??            
         BNE   CMT65AA                                                          
         CLI   FORMAT,X'FF'        YES, REP??                                   
         BNE   CMT65AA                                                          
         MVC   16(47,R2),=C'THIS AGENCY DOES NOT REQUIRE A PRINTED CONT+        
               RACT'                                                            
         DROP  RF                                                               
*&&                                                                             
*                                                                               
CMT65AA  GOTO1 =A(PRINT),RR=Y                                                   
*                                                                               
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    CMT66                                                            
         TM    RCONDRF2,X'80'                                                   
         BZ    CMT65A                                                           
         MVC   P+6(71),=C'*THIS VARIOUS ORDER WILL BE REPLACED BY BRANDX        
                ORDERS UPON CONFIRMATION*'                                      
         GOTO1 =A(PRINT),RR=Y                                                   
         B     CMT66                                                            
*                                                                               
CMT65A   DS    0H                                                               
         TM    RCONDRF2,X'40'                                                   
         BZ    CMT66                                                            
         MVC   P+6(103),=C'*THIS ORDER IS ONE OF THE BRANDS BOUGHT ON TX        
               HE VARIOUS DARE AGENCY ORDER#         , CONTRACT#       X        
                 *'                                                             
         GOTO1 HEXOUT,DMCB,RCONDRVN,P+80,4                                      
         GOTO1 HEXOUT,DMCB,RCONDRCN,P+100,4                                     
         GOTO1 =A(PRINT),RR=Y                                                   
         DROP  R6                                                               
*                                                                               
CMT66    DS    0H                                                               
         BAS   RE,HIATUS           PRINT DARE HIATUS INFO                       
         B     CMT70                                                            
*                                                                               
CMT67    DS    0H                                                               
         BAS   RE,NODAHIAT         PRINT NON-DARE HIATUS INFO                   
*                                                                               
CMT70    DS    0H                                                               
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1C'        IS DARE AGY ORDER ELEM THERE?                
         BRAS  RE,GETEL                                                         
         BNE   CMT80               NO, CONTINUE                                 
         USING RCONTKEL,R6                                                      
         BAS   RE,CMTSECT                                                       
         MVC   P+6(31),=C'*FORMER REP CONTRACT #        *'                      
         GOTO1 HEXOUT,DMCB,RCONTKCN,P+28,4                                      
         DROP  R6                                                               
         GOTO1 =A(PRINT),RR=Y                                                   
*                                                                               
CMT80    DS    0H                                                               
         OC    SVCONCOM(60),SVCONCOM         CONTRACT COMMENT LINE 1            
         BZ    CMT90                                                            
         BAS   RE,CMTSECT                                                       
         GOTO1 =A(PRINT),RR=Y      SKIP A LINE                                  
         MVC   P+6(18),=C'*CONTRACT COMMENT*'                                   
*                                                                               
         GOTO1 =A(PSTCMT),DMCB,(3,SVCONCOM),(RC),RR=Y  PRINT K CMT 1            
*                                                                               
CMT90    OC    SVCONCOM+60(60),SVCONCOM+60    CONTRACT COMMENT LINE 2           
         BZ    CMT120                                                           
*                                                                               
         OC    SVCONCOM(60),SVCONCOM                                            
         BNZ   CMT100                                                           
         GOTO1 =A(PRINT),RR=Y                                                   
         MVC   P+6(18),=C'*CONTRACT COMMENT*'                                   
*                                                                               
CMT100   DS    0H                                                               
         BAS   RE,CMTSECT                                                       
         GOTO1 =A(PSTCMT),DMCB,(3,SVCONCOM+60),(RC),RR=Y  PRINT K CMT 2         
*                                                                               
CMT120   DS    0H                                                               
*                                                                               
         ICM   R2,15,ASVACMT       ADDRESS AGY COMMENT                          
         OC    0(60,R2),0(R2)                                                   
         BZ    CMT130                                                           
         BAS   RE,CMTSECT                                                       
         GOTO1 =A(PRINT),RR=Y      SKIP A LINE                                  
         MVC   P+6(16),=C'*AGENCY COMMENT*'                                     
         GOTO1 =A(PSTCMT),DMCB,(3,0(R2)),(RC),RR=Y  PRINT AGY CMT 1             
         OC    60(60,R2),60(R2)                                                 
         BZ    CMT130                                                           
         GOTO1 =A(PSTCMT),DMCB,(3,60(R2)),(RC),RR=Y  PRINT AGY CMT 2            
*                                                                               
* FORCED OCM (NON-DISCRIMINATION CLAUSE)                                        
*                                                                               
CMT130   DS    0H                                                               
         GOTO1 =A(PRINT),RR=Y      SKIP A LINE                                  
         GOTO1 =A(PSTCMT),DMCB,(3,=C'SC=*'),(RC),RR=Y                           
*                                                                               
CMTX     B     XIT                                                              
*                                                                               
CMTSECT  DS    0H        ROUTINE TO PRINT THE BORDER OF THE CMT SECTION         
         LR    R0,RE                                                            
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'08'  DID WE ALREADY PRINT THE SECTION HEADER          
         BO    CMTSX             YES - NO NEED TO DO IT AGAIN                   
         OI    TWAWSFLG,X'10'    NEED TO CLOSE SECTION                          
         OI    TWAWSFLG,X'08'    WE'RE PRINTING TOP                             
         DROP  RF                                                               
         MVI   P,C'*'                                                           
*                                                                               
         CLI   FORMAT,C'A'         FOR FAX, LIMIT COMMENT MARKER LINE           
         BE    CMTS10                TO 66 CHARS                                
         CLI   FORMAT,C'G'                                                      
         BNE   CMTS20                                                           
*                                                                               
CMTS10   MVC   P+1(65),P                                                        
         B     *+10                                                             
CMTS20   MVC   P+1(107),P                                                       
         MVC   P+3(19),=C' START OF COMMENTS '                                  
         GOTO1 =A(PRINT),RR=Y      HIGHLIGHT COMMENTS                           
CMTSX    LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO PRINT AGENCY CREDIT RISK STATEMENT                                 
*********************************************************************           
AGYRISK  NTR1                                                                   
         LR    R4,RA                                                            
         AH    R4,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R4                                                       
         CLI   TWAARISK,0          PRINT IF CREDIT RISK OTHER THAN OK           
         BE    ARISKX              OR MISSING                                   
         CLI   TWAARISK,1                                                       
         BE    ARISKX                                                           
         CLI   TWAARISK,6          SHOULDN'T BE GREATER THAN 6,                 
         BH    ARISKX              IF IT IS, JUST EXIT                          
         BAS   RE,CMTSECT          PRINT CMT SECTION IF WE HAVEN'T              
         LA    RE,RISKTAB          GET APPROPRIATE MESSAGE                      
         ZIC   RF,TWAARISK                                                      
         BCTR  RF,0                                                             
         MH    RF,=H'40'                                                        
         AR    RE,RF                                                            
         MVC   P+30(40),0(RE)                                                   
         MVC   P+6(17),=C'*CREDIT ADVISORY*'                                    
         GOTO1 =A(PRINT),RR=Y                                                   
         GOTO1 =A(PRINT),RR=Y                                                   
*                                                                               
ARISKX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*********************************************************************           
* PRINT AGENCY LIABILITY POSITION COMMENTS                                      
*********************************************************************           
AGYLIAB  NTR1                                                                   
         LR    R4,RA                                                            
         AH    R4,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R4                                                       
         CLI   TWAALIAB,0          PRINT IF CREDIT RISK OTHER THAN OK           
         BE    ALIABX              OR MISSING                                   
*                                                                               
         BAS   RE,CMTSECT          PRINT CMT SECTION IF WE HAVEN'T              
         GOTO1 =A(PSTCMT),DMCB,(4,TWAALIAB),(RC),RR=Y  AGY LIAB CMT             
*                                                                               
ALIABX   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*********************************************************************           
* PRINT CONTRACT TYPE AND DEVELOPMENT TYPE DESCRIPTION IF ANY                   
* AS DICTATED BY CONTRACT PROFILE 22                                            
*********************************************************************           
TYPEDESC NTR1                                                                   
         CLI   RCONTYPE,0                                                       
         BE    TYPED30                                                          
TKEYD    USING RCTYKEY,KEY                                                      
         XC    KEY,KEY             GET CONTRACT TYPE                            
         MVI   TKEYD.RCTYKTYP,X'32'                                             
         MVC   TKEYD.RCTYKCTY,RCONTYPE                                          
         MVC   TKEYD.RCTYKREP,REPALPHA                                          
         DROP  TKEYD                                                            
*                                                                               
         MVI   ION,3                                                            
         GOTO1 AHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   TYPED30                                                          
*                                                                               
TYPED10  GOTO1 AGETREC                                                          
*                                                                               
         L     R6,AIO3             PRINT DESCRIPTION                            
         USING RCTYREC,R6                                                       
         MVC   P+6(L'RCTYDESC),RCTYDESC                                         
         DROP  R6                                                               
*                                                                               
         GOTO1 =A(PRINT),RR=Y                                                   
*                                                                               
TYPED30  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'18'        DEVELOPMENTAL ELEMENT PRESENT?               
         BRAS  RE,GETEL                                                         
         BNE   TYPEDX                                                           
*                                                                               
         USING RCONDVEL,R6                                                      
         CLI   RCONDVCT,0                                                       
         BE    TYPEDX                                                           
*                                                                               
DKEYD    USING RDCTKEY,KEY                                                      
         XC    KEY,KEY             GET DEVELOPMENTAL CONTRACT TYPE              
         MVI   DKEYD.RDCTKTYP,X'3B'                                             
         MVC   DKEYD.RDCTKCTY,RCONDVCT                                          
         MVC   DKEYD.RDCTKREP,REPALPHA                                          
         DROP  DKEYD                                                            
*                                                                               
         MVI   ION,3                                                            
         GOTO1 AHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   TYPEDX                                                           
*                                                                               
         GOTO1 AGETREC                                                          
*                                                                               
         L     R6,AIO3             PRINT DESCRIPTION                            
         USING RDCTREC,R6                                                       
         MVC   P+6(L'RDCTDESC),RDCTDESC                                         
         DROP  R6                                                               
*                                                                               
         GOTO1 =A(PRINT),RR=Y                                                   
*                                                                               
TYPEDX   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
* READ & DISPLAY SONNET INFORMATION                                             
*********************************************************************           
DOSONNET NTR1                                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A3'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DOSONX                                                           
*                                                                               
         SR    R2,R2                                                            
         ZIC   R3,1(R6)                                                         
         SHI   R3,RCONSOVQ                                                      
         D     R2,=A(L'RCONSONM)                                                
         LTR   R3,R3                                                            
         BZ    DOSONX                                                           
*                                                                               
         LA    R6,RCONSONM-RCONSON(R6)                                          
         OC    3(2,R6),3(R6)                                                    
         BE    DOSONX                                                           
*                                                                               
         MVC   P+6(16),=C'*SONNET HISTORY*'                                     
DOSON10  OC    3(2,R6),3(R6)                                                    
         BE    DOSON30                                                          
*                                                                               
         MVC   P+25(3),0(R6)                                                    
         GOTO1 DATCON,DMCB,(2,3(R6)),(11,P+30)                                  
*                                                                               
         CLI   5(R6),C'Y'                                                       
         BNE   *+14                                                             
         MVC   P+40(8),=C'APPROVED'                                             
         B     *+10                                                             
         MVC   P+40(12),=C'NOT APPROVED'                                        
*                                                                               
         GOTO1 =A(PRINT),RR=Y                                                   
*                                                                               
         LA    R6,L'RCONSONM(R6)                                                
         BCT   R3,DOSON10                                                       
*                                                                               
DOSON30  DS    0H                  PRINT COMMENTS                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A5'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DOSONX                                                           
*                                                                               
DOSON32  DS    0H                                                               
         ZIC   RE,1(R6)            ELEMENT LENGTH                               
         AHI   RE,-(RCONSCOV+1)    SUBTRACT OVERHEAD  +1 FOR EX                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   P+30(0),RCONSCCM-RCONSCEL(R6)                                    
*                                                                               
         GOTO1 =A(PRINT),RR=Y                                                   
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    DOSON32                                                          
*                                                                               
DOSONX   B     XIT                                                              
*********************************************************************           
* FOR TYPE N CONTRACTS ONLY, IF A STANDARD COMMENT CODE IS FOUND                
* IN THE TYPE RECORD, PRINT IT IN THE COMMENT SECTION                           
*********************************************************************           
TYPECMT NTR1                                                                    
         XC    KEY,KEY             GET CONTRACT TYPE                            
TKEYD    USING RCTYKEY,KEY                                                      
         MVI   TKEYD.RCTYKTYP,X'32'                                             
         MVC   TKEYD.RCTYKCTY,RCONTYPE                                          
         MVC   TKEYD.RCTYKREP,REPALPHA                                          
         DROP  TKEYD                                                            
*                                                                               
         MVI   ION,3                                                            
         GOTO1 AHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   TYPENX                                                           
*                                                                               
         GOTO1 AGETREC                                                          
*                                                                               
         L     R6,AIO3                                                          
         USING RCTYREC,R6                                                       
         CLI   RCTY1LEN,RCTYELMX                                                
         BL    TYPENX              OLD RECORD DO NOT HAS S/C CODE               
*                                                                               
         L     R2,AIO4                                                          
         XC    WORK,WORK                                                        
         MVC   WORK(2),=C'C='                                                   
         MVC   WORK+2(L'RCTYCMMT),RCTYCMMT                                      
         DROP  R6                                                               
*                                                                               
         GOTO1 VREGENSC,DMCB,(X'03',WORK),(R2),DATAMGR,RCONREC,GETTXT           
         BNZ   TYPENX              COMMENT NOT FOUND, PRINT NOTHING             
         CLI   0(R2),X'FF'         IF X'FF', PRINT NOTHING                      
         BE    TYPENX                                                           
         CLI   0(R2),0             IF NULL, PRINT FREE FORM COMMENT             
         BE    TYPENX                                                           
*                                                                               
TYPEN10  DS    0H                                                               
         BAS   RE,CMTSECT                                                       
         ZIC   R4,0(R2)            GET LENGTH OF COMMENT                        
         BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+30(0),1(R2)                                                    
         GOTO1 =A(PRINT),RR=Y                                                   
*                                                                               
         ZIC   R4,0(R2)            BUMP TO NEXT COMMENT ENTRY                   
         AR    R2,R4                                                            
         CLI   0(R2),X'FF'         IF X'FF', DONE                               
         BE    TYPENX                                                           
*                                                                               
         L     R4,AIO4             BOUNDARY CHECK FOR R2                        
         A     R4,=AL4(L'IO4+1)                                                 
         CR    R4,R2                                                            
         BH    TYPEN10                                                          
*                                                                               
TYPENX   DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
* PRINT DARE AGENCY HIATUS DATES IF ANY                                         
*********************************************************************           
HIATUS   NTR1                                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BRAS  RE,GETEL                                                         
         BNE   HIATUSX                                                          
         LR    R5,R6                                                            
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'25'                                                     
         BRAS  RE,GETEL                                                         
         BNE   HIATUSX                                                          
         USING RCONHIEL,R6                                                      
                                                                                
         CLI   RCONHILN,2          SKIP IF NO DATES                             
         BNH   HIATUSX                                                          
                                                                                
         BAS   RE,CMTSECT                                                       
         GOTO1 =A(PRINT),RR=Y                                                   
         MVC   P+6(23),=C'*AGENCY HIATUS DATE(S)*'                              
         GOTO1 =A(PRINT),RR=Y                                                   
                                                                                
         ZIC   R2,RCONHILN                                                      
         SH    R2,=H'2'            SUBTRACT OVERHEAD AND                        
         SRL   R2,1                DIVIDE BY 2 TO GET NUMBER OF ENTRIES         
                                                                                
         LA    R6,RCONHIDT                                                      
         DROP  R6                                                               
                                                                                
         LA    R4,P+6                                                           
                                                                                
* IF WEEKLY, WILL TRY TO COLLASP DATES. IE AUG24-3W                             
                                                                                
HIATUS20 DS    0H                                                               
         LA    R3,1                NUMBER OF CONSECUTIVE WEEKS                  
         GOTO1 DATCON,DMCB,(2,0(R6)),(4,0(R4))                                  
         LA    R4,5(R4)                                                         
                                                                                
         USING RCONDREL,R5                                                      
         TM    RCONDRFG,X'08'      DAILY?                                       
         BO    HIATUS50                                                         
         DROP  R5                                                               
                                                                                
         CH    R2,=H'1'                                                         
         BNH   HIATUS40                                                         
                                                                                
HIATUS30 DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,0(R6)),(0,WORK3)                                  
         GOTO1 DATCON,DMCB,(2,2(R6)),(0,WORK3+6)                                
         GOTO1 ADDAY,DMCB,WORK3,WORK,7                                          
         CLC   WORK(6),WORK3+6     IF NEXT DATE IS EXACTLY ONE WEEK             
         BNE   HIATUS40            AWAY, KEEP LOOKING                           
                                                                                
         MVC   WORK3(6),WORK3+6                                                 
         LA    R3,1(R3)                                                         
         LA    R6,2(R6)                                                         
         BCTR  R2,0                                                             
         CH    R2,=H'1'                                                         
         BH    HIATUS30                                                         
         SR    R2,R2                                                            
                                                                                
HIATUS40 DS    0H                                                               
         MVI   0(R4),C'-'                                                       
         EDIT  (R3),(2,1(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         MVI   1(R4),C'W'                                                       
         LA    R4,2(R4)                                                         
                                                                                
HIATUS50 DS    0H                                                               
         LTR   R2,R2                                                            
         BZ    HIATUS80                                                         
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BZ    HIATUS80                                                         
                                                                                
         LA    R4,1(R4)                                                         
         LA    R6,2(R6)                                                         
         LA    RF,P+66                                                          
         CR    R4,RF                                                            
         BL    HIATUS20                                                         
         GOTO1 =A(PRINT),RR=Y                                                   
         LA    R4,P+6                                                           
         B     HIATUS20                                                         
                                                                                
HIATUS80 DS    0H                                                               
         GOTO1 =A(PRINT),RR=Y                                                   
                                                                                
HIATUSX  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
* PRINT NON-DARE HIATUS DATES AND COMMENTS IF ANY                               
*********************************************************************           
* ADAPTED FROM RECNT54                                                          
NODAHIAT NTR1                                                                   
         MVI   WORK3,C' '                                                       
         MVC   WORK3+1(L'WORK3-1),WORK3   CLEAR OUT WORK3                       
*                                                                               
*              PRINT EFFECTIVE DATES                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'25'        HIATUS DATES ELEMENT                         
         BRAS  RE,GETEL                                                         
         BNE   NDHI205             PRINT COMMENTS                               
*                                                                               
         BAS   RE,CMTSECT                                                       
         GOTO1 =A(PRINT),RR=Y                                                   
         MVC   P+6(16),=C'*HIATUS DATE(S)*'                                     
         GOTO1 =A(PRINT),RR=Y                                                   
*                                                                               
         LA    R2,WORK3            OUTPUT                                       
         LA    R5,WORK3+60         OUTPUT END                                   
         ZIC   R4,1(R6)                                                         
         AR    R4,R6               R4 = ELEMENT END                             
         LA    R6,2(R6)            R6 = POSITION IN ELEMENT                     
*                                                                               
PRINDTES LA    R3,WORK+20          BUILD AREA                                   
*              PRINT DATES                                                      
* 3 BYTE DATE ENTRIES: 2 BYTE COMPRESSED START DATE, 1 BYTE NUM OF              
* DAYS THAT HIATUS LASTS FROM START DATE                                        
         GOTO1 DATCON,DMCB,(2,(R6)),(4,(R3))     START DATE                     
*                                                                               
         LA    RE,5                                                             
         CLI   3(R3),C'0'          DAY =JAN01?                                  
         BNE   NDHI100                                                          
         MVC   3(1,R3),4(R3)       COMPRESS TO JAN1                             
         MVI   4(R3),C' '                                                       
         BCTR  RE,0                                                             
NDHI100  AR    R3,RE                                                            
*                                                                               
         CLI   2(R6),0             NON-ZERO NUM OF DAYS FROM START DAT?         
         BE    NDHI160             NO                                           
*                                                                               
         MVI   0(R3),C'-'          YES, THERE ARE SOME DAYS                     
         LA    R3,1(R3)                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(2,(R6)),WORK3+200   START DATE FOR ADDAY            
         ZIC   RE,2(R6)                         GET NUMBER OF DAYS              
         ST    RE,DMCB+8                                                        
         GOTO1 ADDAY,DMCB,WORK3+200,WORK3+206   END DATE IN ADDAY FORM          
*                                                                               
         GOTO1 DATCON,DMCB,WORK3+206,(4,(R3))   END DATE IN PRINT FORM          
*                                                                               
         LA    RE,5                                                             
         CLI   3(R3),C'0'          DAY =JAN01?                                  
         BNE   NDHI120                                                          
         MVC   3(1,R3),4(R3)       COMPRESS TO JAN1                             
         MVI   4(R3),C' '                                                       
         BCTR  RE,0                                                             
NDHI120  AR    R3,RE                                                            
*                                                                               
NDHI160  LA    RE,WORK+20                                                       
         SR    R3,RE               GET ELEM PRINT LEN                           
         LR    RF,R2                                                            
         AR    RF,R3               OUTPUT PTR                                   
* CHECK IF ROOM IN FIRST LINE                                                   
         CR    RF,R5               WORK3+60                                     
         BNH   NDHI164                                                          
* FIRST LINE EXCEEDED - START AT SECOND LINE                                    
         LA    R5,500(R5)          ELIM. FIRST TEST                             
         LA    R2,WORK3+60         START 2D LINE                                
         CLI   WORK3+60,0          DELIMITER?                                   
         BNE   NDHI164                                                          
         LA    R2,1(R2)                                                         
*                                                                               
NDHI164  BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WORK+20                                                  
         MVC   WORK+20(20),MYSPACES                                             
         LA    R2,1(R3,R2)         OUTPUT PTR                                   
*                                                                               
         LA    RE,WORK3+180                                                     
         CR    R2,RE               SECOND LINE EXCEEDED?                        
         BNH   *+12                                                             
         MVI   WORK3+179,C'>'      DOESN'T FIT                                  
         B     NDHI200                                                          
*                                                                               
         LA    R6,3(R6)                                                         
         CR    R6,R4               END OF ELEMENT?                              
         BNL   NDHI200             YES                                          
*                                                                               
         MVI   0(R2),0             DELIMITER                                    
         LA    R2,1(R2)                                                         
         B     PRINDTES                                                         
*                                                                               
NDHI200  MVC   P+6(60),WORK3       DATES                                        
         GOTO1 =A(PRINT),RR=Y                                                   
         MVC   P+6(120),WORK3+60   MOVE 2D LINE (PLUS EXTRA FOR OVERFL)         
         GOTO1 =A(PRINT),RR=Y                                                   
*                                                                               
* PRINT COMMENTS                                                                
NDHI205  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'26'                                                     
         BRAS  RE,GETEL            ANY COMMENT ELEMENTS?                        
         BNE   NDHIX               NO, GO OUT                                   
*                                                                               
         BAS   RE,CMTSECT                                                       
         GOTO1 =A(PRINT),RR=Y                                                   
         MVC   P+6(19),=C'*HIATUS COMMENT(S)*'                                  
         GOTO1 =A(PRINT),RR=Y                                                   
*                                                                               
         USING RCONHCEL,R6                                                      
NDHI220  ZIC   R1,RCONHCLN         ELEMENT LENGTH                               
         LA    R4,2                                                             
         SR    R1,R4               SUBTRACT OVERHEAD                            
         LTR   R1,R1               ZERO LENGTH?                                 
         BZ    NDHI230             YES, DON'T PRINT ANYTHING                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+6(0),RCONHCCM     MOVE TO PRINT LINE                           
         GOTO1 =A(PRINT),RR=Y                                                   
         DROP  R6                                                               
*                                                                               
NDHI230  BRAS  RE,NEXTEL           ANY MORE ELEMENTS?                           
         BE    NDHI220             YES                                          
*                                                                               
NDHIX    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* ROUTINE TO PRINT A LINE OF STORED COMMENT                                     
*                                                                               
* FOR FORMATS A & G, LEADING BLANKS ARE DELETED AND THE HEADER LINE             
*  IS COMPRESSED.  FURTHERMORE, FORMAT G WILL WORD WRAP AT COL 66               
*********************************************************************           
PRINT    NTR1  BASE=*,LABEL=*                                                   
         CLI   FORMAT,C'A'         FOR FAX, DON'T WANT PAGE HEADER              
         BE    PRINT10             ON ALL PAGES                                 
         CLI   FORMAT,C'G'         FOR KWX, DON'T WANT PAGE HEADER              
         BNE   PRINT80             ON ALL PAGES                                 
*                                                                               
PRINT10  MVC   SVPRNT,P            DELETE LEADING BLANKS                        
         MVC   P,SPACES                                                         
         LA    R3,SVPRNT                                                        
         LA    R4,132                                                           
PRINT20  CLI   0(R3),C' '          LOOP UNTIL NO MORE LEADING BLANKS            
         BNE   PRINT30                                                          
         LA    R3,1(R3)                                                         
         BCT   R4,PRINT20                                                       
         B     PRINT80                                                          
*                                                                               
PRINT30  DS    0H                                                               
*                                                                               
         CLC   =C'*CONTRACT COMMENT*',0(R3)  FOR FORMAT A & G (FAX), WE         
         BE    PRINT40                       NEED TO SQUASH THE LINE SO         
         CLC   =C'*REP ORDER COMMENT*',0(R3) WE CAN PRINT AS MUCH DATA          
         BE    PRINT50                       AS WE CAN!!                        
         CLC   =C'*AGENCY POLICY*',0(R3)                                        
         BE    PRINT60                                                          
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),0(R3)                                                       
         B     PRINT70                                                          
*                                                                               
PRINT40  MVC   P(10),=C'*CON CMT* ' COMPRESS HEADER                             
         B     *+10                                                             
PRINT50  MVC   P(10),=C'*ORD CMT* '                                             
         MVC   P+10(70),24(R3)                                                  
         B     PRINT70                                                          
*                                                                               
PRINT60  MVC   P(16),0(R3)         AGENCY POLICY                                
         MVC   P+16(70),24(R3)                                                  
*                                                                               
PRINT70  DS    0H                  FOR KWX(G), WORD WRAP AT 66                  
         CLI   FORMAT,C'A'                                                      
         BE    PRINT80                                                          
         CLC   P+66(24),SPACES                                                  
         BE    PRINT80                                                          
         MVC   SVPRNT,P                                                         
         MVC   P+66(66),SPACES                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(24),SVPRNT+66                                                  
*                                                                               
PRINT80  GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRINTX              FOR COMMENTS PRINT AT END-OF-K               
*                                                                               
PRINTX   MVI   MYHEDSW,C'N'                                                     
         B     XIT                                                              
         DROP  R9                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUY COMBO CONTRACT PRINT                                            *         
***********************************************************************         
PRTCOMBO NMOD1 PTCOMBOX-PTCOMBOD,*PCOMBO*                                       
         LR    R5,RC                                                            
         USING PTCOMBOD,R5                                                      
         L     RC,0(R1)                                                         
         L     R7,4(R1)                                                         
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         XC    COMTOTAL,COMTOTAL   INITIALIZE GRAND TOTAL STORAGE               
         MVC   SVCKEY,RCONREC      SAVE FIRST COMBO K KEY                       
         MVC   SVKNUM,TWACNUM      SAVE OFF CURRENT TWACNUM                     
         MVI   CMBNDX,1            INDEX, SET TO FIRST COMPONENT K              
*                                                                               
         LA    R6,RCONREC          PRINT BUYLINES IN THE ORDER AS               
         USING RCONCBEL,R6         FOUND IN THE COMBO ELEMENT                   
         MVI   ELCODE,X'17'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    SVCMB17,SVCMB17     SAVE OFF COMPONENTS, CONTRACT IO             
         ZIC   R1,RCONCBLN         AREA GETS USED BY OTHER CONTRACTS            
         SH    R1,=H'3'            OVERHEAD                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVCMB17(0),RCONCBST                                              
*                                                                               
         LA    R2,SVCMB17+5        GET NEXT COMPONENT K#                        
         DROP  R6                                                               
*                                                                               
PCOMBO10 DS    0H                  LOAD NEXT COMPONENT K INTO RCONREC           
         ZAP   MYWORK+10(5),=P'0'                                               
         MVO   MYWORK+10(5),0(4,R2) CHANGE TO PACK WITH SIGN                    
         ZAP   MYWORK+5(5),=P'99999999'                                         
         SP    MYWORK+5(5),MYWORK+10(5) GET 9'S COMPLEMENT                      
         MVO   MYWORK(5),MYWORK+5(5) CHANGE TO PWOS                             
*                                                                               
         XC    RCONREC(32),RCONREC                                              
         MVC   RCONPCON,MYWORK     NUMBER                                       
         MVC   RCONPREP,REPALPHA                                                
         MVI   RCONPTYP,X'8C'                                                   
         MVC   KEY,RCONREC                                                      
         OI    DMINBTS,X'08'                                                    
         GOTO1 VHIGH               GET CONTRACT KEY                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TWAKADDR,KEY+28                                                  
         MVC   TWACNUM,KEY+23                                                   
         PACK  TWACNUM(1),KEY+26(1)      REVERSE THE COMPLIMENT                 
         PACK  TWACNUM+1(1),KEY+25(1)                                           
         PACK  TWACNUM+2(1),KEY+24(1)                                           
         PACK  TWACNUM+3(1),KEY+23(1)                                           
*                                                                               
         OI    DMINBTS,X'08'                                                    
         GOTO1 VGETREC,DMCB,RCONREC                                             
         TM    RCONCNTL,X'80'                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    TWAWSFLG,X'40'      ORD CMT ALREADY PRINTED ON WRKSHT?           
         BO    PCOMBO20            YES - SKIP FORCING IT TO PRINT NOW           
*                                                                               
         OI    TWAWSFLG,X'01'      FORCE WRKSHT HEADER TO PRINT BEFORE          
         OI    TWAWSFLG,X'04'      REQUEST SEQ LOOP BE RESTORED                 
         GOTO1 AFMT,DMCB,(RC),(R7),0                                            
         NI    TWAWSFLG,X'FF'-X'04'-X'01'                                       
*                                                                               
PCOMBO20 DS    0H                                                               
         MVC   P+10(4),RCONKSTA    PRINT COMPONENT STATIONS                     
         MVI   P+14,C'-'                                                        
         MVC   P+15(1),RCONKSTA+4                                               
         ZAP   DUB,=P'0'                                                        
         MVO   DUB+3(5),RCONKCON   K NUMBER                                     
         EDIT  (P5,DUB+3),(8,P+25),ALIGN=LEFT                                   
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
*                                                                               
         TM    RCONMODR+1,X'20'    FOR MON, DON'T PRINT BUYLINES                
         BO    PCOMBO43                                                         
         GOTO1 =A(DOBUYS),RR=Y    GO PRINT BUYLINES                             
*                                                                               
PCOMBO43 DS    0H                                                               
         GOTO1 =A(PTOTAL),RR=Y                                                  
*                                                                               
         TM    RCONMODR+1,X'20'    MON???                                       
         BZ    PCOMBO45                                                         
         GOTO1 =V(REGENBUF),DMCB,RCONREC,WORK3X,RR=YES                          
*                                                                               
PCOMBO45 DS    0H                                                               
         CLI   CMBNDX,1            SKIP GRAND TOTAL CALCULATION FOR             
         BNE   PCOMBO50            FIRST PASS                                   
         MVC   COMTOTAL,WORK3X                                                  
         B     PCOMBO60                                                         
*                                                                               
PCOMBO50 DS    0H                                                               
         GOTO1 =A(TOTLTOTL),DMCB,(RC),COMTOTAL,RR=Y                             
*                                                                               
PCOMBO60 DS    0H                                                               
         XC    WORK3X,WORK3X                                                    
         MVC   WORK3X(2),=H'2'     LENGTH                                       
         XC    XWORK4,XWORK4                                                    
         MVC   XWORK4(2),=H'2'     LENGTH                                       
         ZIC   RF,CMBNDX           BUMP COMPONENT K POINTER                     
         LA    RF,1(RF)                                                         
         STC   RF,CMBNDX                                                        
*                                                                               
         CLC   CMBNDX,TWACOMBO     WE HAVE THIS MANY COMBO K'S TO DO            
         BH    PCOMBO70                                                         
         LA    R2,9(R2)            GET NEXT COMPONENT K#                        
         B     PCOMBO10                                                         
*                                                                               
PCOMBO70 DS    0H                                                               
         BAS   RE,GRANDTOT         PRINT THE GRAND TOTAL                        
*                                  ALL DONE, RESTORE ORIGINAL COMBO K           
*                                  TO RCONREC AREA                              
         MVC   KEY(L'RCONKEY),SVCKEY                                            
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         TM    RCONCNTL,X'F2'                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TWAKADDR,KEY+28     RESTORE K INFO IN TWA, TOO                   
         MVC   TWACNUM,SVKNUM                                                   
*                                                                               
PCOMBOX  DS    0H                                                               
         DROP  R4                                                               
         XMOD1                                                                  
***********************************************************************         
* PRINT THE GRAND TOTAL - COMBO ONLY                                  *         
***********************************************************************         
GRANDTOT NTR1                                                                   
         LR    R4,RA                                                            
         AHI   R4,TWAWORKQ                                                      
         USING TWAWORK,R4                                                       
*                                                                               
         L     R6,AIO2                                                          
*                                                                               
         OC    XWORK6(2),XWORK6    ARRAY INITIALIZED?                           
         BNZ   GTOT0001            YES                                          
         MVC   XWORK6(2),=X'0002'  NO  - INITIALIZE ARRAY                       
GTOT0001 EQU   *                                                                
         OC    XWORK5(2),XWORK5    ARRAY INITIALIZED?                           
         BNZ   GTOT0002            YES                                          
         MVC   XWORK5(2),=X'0002'  NO  - INITIALIZE ARRAY                       
GTOT0002 EQU   *                                                                
         CLC   =X'0002',XWORK6     ANY TRADE GRAND TOTALS IN ARRAY?             
         BE    GTOT005             NO  - JUST SHOW COMBO TOTALS                 
         CLC   =X'0002',XWORK5     YES - ANY CASH GRAND TOTALS?                 
         BE    GTOT0400            NO  - JUST SHOW TRADE                        
         B     GTOT0200            YES - SHOW CASH+TRADE                        
*                                                                               
GTOT005  EQU   *                                                                
*                                                                               
         GOTO1 REGENTL2,DMCB,(SVDEMFLG,COMTOTAL),(R6)                           
         ZIC   R2,DMCB+4           TOTAL LINES                                  
*                                                                               
         MVC   P(13),=C'COMBO TOTALS:'                                          
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
*                                                                               
GTOT20   DS    0H                                                               
         MVC   P+1(79),0(R6)       TOTAL LINES                                  
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
         LA    R6,80(R6)                                                        
         BCT   R2,GTOT20                                                        
         B     GTOTX                                                            
GTOT0200 EQU   *                                                                
         GOTO1 REGENTL2,DMCB,(SVDEMFLG,XWORK5),(R6)                             
         ZIC   R2,DMCB+4           TOTAL LINES                                  
*                                                                               
         MVC   P(15),=C'**CASH TOTALS**'                                        
         MVC   P+58(15),=C'**CASH TOTALS**'                                     
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
*                                                                               
GTOT220  DS    0H                                                               
         MVC   P+1(79),0(R6)       TOTAL LINES                                  
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
         LA    R6,80(R6)                                                        
         BCT   R2,GTOT220                                                       
*                                                                               
GTOT0400 EQU   *                                                                
         GOTO1 REGENTL2,DMCB,(SVDEMFLG,XWORK6),(R6)                             
         ZIC   R2,DMCB+4           TOTAL LINES                                  
*                                                                               
         MVC   P(16),=C'**TRADE TOTALS**'                                       
         MVC   P+58(16),=C'**TRADE TOTALS**'                                    
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
*                                                                               
GTOT420  DS    0H                                                               
         MVC   P+1(79),0(R6)       TOTAL LINES                                  
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
         LA    R6,80(R6)                                                        
         BCT   R2,GTOT420                                                       
*                                                                               
         CLC   =X'0002',XWORK5     ANY CASH GRAND TOTALS?                       
         BE    GTOTX               NO  - EXIT                                   
*                                  YES - SHOW CASH+TRADE TOTALS                 
         GOTO1 =A(GENLTOTL),DMCB,XWORK6,XWORK5,RR=Y                             
*                                  CASH+TRADE GRAND TOTAL                       
         GOTO1 REGENTL2,DMCB,(SVDEMFLG,XWORK6),(R6),ASPULAR                     
         ZIC   R2,DMCB+4           TOTAL LINES                                  
*                                                                               
         MVC   P(16),=C'**GRAND TOTALS**'                                       
         MVC   P+58(16),=C'**GRAND TOTALS**'                                    
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
*                                                                               
GTOT520  DS    0H                                                               
         MVC   P+1(79),0(R6)       TOTAL LINES                                  
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
         LA    R6,80(R6)                                                        
         BCT   R2,GTOT520                                                       
*                                                                               
GTOTX    DS    0H                                                               
         XIT1                                                                   
         DROP  R4,R5,R8                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* LOCAL WORK STORAGE AREA                                                       
PTCOMBOD DSECT                                                                  
CMBNDX   DS    XL1                 PRINT COMBO POINTER, SO WE KNOW              
*                                    WHICH CONTRACT WE ARE READING              
MYWORK   DS    XL20                                                             
SVCKEY   DS    CL27                SAVE CONTRACT KEY                            
SVKNUM   DS    F                   SAVE ORIGINAL TWACNUM                        
SVCMB17  DS    XL36                SAVES OFF 17 ELEMENT                         
COMTOTAL DS    CL200                                                            
PTCOMBOX EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
*  GENLTOTL:  ACCUMULATES HIGHER-LEVEL TOTALS                                   
*    P1            =    ADDRESS OF HIGHER LEVEL TOTALS                          
*    P2            =    TOTALS TO BE ADDED IN                                   
*    IO2           =    TEMPORARY WORK SPACE (THIS IS IO2)                      
*                                                                               
*  WARNING: USES R8 SINCE NO SPOOLD VARIABLES ARE REFERENCED IN HERE            
***********************************************************************         
*                                                                               
*  EQUATES ARE ESTABLISHED IN ROUTINE TOTLTOTL                                  
*                                                                               
*BCKTDATE EQU   2                                                               
*BCKTVALU EQU   6                                                               
*BCKTSPOT EQU   10                                                              
*                                                                               
T80263   CSECT                                                                  
GENLTOTL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,AIO2                                                          
         L     R6,0(R1)            SET A(HIGHER LEVEL TOTALS)                   
         LR    R8,R6               SAVE A(HIGH LEVEL TOTALS)                    
         L     R3,4(R1)            SET A(TOTALS TO BE ADDED IN)                 
*                                    OUTSIDE DSECT ADDRESSABILITY               
         MVC   0(240,R2),0(R6)     LOAD TEMPORARY WORK SPACE                    
*                                                                               
         LR    RE,R3               A(LOWER LEVEL TOTALS)                        
         ZICM  R0,0(RE),2          L(ENTRY)                                     
         AR    RE,R0               A(END OF ENTRIES)                            
*                                                                               
         LR    RF,R2               A(TEMP WORK SPACE)                           
         ZICM  R0,0(RF),2          L(ENTRY)                                     
         AR    RF,R0               A(END OF ENTRIES)                            
*                                                                               
         LA    R1,2(R3)            A(1ST BUCKET STA IN PROG)                    
         LA    R2,2(R2)            A(1ST BUCKET TEMP WORK SPACE)                
         LA    R3,2(R6)            A(1ST BUCKET ACCUMULATOR)                    
*                                                                               
GENL0010 EQU   *                                                                
*                                                                               
         CR    R1,RE               STA IN PROGRESS AT END?                      
         BNE   GENL0020            NO  - CHECK TEMP WORK SPACE                  
*                                                                               
         CR    R2,RF               YES - TEMP WORK SPACE AT END?                
         BNE   GENL0050            NO  - RUN TEMP WORK SPACE                    
*                                                                               
         B     GENL0100            YES -                                        
*                                                                               
GENL0020 EQU   *                                                                
         CR    R2,RF               TEMP WORK SPACE AT END?                      
         BE    GENL0060            YES - RUN OUT STA IN PROGRESS                
*                                                                               
*   NEITHER ARRAY AT END:  COMPARE DATES                                        
*                                                                               
         CLC   BCKTDATE(2,R1),BCKTDATE(R2)                                      
         BL    GENL0030            STA IN PROG < TEMP WORK SPACE                
         BH    GENL0040            TEMP WORK SPACE < STA IN PROG                
*                                                                               
*   DATES EQUAL:  ACCUMULATE SPOTS AND TOTALS                                   
*                                                                               
         MVC   DUB(4),BCKTVALU(R1)                                              
         L     R4,DUB                                                           
*                                                                               
         MVC   DUB(4),BCKTVALU(R2)                                              
         L     R5,DUB                                                           
*                                                                               
         AR    R4,R5                                                            
         ST    R4,DUB                                                           
*                                                                               
         LLC   R9,1(R1)            ELEMENT LENGTH                               
         BCTR  R9,0                DECREMENT FOR EXECUTE                        
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)       SET UP ACCUMULATOR ELEMENT                   
*                                                                               
         MVC   BCKTVALU(4,R3),DUB  LOAD NEW DOLLARS                             
*                                                                               
* ACCUMULATE TOTAL SPOTS                                                        
*                                                                               
         ZICM  R4,BCKTSPOT(R1),4                                                
         ZICM  R5,BCKTSPOT(R2),4                                                
         AR    R4,R5                                                            
         STCM  R4,15,BCKTSPOT(R3)                                               
*                                                                               
* ACCUMULATE TOTAL GRPS                                                         
*                                                                               
         CLI   1(R1),14            SKIP IF NO GRPS                              
         BNH   GENL0025                                                         
*                                                                               
         ZICM  R4,BCKTGRPS(R1),4                                                
         ZICM  R5,BCKTGRPS(R2),4                                                
         AR    R4,R5                                                            
         STCM  R4,15,BCKTGRPS(R3)                                               
*                                                                               
GENL0025 EQU   *                                                                
*                                                                               
         LLC   R9,1(R1)            ELEMENT LENGTH                               
         LA    R1,0(R9,R1)         NEXT ELEMENT                                 
*                                                                               
         LLC   R9,1(R2)            ELEMENT LENGTH                               
         LA    R2,0(R9,R2)         NEXT ELEMENT                                 
*                                                                               
         LLC   R9,1(R3)            ELEMENT LENGTH                               
         LA    R3,0(R9,R3)         NEXT ELEMENT                                 
*                                                                               
         B     GENL0010                                                         
*                                                                               
GENL0030 EQU   *                                                                
*                                                                               
         LLC   R9,1(R1)            ELEMENT LENGTH                               
         BCTR  R9,0                DECREMENT FOR EXECUTE                        
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)       RUN OUT STA IN PROGRESS                      
*                                                                               
         LLC   R9,1(R1)            ELEMENT LENGTH                               
         LA    R1,0(R9,R1)         NEXT ELEMENT                                 
*                                                                               
         LLC   R9,1(R3)            ELEMENT LENGTH                               
         LA    R3,0(R9,R3)         NEXT ELEMENT                                 
*                                                                               
         B     GENL0010                                                         
*                                                                               
GENL0040 EQU   *                                                                
*                                                                               
         LLC   R9,1(R2)            ELEMENT LENGTH                               
         BCTR  R9,0                DECREMENT FOR EXECUTE                        
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)       RUN OUT TEMP WORK SPACE                      
*                                                                               
         LLC   R9,1(R2)            ELEMENT LENGTH                               
         LA    R2,0(R9,R2)         NEXT ELEMENT                                 
*                                                                               
         LLC   R9,1(R3)            ELEMENT LENGTH                               
         LA    R3,0(R9,R3)         NEXT ELEMENT                                 
*                                                                               
         B     GENL0010                                                         
*                                                                               
GENL0050 EQU   *                   RUN OUT TEMP WORK SPACE                      
*                                                                               
         CR    R2,RF               END OF TEMP WORK SPACE?                      
         BE    GENL0100            YES                                          
*                                                                               
         LLC   R9,1(R2)            ELEMENT LENGTH                               
         BCTR  R9,0                DECREMENT FOR EXECUTE                        
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)       RUN OUT TEMP WORK SPACE                      
*                                                                               
         LLC   R9,1(R2)            ELEMENT LENGTH                               
         LA    R2,0(R9,R2)         NEXT ELEMENT                                 
*                                                                               
         LLC   R9,1(R3)            ELEMENT LENGTH                               
         LA    R3,0(R9,R3)         NEXT ELEMENT                                 
*                                                                               
         B     GENL0050                                                         
*                                                                               
GENL0060 EQU   *                   RUN OUT STA IN PROGRESS                      
*                                                                               
         CR    R1,RE               END OF STA IN PROGRESS?                      
         BE    GENL0100            YES                                          
*                                                                               
         LLC   R9,1(R1)            ELEMENT LENGTH                               
         BCTR  R9,0                DECREMENT FOR EXECUTE                        
         EX    R9,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)       RUN OUT STA IN PROGRESS                      
*                                                                               
         LLC   R9,1(R1)            ELEMENT LENGTH                               
         LA    R1,0(R9,R1)         NEXT ELEMENT                                 
*                                                                               
         LLC   R9,1(R3)            ELEMENT LENGTH                               
         LA    R3,0(R9,R3)         NEXT ELEMENT                                 
*                                                                               
         B     GENL0060                                                         
*                                                                               
GENL0100 EQU   *                                                                
*                                                                               
         LR    RF,R8               RECALCULATE LENGTH                           
         SR    R3,RF                                                            
         STH   R3,DUB                                                           
*                                                                               
         MVC   0(2,RF),DUB         INSERT NEW LENGTH                            
*                                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*  TOTLTOTL:  ACCUMULATES EACH STATION'S TOTALS INTO A GRAND                    
*    TOTAL ARRAY, CONSIDERING DIFFERENCES IN THE MONTHS REPORTED                
*    WITHIN STATIONS.  THE FINAL TOTALS WILL BE FED BACK THROUGH                
*    THE PRINT ROUTINE TO PRODUCE GRAND TOTALS ON THE SCREEN                    
*    COMTOTAL      =    ADDRESS OF GRAND TOTALS                                 
*    WORK3X        =    TOTALS OF STATION IN PROGRESS                           
*    IO2           =    TEMPORARY WORK SPACE (THIS IS IO2)                      
***********************************************************************         
BCKTDATE EQU   2                                                                
BCKTVALU EQU   6                                                                
BCKTSPOT EQU   10                                                               
BCKTGRPS EQU   14                                                               
*                                                                               
TOTLTOTL NMOD1 0,**TOTL**                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R6,4(R1)            A(TOTAL AREA)                                
*                                                                               
         L     R2,AIO2                                                          
         MVC   0(240,R2),0(R6)     LOAD TEMPORARY WORK SPACE                    
*                                    OUTSIDE DSECT ADDRESSABILITY               
         LA    RE,WORK3X           A(STATION IN PROGRESS)                       
         ZICM  R0,0(RE),2          L(ENTRY)                                     
         AR    RE,R0               A(END OF ENTRIES)                            
*                                                                               
         LR    RF,R2               A(TEMP WORK SPACE)                           
         ZICM  R0,0(RF),2          L(ENTRY)                                     
         AR    RF,R0               A(END OF ENTRIES)                            
*                                                                               
         LA    R1,WORK3X+2         A(1ST BUCKET STA IN PROG)                    
         LA    R2,2(R2)            A(1ST BUCKET TEMP WORK SPACE)                
         LA    R3,2(R6)            A(1ST BUCKET ACCUMULATOR)                    
*                                                                               
TOTO0010 EQU   *                                                                
*                                                                               
         CR    R1,RE               STA IN PROGRESS AT END?                      
         BL    TOTO0020            NO  - CHECK TEMP WORK SPACE                  
*                                                                               
         CR    R2,RF               YES - TEMP WORK SPACE AT END?                
         BL    TOTO0050            NO  - RUN TEMP WORK SPACE                    
*                                                                               
         B     TOTO0100            YES -                                        
*                                                                               
TOTO0020 EQU   *                                                                
*                                                                               
         CR    R2,RF               TEMP WORK SPACE AT END?                      
         BNL   TOTO0060            YES - RUN OUT STA IN PROGRESS                
*                                                                               
*   NEITHER ARRAY AT END:  COMPARE DATES                                        
*                                                                               
         CLC   BCKTDATE(2,R1),BCKTDATE(R2)                                      
         BL    TOTO0030            STA IN PROG < TEMP WORK SPACE                
         BH    TOTO0040            TEMP WORK SPACE < STA IN PROG                
*                                                                               
*   DATES EQUAL:  ACCUMULATE TOTALS                                             
*                                                                               
         XC    BCKTSPOT(4,R3),BCKTSPOT(R3) CLEAR SPOTS                          
*                                                                               
         MVC   DUB(4),BCKTVALU(R1)                                              
         L     R4,DUB                                                           
*                                                                               
         MVC   DUB(4),BCKTVALU(R2)                                              
         L     R5,DUB                                                           
*                                                                               
         AR    R4,R5                                                            
         ST    R4,DUB                                                           
*                                                                               
         LLC   R8,1(R1)            ACCUMULATOR ELM LENGTH                       
         BCTR  R8,0                DECREMENT FOR EXECUTE                        
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)       SET UP ACCUMULATOR ELEMENT                   
*                                                                               
         MVC   BCKTVALU(4,R3),DUB  LOAD NEW DOLLARS                             
*                                                                               
* ACCUMULATE TOTAL SPOTS                                                        
*                                                                               
         ZICM  R4,BCKTSPOT(R1),4                                                
         ZICM  R5,BCKTSPOT(R2),4                                                
         AR    R4,R5                                                            
         STCM  R4,15,BCKTSPOT(R3)                                               
*                                                                               
* ACCUMULATE TOTAL GRPS                                                         
*                                                                               
         CLI   1(R1),14            SKIP IF NO GRPS                              
         BNH   TOTO0025                                                         
*                                                                               
         ZICM  R4,BCKTGRPS(R1),4                                                
         ZICM  R5,BCKTGRPS(R2),4                                                
         AR    R4,R5                                                            
         STCM  R4,15,BCKTGRPS(R3)                                               
*                                                                               
TOTO0025 EQU   *                                                                
*                                                                               
         LLC   R8,1(R1)            BUCKET LENGTH                                
         LA    R1,0(R8,R1)         NEXT SET OF BUCKETS                          
         LLC   R8,1(R2)            BUCKET LENGTH                                
         LA    R2,0(R8,R2)         NEXT SET OF BUCKETS                          
         LLC   R8,1(R3)            BUCKET LENGTH                                
         LA    R3,0(R8,R3)         NEXT SET OF BUCKETS                          
*                                                                               
         B     TOTO0010                                                         
*                                                                               
TOTO0030 EQU   *                                                                
*                                                                               
         LLC   R8,1(R1)            ACCUMULATOR ELM LENGTH                       
         BCTR  R8,0                DECREMENT FOR EXECUTE                        
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)       RUN OUT STATION IN PROGRESS                  
*                                                                               
         LLC   R8,1(R1)            BUCKET LENGTH                                
         LA    R1,0(R8,R1)         NEXT SET OF BUCKETS                          
         LLC   R8,1(R3)            BUCKET LENGTH                                
         LA    R3,0(R8,R3)         NEXT SET OF BUCKETS                          
*                                                                               
         B     TOTO0010                                                         
*                                                                               
TOTO0040 EQU   *                                                                
*                                                                               
         LLC   R8,1(R2)            ACCUMULATOR ELM LENGTH                       
         BCTR  R8,0                DECREMENT FOR EXECUTE                        
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)       RUN OUT TEMP WORK SPACE                      
*                                                                               
         LLC   R8,1(R2)            BUCKET LENGTH                                
         LA    R2,0(R8,R2)         NEXT SET OF BUCKETS                          
         LLC   R8,1(R3)            BUCKET LENGTH                                
         LA    R3,0(R8,R3)         NEXT SET OF BUCKETS                          
*                                                                               
         B     TOTO0010                                                         
*                                                                               
TOTO0050 EQU   *                   RUN OUT TEMP WORK SPACE                      
*                                                                               
         CR    R2,RF               END OF TEMP WORK SPACE?                      
         BNL   TOTO0100            YES                                          
*                                                                               
         LLC   R8,1(R2)            ACCUMULATOR ELM LENGTH                       
         BCTR  R8,0                DECREMENT FOR EXECUTE                        
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)       RUN OUT TEMP WORK SPACE                      
*                                                                               
         LLC   R8,1(R2)            BUCKET LENGTH                                
         LA    R2,0(R8,R2)         NEXT SET OF BUCKETS                          
         LLC   R8,1(R3)            BUCKET LENGTH                                
         LA    R3,0(R8,R3)         NEXT SET OF BUCKETS                          
*                                                                               
         B     TOTO0050                                                         
*                                                                               
TOTO0060 EQU   *                   RUN OUT STA IN PROGRESS                      
*                                                                               
         CR    R1,RE               END OF STA IN PROGRESS?                      
         BNL   TOTO0100            YES                                          
*                                                                               
         LLC   R8,1(R1)            ACCUMULATOR ELM LENGTH                       
         BCTR  R8,0                DECREMENT FOR EXECUTE                        
         EX    R8,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)       RUN OUT STATION IN PROGRESS                  
*                                                                               
         LLC   R8,1(R1)            BUCKET LENGTH                                
         LA    R1,0(R8,R1)         NEXT SET OF BUCKETS                          
         LLC   R8,1(R3)            BUCKET LENGTH                                
         LA    R3,0(R8,R3)         NEXT SET OF BUCKETS                          
*                                                                               
         B     TOTO0060                                                         
*                                                                               
TOTO0100 EQU   *                                                                
*                                                                               
         LR    RF,R6               RECALCULATE LENGTH                           
         SR    R3,RF                                                            
         STH   R3,DUB                                                           
         MVC   0(2,RF),DUB      INSERT NEW LENGTH                               
*                                                                               
TOTLTOTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* FOR COMBO ORDER, CHECK IF STATION WAS REASSIGNED TO A DIFFERENT     *         
* PARENT COMBO STATION AFTER THE CONTRACTS WERE CREATED. IF YES, USE  *         
* A DIFFERENT STATION IN THE CONTROL ELEMENT TO LOOK FOR PREVIOUS     *         
* PARENT COMBO STATION. IF NO ORIGINAL PARENT COMBO FOUND, ASSUME NO  *         
* PREFERENCE AND SEND COPIES TO ALL STATIONS.                         *         
*                                                                     *         
* USES IO3.                                                           *         
***********************************************************************         
CHKPREF  NMOD1 0,*CKPREF*                                                       
         L     RC,0(R1)                                                         
         L     R7,4(R1)                                                         
         LR    R4,RA                                                            
         AH    R4,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,R4                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         NI    TWASPREF,X'FF'-PREFSENT DEFAULT                                  
         CLI   TWACOMBO,0          FOR COMBO ORDER ONLY                         
         BE    CHKPX                                                            
*                                                                               
         LA    R5,KEY                                                           
         USING RSTAKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,ACTSTAT                                                 
         DROP  R5                                                               
*                                                                               
         MVI   ION,3               USE IO3                                      
         GOTO1 AHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    CHKP10                                                           
         DC    H'0'                                                             
CHKP10   GOTO1 AGETREC                                                          
*                                                                               
         L     R6,AIO3             STATION RECORD                               
         USING RSTACSEL,R6                                                      
         MVI   ELCODE,X'0A'        COMBINED STATION ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   CHKP160                                                          
***>     BE    CHKP20              NOT THERE                                    
                                                                                
*&&DO                                                                           
* THIS CHECK WAS SET TO DUMP IF IT FAILED THE TEST. THIS IS DUE TO THE          
* NEED TO UNWIND ALL TRANSCATIONS FOR ALL COMBO CONTRACTS. IN AN                
* ATTEMPT TO CURB OCCASSIONAL DUMPS AS A RESULT OF THIS, $ABEND IS              
* INSERTED HERE TO PREVENT THE USERS FROM SEEING THE DUMP SCREENS.              
                                                                                
         XC    CONMSG,CONMSG                                                    
         MVC   CONMSG(ERRCOMLQ),ERRCOMBO                                        
         OI    CONMSGH+6,X'80'     XMIT                                         
         OI    CONCACTH+6,X'40'    FORCE CURSOR HERE                            
         L     R1,AFACILS                                                       
         L     RF,0(R1)                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBALRM   SOUND A BEEP                                 
         DROP  RF                                                               
         DC    H'0',C'$ABEND'                                                   
ERRCOMBO DC    C'*ERROR* : NEED PARENT COMBO ASSIGNMENT IN STATION '            
         DC    C'RECORD'                                                        
ERRCOMLQ EQU   *-ERRCOMBO                                                       
*&&                                                                             
CHKP20   DS    0H                                                               
         CLI   RSTACS+4,C'C'                                                    
         BNE   CHKP70              IF NO PARENT COMBO FOUND,                    
*                                    USE OTHER STAS IN CTRL ELEMENT             
CHKP30   DS    0H                                                               
         LA    R5,KEY                                                           
         USING RSTAKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RSTACS                                                  
         DROP  R5,R6                                                            
*                                                                               
         MVI   ION,3               USE IO3                                      
         GOTO1 AHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    CHKP40                                                           
         DC    H'0'                                                             
CHKP40   GOTO1 AGETREC                                                          
*                                                                               
         L     R6,AIO3             COMBO STATION RECORD                         
         USING RSTACSEL,R6                                                      
         MVI   ELCODE,X'0A'        COMBINED STATION ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   CHKP160             CHILDREN STATIONS REMOVED FROM THIS          
*                         PARENT COMBO, JUST USE CONTRACT'S OWN STATION         
CHKP50   DS    0H                                                               
         CLC   RSTACS,ACTSTAT      IS STATION IN THIS PARENT COMBO?             
         BE    CHKP60                                                           
         BRAS  RE,NEXTEL                                                        
         BE    CHKP50                                                           
         B     CHKP70              HAS NOT BEEN REASSIGNED YET                  
*                                                                               
CHKP60   DS    0H                                                               
         CLI   RSTACSLN,10         CHECK IF ELEMENT HAS ENTRY DATE              
         BL    CHKP160                                                          
         GOTO1 DATCON,DMCB,(2,RSTACDTE),(3,WORK)                                
         DROP  R6                                                               
         CLC   RCONHDRD,WORK       CONTRACT CREATED BEFORE STATION              
         BH    CHKP160               WAS REASSIGNED?                            
*                                                                               
CHKP70   DS    0H                  STATION WAS REASSIGNED AFTER K WAS           
         ZIC   R5,TWACOMBO           CREATED                                    
         LA    R3,TWACMBSC                                                      
                                                                                
CHKP80   DS    0H                                                               
         CLC   ACTSTAT,0(R3)       SKIP IF WE'VE CHECKED THIS ONE               
         BE    CHKP150               ALREADY                                    
                                                                                
CHKP90   DS    0H                                                               
         LA    R6,KEY                                                           
         USING RSTAKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,0(R3)                                                   
         DROP  R6                                                               
*                                                                               
         MVI   ION,3               USE IO3                                      
         GOTO1 AHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    CHKP100                                                          
         DC    H'0'                                                             
*                                                                               
CHKP100  GOTO1 AGETREC                                                          
         L     R6,AIO3             STATION RECORD                               
         USING RSTACSEL,R6                                                      
         MVI   ELCODE,X'0A'        COMBINED STATION ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   CHKP150             NOT THERE                                    
*                                                                               
CHKP110  DS    0H                                                               
         CLI   RSTACS+4,C'C'                                                    
         BNE   CHKP150             IF NO PARENT COMBO FOUND,                    
*                                    USE OTHER STAS IN CTRL ELEMENT             
CHKP120  DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING RSTAKEY,RF                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RSTACS                                                  
         MVC   KEY(L'RSTAKEY),RSTAKEY                                           
         DROP  RF,R6                                                            
*                                                                               
         MVI   ION,3               USE IO3                                      
         GOTO1 AHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AGETREC                                                          
         L     R6,AIO3             COMBO STATION RECORD                         
         USING RSTACSEL,R6                                                      
         MVI   ELCODE,X'0A'        COMBINED STATION ELEMENT                     
         BRAS  RE,GETEL                                                         
         BE    *+6                 NOT THERE                                    
         DC    H'0'                                                             
*                                                                               
CHKP130  DS    0H                                                               
         CLC   RSTACS,0(R3)        IS STATION IN THIS PARENT COMBO?             
         BE    CHKP140                                                          
         BRAS  RE,NEXTEL                                                        
         BE    CHKP130                                                          
         B     CHKP150                                                          
                                                                                
CHKP140  DS    0H                                                               
         CLI   RSTACSLN,10         CHECK IF ELEMENT HAS ENTRY DATE              
         BL    CHKP168                                                          
         GOTO1 DATCON,DMCB,(2,RSTACDTE),(3,WORK)                                
         DROP  R6                                                               
         CLC   RCONHDRD,WORK       CONTRACT CREATED BEFORE STATION              
         BH    CHKP168               WAS REASSIGNED?                            
*                                                                               
CHKP150  DS    0H                                                               
         LA    R3,L'TWACMBSC(R3)                                                
         BCT   R5,CHKP80                                                        
         B     CHKPX                                                            
*                                                                               
CHKP160  DS    0H                  CONTRACT WAS CREATED AFTER THE               
         LA    R5,KEY              STATION WAS REASSIGNED/DELETED               
         USING RSTAKEY,R5          SO OKAY TO USE CONTRACT'S                    
         XC    KEY,KEY             OWN STATION                                  
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,ACTSTAT                                                 
         DROP  R5                                                               
*                                                                               
         MVI   ION,3               USE IO3                                      
         GOTO1 AHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    CHKP165                                                          
         DC    H'0'                                                             
*                                                                               
CHKP165  GOTO1 AGETREC                                                          
         L     R6,AIO3             COMBO STATION RECORD                         
         USING RSTACSEL,R6                                                      
         MVI   ELCODE,X'0A'        COMBINED STATION ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   CHKPX               NOT THERE, SEND TO ALL STATIONS              
*                                                                               
         CLI   RSTACS+4,C'C'                                                    
         BNE   CHKPX               NOT THERE, SEND TO ALL STATIONS              
*                                                                               
         LA    R5,KEY                                                           
         USING RSTAKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,RSTACS                                                  
         DROP  R5,R6                                                            
*                                                                               
         MVI   ION,3               USE IO3                                      
         GOTO1 AHIGH                                                            
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AGETREC                                                          
*                                                                               
CHKP168  DS    0H                                                               
         L     R6,AIO3             COMBO STATION RECORD                         
         USING RSTACSEL,R6                                                      
         MVI   ELCODE,X'0A'        COMBINED STATION ELEMENT                     
         BRAS  RE,GETEL                                                         
         BE    CHKP170             NOT THERE                                    
         DC    H'0'                                                             
*                                                                               
CHKP170  DS    0H                                                               
         CLI   RSTACSLN,8          NEW LENGTH HAS PREFERENCE                    
         BL    CHKP175                                                          
         CLI   RSTACPRF,C'*'       PREFERENCE?                                  
         BE    CHKP180                                                          
*                                                                               
CHKP175  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   CHKPX               NO PREFERENCE, SEND TO ALL STATIONS          
         B     CHKP170                                                          
*                                                                               
CHKP180  DS    0H                                                               
         OI    TWASPREF,PREFSENT   YES, SEND TO PREFERRED STATION               
         MVC   ACTSTAT,RSTACS                                                   
         B     CHKPX                                                            
*                                                                               
CHKPX    DS    0H                                                               
         DROP  R4,R6                                                            
         XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
*********************************************************************           
*        NEWSPL --- PROCESS SPL INFORMATION WITH NEW FORMAT                     
*             R2  =  A(SPL ELEMENT IN RCONREC)                                  
*********************************************************************           
NEWSPL   NMOD1 0,*NEWSPL*                                                       
         L     RC,0(R1)            RELOAD A(WORKSPACE)                          
         LA    R1,RCONELEM         ACCUM $$ FROM ESTIMATES                      
         SR    R7,R7                                                            
NEWS0010 EQU   *                                                                
         CLI   0(R1),0             END OF RECORD?                               
         BE    NEWS0040            YES                                          
         CLI   0(R1),3             ESTIMATE?                                    
         BE    NEWS0030            YES                                          
NEWS0020 EQU   *                                                                
         ZIC   R3,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R3                                                            
         B     NEWS0010            GO BACK FOR NEXT                             
NEWS0030 EQU   *                                                                
         A     R7,6(R1)            ACCUMULATE ESTIMATE $$                       
         B     NEWS0020                                                         
NEWS0040 EQU   *                                                                
         SR    R6,R6                                                            
         D     R6,=F'100'          GET RID OF PENNIES                           
         ST    R7,EST$$$           SAVE RESULT                                  
*                                                                               
         TM    RCONSPES-RCONSPEL(R2),X'40'                                      
*                                  NO REP'D STA:  VALUE = TOTAL?                
         BNO   NEWS0050            NO                                           
         BAS   RE,PERCDOLS         YES - RETRIEVE $$ FROM X'08' ELT             
         B     NEWS0070                                                         
NEWS0050 EQU   *                                                                
         TM    RCONSPES-RCONSPEL(R2),X'20'                                      
*                                  REP STATION OVERRIDE $$?                     
         BNO   NEWS0060            NO                                           
         BAS   RE,PERCDOLS         YES - RETRIEVE $$ FROM X'08' ELT             
NEWS0060 EQU   *                                                                
         GOTO1 CALCTOT$,DMCB,(R2)                                               
*                                  CALC TOTAL $$ FROM COMPONENTS                
NEWS0070 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   AT THIS POINT, EST$$$ CONTAINS TOTAL MARKET BUDGET, EITHER                  
*      CALCULATED FROM REP'D STATION PERCENT AND $$, OR AS A FIXED              
*      FIGURE BECAUSE REP'D STATION HAS NO PERCENT VALUE                        
*                                                                               
*   PERCDOLS:  RETRIEVE X'08' ELEMENT, GET DOLLARS FROM IT                      
*                                                                               
PERCDOLS NTR1                                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'08'        SET FOR X'08' ELEMENT                        
         BRAS  RE,GETEL                                                         
         BE    PDOL0010            FOUND                                        
         DC    H'0'                MUST BE FOUND!!                              
PDOL0010 EQU   *                                                                
         MVC   EST$$$,RCONAC$$-RCONACEL(R6)                                     
*                                  UNLOAD DOLLARS                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CALCTOT$:  CALCULATE THE TOTAL MARKET DOLLARS FROM STATION                  
*      DOLLARS AND PERCENT                                                      
*                                                                               
CALCTOT$ NTR1                                                                   
         L     R2,0(R1)            RELOAD A(X'06' ELEMENT)                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
* RETURN 0 IF DIVISION BY ZERO                                                  
         OC    RCONSPAM-RCONSPEL(4,R2),RCONSPAM-RCONSPEL(R2)                    
         BZ    CALCTOTX                                                         
*                                                                               
         L     RF,EST$$$           LOAD ESTIMATE DOLLARS                        
         M     RE,=F'10000'                                                     
*                                                                               
*   10,000 PROPERLY DECIMAL-ALIGNS A PERCENT VALUE OF FORMAT X.XX%              
*                                                                               
         SLDA  RE,1                DOUBLE FOR ROUNDING                          
         A     RF,RCONSPAM-RCONSPEL(R2)                                         
*                                  PERCENT FROM 1ST MINI-ELEMENT IN             
*                                     X'06' ELT FOR ROUNDING                    
         D     RE,RCONSPAM-RCONSPEL(R2) DIVIDE BY PERCENT                       
         SRA   RF,1                DIVIDE BY 2                                  
*                                                                               
CALCTOTX DS    0H                                                               
         ST    RF,EST$$$           STORE IT BACK                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* PROFILE TO PRINT FORECAST/BUDGET$$                                            
**********************************************************************          
DOXSPL   NMOD1 0,*DOXSPL*                                                       
         L     RC,0(R1)                                                         
                                                                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWATIME,X'20'                                                    
         BZ    DOXSPLX                                                          
         DROP  RF                                                               
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,9            GET EXTRA SPL ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   DOXSPLX                                                          
                                                                                
         USING RCONSXEL,R6                                                      
         MVC   P(10),=C'FORECAST $'                                             
         MVC   P+22(8),=C'BUDGET $'                                             
                                                                                
         OC    RCONSXFD,RCONSXFD                                                
         BNZ   DOXS10                                                           
         TM    RCONSXFG,X'40'                                                   
         BZ    DOXS20                                                           
                                                                                
DOXS10   DS    0H                                                               
         EDIT  RCONSXFD,(8,P+12),ALIGN=LEFT,ZERO=NOBLANK                        
                                                                                
DOXS20   DS    0H                                                               
         OC    RCONSXBD,RCONSXBD                                                
         BNZ   DOXS30                                                           
         TM    RCONSXFG,X'20'                                                   
         BZ    DOXS40                                                           
                                                                                
DOXS30   DS    0H                                                               
         EDIT  RCONSXBD,(8,P+31),ALIGN=LEFT,ZERO=NOBLANK                        
         DROP  R6                                                               
                                                                                
DOXS40   DS    0H                                                               
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
                                                                                
DOXSPLX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*        PPTP --- READ POINT PERSON RECORD FOR NAME                             
*********************************************************************           
PPTP     NMOD1 0,**PPTP**                                                       
         L     RC,0(R1)                                                         
         L     R7,4(R1)                                                         
*                                                                               
         LR    R4,RA                                                            
         AH    R4,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R4                                                       
*                                                                               
         LA    R5,KEY                                                           
         USING RPTPKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RPTPKTYP,X'31'                                                   
         MVC   RPTPKREP,REPALPHA                                                
         MVC   RPTPKREC,TWAPDPTP   POINT PERSON CODE FROM PRD '02' ELEM         
         DROP  R5                                                               
*                                                                               
         MVI   ION,3                                                            
         GOTO1 AHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   PPTPXIT                                                          
*                                                                               
         GOTO1 AGETREC                                                          
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         USING RPTPELEM,R6                                                      
         MVC   WPTPEXP,RPTPNAME    POINT PERSON NAME                            
         MVC   WPTPPH#,RPTPFONE    POINT PERSON PHONE NUMBER                    
*                                                                               
         DROP  R6                                                               
*                                                                               
PPTPXIT  XIT1                                                                   
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* RETRIEVE CORRESPONDING EOP RECORD FOR EOP DISPLAY                             
* WILL ALSO PROTECT FIELD IF ACTIVE FLAG IS SET                                 
* P1=(B1)RECORD TYPE, (B2-3)ADDRESS FOR PRINTING                                
* P2=(B1)LENGTH OF EOP FIELD, (B2-3) ADDRESS OF EOP FIELD                       
* P3=(B1)OFFSET TO FIRST NON-NULL FIELD OF EOP KEY                              
*********************************************************************           
GETEOP   NMOD1 0,*GETEOP*                                                       
         L     RC,0(R1)                                                         
         L     R7,4(R1)                                                         
*                                                                               
         MVC   WORK(12),8(R1)                                                   
                                                                                
         LA    R6,KEY                                                           
         USING REOPKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVC   REOPKTYP(1),WORK    RECORD TYPE                                  
         DROP  R6                                                               
                                                                                
         ZIC   RF,WORK+8           GET OFFSET OF EOP FIELDS IN KEY              
         AR    R6,RF                                                            
         MVC   0(L'REOPKREP,R6),REPALPHA   REP                                  
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVI   2(R6),1             BIAS                                         
         CLI   TWATRFMT,C'B'                                                    
         BE    GETEOP10                                                         
         CLI   TWATRFMT,C'W'       ALLOW W FOR BIAS                             
         BE    GETEOP10                                                         
         MVI   2(R6),2             JDS                                          
         CLI   TWATRFMT,C'J'                                                    
         BE    GETEOP10                                                         
         MVI   2(R6),3             ENTERPRISE                                   
         CLI   TWATRFMT,C'K'                                                    
         BE    GETEOP10                                                         
         CLI   TWATRFMT,C'N'                                                    
         BE    GETEOP10                                                         
         CLI   TWATRFMT,C'Z'       VSS                                          
         BE    GETEOP10                                                         
         CLI   TWATRFMT,C'X'       VSS/IBS                                      
         BE    GETEOP10                                                         
         CLI   TWATRFMT,C'H'       ENTERPRISE II                                
         BE    GETEOP10                                                         
         MVI   2(R6),4             COLUMBINE                                    
         CLI   TWATRFMT,C'C'                                                    
         BE    GETEOP10                                                         
         MVI   2(R6),5             VCI                                          
         CLI   TWATRFMT,C'S'                                                    
         BNE   GETEOPX                                                          
         DROP  RF                                                               
*                                                                               
GETEOP10 DS    0H                                                               
         MVC   3(L'REOPKSTA,R6),RCONKSTA   STATION+MEDIA                        
                                                                                
         ZICM  RF,WORK+5,3                                                      
         ZIC   R1,WORK+4                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R6),0(RF)       OFFICE/SALESPERSON                           
                                                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(L'REOPKEY),KEYSAVE                                           
         BNE   GETEOPX                                                          
                                                                                
         L     R6,AIO4             USE IO4                                      
                                                                                
         GOTO1 VGETREC,DMCB,(R6)                                                
         USING REOPREC,R6                                                       
         ZICM  RF,WORK+1,3                                                      
         MVC   0(6,RF),REOPEQUV                                                 
                                                                                
GETEOPX  DS    0H                                                               
         XMOD1                                                                  
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*        GAGY --- GET AGENCY NAME AND ADDRESS                                   
*********************************************************************           
T80263   CSECT                                                                  
GAGY     NMOD1 0,**GAGY**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R4,AIO                                                           
         USING RCONKEY,R4                                                       
*                                                                               
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   0(R5),X'0A'                                                      
         MVC   19(4,R5),RCONKAGY     AGENCY                                     
         MVC   23(2,R5),RCONKAOF     OFFICE                                     
         MVC   25(2,R5),REPALPHA     REPALPHA                                   
         MVI   ION,3                                                            
*                                                                               
GAGY10   GOTO1 AHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GAGY20                                                           
*        CLC   KEY(25),KEYSAVE                                                  
*        BE    GAGY15                                                           
         DC    H'0'                                                             
*AGY15   MVC   KEY+25(2),=C'ZZ'                                                 
*        XC    KEY+27(5),KEY+27                                                 
*        B     GAGY10                                                           
*                                                                               
GAGY20   DS    0H                                                               
         GOTO1 AGETREC                                                          
         L     R6,AIO3                                                          
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         USING RAGYELEM,R6                                                      
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   TWAAGNM1,RAGYNAM1   AGENCY NAME FOR SCREEN (20)                  
         MVC   TWAAGNM2,RAGYNAM2   AGENCY NAME FOR CONTRACTS (33)               
         XC    TWAAGAD1,TWAAGAD1                                                
         XC    TWAAGAD2,TWAAGAD2                                                
         XC    TWAAGAD3,TWAAGAD3                                                
         MVC   TWAAGAD1(20),RAGYADD1   ADDRESS LINE 1                           
         MVC   TWAAGAD3(20),RAGYCITY   CITY                                     
         MVC   TWAAGSTT,RAGYSTAT   STATE                                        
         MVC   TWAAGZIP,RAGYZIP    ZIP                                          
*                                                                               
         TM    RAGYFLAG,X'80'           EXPANDED ADDRESS                        
         BZ    GAGY50                                                           
         DROP  RF                                                               
*                                                                               
         LA    R5,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   0(R5),X'1A'                                                      
         MVC   19(4,R5),RCONKAGY     AGENCY                                     
         MVC   23(2,R5),RCONKAOF     OFFICE                                     
         MVC   25(2,R5),REPALPHA     REPALPHA                                   
         MVI   ION,3                                                            
         DROP  R6                                                               
*                                                                               
         GOTO1 AHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GAGY50                                                           
*                                                                               
         GOTO1 AGETREC                                                          
*                                                                               
*  SAVE OFF AGENCY COMMENT                                                      
*                                                                               
         ICM   R2,15,ASVACMT         ESTABLISH AGY CMT ADDRESS                  
         XC    0(L'SVACMT,R2),0(R2)  CLEAR OUT                                  
         L     R6,AIO3                                                          
         MVI   ELCODE,X'40'          1ST AGY COMMENT                            
         BRAS  RE,GETEL                                                         
         BNE   GAGY30                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),2(R6)         SAVE 1ST COMMENT                           
         BRAS  RE,NEXTEL                                                        
         BNE   GAGY30                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   60(0,R2),2(R6)        SAVE 2ND COMMENT                           
*                                                                               
GAGY30   L     R6,AIO3                                                          
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GAGY50                                                           
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   TWAAGAD1,02(R6)     ADDRESS LINE 1                               
         MVC   TWAAGAD2(34),36(R6) ADDRESS LINE 2                               
*                                                                               
* NOTE: THE NEXT 3 LINES ARE TO FIX THE PROBLEM OF THE CITY BEING               
*       DUPLICATED IN THE 2ND ADDRESS FIELD IN PETRY CONVERTED RECS             
         CLC   TWAAGAD3(20),TWAAGAD2                                            
         BNE   *+10                                                             
         XC    TWAAGAD3,TWAAGAD3                                                
*                                                                               
GAGY50   DS    0H                                                               
*                                                                               
*          FLOAT STATE AND ZIP                                                  
         LA    R1,TWAAGAD3                                                      
         LA    R2,L'TWAAGAD3-1(R1)                                              
GAGY60   CR    R2,R1                                                            
         BL    GAGY70                                                           
         OI    0(R2),X'40'                                                      
         CLI   0(R2),X'40'                                                      
         BNE   GAGY70                                                           
         BCT   R2,GAGY60                                                        
GAGY70   MVC   3(2,R2),TWAAGSTT                                                 
         MVC   7(10,R2),TWAAGZIP                                                
*                                                                               
         OC    TWAAGAD2,SPACES                                                  
         CLC   TWAAGAD2,SPACES                                                  
         BNE   GAGY80                                                           
         MVC   TWAAGAD2,TWAAGAD3                                                
         XC    TWAAGAD3,TWAAGAD3                                                
*                                                                               
GAGY80   MVC   P,SPACES                                                         
         XMOD1                                                                  
         DROP  R4,RF                                                            
         EJECT                                                                  
*********************************************************************           
*     ORD CMT  --- DISPLAY CONTRACT ORDER COMMENT                               
*********************************************************************           
ORDCMT   NMOD1 0,*ORDCMT*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         OI    TWAWSFLG,X'40'      ORD CMT BEING PRINTED NOW                    
*                                                                               
         L     R4,ASVRCOC          REP CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         SPACE 1                                                                
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    OC1D                                                             
         MVC   P+6(19),=C'*REP ORDER COMMENT*'                                  
*                                                                               
OC1B     OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    OC1C                                                             
*                                                                               
         DROP  RF                                                               
         GOTO1 =A(PSTCMT),DMCB,(3,0(R4)),(RC),RR=Y PRINT ORD CMTS               
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,OC1B                                                          
*                                                                               
OC1C     DS    0H                                                               
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
         SPACE 1                                                                
*                                                                               
OC1D     LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         L     R4,ASVSCOC          STA CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    OC1G                                                             
         MVC   P+6(23),=C'*STATION ORDER COMMENT*'                              
*                                                                               
OC1E     OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    OC1F                                                             
*                                                                               
         DROP  RF                                                               
         MVC   P+30(60),0(R4)                                                   
         OC    P+30(60),SPACES                                                  
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,OC1E                                                          
*                                                                               
OC1F     DS    0H                                                               
         GOTO1 =A(GOSPOOL),DMCB,(RC),(R7),RR=Y                                  
*                                                                               
OC1G     DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'20'                                             
         DROP  RF                                                               
         XMOD1                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*     MODCODES --- DISPLAY GLOSSARY OF MOD CODES                                
*********************************************************************           
MODCODES NMOD1 0,*CODES*                                                        
         L     RC,0(R1)                                                         
         BAS   RE,MCSPOOL                                                       
         MVI   ALLOWLIN,4                                                       
         CLC   SENDID,=X'0406'     CLASS G REPORT?                              
         BE    MC010               YES - CONDENSED CODE LIST                    
         MVC   P+02(07),=C'A=ADDED'                                             
         MVC   P+34(15),=C'L=LENGTH CHANGE'                                     
         MVC   P+66(19),=C'S=SPOTS/WEEK CHANGE'                                 
         MVC   P2+02(11),=C'C=CANCELLED'                                        
         MVC   P2+34(10),=C'M=MAKEGOOD'                                         
         MVC   P2+66(13),=C'T=TIME CHANGE'                                      
         MVC   P3+02(12),=C'D=DAY CHANGE'                                       
         MVC   P3+34(30),=C'P=PLAN,SECTION OR CLASS CHANGE'                     
         MVC   P3+66(24),=C'Z=COMMENT/PROGRAM CHANGE'                           
         MVC   P4+02(24),=C'E=EFFECTIVE DATES CHANGE'                           
         MVC   P4+34(13),=C'R=RATE CHANGE'                                      
         MVC   P4+66(28),=C'*=MORE THAN TWO CHANGE CODES'                       
         BAS   RE,MCSPOOL                                                       
         MVC   P+02(22),=C'O=ORDER COMMENT CHANGE'                              
         B     MC020                                                            
MC010    DS    0H                                                               
         MVC   P+00(07),=C'A=ADDED'                                             
         MVC   P+26(15),=C'L=LENGTH CHANGE'                                     
         MVC   P+58(19),=C'S=SPOTS/WEEK CHANGE'                                 
         MVC   P2+00(11),=C'C=CANCELLED'                                        
         MVC   P2+26(10),=C'M=MAKEGOOD'                                         
         MVC   P2+58(13),=C'T=TIME CHANGE'                                      
         MVC   P3+00(12),=C'D=DAY CHANGE'                                       
         MVC   P3+26(30),=C'P=PLAN,SECTION OR CLASS CHANGE'                     
         MVC   P3+58(16),=C'Z=COMMENT CHANGE'                                   
         MVC   P4+00(24),=C'E=EFFECTIVE DATES CHANGE'                           
         MVC   P4+26(13),=C'R=RATE CHANGE'                                      
         MVC   P4+58(21),=C'*=MORE THAN TWO CODES'                              
         BAS   RE,MCSPOOL                                                       
         MVC   P+00(22),=C'O=ORDER COMMENT CHANGE'                              
MC020    DS    0H                                                               
         BAS   RE,MCSPOOL                                                       
         BAS   RE,MCSPOOL                                                       
         XMOD1                                                                  
*********************************************************************           
*        MGSPOOL ---   ROUTINE TO HANDLE SPOOL INTERFACE FOR MAKEGOOD           
*********************************************************************           
MCSPOOL  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PROCESS MAKEGOOD OFFERS                                                       
***********************************************************************         
DOMKGS   NMOD1 DOMKGSX-DOMKGSD,*DOMKGS*                                         
         LR    R9,RC                                                            
         USING DOMKGSD,R9                                                       
         L     RC,0(R1)                                                         
         L     R7,4(R1)                                                         
                                                                                
         LA    R5,RCONREC          NEED THIS TO GET PROPER                      
         USING RCONREC,R5          ADDRESSIBILITY TO THE CONTRACT REC           
                                                                                
* DON'T PASS DELETES                                                            
         NI    DMINBTS,X'FF'-X'08'                                              
         XC    KEY,KEY                                                          
KEYD     USING RMKGKEY,KEY                                                      
         MVI   KEYD.RMKGKTYP,X'11'                                              
         MVC   KEYD.RMKGKREP,REPALPHA                                           
         MVC   KEYD.RMKGKOFF,RCONKOFF                                           
         MVC   KEYD.RMKGKSTA,RCONKSTA                                           
         DROP  R5                                                               
                                                                                
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   KEYD.RMKGKCON,TWACNUM                                            
         DROP  RF                                                               
                                                                                
         GOTO1 AHIGH                                                            
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   DOMKGX                                                           
*                                                                               
* TAKEOVER DID NOT PULL THE MAKEGOOD RECORDS OVER CORRECTLY, SKIP               
*                                                                               
         OC    KEYD.RMKGKPLN(6),KEYD.RMKGKPLN                                   
         BNZ   DOMKGX              MUST BE GROUP RECORD                         
         DROP  KEYD                                                             
                                                                                
         OI    MKGSTAT,X'80'       SET TO PRINT HEADER LINES                    
                                                                                
         CLC   =C'MGS',CONACT                                                   
         BNE   DOMKG10                                                          
*                                                                               
* ALWAYS SHOW HEADER FOR MGS                                                    
*                                                                               
         MVC   P(34),=C'*** START OF MAKEGOOD OFFER(S) ***'                     
         BRAS  RE,MGSPOOL                                                       
                                                                                
         NI    MKGSTAT,X'FF'-X'80'                                              
*                                                                               
DOMKG10  DS    0H                                                               
***>     LA    R6,SCPAGES          GET PAGE HEADING FOR MAKEGOOD OFFERS         
***>G20  CLC   FORMAT,0(R6)        COMPARE FORMAT                               
***>     BNE   DOMKG30                                                          
***>     MVC   RCSUBPRG,1(R6)                                                   
***>     B     DOMKG40                                                          
***>G30  LA    R6,L'SCPAGES(R6)    KEEP BUMPING TABLE                           
***>     CLI   0(R6),0             END OF TABLE??                               
***>     BNE   DOMKG20                                                          
*                                                                               
* OVERALL GROUP COMMENTS?                                                       
*                                                                               
DOMKG40  DS    0H                                                               
         MVI   ION,3                                                            
         GOTO1 AGETREC                                                          
*                                                                               
* DISPLAY GROUP CODE                                                            
*                                                                               
         LA    R5,RCONREC                                                       
         USING RCONREC,R5                                                       
*                                                                               
         L     R6,AIO3                                                          
         USING RMKGREC,R6                                                       
*                                                                               
* IF OFFER IS APPLIED                                                           
* SKIP IT IF IT WAS APPLIED PREVIOUS TO CURRENT MOD NUM                         
*                                                                               
         TM    RMKGSCST,RMKGSAPQ                                                
         BZ    DOMKG50                                                          
*                                                                               
         CLC   RCONMOD,RMKGAPMN                                                 
         BH    DOMKG70                                                          
         DROP  R5,R6                                                            
*                                                                               
* NEWLY CREATED OFFERS THAT HAVE NOT BEEN MGS'ED SHOULD NOT APPEAR ON           
* THE WORKSHEET OF THE OPPOSITE PARTY                                           
*                                                                               
DOMKG50  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'21'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DOMKG80                                                          
*                                                                               
         LR    R5,R6                                                            
         USING RCONMGEL,R5                                                      
*                                                                               
         L     R6,AIO3                                                          
         USING RMKGREC,R6                                                       
*                                                                               
         TM    RMKGSCST,RMKGSCRQ                                                
         BZ    DOMKG60             REP IS OFFERER                               
         CLI   TWAACCS,C'$'        STATION IS DOING THE LIST                    
         BNE   DOMKG80                                                          
*                                                                               
         CLC   RMKGSCRD,RCONRMDT   IF CREATION DATE/TIME IS LATER               
         BH    DOMKG70             THAN LAST REP MGS DATE/TIME                  
         BL    DOMKG80             DON'T SHOW IN LIST                           
         CLC   RMKGSCRT,RCONRMTM                                                
         BH    DOMKG70                                                          
         B     DOMKG80                                                          
*                                                                               
DOMKG60 DS     0H                  STATION IS OFFERER                           
         CLI   TWAACCS,C'$'                                                     
         BE    DOMKG80                                                          
*                                                                               
         CLC   RMKGSCRD,RCONMGDT   IF CREATION DATE/TIME IS LATER               
         BH    DOMKG70             THAN LAST STATION MGS DATE/TIME              
         BL    DOMKG80             DON'T SHOW IN LIST                           
         CLC   RMKGSCRT,RCONMGTM                                                
         BL    DOMKG80                                                          
         DROP  R5                                                               
*                                                                               
DOMKG70  DS    0H                  SKIP TO NEXT GROUP, IF ANY                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 ASEQ                                                             
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BE    DOMKG70                                                          
*                                                                               
* IF NEXT GROUP IS MISSING GROUP RECORD SKIP IT                                 
*                                                                               
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   DOMKG170                                                         
         OC    KEY+21(6),KEY+21                                                 
         BNZ   DOMKG70                                                          
*                                                                               
         B     DOMKG170                                                         
                                                                                
DOMKG80  DS    0H                                                               
         TM    MKGSTAT,X'80'                                                    
         BZ    DOMKG90                                                          
*                                                                               
         MVC   P(34),=C'*** START OF MAKEGOOD OFFER(S) ***'                     
         BRAS  RE,MGSPOOL                                                       
         NI    MKGSTAT,X'FF'-X'80'                                              
*                                                                               
DOMKG90  DS    0H                                                               
         OI    MKGSTAT,X'40'       MAKEGOOD PRINTED                             
*                                                                               
         GOTO1 =A(MGOTOTAL),RR=RELO CALCULATE MG MISSED/OFFER TOTALS            
         GOTO1 =A(GETDEMO),RR=RELO  GET AGENCY DEMO CATEGORY                    
*                                                                               
         L     R6,AIO3                                                          
         USING RMKGREC,R6                                                       
         MVC   MKGTYPE,RMKGRTS                                                  
*                                                                               
         MVC   P(8),=C'GROUP  :'                                                
         MVC   P+9(2),RMKGKGR1                                                  
*                                                                               
         MVC   P+35(7),=C'MISSED:'                                              
         EDIT  MISSTOT$,(12,P+43),2,FLOAT=$,ZERO=NOBLANK                        
*                                                                               
         MVC   P+57(6),=C'SPOTS:'                                               
         EDIT  MISSTOT#,(4,P+64),ZERO=NOBLANK                                   
*                                                                               
         MVC   P+70(5),=C'GRPS:'                                                
         EDIT  MISSGRPS,(8,P+77),1,ZERO=NOBLANK                                 
*                                                                               
         MVC   P+89(11),=C'CREATED BY:'                                         
         MVC   P+101(3),=C'REP'                                                 
         TM    RMKGSCST,RMKGSCRQ                                                
         BO    *+10                                                             
         MVC   P+101(3),=C'STA'                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(2,RMKGSCRD),(10,P+105)                              
*                                                                               
         BRAS  RE,MGSPOOL                                                       
*                                                                               
         MVC   P(8),=C'STATUS :'                                                
         MVC   P+9(3),=C'NEW'                                                   
*                                                                               
         OC    RMKGSFG1,RMKGSFG1                                                
         BZ    DOMKG104                                                         
         MVC   P+9(4),=C'SENT'                                                  
         TM    RMKGSFG1,RMGF1MSN                                                
         BO    DOMKG110                                                         
         MVC   P+9(5),=C'ERROR'                                                 
         TM    RMKGSFG1,RMGF1MER                                                
         BO    DOMKG110                                                         
         MVC   P+9(6),=C'RESENT'                                                
         TM    RMKGSFG1,RMGF1MCR                                                
         BO    DOMKG110                                                         
*                                                                               
         MVC   P+9(7),=C'APPLIED'                                               
         TM    RMKGSFG1,RMGF1MCF                                                
         BZ    DOMKG103                                                         
         TM    RMKGSFG3,RMGF3SAQ                                                
         BZ    DOMKG110                                                         
         TM    RMKGSFG3,RMGF3ARQ                                                
         BO    DOMKG110                                                         
         MVC   P+9(7),=C'SELFAPP'                                               
*                                                                               
         LA    R5,RCONREC                                                       
         USING RCONREC,R5                                                       
         CLC   RMKGAPMN,RCONMOD                                                 
         BE    DOMKG110                                                         
         MVI   P+10,C'*'                                                        
         B     DOMKG110                                                         
         DROP  R5                                                               
*                                                                               
DOMKG103 DS    0H                                                               
         MVC   P+9(8),=C'RECALLED'                                              
         TM    RMKGSFG1,RMGF1MCM                                                
         BO    DOMKG110                                                         
         MVC   P+9(8),=C'APPROVED'                                              
         TM    RMKGSFG1,RMGF1MAR                                                
         BO    DOMKG110                                                         
         MVC   P+9(8),=C'REJECTED'                                              
         TM    RMKGSFG1,RMGF1MRR                                                
         BO    DOMKG110                                                         
         MVC   P+9(9),=C'CANCELLED'                                             
         B     DOMKG110                                                         
*                                                                               
DOMKG104 DS    0H                                                               
         TM    RMKGSCST,RMKGSAPQ                                                
         BZ    *+14                                                             
         MVC   P+9(7),=C'APPLIED'                                               
         B     DOMKG110                                                         
                                                                                
         TM    RMKGSCST,RMKGSBOQ                                                
         BZ    *+14                                                             
         MVC   P+9(10),=C'BACKED-OUT'                                           
         B     DOMKG110                                                         
                                                                                
         TM    RMKGSCST,RMKGSRCQ                                                
         BZ    *+14                                                             
         MVC   P+9(8),=C'RECALLED'                                              
         B     DOMKG110                                                         
                                                                                
         TM    RMKGSCST,RMKGSRJQ                                                
         BZ    DOMKG105                                                         
         MVC   P+9(8),=C'REJECTED'                                              
         TM    RMKGSFG1,RMGF1MCN   SET IF DARE AND CANCELLED TO AGY             
         BZ    DOMKG110                                                         
         MVC   P+9(8),=C'DARECAND'                                              
         B     DOMKG110                                                         
                                                                                
DOMKG105 DS    0H                                                               
         TM    RMKGSCST,RMKGSRVQ                                                
         BZ    *+10                                                             
         MVC   P+9(7),=C'REVISED'                                               
*                                                                               
DOMKG110 DS    0H                                                               
         MVC   P+34(8),=C'OFFERED:'                                             
         EDIT  OFFRTOT$,(12,P+43),2,FLOAT=$,ZERO=NOBLANK                        
*                                                                               
         MVC   P+57(6),=C'SPOTS:'                                               
         EDIT  OFFRTOT#,(4,P+64),ZERO=NOBLANK                                   
*                                                                               
         MVC   P+70(5),=C'GRPS:'                                                
         EDIT  OFFRGRPS,(8,P+77),1,ZERO=NOBLANK                                 
*                                                                               
*        MVC   P+89(16),=C'LAST ACTIVITY  :'                                    
*        OC    RMKGSLAD,RMKGSLAD                                                
*        BZ    DOMKG120                                                         
*        GOTO1 DATCON,DMCB,(2,RMKGSLAD),(5,P+92)                                
*                                                                               
DOMKG120 DS    0H                                                               
*        MVC   P+43(9),=C'ASSUME OK'                                            
*        TM    RMKGSCST,RMKGSPAQ                                                
*        BZ    *+10                                                             
*        MVC   P+43(13),=C'PLEASE ADVISE'                                       
*                                                                               
         MVC   P+89(15),=C'LAST CHANGED BY'                                     
         MVC   P+105(3),=C'REP'                                                 
         TM    RMKGSFG2,RMGF2STQ                                                
         BZ    *+10                                                             
         MVC   P+105(3),=C'STA'                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(2,RMKGSLAD),(10,P+109)                              
         OC    RMKGSLAD,RMKGSLAD                                                
         BNZ   DOMKG100                                                         
         GOTO1 DATCON,DMCB,(2,RMKGSCRD),(10,P+109)                              
*                                                                               
DOMKG100 DS    0H                                                               
*                                                                               
         BRAS  RE,MGSPOOL                                                       
*                                                                               
         MVC   P+37(5),=C'DIFF:'                                                
         L     RF,OFFRTOT$                                                      
         S     RF,MISSTOT$                                                      
         EDIT  (RF),(12,P+43),2,FLOAT=-,ZERO=NOBLANK                            
*                                                                               
         LA    R1,P+43           FIND FIRST NON-BLANK                           
*                                                                               
         CLI   0(R1),C' '                                                       
         BH    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
*                                                                               
         BCTR  R1,0                BACKUP A POSITION                            
         MVI   0(R1),C'$'          ADD DOLLAR SIGN                              
*                                                                               
         MVC   P+57(6),=C'SPOTS:'                                               
         L     RF,OFFRTOT#                                                      
         S     RF,MISSTOT#                                                      
         EDIT  (RF),(4,P+64),ZERO=NOBLANK,FLOAT=-                               
*                                                                               
         MVC   P+70(5),=C'GRPS:'                                                
         L     RF,OFFRGRPS                                                      
         S     RF,MISSGRPS                                                      
         EDIT  (RF),(8,P+77),1,ZERO=NOBLANK,FLOAT=-                             
*                                                                               
         BRAS  RE,MGSPOOL                                                       
*                                                                               
         DROP  R6                                                               
*                                                                               
* DISPLAY OVERALL GROUP COMMENTS, IF ANY                                        
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DOMKG130                                                         
         USING RMKGGCEM,R6                                                      
         MVC   P(8),=C'GRP CMT:'                                                
*                                                                               
DOMKG125 DS    0H                                                               
         ZIC   R1,RMKGGCLN                                                      
         SH    R1,=H'3'            OVERHEAD                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+9(0),RMKGGCCM                                                  
         BRAS  RE,MGSPOOL                                                       
         BRAS  RE,NEXTEL                                                        
         BE    DOMKG125                                                         
         DROP  R6                                                               
*                                                                               
* DISPLAY MISSED LINE COMMENT                                                   
DOMKG130 DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 ASEQ                                                             
*                                                                               
* TAKEOVER DID NOT PULL THE MAKEGOOD RECORDS OVER CORRECTLY, SKIP               
*                                                                               
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   DOMKG170                                                         
*                                                                               
         MVI   ION,3                                                            
         GOTO1 AGETREC                                                          
                                                                                
         L     R6,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DOMKG140                                                         
                                                                                
         USING RMKGCDEL,R6                                                      
         CLI   RMKGCDLN,RMKGCDDS-RMKGCDEL                                       
         BNH   DOMKG140                                                         
                                                                                
         ZIC   R1,RMKGCDLN                                                      
         SH    R1,=H'11'           OVERHEAD                                     
         BM    DOMKG140                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+9(0),RMKGCDDS                                                  
         MVC   P(8),=C'MSL CMT:'                                                
         BRAS  RE,MGSPOOL                                                       
         DROP  R6                                                               
*                                                                               
DOMKG140 DS    0H                                                               
         MVI   MKGINDEX,1                                                       
*                                                                               
DOMKG150 DS    0H                                                               
         L     R6,AIO3                                                          
         USING RMKGREC,R6                                                       
*                                                                               
         MVC   MKGTYPE,RMKGRTS                                                  
*                                                                               
         TM    RMKGRTS,X'10'+X'08'+X'04'                                        
         BNZ   DOMKG153                                                         
*                                                                               
         ZIC   RF,RMKGKRTY         CHECK RECORD NUMBER                          
         SLL   RF,28               DROP ALL BUT LAST 4 BITS                     
         SRL   RF,28               RESTORE                                      
         CH    RF,=H'1'            FIRST LINE OF POSSIBLE SET? (0/1)            
         BH    DOMKG160            YES - DISPLAY MISSED DATA                    
*                                                                               
DOMKG153 DS    0H                                                               
         TM    RMKGRTS,X'20'       BONUS OFFERS DOESN'T HAVE MISSED             
         BZ    DOMKG155            DATE ELEMENT                                 
         MVC   P(8),=C'BONUS  :'                                                
         BRAS  RE,MGSPOOL                                                       
*                                                                               
         CLI   MKGINDEX,1          PRINT ONLY ONCE                              
         BH    DOMKG160                                                         
         LA    R3,P                                                             
         USING PMBUY,R3                                                         
         MVC   PMBBUYNM,=C'LN#'                                                 
         MVC   PMBDATE(7),=C'DATE(S)'                                           
         MVC   PMBNPW-2(5),=C'SP/WK'                                            
         MVC   PMBDYTM(8),=C'DAY/TIME'                                          
         MVC   PMBLEN(3),=C'LEN'                                                
         MVC   PMBCOST+6(4),=C'COST'                                            
         MVC   PMBPROG(7),=C'PROGRAM'                                           
         MVC   PMBDEMO,DISADEMO                                                 
         MVC   PMBGRPS,=C'  GRPS'                                               
         BRAS  RE,MGSPOOL                                                       
*                                                                               
         B     DOMKG160                                                         
         DROP  R3,R6                                                            
*                                                                               
DOMKG155 DS    0H                                                               
         BAS   RE,MISSBUY          DISPLAY INFO FROM MISSED BUYLINE             
*                                                                               
         TM    MKGTYPE,X'10'       PREEMPTED?                                   
         BO    DOMKG165                                                         
         MVC   P(8),=C'OFFERED:'                                                
         BRAS  RE,MGSPOOL                                                       
         TM    MKGTYPE,X'08'+X'04'                                              
         BNZ   DOMKG160                                                         
         MVI   MKGINDEX,1                                                       
*                                                                               
DOMKG160 DS    0H                                                               
         BAS   RE,MGFORMAT                                                      
*                                                                               
DOMKG165 DS    0H                                                               
****     BRAS  RE,MGPGMNAM                                                      
****     BRAS  RE,MGDEMO                                                        
****     BRAS  RE,MGSPOOL                                                       
         BRAS  RE,MGDETCMT                                                      
         BRAS  RE,MGSPOOL                                                       
*                                                                               
         ZIC   R1,MKGINDEX                                                      
         AHI   R1,1                                                             
         STC   R1,MKGINDEX                                                      
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 ASEQ                                                             
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   DOMKG170                                                         
         MVI   ION,3                                                            
         GOTO1 AGETREC                                                          
         B     DOMKG150                                                         
*                                                                               
* BREAK TO NEXT GROUP, IF ANY                                                   
*                                                                               
DOMKG170 DS    0H                                                               
         CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE                                    
         BNE   DOMKG180                                                         
         TM    MKGSTAT,X'40'       WERE ANY MAKEGOOD PRINTED?                   
         BZ    DOMKG40                                                          
         MVC   P,=132C'-'          YES, PRINT SEPARATOR                         
         BRAS  RE,MGSPOOL                                                       
         NI    MKGSTAT,X'FF'-X'40'                                              
         B     DOMKG40                                                          
                                                                                
DOMKG180 DS    0H                                                               
         TM    MKGSTAT,X'80'       PRINT TRAILER IF HEADER WAS PRINTED          
         BO    DOMKGX                                                           
                                                                                
         MVC   P(32),=C'*** END OF MAKEGOOD OFFER(S) ***'                       
         BRAS  RE,MGSPOOL                                                       
         BRAS  RE,MGSPOOL                                                       
                                                                                
DOMKGX   DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY INFO FROM MISSED BUYLINE                                              
***********************************************************************         
MISSBUY  NTR1                                                                   
         XC    MKGDMSDT,MKGDMSDT   CLEAR POINTERS TO MISSED DATE                
         MVC   SVKEY,KEY           SAVE OFF KEY TO RE-ESTABLISHED SEQ           
*                                  ORDER LATER                                  
         L     R6,AIO3                                                          
         USING RMKGMGEL,R6                                                      
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MISB005  DS    0H                                                               
         XC    KEY,KEY             CONSTRUCT TARGET MISSED BUYLINE KEY          
MGD      USING RBUYKEY,KEY                                                      
         MVI   MGD.RBUYKTYP,X'0B'                                               
         MVC   MGD.RBUYKREP,REPALPHA                                            
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   MGD.RBUYKCON,TWACNUM                                             
         DROP  RF                                                               
*                                                                               
         MVI   ION,2                                                            
         GOTO1 AHIGH                                                            
*                                                                               
MISB010  DS    0H                                                               
         CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   MISSBUYX                                                         
*                                                                               
         CLC   MGD.RBUYKLIN,RMKGMGLI                                            
         BE    MISB020             INCASE WE HAVE MAKEGOOD FOR MAKEGOOD         
         GOTO1 ASEQ                                                             
         B     MISB010                                                          
         DROP  MGD,R6                                                           
*                                                                               
MISB020  DS    0H                                                               
         GOTO1 AGETREC                                                          
*                                                                               
MISB025  DS    0H                                                               
         LA    R3,P                                                             
         USING PMBUY,R3                                                         
*                                                                               
         OC    MKGDMSDT,MKGDMSDT                                                
         BNZ   MISB030             HEADER ONLY PRINTS ONCE                      
*                                                                               
         MVC   PMBHEAD(8),=C'MISSED :'                                          
*                                                                               
         TM    MKGTYPE,X'10'+X'08'+X'04'                                        
         BZ    MISB028                                                          
*                                                                               
         TM    MKGTYPE,X'08'+X'04' LATE RUN                                     
         BZ    MISB026                                                          
         CLI   MKGINDEX,1          PRINT ONLY ONCE                              
         BE    MISB028                                                          
         B     MISB029                                                          
*                                                                               
MISB026  DS    0H                  PREEMPT                                      
         MVC   PMBHEAD(8),=8C' '                                                
         CLI   MKGINDEX,1          PRINT ONLY ONCE                              
         BH    MISB030                                                          
         MVC   PMBHEAD(10),=C'PREEMPTED:'                                       
*                                                                               
MISB028  DS    0H                                                               
         BRAS  RE,MGSPOOL                                                       
         MVC   PMBBUYNM,=C'LN#'                                                 
         MVC   PMBDATE(7),=C'DATE(S)'                                           
         MVC   PMBNPW-2(5),=C'SP/WK'                                            
         MVC   PMBDYTM(8),=C'DAY/TIME'                                          
         MVC   PMBLEN(3),=C'LEN'                                                
         MVC   PMBCOST+6(4),=C'COST'                                            
         MVC   PMBPROG(7),=C'PROGRAM'                                           
         MVC   PMBDEMO,DISADEMO                                                 
         MVC   PMBGRPS,=C'  GRPS'                                               
*                                                                               
MISB029  DS    0H                                                               
         BRAS  RE,MGSPOOL                                                       
*                                                                               
* MISSED BUY NPW/LEN/COST                                                       
MISB030  DS    0H                                                               
*                                                                               
         ST    R6,MKGDMSDT         SAVE DATE ELEMENT ADDRESS                    
*                                                                               
         L     R6,AIO2                                                          
         USING RBUYREC,R6                                                       
*                                                                               
* REPLACEMENT INDICATOR                                                         
*                                                                               
         L     R1,AIO3                                                          
MGD      USING RMKGREC,R1                                                       
         TM    MGD.RMKGRTS,X'02'                                                
         BZ    *+10                                                             
         MVC   PMBHEAD(2),=C'NA'                                                
         DROP  MGD                                                              
*                                                                               
* LENGTH                                                                        
         MVC   HALF,RBUYDUR                                                     
                                                                                
         NI    HALF,X'7F'                                                       
         EDIT  (2,HALF),(3,PMBLEN)                                              
*                                  BECOMES INPUT FOR REMAINING                  
*                                  COMBO CONTRACTS                              
         LA    RF,PMBLEN                                                        
         LR    RE,R0               ADD LENGTH OF EDITED FIELD                   
         AR    RE,RF                                                            
         TM    RBUYDUR,X'80'       MINUTES?                                     
         BZ    *+8                                                              
         MVI   0(RE),C'M'                                                       
* COST                                                                          
         EDIT  RBUYCOS,(10,PMBCOST),2                                           
*                                                                               
*        PRINT PROGRAM NAME                                                     
*                                                                               
         BRAS  RE,MBPGMNAM         PRINT PROGRAM NAME                           
*                                                                               
*        PRINT DEMOS AND GRPS                                                   
*                                                                               
         BRAS  RE,MBDEMO           PRINT DEMOS AND GRPS                         
*                                                                               
         DROP  R6                                                               
*                                                                               
* AGENCY DEMOS IF ANY                                                           
*&&DO                                                                           
         MVI   ELCODE,X'0D'                                                     
         BRAS  RE,GETEL                                                         
         BNE   MISB035                                                          
         USING RBUYDMEL,R6                                                      
         ICM   R4,15,RBUYDMDM                                                   
         SLL   R4,4                SHIFT TO ADD SIGN                            
         STCM  R4,15,FULL                                                       
         OI    FULL+3,X'0C'        ADD SIGN                                     
         EDIT  (P4,FULL),(8,PMBDEMO),1,ALIGN=LEFT                               
         DROP  R6                                                               
*&&                                                                             
* MISSED DATE(SPOT)                                                             
*                                                                               
MISB035  DS    0H                                                               
         OC    MKGDMSDT,MKGDMSDT                                                
         BNZ   MISB040                                                          
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R6,MKGDMSDT         STORE POINTER TO MISSED DATE ELEM            
*                                                                               
MISB040  DS    0H                                                               
         L     R6,MKGDMSDT                                                      
         USING RMKGMGEL,R6                                                      
*                                                                               
         EDIT  MKGINDEX,(3,PMBINDEX)                                            
*                                                                               
         TM    MKGTYPE,X'10'+X'08'+X'04'                                        
         BNZ   MISB045                                                          
         ZIC   R1,MKGINDEX                                                      
         AHI   R1,1                                                             
         STC   R1,MKGINDEX                                                      
*                                                                               
* MISSED LINE NUMBER                                                            
MISB045  DS    0H                                                               
         EDIT  RMKGMGLI,(3,PMBBUYNM)                                            
*                                                                               
         LA    R4,PMBDATE                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,RMKGMGD1),(4,(R4))                                
         AHI   R4,5                                                             
         OC    RMKGMGD2,RMKGMGD2                                                
         BZ    MISB050                                                          
         MVI   0(R4),C'-'          INSERT RANGE INDICATOR                       
         AHI   R4,1                                                             
         GOTO1 DATCON,DMCB,(3,RMKGMGD2),(4,(R4))                                
         AHI   R4,5                                                             
*                                                                               
* NPW                                                                           
MISB050  DS    0H                                                               
         EDIT  RMKGMGSP,(3,PMBNPW)                                              
         DROP  R6                                                               
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'02'                                                     
         USING RBUYDYEL,R6                                                      
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  DISPLAY DAY-TIME FIELDS                      
MISB070  EQU   *                                                                
         GOTO1 OUTDAY,DMCB,RBUYDAYS,RBUYDYIN,PMBDYTM                            
*                                  DISPLAY DAY-TIME FIELDS                      
         LA    R4,PMBDYTM          CHECK FOR LAST POSITION                      
         LA    RF,L'PMBDYTM        MAX FIELD SIZE                               
*                                                                               
MISB080  EQU   *                                                                
         CLI   0(R4),C' '          SPACE FOUND?                                 
         BE    MISB090             YES                                          
         LA    R4,1(R4)            GO BACK FOR NEXT                             
         BCT   RF,MISB080                                                       
         DC    H'0'                NOT FOUND - ERROR                            
*                                                                               
MISB090  EQU   *                                                                
         MVI   0(R4),C'/'          INSERT SEPARATOR                             
         LA    R4,1(R4)            BUMP TO NEXT POSITION                        
*                                  TIME                                         
         GOTO1 UNTIME,DMCB,RBUYDYT1,0(R4)                                       
*                                                                               
         BRAS  RE,NEXTEL           ANYMORE DAY/TIME ELM LEFT?                   
         BNE   MISB130                                                          
*                                                                               
MISB100  DS    0H                                                               
         CLI   0(R4),C' '          FIND END OF STRING                           
         BE    MISB110                                                          
         LA    R4,1(R4)                                                         
         B     MISB100                                                          
*                                                                               
MISB110  DS    0H                                                               
         MVI   0(R4),C'*'          SHOW WE'LL CONTINUE ON NEXT LINE             
         BRAS  RE,MGSPOOL                                                       
         B     MISB070                                                          
         DROP  R3,R6                                                            
*                                                                               
MISB130  DS    0H                                                               
         BRAS  RE,MGSPOOL                                                       
*                                                                               
         L     R6,MKGDMSDT         STORE POINTER TO MISSED DATE ELEM            
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   MISSBUYX                                                         
         USING RBUYMGEL,R6                                                      
*                                                                               
         ST    R6,MKGDMSDT         STORE POINTER TO MISSED DATE ELEM            
         L     R4,AIO2                                                          
         CLC   RBUYMGLI,26(R4)     IF SAME TARGET, NO NEED TO GETREC            
         BE    MISB025                                                          
         B     MISB005                                                          
         DROP  R6                                                               
*                                                                               
MISSBUYX DS    0H                  RE-ESTABLISH SEQ ORDER                       
         MVC   KEY(L'SVKEY),SVKEY                                               
         GOTO1 AHIGH                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* FORMAT MAKEGOOD OFFER                                                         
***********************************************************************         
MGFORMAT NTR1                                                                   
         LA    R3,P                                                             
         USING PMGD,R3                                                          
                                                                                
         L     R6,AIO3                                                          
         USING RMKGREC,R6                                                       
*                                                                               
         ZIC   RF,RMKGKRTY         CHECK RECORD NUMBER                          
         SLL   RF,28               DROP ALL BUT LAST 4 BITS                     
         SRL   RF,28               RESTORE                                      
         CH    RF,=H'1'            FIRST LINE OF POSSIBLE SET? (0/1)            
         BNH   MGFMT10             YES - NO FLAGS TO SET                        
*                                                                               
         TM    RMKGRTS,X'20'+X'10'+X'08'+X'04'                                  
         BNZ   MGFMT10                                                          
*                                  PROCESS ONLY FOR REGULAR MAKEGOOD            
         MVC   PMGHEAD,=C'AND'     NO  - INSERT 'AND' INDICATOR                 
         TM    RMKGKRTY,X'10'      'CHOICE' INDICATOR SET?                      
         BNO   MGFMT20             NO  - LEAVE 'AND' SET                        
         MVC   PMGHEAD,=C'OR '     YES - SET 'OR' INDICATOR                     
         B     MGFMT18                                                          
                                                                                
MGFMT10  DS    0H                                                               
         L     R6,AIO3                                                          
         USING RMKGREC,R6                                                       
         TM    RMKGKRTY,X'10'      'CHOICE' INDICATOR SET?                      
         BNO   MGFMT20             NO  - LEAVE 'AND' SET                        
         DROP  R6                                                               
                                                                                
MGFMT18  DS    0H                                                               
         MVI   ELCODE,X'20'        STATUS CONTROL ELEMENT                       
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING RMKGSTEL,R6                                                      
         TM    RMKGSTCH,X'01'      TAG WITH '*' IF SELECTED?                    
         BZ    MGFMT20                                                          
         MVI   PMGINDEX+3,C'*'                                                  
                                                                                
* OFFERED DATE(SPOT)                                                            
MGFMT20  DS    0H                                                               
         TM    MKGTYPE,X'20'       SKIP INCASE OF LATE RUN BONUS                
         BO    MGFMT25                                                          
         TM    MKGTYPE,X'08'+X'04' LATE RUN?                                    
         BNZ   MGFMT30                                                          
*                                                                               
MGFMT25  DS    0H                                                               
         EDIT  MKGINDEX,(3,PMGINDEX)                                            
*                                                                               
MGFMT30  DS    0H                                                               
         BRAS  RE,OFFRDATE                                                      
                                                                                
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
*  OFFRDATE:  RETRIEVE/DISPLAY OFFER DATE(S) ON SECOND LINE OF                  
*        DISPLAY.  INSERT # SPOTS/WEEK OFFERED.                                 
***********************************************************************         
OFFRDATE NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2                                                            
         SR    R5,R5                                                            
*                                                                               
         L     R6,AIO3                                                          
         USING RMKGREC,R6                                                       
*                                                                               
         LA    R3,P                                                             
         USING PMGD,R3                                                          
*                                                                               
         TM    RMKGRTS,X'10'       SKIP OFFER INFO IF PREEMPT                   
         BZ    OFDT0003                                                         
         MVC   PMGOFDAT(7),=C'PREEMPT'                                          
         BRAS  RE,MGSPOOL                                                       
         B     OFDTX                                                            
*                                                                               
OFDT0003 DS    0H                                                               
         TM    RMKGRTS,X'08'+X'04' LATE RUN                                     
         BZ    OFDT0004                                                         
         TM    RMKGRTS,X'20'       LATE RUN BONUS, SKIP                         
         BO    *+10                                                             
         MVC   PMGOFDAT(8),=C'LATE RUN'                                         
*                                                                               
OFDT0004 DS    0H                                                               
         TM    RMKGRTS,X'02'       REPLACEMENT OFFER?                           
         BZ    OFDT0005                                                         
         MVC   PMGOFDAT(2),=C'NA'                                               
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    OFDT0005                                                         
         MVC   PMGOFDAT(9),=C'CREDIT NA'                                        
         BRAS  RE,MGSPOOL                                                       
         B     OFDTX                                                            
*                                                                               
* NPW                                                                           
OFDT0005 DS    0H                                                               
         L     R6,AIO3                                                          
         EDIT  RMKGNW,(3,PMGNPW)                                                
* LENGTH                                                                        
         MVC   HALF,RMKGDUR                                                     
                                                                                
         NI    HALF,X'7F'                                                       
         EDIT  (2,HALF),(3,PMGLEN)                                              
*                                  BECOMES INPUT FOR REMAINING                  
*                                  COMBO CONTRACTS                              
         LA    RF,PMGLEN                                                        
         LR    RE,R0               ADD LENGTH OF EDITED FIELD                   
         AR    RE,RF                                                            
         TM    RMKGDUR,X'80'       MINUTES?                                     
         BZ    *+8                                                              
         MVI   0(RE),C'M'                                                       
* COST                                                                          
         EDIT  RMKGCOS,(10,PMGCOST),2                                           
*                                                                               
         BRAS  RE,MGPGMNAM         PRINT PROGRAM NAME                           
         BRAS  RE,MGDEMO           PRINT DEMOS AND GRPS                         
*                                                                               
         TM    RMKGRTS,X'20'       LATE RUN BONUS                               
         BO    OFDT0008            CONTINUE                                     
         TM    RMKGRTS,X'08'+X'04' SKIP DATE PRINT IF LATE RUN                  
         BNZ   OFDT0040                                                         
         DROP  R6                                                               
                                                                                
* DATE/DAY/TIME                                                                 
OFDT0008 DS    0H                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
OFDT0010 DS    0H                                                               
         LR    R5,R6                                                            
                                                                                
*        LA    R4,P                OUTPUT                                       
*        USING PMGD,R4                                                          
         LA    R4,PMGOFDAT                                                      
*        DROP  R4                                                               
*                                  DISPLAY DATES                                
         GOTO1 DATCON,DMCB,(3,2(R6)),(4,(R4))                                   
*                                  START DATE                                   
         CLC   2(3,R6),5(R6)                                                    
         BNE   *+12                                                             
         LA    R4,5(R4)                                                         
         B     OFDT0020                                                         
*                                                                               
         MVI   5(R4),C'-'                                                       
*                                                                               
         GOTO1 (RF),(R1),(3,5(R6)),(4,6(R4))                                    
*                                  END DATE                                     
         LA    R4,11(R4)                                                        
*                                                                               
OFDT0020 TM    8(R6),X'40'         ALT WEEKS?                                   
         BZ    *+12                                                             
         MVI   0(R4),C'A'                                                       
         LA    R4,1(R4)                                                         
*                                  DISPLAY NPW IF NOT = RBUYNW                  
         L     RF,AIO3                                                          
         USING RMKGREC,RF                                                       
         CLC   RMKGNW,9(R6)                                                     
         BE    OFDT0030                                                         
         DROP  RF                                                               
                                                                                
         MVI   0(R4),C'('                                                       
         EDIT  (1,9(R6)),(3,1(R4)),ALIGN=LEFT                                   
         CLI   9(R6),0                                                          
         BNE   *+8                                                              
         MVI   1(R4),C'0'                                                       
         LA    R4,2(R4)                                                         
         MVI   2(R4),C' '                                                       
         CLI   0(R4),C' '                                                       
         BE    *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
         MVI   0(R4),C')'                                                       
*                                                                               
*   MKGDETLS:  DISPLAY DAY/TIME/LENGTH/COST OF SPOTS OF MAKEGOOD                
*        OFFER.                                                                 
*                                                                               
OFDT0030 DS    0H                                                               
         ST    R6,MKGDOSDY                                                      
         BRAS  RE,NEXTEL                                                        
         BNE   OFDT0035                                                         
*                                                                               
         MVI   0(R4),C'*'                                                       
*                                                                               
OFDT0035 DS    0H                                                               
         L     R6,MKGDOSDY         RESTORE POINTER TO MISSED DATE ELEM          
                                                                                
         LTR   R2,R2                                                            
         BZ    OFDT0040                                                         
         LR    R6,R2                                                            
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   OFDT0090                                                         
         B     OFDT0050                                                         
                                                                                
OFDT0040 DS    0H                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
OFDT0050 DS    0H                                                               
         LR    R2,R6                                                            
         MVC   PMGOFDT,SPACES                                                   
*                                  CLEAR DAY/TIME STRING AREA                   
*                                  DISPLAY DAY-TIME FIELDS                      
         GOTO1 OUTDAY,DMCB,3(R6),2(R6),PMGOFDT                                  
*                                  TIME                                         
         LA    R4,PMGOFDT          CHECK FOR LAST POSITION                      
         LA    RF,L'PMGOFDT        MAX FIELD SIZE                               
OFDT0060 EQU   *                                                                
         CLI   0(R4),C' '          SPACE FOUND?                                 
         BE    OFDT0070            YES                                          
         LA    R4,1(R4)            GO BACK FOR NEXT                             
         BCT   RF,OFDT0060                                                      
         DC    H'0'                NOT FOUND - ERROR                            
OFDT0070 EQU   *                                                                
         MVI   0(R4),C'/'          INSERT SEPARATOR                             
         LA    R4,1(R4)            BUMP TO NEXT POSITION                        
         MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         GOTO1 UNTIME,DMCB,4(R6),0(R4)                                          
*                                                                               
         ST    R6,MKGDOSDY                                                      
         BRAS  RE,NEXTEL                                                        
         BNE   OFDT0080                                                         
*                                                                               
OFDT0075 DS    0H                                                               
         CLI   0(R4),C' '                                                       
         BE    OFDT0078                                                         
         LA    R4,1(R4)                                                         
         B     OFDT0075                                                         
*                                                                               
OFDT0078 DS    0H                                                               
         MVI   0(R4),C'*'                                                       
*                                                                               
OFDT0080 DS    0H                                                               
         L     R6,MKGDOSDY         RESTORE POINTER TO MISSED DATE ELEM          
                                                                                
OFDT0090 EQU   *                                                                
         BRAS  RE,MGSPOOL                                                       
                                                                                
         LR    R6,R5                                                            
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    OFDT0010                                                         
                                                                                
         LR    R6,R2                                                            
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    OFDT0050                                                         
*                                                                               
OFDTX    DS    0H                                                               
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
***********************************************************************         
* DISPLAY MISSED BUY PROGRAM NAME                                               
***********************************************************************         
MBPGMNAM NTR1  BASE=*,LABEL=*                                                   
         LA    R2,P                                                             
         USING PMBUY,R2            ESTABLISH PRINT LINE                         
*                                                                               
         L     R6,AIO2             POINT TO BUY RECORD                          
         MVI   ELCODE,RBUYPGEQ     LOOK FOR PROGRAM ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   MBPGMX              SKIP IF NONE FOUND                           
*                                                                               
         USING RBUYPGEL,R6         ESTABLISH PROGRAM ELEMENT                    
*                                                                               
         CLI   RBUYPGLN,RBUYPGM-RBUYPGEL  SKIP IF NO NAME IN ELM                
         BNH   MBPGMX                                                           
*                                                                               
         ZIC   R1,RBUYPGLN         GET ELEMENT LENGTH                           
         SHI   R1,3                OVERHEAD                                     
         BM    MBPGMX              NO NAME IN ELM                               
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PMBPROG(0),RBUYPGM  PRINT NAME                                   
                                                                                
*                                                                               
MBPGMX   DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R2,R6                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* DISPLAY MAKEGOOD PROGRAM NAME                                                 
***********************************************************************         
MGPGMNAM NTR1  BASE=*,LABEL=*                                                   
         LA    R2,P                                                             
         USING PMGD,R2             ESTABLISH PRINT LINE                         
*                                                                               
         L     R6,AIO3             POINT TO MAKEGOOD RECORD                     
         MVI   ELCODE,RMKGPGEQ     LOOK FOR PROGRAM ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   MGPGMX              SKIP IF NONE FOUND                           
*                                                                               
         USING RMKGPGEL,R6         ESTABLISH PROGRAM ELEMENT                    
*                                                                               
         CLI   RMKGPGLN,RMKGPGM-RMKGPGEL  SKIP IF NO NAME IN ELM                
         BNH   MGPGMX                                                           
*                                                                               
         ZIC   R1,RMKGPGLN         GET ELEMENT LENGTH                           
         SHI   R1,3                OVERHEAD                                     
         BM    MGPGMX              NO NAME IN ELM                               
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PMGPROG(0),RMKGPGM  PRINT NAME                                   
                                                                                
*                                                                               
MGPGMX   DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R2,R6                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY MISSED DEMO VALUE AND GRPS                                     
***********************************************************************         
MBDEMO   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,P                                                             
         USING PMBUY,R2            ESTABLISH PRINT LINE                         
*                                                                               
         L     R6,AIO2             POINT TO BUY RECORD                          
         MVI   ELCODE,RBUYRDCQ     LOOK FOR REP OVERRIDE DEMO ELM               
         BRAS  RE,GETEL                                                         
         BE    MBDEMO10            ELEMENT FOUND                                
*                                                                               
         L     R6,AIO2             POINT TO BUY RECORD                          
         MVI   ELCODE,RBUYDMCQ     LOOK FOR DEMO ELM                            
         BRAS  RE,GETEL                                                         
         BNE   MBDEMOX             SKIP IF NONE FOUND                           
*                                                                               
MBDEMO10 DS    0H                                                               
*                                                                               
         USING RBUYDMEL,R6         ESTABLISH DEMO ELEMENT                       
*                                                                               
         CLI   RBUYDMLN,RBUYDMCV-RBUYDMEL  SKIP IF NO DEMOS IN ELM              
         BNH   MBDEMOX                                                          
*                                                                               
         CLC   RBUYDMDM,=X'FFFFFFFF' SKIP IF NOT ENTERED                        
         BE    MBDEMOX                                                          
*                                                                               
         ZAP   DUB,=P'0'           INIT WORKAREA                                
         MVC   DUB+3(4),RBUYDMDM                                                
         SRP   DUB,64-1,0          MAKE PACKED                                  
         EDIT  (P8,DUB),(7,PMBDEMO),1,ZERO=BLANK                                
*                                                                               
         CVB   RF,DUB              SAVE RATING                                  
*                                                                               
*        FIND MISSED ELEMENT                                                    
*                                                                               
         L     R6,MKGDMSDT         POINT TO MISSED DATE ELEMENT                 
         USING RMKGMGEL,R6         ESTABLISH MAKE GOOD REF ELM                  
*                                                                               
         MVC   HALF+1(1),RMKGMGSP  NUMBER OF MISSED SPOTS                       
*                                                                               
         CVB   RF,DUB              GET RATING                                   
         MH    RF,HALF             GRPS                                         
*                                                                               
         EDIT  (RF),PMBGRPS,1,ZERO=BLANK DISPLAY GRPS                           
*                                                                               
MBDEMOX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY DEMO VALUE AND GRPS                                            
***********************************************************************         
MGDEMO   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,P                                                             
         USING PMGD,R2             ESTABLISH PRINT LINE                         
*                                                                               
         L     R6,AIO3             POINT TO MAKEGOOD RECORD                     
         MVI   ELCODE,RMKGDMCQ     LOOK FOR DEMO ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   MGDEMOX             SKIP IF NONE FOUND                           
*                                                                               
         USING RMKGDMEL,R6         ESTABLISH DEMO ELEMENT                       
*                                                                               
         CLI   RMKGDMLN,RMKGDMCV-RMKGDMEL  SKIP IF NO DEMOS IN ELM              
         BNH   MGDEMOX                                                          
*                                                                               
         CLC   RMKGDMDM,=X'FFFFFFFF' SKIP IF NOT ENTERED                        
         BE    MGDEMOX                                                          
*                                                                               
         ZAP   DUB,=P'0'           INIT WORKAREA                                
         MVC   DUB+3(4),RMKGDMDM                                                
         SRP   DUB,64-1,0          MAKE PACKED                                  
         EDIT  (P8,DUB),(7,PMGDEMO),1,ZERO=BLANK                                
*                                                                               
         CVB   RF,DUB              SAVE RATING                                  
*                                                                               
         L     R6,AIO3             POINT TO MAKEGOOD RECORD                     
         USING RMKGREC,R6          ESTABLISH RECORD                             
*                                                                               
         MVC   HALF,RMKGTSPT       TOTAL SPOTS                                  
*                                                                               
         MH    RF,HALF             GRPS                                         
*                                                                               
         EDIT  (RF),PMGGRPS,1,ZERO=BLANK DISPLAY GRPS                           
*                                                                               
MGDEMOX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY MAKEGOOD DETAIL COMMENTS                                              
***********************************************************************         
MGDETCMT NTR1  BASE=*,LABEL=*                                                   
         LA    R2,P                                                             
         USING PMGD,R2                                                          
                                                                                
         L     R6,AIO3                                                          
         MVI   ELCODE,X'11'                                                     
         BRAS  RE,GETEL                                                         
         BNE   MGCMTX                                                           
                                                                                
         USING RMKGDCEL,R6                                                      
         CLI   RMKGDCLN,RMKGDCCM-RMKGDCEL                                       
         BNH   MGCMTX                                                           
         ZIC   R1,RMKGDCLN                                                      
         SH    R1,=H'3'            OVERHEAD                                     
         BM    MGCMTX                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PMGOFDAT+5(0),RMKGDCCM                                           
                                                                                
         MVC   PMGOFDAT(4),=C'CMT:'                                             
                                                                                
MGCMTX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
*********************************************************************           
*        MGSPOOL ---   ROUTINE TO HANDLE SPOOL INTERFACE FOR MAKEGOOD           
*********************************************************************           
MGSPOOL  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* CALCULATE MAKEGOOD MISSED/OFFER TOTALS                                        
*********************************************************************           
MGOTOTAL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    TOTALS(TOTALSLQ),TOTALS INIT TOTAL BUCKETS                       
*****    XC    MISSTOT$(24),MISSTOT$                                            
         MVC   MGSVKEY,KEY                                                      
*                                                                               
* PROCESS DETAIL RECORDS                                                        
*                                                                               
MTOT10   DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 ASEQ                                                             
*                                                                               
         CLC   KEY(RMKGKPLN-RMKGKEY),KEYSAVE                                    
         BNE   MTOTX                                                            
*                                                                               
         MVI   ION,3                                                            
         GOTO1 AGETREC                                                          
*                                                                               
* CALCULATE TOTAL MISSED DOLLARS AND SPOTS                                      
*                                                                               
         L     R6,AIO3                                                          
         USING RMKGREC,R6                                                       
         TM    RMKGKRTY,X'F0'      CHECK FOR ALL VS. CHOICE                     
         BZ    MTOT20                                                           
         DROP  R6                                                               
*                                                                               
* READ THE X'20' ELEMENT TO SEE IF THIS MAKEGOOD HAS BEEN 'CHOSEN'              
*                                                                               
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RMKGSTEL,R6                                                      
*                                                                               
         CLI   RMKGSTCH,0          CHOSEN?                                      
         BNE   MTOT20              YES, COUNT THIS OFFER                        
****     OC    MISSTOT$(8),MISSTOT$                                             
         OC    TOTMISS(TOTMISLQ),TOTMISS                                        
         BZ    MTOT30              CHOICE OFFERS SHOULD ONLY TOTAL              
         B     MTOT90              MISSED FROM FIRST DETAIL RECORD              
         DROP  R6                  SKIP, IF WE ALREADY HAVE A TOTAL             
*                                                                               
* CALCULATE OFFER TOTAL DOLLARS AND SPOT                                        
*                                                                               
MTOT20   DS    0H                                                               
         L     R6,AIO3                                                          
         USING RMKGREC,R6                                                       
*                                                                               
         ICM   RE,15,RMKGTCOS      AGGREGATE TOTAL DOLLARS AND SPOTS            
         ICM   RF,15,OFFRTOT$                                                   
         AR    RE,RF                                                            
         STCM  RE,15,OFFRTOT$                                                   
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,RMKGTSPT                                                    
         ICM   RF,15,OFFRTOT#                                                   
         AR    RE,RF                                                            
         STCM  RE,15,OFFRTOT#                                                   
*                                                                               
*        AGGREGATE TOTAL GRPS                                                   
*                                                                               
         MVC   HALF,RMKGTSPT       SAVE NUMBER OF SPOTS                         
*                                                                               
         L     R6,AIO3             POINT TO MAKEGOOD RECORD                     
         MVI   ELCODE,RMKGDMCQ     LOOK FOR DEMO ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   MTOT25              SKIP IF NONE FOUND                           
*                                                                               
         USING RMKGDMEL,R6         ESTABLISH DEMO ELEMENT                       
*                                                                               
         CLI   RMKGDMLN,RMKGDMCV-RMKGDMEL  SKIP IF NO DEMOS IN ELM              
         BNH   MTOT25                                                           
*                                                                               
         CLC   RMKGDMDM,=X'FFFFFFFF' SKIP IF NOT ENTERED                        
         BE    MTOT25                                                           
*                                                                               
         ZAP   DUB,=P'0'           INIT WORKAREA                                
         MVC   DUB+3(4),RMKGDMDM                                                
         SRP   DUB,64-1,0          MAKE PACKED                                  
*                                                                               
         CVB   RF,DUB              SAVE RATING                                  
*                                                                               
         MH    RF,HALF             GRPS                                         
*                                                                               
         ICM   RE,15,OFFRGRPS      AGGREGATE GRPS                               
         AR    RE,RF                                                            
         STCM  RE,15,OFFRGRPS                                                   
*                                                                               
MTOT25   DS    0H                                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
MTOT30   DS    0H                                                               
         L     R6,AIO3                                                          
         USING RMKGMGEL,R6                                                      
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BNE   MTOT90                                                           
*                                                                               
         XC    KEY,KEY                                                          
         CLI   RMKGMGSP,0                                                       
         BE    MTOT80              IF NO SPOTS MISSED, SKIP READING             
*                                  TARGET MISSED BUY                            
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),RMKGMGSP                                               
*                                                                               
* CHECK TO SEE IF END DATE USED. IF USED, NEED TO CALCULATE THE NUMBER          
* OF WEEKS MISSED IN ORDER TO GET THE CORRECT OVERALL TOTAL                     
*                                                                               
MTOT33   DS    0H                                                               
         OC    RMKGMGD2,RMKGMGD2                                                
         BZ    MTOT40                                                           
         LA    R3,7                                                             
         MVC   WORK(3),RMKGMGD1                                                 
*                                                                               
MTOT35   DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,WORK),WORK+3                                      
         GOTO1 ADDAY,DMCB,WORK+3,WORK+3,(R3)                                    
         GOTO1 DATCON,DMCB,WORK+3,(3,WORK)                                      
         CLC   WORK(3),RMKGMGD2                                                 
         BH    MTOT40                                                           
         LH    RE,HALF                                                          
         ZIC   RF,RMKGMGSP                                                      
         AR    RE,RF                                                            
         STH   RE,HALF                                                          
         B     MTOT35                                                           
*                                                                               
MTOT40   DS    0H                                                               
         XC    KEY,KEY             CONSTRUCT TARGET MISSED BUYLINE KEY          
MGD      USING RBUYKEY,KEY                                                      
         MVI   MGD.RBUYKTYP,X'0B'                                               
         MVC   MGD.RBUYKREP,REPALPHA                                            
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,TWAWORKQ                                                      
         USING TWAWORK,RF                                                       
         MVC   MGD.RBUYKCON,TWACNUM                                             
         DROP  RF                                                               
*                                                                               
         MVI   ION,2                                                            
*                                                                               
         GOTO1 AHIGH                                                            
*                                                                               
MTOT50   DS    0H                                                               
         CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   MTOTX               NOT FOUND, EXIT                              
*                                                                               
         CLC   MGD.RBUYKLIN,RMKGMGLI                                            
         BE    MTOT60              INCASE WE HAVE MAKEGOOD FOR MAKEGOOD         
*                                                                               
MTOT55   DS    0H                                                               
         GOTO1 ASEQ                                                             
         B     MTOT50                                                           
         DROP  R6                                                               
*                                                                               
MTOT60   DS    0H                                                               
         GOTO1 AGETREC                                                          
*                                                                               
MTOT70   DS    0H                                                               
         L     R5,AIO2                                                          
         USING RBUYREC,R5                                                       
         ICM   R4,15,RBUYCOS                                                    
         MH    R4,HALF                                                          
*                                                                               
         ICM   RE,15,MISSTOT$                                                   
         AR    R4,RE                                                            
         STCM  R4,15,MISSTOT$                                                   
*                                                                               
         LH    R4,HALF                                                          
         ICM   RE,15,MISSTOT#                                                   
         AR    R4,RE                                                            
         STCM  R4,15,MISSTOT#                                                   
*                                                                               
*        GET GRPS MISSED                                                        
*                                                                               
         LR    R0,R6               SAVE ELEMENT POINTER                         
         MVC   FULL(1),ELCODE      SAVE CURRENT ELEMENT CODE                    
*                                  FIND DEMO ELEMENT                            
         L     R6,AIO2             POINT TO BUY RECORD                          
         MVI   ELCODE,RBUYRDCQ     SET FOR REP DEMO ELEMENT                     
         BRAS  RE,GETEL            FIND REP DEMO ELEMENT                        
         BE    MTOT72              FOUND                                        
*                                                                               
         L     R6,AIO2             POINT TO BUY RECORD                          
         MVI   ELCODE,RBUYDMCQ     SET FOR AGY DEMO ELEMENT                     
         BRAS  RE,GETEL            FIND AGY DEMO ELEMENT                        
         BNE   MTOT75              SKIP IF NOT FOUND                            
*                                                                               
MTOT72   DS    0H                                                               
*                                                                               
         USING RBUYDMEL,R6         ESTABLISH DEMO ELEMENT                       
*                                                                               
         CLI   RBUYDMLN,RBUYDMCV-RBUYDMEL  SKIP IF NO DEMOS IN ELM              
         BNH   MTOT75                                                           
*                                                                               
         CLC   RBUYDMDM,=X'FFFFFFFF' SKIP IF NOT ENTERED                        
         BE    MTOT75                                                           
*                                                                               
         ZAP   DUB,=P'0'           INIT WORKAREA                                
         MVC   DUB+3(4),RBUYDMDM                                                
         SRP   DUB,64-1,0          MAKE PACKED                                  
*                                                                               
         CVB   RF,DUB              SAVE RATING                                  
*                                                                               
         MH    RF,HALF             GRPS=SPTS*RTG                                
*                                                                               
         ICM   RE,15,MISSGRPS      ACCUMULATE MISSED GRPS                       
         AR    RE,RF                                                            
         STCM  RE,15,MISSGRPS                                                   
*                                                                               
         DROP  R6                                                               
*                                                                               
MTOT75   DS    0H                                                               
         LR    R6,R0               RESTORE ELEMENT POINTER                      
         MVC   ELCODE,FULL         RESTORE ELEMENT ID                           
*                                                                               
         DROP  R5                                                               
*                                                                               
MTOT80   DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   MTOT90                                                           
         USING RMKGMGEL,R6                                                      
         CLI   RMKGMGSP,0          SKIP IF NO SPOTS MISSED                      
         BE    MTOT80                                                           
         OC    KEY,KEY                                                          
         BZ    MTOT33                                                           
*                                                                               
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),RMKGMGSP                                               
*                                                                               
         CLC   RMKGMGLI,MGD.RBUYKLIN   SAME BUY?                                
         BNE   MTOT33                                                           
*                                                                               
         OC    RMKGMGD2,RMKGMGD2                                                
         BZ    MTOT70                                                           
         LA    R3,7                                                             
         MVC   WORK(3),RMKGMGD1                                                 
*                                                                               
MTOT85   DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,WORK),WORK+3                                      
         GOTO1 ADDAY,DMCB,WORK+3,WORK+3,(R3)                                    
         GOTO1 DATCON,DMCB,WORK+3,(3,WORK)                                      
         CLC   WORK(3),RMKGMGD2                                                 
         BH    MTOT70                                                           
         LH    RE,HALF                                                          
         ZIC   RF,RMKGMGSP                                                      
         AR    RE,RF                                                            
         STH   RE,HALF                                                          
         B     MTOT85                                                           
*                                                                               
*        B     MTOT70                                                           
         DROP  MGD,R6                                                           
*                                                                               
MTOT90   DS    0H                  RE-ESTABLISH MAKEGOOD OFFER RECORD           
         L     R6,AIO3                                                          
         MVC   KEY(27),0(R6)                                                    
*                                                                               
         MVI   ION,3                                                            
         GOTO1 AHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    MTOT10                                                           
*                                                                               
MTOTX    DS    0H                                                               
         MVC   KEY,MGSVKEY                                                      
         GOTO1 AHIGH                                                            
         MVI   ION,3                                                            
         GOTO1 AGETREC                                                          
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* GET AGENCY DEMO CATEGORY FROM CONFIRMED DARE HEADER RECORD                    
*********************************************************************           
GETDEMO  NTR1  BASE=*,LABEL=*                                                   
         MVC   MGSVKEY,KEY                                                      
         XC    DISADEMO,DISADEMO                                                
         XC    AGYDEMOS(16),AGYDEMOS                                            
*                                                                               
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'12'        GET SAR ELEMENT                              
         BRAS  RE,GETEL                                                         
         BNE   GETDEM05            FOUND                                        
*                                                                               
         USING RSAREL,R6           ESTABLISH SAR ELEMENT                        
*                                                                               
         OC    RSARDEM,RSARDEM     SKIP IF NO DEMOS                             
         BZ    GETDEM05                                                         
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(3),RSARDEM     SAVE FIRST DEMO                              
         NI    WORK,X'FF'-X'80'    KILL PRIMARY DEMO INDICATOR                  
         B     GETDEM25                                                         
*                                                                               
         DROP  R6                                                               
*                                                                               
GETDEM05 DS    0H                  LOOK FOR DARE ELEMENT                        
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BRAS  RE,GETEL                                                         
         BNE   GETDEMX                                                          
*                                                                               
         XC    KEY,KEY                                                          
KEYD     USING RAGY2KEY,KEY                                                     
         MVI   KEYD.RAGK2TYP,RAGK2TYQ                                           
         MVC   KEYD.RAGK2AGY,RCONKAGY                                           
         MVC   KEYD.RAGK2AOF,RCONKAOF                                           
         MVC   KEYD.RAGK2REP,RCONKREP                                           
         DROP  KEYD                                                             
*                                                                               
         MVI   ION,4                                                            
         GOTO1 AHIGH                                                            
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
         GOTO1 AGETREC,DMCB,AIO4                                                
*                                                                               
         LA    R5,KEY                                                           
         USING RDARKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RDARKTYP,X'51'                                                   
         MVC   RDARKREP,REPALPHA                                                
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     MUST SPECIFY IF TV                           
         OC    RDARKSTA,=6C' '                                                  
*                                                                               
         L     R6,AIO4                                                          
         USING RAGY2REC,R6                                                      
         MVC   RDARKAGY(5),RAGY2DAR   EQUIVALENCY CODE                          
         DROP  R6                                                               
                                                                                
         LA    R6,RCONREC                                                       
         USING RCONDREL,R6                                                      
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   RDARKORD,RCONDRLK   ORDER NUMBER                                 
         MVI   RDARKRT,X'10'       AGENCY HEADER ONLY                           
         DROP  R6                                                               
                                                                                
         L     R6,AIO4                                                          
         USING RAGY2REC,R6                                                      
         LA    R4,RAGY2DAR         THERE ARE MAX 4 AGENCY ASSIGNMENT            
         LA    R3,3                COMBINATIONS WE NEED TO CHECK                
                                                                                
GETDEM10 DS    0H                                                               
         MVI   ION,4                                                            
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    GETDEM20                                                         
         CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         BNL   *+6                 HAS MULTI-DARE AGENCY ASSIGNMENTS            
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE                                                      
         LA    R4,5(R4)                                                         
         MVC   RDARKAGY(5),0(R4)   EQUIVALENCY CODE                             
         BCT   R3,GETDEM10                                                      
         B     GETDEMX                                                          
         DROP  R5,R6                                                            
*                                                                               
GETDEM20 DS    0H                                                               
         GOTO1 AGETREC,DMCB,AIO4                                                
*                                                                               
         L     R6,AIO4                                                          
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GETDEMX                                                          
         USING RDARDMEL,R6                                                      
         MVC   AGYDEMOS(16),RDARDEM1                                            
*                                                                               
         OC    AGYDEMOS,SPACES                                                  
         CLC   AGYDEMOS,SPACES                                                  
         BE    GETDEMX                                                          
         DROP  R6                                                               
*                                                                               
         XC    WORK,WORK                                                        
*                                                                               
         CLI   AGYDEMOS,C'('       SKIP USER DEFINED DEMOS                      
         BE    GETDEMX                                                          
         MVC   WORK+1(1),AGYDEMOS                                               
         CLI   WORK+1,C'T'         FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   WORK+1,C'I'                                                      
         PACK  DUB(8),AGYDEMOS+1(3)                                             
         CVB   RF,DUB                                                           
         STC   RF,WORK+2                                                        
*                                                                               
GETDEM25 DS    0H                                                               
*                                                                               
         L     R4,AIO4                                                          
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
                                                                                
         GOTO1 DEMCON,DMCB,(1,WORK),(2,DISADEMO),(0,DBLOCKD)                    
         DROP  R4                                                               
*                                                                               
GETDEMX  DS    0H                                                               
         MVC   KEY,MGSVKEY                                                      
         GOTO1 AHIGH                                                            
         MVI   ION,3                                                            
         GOTO1 AGETREC                                                          
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         ORG   T80263+(((*-T80263)/2048)+1)*2048                                
         EJECT                                                                  
* LOCAL WORK STORAGE AREA                                                       
DOMKGSD  DSECT                                                                  
PERVAL   DS    A                                                                
MKGDMSDT DS    A                   MAKEGOOD MISSED DATE ELEMENT POINTER         
MKGDOSDY DS    A                   MAKEGOOD OFFER MISSED DATE/DAY/TIME          
MKGSTAT  DS    X                   STATUS FLAG FOR MAKEGOOD PROCESSING          
MKGTYPE  DS    X                   MAKEGOOD OFFER TYPE (SEE RMKGRTS)            
SVKEY    DS    CL27                                                             
MGSVKEY  DS    CL27                                                             
MKGINDEX DS    X                                                                
AGYDEMOS DS    4CL4                DARE AGENCY DEMOS (FIRST = PRIMARY)          
DISADEMO DS    CL8                                                              
*                                                                               
TOTALS   DS    0D                  TOTAL BUCKETS                                
TOTMISS  DS    0X                  MISSED SPOT TOTALS                           
MISSTOT$ DS    XL4                   TOTAL MISSED  DOLLARS                      
MISSTOT# DS    XL4                   TOTAL MISSED  SPOTS                        
MISSGRPS DS    XL4                   TOTAL MISSED  GRPS                         
TOTMISLQ EQU   *-TOTMISS           LENGTH OF MISSED BUCKETS                     
TOTOFFR  DS    0X                  OFFER  SPOT TOTALS                           
OFFRTOT$ DS    XL4                   TOTAL OFFERED DOLLARS                      
OFFRTOT# DS    XL4                   TOTAL OFFERED SPOTS                        
OFFRGRPS DS    XL4                   TOTAL OFFERED GRPS                         
TOTOFFLQ EQU   *-TOTOFFR           LENGTH OF OFFER  BUCKETS                     
TOTALSLQ EQU   *-TOTALS            LENGTH OF TOTALS BUCKETS                     
*                                                                               
BLOCK    DS    XL128                                                            
DOMKGSX  EQU   *                                                                
*                                                                               
* MISSED BUY PRINT LINE                                                         
*                                                                               
PMBUY    DSECT                                                                  
PMBHEAD  DS    CL3                                                              
PMBINDEX DS    CL3                                                              
         DS    CL3                                                              
PMBBUYNM DS    CL3                                                              
         DS    CL2                                                              
PMBDATE  DS    CL11                                                             
         DS    CL2                                                              
PMBNPW   DS    CL3                                                              
         DS    CL2                                                              
PMBDYTM  DS    CL22                                                             
         DS    CL2                                                              
PMBLEN   DS    CL3                                                              
         DS    CL2                                                              
PMBCOST  DS    CL10                                                             
         DS    CL2                                                              
PMBPROG  DS    CL28                                                             
         DS    CL2                                                              
PMBDEMO  DS    CL8                                                              
         DS    CL2                                                              
PMBGRPS  DS    CL6                 GRPS                                         
*                                                                               
* MAKEGODD OFFER PRINT LINE                                                     
*                                                                               
PMGD     DSECT                                                                  
PMGHEAD  DS    CL3                                                              
PMGINDEX DS    CL3                                                              
         DS    CL3                                                              
PMGOFF#  DS    CL3                                                              
         DS    CL2                                                              
PMGOFDAT DS    CL11                                                             
         DS    CL2                                                              
PMGNPW   DS    CL3                                                              
         DS    CL2                                                              
PMGOFDT  DS    CL22                                                             
         DS    CL2                                                              
PMGLEN   DS    CL3                                                              
         DS    CL2                                                              
PMGCOST  DS    CL10                                                             
         DS    CL2                                                              
PMGPROG  DS    CL28                                                             
         DS    CL2                                                              
PMGDEMO  DS    CL8                                                              
         DS    CL2                                                              
PMGGRPS  DS    CL6                 GRPS                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'127RECNT63   08/15/13'                                      
         END                                                                    
