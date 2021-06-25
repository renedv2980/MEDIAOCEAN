*          DATA SET RECNT00S   AT LEVEL 070 AS OF 05/19/03                      
*PHASE T80200A,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE REGENSTC                                                               
*INCLUDE REGENDAY                                                               
*INCLUDE MEDGET                                                                 
*INCLUDE REGENDMV                                                               
         TITLE 'T80200 - RECNT00 - REPPAK CONTRACT BASE PROGRAM'                
*                                                                               
***********************************************************************         
*                                                                     *         
*        RECNT00 (T80200) --- CONTRACT PROGRAM - BASE                 *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* SEE RECNTHIST FOR PREVIOUS HISTORY                                  *         
*                                                                     *         
* 04NOV02 SKU DARE/AMEND TRIGGERS AUTO SEND VIA GLOBBER               *         
*         SKU MOVE CALL TO REGENVER VIA REPFACS                       *         
* 07OCT02 SKU DO NOT CHECK MAX REC SIZE FOR X'41'/X'51' DARE RECORDS  *         
* 26SEP02 SKU FIX WEB CONFIRMATION R11 BUG                            *         
* 01FEB02 SKU REPLACEMENT OFFER SUPPORT                               *         
* 22AUG01 SKU FIX TURNAROUND DIRECT ROUTING FOR NY CONTRACTS          *         
* 14AUG01 SKU ADD 'DONE' ACTION TO CKTAREQ                            *         
* 24JUL01 BU  NEW SCREENS: ADD BUY CODE                               *         
* 11JUL01 SKU ADD FB (FOX STATION SALES) TO 1KTAREQ T/A REQUEST       *         
* 09JUL01 BU  SCREEN CONTROL FOR TD/TC: JDS/2000 VERSION              *         
* 25APR01 BU  SET 'DAILY PACING' FROM REPPROF+27                      *         
* 01FEB01 BU  SCREEN CONTROL FOR TD/TC: COLUMBINE VERSION             *         
* 16JAN01 RHV AUTOGEN CHANGE SELECTIVE UPDATE                         *         
* 14JUL00 SKU CHECK PENDING PROFILE FOR DARE ADD/ADDS                 *         
* 28JUN00 BU  REMOVE REFERENCE TO GLV1GOTO PER MEL HERTZIG            *         
* 22MAY00 BU  CLEAR COMBO TRADE FLAG                                  *         
* 04MAY00 SKU NEW MAKEGOOD OFFER SUPPORT                              *         
* 23FEB00 RHV AUDIT ELEMENTS IN RCONREC                               *         
* 02JUN99 SKU LIMIT BUY RECORD SIZE TO <= 972 BYTES                   *         
* 22FEB99 RHV NBC HOME MARKET                                         *         
* 01FEB99 SKU OFFICE OPTION TO PRINT T/A AT DDS                       *         
* 09DEC98 RHV HOME MKT PROF#38 STA/REP INTERCHANGEABLE                *         
* 16NOV98 SKU IF XFER FROM RIS, CHECK APPROPRIATE EQUATES ONLY        *         
* 12NOV98 JRD P3 FROM MGL/TOTAL                                       *         
* 06NOV98 SKU HOTKEY TO DARE                                          *         
* 07JUL98 SKU ALLOW 3972-BYTE MAX CONTRACTS                           *         
* 09JUN98 RHV REVISE CKGLOB, AUTOGEN CHA MODE, BROWSE THRU REPFACS    *         
* 17APR98 SKU HARDCODE (UGH!) NPTVNY TO PRINT CONTRACTS AT DDS        *         
* 02FEB98 SKU 4K CONTRACT SUPPORT                                     *         
* 10DEC97 SKU SELTEL NY CONTRACTS PRINTS AT DDS                       *         
* 19NOV97 BU  CF* ACTION FOR TAKEOVER PAPERWORK                       *         
* 06OCT97 RHV ENABLE AUTO RETURN TO CALLER ON GENERIC GLOBBER CALL    *         
* 26SEP97 RHV SAVE OFF VREPFACS                                       *         
* 26AUG97 RHV USE BROWSE INTERFACE MODULE                             *         
* 17AUG97 BU  KATZ NATIONAL LOCKOUT                                   *         
* 21JUL97 SKU SUPPORT FOR AUTOHEADER CONTRACT TYPE                    *         
* 15JUL97 SKU FIX GLOBBER SESSION BUG                                 *         
* 28MAY97 RHV SUPPORT CFC ACTION                                      *         
*             NEW MECHANISM FOR CALLING REGENVER                      *         
* 27MAY97 BU  LOCK OUT GROUP W                                        *         
* 22APR97 RHV GENERIC PF12 RETURN SWAP TO CALLER                      *         
*             SUPPORT SWAP FROM SFM MOVE HISTORY DISPLAY              *         
* 18MAR97 RHV SUPPORT FULL SIZE SUBSCRENS                             *         
*             NEW CF ACTION LOGIC                                     *         
* 18DEC96 SKU EXPAND ESTIMATE TO 8 CHARS FOR LEO EDI ORDERS           *         
* 06DEC96 RHV READ REP LOGO FILENAME FROM REPREC                      *         
* 12NOV96 RHV GLOBBER RETURN FROM BROWSE                              *         
* 17SEP96 SKU FIX GLOBBER CALL FROM RIS                               *         
* 02AUG96 RHV GENERATE NORATE COPY FOR TURNAROUNDS PER CONTYPE OPT #2 *         
* 01AUG96 SKU MOVE GLOBBER CHECK BEFORE HOTKEY CHECK                  *         
* 29JUL96 BU  LOCKOUT CM/I1/TO/AQ UPDATE ACTIONS                      *         
* 17JUL96 BU  LOCKOUT CM/I1/TO UPDATE ACTIONS                         *         
* 12JUN96 SKU SAVE OFF REP PARENT CODE                                *         
*             FIX KATZ TV NY CONFIRMATION REDIRECTION                 *         
* 03MAY96 BU  LOCKOUT V4/V5/V6 UPDATE ACTIONS                         *         
* 22APR96 PXZ PF KEY ROUTINES ADDED FOR SONNET                        *         
* 08APR96 PXZ PF KEY ROUTINES ADDED FOR SONNET                        *         
* 26MAR96 WSB IF STEREO, SHOW PROF BITS ON MESG LINE WHEN ENTER CONTR *         
* 21MAR96 SKU EDI FLIGHT# SUPPORT                                     *         
* 15MAR96 SKU FIX CONX BUG OF SKIP T/A REQUEST                        *         
* 09MAR96 SKU STEREO SUPPORT                                          *         
* 26JAN96 SKU AUTO-APPLY FROM DARE MAKEGOOD                           *         
* 02JAN96 SKU AUTO HEADER GENERATION. TOMBSTONE MOVED TO RECNTHIST    *         
*             GETREC-CHECK FOR NON-CONTRACT RECS > 1000 BYTES         *         
* 28DEC95 SKU REMOVE 1K fESTRICTION IN DATAMGR PUTREC                 *         
* 13DEC95 SKU 2K CONTRACT SUPPORT                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
T80200   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL GENOLDX-GENOLD,T80200,R9,RR=R5,CLEAR=YES                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         L     RE,=V(GETBROAD)                                                  
         AR    RE,R5               RELOCATE VCON                                
         ST    RE,VGTBROAD                                                      
*                                                                               
         L     RE,=V(REGENSTC)                                                  
         AR    RE,R5               RELOCATE                                     
         ST    RE,VREGENSC         EXTERNAL ROUTINE, STORED COMMENTS            
*                                                                               
         L     RE,=A(DOGENVER)                                                  
         AR    RE,R5               RELOCATE                                     
         ST    RE,VREGENVR         EXTERNAL ROUTINE, SAVES VERS DATES           
*                                                                               
*       PREVIOUSLY THE FIELD REGENBUC WAS SET TO ADDR. OF AN INCLUDED           
*       REGENBUC MODULE. IT NOW HOLDS THE ADDR. OF AN INTERNAL                  
*       ROUTINE THAT PEFORMS A PASS THROUGH TO THE REPFACS RFGENBUC             
*                                                                               
***>     LA    RE,CALLGENB                                                      
         L     RE,=A(CALLGENB)                                                  
         AR    RE,R5                                                            
         ST    RE,REGENBUC         INTERNAL ROUTINE                             
*                                                                               
         L     RE,=V(REGENDAY)                                                  
         AR    RE,R5               RELOCATE                                     
         ST    RE,VREGENDP         EXTERNAL ROUTINE, DAYPART VALIDATION         
*                                                                               
         L     RE,=V(MEDGET)                                                    
         AR    RE,R5               RELOCATE                                     
         ST    RE,VMEDGET          EXTERNAL ROUTINE, MEDGET NEEDED BY           
*                                    REGENDAY                                   
         L     RE,=V(REGENDMV)                                                  
         AR    RE,R5               RELOCATE                                     
         ST    RE,VGENDMV          EXTERNAL ROUTINE, WORKSHEET MOD/VER          
*                                    DISPLAY                                    
         L     RE,=A(AGEN)                                                      
         AR    RE,R5               RELOCATE                                     
         ST    RE,AAUTOGEN         AUTOGEN SCREEN FILL ROUTINE                  
*                                                                               
* SET UP ADDRESSIBILITY TO IOAREA, IO2, IO3 AND IO4                             
*                                                                               
         LR    RE,RC                                                            
         A     RE,=AL4(IOAREA-GENOLD)                                           
         ST    RE,AIOAREA                                                       
         LR    RE,RC                                                            
         A     RE,=AL4(IO2-GENOLD)                                              
         ST    RE,AIO2                                                          
         LR    RE,RC                                                            
         A     RE,=AL4(IO3-GENOLD)                                              
         ST    RE,AIO3                                                          
         LR    RE,RC                                                            
         A     RE,=AL4(IO4-GENOLD)                                              
         ST    RE,AIO4                                                          
         LR    RE,RC                                                            
         A     RE,=AL4(SPOOLAR-GENOLD)                                          
         ST    RE,ASPULAR                                                       
         LR    RE,RC                                                            
         A     RE,=AL4(SPOOLAR+CONLENQ-GENOLD)                                  
         ST    RE,AALTIO                                                        
*                                                                               
         BAS   RE,INITL            INITIALIZE                                   
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
         ST    RB,BASERB                                                        
         ST    R9,BASER9                                                        
         ST    RD,BASERD                                                        
         SPACE 1                                                                
         ST    R1,AFACILS                                                       
         MVC   ACOMFACS,16(R1)                                                  
         MVC   LOCALUP(4),VRECUP                                                
         LA    R1,MYRECUP                                                       
         ST    R1,VRECUP                                                        
*                                  ESTABLISH A(REPFACS)                         
         GOTO1 CALLOV,DMCB,0,X'D9000AAC',0                                      
         MVC   VREPFACS,0(R1)      EXTERNAL ROUTINE, MISC. SUBROUTINES          
*                                                                               
         MVI   TWACFFLG,0          INITIALIZE CONFIRMATION FLAGS                
*                                                                               
         LA    R1,CONMSGH          SET SERVICE-REQUEST FIELD TO                 
         ZIC   R0,0(R1)             MODIFIED FOR NEXT INPUT...                  
         AR    R1,R0                                                            
         OI    6(R1),X'81'                                                      
         SPACE 1                                                                
         TM    CONCACTH+1,X'01'    FIELD MODIFIED?                              
         BZ    *+12                                                             
         NI    CONCACTH+1,X'FE'    TURN OFF MODIFIED                            
         OI    CONCACTH+6,X'80'    XMIT FIELD                                   
         SPACE 1                                                                
         NI    TWAPROST,X'FF'-X'3C'  TURN OFF CHANGED THIS TIME                 
*                                     AND NEW SUBSCREEN LOADED                  
         OC    TWAUSER,TWAUSER                                                  
         BNZ   CON10               NOT FIRST TIME                               
         MVI   TWASCRN,X'FF'       VIRGIN SCREEN                                
         MVI   TWAPROST,X'40'      COMMENTS                                     
         LA    R3,CONBACTH-TWATASK                                              
         ST    R3,ABUYFH           A(BUY ACTION FIELD)                          
         LA    R3,CONLAST-TWATASK                                               
         ST    R3,ALSTFH           A(END OF SCREEN)                             
         MVC   LASTCACT,MYSPACES                                                
*                                                                               
         GOTO1 GETFACT,DMCB,0      CHECK FOR AVN TERMINAL LUID                  
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         LA    RE,AVNLUID                                                       
         B     *+8                                                              
         LA    RE,L'AVNTAB(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BE    *+18                                                             
         CLC   FASYM,0(RE)                                                      
         BNE   *-18                                                             
         OI    TWAFLAGS,TWAFLAV2   AVN TERMINAL LUID                            
         DROP  RF                                                               
*                                                                               
         GOTO1 =A(LGETPROF),DMCB,(RC),RR=Y  FETCH PROFILES                      
*                                                                               
         MVC   TWASVACS,TWAACCS                                                 
         TM    PROFILES+CNTRPECB,CNTRPECA   HOME MARKET BY PROF 38?             
         BO    CON09                                                            
*                                                                               
         TM    PROFILES+CNTRPEAB,CNTRPEAA   PROF 46 (CHECK FOR L)               
         BZ    CON10                                                            
         XC    DUB,DUB                                                          
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTOX (RFGETID,VREPFACS),DMCB,(RA),WORK,0,DUB                          
         CLI   WORK+4,C'L'         5 POSITION OF SIGNON = 'L'?                  
         BNE   CON10                                                            
CON09    OI    TWAFLAGS,TWAFLHMQ   HOME MARKET IN PROGRESS                      
*                                                                               
*              SET UP LINKAGE TO COMMON SUB-ROUTINES                            
*                                                                               
CON10    DS    0H                                                               
         LA    R6,COMMIN                                                        
         SR    R5,R5                                                            
         LA    R8,VADDELEM                                                      
         LA    R0,COMMEND                                                       
*                                                                               
COMNXT   ST    R6,0(R8)                                                         
         STC   R5,0(R8)                                                         
         LA    R5,4(R5)                                                         
         LA    R8,4(R8)                                                         
         BCT   R0,COMNXT                                                        
*                                                                               
         L     RE,ACOMFACS         CALL GETFACT TO GET UTL ENTRIES              
         USING COMFACSD,RE         WE NEED TO CHECK IF STEREO IN USE            
         L     RF,CGETFACT                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,(X'80',0),F#UTLD                                       
         L     R1,0(R1)                                                         
         USING F@UTLD,R1           TEST FOR STEREO                              
         TM    F@TSTAT6,TST6STRO+TST6STFU                                       
         BO    CON13                                                            
         DROP  R1                                                               
*                                                                               
         XC    TWASTREO,TWASTREO                                                
         B     CON14                                                            
*                                                                               
CON13    DS    0H                                                               
         OI    TWASTREO,X'80'      SET STEREO IN USE                            
         TM    TWASTREO,X'40'      DSM STEREO IN PROGRESS?                      
         BO    CON50                                                            
*                                                                               
         CLI   TWASCRN,X'D3'       RESTORE BASE SCREEN IF WE ARE                
         BNE   CON14               DONE WITH STEREO DSM                         
         MVC   NUMFLD,=H'2'        FAKE OUT CONTRACT                            
         MVI   TWASCRN,X'FF'       SET WHICH SCREEN LOADED                      
         OI    TWAPROST,X'20'                                                   
         LA    R3,CONMSGH                                                       
         MVC   DMCB+4(3),=X'D90802'                                             
*                                                                               
         MVI   DMCB+7,X'FF'                                                     
         GOTO1 CALLOV,DMCB,(R3)    LOAD SCREEN # FF                             
         CLI   DMCB+4,X'FF'        SCREEN FOUND?                                
         BNE   *+6                 YES                                          
         DC    H'0'                NO  - DUMP IT OUT                            
*                                                                               
         MVC   CONCACT(8),=C'DIS,HIST'                                          
         MVI   CONCACTH+5,8                                                     
         OI    CONCACT+6,X'80'     XMIT FIELD                                   
*                                                                               
         PACK  WORK+25(1),TWACNUM+3(1)  REVERSE THE COMPLIMENT                  
         PACK  WORK+26(1),TWACNUM+2(1)                                          
         PACK  WORK+27(1),TWACNUM+1(1)                                          
         PACK  WORK+28(1),TWACNUM(1)                                            
*                                                                               
         ZAP   WORK+30(5),=P'0'                                                 
         MVO   WORK+30(5),WORK+25(4)                                            
         ZAP   WORK+20(5),=P'99999999'                                          
         SP    WORK+20(5),WORK+30(5)                                            
         EDIT  (P5,WORK+20),(8,CONCNUM),ALIGN=LEFT                              
         STC   R0,CONCNUMH+5                                                    
         OI    CONCACTH+1,X'01'    MODIFIED                                     
         OI    CONCACTH+6,X'80'    XMIT FIELD                                   
         OI    CONCNUMH+1,X'01'    MODIFIED                                     
         OI    CONCNUMH+4,X'08'    SET NUMERIC                                  
         OI    CONCNUMH+6,X'80'    XMIT FIELD                                   
*                                                                               
CON14    DS    0H                                                               
         GOTOX (RFBROWSE,VREPFACS),DMCB,ACOMFACS,(RD),0                         
*                                                                               
         GOTO1 =A(CKGLOB),DMCB,(RC),RR=Y                                        
         BZ    CON15                                                            
                                                                                
         GOTO1 =A(HOTKEY),DMCB,(RC),RR=Y                                        
         BZ    EXXMOD                                                           
                                                                                
* IF NOT AT LEAST ONE FIELD CHANGED, DO NOT ALLOW ANOTHER CON TO BE             
* ADDED BY JUST PRESSING ENTER WHEN USING $SW.  CON BASE WILL ALWAYS            
* SET SERV-REQ FIELD TO MODIFIED TO ALLOW CONSECUTIVE CON ADDS.                 
         SPACE 1                                                                
         CLC   NUMFLD,=H'1'                                                     
         BH    CON15                                                            
*        LA    R3,336              INVALID OR NO DATA REC'D                     
         LA    R2,CONCACTH                                                      
         OI    CONCACTH+6,X'40'+X'80'                                           
         GOTO1 GETTXT,DMCB,156,0,(C'I',0),0,0,0                                 
*                                                                               
         TM    TWASTREO,X'80'      IS STEREO ON?                                
         BNO   CON14X              NO                                           
         CLI   PROFEQU,RREPQCNT    IF PROFILE NOT READ, READ IT!                
         BE    CON14X              AND DISPLAY IT (FIRST TIME ONLY)             
         GOTO1 =A(LGETPROF),DMCB,(RC),RR=Y                                      
*                                                                               
         LA    R2,CONMSGH          FLD HDR OF MESSAGE LINE                      
         GOTO1 BITOUT60,DMCB,PROFILES    DISPLAY FIRST 60 PROFILE BITS          
*                                                                               
CON14X   DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
*- BITOUT60 -- DISPLAY FIRST 60 Y/N BYTES                                       
*  P1 = A(DBLWD DATA BITS)                                                      
*  R2 = A(FLD HEADER FOR OUTPUT)                                                
***********************************************************************         
BITOUT60 NTR1                                                                   
         LA    R0,60               FIRST 60 BITS TO DISPLAY                     
         LA    R2,8(R2)                                                         
         L     R1,0(R1)            A(DATA BITS)                                 
         LM    R4,R5,0(R1)                                                      
         B     BOUT30                                                           
*                                                                               
BOUT20   SLDL  R4,1                                                             
BOUT30   MVI   0(R2),C'Y'                                                       
         LTR   R4,R4               HI-ORDER BIT ON?                             
         BM    BOUT40              YES. LEAVE AS 'Y'                            
         MVI   0(R2),C'N'                                                       
BOUT40   LA    R2,1(R2)            NEXT A(OUT)                                  
         BCT   R0,BOUT20                                                        
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  IF NOT ALREADY THERE, GET REP PROFILES FOR CONTRACT PROGRAM                  
***********************************************************************         
CON15    DS    0H                                                               
         CLI   PROFEQU,RREPQCNT                                                 
         BE    CON17                                                            
         GOTO1 =A(LGETPROF),DMCB,(RC),RR=Y                                      
*                                                                               
CON17    EQU   *                                                                
         TM    TWAFLAGS,TWAFLHMQ   HOME MARKET IN PROGRESS?                     
         BZ    CON18                                                            
         MVC   TWAACCS,TWASVACS    RESTORE LIMIT ACCESS CODE                    
*                                  IN CASE WE OVERRODE IT PREVIOUSLY            
CON18    EQU   *                                                                
         CLI   PROFEQU+1,1         KATZ REP?                                    
         BNE   CON18100            NO                                           
         LA    R2,CONCACTH         YES - CHECK FOR VALID ACTIONS                
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    CON18040            NO  - CHECK BUY                              
         CLI   5(R2),4             DON'T ALLOW CON,BUY ACTION                   
         BH    CON18090            TOO MANY CHARS:  DON'T ALLOW                 
         CLC   =C'DIS',8(R2)       DISPLAY?                                     
         BE    CON18040            YES - GO CHECK BUY FIELD                     
         CLC   =C'PRI',8(R2)       PRINT?                                       
         BE    CON18040            YES - GO CHECK BUY FIELD                     
         CLC   =C'LAST',8(R2)      LAST?                                        
         BNE   CON18090            NO  - INVALID CONTRACT ACTION                
CON18040 EQU   *                                                                
         LA    R2,CONBACTH         CHECK FOR VALID ACTIONS IN BUY               
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    CON18100            NO  - PERMIT IT                              
         CLC   =C'DIS',8(R2)       DISPLAY?                                     
         BE    CON18100            YES - PERMIT IT                              
         CLC   =C'DSM',8(R2)       DISPLAY MULTI?                               
         BE    CON18100            YES - PERMIT IT                              
         CLC   =C'HIST',8(R2)      HISTORY?                                     
         BNE   CON18090            NO  - INVALID CONTRACT ACTION                
         B     CON18100            ACCEPTED:  PROCESS ACTION                    
CON18090 EQU   *                                                                
         LA    R3,519              SET ERROR MESSAGE                            
         B     ERROR                                                            
CON18100 EQU   *                                                                
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(5,0),(3,TODAY)                                      
* GET MONDAY WEEK DATE                                                          
         GOTO1 (RF),(R1),(3,TODAY),DUB                                          
* GET DAY OF WEEK                                                               
         GOTO1 GETDAY,(R1),DUB,FULL                                             
         CLC   FULL(3),MYSPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                WRONG COMPUTER DATE                          
         SR    R4,R4                                                            
         IC    R4,DMCB                                                          
         BCTR  R4,R0               DAY                                          
         LNR   R4,R4                                                            
         GOTO1 ADDAY,(R1),DUB,WORK,(R4)                                         
         GOTO1 DATCON,(R1),WORK,(2,MONDATE)                                     
         EJECT                                                                  
*              GET REP RECORD AND SPL DATE                                      
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,1                                                            
         MVC   KEY+25(2),REPALPHA                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   NOREP                                                            
         SPACE 1                                                                
         GOTO1 VGETREC,DMCB,RREPREC                                             
         MVC   TWAREPNM,RREPNAME                                                
         MVC   INTREPNO,REPALPHA                                                
         MVC   TWAREPAR,REPALPHA                                                
* SAVE OFF MASTER REP                                                           
         CLC   RREPMAST,=X'0000'     NO MASTER/SUBSID CONTROL                   
         BE    CON18110                                                         
         CLC   RREPMAST,=X'4040'     NO MASTER/SUBSID CONTROL                   
         BE    CON18110                                                         
         CLC   RREPMAST,=X'FFFF'     THIS IS MASTER REP                         
         BNE   CON18105                                                         
***>     MVC   TWARMAST,RREPKREP                                                
***>     B     CON18110                                                         
         LA    R3,842                                                           
         B     ERROR                                                            
CON18105 MVC   TWARMAST,RREPMAST     SAVE MASTER REP                            
CON18110 OC    RREPPAR,MYSPACES                                                 
         CLC   RREPPAR,MYSPACES                                                 
         BE    CON18120                                                         
         MVC   INTREPNO,RREPPAR                                                 
         MVC   TWAREPAR,RREPPAR                                                 
*                                                                               
* GET X'06' ELEM - REP LOGO FILENAME                                            
*                                                                               
CON18120 DS    0H                                                               
         XC    TWALOGO,TWALOGO     CLEAR FILENAME                               
         MVI   ELCODE,X'06'        GET FILENAME ELEM                            
         LA    R6,RREPREC                                                       
         BAS   RE,GETEL                                                         
         BNE   CON18130                                                         
         MVC   TWALOGO,2(R6)       SAVE FILENAME                                
*                                                                               
* GET X'05' ELEM - SPOTPAK INTERFACE CODES                                      
*                                                                               
CON18130 DS    0H                                                               
         XC    TWASPAG(3),TWASPAG  CLEAR SPOTPAK DATA                           
         MVI   ELCODE,X'05'                                                     
         LA    R6,RREPREC                                                       
         BAS   RE,GETEL                                                         
         BNE   CON19                                                            
         USING RREPSPOT,R6                                                      
         MVC   TWASPAG,RREPSPPC                                                 
         MVC   TWASPMD,RREPMED                                                  
         DROP  R6                                                               
         SPACE 1                                                                
CON19    OC    RREPSPLM,RREPSPLM   IS THERE AN OVERRIDE                         
         BZ    SPLCOMP             NO                                           
         CLC   RREPSPLE,TODAY                                                   
         BL    SPLCOMP             NO                                           
         MVC   TWASPLMN,RREPSPLM   SO USE IT                                    
         B     CON20                                                            
         SPACE 1                                                                
SPLCOMP  GOTO1 DATCON,DMCB,(3,TODAY),(0,WORK)                                   
         L     R4,=F'-14'                                                       
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R4)                                      
         GOTO1 VGTBROAD,DMCB,(1,WORK+6),WORK+12,GETDAY,ADDAY                    
         GOTO1 DATCON,DMCB,(0,WORK+18),(3,WORK)                                 
         MVC   TWASPLMN,WORK                                                    
         B     CON20                                                            
         SPACE 1                                                                
NOREP    LA    R3,337              NO REP RECORD                                
         LA    R2,CONCACTH                                                      
         B     ERROR                                                            
         EJECT                                                                  
*              GET THE CONTRACT                                                 
         SPACE 1                                                                
CON20    DS    0H                                                               
         TM    TWAPROST,X'01'+X'02'                                             
         BNZ   CON25              EXPANDED SCREENS DON'T HAVE THIS FLD!         
         CLC   =C'ADDR',CONSTA                                                  
         BNE   CON25                                                            
         MVC   CONCACT(4),=C'ADDR'                                              
         NI    CONCACTH+4,X'DF'                                                 
         MVI   CONCACTH+5,4                                                     
         SPACE 1                                                                
CON25    CLC   =C'ADDN',CONCACT    TEST FOR SPECIAL ACTION                      
         BE    CON35                                                            
         CLC   =C'ADD',CONCACT                                                  
         BE    CON40                                                            
*                                                                               
         LA    R2,CONCNUMH                                                      
*** COMBO                          NEED TO RE-READ CONTRACT IF                  
         CLI   TWACOMBO,0          PREVIOUS CONTRACT IS A COMBO.                
         BNE   CON28               WE MAY HAVE THE WRONG K IN RCONREC           
*                                  IF WE EXITED WITH ERROR DURING A             
*** COMBO                          COMBO ACTION LOOP IN RECNT01                 
         TM    4(R2),X'20'                                                      
         BO    CON30               NUMBER NOT CHANGE                            
         NI    CONCACTH+4,X'DF'    ACTION MUST BE VALIDATED FOR NEW CON         
         MVI   TWASTAT,0           ALL LOWER ACTIONS ARE INVALID                
CON28    GOTO1 VANY                                                             
         LA    R3,CONERR                                                        
                                                                                
*** SPECIAL TO DISPLAY LAST CONTRACT FOR AN AGENCY                              
         CLC   =C'HI',CONCNUM                                                   
         BNE   CON29                                                            
         XC    WORK,WORK                                                        
         B     CON29A                                                           
***                                                                             
CON29    DS    0H                                                               
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    ERROR                                                            
         SPACE 1                                                                
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)                                               
                                                                                
CON29A   DS    0H                                                               
         XC    RCONREC(32),RCONREC                                              
         MVC   RCONPCON,WORK       NUMBER                                       
         MVC   RCONPREP,REPALPHA                                                
         MVI   RCONPTYP,X'8C'                                                   
         MVC   KEY,RCONREC                                                      
         OI    DMINBTS,X'08'                                                    
         GOTO1 VHIGH                                                            
                                                                                
*** SPECIAL TO DISPLAY LAST CONTRACT FOR AN AGENCY                              
         CLC   =C'HI',CONCNUM                                                   
         BNE   CON29B                                                           
         CLC   KEY(23),KEYSAVE                                                  
         BNE   ERROR                                                            
         B     CON29C                                                           
***                                                                             
CON29B   DS    0H                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERROR                                                            
         SPACE 1                                                                
CON29C   DS    0H                                                               
         MVC   TWAKADDR,KEY+28     SAVE DISK ADDR                               
         MVC   TWACNUM,KEY+23                                                   
         PACK  TWACNUM(1),KEY+26(1)     REVERSE THE COMPLIMENT                  
         PACK  TWACNUM+1(1),KEY+25(1)                                           
         PACK  TWACNUM+2(1),KEY+24(1)                                           
         PACK  TWACNUM+3(1),KEY+23(1)                                           
                                                                                
CON30    MVC   KEY+28(4),TWAKADDR                                               
         OI    DMINBTS,X'08'                                                    
         GOTO1 VGETREC,DMCB,RCONREC                                             
         TM    RCONCNTL,X'80'                                                   
         BO    ERROR                                                            
                                                                                
*** SPECIAL TO DISPLAY LAST CONTRACT FOR AN AGENCY                              
         CLC   =C'HI',CONCNUM                                                   
         BNE   CON33                                                            
         ZAP   WORK+20(5),=P'0'    EDIT USES FIRST 17 BYTES OF WORK             
         MVO   WORK+20(5),RCONKCON                                              
         EDIT  (P5,WORK+20),(8,CONCNUM),ALIGN=LEFT                              
         OI    CONCNUMH+6,X'80'    XMIT FIELD                                   
*                                                                               
CON33    DS    0H                                                               
         XC    WORK,WORK           BULD PAR SECURITY CHECK                      
         LA    R1,WORK                                                          
         USING SBLOCK,R1                                                        
         MVC   SBOFFICE(2),RCONKOFF                                             
         MVC   SBSTATN(5),RCONKSTA                                              
         MVC   SBSALES(3),RCONSAL                                               
         OI    SBBREAKS,STABREAK+OFFBREAK+SALBREAK                              
         DROP  R1                                                               
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         L     R1,AFACILS                                                       
         L     R3,0(R1)            ATIO                                         
         GOTO1 (RFCKSEC,VREPFACS),DMCB,WORK,CONMSGH,(R3),DUB                    
         BE    CON40                                                            
         LA    R3,860                                                           
         LA    R2,CONCNUMH                                                      
         OI    6(R2),X'40'+X'80'                                                
         B     ERROR                                                            
*                                                                               
* SPECIAL ACTION ADDN TO ALLOW RAR TO PRE-ASSIGN CONTRACT NUMBERS               
CON35    LA    R2,CONCACTH                                                      
         LA    R3,INVINP                                                        
         CLC   REPALPHA,=C'UV'     TEST FOR UNIVISION                           
         BNE   ERROR                                                            
         LA    R2,CONCNUMH                                                      
         GOTO1 SCANNER,DMCB,(R2),(1,RCONREC),0                                  
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         LA    R4,RCONREC          POINT TO SCANNER BLOCK                       
         CLI   0(R4),0                                                          
         BE    ERROR                                                            
         CLI   0(R4),8             K NUMBER UP TO 8 DIGITS                      
         BH    ERROR                                                            
         TM    2(R4),X'80'         TEST FOR NUMERIC INPUT                       
         BZ    ERROR                                                            
         ICM   R0,15,4(R4)                                                      
         CVD   R0,DUB                                                           
         CP    DUB,MAXNUM         TEST IF WITHIN LIMITS FOR PRE-ASSIGNS         
         BNL   ERROR                                                            
         CP    DUB,MINNUM                                                       
         BL    ERROR                                                            
         SRP   DUB+3(5),1,0        SHIFT DIGITS 1 TO LEFT                       
         MVC   INTCONNO,DUB+3      SAVE K NUM AS PWO                            
         SRP   DUB+3(5),64-1,0     RESTORE PACKED NUMBER                        
         SPACE                                                                  
CON36    ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5) FIND NINES COMPLEMENT OF K NUM               
         MVO   WORK(5),WORK+10(5)                                               
         XC    RCONREC(32),RCONREC                                              
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,REPALPHA                                                
         MVC   RCONPCON,WORK       CHECK IF NUMBER IS ON FILE                   
         MVC   KEY,RCONREC                                                      
         OI    DMINBTS,X'08'                                                    
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CON37               NOT ON FILE                                  
         LA    R3,338              NUMBER ALREADY ON FILE                       
         B     ERROR                                                            
         SPACE                                                                  
CON37    MVC   CONCACT,MYSPACES                                                 
         MVC   CONCACT(4),=C'ADDN'                                              
         MVI   5(R2),4                                                          
         NI    4(R2),X'F0'                                                      
         OI    4(R2),X'04'                                                      
         OI    6(R2),X'80'                                                      
         B     CON40                                                            
         SPACE 2                                                                
MINNUM   DC    PL8'00000001'                                                    
MAXNUM   DC    PL8'00100000'                                                    
         EJECT                                                                  
*              LOOK AT THE CONTRACT ACTION                                      
         SPACE 1                                                                
CON40    DC    0H'0'                                                            
         XC    TWACOMBO,TWACOMBO   COMBO CONTRACT INDICATOR                     
*                                                                               
         MVI   TWACMTRD,0          CLEAR COMBO TRADE FLAG                       
*                                                                               
         XC    TWACMBPT,TWACMBPT   DEFAULT TO NOT COMBO                         
         CLC   =C'ADD',CONACT                                                   
         BE    CON50                                                            
*                                                                               
         LA    R6,RCONREC          PRESENCE OF A 17 ELEMENT MEANS THE           
         MVI   ELCODE,X'17'        CONTRACT IS A COMBO CONTRACT                 
         BAS   RE,GETEL                                                         
         BNE   CON50                                                            
*                                                                               
         ZIC   RF,1(R6)            GET LENGTH OF ELEMENT                        
         S     RF,=F'2'            SUBT ELEMENT CODE AND LENGTHBYTES            
         SR    RE,RE                                                            
         D     RE,=F'9'            DIVIDE BY LENGTH OF ONE COMBO ENTRY          
         STC   RF,TWACOMBO         =NUMBER OF COMBO COMPONENTS                  
*                                                                               
         ZIC   RF,TWACOMBO         SAVE OFF COMBO COMPONENT K'S IN TWA          
         XC    TWACMBS1(36),TWACMBS1                                            
         LA    RE,TWACMBS2                                                      
         LA    R6,2(R6)                                                         
*                                                                               
CON43    CLC   RCONKSTA,0(R6)      SORT BY CURRENTLY DISPLAYED COMBO            
         BNE   CON45                 FIRST                                      
         MVC   TWACMBS1(9),0(R6)                                                
         B     CON48                                                            
*                                                                               
CON45    DS    0H                                                               
         MVC   0(9,RE),0(R6)       MOVE COMBO K STA AND #S                      
         LA    RE,9(RE)                                                         
CON48    LA    R6,9(R6)                                                         
         BCT   RF,CON43                                                         
*                                                                               
CON50    DC    0H'0'                                                            
         GOTO1 VLOAD,DMCB,(X'01',0)                                             
         GOTO1 =A(DELCFC),DMCB,(RC),RR=Y                                        
         B     EXXMOD                                                           
         LTORG                                                                  
       ++INCLUDE REAVNLUID                                                      
         EJECT                                                                  
*              LINKAGE TO COMMON SUB-ROUTINES                                   
         DS    0H                                                               
COMMIN   NTR1  BASE=BASERB                                                      
         L     R9,BASER9                                                        
         SRL   RF,24                                                            
         B     COMTAB(RF)                                                       
         SPACE 1                                                                
COMTAB   B     ADDELEM                                                          
         B     DELELEM                                                          
         B     MOVEREC                                                          
         B     DISMSG                                                           
         B     FOUTBLK                                                          
         B     CHECKEL                                                          
         B     GETMNDY                                                          
         B     LOAD                                                             
         B     LDSCRN                                                           
         B     ANY                                                              
         B     ANY2                                                             
         B     PACK                                                             
         B     MOVE                                                             
         B     READ                                                             
         B     SEQ                                                              
         B     HIGH                                                             
         B     ADD                                                              
         B     WRITE                                                            
         B     GETREC                                                           
         B     PUTREC                                                           
         B     ADDREC                                                           
         B     CKTAREQ                                                          
         B     BERROR                                                           
         B     BEXIT                                                            
         B     CLEAR                                                            
COMMEND  EQU   (*-COMTAB)/4                                                     
* PLACE LTORG HERE TO INSURE RB'S ADDRESSIBILITY TO LTORG IN NTR1               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD ELEMENT                                           
*                                                                               
*              PARAMETER 1 =       A(RECORD)                                    
*              PARAMETER 2 =       A(ELEMENT TO BE INSERTED)                    
*              ELEMENT IS ADDED IMMEDIATELY BEFORE HIGHER ELEM OR END           
         SPACE 1                                                                
ADDELEM  EQU   *                                                                
         LR    R8,R2               A(FLD HEADER)                                
         L     R2,0(R1)                                                         
         L     R6,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),0(R2),0(R6),=C'ADD=CODE'           
         TM    DMCB+12,X'05'       REC TOO LONG                                 
         BZ    ADEL05                                                           
         LR    R2,R8                                                            
                                                                                
         LA    RF,64(RA)           CHECK IF CALLER SET R2 TO A                  
         CR    R2,RF               VALID FIELD HEADER ADDRESS                   
         BL    ADEL02              IF NOT, FORCE CURSOR TO CONCACTH             
         LA    RF,3520(RA)                                                      
         CR    R2,RF               MAX SIZE OF SCREEN DSECT                     
         BNH   ADEL03                                                           
                                                                                
ADEL02   DS    0H                                                               
         LA    R2,CONCACTH                                                      
                                                                                
ADEL03   DS    0H                                                               
         LA    R3,339              RECORD FULL - CHANGE NOT PROCESSED           
         GOTO1 GETTXT,DMCB,(R3),0,(C'E',0),0,0,0                                
         DC    H'0',C'$ABEND'      UNWIND THIS TRANSACTION                      
***      B     ERROR                                                            
ADEL05   EQU   *                                                                
*                                                                               
*- IF THIS IS CONTRACT RECORD, MAKE SURE COMMENTS DON'T EXCEED MAX.             
         CLI   0(R2),X'0C'                                                      
         BNE   ADEL99                                                           
*                                                                               
*- ONLY ADD UP COMMENTS IF WE ADDING A COMMENT TO RECORD.                       
*  (ALLOWS CHANGES TO EXISTING RECORDS)                                         
         LA    R1,CMTEL                                                         
         LA    R0,#CMTEL                                                        
ADEL10   CLC   0(1,R1),0(R6)       TBL -VS- ELEM WE ADDED.                      
         BE    ADEL15                                                           
         LA    R1,1(R1)            NEXT TBL ENTRY                               
         BCT   R0,ADEL10                                                        
         B     ADEL99              NOT A COMMENT ELEM                           
*                                                                               
ADEL15   EQU   *                                                                
         SR    R0,R0               COUNT LENGTH OF ALL COMMENTS HERE            
         LA    RE,34(R2)           A(1ST ELEM)                                  
ADEL20   CLI   0(RE),0                                                          
         BE    ADEL80              END OF RECORD.                               
*                                                                               
*- ACCUMULATE ELEMENT DATA SIZE IF ELEMENT CODE FOUND IN LIST.                  
         LA    R1,#CMTEL           NUMBER OF 1 BYTE ELEMENT CODES.              
         BAS   R3,ADEL40                                                        
CMTEL    DC    X'02'               CONTRACT COMMENT                             
         DC    X'07'               SPL COMMENT                                  
         DC    X'11'               SAR/BOP COMMENT                              
         DC    X'82'               REP ORDER COMMENT                            
         DC    X'92'               STA ORDER COMMENT                            
#CMTEL   EQU   *-CMTEL             INSERT NEW EL CODES ABOVE THIS               
*                                                                               
ADEL40   CLC   0(1,R3),0(RE)       TBL -VS- RECORD EL CODE                      
         BNE   ADEL50                                                           
         ZIC   RF,1(RE)            EL LEN                                       
         BCTR  RF,0                                                             
         BCTR  RF,0                LESS 2 = DATA LENGTH                         
         AR    R0,RF               ADD TO RUNNING LENGTH                        
         B     ADEL60                                                           
ADEL50   LA    R3,1(R3)            NEXT CODE IN LIST                            
         BCT   R1,ADEL40                                                        
ADEL60   EQU   *                   GET NEXT EL IN REC.                          
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     ADEL20                                                           
*                                                                               
*  EXCEEDED MAX?                                                                
ADEL80   LA    R1,2001             MAX+1                                        
         CR    R1,R0                                                            
         BH    ADEL99              COMMENT SIZE OK.                             
*                                                                               
         BCTR  R1,0                R1=200                                       
         SR    R0,R1               R0 = AMOUNT OVER 200                         
         ST    R0,FULL             SAVE FOR EDITING                             
*                                                                               
         EDIT  (4,FULL),(3,TEMP),ALIGN=LEFT                                     
         LR    R4,R0                                                            
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+2(2),=H'346'                                                
         GOTO1 GETTXT,DMCB,,,(C'E',0),((R4),TEMP),(X'44',0)                     
         OI    6(R8),X'40'         PUT CURSOR HERE                              
         L     RD,BASERD           INSTANT STACK UNWIND                         
*                                                                               
ADEL99   EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*              ROUTINE TO DELETE ELEMENT                                        
*                                                                               
*              THIS ROUTINE DELETES ALL ELEMENTS WITH GIVEN CODE                
*              PARAMETER 1 =       BYTE  0   = ELEMENT CODE TO DELETE           
*                                  BYTES 1-3 = A(RECORD)                        
         SPACE 1                                                                
DELELEM  L     R2,0(R1)            A(RECORD)                                    
         ZIC   R3,0(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),((R3),(R2)),0                      
         B     EXXMOD                                                           
         EJECT                                                                  
*              ROUTINE TO MOVE RECORD FROM 1 AREA TO ANOTHER                    
*                                                                               
*              PARAMETER 1 =       A(FROM RECORD AREA)                          
*              PARAMETER 2 =       A(TO RECORD AREA)                            
         SPACE 1                                                                
MOVEREC  DS    0H                                                               
         L     R2,0(R1)            'FROM' ADDRESS                               
         LH    R3,27(R2)           'FROM' LENGTH                                
         L     RE,4(R1)            'TO' ADDRESS                                 
         LR    RF,R3               'TO' LENGTH = 'FROM' LENGTH                  
         MVCL  (RE),(R2)                                                        
         L     R2,4(R1)                                                         
         LH    R3,27(R2)                                                        
         AR    R2,R3                                                            
         MVI   0(R2),0                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*  ROUTINE TO DISPLAY INFORMATIONAL MESSAGES USING GETTXT                       
*  P1    BYTE  0       INDEX                                                    
*        BYTE  1       0                                                        
*        BYTE  2-3     MESSAGE NUMBER (BYTE 2=0 IF 1 BYTE MSG NO)               
*  P2                  0                                                        
*  P3                  0                                                        
*  P4    BYTE  0       LEN OF TXT STR TO APPEND TO MSG                          
*        BYTE  1-3     A(TEXT)                                                  
*                                                                               
*                                                                               
DISMSG   DS    0H                                                               
         OC    DMCB+2(2),DMCB+2                                                 
         BZ    EXXMOD                                                           
         GOTO1 GETTXT,DMCB,,0,(C'I',0),,X'44',0                                 
         B     EXXMOD                                                           
         EJECT                                                                  
*              ROUTINE TO FOUT BLANKS                                           
         SPACE 1                                                                
*              PARAMETER  1        A(FIRST FIELD)                               
*              PARAMETER  2        A(END-ADDR)  EX=BUYLAST                      
*              PARAMETER  3        0=DO NOT FOUT IF SPACES                      
*                                  1=FOUT IF NOT SPACES                         
         SPACE 1                                                                
FOUTBLK  LM    R2,R4,0(R1)                                                      
         MVI   WORK2,C' '                                                       
         MVC   WORK2+1(79),WORK2                                                
         SPACE 1                                                                
FOUT1    ZIC   RE,0(R2)                                                         
         TM    1(R2),X'20'         PROTECTED                                    
         BO    FOUT9                                                            
         SPACE 1                                                                
         LR    RF,RE                                                            
         SH    RF,=H'9'                                                         
         TM    1(R2),X'02'         EXTENDED FLD HDR?                            
         BZ    *+8                                                              
         SH    RF,=H'8'                                                         
         SPACE 1                                                                
         EX    RF,ORSPAC                                                        
         EX    RF,COMPSPAC                                                      
         BE    FOUT9               ALREADY                                      
         LTR   R4,R4                                                            
         BP    *+8                 SENDING  NON MYSPACES DATA                   
         EX    RF,MOVESPAC         CLEARING SCREEN                              
         FOUT  (R2)                                                             
         SPACE 1                                                                
FOUT9    LA    R2,0(RE,R2)         NEXT FIELD                                   
         CR    R2,R3               LAST FIELD                                   
         BL    FOUT1                                                            
         B     EXXMOD                                                           
         SPACE 1                                                                
ORSPAC   OC    8(0,R2),WORK2                                                    
COMPSPAC CLC   8(0,R2),WORK2                                                    
MOVESPAC MVC   8(0,R2),WORK2                                                    
         EJECT                                                                  
*              ROUTINE TO CHECK ELEMENT CHANGES                                 
         SPACE 1                                                                
* ROUTINE TO CHECK IF ELEMENT      P1=A(NEW RECORD) BYTE 0=ELEM CODE            
* CHANGES HAVE OCCURRED            P2=A(OLD RECORD)                             
*                                  P3=A(CHANGE CODE) ZEROED IF NOT              
*                                                    CHANGED                    
*                                       -FOR DAY-TIME CHANGES IN BUYREC         
*                                        ONLY 'D' OR 'T' IS CHECKED             
         SPACE 1                                                                
CHECKEL  L     R2,0(R1)            NEW REC                                      
         L     R3,4(R1)            OLD REC                                      
         SR    R4,R4                                                            
         L     R5,8(R1)            CHANGE CODE                                  
         LA    R2,34(R2)           1ST ELEM                                     
         LA    R3,34(R3)                                                        
*                                                                               
CK50     CLI   0(R2),0             LAST                                         
         BNE   CK100                                                            
*                                                                               
* LAST NEW ELEM - CHECK IF ANY IN OLD REC LEFT                                  
CK65     CLI   0(R3),0             LAST OLD ELEM?                               
         BNE   CK75                                                             
         MVI   0(R5),0             NO CHANGE IND                                
CKXIT    B     EXXMOD                                                           
*                                                                               
CK75     CLC   0(1,R3),0(R1)       OLD ELEM HIT?                                
         BE    CKXIT                                                            
         IC    R4,1(R3)                                                         
         AR    R3,R4               NEXT OLD ELEM                                
         B     CK65                                                             
*                                                                               
CK100    CLC   0(1,R2),0(R1)       NEW REC ELEM CODE                            
         BE    CK200                                                            
*                                                                               
CK150    IC    R4,1(R2)                                                         
         LA    R2,0(R4,R2)         NEXT NEW ELEM                                
         B     CK50                                                             
* CHECK OLD REC FOR THIS ELEM                                                   
CK200    CLI   0(R3),0                                                          
         BE    CKXIT               NOT FOUND                                    
*                                                                               
         CLC   0(1,R3),0(R1)                                                    
         BE    CK300                                                            
*                                                                               
         IC    R4,1(R3)                                                         
         LA    R3,0(R4,R3)         NEXT OLD ELEM                                
         B     CK200                                                            
* OLD ELEM FOUND                                                                
CK300    CLI   0(R5),C'D'          DAYS?                                        
         BNE   CK400                                                            
* CHECK DAY FIELD OF X'02' ELEM                                                 
         CLC   2(2,R2),2(R3)       NEW V OLD DAYS                               
         BNE   CKXIT                                                            
         B     CK600                                                            
*                                                                               
CK400    CLI   0(R5),C'T'          TIMES?                                       
         BNE   CK500                                                            
         CLC   4(4,R2),4(R3)       NEW V OLD TIME                               
         BNE   CKXIT                                                            
         B     CK600                                                            
* CHECK NEW V OLD ELEM                                                          
CK500    IC    R4,1(R3)            NEW ELEM LEN                                 
         BCTR  R4,R0                                                            
         EX    R4,CK700            COMPARE ELEMENTS                             
         BNE   CKXIT                                                            
CK600    IC    R4,1(R3)                                                         
         LA    R3,0(R4,R3)         NEXT OLD ELEM                                
         B     CK150                                                            
*                                                                               
CK700    CLC   0(0,R2),0(R3)                                                    
         EJECT                                                                  
* CHECK FOR TURN-AROUND CONTRACT REQUEST (TAREQ NOT ZERO)                       
CKTAREQ  TM    TAREQ,X'FF'         T/A REQ IND                                  
         BZ    EXXMOD                                                           
         SPACE 1                                                                
         CLC   =C'CF ',CONACT     IF CF, ACE OR TWX/FAX                         
         BE    TA03A               CAN GET TA IF PROFILE SET                    
         CLC   =C'CF*',CONACT     IF CF*                                        
         BE    TA03                CAN GET TA IF PROFILE SET                    
         CLC   =C'CONX',CONACT     OKAY FOR CONX                                
         BE    TA03                CAN GET TA IF PROFILE SET                    
         CLC   =C'CFX',CONACT      ALSO CHECK FOR CFX                           
         BE    TA03                                                             
         CLC   =C'DONE',CONACT     ALSO CHECK FOR DONE                          
         BNE   TA05                                                             
*                                                                               
* SPECIAL FOR RADIO WEB CONFIRMATION. ALL T/A WILL BE DESTINED TO THE           
* ORIGINATING REP'S PQ IF CONTRACT WAS CONFIRMED BY THE STATION VIA WEB         
*                                                                               
TA03A    DS    0H                                                               
         CLI   TWAACCS,C'$'        AM I A STATION?                              
         BNE   TA03                                                             
         TM    TWASTAOB,X'08'      WEB CONFIRM ENABLED FOR THIS STA?            
         BZ    TA03                                                             
         CLI   RCONKSTA+4,C' '     ONLY RADIO FOR NOW                           
         BE    TA03                                                             
         XC    WORK2(26),WORK2                                                  
         MVI   WORK2+10,11         REQUEST NUMBER                               
         MVI   WORK2+14,106        ORIGIN ID NUMBER                             
         B     TA50                                                             
*                                                                               
*                                                                               
TA03     DS    0H                                                               
         TM    PROFILES+CNTCONFB,CNTCONFA                                       
         BO    TA10                CAN GET PROFILE IF BIT SET                   
         TM    PROFILES+CNTPORGB,CNTPORGA                                       
         BO    TA30                CONTRACT PROFILE BIT 23 SET ?                
*                                                                               
TA05     DS    0H                                                               
         TM    RCONMODR+1,X'C0'    ACE/GRAPHNET NEVER GET T/A                   
         BNZ   EXXMOD                                                           
*                                                                               
TA10     DS    0H                  CHECK IF INTEREP SUBSIDIARY                  
*                                                                               
TA20     CLC   RCONKOFF,=C'NY'     NEW YORK OFFICES GET T/A                     
         BE    TA25                                                             
*                                                                               
TA23     DS    0H                  BUT CHECK IF                                 
         TM    PROFILES+CNTPORGB,CNTPORGA                                       
         BO    TA30                CONTRACT PROFILE BIT 23 SET ?                
         B     EXXMOD                                                           
*                                                                               
* CREATE  REQUEST HEADER                                                        
*                                                                               
TA25     DS    0H                                                               
         XC    WORK2(26),WORK2                                                  
         MVI   WORK2+10,11         REQUEST NUMBER                               
         MVI   WORK2+14,106        ORIGIN ID NUMBER                             
*                                                                               
* FOR KATZ TV, NY CONTRACTS PRINT AT DDS AND GET SHIPPED TO RCONSSID            
* IN THE X'20' SEND ELEMENT                                                     
*                                                                               
* SAME THING FOR SELTEL TV                                                      
* AND FOR FOX STATION SALES                                                     
*                                                                               
         CLC   =C'MR',TWARMAST                                                  
         BE    TA28                                                             
         CLC   =C'SZ',TWAAGY                                                    
         BE    TA28                                                             
         CLC   =C'FB',TWAAGY                                                    
         BE    TA28                                                             
         CLC   =C'UT',TWAAGY       SPECIAL FOR UTS                              
         BE    TA40                                                             
         B     TA60                                                             
*                                                                               
TA28     DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   TA60                                                             
         USING RCONSEND,R6                                                      
RQHD     USING RQHIUSR,WORK2                                                    
         MVC   RQHD.RQHORIG,RCONSSID   ORIGIN                                   
         OI    RQHD.RQHFLAG,RQHFLNK+RQHFTWO                                     
         B     TA60                                                             
         DROP  RQHD                                                             
         DROP  R6                                                               
*                                                                               
TA30     DS    0H                                                               
         XC    WORK2(26),WORK2                                                  
         MVI   WORK2+10,11         REQUEST NUMBER                               
         MVI   WORK2+14,106        ORIGIN ID NUMBER                             
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   TA60                                                             
         USING RCONSEND,R6                                                      
RQHD     USING RQHIUSR,WORK2                                                    
*                                                                               
* TEMPORARY HARD CODING FOR NPTVNY. ALL CONFIRMATION BY NPTVNY WILL             
* PRINT AT DDS REGARDLESS OF THE SALESPERSON'S OFFICE                           
*                                                                               
         CLC   RCONSSID,=AL2(6614)                                              
         BNE   TA40                                                             
         MVC   RQHD.RQHORIG,RCONSSID   ORIGIN                                   
         OI    RQHD.RQHFLAG,RQHFLNK+RQHFTWO                                     
         B     TA60                                                             
*                                                                               
* PUT T/A TO REP'S PQ                                                           
TA40     DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   TA60                                                             
*                                                                               
         CLI   TWAPRDDS,C'Y'       OFFICE PROFILE TO PRINT AT DDS?              
         BE    TA50                IF SO, SKIP DIRECT CARD AND DEST.            
         MVC   RQHD.RQHOUT,=C'DIRECT'                                           
         MVC   RQHD.RQHDEST,RCONSSID   DESTINATION                              
*                                                                               
TA50     DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   TA60                                                             
*                                                                               
         MVC   RQHD.RQHORIG,RCONSSID   ORIGIN                                   
*                                                                               
* THE RQHFLNK BIT CAUSES DATA MANAGER NOT TO OVERRIDE ORIGIN ID                 
* WE NEED THIS SINCE WE WANT TO 'LUMP' THE REQUESTS TOGETHER BEFORE             
* DIRECTING THEM TO THE REP'S PRINT QUEUE                                       
*                                                                               
         OI    RQHD.RQHFLAG,RQHFLNK+RQHFTWO                                     
         DROP  RQHD                                                             
         DROP  R6                                                               
*                                                                               
TA60     DS    0H                                                               
         MVI   WORK2+26,C' '                                                    
         MVC   WORK2+27(79),WORK2+26                                            
*                                                                               
         USING REQD,R8                                                          
TA75     LA    R8,WORK2+26                                                      
         MVC   QPROG,=C'11'                                                     
*                                                                               
TA78     DS    0H                                                               
         MVC   QREP,REPALPHA                                                    
* FIND ORIGINATING OFFICE                                                       
         MVC   QREQOFF,RCONKOFF                                                 
*                                                                               
         CLC   =C'O=',TWAACCS                                                   
         BNE   *+10                                                             
         MVC   QREQOFF,TWAACCS+2   ORIGINATING OFFICE                           
*                                                                               
TA150    MVC   QUESTOR,RCONBUYR                                                 
         MVC   QOFFICE,RCONKOFF                                                 
         MVC   QSTATION,RCONKSTA                                                
         MVC   QDIV(2),RCONTEM                                                  
         CLI   QDIV,C'R'                                                        
         BE    *+10                FOR TV NEED CONTRACTS BY                     
         MVC   QDIV(2),RCONKGRP    GROUP/SBGROUP SO MERZ CAN COUNT              
*                                  BY AFFILIATE                                 
         MVC   QMAN,RCONSAL                                                     
         MVC   QAGENCY(6),RCONKAGY                                              
         MVC   QADV,RCONKADV                                                    
         MVI   QSEQ,C'1'                                                        
         UNPK  QCONT(8),RCONKCON(5)                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REPREQ',WORK2,WORK2,          X        
               (TERMNAL,0)                                                      
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   QPROG,=C'11'        CHECK SECOND T/A FOR 11'S ONLY               
         BNE   EXXMOD                                                           
         TM    TWAPRFA,X'40'       CHECK CONTYPE ADDTL OPTION #2                
         BZ    EXXMOD              NOT ON - SKIP NO RATE COPY                   
* CREATE SECOND T/A                                                             
TA175    MVI   QOPTION1,C'N'                                                    
         BASR  RE,RF                                                            
         TM    DMCB+8,X'FF'                                                     
         BZ    EXXMOD                                                           
         DC    H'0'                                                             
         DROP  R8                                                               
         EJECT                                                                  
* ROUTINE TO GET PREVIOUS          P1=A(3-BYTE YMD INPUT)                       
* MONDAY-WEEK DATE                 P2=A(3-BYTE YMD OUTPUT)                      
         SPACE 1                                                                
GETMNDY  L     R2,0(R1)            INPUT                                        
         L     R3,4(R1)            OUTPUT                                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,(R2)),DUB                                         
* GET DAY OF WEEK                                                               
         GOTO1 GETDAY,(R1),DUB,FULL                                             
         CLC   FULL(3),MYSPACES                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    R4,R4                                                            
         IC    R4,DMCB             DAY OF WEEK                                  
         BCTR  R4,R0                                                            
         LNR   R4,R4                                                            
         GOTO1 ADDAY,(R1),DUB,DMCB+12,(R4)                                      
         GOTO1 DATCON,(R1),DMCB+12,(3,(R3))                                     
         B     EXXMOD                                                           
         EJECT                                                                  
*              ROUTINE TO LOAD AND GO TO OVERLAY                                
         SPACE 1                                                                
*              P1  HAS OVERLAY NUMBER                                           
         SPACE 1                                                                
LOAD     ZIC   R2,0(R1)                                                         
         L     R3,4(R1)                                                         
         GOTO1 CALLOV,DMCB,((R2),0),(RA)                                        
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC),(R3)                                              
         B     EXXMOD                                                           
         EJECT                                                                  
*              ROUTINE TO LOAD SCREEN                                           
         SPACE 1                                                                
*              P1   BYTE 0         SCREEN NUMBER                                
*                   BYTE 1-3       LOAD ADDRESS                                 
*              P2   BYTE 0         SCREEN STATUS                                
*                                  X'80' HEADLINE TO BE PROTECTED               
*                                  X'40' COMMENTS ON SCREEN                     
*                                  X'20' FORCE SCREEN RELOAD                    
*                                  X'01' SUB SCREEN IS FULL SIZE SCREEN         
*                   BYTE 1-3       AL3(0)                                       
         SPACE 1                                                                
LDSCRN   MVC   DMWORK(8),0(R1)     SAVE PARAMETERS                              
         TM    DMWORK+4,X'40'                                                   
         BZ    LDSCRN10                                                         
         BAS   R8,YESCOM           COMMENTS MUST BE THERE                       
         B     LDSCRN20                                                         
         SPACE 1                                                                
LDSCRN10 BAS   R8,NOCOM            NO COMMENTS                                  
         SPACE 1                                                                
LDSCRN20 TM    DMWORK+4,X'80'                                                   
         BZ    LDSCRN30                                                         
         BAS   R8,PROTECT          PROTECT IT                                   
         B     LDSCRN40                                                         
         SPACE 1                                                                
LDSCRN30 BAS   R8,UNPROT           UNPROTECT IT                                 
         SPACE 1                                                                
LDSCRN40 CLI   DMWORK,0                                                         
         BE    LDSCRN50                                                         
         BAS   R8,LOWER            LOAD A LOWER SCREEN                          
         TM    DMWORK+4,X'01'      IS LOWER SCREEN A FULL SCREEN?               
         BZ    *+8                 NO                                           
         OI    TWAPROST,X'01'      YES - FLAG IT SO                             
         B     LDSCRN60                                                         
         SPACE 1                                                                
LDSCRN50 BAS   R8,NOLOW                                                         
         SPACE 1                                                                
LDSCRN60 DC    0H'0'                                                            
         CLI   DMWORK,X'FE'        BUY SCREEN SPECIAL CASE                      
         BNE   LDSCRN70                                                         
         FOUT  BUYCLSPH                                                         
         FOUT  BUYCLSH                                                          
         FOUT  BUYUPTLH                 'USE PATTERN'                           
         FOUT  BUYNOTLH                 NOTATION                                
         TM    TWAFLAGS,TWAFLPTQ        PROFILE FOR PATTERN?                    
         BZ    EXXMOD                                                           
         MVC   BUYCLSP(3),=C'Ptn'                                               
         NI    BUYNOTLH+1,X'FF'-X'0C'   DISPLAY NOTATION                        
         NI    BUYUPTLH+1,X'FF'-X'0C'   DISPLAY 'USE PATTERN'                   
         NI    BUYCLSH+1,X'FF'-X'20'    UNPROT CLASS/PATTERN                    
         NI    BUYNOTH+1,X'FF'-X'20'    UNPROT NOTATION                         
         NI    BUYUPTH+1,X'FF'-X'20'    UNPROT 'USE PATTERN'                    
         B     EXXMOD                                                           
LDSCRN70 DC    0H'0'                                                            
         CLI   DMWORK,X'DE'        BUY SCREEN SPECIAL CASE                      
         BNE   EXXMOD                                                           
         TM    PROFILES+CNTAVNFB,CNTAVNFA 'ANTI-AVN' FIELD PROFILE              
         BZ    LDSCRN80                                                         
         CLI   TWAACCS,C'$'             STATION?                                
         BE    LDSCRN80                                                         
         GOTO1 =A(AVNFLD),RR=Y                                                  
LDSCRN80 FOUT  BU2CLSPH                                                         
         FOUT  BU2UPTLH                 'USE PATTERN'                           
         FOUT  BU2CLSH                 'USE PATTERN'                            
         FOUT  BU2NOTLH                 NOTATION                                
         TM    TWAFLAGS,TWAFLPTQ        PROFILE FOR PATTERN?                    
         BZ    EXXMOD                                                           
         MVC   BU2CLSP(3),=C'Ptn'                                               
         NI    BU2NOTLH+1,X'FF'-X'0C'   DISPLAY NOTATION                        
         NI    BU2UPTLH+1,X'FF'-X'0C'   DISPLAY 'USE PATTERN'                   
         NI    BU2CLSH+1,X'FF'-X'20'    UNPROT CLASS/PATTERN                    
         OI    BU2CLSH+6,X'80'                                                  
         NI    BU2NOTH+1,X'FF'-X'20'    UNPROT NOTATION                         
         OI    BU2NOTH+6,X'80'                                                  
         NI    BU2UPTH+1,X'FF'-X'20'    UNPROT 'USE PATTERN'                    
         OI    BU2UPTH+6,X'80'                                                  
         B     EXXMOD                                                           
         EJECT                                                                  
YESCOM   TM    TWAPROST,X'40'                                                   
         BOR   R8                  ALREADY HAVE COMMENTS                        
         GOTO1 CALLOV,DMCB,CONSHRTH,X'D90802F4'                                 
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         OI    TWAPROST,X'40'      COMMENTS                                     
         OI    TWAPROST,X'80'      PROTECTED                                    
         OI    TWAPROST,X'20'      CHANGED                                      
         MVI   TWASCRN,X'FF'                                                    
         LA    R3,CONBACTH-TWATASK                                              
         ST    R3,ABUYFH           A(BUY ACTION FIELD HEADER)                   
         LA    R3,CONLAST-TWATASK                                               
         ST    R3,ALSTFH           A(END OF ACREEN)                             
         AR    R3,RA                                                            
         MVC   0(3,R3),=X'000101'  SEND ENTIRE SCREEN                           
         BR    R8                                                               
         SPACE 1                                                                
NOCOM    TM    TWAPROST,X'40'                                                   
         BZR   R8                  DON'T HAVE COMMENTS                          
         LA    R3,CONSHRTH-TWATASK                                              
         ST    R3,ALSTFH           END OF SCREEN IS AFTER TYPE                  
*                                                                               
         CLI   TWASCRN,X'DE'                                                    
         BNE   *+8                 SPECIAL EXTENDED BUY SCREEN                  
         LA    R3,BU2BACTH-TWATASK                                              
*                                                                               
         CLI   TWASCRN,X'C8'                                                    
         BE    SETTDTC             SPECIAL EXTENDED TD/TC:COLUMBINE             
         CLI   TWASCRN,X'CA'                                                    
         BNE   *+8                 SPECIAL EXTENDED TD/TC:JDS/2000              
SETTDTC  EQU   *                                                                
         LA    R3,COLBACTH-TWATASK                                              
*                                                                               
         ST    R3,ABUYFH           I HAVE NO BUY ACTION                         
         AR    R3,RA                                                            
         MVC   0(3,R3),=X'000101'  NEW END - SEND IT ALL                        
         SPACE 1                                                                
         MVI   TWASCRN,X'FF'                                                    
         NI    TWAPROST,X'BF'      NO COMMENTS                                  
         OI    TWAPROST,X'20'      CHANGED                                      
         BR    R8                                                               
         EJECT                                                                  
PROTECT  TM    CONBUYH+1,X'20'     IF LAST WAS ADDR BUYER                       
         BO    *+8                 WAS NOT PROTECTED                            
         NI    TWAPROST,X'7F'                                                   
         TM    TWAPROST,X'80'                                                   
         BOR   R8                  ALREADY PROTECTED                            
         LA    R1,FLDLIST                                                       
         SPACE 1                                                                
PROTECT1 CLI   0(R1),X'FF'                                                      
         BE    PROTECT5            END OF LIST                                  
         CLI   0(R1),0                                                          
         BE    PROTECT4            THIS FIELD IS NEVER UNPROTECTED              
         CLI   0(R1),C'C'                                                       
         BNE   PROTECT2            NOT A COMMENT FIELD                          
         TM    TWAPROST,X'40'                                                   
         BZ    PROTECT4            NO COMMENTS ON SCREEN - SKIP IT              
         SPACE 1                                                                
PROTECT2 L     R2,0(R1)                                                         
         SLL   R2,8                                                             
         SRL   R2,8                                                             
         AR    R2,RA               R2 TO FIELD HEADER                           
         TM    1(R2),X'20'                                                      
         BO    PROTECT4            ALREADY PROTECTED                            
         SPACE 1                                                                
         OI    1(R2),X'20'                                                      
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         SH    RF,=H'8'                                                         
         TM    1(R2),X'02'         EXTENDED FLD HEADER?                         
         BZ    *+8                 NO                                           
         SH    RF,=H'8'            YES - SHORTEN OUTPUT LEN                     
         STC   RF,7(R2)                                                         
         SPACE 1                                                                
PROTECT4 LA    R1,4(R1)                                                         
         B     PROTECT1                                                         
         SPACE 1                                                                
PROTECT5 L     R3,ALSTFH                                                        
         AR    R3,RA                                                            
         MVC   0(3,R3),=X'000101'  SEND SCREEN                                  
         OI    TWAPROST,X'80'                                                   
         OI    TWAPROST,X'20'                                                   
         BR    R8                                                               
         EJECT                                                                  
UNPROT   TM    TWAPROST,X'80'                                                   
         BZR   R8                  NOT PROTECTED                                
         LA    R1,FLDLIST                                                       
         SPACE 1                                                                
UNPROT1  CLI   0(R1),X'FF'                                                      
         BE    UNPROT5             END OF LIST                                  
         CLI   0(R1),0                                                          
         BE    UNPROT4             THIS FIELD IS NEVER UNPROTECTED              
         CLI   0(R1),C'C'                                                       
         BNE   UNPROT2             NOT A COMMENT                                
         TM    TWAPROST,X'40'                                                   
         BZ    UNPROT4             NO COMMENTS ON SCREEN                        
         SPACE 1                                                                
UNPROT2  L     R2,0(R1)                                                         
         SLL   R2,8                                                             
         SRL   R2,8                                                             
         AR    R2,RA               R2 TO FIELD HEADER                           
         TM    1(R2),X'20'                                                      
         BZ    UNPROT4             NOT PROTECTED                                
         SPACE 1                                                                
         NI    1(R2),X'FF'-X'20'   UNPROTECT                                    
         FOUT  (R2)                XMIT                                         
         ZIC   RF,0(R2)                                                         
         SH    RF,=H'8'                                                         
         TM    1(R2),X'02'         EXTENDED FLD HEADER?                         
         BZ    *+8                 NO                                           
         SH    RF,=H'8'            YES - SHORTEN OUTPUT LEN                     
         STC   RF,7(R2)                                                         
         SPACE 1                                                                
UNPROT4  LA    R1,4(R1)                                                         
         B     UNPROT1                                                          
         SPACE 1                                                                
UNPROT5  NI    TWAPROST,X'7F'      NOT PROTECTED                                
         OI    TWAPROST,X'20'                                                   
         L     R3,ALSTFH                                                        
         AR    R3,RA                                                            
         MVC   0(3,R3),=X'000101'                                               
         BR    R8                                                               
         EJECT                                                                  
LOWER    DS    0H                                                               
         TM    DMWORK+4,X'20'        FORCE RELOAD?                              
         BO    *+14                                                             
         CLC   TWASCRN,DMWORK      IS IT CORRECT LOWER SCREEN                   
         BE    LOWER30                                                          
         L     R3,DMWORK           LOAD ADDRESS                                 
         SLL   R3,8                                                             
         SRL   R3,8                                                             
         MVC   DMCB+4(3),=X'D90802'                                             
         MVC   DMCB+7(1),DMWORK    SCREEN NUMBER                                
         SPACE 1                                                                
         GOTO1 CALLOV,DMCB,(R3)                                                 
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         MVC   TWASCRN,DMWORK      NEW SCREEN NUMBER                            
         OI    TWAPROST,X'04'                                                   
         SPACE 1                                                                
         L     R2,DMWORK                                                        
         SLL   R2,8                                                             
         SRL   R2,8                                                             
         SPACE 1                                                                
         LA    R1,CONBACTH                                                      
         CR    R2,R1                                                            
         BL    *+6                 THIS SCREEN WIPED-OUT BUY ACTION             
         LR    R2,R1                                                            
         SPACE 1                                                                
LOWER1   TM    1(R2),X'20'                                                      
         BZ    LOWER2                                                           
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     LOWER1                                                           
         SPACE 1                                                                
LOWER2   MVI   5(R2),3                                                          
         MVC   8(3,R2),BUYACT      FIRST UNPROTECTED IS ACTION                  
         OI    4(R2),X'80'                                                      
         FOUT  (R2)                                                             
         SR    R2,RA                                                            
         ST    R2,ABUYFH                                                        
         AR    R2,RA                                                            
         SPACE 1                                                                
LOWER3   ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         TM    1(R2),X'20'         SECOND UNPROTECTED IS BUY NUMBER             
         BO    LOWER3                                                           
         SPACE 1                                                                
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'8'            FIELD LENGTH IN R1                           
         TM    1(R2),X'02'         EXTENDED FLD HDR?                            
         BZ    *+8                 NO                                           
         SH    R1,=H'8'            YES - SHORTEN LEN                            
         ZIC   R4,BYNMLN           LENGTH OF BUY NUMBER                         
         CR    R1,R4                                                            
         BL    *+6                                                              
         LR    R1,R4               USE SMALLER                                  
         CH    R1,=H'3'                                                         
         BNH   *+8                                                              
         LH    R1,=H'3'                                                         
         STC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BNP   *+6                                                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),BUYNUM                                                   
         OI    4(R2),X'80'                                                      
         CLC   BUYNUM,MYSPACES                                                  
         BNE   *+12                                                             
         MVI   5(R2),0                                                          
         MVI   BYNMST,0                                                         
         MVC   4(1,R2),BYNMST                                                   
         MVC   BYNMLN,5(R2)                                                     
         MVC   BUYNUM,8(R2)                                                     
         FOUT  (R2)                                                             
         SPACE 1                                                                
LOWER5   CLI   0(R3),0             FIND END OF SCREEN                           
         BE    LOWER10                                                          
         ZIC   R1,0(R3)                                                         
         AR    R3,R1                                                            
         B     LOWER5                                                           
         SPACE 1                                                                
LOWER10  SR    R3,RA                                                            
         ST    R3,ALSTFH           SAVE ADDRESS OF END                          
         AR    R3,RA                                                            
         L     R2,DMWORK           START OF SCREEN                              
         SLL   R2,8                                                             
         SRL   R2,8                                                             
         SPACE 1                                                                
LOWER15  CR    R2,R3                                                            
         BE    LOWER17             END OF SCREEN                                
         OI    7(R2),X'80'         SEND ONLY BOTTOM - SINCE TOP IS              
         ZIC   R1,0(R2)            UNCHANGED                                    
         AR    R2,R1                                                            
         B     LOWER15                                                          
         SPACE 1                                                                
LOWER17  XC    0(3,R2),0(R2)                                                    
         TM    TWAPROST,X'20'      HAS STATUS CHANGED                           
         BZ    LOWER30                                                          
         MVC   0(3,R3),=X'000101'  SEND ENTIRE SCREEN                           
         SPACE 1                                                                
LOWER30  DC    0H'0'                                                            
         BR    R8                                                               
         EJECT                                                                  
NOLOW    CLI   TWASCRN,X'FF'                                                    
         BER   R8                  I DON'T HAVE LOWER                           
         LA    R3,CONSHRTH                                                      
         TM    TWAPROST,X'40'                                                   
         BZ    *+8                 NO COMMENTS                                  
         LA    R3,CONLAST                                                       
         SPACE 1                                                                
         SR    R3,RA                                                            
         ST    R3,ALSTFH           A(END OF SCREEN)                             
         AR    R3,RA                                                            
         MVC   0(3,R3),=X'000101'                                               
         MVI   TWASCRN,X'FF'                                                    
         BR    R8                                                               
         EJECT                                                                  
*              ANY, ANY2, PACK AND MOVE R2 HAS ADDRESS OF HEADER                
         SPACE 1                                                                
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                MISSING INPUT FLD                            
         B     ERROR                                                            
         SPACE 1                                                                
ANY2     TM    4(R2),X'10'         IS FIELD INVALID                             
         BZ    EXXMOD              ITS VALID                                    
         LA    R3,3                NOT NUMERIC                                  
         B     ERROR                                                            
         SPACE 1                                                                
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    PACKX               LENGTH ERROR                                 
         TM    4(R2),X'08'                                                      
         BZ    PACKX               NON-NUMERIC                                  
         BCTR  R1,0                                                             
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
PACKX    XIT1  REGS=(R0)                                                        
         SPACE 1                                                                
VARPACK  PACK  DUB,8(0,R2)                                                      
         SPACE 1                                                                
MOVE     MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    EXXMOD                                                           
         BCTR  R1,0                                                             
         EX    R1,VARMOVE                                                       
         B     EXXMOD                                                           
         SPACE 1                                                                
VARMOVE  MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
         SPACE 3                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
         SPACE 1                                                                
DIRCTRY  CLI   UPDATE,C'Y'                                                      
         BNE   *+8                                                              
         OI    DMINBTS,X'80'                                                    
         ZIC   R4,DMINBTS                                                       
         ZIC   R3,TERMNAL                                                       
         MVC   KEYSAVE(27),KEY                                                  
         GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPDIR',KEYSAVE,KEY,     X        
               ((R3),0)                                                         
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
*                                                                               
* NEED TO CHECK IF PUTREC IS CALLING DATAMGR WITH A RECORD SIZE GREATER         
* THAN MAXIMUM ALLOWABLE LIMIT. THIS IS NEEDED SINCE THERE ARE HELLO            
* CALLS SCATTERED THROUGH OUT CONTRACT WHICH DOES NOT CHECK IF MAX              
* LENGTH HAS BEEN EXCEEDED.                                                     
*                                                                               
         L     R2,0(R1)                                                         
*                                                                               
         CLI   0(R2),X'0B'         FOR BUY RECORDS ONLY                         
         BNE   PUTREC10                                                         
         CLC   27(2,R2),=AL2(972)                                               
         BNH   FILE                IS RECORD SIZE > 972 BYTES?                  
         B     PUTREC20                                                         
*                                                                               
PUTREC10 DS    0H                                                               
         CLC   27(2,R2),=AL2(3972)                                              
         BNH   FILE                IS RECORD SIZE > 3972 BYTES?                 
*                                                                               
PUTREC20 DS    0H                                                               
         LA    R3,339              REC FULL, CHANGE NOT PROCESSED               
         LA    R2,CONCACTH                                                      
         GOTO1 GETTXT,DMCB,(R3),0,(C'E',0),0,0,0                                
         DC    H'0',C'$ABEND'      UNWIND THIS TRANSACTION                      
***      B     ERROR                                                            
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
*                                                                               
         L     R2,0(R1)                                                         
         CLI   0(R2),X'0B'         FOR BUY RECORDS ONLY                         
         BNE   FILE                                                             
         CLC   27(2,R2),=AL2(972)                                               
         BH    PUTREC20            IS RECORD SIZE > 972 BYTES?                  
*                                                                               
         SPACE 1                                                                
FILE     CLI   UPDATE,C'Y'                                                      
         BNE   *+8                                                              
         OI    DMINBTS,X'80'                                                    
         MVC   AIOAREA,0(R1)                                                    
         LA    R2,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         ZIC   R3,TERMNAL                                                       
         ZIC   R4,DMINBTS                                                       
         L     RF,AIOAREA                                                       
         CLI   0(RF),X'0C'         CONTRACT REC?                                
         BNE   FILE4               NO - SKIP AUDIT ELEM                         
         CLI   COMMAND,C'A'                                                     
         BE    *+12                                                             
         CLI   COMMAND,C'P'                                                     
         BNE   FILE4                                                            
         XC    WORK+38(10),WORK+38                                              
         MVC   WORK+38(4),CONACT                                                
         MVC   WORK+42(3),BUYACT                                                
         MVC   WORK+45(3),BUYNUM                                                
        GOTO1 (RFAUDIT,VREPFACS),DMCB,AIOAREA,(8,ACOMFACS),WORK+38,(RA)         
FILE4    GOTO1 DATAMGR,DMCB,((R4),COMMAND),=C'REPFILE',                X        
               (R2),AIOAREA,((R3),DMWORK)                                       
         SPACE 2                                                                
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVI   DMINBTS,X'00'                                                    
         MVI   UPDATE,C'N'                                                      
         MVC   DMBYTE,DMCB+8                                                    
*                                                                               
* SINCE IOAREA AND RBUYREC AREAS ARE STILL USING 1000-BYTE BUFFERS              
* GETREC MUST BE CAREFUL THAT WE DON'T PICK UP SUPPORT RECORDS THAT             
* HAVE BECOME TOO BIG FOR CONTRACT TO HANDLE. IT IS SAFER FOR CONTRACT          
* TO CATCH IT HERE SO WE CAN IDENTIFY THE OFFENDING RECORD(S)                   
*                                                                               
         CLC   =C'GETREC',COMMAND                                               
         BNE   DMCHK10                                                          
         L     R2,AIOAREA                                                       
         CLC   27(2,R2),=AL2(1000)                                              
         BNH   DMCHK10             IS RECORD SIZE > 1000 BYTES?                 
         CLI   0(R2),X'0C'         YES? ONLY THE CONTRACT CAN BE                
         BE    DMCHK10             > 1000 BYTES                                 
         CLI   0(R2),X'41'         AND DARE RECORDS, TOO                        
         BE    DMCHK10             > 1000 BYTES                                 
         CLI   0(R2),X'43'         AND PROPOSAL RECORDS, TOO                    
         BE    DMCHK10             > 1000 BYTES                                 
         CLI   0(R2),X'49'         AND COVER SHEET RECORDS, TOO                 
         BE    DMCHK10             > 1000 BYTES                                 
         CLI   0(R2),X'51'         AND CONFIRMED DARE RECORDS, TOO              
         BE    DMCHK10             > 1000 BYTES                                 
         LA    R2,CONCACTH         ALL ELSE STOPS HERE                          
         LA    R3,550                                                           
         B     ERROR                                                            
*                                                                               
DMCHK10  DS    0H                                                               
         NC    DMBYTE,DMOUTBTS                                                  
         BZ    EXXMOD                                                           
*                                                                               
         SR    R3,R3                                                            
         B     ERROR                                                            
         EJECT                                                                  
*              ERROR EXIT ROUTINE                                               
         SPACE 1                                                                
BERROR   DS    0H                                                               
*                                                                               
* CHECK IF CONTRACT ENCOUNTERED AN ERROR WHILE IN AUTOGEN MODE                  
* IF WE DID, RETURN TO CALLER WITH THE ERROR NUMBER INSTEAD OF                  
* GOING TO EXIT                                                                 
*                                                                               
         TM    TWAGENFG,TWQGLCON   CON-ACT MODE?                                
         BO    BERROR08            YES                                          
*                                                                               
         TM    TWAGENFG,TWQGOGEN+TWQRTERR                                       
         BNO   BERROR10            SET TO RETURN ERROR TO CALLER?               
*                                                                               
         CLC   =C'ADD',CONCACT     MUST BE DOING AN ADD                         
         BE    BERROR05                                                         
         CLC   =C'CHA',CONCACT     OR CHANGE                                    
         BNE   BERROR10                                                         
*                                                                               
BERROR05 DS    0H                                                               
         XC    WORK2,WORK2         TURN OFF AUTOGEN MODE                        
         NI    TWAGENFG,X'FF'-TWQGOGEN-TWQRTERR                                 
*                                                                               
* CHECK IN CASE WE RUN INTO A LOOP WHERE CONTRACT TRIES TO CALL ITSELF          
* UNTIL WE FIND THE PROBLEM, WE NEED TO EXIT HERE                               
*                                                                               
         CLC   =C'CON',TWAGENCP    SHOULDN'T HAPPEN                             
         BE    BERROR10                                                         
*                                                                               
         LA    R1,WORK2            PASS BACK ERROR NUMBER                       
         USING RCAUTOD,R1                                                       
         STCM  R3,3,RCAUERR#                                                    
*                                                                               
         CLC   =C'ADD',CONCACT     ADD?                                         
         BE    BERROR06            YES - SKIP PASSIN K NUM ON ERROR             
         MVC   RCAUCON#,RCONKCON                                                
         DROP  R1                                                               
*                                                                               
BERROR06 DS    0H                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
                                                                                
         GOTO1 (RF),DMCB,=C'PUTD',WORK2,RCAUELRQ,GLRCAUTO                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,WORK2                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'CON'    CONTRACT PROGRAM                             
         MVC   GLVXTOSY,TWAGENCS   TO THE CALLER'S SYSTEM                       
         MVC   GLVXTOPR,TWAGENCP   TO THE CALLER'S PROGRAM                      
         OI    GLVXFLG1,GLV1RETN+GLV1SEPS   THIS IS A RETURN CALL               
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
                                                                                
         GOTO1 (RF),DMCB,=C'PUTD',WORK2,24,GLVXCTL                              
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    BEXIT                                                            
         DC    H'0'                                                             
*                                                                               
BERROR08 DS    0H                                                               
         TM    TWAGENFG,TWQRTERR                                                
         BZ    BERROR10            SET TO RETURN ERROR TO CALLER?               
*                                                                               
         CLC   =C'CON',TWAGENCP    SHOULDN'T HAPPEN                             
         BE    BERROR10                                                         
*                                                                               
         LTR   R3,R3               ANY ERR MSG?                                 
         BZ    BEXIT                                                            
*                                                                               
         L     RE,ACOMFACS         GET CON-ACT GLOBBER ELEM                     
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,=C'GETD',WORK2,GLCONLNQ,GLRKACT                        
         TM    DMCB+8,X'10'                                                     
         BZ    *+6                                                              
         DC    H'0'                MUST BE AROUND FOR RETURN                    
         GOTO1 (RF),DMCB,=C'DELE',,,GLRKACT                                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE AROUND FOR RETURN                    
         LA    R1,WORK2                                                         
         USING GLCONNUM,R1                                                      
*                                                                               
         XC    GLCONERR,GLCONERR                                                
         STCM  R3,3,GLCONERR       RETURN ERROR NUMBER                          
         DROP  R1                                                               
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,=C'PUTD',WORK2,GLCONLNQ,GLRKACT                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     BEXIT                                                            
*                                                                               
BERROR10 DS    0H                                                               
         CHI   R3,860              PAR ERROR?                                   
         BE    BEXIT               YES - PAR SET ITS OWN MESSAGE                
         GOTO1 GETTXT,DMCB+12,(R3),0,(C'E',DMCB),0,0,0                          
         LTR   R3,R3               IF DATAMGR ERROR, DON'T SET CURPOS           
         BZ    BEXIT                                                            
*                                                                               
         LA    RE,CONCACTH         CHECK IF R2 HAS BEEN INITIALIZED             
         CR    R2,RE                                                            
         BL    BERROR30                                                         
         LA    RE,4095(RA)                                                      
         CR    R2,RE                                                            
         BL    BERROR40                                                         
*                                                                               
BERROR30 DS    0H                                                               
         LA    R2,CONCACTH         IF NOT, DEFAULT TO CON ACT FIELD             
*                                                                               
BERROR40 DS    0H                                                               
         OI    6(R2),X'40'                                                      
BEXIT    DS    0H                                                               
*                                                                               
         GOTO1 =A(DELCFC),DMCB,(RC),RR=Y   BLOW AWAY CFC IF NEEDED              
         L     RD,BASERD                                                        
         B     EXXMOD              RETURN TO USER                               
         EJECT                                                                  
*              ROUTINE TO CLEAR CONTRACT HEADLINE FIELDS                        
         SPACE 1                                                                
*              PARM 1   0=FOUT MYSPACES                                         
*                       1= FOUT NON-MYSPACES                                    
         SPACE 1                                                                
CLEAR    MVI   WORK2,C' '                                                       
         MVC   WORK2+1(79),WORK2                                                
         L     R3,0(R1)                                                         
         LA    R1,FLDLIST          SET A(CONTRACT HEADER FIELD LIST)            
         SPACE 1                                                                
CLEAR2   CLI   0(R1),X'FF'                                                      
         BE    EXXMOD              END OF LIST                                  
         CLI   0(R1),C'C'          COMMENT FIELD?                               
         BNE   CLEAR3              NO                                           
         TM    TWAPROST,X'40'      YES                                          
         BZ    CLEAR10             NO COMMENTS SKIP FIELD                       
         SPACE 1                                                                
CLEAR3   L     R2,0(R1)            LOAD A(FIELD)                                
         SLL   R2,8                STRIP OFF FIELD TYPE INDICATOR               
         SRL   R2,8                                                             
         AR    R2,RA               SET ADDRESSABILITY                           
         ZIC   RF,0(R2)            SET FIELD LENGTH                             
         SH    RF,=H'9'               - L(CONTROL) + 1 FOR EX STAT              
         TM    1(R2),X'02'         EXTENDED FLD HEADER?                         
         BZ    *+8                 NO                                           
         SH    RF,=H'8'            YES   - 1 FOR 'EX' LEN                       
         EX    RF,ORSPA                                                         
         EX    RF,COMSPA                                                        
         BE    CLEAR10             ALREADY MYSPACES                             
         LTR   R3,R3                                                            
         BP    CLEAR4              FOUT NON-MYSPACES                            
         EX    RF,MOVESPA                                                       
         SPACE 1                                                                
CLEAR4   FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         SH    RF,=H'8'                                                         
         TM    1(R2),X'02'         EXTENDED FLD HEADER?                         
         BZ    *+8                 NO                                           
         SH    RF,=H'8'            YES - SHORTEN OUTPUT LEN                     
         STC   RF,7(R2)                                                         
         SPACE 1                                                                
CLEAR10  LA    R1,4(R1)                                                         
         B     CLEAR2                                                           
         SPACE 1                                                                
ORSPA    OC    8(0,R2),WORK2                                                    
COMSPA   CLC   8(0,R2),WORK2                                                    
MOVESPA  MVC   8(0,R2),WORK2                                                    
         EJECT                                                                  
*                                                                               
*   FLDLIST:  BYTE 1   =  FIELD TYPE                                            
*                       C'U'    =  UNPROTECTED FIELD                            
*                       X'00'   =  PROTECTED FIELD                              
*                       C'C'    =  COMMENT FIELD                                
*             BYTE 2-4  = ADDRESS OF FIELD IN TWA                               
*                                                                               
FLDLIST  DS    0F                                                               
         DC    C'U',AL3(CONTYPEH-TWATASK)                                       
         DC    X'00',AL3(CONMODH-TWATASK)                                       
         DC    C'U',AL3(CONAGYH-TWATASK)                                        
         DC    X'00',AL3(CONAGYNH-TWATASK)                                      
         DC    C'U',AL3(CONBUYH-TWATASK)                                        
         DC    C'U',AL3(CONADVH-TWATASK)                                        
         DC    X'00',AL3(CONADVNH-TWATASK)                                      
         DC    C'U',AL3(CONPRDH-TWATASK)                                        
         DC    C'U',AL3(CONCATH-TWATASK)                                        
         DC    C'U',AL3(CONSTAH-TWATASK)                                        
         DC    X'00',AL3(CONSTAMH-TWATASK)                                      
         DC    C'U',AL3(CONDTESH-TWATASK)                                       
         DC    X'00',AL3(CONARSKH-TWATASK)                                      
         DC    C'U',AL3(CONSALH-TWATASK)                                        
         DC    X'00',AL3(CONSALNH-TWATASK)                                      
         DC    X'00',AL3(CONOFFNH-TWATASK)                                      
         DC    C'U',AL3(CONRTGH-TWATASK)                                        
         DC    C'U',AL3(CONIADVH-TWATASK)                                       
         DC    C'U',AL3(CONIPRDH-TWATASK)                                       
         DC    C'U',AL3(CONIESTH-TWATASK)                                       
         DC    C'C',AL3(CONCOM1H-TWATASK)                                       
         DC    C'C',AL3(CONCOM2H-TWATASK)                                       
         DC    X'00',AL3(CONCMBSH-TWATASK)                                      
         DC    X'00',AL3(CONCMBCH-TWATASK)                                      
         DC    X'00',AL3(CONCBS2H-TWATASK)                                      
         DC    X'00',AL3(CONCBC2H-TWATASK)                                      
         DC    X'00',AL3(CONCBS3H-TWATASK)                                      
         DC    X'00',AL3(CONCBC3H-TWATASK)                                      
         DC    X'00',AL3(CONCBSLH-TWATASK)                                      
         DC    X'00',AL3(CONCMBLH-TWATASK)                                      
         DC    C'U',AL3(CONDSPH-TWATASK)                                        
         DC    X'00',AL3(CONDSPNH-TWATASK)                                      
         DC    C'U',AL3(CONDCTH-TWATASK)                                        
         DC    X'00',AL3(CONDCTNH-TWATASK)                                      
         DC    X'FF'                                                            
         EJECT                                                                  
*                  INITIALIZATION CODE                                          
         SPACE 1                                                                
INITL    LR    R0,RE                                                            
         LM    R2,R4,0(R1)                                                      
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD .        A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD .        A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2) .      NUMBER OF FIELDS                             
         ST    R3,VTWA .           A(TWA)                                       
         MVC   VRECUP,24(R4)                                                    
         SPACE 1                                                                
         L     RE,16(R1)           COMFACS-GET ADDRESS OF DEMOVAL               
         USING COMFACSD,RE                                                      
         MVC   DATAMGR(92),CDATAMGR FACILITY LIST                               
         MVC   VDEMOVAL,CDEMOVAL                                                
         DROP  RE                                                               
         SPACE 1                                                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA) .     TERMINAL NUMBER                              
         MVC   REPALPHA,14(RA)          REP CODE                                
         LA    R3,64(R3)                                                        
         MVI   DMINBTS,X'00'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
*        LOACAL RECUP                                                           
*                                                                               
MYRECUP  NTR1                                                                   
*                                                                               
*- FIND MYSELF IN CORE                                                          
         BASR  R7,0                                                             
         USING MYRCUP10,R7                                                      
MYRCUP10 EQU   *                                                                
*                                                                               
*- BACK UP REGISTER STACK UNTIL I FIND A CONTRACT PHASE                         
         LR    R2,RD                                                            
MYRCUP20 L     R2,4(R2)            BACK POINTER                                 
         CLI   0(R2),C'0'                                                       
         BNE   MYRCUP20                                                         
         CLI   1(R2),C'2'          CONTRACT PROGRAM = C'02'                     
         BNE   MYRCUP20                                                         
*                                                                               
         DROP  R7                                                               
         L     RC,68(R2)           RESTORE RC                                   
         L     RA,60(R2)           RESTORE RA                                   
         L     R9,BASER9           RESTORE R9                                   
         L     RB,BASERB           RESTORE RB                                   
*                                                                               
         MVI   8(R1),C'R'                                                       
         GOTO1 LOCALUP                                                          
         CLI   8(R1),0                                                          
         BNE   EXXMOD                                                           
         LA    R3,339                                                           
         B     ERROR                                                            
         DS    0F                                                               
LOCALUP  DS    A                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTFED                                                       
         EJECT                                                                  
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTDFD                                                       
         EJECT                                                                  
         ORG   CONCNUMX+8                                                       
       ++INCLUDE RECNTC8D                                                       
         EJECT                                                                  
         CSECT                                                                  
***********************************************************************         
* AVNFLD - RANDOMLY ACTIVATES UNPROT. FIELDS ON EXPANDED BUY (DE) SCRN          
* ** DYNAMIC SCREEN CHANGES TO RECNTDE / RECNTDF **                             
***********************************************************************         
AVNFLD   NTR1  BASE=*,LABEL=*                                                   
         L     RE,ACOMFACS         CALL GETFACT FOR TIME                        
         USING COMFACSD,RE                                                      
         L     RF,CGETFACT                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,(2,0),0                                                
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         L     R5,FATIME           SYS TIME IN TU'S                             
         DROP  R1                                                               
         SR    R4,R4                                                            
         XC    FULL,FULL                                                        
         STCM  R5,6,FULL+2         2 MIDDLE BYTES                               
         OC    FULL,FULL                                                        
         BZ    *+8                 DON'T DIV BY 0                               
         D     R4,FULL             DIV TIME BY 2 MIDDLE BYTES OF TIME           
         ST    R4,FULL             SAVE REMAINDER AS RANDOM NUMBER              
*                                                                               
         LA    R2,TWATASK  FIND LAST FLD ON SCREEN                              
         LA    R2,64(R2)                                                        
AVN050   DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    AVN060                                                           
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     AVN050                                                           
*                                                                               
AVN060   DS    0H                                                               
         LA    R3,AVNTAB                                                        
AVN070   DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    AVNX                                                             
*                                                                               
         ZIC   R1,3(R3)            BIT TO TEST FOR FIELD                        
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    FULL+3,0            TEST BIT IN FULL (QUASI-RANDOM)              
         BO    AVN100                                                           
*                                                                               
         USING FHD,R2              GENERATE NEW FIELD                           
         XC    0(8,R2),0(R2)                                                    
         OI    FHAT,FHATLO         ZERO INTENSITY                               
         ZIC   R1,0(R3)            ROW                                          
         ZIC   R0,1(R3)            COL                                          
         BCTR  R1,0                                                             
         AHI   R0,-1                                                            
         MH    R1,=H'80'                                                        
         AR    R1,R0                                                            
         STCM  R1,3,FHAD           SCREEN ADDRESS                               
         OI    FHOI,X'80'          XMIT                                         
         MVI   FHDA,C' '           BLANK FIELD                                  
         ZIC   R1,2(R3)            FIELD LEN                                    
         LA    R1,8(R1)            +HDR                                         
         STC   R1,FHLN                                                          
         AR    R2,R1                                                            
         MVC   0(3,R2),=X'000101'                                               
         LR    RF,R2                                                            
         SR    RF,RA                                                            
         ST    RF,ALSTFH           NEW A(LAST FIELD)                            
*                                                                               
AVN100   DS    0H                                                               
         LA    R3,L'AVNTAB(R3)     NEXT FIELD                                   
         B     AVN070                                                           
*                                                                               
AVNX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
AVNTAB   DS    0XL4        FIELDS: ROW,COL,LEN,MASK                             
         DC    AL1(24),AL1(78),AL1(1),X'FF'  *MUST ALWAYS BE PRESENT*           
         DC    AL1(02),AL1(52),AL1(1),X'80'                                     
         DC    AL1(02),AL1(54),AL1(1),X'40'                                     
         DC    AL1(03),AL1(22),AL1(1),X'20'                                     
         DC    AL1(11),AL1(78),AL1(1),X'10'                                     
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*- CALLGENB -- PEFORMS A PASS THROUGH CALL TO REGENBUC IN REPFACS               
*  R1 = A(PARAMETER LIST)                                                       
***********************************************************************         
CALLGENB NTR1  BASE=*,LABEL=*                                                   
         GOTO1 (RFGENBUC,VREPFACS),(R1),,,,,,                                   
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
* HOTKEY - SEE IF PF4, PF5 OR PF6 WAS HIT.  IF SO, STORE CONTRACT#,             
*          PFKEY AND SAVE A STATUS-WORD: X'8000' = SAR ELEM ON RECORD,          
*          THEN SWITCH TO SFM,AHEAD/PHEAD,ADD,NNNNNN,I                          
*                                                                               
* ADD HOTKEY SUPPORT FOR MAKEGOOD GROUP OFFER: PRESSING PF12 ON THE MGC         
* SCREEN WILL ISSUE A MGL COMMAND                                               
*                                                                               
**********************************************************************          
* AGEN - AUTOGEN - FILL IN CONTRACT SCREEN FIELDS                               
**********************************************************************          
AGEN     NTR1  BASE=*,LABEL=*                                                   
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
         LA    R3,TWAGENBK         STUFF DATA IN TO THE CONTRACT SCREEN         
         USING RCAUTOD,R3                                                       
*                                                                               
         OI    CONPRDH+4,X'20'                                                  
*                                                                               
         OC    RCAUAGY(6),RCAUAGY     AGENCY                                    
         BZ    AGEN61                                                           
         OC    RCAUAGY(6),LOCSPACE                                              
         MVC   DUB,LOCSPACE                                                     
         MVC   DUB(4),RCAUAGY                                                   
*                                                                               
         CLC   RCAUAGOF,LOCSPACE                                                
         BE    AGEN60                                                           
         LA    RE,DUB                                                           
         MVI   DUB+4,C' '                                                       
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),RCAUAGOF    AGENCY OFFICE                                
AGEN60   CLC   DUB(7),CONAGY                                                    
         BE    AGEN61                                                           
         MVC   CONAGY(7),DUB                                                    
         LA    R1,CONAGY+7                                                      
         BCTR  R1,0                                                             
         CLI   0(R1),C' '                                                       
         BE    *-6                                                              
         LA    RF,CONAGY                                                        
         LA    R1,1(R1)                                                         
         SR    R1,RF                                                            
         STC   R1,CONAGYH+5        LENGTH                                       
         NI    CONAGYH+4,X'FF'-X'20'                                            
*                                                                               
AGEN61   DS    0H                                                               
         OC    RCAUTYPE,RCAUTYPE                                                
         BZ    AGEN61A                                                          
         NI    CONTYPEH+4,X'FF'-X'20'                                           
         MVC   CONTYPE(1),RCAUTYPE                                              
         MVI   CONTYPEH+5,1                                                     
*                                                                               
AGEN61A  DS    0H                                                               
         OC    RCAUBUYR,RCAUBUYR                                                
         BZ    AGEN62                                                           
         CLC   RCAUBUYR,CONBUY                                                  
         BE    AGEN62                                                           
         NI    CONBUYH+4,X'FF'-X'20'                                            
         MVC   CONBUY(20),RCAUBUYR                                              
         MVI   CONBUYH+5,20                                                     
*                                                                               
AGEN62 DS      0H                                                               
         OC    RCAUADV,RCAUADV                                                  
         BZ    AGEN63                                                           
         CLC   RCAUADV,CONADV                                                   
         BE    AGEN63                                                           
         NI    CONADVH+4,X'FF'-X'20'                                            
         MVC   CONADV(4),RCAUADV                                                
         MVI   CONADVH+5,4                                                      
*                                                                               
AGEN63   DS    0H                                                               
         OC    RCAUPRD,RCAUPRD                                                  
         BZ    AGEN64                                                           
         CLC   RCAUPRD,CONPRD                                                   
         BE    AGEN64                                                           
         NI    CONPRDH+4,X'FF'-X'20'                                            
         MVC   CONPRD,RCAUPRD                                                   
         MVI   CONPRDH+5,20                                                     
*                                                                               
AGEN64 DS      0H                                                               
         OC    RCAUSTAT,RCAUSTAT                                                
         BZ    AGEN65                                                           
         MVC   DUB,LOCSPACE                                                     
         GOTO1 (RFSTAOUT,VREPFACS),DMCB,RCAUSTAT,DUB                            
         CLC   DUB,CONSTA                                                       
         BE    AGEN65                                                           
         NI    CONSTAH+4,X'FF'-X'20'                                            
         MVC   CONSTA(7),DUB                                                    
         MVI   CONSTAH+5,7                                                      
*                                                                               
AGEN65 DS      0H                                                               
         OC    RCAUSAL,RCAUSAL                                                  
         BZ    AGEN66                                                           
         CLC   RCAUSAL,CONSAL                                                   
         BE    AGEN66                                                           
         NI    CONSALH+4,X'FF'-X'20'                                            
         MVC   CONSAL(3),RCAUSAL                                                
         MVI   CONSALH+5,3                                                      
*                                                                               
AGEN66 DS      0H                                                               
         OC    RCAUFLT,RCAUFLT                                                  
         BZ    AGEN67                                                           
         GOTO1 DATCON,DMCB,(2,RCAUFLT),(5,WORK)                                 
         MVI   WORK+8,C'-'                                                      
         GOTO1 DATCON,DMCB,(2,RCAUFLT+2),(5,WORK+9)                             
         CLC   WORK(17),CONDTES                                                 
         BE    AGEN67                                                           
         MVC   CONDTES(17),WORK                                                 
         MVI   CONDTESH+5,17                                                    
         NI    CONDTESH+4,X'FF'-X'20'                                           
*                                                                               
AGEN67 DS      0H                                                               
         OC    RCAUDVSL,RCAUDVSL                                                
         BZ    AGEN70                                                           
         CLC   RCAUDVSL,CONDSP                                                  
         BE    AGEN70                                                           
         MVC   CONDSP,RCAUDVSL                                                  
         OC    CONDSP,LOCSPACE                                                  
         NI    CONDSPH+4,X'FF'-X'20'                                            
         MVI   CONDSPH+5,3                                                      
*                                                                               
AGEN70 DS      0H                                                               
         OC    RCAUDVCT,RCAUDVCT                                                
         BZ    AGEN75                                                           
         CLC   RCAUDVCT,CONDCT                                                  
         BE    AGEN75                                                           
         NI    CONDCTH+4,X'FF'-X'20'                                            
         MVC   CONDCT,RCAUDVCT                                                  
         MVI   CONDCTH+5,2                                                      
*                                                                               
AGEN75 DS      0H                  FOR LEO EDI ONLY, MOVE ESTIMATE              
         CLC   =C'LEOB',RCAUAGY    TO ESTIMATE FIELD INSTEAD OF COMMENT         
         BNE   AGEN78                                                           
         OC    RCAUFLTN,RCAUFLTN                                                
         BZ    AGEN80                                                           
         EDIT  (C8,RCAUFLTN),(8,WORK),ALIGN=LEFT,FILL=0                         
         STC   R0,BYTE                                                          
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),CONIEST                                                  
         BNE   AGEN76                                                           
         CLC   BYTE,CONIESTH+5                                                  
         BE    AGEN80                                                           
AGEN76   EX    R1,*+4                                                           
         MVC   CONIEST(0),WORK                                                  
         STC   R0,CONIESTH+5                                                    
         NI    CONIESTH+4,X'FF'-X'20'                                           
         B     AGEN80                                                           
*                                                                               
AGEN78 DS      0H                                                               
         OC    RCAUFLTN,RCAUFLTN                                                
         BZ    AGEN80                                                           
         CLC   RCAUFLTN,CONCOM1                                                 
         BE    AGEN80                                                           
         NI    CONCOM1H+4,X'FF'-X'20'                                           
         MVC   CONCOM1(8),RCAUFLTN                                              
         MVI   CONCOM1H+5,8                                                     
*                                                                               
AGEN80 DS      0H                                                               
         OC    RCAUADVN,RCAUADVN                                                
         BZ    AGEN90                                                           
         CLC   RCAUADVN,WADVEXP                                                 
         BE    AGEN90                                                           
         MVC   WADVEXP,RCAUADVN                                                 
*                                                                               
AGEN90 DS      0H                                                               
         XIT1                                                                   
         DROP  R3                                                               
LOCSPACE DC    20C' '                                                           
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*- LGETPROF -- GET CONTRACT PROGRAM PROFILES FROM REP RECORD                    
*             AND SAVE IN THE TWA.  SET ALL BITS TO OFF IF                      
*             NO PROFILE ELEMENT FOUND.                                         
*                                                                               
*  THIS ROUTINE ONLY CALLED THE 1ST TIME INTO CONTRACT FOR A                    
*  SESSION.                                                                     
*                                                                               
LGETPROF NMOD1 0,*GPRF*                                                         
         L     RC,0(R1)                                                         
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
         XC    TWATIME,TWATIME     INIT                                         
         XC    PROFDATA,PROFDATA   START CLEAN                                  
         MVI   PROFEQU,RREPQCNT    WE CHECKED FOR PROFILES                      
*                                                                               
         XC    KEY,KEY             GET REP RECORD                               
         MVI   KEY,X'01'                                                        
         MVC   KEY+25(2),REPALPHA                                               
         XC    DUB,DUB                                                          
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTOX (RFGETREC,VREPFACS),DMCB,KEY,RREPREC,0,DUB                       
         BNE   GPROFEXT            REC NOT FOUND                                
*                                                                               
         MVC   TWARTS,RREPPROF+11  SAVE REP TO SPOT XFER CONTYPE                
*                                                                               
         CLI   RREPPROF+27,C'Y'    DAILY PACING?                                
         BNE   *+8                 NO                                           
         OI    TWAFLAGS,X'08'      YES - SET TWA INDICATOR                      
*                                                                               
         CLI   RREPPROF+4,C'Y'                                                  
         BNE   *+8                                                              
         OI    TWATIME,X'80'       USE 6A-559 INSTEAD                           
*                                  IF N USE NSI DEFAULT, IF Y USE ARB           
         CLI   RREPPROF+5,C'Y'                                                  
         BNE   *+8                                                              
         OI    TWATIME,X'40'       USE ARB                                      
*                                                                               
         CLI   RREPPROF+6,C'Y'                                                  
         BNE   *+8                                                              
         OI    TWATIME,X'20'       USE ALTERNATE SPL SCREEN                     
*                                                                               
         CLI   RREPPROF+7,C'Y'                                                  
         BNE   *+8                                                              
         OI    TWATIME,X'10'       ALLOW HIST/INV DISPLAY                       
*                                                                               
         LA    R6,RREPREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   GPROFEXT            NO PROFILE ELEMENT                           
         USING RREPPGMP,R6                                                      
         ZIC   RF,RREPPGM#         # OF PROGRAM UNITS (LOOP COUNTER)            
         LA    R6,RREPPGM1                                                      
         USING RREPPGM1,R6                                                      
GPROF10  CLI   RREPPGM1,RREPQCNT   CONTRACT?                                    
         BE    GPROF20                                                          
         LA    R6,RREPPGML(R6)                                                  
         BCT   RF,GPROF10                                                       
         B     GPROFEXT            CONTRACT NOT FOUND. USE DEFAULTS.            
*                                                                               
GPROF20  MVC   PROFDATA,RREPPGM1   SAVE PROGRAM PROFILES UNIT                   
         DROP  R6                                                               
GPROFEXT EQU   *                                                                
*                                                                               
*  BYPASS LOCKOUT TESTS:  ACTIVATE NEXT STATEMENT                               
*                                                                               
*        B     GPROFXT3                                                         
*                                                                               
*        CLC   RREPKREP,=C'AQ'     ALLIED LOCKOUT?                              
*        BE    GPROFXT2            YES                                          
         CLC   RREPKREP,=C'NK'     KATZ NATIONAL LOCKOUT?                       
         BE    GPROFXT2            YES                                          
         CLC   RREPKREP,=C'GP'     GROUP W LOCKOUT?                             
         BE    GPROFXT2            YES                                          
         CLC   RREPKREP,=C'CM'     CONCERT LOCKOUT?                             
         BE    GPROFXT2            YES                                          
         CLC   RREPKREP,=C'I1'     MAJOR MARKET LOCKOUT?                        
         BE    GPROFXT2            YES                                          
         CLC   RREPKREP,=C'TO'     TORBET LOCKOUT?                              
         BNE   GPROFXT3            NO                                           
GPROFXT2 EQU   *                                                                
         MVI   PROFEQU+1,1         YES - SET FLAG                               
GPROFXT3 EQU   *                                                                
         TM    TWAFLAGS,TWAFLAV2   AVN TERMINAL?                                
         BZ    GPROFXT4                                                         
         NI    PROFILES+CNTXBUYB,X'FF'-CNTXBUYA CAN'T USE EXP BUY SCRN          
GPROFXT4 EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
         DROP  R7                                                               
         LTORG                                                                  
*                                                                               
* CALL REGENVER VIA REPFACS                                                     
*                                                                               
DOGENVER NTR1  BASE=*,WORK=(R7,1),LABEL=*                                       
         MVC   0(4,R7),HELLO                                                    
         MVC   4(4,R7),DATCON                                                   
         L     R2,DMCB+4                                                        
         CLC   0(8,R7),0(R2)                                                    
         BE    DOGENV10                                                         
         ST    R7,DMCB+4                                                        
DOGENV10 DS    0H                                                               
         GOTOX (RFGENVER,VREPFACS)                                              
         XMOD1                                                                  
         LTORG                                                                  
*                                                                               
* DELETE CFC COMMENT                                                            
*                                                                               
DELCFC   NMOD1 0,*DCFC**                                                        
         L     RC,0(R1)                                                         
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
         TM    TWACFCFL,X'80'      NEED TO DELETE CFC COMMNET?                  
         BZ    EXXMOD              NO - GET OUT OF HERE                         
*                                                                               
         NI    TWACFCFL,X'FF'-X'80'  RESET FLAG                                 
         DROP  R7                                                               
*                                                                               
         BAS   RE,SETCOMBO         FIND K NUM FOR CFC REC                       
         XC    KEY,KEY             READ CFC REC                                 
         LA    R6,KEY                                                           
         USING RCFCREC,R6                                                       
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,RCONKREP                                                
         MVC   RCFCKCON,FULL                                                    
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCFCKEY),KEYSAVE   HAVE CFC REC?                           
         BNE   EXXMOD                   NO - GET OUT                            
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         OI    RCFCCNTL,X'80'          DELETE RECORD                            
         GOTO1 VPUTREC,DMCB,RCFCREC                                             
         LA    R6,KEY                                                           
         OI    RCFCKEY+27,X'80'        DELETE KEY                               
         GOTO1 VWRITE,DMCB,RCFCKEY                                              
         B     EXXMOD                                                           
*                                                                               
*  IF COMBO ORDER, RETURNS LOWEST K NUMBER IN COMBO IN 'FULL' ELSE              
*  RETURNS K NUMBER                                                             
*                                                                               
SETCOMBO NTR1                                                                   
         MVC   FULL,RCONKCON                                                    
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'17'                                                     
         BAS   RE,GETEL                                                         
         BNE   SCMBX                                                            
*                                                                               
         ZIC   R3,1(R6)            17 ELEM LEN                                  
         SH    R3,=H'2'            - ELCODE & LEN                               
         SR    R2,R2                                                            
         D     R2,=F'9'            LEN OF MINI ELEM                             
         LTR   R2,R2               DIVISION SHOULD BE EVEN                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R5,7(R6)            FIRST K NUMBER IN 17 ELEM                    
SCMB20   DS    0H                                                               
         CLC   FULL,0(R5)          FULL VS. CURRENT K?                          
         BL    *+10                FULL IS LOWER - SKIP                         
         MVC   FULL,0(R5)          FULL IS HIGHER - REPLACE W/CURRENT           
         LA    R5,9(R5)            NEXT MINI ELEM IN 17 ELEM                    
         BCT   R3,SCMB20                                                        
                                                                                
SCMBX    DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
* CHECK FOR GLOBBER LOADER VARIABLES                                            
*                                                                               
CKGLOB   NMOD1 0,*CKGLB*                                                        
         L     RC,0(R1)                                                         
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
*                                                                               
* CHECK FOR AUTO CONTRACT GENERATION REQUEST                                    
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',TWAGLVX,24,GLVXCTL                            
         TM    DMCB+8,X'10'                                                     
         BZ    CKGLOB01            WE HAVE CONTROL ELEM - WORK IT!              
*                                                                               
         NI    TWADARE,X'FF'-X'02'-X'04'                                        
         B     CKGLOB08            CHECK FOR OLD STYLE DATA ELEMENTS            
*                                                                               
CKGLOB01 DS    0H                                                               
         GOTO1 (RF),DMCB,=C'DELE',,,GLVXCTL                                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
                                                                                
         LA    R3,TWAGLVX                                                       
         USING GLVXFRSY,R3                                                      
                                                                                
         TM    GLVXFLG1,GLV1RETN   IS THIS A RETURN CALL?                       
         BZ    CKGL0150                                                         
         CLC   =C'COV',GLVXFRPR    FROM COVERSHEET PROGRAM?                     
         BNE   CKGL0150                                                         
         OI    GLVXFLG1,GLV1SIDR   YES,SET SESSION ID FLAG                      
         B     CKGLOB05            + DO NOT SAVE RETURN CALL                    
*                                  SYSTEM, PROGRAM                              
CKGL0150 DS    0H                                                               
         MVC   TWAGENCS,GLVXFRSY   SAVE SYSTEM                                  
         MVC   TWAGENCP,GLVXFRPR   SAVE PROGRAM                                 
         MVC   TWAGENGF,GLVXFLG1   SAVE GLOBBER FLAGS                           
*                                                                               
         TM    GLVXFLG1,GLV1SIDR   SAVE OFF CALLER SESSION IF DEFINED           
         BZ    *+10                                                             
         MVC   TWAGENSS,GLVXSESR   SAVE GLOBBER CALLER SESSION ID               
         DROP  R3                                                               
*                                                                               
* IF CALLER IS CON, FORCE SCREEN REFRESH                                        
*                                                                               
         CLC   =C'CON',TWAGENCP                                                 
         BNE   CKGLOB02                                                         
         TM    TWAGENGF,GLV1RETN   RETURN FRM ANOTHER CONTRACT SESSION?         
         BZ    CKGLOB02                                                         
         XC    CONSVC,CONSVC                                                    
         MVC   CONSVC(3),=C'=RE'   CLEARS GARBAGE ON THE SCREEN                 
         MVI   CONSVCH+5,3                                                      
         OI    CONSVCH+1,X'01'    MODIFIED                                      
         MVI   CONSVCH+4,X'80'    INPUT INDICATORS                              
         OI    CONSVCH+6,X'80'                                                  
         OI    CONCACTH+1,X'01'    SET MODIFIED                                 
*                                                                               
* IF CALLER IS RIS, SKIP DARE EQUATES                                           
*                                                                               
CKGLOB02 DS    0H                  SET THIS FLAG IF NOT AUTOGEN CALL            
         CLC   =C'RIS',TWAGENCP                                                 
         BE    CKGLOB04                                                         
*                                                                               
* CHECK FOR AUTOGEN CALL (THIS NEEDS TO BE FIRST CHECK)                         
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',TWAGENBK,RCAUELLQ,GLRCAUTO                    
         TM    DMCB+8,X'10'        NO VARIABLES FOUND, SKIP                     
         BNZ   CKGLOB03                                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLRCAUTO                                    
         CLI   8(R1),0                                                          
         BE    CKGLOB60                                                         
         DC    H'0'                                                             
                                                                                
CKGLOB03 DS    0H                  SET THIS FLAG IF NOT AUTOGEN CALL            
         NI    TWADARE,X'FF'-X'02'-X'04'                                        
*                                                                               
* CHECK FOR AUTO DARE MAKEGOOD APPLY                                            
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',WORK2,26,GLRMGAPL                             
         TM    DMCB+8,X'10'        NO VARIABLES FOUND, SKIP                     
         BNZ   CKGLOB04                                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLRMGAPL                                    
         B     CKGLOB50                                                         
*                                                                               
* CHECK FOR GENERIC CONTRACT DISPLAY CALL                                       
*                                                                               
CKGLOB04 DS    0H                                                               
         GOTO1 (RF),DMCB,=C'GETD',FULL,L'FULL,GLRDISPK                          
         TM    DMCB+8,X'10'        NO VARIABLES FOUND, SKIP                     
         BNZ   CKGLOB05                                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLRDISPK                                    
         ZAP   WORK+20(5),=P'0'    EDIT USES FIRST 17 BYTES OF WORK             
         MVO   WORK+20(5),FULL                                                  
         EDIT  (P5,WORK+20),(8,CONCNUM),ALIGN=LEFT                              
         STC   R0,CONCNUMH+5       INPUT LENGTH                                 
         OI    CONCNUMH+6,X'80'    XMIT FIELD                                   
         MVI   CONCNUMH+1,X'02'    MODIFIED                                     
         MVI   CONCNUMH+4,X'CA'    INPUT INDICATORS                             
*                                                                               
         MVC   CONCACT(3),=C'DIS'                                               
         MVI   CONCACTH+5,3                                                     
         OI    CONCACTH+6,X'80'    XMIT FIELD                                   
         MVI   CONCACTH+1,X'02'    MODIFIED                                     
         MVI   CONCACTH+4,X'C4'    INPUT INDICATORS                             
         B     CKGLOBYS                                                         
*                                                                               
* CHECK FOR GENERIC CON/BUY ACTION CALL                                         
*                                                                               
CKGLOB05 DS    0H                                                               
         GOTO1 (RF),DMCB,=C'GETD',WORK2,GLCONLNQ,GLRKACT                        
         TM    DMCB+8,X'10'        NO VARIABLES FOUND, SKIP                     
         BNZ   CKGLOB08                                                         
         LA    R3,WORK2                                                         
         BCTR  R3,0                                                             
         BCTR  R3,0                                                             
         USING GLCONEL,R3                                                       
         TM    GLCONFLG,GLCONRET+GLCONRPQ   AUTO RETURN TO CALLER?              
         BNZ   CKGLOB5A            YES - DON'T DELETE GLRKACT ELEM              
         GOTO1 (RF),DMCB,=C'DELE',,,GLRKACT                                     
CKGLOB5A DS    0H                                                               
         OI    TWAGENFG,TWQGLCON                                                
*                                                                               
         OC    GLCONNUM,MYSPACES                                                
         CLC   GLCONNUM,MYSPACES   K NUMBER?                                    
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE K NUMBER                           
*                                                                               
         LA    R5,GLCONNUM+L'GLCONNUM                                           
         BCTR  R5,0                REMOVE TRAILING SPACES                       
         CLI   0(R5),C' '                                                       
         BE    *-6                                                              
*                                                                               
         LA    R4,GLCONNUM-1                                                    
         LA    R4,1(R4)            REMOVE LEADING SPACES                        
         CLI   0(R4),C' '                                                       
         BE    *-8                                                              
*                                                                               
         BCTR  R4,0                BACK UP FOR EX                               
         SR    R5,R4                                                            
         BNM   *+6                                                              
         DC    H'0'                MUST HAVE K NUMBER                           
*                                                                               
         LA    R2,CONCNUMH                                                      
         STC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),1(R4)                                                    
         NI    4(R2),X'FF'-X'20'                                                
         OI    CONCNUMH+6,X'80'    XMIT FIELD                                   
         OI    CONCNUMH+1,X'01'    MODIFIED                                     
         MVI   CONCNUMH+4,X'88'    INPUT INDICATORS                             
*                                                                               
         OC    GLCONCA,MYSPACES                                                 
         CLC   GLCONCA,MYSPACES    K ACTION?                                    
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE ACTION                             
         LA    R5,GLCONCA+L'GLCONCA                                             
         BCTR  R5,0                                                             
         CLI   0(R5),C' '                                                       
         BE    *-6                                                              
         LA    R4,GLCONCA-1                                                     
         SR    R5,R4                                                            
         LA    R2,CONCACTH                                                      
         STC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),GLCONCA                                                  
         OI    CONCACTH+6,X'80'    XMIT FIELD                                   
         OI    CONCACTH+1,X'01'    MODIFIED                                     
         MVI   CONCACTH+4,X'84'    INPUT INDICATORS                             
*                                                                               
         OC    GLCONBA,MYSPACES                                                 
         CLC   GLCONBA,MYSPACES    BUY ACTION?                                  
         BE    CKGLOB06            NO - SKIP                                    
         LA    R5,GLCONBA+L'GLCONBA                                             
         BCTR  R5,0                                                             
         CLI   0(R5),C' '                                                       
         BE    *-6                                                              
         LA    R4,GLCONBA-1                                                     
         SR    R5,R4                                                            
         LA    R2,CONBACTH                                                      
         STC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),GLCONBA                                                  
         OI    CONBACTH+6,X'80'    XMIT FIELD                                   
         OI    CONBACTH+1,X'01'    MODIFIED                                     
         MVI   CONBACTH+4,X'84'    INPUT INDICATORS                             
CKGLOB06 DS    0H                                                               
         OC    GLCONBN,MYSPACES                                                 
         CLC   GLCONBN,MYSPACES    BUY NUMBER?                                  
         BE    CKGLOB07            NO - SKIP                                    
         LA    R5,GLCONBN+L'GLCONBN                                             
         BCTR  R5,0                                                             
         CLI   0(R5),C' '                                                       
         BE    *-6                                                              
         LA    R4,GLCONBN-1                                                     
         SR    R5,R4                                                            
         LA    R2,CONBNUMH                                                      
         STC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),GLCONBN                                                  
         OI    CONBNUMH+6,X'80'    XMIT FIELD                                   
         OI    CONBNUMH+1,X'01'    MODIFIED                                     
         MVI   CONBNUMH+4,X'80'    INPUT INDICATORS                             
*                                                                               
         LA    R4,WORK2+GLCONLNQ   USE SOME WORK AFTER ELEMENT                  
         MVC   0(8,R4),=8X'F0'                                                  
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVZ   0(0,R4),8(R2)                                                    
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),=8X'F0'     NUMERIC?                                     
         BNE   *+8                 NO                                           
         OI    CONBNUMH+4,X'0A'    SET NUMERIC/HEX                              
*                                                                               
CKGLOB07 DS    0H                                                               
         XC    CONSVC,CONSVC                                                    
         MVC   CONSVC(3),=C'=RE'   CLEARS GARBAGE ON THE SCREEN                 
         MVI   CONSVCH+5,3                                                      
         OI    CONSVCH+1,X'01'    MODIFIED                                      
         MVI   CONSVCH+4,X'80'    INPUT INDICATORS                              
         OI    CONSVCH+6,X'80'                                                  
*                                                                               
         TM    GLCONFLG,GLCONRET+GLCONRPQ   RETURN TO CALLER?                   
         BZ    CKGLOBYS                                                         
         TM    GLCONFLG,GLCONRPQ   REPLY TO CALLER?                             
         BZ    *+8                                                              
         OI    TWAGENFG,TWQRTERR   RETURN ERRORS                                
         DROP  R3                                                               
         XC    WORK2,WORK2                                                      
         LA    R3,WORK2                                                         
         USING GLVXFRSY,R3                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'CON'    CONTRACT PROGRAM                             
         MVC   GLVXTOSY,TWAGENCS     LAST CALLER SYSTEM                         
         MVC   GLVXTOPR,TWAGENCP     LAST CALLER PROG                           
         OI    GLVXFLG1,GLV1RETN+GLV1SEPS                                       
***>     OI    GLVXFLG1,GLV1RETN                                                
         GOTO1 (RF),DMCB,=C'PUTD',WORK2,24,GLVXCTL                              
         CLI   DMCB+8,0                                                         
         BE    CKGLOBYS                                                         
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
* CHECK FOR CONTRACT CALL/SONNET CALL (OLD ROUTINES)                            
*                                                                               
CKGLOB08 DS    0H                                                               
         GOTO1 (RF),DMCB,=C'GETD',FULL,L'FULL,GLRCONNO                          
         TM    DMCB+8,X'10'        NO VARIABLES FOUND, SKIP                     
         BNZ   CKGLOBNO            GET OUT                                      
         GOTO1 (RF),DMCB,=C'DELE',,,GLRCONNO                                    
                                                                                
         ZAP   WORK+20(5),=P'0'    EDIT USES FIRST 17 BYTES OF WORK             
         MVO   WORK+20(5),FULL                                                  
         EDIT  (P5,WORK+20),(8,CONCNUM),ALIGN=LEFT                              
         STC   R0,CONCNUMH+5       INPUT LENGTH                                 
         OI    CONCNUMH+6,X'80'    XMIT FIELD                                   
         MVI   CONCNUMH+1,X'02'    MODIFIED                                     
         MVI   CONCNUMH+4,X'CA'    INPUT INDICATORS                             
                                                                                
         GOTO1 (RF),DMCB,=C'GETD',FULL,1,GLRPFKEY                               
         TM    DMCB+8,X'10'        NO VARIABLES FOUND, SKIP                     
         BNZ   CKGLOBNO                                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLRPFKEY                                    
                                                                                
         CLI   FULL,C'D'           FROM DARE, DISPLAY ONLY                      
         BNE   CKGLOB09                                                         
         MVC   CONCACT(7),=C'DIS,DSM'                                           
         MVI   CONCACTH+5,7                                                     
         B     CKGLOB30                                                         
                                                                                
CKGLOB09 DS    0H                                                               
         CLI   FULL,C'S'           FROM DARE, SEND ORDER                        
         BNE   CKGLOB10                                                         
         MVC   CONCACT(4),=C'SEND'                                              
         MVI   CONCACTH+5,4                                                     
         OI    TWAGENFG,TWQGOSND                                                
         B     CKGLOB30                                                         
                                                                                
CKGLOB10 CLI   FULL,C'P'           FROM PENDCOM/SFM, PD OR                      
         BE    CKGLOB20                                                         
         CLI   FULL,C'F'           FD                                           
         BE    CKGLOB20                                                         
*                                                                               
         CLI   FULL,C'X'           FROM SONNET  X                               
         BNE   CKGLOB12                                                         
         MVC   CONCACT(6),=C'DIS,DX'                                            
         MVI   CONCACTH+5,6                                                     
         B     CKGLOB30                                                         
                                                                                
CKGLOB12 CLI   FULL,C'M'           FROM SONNET  M                               
         BNE   CKGLOB14                                                         
         MVC   CONCACT(3),=C'MGL'                                               
         MVI   CONCACTH+5,3                                                     
         B     CKGLOB30                                                         
                                                                                
CKGLOB14 CLI   FULL,C'R'           FROM SONNET  R                               
         BNE   CKGLOBNO                                                         
         MVC   CONCACT(7),=C'DIS,ORD'                                           
         MVI   CONCACTH+5,7                                                     
         B     CKGLOB30                                                         
                                                                                
                                                                                
*                                                                               
CKGLOB20 DS    0H                                                               
         MVC   CONCACT(1),FULL                                                  
         MVI   CONCACT+1,C'D'                                                   
         MVI   CONCACTH+5,2        INPUT LENGTH                                 
                                                                                
CKGLOB30 DS    0H                                                               
         OI    CONCACTH+6,X'80'    XMIT FIELD                                   
         MVI   CONCACTH+1,X'02'    MODIFIED                                     
         MVI   CONCACTH+4,X'C4'    INPUT INDICATORS                             
         B     CKGLOBYS                                                         
*                                                                               
* FOR NOW, WE'LL DISPLAY THE CONTRACT WITH ACTION MGL COMING FROM DARE          
* LATER ON WE'LL AUTOMATICALLY APPLY THE SPECIFIC MAKEGOOD GROUP                
* AND RETURN TO DARE WITHOUT THE USER EVER SEEING THE CONTRACT PROGRAM          
*                                                                               
CKGLOB50 DS    0H                                                               
         ZAP   WORK+20(5),=P'0'    EDIT USES FIRST 17 BYTES OF WORK             
         MVO   WORK+20(5),WORK2(4)                                              
         EDIT  (P5,WORK+20),(8,CONCNUM),ALIGN=LEFT                              
         STC   R0,CONCNUMH+5       INPUT LENGTH                                 
         OI    CONCNUMH+6,X'80'    XMIT FIELD                                   
         MVI   CONCNUMH+1,X'02'    MODIFIED                                     
         MVI   CONCNUMH+4,X'CA'    INPUT INDICATORS                             
*                                                                               
         XC    CONCACT,CONCACT                                                  
         MVC   CONCACT(3),=C'MGL'                                               
         MVI   CONCACTH+5,3                                                     
*                                                                               
         MVC   TWAMGBYR,WORK2+6                                                 
*                                                                               
         CLC   WORK2+4(2),MYSPACES                                              
         BE    CKGLOB55            NORMAL SWAP FROM DARE                        
         MVI   CONCACT+3,C','      OR AUTO-APPLY FROM DARE?                     
         MVC   CONCACT+4(2),WORK2+4                                             
         MVI   CONCACTH+5,6                                                     
         OI    TWADARE,X'02'       SET AUTO-APPLY                               
         NI    TWADARE,X'FF'-X'04'                                              
         MVC   TWAGENCS,=C'REP'    SYSTEM                                       
         MVC   TWAGENCP,=C'DAR'    PROGRAM, SET TO AUTO RETURN TO DARE          
*                                                                               
CKGLOB55 DS    0H                                                               
         OI    CONCACTH+6,X'80'    XMIT FIELD                                   
         MVI   CONCACTH+1,X'02'    MODIFIED                                     
         MVI   CONCACTH+4,X'C4'    INPUT INDICATORS                             
         B     CKGLOBYS                                                         
*                                                                               
* AUTO CONTRACT GENERATION:                                                     
* CONTRACT WILL BE PROVIDED WITH THE NECESSARY DATA IN ORDER TO                 
* GENERATE A HEADER AUTOMATICALLY. IF AN ERROR IS ENCOUNTERED, IT'LL            
* RETURN WITH AN ERROR CODE. IF SUCCESSFUL, IT'LL RETURN WITH THE               
* CONTRACT NUMBER TO THE CALLER                                                 
*                                                                               
CKGLOB60 DS    0H                                                               
         OI    TWAGENFG,TWQGOGEN   SET AUTOGEN MODE                             
*                                                                               
         LA    R3,TWAGENBK         AUTOGEN ELEM                                 
         USING RCAUTOD,R3                                                       
*                                                                               
         TM    RCAUFLAG,X'80'      DON'T RETURN IF ERROR?                       
         BO    *+8                                                              
         OI    TWAGENFG,TWQRTERR   SET RETURN TO CALLER ON ERROR                
*                                                                               
         TM    RCAUFLAG,X'20'      CHANGE MODE?                                 
         BZ    CKGLOB70            NO                                           
         XC    CONCACT,CONCACT                                                  
         MVC   CONCACT(3),=C'CHA'                                               
         MVI   CONCACTH+5,3                                                     
         OI    CONCACTH+6,X'80'    XMIT FIELD                                   
         OI    CONCACTH+1,X'01'    MODIFIED                                     
         MVI   CONCACTH+4,X'C4'    INPUT INDICATORS                             
*                                                                               
         XC    CONCNUM,CONCNUM                                                  
         GOTOX (RFCONNUM,VREPFACS),DMCB,(1,RCAUSCON),(6,CONCNUM)                
         MVC   CONCNUMH+5(1),0(R1)                                              
         OI    CONCNUMH+6,X'80'    XMIT FIELD                                   
         OI    CONCNUMH+1,X'01'    MODIFIED                                     
         MVI   CONCNUMH+4,X'CA'    INPUT INDICATORS                             
         B     CKGLOBYS                                                         
*                                                                               
CKGLOB70 DS    0H                                                               
         GOTO1 AAUTOGEN            FILL IN SCREEN FIELDS                        
*                                                                               
         XC    CONSVC,CONSVC                                                    
         MVC   CONSVC(3),=C'=RE'   CLEARS GARBAGE ON THE SCREEN                 
         MVI   CONSVCH+5,3                                                      
         XC    CONCACT,CONCACT                                                  
         MVC   CONCACT(3),=C'ADD'                                               
         MVI   CONCACTH+5,3                                                     
         CLC   =C'DAR',TWAGENCP                                                 
         BNE   CKGLOB95                                                         
         TM    PROFILES+CNTMEDTB,CNTMEDTA                                       
         BZ    CKGLOB95                                                         
         MVC   CONCACT(4),=C'ADDS'                                              
         MVI   CONCACTH+5,4                                                     
*                                                                               
CKGLOB95 DS    0H                                                               
         OI    CONCACTH+6,X'80'    XMIT FIELD                                   
         MVI   CONCACTH+1,X'02'    MODIFIED                                     
         MVI   CONCACTH+4,X'C4'    INPUT INDICATORS                             
         DROP  R3                                                               
*                                                                               
CKGLOBYS SR    R0,R0                                                            
         B     *+8                                                              
CKGLOBNO LA    R0,1                                                             
         LTR   R0,R0                                                            
         B     EXXMOD                                                           
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
HOTKEY   NMOD1 0,*HTKY*                                                         
         L     RC,0(R1)                                                         
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
*                                                                               
         CLI   TWASCRN,X'DD'       BUY TEMPLATE SREEN UP?                       
         BE    HKX1                I'LL HANDLE MY OWN PFKEYS                    
*                                                                               
         CLI   CONCACT,C'M'        CHECK ALL THE TIME IF MAKEGOOD               
         BE    HK10                                                             
         CLC   =C'RNA',CONCACT     OR REPLACEMENT OFFER                         
         BE    HK10                                                             
         TM    CONCNUMH+4,X'20'    TEST IF VALID CON                            
         BZ    HKX1                                                             
*                                                                               
HK10     DS    0H                                                               
         L     R1,AFACILS                                                       
         L     RF,0(R1)                                                         
         USING TIOBD,RF                                                         
         MVC   DUB(2),TIOBCURD                                                  
         ZIC   R0,TIOBAID                                                       
         C     R0,=F'12'                                                        
         BNH   *+8                                                              
         S     R0,=F'12'                                                        
* PFKEY IS STORED IN HALF, STATUS IS STORED IN FULL                             
         STC   R0,HALF             SAVE OFF KEY HIT                             
         CLI   HALF,2              SWAP TO DARE/MAKEGOOD?                       
         BE    HK300                                                            
         CLI   HALF,3              MGL/MAKEGOOD GROUP LIST?                     
         BE    HK100                                                            
         CLI   HALF,4              SWAP TO AVAILS?                              
         BE    HK30                                                             
         CLI   HALF,5              SWAP TO PROPOSALS?                           
         BE    HK30                                                             
         CLI   HALF,6              SWAP TO HELL?                                
         BE    HK30                                                             
         CLI   HALF,7              SWAP TO SIN00?                               
         BE    HK40                                                             
         CLI   HALF,12             GENERIC RETURN TO CALLER?                    
         BE    HK900                                                            
         B     HKX1                                                             
HK30     DS    0H                                                               
         CLC   =C'RNA',CONCACT     REPLACEMENT OFFER OR                         
         BE    HK31                                                             
         CLI   CONCACT,C'M'        MAKEGOOD?                                    
         BNE   HK33                                                             
HK31     DS    0H                                                               
         CLI   HALF,4              PASS OTHER KEYS TO MODULES                   
         BNE   HKX1                                                             
*                                                                               
* SWAP TO NEXT SESSION AND BRING UP SAME CONTRACT WITH DSM                      
*                                                                               
         XC    WORK2,WORK2                                                      
         LA    R6,WORK2                                                         
         USING GLCONNUM,R6                                                      
         MVC   GLCONNUM,CONCNUM                                                 
         MVC   GLCONCA(3),=C'DIS'                                               
         MVC   GLCONBA(3),=C'DSM'                                               
         DROP  R6                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,=C'PUTD',WORK2,GLCONLNQ,GLRKACT                        
         CLI   DMCB+8,0                                                         
         BE    HK35                                                             
         DC    H'0'                                                             
*                                                                               
* SWAP TO SFM                                                                   
*                                                                               
HK33     DS    0H                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,=C'PUTD',HALF,1,GLRPFKEY                               
* SEE IF SAR BIT ELEM ON RECORD AND SET STATUS ACCORDINGLY                      
         MVC   KEY+28(4),TWAKADDR                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+14                ELEM NOT FOUND                               
         MVC   FULL,=X'80000000'                                                
         B     *+10                                                             
         XC    FULL,FULL                                                        
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,=C'PUTD',FULL,L'FULL,GLRSTAT                           
         GOTO1 (RF),DMCB,=C'PUTF',CONCNUMH,,GLRCONNO                            
*                                                                               
HK35     XC    WORK2,WORK2                                                      
         LA    R1,WORK2                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'CON'    CONTRACT PROGRAM                             
         MVC   GLVXTOSY,=C'REP'                                                 
         MVC   GLVXTOPR,=C'SFM'    TO SFM                                       
         CLI   CONCACT,C'M'                                                     
         BE    *+14                                                             
         CLC   =C'RNA',CONCACT                                                  
         BNE   *+10                                                             
         MVC   GLVXTOPR,=C'CON'    TO CON                                       
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS                                       
         OI    GLVXFLG1,GLV1SEPS                                                
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
                                                                                
         GOTO1 (RF),DMCB,=C'PUTD',WORK2,24,GLVXCTL                              
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     HKX2                                                             
*                                                                               
* IF K PROF 16=Y AND K IS A FORECAST K, FORBID PF7 TO SIN                       
*                                                                               
HK40     DS    0H                                                               
         TM    PROFILES+CNT4CASB,CNT4CASA                                       
         BZ    HK45                                                             
                                                                                
         MVC   KEY+28(4),TWAKADDR                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BNE   HK45                NOT FOUND, NOT A FORECAST K                  
                                                                                
         USING RSARXEL,R6          FOUND. IS THERE A FORECAST FLAG?             
         CLI   RSARXLEN,RSARXLTH   NO ELEMENT IS OLD SAR ELEMENT                
         BNE   HK45                                                             
         TM    RSARXFLG,X'08'      NEW ELEMENT HAS FORECAST FLAG                
         BZ    HK45                OFF MEANS NOT FORECAST K                     
         DROP  R6                                                               
                                                                                
         LA    R2,CONCACTH                                                      
         LA    R3,136              CANNOT APPLY INVOICES TO 4CAST               
         B     ERROR                                                            
                                                                                
HK45     DS    0H                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,=C'PUTD',HALF,1,GLRPFKEY                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),DMCB,=C'PUTF',CONCNUMH,,GLRCONNO                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,CONMSGH                                                       
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               POINT AT SVC REQ FLD HDR                     
         OI    6(R2),X'80'         XMIT FIELD                                   
         MVC   8(17,R2),=C'+SIN,SV          '                                   
         B     HKX2                                                             
         EJECT                                                                  
*                                                                               
* MAKEGOOD GROUP LIST. VALID ONLY CALLING WITH MGC/MGX/MGO/RNA AS               
* CONTRACT ACTIONS                                                              
*                                                                               
HK100    DS    0H                                                               
         CLI   CONCACT,C'M'        MUST BE A MAKEGOOD GROUP ACTION              
         BE    HK110                                                            
         CLC   =C'RNA',CONCACT     OR RNA                                       
         BNE   HKX2                                                             
*                                                                               
HK110    DS    0H                                                               
         NI    TWAMKGFG,X'FF'-X'08' RESET BONUS FLAG                            
         NI    CONCACTH+4,X'FF'-X'20'                                           
         OI    CONCACTH+4,X'80'                                                 
         OI    CONCACTH+1,X'01'                                                 
         MVC   CONCACT(4),=C'MGL ' FORCE MGL                                    
         MVC   LASTCACT,=C'MGC '                                                
         MVI   CONCACTH+5,3                                                     
         MVC   NUMFLD,=H'2'        FAKE OUT CONTRACT                            
         B     HKX1                                                             
         EJECT                                                                  
**********************************************************************          
* HOT KEY TO DARE/MAKEGOOD                                                      
**********************************************************************          
HK300    DS    0H                                                               
         CLI   TWASCRN,X'E5'       ONLY CHECK FOR MAKEGOOD SCREENS              
         BE    HK350                                                            
         CLI   TWASCRN,X'CC'       ONLY CHECK FOR MAKEGOOD SCREENS              
         BE    HK350                                                            
         CLI   TWASCRN,X'C1'       ONLY CHECK FOR MAKEGOOD SCREENS              
         BL    HKX1                                                             
         CLI   TWASCRN,X'C5'                                                    
         BH    HKX1                                                             
*                                                                               
* FOR NON-DARE ORDERS, PASS PFKEY TO RECNT3A AND APPLY GROUP                    
*                                                                               
HK350    DS    0H                                                               
         MVC   KEY+28(4),TWAKADDR                                               
         GOTO1 VGETREC,DMCB,RCONREC                                             
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   HKX1                                                             
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'+X'40'+X'01'                                       
         BZ    HKX1                                                             
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    HK380                                                            
         TM    RCONDRF2,X'08'      ORDER WAS REMOVED??                          
         BO    HKX1                YES, NOT A DARE ORDER ANYMORE                
         DROP  R6                                                               
*                                                                               
* FOR DARE ORDERS, SWAP TO DARE PROGRAM INSTEAD                                 
*                                                                               
HK380    DS    0H                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
*                                                                               
         GOTO1 (RF),DMCB,=C'PUTF',CONCNUMH,,GLRCONNO                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    FULL,FULL                                                        
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
*                                                                               
         MVC   FULL(2),TWAMKGDT   MOVE GROUP                                    
*                                                                               
HK390    DS    0H                                                               
         GOTO1 (RF),DMCB,=C'PUTD',FULL,L'FULL,GLRSTAT                           
*                                                                               
         XC    WORK2,WORK2                                                      
         LA    R1,WORK2                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'CON'    CONTRACT PROGRAM                             
         MVC   GLVXTOSY,=C'REP'                                                 
         MVC   GLVXTOPR,=C'DAR'    TO DARE                                      
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS                                       
         OI    GLVXFLG1,GLV1SEPS                                                
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
                                                                                
         GOTO1 (RF),DMCB,=C'PUTD',WORK2,24,GLVXCTL                              
*                                                                               
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     HKX2                                                             
         EJECT                                                                  
**********************************************************************          
* GENERIC RETURN TO CALLER (PF12)                                               
**********************************************************************          
HK900    DS    0H                  GENERIC RETURN TO CALLER (PF12)              
         LA    R3,696                                                           
         OC    TWAGENCS,TWAGENCS   DO WE HAVE A CALLER SYSTEM?                  
         BZ    ERROR               NO - CAN'T RETURN                            
         OC    TWAGENCP,TWAGENCP   DO WE HAVE A CALLER PROGRAM?                 
         BZ    ERROR               NO - CAN'T RETURN                            
         TM    TWAGENGF,GLV1RETN   WAS LAST CALL A RETURN CALL?                 
         BO    ERROR               YES - DON'T RETURN A RETURN CALL!            
*                                                                               
         XC    WORK2,WORK2                                                      
         MVC   WORK2(22),TWAGLVX   RESTORE CONTROL BYTES OF ORIG ELEM           
         LA    R1,WORK2                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'CON'    CONTRACT PROGRAM                             
         MVC   GLVXTOSY,TWAGENCS     LAST CALLER SYSTEM                         
         MVC   GLVXTOPR,TWAGENCP     LAST CALLER PROG                           
         OI    GLVXFLG1,GLV1RETN+GLV1SEPS                                       
         NI    GLVXFLG1,X'FF'-GLV1IGN                                           
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CGLOBBER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,=C'PUTD',WORK2,24,GLVXCTL                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     HKX2                                                             
*                                                                               
HKX1     LA    R0,1                                                             
         LTR   R0,R0                                                            
         B     HKX                                                              
HKX2     DS    0H                                                               
         LR    R1,RA                                                            
         AH    R1,DUB                                                           
         OI    6(R1),X'C0'                                                      
         SR    R0,R0               CLEAR CC                                     
HKX      DS    0H                                                               
         XMOD1                                                                  
         DROP  R7                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE FAUTL                                                          
         EJECT                                                                  
REQD     DSECT                                                                  
       ++INCLUDE REGENREQ                                                       
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
       ++INCLUDE DDGLVXCTLD                                                     
         EJECT                                                                  
       ++INCLUDE REGLCON                                                        
         EJECT                                                                  
       ++INCLUDE RECNTAUTOD                                                     
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE DDFH                                                           
         EJECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
         EJECT                                                                  
       ++INCLUDE REGLBRW                                                        
       ++INCLUDE REGENCFC                                                       
       ++INCLUDE REGENSBLK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070RECNT00S  05/19/03'                                      
         END                                                                    
