*          DATA SET SPSFM2C    AT LEVEL 100 AS OF 08/11/11                      
*PHASE T2172CA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: T2172C - DEMO OVERIDE DEFINITION MAINTENANCE (DEMDEF)       *         
*                                                                     *         
* AKAT 099 25JAN08 FIX DEMO IMPS DISPLAY FOR USER DEMOS               *         
* MHER 095 14JUL05 IF DEFAULT DEMO VALUE IS 0, FLAG DEMO AS INPUT WHEN*         
*                  CATEGORY ADDED TO RECORD AND SET X'8000' AS VALUES *         
* PWES 094 26OCT04 ENHANCE LIST LINE CONTENT FOR IMPROVED USABILITY   *         
*                  + RENAME BASE/USE TIL BOOK + FIX LIST E-O-RECS TEST*         
* PWES 093 31AUG04 RELINK ONLY FOR NEW EF SCREEN - PANAPT NEEDS LEVEL *         
* PWES 092 ??JUL04 REVISE COPY ACTION(+SEPARATE VKEY)+SOFTEN/EQUS/TIDY*         
*                  + DEMO NOT RTG MSG                                 *         
* PWES 091 23JUN04 ERROR DIFFERENTIATION FOR ADD05 ROUTINE ERRORS     *         
* PWES 090 17JUN04 DON'T DUMP ON RECUP OVERFLOW IN ADD05 ROUTINE      *         
* PWES 089 14MAY04 FIX <PF4> ON ADD / GENNDOV EQUS / BBM DEFAULT RTG  *         
*                  + FIX DIR KEY FILTERS                              *         
* AKAT 088 19FEB04 ERROR MSG ON RECUP OVERFLOW                        *         
* PWES 087 28MAY03 MSG NOT DUMP IF STATIONS/SPILL EXCEED MAX# 05 ELS  *         
* PWES 086 08MAY03 ALWAYS USE AGENCY LEVEL SPILL RECORD               *         
* PWES 085 06FEB03 GENERAL NICETIES TO SHOW/DEMD/DEMO                 *         
* TZIH 084 06NOV02 FIX DUPLICATE DEMO IMPRESSIONS CHECK               *         
* TZIH 083 11OCT02 PREVENTING DUPLICATE DEMO IMPRESSIONS              *         
* PWES 082 22JUL02 ENSURE 05 ELEM LONG ENOUGH FOR #DEMOS WHEN ADD DEMO*         
* ???? 081 ??????? ??? DUPLICATE DEMO                                 *         
* AKAT 080 07MAY02 PREVENT DUPES IN LIST AND IMPS                     *         
* PWES 079 27MAR02 DEMO TEAM CHANGED USER DEMO FORMAT EBCDIC->BINARY  *         
* PWES 078 08MAR02 ENSURE IMP NOT >MAX THAT FITS 2 BYTES (6553.5)     *         
* PWES 077 22FEB02 DON'T CORRUPT 05 ELEM LENGTH WHEN CLEAR DEMOS      *         
* PWES 076 19NOV01 CLEAR ACURFORC SO ERROR IN CORRECT FIELD           *         
* ABEA 075 05NOV01 SPECIAL USER DEMOS                                 *         
* PWES 074 26OCT01 CANADIAN CABLE                                     *         
*                                                                     *         
*  CALLED FROM: SPOT CONTROLLER (T21700), WHICH CALLS                 *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  ***  'NETWORK UPLOAD' SCSPFILDEM SCRIPTS THE MAINT SCREEN!  ***    *         
*                                                                     *         
*  INPUTS: SCREENS SPSFMA7  (T217A7) -- MAINTENANCE                   *         
*          SCREENS SPSFMAA  (T217AA) -- LIST                          *         
*                                                                     *         
*  OUTPUTS: UPDATED DEMODEF RECORDS                                   *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - SECOND BASE REGISTER                                  *         
*          R8 - WORK                                                  *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'T2172C DEMO DEFINITION OVERIDE'                                 
T2172C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2172C*,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         XC    SET0LIST,SET0LIST                                                
*                                                                               
         MVC   DMCB+4(4),=X'D9000AD9'   DEMOVAL                                 
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDEMOVAL,DMCB       ADDRESS OF DEMOVAL                           
*                                                                               
         BRAS  RE,SETUP            PFKEYS SETUP                                 
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     DS    0H                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*-----------------------------------------------------------*                   
*                    VALIDATE KEY                           *                   
*-----------------------------------------------------------*                   
VK       DS    0H                                                               
         XC    MYFLAG,MYFLAG                                                    
         XC    SAVEKEY,SAVEKEY                                                  
*                                                                               
         LA    R4,SAVEKEY                                                       
         USING DOVRECD,R4                                                       
*                                                                               
         MVC   DOVKTYP,=X'0D17'    RECORD TYPE                                  
*                                                                               
         XC    TEMPHEAD,TEMPHEAD                                                
         MVC   TEMPHEAD(9),=XL9'0900000000010000D5' N FOR NETWORK               
         LA    R2,TEMPHEAD                                                      
         GOTO1 VALIMED             READ AGYHDR & FILL IN SVAPROF                
         MVC   DOVKAGMD,BAGYMD     AGENCY/MEDIA                                 
*                                                                               
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VKL                                                              
         CLI   ACTNUM,ACTCOPY      ACTION COPY?                                 
         BE    VKCOPY                                                           
*                                                                               
         XC    DEMPGMD,DEMPGMD     CLEAR PROGRAM/DAY/TIME FIELD                 
         OI    DEMPGMDH+FHOID,FHOITR                                            
         LA    R2,DEMNETH                                                       
         CLI   5(R2),0             ANY NETWORK?                                 
         BE    ERRMISS                                                          
         BAS   RE,VALNET           YES - VALIDATE NETWORK                       
         BNE   ERRINV                                                           
         MVC   DOVKNET,BSTA        NETWORK STATION CODE                         
*                                                                               
         LA    R2,DEMPGMH                                                       
         CLI   5(R2),0             ANY PROGRAM?                                 
         BE    ERRMISS                                                          
         OC    8(4,R2),=C'    '                                                 
         MVC   TEMPPGME,8(R2)      PROGRAM OFF SCREEN                           
*                                                                               
         BAS   RE,VALPROG          YES - VALIDATE PROGRAM                       
*                                                                               
         MVC   DOVKPGM,8(R2)       PROGRAM NAME                                 
*                                  PROGRAM DESCRIPTION                          
         MVC   DEMPGMD(L'PGMDNAME),TEMPPGM                                      
         MVC   DEMPGMD+PGMDDAY-PGMDETSD(L'PGMDDAY),TEMPDAY                      
         MVC   DEMPGMD+PGMDTIME-PGMDETSD(L'PGMDTIME),TEMPTIME                   
*                                                                               
         LA    R2,DEMRTSH          RATING SERVICE                               
         CLI   5(R2),0             ANY RATING SERVICE?                          
         BNE   VK7                                                              
         CLI   CALLSP,0                                                         
         BE    *+12                                                             
         CLI   CALLSTCK,X'F0'      PF FROM SHOWDEF - WON'T HAVE RTS             
         BE    VK8                 HANDLE LOGICALLY LATER                       
         MVC   FHDAD(3,R2),=C'BBM' SIMPLY DEFAULT TO BBM                        
         MVI   FHILD(R2),3                                                      
         OI    FHIID(R2),FHIITH                                                 
         OI    FHOID(R2),FHOITR                                                 
*                                                                               
VK7      BAS   RE,VALRTS           YES - VALIDATE RATING SERVICE                
*                                                                               
         MVC   DOVKRTS,SVRTS       RATING SERVICE                               
*                                                                               
VK8      LA    R2,DEMCLTH          CLIENT                                       
         CLI   5(R2),0             ANY CLIENT?                                  
         BE    VK10                                                             
         CLC   =C'ALL',8(R2)       ALL CLIENTS?                                 
         BE    VK10                                                             
         GOTO1 VALICLT             YES - VALIDATE CLIENT                        
         MVC   DOVKCLT,BCLT        CLIENT (PACKED)                              
*                                                                               
VK10     LA    R2,DEMSEQH          SEQUENCE NUMBER                              
         CLI   5(R2),0             ANY SEQUENCE NUMBER?                         
         BE    VK20                                                             
         TM    4(R2),X'08'         NUMERIC?                                     
         BZ    ERRNUMB                                                          
         PACK  DUB,8(1,R2)                                                      
         CVB   R5,DUB                                                           
         CH    R5,=H'1'            TEST IN RANGE 1-255?                         
         BH    ERRINV                                                           
*                                                                               
         STC   R5,DOVKSEQ          SEQUENCE NUMBER                              
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   VK20                NO                                           
         CLI   DOVKSEQ,X'01'       SEQUENCE RECORD 1?                           
         BNE   VK20                                                             
         BAS   RE,SEQCHECK         CHECK FOR 0 SEQUENCE RECORD                  
*                                                                               
VK20     CLI   DEMRTSH+5,0         DEAL WITH MISSING RTS FROM SHOWDEF           
         BNE   VKX                                                              
         LA    R2,DEMRTSH                                                       
         OC    DOVKCLT,DOVKCLT     IF CLIENT, USER INPUT IT SO ERROR            
         BNZ   ERRMISS                                                          
         OC    DOVKSEQ,DOVKSEQ     DITTO SEQ                                    
         BNZ   ERRMISS                                                          
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(DOVKRTS-DOVKEY),KEYSAVE    CHECK UPTO RTS                    
         BNE   VK25                           DFLT TO BBM, MOST COMMON          
         MVC   SAVEKEY(L'DOVKEY),KEY          GO WITH THIS KEY                  
         MVC   FHDAD(3,R2),=C'NSI'                                              
         CLI   DOVKRTS,C'0'                                                     
         BE    *+10                                                             
VK25     MVC   FHDAD(3,R2),=C'BBM'                                              
         OI    FHOID(R2),FHOITR                                                 
*                                                                               
VKX      MVC   KEY,SAVEKEY                                                      
*                                                                               
         LA    R2,CONRECH                                                       
         STCM  R2,15,ACURFORC      FORCE CURSOR TO RECORD FIELD                 
*                                                                               
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*  VALIDATE COPY KEY                                                  *         
***********************************************************************         
*                                                                               
         USING DOVRECD,R4                                                       
VKCOPY   XC    DCPFPGM,DCPFPGM                                                  
         OI    DCPFPGMH+FHOID,FHOITR                                            
         XC    DCPTPGM,DCPTPGM                                                  
         OI    DCPTPGMH+FHOID,FHOITR                                            
*                                                                               
         LA    R2,DCPNETH                                                       
         CLI   5(R2),0             ANY NETWORK?                                 
         BE    ERRMISS                                                          
         BAS   RE,VALNET           YES - VALIDATE NETWORK                       
         BNE   ERRINV                                                           
         MVC   DOVKNET,BSTA        NETWORK STATION CODE                         
*                                                                               
         LA    R2,DCPRTSH          RATING SERVICE                               
         CLI   FHILD(R2),0         ANY RATING SERVICE?                          
         BNE   VKC005                                                           
         MVC   FHDAD(3,R2),=C'BBM' DEFAULT TO BBM                               
         MVI   FHILD(R2),3                                                      
         OI    FHIID(R2),FHIITH                                                 
         OI    FHOID(R2),FHOITR                                                 
VKC005   BAS   RE,VALRTS           YES - VALIDATE RATING SERVICE                
         MVC   DOVKRTS,SVRTS       RATING SERVICE                               
*                                                                               
         LA    R2,DCPCLTH          CLIENT                                       
         CLI   FHILD(R2),0         ANY CLIENT?                                  
         BE    VKC10                                                            
         GOTO1 VALICLT             YES - VALIDATE CLIENT                        
         MVC   DOVKCLT,BCLT        CLIENT (PACKED)                              
*                                                                               
VKC10    LA    R2,DCPSEQH          SEQUENCE NUMBER                              
         CLI   5(R2),0             ANY SEQUENCE NUMBER?                         
         BE    VKC20                                                            
         TM    4(R2),X'08'         NUMERIC?                                     
         BZ    ERRNUMB                                                          
         PACK  DUB,8(1,R2)                                                      
         CVB   R5,DUB                                                           
         CH    R5,=H'1'            TEST IN RANGE 1-255?                         
         BH    ERRINV                                                           
         STC   R5,DOVKSEQ          SEQUENCE NUMBER                              
*                                                                               
VKC20    LA    R2,DCPFSHWH         FROM SHOW                                    
         CLI   DCPFSHWH+5,0                                                     
         BE    ERRMISS                                                          
         TM    DCPFSHWH+FHIID,FHIITH  WHEN NEW FROM SHOW INPUT                  
         BZ    VKC25                                                            
         BAS   RE,CLRDEM              CLEAR DEMO LIST                           
         BAS   RE,CLRIMP              CLEAR DEMO IMPRESSION LIST                
         MVI   DCPALLD,0              CLEAR 'ALL' SELECTIONS                    
         MVI   DCPALLDH+FHILD,0                                                 
         OI    DCPALLDH+FHOID,FHOITR                                            
         MVI   DCPALLI,0                                                        
         MVI   DCPALLIH+FHILD,0                                                 
         OI    DCPALLIH+FHOID,FHOITR                                            
VKC25    OC    DCPFSHW,=C'    '                                                 
         MVC   TEMPNETE,DCPNET     NETWORK (OFF SCREEN)                         
         MVC   TEMPPGME,DCPFSHW    SOURCE SHOW (OFF SCREEN)                     
         BAS   RE,VALPROG          VALIDATE PROGRAM                             
*                                                                               
         L     R8,ASPOOLD                                                       
         MVC   DCPFPGM,SPACES-SPOOLD(R8) SOURCE SHOW DETAILS                    
         MVC   DCPFPGM(L'PGMDNAME),TEMPPGM                                      
         MVC   DCPFPGM+PGMDDAY-PGMDETSD(L'PGMDDAY),TEMPDAY                      
         MVC   DCPFPGM+PGMDTIME-PGMDETSD(L'PGMDTIME),TEMPTIME                   
*                                                                               
         LA    R2,DCPTSHWH         TO SHOW                                      
         CLI   FHILD(R2),0                                                      
         BE    ERRMISS                                                          
         OC    FHDAD(4,R2),=C'    '                                             
         MVC   TEMPPGME,FHDAD(R2)                                               
         BAS   RE,VALPROG          YES - VALIDATE PROGRAM                       
         MVC   DOVKPGM,FHDAD(R2)   PROGRAM NAME                                 
*                                                                               
         MVC   DCPTPGM,SPACES-SPOOLD(R8)  DEST SHOW DETAILS                     
         MVC   DCPTPGM(L'PGMDNAME),TEMPPGM                                      
         MVC   DCPTPGM+PGMDDAY-PGMDETSD(L'PGMDDAY),TEMPDAY                      
         MVC   DCPTPGM+PGMDTIME-PGMDETSD(L'PGMDTIME),TEMPTIME                   
*                                                                               
         CLC   DCPFSHW,DOVKPGM     TRYING TO COPY SAME RECORD?                  
         BE    ERRSAME                                                          
*                                                                               
         CLI   DOVKSEQ,X'01'       SEQUENCE RECORD 1?                           
         BNE   *+8                                                              
         BAS   RE,SEQCHECK         CHECK FOR 0 SEQUENCE RECORD                  
*                                                                               
         B     VKX                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE LIST KEY (FILTERS)                                                   
***********************************************************************         
VKL      EQU   *                                                                
         USING DOVRECD,R4                                                       
         LA    R2,LSTNETH                                                       
         CLI   5(R2),0             FILTER ON NETWORK?                           
         BE    VKL010                                                           
         BAS   RE,VALNET           VALIDATE NETWORK                             
         BNE   ERRINV                                                           
         MVC   DOVKNET,BSTA        SET NETWORK STATION CODE IN KEY              
*                                                                               
VKL010   LA    R2,LSTPGMH          PROGRAMME                                    
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         OC    LSTPGM,=C'    '     ENSURE SPACE PADDED                          
*                                                                               
         LA    R2,LSTRTSH          RATING SERVICE                               
         CLI   5(R2),0                                                          
         BE    VKL020                                                           
         MVI   SVRTS,C'1'          RATING SERVICE OF C'1'                       
         CLC   =C'BBM',LSTRTS      RATING SERVICE BBM?                          
         BE    VKL020              NO                                           
         CLC   =C'NSI',LSTRTS      RATING SERVICE NSI?                          
         BNE   ERRINV                                                           
         MVI   SVRTS,C'0'          RATING SERVICE NSI                           
*                                                                               
VKL020   LA    R2,LSTCLTH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    VKL030                                                           
         OC    LSTCLT,=C'    '                                                  
         LA    RF,LSTCLT                                                        
         CLI   LSTCLT,C'>'         START AT FILTER?                             
         BNE   VKL022                                                           
         CLI   LSTCLTH+5,3         MINIMUM OF 2 CHARS AFTER '>'?                
         BL    ERRINV                                                           
         MVI   LSTCLT+3,C'A'       FORCE 'A' FOR FILTERING                      
         LA    RF,1(RF)                                                         
VKL022   GOTO1 CLPACK,DMCB,(RF),SVCLT                                           
VKL030   EQU   *                                                                
         XIT1  ,                                                                
         DROP  R4                                                               
*-----------------------------------------------------------*                   
*                    VALIDATE RECORD                        *                   
*-----------------------------------------------------------*                   
VR       DS    0H                                                               
         XC    ACURFORC,ACURFORC   CLEAR CURSOR O/R - WHY IS IT SET?            
*                                                                               
         BAS   RE,RDPROF           GET 00 USER DEMO PROFILE OPT                 
*                                                                               
         L     R3,AIO                                                           
         USING DOVRECD,R3                                                       
         MVC   SAVEKEY,DOVKEY                                                   
*                                                                               
         CLI   ACTNUM,ACTCOPY      ACTION COPY?                                 
         BE    CR                  YES                                          
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VR5                 YES                                          
*                                                                               
         LA    R4,DOVEL01          A(X'01' ELEMENT)                             
         LLC   R5,DOVEL01+1        ELEM LENGTH                                  
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   SVELEM01(0),0(R4)   SAVE ORIGINAL X'01' ELEMENT                  
*                                                                               
         GOTO1 RECUP,DMCB,(R3),(R4)  REMOVE X'01' ELEMENT FROM RECORD           
         DROP  R3                                                               
*                                                                               
VR5      XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING DOVEL01,R4                                                       
*                                                                               
         MVI   DOVEL01,X'01'       ELEMENT CODE                                 
*                                                                               
         MVC   DOVCDAT,26(R3)      CREATION DATE FROM AIO                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,DOVADAT)    LAST ACTIVITY DATE              
*                                                                               
         LA    R2,DEMSRCBH                                                      
         CLI   DEMSRCBH+5,0        ANY BASE BOOK?                               
         BE    ERRMISS                                                          
*                                                                               
         GOTO1 DATVAL,DMCB,(2,DEMSRCB),WORK                                     
         OC    DMCB(4),DMCB        VALID BASE BOOK?                             
         BZ    ERRINV              NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(3,DOVBBK)                                  
*                                                                               
         LA    R2,DEMUNTBH                                                      
         CLI   DEMUNTBH+5,0        ANY USE TILL BOOK?                           
         BE    ERRMISS                                                          
*                                                                               
         GOTO1 DATVAL,DMCB,(2,DEMUNTB),WORK                                     
         OC    DMCB(4),DMCB        VALID USE TILL BK?                           
         BZ    ERRINV              NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(3,DOVUTBK)                                 
*                                                                               
         SR    R5,R5                                                            
         STC   R5,DEMCOUNT         NUMBER OF DEMOS                              
         SR    R8,R8                                                            
*                                                                               
         LA    R8,DOVDMAXQ         MAXIMUM NUMBER OF DEMOS                      
         LA    R6,DOVDLSTC                                                      
         LA    R2,DEMDEM1H                                                      
VR7      CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VR10                YES                                          
         CLI   5(R2),0             ANY DEMO HERE? (ACTION CHANGE)               
         BE    VR15                NO                                           
         CLC   =C'DELETE',8(R2)    DEMO DELETED IN THIS POSITION?               
         BNE   VR12                YES                                          
         BAS   RE,CLRRATES         CLEARS CORRESPOND. RATINGS IN X'05'          
         B     VR18                                                             
*                                                                               
VR10     CLI   5(R2),0             ANY DEMO HERE? (ACTION ADD)                  
         BNE   VR12                                                             
*                                                                               
         BAS   RE,BMPFLD                                                        
         BCT   R8,VR10                                                          
         B     VR20                                                             
*                                                                               
VR12     LA    R5,IOBLOCK          VALIDATE DEMO                                
         XCEF  (R5),1000                                                        
         USING DBLOCK,R5                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TPT'                                                   
         MVI   DBSELMED,C'C'       CANADIAN                                     
*                                                                               
         CLI   SV00PROF+10,C'Y'    VALIDATE WITH US DEMOS                       
         BNE   *+8                                                              
         OI    DBVOPT,X'80'                                                     
*                                                                               
VR13     GOTO1 VDEMOVAL,DMCB,(1,0(R2)),(1,0(R6)),(C'S',DBLOCK),TMPWRK           
         CLI   DMCB+4,0            VALID DEMO?                                  
         BE    ERRINVD             NO                                           
*                                                                               
VR14     DS    0H                                                               
         L     R3,AIO                                                           
         BAS   RE,VALDEM           CHECK FOR DUPLICATE DEMO                     
*                                                                               
         TM    MYFLAG,NODEMO       DEMO MATCH FOUND IN SEQ. RECORD?             
         BO    ERRDUPD             YES - DUPLICATE DEMO FOUND                   
*                                                                               
VR15     CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VR17                YES                                          
*                                                                               
         CLI   5(R2),0             NO DEMO?                                     
         BNE   VR16                                                             
         BAS   RE,DEMNONE          CHECK IF NULL DEMO IN ORIG POSITION          
*                                                                               
         TM    MYFLAG,NODEMERR     SHOULD BE A 'DELETE' HERE?                   
         BO    ERRDELD             YES                                          
         B     VR18                                                             
                                                                                
VR16     BAS   RE,DEMBIT           CHECK DEMO BITS ON CHANGE (X'80)             
*                                                                               
         CLI   MYDEMCNT,X'FF'      TEST NOT IN OLD ELEM                         
         BE    *+14                                                             
         CLC   DEMCOUNT,MYDEMCNT   MAKE SURE DEMO NOT MOVED                     
         BNE   ERRMVDEM            TELL THEM NOT TO MOVE DEMOS !                
*                                                                               
         TM    MYFLAG,DEMBITON     DEMO BIT ON?                                 
         BZ    *+12                                                             
         MVI   0(R6),X'80'                                                      
         B     VR18                                                             
*                                                                               
VR17     CLI   SV00APRF+1,C'L'     TEST DEFAULT TO LOOKUP                       
         BE    VR18                                                             
         MVI   0(R6),X'80'         ELSE ALWAYS CONSIDER AS INPUT                
*                                                                               
         LA    RE,SET0LIST         ADD DEMO NUM TO LIST OF 0 OVRDS              
         CLI   0(RE),0                                                          
         BE    *+12                                                             
         AHI   RE,1                                                             
         B     *-12                                                             
         IC    R0,DEMCOUNT         MAKE DEMCOUNT 1 BASED                        
         AHI   R0,1                                                             
         STC   R0,0(RE)                                                         
*                                                                               
VR18     BAS   RE,BMPFLD                                                        
         LA    R6,3(R6)            NEXT DEMO LIST ENTRY IN RECORD               
*                                                                               
         XC    0(3,R6),0(R6)       NULLS FOR DELETED DEMO                       
*                                                                               
         SR    R5,R5                                                            
         IC    R5,DEMCOUNT                                                      
         AHI   R5,1                NUMBER OF DEMOS                              
         STC   R5,DEMCOUNT                                                      
         BCT   R8,VR7                                                           
         DROP  R5                                                               
*                                                                               
VR20     LA    R2,DEMDEM1H                                                      
         LA    R5,42                                                            
         STC   R5,DOVEL01+1        ELEMENT LENGTH                               
*                                                                               
VR30     L     R3,AIO                                                           
         USING DOVRECD,R3                                                       
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   VR35                                                             
*                                                                               
         MVC   DOVKEY,SAVEKEY      RECORD KEY                                   
         MVC   DOVAGYA,AGENCY      AGENCY/MEDIA                                 
         MVI   DOVLEN+1,DOVEL01-DOVKEY      RECORD LENGTH                       
*                                                                               
VR35     LA    R5,24(R3)                                                        
*                                                                               
         GOTO1 RECUP,DMCB,(R3),ELEM,(R5)   ADD X'01' ELEMENT                    
         DROP  R4                                                               
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VR40                YES                                          
*                                                                               
         MVI   ELCODE,X'02'                                                     
         L     R3,AIO                                                           
         BAS   RE,GETEL            FIND X'02' ELEMENT                           
         BNE   VR40                X'02' ELEMENT NOT THERE                      
*                                                                               
         LR    R4,R3               A(X'02' ELEMENT)                             
         L     R3,AIO                                                           
*                                                                               
         GOTO1 RECUP,DMCB,(R3),(R4)  REMOVE X'02' ELEMENT FROM RECORD           
*                                                                               
VR40     XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING DOVEL02,R4                                                       
         SR    R3,R3               COUNTER FOR DEMO IMPRESSIONS                 
         STC   R3,DEMCOUNT                                                      
*                                                                               
         MVI   DOVEL02,X'02'       ELEMENT CODE                                 
*                                                                               
         LA    R6,DOVIMPC                                                       
         LA    R2,DEMIMP1H                                                      
*                                                                               
VR50     CLI   8(R2),C'E'          IMPRESSION SHOULD NOT START WITH E           
         BE    ERRIMP                                                           
         CLI   8(R2),C'R'          IMPRESSION SHOULD NOT START WITH R           
         BE    ERRIMP                                                           
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),IOBLOCK                                        
         LA    R3,TEMPHEAD                                                      
         XC    TEMPHEAD,TEMPHEAD                                                
         MVC   5(1,R3),IOBLOCK     LENGTH OF DEMO                               
         LLC   R1,5(R3)                                                         
         LA    R1,8(R1)            ADD 8 FOR HEADER                             
         STC   R1,0(R3)                                                         
         AHI   R1,-9               INCLUDES 'BCTR R1,0'                         
         BM    VR55                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMPHEAD+8(0),IOBLOCK+12                                         
*                                                                               
         LLC   R8,IOBLOCK+1        LENGTH OF IMPRESSION NUMBER                  
*                                                                               
         LA    R5,BLOCK                                                         
         XC    BLOCK,BLOCK                                                      
         USING DBLOCK,R5                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TPT'                                                   
         MVI   DBSELMED,C'C'       CANADIAN                                     
*                                                                               
VR52     GOTO1 VDEMOVAL,DMCB,(1,0(R3)),(15,0(R6)),(C'S',DBLOCK),TMPWRK          
         CLI   DMCB+4,0            VALID DEMO?                                  
         BE    ERRINVD                                                          
*                                                                               
         L     R3,AIO                                                           
         BAS   RE,VALIMP           CHECK FOR DUPLICATE DEMO IMP                 
         TM    MYFLAG,NODEMO                                                    
         BO    ERRDUPD                                                          
*                                                                               
VR54     GOTO1 CASHVAL,DMCB,(1,IOBLOCK+22),(R8)                                 
         CLI   DMCB,0              VALID NUMBER?                                
         BNE   ERRNUMB                                                          
*                                                                               
         OC    DMCB+4(2),DMCB+4    MUST FIT 2 BYTES (MAX=6553.5)                
         BNZ   ERMAXIMP                                                         
         MVC   3(2,R6),DMCB+6      IMPRESSION NUMBER                            
*                                                                               
         LLC   R3,DEMCOUNT                                                      
         LA    R3,1(R3)            NUMBER OF DEMO IMPRESSIONS                   
         STC   R3,DEMCOUNT                                                      
         LA    R6,5(R6)            NEXT DEMO IMPRESSION IN ELEMENT              
*                                                                               
VR55     BAS   RE,BMPFLD                                                        
         LA    R1,DEMIMPXH                                                      
         CR    R2,R1               LAST IMPRESSION ON SCREEN?                   
         BNH   VR50                                                             
*                                                                               
VR60     LLC   R3,DEMCOUNT                                                      
         CHI   R3,0                USER INPUT IMPRESSION?                       
         BNH   VR100               NO                                           
*                                                                               
         MHI   R3,5                5 BYTES PER DEMO IMPRESSION                  
         AHI   R3,2                2 MORE FOR ID AND LENGTH                     
         STC   R3,DOVEL02+1        RECORD LENGTH                                
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LLC   R5,1(R3)                                                         
         AR    R5,R3               A(FIRST AVAILABLE ELEMENT POSITION)          
         L     R3,AIO                                                           
*                                                                               
         GOTO1 RECUP,DMCB,(R3),ELEM,(R5)   ADD X'02' ELEMENT                    
*****                                                                           
VR100    CLI   DEMDMOV,C'!'        SCRIPT HAS TOO MANY DEMO CATEGORIES?         
         BE    ERR2MNYD            GIVE AN ERROR BACK TO SCRIPT                 
*****                                                                           
VR110    CLI   ACTNUM,ACTADD                                                    
         BE    *+12                                                             
         BAS   RE,UPDATE05         UPDATE 05 ELEMS                              
         B     VRX                                                              
         BRAS  RE,ADD05            ADD 05 ELEMS                                 
         BNE   ERR05S                                                           
*                                                                               
VRX      MVC   KEY,SAVEKEY                                                      
         MVC   AIO,AIO3                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         OI    GENSTAT2,RETEQSEL      REDISPLAY SAME SELECTION                  
         B     DR                                                               
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*  COPY RECORD                                                        *         
***********************************************************************         
CR       DS    0H                                                               
         XC    COPYFLAG,COPYFLAG                                                
*                                  *** CHECK FOR DEST DEMDEF REC ***            
         LA    R2,DCPNETH                                                       
         MVC   SAVEKEY,KEY         KEY FOR DEST DEMD                            
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CR10                NOT FOUND                                    
         OI    COPYFLAG,NEWXISTQ   TO RECORD EXISTS                             
         LA    R2,DCPOVRWH                                                      
         CLI   FHILD(R2),0         MUST SAY IF WANT TO OVERWRITE                
         BE    ERRMISS                                                          
         CLI   FHDAD(R2),C'Y'                                                   
         BE    CR10                YES                                          
         CLI   FHDAD(R2),C'N'                                                   
         BNE   ERRINV              NOT 'Y' OR 'N'                               
         B     CRX                 NO - DON'T OVERWRITE RECORD - EXIT           
*                                                                               
*                                  *** CHECK FOR SOURCE DEMDEF REC ***          
CR10     XC    KEY,KEY                                                          
         MVC   KEY,SAVEKEY         DEST DEMD KEY                                
         LA    R3,KEY                                                           
         USING DOVRECD,R3                                                       
         MVC   DOVKPGM,DCPFSHW     REPLACE SHOW WITH SOURCE SHOW                
         DROP  R3                                                               
*                                                                               
         LA    R2,DCPFSHWH                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERRNOTF                                                          
         GOTO1 GETREC              GET SOURCE/FROM DEMDEF RECORD                
*                                                                               
         L     R3,AIO                                                           
         USING DOVRECD,R3                                                       
         CLI   DOVEL01,X'01'       X'01' ELEMENT PRESENT?                       
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,DISPDEM          DISPLAY DEMO                                 
         BAS   RE,DISPIMP          DISPLAY IMPRESSIONS                          
*                                                                               
*                                  *** VALIDATE DEMOS TO COPY ***               
         MVI   SVDEMO,C'X'         INIT ALL DEMOS SELECTED                      
         MVC   SVDEMO+1(L'SVDEMO-1),SVDEMO                                      
         LA    R2,DCPALLDH                                                      
         CLI   FHILD(R2),0         SELECT ALL?                                  
         BE    ERRMISS                                                          
         CLI   FHDAD(R2),C'Y'                                                   
         BE    CR30                - YES                                        
*                                                                               
         LA    R2,DCPDEM1H                                                      
         LA    R4,DOVDMAXQ         MAXIMUM NUMBER OF DEMOS                      
         LA    R5,SVDEMO                                                        
         XC    SVDEMO,SVDEMO                                                    
CR20     CLI   FHILD(R2),0         ANY INPUT TAKEN AS SELECT                    
         BE    CR25                                                             
         CLI   FHDAD(R2),C'N'                                                   
         BE    CR25                OBVIOUSLY NOT 'N' THOUGH!                    
         MVI   FHDAD(R2),C'Y'      DEMO POSITION MARKED FOR SELECT              
         OI    FHOID(R2),FHOITR                                                 
         MVI   0(R5),C'X'          DEMO POSITION MARKED FOR SELECT              
CR25     LA    R5,1(R5)                                                         
         BAS   RE,BMPFLD                                                        
         BAS   RE,BMPFLD                                                        
         BCT   R4,CR20                                                          
*                                                                               
*                                  *** VALIDATE IMPS TO COPY ***                
CR30     LA    R2,DCPALLIH                                                      
         CLI   5(R2),0             COPY ALL IMPS?                               
         BE    ERRMISS                                                          
*???     OI    6(R2),X'80'                                                      
         CLI   8(R2),C'Y'                                                       
         BNE   *+12                NO                                           
         OI    COPYFLAG,ALLIMPSQ   YES - COPY ALL IMPRESSIONS                   
         B     *+12                                                             
         CLI   8(R2),C'N'                                                       
         BNE   ERRINV              NO                                           
*                                                                               
*                                  *** VALIDATE ANY ADJUSTMENT ***              
         LA    R2,DCPADJH                                                       
         CLI   DCPADJH+5,0         ANY ADJUST PERCENTAGE?                       
         BE    *+8                                                              
         BAS   RE,ADJUST                                                        
*                                                                               
*                                  *** DO THE COPY ***                          
         LA    R2,DCPDEM1H                                                      
*???     OI    DCPALLDH+6,X'80'                                                 
         OC    DCPTSHW,=C'    '                                                 
         MVC   DOVKPGM,DCPTSHW     PUT NEW SHOW IN KEY                          
*                                                                               
         CLI   DOVEL01,X'01'       X'01' ELEMENT PRESENT?                       
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,COPY01EL         BUILD X'01' ELEMENT WITH NEW DEMOS           
*                                                                               
         L     R3,AIO                                                           
         LA    R4,DOVEL01          A(X'01' ELEMENT)                             
*                                                                               
         GOTO1 RECUP,DMCB,(R3),(R4)       REMOVE X'01' ELEMENT                  
         GOTO1 RECUP,DMCB,(R3),SVELEM01,(R4)   ADD X'01' ELEMENT                
*                                                                               
         BRAS  RE,DEMCNT                                                        
*                                                                               
         MVI   ELCODE,X'05'        X'05' ELEMENT - RATINGS FOR DEMOS            
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         BAS   RE,COPY05EL         BUILD X'05' ELEMENT WITH RATINGS             
*                                                                               
         BAS   RE,COPY02EL         CHANGE X'02'ELEM WITH DEM IMPS               
*                                                                               
         MVI   DCPOVRW,X'00'       CLEAR ANY OVERWRITE FOR NEXT TIME            
         MVI   DCPOVRWH+FHILD,0                                                 
         OI    DCPOVRWH+FHOID,FHOITR                                            
*                                                                               
         TM    COPYFLAG,NEWXISTQ   CHANGE DEST DEMO RECORD ON COPY?             
         BZ    CR70                NO - MUST ADD DEST DEMO RECORD               
*                                                                               
         MVC   AIO,AIO2                                                         
         MVC   KEY,SAVEKEY         DESTINATION RECORD                           
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC                                                           
         B     CRX                                                              
*                                                                               
CR70     GOTO1 ADDREC                                                           
*                                                                               
CRX      XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*-----------------------------------------------------------*                   
*                    DISPLAY RECORD                         *                   
*-----------------------------------------------------------*                   
DR       DS    0H                                                               
         BAS   RE,RDPROF           GET 00 USER DEMO PROFILE OPT                 
*                                                                               
         BAS   RE,CLRDEM           CLEAR DEMO LIST                              
         BAS   RE,CLRIMP           CLEAR DEMO IMPRESSIONS LIST                  
*                                                                               
         L     R4,AIO                                                           
         USING DOVRECD,R4                                                       
         CLI   DOVEL01,X'01'       X'01' ELEMENT PRESENT?                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(3,DOVBBK),(6,WORK)    BASE BOOK                     
         FOUT  DEMSRCBH,WORK,6                                                  
         GOTO1 DATCON,DMCB,(3,DOVUTBK),(6,WORK)   USE TILL BOOK                 
         FOUT  DEMUNTBH,WORK,6                                                  
*                                                                               
         BAS   RE,DISPDEM          DISPLAY DEMO LIST                            
*                                                                               
         BAS   RE,DISPIMP          DISPLAY DEMO IMPRESSIONS                     
*                                                                               
         CLI   NONRTGDM,C'Y'       DISPLAY WARNING MESSAGES                     
         BE    MYDISP2                                                          
*>>>     OI    DEMMSGHH+FHATD,FHATLO                                            
         OI    DEMMSGDH+FHATD,FHATLO                                            
*>>>     OI    DEMMSGIH+FHATD,FHATLO                                            
         B     MYDISP3                                                          
MYDISP2  EQU   *                                                                
*>>>     NI    DEMMSGHH+FHATD,255-FHATLO                                        
         NI    DEMMSGDH+FHATD,255-(FHATLO-FHATHI)                               
*>>>     NI    DEMMSGIH+FHATD,255-FHATLO                                        
MYDISP3  EQU   *                                                                
*>>>     OI    DEMMSGHH+FHOID,FHOITR                                            
         OI    DEMMSGDH+FHOID,FHOITR                                            
*>>>     OI    DEMMSGIH+FHOID,FHOITR                                            
*                                                                               
DRX      OI    GENSTAT2,DISTHSPG   REDISPLAY SAME LIST PAGE                     
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LIST RECORDS                                                        *         
***********************************************************************         
*                                                                               
LR       LA    R4,KEY                                                           
         USING DOVRECD,R4                                                       
         OC    KEY(L'DOVKEY),KEY   TEST FOR 1ST PASS (KEY NULL)                 
         BNZ   LR10                NO                                           
         MVC   KEY,SAVEKEY                                                      
LR10     GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR15     GOTO1 SEQ                                                              
*                                                                               
*R20     CLC   KEY(3),KEYSAVE      SAME REC TYPE/AGENCY                         
LR20     CLC   KEY(3),SAVEKEY      SAME REC TYPE/AGENCY                         
         BNE   LRX                                                              
         CLI   LSTNETH+5,0         FILTER BY NETWORK?                           
         BE    *+14                                                             
         CLC   DOVKNET,SAVEKEY+(DOVKNET-DOVKEY)  STILL REQD NWK?                
         BNE   LRX                 NO - DONE                                    
         BAS   RE,FILTCHK                                                       
         BNE   LR15                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         XC    LISTAR,LISTAR                                                    
*                                                                               
         XC    TMPWRK,TMPWRK       MARKET IS NETWORK = NULL                     
         MVC   TMPWRK+2(2),DOVKNET DITTO FOR STATION SEQ                        
         GOTO1 MSUNPK,DMCB,TMPWRK,TMPWRK+5,TMPWRK+9  NETWORK (EBCDIC)           
         MVC   LSTNETD,TMPWRK+9                                                 
*                                                                               
         MVC   LSTPGMD,DOVKPGM     PROGRAM (EBCDIC)                             
*                                                                               
         MVC   LSTRTSD,=C'BBM '    RATING SERVICE BBM                           
         CLI   DOVKRTS,C'1'        RATING SERVICE OF C'1'?                      
         BE    *+10                NO                                           
         MVC   LSTRTSD,=C'NSI '    RATING SERVICE NSI                           
*                                                                               
         EDIT  DOVKSEQ,LSTSEQD,ZERO=BLANK    SEQUENCE NUMBER                    
*                                                                               
         GOTO1 CLUNPK,DMCB,DOVKCLT,LSTCLTD   CLIENT                             
*                                                                               
         CLI   DOVEL01,X'01'       X'01' ELEMENT PRESENT?                       
         BE    *+6                                                              
         DC    H'00'               X'01' ELEMENT SHOULD BE PRESENT              
         GOTO1 DATCON,DMCB,(X'43',DOVBBK),(6,LSTBASE)   - BASE BOOK             
         GOTO1 DATCON,DMCB,(X'43',DOVUTBK),(6,LSTUNTIL) - USE TILL BOOK         
*                                                                               
         L     R3,AIO              COUNT NUMBER OF DEMO CATS                    
         MVI   ELCODE,X'01'        01 ELEMENT USED TO FIND DEMO                 
         USING DOVEL01,R3          POSITION                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   1(R3),DOV01LNQ                                                   
         BE    *+6                                                              
         DC    H'0'                SHOULD BE L'FIXED FOR 10 DEMOS               
         LHI   RF,DOVDMAXQ         RF=#POSSIBLE DEMOS                           
*                                                                               
         LA    R1,DOVDLSTC                                                      
         SR    RE,RE               CLEAR DEMO COUNTER                           
         MVI   BYTE,0                                                           
LR30     MVC   FULL(L'DOVDEMO),0(R1)                                            
         NI    FULL,X'FF'-DOVDVALQ TURN OFF HIGH ORDER BIT                      
         OC    FULL(3),FULL        DEMO NULL (DELETED)?                         
         BZ    LR32                IGNORE THIS POSITON                          
         TM    0(R1),DOVDVALQ      CHECK DEMO HAS VALUE                         
         BZ    *+8                                                              
         OI    BYTE,DOVDVALQ                                                    
         LA    RE,1(RE)                                                         
LR32     LA    R1,L'DOVDEMO(R1)                                                 
         BCT   RF,LR30                                                          
         LTR   RE,RE                                                            
         BZ    LR33                                                             
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LSTDEMO(2),DUB                                                   
         CLI   LSTDEMO,C'0'                                                     
         BNE   *+8                                                              
         MVI   LSTDEMO,C' '                                                     
*NOOP    TM    BYTE,DOVDVALQ                                                    
*NOOP    BNZ   *+8                                                              
*NOOP    MVI   LSTDEMOX,C'*'       FLAG NO DEMO VALUE SET                       
         DROP  R3                                                               
*                                                                               
LR33     L     R3,AIO              COUNT NUMBER OF AUD IMPS CATS                
         MVI   ELCODE,X'02'                                                     
         USING DOVEL02,R3                                                       
         BAS   RE,GETEL                                                         
         BNE   LR40                                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RF,DOVEL02+1                                                     
         SHI   RF,2                                                             
         LTR   RF,RF                                                            
         BZ    LR40                                                             
         D     RE,=F'5'                                                         
         LR    RE,RF                                                            
         LA    R1,DOVIMPC                                                       
LR35     OC    0(L'DOVIMPC,R1),0(R1)                                            
         BNZ   *+8                 IGNORE THIS POSITON                          
         SHI   RE,1                                                             
         LA    R1,L'DOVIMPC(R1)                                                 
         BCT   RF,LR35                                                          
         LTR   RE,RE                                                            
         BZ    LR40                                                             
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LSTIMP(2),DUB                                                    
         CLI   LSTIMP,C'0'                                                      
         BNE   *+8                                                              
         MVI   LSTIMP,C' '                                                      
         DROP  R3                                                               
*                                                                               
LR40     GOTO1 LISTMON                                                          
*        MVC   KEYSAVE,KEY                                                      
         LA    R4,KEY                                                           
         B     LR15                                                             
*                                                                               
LRX      XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*-----------------------------------------------------------*                   
*                    DISPLAY KEY                            *                   
*-----------------------------------------------------------*                   
DK       DS    0H                                                               
*                                                                               
         L     R4,AIO                                                           
         USING DOVRECD,R4                                                       
         XC    DEMNET,DEMNET                                                    
         OI    DEMNETH+6,X'80'                                                  
         XC    DEMPGM,DEMPGM                                                    
         OI    DEMPGMH+6,X'80'                                                  
         XC    DEMPGMD,DEMPGMD                                                  
         OI    DEMPGMDH+6,X'80'                                                 
         XC    DEMRTS,DEMRTS                                                    
         OI    DEMRTSH+6,X'80'                                                  
         XC    DEMCLT,DEMCLT                                                    
         OI    DEMCLTH+6,X'80'                                                  
*                                                                               
         XC    TMPWRK,TMPWRK       MARKET IS NETWORK = NULL                     
         MVC   TMPWRK+2(2),DOVKNET DITTO FOR STATION SEQ                        
         GOTO1 MSUNPK,DMCB,TMPWRK,TMPWRK+5,TMPWRK+9  NETWORK (EBCDIC)           
         MVC   DEMNET(4),TMPWRK+9                                               
         OI    DEMNETH+6,X'80'                                                  
*                                                                               
         MVC   DEMPGM,DOVKPGM      PROGRAM (EBCDIC)                             
         OI    DEMPGMH+6,X'80'                                                  
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         MVC   TEMPNETE,DEMNET     NETWORK OFF SCREEN                           
         MVC   TEMPPGME,DEMPGM     PROGRAM OFF SCREEN                           
*                                                                               
         BAS   RE,VALPROG                                                       
*                                                                               
         MVC   DEMPGMD(L'PGMDNAME),TEMPPGM                                      
         MVC   DEMPGMD+PGMDDAY-PGMDETSD(L'PGMDDAY),TEMPDAY                      
         MVC   DEMPGMD+PGMDTIME-PGMDETSD(L'PGMDTIME),TEMPTIME                   
         OI    DEMPGMDH+6,X'80'                                                 
*                                                                               
         MVC   KEY(L'DOVKEY),SAVEKEY      RESTORE KEY                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOVKEY),KEYSAVE      RECORD EXIST?                         
         BNE   ERRINV              RECORD SHOULD EXIST                          
         GOTO1 GETREC              RESTORE RECORD TO CHECK NETWORK              
*                                                                               
         MVC   DEMRTS,=C'BBM '     RATING SERVICE BBM                           
         CLI   DOVKRTS,C'1'        RATING SERVICE OF C'1'?                      
         BE    *+10                NO                                           
         MVC   DEMRTS,=C'NSI '     RATING SERVICE NSI                           
         OI    DEMRTSH+6,X'80'                                                  
*                                                                               
         OC    DOVKCLT,DOVKCLT                                                  
         BZ    DK08                                                             
         GOTO1 CLUNPK,DMCB,DOVKCLT,DEMCLT    CLIENT                             
         OI    DEMCLTH+6,X'80'                                                  
*                                                                               
DK08     LLC   R1,DOVKSEQ          SEQUENCE NUMBER                              
         CVD   R1,DUB                                                           
         UNPK  DEMSEQ,DUB                                                       
         OI    DEMSEQ,X'F0'                                                     
*                                                                               
         CLI   DEMSEQ,C'0'         SEQUENCE RECORD 0?                           
         BE    DK10                                                             
         OI    DEMSEQH+6,X'80'     NO - SEQUENCE RECORD 1                       
         B     DKX                                                              
*                                                                               
DK10     XC    DEMSEQ,DEMSEQ       YES - SEQUENCE RECORD 0                      
         OI    DEMSEQH+6,X'80'                                                  
*                                                                               
DKX      XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*-----------------------------------------------------------*                   
*  COPY - ADJUST PERCENTAGE OF RATINGS                      *                   
*-----------------------------------------------------------*                   
ADJUST   NTR1                                                                   
         LA    R2,DCPADJH                                                       
         CLI   DCPADJ,C'+'         INCREASE RATINGS?                            
         BNE   *+12                                                             
         OI    COPYFLAG,ADJPOSQ    YES                                          
         B     ADJUST5                                                          
         CLI   DCPADJ,C'-'         DECREASE RATINGS?                            
         BNE   ERRADJ                                                           
         OI    COPYFLAG,ADJNEGQ                                                 
*                                                                               
ADJUST5  LLC   RF,DCPADJH+5        INPUT LENGTH                                 
         BCTR  RF,0                SUBTRACT OFF FOR SIGN                        
*                                                                               
         GOTO1 CASHVAL,DMCB,(1,DCPADJ+1),(RF)                                   
         CLI   DMCB,0              VALID NUMBER?                                
         BNE   ERRNUMB                                                          
*                                                                               
         L     RF,DMCB+4           PERCENTAGE IN HEX                            
         CH    RF,=H'1000'                                                      
         BH    ERRINV                                                           
         STH   RF,SVADJ                                                         
*                                                                               
ADJUSTX  XIT1                                                                   
         EJECT                                                                  
*-----------------------------------------------------------*                   
*            BUILD X'01' ELEMENT SUBROUTINE                 *                   
*-----------------------------------------------------------*                   
COPY01EL NTR1                                                                   
         L     R3,AIO                                                           
         USING DOVRECD,R3                                                       
*                                                                               
         LLC   R5,DOVEL01+1        X'01' ELEMENT LENGTH                         
         SH    R5,=H'12'           NUMBER OF FIXED BYTES IN ELEMENT             
         BNP   ERRINV                                                           
         SR    R4,R4                                                            
         D     R4,=F'3'            FIND NUMBER OF DEMOS                         
         STC   R5,DEMCOUNT                                                      
*                                                                               
         XC    SVELEM01,SVELEM01   BUILD X'01' ELEMENT HERE                     
         LA    R4,DOVEL01                                                       
         MVC   SVELEM01(12),0(R4)  COPY X'01' ELEMENT                           
         MVI   SVELEM01+1,DOV01LNQ FIXED LENGTH OF 42                           
*                                                                               
         LA    R2,DCPDEM1H                                                      
         LA    R4,DOVDLSTC         DEMO LIST IN SOURCE SHOW                     
         LA    R5,SVDEMO           DEMO POSITION MARKERS                        
         LA    R6,SVELEM01+12      DEMOS IN DEST SHOW                           
*        LA    R8,10               MAXIMUM NUMBER OF DEMOS                      
         LLC   R8,DEMCOUNT         NUMBER OF DEMOS                              
*                                                                               
COPY0105 CLI   0(R5),0             IS THIS DEMO SELECTED?                       
         BNE   COPY0110            YES                                          
         LA    R4,3(R4)            NEXT DEMO IN SOURCE                          
         LA    R5,1(R5)            NEXT DEMO POSITION                           
         BCT   R8,COPY0105                                                      
         B     COPY01X                                                          
*                                                                               
COPY0110 CLI   DCPALLD,C'Y'        SELECT ALL DEMOS?                            
         BE    *+14                                                             
         CLC   0(3,R4),=X'000000'  ANY DEMO HERE?                               
         BE    ERRINV                                                           
         MVC   0(3,R6),0(R4)       COPY DEMO FROM SOURCE TO DEST                
         LA    R6,3(R6)            NEXT DEMO IN DEST                            
         LA    R4,3(R4)            NEXT DEMO IN SOURCE                          
         LA    R5,1(R5)            NEXT DEMO POSITION                           
         OI    COPYFLAG,COPYOKQ    SOMETHING WAS COPIED                         
         BCT   R8,COPY0105                                                      
*                                                                               
COPY01X  XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*-----------------------------------------------------------*                   
*  COPY - BUILD X'05' ELEMENT SUBROUTINE                    *                   
*-----------------------------------------------------------*                   
COPY05EL NTR1                                                                   
         L     R3,AIO                                                           
         USING DOVRECD,R3                                                       
*                                                                               
         MVI   ELCODE,X'05'        X'05' ELEMENT - RATINGS FOR DEMOS            
         BAS   RE,GETEL                                                         
         BNE   CRX                 NO - X'05' ELEMENT DOESN'T EXIST             
*                                                                               
COPY0500 XC    SVELEM05,SVELEM05                                                
         MVC   SVELEM05(5),0(R3)   BUILD X'05' ELEMENT HERE                     
         MVI   SVELEM05+1,25       FIXED LENGTH OF 25                           
*                                                                               
         LA    R6,SVELEM05+5       A(FIRST RATING)                              
         LA    R5,SVDEMO           DEMO POSITION MARKERS                        
         LA    R4,5(R3)            A(FIRST RATING)                              
         LA    R8,DOVDMAXQ                                                      
*                                                                               
COPY0505 CLI   0(R5),0             THIS DEMO SELECTED?                          
         BNE   COPY0510            YES                                          
         LA    R5,1(R5)            NEXT DEMO POSITION MARKER                    
         LA    R4,2(R4)            NEXT RATING IN SOURCE                        
         BCT   R8,COPY0505                                                      
         B     COPY0520                                                         
*                                                                               
COPY0510 OC    0(2,R4),0(R4)       ANY RATING?                                  
         BZ    COPY515                                                          
*                                                                               
         TM    COPYFLAG,ADJPCTQ    ADJUST PERCENTAGE?                           
         BZ    COPY515                                                          
*                                                                               
         SR    R0,R0                                                            
         LH    R1,0(R4)                                                         
         MH    R1,SVADJ                                                         
         D     R0,=F'1000'                                                      
*                                                                               
         TM    COPYFLAG,ADJPOSQ    INCREASE RATING?                             
         BZ    *+12                                                             
         AH    R1,0(R4)            YES                                          
         B     COPY512                                                          
         LH    R0,0(R4)                                                         
         SR    R0,R1                                                            
         LR    R1,R0                                                            
*                                                                               
COPY512  STH   R1,0(R6)                                                         
         B     *+10                                                             
COPY515  MVC   0(2,R6),0(R4)       COPY RATING FOR DEMO                         
         LA    R5,1(R5)            NEXT DEMO POSITION MARKER                    
         LA    R4,2(R4)            NEXT RATING IN SOURCE                        
         LA    R6,2(R6)            NEXT RATING IN DEST                          
         BCT   R8,COPY0505                                                      
*                                                                               
COPY0520 LR    R4,R3               A(X'05' ELEMENT)                             
         L     R3,AIO                                                           
* CALCULATE ELEMENT LENGTH FROM NUMBER OF DEMOS AND PUT IT IN                   
         LLC   R0,DEMONUM                                                       
         MHI   R0,2                                                             
         AHI   R0,5                                                             
         STC   R0,SVELEM05+1                                                    
*                                                                               
         GOTO1 RECUP,DMCB,(R3),(R4)  REMOVE X'05' ELEMENT FROM RECORD           
*                                                                               
         GOTO1 RECUP,DMCB,(R3),SVELEM05,(R4)   ADD X'05' ELEMENT                
*                                                                               
         LR    R3,R4                                                            
         BAS   RE,NEXTEL           GET NEXT X'05' ELEMENT                       
         BE    COPY0500                                                         
*                                                                               
COPY05X  XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*-----------------------------------------------------------*                   
*      DO NOT COPY UNSELECTED DEMO IMPRESSIONS               *                  
*-----------------------------------------------------------*                   
COPY02EL NTR1                                                                   
         L     R3,AIO                                                           
         MVI   ELCODE,X'02'        DEMO IMP ELEM                                
         BAS   RE,GETEL                                                         
         BNE   COPY02X                                                          
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RF,1(R3)            L'ORIGINAL ELEM                              
         AHI   RF,-2               -FIXED PORTION (UP TO DEMO LIST)             
         BZ    COPY02X                                                          
         D     RE,=F'5'                                                         
         LR    R6,RF               #IMPS IN ORIGINAL ELEM                       
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'02'          IMP EL CODE                                  
         MVI   ELEM+1,2            EL CODE AND LENGTH                           
*                                                                               
         LA    R2,DCPDI1H          POINT TO FIRST IMP ON SCREEN                 
         LA    R4,ELEM+2           FIRST IMP SLOT IN NEW ELEM                   
         LA    R5,2(R3)            FIRST IMP IN SOURCE ELEM                     
*                                                                               
COPY0210 TM    COPYFLAG,ALLIMPSQ   COPY ALL IMPRESSIONS?                        
         BO    COPY0212             YES                                         
*                                                                               
         CLI   5(R2),0             IS THE IMP SELECTED?                         
         BE    COPY0220            NO                                           
         CLI   FHDAD(R2),C'N'                                                   
         BE    COPY0220            OBVIOUSLY NOT!                               
         MVI   FHDAD(R2),C'Y'      SELECTED - MOVE IN A 'Y'                     
         OI    FHOID(R2),FHOITR                                                 
*                                                                               
COPY0212 OI    COPYFLAG,COPYOKQ    SOMETHING WAS COPIED                         
         MVC   0(5,R4),0(R5)       COPY IMPRESSION                              
*                                                                               
         TM    COPYFLAG,ADJPCTQ    ADJUST PERCENTAGE?                           
         BZ    COPY0215            NO                                           
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         ICM   RF,3,3(R4)          GET IMP # FROM RECORD                        
         LR    R1,RF                                                            
         MH    R1,SVADJ            CALCULATE PERCENTAGE                         
         D     R0,=F'1000'                                                      
         TM    COPYFLAG,ADJPOSQ    INCREASE RATING?                             
         BZ    *+10                                                             
         AR    RF,R1               YES - RF CONTAINS NEW IMP #                  
         B     *+6                                                              
         SR    RF,R1               NO - RF CONTAINS NEW IMP #                   
         STCM  RF,3,3(R4)          PUT NEW IMP # IN RECORD                      
*                                                                               
COPY0215 LA    R4,5(R4)            BUMP TO NEXT AVAILABLE POSITION              
         LLC   R1,ELEM+1           INCREMENT ELEMENT LENGTH                     
         LA    R1,5(R1)                                                         
         STC   R1,ELEM+1                                                        
*                                                                               
COPY0220 LA    R5,5(R5)            NEXT IMP IN SOURCE ELEM                      
         BAS   RE,BMPFLD           POINT TO NEXT IMP ON SCREEN                  
         BAS   RE,BMPFLD                                                        
         BCT   R6,COPY0210                                                      
*                                                                               
COPY0230 DS    0H                                                               
         GOTO1 RECUP,DMCB,AIO,(R3)  REMOVE X'02' ELEMENT FROM RECORD            
         GOTO1 RECUP,DMCB,AIO,ELEM,(R3)   ADD X'02' ELEMENT                     
*                                                                               
COPY02X  DS    0H                                                               
         LA    R2,DCPDEM1H                                                      
         TM    COPYFLAG,COPYOKQ    SOMETHING SELECTED FOR COPY?                 
         BZ    ERRONED             YES                                          
         XIT1                                                                   
***********************************************************************         
* UPDATE 05 ELEMS TO CATER FOR NUMBER OF DEMOS IN DEMOLIST            *         
* 9JUL02 - ENSURE 05 ELEMS LARGE ENOUGH TO CATER FOR DEMOLIST         *         
* PREVIOUSLY USER HAD TO DEMOVER AFTER CHANGING DEMOLIST TO GET THE   *         
* 05 ELEMS REBUILT. IF FORGOT THEN C,LOOK READ PAST END OF 05 GIVING  *         
* 05 GIVING DUFF VALUES AND IF LATER TOOK OUT THE DEMO FROM LIST THE  *         
* CLRRATE BUG APPEARED                                                *         
* NOTE: WHEN ADD NEW DEMODEF STILL NEED TO DEMOVER TO ADD 05S FOR ALL *         
*       STATIONS/SPILL                                                *         
***********************************************************************         
         SPACE                                                                  
UPDATE05 NTR1  ,                                                                
         BRAS  RE,DEMCNT           NUMBER OF DEMOS                              
         L     R3,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   UPD05X              WON'T BE ANY ON ADD                          
*                                                                               
         SR    R2,R2               CALC MIN SIZE OF 05 ELEMS FOR DEMOS          
         IC    R2,DEMONUM                                                       
         MHI   R2,2                                                             
         AHI   R2,5                                                             
         CLM   R2,1,1(R3)                                                       
         BH    UPD0510                                                          
         OC    SET0LIST,SET0LIST   TEST NEED TO SET 0 OVRDS                     
         BZ    UPD05X              NO - 05 ELEMS LARGE ENOUGH                   
*                                                                               
UPD0510  XC    SVELEM05,SVELEM05                                                
         SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVELEM05(0),0(R3)   COPY CURRENT 05 ELEM                         
         STC   R2,SVELEM05+1       SET NEW LENGTH                               
*                                                                               
         LA    RE,SET0LIST         GET LIST OF NEW ZERO DEM OVRDS               
*                                                                               
UPD0512  CLI   0(RE),0                                                          
         BE    UPD0514                                                          
         SR    R1,R1                                                            
         IC    R1,0(RE)                                                         
         BCTR  R1,0                                                             
         AR    R1,R1                                                            
         LA    R1,SVELEM05+5(R1)                                                
         XC    0(2,R1),0(R1)       CLEAR OLD VALUE                              
         CLI   SV00APRF+1,C'L'     TEST DEFAULT=LOOKUP                          
         BE    *+8                                                              
         OI    0(R1),X'80'         SET ZERO OVERRIDE                            
         AHI   RE,1                                                             
         B     UPD0512                                                          
*                                                                               
UPD0514  LR    R4,R3               A(X'05' ELEMENT)                             
         L     R3,AIO                                                           
         GOTO1 RECUP,DMCB,(R3),(R4)           REMOVE X'05' ELEMENT              
*        GOTO1 RECUP,DMCB,(R3),SVELEM05,(R4)  ADD NEW X'05' ELEMENT             
         GOTO1 RECUP,DMCB,(R3),SVELEM05,(C'R',(R4)) ADD NEW X'05' ELEM          
         CLI   8(R1),0                        OVERFLOW?                         
         BE    ERRSEQ                         YES                               
         LR    R3,R4                                                            
         BAS   RE,NEXTEL           GET NEXT X'05' ELEMENT                       
         BE    UPD0510                                                          
*                                                                               
UPD05X   XIT1  ,                                                                
         EJECT                                                                  
*-----------------------------------------------------------*                   
*              CLEAR DEMO LIST (ON SCREEN)                  *                   
*-----------------------------------------------------------*                   
CLRDEM   NTR1                                                                   
         SR    R4,R4                                                            
         CLI   ACTNUM,ACTCOPY      ACTION COPY?                                 
         BE    CLRDEM20            YES                                          
*                                                                               
         LA    R2,DEMDEM1H                                                      
         LA    R4,DOVDMAXQ         MAXIMUM NUMBER OF DEMOS                      
CLRDEM10 XC    8(L'DEMDEM1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         BAS   RE,BMPFLD                                                        
         BCT   R4,CLRDEM10                                                      
         B     CLRDEMX                                                          
*                                                                               
CLRDEM20 LA    R2,DCPDEM1H                                                      
         LA    R4,DOVDMAXQ         MAX DEMOS AND SELECT FIELDS                  
CLRDEM30 XC    8(L'DCPDEM1,R2),8(R2)                                            
         MVI   FHILD(R2),0         CLEAR INPUT FIELD PROPERLY!                  
         OI    6(R2),X'80'                                                      
         BAS   RE,BMPFLD                                                        
         XC    8(L'DCPDM1N,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         BAS   RE,BMPFLD                                                        
         BCT   R4,CLRDEM30                                                      
*                                                                               
CLRDEMX  XIT1                                                                   
*-----------------------------------------------------------*                   
*            CLEAR DEMO IMPRESSIONS (ON SCREEN)             *                   
*-----------------------------------------------------------*                   
CLRIMP   NTR1                                                                   
         SR    R4,R4                                                            
         CLI   ACTNUM,ACTCOPY      ACTION COPY?                                 
         BE    CLRIMP20            YES                                          
*                                                                               
         LA    R2,DEMIMP1H                                                      
         LA    R4,15               MAXIMUM NUMBER OF IMPRESSIONS                
CLRIMP10 XC    8(L'DEMIMP1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         BAS   RE,BMPFLD                                                        
         BCT   R4,CLRIMP10                                                      
         B     CLRIMPX                                                          
*                                                                               
CLRIMP20 LA    R2,DCPDI1H                                                       
         LA    R4,DOVIMAXQ         MAX DEMO IMPS AND SELECT FIELDS              
CLRIMP30 XC    8(1,R2),8(R2)                                                    
         MVI   FHILD(R2),0         CLEAR INPUT FIELD PROPERLY!                  
         OI    6(R2),X'80'                                                      
         BAS   RE,BMPFLD                                                        
         XC    8(14,R2),8(R2)                                                   
         OI    6(R2),X'80'                                                      
         BAS   RE,BMPFLD                                                        
         BCT   R4,CLRIMP30                                                      
*                                                                               
CLRIMPX  XIT1                                                                   
         EJECT                                                                  
*-----------------------------------------------------------*                   
*          CLEAR RATINGS FOR CORRESPONDING DEMOS            *                   
*-----------------------------------------------------------*                   
CLRRATES NTR1                                                                   
*                                                                               
         LA    R6,DOVDMAXQ                                                      
         SR    R6,R8               POSITION OF DELETED DEMO                     
         MH    R6,=H'2'            CORRESPONDING POS. IN X'05' ELEM             
*                                                                               
         L     R3,AIO                                                           
         USING DOVEL05,R3                                                       
*                                                                               
         MVC   BYTE,ELCODE                                                      
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL            ANY X'05' ELEMENT?                           
CLRRATE5 BNE   CLRRATEX            NO - EXIT                                    
*                                                                               
         LA    R4,DOVDEMV          A(1ST RATING IN X'05' ELEM)                  
         LA    R4,0(R6,R4)                                                      
* 25FEB02 - SOME INSTANCES OF CLEARING PAST END-OF-ELEM                         
* THUS CORRUPTING FOLLOWING ELEM LENGTH - HOPEFULLY JUST A FEW DUFF             
* ELEMS FROM EARLIER BUG CAUSING THIS                                           
         SR    R1,R1               POINT R1 TO LAST DEMO ENTRY IN ELEM          
         IC    R1,1(R3)                                                         
         AR    R1,R3                                                            
         SHI   R1,2                                                             
         CR    R4,R1               ENSURE NOT CLEARING PAST END                 
         BH    *+10                YEP - DON'T CLEAR ANYTHING THEN              
*                                                                               
         XC    0(2,R4),0(R4)                                                    
*                                                                               
         BAS   RE,NEXTEL           GET NEXT X'05' ELEMENT                       
         B     CLRRATE5                                                         
*                                                                               
CLRRATEX MVC   ELCODE,BYTE                                                      
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*-----------------------------------------------------------*                   
*   CHECK X'80' BIT IN ORIG ELEMENT (DEMO - ACTION CHANGE)  *                   
*-----------------------------------------------------------*                   
DEMBIT   NTR1                                                                   
         MVI   MYDEMCNT,X'FF'      SET NON-MATCHING VALUE FOR MCBEALS           
*                                                                               
         LA    R1,SVELEM01                                                      
         LA    R1,DOVDLSTC-DOVEL01(R1) POINT TO FIRST DEMO IN ELEMENT           
         LA    R8,DOVDMAXQ             MAXIMUM OF 10 DEMOS                      
         NI    MYFLAG,X'FF'-DEMBITON                                            
*                                                                               
DEMBIT5  CLC   1(2,R1),1(R6)       IS ORIG AND CURRENT DEMO THE SAME?           
         BE    DEMBIT10            NO                                           
         LA    R1,3(R1)            BUMP TO NEXT DEMO IN ORIG ELEMENT            
         BCT   R8,DEMBIT5                                                       
         B     DEMBITX             NO MATCH FOUND                               
*                                                                               
DEMBIT10 TM    0(R1),X'80'         IS THE X'80' BIT TURNED ON?                  
         BZ    *+8                 NO                                           
         OI    MYFLAG,DEMBITON     DEMO BIT IS ON                               
*                                                                               
         LHI   R0,DOVDMAXQ                                                      
         SR    R0,R8                                                            
         STC   R0,MYDEMCNT         SET REL DEMO NUM IN OLD 01 ELEM              
*                                                                               
DEMBITX  XIT1                                                                   
         EJECT                                                                  
*-----------------------------------------------------------*                   
*   CHECK IF NULL DEMO IN ORIGINAL ELEMENT (ACTION CHANGE)  *                   
*-----------------------------------------------------------*                   
DEMNONE  NTR1                                                                   
         LA    R0,DOVDMAXQ                                                      
         SR    R0,R8               POSITION OF DEMO IN LIST                     
         NI    MYFLAG,X'FF'-NODEMERR                                            
*                                                                               
         LA    R1,SVELEM01                                                      
         LLC   R5,1(R1)            ORIGINAL LENGTH FOR X'01' ELEMENT            
         SH    R5,=H'12'           FIND HOW MANY DEMOS THERE WERE -             
         BM    ERRINV                                                           
         SR    R4,R4                                                            
         D     R4,=F'3'            IN THE ORIGINAL RECORD                       
         CR    R0,R5               CURRENT NUM OF DEMOS > ORIG NUM?             
         BNL   DEMNONEX            YES                                          
         LA    R1,12(R1)           POINT TO FIRST DEMO IN ELEMENT               
*                                                                               
         LTR   R0,R0               FIRST DEMO POSITION?                         
         BZ    DEMNON10            YES                                          
*                                                                               
DEMNONE5 LA    R1,3(R1)                                                         
         BCT   R0,DEMNONE5                                                      
*                                                                               
DEMNON10 OC    0(3,R1),0(R1)       WAS DEMO POSITION ORIGINALLY NULLS?          
         BZ    DEMNONEX            YES                                          
         OI    MYFLAG,NODEMERR     THERE SHOULD BE A 'DELETE' HERE              
*                                                                               
DEMNONEX XIT1                                                                   
         EJECT                                                                  
*-----------------------------------------------------------*                   
*                  DISPLAY DEMO SUBROUTINE                  *                   
*-----------------------------------------------------------*                   
DISPDEM  NTR1                                                                   
*                                                                               
         L     R3,AIO                                                           
         USING DOVRECD,R3                                                       
         MVI   NONRTGDM,C'N'                                                    
*                                                                               
         SR    R4,R4                                                            
         LLC   R5,DOVEL01+1        LENGTH OF ELEMENT                            
         SH    R5,=H'12'           LENGTH OF ELEMENT UP TO DEMO LIST            
         BNP   ERRINV                                                           
         D     R4,=F'3'            TOTAL NUMBER OF DEMO ELEMENTS                
         LR    R4,R5                                                            
         CH    R4,=H'10'                                                        
         BH    ERRINV                                                           
*                                                                               
         LA    R6,DOVDLSTC                                                      
         LA    R2,DEMDEM1H                                                      
         CLI   ACTNUM,ACTCOPY      ACTION COPY?                                 
         BNE   *+12                                                             
         LA    R2,DCPDEM1H                                                      
         BAS   RE,BMPFLD                                                        
*                                                                               
DISPD5   OC    0(3,R6),0(R6)       NULL DEMO IN LIST?                           
         BNZ   DISPD10                                                          
         XC    8(L'DEMDEM1,R2),8(R2)    CLEAR FIELD ON SCREEN                   
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   ACTNUM,ACTCOPY      ACTION COPY?                                 
         BNE   *+8                                                              
         BAS   RE,BMPFLD           NEXT FIELD ON SCREEN                         
         BAS   RE,BMPFLD           NEXT FIELD ON SCREEN                         
*                                                                               
         LA    R6,3(R6)            NEXT DEMO IN RECORD                          
         BCT   R4,DISPD5                                                        
         B     DISPDEMX                                                         
*                                                                               
DISPD10  XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING DBLOCK,R5                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'C'       CANADIAN                                     
*                                                                               
DISPD15  GOTO1 DEMOCON,DMCB,(1,0(R6)),(2,AIO2),(C'S',DBLOCK),0                  
*                                                                               
         DROP  R5                                                               
         L     R5,AIO2                                                          
         CLC   =C'USER',0(R5)      USER DEMO?                                   
         BNE   DISPD30                                                          
         MVC   8(3,R2),=C'U /'     FORMAT - U#/                                 
         MVC   9(1,R2),4(R5)       NUMBER AFTER 'USER' IN AIO2                  
         OI    9(R2),X'F0'         DEMOS TEAM CHANGED SO NOT EBCDIC 1-9         
         MVC   11(4,R2),0(R5)      MOVE 'USER' ONTO SCREEN                      
         B     *+10                                                             
*                                                                               
DISPD30  MVC   8(7,R2),0(R5)       DEMO LIST                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   FHDAD(R2),C'R'      SEE IF DOING A RATING                        
         BE    DISPD40                                                          
         CLI   FHDAD(R2),C'E'      SEE IF DOING AN EXTENDED DEMO                
         BE    DISPD40                                                          
         CLI   FHDAD(R2),C'U'      SEE IF DOING A USER DEMO                     
         BE    DISPD40                                                          
         MVI   NONRTGDM,C'Y'                                                    
*                                                                               
DISPD40  CLI   ACTNUM,ACTCOPY      ACTION COPY?                                 
         BNE   *+8                                                              
         BAS   RE,BMPFLD           NEXT FIELD ON SCREEN                         
         BAS   RE,BMPFLD           NEXT FIELD ON SCREEN                         
*                                                                               
         LA    R6,3(R6)            NEXT DEMO LIST ENTRY                         
         BCT   R4,DISPD5                                                        
*                                                                               
DISPDEMX XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*-----------------------------------------------------------*                   
*           DISPLAY DEMO IMPRESSIONS SUBROUTINE             *                   
*-----------------------------------------------------------*                   
DISPIMP  NTR1                                                                   
*                                                                               
         L     R3,AIO                                                           
*                                                                               
         MVI   ELCODE,X'02'        X'02' ELEMENT                                
         BAS   RE,GETEL                                                         
         BNE   DISPIMPX                                                         
         USING DOVEL02,R3                                                       
*                                                                               
         SR    R4,R4                                                            
         LLC   R5,DOVEL02+1        LENGTH OF ELEMENT                            
         AHI   R5,-2               LENGTH OF ELEMENT UP TO DEMO LIST            
         BZ    DISPIMPX                                                         
*                                                                               
         D     R4,=F'5'            TOTAL NUMBER OF DEMO ELEMENTS                
         LR    R4,R5                                                            
         CHI   R4,16               MAXIMUM ALLOCATED SCREEN FIELDS -            
         BH    ERRINV              FOR DEMO IMPRESSIONS                         
*                                                                               
         LA    R8,DOVIMPC          A(1ST DEMO NUMBER)                           
         LA    R2,DEMIMP1H                                                      
*                                                                               
         CLI   ACTNUM,ACTCOPY      ACTION COPY?                                 
         BNE   *+12                                                             
         LA    R2,DCPDI1H                                                       
         BAS   RE,BMPFLD           NEXT FIELD ON SCREEN                         
*                                                                               
DISPIMP5 STC   R4,DEMCOUNT                                                      
*                                                                               
DISIMP10 XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING DBLOCK,R5                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBSELMED,C'C'       CANADIAN                                     
*                                                                               
         GOTO1 DEMOCON,DMCB,(1,0(R8)),(2,AIO2),(C'S',ELEM),0                    
*                                                                               
         DROP  R5                                                               
         L     R5,AIO2                                                          
         CLC   =C'USER',0(R5)      USER DEMO?                                   
         BNE   DISIMP15                                                         
         MVC   8(3,R2),=C'U /'     FORMAT - U#/                                 
         MVC   9(1,R2),4(R5)       NUMBER AFTER 'USER' IN AIO2                  
         OI    9(R2),X'F0'         DEMOS TEAM CHANGED SO NOT EBCDIC 1-9         
         MVC   11(4,R2),0(R5)      MOVE 'USER' ONTO SCREEN                      
         B     DISIMP20                                                         
*                                                                               
DISIMP15 MVC   8(6,R2),0(R5)       DEMO CATEGORY                                
         TM    8+4(R2),X'40'                                                    
         BNZ   *+8                                                              
         OI    8+4(R2),X'F0'       DEMOS TEAM CHANGED SO NOT EBCDIC 1-9         
*                                                                               
DISIMP20 MVI   15(R2),C'='                                                      
         SR    RF,RF                                                            
         ICM   RF,3,3(R8)          IMPRESSION                                   
         EDIT  (RF),TEMPIMP,1,ALIGN=LEFT     IMPRESSION                         
         MVC   16(6,R2),TEMPIMP                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R8,5(R8)            NEXT DEMO IMPRESSION                         
*                                                                               
         CLI   ACTNUM,ACTCOPY      ACTION COPY?                                 
         BNE   *+8                                                              
         BAS   RE,BMPFLD           NEXT FIELD ON SCREEN                         
*                                                                               
         BAS   RE,BMPFLD                                                        
         LLC   R4,DEMCOUNT                                                      
         BCT   R4,DISPIMP5                                                      
*                                                                               
DISPIMPX XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* APPLY LIST FILTERS TO DIRECTORY KEY                                 *         
* ENTRY - R4=A(KEY)                                                   *         
***********************************************************************         
FILTCHK  NTR1  ,                                                                
         USING DOVRECD,R4                                                       
*                                                                               
         SR    RE,RE               FILTER BY SHOW                               
         ICM   RE,1,LSTPGMH+FHILD                                               
         BZ    FCHK20              NO                                           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   LSTPGM(0),DOVKPGM                                                
         BNE   FCHKNOX                                                          
*                                                                               
FCHK20   CLI   LSTRTSH+5,0         FILTER BY RATING SERVICE?                    
         BE    *+14                                                             
         CLC   DOVKRTS,SVRTS       RATING SERVICE MATCH?                        
         BNE   FCHKNOX                                                          
*                                                                               
         CLI   LSTCLTH+5,0         FILTER BY CLIENT?                            
         BE    FCHKYESX                                                         
         CLI   LSTCLT,C'>'         START AT FILTER?                             
         BNE   FCHK35                                                           
         CLC   DOVKCLT,SVCLT       CLIENT MATCH?                                
         BNL   FCHKYESX                                                         
         B     FCHKNOX                                                          
FCHK35   CLC   SVCLT,DOVKCLT       CLIENT MATCH?                                
         BNE   FCHKNOX                                                          
*                                                                               
FCHKYESX CR    RB,RB                                                            
         B     *+6                                                              
FCHKNOX  LTR   RB,RB                                                            
         XIT1  ,                                                                
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
*-----------------------------------------------------------*                   
*          CHECK FOR 0 SEQUENCE RECORD                      *                   
*-----------------------------------------------------------*                   
SEQCHECK NTR1                                                                   
         LA    R4,SAVEKEY                                                       
         USING DOVRECD,R4                                                       
*                                                                               
         MVI   DOVKSEQ,X'00'       SEQUENCE NUMBER 0                            
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     DOES 0 SEQ. RECORD EXIST?                    
         BNE   ERRSEQU             NO                                           
         MVI   DOVKSEQ,X'01'                                                    
*                                                                               
SEQCHKX  XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*-----------------------------------------------------------*                   
*          CHECK FOR DUPLICATE DEMOS SUBROUTINE             *                   
*-----------------------------------------------------------*                   
VALDEM   NTR1                                                                   
         LA    R1,ELEM+DOVDLSTC-DOVEL01   POINT TO DEMOS IN THE ELEMENT         
         LHI   R0,DOVDMAXQ         MAX DEMOS                                    
VALDEM01 CR    R6,R1               PAST THE END OF DEMO LIST?                   
         BNH   VALDEM02            YES = GOOD, FOUND NO DUPLICATES              
         CLC   1(2,R1),1(R6)      SAME DEMO AS WE'RE ATTEMPTING TO ADD?         
         BE    ERRDUP                                                           
         LA    R1,3(R1)            NEXT DEMO                                    
         BCT   R0,VALDEM01                                                      
*                                                                               
VALDEM02 DS    0H                                                               
         MVC   SAVEKEY,0(R3)                                                    
         MVC   AIO,AIO3                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DOVRECD,R4                                                       
*                                                                               
         MVC   KEY(L'DOVKEY+11),0(R3)                                           
*                                                                               
         CLI   DEMSEQ,C'1'         SEQUENCE NUMBER 1?                           
         BNE   *+12                                                             
         MVI   DOVKSEQ,X'00'       0 SEQUENCE RECORD KEY                        
         B     *+8                                                              
         MVI   DOVKSEQ,X'01'       1 SEQUENCE RECORD KEY                        
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                GET OTHER SEQUENCE RECORD                    
         CLC   KEY(L'DOVKEY),KEYSAVE                                            
         BNE   VALDEM30                                                         
*                                                                               
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         USING DOVRECD,R3                                                       
*                                                                               
         LA    R4,DOVDLSTC         DEMO LIST                                    
         XC    MYFLAG,MYFLAG                                                    
         SR    R5,R5                                                            
         LA    R5,10               MAXIMUM NUMBER OF DEMOS                      
*                                                                               
VALDEM10 CLC   1(2,R6),1(R4)       DEMO MATCH?                                  
         BE    VALDEM20                                                         
         LA    R4,3(R4)            NEXT DEMO IN OTHER SEQUENCE RECORD           
         BCT   R5,VALDEM10                                                      
         B     *+8                                                              
         DROP  R3                                                               
*                                                                               
VALDEM20 OI    MYFLAG,NODEMO       DEMO NOT FOUND IN OTHER SEQ RECORD           
         MVC   KEY,SAVEKEY                                                      
         LA    R4,KEY                                                           
         USING DOVRECD,R4                                                       
VALDEM30 MVC   AIO,AIO1                                                         
*                                                                               
VALDEMX  XIT1                                                                   
         EJECT                                                                  
*-----------------------------------------------------------*                   
*          CHECK FOR DUPLICATE DEMO IMPS SUBROUTINE         *                   
*-----------------------------------------------------------*                   
VALIMP   NTR1                                                                   
*                                                                               
         LA    R1,ELEM+DOVIMPC-DOVEL02                                          
         LHI   R0,DOVDMAXQ         MAX DEMOS                                    
VALIMP01 CR    R6,R1                                                            
         BNH   VALIMP02                                                         
         CLC   1(2,R1),1(R6)                                                    
         BE    ERRDUP                                                           
         LA    R1,5(R1)                                                         
         BCT   R0,VALIMP01                                                      
*                                                                               
VALIMP02 DS    0H                                                               
*                                                                               
         XC    MYFLAG,MYFLAG                                                    
         MVC   SAVEKEY,0(R3)                                                    
         MVC   AIO,AIO3                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DOVRECD,R4                                                       
*                                                                               
         MVC   KEY(L'DOVKEY+11),0(R3)                                           
*                                                                               
         MVI   DOVKSEQ,X'00'       0 SEQUENCE RECORD KEY                        
         CLI   DEMSEQ,C'1'         SEQUENCE NUMBER 1?                           
         BE    *+8                                                              
         MVI   DOVKSEQ,X'01'       1 SEQUENCE RECORD KEY                        
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                GET OTHER SEQUENCE RECORD                    
         CLC   KEY(L'DOVKEY),KEYSAVE                                            
         BNE   VALIMP30                                                         
*                                                                               
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         USING DOVEL02,R3                                                       
*                                                                               
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   VALIMP30                                                         
         LA    R4,DOVIMPC          DEMO IMP LIST                                
         LHI   R5,DOVDMAXQ         MAXIMUM NUMBER OF DEMOS                      
*                                                                               
VALIMP10 CLC   0(3,R6),0(R4)       DEMO MATCH?                                  
         BE    VALIMP20                                                         
         LA    R4,5(R4)            NEXT DEMO IN OTHER SEQUENCE RECORD           
         BCT   R5,VALIMP10                                                      
         B     *+8                                                              
         DROP  R3                                                               
*                                                                               
VALIMP20 OI    MYFLAG,NODEMO       DEMO NOT FOUND IN OTHER SEQ RECORD           
         MVC   KEY,SAVEKEY                                                      
         LA    R4,KEY                                                           
         USING DOVRECD,R4                                                       
VALIMP30 MVC   AIO,AIO1                                                         
*                                                                               
VALIMPX  XIT1                                                                   
         EJECT                                                                  
*-----------------------------------------------------------*                   
*                VALIDATE NETWORK SUBROUTINE                *                   
* ENTRY - R2=A(NETWORK FIELD HDR)                           *                   
* EXIT  - BSTA SET                                          *                   
*         TEMPNETE=NETWORK FIELD TEXT                       *                   
*         CC NOT EQUAL IF INVALID                           *                   
*-----------------------------------------------------------*                   
VALNET   NTR1  ,                                                                
         OC    8(4,R2),=C'    '                                                 
         MVC   TEMPNETE,8(R2)      NETWORK OFF SCREEN                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NDEFRECD,R4         NETWORK DEF'N RECORD                         
         MVC   NDEFKTYP,=X'0D11'   RECORD ID                                    
         MVC   NDEFKAGY,AGENCY     AGENCY ID                                    
         MVC   NDEFKNET,8(R2)      NETWORK (OFF SCREEN)                         
         GOTO1 HIGH                DOES NETWORK EXIST?                          
         CLC   KEY(8),KEYSAVE                                                   
         BNE   VNETERX             NO - NETWORK DOESN'T EXIST                   
*                                                                               
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVI   ELCODE,X'02'       SEQNUM ELEMENT                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVNETSEQ,NDEFNET-NDEFEL02(R3)                                    
*                                                                               
         GOTO1 VALISTA             VALIDATE STATION (NEED BSTA)                 
         XC    KEY,KEY                                                          
*                                                                               
VNETOKX  CR    RB,RB                                                            
         B     *+6                                                              
VNETERX  LTR   RB,RB                                                            
         XIT1  ,                                                                
         DROP  R4                                                               
         EJECT                                                                  
*-----------------------------------------------------------*                   
*                 VALIDATE PROGRAM SUBROUTINE               *                   
*-----------------------------------------------------------*                   
VALPROG  NTR1                                                                   
*                                                                               
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING NPGMRECD,R3                                                      
         MVC   NPGMKTYP,=X'0D12'   RECORD ID                                    
         MVC   NPGMKAGY,AGENCY     AGENCY ID                                    
         MVC   NPGMKNET,TEMPNETE   NETWORK (OFF SCREEN)                         
         MVC   NPGMKID,TEMPPGME    PROGRAM NAME (OFF SCREEN)                    
         GOTO1 HIGH                DOES PROGRAM EXIST?                          
         CLC   KEY(12),KEYSAVE                                                  
         BNE   ERRNOTF             NO - NETWORK DOESN'T EXIST                   
         GOTO1 GETREC                                                           
         DROP  R3                                                               
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'01'        X'01' ELEMENT                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING NPGMEL01,R3                                                      
         MVC   TEMPPGM,NPGMPGM     PROGRAM DESCRIPTION                          
*                                                                               
         GOTO1 UNDAY,DMCB,NPGMDAY,TEMPDAY    PROGRAM DAY                        
*                                                                               
         XC    TEMPTIME,TEMPTIME                                                
         GOTO1 UNTIME,DMCB,NPGMSTR,TEMPTIME    START AND END TIME               
*                                                                               
         MVC   AIO,AIO1                                                         
         DROP  R3                                                               
*                                                                               
VALPROGX XIT1                                                                   
         EJECT                                                                  
*-----------------------------------------------------------*                   
*            READ 00 PROFILE FOR USER DEMO SEQNUMS OPTION   *                   
*-----------------------------------------------------------*                   
RDPROF   NTR1                                                                   
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S000'                                                 
         MVC   WORK+4(2),AGENCY    READ AGENCY LEVEL                            
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,ELEM,DATAMGR                                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SV00PROF+9(1),ELEM+9   SAVE USER DEMO OPTION                     
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S00A'                                                 
         NI    WORK,X'BF'          MAKE S LOWERCASE                             
         MVC   WORK+4(2),AGENCY    READ AGENCY LEVEL                            
         GOTO1 (RF),DMCB,WORK,SV00APRF,DATAMGR                                  
         CLI   SV00APRF+1,C'0'     IS IT ZERO                                   
         BE    *+8                 YES                                          
         MVI   SV00APRF+1,C'L'     THEN SET TO LOOKUP DEFAULT                   
*                                                                               
RPX      XIT1                                                                   
         EJECT                                                                  
*-----------------------------------------------------------*                   
*        VALIDATE 2ND BYTE OF NEW USER DEMO 1-9, A-Z IN BYTE2                   
*-----------------------------------------------------------*                   
VALUDEM  NTR1                                                                   
*                                                                               
         CLI   BYTE2,C'A'                                                       
         BL    ERRINV                                                           
         CLI   BYTE2,C'Z'                                                       
         BL    VUX                                                              
         CLI   BYTE2,C'0'                                                       
         BL    ERRINV                                                           
         CLI   BYTE2,C'9'                                                       
         BH    ERRINV                                                           
*                                                                               
VUX      XIT1                                                                   
         EJECT                                                                  
*-----------------------------------------------------------*                   
*     CONVERT SPECIAL USER DEMO TO CORRESPODING IMPRESSION                      
*     BYTE2 HAS 2ND BYTE OF USER DEMO THAT WILL BE CONVERTED                    
*-----------------------------------------------------------*                   
CNVUDEM  NTR1                                                                   
*                                                                               
         LA    R3,IMPTAB                                                        
CU10     CLI   0(R3),X'FF'         EOT                                          
         BE    CUX                                                              
         CLC   BYTE2,0(R3)         MATCH ON FIRST CHAR                          
         BE    CU20                                                             
         LA    R3,2(R3)            NEXT                                         
         B     CU10                                                             
CU20     MVC   BYTE2,1(R3)         REPLACE                                      
CUX      XIT1                                                                   
*                                                                               
IMPTAB   DC    C'0',C'I'                                                        
         DC    C'1',C'J'                                                        
         DC    C'2',C'K'                                                        
         DC    C'3',C'L'                                                        
         DC    C'4',C'M'                                                        
         DC    C'5',C'N'                                                        
         DC    C'6',C'O'                                                        
         DC    C'7',C'P'                                                        
         DC    C'8',C'Q'                                                        
         DC    C'9',C'R'                                                        
         DC    C'A',C'S'                                                        
         DC    C'B',C'T'                                                        
         DC    C'C',C'U'                                                        
         DC    C'D',C'V'                                                        
         DC    C'E',C'W'                                                        
         DC    C'F',C'X'                                                        
         DC    C'G',C'Y'                                                        
         DC    C'H',C'Z'                                                        
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*-----------------------------------------------------------*                   
*            VALIDATE RATING SERVICE SUBROUTINE             *                   
* R2=A(RATING SERVICE HDR)                                                      
*-----------------------------------------------------------*                   
VALRTS   NTR1                                                                   
*        XC    TEMPHEAD,TEMPHEAD                                                
*        MVC   TEMPHEAD(9),=XL9'0900000000010000D5' N FOR NETWORK               
*        LA    R2,TEMPHEAD                                                      
*        GOTO1 VALIMED                                                          
*                                                                               
         MVI   SVRTS,C'1'          RATING SERVICE OF C'1'                       
         CLC   =C'BBM',8(R2)       RATING SERVICE BBM?                          
         BE    VALRTSX             YES                                          
         MVI   SVRTS,C'0'                                                       
         CLC   =C'NSI',8(R2)       RATING SERVICE NSI?                          
         BNE   ERRINV              YES                                          
*                                                                               
VALRTSX  XIT1                                                                   
         EJECT                                                                  
*                                                                               
*-----------------------------------------------------------*                   
*            ERROR AND MISCELLANEOUS SUBROUTINES            *                   
*-----------------------------------------------------------*                   
BMPFLD   LLC R0,0(R2)              BUMP R2 TO NEXT FIELD                        
         AR   R2,R0                                                             
         BR   RE                                                                
*                                                                               
ERRINV   MVI   ERROR,INVALID       ERROR: INVALID                               
         B     ERR                                                              
*                                                                               
ERRMISS  MVI   ERROR,MISSING       ERROR: DATA MISSING                          
         B     ERR                                                              
*                                                                               
ERRNOTF  MVI   ERROR,NOTFOUND      ERROR: NOT FOUND                             
         B     ERR                                                              
*                                                                               
ERRNUMB  MVI   ERROR,NOTNUM        ERROR: NOT NUMERIC                           
         B     ERR                                                              
*                                                                               
ERRINVD  MVC   ERRNUM,=AL2(INVDEM)    ERROR: INVALID DEMO                       
         B     MYERR                                                            
*                                                                               
ERRDUPD  MVC   ERRNUM,=AL2(DUPDEMO)   ERROR: DUPLICATE DEMO FOUND -             
         B     MYERR                  IN OTHER SEQUENCE RECORD                  
*                                                                               
ERRDUP   MVC   ERRNUM,=AL2(291)       ERROR: DUPLICATE DEMO                     
         B     MYERR                                                            
*                                                                               
ERRDELD  MVC   ERRNUM,=AL2(DELDEMO)   ERROR: SHOULD BE A VALID -                
         B     MYERR                  DEMO OR 'DELETE'                          
*                                                                               
ERRONED  MVC   ERRNUM,=AL2(ONEDEMO)   ERROR: MUST SELECT AT LEAST               
         B     MYERR                  ONE DEMO                                  
*                                                                               
ERRADJ   MVC   ERRNUM,=AL2(ADJSIGN)   ERROR: ADJUST % MUST BE                   
         B     MYERR                  PRECEDED WITH '+' OR '-'                  
*                                                                               
ERRSAME  MVC   ERRNUM,=AL2(SAMEREC)   ERROR: SOURCE AND DESTINATION             
         B     MYERR                  RECORDS ARE THE SAME                      
*                                                                               
ERRCURS  MVC   ERRNUM,=AL2(CURSOR)    ERROR: CURSOR MUST BE ON A                
         B     MYERR                  DEMO IN THE DEMO LIST                     
*                                                                               
ERRSEQU  MVC   ERRNUM,=AL2(NOSEQU)    ERROR: MUST ADD 0 SEQUENCE                
         B     MYERR                  RECORD FIRST                              
*                                                                               
ERR2MNYD MVC   ERRNUM,=AL2(TOOMNYDM)  ERROR: INVALID OR TOO MANY DEMOS          
         LA    R2,DEMDEMXH                                                      
         B     MYERR                                                            
*                                                                               
*RR05S   MVC   ERRNUM,=AL2(9)         ERROR ADDING 05 ELS:                      
ERR05S   CLC   ERRNUM,=AL2(9)         ERROR ADDING 05 ELS:                      
         BNE   ERRSEQ                                                           
         LA    R2,DEMNETH                                                       
         B     MYERR                  TOO MANY STATIONS/SPILL MKTS              
*                                                                               
ERRSEQ   MVC   ERRNUM,=AL2(1215)      ADD DEMOS TO NEXT SEQ#                    
         LA    R2,DEMDEM1H                                                      
         LA    R3,DEMDEMXH                                                      
         LR    R4,R2                                                            
*                                                                               
ES00     CR    R2,R3                  LAST DEMO?                                
         BL    ES01                   NO                                        
         CLI   5(R2),0                ANY INPUT?                                
         BE    ES02                   NO, ERROR ON PREVIOUS ENTRY               
         B     ESX                                                              
*                                                                               
ES01     CLI   5(R2),0                ANY INPUT?                                
         BE    ES02                   NO, PREVIOUS ENTRY                        
         LR    R4,R2                                                            
         LLC   R1,0(R2)               LENGTH TO NEXT FIELD                      
         AR    R2,R1                                                            
         B     ES00                                                             
*                                                                               
ES02     LR    R2,R4                                                            
*                                                                               
ESX      B     MYERR                  RECORD OVERFLOW                           
*                                                                               
ERRIMP   MVC   ERRNUM,=AL2(BADIMP)    ERROR: AUDIENCE CATEGORY                  
         B     MYERR                  CAN NOT START WITH AN R OR E              
*                                                                               
ERRMVDEM MVC   ERRNUM,=AL2(MOVEDEM)   CANNOT MOVE DEMO POSITION                 
         B     MYERR                                                            
*                                                                               
ERMAXIMP MVC   ERRNUM,=AL2(942)    EXCEEDS MAX                                  
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTLTXT,6                                                         
         LA    R1,=CL6'6553.5'                                                  
         STCM  R1,7,GTATXT                                                      
*                                                                               
MYERR    OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         DROP  RF                                                               
*                                                                               
ERR      GOTO1 ERREX                                                            
*                                                                               
TOOMNYDM EQU   111                 INVALID OR TOO MANY DEMOS                    
INVDEM   EQU   388                 INVALID DEMO                                 
DUPDEMO  EQU   389                 DUPLICATE DEMO IN OTHER SEQUENCE             
DELDEMO  EQU   390                 SHOULD BE VALID DEMO OR 'DELETE'             
ONEDEMO  EQU   396                 MUST SELECT AT LEAST ONE DEMO                
ADJSIGN  EQU   397                 ADJ. % PRECEDED WITH '+' OR '-'              
SAMEREC  EQU   398                 TRYING TO COPY THE SAME RECORD               
CURSOR   EQU   399                 CURSOR SHOULD BE ON DEMO IN LIST             
NOSEQU   EQU   407                 MUST ADD 0 SEQUENCE RECORD FIRST             
BADIMP   EQU   409                 AUDIENCE CATEGORY, NO R OR E                 
MOVEDEM  EQU   1271                CANNOT MOVE DEMO POSITION                    
*                                                                               
         GETEL R3,DATADISP,ELCODE                                               
*-----------------------------------------------------------*                   
*                   PFKEY TABLE                             *                   
*-----------------------------------------------------------*                   
MPFTABLE DS    0C                                                               
* PF04 = DEMOVER DISPLAY                                                        
         DC    AL1(MPF04X-*,04)                                                 
MPF04ST1 DC    AL1(PFTCPROG)                                                    
         DC    AL1((MPF04X-MPF04K)/KEYLNQ,0)                                    
         DC    CL3'   '                                                         
         DC    CL8'DEMOVER'        RECORD: DEMOVER                              
MPF04ACT DC    CL8'       '        ACTION:                                      
MPF04K   DC    AL1(KEYTYTWA,L'DEMNET-1),AL2(DEMNET-T217FFD)                     
         DC    AL1(KEYTYTWA,L'DEMPGM-1),AL2(DEMPGM-T217FFD)                     
         DC    AL1(KEYTYTWA,L'DEMRTS-1),AL2(DEMRTS-T217FFD)                     
         DC    AL1(KEYTYWS,L'SVDEMO-1),AL2(SVDEMO-SYSSPARE)                     
         DC    AL1(KEYTYTWA,L'DEMCLT-1),AL2(DEMCLT-T217FFD)                     
         DC    AL1(KEYTYTWA,L'DEMSEQ-1),AL2(DEMSEQ-T217FFD)                     
MPF04X   EQU   *                                                                
*                                                                               
* PF12 = RETURN TO CALLER                                                       
         DC    AL1(MPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
MPF12X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
* PFKEY TEXT MIXCASE CONSTANTS                                                  
PF04DOVR DC    X'D7C6F47EC48594D6A58599'  PF4=DEMOVER                           
PF12RET  DC    X'D7C6F1F27ED985A3A49995'  PF12=RETURN                           
*                                                                               
         LTORG                                                                  
*==============================================================*                
* DEMCOUNT, THE SUBROUTINE TO COUNT NUMBER OF DEMOS                             
* DEMONUM WILL HAVE NUMBER OF DEMOS AT RETURN                                   
*==============================================================*                
DEMCNT   NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         USING DOVEL01,R3                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,DOVDLSTC                                                      
         LR    RF,R4               POINTER TO LAST DEMO                         
         LR    RE,R4               SAVE ADDRESS OF DEMO LIST                    
         LHI   R0,DOVDMAXQ         NO MORE THAN 10 DEMOS                        
*                                                                               
         LLC   R1,DOVEL01+1        LENGTH OF ELEMENT                            
         LA    R1,0(R1,R3)         A(BYTE FOLLOWING '01' ELEMENT)               
*                                                                               
DEMC10   DS    0H                                                               
         CR    R4,R1                                                            
         BNL   DEMC20                                                           
         CLC   0(3,R4),=XL3'0'                                                  
         BE    *+6                                                              
         LR    RF,R4               DEMO AT THIS POSITION                        
         AHI   R4,3                ADVANCE TO NEXT DEMO                         
         BCT   R0,DEMC10                                                        
*                                  ALL 10 POSITIONS SCANNED                     
DEMC20   DS    0H                                                               
         SR    RF,RE               NO. OF BYTES TO LAST DEMO                    
         SR    RE,RE                                                            
         D     RE,=F'3'            NUMBER OF DEMOS                              
         AHI   RF,1                                                             
         STC   RF,DEMONUM                                                       
*                                                                               
         J     EQXIT                                                            
         DROP  R3                                                               
         LTORG                                                                  
*==============================================================*                
* ADD05 STATION/SPILL MKT ELEMENTS TO NEWLY ADDED RECORD AS PER                 
* DEMOVER/DISP (WHICH USERS FORGET TO DO BEFORE MAKING BUYS!)                   
* EXIT - CC NE IF CANNOT ADD ALL REQUIRED 05 ELS (ERRNUM SET)                   
*==============================================================*                
ADD05    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,DEMCNT           COUNT DEMOS                                  
         BRAS  RE,BLDSTAB          BUILD STATION/SPILL TABLE                    
         BH    ADD05ERS                                                         
*                                                                               
         L     R3,AIO              GET POSITION WHERE TO PUT 05'S               
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         LR    R4,R3               (I.E. EOR)                                   
*                                                                               
         LA    R5,STATAB           TABLE OF STATIONS                            
         LA    R6,MAXELS           MAX STATION/SPILL MKTS ON SCREEN             
*                                                                               
ADD0505  CLC   0(2,R5),=X'FFFF'    END OF STATION/SP MKT TABLE?                 
         BE    ADD05X              YES                                          
*                                                                               
         XC    ELEMENT,ELEMENT     BUILD ELEM AND ADD TO NEW REC                
         MVI   ELEMENT,X'05'                                                    
         SR    R0,R0                                                            
         IC    R0,DEMONUM          CALC L'ELEM FROM #DEMOS                      
         MHI   R0,2                                                             
         AHI   R0,5                                                             
         STC   R0,ELEMENT+1                                                     
*                                                                               
         MVC   ELEMENT+2(L'DOVSTA),0(R5)                                        
*                                                                               
         LA    RE,SET0LIST         GET LIST OF NEW ZERO DEM OVRDS               
*                                                                               
ADD0507  SR    R1,R1                                                            
         IC    R1,0(RE)                                                         
         BCTR  R1,0                                                             
         AR    R1,R1                                                            
         LA    R1,ELEMENT+5(R1)                                                 
         CLI   SV00APRF+1,C'L'     TEST DEFAULT=LOOKUP                          
         BE    *+8                                                              
         OI    0(R1),X'80'         SET ZERO VALUE                               
         AHI   RE,1                                                             
         CLI   0(RE),0                                                          
         BNE   ADD0507                                                          
*                                                                               
         GOTO1 RECUP,DMCB,(0,AIO1),ELEM,(C'R',(R4))                             
         CLI   8(R1),0                                                          
         BE    ADD05ERD            OVERFLOW                                     
         LLC   R0,1(R4)            LENGTH OF ELEMENT                            
         AR    R4,R0               POINT TO NEXT PLACE FOR EL INSERT            
*                                                                               
         LA    R5,3(R5)            BUMP TO NEXT TABLE ENTRY                     
         BCT   R6,ADD0505                                                       
*                                  TOO MANY ELEMS                               
ADD05ERS MVC   ERRNUM,=AL2(9)      ERROR WITH STATIONS/SPILL                    
         B     ADD05ERX                                                         
ADD05ERD MVC   ERRNUM,=AL2(1215)   ERROR WITH DEMOS - ADD TO NEXT SEQ#          
ADD05ERX LTR   RB,RB                                                            
         B     *+6                                                              
ADD05X   CR    RB,RB                                                            
         XIT1  ,                                                                
MAXELS   EQU   DOV5MAXQ                                                         
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
*  BLDSTAB        BUILD THE STATION AND SPILL MARKET TABLE     *                
*  USES IO2                                                    *                
*  EXIT - CC SET WRT NUMBER OF TABLE ENTRIES VS MAX ALLOWED    *                
*==============================================================*                
BLDSTAB  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING NDEFRECD,R3                                                      
         LA    R2,SAVEKEY                                                       
         MVC   NDEFKTYP,=X'0D11'                                                
         MVC   NDEFKAGY,AGENCY                                                  
         MVC   NDEFKNET,DCPNET                                                  
         OC    NDEFKNET,=C'    '                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(NDEFKCLT-NDEFKEY),KEYSAVE                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         LA    R4,STATAB                                                        
         SR    R6,R6               R6=STATAB ENTRY COUNT                        
         USING NDEFEL01,R3                                                      
         MVI   ELCODE,X'01'                                                     
*                                                                               
         BAS   RE,GETEL                                                         
         B     *+8                 PICK UP STATIONS UNDER THIS NETWORK          
BS20     BAS   RE,NEXTEL                                                        
         BNE   BSX                                                              
*                                                                               
         CLC   NDEFSTA,=C'ZZZZ'    USED AS A DUMMY STATION                      
         BE    BS20                SKIP THEM                                    
*                                                                               
         XC    MYQSTA,MYQSTA                                                    
         MVC   MYQMKT,=C'3619'                                                  
         CLI   SVNETSEQ,NDEFCABQ                                                
         BE    BS22                                                             
         MVC   MYQSTA(L'NDEFSTA),NDEFSTA                                        
         OC    MYQSTA(4),=C'    '                                               
         MVI   MYQSTA+4,C' '                                                    
         B     BS25                                                             
BS22     MVC   MYQSTA(4),DCPNET                                                 
         OC    MYQSTA(4),=C'    '                                               
         MVI   MYQSTA+4,C'/'                                                    
         MVC   MYQSTA+5(2),NDEFMSUF                                             
BS25     GOTO1 MSPACK,DMCB,MYQMKT,MYQSTA,BMKTSTA                                
         AHI   R6,1                                                             
         CHI   R6,MAXELS                                                        
         BH    BSX                                                              
         MVC   0(3,R4),BSTA                                                     
         LA    R4,3(R4)                                                         
         BRAS  RE,ADDSPILL         GET SPILL MKTS FOR STATION                   
         BH    BSX                                                              
         B     BS20                                                             
*                                                                               
BSX      MVC   0(2,R4),=X'FFFF'    SET EOT                                      
         MVC   AIO,AIO1                                                         
         CHI   R6,MAXELS           SET CC                                       
         XIT1  ,                                                                
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
*  ADDSPILL  ADD THE SPILL MARKETS TO STATION/SPILL MARKET TABLE *              
*  USES IO3                                                                     
*  ENTRY - R4 POINTS TO SLOT IN TABLE                                           
*          R6 COUNT OF STATAB ENTIRES                                           
*  EXIT  - R4 POINTS TO NEXT AVAILABLE SPACE IN THE TABLE                       
*          R6 UPDATED COUNT OF STATAB ENTIRES                                   
*================================================================*              
ADDSPILL NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING SDEFRECD,R3                                                      
         LA    R2,SAVEKEY                                                       
         USING DOVRECD,R2                                                       
         MVC   SDEFKTYP,=X'0D13'     CHECK FOR SPILL RECS                       
         MVC   SDEFKAGY,AGENCY                                                  
         MVC   SDEFKRSV,DOVKRTS       RATING SERVICE                            
         MVC   SDEFKSTA(4),MYQSTA                                               
*ALWAYS  MVC   SDEFKCLT,BCLT       CHECK FOR CLIENT LEVEL                       
*USE     GOTO1 HIGH                WORK HAS STATION                             
*AGENCY  CLC   KEY(13),KEYSAVE                                                  
*LEVEL   BE    ASP04               FOUND CLIENT LEVEL                           
*SPILL   XC    KEY,KEY                                                          
*11MAR03 MVC   KEY(10),KEYSAVE     LEAVE OUT CLIENT                             
         GOTO1 HIGH                WORK HAS STATION                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   ASP100              NO SPILLS                                    
         DROP  R2,R3                                                            
ASP04    GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO                                                           
         USING SDEFEL05,R3                                                      
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
AS10     BAS   RE,NEXTEL           ANY MORE SPILLS?                             
         BNE   ASP100              NO                                           
*                                                                               
         AHI   R6,1                                                             
         CHI   R6,MAXELS           WILL THIS 'BREAK THE BANK'?                  
         BH    ASP100                                                           
         XC    0(3,R4),0(R4)                                                    
         MVC   1(2,R4),SDEFAMKT    STORE SPILL MKT                              
         LA    R4,3(R4)                                                         
         B     AS10                                                             
*                                                                               
ASP100   MVI   ELCODE,X'01'        RESET ELCODE                                 
         MVC   AIO,AIO2                                                         
         CHI   R6,MAXELS                                                        
         XIT1  REGS=(R4,R6)        LEAVE CHANGES TO R4 & R6                     
         DROP  R3                                                               
         EJECT                                                                  
         LTORG                                                                  
*-----------------------------------------------------------*                   
*                    PFKEYS SETUP                           *                   
*-----------------------------------------------------------*                   
SETUP    NTR1  BASE=*,LABEL=*                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         CLI   ACTNUM,ACTADD                                                    
         BE    SETUP0                                                           
         XC    SVADDPFK,SVADDPFK                                                
         XC    SVADDKEY,SVADDKEY                                                
SETUP0   CLI   PFKEY,12            GO BACK TO SHOWDEF RECORDS?                  
         BE    SETUP99             YES                                          
*                                                                               
         OI    GENSTAT4,CONFDEL    CONFIRM DELETION                             
*                                                                               
*        NI    DMINBTS,X'FF'-X'08' DON'T READ FOR DELETES                       
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BE    SETUPX              NO PFKEYS ON LIST                            
         CLI   ACTNUM,ACTCOPY                                                   
         BE    SETUPX              NO PFKEYS ON COPY                            
*                                                                               
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
         L     RF,ASPOOLD                                                       
         MVC   DEMLN24,SPACES-SPOOLD(RF)    CLEAR PFK TEXT                      
         OI    DEMLN24H+6,X'80'    TRANSMIT THE RESULT                          
         MVC   DEMLN24(L'PF04DOVR),PF04DOVR                                     
*                                                                               
* ADD - ONLY ACTION PFKEYS AFTER REC ADDED                                      
* ALLOWS <PF4> RATHER THAN <ENTER><PF4> TO ADD DEMD THEN SWAP TO DEMO           
         CLI   ACTNUM,ACTADD                                                    
         BNE   SETUP00                                                          
SETUPADD CLI   MODE,XRECADD                                                     
         BE    SETUPA2                                                          
         CLI   MODE,VALKEY         ADD ALWAYS GETS VALKEY                       
         BNE   SETUPA1                                                          
         CLC   SVADDKEY,KEY        ACTION PFKEY IF JUST ADDED THIS REC          
         BE    SETUP00                                                          
         MVC   SVADDPFK,PFKEY      SAVE PFKEY ACTIVE ON VALKEY                  
         XC    SVADDKEY,SVADDKEY                                                
SETUPA1  XC    PFKEY,PFKEY         IGNORE PFKEYS UNTIL READY TO ACTION          
         B     SETUPX                                                           
SETUPA2  MVC   PFKEY,SVADDPFK      ACTION ANY PENDING PFKEY NOW                 
         XC    SVADDPFK,SVADDPFK   CLEAR PFKEY PENDING                          
         MVC   SVADDKEY,KEY        REMEMBER THIS KEY JUST ADDED                 
*                                                                               
SETUP00  CLI   PFKEY,4             PF4 TO DEMOVER?                              
         BNE   SETUP12             NO                                           
         LH    R1,CURDISP                                                       
         AR    R1,RA               DISPLACEMENT TO CURSOR                       
         LA    R2,DEMDEM1H         FIRST DEMO                                   
*                                                                               
SETUP04  CR    R1,R2               CURSOR ON A DEMO?                            
         BE    SETUP04B            YES                                          
         BAS   RE,BMPFLD                                                        
         LA    R0,DEMDEMXH                                                      
         CR    R2,R0               END OF DEMO LIST?                            
         BNH   SETUP04             NO                                           
         LA    R2,DEMDEM1H         CURSOR NOT ON A DEMO                         
SETUP04A CLI   5(R2),0             LOCATE FIRST DEMO                            
         BNE   SETUP04B                                                         
         BAS   RE,BMPFLD                                                        
         LA    R0,DEMDEMXH                                                      
         CR    R2,R0               END OF DEMO LIST?                            
         BNH   SETUP04A            NO                                           
         LA    R2,DEMDEM1H         NO DEMOS AT ALL                              
         B     ERRCURS                                                          
*                                                                               
SETUP04B CLI   5(R2),0             ANY DEMO?                                    
         BE    ERRINV                                                           
         MVC   SVDEMO,8(R2)                                                     
*                                                                               
         MVC   MPF04ACT,=C'        '                                            
         CLI   ACTNUM,ACTSEL       ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF04ACT,=CL8'DISPLAY'                                           
         CLI   CALLSP,0            DID WE ORIGINALLY COME FROM SHOWDEF?         
         BE    SETUP12                                                          
         CLI   CALLSTCK,X'F0'      (SHOWDEF SCREEN NUMBER)                      
         BNE   SETUP12                                                          
         NI    MPF04ST1,255-PFTCPROG  DON'T PUSH SO RETURN TO SHOWDEF           
*                                                                               
SETUP12  CLI   CALLSP,0            ANYTHING TO RETURN TO?                       
         BE    *+10                NO                                           
         MVC   (DEMLN24+L'DEMLN24-(L'PF12RET+2))(L'PF12RET),PF12RET             
         OC    PFKEY,PFKEY                                                      
         BZ    SETUPX                                                           
SETUP99  GOTO1 INITPFKY,DMCB,MPFTABLE                                           
*                                                                               
SETUPX   XIT1  ,                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*-----------------------------------------------------------*                   
*  PRINT OFF -- SPGENNDEF,SPGENNPGM,DDFLDIND,DDPERVALD,DDCOMFACS                
*               FATIOB,SPGENNDOV,SPGENCLT,DDSPLWORKD,SPSFMFFD,                  
*               SPSFMA7D,SPSFMAAD,DDSPOOLD,SPSFMWORKD,DEDBLOCK                  
         PRINT OFF                                                              
       ++INCLUDE SPGENNDOV         NETWORK DEFINITION DSECT                     
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT          CLIENT DSECT                                 
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         ORG   LISTAR              FOR LISTREC LOGIC                            
         DS    CL1                                                              
LSTNETD  DS    CL4                 NETWORK                                      
         DS    CL4                                                              
LSTPGMD  DS    CL4                 PROGRAM                                      
         DS    CL5                                                              
LSTRTSD  DS    CL3                 RATING SERVICE                               
         DS    CL5                                                              
LSTCLTD  DS    CL4                 CLIENT                                       
         DS    CL4                                                              
LSTSEQD  DS    CL1                 SEQUENCE                                     
         DS    CL6                                                              
LSTDEMO  DS    CL2                                                              
LSTDEMOX DS    CL1                                                              
         DS    CL5                                                              
LSTIMP   DS    CL2                                                              
         DS    CL4                                                              
LSTBASE  DS    CL6                 BASE BOOK                                    
         DS    CL5                                                              
LSTUNTIL DS    CL6                 USE TIL BOOK                                 
*                                                                               
PGMDETSD DSECT                                                                  
PGMDNAME DS    CL17                                                             
         DS    CL1                                                              
PGMDDAY  DS    CL8                                                              
         DS    CL1                                                              
PGMDTIME DS    CL11                                                             
*        PRINT OFF                                                              
       ++INCLUDE SPGENNDEF                                                      
       ++INCLUDE SPGENSDEF                                                      
       ++INCLUDE SPGENNPGM                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDFH                                                           
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMA7D     MAINTENANCE SCREEN                                
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMAAD     LIST SCREEN                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMEFD     COPY SCREEN                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
*                                                                               
* STORAGE DSECT                                                                 
*                                                                               
         ORG   SYSSPARE                                                         
DEMCOUNT DS    XL1                                                              
DEMONUM  DS    XL1                                                              
ERRNUM   DS    XL2                 ERROR                                        
*                                                                               
MYFLAG   DS    XL1                 FLAGS                                        
NODEMO   EQU   X'01'               DEMO MATCH NOT FOUND                         
IMPFOUND EQU   X'02'               USER INPUT IMPRESSION                        
DEMBITON EQU   X'04'               DEMO BIT X'80' IS ON                         
NODEMERR EQU   X'08'               SHOULD BE A 'DELETE' IN DEMO                 
MYDEMCNT DS    XL1                                                              
*                                                                               
COPYFLAG DS    XL1                 FLAGS                                        
NEWXISTQ EQU   X'02'               TO RECORD ALREADY EXISTS (DO PUT)            
ADJPOSQ  EQU   X'08'               INCREASE RATINGS                             
ADJNEGQ  EQU   X'10'               DECREASE RATINGS                             
ADJPCTQ  EQU   ADJPOSQ+ADJNEGQ     ADJUST PERCENTAGE EXISTS                     
COPYOKQ  EQU   X'40'               NO DEMO WAS SELECTED FOR COPY                
ALLIMPSQ EQU   X'80'               COPY ALL IMPRESSIONS                         
*                                                                               
BYTE2    DS    XL1                                                              
* RTSADDR  DS  F                   ADDRESS OF RATING SERVICE HEADER             
SVADDR   DS    F                   SAVED ADDRESS                                
SVADJ    DS    H                   SAVED ADJUST PERCENTAGE                      
SAVEKEY  DS    CL13                DOVREC KEY                                   
SVELEM01 DS    XL42                SAVED X'01'ELEMENT                           
SVELEM05 DS    XL25                SAVED X'05'ELEMENT                           
         DS    0D                                                               
SET0LIST DS    XL16                                                             
SV00PROF DS    CL16                SAVED 00 PROF                                
SV00APRF DS    CL16                                                             
SVRTS    DS    CL1                 RATING SERVICE FOR DOVREC DSECT              
SVCLT    DS    XL2                 CLIENT                                       
SVDEMO   DS    CL10                SAVED DEMO NAME                              
TEMPDAY  DS    CL8                 PROGRAM DAY                                  
TEMPHEAD DS    CL20                TEMP HEADER AND 1 BYTE DATA                  
TEMPIMP  DS    CL7                 TEMP IMPRESSION                              
TEMPNETE DS    CL4                 TEMP NETWORK (EBCDIC)                        
TEMPPGM  DS    CL17                PROGRAM DESCRIPTION                          
TEMPPGME DS    CL4                 TEMP PROGRAM (EBCDIC)                        
TEMPTIME DS    CL11                START AND END TIME OF PROGRAM                
TMPWRK   DS    CL48                TEMPORARY WORK AREA                          
VDEMOVAL DS    F                   ADDRESS OF DEMOVAL                           
* NETTAB   DS    75CL5               NETWORK(4)/SEQNUM(1)*75 - END FFFF         
SVNETSEQ DS    X                                                                
MYQSTA   DS    CL8                                                              
MYQMKT   DS    CL4                                                              
SVADDPFK DS    XL(L'PFKEY)                                                      
SVADDKEY DS    XL(L'KEY)                                                        
NONRTGDM DS    XL1                 Y IF NON RATING DEMO ENCOUNTERED             
STATAB   DS    89XL3                                                            
IOBLOCK  DS    CL1000              TEMPORARY BLOCK                              
         EJECT                                                                  
*                                                                               
AIOD     DSECT                                                                  
DOVREC   DS    CL2000                                                           
*                                                                               
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'100SPSFM2C   08/11/11'                                      
         END                                                                    
