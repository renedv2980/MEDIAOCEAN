*          DATA SET REROIUP0S  AT LEVEL 214 AS OF 05/01/02                      
*PHASE REROIU0A,*                                                               
*INCLUDE SORTER                                                                 
*INCLUDE STXITER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE HEXIN                                                                  
ROIU     TITLE 'REROIUP0 -- REP OFFLINE INFO - FILE UPDATE'                     
*                                                                               
***********************************************************************         
*                                                                     *         
*  SOURCE:  REROIUP0                                                  *         
*  PHASE:   REROIU0                                                   *         
*                                                                     *         
*        REP OFFLINE INFORMATION SYSTEM - STAND-ALONE PROGRAM         *         
*        END OF WEEK PROCESS.                                         *         
*                                                                     *         
*        READS WORKER FILES BUILT DURING THE WEEK AND UPDATES         *         
*        REP 'ROI' (OINK) FILE.                                       *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
*  CONTROL CARDS:                                                     *         
*                                                                     *         
*   *    DATE=NORMAL   - ASSUMES SYSTEM DATE DEFINES 'WEEK OF'        *         
*                        DATE BOUNDRIES FOR WORKER FILE INDICES.      *         
*        DATE=MMMDD/YY - SPECIFIES A SINGLE DAY FOR PROCESSING.       *         
*                                                                     *         
*   *    WRITE=Y       - ENABLE FILE UPDATING                         *         
*                        (BOTH OINK AND WORKER INDEX MARKING)         *         
*        WRITE=N       - DISABLE FILE UPDATES                         *         
*                                                                     *         
*   *    PAPERWORK=Y   - ENABLE PAPERWORK COUNTING PROCESS            *         
*        PAPERWORK=N   - DISABLE PAPERWORK                            *         
*                                                                     *         
*   *    ATHENA=Y      - ENABLE ATHENA PROCESS                        *         
*        ATHENA=N      - DISABLE ATHENA PROCESS                       *         
*                                                                     *         
* (OPT)  DDSIO=(DDSIO) - ALLOW TEST VERSION OF DDSIO                  *         
*                                                                     *         
* (OPT)  UPDID=(ID)    - ALLOW CONCURRENT FILE UPDATES (ID = XX)      *         
*                        ** MUST POINT TO ALTERNATE RECOVERY FILE **  *         
*                        ** WHEN USING THIS CARD.                 **  *         
*                        ** //REPRCV1 DD (ALT)                    **  *         
*                                                                     *         
*                                                                     *         
*  '*' = CARDS EXPECTED FOR NORMAL RUN.                               *         
*                                                                     *         
*        CONTROL CARDS MUST BE SPECIFIED.  ! NO DEFAULTING !          *         
*                                                                     *         
*                                                                     *         
*        WORKER FILE INDEX FORMAT:                                    *         
*                                                                     *         
*        SJROI.XXR  WHERE:                                            *         
*              'SJROI' = FIXED (USER, SYSTEM, PROGRAM)                *         
*              .       = X'00' (1 BYTE BINARY 0)                      *         
*              XX      = 1 BYTE PACKED W/O SIGN DAY OF MONTH          *         
*              'R'     = FIXED (CLASS)                                *         
*                                                                     *         
*                                                                     *         
*        RECORDS EXPECTED FROM WORKER FILE:                           *         
*                                                                     *         
*        ID    RECORD                                                 *         
*        --    ------                                                 *         
*        0B    BUY                                                    *         
*        0C    CONTRACT                                               *         
*        14    AVAIL                                                  *         
*        16    PROPOSAL                                               *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
*  MOD LOG                                                            *         
*  -------                                                            *         
*  09/27/89  PJS  ORIGINAL DEVELOPMENT                                *         
*                                                                     *         
*  12/08/89  PJS  WHEN MARKING INDICES AS KEEP, MUST READ EACH INDEX  *         
*                 BEFORE WRITE TO UPDATE INTERNAL DMGR KEY.           *         
*                                                                     *         
* MAR06/90 (MRR) --- WHEN AN UPDATE IS MADE OUTSIDE OR PAST THE       *         
*                     FIXED WEEK GRID---PUT OUT A MESSAGE AND GO      *         
*                     ON, DON'T DUMP                                  *         
*                                                                     *         
* JUL17/92 (BU ) --- PROCESS ALL DUPLICATE ELEMENTS BECAUSE AN OUT-   *         
*                    SIDE OF GRID MESSAGE WAS BEING IGNORED           *         
*                                                                     *         
* NOV17/92 (SKU) --- PRINT CORRECT CONTRACT NUMBER WHEN AN OUTSIDE    *         
*                    OF WEEK GRID ERROR IS ENCOUNTERED                *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
*  JOB STEPS                                                          *         
*        A. HOUSEKEEPING                                              *         
*              INITIALIZE REPORT STUFF                                *         
*              READ IN CONTROL CARDS.  STOP IF ANY ARE MISSING.       *         
*              OPEN WORKER FILE.                                      *         
*              FIND WORKER INDICES FOR RUN.                           *         
*              OPEN SORT FILE                                         *         
*                                                                     *         
*        B. MAINLINE                                                  *         
*              READ RECORDS FROM ALL INDICES. AT END, C.              *         
*              CALL SORT BUILD ROUTINES BASED ON CONTROL CARDS        *         
*                                                                     *         
*        C. SORT WORKFILE                                             *         
*                                                                     *         
*        D. PROCESS SORT FILE.                                        *         
*              OPEN OINK FILE                                         *         
*              RETURN SORTED RECORD.  AT END, E.                      *         
*              CALL PROCESS ROUTINE BASED ON SORT REC TYPE.           *         
*                (1 SORT TYPE PER PROCESS TYPE)                       *         
*                                                                     *         
*        E. CLOSE FILE                                                *         
*              IF WRITE=YES, MARK INDEX FILES 'KEEP'                  *         
*                 (KEEP EQUIVALENT TO 'USED')                         *         
*              CLOSE WORKER FILE                                      *         
*              CLOSE SORT FILE                                        *         
*                                                                     *         
*        F. PRINT RUN STATS AND RECORD COUNTS                         *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
* SORT INFORMATION                                                    *         
*                                                                     *         
*        PAPERWORK COUNTING:                                          *         
*              SORT TYPE, REP, CONTRACT NUMBER (9'S COMPLEMENT)       *         
*                                                                     *         
*        ATHENA:                                                      *         
*              TO BE DETERMINED                                       *         
*                                                                     *         
* SORT TYPES:  (1 TYPE PER PROCESSES                                  *         
*                                                                     *         
*        PAPERWORK = C'10'                                            *         
*        ATHENA    = C'20'                                            *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
*        B. PROCESS RECOVERY TAPE                                     *         
*              ACCEPT CONTRACT, BUY, AVAIL, AND PROPOSAL RECORDS      *         
*                (ADDS AND CHANGES ONLY)                              *         
*              CHECK REP FOR PAPERWORK COUNTING ENABLED PROFILE       *         
*              BUILD SORT RECORD (REP CODE/CONTRACT NUMBER/ID)        *         
*              RELEASE SORT REC TO FILE                               *         
*              COUNT NUMBER OF RECORDS IN SORT                        *         
*                                                                     *         
*        C. SORT FILE.  KEY = REP CODE/CONTRACT NUMBER/REC ID         *         
*                                                                     *         
*        D. PROCESS SORTED FILE                                       *         
*              RETURN SORT REC.  AT END, FINISH LAST CONTRACT         *         
*                                        PRINT RUN STATS              *         
*                                        STOP                         *         
*              CURRENT REP/CONTRACT = PRIOR REP/CONTRACT?             *         
*                NO = FINISH PRIOR CONTRACT                           *         
*                     GET PWC REC FOR NEW CONTRACT                    *         
*                         (ADD NEW PWC REC IF NONE FOUND)             *         
*                     UPDATE LAST ACTIVITY DATE WITH RUN DATE         *         
*                     DETERMINE RELATIVE WEEK NUMBER OF TAPE          *         
*                        WEEK-OF DATE -VS- CONTRACT WEEK-OF ADD.      *         
*              BASED ON SORT REC ID, ADD 1 TO WEEK COUNTER IN         *         
*                APPROPRIATE ELEMENT.  NOTE: BUYS & CONTRACTS         *         
*                HAVE THEIR OWN ELEMENTS, AVAILS & PROPOSALS          *         
*                SHARE THE SAME PWC ELEMENT.                          *         
*                                                                     *         
***********************************************************************         
*  CHANGES:                                                           *         
*  12/30/95 (BU ) --- EXPAND RECORD SIZE OF SORT                      *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                     ***  END TOMBSTONE  ***                         *         
***********************************************************************         
*                                                                               
ROIU0    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**ROIUP0,VREGSAVE,R9,RA                                        
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         B     MAIN100                                                          
STATMSG  DC    CL20' '             WINDOW FOR DUMPS                             
         SPACE 2                                                                
DUMPADDR DS    0F'0'               LIST OF ADDRESSES TO DUMP                    
         DC    A(0),A(DUMPADDR)                                                 
         DC    A(WORKD),V(HEXIN)                                                
         DC    X'00017428',X'80020000'                                          
         DC    2F'0'                                                            
         SPACE 2                                                                
VREGSAVE DC    V(REGSAVE)                                                       
         SPACE 2                                                                
MAIN100  EQU   *                                                                
         ST    RB,DUMPADDR                                                      
         GOTO1 =V(STXITER),DMCB,DUMPADDR                                        
*                                                                               
         BAS   RE,INITIAL          OPENING HOUSEKEEPING                         
         BNZ   ERROR                                                            
*                                                                               
         BAS   RE,WRKRPROC         PROCESS WORKER FILE                          
         BNZ   ERROR                                                            
*                                                                               
         BAS   RE,UPDATE           PROCESS SORT FILE/OINK UPDATES               
         BNZ   ERROR                                                            
*                                                                               
         BAS   RE,MARKINDX         DELETE INDEX RECORDS                         
         BNZ   ERROR                                                            
*                                                                               
         BAS   RE,RUNSTAT          PRINT RUN STATS AND COUNTS                   
         BNZ   ERROR                                                            
*                                                                               
         XC    RETCODE,RETCODE     SET 0 (GOOD) RETURN CODE                     
         B     EXITBASE                                                         
*                                                                               
*- ERRORS FOUND IN RUN.  PRINT AN OBVIOUS MESSAGE                               
ERROR    EQU   *                                                                
         GOTO1 VPRINTER                                                         
         MVC   P(L'ERRMSG),ERRMSG                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         MVI   RETCODE,127         ERROR CODE                                   
*                                                                               
EXITBASE XBASE RC=RETCODE,RL=1                                                  
*                                                                               
EXIT     XIT1                      DONE                                         
         TITLE 'OPENING HOUSEKEEPING'                                           
*                                                                               
*- INITIAL HOUSEKEEPING                                                         
*        MAJOR STEPS                                                            
*              INITIALIZE REPORT STUFF                                          
*              READ IN CONTROL CARDS.  STOP IF ANY ARE MISSING.                 
*              FIND WORKER INDICES FOR RUN.                                     
*              OPEN SORT FILE                                                   
*                                                                               
INITIAL  NTR1                                                                   
         MVC   STATMSG,=CL20'INITIAL'                                           
*                                                                               
         MVI   STOP,C'N'                                                        
*                                                                               
         MVC   LINE,=PL2'99'       FORCE HEADLINE                               
         MVC   TITLE(L'TTL1),TTL1                                               
         MVC   HEADUSER(L'TTL2),TTL2                                            
*                                                                               
*- VALIDATE CONTROL CARDS.                                                      
         BAS   RE,CTLCARDS                                                      
         BNZ   INITERR                                                          
*                                                                               
*- DETERMINE START/END DATES                                                    
         BAS   RE,DATES                                                         
         BNZ   INITERR                                                          
*                                                                               
*- FIND ALL WORKER FILE INDEX RECORDS FOR THIS RUN                              
         BAS   RE,FINDINDX                                                      
         BNZ   INITERR                                                          
*                                                                               
*- OPEN SORT FILE.                                                              
         GOTO1 =V(SORTER),DMCB,SRTFLD,RECTYP,0                                  
*                                                                               
INITOK   EQU   *                                                                
         MVC   STATMSG,=CL20'INITIAL IS OK'                                     
         SR    R0,R0               SET 0 (GOOD) CC.                             
         B     INITEXIT                                                         
*                                                                               
INITERR  LTR   RD,RD               SET ^0 (BAD) CC                              
*                                                                               
INITEXIT XIT1                                                                   
         TITLE 'PROCESS CONTROL CARDS'                                          
*                                                                               
*- CTLCARDS -- READ CONTROL CARDS FROM USER'S JCL.                              
*                                                                               
*        LOG CONTROL CARDS TO PRINTER                                           
*        VALIDATE ALL PASSED CARDS                                              
*        INSURE ALL REQUIRED CARDS ARE PRESENT                                  
*                                                                               
CTLCARDS NTR1                                                                   
         MVC   STATMSG,=CL20'CONTROL CARDS'                                     
         MVC   P(13),=CL13'CONTROL CARDS'                                       
         GOTO1 VPRINTER                                                         
         MVC   P(13),=CL13'-------------'                                       
         GOTO1 VPRINTER                                                         
*                                                                               
*- READ IN CONTROL CARDS                                                        
CARD100  GOTO1 =V(CARDS),DMCB,IOAREA,=C'RE00'                                   
         CLC   =C'/*',IOAREA                                                    
         BE    CARD900             END OF CARD INPUT                            
*                                                                               
         MVC   P(80),IOAREA        LOG CONTROL CARDS TO PRINTER                 
         GOTO1 VPRINTER                                                         
*                                                                               
*- CHECK FOR VALID KEYWORD                                                      
*                                                                               
         CLC   =C'DATE=',IOAREA                                                 
         BE    CARD200                                                          
*                                                                               
         CLC   =C'WRITE=',IOAREA                                                
         BE    CARD300                                                          
*                                                                               
         CLC   =C'PAPERWORK=',IOAREA                                            
         BE    CARD310                                                          
*                                                                               
         CLC   =C'ATHENA=',IOAREA                                               
         BE    CARD320                                                          
*                                                                               
         CLC   =C'DDSIO=',IOAREA                                                
         BE    CARD500                                                          
*                                                                               
         CLC   =C'UPDID=',IOAREA                                                
         BE    CARD550                                                          
*                                                                               
         CLC   =C'IGNORE INDEX STATUS',IOAREA                                   
         BE    CARD560                                                          
*                                                                               
*- INVALID CONTROL CARD.  STOP AFTER ALL CARDS PROCESSED.                       
         MVC   P(26),=CL26'** INVALID CONTROL CARD **'                          
CARD150  GOTO1 VPRINTER                                                         
         MVI   STOP,C'Y'                                                        
         B     CARD100             PROCESS ALL CARDS                            
         SPACE 2                                                                
*                                                                               
*- DATE CARD.  NORMAL OR MMMDD/YY DATE                                          
CARD200  EQU   *                                                                
         CLI   CTLDATE,C'Y'                                                     
         BNE   CARD220                                                          
         MVC   P(30),=CL30'** ONLY 1 DATE CARD ALLOWED **'                      
         B     CARD150                                                          
*                                                                               
CARD220  MVI   CTLDATE,C'Y'        PROCESSED THE DATE CARD                      
*                                                                               
         CLC   =C'NORMAL',IOAREA+5                                              
         BE    CARD240                                                          
*                                                                               
*- DATE SPECIFIED                                                               
         MVI   FULLWEEK,C'N'       1 DAY                                        
*                                                                               
         GOTO1 =V(DATVAL),DMCB,(0,IOAREA+5),RUNDATE                             
         ICM   RF,15,P1                                                         
         BNZ   CARD100             DATE OK.  NEXT CARD                          
         MVC   P(40),=CL40'** INVALID DATE. MUST BE MMMDD/YY **'                
         B     CARD150                                                          
*                                                                               
*- NORMAL RUN.  GET TODAY'S DATE                                                
CARD240  EQU   *                   GET SYSTEM DATE, YYMMDD FORMAT               
         MVI   FULLWEEK,C'Y'       FULL WEEK'S PROCESSING                       
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,IOAREA+5),(0,RUNDATE)                         
         B     CARD100             GET NEXT CARD.                               
         SPACE 2                                                                
*                                                                               
*- Y/N CARDS COME THRU HERE.                                                    
*                                                                               
*  R1=LENGTH OF KEYWORD (INCLUDES '=')                                          
*  R2=A(PROCESSED SWITCH) (PREVENT DUPLICATE CARDS)                             
*  R3=A(INTERNAL Y/N SWITCH)                                                    
         SPACE                                                                  
*                                                                               
*- WRITE=                                                                       
CARD300  EQU   *                                                                
         LA    R1,6                                                             
         LA    R2,CTLWRITE                                                      
         LA    R3,MARKFILE                                                      
         B     CARD400                                                          
*                                                                               
*- PAPERWORK=                                                                   
CARD310  EQU   *                                                                
         LA    R1,10                                                            
         LA    R2,CTLPWC                                                        
         LA    R3,PWC                                                           
         B     CARD400                                                          
*                                                                               
*- ATHENA=                                                                      
CARD320  EQU   *                                                                
         LA    R1,7                                                             
         LA    R2,CTLATHEN                                                      
         LA    R3,ATHENA                                                        
         B     CARD400                                                          
*                                                                               
*- PREVENT DUPLICATE CONTROL CARDS                                              
CARD400  EQU   *                                                                
         CLI   0(R2),C'Y'                                                       
         BNE   CARD420                                                          
         MVC   P(L'ONLY1),ONLY1    DUPLICATE                                    
         B     CARD150                                                          
*                                                                               
*- CHECK ACTUAL INPUT FOR 'Y' OR 'N'                                            
CARD420  MVI   0(R2),C'Y'          MARK AS PROCESSED.                           
         LA    RF,IOAREA                                                        
         AR    R1,RF               A(INPUT BYTE)                                
*                                                                               
         CLI   0(R1),C'Y'                                                       
         BE    CARD440                                                          
         CLI   0(R1),C'N'                                                       
         BE    CARD440                                                          
         MVC   P(L'YORN),YORN      Y OR N ONLY                                  
         B     CARD150                                                          
*                                                                               
CARD440  MVC   0(1,R3),0(R1)         SAVE 'Y' OR 'N' TO SWITCH AREA             
         B     CARD100             NEXT CARD                                    
*                                                                               
*- DDSIO=  *OPTIONAL CARD FOR TESTING DDSIO*                                    
CARD500  L     R1,=V(DDSIO)                                                     
         MVC   0(8,R1),IOAREA+6                                                 
         B     CARD100                                                          
*                                                                               
*- UPDID=  *OPTIONAL CARD ALLOWS CONCURRENT FILE UPDATES* *                     
CARD550  EQU   *                                                                
         GOTO1 DATAMGR,P1,=C'UPDID',=C'REP',FLIST,IOAREA                        
         L     R1,P4               A(UPDATE ID FROM DMGR)                       
         MVC   0(2,R1),IOAREA+6                                                 
         B     CARD100                                                          
*                                                                               
*- IGNORE INDEX STATUS -- RE-USE MARKED INDICES (J.I.C. FOUL UP)                
CARD560  MVI   ALLINDEX,C'Y'                                                    
         B     CARD100                                                          
         SPACE 2                                                                
*                                                                               
*- MAKE SURE ALL CONTROL CARDS WERE PASSED                                      
CARD900  EQU   *                                                                
         LA    R2,CTLTABLE                                                      
CARD920  CLC   =F'0',0(R2)                                                      
         BE    CARDEXIT            END OF LIST                                  
*                                                                               
         L     R1,0(R2)            A(FIELD)                                     
         CLI   0(R1),C'Y'          CONTROL CARD PASSED?                         
         BE    CARD940                                                          
*                                                                               
         MVI   STOP,C'Y'           ERROR WITH CARDS. TERMINATE                  
*                                                                               
         MVC   P(L'MISSING),MISSING                                             
         MVC   P+1+L'MISSING(12),4(R2) TAG TO PRINT LINE                        
         GOTO1 VPRINTER                                                         
*                                                                               
CARD940  LA    R2,16(R2)           NEXT ENTRY                                   
         B     CARD920                                                          
*                                                                               
CARDEXIT EQU   *                                                                
         GOTO1 VPRINTER            BLANK LINE AFTER CONTROL LOG                 
*                                                                               
         CLI   STOP,C'Y'                                                        
         BE    CARDERR                                                          
*                                                                               
         MVC   P(L'OKCARDS),OKCARDS                                             
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SR    R0,R0               SET 0 CC                                     
         B     EXIT                                                             
*                                                                               
CARDERR  EQU   *                                                                
         MVC   P(L'BADCARDS),BADCARDS                                           
         GOTO1 VPRINTER                                                         
         LTR   RD,RD               NON-0 CC                                     
         B     EXIT                                                             
         SPACE 2                                                                
*                                                                               
*- CONTROL CARD LIST.  EVERY CARD IN THIS LIST MUST BE PASSED.                  
*  FORMAT:                                                                      
*        +0    A(1 BYTE CONTROL FIELD). 'Y' = CARD PASSED                       
*        +4    CL12'LABEL IF CARD NOT PASSED'                                   
*                                                                               
         DS    0F                  MUST BE WORD BOUNDARY                        
CTLTABLE EQU   *                                                                
         DC    A(CTLDATE),CL12'DATE'                                            
         DC    A(CTLWRITE),CL12'WRITE'                                          
         DC    A(CTLPWC),CL12'PAPERWORK'                                        
         DC    A(CTLATHEN),CL12'ATHENA'                                         
         DC    F'0'                END OF LIST                                  
         TITLE 'DETERMINE START AND END DATES'                                  
*                                                                               
*- DATES -- DETERMINE START/END DATES                                           
*                                                                               
*  FILL IN 'START' AND 'END' DATES                                              
*                                                                               
*  FOR SINGLE DATE RUN (FULLWEEK = 'N'), 'RUNDATE' IS THE DATE.                 
*                                                                               
*  FOR NORMAL WEEKLY RUN, RUNDATE IS WITHIN DESIRED WEEK.                       
*        SET START TO MONDAY DATE.                                              
*        SET END TO SATURDAY DATE.                                              
*                                                                               
*  FIND SYSTEM DATE, SAVE AS 'TODAY' (ACTIVITY MARKER)                          
*  FIND 'WEEKOF' (MONDAY) DATE (BACK UP START DATE TO MONDAY)                   
*                                                                               
DATES    NTR1                                                                   
         MVC   STATMSG,=CL20'DATES'                                             
         MVC   START(6),RUNDATE    ASSUME 1 DATE RUN                            
         MVC   END(6),RUNDATE                                                   
         CLI   FULLWEEK,C'N'                                                    
         BE    DATES200                                                         
*                                                                               
*- FULL WEEK RUN.  BACK UP 'RUNDATE' TO MONDAY (SAVE AS 'START')                
         GOTO1 =V(GETDAY),P1,RUNDATE,THREE                                      
         LA    RE,1                                                             
         ZIC   RF,P1               RETURNED DAY OF WEEK                         
         SR    RE,RF                                                            
         ST    RE,P3               # OF DAYS TO BACKUP (MAY BE 0)               
         GOTO1 =V(ADDAY),P1,RUNDATE,START                                       
*                                                                               
*- SET END TO SATURDAY DATE.                                                    
DATES100 EQU   *                                                                
         GOTO1 =V(ADDAY),P1,START,END,5                                         
*                                                                               
*- FIND TODAY                                                                   
DATES200 EQU   *                                                                
         GOTO1 =V(DATCON),DMCB,(5,IOAREA+5),(0,TODAY)                           
*                                                                               
*- FIND WEEKOF DATE (BACK UP START DATE TO MONDAY)                              
*  (COULD BE DIFFERENT ON SINGLE DAY RUN)                                       
         GOTO1 =V(GETDAY),P1,START,THREE                                        
         LA    RE,1                                                             
         ZIC   RF,P1               RETURNED DAY OF WEEK                         
         SR    RE,RF                                                            
         ST    RE,P3               # DAYS TO BACKUP (COULD BE 0)                
         GOTO1 =V(ADDAY),P1,START,WEEKOF                                        
*                                                                               
         SR    R0,R0                                                            
         B     EXIT                                                             
         TITLE 'FIND WORKER INDICES FOR THIS RUN'                               
*                                                                               
*- FIND WORKER FILE INDICES FOR THIS RUN.                                       
FINDINDX NTR1                                                                   
         MVC   STATMSG,=CL20'FINDINDX'                                          
*                                                                               
*- FIND INDICES FOR EACH DAY BETWEEN START AND END DATES.                       
*  SAVE THEM IN THE INDEX TABLE.                                                
         LA    R2,INDEXTBL                                                      
         SR    R3,R3               DAY INCREMENT                                
FINDX100 EQU   *                                                                
         GOTO1 =V(ADDAY),P1,START,WORKDATE,(R3)                                 
*                                                                               
         CLC   WORKDATE(6),END                                                  
         BH    FINDX400            PAST ENDING DATE                             
*                                                                               
*- LOOK FOR INDEX FOR GIVEN YYMMDD.                                             
         GOTO1 GETINDEX,P1,(1,WORKDATE)                                         
         BNZ   FINDX200            NOT FOUND                                    
*                                                                               
*- INDEX WAS FOUND.  IF MULTIPLE INDICES FOR SAME DATE, LOG ERROR.              
*  SET STOP SWITCH TO TERMINATE AFTER ALL INDICES CHECKED.                      
         CLI   P2,1                OCCURRENCE COUNT FROM GETINDEX               
         BE    FINDX140                                                         
*                                                                               
         MVC   P(L'MULTINDX),MULTINDX                                           
         MVI   STOP,C'Y'                                                        
         B     FINDX220                                                         
*                                                                               
FINDX140 MVC   NDXNDX(16,R2),INDEX      SAVE 16 BYTE INDEX                      
         MVC   NDXDATE(6,R2),WORKDATE   SAVE YYMMDD FORMAT DATE                 
*                                                                               
         LA    R2,NDXLEN(R2)                                                    
         B     FINDX300                                                         
*                                                                               
*- INDEX MISSING FOR A PARTICULAR DAY.                                          
*  LOG THIS WARNING TO REPORT.                                                  
FINDX200 EQU   *                                                                
         MVC   P(L'INDXGONE),INDXGONE                                           
FINDX220 MVC   P+L'INDXGONE(6),WORKDATE       DATE OF MISSING INDEX             
         GOTO1 =V(GETDAY),P1,WORKDATE,DUB                                       
         ZIC   RF,P1                                                            
         SLL   RF,4                (DAY NUMBER) X 16                            
         LA    RE,DAYLITS-16                                                    
         AR    RE,RF                                                            
         MVC   P+8+L'INDXGONE(16),0(RE)  DAY LITERAL                            
         GOTO1 VPRINTER                                                         
*                                                                               
*- IF THIS IS A SINGLE DAY'S RUN, WE'RE GOT AN ERROR                            
         CLI   FULLWEEK,C'Y'                                                    
         BNE   EXIT                NON-0 (BAD) CC                               
*                                                                               
*- LOOK FOR NEXT DAY'S INDEX                                                    
FINDX300 LA    R3,1(R3)                                                         
         B     FINDX100                                                         
*                                                                               
FINDX400 EQU   *                                                                
         CLI   STOP,C'N'           SET RC. (0=GOOD, ^0=BAD)                     
         B     EXIT                                                             
         TITLE 'PROCESS WORKER FILE.  BUILD SORT FILE'                          
*                                                                               
*- WRKRPROC -- PROCESS WORKER FILE INDICES                                      
*              BUILD SORT RECORDS                                               
*                                                                               
WRKRPROC NTR1                                                                   
         MVC   STATMSG,=CL20'WRKRPROC'                                          
*                                                                               
         GOTO1 VPRINTER            STARTING WORKER PROCESS                      
         MVC   P(L'DOWRKR),DOWRKR                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         MVI   FIRSTSW,C'Y'                                                     
*                                                                               
*- READ WORKER RECORDS FOR EACH INDEX IN THE INDEX LIST.                        
*  WE'RE DONE WHEN WE READ LAST RECORD FROM LAST INDEX.                         
         LA    R2,INDEXTBL                                                      
*                                                                               
         LA    RC,SORTREC          SORT RECORD AREA                             
         USING SORTRECD,RC                                                      
*                                                                               
WRKR100  EQU   *                                                                
         CLI   0(R2),0             END OF INDEX TABLE?                          
         BE    WRKR950                                                          
*                                                                               
         GOTO1 GETINDEX,P1,(2,(R2))                                             
         BZ    *+6                                                              
         DC    H'0'                WHERE DID THE INDEX GO?                      
WRKR120  EQU   *                                                                
         GOTO1 DATAMGR,P1,=C'READ',=C'WKFILE',INDEX,WRKREC,AWRKRB               
         TM    P3,X'80'                                                         
         BO    WRKR900             END OF RECORDS                               
*                                                                               
*****    MVC   P(11),=C'WORKER DATA'                                            
*****    MVC   P+13(132-13),WRKREC                                              
*****    GOTO1 VPRINTER                                                         
*                                                                               
*****    GOTO1 =V(HEXOUT),P1,WRKREC,P,66,=C'STD'                                
*****    GOTO1 VPRINTER                                                         
*****    GOTO1 VPRINTER                                                         
*                                                                               
         CLI   PWC,C'Y'            PAPERWORK PROCESSING?                        
         BNE   WRKR200                                                          
         BAS   RE,DOPWC                                                         
         BNZ   WRKRERR                                                          
*                                                                               
WRKR200  CLI   ATHENA,C'Y'         ATHENA PROCESSING?                           
         BNE   WRKR220                                                          
         BAS   RE,DOATHENA                                                      
         BNZ   WRKRERR                                                          
*                                                                               
WRKR220  EQU   *                                                                
         MVI   FIRSTSW,C'N'                                                     
         B     WRKR120             READ NEXT RECORD                             
*                                                                               
WRKR900  EQU   *                   DO NEXT INDEX                                
         LA    R2,NDXLEN(R2)                                                    
         B     WRKR100                                                          
*                                                                               
WRKR950  SR    R0,R0               0 CC                                         
         B     WRKREXIT                                                         
*                                                                               
WRKRERR  LTR   RD,RD                                                            
*                                                                               
WRKREXIT B     EXIT                                                             
         TITLE 'PAPERWORK DETAIL PROCESS'                                       
*                                                                               
*- DOPWC -- BUILD PAPERWORK SORT RECORD FROM WORKER RECORD                      
*                                                                               
*  WRITE OUT END OF PWC REC INDICATOR ON 1ST PASS                               
*    (REP CODE = X'FFFF')                                                       
*                                                                               
DOPWC    NTR1                                                                   
         MVC   STATMSG,=CL20'DOPWC'                                             
*                                                                               
*- SEE IF THIS RECORD APPLIES TO PAPERWORK                                      
         LA    R2,FILTPWC                                                       
DOPWC100 CLC   RKEY(1),FPWCKEY(R2)                                              
         BE    DOPWC120                                                         
         LA    R2,FPWCNTRY(R2)     NEXT FILTER ENTRY                            
         CLI   0(R2),0                                                          
         BNE   DOPWC100                                                         
         B     DOPWCOK             RECORD DOES NOT APPLY                        
*                                                                               
*- BUILD SORT KEY                                                               
DOPWC120 XC    SORTKEY,SORTKEY                                                  
         MVC   SRTKTYPE,PWCTYPE    SORT RECORD TYPE                             
*                                                                               
         LA    RF,RKEY             START OF DATA RECORD                         
*                                                                               
         ZIC   R7,FPWCREP(R2)      REP CODE DISPLACEMENT                        
         AR    R7,RF                                                            
         MVC   SRTPREP,0(R7)                                                    
*                                                                               
         ZIC   R8,FPWCCON(R2)      CONTRACT NUMBER                              
         AR    R8,RF                                                            
         MVC   FULL,0(R8)                                                       
*                                                                               
         CLI   FPWCNINE(R2),X'9'   9'S COMPLEMENT?                              
         BNE   DOPWC140                                                         
*                                                                               
*- UNDO A CLASSIC DONALDISM.....REVERSE 9'S COMPLEMENT                          
         MVC   DUB,FULL            GET NORMAL 9'S COMPLEMENT                    
         PACK  FULL+0(1),DUB+3(1)                                               
         PACK  FULL+1(1),DUB+2(1)                                               
         PACK  FULL+2(1),DUB+1(1)                                               
         PACK  FULL+3(1),DUB+0(1)  FULL = PWOS 9'S COMPLEMENT                   
*                                                                               
         ZAP   WORK(5),=P'99999999'                                             
*                                                                               
         XC    WORK+5(5),WORK+5                                                 
         MVI   WORK+5+4,X'0C'      POSITIVE SIGN                                
         MVO   WORK+5(5),FULL(4)                                                
         SP    WORK(5),WORK+5(5)   WORK=PACK CONTRACT #                         
*                                                                               
         MVO   FULL(5),WORK(5)     FULL= 4 BYTE PWOS CONTRACT #                 
*                                                                               
DOPWC140 MVC   SRTPCON,FULL                                                     
*                                                                               
         MVC   SRTPID,RKEY         REC ID = KEY ID                              
*                                                                               
*- SET SORT RECORD LENGTH                                                       
         LH    RF,WRKREC           WORKER RECORD LENGTH                         
         LA    RF,LSORTKEY+4(RF)   + SORT KEY OVERHEAD    (....LLLL)            
         SLL   RF,16               (LLLL....)                                   
         ST    RF,SORTREC          = SORT RECORD LENGTH                         
*                                                                               
*- RELEASE SORT RECORD                                                          
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         AP    NUMPWC,=P'1'                                                     
*                                                                               
         CLI   FIRSTSW,C'Y'        NEED TO WRITE OUT LAST REC?                  
         BNE   DOPWCOK                                                          
*                                                                               
         XC    SORTKEY,SORTKEY                                                  
         MVC   SRTKTYPE,PWCTYPE                                                 
         MVC   SRTPREP,=XL2'FFFF'  SORT TO END OF PWC'S                         
         MVC   SORTREC(2),=AL2(LSORTKEY+4)                                      
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
*                                                                               
DOPWCOK  SR    R0,R0                                                            
DOPWCEXT B     EXIT                                                             
         SPACE 2                                                                
FPWCKEY  EQU   0                   KEY ID                                       
FPWCREP  EQU   1                   REP CODE DISP (IN RKEY)                      
FPWCCON  EQU   2                   CONTRACT NUMBER DISP                         
FPWCELEM EQU   3                   PWC ACTIVITY ELEMENT CODE                    
FPWCNINE EQU   4                   9=CONTRACT IS NINE'S COMPLEMENT              
FPWCNTRY EQU   5                   *ENTRY LENGTH                                
*                                                                               
FILTPWC  EQU   *                                                                
         DC    X'0B',AL1(16),AL1(18),X'02',X'9'   BUY                           
         DC    X'0C',AL1(02),AL1(23),X'03',X'0'   CONTRACT                      
         DC    X'14',AL1(17),AL1(19),X'04',X'9'   AVAIL                         
         DC    X'16',AL1(14),AL1(16),X'04',X'9'   PROPOSAL                      
         DC    H'0'                                                             
         TITLE 'ATHENA DETAIL PROCESS'                                          
*                                                                               
*- DOATHENA -- BUILD ATHENA SORT RECORD                                         
*                                                                               
DOATHENA NTR1                                                                   
         MVC   STATMSG,=CL20'DOATHENA'                                          
********************************************************************            
********************************************************************            
**                                                                **            
**                                                                **            
**                                                                **            
**                                                                **            
**                                                                **            
**             A LITTLE CODE HERE WOULD BE A NICE TOUCH           **            
**                                                                **            
**                                                                **            
**                                                                **            
**                                                                **            
**                                                                **            
**                                                                **            
**                                                                **            
********************************************************************            
********************************************************************            
         SR    R0,R0               GOOD RETURN CODE                             
         B     EXIT                                                             
         SPACE 2                                                                
         DROP  RC                  (SORT RECORD)                                
         TITLE 'PROCESS SORT FILE, UPDATE OINK FILE.'                           
*                                                                               
*- PROCESS SORT FILE, UPDATE OINK FILE                                          
UPDATE   NTR1                                                                   
         MVC   STATMSG,=CL20'UPDATE'                                            
*                                                                               
         GOTO1 VPRINTER            STARTING UPDATE PROCESS                      
         MVC   P(L'DOUPDAT),DOUPDAT                                             
         GOTO1 VPRINTER                                                         
*                                                                               
*- POINT TO CURRENT IO AREA                                                     
         LA    RF,IOAREA                                                        
         ST    RF,AIOAREA          POINT TO I/O BUFFER                          
*                                                                               
*- OPEN DATAMGR                                                                 
         BAS   RE,ROIOPENU                                                      
*                                                                               
         MVC   STATMSG,=CL20'UPDATE/DMGR OPEN'                                  
*                                                                               
         MVI   FIRSTSW,C'Y'        1ST PASS INDICATOR                           
*                                                                               
*- GET SORT RECORD                                                              
UPDAT100 EQU   *                                                                
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         L     RC,DMCB+4           A(SORT REC) OR X'0' = EOF                    
         LTR   RC,RC                                                            
         BZ    UPDATOK             ALL DONE                                     
*                                                                               
         AP    NUMSORT,=P'1'       COUNT SORTED RECS                            
*                                                                               
         USING SORTRECD,RC         COVER SORT REC WITH RC                       
*                                                                               
         CLC   SRTKTYPE,PWCTYPE                                                 
         BNE   UPDAT210                                                         
*                                                                               
         BAS   RE,UPPWC            PWC UPDATE                                   
         BNZ   UPDATERR                                                         
*                                                                               
UPDAT210 CLC   SRTKTYPE,ATHTYPE                                                 
         BNE   UPDAT220                                                         
*                                                                               
         BAS   RE,UPATHENA         ATHENA UPDATE                                
         BNZ   UPDATERR                                                         
*                                                                               
UPDAT220 EQU   *                                                                
         MVI   FIRSTSW,C'N'        NOT 1ST PASS ANYMORE                         
         B     UPDAT100            READ NEXT SORT RECORD                        
*                                                                               
UPDATOK  SR    R0,R0               0 CC                                         
         B     UPDATEXT                                                         
*                                                                               
UPDATERR LTR   RD,RD               NON-0 CC                                     
UPDATEXT B     EXIT                                                             
         TITLE 'UPDATE PWC RECORD FROM SORTED RECORD'                           
*                                                                               
*- UPPWC -- UPDATE PWC RECORD FROM SORTED RECORD                                
*                                                                               
UPPWC    NTR1                                                                   
         MVC   STATMSG,=CL20'UPPWC'                                             
*                                                                               
*****    MVC   P(7),=C'SORTKEY'                                                 
*****    GOTO1 =V(HEXOUT),P1,SORTKEY,P+10,10,=C'STD'                            
*****    GOTO1 VPRINTER                                                         
*                                                                               
*- IF SORT RECORD IS 'END OF PWC' MARKER, WRITE OUT LAST                        
*  PWC RECORD AND EXIT.                                                         
         CLC   SRTPREP,=XL2'FFFF'                                               
         BNE   UPPWC100                                                         
*                                                                               
UPPWC050 BAS   RE,WRITPWC          WRITE OUT LAST RECORD                        
         B     UPPWCEXT            AND EXIT.                                    
*                                                                               
*- IF REP/CONTRACT BREAK, FINISH PRIOR RECORD AND GET NEW PWC REC               
UPPWC100 EQU   *                                                                
         CLI   FIRSTSW,C'Y'                                                     
         BE    UPPWC300            1ST PASS. NO PRIOR RECORD                    
*                                                                               
         CLC   SAVEKEY(8),SORTKEY  REC TYPE/REP CODE/CONTRACT NUMBER            
         BE    UPPWC400                                                         
*                                                                               
         BAS   RE,WRITPWC          FINISH PRIOR PWC REC                         
*                                                                               
UPPWC300 BAS   RE,GETPWC           GET NEW PWC REC                              
*                                                                               
         MVC   SAVEKEY(8),SORTKEY  SAVE NEW KEY (REP/CONTRACT NUMBER)           
*                                                                               
         XC    SAVEID,SAVEID       FORCE ELEMENT LOOKUP                         
*                                                                               
*- FIND ACTIVITY ELEMENT BASED ON SORT RECORD ID.                               
*  IF REC ID HAS NOT CHANGED FROM LAST TIME, USE A(ELEM)                        
*     FROM LAST PASS.                                                           
UPPWC400 EQU   *                                                                
         L     RF,PWCNUM           # SORT RECS IN PWC REC                       
         LA    RF,1(RF)                                                         
         ST    RF,PWCNUM                                                        
*                                                                               
         L     RF,SAVEADDR         A(ELEMENT)                                   
****     CLC   SRTPID,SAVEID       SAME ID AS LAST PASS?                        
****     BE    UPPWC500                                                         
*                                                                               
*   ABOVE INSTRUCTIONS DROPPED BECAUSE OF OUT-OF-RANGE MESSAGE                  
*                                                                               
         MVC   SAVEID,SRTPID     SAVE FOR NEXT TIME                             
*                                                                               
         LA    RE,FILTPWC          FIND ELEMENT ID FOR THIS RECORD              
*                                                                               
UPPWC420 CLC   SRTPID,FPWCKEY(RE)                                               
         BE    UPPWC440                                                         
*                                                                               
         LA    RE,FPWCNTRY(RE)                                                  
         CLI   0(RE),0                                                          
         BNE   UPPWC420                                                         
         DC    H'0'                SORT REC ID NOT IN TABLE                     
*                                                                               
*- FIND ELEMENT WITHIN PWC RECORD                                               
UPPWC440 EQU   *                                                                
         LA    RF,RPWCEL01                                                      
UPPWC460 CLI   0(RF),0                                                          
         BNE   UPPWC480                                                         
         DC    H'0'                END OF PWC REC, ELEMENT NOT FOUND            
*                                                                               
UPPWC480 CLC   0(1,RF),FPWCELEM(RE)  MATCH ON ELEMENT CODE?                     
         BE    UPPWC490                                                         
*                                                                               
         ZIC   R0,1(RF)            NEXT ELEMENT                                 
         AR    RF,R0                                                            
         B     UPPWC460                                                         
*                                                                               
*- MAKE SURE ADDRESS + WEEKDISP IS STILL  WITHIN ELEMENT                        
UPPWC490 EQU   *                                                                
         ST    RF,SAVEADDR         ASSUME IT'S OK                               
         ZIC   R0,1(RF)            EL LEN                                       
         C     R0,WEEKDISP                                                      
         BH    UPPWC500                                                         
*                                                                               
         MVC   P(42),=C'* CONTRACT ACTIVITY OUTSIDE OF WEEK GRID *'             
         GOTO1 VPRINTER                                                         
         MVC   P(8),=C'REP CODE'                                                
         MVC   P+9(2),RPWCKREP                                                  
         MVC   P+12(8),=C'CONTRACT'                                             
*                                                                               
         ZAP   WORK(5),=P'0'       CONTRACT NUMBER                              
         MVO   WORK(5),RPWCKCDE                                                 
         EDIT  (P5,WORK),(8,P+22),ALIGN=LEFT                                    
*                                                                               
         MVC   P+32(7),=C'EL CODE'                                              
         MVC   P+40(1),0(RF)                                                    
         OI    P+40,X'F0'                                                       
         MVC   P+42(17),=C'REC/CURRENT DATES'                                   
         MVC   P+60(8),RPWCCWOD                                                 
         MVI   P+69,C'/'                                                        
         MVC   P+71(8),WEEKOF                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         B     UPPWC520            BY-PASS THIS UPDATE                          
*                                                                               
*- UPDATE WEEKLY ACTIVITY COUNTER                                               
UPPWC500 EQU   *                                                                
         A     RF,WEEKDISP                                                      
         CLI   0(RF),X'FF'         ALREADY AT MAX COUNT?                        
         BE    UPPWC520                                                         
*                                                                               
         ZIC   RE,0(RF)            INCREMENT COUNT BY 1                         
         LA    RE,1(RE)                                                         
         STC   RE,0(RF)                                                         
*                                                                               
*- IF CONTRACT WAS DELETED WE'LL NEED TO DELETE PWC RECORD.                     
UPPWC520 EQU   *                                                                
         CLI   RKEY,X'0C'          CONTRACT?                                    
         BNE   UPPWCEXT                                                         
*                                                                               
         LA    RF,RKEY+27+2        A(RECORD CONTROL BYTE)                       
         TM    0(RF),X'80'                                                      
         BZ    UPPWCEXT                                                         
         MVI   DELPWC,C'Y'         DELETE PWC FOR THIS CONTRACT                 
*                                                                               
*****    MVC   P(8),=C'DELETED *'                                               
*****    GOTO1 =V(HEXOUT),P1,RKEY,P+8,50,=C'STD'                                
*****    GOTO1 VPRINTER                                                         
*                                                                               
UPPWCEXT SR    R0,R0               DONE WITH THIS RECORD                        
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*- GETPWC -- GET PWC RECORD FOR CURRENT REP/CONTRACT NUMBER.                    
*                                                                               
*  IF PWC RECORD IS ON FILE, READ IT IN AND INDICATE EXISTING RECORD.           
*  ELSE BUILD NEW PWC RECORD AND INDICATE NEW RECORD.                           
*                                                                               
*  INPUT: SRTREP - REP CODE                                                     
*         SRTCNUM- CONTRACT NUMBER                                              
*                                                                               
*  OUTPUT: RPWCREC                                                              
*          OLDREC/NEWREC SWITCH                                                 
*          WEEKDISP - BYTE DISPLACEMENT INTO ACTIVITY COUNT ELEMENT             
*                     FOR TAPE WEEK-OF DATE (RELATIVE TO CONTRACT ADD)          
*                     (ALL ELEMENTS HAVE SAME DISP FOR GIVEN DATE)              
*                                                                               
GETPWC   NTR1                                                                   
         MVC   STATMSG,=CL20'GETPWC'                                            
         LA    RF,RPWCREC                                                       
         ST    RF,AIOAREA                                                       
*                                                                               
         XC    PWCNUM,PWCNUM       RESET # SORT REC IN PWC COUNT                
*                                                                               
         MVI   OLDREC,C'N'                                                      
         MVI   NEWREC,C'N'                                                      
         MVI   DELPWC,C'N'                                                      
*                                                                               
*- LOOK FOR EXISTING                                                            
         XC    RPWCKEY,RPWCKEY                                                  
         MVI   RPWCKEY,X'2F'       PWC REC ID                                   
         MVC   RPWCKREP(6),SRTPREP    REP (2), CONTRACT (4)                     
         MVC   KEY,RPWCKEY         SAVE FOR COMPARE                             
*                                                                               
         BAS   RE,HDROI                                                         
         CLC   RPWCKEY,KEY         FIND REC ON FILE?                            
         BNE   GETPWC20                                                         
*                                                                               
         BAS   RE,GFROI            READ IN RECORD                               
*         CLC   RPWCKCDE,=X'03034349' BAD CONTRACT: BLAIR                       
*         BNE   GETPWC10            NOT IT                                      
*         CLC   RPWCKREP,=C'BL'     BLAIR?                                      
*         BNE   GETPWC10            NO                                          
*         MVI   RPWCEL03,X'03'      RESET                                       
*         MVI   RPWC3LEN,X'66'      RESET                                       
*         MVI   RPWCEL04,X'04'      RESET                                       
*         MVI   RPWC4LEN,X'16'      RESET                                       
*GETPWC10 EQU   *                                                               
         MVI   OLDREC,C'Y'         EXISTING RECORD                              
         MVC   RPWCALST,TODAY      UPDATED TODAY                                
         B     GETPWC50                                                         
*                                                                               
*- NEW RECORD.                                                                  
GETPWC20 XC    RPWCCNTL(256),RPWCCNTL                                           
         XC    RPWCEL02(102),RPWCEL02                                           
         XC    RPWCEL03(102),RPWCEL03                                           
         XC    RPWCEL04(52),RPWCEL04                                            
*                                                                               
         MVC   RPWCLEN(2),=AL2(RPWCRLEN) REC LENGTH                             
*                                                                               
         MVC   RPWCEL01(2),=X'011C'                                             
         GOTO1 DATCON,P1,(3,RDATE),(0,RPWCCADD)  RECOVERY TAPE DATE             
*                                                                               
         MVC   RPWCCWOD,WEEKOF     'WEEK-OF' DATE                               
         MVI   RPWCFMT,C'L'        LONG FORMAT (FIXED ELEMENT)                  
*                                                                               
         MVC   RPWCEL02(2),=X'0266'  BUYS (100 WEEKS)                           
         MVC   RPWCEL03(2),=X'0366'  CONTRACTS (100 WEEKS)                      
         MVC   RPWCEL04(2),=X'0416'  AVAIL/PROPOSAL (20 WEEKS)                  
*                                                                               
         MVC   RPWCELEF(2),=X'EF0E' ACTIVITY ELEMENT                            
         MVC   RPWCAFST,TODAY      ADDED TODAY                                  
         MVC   RPWCALST,TODAY      UPDATED TODAY                                
*                                                                               
         MVI   NEWREC,C'Y'         NEW RECORD                                   
*                                                                               
*- DETERMINE 'WEEKDISP' DISPLACEMENT                                            
GETPWC50 EQU   *                                                                
         GOTO1 RELWEEK,DMCB,RPWCCWOD,WEEKOF,WORK,0,0,0                          
*                                                                               
         L     RE,P1               # WEEKS                                      
         LA    RE,2(RE)            +2 EL CODE/LEN                               
         ST    RE,WEEKDISP         = BYTE DISP.                                 
*                                                                               
         LA    RF,IOAREA                                                        
         ST    RF,AIOAREA                                                       
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*- WRITPWC -- WRITE PWC RECORD BACK TO FILE.  (ADD IF NEW)                      
*             COUNT NUMBER OF CHANGES/ADDS                                      
*                                                                               
WRITPWC  NTR1                                                                   
         B     WRIT040                                                          
*                                                                               
         MVC   P(3),=C'PWC'                                                     
*                                                                               
         EDIT  PWCNUM,(4,P+5)                                                   
*                                                                               
         MVC   P+13(132-20),RPWCREC                                             
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R2,RPWCREC                                                       
         LA    R3,RPWCRLEN                                                      
         AR    R3,R2                                                            
*                                                                               
WRIT020  GOTO1 =V(HEXOUT),P1,(R2),P,66,=C'STD'                                  
         GOTO1 VPRINTER                                                         
         LA    R2,66(R2)                                                        
         CR    R2,R3                                                            
         BL    WRIT020                                                          
         GOTO1 VPRINTER                                                         
*                                                                               
         SPACE 5                                                                
WRIT040  MVC   STATMSG,=CL20'WRITPWC'                                           
         LA    RF,RPWCREC                                                       
         ST    RF,AIOAREA                                                       
*                                                                               
*- DELETE PWC RECORD?                                                           
         CLI   DELPWC,C'Y'                                                      
         BE    WPWC300                                                          
*                                                                               
         CLI   OLDREC,C'Y'                                                      
         BE    WPWC100                                                          
*                                                                               
         CLI   NEWREC,C'Y'                                                      
         BE    WPWC200                                                          
*                                                                               
         DC    H'0'                WHAT ARE WE DOING HERE?                      
*                                                                               
*- EXISTING RECORD.                                                             
WPWC100  L     RF,PWCCHA           COUNT # RECS CHANGED                         
         LA    R0,1                                                             
         AR    RF,R0                                                            
         ST    RF,PWCCHA                                                        
*                                                                               
         CLI   MARKFILE,C'Y'       MARK FILE?                                   
         BNE   WPWC900                                                          
*                                                                               
         BAS   RE,PFROI            WRITE BACK RECORD                            
         B     WPWC900                                                          
*                                                                               
*- NEW RECORD. ADD TO FILE                                                      
WPWC200  EQU   *                                                                
         L     RF,PWCADD           COUNT # RECS ADDED                           
         LA    R0,1                                                             
         AR    RF,R0                                                            
         ST    RF,PWCADD                                                        
*                                                                               
         CLI   MARKFILE,C'Y'       MARK FILE?                                   
         BNE   WPWC900                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'RPWCKEY),RPWCKEY                                           
*                                                                               
         BAS   RE,AFROI            ADD RECORD                                   
*                                                                               
         MVC   KEY+L'RPWCKEY+1(4),KEY   MOVE IN DISK ADDRESS                    
         MVC   KEY(L'RPWCKEY),RPWCKEY   RESTORE KEY DATA                        
         BAS   RE,ADROI                 ADD KEY                                 
         B     WPWC900                                                          
*                                                                               
*- DELETE -- IF NEW RECORD, DO NOTHING (NOT ON FILE TO DELETE)                  
*            IF OLD RECORD, DELETE KEY & RECORD.                                
WPWC300  EQU   *                                                                
         L     RF,PWCDEL                                                        
         LA    RF,1(RF)                                                         
         ST    RF,PWCDEL                                                        
*                                                                               
         CLI   NEWREC,C'Y'                                                      
         BE    WPWC900             NO REC ON FILE TO DELETE                     
*                                                                               
         CLI   MARKFILE,C'Y'                                                    
         BNE   WPWC900             TEST RUN ONLY. DON'T MARK FILE               
*                                                                               
         NI    RPWCCNTL,X'80'      MARK RECORD FOR DELETE                       
         BAS   RE,PFROI                                                         
*                                                                               
         MVC   KEY(L'RPWCKEY),RPWCKEY                                           
         GOTO1 HDROI                                                            
         TM    P3,X'10'                                                         
         BZ    *+6                                                              
         DC    H'0'                KEY DISAPPEARED?                             
*                                                                               
         NI    KEY+L'RPWCKEY,X'80' DELETE KEY                                   
         BAS   RE,WDROI                                                         
*                                                                               
WPWC900  EQU   *                   RE-POINT AIOAREA                             
         LA    RF,IOAREA                                                        
         ST    RF,AIOAREA                                                       
         XIT1                                                                   
         TITLE 'UPDATE ATHENA RECORDS'                                          
*                                                                               
*- UPATHENA -- UPDATE ATHENA RECORDS FROM SORT RECORD                           
*                                                                               
UPATHENA NTR1                                                                   
         MVC   STATMSG,=CL20'UPATHENA'                                          
         SR    R0,R0               GOOD RETURN CODE                             
         B     EXIT                                                             
*                                                                               
*                                                                               
*- MARKINDX -- DELETE INDCIES USED IN THIS RUN                                  
*                                                                               
*  IF REALLY MARKING THE FILE, SET EACH INDEX USED TO KEEP STATUS               
*                                                                               
*                                                                               
MARKINDX NTR1                                                                   
         MVC   STATMSG,=CL20'MARKINDX'                                          
*                                                                               
         GOTO1 VPRINTER                                                         
         CLI   MARKFILE,C'Y'                                                    
         BE    MRKNDX10                                                         
*                                                                               
         MVC   P(L'XMRKNDX),XMRKNDX   SOFT--NO MARKING                          
         B     MRKNDX15                                                         
*                                                                               
MRKNDX10 MVC   P(L'MSGMARKX),MSGMARKX                                           
MRKNDX15 GOTO1 VPRINTER                                                         
*                                                                               
         LA    R2,INDEXTBL                                                      
MRKNDX20 CLI   0(R2),0                                                          
         BE    MRKNDXOK            END OF INDEX LIST                            
*                                                                               
*- READ EXACT INDEX TO UPDATE INTERNAL DMGR POINTER.                            
         GOTO1 GETINDEX,P1,(2,(R2))                                             
         BZ    *+6                                                              
         DC    H'0'                WHERE DID THE INDEX GO?                      
*                                                                               
         MVC   P(L'KEEPINDX),KEEPINDX                                           
         GOTO1 PRINTNDX,P1,(R2),P+L'KEEPINDX+1                                  
*                                                                               
         CLI   MARKFILE,C'Y'       DO EVERYTHING BUT THE WRITE                  
         BNE   MRKNDX30                                                         
*                                                                               
         GOTO1 DATAMGR,P1,=C'KEEP',=C'WKFILE',(R2),IOAREA,AWRKRB                
*                                                                               
MRKNDX30 LA    R2,NDXLEN(R2)       NEXT INDEX                                   
         B     MRKNDX20                                                         
*                                                                               
MRKNDXOK SR    R0,R0               GOOD RETURN CODE                             
         B     EXIT                                                             
         TITLE 'PRINT OUT END OF RUN STATS'                                     
*                                                                               
*- RUNSTAT -- PRINT RUN STATS                                                   
*                                                                               
RUNSTAT  NTR1                                                                   
         GOTO1 VPRINTER                                                         
         MVC   P(L'GOODRUN),GOODRUN                                             
         GOTO1 VPRINTER                                                         
         MVC   STATMSG,=CL20'RUNSTAT'                                           
         GOTO1 VPRINTER                                                         
*                                                                               
*- NUMBER OF RECORDS SORTED                                                     
         EDIT  (P4,NUMSORT),(10,P),COMMAS=YES,ALIGN=LEFT                        
         LA    RF,P                                                             
RSTAT020 LA    RF,1(RF)            FIND 1ST BLANK AFTER NUMBER                  
         CLI   0(RF),C' '                                                       
         BE    RSTAT030                                                         
         CLI   0(RF),0                                                          
         BNE   RSTAT020                                                         
*                                                                               
RSTAT030 EQU   *                                                                
         LA    RF,1(RF)            LEAVE 1 BLANK AFTER NUMBER                   
         MVC   0(L'MSGSORT,RF),MSGSORT                                          
         GOTO1 VPRINTER                                                         
*                                                                               
*- PWC STATS                                                                    
         CLI   PWC,C'Y'                                                         
         BNE   RSTAT100                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(L'MSGPWC),MSGPWC                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         EDIT  (4,PWCADD),(10,P+2),COMMAS=YES                                   
         MVC   P+13(L'MSGPWCA),MSGPWCA                                          
         GOTO1 VPRINTER                                                         
*                                                                               
         EDIT  (4,PWCCHA),(10,P+2),COMMAS=YES                                   
         MVC   P+13(L'MSGPWCC),MSGPWCC                                          
         GOTO1 VPRINTER                                                         
*                                                                               
         EDIT  (4,PWCDEL),(10,P+2),COMMAS=YES                                   
         MVC   P+13(L'MSGPWCD),MSGPWCD                                          
         GOTO1 VPRINTER                                                         
*                                                                               
*- ATHENA STATS                                                                 
RSTAT100 CLI   ATHENA,C'Y'                                                      
         BNE   RSTAT200                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P(L'MSGATH),MSGATH                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         EDIT  (4,ATHADD),(10,P+2),COMMAS=YES                                   
         MVC   P+13(L'MSGATHA),MSGATHA                                          
         GOTO1 VPRINTER                                                         
*                                                                               
         EDIT  (4,ATHCHA),(10,P+2),COMMAS=YES                                   
         MVC   P+13(L'MSGATHC),MSGATHC                                          
         GOTO1 VPRINTER                                                         
*                                                                               
         EDIT  (4,ATHDEL),(10,P+2),COMMAS=YES                                   
         MVC   P+13(L'MSGATHD),MSGATHD                                          
         GOTO1 VPRINTER                                                         
*                                                                               
RSTAT200 EQU   *                                                                
         SR    R0,R0               GOOD RETURN CODE                             
         B     EXIT                                                             
*                                                                               
         TITLE 'RELWEEK -- DETERMINE # WEEKS BETWEEN 2 DATES'                   
*                                                                               
*- RELWEEK -- DETERMINE RELATIVE NUMBER OF WEEKS BETWEEN                        
*             2 DATES.                                                          
*                                                                               
*  INPUT:  P1 = A(DATE 1 - 6 BYTE INPUT DATE, YYMMDD)                           
*          P2 = A(DATE 2 - 6 BYTE INPUT DATE, YYMMDD)                           
*          P3 = A(RELWEEK WORK AREA)                                            
*          P4-P6 = UNUSED.  MUST BE X'0'                                        
*                                                                               
* OUTPUT:  P1 = NUMBER OF WEEKS BETWEEN DATES                                   
*                                                                               
RELWEEK  NTR1                                                                   
         MVC   STATMSG,=CL20'RELWEEK'                                           
         L     R5,0(R1)            A(DATE 1)                                    
         L     R6,4(R1)            A(DATE 2)                                    
         L     R7,8(R1)            A(RELWEEK WORK AREA)                         
         USING RELWEEKD,R7                                                      
*                                                                               
*- PICK UP INPUT DATES (6 BYTE YYMMDD FORMAT)                                   
         MVC   RWDATE1,0(R5)                                                    
         MVC   RWDATE2,0(R6)                                                    
*                                                                               
*- FIND MONDAY DATE FOR BOTH INPUT DATES                                        
         GOTO1 =V(GETDAY),P1,RWDATE1,P2                                         
         LA    RE,1                                                             
         ZIC   RF,P1               RETURNED DAY OF WEEK                         
         SR    RE,RF                                                            
         ST    RE,P3               NUMBER OF DAYS TO BACKUP                     
         GOTO1 =V(ADDAY),P1,RWDATE1,RWDATE1                                     
*                                                                               
RW100    EQU   *                                                                
         GOTO1 =V(GETDAY),P1,RWDATE2,P2                                         
         LA    RE,1                                                             
         ZIC   RF,P1               RETURNED DAY OF WEEK                         
         SR    RE,RF                                                            
         ST    RE,P3               NUMBER OF DAYS TO BACKUP                     
         GOTO1 =V(ADDAY),P1,RWDATE2,RWDATE2                                     
*                                                                               
RW120    EQU   *                                                                
*                                                                               
         XC    RWDAYS,RWDAYS       RUNNING DAY COUNTER                          
*                                                                               
*- GET JULIAN DATE 1. (FULL PACKED JULIAN)                                      
RW200    EQU   *                                                                
         GOTO1 DATCON,DMCB,(0,RWDATE1),(15,RWJDATE1),0                          
*                                                                               
*- DATES 1 AND 2 IN SAME YEAR?                                                  
         CLC   RWDATE1(2),RWDATE2                                               
         BE    RW300                                                            
*                                                                               
*- FIND JULIAN DATE FOR DEC 31, CURRENT YEAR.                                   
         MVC   RWDATE1+2(4),=C'1231'                                            
         GOTO1 DATCON,DMCB,(0,RWDATE1),(15,RWWORK),0                            
*                                                                               
*- CONVERT PACKED JULIAN DAY TO BINARY. (STRIP CENTRURY, YEAR)                  
         XC    DUB,DUB                                                          
         MVC   DUB+6(2),RWWORK+2   DEC31/XX DAY                                 
         CVB   R1,DUB                                                           
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB+6(2),RWJDATE1+2  DATE 1 DAY                                  
         CVB   R2,DUB                                                           
*                                                                               
*- FIND NUMBER OF DAYS REMAINING IN DATE1 YEAR AND ADD TO RUNNING COUNT         
         SR    R1,R2                                                            
         LA    R1,1(R1)            # DAYS UNTIL END OF YEAR                     
         A     R1,RWDAYS           + RUNNING DAY COUNT                          
         ST    R1,RWDAYS                                                        
*                                                                               
*- SET DATE 1 TO JAN 1, NEXT YEAR. (CURRENTLY DEC31)                            
         GOTO1 =V(ADDAY),DMCB,RWDATE1,RWDATE1,1                                 
         B     RW200                                                            
*                                                                               
*- GET JULIAN DATE 2. (FULL PACKED JULIAN)                                      
RW300    EQU   *                                                                
         GOTO1 =V(DATCON),DMCB,(0,RWDATE2),(15,RWJDATE2),0                      
*                                                                               
*- CONVERT PACKED JULIAN DAY TO BINARY. (STRIP CENTRURY, YEAR)                  
         XC    DUB,DUB                                                          
         MVC   DUB+6(2),RWJDATE2+2  DATE 2 DAY                                  
         CVB   R1,DUB                                                           
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB+6(2),RWJDATE1+2  DATE 1 DAY                                  
         CVB   R2,DUB                                                           
*                                                                               
*- FIND NUMBER OF DAYS BETWEEN DATES 1 AND 2.                                   
*  ADD IN DAYS FROM PRIOR YEARS (RUNNING DAY COUNT)                             
         SR    R1,R2                                                            
         LA    R1,1(R1)            # DAYS UNTIL END OF YEAR                     
         A     R1,RWDAYS           + RUNNING DAY COUNT                          
*                                                                               
*- DIVIDE TOTAL NUMBER OF DAYS BETWEEN DATES BY 7.                              
*  THIS IS NUMBER OF WEEKS (THE POINT OF THIS WHOLE MESS)                       
         SR    R0,R0                                                            
         LA    R2,7                                                             
         DR    R0,R2                                                            
         ST    R1,P1               PASS BACK NUMBER OF WEEKS                    
*                                                                               
         DROP  R7                                                               
         XIT1                                                                   
         TITLE 'MISC SUBROUTINES'                                               
*                                                                               
*- PWOS -- CONVERT 2 BYTE EBCDIC INPUT INTO 1 BYTE PWOS OUTPUT.                 
*                                                                               
*  INPUT:                                                                       
*        P1 = A(2 BYTE NUMERIC EBCDIC INPUT)                                    
*        P2 = A(1 BYTE PWOS OUTPUT)                                             
*                                                                               
PWOS     NTR1                                                                   
         L     R2,0(R1)            A(INPUT)                                     
         L     R3,4(R1)            A(OUTPUT)                                    
         PACK  8(4,R1),0(2,R2)     PACK INPUT INTO P3.                          
         L     RF,8(R1)                                                         
         SRL   RF,4                BAG THE SIGN NIBBLE                          
         STC   RF,0(R3)                                                         
         XIT1                                                                   
         SPACE 2                                                                
*                                                                               
*- UNPWOS -- CONVERT 1 BYTE PWOS INPUT INTO 2 BYTE EBCDIC OUTPUT.               
*                                                                               
*  INPUT:                                                                       
*        P1 = A(1 BYTE PWOS INPUT)           * ASSUMED POSITIVE *               
*        P2 = A(2 BYTE NUMERIC EBCDIC OUTPUT)                                   
*                                                                               
UNPWOS   NTR1                                                                   
         L     R2,0(R1)            A(INPUT)                                     
         L     R3,4(R1)            A(OUTPUT)                                    
         ZIC   RF,0(R2)                                                         
         SLL   RF,4                MAKE ROOM FOR THE SIGN                       
         LA    RE,12               POSITIVE SIGN (X'C')                         
         OR    RF,RE                                                            
         ST    RF,8(R1)            SAVE PACKED INPUT IN P3.                     
         UNPK  0(2,R3),8(4,R1)     UNPACK TO OUTPUT AREA.                       
         OI    1(R3),X'F0'         CONVERT SIGN TO EBCDIC                       
         XIT1                                                                   
         TITLE 'GET A PARTICULAR INDEX'                                         
*                                                                               
*- GETINDEX -- GET A PARTICULAR INDEX                                           
*                                                                               
*  P1 = BYTE 1 - MODE: 1 = GET INDEX FOR GIVEN DATE                             
*                          (BYTES 2-4 = A(YYMMDD DATE)                          
*                                                                               
*                      2 = LOOK FOR EXACT INDEX                                 
*                          (BYTES 2-4 = A(16 BYTE INDEX)                        
*                                                                               
*       BYTES 2-4 = A(INDEX INPUT) - SEE BYTE 1                                 
*                                                                               
*  RETURN: P2 = BYTE 1 = NUMBER INDICES FOUND (N/D FOR MODE 2)                  
*                                                                               
*  CC: 0 = INDEX FOUND.  NON-0 = INDEX NOT FOUND.                               
*                                                                               
*  ONLY INDICES ON ACTIVE, UNKEEP STATUS ARE RETURNED.                          
*                                                                               
GETINDEX NTR1                                                                   
         MVC   STATMSG,=CL20'GETINDEX'                                          
         ZIC   R2,0(R1)            MODE. (1 OR 2)                               
         BCTR  R2,0                MODE = 0 OR 1                                
         L     R3,0(R1)            A(INPUT)                                     
         XC    IOAREA(200),IOAREA                                               
         SR    R6,R6               COUNT # INDICES HERE                         
*                                                                               
         MVC   KEYSAVE(16),0(R3)   ASSUME MODE 2                                
         LA    R4,15               COMPARE LEN, LESS 1                          
         LTR   R2,R2                                                            
         BNZ   GINDX100            B = EXACT HIT REQUIRED                       
*                                                                               
*- BUILD HI-ORDER INDEX NAME.                                                   
*   +0    'SJ' (USER)                                                           
*   +2    'R'  (SYSTEM = REP)                                                   
*   +3    'OI' (PROGRAM = OFFLINE INFO)                                         
*   +5   X'0'  (SUBPROGRAM)                                                     
*   +6   X'.'  PWOS DAY OF RUN (DD VALUE FROM RUN DATE)                         
*   +7    'R'  (CLASS REP)                                                      
*                                                                               
         XC    INDEX,INDEX         READ 1ST INDEX                               
         MVC   INDEX(5),=C'SJROI'                                               
         MVI   INDEX+5,0                                                        
         LA    RF,4(R3)            A(DAY OF MONTH)                              
         ST    RF,P1                                                            
         GOTO1 PWOS,P1,,INDEX+6                                                 
         MVI   INDEX+7,C'R'                                                     
*                                                                               
         LA    R4,7                COMPARE LEN, LESS 1                          
*                                                                               
         MVC   KEYSAVE(16),INDEX                                                
*                                                                               
GINDX100 XC    INDEX,INDEX                                                      
*                                                                               
         MVC   P(17),=C'LOOKING FOR INDEX'                                      
         GOTO1 PRINTNDX,P1,KEYSAVE,P+20                                         
*                                                                               
*- READ ALL INDICES UNTIL MATCH FOUND OR END OF FILE.                           
*  ONLY ACCEPT ACTIVE, UNKEEP INDICES.                                          
         MVC   STATMSG,=CL20'INDEX READ'                                        
         LA    R7,INDEX                                                         
         USING UKRECD,R7                                                        
*                                                                               
GINDX200 EQU   *                                                                
         GOTO1 DATAMGR,DMCB,=C'INDEX',=C'WKFILE',INDEX,IOAREA,AWRKRB            
*                                                                               
         TM    P3,X'80'            END OF INDEX                                 
         BO    GINDX900                                                         
*                                                                               
         CLI   P3,0                                                             
         BE    *+6                                                              
         DC    H'0'                DISK ERROR ON READ                           
*                                                                               
***      MVC   P+2(15),=C'INDEX ON RETURN'                                      
***      GOTO1 =V(HEXOUT),P1,INDEX,P+40,16                                      
***      GOTO1 =V(HEXOUT),P1,IOAREA,P+80,16                                     
***      GOTO1 VPRINTER                                                         
*                                                                               
         CLI   ALLINDEX,C'Y'       IGNORE STATUS TEST                           
         BE    GINDX210                                                         
*                                                                               
         TM    UKSTAT,X'80'        ACTIVE STATUS?                               
         BZ    GINDX200            NO.  REJECT.                                 
*                                                                               
         TM    UKSTAT,X'08'        IN KEEP STATUS?                              
         BO    GINDX200            YES.  REJECT.                                
*                                                                               
GINDX210 EX    R4,GINDXCMP         = THRU BREAK LEN?                            
         BNE   GINDX200                                                         
*                                                                               
*- IF WE ARE 'EXACT HIT' MODE, EXIT NOW                                         
*  ELSE LOOP BACK TO COUNT NUMBER OF MATCHING ENTRIES                           
         LTR   R2,R2                                                            
         BNZ   GINDXOK                                                          
*                                                                               
         LTR   R6,R6                                                            
         BNZ   GINDX220                                                         
         MVC   KEYSAVE(16),INDEX    SAVE 1ST INDEX                              
*                                                                               
GINDX220 LA    R6,1(R6)            COUNT MATCHING INDICES                       
         B     GINDX200                                                         
*                                                                               
*- END OF ALL INDICES. ERROR IF NONE FOUND                                      
GINDX900 EQU   *                                                                
         MVC   INDEX(16),KEYSAVE                                                
         STC   R6,P2               PASS BACK COUNT                              
         LTR   R6,R6                                                            
         BZ    GINDXERR                                                         
*                                                                               
*- INDEX FOUND.  RETURN 0 CC.                                                   
GINDXOK  SR    R0,R0                                                            
         B     GINDXEXT                                                         
*                                                                               
*- INDEX NOT FOUND.  RETURN NON-0 CC.                                           
GINDXERR LTR   RD,RD                                                            
GINDXEXT B     EXIT                                                             
         DROP  R7                                                               
         SPACE                                                                  
GINDXCMP CLC   INDEX(0),KEYSAVE                                                 
         SPACE 2                                                                
*                                                                               
*- PRINTNDX -- FORMAT WORKER INDEX FOR PRINTING                                 
*  INPUT:  P1 = A(INPUT INDEX)                                                  
*          P2 = A(OUTPUT AREA)                                                  
PRINTNDX NTR1                                                                   
         L     R2,0(R1)            A(INPUT)                                     
         L     R3,4(R1)            A(OUTPUT)                                    
*                                                                               
         MVC   0(6,R3),0(R2)       USER, SYSTEM, PGM, SUB-PGM                   
*                                                                               
         LA    R3,6(R3)                                                         
         LA    R2,6(R2)                                                         
*                                                                               
         ST    R2,0(R1)                                                         
         ST    R3,4(R1)                                                         
         BAS   RE,UNPWOS           DAY OF MONTH                                 
*                                                                               
         MVC   2(1,R3),1(R2)       CLASS                                        
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         TITLE 'DATA MANAGER INTERFACE -- INCLUDE CODE'                         
       ++INCLUDE RGENROI                                                        
         TITLE 'MESSAGES'                                                       
*                                                                               
*- MESSAGES AND OTHER STUFF PRINTED ON REPORT                                   
         SPACE 1                                                                
ERRMSG   DC    C'*** ERRORS FOUND DURING RUN!  RUN ABORTED! ***'                
*                                                                               
TTL1     DC    C'REP OFFLINE INFORMATION SYSTEM - UPDATE'                       
*                                                                               
TTL2     DC    C'WEEKEND FILE UPDATE'                                           
*                                                                               
BADDATE  DC    C'  ** INVALID DATE. FORMAT IS ''DATE=MMMDD/YY'' **'             
*                                                                               
YORN     DC    C'  ** CARD REQUIRES A ''Y'' OR ''N'' RESPONSE'                  
*                                                                               
ONLY1    DC    C'  ** DUPLICATE CONTROL CARDS NOT ALLOWED **'                   
*                                                                               
MISSING  DC    C'** MISSING CONTROL CARD:'                                      
*                                                                               
OKCARDS  DC    C'* CONTROL CARDS PASS VALIDATION *'                             
*                                                                               
BADCARDS DC    C'** CONTROL CARDS ARE INCORRECT **'                             
*                                                                               
INDXGONE DC    C'* WARNING: EXPECTED INDEX NOT FOUND - '                        
*                                                                               
MULTINDX DC    C'** MULTIPLE INDICES FOR SAME DATE **  '                        
*                                                                               
DOWRKR   DC    C'* BEGIN WORKER FILE PROCESS *'                                 
*                                                                               
DOUPDAT  DC    C'* BEGIN ROI UPDATE *'                                          
*                                                                               
MSGMARKX DC    C'* MARKING INDICES USED FOR THIS RUN *'                         
*                                                                               
XMRKNDX  DC    C'* SOFT RUN -- INDICES REMAIN ON FILE *'                        
*                                                                               
GOODRUN  DC    C'* UPDATES ARE COMPLETE *'                                      
*                                                                               
MSGSORT  DC    C'RECORDS SORTED IN THIS RUN'                                    
*                                                                               
MSGPWC   DC    C'PAPERWORK COUNTING DETAILS'                                    
*                                                                               
MSGPWCA  DC    C'PWC RECORDS ADDED'                                             
*                                                                               
MSGPWCC  DC    C'PWC RECORDS CHANGED'                                           
*                                                                               
MSGPWCD  DC    C'PWC RECORDS DELETED'                                           
*                                                                               
MSGATH   DC    C'ATHENA DETAILS'                                                
*                                                                               
MSGATHA  DC    C'ATHENA RECORDS ADDED'                                          
*                                                                               
MSGATHC  DC    C'ATHENA RECORDS CHANGED'                                        
*                                                                               
MSGATHD  DC    C'ATHENA RECORDS DELETED'                                        
*                                                                               
KEEPINDX DC    C'SETTING ''KEEP'' STATUS ON INDEX:'                             
         DS    0H                  ALIGNMENT                                    
         TITLE 'LITERALS AND CONSTANTS'                                         
         LTORG                                                                  
         SPACE 2                                                                
DAYLITS  DS    0C                                                               
         DC    CL16'(MONDAY)'                                                   
         DC    CL16'(TUESDAY)'                                                  
         DC    CL16'(WEDNESDAY)'                                                
         DC    CL16'(THURSDAY)'                                                 
         DC    CL16'(FRIDAY)'                                                   
         DC    CL16'(SATURDAY)'                                                 
         DC    CL16'(SUNDAY)'                                                   
         SPACE 2                                                                
FLIST    DS    0H                  DATAMGR FILE LIST                            
         DC    CL8'UROIFILE'                                                    
         DC    CL8' ROIDIR '                                                    
         DC    CL8'X       '                                                    
         SPACE 2                                                                
PWCTYPE  DC    CL2'10'             SORT REC TYPE: PWC                           
*                                                                               
ATHTYPE  DC    CL2'20'             ATHENA                                       
         SPACE 2                                                                
SRTFLD   DC    CL80'SORT FIELDS=(4,30,BI,A),WORK=1 '                            
*                                                                               
RECTYP   DC    CL80'RECORD TYPE=V,LENGTH=(2048) '                               
*ECTYP   DC    CL80'RECORD TYPE=V,LENGTH=(1100) '                               
*                                  SORT RECORD MAX SIZE CHANGED..               
*                                                                               
         DS    0F                                                               
         TITLE 'WORK AREA'                                                      
         DS    0F                                                               
WORKD    EQU   *                                                                
         DC    C'DMCB'                                                          
DMCB     DS    0CL24               PARAMETER LIST                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
         DC    CL4'DUB*'                                                        
DUB      DS    D                                                                
DOUBLE   DS    D                                                                
         DC    C'WORK'                                                          
WORK     DS    CL64                                                             
         DC    C'FULL'                                                          
FULL     DS    F                                                                
FULL2    DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
THREE    DS    CL3                                                              
BYTE     DS    CL1                                                              
         SPACE 1                                                                
* FILE HANDLING AREAS *                                                         
         SPACE 1                                                                
         DC    C'KEYS'                                                          
ROIKEY   DS    0CL36                                                            
KEY      DS    CL36                KEY                                          
ROIKEYSV DS    0CL36                                                            
KEYSAVE  DS    CL36                KEY SAVED BEFORE READ HIGH                   
DMWORK   DS    CL96                                                             
         SPACE 2                                                                
DMREAD   DC    CL8'DMREAD'         COMMANDS                                     
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMADD    DC    CL8'DMADD'                                                       
DMWRT    DC    CL8'DMWRT'                                                       
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
         SPACE 2                                                                
LASTFILE DS    F                   BETWEEN I/O CONTROLS                         
LASTDA   DS    F                                                                
LASTIO   DS    F                                                                
LASTLEN  DS    H                                                                
LASTKYST DS    H                                                                
DATADISP DS    H                   DISPLACEMENT TO FIRST ELEMENT                
DMINBTS  DS    CL1                 SET TO X'80'                                 
         EJECT                                                                  
* ADDRESS LIST *                                                                
         DC    0F'0'                                                            
         DC    C'ADDR'                                                          
DATAMGR  DC    V(DATAMGR)                                                       
PRINT    DC    V(PRINT)                                                         
DATCON   DC    V(DATCON)                                                        
DWRKRB   DC    A(MY4KBUF)          WORKER BUFFER                                
VPRINTER DC    V(PRINTER)                                                       
AWRKRB   DC    A(MY4KBUF)                                                       
ASORTREC DS    A                   A(SORTED RECORD)                             
         SPACE                                                                  
NUMSORT  DC    PL4'0'                                                           
         SPACE 2                                                                
         DC    C'SWITCHES*'                                                     
CTLDATE  DS    CL1                 CONTROL CARD PROCESS FLDS                    
CTLWRITE DS    CL1                 ('Y' = PROCESSED)                            
CTLPWC   DS    CL1                                                              
CTLATHEN DS    CL1                                                              
*                                                                               
MARKFILE DS    CL1                 C'Y' = UPDATE FILE. 'N'=REPORT ONLY          
PWC      DS    CL1                 C'Y' = UPDATE PWC RECORDS                    
ATHENA   DS    CL1                 C'Y' = UPDATE ATHENA RECORDS                 
ALLINDEX DC    C'N'                C'Y' = RE-USE MARKED INDICES                 
*                                                                               
FIRSTSW  DS    CL1                 C'Y' IN 1ST PASS.                            
*                                                                               
FULLWEEK DS    CL1                 C'Y' = DO ALL INDICES FOR WEEK.              
*                                  C'N' = SINGLE DAY INDEX ONLY                 
*                                                                               
DELPWC   DS    CL1                 DELETE PWC INSTEAD OF WRITING BACK           
*                                                                               
STOP     DS    CL1                 STOP RUN IF 'Y'                              
RETCODE  DS    CL1                 RETURN CODE VALUE                            
*                                                                               
RUNDATE  DS    0CL6                TODAY IF FULLWEEK=Y                          
RUNYY    DS    CL2                 OR DAY FROM DATE CARD.                       
RUNMM    DS    CL2                                                              
RUNDD    DS    CL2                                                              
*                                                                               
WEEKOF   DS    CL6                 MONDAY START DATE                            
TODAY    DS    CL6                 CURRENT SYSTEM DATE                          
CURINDX  DS    CL6                 DATE ASSOCIATED WITH CURRENT INDEX           
*                                                                               
START    DS    CL6                 YYMMDD START DATE                            
END      DS    CL6                 YYMMDD END DATE                              
*                                                                               
WORKDATE DS    CL6                 USED BY FINDINDX                             
*                                                                               
SAVEDATE DS    CL3                 BINARY TAPE DATE.                            
*                                                                               
NUMPWC   DC    PL4'0'              # PWC RECS RELEASED TO SORT                  
NUMATHEN DC    PL4'0'                                                           
*                                                                               
WEEKDISP DS    F                   WEEK ACTIVITY DISPLACEMENT                   
*                                                                               
COMMAND  DS    CL8                 COMMAND FOR RGENIO                           
AIOAREA  DS    A                   FOR RGENIO                                   
         SPACE                                                                  
SAVEKEY  DS    CL8                 PWC SAVED SORT KEY                           
SAVEID   DS    CL1                 PWC SAVED KEY ID                             
OLDREC   DS    CL1                 PWC OLD/NEW RECORD FLAGS                     
NEWREC   DS    CL1                                                              
SAVEADDR DS    A                   PWC SAVED A(ELEMENT)                         
*                                                                               
PWCNUM   DS    F                   # SORT RECS IN PWC REC                       
PWCADD   DC    F'0'                # PWC RECS ADDED                             
PWCCHA   DC    F'0'                # PWC RECS CHANGED                           
PWCDEL   DC    F'0'                # PWC KEYS DELETED                           
         SPACE 2                                                                
ATHADD   DC    F'0'                # ATHENA RECS ADDED                          
ATHCHA   DC    F'0'                # ATHENA RECS CHANGED                        
ATHDEL   DC    F'0'                # ATHENA KEYS DELETED                        
         SPACE 2                                                                
*                                                                               
*- WORKER FILE STUFF                                                            
         DC    C'INDEX*'                                                        
INDEX    DS    CL16                                                             
         SPACE                                                                  
NDXNDX   EQU   0                   16 BYTE INDEX                                
NDXDATE  EQU   16                  YYMMDD DATE FOR THIS INDEX                   
*                                                                               
NDXLEN   EQU   22                  ENTRY LEN                                    
NDXSIZE  EQU   (16*NDXLEN)+2       TABLE SIZE                                   
*                                                                               
INDEXTBL DC    (NDXSIZE)X'00'                                                   
LNDEXTBL EQU   *-INDEXTBL                                                       
         SPACE 2                                                                
         DC    C'IOAREA*'                                                       
*                                                                               
IOAREA   DS    2000X                                                            
         ORG   IOAREA                                                           
       ++INCLUDE REGENPWC          PWC RECORD DSECT                             
         ORG   IOAREA+2000                                                      
         DC    C'SORTREC*'                                                      
         SPACE                                                                  
         DS    0F                                                               
SORTREC  DS    3000X                                                            
         DC    C'4KBUF*'                                                        
MY4KBUF  DS    4096X               4K WORKER BUFFER                             
         SPACE                                                                  
         ENTRY UTL                 FOR DATAMGR                                  
UTL      DC    F'0',X'08'          REP FILE                                     
         SPACE                                                                  
         ENTRY SSB                 TURN OFF ROI FILE RECOVERY                   
SSB      DC    F'2'                                                             
*                                                                               
         SPACE 2                                                                
SORTRECD DSECT                                                                  
         DS    F                   REC LEN/CONTROL BYTE                         
SORTKEY  DS    0CL30                                                            
SRTKTYPE DS    CL2                 RECORD TYPE. '10'=PWC, '20'=ATHENA           
SRTKDATA DS    CL28                SORT KEY DATA                                
LSORTKEY EQU   *-SORTKEY                                                        
*                                                                               
*- PWC SORT KEY                                                                 
         ORG   SRTKDATA                                                         
SRTPREP  DS    CL2                 REP CODE                                     
SRTPCON  DS    CL4                 CONTRACT NUMBER                              
SRTPID   DS    CL1                 SORT REC ID                                  
*                                                                               
*- ATHENA SORT KEY  (TO BE DETERMINED)                                          
*                                                                               
         ORG   SRTKDATA                                                         
SRTADATA DS    CL28                                                             
*                                                                               
*- SORT RECORD DATA (VARIABLE WORKER RECORD)                                    
         ORG   SORTKEY+LSORTKEY                                                 
WRKREC   DS    0C                                                               
         DS    XL4                 REC LEN/BYTE OF 0                            
         SPACE 2                                                                
       ++INCLUDE DMRCVRHDR                                                      
RKEY     DS    CL27                                                             
         DS    2050C                                                            
       ++INCLUDE REDPRINTD                                                      
         SPACE 2                                                                
       ++INCLUDE DMWRKRK                                                        
         SPACE 2                                                                
       ++INCLUDE DMWRKRD                                                        
*                                                                               
*- RELWEEK WORK DSECT                                                           
RELWEEKD DSECT                                                                  
RWDATE1  DS    CL6                 DATE 1, 6 BYTE EBCDIC                        
RWDATE2  DS    CL6                 DATE 2, 6 BYTE EBCDIC                        
RWWORK   DS    CL6                 WORK AREA                                    
*                                                                               
RWJDATE1 DS    CL4                 DATE 1 JULIAN                                
RWJDATE2 DS    CL4                 DATE 2 JULIAN                                
RWDAYS   DS    F                                                                
*                                                                               
RWDLEN   EQU   *-RELWEEKD          LENGTH OF RELWEEK DSECT                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'214REROIUP0S 05/01/02'                                      
         END                                                                    
