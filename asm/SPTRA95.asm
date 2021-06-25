*          DATA SET SPTRA95    AT LEVEL 101 AS OF 09/11/19                      
*PHASE T21695C                                                                  
*INCLUDE OFFOUT                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE PRTREC                                                                 
*                                                                               
*************************************************************                   
*                                                                     *         
*   THIS PROGRAM PURGES EXPIRED PROGRAM EQUIVALENCY RECORDS           *         
*   AND SKED UNITS  BASED ON DATE.                                    *         
*   THE REVISION AND PATTERN RECORDS ARE PURGED IF THERE ARE NO       *         
*   UNIT RECORDS FOR THEM AND THE  DATE IS PRIOR TO THE EXPIRATION    *         
*   DATE. COMMERCIAL RECORDS ARE PURGED ONLY IF THERE ARE NO PATTERN  *         
*   RECORDS USING THESE CMLS.                                         *         
*   ANY COMMERCIAL IN AN INVOICE RECORD CANNOT BE PURGED                        
*                                                                     *         
*   WILL ONLY RUN OFFLINE, AS IT READS AND WRITES TOO MANY RECORDS    *         
*   FOR ONLINE.                                                       *         
*                                                                     *         
*   IT NOW CHECKS FOR ANY TRAFFIC RECORDS THAT HAVE NO CLIENT HEADER  *         
*   RECORD                                                            *         
*                                                                     *         
*              UNIT FILE                                              *         
*              ALSO READ 21 REVISION                 SPTRNREV         *         
*                        23 PATTERN                  SPTRNPAT         *         
*                        24 PROGRAM EQUIVALENT       SPTRNEQPRG       *         
*                        26 NETWORK FAX RECORD       SPTRNFAX         *         
*                        27 NETWORK TRAFFIC SUPPLIER SPTRNTSUPP       *         
*                                                                     *         
*              SPOT FILE                                              *         
*                        0A2B FEED RECORD            SPTRNFEED        *         
*                        0A41 CLIENT DIST LIST       SPTRNCLT         *         
*                        0A42 PRODUCT DIST LIST      SPTRNPRD         *         
*                                                                     *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO1 - UNIT RECORD                                      *         
*             AIO2 - VOFF, FCLT, FCLEN, FCDTE (CML)                   *         
*             AIO3 - READ IN COMMERCIAL TEXT RECS FOR COMML           *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR       *         
*        R3 - WORK REG - HOLDS PAT REF # IN UNTPAT                    *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - WORK REG - IN APL ACTIVE PATTERN LIST PTR               *         
*                      - POINTS TO CML                                *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 - SECOND BASE                                             *         
*        R8 - POINTER TO SPOOLD                                       *         
*        R9 - POINTER TO SYSD                                         *         
*        RA - POINTER TO ATWA                                         *         
*        RB - FIRST BASE                                              *         
*        RC - POINTER TO GEND                                         *         
*        RD - SAVE AREA POINTER                                       *         
*        RE - GOTO1 REG                                               *         
*        RF - GOTO1 REG                                               *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
* LEV   5  DEC29/95 MOVED GETMAIN                                     *         
* LEV   7  NOV06/96 CHANGE PRINTING FOR OFFICE CODE- SPRI             *         
* LEV   8  JUL16/97 USE DATCON/ADDAY FOR DATE ARITHMETICS (YR 2000)   *         
* LEV   9  DEC08/97 INCREASE TSAR STORAGE                             *         
* LEV  10  FEB19/97 IF CLT SPECIFIC REQ IS FOR EXCLUDED CLT - STOP    *         
* LEV  11  MAR18/98 ADD TRACE                                         *         
* LEV  12  MAR30/98 DEL PAT RECS IF NOT REFERENCED IN THE UNITS AND   *         
*                   IF THE REL DATE IS BEFORE EXP DATE                *         
* LEV  13  DEC07/98 CLEAR TABLES AT START OF RUN, MAKE ACTIVE PROGRAM *         
*                   LIST LONGER - FROM 60,000 TO 80,000               *         
* LEV  14 SMUR NOV10/99 USE RECUP FROM FACPAK                         *         
* LEV  15 SMUR MAY29/01 CHANGE DUMMY,USE RESIDENT GETBROAD            *         
*                       USE TRAFFIC OFFICE                            *         
* LEV  16 SMUR DEC20/01 INCREASE PROGRAM TBLE & CLEAR IT BETWEEN CLTS *         
* LEV  18 SMUR JUL03/03 2 CHAR DAYPART CODE                           *         
* LEV  19 SMUR APR20/04 FIX MEDIA SPECIFIC PATTERN KEY                *         
* LEV  20 SMUR JUN09/04 SKIP PU PROF, PAT SEQ, 253 PRODS              *         
* LEV  21 SMUR MAR04/05 DO NOT PURGE EASI CMLS, CODE FOR AD-ID, CUT-IN*         
*                       PRINT CLIENTS THAT WERE BYPASSED              *         
*                       CODE FOR ACTUAL CML(X'60') FOR THE COVER CML  *         
*                   FIX PROGRAM EQUIVALENCY DELETE PASSIVE KEY LOGIC  *         
* LEV  21 BGRI MAY16/05 CK FOR MISSNG CLIENT TRAFFIC RECS ** DISABLED**         
* LEV  22 SMUR JUN01/06 2 CHAR OFFICE CODE                            *         
* LEV  23 SMUR MAY02/07 MORE BRANDS, KEEP REFERENCE CML FOR ACTIVE CML*         
*                       OPTIMIZE, DO NOT UPDATE F1 ACTIVITY ELEM      *         
* LEV  24 MHER AUG/09   ADID SUPPORT                                  *         
* LEV  90 SMUR JUL/10   FIX DELETING ADID PASSIVE POINTER BUG         *         
* LEV  91 SMUR AUG/10   ADD ONLY VALID TRAFFIC INVOICE COMMERCIALS    *         
* LEV  92 SMUR SEP/10   CHG TSAR KEY FR 2 TO 3 BYTES & INCR TSR BUFFER*         
*                   CHG TSAGET TO TSARDH (GET DOESNT WORK W/3BYTE KEY)*         
* LEV  93 SMUR NOV/10   READ AGENCY LEVEL PU PROFILE NOT USER ID      *         
* LVE  94 MHER MAY/11   PROCESS PATTERN NETWORK LIST RECORDS          *         
* LEV  98 SMUR AUG/12   DELETE HIDEF PASSIVE POINTER WITH CORRECT ID  *         
* LEV  99 SMUR AUG/13   DO GETREC BEFORE PUTREC IN DELETE SKED UNIT   *         
* SPEC-38122  SMUR 9/1/19  ADD CODE TO CLOSEOUT COMTEXT               *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21695 PURGE PAT/COMML/REV/PROGRAM EQUIV RECS'                  
*                                                                               
T21695   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PURG**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,SPTR95RR                                                      
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       NOW PURGE RECORDS                            
         BE    LR                                                               
         B     EXIT                                                             
EQXIT    CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* INITIALIZE TSAROFF                                                            
*                                                                               
INITSROF NTR1                                                                   
         XC    TSAROFF,TSAROFF                                                  
*                                                                               
         LHI   RE,TSARBLK-SYSD                                                  
         AR    RE,R9                                                            
         LR    R2,RE                                                            
         LA    RF,TSARDL2                                                       
         XCEF                                                                   
         USING TSARD,R2                                                         
*                                                                               
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,3                                                         
         MVI   TSRECL+1,15                                                      
         MVC   TSABUF,TSARBUFF                                                  
         MVC   TSAREC,TSARBUFL                                                  
*                                                                               
         OI    TSINDS,TSINODSK                                                  
         OI    TSIND2,TSI2MANY+TSI2BIGN   USE BOTH BUFFERS                      
*                                                                               
* TSAR CALLOV HERE                                                              
*                                                                               
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A7D')                                 
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,TSAROFF                                                       
*                                                                               
         MVI   TSOFFACT,TSAINI      SET 1ST TSAROFF FOR INIT                    
         GOTO1 TSAROFF,(R2)                                                     
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
VK       BRAS  RE,VKEY             GO TO VALIDATE KEY ROUTINE                   
*                                                                               
* INITIALIZE TSAROFF BUFFER ONCE ONLY *                                         
*                                                                               
         L     R0,=F'800000'                                                    
         ST    R0,TSARBUFL                                                      
         GETMAIN RC,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,TSARBUFF                                                      
         B     EXIT                                                             
         EJECT                                                                  
* INITIALIZE *                                                                  
*                                                                               
LR       TM    WHEN,X'18'          OV OR DDS                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ADDREC,ADDREC                                                    
         MVC   FINDBUG,ANY         SAVE ADCONS FOR BUG                          
*                                                                               
         MVC   BBCLT,BCLT          SAVE BCLT IF ANY                             
         MVC   SAVEBCLT,SVBCLT     AND SVBCLT                                   
*                                                                               
         MVI   ACTELOPT,C'N'       DO NOT TO MONITOR F1 ACTIVITY ELEM           
*                                                                               
*NOP     BRAS  RE,FIXMIS           GO FIND ANY MISSING CLIENT RECORDS           
*                                                                               
         MVC   BCLT,BBCLT          RESTORE BCLT                                 
         MVC   SVBCLT,SAVEBCLT     AND SVBCLT                                   
*                                                                               
         LARL  R0,HEADING          HEADING LINE FOR REPORT                      
         ST    R0,SPECS            STORE FOR CONTROLLER                         
         LARL  R0,HDHK                                                          
         ST    R0,HEADHOOK         STORE FOR CONTROLLER                         
         XC    COUNTERS(ENDCTRS-COUNTERS),COUNTERS                              
         XC    AGYCTRS(ENDCTRS-COUNTERS),AGYCTRS                                
*                                                                               
* GET ADDRESS OF TRPACK                                                         
*                                                                               
         XC    DMCB,DMCB                                                        
         LA    R1,DMCB                                                          
         L     RF,CALLOV                                                        
         MVC   DMCB+4(4),=X'D9000AFE'  TRPACK                                   
         GOTO1 (RF),(R1),0                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
* INIT ONLY ONCE EXCLUDE CLT LIST                                               
*                                                                               
         L     R1,VADUMMY                                                       
         LA    RE,8(R1)            SAVE START OF TABLE                          
*                                                                               
         MVC   0(8,R1),=C'XCLIENT*'  EXCLUDED CLIENTS                           
         LA    R1,8(R1)                                                         
         ST    R1,AXCLTBLE                                                      
         AHI   R1,600              200 CLIENTS                                  
         ST    R1,MAXCLTBL                                                      
*                                                                               
         LR    RF,R1               GET END OF TABLES                            
         SR    RF,RE               VDUMMY IN RE, RF = TOTAL LENGTH              
         XCEF                                                                   
*                                                                               
         OC    SVBCLT,SVBCLT                                                    
         BNZ   LR30                                                             
*                                                                               
LR10     CLI   OFFICE,0            BY OFFICE                                    
         BE    LR20                 YES GET NEXT CLIENT FOR OFFICE              
*                                                                               
         BRAS  RE,NOFF                                                          
         BNE   EOJ                                                              
         B     LR30                                                             
*                                                                               
* GET NEXT SEQUENTIAL CLIENT *                                                  
*                                                                               
LR20     DS    0H                                                               
*                                                                               
         BRAS  RE,INITSPT          SET TO SPOT                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVBCLT                                                  
LR24     MVI   KEY+4,X'FF'                                                      
         GOTO1 HIGH                                                             
         MVI   TRCETYP,1           INDICATE HIGH                                
         BAS   RE,TRACE                                                         
*                                                                               
         CLC   KEY(2),KEYSAVE                                                   
         BNE   EOJ                                                              
         OC    KEY+4(9),KEY+4      CLIENT HDR                                   
         BNZ   LR24                                                             
         MVC   SVBCLT,KEY+2                                                     
LR30     BRAS  RE,FCLT                                                          
         BNE   *+18                PU PROFILE SET TO DO THIS CLT                
         OC    BCLT,BCLT           1 CLIENT                                     
         BZ    LR20                 NO, GO GET NEXT CLT                         
         B     EXIT                                                             
*                                                                               
         BRAS  RE,INITNET          SET TO NET                                   
         EJECT                                                                  
         XC    DMCB,DMCB                                                        
         LA    R1,DMCB                                                          
         L     RF,CALLOV                                                        
         MVC   DMCB+4(4),=X'D9000A1D'  GETBROAD                                 
         GOTO1 (RF),(R1),0                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VGTBROAD,0(R1)                                                   
*                                                                               
* SET UP TABLE POINTERS AND MAX TABLE SIZES                                     
*                                                                               
         LA    R1,TABLES                                                        
         ST    R1,STRAPLST         START OF ACTIVE PATTERN LIST                 
         ST    R1,ENDAPLST                                                      
         MVC   0(3,R1),=X'FFFFFF'                                               
         AHI   R1,8192             MAKE PATTN LIST LARGER - WAS 1024            
*                                                                               
         ST    R1,STRACLST                                                      
         ST    R1,ENDACLST                                                      
         MVI   0(R1),X'FF'                                                      
         LR    R0,R9               GET MAX TABLE SIZE BOUNDARY                  
         A     R0,LSYSD                                                         
         ST    R0,MAXACLST                                                      
*                                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BNE   LR34                USE DUMMY AREA                               
*                                                                               
         L     R1,MAXCLTBL         END OF EXCLUDE CLT LIST                      
         LR    RE,R1                                                            
         ST    R1,STRAGLST         START OF ACTIVE PROGRAM LIST                 
         ST    R1,ENDAGLST                                                      
         MVI   0(R1),X'FF'                                                      
*                                                                               
         A     R1,=F'150000'                                                    
         ST    R1,STRAPLST         START OF ACTIVE PATTERN LIST                 
         ST    R1,ENDAPLST                                                      
         MVC   0(3,R1),=X'FFFFFF'                                               
*                                                                               
         A     R1,=F'40000'                                                     
         ST    R1,STRACLST         ACTIVE COMMERCIAL LIST                       
         ST    R1,ENDACLST                                                      
         MVI   0(R1),X'FF'                                                      
*                                                                               
         A     R1,=F'40000'                                                     
         A     R1,=F'40000'                                                     
         ST    R1,STRARLST         TAKE OUT AT A LATER DATE **TEMP**            
         ST    R1,ENDARLST                       *****                          
         ST    R1,MAXTBLSZ         SET MAX TABLE SIZE                           
*                                                                               
         ST    R1,STRLCLST         START OF LOST CML REC LIST                   
         ST    R1,ENDLCLST           AND PRESET END OF LIST                     
         AHI   R1,1600             HOLDS UPTO 200 CMLS                          
         ST    R1,MAXLCSZ                                                       
*                                                                               
         ST    R1,STADILST         START OF LOST AD-ID LIST                     
         ST    R1,ENADILST           AND PRESET END OF LIST                     
         AHI   R1,2400             HOLDS UPTO 200 AD-ID                         
         ST    R1,MAXADSZ                                                       
*                                                                               
         LR    RF,R1               GET END OF TABLES                            
         SR    RF,RE               VDUMMY IN RE, RF = TOTAL LENGTH              
         XCEF                                                                   
*                                                                               
         BAS   RE,INITSROF         INIT TSAROFF                                 
*                                                                               
         EJECT                                                                  
* READ UNIT RECORDS FOR THIS CLIENT *                                           
*                                                                               
LR34     DS    0H                                                               
         CLI   SVPROF+1,C'Y'       BETTER NOT BE BYPASS THIS CLIENT             
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RCSUBPRG,5                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         XC    SVSTDTEP,SVSTDTEP   SAVE START AND                               
         XC    SVENDTEP,SVENDTEP   END UNIT PERIOD IN BINARY                    
         XC    REVDATA,REVDATA     INIT REVISION DATA                           
         XC    REVPER,REVPER       REV PERIOD                                   
         XC    REVBPER,REVBPER     REV PERIOD IN BINARY                         
         MVI   REVNUM,0            REVISION NUMBER FOR THIS PERIOD              
*                                                                               
         BRAS  RE,BLDINV           BUILD LIST OF CMMLS FROM INVRECS             
*                                                                               
         BRAS  RE,INITNET          SET TO READ UNTDIR                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NUKPKEY,R4                                                       
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,BAGYMD                                                    
         MVC   NUKPCLT,SVBCLT                                                   
*                                                                               
         GOTO1 HIGH                                                             
         MVI   TRCETYP,1           INDICATE HIGH                                
         BAS   RE,TRACE                                                         
*                                                                               
         B     UNT20                                                            
*                                                                               
* UNIT DATES ARE AFTER PURGE DATES, JUMP TO NEXT PROGRAM                        
*                                                                               
UNT05    DS    0H                                                               
         MVC   NUKPDATE,=X'FFFF'                                                
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      SAME A/M/CLT                                 
         BNE   UNTX                                                             
         MVI   TRCETYP,1           INDICATE HIGH                                
         B     UNT12                                                            
*                                                                               
UNT10    GOTO1 SEQ                                                              
         MVI   TRCETYP,2           INDICATE SEQ                                 
UNT12    BAS   RE,TRACE                                                         
*                                                                               
UNT20    XC    SVFIELDS,SVFIELDS                                                
*                                                                               
         CLC   KEY(4),KEYSAVE      SAME A/M/CLT                                 
         BNE   UNTX                                                             
*                                                                               
         MVC   SVUNTKEY,KEY        SAVE UNIT KEY                                
*                                                                               
         MVC   SVMEDIA,NUKSTAT     SAVE MEDIA                                   
         MVC   SVNUKNET,NUKPNET         NETWORK                                 
         MVC   SVKPROG,NUKPPROG         PROGRAM                                 
         MVC   SVKDATE,NUKPDATE         AIR DATE                                
         MVC   SVDPC,NUKPDP             DAYPART CODE                            
*                                                                               
* LOOK FOR EXPIRED SKED UNIT                                                    
         CLI   NUKPSUB,X'C1'       IS THIS A SKED UNIT                          
         BL    UNT25                NO                                          
*                                                                               
         L     R1,SKURECR          CT SKED UNIT RECORDS READ                    
         LA    R1,1(,R1)                                                        
         ST    R1,SKURECR                                                       
*                                                                               
         CLC   EXPDTCMP,NUKPDATE   IS IT EXPIRED                                
         BL    UNT25                NO                                          
*                                                                               
         OI    FOUNDSW,XSKEDSW     TURN ON EXPIRED SWITCH                       
         B     UNT38                                                            
*                                                                               
* SEE IF THE (WEEK OF) FOR THE UNIT DATE IS BEFORE EXPIRATION DATE              
*                                                                               
UNT25    DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,NUKPDATE),(0,WORK)                                
         GOTO1 GETDAY,(R1),(0,WORK),WORK+6                                      
         CLC   WORK+6(3),=CL3' '   CHK FOR BAD UNIT DATE                        
         BNE   UNT2501                                                          
         LA    R0,1                DATE=00 FAKE TO 01                           
         GOTO1 ADDAY,(R1),WORK,WORK,(R0)                                        
         GOTO1 DATCON,(R1),(0,WORK),(2,WORK+6)                                  
*                                                                               
UNT2501  LLC   R0,DMCB             GET DAY NUMBER                               
         CHI   R0,1                IF MONDAY                                    
         BE    UNT25A                THEN DONE                                  
*                                                                               
* SET DATE TO PREV MONDAY                                                       
*                                                                               
         BCTR  R0,0                                                             
         LNR   R0,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK,(R0)                                        
         GOTO1 DATCON,(R1),(0,WORK),(2,WORK+6)                                  
         CLC   EXPDTCMP,WORK+6     IS IT EXPIRED                                
         BL    UNT05                                                            
         B     *+14                                                             
UNT25A   CLC   EXPDTCMP,NUKPDATE   IS IT EXPIRED                                
         BL    UNT05                NO                                          
*                                                                               
         OI    FOUNDSW,UNTFSW      SET ACTIVE UNIT RECORD FOUND                 
         LM    RE,RF,STRAGLST      ACTIVE PROGRAM LIST                          
UNT25B   CR    RE,RF               CHK IF AT THE END OF THE TABLE               
         BE    UNT25C                                                           
         CLC   0(6,RE),SVKPROG     SAME PROGRAM ?                               
         BE    UNT26                                                            
         LA    RE,6(RE)                                                         
         B     UNT25B                                                           
*                                                                               
UNT25C   MVC   0(6,RE),SVKPROG     SAVE PROG IN TABLE                           
         LA    RE,6(RE)                                                         
         ST    RE,ENDAGLST                                                      
         C     RE,STRAPLST         CHK FOR TABLE OVERFLOW                       
         BL    *+6                                                              
         DC    H'0'                MAKE PROG TABLE LARGER                       
*                                                                               
* GET BROADCAST MONTH *                                                         
*                                                                               
UNT26    GOTO1 DATCON,DMCB,(2,SVKDATE),(0,WORK)       YYMMDD                    
         GOTO1 VGTBROAD,DMCB,(1,WORK),STDATEB,GETDAY,ADDAY,            C        
               RR=SPTR95RR                                                      
         TM    DMCB,X'FF'          INVALID DATE?                                
         BNO   *+6                                                              
         DC    H'0'                I MUST'VE GOOFED                             
*                                                                               
* CONVERT BROADCAST DATES TO BINARY                                             
*                                                                               
         GOTO1 DATCON,DMCB,(0,STDATEB),(3,STDATEBB)    BROAD BIN START          
         GOTO1 DATCON,DMCB,(0,ENDATEB),(3,ENDATEBB)    END DATES (YMD)          
*                                                                               
* GET CALENDAR MONTH *                                                          
*                                                                               
         MVC   STDATEC,WORK        MOVE IN UNIT DATE TO GET START DATE          
         MVC   STDATEC+4(2),=C'01'         YYMMDD                               
*                                                                               
* GET CALENDAR START DATE                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(0,STDATEC),(3,STDATECB)  BINARY                     
*                                                                               
* GET CALENDAR END DATE                                                         
*                                                                               
         MVC   WORK(6),STDATEC                                                  
         GOTO1 ADDAY,DMCB,(C'Y',WORK),(X'80',ENDATEC),0                         
*                                                                               
* AND CONVERT TO BINARY                                                         
         GOTO1 DATCON,DMCB,(0,ENDATEC),(3,ENDATECB)    YMD BINARY               
*                                                                               
* GET A PERIOD                                                                  
*                                                                               
         CLC   STDATEBB,STDATECB   GET EARLIER START DATE                       
         BL    *+14                                                             
         MVC   STDATEP,STDATECB                                                 
         B     *+10                                                             
         MVC   STDATEP,STDATEBB                                                 
*                                                                               
         CLC   ENDATEBB,ENDATECB   GET LATER END DATE                           
         BH    *+14                                                             
         MVC   ENDATEP,ENDATECB                                                 
         B     *+10                                                             
         MVC   ENDATEP,ENDATEBB                                                 
*                                                                               
UNT38    BRAS  RE,INITNET          SET FILES TO NET                             
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORD                          
         NI    DMOUTBTS,X'FF'-X'02' PASS DELETED RECORD                         
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'F7'       TURN OFF PASS DELETED RECORD                 
         OI    DMOUTBTS,X'02'       RESET FOR DELETE                            
*                                                                               
         MVI   TRCETYP,3           INDICATE GET                                 
         BAS   RE,TRACE                                                         
*                                                                               
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R6                                                      
*                                                                               
         TM    NUUNITST,X'02'      MISSED UNIT                                  
         BZ    *+12                                                             
         TM    FOUNDSW,XSKEDSW     EXPIRED SKED UNIT                            
         BZ    UNT80                BYPASS                                      
*                                                                               
         BAS   RE,GPROD            GET 3 CHAR PROD                              
*                                                                               
         OC    SVPROD1,SVPROD1     ANY PROD                                     
         BZ    UNT40                NO                                          
         MVC   SVPLEN1,NULEN        AND LEN                                     
*                                                                               
         OC    SVPROD2,SVPROD2     ANY PROD2 FORM 19 ELEM                       
         BNZ   UNT38J                                                           
*                                                                               
         CLI   NUPRD2,0            IS IT P/B                                    
         BE    UNT40                NO                                          
*                                                                               
         LA    R0,NUPRD2                                                        
         BAS   RE,FPRD                                                          
         MVC   SVPROD2,DUB         MOVE 3 CHAR PROD                             
*                                                                               
UNT38J   LLC   R1,NULEN1           LEN OF 1ST PRD                               
         LLC   R2,NULEN             TOTAL LEN                                   
         CLI   NULEN1,0            IF 1ST LEN IS ZERO THEN                      
         BNE   UNT39                                                            
         SRL   R2,1                TOTAL LEN DIV BY 2                           
         LR    R1,R2                                                            
         B     *+6                                                              
UNT39    SR    R2,R1               TOT LEN-1ST PRD LEN                          
         STC   R1,SVPLEN1                                                       
         STC   R2,SVPLEN2                                                       
*                                                                               
UNT40    DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'22'        FEED ELEMENT                                 
         BRAS  RE,GETEL                                                         
         BNE   UNT40C                                                           
         USING NUFEDEL,R6                                                       
*                                                                               
         OC    NUFEEDCD,NUFEEDCD   FEED                                         
         BZ    *+10                                                             
         MVC   SVFEED,NUFEEDCD     SAVE FEED                                    
*                                                                               
UNT40C   L     R6,AIO                                                           
         MVI   ELCODE,X'21'        UNIT CML ELEM                                
         BRAS  RE,GETEL                                                         
         BNE   UNT58                                                            
         USING NUCMLEL,R6                                                       
*                                                                               
         TM    FOUNDSW,XSKEDSW     IF SKED                                      
         BZ    UNT42                                                            
                                                                                
* SKED (TRAFFIC) UNIT PROCESSING *                                              
                                                                                
         MVC   SVCML1(16),NUCML1   THEN SAVE CMLS FOR PRINTING                  
         MVC   SVADID1(24),SPACES                                               
         MVC   SVADID1(8),SVCML1                                                
         MVC   SVADID2(8),SVCML2                                                
*                                                                               
         OC    NUCML1,NUCML1                                                    
         BZ    UNT41                                                            
         TM    NUCMADFL,NUCMADF1   IS THIS AN AD-ID                             
         BZ    UNT41               NO                                           
         LA    R5,SVADID1                                                       
         BRAS  RE,GETISCI          GET ISCI/ADI                                 
*                                                                               
UNT41    OC    NUCML2,NUCML2                                                    
         BZ    UNT85               GO,DEL SKED UNIT                             
         TM    NUCMADFL,NUCMADF2   IS THIS AN AD-ID                             
         BZ    UNT85               NO, GO DEL SKED UNIT                         
         LA    R5,SVADID2                                                       
         BRAS  RE,GETISCI          GET ISCI/ADI                                 
         B     UNT85                                                            
*                                                                               
* NETBUY UNIT                                                                   
*                                                                               
UNT42    TM    NUCMLFLG,X'E0'      INVALID ELEM                                 
         BO    UNT80                YES, GO READ NEXT UNIT REC                  
*                                                                               
         MVC   SVCML1(16),NUCMLBSN   MOVE BILLBOARD SLIDE NUMBER                
         MVC   SVADID1(24),SPACES                                               
         MVC   SVADID1(8),SVCML1                                                
         MVC   SVADID2(8),SVCML2                                                
*                                                                               
         OC    SVCML1,SVCML1                                                    
         BZ    UNT44                                                            
*                                                                               
         LA    R5,SVADID1                                                       
         TM    NUCMADFL,NUCMADF3   IS THIS AN AD-ID                             
         BZ    *+8                                                              
         BRAS  RE,GETISCI          GET 8 CHAR ISCI                              
         BRAS  RE,ACTU                                                          
*                                                                               
UNT44    OC    NUCMLBCN,NUCMLBCN   MOVE BILLBOARD COPY NUMBER                   
         BZ    UNT48                                                            
         LA    R5,NUCMLBCN                                                      
*                                                                               
         TM    NUCMADFL,NUCMADF4   IS THIS AN AD-ID                             
         BZ    *+8                                                              
         BRAS  RE,GETISCI          GET 8 CHAR ISCI                              
         BRAS  RE,ACTU                                                          
*                                                                               
UNT48    MVC   SVCML1(16),NUCML1   MOVE COMMERCIALS                             
         MVC   SVADID1(24),SPACES                                               
         MVC   SVADID1(8),SVCML1                                                
         MVC   SVADID2(8),SVCML2                                                
*                                                                               
         OC    NUCML1,NUCML1                                                    
         BZ    UNT52                                                            
*                                                                               
         LA    R5,SVADID1                                                       
         TM    NUCMADFL,NUCMADF1   IS THIS AN AD-ID                             
         BZ    *+8                                                              
         BRAS  RE,GETISCI          GET 8 CHAR ISCI                              
         BRAS  RE,ACTU             ADD TO ACTIVE CML TABLE                      
*                                                                               
UNT52    OC    NUCML2,NUCML2                                                    
         BZ    UNT55                                                            
*                                                                               
         LA    R5,SVADID2                                                       
         TM    NUCMADFL,NUCMADF2   IS THIS AN AD-ID                             
         BZ    *+8                                                              
         BRAS  RE,GETISCI          GET 8 CHAR ISCI                              
         BRAS  RE,ACTU             ADD TO ACTIVE CML TABLE                      
                                                                                
*==============================================================                 
* IF A COMMERCIAL HAS VIGNETTE/DELETE/ OR REASSIGN                              
* DO NOT PUT THE PATTERN IN THE ACTIVE PATTERN LIST                             
*==============================================================                 
                                                                                
*                                                                               
UNT55    OC    NUCML1,NUCML1        ANY CML                                     
         BZ    UNT55C               NO                                          
         LA    R5,NUCML1                                                        
         BAS   RE,CMLCK                                                         
         BE    UNT58               CML1 IS VIGNETTE/DEL/RE-ASGN                 
*                                                                               
UNT55C   OC    NUCML2,NUCML2        ANY CML                                     
         BZ    UNT55F               NO                                          
         LA    R5,NUCML2                                                        
         BAS   RE,CMLCK                                                         
         BE    UNT58               CML2 IS VIGNETTE/DEL/RE-ASGN                 
*                                                                               
UNT55F   DS    0H                                                               
         OC    NUCMLR3F,NUCMLR3F   ANY PAT ?                                    
         BZ    UNT58                NO                                          
*                                                                               
         MVC   SVPKSPEC,NUCMLKEY   SAVE PAT KEY SPEC BITS                       
*                                                                               
UNT55J   OC    SVPROD1,SVPROD1      ANY PROD                                    
         BZ    UNT80                NO BY-PASS PER BGRI                         
*                                                                               
         LA    R3,NUCMLR3F                                                      
         BAS   RE,UNPAT            GO BUILD PAT KEY                             
         DROP  R6                                                               
*                                                                               
UNT58    DS    0H                                                               
         TM    FOUNDSW,XSKEDSW     SKED ?                                       
         BO    UNT85                YES, GO DELETE IT                           
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'23'        FEED W/CML ELEM                              
         BRAS  RE,GETEL                                                         
         BNE   UNT80                                                            
*                                                                               
         USING NUFDCEL,R6                                                       
UNT60    OC    NUFDCBSN,NUFDCBSN                                                
         BZ    UNT62                                                            
*                                                                               
         MVC   SVCML1(16),NUFDCBSN    BILLBOARD SLIDE/COPY NUMBERS              
         MVC   SVADID1(24),SPACES                                               
         MVC   SVADID1(8),SVCML1                                                
         MVC   SVADID2(8),SVCML2                                                
*                                                                               
         LA    R5,SVADID1                                                       
         TM    NUFDADFL,NUFDADF3   IS THIS AN AD-ID                             
         BZ    *+8                                                              
         BRAS  RE,GETISCI          GET 8 CHAR ISCI                              
         BRAS  RE,ACTU             ADD TO ACTIVE CML TABLE                      
*                                                                               
UNT62    OC    NUFDCBCN,NUFDCBCN   BILLBOARD COPY NUMBER                        
         BZ    UNT64                                                            
*                                                                               
         LA    R5,SVADID2                                                       
         TM    NUFDADFL,NUFDADF4   IS THIS AN AD-ID                             
         BZ    *+8                                                              
         BRAS  RE,GETISCI          GET 8 CHAR ISCI                              
         BRAS  RE,ACTU             ADD TO ACTIVE CML TABLE                      
*                                                                               
UNT64    OC    NUFDCML1,NUFDCML1                                                
         BZ    UNT67                                                            
*                                                                               
         MVC   SVCML1(16),NUFDCML1                                              
         MVC   SVADID1(24),SPACES                                               
         MVC   SVADID1(8),SVCML1                                                
         MVC   SVADID2(8),SVCML2                                                
*                                                                               
         LA    R5,SVADID1                                                       
         TM    NUFDADFL,NUFDADF1   IS THIS AN AD-ID                             
         BZ    *+8                                                              
         BRAS  RE,GETISCI          GET 8 CHAR ISCI                              
         BRAS  RE,ACTU             ADD TO ACTIVE CML TABLE                      
*                                                                               
UNT67    OC    NUFDCML2,NUFDCML2                                                
         BZ    UNT75                                                            
*                                                                               
         LA    R5,SVADID2                                                       
         TM    NUFDADFL,NUFDADF2 IS THIS AN AD-ID                               
         BZ    *+8                                                              
         BRAS  RE,GETISCI          GET 8 CHAR ISCI                              
         BRAS  RE,ACTU              ADD TO ACTIVE CML TABLE                     
*                                                                               
UNT75    OC    NUFDCML1,NUFDCML1     ANY CML                                    
         BZ    UNT75C                                                           
         LA    R5,NUFDCML1                                                      
         BAS   RE,CMLCK                                                         
         BE    UNT80               CML1 IS VIGNETTE/DEL/RE-ASGN                 
*                                                                               
UNT75C   OC    NUFDCML2,NUFDCML2                                                
         BZ    UNT75F                                                           
         LA    R5,NUFDCML2                                                      
         BAS   RE,CMLCK                                                         
         BE    UNT80               CML1 IS VIGNETTE/DEL/RE-ASGN                 
*                                                                               
UNT75F   DS    0H                                                               
         OC    NUFDCR3F,NUFDCR3F   ANY PAT ?                                    
         BZ    UNT80                NO                                          
*                                                                               
         OC    NUFDCFED,NUFDCFED   FEED                                         
         BZ    *+10                                                             
         MVC   SVFEED,NUFDCFED     SAVE FEED                                    
*                                                                               
         OC    NUFDPROD,NUFDPROD   3 CHAR C/S PROD                              
         BZ    *+14                                                             
         MVC   SVCSPROD,NUFDPROD                                                
         B     UNT76                                                            
*                                                                               
UNT75J   OC    SVPROD1,SVPROD1     ANY PROD                                     
         BZ    UNT80                NO BY-PASS PER BGRI                         
*                                                                               
UNT76    MVC   SVPKSPEC,NUFDCKEY   SAVE PAT KEY SPEC BITS                       
         LA    R3,NUFDCR3F                                                      
         BAS   RE,UNPAT            GO BUILD PAT KEY                             
*                                                                               
         BRAS  RE,INITNET          SET FOR NET FILES AGAIN                      
         MVI   ELCODE,X'23'                                                     
         BRAS  RE,NEXTEL           CHECK FOR ANOTHER 23 ELEM                    
         BE    UNT60                                                            
*                                                                               
* SEE IF THERE ARE ANY CUT-INS                                                  
*                                                                               
UNT80    DS    0H                                                               
         BRAS  RE,GCUT                                                          
*                                                                               
         MVI   BYTE,0              INIT NO CHANGE FOR REVISION                  
*                                                                               
         TM    FOUNDSW,UNTFSW      WAS UNIT RECORD FOUND                        
         BZ    UNT100                                                           
*                                                                               
         CLC   SVBCLT,REVDATA      SAME CLIENT                                  
         BNE   UNT81F                                                           
         CLC   SVNUKNET(10),REVDATA+2 SAME NET/PROG                             
         BNE   UNT81F                                                           
*                                                                               
         CLC   SVSTDTEP,STDATEP    SAME PERIOD DATES                            
         BNE   *+14                                                             
         CLC   SVENDTEP,ENDATEP                                                 
         BE    UNT82                YES                                         
*                                                                               
         LA    R1,2                PRESET LEN FOR EX CLC                        
         CLI   REVPER+2,0          MONTHLY REVISION                             
         BNE   *+8                  NO, WEEKLY                                  
         LA    R1,1                LEN FOR EX CLC                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   STDATEP(0),REVPER   UNIT START DATE TO REV PERIOD                
         BH    UNT81                                                            
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ENDATEP(0),REVPER   UNIT END DATE TO REV PERIOD                  
         BNH   UNT82               WE ALREADY PROCESSED THIS REV                
*                                                                               
UNT81    MVI   BYTE,1              SAME CLT/NET/PROG ONLY DATE CHGED            
         B     UNT81H                                                           
*                                                                               
UNT81F   MVC   REVDATA(2),SVBCLT   SAVE CLIENT                                  
         MVC   REVDATA+2(10),SVNUKNET   NET/PROG                                
         XC    REVPER,REVPER       CLEAR PERIOD                                 
         XC    REVBPER,REVBPER                                                  
         MVC   REVNUM,REVNUM       AND REV NUMBER                               
*                                                                               
UNT81H   BAS   RE,UNREV            SEE IF THERE'RE ACTIVE REV RECS              
*                                                                               
UNT82    DS    0H                                                               
         NI    FOUNDSW,X'FF'-UNTFSW                                             
*                                                                               
* PRINT UNIT RECORD INFO AND DELETE THE SKED UNIT                               
*                                                                               
UNT85    DS    0H                                                               
         TM    FOUNDSW,XSKEDSW     IS IT EXPIRED SKED UNIT                      
         BZ    UNT100               NO                                          
         NI    FOUNDSW,X'FF'-XSKEDSW                                            
         TM    FTRSW2,SKULSW       LIST DELETED SKED UNITS?                     
         BZ    UNT95                NO                                          
*                                                                               
         L     R1,SKURECS          CT TO BE DELETED                             
         LA    R1,1(,R1)                                                        
         ST    R1,SKURECS                                                       
*                                                                               
         MVC   PSUNET,SVNUKNET     PRINT NETWORK                                
         MVC   PSUPRG,SVKPROG       AND PROGRAM                                 
*                                                                               
         OC    SVPROD1,SVPROD1                                                  
         BZ    UNT86                                                            
         MVC   PSUPRD,SVPROD1      PRINT PRD1                                   
         OC    SVPROD2,SVPROD2                                                  
         BZ    UNT86                                                            
         MVC   PSUPTR,SVPROD2      PRINT PRODUCT PARTNER                        
*                                                                               
UNT86    DS    0H                                                               
         OC    SVCML1(16),SVCML1                                                
         BZ    UNT90                                                            
         MVC   PSUCML(12),SVADID1          AND CMLS                             
         OC    SVCML2,SVCML2                                                    
         BZ    UNT90                                                            
         LA    RE,PSUCML+13                                                     
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   0(RE),C'-'                                                       
         MVC   1(12,RE),SVADID2                                                 
*                                                                               
UNT90    DS    0H                                                               
         CLI   SVDPC,0                                                          
         BE    UNT93                                                            
*                                                                               
         XC    GERROR,GERROR                                                    
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
*                                                                               
         GOTO1 VALIDPT,DMCB,(X'01',SVDPC)  GET 2 CHAR DAYPART                   
*                                                                               
         MVI   ERROPT,0                                                         
         OC    GERROR,GERROR                                                    
         BZ    UNT92                                                            
*                                                                               
         MVC   PSUDPC,=C'??'       BAD DAYPART CODE PRINT '??'                  
         B     *+10                                                             
UNT92    MVC   PSUDPC,QDPT2                                                     
*                                                                               
UNT93    GOTO1 DATCON,DMCB,(2,SVKDATE),(5,PSUDATE)                              
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
UNT95    CLC   SVUNTKEY,KEY                                                     
         BE    UNT97                                                            
*                                                                               
         MVC   KEY(L'SVUNTKEY),SVUNTKEY  RESTORE UNIT KEY                       
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 HIGH                DUMMY HI FOR SEQ                             
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
*                                                                               
         OI    DMINBTS,X'08'                                                    
         NI    DMOUTBTS,X'FF'-X'02'                                             
         GOTO1 GETREC              DUMMY GETREC FOR PUTREC                      
         NI    DMINBTS,X'F7'                                                    
         OI    DMOUTBTS,X'02'                                                   
*                                                                               
         L     R6,AIO1             RESET                                        
         ST    R6,AIO                                                           
*                                                                               
UNT97    TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    UNT100                                                           
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    UNT100                                                           
*                                                                               
         BAS   RE,DELREC           GO DELETE RECORD AND KEY                     
*                                                                               
         L     R1,SKURECDL                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,SKURECDL                                                      
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORD                          
         NI    DMOUTBTS,X'FF'-X'02' PASS DELETED RECORD                         
         GOTO1 HIGH                GET BACK INTO SEQUENCE                       
         NI    DMINBTS,X'F7'       TURN OFF PASS DELETED RECORD                 
         OI    DMOUTBTS,X'02'       RESET FOR DELETE                            
*                                                                               
         MVI   TRCETYP,1           INDICATE HIGH                                
         BAS   RE,TRACE                                                         
*                                                                               
         B     UNT10                                                            
*                                                                               
UNT100   GOTO1 HIGH                                                             
         MVI   TRCETYP,1           INDICATE HIGH                                
         BAS   RE,TRACE                                                         
*                                                                               
         B     UNT10                                                            
*                                                                               
* END OF SKED UNIT FOR CLIENT, PRINT TOTALS, ADD TO MEDIA TOTALS *              
*                                                                               
UNTX     DS    0H                                                               
         TM    FTRSW2,SKULSW       LIST DELETED SKED UNITS?                     
         BZ    UNTX10               NO                                          
         LA    R2,SKURECR                                                       
         LA    R3,=C'UNT'                                                       
         BRAS  RE,RECTOT                                                        
*                                                                               
         LA    R2,SKURECR                                                       
         LA    R3,AGYCTRS+SKURECR-COUNTERS                                      
         LA    R4,3                                                             
         BAS   RE,ACT              ADD CLIENT TO MEDIA CTRS                     
UNTX10   BRAS  RE,REV                                                           
         B     PEQV                                                             
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
*============================================================                   
* GET 3 CHAR PRODUCT FROM 19 ELEM                                               
*============================================================                   
                                                                                
GPROD    NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'19'        3 CHAR PROD ELEM                             
         BRAS  RE,GETEL                                                         
         BNE   GPRODX                                                           
*                                                                               
         USING NUPDED,R6                                                        
         SR    R0,R0                                                            
         LLC   R1,1(R6)                                                         
         D     R0,=F'7'                                                         
*                                                                               
         LTR   R0,R1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,NUPDEPR                                                       
         MVC   SVPROD1,0(R1)       SAVE 3 CHAR PROD                             
         CHI   R0,2                P/B PROD                                     
         BL    GPRODX                                                           
         LA    R1,7(R1)            POINT TO NEXT PROD                           
         MVC   SVPROD2,0(R1)                                                    
*                                                                               
GPRODX   XIT1                                                                   
         EJECT                                                                  
*===============================================================                
* SEE THAT COMMERCIAL IS NOT VIGNETTE/REASSIGN/DELETED                          
*===============================================================                
                                                                                
CMLCK    NTR1                                                                   
         CLC   =X'5C00',0(R5)        DELETED COMML                              
         BE    CMLCKX                                                           
         CLC   =C'REASSIGN',0(R5)                                               
         BE    CMLCKX                                                           
         CLC   =C'VIGNETTE',0(R5)    VIGNETTE                                   
         BE    CMLCKX                                                           
         LTR   RB,RB               SET CC OF <>                                 
CMLCKX   XIT1                                                                   
         EJECT                                                                  
*=============================================================                  
* BUILD ACTIVE PATTERN TABLE FOR THE EXISTING UNITS                             
* ON ENTRY R3 POINTS TO 2-BYTE PATTERN SEQNUM                                   
*=============================================================                  
                                                                                
* BUILD PATTERN RECORD KEY FOR THIS UNIT                                        
                                                                                
UNPAT    NTR1                                                                   
*                                                                               
         BRAS  RE,INITNET          SET TO NET                                   
         MVI   CSPRDSW,C'N'        RESET C/S SWITCH                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPTXKEY,R4                                                       
         MVC   NPTPXID,=X'0AE1'    PASSIVE POINTER                              
         MVC   NPTPXAM,BAGYMD                                                   
         MVC   NPTPXCLT,SVBCLT                                                  
         MVC   NPTPXS3Q,0(R3)                                                   
*                                                                               
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ+XSPQ,0                                   
*NOP     GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(32),KEYSAVE      SAME A/M/CLT ETC.                           
         BNE   UNPATX                                                           
*                                                                               
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 AIOCALL,DMCB,GETQ+FILQ+XSPQ,0(R6)                                
*                                                                               
* ADD TO ACTIVE PATTERN LIST *                                                  
*                                                                               
         LR    R4,R6               POINT TO X'23' KEY INSTEAD OF X'A3'          
         BRAS  RE,APL                                                           
*                                                                               
         BRAS  RE,INITXSP                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NPTDTAEL,R6                                                      
         MVI   ADIDFLAG,C'Y'                                                    
         TM    NPTSTAT,NPTS_ADID                                                
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BNE   UNPATX                                                           
*                                                                               
         USING NPTCMLEL,R6                                                      
*                                                                               
* ADD TO ACTIVE COMMERCIAL LIST *                                               
*                                                                               
         BRAS  RE,ACL                                                           
UNPATX   BRAS  RE,INITNET          RESTORE TO NET FILES                         
         XIT1                                                                   
         EJECT                                                                  
* LOOK FOR ACTIVE REVISON RECORD FOR THIS UNIT                                  
*                                                                               
UNREV    NTR1                                                                   
*                                                                               
         MVC   SVSTDTEP,STDATEP    SAVE PERIOD DATES                            
         MVC   SVENDTEP,ENDATEP                                                 
*                                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    UNREV05                                                          
         TM    FTRSW1,REVSW        PURGE REVISION RECORDS                       
         BZ    UNREVX               NO, DON'T BUILD ACTIVE REV TBL              
*                                                                               
* BUILD REVISION RECORD KEY                                                     
*                                                                               
UNREV05  DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING REVXKEY,R4                                                       
         MVC   REVXKID,=X'0A1D'                                                 
         MVC   REVXKAM,BAGYMD      AGY/MED                                      
         MVC   REVXKCLT,SVBCLT     CLIENT                                       
         MVC   REVXKNET(10),SVNUKNET  NETWORK AND PROGRAM                       
*                                                                               
         CLI   BYTE,1              JUST NEED REV FOR DIFF PERIOD                
         BNE   UNREV08                                                          
*                                                                               
         MVC   REVXKPER,REVBPER    PERIOD                                       
         MVC   REVXKNUM,REVNUM     REVISION NUMBER                              
*                                                                               
UNREV08  GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ+XSPQ,0                                   
*                                                                               
         MVI   TRCETYP,1           INDICATE HIGH                                
         BAS   RE,TRACE                                                         
*                                                                               
UNREV10  CLC   KEY(15),KEYSAVE      SAME A/M/CLT/NET/PRG                        
         BNE   UNREVX                NO                                         
*                                                                               
         DS    0H                                                               
         MVI   WORK+2,0                                                         
         MVC   WORK(2),REVXKPER                                                 
         MVI   WORK+52,0                                                        
         MVC   WORK+50(2),REVXKPER                                              
         LA    R1,1                LEN FOR EXECUTED CLC                         
         TM    REVXKPER,X'80'      THIS WEEKLY REV REC                          
         BZ    UNREV18              NO                                          
*                                                                               
         CLC   EXPDTCMP,REVXKPER   IF REV PER AFTER PURGE DATE                  
*NOP     BL    UNREV40             DO NOT SAVE IN ACTIVE TABLE                  
         BL    UNREVX              DONE FOR THIS CLT/NET/PROG                   
*                                                                               
         GOTO1 DATCON,DMCB,(2,WORK),(3,WORK+50)                                 
         LA    R1,2                LEN FOR EXECUTED CLC                         
         B     UNREV20                                                          
*                                                                               
UNREV18  DS    0H                                                               
         CLC   EXPDATE(2),REVXKPER IF REV PER AFTER PURGE DATE                  
*NOP     BL    UNREV40             DO NOT SAVE IN ACTIVE TABLE                  
         BL    UNREVX              DONE FOR THIS CLT/NET/PROG                   
*                                                                               
UNREV20  DS    0H                                                               
         EX    R1,SDATECLC                                                      
         BH    UNREV40              NO, DO READ SEQ                             
         B     *+10                                                             
SDATECLC CLC   STDATEP(0),WORK+50   IS THIS REC WITHIN THE UNIT PERIOD          
*                                                                               
         EX    R1,*+8                                                           
         BL    UNREVX              WAS UNREV40                                  
         CLC   ENDATEP(0),WORK+50                                               
*                                                                               
* BUILD ACTIVE REVISION TABLE FROM UNIT RECORDS                                 
*                                                                               
         MVC   REVPER,WORK+50      SAVE PERIOD PROCESSED                        
         MVC   REVBPER,REVXKPER    PERIOD IN BINARY                             
         MVC   REVNUM,REVXKNUM     AND REV NUMBER                               
*                                                                               
         BRAS  RE,ARL                                                           
*                                                                               
UNREV40  MVC   KEYSAVE,KEY                                                      
*NOP     GOTO1 SEQ                                                              
         MVC   MYKEYSV,KEY                                                      
         GOTO1 AIOCALL,DMCB,SEQQ+DIRQ+XSPQ,0                                    
         MVC   KEYSAVE,MYKEYSV                                                  
*                                                                               
         MVI   TRCETYP,2           INDICATE SEQ                                 
         BAS   RE,TRACE                                                         
         B     UNREV10                                                          
*                                                                               
UNREVX   XIT1                                                                   
         EJECT                                                                  
* DELETE ANY PROGRAM EQUIVALENCY RECORDS FOR THIS CLIENT *                      
*                                                                               
PEQV     DS    0H                                                               
         MVI   RCSUBPRG,2                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    *+12                                                             
         TM    FTRSW1,PEQVSW       DO PROGRAM EQUIV RECORDS                     
         BZ    PAT                  NO                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PGEKEY,R4                                                        
         MVI   PGEKID,X'24'                                                     
         MVC   PGEKAM,BAGYMD                                                    
         MVC   PGEKCLT,SVBCLT                                                   
*                                                                               
         BRAS  RE,INITNET          SET TO NET                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
PEQV10   CLC   KEY(4),KEYSAVE      SAME A/M/CLT                                 
         BNE   PEQVX                                                            
*                                                                               
         XC    SVBPLST,SVBPLST     INIT BASE PROGRAM LIST                       
*                                                                               
         L     R1,PEQRECR          RECS READ                                    
         LA    R1,1(,R1)                                                        
         ST    R1,PEQRECR                                                       
*                                                                               
         MVI   WRTRECSW,0                                                       
         MVC   AIO,AIO1                                                         
         L     R6,AIO1                                                          
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PGEDTAEL,R6                                                      
PEQV20   DS    0H                                                               
         L     R1,PEQRECER         ELEM CT READ                                 
         LA    R1,1(,R1)                                                        
         ST    R1,PEQRECER                                                      
*                                                                               
         LM    RE,RF,STRAGLST      ACTIVE PROGRAM LIST                          
         CR    RE,RF               CHK IF AT THE END OF THE TABLE               
         BE    PEQV21                                                           
         CLC   0(6,RE),PGEPROG     SAME PROGRAM ?                               
         BE    PEQV22C              YES, GO ON TO THE NEXT                      
         LA    RE,6(RE)                                                         
         B     *-20                                                             
*                                                                               
PEQV21   CLC   EXPDATE,PGESTR      START DATE BEFORE EXP DATE                   
         BL    PEQV22C                                                          
         CLC   EXPDATE,PGEEND      IS THIS AN EXPIRED RECORD                    
         BL    PEQV22C              NO                                          
*                                                                               
* SAVE BASE PROGRAM FOR PASSIVE KEY DELETE                                      
*                                                                               
         LA    R1,SVBPLST          PT TO BASE PROGRAM LIST                      
         LR    RE,R1                                                            
         LA    RE,L'SVBPLST-1(RE)  POINT TO THE END OF LIST                     
PEQV21B  CR    RE,R1               CHK FOR END OF LIST                          
         BH    *+6                                                              
         DC    H'0'                TABLE TOO SMALL                              
         CLI   0(R1),0             EMPTY ENTRY                                  
         BE    *+22                                                             
         CLC   PGEPROG,0(R1)       SAME PROG ?                                  
         BE    PEQV21C                                                          
         LA    R1,6(R1)            NEXT ENTRY                                   
         B     PEQV21B                                                          
         MVC   0(6,R1),PGEPROG     SAVE BASE PROG                               
*                                                                               
* PRINT OUT EQV INFO AND DELETE ELEMENTS *                                      
*                                                                               
PEQV21C  MVC   PENET,PGEKNET                                                    
         MVC   PEPRG,PGEKPRG       PRINT EQV PROGRAM                            
         MVC   PEBPRG,PGEPROG         BASE                                      
         GOTO1 DATCON,DMCB,(3,PGESTR),(5,PESTR)                                 
         MVI   PESTR+10,C'-'                                                    
         GOTO1 DATCON,DMCB,(3,PGEEND),(5,PEEND)                                 
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R1,PEQRECEL         ELEM CT TO BE DELETED                        
         LA    R1,1(,R1)                                                        
         ST    R1,PEQRECEL                                                      
*                                                                               
* DELETE ELEMENT *                                                              
*                                                                               
         GOTO1 VRECUP,DMCB,(C'U',AIO),(R6)                                      
         MVI   WRTRECSW,1                                                       
         TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    PEQV22                                                           
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    PEQV22                                                           
*                                                                               
         L     R1,PEQRECED         ELEM DELETED CT                              
         LA    R1,1(,R1)                                                        
         ST    R1,PEQRECED                                                      
*                                                                               
PEQV22   CLC   ELCODE,0(R6)        IS THIS THE SAME ELEMENT                     
         BE    PEQV20               YES                                         
*                                                                               
PEQV22C  BRAS  RE,NEXTEL                                                        
         BE    PEQV20                                                           
*                                                                               
         CLI   WRTRECSW,1          THIS RECORD HAVE ELEMS REMOVED               
         BNE   PEQV70                                                           
*                                                                               
         L     R1,PEQRECU          CT UPDATED                                   
         LA    R1,1(,R1)                                                        
         ST    R1,PEQRECU                                                       
*                                                                               
* IS THE RECORD EMPTY NOW                                                       
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PEQV66                                                           
*                                                                               
* REC NOT EMPTY, UPDATE IT                                                      
*                                                                               
         TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    PEQV70                                                           
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    PEQV70                                                           
*                                                                               
         L     R1,PEQRECW          CT WRITTEN                                   
         LA    R1,1(,R1)                                                        
         ST    R1,PEQRECW                                                       
*                                                                               
         GOTO1 PUTREC                                                           
         B     PEQV70                                                           
*                                                                               
*  REC IS EMPTY DEL REC AND THE KEYS                                            
*                                                                               
PEQV66   TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    PEQV70                                                           
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    PEQV70                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R1,PEQRECDL         EQV REC DEL CT                               
         LA    R1,1(,R1)                                                        
         ST    R1,PEQRECDL                                                      
*                                                                               
         BAS   RE,DELREC           GO DELETE RECORD AND KEY                     
*                                                                               
         LA    R1,SVBPLST                                                       
         CLI   0(R1),0                                                          
         BE    PEQV70              THE ARE NO BASE PROGS                        
*                                                                               
         MVC   SVKEY,KEY           SAVE PROGRAM EQUIV KEY                       
*                                                                               
         MVI   PGEPID,X'A4'        DELETE PASSIVE KEY                           
         MVC   PGEPEPRG,PGEKPRG    EQV PRG                                      
*                                                                               
         LR    RF,R1                                                            
         LA    RF,L'SVBPLST(RF)                                                 
         LA    RE,1                                                             
*                                                                               
         LA    R1,6(R1)                                                         
         CR    RF,R1                                                            
         BNH   *+20                                                             
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-22                                                             
*                                                                               
         LA    R1,SVBPLST                                                       
         LR    R0,RE               NUMBER OF BASE PROGS                         
*                                                                               
         MVC   PGEPBPRG,0(R1)      BASE PRG                                     
*                                                                               
         BAS   RE,DELKEY                                                        
         LA    R1,6(R1)                                                         
         BCT   R0,*-14                                                          
*                                                                               
*NOP     MVC   KEY,SVKEY                                                        
*****    GOTO1 HIGH                DUMMY HI FOR SEQ                             
*                                                                               
PEQV70   GOTO1 SEQ                                                              
         B     PEQV10                                                           
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
* END OF PROG EQV REC FOR CLIENT, PRINT TOTALS, ADD TO MEDIA TOTALS *           
*                                                                               
PEQVX    LA    R2,PEQRECER                                                      
         LA    R3,=C'EQV'                                                       
         BRAS  RE,ELEMTOT                                                       
*                                                                               
         LA    R2,PEQRECER                                                      
         LA    R3,AGYCTRS+PEQRECER-COUNTERS                                     
         LA    R4,7                                                             
         BAS   RE,ACT              ADD CLIENT TO MEDIA CTRS                     
         EJECT                                                                  
* PURGE PATTERN RECORDS *                                                       
*                                                                               
PAT      DS    0H                                                               
*                                                                               
         MVI   RCSUBPRG,3                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    PAT00                                                            
*                                                                               
         TM    FTRSW1,PATSW        DO PATTERNS                                  
         BO    PAT00                                                            
*                                                                               
         TM    FTRSW1,CMLSW        DO COMMERCIAL                                
         BZ    DCML                 NO                                          
*                                                                               
PAT00    XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NPTXKEY,R4                                                       
         MVC   NPTXKID,=X'0A61'                                                 
         MVC   NPTXAM,BAGYMD                                                    
         MVC   NPTXCLT,SVBCLT                                                   
         MVI   NPTXR3F,X'A0'       BYPASS ANY SAVED PATTERNS                    
         DROP  R4                                                               
*                                                                               
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ+XSPQ,0                                   
         B     PAT04                                                            
*                                                                               
PAT02    DS    0H                                                               
         MVC   MYKEYSV,KEY                                                      
         GOTO1 AIOCALL,DMCB,SEQQ+DIRQ+XSPQ,0                                    
         MVC   KEYSAVE,MYKEYSV                                                  
*                                                                               
PAT04    LA    R4,KEY              RESTORE KEY POINTER                          
         USING NPTXKEY,R4                                                       
*                                                                               
         CLC   KEY(5),KEYSAVE                                                   
         BNE   PATX                                                             
*                                                                               
         OC    KEY+5(27),KEY+5     THIS A PTN SEQ REC                           
         BZ    PAT02                                                            
*                                                                               
         CLI   KEY+23,X'A0'        THIS SAVED PTN                               
         BL    PAT02                                                            
*                                                                               
         CLI   NPTXPSSV-NPTXKEY+KEY,0   TEST PASSIVE                            
         BNE   PAT02                                                            
*                                                                               
         L     R1,PATRECR          RECS READ                                    
         LA    R1,1(,R1)                                                        
         ST    R1,PATRECR                                                       
*                                                                               
         OC    QPRD,QPRD           PROD ENTERED                                 
         BZ    *+14                 NO                                          
         CLC   NPTXPRD,QPRD                                                     
         BNE   PAT80                                                            
         OC    QPRD2,QPRD2         PROD2 ENTERED                                
         BZ    *+14                 NO                                          
         CLC   NPTXPRD2,QPRD2                                                   
         BNE   PAT80                                                            
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 AIOCALL,DMCB,GETQ+FILQ+XSPQ,(R6)                                 
*                                                                               
         CLI   NPTXNET-NPTXKEY(R6),C'$' TEST PATTERN LIST REC                   
         BE    PAT10                                                            
*                                                                               
         LM    RE,RF,STRAPLST      START OF ACTIVE PATTERN LIST                 
         USING APLIST,RE                                                        
*                                                                               
PAT06    CR    RE,RF               END OF THE TABLE                             
         BE    PAT15                                                            
*                                                                               
         CLC   APLNET(APLISTLN),NPTXNET-NPTXKEY+KEY   TEST SAME PATTERN         
         BE    PAT60                                                            
         LA    RE,APLISTLN(RE)                                                  
         B     PAT06                                                            
         DROP  RE                                                               
                                                                                
* FOR PATTERN LIST RECORD, CHECK THE LIST FOR EACH NETWORK *                    
                                                                                
PAT10    MVI   ELCODE,X'5B'                                                     
         MVI   DATADISP+1,42                                                    
         BRAS  RE,GETEL                                                         
         BNE   PAT15                                                            
*                                                                               
PAT11    MVC   WORK(APLISTLN),NPTXNET-NPTKEY+KEY                                
         MVC   WORK(4),2(R6)       MOVE NETWORK                                 
*                                                                               
         LM    RE,RF,STRAPLST      START OF ACTIVE PATTERN LIST                 
         USING APLIST,RE                                                        
*                                                                               
PAT12    CR    RE,RF               END OF THE TABLE                             
         BE    PAT14                                                            
*                                                                               
         CLC   APLNET(APLISTLN),WORK   TEST SAME PATTERN                        
         BE    PAT60                   STILL ACTIVE                             
         LA    RE,APLISTLN(RE)                                                  
         B     PAT12                                                            
*                                                                               
PAT14    BRAS  RE,NEXTEL                                                        
         BE    PAT11                                                            
         DROP  RE                                                               
         EJECT                                                                  
* CHECK PATTERN'S START AND END DATES *                                         
*                                                                               
PAT15    MVI   ELCODE,X'10'                                                     
         MVI   DATADISP+1,42                                                    
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BE    PAT20                                                            
         DC    H'0'                                                             
         USING NPTDTAEL,R6                                                      
PAT20    EQU   *                                                                
         CLC   NPTEND,EXPDATE      IS END DATE BEFORE EXP DATE                  
         BNH   PAT21                 YES, GO DELETE IT                          
         CLC   EXPDATE,NPTSTART    IS EXP DATE BEFORE STR DATE                  
         BL    PAT80               GET NEXT PAT REC                             
*                                                                               
         BRAS  RE,APL               ADD TO ACTIVE PATTERN LIST                  
         B     PAT64                GO ADD COMMLS TO ACTIVE LIST                
*                                                                               
PAT21    DS    0H                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    *+12                                                             
         TM    FTRSW1,PATSW        DO PAT                                       
         BZ    PAT64                NO, BYPASS DELETE                           
*                                                                               
         L     R1,PATRECS          REC CT TO BE DELETED                         
         LA    R1,1(,R1)                                                        
         ST    R1,PATRECS                                                       
*                                                                               
* PRINT OUT PATTERN RECORD FOR DELETE LIST *                                    
*                                                                               
         MVC   PPPRD(3),NPTXPRD                                                 
         OC    NPTXPRD2,NPTXPRD2                                                
         BZ    PAT22                                                            
*                                                                               
         MVI   PPPRD+3,C'-'                                                     
         LLC   R0,NPTXSLN                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PPPRD+4(3),DUB                                                   
*                                                                               
         MVC   PPPTR(3),NPTXPRD2                                                
         MVI   PPPTR+3,C'-'                                                     
         LLC   R0,NPTXSLN2                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PPPTR+4(3),DUB                                                   
*                                                                               
PAT22    DS    0H                                                               
         CLI   NPTXNET,X'00'       TEST FOR PATTERN BY MEDIA                    
         BNE   PAT22C                                                           
         CLI   NPTXNET+2,X'FF'                                                  
         BNE   PAT22C                                                           
         MVC   PPNET(2),=C'M='                                                  
         MVC   PPNET+2(1),NPTXNET+1                                             
         B     *+10                                                             
*                                                                               
PAT22C   MVC   PPNET,NPTXNET                                                    
         CLI   NPTXPROG,X'FF'                                                   
         BE    PAT22F                                                           
         MVC   PPROG,NPTXPROG                                                   
         B     PAT24                                                            
*                                                                               
PAT22F   MVC   PPCODE,NPTXPROG+1                                                
         MVC   PPFEED,NPTXPROG+2                                                
*                                                                               
PAT24    DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,NPTSTART),(5,PPERIOD)                             
         MVI   PPERIOD+5,C'-'                                                   
         CLC   =X'FFFFFF',NPTEND                                                
         BNE   *+14                                                             
         MVC   PPERIOD+6(3),=C'UFN'                                             
         B     PAT26                                                            
         GOTO1 (RF),(R1),(3,NPTEND),(5,PPERIOD+6)                               
*                                                                               
PAT26    SR    RE,RE                                                            
         ICM   RE,7,NPTXR3F                                                     
         X     RE,=XL4'00FFFFFF'                                                
         EDIT  (RE),(4,PPREF)                                                   
         DROP  R4                                                               
*                                                                               
         MVC   PPDESC,NPTDESC                                                   
*                                                                               
         MVI   ADIDFLAG,C'Y'                                                    
         TM    NPTSTAT,NPTS_ADID                                                
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         MVI   ELCODE,X'30'        CMLS LIST ELEM                               
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPTCMLEL,R6                                                      
         LLC   R2,NPTCMLLN                                                      
         SRL   R2,4                DIV BY 16 TO GET ENTRIES                     
         LA    R3,4                FORMAT 4 LINES BEFORE PRINT                  
         LA    R6,NPTCML           START OF LIST                                
         DROP  R6                                                               
         LA    R5,ATBL                                                          
*                                                                               
PAT32    LA    R4,PPCMLS            START                                       
*                                                                               
PAT36    MVC   0(1,R4),0(R5)                                                    
         MVI   1(R4),C'='                                                       
         MVC   2(8,R4),0(R6)                                                    
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   PAT36X                                                           
         GOTO1 VTRPACK,DMCB,(C'U',(R6)),2(R4)                                   
*                                                                               
PAT36X   OC    8(8,R6),8(R6)       PIGGY-BACK COMML                             
         BZ    PAT38               NO                                           
*                                                                               
         LA    R4,15(R4)           BACK UP TO LAST CHAR                         
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C'-'                                                       
         MVC   2(8,R4),8(R6)                                                    
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   PAT38                                                            
         GOTO1 VTRPACK,DMCB,(C'U',8(R6)),2(R4)                                  
*                                                                               
PAT38    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R5,1(R5)                                                         
         LA    R6,16(R6)                                                        
         BCT   R2,PAT32                                                         
*                                                                               
PAT58    TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    PAT80                                                            
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    PAT80                                                            
*                                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    *+12                                                             
         TM    FTRSW1,PATSW        DO PAT                                       
         BZ    PAT64                NO, BYPASS DELETE                           
*                                                                               
         L     R1,PATRECDL                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,PATRECDL                                                      
*                                                                               
         BAS   RE,DELREC           GO DELETE RECORD AND KEY                     
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         OI    DMINBTS,X'08'       PASS DELETED RECORD                          
         NI    DMOUTBTS,X'FF'-X'02' PASS DELETED RECORD                         
*NOP     GOTO1 HIGH                GET BACK INTO SEQUENCE                       
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ+XSPQ,0                                   
         NI    DMINBTS,X'F7'       TURN OFF PASS DELETED RECORD                 
         OI    DMOUTBTS,X'02'       RESET FOR DELETE                            
*                                                                               
         MVC   MYKEYSV,KEY                                                      
         GOTO1 AIOCALL,DMCB,SEQQ+DIRQ+XSPQ,0                                    
         MVC   KEYSAVE,MYKEYSV                                                  
*                                                                               
         B     PAT04                                                            
*                                                                               
* THIS IS AN ACTIVE PATTERN                                                     
*                                                                               
PAT60    DS    0H                                                               
*                                                                               
PAT64    MVI   ELCODE,X'30'        FOR CML LIST                                 
         MVI   DATADISP+1,42                                                    
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* BUILD ACTIVE COMMERCIAL LIST *                                                
*                                                                               
         MVC   SVPATKEY,KEY        SAVE PAT KEY FOR SEQ READS                   
         BRAS  RE,ACL              GO ADD TO ACTIVE CML LIST                    
         MVC   KEY(L'SVPATKEY),SVPATKEY  RESTORE PAT KEY                        
         EJECT                                                                  
* GET NEXT SEQUENTIAL PATTERN RECORD *                                          
*                                                                               
PAT80    MVC   KEYSAVE,KEY                                                      
*NOP     GOTO1 HIGH                GET BACK INTO SEQUENCE                       
         GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ+XSPQ,0                                   
*                                                                               
*NOP     GOTO1 SEQ                  AND NOW READ SEQ                            
         MVC   MYKEYSV,KEY                                                      
         GOTO1 AIOCALL,DMCB,SEQQ+DIRQ+XSPQ,0                                    
         MVC   KEYSAVE,MYKEYSV                                                  
*                                                                               
         B     PAT04                                                            
*                                                                               
* AT END OF PATTERNS FOR CLIENT, SORT ACTIVE         *                          
* COMMERCIAL LIST, PRINT TOTALS, ADD TO MEDIA TOTALS *                          
*                                                                               
PATX     LM    R2,R3,STRACLST                                                   
         LR    R4,R3                                                            
         CR    R2,R3               ANY ACTIVE COMMERCIALS FOUND                 
         BNL   PATX20               NO                                          
         SR    R3,R2                                                            
         SR    R0,R0                                                            
         LR    R1,R3                                                            
         D     R0,=F'12'           DIV BY THE LENGTH OF AN ENTRY                
         LR    R3,R1                                                            
*                                                                               
         GOTO1 XSORT,DMCB,(R2),(R3),12,12,1                                     
*                                                                               
PATX20   MVI   0(R4),X'FF'                                                      
*                                                                               
         LA    R2,PATRECR                                                       
         LA    R3,=C'PAT'                                                       
         BRAS  RE,RECTOT                                                        
*                                                                               
         LA    R2,PATRECR                                                       
         LA    R3,AGYCTRS+PATRECR-COUNTERS                                      
         LA    R4,3                                                             
         BAS   RE,ACT              ADD CLIENT TO MEDIA CTRS                     
         EJECT                                                                  
* DELETE COMMERCIAL RECORDS                                                     
*                                                                               
DCML     DS    0H                                                               
         BRAS  RE,DELCML           PURGE CML RECORDS                            
*                                                                               
         EJECT                                                                  
* NOW SEE IF THERE WERE ANY LOST CML RECORDS *                                  
*                                                                               
CKLCML   LM    R3,R4,STRLCLST                                                   
         SR    R4,R3                                                            
         LTR   R4,R4                                                            
         BZ    CKLCML30                                                         
*                                                                               
         SRL   R4,3                DIV BY 8 (LEN OF AN ENTRY)                   
*                                                                               
         L     R1,TOTLCMLC                                                      
         AR    R1,R4                                                            
         ST    R1,TOTLCMLC         TOTAL LOST COMMERCIALS                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+3(4),=C'*** '                                                  
         EDIT  (R4),(2,P+7),COMMAS=YES,ZERO=NOBLANK                             
         MVC   P+9(21),=C' LOST COMMERCIALS ***'                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R3,STRLCLST                                                      
         LA    R5,P                                                             
         MVI   CLMCT,0                                                          
*                                                                               
CKLCML05 DS    0H                                                               
         MVC   0(8,R5),0(R3)                                                    
         LA    R3,8(R3)                                                         
         LA    R5,15(R5)                                                        
         C     R3,ENDLCLST                                                      
         BE    CKLCML10                                                         
         LLC   R1,CLMCT                                                         
         LA    R1,1(R1)                                                         
         STC   R1,CLMCT                                                         
         CLI   CLMCT,3                                                          
         BE    CKLCML08                                                         
CKLCML06 BCT   R4,CKLCML05                                                      
         B     CKLCML10                                                         
*                                                                               
CKLCML08 MVI   CLMCT,0                                                          
         LA    R5,P                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     CKLCML06                                                         
*                                                                               
CKLCML10 GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
CKLCML30 LM    R3,R4,STADILST      ANY LOST AD-ID                               
         SR    R4,R3                                                            
         LTR   R4,R4                                                            
         BZ    CKEOJ                                                            
*                                                                               
         SR    R0,R0                                                            
         LR    R1,R4                                                            
         D     R0,=F'12'           DIV BY THE LENGTH OF AN ENTRY                
         LR    R4,R1                                                            
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+3(4),=C'*** '                                                  
         EDIT  (R4),(2,P+7),COMMAS=YES,ZERO=NOBLANK                             
         MVC   P+9(21),=C' LOST AD-ID CODES ***'                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R1,TOTLCMLC                                                      
         AR    R1,R4                                                            
         ST    R1,TOTLCMLC         TOTAL LOST COMMERCIALS                       
*                                                                               
         L     R3,STADILST                                                      
         LA    R5,P                                                             
         MVI   CLMCT,0                                                          
*                                                                               
CKLCML40 DS    0H                                                               
         MVC   0(12,R5),0(R3)                                                   
         LA    R3,12(R3)                                                        
         LA    R5,15(R5)                                                        
         C     R3,ENADILST                                                      
         BE    CKLCML80                                                         
*                                                                               
         LLC   R1,CLMCT                                                         
         LA    R1,1(R1)                                                         
         STC   R1,CLMCT                                                         
         CLI   CLMCT,3                                                          
         BE    CKLCML60                                                         
*                                                                               
CKLCML50 BCT   R4,CKLCML40                                                      
         B     CKLCML80                                                         
*                                                                               
CKLCML60 MVI   CLMCT,0                                                          
         LA    R5,P                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     CKLCML50                                                         
*                                                                               
CKLCML80 GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* NOW CK IF 1 CLIENT REQUEST (QUIT) OR GET NEXT CLIENT *                        
*                                                                               
CKEOJ    OC    BCLT,BCLT           1 CLIENT                                     
         BZ    CLRTBLS              NO                                          
         CLI   OFFICE,0            BY OFFICE                                    
         BNE   CLRTBLS              YES, GET NEXT CLT                           
         B     EXIT                                                             
*                                                                               
* CLEAR ACTIVE COMMERCIAL TABLE AND ACTIVE PATTERN TABLE                        
*                                                                               
CLRTBLS  DS    0H                                                               
         L     RE,STRAPLST         START OF ACTIVE PATTERN LIST                 
         L     RF,ENDAPLST                                                      
         SR    RF,RE                                                            
         LTR   RF,RF                                                            
         BZ    CLR10                                                            
         XCEFL                                                                  
*                                                                               
CLR10    L     RE,STRACLST         ACTIVE CML LIST                              
         L     RF,ENDACLST                                                      
         SR    RF,RE                                                            
         BZ    CLR15                                                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
CLR15    L     RE,STRARLST         ACTIVE REV LIST                              
         L     RF,ENDARLST                                                      
         SR    RF,RE                                                            
         BZ    CLR20                                                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
CLR20    L     RE,STRLCLST         LOST COMMERCIALS LIST                        
         L     RF,ENDLCLST                                                      
         SR    RF,RE                                                            
         BZ    CLR30                                                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
CLR30    L     RE,STRAGLST         ACTIVE PROGRAMS LIST                         
         L     RF,ENDAGLST                                                      
         SR    RF,RE                                                            
         BZ    CLR40                                                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
CLR40    L     RE,STADILST         LOST AD-ID LIST                              
         L     RF,ENADILST                                                      
         SR    RF,RE                                                            
         BZ    CLRX                                                             
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
CLRX     B     LR10                                                             
*                                                                               
* PRINT TOTALS FOR AGENCY/MEDIA (ONLY USED FOR ALL CLT REQUEST) *               
*                                                                               
EOJ      EQU   *                                                                
         BRAS  RE,EOJTOT                                                        
         BRAS  RE,INITNET          SET TO NET                                   
         B     EXIT                                                             
         EJECT                                                                  
*=================================================================              
* DELETE RECORD AND KEY (UNTDIR/UNTFIL)                                         
*=================================================================              
                                                                                
DELREC   NTR1                                                                   
         TM    FTRSW2,TESTSW       TEST RUN                                     
         JO    EXIT                                                             
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         JE    EXIT                                                             
*                                                                               
         L     R6,AIO                                                           
*                                                                               
         CLC   =X'0A61',KEY        IS IT PAT REC                                
         BE    DELREC10                                                         
*                                                                               
         CLC   =X'0A1D',KEY        IS IT REV REC                                
         BE    DELREC10                                                         
*                                                                               
         CLC   =X'0A35',KEY        IS IT COMTEXT                                
         BE    DELREC20                                                         
*                                                                               
         OI    22(R6),X'C0'                                                     
         OI    KEY+20,X'C0'                                                     
         NI    DMOUTBTS,X'FF'-X'02' SET FOR DELETE                              
         GOTO1 PUTREC                                                           
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'UNTDIR',KEY,KEY                        
         OI    DMOUTBTS,X'02'       RESET FOR DELETE                            
         J     EXIT                                                             
*                                                                               
DELREC10 DS    0H                                                               
         OI    34(R6),X'C0'                                                     
         GOTO1 AIOCALL,DMCB,PUTQ+FILQ+XSPQ,AIO                                  
*                                                                               
         OI    KEY+32,X'C0'                                                     
         GOTO1 AIOCALL,DMCB,WRITEQ+DIRQ,0                                       
         J     EXIT                                                             
*                                                                               
DELREC20 DS    0H                                                               
         OI    34(R6),X'C0'                                                     
         OI    KEY+32,X'C0'                                                     
         GOTO1 PUTREC                                                           
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'XSPDIR  ',KEY,KEY,0                    
         J     EXIT                                                             
                                                                                
*=======================================================                        
* DELETE KEY ONLY                                                               
*=======================================================                        
                                                                                
DELKEY   NTR1                                                                   
         TM    FTRSW2,TESTSW       TEST RUN                                     
         BO    EXIT                                                             
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    EXIT                                                             
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+20,X'C0'                                                     
         NI    DMINBTS,X'FF'-X'80' SET FOR DELETE                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'UNTDIR',KEY,KEY                        
         OI    DMINBTS,X'80'       RESET FOR DELETE                             
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY                                               
         OI    DMINBTS,X'08'       PASS DELETED KEY                             
         NI    DMOUTBTS,X'FF'-X'02'                                             
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08' RESET                                        
         OI    DMINBTS,X'02'       RESET FOR DELETE                             
         CLC   KEY(20),KEYSAVE                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
* FIND PRINTABLE PRODUCT *                                                      
*                                                                               
FPRD     LR    R1,R0                                                            
         LA    R0,NCLSTSIZ         MAX COUNT BUG CATCHER                        
         L     RF,ASVNCLST         TABLE OF CLIENT PROD CODES                   
FPRD10   CLI   0(RF),C' '          END OF LIST                                  
         BNH   FPRD30                                                           
         CLC   0(1,R1),3(RF)                                                    
         BE    FPRD20                                                           
         LA    RF,4(,RF)                                                        
         BCT   R0,FPRD10                                                        
         B     FPRD30                                                           
*                                                                               
FPRD20   MVC   DUB(3),0(RF)                                                     
*                                                                               
         BR    RE                                                               
FPRD30   MVC   WORK(3),=C'???'                                                  
         BR    RE                                                               
*                                                                               
* FIND PRINTABLE PRODUCT & SPOT LEN                                             
*                                                                               
FPS      MVC   WORK(7),SPACES                                                   
         LR    R1,R0                                                            
*        LA    RF,SVCLIST                                                       
         LA    R0,NCLSTSIZ         MAX COUNT BUG CATCHER                        
         L     RF,ASVNCLST         TABLE OF CLIENT PROD CODES                   
FPS10    CLI   0(RF),C' '          END OF LIST                                  
         BNH   FPS30                                                            
         CLC   0(1,R1),3(RF)                                                    
         BE    FPS20                                                            
         LA    RF,4(,RF)                                                        
         BCT   R0,FPS10                                                         
         B     FPS30                                                            
*                                                                               
FPS20    MVC   WORK(3),0(RF)                                                    
         B     FPS40                                                            
FPS30    MVC   WORK(3),=C'???'                                                  
*                                                                               
FPS40    LA    RF,WORK+3                                                        
         CLI   WORK+2,C' '                                                      
         BH    FPS44                                                            
         BCTR  RF,0                                                             
FPS44    MVI   0(RF),C'-'                                                       
         LLC   R0,1(R1)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+7(3),DUB                                                    
         CLI   WORK+7,C'0'                                                      
         BNE   FPS46                                                            
         MVC   1(2,RF),WORK+8                                                   
         BR    RE                                                               
FPS46    MVC   1(3,RF),WORK+7                                                   
         BR    RE                                                               
         EJECT                                                                  
* ADD CLT COUNTERS TO AGENCY AND ZERO CLT                                       
*                                                                               
ACT      LA    R0,4                                                             
         SR    R1,R1                                                            
ACT10    L     RF,0(R2)            GET INPUT CTR                                
         A     RF,0(R3) ADD TO OUTPUT                                           
         ST    RF,0(R3)                                                         
         ST    R1,0(R2)            ZERO INPUT                                   
         AR    R2,R0                                                            
         AR    R3,R0                                                            
         BCT   R4,ACT10                                                         
         BR    RE                                                               
*                                                                               
* RESET FILES TO SPOT *                                                         
*                                                                               
INITSPT  MVI   DATADISP+1,24                                                    
         MVI   LKEY+1,13                                                        
         MVI   LSTATUS+1,1                                                      
         BRAS  R1,INITALL                                                       
         DC    CL4'SPT'                                                         
*                                                                               
INITTRF  MVI   DATADISP+1,24       RESET FILES TO TRAFFIC                       
         MVI   LKEY+1,13                                                        
         MVI   LSTATUS+1,1                                                      
         BRAS  R1,INITALL                                                       
         DC    CL4'TRF'                                                         
*                                                                               
* RESET FILES TO NET *                                                          
*                                                                               
INITNET  MVI   DATADISP+1,27                                                    
         MVI   LKEY+1,20                                                        
         MVI   LSTATUS+1,1                                                      
         BRAS  R1,INITALL                                                       
         DC    CL4'UNT'                                                         
*                                                                               
* RESET FILES TO XFILE *                                                        
*                                                                               
INITXSP  MVI   DATADISP+1,42                                                    
         MVI   LKEY+1,32                                                        
         MVI   LSTATUS+1,4                                                      
         BRAS  R1,INITALL                                                       
         DC    CL4'XSP'                                                         
*                                                                               
INITALL  MVC   SYSDIR(3),0(R1)                                                  
         MVC   SYSFIL(3),0(R1)                                                  
         XC    FILENAME,FILENAME                                                
         BR    RE                                                               
*                                                                               
TRACE    TM    FTRSW2,TRCESW       TRACE ON                                     
         BZR   RE                                                               
TRA      NTR1                                                                   
         MVC   P(8),SYSDIR                                                      
         CLI   TRCETYP,3                                                        
         BL    *+10                                                             
         MVC   P(8),SYSFIL                                                      
*                                                                               
         MVC   P+10(4),=C'HIGH'                                                 
         CLI   TRCETYP,1                                                        
         BE    TRA20                                                            
         MVC   P+10(4),=C'SEQ '                                                 
         CLI   TRCETYP,2                                                        
         BE    TRA20                                                            
         MVC   P+10(4),=C'GET '                                                 
         CLI   TRCETYP,3                                                        
         BE    TRA20                                                            
         DC    H'0'                                                             
TRA20    MVC   P+16(4),=C'KEY='                                                 
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,P+20,25,0,0                                      
*                                                                               
         CLC   =X'0A61',KEY        IS IT PAT REC                                
         BE    TRA40                                                            
*                                                                               
         CLC   =X'0A1D',KEY        IS IT REV REC                                
         BE    TRA40                                                            
*                                                                               
TRA30    GOTO1 HEXOUT,DMCB,KEY,P+20,25,0,0                                      
         B     TRA50                                                            
*                                                                               
TRA40    GOTO1 HEXOUT,DMCB,KEY,P+20,20,0,0                                      
         MVC   P+60(6),=C'  D/A='                                               
         GOTO1 HEXOUT,DMCB,KEY+36,P+66,4,0,0                                    
*                                                                               
TRA50    MVC   P+75(20),KEY                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
ATBL     DC    CL12'ABCDEFGHIJKL'  12 LETTERS FOR 12 COMMERCIALS                
REPERMS  DC    CL60'* ERROR * MUST BE RUN OV OR DDS *'                          
OFFLNMS  DC    CL60'* ERROR * OFFICE CODE MUST BE * AND 1 CHAR *'               
OFFERMS  DC    CL60'* ERROR * NO CLIENTS FOR OFFICE *'                          
PRDOFFMS DC    CL60'* ERROR * CLIENT MUST BE ENTERED IF PROD ENTERED *'         
*                                                                               
HEADING  SPROG 0,THRU,5                                                         
         SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H2,3,C'--------------'                                           
         SSPEC H4,3,C'MEDIA'                                                    
         SSPEC H5,3,C'CLIENT'                                                   
         SSPEC H1,30,C'T R A F F I C  P U R G E  L I S T'                       
         SSPEC H2,30,C'-----------------------------------'                     
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,73,PAGE                                                       
         SSPEC H6,73,REQUESTOR                                                  
*                                                                               
* REVISION RECORDS                                                              
*                                                                               
         SPROG 1                                                                
         SSPEC H7,3,C'REVISION RECORDS'                                         
         SSPEC H8,3,C'NET'                                                      
         SSPEC H9,3,C'---'                                                      
         SSPEC H8,9,C'PROGRAM'                                                  
         SSPEC H9,9,C'-------'                                                  
         SSPEC H8,17,C'PERIOD'                                                  
         SSPEC H9,17,C'------'                                                  
         SSPEC H8,26,C'REV'                                                     
         SSPEC H9,26,C'---'                                                     
         SSPEC H8,30,C'PROD'                                                    
         SSPEC H9,30,C'----'                                                    
         SSPEC H8,35,C'ORIG DATE REV DATE INST DATE'                            
         SSPEC H9,35,C'--------- -------- ---------'                            
         SSPEC H8,64,C'REVISION COMMENT'                                        
         SSPEC H9,64,C'----------------'                                        
*                                                                               
* PROGRAM EQUIVALENCY RECORDS                                                   
*                                                                               
         SPROG 2                                                                
         SSPEC H7,3,C'PROGRAM EQUIVALENCY'                                      
         SSPEC H8,3,C'NETWORK'                                                  
         SSPEC H9,3,C'-------'                                                  
         SSPEC H8,11,C'EQUIVALENT PROGRAM'                                      
         SSPEC H9,11,C'-----------------'                                       
         SSPEC H8,36,C'BASE PROGRAM'                                            
         SSPEC H9,36,C'------------'                                            
         SSPEC H8,58,C'START DATE'                                              
         SSPEC H9,58,C'----------'                                              
         SSPEC H8,70,C'END DATE'                                                
         SSPEC H9,70,C'--------'                                                
*                                                                               
* PATTERN RECORDS                                                               
*                                                                               
         SPROG 3                                                                
         SSPEC H7,1,C'PATTERN RECS'                                             
         SSPEC H8,1,C'PRD-LEN'                                                  
         SSPEC H9,1,C'-------'                                                  
         SSPEC H8,9,C'PTR-LEN'                                                  
         SSPEC H9,9,C'-------'                                                  
         SSPEC H8,17,C'CODE'                                                    
         SSPEC H9,17,C'----'                                                    
         SSPEC H8,22,C'FEED'                                                    
         SSPEC H9,22,C'----'                                                    
         SSPEC H8,28,C'NETWORK'                                                 
         SSPEC H9,28,C'-------'                                                 
         SSPEC H8,36,C'PROGRAM'                                                 
         SSPEC H9,36,C'-------'                                                 
         SSPEC H8,44,C'PTTN DATES'                                              
         SSPEC H9,44,C'----------'                                              
         SSPEC H8,59,C'DESCRIPTION'                                             
         SSPEC H9,59,C'-----------'                                             
         SSPEC H8,83,C'REF'                                                     
         SSPEC H9,83,C'---'                                                     
         SSPEC H8,89,C'COMMERCIALS'                                             
         SSPEC H9,89,C'-----------'                                             
*                                                                               
* COMMERCIAL RECORDS                                                            
*                                                                               
         SPROG 4                                                                
         SSPEC H7,3,C'COMMERCIAL RECS'                                          
         SSPEC H8,3,C'COMMERCIAL'                                               
         SSPEC H9,3,C'------------'                                             
         SSPEC H8,17,C'CMML-TITLE'                                              
         SSPEC H9,17,C'---------------'                                         
         SSPEC H8,34,C'SLN'                                                     
         SSPEC H9,34,C'---'                                                     
         SSPEC H8,39,C'TYPE'                                                    
         SSPEC H9,39,C'----'                                                    
         SSPEC H8,46,C'CMML DATES'                                              
         SSPEC H9,46,C'-----------------'                                       
         SSPEC H8,65,C'CLT COMML NO'                                            
         SSPEC H9,65,C'-------------------'                                     
*                                                                               
* SKED UNIT RECORDS                                                             
*                                                                               
         SPROG 5                                                                
         SSPEC H7,3,C'SKED UNIT RECORDS'                                        
         SSPEC H8,3,C'NETWORK'                                                  
         SSPEC H9,3,C'-------'                                                  
         SSPEC H8,13,C'PROGRAM'                                                 
         SSPEC H9,13,C'-------'                                                 
         SSPEC H8,23,C'AIR DATE'                                                
         SSPEC H9,23,C'--------'                                                
         SSPEC H8,34,C'DPC'                                                     
         SSPEC H9,34,C'---'                                                     
         SSPEC H8,39,C'PRD'                                                     
         SSPEC H9,39,C'---'                                                     
         SSPEC H8,47,C'PTR'                                                     
         SSPEC H9,47,C'---'                                                     
         SSPEC H8,59,C'COMMERCIALS'                                             
         SSPEC H9,59,C'-----------'                                             
         DC    X'00'               END MARKER FOR SSPEC                         
         EJECT                                                                  
* GET OFFICE FOR SINGLE CLIENT LIMITED ACCESS                                   
*                                                                               
GOFF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),T216FFD+6  LIMITED ACCESS CLT                           
*                                                                               
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BE    *+6                                                              
         DC    H'0'                WHAT'S WRONG                                 
*                                                                               
         CLC   KEY+1(1),BAGYMD                                                  
         BE    *+6                                                              
         DC    H'0'                BETTER BE THERE                              
*                                                                               
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
*                                                                               
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   LAOFFICE,CTRAFOFC   USE TRAFFIC OFFICE                           
         CLI   LAOFFICE,C'A'       IF THERE IS ONE                              
         BNL   *+10                                                             
         MVC   LAOFFICE,COFFICE    USE MEDIA OFFICE                             
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         XIT1                                                                   
         DROP  R7                                                               
         EJECT                                                                  
*==============================================================                 
* BUILD ACTIVE COMMERCIAL TABLE FROM UNIT RECORD DATA AT 0(R5)                  
*==============================================================                 
                                                                                
ACTU     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVACTCML,SVACTCML   CLEAR SAVE ACTUAL CML AREA                   
*                                                                               
         OC    0(8,R5),0(R5)        ANY COMMERCIAL                              
         BZ    ACTUX                                                            
         CLC   0(2,R5),=X'5C00'      DELETED COMML                              
         BE    ACTUX                                                            
         CLC   =C'REASSIGN',0(R5)                                               
         BE    ACTUX                                                            
         CLC   =C'VIGNETTE',0(R5)    VIGNETTE                                   
         BE    ACTUX                                                            
*                                                                               
         BRAS  RE,FCDTE            FIND CML REL/RCL DATES                       
         TM    FOUNDSW,ACTCSW      ACTIVE CML FOUND TO ADD TO TBLE              
         BZ    ACTUX               NO, DO NOT ADD TO TABLE                      
*                                                                               
         BRAS  RE,FCLEN            FIND CML LEN                                 
         TM    FOUNDSW,CMLFSW                                                   
         BZ    ACTUX               CML RECORD NOT FOUND                         
*                                                                               
ACTU05   L     RE,STRACLST                                                      
         C     RE,ENDACLST         AT THE END OF LIST                           
         BE    ACTU20                                                           
*                                                                               
ACTU10   CLC   0(12,R5),0(RE)       SAME CML                                    
         BNE   ACTU15                                                           
*                                                                               
         OC    SVACTCML,SVACTCML   ARE WE DOING COVER CMLS                      
         BZ    ACTUX               YES, DONE ACTUAL CMLS BEFORE                 
*                                                                               
         LA    R5,12(R5)           GET NEXT ACTUAL CML                          
         B     ACTU30                                                           
*                                                                               
ACTU15   OC    0(12,RE),0(RE)       EMPTY ENTRY IN TABLE                        
         BZ    ACTU20               YES                                         
*                                                                               
         LA    RE,12(RE)            BUMP IN CML TABLE                           
         C     RE,STRARLST         OVERFLOW TO REV LIST                         
         BL    *+6                                                              
         DC    H'0'                                                             
         B     ACTU10                                                           
*                                                                               
ACTU20   MVC   0(12,RE),0(R5)        AND CML                                    
         LA    RE,12(RE)                                                        
         ST    RE,ENDACLST                                                      
         C     RE,STRARLST         SEE IF RUNNING INTO REV LIST                 
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    SVACTCML,SVACTCML   DID WE LOOK FOR ACTUAL CMLS                  
         BZ    ACTU25               NO                                          
*                                                                               
         LA    R5,12(R5)            NEXT CML                                    
         B     ACTU30                                                           
*                                                                               
ACTU25   BRAS  RE,FACTCML           FIND ACTUAL CMLS IF ANY (SVACTCML)          
*                                                                               
         OC    SVACTCML,SVACTCML   ANY ACTUAL CMLS                              
         BZ    ACTUX                NO, DONE                                    
*                                                                               
         LA    R5,SVACTCML                                                      
ACTU30   CLI   0(R5),0             END OF CML LIST                              
         BE    ACTUX                YES, DONE                                   
*                                                                               
         BRAS  RE,FCDTE            FIND CML REL/RCL DATES                       
         TM    FOUNDSW,ACTCSW      ACTIVE CML FOUND TO ADD TO TBLE              
         BZ    ACTU35              NO, DO NOT ADD TO TABLE                      
*                                                                               
         BRAS  RE,FCLEN            FIND CML LEN                                 
         TM    FOUNDSW,CMLFSW      CML REC FOUND                                
         BO    ACTU05                                                           
*                                                                               
ACTU35   LA    R5,12(R5)                                                        
         B     ACTU30                                                           
ACTUX    XIT1                                                                   
         EJECT                                                                  
* DELETE ANY REVISION RECORDS FOR THIS CLIENT *                                 
*                                                                               
REV      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    *+12                                                             
         TM    FTRSW1,REVSW        DO REVISION RECORDS                          
         BZ    REVX10               NO, CK PROG EQUIV REC                       
*                                                                               
         LHI   R2,TSARBLK-SYSD    GET ADDR OF TSAR TABLE                        
         AR    R2,R9                                                            
         USING TSARD,R2                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING REVXKEY,R4                                                       
         MVC   REVXKID,=X'0A1D'                                                 
         MVC   REVXKAM,BAGYMD      AGY/MED                                      
         MVC   REVXKCLT,SVBCLT     CLIENT                                       
*                                                                               
REV05    GOTO1 AIOCALL,DMCB,HIGHQ+DIRQ+XSPQ,0                                   
*                                                                               
REV10    CLC   KEY(5),KEYSAVE      SAME A/M/CLT                                 
         BNE   REVX                 NO                                          
*                                                                               
         L     R1,REVRECR          CT REV RECORDS READ                          
         LA    R1,1(,R1)                                                        
         ST    R1,REVRECR                                                       
*                                                                               
         OC    QPRD,QPRD           PROD ENTERED                                 
         BZ    REV12                NO                                          
*                                                                               
         CLC   REVXKPRD,QPRD                                                    
         BNE   REV40                                                            
*                                                                               
REV12    DS    0H                                                               
         TM    REVXKPER,X'80'      THIS WEEKLY REV REC                          
         BZ    REV14                NO                                          
*                                                                               
         CLC   EXPDTCMP,REVXKPER   IF REV PER AFTER PURGE DATE                  
         BL    REV39               DO NOT PURGE                                 
         B     REV15                                                            
*                                                                               
REV14    DS    0H                                                               
         CLC   EXPDATE(2),REVXKPER IF REV PER AFTER PURGE DATE                  
         BL    REV39               DO NOT PURGE                                 
*                                                                               
* SEE IF REVISION RECORD IS ACTIVE (IN ACT REV TABLE)                           
*                                                                               
REV15    LA    R5,ELEM+3                                                        
         USING ARLIST,R5                                                        
*                                                                               
         LA    RF,1                                                             
         STCM  RF,7,TSRCOUNT                                                    
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R0,ELEM                                                          
         ST    R0,TSAREC           SET ADDRESS OF RECORD                        
*                                                                               
REV16    DS    0H                                                               
         STCM  RF,7,ELEM            KEY (REC NUM)                               
         MVI   TSOFFACT,TSARDH      SET TSAROFF GET RECORD                      
         GOTO1 TSAROFF,(R2)                                                     
         TM    TSERRS,X'90'        END OF FILE                                  
         BNZ   REV18                YES                                         
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   REVXKNET(ARLISTLN),ARLNET SAME REV ENTRY                         
         BE    REV40                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,TSRCOUNT                                                    
         LA    RF,1(,RF)                                                        
         STCM  RF,7,TSRCOUNT                                                    
         B     REV16                                                            
*                                                                               
REV18    DS    0H                  CHK START AND END DATE                       
         MVI   WORK+2,0                                                         
         MVC   WORK(2),REVXKPER                                                 
         MVI   WORK+52,0                                                        
         MVC   WORK+50(2),REVXKPER                                              
         TM    REVXKPER,X'80'      THIS WEEKLY REV REC                          
         BZ    REV20                NO                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,WORK),(3,WORK+50)                                 
*                                                                               
REV20    CLC   EXPDATE,WORK+50     IS THIS EXPIRED RECORD                       
         BL    REV39                NO                                          
*                                                                               
         DROP  R4                                                               
*                                                                               
* PRINT OUT REVISION RECORD AND DELETE *                                        
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 AIOCALL,DMCB,GETQ+FILQ+XSPQ,0(R6)                                
*                                                                               
         L     R1,REVRECS          CT TO BE DELETED                             
         LA    R1,1(,R1)                                                        
         ST    R1,REVRECS                                                       
*                                                                               
         USING REVXKEY,R6                                                       
*                                                                               
         MVC   PRPNET,REVXKNET     PRINT NETWORK                                
         MVC   PRPROG,REVXKPRG      AND PROGRAM                                 
*                                                                               
         CLI   REVXKNUM,0                                                       
         BE    REV23                                                            
         EDIT  (B1,REVXKNUM),(3,PRREV),ZERO=NOBLANK                             
         B     *+8                                                              
REV23    MVI   PRREV+2,C'0'                                                     
*                                                                               
         OC    REVXKPRD,REVXKPRD                                                
         BZ    REV25                                                            
         MVC   PRPRD,REVXKPRD                                                   
         DROP  R6                                                               
*                                                                               
REV25    DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,WORK+50),(5,PREDATE)                              
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         MVI   DATADISP+1,42                                                    
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING REVREVEL,R6                                                      
         OC    REVADATE,REVADATE                                                
         BZ    REV30                                                            
         GOTO1 DATCON,DMCB,(3,REVADATE),(8,PRADATE)                             
*                                                                               
REV30    DS    0H                                                               
         OC    REVRDATE,REVRDATE                                                
         BZ    REV32                                                            
         GOTO1 DATCON,DMCB,(3,REVRDATE),(8,PRRDATE)                             
*                                                                               
REV32    DS    0H                                                               
         OC    REVIDATE,REVIDATE                                                
         BZ    REV34                                                            
         GOTO1 DATCON,DMCB,(3,REVIDATE),(8,PRIDATE)                             
*                                                                               
REV34    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R6,AIO                                                           
         USING REVCMTEL,R6                                                      
*                                                                               
         MVI   ELCODE,X'40'        COMMENT ELEMENT                              
         MVI   DATADISP+1,42                                                    
         BRAS  RE,GETEL                                                         
         BNE   REV38                                                            
*                                                                               
REV36    MVC   PRCMT,REVCMT                                                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BRAS  RE,NEXTEL                                                        
         BE    REV36                                                            
*                                                                               
REV38    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    REV40                                                            
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    REV40                                                            
*                                                                               
         BAS   RE,DELREV           GO DELETE RECORD AND KEY                     
*                                                                               
         L     R1,REVRECDL                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,REVRECDL                                                      
*                                                                               
         B     REV40                                                            
*                                                                               
* REVISION DATES ARE AFTER PURGE DATES FOR THIS PROGRAM                         
*                                                                               
REV39    DS    0H                                                               
         LA    R4,KEY                                                           
         USING REVXKEY,R4                                                       
*                                                                               
         MVC   REVXKPER,=X'FFFF'   JUMP TO NEXT PROGRAM                         
         B     REV05                                                            
*                                                                               
REV40    MVC   KEYSAVE,KEY                                                      
*NOP     GOTO1 SEQ                                                              
         MVC   MYKEYSV,KEY                                                      
         GOTO1 AIOCALL,DMCB,SEQQ+DIRQ+XSPQ,0                                    
         MVC   KEYSAVE,MYKEYSV                                                  
         B     REV10                                                            
         DROP  R6                                                               
*                                                                               
* END OF REVISIONS FOR CLIENT, PRINT TOTALS, ADD TO MEDIA TOTALS *              
*                                                                               
REVX     LA    R2,REVRECR                                                       
         LA    R3,=C'REV'                                                       
         BRAS  RE,RECTOT                                                        
*                                                                               
         LA    R2,REVRECR                                                       
         LA    R3,AGYCTRS+REVRECR-COUNTERS                                      
         LA    R4,3                                                             
         BAS   RE,ACTR             ADD CLIENT TO MEDIA CTRS                     
*                                                                               
REVX10   XIT1                                                                   
*                                                                               
* ADD CLT COUNTERS TO AGENCY AND ZERO CLT                                       
*                                                                               
ACTR     LA    R0,4                                                             
         SR    R1,R1                                                            
ACTR10   L     RF,0(R2)            GET INPUT CTR                                
         A     RF,0(R3) ADD TO OUTPUT                                           
         ST    RF,0(R3)                                                         
         ST    R1,0(R2)            ZERO INPUT                                   
         AR    R2,R0                                                            
         AR    R3,R0                                                            
         BCT   R4,ACTR10                                                        
         BR    RE                                                               
*                                                                               
* DELETE RECORD AND KEY (UNTDIR/UNTFIL)                                         
*                                                                               
DELREV   NTR1                                                                   
         TM    FTRSW2,TESTSW       TEST RUN                                     
         BO    REVX10                                                           
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    REVX10                                                           
*                                                                               
         L     R6,AIO                                                           
*                                                                               
         OI    34(R6),X'C0'                                                     
         OI    KEY+32,X'C0'        CLEARED AFTER AIO CALL (WHY?)                
         GOTO1 AIOCALL,DMCB,PUTQ+FILQ+XSPQ,AIO                                  
*                                                                               
         OI    KEY+32,X'C0'        SET IT AGAIN                                 
         GOTO1 AIOCALL,DMCB,WRITEQ+DIRQ,0                                       
         B     REVX10                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* VALIDATE KEY ROUTINE                                                          
*==============================================================                 
                                                                                
VKEY     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   OFFLINE,C'Y'        DON'T CHECK IF OFFLINE                       
         BE    VK00                                                             
         CLI   1(RA),C'*'          THIS A DDS TERMINAL                          
         BNE   INVACTER             NO, PURGE NOT ALLOWED                       
VK00     LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
VK10     LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT(6),BCLT        CLEAR BCLT, BPRD, BSLN, BPRD2, BSLN2         
         XC    QPRD,QPRD           CLEAR QPRD                                   
         XC    QPRD2,QPRD2          AND QPRD2                                   
         XC    SVBCLT,SVBCLT                                                    
         MVI   OFFICE,0                                                         
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK20                                                             
         CLI   8(R2),C'*'          BY OFFICE                                    
         BE    VK14                                                             
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT                                                      
         B     VK20                                                             
VK14     CLI   5(R2),3                                                          
         BH    OFFLNERR                                                         
         TM    WHEN,X'38'         TEST REPORT OPTION                            
         BZ    REPERR                                                           
*                                                                               
* VALIDATE AND CONVERT TO 1 BYTE IF NEEDED                                      
*                                                                               
         XC    WORKSEC,WORKSEC                                                  
         LA    R3,WORKSEC                                                       
         USING OFFICED,R3                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,9(R2)       2 CHAR OFFICE CODE                           
         CLI   5(R2),3                                                          
         BE    *+10                                                             
         OC    OFCOFC2,SPACES      1 CHAR OFFICE PADDED W/SPACE                 
*                                                                               
         OC    T216FFD+4(2),T216FFD+4  NEW SECURITY                             
         BNZ   VK18                JUST CONVERT, DO NOT VALIDATE                
*                                                                               
         OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    VK15                                                             
*                                                                               
         CLI   T216FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BE    VK15                VALIDATE AND CONVERT                         
*                                                                               
         CLI   T216FFD+6,C'$'      TEST OFFICE LIST                             
         BE    VK15                                                             
*                                                                               
*                                  SINGLE CLIENT ACCESS                         
         MVI   LAOFFICE,0          INIT LIMITED ACCESS OFFICE                   
*                                                                               
         BRAS  RE,GOFF             GET OFFICE FOR THIS CLIENT                   
         B     VK18                                                             
*                                                                               
VK15     MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCLMT(4),T216FFD+6                                              
*                                                                               
VK18     XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'2',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   OFFLNERR                                                         
*                                                                               
         TM    OFCINDS,OFCINOLA    NO OFFICE LIMIT ACCESS REC                   
         BO    VK16                                                             
*                                                                               
         MVC   OFFICE,OFCOFC       SAVE 1 BYTE OFFICE CODE                      
         B     *+10                                                             
VK16     MVC   OFFICE,9(R2)                                                     
*                                                                               
         BRAS  RE,VOFF             GO VALIDATE OFFICE                           
         BNE   OFFERR                                                           
*                                                                               
         DROP  R3                                                               
*                                                                               
         MVC   SVBCLT,BCLT                                                      
*                                                                               
VK20     LA    R2,TRAPRDH          PRODUCT                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK30                NO                                           
         OC    BCLT,BCLT                                                        
         BZ    MISSCLT                                                          
         CLI   TRACLT,C'*'         BY OFFICE                                    
         BE    PRDOFFER                                                         
*                                                                               
         GOTO1 VALIPRD                                                          
         MVC   QPRD,WORK                                                        
*                                                                               
VK30     CLI   TRAPTRH+5,0         ANY ENTRY                                    
         BE    VK40                NO                                           
         CLI   TRAPRDH+5,0         PRODUCT ENTERED                              
         BE    PRDOFFER                                                         
         LA    R2,TRAPTRH          PARTNER PRODUCT                              
*                                                                               
         GOTO1 VALIPRD                                                          
         MVC   QPRD2,WORK                                                       
*                                                                               
VK40     LA    R2,TRAEDTH          END DATE                                     
         BRAS  RE,VEDT                                                          
*                                                                               
         LA    R2,TRAFLTRH                                                      
         BRAS  RE,VFTR             GO VALIDATE FILTERS                          
VKX      XIT1                                                                   
*                                                                               
*                                                                               
OFFLNERR L     R1,=A(OFFLNMS)                                                   
         B     ERREXIT                                                          
REPERR   L     R1,=A(REPERMS)                                                   
         B     ERREXIT                                                          
OFFERR   L     R1,=A(OFFERMS)                                                   
         B     ERREXIT                                                          
PRDOFFER L     R1,=A(PRDOFFMS)                                                  
         LA    R2,TRACLTH                                                       
ERREXIT  A     R1,SPTR95RR                                                      
         MVC   CONHEAD,0(R1)                                                    
         GOTO1 ERREX2                                                           
*                                                                               
MISSCLT  LA    R2,TRACLTH                                                       
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
INVACTER MVI   ERROR,INVACT                                                     
         LA    R2,CONRECH                                                       
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* FIND ACTUAL COMMERCIAL IF ANY                                                 
*                                                                               
FACTCML  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,INITSPT         SET FOR SPTFILE                               
*                                                                               
         LA    R1,SVACTCML                                                      
         LA    R0,4                4 ACTUAL CMLS                                
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
*                                                                               
         MVC   SVELCODE,ELCODE                                                  
         MVI   ELCODE,X'60'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FACMLX              CC NE                                        
*                                                                               
         USING CMLACTEL,R6                                                      
*                                                                               
FACML10  MVC   0(12,R1),SPACES                                                  
         LLC   RE,1(R6)                                                         
         AHI   RE,-3                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),CMLACTID    MOVE IN CML                                  
*                                                                               
         LA    R1,12(R1)                                                        
         BCT   R0,FACMLX                                                        
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    FACML10                                                          
         CR    RB,RB               SET CC EQ                                    
*                                                                               
FACMLX   DS    0H                                                               
         MVC   ELCODE,SVELCODE                                                  
         BRAS  RE,INITNET          RESTORE TO UNTFILE                           
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* GET CUT-INS IF ANY                                                            
*=============================================================                  
                                                                                
GCUT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* NEED TO GET UNIT ROTATION FIRST                                               
*                                                                               
         MVI   SVROT,0             INIT UNIT ROTATION                           
         XC    WORK+40(20),WORK+40                                              
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GCUT50                                                           
*                                                                               
         USING NUSDREL,R6                                                       
         MVC   SVROT,NUSDROT       SAVE UNIT ROTATION                           
         DROP  R6                                                               
*                                                                               
* DEVELOP UNIT END DATE FROM START DATE PLUS ROTATION                           
*                                                                               
         GOTO1 DATCON,DMCB,(2,SVKDATE),(0,WORK+40)                              
         GOTO1 GETDAY,DMCB,WORK+40,WORK+46                                      
         LLC   RF,0(R1)            GET DAY OF WEEK                              
*                                                                               
         LLC   R0,SVROT                                                         
         SLL   R0,25                                                            
         SR    R1,R1               CLEAR COUNTER                                
*                                                                               
         LTR   RF,RF                                                            
         BZ    GCUT30                                                           
         SLL   R0,1                ONLY COUNT DAYS AFTER NUKDATE                
         BCT   RF,*-4                                                           
*                                                                               
GCUT30   LTR   R0,R0               COUNT UNTIL NO DAYS LEFT                     
         BZ    GCUT40                                                           
         SLL   R0,1                                                             
         BCT   R1,GCUT30                                                        
*                                                                               
GCUT40   LPR   R0,R1                                                            
*                                                                               
GCUT50   GOTO1 DATCON,DMCB,(2,SVKDATE),(0,WORK+40) UNIT DATE                    
*                                                                               
         CLI   SVROT,0             ANY ROTATION                                 
         BE    GCUT60                                                           
*                                                                               
         GOTO1 ADDAY,(R1),WORK+40,WORK+46,(R0)                                  
         GOTO1 DATCON,(R1),(0,WORK+46),(3,WORK+52) PLUS ROTATION                
*                                                                               
GCUT60   GOTO1 (RF),(R1),(0,WORK+40),(3,WORK+46)  UNIT DATE                     
*                                                                               
         MVC   SVSTDATE,WORK+46    SAVE UNIT START DATE                         
         MVC   SVENDATE,SVSTDATE   PRESET END DATE                              
         CLI   SVROT,0             ANY ROTATION                                 
         BE    *+10                                                             
         MVC   SVENDATE,WORK+52     END DATE (INCLUDES ROTATION)                
*                                                                               
* SEE IF ANY CUTIN COMML ELEM                                                   
*                                                                               
         L     R6,AIO1                                                          
         XC    BLOCK(256),BLOCK    CLEAR AREA FOR CUTIN COMMLS                  
*                                                                               
         MVI   ELCODE,X'17'        CUT-IN COMMERCIAL ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   GCUTX                DONE                                        
*                                                                               
         LLC   RF,1(R6)                                                         
         BCTR  RF,0                LESS 1 FOR MOVE                              
         EX    RF,GCUTMVC                                                       
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GCUTX                                                            
         USING NUSPREL,R6                                                       
*                                                                               
GCUT70   DS   0H                                                                
         CLI   NUSPRTYP,C'U'       ONLY WANT CUTINS                             
         BNE   GCUT90                                                           
*                                                                               
         CLI   NUSPRLEN,NUSPRLN1   BYPASS OLD ELEMS                             
         BE    GCUT90                                                           
*                                                                               
         CLI   NUSPRLEN,NUSPRLN4   3 CHAR PROD ELEM                             
         BL    GCUT90                                                           
         OC    NUSPRTPC,NUSPRTPC                                                
         BZ    GCUT90               GET NEXT ELEM                               
*                                                                               
         OC    NUSPRCIS,NUSPRCIS   ANY CUTIN STATION?                           
         BZ    GCUT90               NO                                          
*                                                                               
         CLI   NUSPRCMI,0          ANY COMML?                                   
         BE    GCUT90               NO                                          
*                                                                               
         MVC   SVPROD,NUSPRTPC     MOVE PRODUCT CODE                            
         LLC   RF,NUSPRCMI         GET POINTER TO COMML                         
         BCTR  RF,0                                                             
         SLL   RF,3                TIMES 8                                      
         LA    RE,BLOCK+2(RF)                                                   
         MVC   SVCML1,0(RE)                                                     
         OC    SVCML1,SVCML1       THERE HAD BETTER BE SOMETHING                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   SVADID1(8),SVCML1                                                
         MVC   SVADID1+8(4),SPACES                                              
*                                                                               
GCUT80   DS   0H                   READ DIR TO SEE IF ISCI OR ADID              
         BRAS  RE,INITSPT          SET  TO SPOT                                 
*                                                                               
         XC    KEY(13),KEY                                                      
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=XL2'0A21'                                                
         MVC   CMLKAM(1),BAGYMD                                                 
         MVC   CMLKCLT,SVBCLT                                                   
         MVC   CMLKCML,SVCML1                                                   
*                                                                               
         MVI   ADIDFLAG,C'N'                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GCUT82                                                           
         TM    KEY+13,CMLKSTA_PCKD   TEST KEY HAS ADID                          
         BO    GCUT84                                                           
         B     GCUT86                                                           
*                                                                               
GCUT82   MVC   KEYSAVE,KEY         RESTORE                                      
         MVC   KEY(2),=X'0AC1'     TRY FOR ADID                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GCUT90                                                           
*                                                                               
GCUT84   MVI   ADIDFLAG,C'Y'                                                    
         GOTO1 VTRPACK,DMCB,(C'U',KEY+5),SVADID1                                
*                                                                               
GCUT86   LA    R5,SVADID1                                                       
         BRAS  RE,ACTU             ADD TO ACTIVE CML TABLE                      
*                                                                               
GCUT90   BRAS  RE,NEXTEL                                                        
         BE    GCUT70                                                           
*                                                                               
GCUTX    DS    0H                                                               
         BRAS  RE,INITNET          SET FROM SPOT TO NET                         
         XIT1                                                                   
*                                                                               
GCUTMVC  MVC   BLOCK(0),0(R6)                                                   
         EJECT                                                                  
*====================================================================           
* R5 POINTS TO 8 CHAR CMML CODE IN START OF 12 BYTE ADID FIELD                  
* IF COMMERCIAL IS NATIVELY ISCI, NATIVE CODE IS RETURNED AT 0(R5)              
* IF COMMERICAL IS NATIVELY ADID, UNPACKED ADID IS RETURNED IN WORK             
*====================================================================           
                                                                                
GETISCI  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,INITSPT         CHANGE TO SPOT                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AC1'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),SVBCLT                                                  
         MVC   KEY+5(8),0(R5)                                                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GISC10                                                           
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         TM    15(R6),X'01'        TEST PACKED CMML IN KEY                      
         BZ    GISC02                                                           
         MVI   ADIDFLAG,C'Y'                                                    
         GOTO1 VTRPACK,DMCB,(C'U',5(R6)),(R5)                                   
         B     GISC04                                                           
*                                                                               
GISC02   MVC   0(8,R5),5(R6)      RETURN ISCI CMML OVER INPUT                   
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
GISC04   BRAS  RE,INITNET          SET TO NET                                   
         B     GISCX                                                            
*                                                                               
* ADD AD-ID TO THE NOT FOUND COMMERCIAL LIST                                    
*                                                                               
GISC10   DS    0H                                                               
         GOTO1 VTRPACK,DMCB,(C'U',0(R5)),WORK                                   
         LM    RE,RF,STADILST                                                   
*                                                                               
GISC20   CR    RE,RF               AT END OF LIST                               
         BE    GISC30                                                           
         BL    *+6                                                              
         DC    H'0'                                                             
         CLC   WORK(12),0(RE)      EQUAL TO THIS CML                            
         BE    GISC50                                                           
         LA    RE,12(RE)                                                        
         B     GISC20                                                           
*                                                                               
GISC30   MVC   0(12,RE),WORK                                                    
         LA    RF,12(RF)                                                        
         ST    RF,ENADILST                                                      
         C     RF,MAXADSZ          SEE IF EXCEEDED MAX TABLE SIZE               
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GISC50   XC    0(8,R5),0(R5)       CLEAR INPUT CMML                             
*                                                                               
GISCX    DS    0H                                                               
         MVC   AIO,AIO1                                                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* BUILD LIST OF COMMERCIALS IN INVOICE RECORDS - CAN'T PURGE THEM               
*=================================================================              
                                                                                
BLDINV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,INITXSP                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING SNVKEYD,R4                                                       
         MVI   SNVKTYPE,SNVKTYPQ                                                
         MVI   SNVKSUB,SNVKSUBQ                                                 
         MVC   SNVKAM,BAGYMD                                                    
         MVC   SNVKCLT,SVBCLT                                                   
         GOTO1 HIGH                                                             
*                                                                               
INV10    MVC   MYKEYSV,KEY         SAVE INVOICE KEY                             
         CLC   KEY(5),KEYSAVE                                                   
         BNE   INV40                                                            
         L     R1,INVRECR          RECS READ                                    
         LA    R1,1(R1)                                                         
         ST    R1,INVRECR                                                       
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
         USING SNVCMEL,R6                                                       
INV20    BRAS  RE,NEXTEL                                                        
         BNE   INV30                                                            
*                                                                               
         BRAS  RE,ACLI             GO ADD TO ACTIVE CML LIST                    
         B     INV20                                                            
*                                                                               
INV30    BRAS  RE,INITXSP                                                       
         MVC   KEY,MYKEYSV         RESTORE INVOICE KEY                          
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                DUMMY HI FOR SEQ                             
         GOTO1 SEQ                                                              
         B     INV10                                                            
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
*==============================================================                 
* AT END OF INVOICES FOR CLIENT, SORT ACTIVE                                    
* COMMERCIAL LIST AND PRINT TOTALS                                              
*==============================================================                 
                                                                                
INV40    OC    INVRECR,INVRECR     ANY READ                                     
         BZ    INV44                                                            
         MVI   FORCEHED,C'N'                                                    
*                                                                               
INV42    LM    R2,R3,STRACLST                                                   
         MVI   0(R3),X'FF'                                                      
         CR    R2,R3               ANY ACTIVE COMMERCIALS FOUND                 
         BE    INV44                NO                                          
         SR    R3,R2                                                            
         SR    R0,R0                                                            
         LR    R1,R3                                                            
         D     R0,=F'12'           DIV BY THE LENGTH OF AN ENTRY                
         LR    R3,R1                                                            
*                                                                               
         GOTO1 XSORT,DMCB,(R2),(R3),12,12,0                                     
*                                                                               
INV44    LA    R2,INVRECR                                                       
         LA    R3,=C'INV'                                                       
         BRAS  RE,RECTOT                                                        
         J     EXIT                                                             
         EJECT                                                                  
*===========================================================---                 
* ADD INVOICE COMMERCIAL TO ACTIVE COMMERCIAL LIST *                            
* NOTE ALL COMMERCIALS ON INVOICE RECORDS ARE 8-12 CHAR                         
*===========================================================---                 
                                                                                
ACLI     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,2(R6)                                                         
         LLC   R0,1(R6)                                                         
         SRL   R0,4                DIVIDE BY 16                                 
*                                                                               
         MVC   WORK(12),7(R6)      8-12 CHAR CML                                
         OC    WORK(12),SPACES     BLANK PADDED                                 
*                                                                               
         CLI   WORK+8,X'40'        IF 9TH CHAR IS NOT SPACE                     
         BNE   ACLI02              COULD BE ADID                                
*                                                                               
         CLI   WORK+7,X'40'        IF 8TH CHAR IS SPACE                         
         BE    ACLI20              THEN NOT A TRAFFIC CML (<8CHAR)              
*                                                                               
ACLI02   GOTO1 VTRPACK,DMCB,(C'P',WORK),DUB                                     
         BE    ACLI04                                                           
         CLI   WORK+8,X'40'        IF 9TH CHAR IS NOT SPACE                     
         BNE   ACLI20              NOT VALID FOR ADID, DONE                     
*                                                                               
         LA    R5,WORK             8 CHAR WITH SPECIAL CHARS                    
         B     *+12                                                             
ACLI04   LA    R5,DUB                                                           
         OI    FOUNDSW,PCMLSW      PACKED CML                                   
*                                                                               
         BRAS  RE,FCDTE            CHK CML DATES                                
         NI    FOUNDSW,X'FF'-PCMLSW                                             
         TM    FOUNDSW,ACTCSW      ACTIVE CML FOUND, ADD TO TBLE                
         BZ    ACLI20              NO, DO NOT ADD TO TABLE                      
*                                                                               
         BRAS  RE,ACLADD                                                        
*                                                                               
ACLI06   BRAS  RE,INITTRF                                                       
         BRAS  RE,FACTCML          FIND ACTUAL CMLS FOR THIS CML                
         BRAS  RE,INITXSP                                                       
*                                                                               
         LA    R0,4                MAX ACTUALS                                  
         LA    R1,SVACTCML                                                      
*                                                                               
ACLI08   OC    0(12,R1),0(R1)                                                   
         BZ    ACLI20                                                           
         MVC   WORK(12),0(R1)                                                   
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'P',WORK),DUB                                     
         BE    ACLI12                                                           
         CLI   WORK+8,X'40'        IF 9TH CHAR IS NOT SPACE                     
         BNE   ACLI16              NOT VALID FOR ADID, DONE                     
*                                                                               
         LA    R5,WORK             8 CHAR WITH SPECIAL CHARS                    
         B     *+12                                                             
ACLI12   LA    R5,DUB                                                           
         OI    FOUNDSW,PCMLSW      PACKED CML                                   
*                                                                               
         BRAS  RE,FCDTE            CHK CML DATES                                
         NI    FOUNDSW,X'FF'-PCMLSW                                             
         TM    FOUNDSW,ACTCSW      ACTIVE CML FOUND, ADD TO TBLE                
         BZ    *+8                 NO, DO NOT ADD TO TABLE                      
         BRAS  RE,ACLADD                                                        
*                                                                               
ACLI16   LA    R1,12(R1)                                                        
         BCT   R0,ACLI08                                                        
*                                                                               
ACLI20   XIT1                                                                   
*                                                                               
ACLADD   NTR1                                                                   
*                                                                               
         LM    RE,RF,STRACLST                                                   
*                                                                               
ACLADD2  CR    RE,RF               AT END OF LIST                               
         BE    ACLADD4                                                          
         CLC   WORK(12),0(RE)      EQUAL TO THIS CML                            
         BE    ACLADDX                                                          
         LA    RE,12(RE)                                                        
         B     ACLADD2                                                          
                                                                                
ACLADD4  MVC   0(12,RE),WORK                                                    
         LA    RE,12(RE)                                                        
         ST    RE,ENDACLST                                                      
         C     RE,MAXACLST         SEE IF EXCEEDED MAX TABLE SIZE               
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ACLADDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* PURGE COMMERCIAL RECORDS                                                      
*===========================================================                    
                                                                                
DELCML   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,INITSPT          SET TO SPOT                                  
*                                                                               
         MVI   RCSUBPRG,4                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   FTRSW1,0            IF NO SELECTIONS, DO ALL                     
         BE    *+12                                                             
         TM    FTRSW1,CMLSW        DO COMMERCIALS                               
         BZ    DELCMLX              NO, CHECK LOST CMLS                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,SVBCLT                                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
DELC10   MVI   SVCMLSTA,0                                                       
         CLC   KEY(5),KEYSAVE                                                   
         BNE   DELCX                                                            
*                                                                               
         MVC   SVKEY,KEY                                                        
         OC    CMLKCML,CMLKCML     CMML SEQ REC                                 
         BZ    DELC40               YES                                         
*                                                                               
         CLC   CMLKCML,=8C'9'      PROD HOUSE REC                               
         BE    DELC40               YES                                         
*                                                                               
         L     R1,CMLRECR          RECS READ                                    
         LA    R1,1(,R1)                                                        
         ST    R1,CMLRECR                                                       
*                                                                               
         MVC   SVCML,CMLKCML                                                    
         MVC   WORK(8),CMLKCML                                                  
         MVC   WORK+8(4),SPACES                                                 
         TM    KEY+13,CMLKSTA_PCKD                                              
         BZ    DELC10X                                                          
         GOTO1 VTRPACK,DMCB,(C'U',CMLKCML),WORK                                 
*                                                                               
DELC10X  LM    RE,RF,STRACLST      START OF ACTIVE COMML LIST                   
*                                                                               
DELC12   CR    RE,RF               END OF THE TABLE                             
         BE    DELC15                                                           
*                                                                               
         CLC   WORK(12),0(RE)       SAME CML                                    
         BE    DELC40                                                           
*                                                                               
         LA    RE,12(RE)                                                        
         B     DELC12                                                           
*                                                                               
* CHECK COMMERCIAL DATES                                                        
*                                                                               
DELC15   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    DELC20                                                           
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
*                                                                               
DELC20   CLC   EXPDATE,CMLRCL      RECALL BEFORE PURGE DATE                     
         BL    DELC40                                                           
         CLC   EXPDATE,CMLRLSE     RELEASE BEFORE PURGE DATE                    
         BL    DELC40                                                           
*                                                                               
* CHECK IF EASI CML (BYPASS)                                                    
*                                                                               
         TM    CMLSTAT,X'20'                                                    
         BO    DELC40              DO NOT PURGE EASI CMLS                       
*                                                                               
DELC22   OC    QPRD,QPRD           JUST 1 PROD                                  
         BZ    *+12                                                             
         BAS   RE,CCP              CK COMML PRODUCT                             
         BNE   DELC40                                                           
*                                                                               
         L     R1,CMLRECS          REC CT TO BE DELETED                         
         LA    R1,1(,R1)                                                        
         ST    R1,CMLRECS                                                       
*                                                                               
* PRINT OUT COMMERCIAL RECORD FOR DELETE LIST *                                 
*                                                                               
         MVC   PCCML,WORK                                                       
         MVC   SVCMLCOD,WORK                                                    
         MVC   PCTITLE,CMLTITLE                                                 
*                                                                               
         LLC   RE,CMLSLN                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PCLEN,DUB                                                        
*                                                                               
         MVC   PCTYPE,CMLTYPE                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(3,CMLRLSE),(5,PCDATES)                              
         MVI   PCDATES+8,C'-'                                                   
         GOTO1 DATCON,DMCB,(3,CMLRCL),(5,PCDATES+9)                             
*                                                                               
         MVC   PCCLTCML,CMLCLTNO                                                
         MVC   SVCMLSEQ,CMLSEQ                                                  
*                                                                               
         TM    CMLSTAT,X'80'       WAS COMML DELETED                            
         BZ    *+10                                                             
         MVC   PCINACT+20(11),=C'DELETED CML'                                   
*                                                                               
         TM    CMLSTAT,X'40'       COMML TEXT FOR THIS                          
         BZ    *+10                                                             
         MVC   PCINACT+32(10),=C'COMML TEXT'                                    
*                                                                               
         MVC   SVCMLSTA,CMLSTAT                                                 
*                                                                               
*                                                                               
* PRINT PRODUCT LIST *                                                          
*                                                                               
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PCTITLE+132+4(7),=C'PRD(S)='                                     
         LA    R2,2(R6)                                                         
         LLC   R3,1(R6)                                                         
         BCTR  R3,0                                                             
         BCTR  R3,0                                                             
         LA    R1,PCTITLE+132+4+7                                               
         ST    R1,DUB+4                                                         
         B     DELC26                                                           
*                                                                               
DELC24   MVI   0(R1),C','                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,DUB+4                                                         
*                                                                               
DELC26   LA    R0,0(,R2)           ADDR OF PRD IN LIST                          
         BRAS  RE,FPRD                                                          
         L     R1,DUB+4                                                         
         MVC   0(3,R1),DUB                                                      
         CLI   2(R1),C' '                                                       
         BH    *+6                                                              
         BCTR  R1,0                                                             
         LA    R1,3(,R1)                                                        
         ST    R1,DUB+4                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,DELC24                                                        
*                                                                               
DELC28   MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    DELC40                                                           
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    DELC40                                                           
*                                                                               
         L     R1,CMLRECDL                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,CMLRECDL                                                      
*                                                                               
         BAS   RE,DELREC1          GO DELETE RECORD AND KEY                     
*                                                                               
         MVC   KEY(2),=X'0AC1'     SET FOR ADID KEYS                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'A0'        GET ADID ELEMENT                             
         BRAS  RE,GETEL                                                         
         BNE   DELC29                                                           
*                                                                               
         USING CMLADIEL,R6                                                      
         MVC   KEY+5(8),CMLADIDP                                                
         BAS   RE,DELKEY1          DELETE 0AC1 KEY                              
         B     DELC30                                                           
*                                                                               
DELC29   TM    KEY+13,CMLKSTA_PCKD  PACKED CML?                                 
         BO    DELC29C                                                          
*                                                                               
         MVC   WORK(8),KEY+5                                                    
         GOTO1 VTRPACK,DMCB,(C'P',WORK),DUB  PACK IT                            
         BNE   DELC30              SKIP IF INVALID                              
         MVC   KEY+5(8),DUB                                                     
*                                                                               
DELC29C  GOTO1 HIGH                MAKE SURE THE KEY IS THERE                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   DELC30                                                           
         BAS   RE,DELKEY1                                                       
*                                                                               
DELC30   MVI   KEY+1,X'A1'         DELETE PASSIVE KEY                           
         MVC   KEY+5(3),SVCMLSEQ                                                
         XC    KEY+8(5),KEY+8                                                   
*                                                                               
         BAS   RE,DELKEY1                                                       
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'24'        GET EXTENDED DATA ELEMENT                    
         BRAS  RE,GETEL                                                         
         BNE   DELC40                                                           
*                                                                               
         USING CMLXDTEL,R6                                                      
         OC    CMLXHDPK,CMLXHDPK                                                
         BZ    DELC32                                                           
         MVC   KEY(2),=X'0AC2'     SET FOR HIDEF KEY                            
         MVC   KEY+5(8),CMLXHDPK                                                
         BAS   RE,DELKEY1          DELETE HIDEF KEY                             
*                                                                               
DELC32   OC    CMLXCCPK,CMLXCCPK                                                
         BZ    DELC40                                                           
         MVC   KEY(2),=X'0AC3'     SET FOR CNTRCUT KEY                          
         MVC   KEY+5(8),CMLXCCPK                                                
         BAS   RE,DELKEY1          DELETE CENTERCUT KEY                         
*                                                                               
DELC40   TM    SVCMLSTA,X'40'      COMML TEXT FOR THIS                          
         BZ    DELC50               NO                                          
*                                                                               
         BAS   RE,DELCT           GO DELETE ANY COMMERCIAL TEXT RECS            
*                                                                               
* RESTORE DIR FOR SEQUENTIAL READ                                               
*                                                                               
DELC50   MVC   KEY(L'SVKEY),SVKEY                                               
         OI    DMINBTS,X'08'       PASS DELETED KEY                             
         NI    DMOUTBTS,X'FF'-X'02'                                             
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08' RESET                                        
         OI    DMINBTS,X'02'       RESET FOR DELETE                             
*                                                                               
         CLC   KEY(13),KEYSAVE     MUST FIND KEY JUST PROCESSED!                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 SEQ                                                              
         B     DELC10                                                           
         DROP  R4,R6                                                            
*                                                                               
* END OF COMMERCIALS FOR CLIENT, PRINT TOTALS, ADD TO MEDIA TOTALS *            
*                                                                               
DELCX    LA    R2,CMLRECR                                                       
         LA    R3,=C'CML'                                                       
         BRAS  RE,RECTOT                                                        
*                                                                               
         LA    R2,CMTRECR                                                       
         LA    R3,=C'CMT'                                                       
         BRAS  RE,RECTOT                                                        
*                                                                               
         LA    R2,CMLRECR                                                       
         LA    R3,AGYCTRS+CMLRECR-COUNTERS                                      
         LA    R4,3                                                             
         BRAS  RE,ACT              ADD CLIENT TO MEDIA CTRS                     
DELCMLX  XIT1                                                                   
*                                                                               
*                                                                               
*====================================================================           
* DELETE ALL COMMERCIAL TEXT RECS FOR THIS COMML                                
*====================================================================           
*                                                                               
DELCT    NTR1                                                                   
         BRAS  RE,INITXSP1                                                      
         MVI   CMLCMTFL,0                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(5),SVKEY                                                     
         MVI   KEY+1,X'35'         0A35 (COMTEXT RECORD)                        
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(5),KEYSAVE                                                   
         BNE   DELCT40                                                          
*                                                                               
DELCT10  CLC   KEY(5),KEYSAVE                                                   
         BNE   DELCT30                                                          
*                                                                               
         L     R1,CMTRECR          RECS READ                                    
         LA    R1,1(,R1)                                                        
         ST    R1,CMTRECR                                                       
*                                                                               
         CLC   CMXKCML-CMXKEY+KEY,SVCML                                         
         BNE   DELCT20                                                          
*                                                                               
         L     R1,CMTRECS                                                       
         LA    R1,1(,R1)                                                        
         ST    R1,CMTRECS                                                       
*                                                                               
         MVI   CMLCMTFL,1          SET COMMENT FOUND                            
*                                                                               
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    FTRSW2,TESTSW       TEST MODE                                    
         BO    DELCT20                                                          
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    DELCT20                                                          
         L     R1,CMTRECDL                                                      
         LA    R1,1(,R1)                                                        
         ST    R1,CMTRECDL                                                      
         BRAS  RE,DELREC           GO DELETE RECORD AND KEY                     
*                                                                               
DELCT20  MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         B     DELCT10                                                          
*                                                                               
DELCT30  CLI   CMLCMTFL,0          WAS A COMMENT FOUND                          
         BNE   DELCTX                                                           
*                                                                               
DELCT40  MVC   P+10(24),=C'COMMENT FLAGGED FOR CMML'                            
         MVC   P+35(12),SVCMLCOD                                                
         MVC   P+48(10),=C'NONE FOUND'                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
DELCTX   MVC   AIO,AIO1            RESET BACK TO AIO1                           
         BRAS  RE,INITSPT1                                                      
         J     EXIT                                                             
*                                                                               
*                                                                               
INITXSP1 NTR1                                                                   
         MVI   DATADISP+1,42                                                    
         MVI   LKEY+1,32                                                        
         MVI   LSTATUS+1,4                                                      
         MVC   SYSDIR(3),=C'XSP'                                                
         MVC   SYSFIL(3),=C'XSP'                                                
         XIT1                                                                   
*                                                                               
INITSPT1 NTR1                                                                   
         MVI   LKEY+1,13                                                        
         MVI   DATADISP+1,24       SET TO SPOT                                  
         MVI   LSTATUS+1,1                                                      
         MVC   SYSDIR(3),=C'SPT'                                                
         MVC   SYSFIL(3),=C'SPT'                                                
         XIT1                                                                   
*                                                                               
*                                                                               
*=========================================================                      
* DELETE SPOT KEY ONLY                                                          
*=========================================================                      
                                                                                
DELKEY1  NTR1                                                                   
         TM    FTRSW2,TESTSW       TEST RUN                                     
         BO    DELCMLX                                                          
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    DELCMLX                                                          
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'C0'                                                     
         NI    DMINBTS,X'FF'-X'80' SET FOR DELETE                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',KEY,KEY                        
         OI    DMINBTS,X'80'       RESET FOR DELETE                             
         B     DELCMLX                                                          
         EJECT                                                                  
* DELETE RECORD AND KEY (SPTDIR.SPTFIL)                                         
*                                                                               
         DS    0H                                                               
DELREC1  NTR1                                                                   
         TM    FTRSW2,TESTSW       TEST RUN                                     
         BO    DELCMLX                                                          
         CLI   TWAWRITE,C'N'       OK TO WRITE                                  
         BE    DELCMLX                                                          
         L     R6,AIO                                                           
         OI    15(R6),X'C0'                                                     
         OI    KEY+13,X'C0'                                                     
         NI    DMOUTBTS,X'FF'-X'02' SET FOR DELETE                              
         GOTO1 PUTREC                                                           
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',KEY,KEY                        
         OI    DMOUTBTS,X'02'       RESET FOR DELETE                            
         B     DELCMLX                                                          
         EJECT                                                                  
*==============================================================                 
* CHECK COMMERCIAL REC PRODUCT LIST FOR REQUESTED PRODUCT                       
*==============================================================                 
                                                                                
CCP      NTR1                                                                   
         L     R6,AIO1                                                          
         MVI   ELCODE,X'29'        LOOK AT PRODS                                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLMPREL,R6                                                      
         CLI   CMLMPRS,X'FF'       ALL PRODUCTS                                 
         BE    CCP30                                                            
*                                                                               
         LLC   R0,CMLMPRLN         ELEM LEN                                     
         SHI   R0,2                MINUS CODE AND LEN                           
         LA    R1,CMLMPRS                                                       
CCP10    CLC   QPRD,0(R1)                                                       
         BE    CCP20                                                            
         LA    R1,3(,R1)                                                        
         SHI   R0,2                                                             
         BCT   R0,CCP10                                                         
         B     CCP30                                                            
*                                                                               
CCP20    OC    QPRD2,QPRD2         PARTNER PRODUCT                              
         BZ    CCP40                NO                                          
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LLC   R0,CMLMPRLN         ELEM LEN                                     
         SHI   R0,2                MINUS CODE AND LEN                           
         LA    R1,CMLMPRS                                                       
*                                                                               
CCP24    CLC   QPRD2,0(R1)         GOT THIS PROD TOO                            
         BE    CCP40                YES                                         
         LA    R1,3(,R1)                                                        
         SHI   R0,2                                                             
         BCT   R0,CCP24                                                         
*                                                                               
CCP30    MVI   ELCODE,X'10'                                                     
         CR    RB,RD                                                            
         B     DELCMLX                                                          
*                                                                               
CCP40    MVI   ELCODE,X'10'                                                     
         B     DELCMLX                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*==================================================                             
* FIND COMMERCIAL AND CHECK REL/RCL DATES                                       
* IF DATES ARE AFTER PURGE DATE                                                 
* THEN NO NEED TO SAVE IT IN ACTIVE CML TABLE                                   
* THE RECORD WILL NOT GET PURGED ANYWAY                                         
*==================================================                             
*                                                                               
FCDTE    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OI    FOUNDSW,ACTCSW      PRESET ACTIVE CML, ADD TO TABLE              
*                                                                               
         BRAS  RE,INITSPT          SET FROM NET TO SPOT                         
         MVC   SVELCODE,ELCODE                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'     SET TO READ CMML POINTER                     
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,SVBCLT                                                   
         MVC   CMLKCML,0(R5)       R5 POINTS TO CML                             
*                                                                               
         TM    FOUNDSW,PCMLSW      PACKED CML                                   
         BO    FCDTE1              YES, READ ADID POINTER                       
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   FCDTE2                                                           
*                                                                               
FCDTE1   MVC   CMLKID,=X'0AC1'     SET TO READ ADID POINTER                     
*                                                                               
         TM    FOUNDSW,PCMLSW      PACKED CML                                   
         BO    FCDTE2              YES                                          
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'P',(R5)),CMLKCML                                 
*                                                                               
FCDTE2   GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FCDTE6              NO CML RECORD FOUND                          
*                                                                               
FCDTE4   L     R6,AIO2                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
*                                                                               
         CLC   EXPDATE,CMLRCL      PURGE DATE BEFORE RECALL DATE                
         BL    FCDTE6              YES, DO NOT ADD CML TO TABLE                 
*                                                                               
         CLC   EXPDATE,CMLRLSE     PURGE DATE AFTER RELEASE DATE                
         BNL   FCDTEX              YES, ADD IT TO THE ACTIVE TABLE              
*                                                                               
FCDTE6   NI    FOUNDSW,X'FF'-ACTCSW  TURN OFF ADD TO ACTIVE CML TBLE            
*                                                                               
FCDTEX   MVC   ELCODE,SVELCODE     RESTORE ELCODE                               
         J     EXIT                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*==============================================================                 
* FIND COMMERCIAL LENGTH                                                        
* THIS CODE IS NOP AS OF 15SEP09                                                
* THE LENGTH IS NOT IMPORTANT                                                   
* JUST ASSUME ALL COMMERCIALS ARE FOUND                                         
*==============================================================                 
                                                                                
FCLEN    OI    FOUNDSW,CMLFSW                                                   
         BR    RE                                                               
                                                                                
*&&DO                                                                           
FCLEN    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OI    FOUNDSW,CMLFSW      SET CML FOUND SW                             
*                                                                               
         BRAS  RE,INITSPT          SET FROM NET TO SPOT                         
         MVC   SVELCODE,ELCODE                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'     SET TO READ CMML POINTER                     
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,SVBCLT                                                   
         MVC   CMLKCML,0(R5)       R5 POINTS TO CML                             
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   FCLEN2                                                           
*                                                                               
         MVC   CMLKID,=X'0AC1'     SET TO READ ADID POINTER                     
         GOTO1 VTRPACK,DMCB,(C'P',(R5)),CMLKCML                                 
*                                                                               
FCLEN2   GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FCLEN10                                                          
*                                                                               
FCLEN4   L     R6,AIO2                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
*                                                                               
         MVC   SVCMLSLN,CMLSLN     SAVE CML LEN                                 
         B     FCLEN16                                                          
*                                                                               
* ADD CML TO THE NOT FOUND COMMERCIAL LIST                                      
*                                                                               
FCLEN10  NI    FOUNDSW,X'FF'-CMLFSW  CML NOT FOUND SW                           
         LM    RE,RF,STRLCLST                                                   
*                                                                               
FCLEN12  CR    RE,RF               AT END OF LIST                               
         BE    FCLEN14                                                          
         BL    *+6                                                              
         DC    H'0'                                                             
         CLC   0(8,R5),0(RE)       EQUAL TO THIS CML                            
         BE    FCLEN16                                                          
         LA    RE,8(,RE)                                                        
         B     FCLEN12                                                          
*                                                                               
FCLEN14  MVC   0(8,RE),0(R5)                                                    
         LA    RF,8(,RF)                                                        
         ST    RF,ENDLCLST                                                      
         C     RF,MAXLCSZ          SEE IF EXCEEDED MAX TABLE SIZE               
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FCLEN16  BRAS  RE,INITNET          SET FROM SPOT TO NET                         
         MVC   ELCODE,SVELCODE     RESTORE ELCODE                               
         XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
*&&                                                                             
         EJECT                                                                  
* VALIDATE END DATE IN KEY, FROM VK                                             
*                                                                               
VEDT     NTR1  BASE=*,LABEL=*                                                   
         CLI   5(R2),0                                                          
         BE    MISSERRA                                                         
         GOTO1 DATVAL,DMCB,(0,TRAEDT),DATE                                      
         OC    DMCB(4),DMCB                                                     
         BZ    DATERRA                                                          
         GOTO1 DATCON,DMCB,(0,DATE),(3,EXPDATE)   YMD BINARY                    
         GOTO1 DATCON,DMCB,(0,DATE),(2,EXPDTCMP)  COMPRESSED                    
         GOTO1 DATCON,DMCB,(5,0),(0,WORK+20)      TODAY'S DATE (YYMMDD)         
         GOTO1 ADDAY,DMCB,(C'Y',WORK+20),WORK,-1  PREVIOUS YEAR                 
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK)      GET BINARY (YMD)              
         MVC   WORK+1(2),=XL2'0C1F'               FORCE MO/DA TO 12/31          
         CLC   EXPDATE,WORK                                                     
         BH    ENDTERR                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,EXPDATE),(5,PRTDATE)                              
*                                                                               
         XIT1                                                                   
*                                                                               
DATERRA  MVI   ERROR,INVDATE                                                    
         B     TRAPERRA                                                         
MISSERRA MVI   ERROR,MISSING                                                    
TRAPERRA GOTO1 ERREX                                                            
ENDTERR  MVC   CONHEAD,ENDTERMS                                                 
         GOTO1 ERREX2                                                           
ENDTERMS DC    CL60'* ERROR * DATE MUST BE BEFORE START OF THIS YEAR *'         
         EJECT                                                                  
* VALIDATE OFFICE CODE - READ CLIENT HEADER RECORD(S) *                         
*                                                                               
VOFF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   DATADISP+1,24       SET FROM NET TO SPOT                         
         MVI   LKEY+1,13                                                        
         MVC   SYSDIR(2),=C'SP'                                                 
         MVC   SYSFIL(2),=C'SP'                                                 
*                                                                               
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVC   KEY+1(1),BAGYMD                                                  
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
VOFF10   CLI   KEY,0               TEST CLIENT HEADER RECS                      
         BNE   VOFFX                                                            
         CLC   KEY+1(1),BAGYMD                                                  
         BNE   VOFFX                                                            
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    VOFF30               YES                                         
*                                                                               
VOFF20   MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         GOTO1 HIGH                                                             
         B     VOFF10                                                           
*                                                                               
VOFF30   L     R6,AIO2                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SVCLTOFF,CTRAFOFC   USE TRAFFIC OFFICE                           
         CLI   SVCLTOFF,C'A'       IF THERE IS ONE                              
         BNL   VOFF35                                                           
*                                                                               
         MVC   SVCLTOFF,COFFICE    USE MEDIA OFFICE                             
*                                                                               
         CLI   LAOFFICE,0          IS THIS CLT LIMITED ACCESS                   
         BE    VOFF35               NO                                          
         CLC   SVCLTOFF,LAOFFICE   SAME OFFICE                                  
         BNE   VOFF20                                                           
         B     VOFFX                                                            
*                                                                               
VOFF35   BRAS  RE,COFF             CHECK OFFICE VALIDITY                        
         BNE   VOFF20                                                           
*                                                                               
*NOP     OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
*        BZ    VOFF50                                                           
*                                                                               
*        CLI   T216FFD+6,C'*'          TEST OFFICE LOCKOUT                      
*        BE    VOFF50                                                           
*                                                                               
*        CLC   T216FFD+6(2),CKEYCLT    ELSE SINGLE CLIENT ACCESS                
******   BNE   VOFF20                                                           
*                                                                               
*OFF50   CLC   OFFICE,SVCLTOFF     RIGHT OFFICE?                                
******   BNE   VOFF20                                                           
*                                                                               
*OFF70   CLI   TRACLT,C'*'         REQUESTED BY OFFICE                          
*        BNE   VOFF90                                                           
*        SPACE                                                                  
*        CLC   BOFFCD,SVCLTOFF     SAME OFFICE                                  
*        BNE   VOFF20                                                           
*******  B     VOFFX                                                            
*                                                                               
*                                                                               
VOFFX    MVC   BCLT,CKEYCLT                                                     
         MVC   CLTNM,CNAME                                                      
*                                                                               
         MVI   DATADISP+1,27       SET FROM SPOT TO NET                         
         MVI   LKEY+1,20                                                        
         MVC   SYSDIR(2),=C'UN'                                                 
         MVC   SYSFIL(2),=C'UN'                                                 
*                                                                               
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
* CHECK OFFICE TO BE VALID *                                                    
*                                                                               
COFF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 CLUNPK,DMCB,KEY+2,DUB                                            
*                                                                               
         USING CLTHDRD,R6                                                       
*                                                                               
* USE OFFICER TO VALIDATE CLIENT USE FOR THIS OFFICE *                          
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    WORKSEC,WORKSEC                                                  
         LA    R1,WORKSEC                                                       
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,SVCLTOFF                                                  
         MVC   OFCCLT,DUB                                                       
         OC    OFCCLT,SPACES                                                    
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),T216FFD+6                                              
         MVC   OFCACCSC(3),CACCESS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCSECD,ASECBLK                                                  
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*====================================================================           
* VALIDATE FILTER - ONLY OPTIONS ARE PROG EQUIV, REVISION, PATTERN,             
* COMMERCIALS, LIST AND TEST                                                    
*====================================================================           
                                                                                
VFTR     NTR1  BASE=*,LABEL=*                                                   
         XC    FILTERS,FILTERS                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFTRX               NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTR06              YES                                          
         CLI   5(R2),4                                                          
         BNH   VFTR02                                                           
         LA    R1,4                                                             
         B     VFTR04                                                           
VFTR02   LLC   R1,5(R2)                                                         
VFTR04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BNE   VFTR08                                                           
VFTR06   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRHELPA),FTRHELPA                                     
*                                                                               
ERREXITA GOTO1 ERREX2                                                           
*                                                                               
VFTR08   GOTO1 SCANNER,DMCB,(20,TRAFLTRH),(7,BLOCK)                             
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERRB            NO                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
*                                                                               
VFTR10   LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
         CLI   0(R5),X'4C'         LESS THAN                                    
         BE    VFTR12              YES, SAVE IT                                 
         CLI   0(R5),X'6E'         GREATER THAN                                 
         BNE   VFTR20              NO, NETHER                                   
VFTR12   MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
*                                                                               
VFTR20   EX    R1,VFTRCLCA         PATTERN                                      
         BNE   VFTR30                                                           
         OI    FTRSW1,PATSW                                                     
         B     VFTR70                                                           
VFTR30   EX    R1,VFTRCLCB         COMMERCIAL                                   
         BNE   VFTR40                                                           
         OI    FTRSW1,CMLSW                                                     
         B     VFTR70                                                           
VFTR40   EX    R1,VFTRCLCC         PROGRAM EQUIV                                
         BNE   VFTR50                                                           
         OI    FTRSW1,PEQVSW                                                    
         B     VFTR70                                                           
VFTR50   EX    R1,VFTRCLCD         REVISION RECORDS                             
         BNE   VFTR60                                                           
         OI    FTRSW1,REVSW                                                     
         B     VFTR70                                                           
VFTR60   EX    R1,VFTRCLCL         LIST DELETED SKED UNITS                      
         BNE   VFTR64                                                           
         OI    FTRSW2,SKULSW                                                    
         B     VFTR70                                                           
VFTR64   EX    R1,VFTRCLCT         TEST RUN                                     
         BNE   VFTR66                                                           
         OI    FTRSW2,TESTSW                                                    
         B     VFTR70                                                           
*                                                                               
VFTR66   EX    R1,VFTRCLCU         SKIP PU PROFILE                              
         BNE   VFTR68                                                           
         OI    FTRSW2,SKIPUSW                                                   
         B     VFTR70                                                           
*                                                                               
VFTR68   EX    R1,VFTRCLCR         TRACE RUN                                    
         BNE   VFTR80                                                           
         OI    FTRSW2,TRCESW                                                    
*                                                                               
VFTR70   LA    R4,42(,R4)          POINT TO NEXT BLOCK                          
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
VFTRX    XIT1                                                                   
VFTR80   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FTRMSG+L'FTRHELPA),FTRMSG                              
         B     ERREXITA                                                         
VFTRCLCA CLC   12(0,R4),=CL9'PATTERNS'                                          
VFTRCLCB CLC   12(0,R4),=CL12'COMMERCIALS'                                      
VFTRCLCC CLC   12(0,R4),=CL10'PROGEQUIV'                                        
VFTRCLCD CLC   12(0,R4),=CL9'REVISION'                                          
VFTRCLCL CLC   12(0,R4),=CL5'LIST'                                              
VFTRCLCT CLC   12(0,R4),=CL5'TEST'                                              
VFTRCLCR CLC   12(0,R4),=CL6'TRACE'                                             
VFTRCLCU CLC   12(0,R4),=CL6'SKIPPU'   SKIP PU PROFILE                          
DATERRB  MVI   ERROR,INVDATE                                                    
         B     TRAPERRB                                                         
MISSERRB MVI   ERROR,MISSING                                                    
TRAPERRB GOTO1 ERREX                                                            
FTRMSG   DC    C'* ERROR * '                                                    
FTRHELPA DC    C'VALID FILTERS=PAT/COM/REV/PROGEQU/TEST/SKIPPU'                 
         LTORG                                                                  
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
*                                                                               
HDHK     NTR1  BASE=*,LABEL=*                                                   
         TM    FTRSW2,TESTSW       TEST RUN                                     
         BZ    *+10                                                             
         MVC   H4+43(8),=CL8'**TEST**'                                          
*                                                                               
         MVC   H4+10(L'QMED),QMED                                               
         MVC   H4+15(L'MEDNM),MEDNM                                             
         MVC   H3+34(16),=CL16'PURGE ALL BEFORE'                                
         MVC   H3+51(8),PRTDATE                                                 
*                                                                               
         CLI   OFFICE,0            TEST OFFICE CODE GIVEN                       
         BE    HDHK10                                                           
         MVC   H5+43(6),=C'OFFICE'                                              
         BAS   RE,CNVOFF           CONVERT 1 BYTE OFFICE CODE                   
         BE    HDHK10              OFFICE PRINTED                               
*                                                                               
         GOTO1 =V(OFFOUT),DMCB,OFFICE,HEXOUT,H5+52                              
*                                                                               
HDHK10   MVC   H5+10(L'QCLT),QCLT                                               
         MVC   H5+15(L'CLTNM),CLTNM                                             
         XIT1                                                                   
         EJECT                                                                  
* CONVERT 1 BYTE OFFICE CODE AND PRINT 2 CHAR CODE                              
*                                                                               
CNVOFF   NTR1                                                                   
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORKSEC,WORKSEC                                                  
         LA    R3,WORKSEC                                                       
         USING OFFICED,R3                                                       
         MVI   OFCSYS,C'S'         SYSTEM ID                                    
         MVC   OFCAUTH,T216FFD+6   ID AUTH VALUE                                
         MVC   OFCLMT(4),T216FFD+6                                              
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,OFFICE                                                    
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'2',WORKSEC),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   CNVOFFX                                                          
*                                                                               
         TM    OFCINDS,OFCINOLA    NO OFFICE LIMIT ACCESS REC                   
         BO    CNVOFFX             2 CHAR OFFICE IS NOT ON                      
*                                                                               
         MVC   H5+52(2),OFCOFC2                                                 
         CR    RB,RB               SET EQ CC                                    
*                                                                               
CNVOFFX  XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST *                                           
*                                                                               
FCLT     NTR1  BASE=*,LABEL=*                                                   
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT MEDIA SYSTEM                  
*                                                                               
         MVC   SVKEY,KEY                                                        
         MVC   BBCLT,BCLT          SAVE BCLT IF ANY                             
*                                                                               
         GOTO1 CLUNPK,DMCB,SVBCLT,QCLT                                          
*                                                                               
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
*                                                                               
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
*                                                                               
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT06                                                           
*                                                                               
         CLI   ERROR,55            SECURITY?                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FCLT06   DS    0H                                                               
         MVC   BCLT,BBCLT          RESTORE BCLT                                 
*                                                                               
         TM    FTRSW2,SKIPUSW      SKIP PU PROFILE                              
         BO    FCLT10                                                           
*                                                                               
* READ PU PROFILE *                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0PU'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,(X'90',WORK),SVPROF,DATAMGR                         
*                                                                               
FCLT10   GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO STRAFFIC SYSTEM               
*                                                                               
         TM    FTRSW2,SKIPUSW      DID WE SKIP PU PROFILE                       
         BO    FCLT20                                                           
*                                                                               
         CLI   SVPROF+1,C'Y'       IS PROFILE SET TO BYPASS PURGE               
         BNE   FCLT20                NO                                         
*                                                                               
         L     R1,AXCLTBLE         XCLUDE CLT TABLE                             
FCLT15   OC    0(3,R1),0(R1)       ANY ENTRY                                    
         BZ    FCLT18                                                           
         CLC   QCLT,0(R1)          IS CLT ALREADY IN TABLE                      
         BE    FCLTX                YES, EQ CC WILL BYPASS                      
         LA    R1,3(R1)                                                         
         C     R1,MAXCLTBL         END OF TALBE                                 
         BL    FCLT15                                                           
         DC    H'0'                MAKE TABLE BIGGER                            
*                                                                               
FCLT18   MVC   0(3,R1),QCLT        SAVE BYPASSED CLT IN TABLE                   
         CR    RB,RB               SET CC EQ                                    
         B     FCLTX               TO BYPASS THIS CLT                           
*                                                                               
FCLT20   MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         CR    RB,RC               SET CC NE TO ALLOW PURGE TO HAPPEN           
*                                                                               
FCLTX    XIT1                                                                   
         EJECT                                                                  
* GET NEXT SEQUENTIAL CLIENT FOR THIS OFFICE *                                  
*                                                                               
*                                                                               
NOFF     NTR1  BASE=*,LABEL=*                                                   
         MVI   DATADISP+1,24       SET FROM NET TO SPOT                         
         MVI   LKEY+1,13                                                        
         MVC   SYSDIR(2),=C'SP'                                                 
         MVC   SYSFIL(2),=C'SP'                                                 
*                                                                               
         XC    KEY,KEY             SET FOR CLIENT HEADER RECORDS                
         MVI   KEY,X'00'                                                        
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),SVBCLT      LAST CLIENT THIS OFFICE                     
         MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         GOTO1 HIGH                                                             
*                                                                               
NOFF10   CLI   KEY,X'00'           TEST CLIENT HEADER RECS                      
         BNE   NOFF80                                                           
         CLC   KEY+1(1),BAGYMD                                                  
         BNE   NOFF80                                                           
         OC    KEY+4(9),KEY+4      THIS A CLIENT REC                            
         BZ    NOFF30               YES                                         
*                                                                               
NOFF20   MVI   KEY+4,X'FF'         FORCE NEXT CLIENT                            
         GOTO1 HIGH                                                             
         B     NOFF10                                                           
*                                                                               
NOFF30   L     R6,AIO1                                                          
         ST    R6,AIO              SET FOR GETREC                               
         USING CLTHDRD,R6                                                       
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SVCLTOFF,CTRAFOFC   USE TRAFFIC OFFICE                           
         CLI   SVCLTOFF,C'A'       IF THERE IS ONE                              
         BNL   NOFF34                                                           
*                                                                               
         MVC   SVCLTOFF,COFFICE    USE MEDIA OFFICE                             
*                                                                               
NOFF34   BRAS  RE,COFF             CHECK OFFICE VALIDITY                        
         BNE   NOFF20                                                           
*                                                                               
         OC    T216FFD+6(2),T216FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    NOFF70                                                           
*                                                                               
         CLI   T216FFD+6,C'*'          TEST OFFICE LOCKOUT                      
         BE    NOFF70                                                           
*                                                                               
         CLC   T216FFD+6(2),CKEYCLT   ELSE SINGLE CLIENT ACCESS                 
         BNE   NOFF20                                                           
*                                                                               
NOFF70   CLC   OFFICE,SVCLTOFF     RIGHT OFFICE?                                
         BNE   NOFF20                                                           
*                                                                               
         MVC   SVBCLT,CKEYCLT                                                   
         MVC   CLTNM,CNAME                                                      
*                                                                               
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO STRAFFIC SYSTEM               
*                                                                               
         CR    R0,R0                                                            
         B     NOFFX                                                            
*                                                                               
NOFF80   GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO STRAFFIC SYSTEM               
         CR    RB,RC               SET NE COND CODE                             
*                                                                               
NOFFX    XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
* PRINT TOTALS FOR PURGED ELEMENTS AND RECORDS *                                
*                                                                               
ELEMTOT  NTR1  BASE=*,LABEL=* 0,*ELMTOT*                                        
         EDIT  (4,0(R2)),(9,P+3),0,COMMAS=YES,ZERO=NOBLANK                      
         MVC   P+13(3),0(R3)                                                    
         MVC   P+17(10),=C'ELEMS READ'                                          
         EDIT  (4,4(R2)),(9,P+35),0,COMMAS=YES,ZERO=NOBLANK                     
         MVC   P+45(3),0(R3)                                                    
         MVC   P+49(18),=C'ELEMS TO BE PURGED'                                  
         EDIT  (4,8(R2)),(9,P+70),0,COMMAS=YES,ZERO=NOBLANK                     
         MVC   P+80(3),0(R3)                                                    
         MVC   P+84(12),=C'ELEMS PURGED'                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  (4,12(R2)),(9,P+3),0,COMMAS=YES,ZERO=NOBLANK                     
         MVC   P+13(3),0(R3)                                                    
         MVC   P+17(9),=C'RECS READ'                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  (4,16(R2)),(9,P+3),0,COMMAS=YES,ZERO=NOBLANK                     
         MVC   P+13(3),0(R3)                                                    
         MVC   P+17(18),=C'RECS TO BE UPDATED'                                  
         EDIT  (4,20(R2)),(9,P+35),0,COMMAS=YES,ZERO=NOBLANK                    
         MVC   P+45(3),0(R3)                                                    
         MVC   P+49(12),=C'RECS WRITTEN'                                        
         EDIT  (4,24(R2)),(9,P+70),0,COMMAS=YES,ZERO=NOBLANK                    
         MVC   P+80(3),0(R3)                                                    
         MVC   P+84(12),=C'RECS DELETED'                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* PRINT TOTALS FOR PURGED ELEMENTS AND RECORDS *                                
*                                                                               
RECTOT   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  (4,0(R2)),(9,P+3),0,COMMAS=YES,ZERO=NOBLANK                      
         MVC   P+13(3),0(R3)                                                    
         MVC   P+17(9),=C'RECS READ'                                            
         EDIT  (4,4(R2)),(9,P+35),0,COMMAS=YES,ZERO=NOBLANK                     
         MVC   P+45(3),0(R3)                                                    
         MVC   P+49(18),=C'RECS TO BE DELETED'                                  
         EDIT  (4,8(R2)),(9,P+70),0,COMMAS=YES,ZERO=NOBLANK                     
         MVC   P+80(3),0(R3)                                                    
         MVC   P+84(12),=C'RECS DELETED'                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* ADD TO ACTIVE PATTERN LIST                                                    
*===============================================================                
                                                                                
APL      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING NPTXKEY,R4                                                       
*                                                                               
         MVC   WORK(APLISTLN),NPTXNET                                           
         CLI   NPTXNET,C'$'                                                     
         BNE   APL2                                                             
         MVC   WORK(4),NUKPNET-NUKPKEY+SVUNTKEY  NTWK FROM UNIT PSSV            
                                                                                
APL2     L     R5,STRAPLST         START OF ACTIVE PATTERN LIST                 
         USING APLIST,R5                                                        
*                                                                               
APL10    C     R5,ENDAPLST         AT END OF LIST                               
         BE    APL20                YES                                         
         BL    *+6                 BUT NOT PAST END                             
         DC    H'0'                                                             
         CLC   WORK(APLISTLN),APLNET  SAME PAT ENTRY                            
         BE    APL24                                                            
*                                                                               
APL16    LA    R5,APLNEXT                                                       
         B     APL10                                                            
*                                                                               
APL20    DS    0H                                                               
         MVC   APLNET(APLISTLN),WORK       ADD TO TABLE PAT KEY                 
         LA    R5,APLNEXT                                                       
         ST    R5,ENDAPLST                                                      
         C     R5,STRACLST         SEE IF RUNNING INTO CML TABLE                
         BL    *+6                                                              
         DC    H'0'                                                             
APL24    DS    0H                                                               
         XIT1                                                                   
         DROP  R4,R5                                                            
         EJECT                                                                  
*================================================================               
* ADD PATTERN COMMERCIAL LIST TO ACTIVE COMMERCIAL LIST                         
*================================================================               
                                                                                
ACL      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVACTCML,SVACTCML   CLEAR SAVE ACTUAL CML AREA                   
*                                                                               
         LA    R5,2(R6)                                                         
         LLC   R0,1(R6)                                                         
         SRL   R0,4                DIVIDE BY 16                                 
ACL10    LM    R3,R4,STRACLST                                                   
*                                                                               
ACL20    DS    0H                                                               
         CLC   0(2,R5),=X'5C00'     DELETED COMML                               
         BE    ACL30                                                            
         CLC   =C'REASSIGN',0(R5)                                               
         BE    ACL30                                                            
         CLC   =C'VIGNETTE',0(R5)    VIGNETTE                                   
         BE    ACL30                                                            
*                                                                               
ACL21    MVC   WORK(8),0(R5)                                                    
         MVC   WORK+8(4),SPACES                                                 
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   ACL22                                                            
         GOTO1 VTRPACK,DMCB,(C'U',(R5)),WORK                                    
*                                                                               
ACL22    CR    R3,R4               AT END OF LIST                               
         BE    ACL24                                                            
         CLC   WORK(12),0(R3)       EQUAL TO THIS CML                           
         BNE   ACL23                                                            
         OC    SVACTCML,SVACTCML   ARE WE DOING ACTUAL CMLS                     
         BNZ   ACL27                YES, GET NEXT                               
         B     ACL26                                                            
*                                                                               
ACL23    LA    R3,12(R3)                                                        
         B     ACL22                                                            
*                                                                               
ACL24    DS    0H                                                               
         BRAS  RE,FCDTE            FIND CML REL/RCL DATES                       
         TM    FOUNDSW,ACTCSW      ACTIVE CML FOUND, ADD TO TBLE                
         BZ    ACL24C              NO, DO NOT ADD                               
*                                                                               
         BRAS  RE,FCLEN            FIND CML LEN                                 
         TM    FOUNDSW,CMLFSW                                                   
         BO    ACL25                                                            
ACL24C   OC    SVACTCML,SVACTCML   ARE WE DOING ACTUAL CMLS                     
         BNZ   ACL27                YES, GET NEXT                               
         B     ACL26                                                            
*                                                                               
ACL25    MVC   0(12,R3),WORK                                                    
         LA    R3,12(R3)                                                        
         ST    R3,ENDACLST                                                      
         C     R3,STRARLST         SEE IF RUNNING INTO REV LIST                 
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    SVACTCML,SVACTCML   ARE WE DOING ACTUAL CMLS                     
         BNZ   ACL27                YES                                         
*                                                                               
ACL26    BRAS  RE,FACTCML          FIND ACTUAL CMLS IF ANY                      
         BNE   ACL30                                                            
*                                                                               
         MVC   SVADIDFL,ADIDFLAG                                                
         MVI   ADIDFLAG,C'N'       RESET ADIDFLAG - ACTUALS ARE 8-12            
         ST    R5,SVREG            SAVE R5 POINTER                              
         LA    R5,SVACTCML                                                      
         B     *+8                                                              
ACL27    LA    R5,12(R5)                                                        
         CLI   0(R5),0             END OF CML LIST                              
         BE    ACL29                YES, DONE                                   
         LM    R3,R4,STRACLST                                                   
         B     ACL21                YES, ADD TO ACTIVE TABLE                    
*                                                                               
ACL29    L     R5,SVREG            RESTORE R5                                   
         MVC   ADIDFLAG,SVADIDFL   RESTORE ADIDFLAG                             
         XC    SVACTCML,SVACTCML   CLEAR SAVE ACTUAL CML AREA                   
*                                                                               
ACL30    OC    8(8,R5),8(R5)       P/B CML                                      
         BZ    ACL100               NO                                          
         LM    R3,R4,STRACLST                                                   
         CLC   8(2,R5),=X'5C00'      DELETED COMML                              
         BE    ACL100                                                           
         CLC   =C'REASSIGN',0(R5)                                               
         BE    ACL100                                                           
         CLC   =C'VIGNETTE',0(R5)    VIGNETTE                                   
         BE    ACL100                                                           
*                                                                               
         MVC   WORK(8),8(R5)                                                    
         MVC   WORK+8(4),SPACES                                                 
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   ACL40                                                            
         GOTO1 VTRPACK,DMCB,(C'U',8(R5)),WORK                                   
*                                                                               
ACL40    CR    R3,R4               AT END OF LIST                               
         BE    ACL50                                                            
         CLC   WORK(12),0(R3)       EQUAL TO THIS CML                           
         BNE   ACL45                                                            
         OC    SVACTCML,SVACTCML                                                
         BNZ   ACL70               GET NEXT ACTUAL CML                          
         B     ACL60                                                            
*                                                                               
ACL45    LA    R3,12(R3)                                                        
         B     ACL40                                                            
*                                                                               
ACL50    DS    0H                                                               
         BRAS  RE,FCDTE            FIND CML REL/RCL DATES                       
         TM    FOUNDSW,ACTCSW      ACTIVE CML FOUND, ADD TO TBLE                
         BZ    ACL100              NO, DO NOT ADD                               
*                                                                               
         BRAS  RE,FCLEN            FIND CML LEN                                 
         TM    FOUNDSW,CMLFSW                                                   
         BZ    ACL100              CML RECORD NOT FOUND                         
         MVC   0(12,R3),0(R5)                                                   
         LA    R3,12(R3)                                                        
         ST    R3,ENDACLST                                                      
         C     R3,STRARLST         SEE IF OVERFLOW TO REV LIST                  
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    SVACTCML,SVACTCML   ARE WE DOING ACTUAL CMLS                     
         BNZ   ACL70                YES                                         
*                                                                               
ACL60    BRAS  RE,FACTCML          FIND ACTUAL CMLS IF ANY                      
         BNE   ACL100                                                           
*                                                                               
         MVC   SVADIDFL,ADIDFLAG                                                
         MVI   ADIDFLAG,C'N'       RESET ADIDFLAG - ACTUALS ARE ADIDS           
         ST    R5,SVREG            SAVE R5 POINTER                              
         LA    R5,SVACTCML                                                      
         B     *+8                                                              
ACL70    LA    R5,8(R5)                                                         
         CLI   0(R5),0             END OF CML LIST                              
         BE    ACL80                YES, DONE                                   
         LM    R3,R4,STRACLST                                                   
         B     ACL40                YES, ADD TO ACTIVE TABLE                    
*                                                                               
ACL80    L     R5,SVREG            RESTORE R5                                   
         MVC   ADIDFLAG,SVADIDFL                                                
         XC    SVACTCML,SVACTCML   CLEAR SAVE ACTUAL CML AREA                   
*                                                                               
ACL100   LA    R5,16(R5)                                                        
         BCT   R0,ACL10                                                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* ADD TO ACTIVE REVISION LIST *                                                 
*                                                                               
ARL      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING REVXKEY,R4                                                       
*                                                                               
         LA    R5,ELEM+3           BUILD REV ENTRY IN ELEM                      
         USING ARLIST,R5                                                        
*                                                                               
         LHI   R2,TSARBLK-SYSD   GET ADDR OF TSAR TABLE                         
         AR    R2,R9                                                            
         USING TSARD,R2                                                         
*                                                                               
         LA    R3,1                                                             
         STCM  R3,7,TSRCOUNT                                                    
*                                                                               
         LA    R0,ELEM                                                          
         ST    R0,TSAREC           SET ADDRESS OF RECORD                        
*                                                                               
ARL10    XC    ELEM,ELEM                                                        
         STCM  R3,7,ELEM            KEY (REC NUM)                               
         MVI   TSOFFACT,TSARDH      SET TSAROFF GET RECORD                      
         GOTO1 TSAROFF,(R2)                                                     
         TM    TSERRS,X'90'        END OF FILE                                  
         BNZ   ARL20                YES                                         
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   REVXKNET(ARLISTLN),ARLNET  SAME REV ENTRY                        
         BE    ARL24                                                            
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,7,TSRCOUNT                                                    
         LA    R3,1(,R3)                                                        
         STCM  R3,7,TSRCOUNT                                                    
         B     ARL10                                                            
*                                                                               
ARL20    DS    0H                                                               
         MVC   ARLNET(ARLISTLN),REVXKNET    ADD TO TABLE REV KEY                
*                                                                               
         STCM  R3,7,ELEM            KEY (REC NUM)                               
*                                                                               
         MVI   TSOFFACT,TSAADD      SET TSAROFF ADD                             
         GOTO1 TSAROFF,(R2)                                                     
*                                                                               
         CLI   TSERRS,0                                                         
         BE    ARL24                                                            
         DC    H'0'                                                             
*                                                                               
ARL24    XIT1                                                                   
         DROP  R4,R5                                                            
         EJECT                                                                  
* PRINT OVERALL TOTALS FOR ALL CLIENTS *                                        
*                                                                               
EOJTOT   NTR1  BASE=*,LABEL=*                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   SPACING,2                                                        
*                                                                               
         L     R3,AXCLTBLE         EXCLUDED CLIENTS TABLE                       
*                                                                               
         OC    0(3,R3),0(R3)       ANY CLTS                                     
         BZ    XCLT60               NO                                          
*                                                                               
         MVC   P1+2(21),=CL21'EXCLUDED CLIENTS ARE:'                            
         MVC   P2+2(21),=CL21'---------------------'                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R2,P1+2             PRINT LINE                                   
         LA    R0,5                PRINT IN 5 COLUMNS                           
         B     XCLT30                                                           
*                                                                               
XCLT20   LA    R3,3(R3)                                                         
         C     R3,MAXCLTBL         TEST END OF TABLE                            
         BNL   XCLT40               YES                                         
*                                                                               
XCLT30   OC    0(3,R3),0(R3)       ANY MORE CLTS                                
         BZ    XCLT40               NO, DONE                                    
*                                                                               
         MVC   0(3,R2),0(R3)       PRINT CLIENT                                 
         LA    R2,10(R2)                                                        
         BCT   R0,XCLT20                                                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R2,P1+2             PRINT LINE                                   
         LA    R0,5                PRINT IN 5 COLUMNS                           
         B     XCLT20                                                           
*                                                                               
XCLT40   MVI   SPACING,2                                                        
         MVI   P,0                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
XCLT60   MVI   SPACING,2                                                        
         MVC   P+2(33),=CL33'TOTALS FOR ALL CLIENTS THIS MEDIA'                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* REVISION REC TOTALS *                                                         
*                                                                               
         LA    R2,AGYCTRS+REVRECR-COUNTERS                                      
         LA    R3,=C'REV'                                                       
         BRAS  RE,RECTOT                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* PROG EQV REC TOTALS *                                                         
*                                                                               
         LA    R2,AGYCTRS+PEQRECER-COUNTERS                                     
         LA    R3,=C'PEQV'                                                      
         BRAS  RE,ELEMTOT                                                       
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* PATTERN REC TOTALS *                                                          
*                                                                               
         LA    R2,AGYCTRS+PATRECR-COUNTERS                                      
         LA    R3,=C'PAT'                                                       
         BRAS  RE,RECTOT                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* COMMERCIAL REC TOTALS *                                                       
*                                                                               
         LA    R2,AGYCTRS+CMLRECR-COUNTERS                                      
         LA    R3,=C'CML'                                                       
         BRAS  RE,RECTOT                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* LOST COMMERCIALS TOTAL *                                                      
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         L     R2,TOTLCMLC                                                      
         EDIT  (R2),(4,P+8),COMMAS=YES,ZERO=NOBLANK                             
         MVC   P+12(17),=C' LOST COMMERCIALS'                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* ALL OF THIS CODE IS NOP  - BUSINESS ELEMENT                                   
                                                                                
*===========================================================                    
* FURTHER VALIDATE CML (APPROVALS AND DATES IF ANY)                             
*===========================================================                    
*&&DO                                                                           
VCMLAPR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* READ TN1 PROFILE                                                              
*                                                                               
         XC    WORK,WORK           * READ TN PROFILE *                          
         MVC   WORK(4),=X'E2E3D5F1'  STN1                                       
         NI    WORK,X'FF'-X'40'      MAKE S LOWERCASE                           
         MVC   WORK+4(2),AGENCY                                                 
         MVI   WORK+6,C'N'                                                      
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,SVT1PROF,DATAMGR                               
*                                                                               
         MVI   ELCODE,X'90'        BORADCAST BUSINESS ELEMENT                   
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    VAPR10                                                           
*                                                                               
         CLI   SVT1PROF+12,C'Y'    IS THIS BRAND AGENCY                         
         JNE   EQXIT                                                            
         J     NEQXIT                                                           
*                                                                               
         USING CMLBBEL,R6                                                       
VAPR10   DS    0H                                                               
         CLI   CMLBBBAG,C'Y'       IS BRAND AGY=Y                               
         JNE   EQXIT                NO                                          
*                                                                               
         OC    CMLBBMXD,CMLBBMXD   ANY MAX USE DATE                             
         BZ    VAPR20                                                           
*                                                                               
         CLC   SVENDATE,CMLBBMXD UNIT END DATE TO CML MAX USE DTE               
         JH    NEQXIT                                                           
*                                                                               
VAPR20   DS    0H                                                               
         CLI   CMLBBBAG,0          LEO CML?                                     
         BNE   *+12                                                             
         CLI   SVT1PROF+12,C'Y'    IS THIS BRAND AGENCY                         
         BE    *+12                                                             
         CLI   CMLBBBAG,C'Y'       LEO B. CML                                   
         JNE   EQXIT                NO, DONE                                    
*                                                                               
         OC    CMLBBCAD,CMLBBCAD   ANY CLIENT APPROVAL DATE?                    
         JZ    NEQXIT                                                           
*                                                                               
         CLI   CMLBBAPR,C'Y'       BROADCAST BUSINESS APPROVAL?                 
         BNE   VAPR30                                                           
*                                                                               
         OC    CMLBBREF,CMLBBREF   ANY CML REFERENCE?                           
         JZ    NEQXIT                                                           
*                                                                               
* CHECK IF CML FROM REFERENCE FIELD IS APPROVED TO AIR                          
*                                                                               
         XC    KEY(13),KEY                                                      
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=XL2'0A21'                                                
         MVC   CMLKAM(1),BAGYMD                                                 
         MVC   CMLKCLT,SVBCLT                                                   
         MVC   CMLKCML,CMLBBREF    REFERENCE CML                                
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEM                      
         BRAS  RE,GETEL                                                         
         JNE   NEQXIT                                                           
*                                                                               
         USING CMLBBEL,R6                                                       
*                                                                               
VAPR30   CLI   CMLATAIR,C'Y'                                                    
         JNE   NEQXIT              NOT APPROVED TO AIR                          
*                                                                               
         MVC   SVANET,CMLANET      SAVE NET APPROVAL BITS                       
         BAS   RE,GSTATION                                                      
         J     EQXIT                                                            
*                                                                               
* SEE THAT NET IS APPROVED                                                      
*                                                                               
GSTATION NTR1                                                                   
         BRAS  RE,INITNET          CHANGE TO UNIT FILE                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STATRECD,R4         STATION RECORD                               
         MVC   STATKID,=X'29'      RECORD ID                                    
         MVC   STATKAM,BAGYMD      AGY/MED                                      
*                                                                               
         MVC   STATKNET,SVNUKNET   VALIDATE THIS NETWORK ONLY                   
         OC    STATKNET,SPACES                                                  
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(7),KEYSAVE      IS THIS IT                                   
         BNE   GSTATX                                                           
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
*                                                                               
         BRAS  RE,GETEL                                                         
         BE    GSTAT20                                                          
         DC    H'0'                                                             
*                                                                               
         USING STADATEL,R6                                                      
GSTAT20  CLC   STAIDATE,=X'FFFFFF' ANY INACTIVE DATE                            
         BE    GSTAT30              NO                                          
*                                                                               
         MVC   IDATE,STAIDATE                                                   
         XC    IDATE,=X'FFFFFF'    INVERT INACTIVE DATE                         
*                                                                               
         CLC   IDATE,SVSTDATE      IF INACTIVE BEFORE UNIT START                
         BL    GSTATX                                                           
*                                                                               
GSTAT30  DS    0H                                                               
         CLC   STAADATE,SVENDATE   ACTIVE DATE AFTER UNIT END DATE              
         BH    GSTATX               YES                                         
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              AIO2                                         
GSTAT40  MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING STACODEL,R6                                                      
*                                                                               
         MVC   SVCODE,STACODE      SAVE STATION CODE                            
*                                                                               
         MVC   WORK(8),SVANET      APPROVED NETWORKS                            
         NC    WORK(8),SVCODE      AGAINST THIS NETWORK CODE                    
*                                                                               
         BRAS  RE,INITSPT         CHANGE TO SPOT                                
         MVC   AIO,AIO1                                                         
         XC    SVCODE,WORK                                                      
         JZ    EQXIT                                                            
         J     NEQXIT                                                           
*                                                                               
GSTATX   DS    0H                                                               
         BRAS  RE,INITSPT         CHANGE TO SPOT                                
         MVC   AIO,AIO1                                                         
         J     NEQXIT                                                           
         EJECT                                                                  
*===========================================================                    
* ADD REFERENCE COMMERCIAL TO ACTIVE TABLE IF ANY                               
*===========================================================                    
                                                                                
AREFCML  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   DATADISP+1,24       SET FROM NET TO SPOT                         
         MVI   LKEY+1,13                                                        
         MVC   SYSDIR(2),=C'SP'                                                 
         MVC   SYSFIL(2),=C'SP'                                                 
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
*                                                                               
         MVC   SVELCODE,ELCODE                                                  
*                                                                               
         MVI   ELCODE,X'90'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FCLN1XX                                                          
*                                                                               
         USING CMLBBEL,R6                                                       
*                                                                               
         OC    CMLBBREF,CMLBBREF   ANY REFERENCE CML                            
         BZ    FCLN1XX                                                          
*                                                                               
         LA    R5,CMLBBREF                                                      
         BRAS  RE,FCDTE            FIND CML REL/RCL DATES                       
         TM    FOUNDSW,ACTCSW      ACTIVE CML FOUND, ADD TO TBLE                
         BZ    FCLN1XX             NO, DO NOT ADD                               
*                                                                               
         BRAS  RE,FCLEN            GET REF CML LEN                              
         TM    FOUNDSW,CMLFSW                                                   
         BZ    FCLN1XX             CML RECORD NOT FOUND                         
*                                                                               
         L     RE,STRACLST                                                      
*                                                                               
         C     RE,ENDACLST         AT THE END OF LIST                           
         BE    AREFC20                                                          
*                                                                               
AREFC10  CLC   0(12,R5),0(RE)       SAME CML                                    
         BNE   AREFC15                                                          
*                                                                               
AREFC15  OC    0(12,RE),0(RE)       EMPTY ENTRY IN TABLE                        
         BZ    AREFC20              YES                                         
*                                                                               
         LA    RE,12(RE)           BUMP IN CML TABLE                            
         C     RE,STRARLST         OVERFLOW TO REV LIST                         
         BL    *+6                                                              
         DC    H'0'                                                             
         B     AREFC10                                                          
*                                                                               
         MVC   0(8,RE),0(R5)        AND CML                                     
         MVC   8(4,RE),SPACES                                                   
         LA    RE,12(RE)                                                        
         ST    RE,ENDACLST                                                      
         C     RE,STRARLST         SEE IF RUNNING INTO REV LIST                 
         BL    *+6                                                              
         DC    H'0'                                                             
         J     FCLN1XX                                                          
*                                                                               
         DROP  R6                                                               
*&&                                                                             
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENSNV                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPTRKEYS                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE SPTRAFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAE7D                                                       
*        PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR95RR DS    F                                                                
TSAROFF  DS    A                   TSAROFF CORE RESIDENT T00A7D                 
*                                                                               
TSARBUFL DS    A                  BUFFER LENGTH FOR ACIVE REVISION              
TSARBUFF DS    A                   ADDRESS OF GETMAIN BUFFER                    
SEQNUM   DS    H                                                                
SVADIDFL DS    C                                                                
         DS    C                   SPARE                                        
*                                                                               
COUNTERS DS    0F                                                               
SKURECR  DS    F                   SKED UNIT RECS READ                          
SKURECS  DS    F                               TO BE DEL                        
SKURECDL DS    F                               WRITTEN DELETED                  
*                                                                               
REVRECR  DS    F                   REVISION RECS READ                           
REVRECS  DS    F                               TO BE DEL                        
REVRECDL DS    F                               WRITTEN DELETED                  
*                                                                               
PEQRECER DS    F                   PROG EQUIV  ELEMS READ                       
PEQRECEL DS    F                               ELEMS TO BE DELETED              
PEQRECED DS    F                               ELEMS DELETED                    
PEQRECR  DS    F                               RECS READ                        
PEQRECU  DS    F                               RECS UPDATED                     
PEQRECW  DS    F                               RECS WRITTEN                     
PEQRECDL DS    F                               RECS DELETED                     
*                                  PATTERN REC                                  
PATRECR  DS    F                                                                
PATRECS  DS    F                                                                
PATRECDL DS    F                                                                
*                                  COMMERCIAL REC                               
CMLRECR  DS    F                                                                
CMLRECS  DS    F                                                                
CMLRECDL DS    F                                                                
*                                  COMTEXT REC                                  
CMTRECR  DS    F                                                                
CMTRECS  DS    F                                                                
CMTRECDL DS    F                                                                
*                                  INVOICE REC                                  
INVRECR  DS    F                                                                
INVRECS  DS    F                                                                
INVRECDL DS    F                                                                
*                                                                               
TOTLCMLC DS    F                   TOTAL LOST COMMERCIALS (ALL AGY)             
ENDCTRS  EQU   *                                                                
*                                                                               
SVREG    DS    F                   SAVE REGISTER                                
*                                                                               
CLMCT    DS    C                                                                
CTRSIZ   EQU   (*-COUNTERS)/4                                                   
AGYCTRS  DS    (CTRSIZ)F                                                        
STRAGLST DS    F                   START OF ACTIVE PROGRAM LIST                 
ENDAGLST DS    F                                                                
STRAPLST DS    F                   START OF ACTIVE PATTERN LIST                 
ENDAPLST DS    F                                                                
STRACLST DS    F                   START OF ACTIVE COMMERCIAL LIST              
ENDACLST DS    F                                                                
STRARLST DS    F                   ACTIVE REV LIST                              
ENDARLST DS    F                                                                
MAXTBLSZ DS    F                                                                
STRLCLST DS    F                   START OF LOST COMMERCIALS LIST               
ENDLCLST DS    F                                                                
MAXLCSZ  DS    F                                                                
STADILST DS    F                   START OF LOST AI-ID LIST                     
ENADILST DS    F                                                                
MAXACLST DS    A                                                                
MAXADSZ  DS    F                                                                
AXCLTBLE DS    F                   START OF EXCLUDED CLIENTS LIST               
MAXCLTBL DS    F                                                                
VGTBROAD DS    A                                                                
VTRPACK  DS    A                                                                
*                                                                               
ATYPTBL  DS    F                                                                
ATYPMAX  DS    F                                                                
*                                                                               
* FORMAT OF REC TYPES AND COUNTS                                                
*                                                                               
* TYPE   1 BYTE                                                                 
* BAGYMD 1                                                                      
* SPARE  2                                                                      
* COUNT  4                                                                      
*                                                                               
AMISCLT  DS    F                   ADDR OF MISSING CLIENTS TABLE                
AMISMAX  DS    F                                                                
*                                                                               
* FORMAT OF MISSING CLT REC COUNTS TABLE                                        
*                                                                               
* TYPE   1                                                                      
* BAGYMD 1 BYTE                                                                 
* BCLT   2                                                                      
* RECS   4                                                                      
*                                                                               
TOTRECRD DS    F                                                                
TOTRECDL DS    F                                                                
*                                                                               
CLTOKSW  DS    XL1                 0 = CLT FOUND, N = NO CLT                    
*                                                                               
SV41CLT  DS    CL3                 SAVED CHAR CLT CODE FOR 41 REC               
*                                                                               
MISCLTCT DS    H                                                                
DELCTR   DS    H                                                                
*                                                                               
SVTPAGCL DS    XL4                 SAVED TYPE, BAGY, BCLT                       
SVMDCLT  DS    XL3                 SAVED BAGY, BCLT                             
*                                                                               
ENDRECSW DS    CL1                                                              
*                                                                               
FINDBUG  DS    XL40                 SAVE ADCONS FOR BUG                         
*                                                                               
*                                                                               
WRTRECSW DS    XL1                 WRITE RECORD SWITCH                          
*                                                                               
FOUNDSW  DS    XL1                 FOUND SWITCH                                 
CMLFSW   EQU   X'10'               CML FOUND SW                                 
UNTFSW   EQU   X'20'               UNIT RECORD FOUND                            
XSKEDSW  EQU   X'40'               EXPIRED SKED FOUND                           
ACTCSW   EQU   X'80'               ACTIVE CML FOUND ADD TO TABLE                
PCMLSW   EQU   X'01'               PACKED COMMERCIAL                            
*                                                                               
OFFICE   DS    CL1                                                              
LAOFFICE DS    CL1                 OFFICE FOR THIS LIMITED ACCESS               
PRTDATE  DS    CL8                                                              
EXPDATE  DS    XL3                 EXPIRATION DATE (BINARY YMD)                 
EXPDTCMP DS    XL2                             AND COMPRESSED                   
SVBCLT   DS    XL2                                                              
SAVEBCLT DS    XL2                                                              
BBCLT    DS    XL2                                                              
*                                                                               
SVROT    DS    XL1                 SAVE UNIT ROTATION                           
*                                                                               
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
*                                                                               
TSRCOUNT DS    XL3                 TSAR RECORD COUNT                            
*                                                                               
SVELCODE DS    CL1                 ELCODE                                       
SVUNTKEY DS    CL24                SAVE UNIT KEY                                
SVPATKEY DS    CL(L'KEY)           SAVE PATTERN KEY                             
MYKEYSV  DS    CL(L'KEYSAVE)                                                    
*                                                                               
REVDATA  DS    CL12                2-BCLT,4-NET,6-PROG                          
REVPER   DS    XL3                 LAST REV PERIOD PROCESSED                    
REVBPER  DS    XL2                 PERIOD IN BINARY                             
REVNUM   DS    XL1                  AND REVISION NUMBER                         
*                                                                               
*                                                                               
STDATEB  DS    CL6 -----------     EBCDIC BROADCAST START                       
ENDATEB  DS    CL6           º      AND END DATE                                
STDATEC  DS    CL6           º     EBCDIC CALENDAR START                        
ENDATEC  DS    CL6 -----------      AND END DATE                                
*                                                                               
STDATEBB DS    XL3 -----------     BINARY BROADCAST START                       
ENDATEBB DS    XL3           º      AND END DATE                                
STDATECB DS    XL3           º     BINARY CALENDAR START                        
ENDATECB DS    XL3 -----------      AND END DATE                                
*                                                                               
STDATEP  DS    XL3                 START AND                                    
ENDATEP  DS    XL3                  END PERIOD  IN BINARY                       
*                                                                               
SVBPLST  DS    CL60                SAVE BASE PROG (UPTO 10 PROGS)               
*                                                                               
SVFIELDS DS   0CL(SVEND)                                                        
SVFEED   DS    CL4                                                              
SVMEDIA  DS    CL1                                                              
SVNUKNET DS    CL4  ------|  KEEP THESE FIELDS TOGETHER                         
SVKPROG  DS    CL6  ------|  AND IN ORDER -- SAVE NETWORK & PROG                
SVKDATE  DS    CL2                 AIR DATE                                     
SVKDATE2 DS    CL2                 + ROTATION                                   
SVSTDATE DS    XL3                 UNIT START                                   
SVENDATE DS    CL3                 PLUS ROTATION                                
SVPROD1  DS    XL3                                                              
SVPLEN1  DS    XL1                                                              
SVPROD2  DS    XL3                                                              
SVPLEN2  DS    XL1                                                              
SVDPC    DS    XL1                                                              
SVCMLSLN DS    XL1                                                              
SVCML1   DS    XL8     ------                                                   
SVCML2   DS    XL8          º      KEEP TOGETHER                                
SVPKSPEC DS    XL1          º      SAVE PAT KEY SPECIFICS BITS                  
SVCSPRD  DS    XL1     ------                                                   
SVCSPROD DS    XL3                                                              
SVADID1  DS    CL12                                                             
SVADID2  DS    CL12                                                             
SVEND    EQU   *-SVFEED                                                         
*                                                                               
SVSTDTEP DS    XL3                 SAVE START AND                               
SVENDTEP DS    XL3                  END PERIOD  IN BINARY                       
*                                                                               
SVADID   DS    CL12                SAVE PRINTABLE AD-ID                         
*                                                                               
SVACTCML DS    CL49                SAVE ACTUAL CMLS (MAX 4) +1                  
*                                                                               
SVPROD   DS    XL3                                                              
SVANET   DS    XL8                 SAVED BITS FOR APPROVED NETS                 
IDATE    DS    XL3                 INACTIVE DATE FROM STATION APPROVAL          
SVCODE   DS    XL8                 SAVE STATION CODE                            
*                                                                               
CSPRDSW  DS    CL1                 COPY SPLIT SWITCH                            
PRINTPTR DS    H                                                                
*                                                                               
SVCML    DS    CL8                                                              
SVCMLCOD DS    CL12                                                             
SVCMLSEQ DS    XL3                                                              
SVCMLSTA DS    XL1                                                              
CMLCMTFL DS    XL1                 FLAG TO SHOW IF COMMENTS REC FOUND           
*                                                                               
FILTERS  DS    0CL16                                                            
DATE     DS    CL6                                                              
DATEFTR  DS    XL3                                                              
DATE2FTR DS    XL3                                                              
DATESFTR DS    XL1                                                              
HOLDSIGN DS    XL1                                                              
*                                                                               
FTRSW1   DS    XL1                 FILTER SWITCH                                
PATSW    EQU   X'80'          IF FILTER SWITCH IS NOT SPECIFIED                 
CMLSW    EQU   X'40'            THEN PURGE ALL, ELSE DO SELECTED RECS           
PEQVSW   EQU   X'20'                                                            
REVSW    EQU   X'10'                                                            
*                                                                               
FTRSW2   DS    XL1                                                              
TESTSW   EQU   X'10'                                                            
SKULSW   EQU   X'20'               SKED UNIT LIST SWITCH                        
TRCESW   EQU   X'40'                                                            
SKIPUSW  EQU   X'80'               SKIP PU PROFILE                              
TRCETYP  DS    XL1                                                              
         DS    X                                                                
*                                                                               
         DS    0D                                                               
TSARBLK  DS    CL(TSARDL2)                                                      
*                                                                               
TABLES   DS    0D                                                               
*                                                                               
* DSECT FOR ACTIVE PATTERN LIST                                                 
*                                                                               
APLIST   DSECT                                                                  
APLNET   DS   0CL4                 NETWORK                                      
         DS    XL1                                                              
APLMED   DS    XL1                 IF 3RD BYTE = FF THEN                        
APLNFF   DS    XL1                   2ND BYTE IS MEDIA (N/C/S/O)                
         DS    XL1                                                              
*                                                                               
APLPROG  DS   0XL6                                                              
APLFF    DS    XL1                 IF FF THEN                                   
APLCODE  DS    XL1                   DP CODE AND                                
APLFEED  DS    XL4                   FEED                                       
*                                                                               
APLPRD1  DS    XL3                                                              
APLSLN1  DS    XL1                                                              
APLPRD2  DS    XL3                                                              
APLSLN2  DS    XL1                                                              
APLREFS  DS    XL3                                                              
APLNEXT  EQU   *                                                                
APLISTLN EQU   APLNEXT-APLIST                                                   
*                                                                               
* DSECT FOR ACTIVE REVISION LIST                                                
*                                                                               
ARLIST   DSECT                                                                  
ARLENT   DS   0XL12                                                             
ARLNET   DS    CL4                 NETWORK                                      
ARLPRG   DS    CL6                 PROGRAM                                      
ARLPER   DS    XL2                 PERIOD                                       
ARLNEXT  EQU   *                                                                
ARLISTLN EQU   ARLNEXT-ARLIST                                                   
*                                                                               
* OFFLINE PRINT REPORT                                                          
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PSUNET   DS    CL4                 SKED UNIT                                    
         DS    CL6                                                              
PSUPRG   DS    CL6                                                              
         DS    CL4                                                              
PSUDATE  DS    CL8                                                              
         DS    CL3                                                              
PSUDPC   DS    CL2                                                              
         DS    CL3                                                              
PSUPRD   DS    CL7                                                              
         DS    CL1                                                              
PSUPTR   DS    CL7                                                              
         DS    CL5                                                              
PSUCML   DS    CL25                                                             
         ORG   P                                                                
         DS    CL2                                                              
PRPNET   DS    CL4                 REVISION                                     
         DS    CL2                                                              
PRPROG   DS    CL6                                                              
         DS    CL2                                                              
PREDATE  DS    CL8                                                              
         DS    CL1                                                              
PRREV    DS    CL3                                                              
         DS    CL1                                                              
PRPRD    DS    CL3                                                              
         DS    CL2                                                              
PRADATE  DS    CL8                                                              
         DS    CL2                                                              
PRRDATE  DS    CL8                                                              
         DS    CL1                                                              
PRIDATE  DS    CL8                                                              
         DS    CL2                                                              
PRCMT    DS    CL60                                                             
         ORG   P                                                                
         DS    CL2                 EQUIVALENCY RECORD                           
PENET    DS    CL4                                                              
         DS    CL4                                                              
PEPRG    DS    CL6                                                              
         DS    CL19                                                             
PEBPRG   DS    CL6                                                              
         DS    CL16                                                             
PESTR    DS    CL8                                                              
         DS    CL5                                                              
PEEND    DS    CL8                                                              
         ORG   P                                                                
PPPRD    DS    CL7                 PATTERN RECORD                               
         DS    CL1                                                              
PPPTR    DS    CL7                                                              
         DS    CL2                                                              
PPCODE   DS    CL1                                                              
         DS    CL4                                                              
PPFEED   DS    CL4                                                              
         DS    CL1                                                              
PPNET    DS    CL4                                                              
         DS    CL4                                                              
PPROG    DS    CL6                                                              
         DS    CL2                                                              
PPERIOD  DS    CL14                                                             
         DS    CL1                                                              
PPDESC   DS    CL24                                                             
PPREF    DS    CL3                                                              
         DS    CL3                                                              
PPCMLS   DS    CL25                                                             
         ORG   P                                                                
         DS    CL2                                                              
PCCML    DS    CL12                COMMERCIAL RECORD                            
         DS    CL2                                                              
PCTITLE  DS    CL15                                                             
         DS    CL2                                                              
PCLEN    DS    CL3                                                              
         DS    CL3                                                              
PCTYPE   DS    CL4                                                              
         DS    CL2                                                              
PCDATES  DS    CL17                                                             
         DS    CL2                                                              
PCCLTCML DS    CL20                                                             
         DS    CL2                                                              
PCINACT  DS    CL14                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'101SPTRA95   09/11/19'                                      
         END                                                                    
