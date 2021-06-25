*          DATA SET SPTRA34    AT LEVEL 035 AS OF 11/19/15                      
*PHASE T21634A                                                                  
*INCLUDE XSORT                                                                  
*INCLUDE PQPROF                                                                 
*        TITLE 'T21634 COVER LETTER INSTRUCTIONS'                               
***********************************************************************         
*                                                                     *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO1 - READ INSTRUCTION RECAP RECS                      *         
*                    READ PATTERN RECS IN PRT RTN                     *         
*             AIO2 - RD CMLS IN PRT RTN, BUILD CMTS IN PRT            *         
*             AIO3 - PATTERN TABLES BUILT HERE ONLINE                 *         
*             BLOCK - USED IN VOPTIONS                                *         
*      NOT ANY MORE - LR RTN TO BUILD PTN TBL FOR EACH STAT           *         
*                     PRT RTN TO FIND COM REV, INS DATE TEL DATES     *         
*                     PRT RTN TO SAVE CMML TEXT KEYS                  *         
*             TWA+3000 FOR 1000                                       *         
*                     LR RTN TO BUILD PTN TBL FOR EACH STATION        *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR       *         
*        R3 - POINTER TO PATTERN TABLES                               *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - STATION TABLE POINTER                                   *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 - WORK                                                    *         
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
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*                          TOMBSTONE                                  *         
*                                                                     *         
* MAY/08          MHER REWRITE FOR NEW OUTPUT FORMAT                  *         
* LEV 15 AUG08/08 MNAS FIX COMTEXT PRINTING BUG                       *         
* LEV 16 AUG13/08 MNAS ARCHIVING NOW REPORTS                          *         
* LEV ?? SEP??/08 MHER PATTERNS BY TIME AND DAYPART                   *         
* LEV 30 MNAS MAR03/11 HONOR TW PROFILE FOR ID FDMFDA WHEN COMING     *         
*             FROM ANOTHER SCREEN - ELSE VALUES GET CARRIED OVER      *         
* LEV 32 MNAS   APR/11 TELECASTER                                     *         
* LEV 33 MNAS 2/21/13 SEND RUN LAST MODE TO CLOSE FILES ON EXIT       *         
***********************************************************************         
         TITLE 'T21634 COVER LETTER INSTRUCTIONS'                               
T21634   CSECT                                                                  
FAXMAX   EQU   42                                                               
         PRINT NOGEN                                                            
         NMOD1 0,T21634**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,SPTR34RR                                                      
         L     RF,=V(PQPROF)                                                    
         AR    RF,R2                                                            
         ST    RF,PQPROF                                                        
         LH    R0,=AL2(ENDSYSD-SYSD)                                            
         C     R0,LSYSD                                                         
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    PRT                                                              
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   EXIT                                                             
         BRAS  RE,VK               GO VALIDATE KEY FIELDS                       
                                                                                
         MVI   SVQLTYP1,0                                                       
         MVI   SVFAXARC,0                                                       
         XC    WORK,WORK                                                        
         XC    PROFKEY,PROFKEY                                                  
         MVI   PROFKEY,C'S'                                                     
         MVC   PROFKEY+1(2),=C'TV'                                              
         MVC   PROFKEY+3(2),TWAORIG                                             
         GOTO1 PQPROF,DMCB,(X'80',PROFKEY),(0,WORK),ACOMFACS                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,WORK                                                          
         USING REMOTED,R4                                                       
         MVC   SVQLTYP1,REMOTTY1                                                
                                                                                
         CLI   REMOTSUB,C'#'                                                    
         BNE   GEN12                                                            
         CLI   REMOTSUB+1,C'N'                                                  
         BE    GEN12A                                                           
         CLI   REMOTSUB+1,C'A'                                                  
         BNE   GEN12                                                            
         OI    SVFAXARC,REMOTARQ                                                
         B     GEN12A                                                           
GEN12    OI    SVFAXARC,REMOTAEQ                                                
GEN12A   DS    0H                                                               
                                                                                
         TM    WHEN,X'40'                                                       
         BZ    PROC10                                                           
         CLI   TRAFAX,C'Y'                                                      
         BE    PROC10                                                           
         OI    GENSTAT3,NOCLRSPK                                                
         MVI   PQSW,1                                                           
         LA    R1,ELEM                                                          
         ST    R1,SPOOLQLK                                                      
         USING PQPLD,R1                                                         
         XC    ELEM(128),ELEM                                                   
         MVC   QLTYP1,SVQLTYP1                                                  
         MVC   QLDESC(3),=C'SCI'                                                
         MVI   QLEXTRA,X'FF'       NEW PARM LIST                                
         OI    SPOOLIND,X'40'      USER VALUES PRESENT                          
         GOTO1 OPENPQ                                                           
PROC10   DS    0H                                                               
         DROP  R4,R1                                                            
                                                                                
         B     EXIT                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*======================================================                         
* FORMAT OFFLINE REPORT                                                         
*======================================================                         
                                                                                
PRT      BRAS  RE,SETSTOR                                                       
*                                                                               
PRT002   BRAS  RE,RDRECAP                                                       
*                                                                               
*MN                                                                             
*        MVI   TWAFIRST,2          SEND RUN LAST MODE/GLOBAL FILES              
*MN                                                                             
         L     R5,ASTATAB                                                       
         USING STATABD,R5                                                       
*                                                                               
PRT004   L     R0,ASVADDL          CLEAR ADDITIONAL CMML DATA                   
         L     R1,ASVADDLX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BRAS  RE,SETUP                                                         
         BRAS  RE,SETPQ            INITIALIZE PQ REPORT                         
*                                                                               
         MVC   WORK(4),SVGENDTP                                                 
         CLI   SVT1PR5,C'Y'        USE FTD AND LTD, NOT FLIGHT                  
         BNE   *+10                                                             
         MVC   WORK(4),SVTCDTS                                                  
*                                                                               
         GOTO1 DATCON,DMCB,(2,WORK),USERQSTR                                    
         GOTO1 (RF),(R1),(2,WORK+2),USERQEND                                    
*                                                                               
         LA    R0,14                                                            
         LA    R1,H1                                                            
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
                                                                                
*==========================================================                     
* PRINT STATION LIST AND COVER LETTER COMMENTS                                  
*==========================================================                     
                                                                                
         BRAS  RE,PRTSTA                                                        
                                                                                
*==========================================================                     
* PRINT LETTER OF INSTRUCTIONS FOR STATION LIST                                 
*==========================================================                     
                                                                                
         MVI   LINE,99                                                          
         MVC   PAGE,=H'1'                                                       
         MVI   CONTINUE,C'Y'                                                    
                                                                                
* GET PATTERN TABLE ENTRY ADDRESS                                               
                                                                                
PRT010   CLI   BPRD,X'FF'          WE DOING POL INSTR                           
         BNE   PRT020                                                           
*                                                                               
         L     R1,APRDTAB                                                       
         SR    R0,R0                                                            
         ICM   R0,3,CURTABNO                                                    
         BCTR  R0,0                                                             
         SLL   R0,8                TIMES 256                                    
         AR    R1,R0                                                            
         OC    0(256,R1),0(R1)     BETTER BE AN ENTRY                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R1,ACURPRDT                                                      
         L     R0,ASVCLIST                                                      
         ST    R0,ACURPRPT                                                      
         BRAS  RE,FNDCUR           FIND REAL CURTABNO FOR PTN PTR               
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,FULL             POINT TO TABLE ENTRY                         
*                                                                               
PRT020   L     R3,APATTAB                                                       
         USING PTNTABD,R3                                                       
         SR    RE,RE                                                            
         ICM   RE,3,CURTABNO                                                    
         B     *+8                                                              
*                                                                               
PRT022   AH    R3,SVPTNSIZ                                                      
         LH    R1,0(R3)                                                         
         AHI   R1,-HDRLEN                                                       
         STH   R1,SVPTNSIZ                                                      
         LR    RF,R3                                                            
         AHI   R3,HDRLEN                                                        
         BCT   RE,PRT022                                                        
         ST    R3,ACURRPTN                                                      
*                                                                               
         LR    R0,R3                                                            
         AH    R0,SVPTNSIZ                                                      
         ST    R0,ACURRPTX         SAVE ADDR OF PATTERN ENTRY END               
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,STAMKT         GET MKT FROM STATION TABLE                   
         STCM  RE,3,BMKT                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         EJECT                                                                  
*                                                                               
         MVI   PIGSW,C'N'          RESET P/B PRESENT FLAG                       
         L     RE,ACURRPTN                                                      
*                                                                               
PRT024   CLI   PTNBPRD2-PTNTABD(RE),0 TEST P/B PATTERN                          
         BNE   PRT026                                                           
         LA    RE,PTNEXT-PTNTABD(RE)                                            
         C     RE,ACURRPTX                                                      
         BL    PRT024                                                           
         B     PRT028                                                           
*                                                                               
PRT026   MVI   PIGSW,C'Y'                                                       
*                                                                               
PRT028   MVI   HEADSW,0            SUPPRESS MIDS TILL COMMENTS PRINT            
         BRAS  RE,PRTEXT           GO PRINT HEADLINE TEXT                       
*                                                                               
         MVI   HEADSW,C'Y'         SET MIDLINES PRINTING                        
         MVI   FORCEMID,C'Y'                                                    
                                                                                
*=============================================================                  
* GET A PATTERN RECORD                                                          
*=============================================================                  
                                                                                
PRT030   CLC   =X'FFFFFF',PTNREF   TEST TBA PATTERN                             
         BE    PRT048                                                           
         OC    PTNREF,PTNREF       TEST HIATUS PATTERN                          
         BZ    PRT044X                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING PATKEY,R1                                                        
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM(3),BAGYMD AND BCLT                                        
         MVC   PATKPRD(2),PTNBPRD                                               
         MVC   PATKPRD2,PTNBPRD2                                                
         MVC   PATKSLN2,PTNSLN2                                                 
         MVC   PATKCODE,PTNCOPY                                                 
         MVC   PATKREF,PTNREF                                                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRT032                                                           
* IN CASE COPY CODE SHOULDN'T BE THERE, TRY ONE MORE TIME                       
         MVC   KEY,KEYSAVE         RESTORE                                      
         MVI   PATKCODE,0                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRT032                                                           
         B     PRT172              NOT THERE, JUST GO ON                        
         DROP  R1                                                               
*                                                                               
PRT032   L     R6,AIO1             USE IO1 FOR PATTERN REC                      
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'50'        NEED TO ADD TEXT KEY TO ENTRY                
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   PTNSTXT,3(R6)                                                    
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
*                                                                               
         CLC   =X'FFFFFF',PATEND   TEST PATTERN RUNS UFN                        
         BNE   *+8                 NO                                           
         OI    PTNFLAG,PTNFUFN                                                  
                                                                                
         CLI   PATDTALN,38         OLD PATTERN?                                 
         BNH   PRT044                                                           
         TM    PATSTAT,X'04'       TEST INVERT PRODUCTS                         
         BZ    *+8                                                              
         OI    PTNFLAG,PTNFIPR                                                  
*                                                                               
* FIND X'30' ELEMENT TO TEST PATTERN IS HIATUS *                                
*                                                                               
PRT044   L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'HIATUS',2(R6)                                                 
         BNE   PRT046                                                           
PRT044X  OI    PTNFLAG,PTNFHIA                                                  
         XC    PTNREF,PTNREF       CLEAR REF/SUBLINE FOR HIATUS                 
*                                                                               
PRT046   TM    PTNFLAG,PTNFIPR     INVERT PRDS                                  
         BZ    PRT048               NO                                          
         CLI   PTNBPRD2,0          TEST PIGGYBACK                               
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         IC    R1,PTNBPRD2                                                      
         MVC   PTNBPRD2,PTNBPRD                                                 
         STC   R1,PTNBPRD                                                       
         IC    RE,PTNSLN                                                        
         MVC   PTNSLN,PTNSLN2                                                   
         STC   RE,PTNSLN2                                                       
*                                                                               
PRT048   L     R4,APRTBUFF                                                      
         USING PLINED,R4                                                        
*                                                                               
**NOP**  C     R3,APATTAB          TEST FIRST PATTERN                           
**NOP**  BE    PRT050                                                           
**NOP**  MVI   0(R4),0                                                          
**NOP**  LA    R4,132(R4)                                                       
*                                                                               
PRT050   MVC   0(32,R4),=C'=====> ROTATE THE FOLLOWING FROM'                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,PTNFTD),(8,34(R4))                                
         MVI   42(R4),C'-'                                                      
*                                                                               
         GOTO1 (RF),(R1),(2,PTNLTD),(8,43(R4))                                  
*                                                                               
         OC    PTNSTIM,PTNSTIM     TEST TIMES ENTERED                           
         BZ    PRT050A                                                          
         TM    PTNFLAG1,PTNFDLY    TEST TIMES ARE DAILY                         
         BO    PRT051                                                           
*                                                                               
         MVC   WORK(8),43(R4)      SAVE THE PRINTABLE END DATE                  
         MVC   42(9,R4),SPACES                                                  
*                                                                               
         MVC   42(4,R4),=C' AT '                                                
         XC    DUB,DUB                                                          
         MVC   DUB(2),PTNSTIM                                                   
         GOTO1 UNTIME,DMCB,DUB,46(R4)                                           
*                                                                               
         LA    R1,46(R4)                                                        
         LA    R1,1(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    *-8                                                              
         MVC   0(7,R1),=C' UNTIL '                                              
         MVC   7(8,R1),WORK        MOVE SAVED END DATE                          
         MVC   15(4,R1),=C' AT '                                                
*                                                                               
         LA    R1,19(R1)                                                        
         ST    R1,DMCB+4                                                        
         MVC   DUB(2),PTNETIM                                                   
         GOTO1 (RF),DMCB,DUB                                                    
         B     PRT054                                                           
*                                                                               
PRT050A  CLI   SVPROF11,C'D'       COPY CODE DAYPART                            
         BNE   PRT052              NO                                           
         MVC   53(8,R4),=C'DAYPART='                                            
         MVC   61(1,R4),PTNCOPY                                                 
         B     PRT054                                                           
*                                                                               
PRT051   MVC   52(14,R4),=C'RUN DAILY FROM'                                     
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(2),PTNSTIM                                                   
         GOTO1 UNTIME,DMCB,DUB,67(R4)                                           
*                                                                               
         LA    R1,67(R4)           FIND THE END OF THE TIME                     
         LA    R1,1(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    *-8                                                              
         MVC   0(4,R1),=C' TO '                                                 
         LA    R1,4(R1)                                                         
*                                                                               
         ST    R1,DMCB+4                                                        
         MVC   DUB(2),PTNETIM                                                   
         GOTO1 (RF),DMCB,DUB                                                    
*                                                                               
         L     R1,DMCB+4                                                        
         CLI   0(R1),C' '          FIND THE END OF END TIME                     
         BNH   PRT051A                                                          
         LA    R1,1(R1)                                                         
         B     *-12                                                             
*                                                                               
PRT051A  MVC   1(4,R1),=C'ONLY'                                                 
         B     PRT054                                                           
*                                                                               
PRT052   CLI   SVPROF11,C'A'       COPY CODE ADJACENCY                          
         BNE   PRT053              NO                                           
         CLI   PTNCOPY,0           ANY CODE                                     
         BE    PRT054              NO                                           
         MVC   53(9,R4),=C'ADJ CODE='                                           
         MVC   62(1,R4),PTNCOPY                                                 
         B     PRT054                                                           
*                                                                               
PRT053   CLI   PTNDPT,0            TEST DAYPART ENTERED                         
         BE    PRT054              NO                                           
         MVC   53(14,R4),=C'DAYPART=  ONLY'                                     
         MVC   61(1,R4),PTNDPT                                                  
*                                                                               
PRT054   LA    R4,132(R4)                                                       
         ST    R4,FULL             SAVE PRINT LINE ADDRESS                      
*                                                                               
         CLC   QPRD,=C'POL'                                                     
         BNE   PRT056                                                           
         MVC   0(4,R4),=C'PRD='                                                 
         LA    R1,PTNBPRD                                                       
         MVC   AIO,AIO2                                                         
         BRAS  RE,GPRD                                                          
*                                                                               
         USING SVPRDD,R1                                                        
         MVC   4(3,R4),SVPRDEBC                                                 
         MVC   8(20,R4),SVPRDNM                                                 
         LA    R4,132(R4)                                                       
         ST    R4,FULL                                                          
         DROP  R1                                                               
                                                                                
*==================================================================             
* NOW DO PATTERN CHANGE REASONS                                                 
*==================================================================             
                                                                                
PRT056   DS    0H                                                               
*&&DO                                                                           
         OC    SVINSDT,SVINSDT     IF ORIGINAL                                  
         BZ    PRT060                                                           
         CLI   SVINSREV,0          IF REVISION ZERO                             
         BNE   *+12                                                             
         TM    SVOPT,OPTRERUN      AND RERUN (ORIGINAL)                         
         BO    PRT060                                                           
*                                                                               
         TM    PTNFLAG,PTNNEW                                                   
         BZ    *+14                                                             
         MVC   0(25,R4),=C'****** NEW PATTERN ******'                           
         B     PRT058                                                           
*                                                                               
         TM    PTNFLAG,PTNFSTX+PTNCHGE      TEST ANY CHANGES                    
         BNZ   *+12                                                             
         TM    SVTBIND,X'20'                                                    
         BZ    PRT060              NO                                           
*                                                                               
         MVC   0(17,R4),=C'****** CHANGES TO'                                   
         LA    R4,18(R4)                                                        
*                                                                               
         TM    PTNFLAG,PTNCHGE                                                  
         BZ    *+14                                                             
         MVC   0(8,R4),=C'PATTERN,'                                             
         LA    R4,9(R4)                                                         
*                                                                               
         TM    PTNFLAG,PTNFSTX                                                  
         BZ    *+14                                                             
         MVC   0(13,R4),=C'PATTERN TEXT,'                                       
         LA    R4,14(R4)                                                        
*                                                                               
         TM    SVTBIND,X'20'                                                    
         BZ    *+14                                                             
         MVC   0(9,R4),=C'SCHEDULE,'                                            
         LA    R4,10(R4)                                                        
*                                                                               
         AHI   R4,-2               BACK UP TO LAST CHAR                         
         CLI   0(R4),C','                                                       
         BNE   *+10                                                             
         MVI   0(R4),C' '                                                       
         BCTR  R4,0                                                             
         MVC   2(6,R4),=C'******'                                               
*                                                                               
PRT058   L     R4,FULL             UPDATE FIRST PRINT LINE ADDRESS              
         LA    R4,132(R4)                                                       
         ST    R4,FULL                                                          
*&&                                                                             
PRT060   BRAS  RE,FMTSLN                                                        
         LA    R4,132(R4)                                                       
*                                                                               
         CLI   PTNBPRD2,0          TEST REAL PIGGYBACK                          
         BE    PRT062              NO                                           
*                                                                               
         MVC   0(4,R4),=C'PRD='                                                 
         LA    R1,PTNBPRD                                                       
         BRAS  RE,GETPRD                                                        
         MVC   4(3,R4),0(RF)       MOVE PRD                                     
         LA    R4,132(R4)                                                       
*                                                                               
         MVC   0(4,R4),=C'P/B='                                                 
         LA    R1,PTNBPRD2                                                      
         BRAS  RE,GETPRD                                                        
         MVC   4(3,R4),0(RF)       MOVE PRD                                     
*                                                                               
PRT062   L     R4,FULL             RESTORE PRINT LINE ADDRESS                   
*                                                                               
         TM    PTNFLAG,PTNFHIA     TEST HIATUS                                  
         BZ    PRT064              NO                                           
*                                                                               
         MVC   PLROT(36),=C'** HIATUS-NO COMMERCIALS ASSIGNED **'               
         MVI   SPACING,2                                                        
         BRAS  RE,PRTBUFF          GOTO SPOOL RTN                               
         B     PRT170              AND CONTINUE                                 
*                                                                               
PRT064   CLC   =X'FFFFFF',PTNREF   TEST TBA PATTERN                             
         BNE   PRT070                                                           
*                                                                               
PRT065   CLI   SVT1PR3,C'S'        SUPPRESS TO BE ASSIGNED                      
         BE    PRT170                                                           
*                                                                               
         L     R4,FULL             RESTORE PL ADDRESS                           
         MVC   PLROT(32),=C'** COMMERCIALS TO BE ASSIGNED **'                   
*                                                                               
         CLI   SVPROF11,C'P'       USING 1ST CHAR OF PROG NAME                  
         BNE   *+8                 NO                                           
         BAS   RE,FMTCOD                                                        
*                                                                               
         MVI   SPACING,2                                                        
         BRAS  RE,PRTBUFF          GOTO SPOOL RTN                               
         B     PRT170              AND CONTINUE                                 
         EJECT                                                                  
*==================================================================             
* PROCESS A REAL PATTERN ENTRY - NOT HIATUS/MISSING/TBA                         
*==================================================================             
                                                                                
PRT070   BRAS  RE,BLDCMLS                                                       
*                                                                               
         L     R6,AIO1             RESTORE AIO TO PATTERN REC ADDRESS           
         ST    R6,AIO                                                           
*                                                                               
         L     R2,ASVCMLS                                                       
         USING SVCMLD,R2                                                        
         OC    0(8,R2),0(R2)       TEST IF ENTRIES IN LIST                      
         BNZ   PRT072              YES                                          
*MN      DC    H'0'                IF THEY HAVE DELETED ALL CMMLS               
         B     PRT065              DON'T DIE - TREAT AS TBA                     
*MN                                                                             
*                                                                               
PRT072   MVI   DOPCT,C'Y'          ASSUME PRINTING ROT AS PCTS                  
         BRAS  RE,SETPCTS          SET PCTS IN SVCMML BUFFER                    
*                                                                               
PRT080   CLI   SVT1PR12,C'Y'       TEST SUPPRESS PATTERN ROT                    
         BE    PRT084                                                           
         CLI   SVCMLPIG,2          IF THIS IS PRD2, NO PRINT                    
         BE    PRT084                                                           
*                                                                               
         CLI   DOPCT,C'Y'                                                       
         BNE   PRT082                                                           
         SR    R0,R0                                                            
         ICM   R0,3,SVCMLPCT                                                    
         EDIT  (R0),(3,PLROT)                                                   
         MVC   PLROT+4(3),=C'PCT'                                               
         B     PRT084                                                           
*                                                                               
PRT082   LLC   RE,SVCMLPOS         NO PCTS SO PRINT ROT LETTER                  
         LA    RE,ALPHATAB-1(RE)                                                
         MVC   PLROT+2(3),=C'( )'                                               
         MVC   PLROT+3(1),0(RE)                                                 
         B     PRT084                                                           
ALPHATAB DC    C'ABCDEFGHIJKLMNO'                                               
*                                                                               
PRT084   LA    R1,PLCML                                                         
         CLI   SVCMLPIG,2          IS THIS PARTNER ENTRY                        
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
*                                                                               
         MVC   0(12,R1),SVCMLADI                                                
         CLI   SVCMLPIG,1                                                       
         BNE   PRT086                                                           
         MVI   PLCML-1,C'('                                                     
*                                                                               
         LA    R1,11(R1)           POINT TO LAST CHAR                           
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'-'                                                       
         B     PRT090                                                           
*                                                                               
PRT086   CLI   SVCMLPIG,2                                                       
         BNE   PRT090                                                           
         LA    R1,11(R1)                                                        
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C')'                                                       
                                                                                
*==============================================================                 
* PRINT LENGTH OVERRIDES OR LEN= WHEN                                           
* THERE ARE 1 FILM/2 PRDS  (SVCMLPIG=0 IF ONLY 1 FILM)                          
*        OR 2 FILMS/1 PRD  (SVCMLPIG>0 AND PTNBPRD2=0)_                         
*==============================================================                 
                                                                                
PRT090   ST    R4,FULL             SAVE PRINT LINE ADDRESS                      
         CLI   SVCMLOV1,0          TEST FOR SLN OVERRIDE                        
         BE    PRT092              NO                                           
         MVC   PLCMLNAM(9),=C'LEN OVRD='                                        
         LLC   R0,SVCMLOV1                                                      
         BAS   RE,EDTSLN                                                        
         MVC   PLCMLNAM+9(3),DUB                                                
         LLC   R0,SVCMLOV2                                                      
         BAS   RE,EDTSLN                                                        
         LA    RE,PLCMLNAM+11      POINT TO LAST CHAR                           
         CLI   0(RE),C' '                                                       
         BH    *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'/'                                                       
         MVC   2(3,RE),DUB                                                      
         B     PRT096                                                           
*                                                                               
PRT092   CLI   PTNBPRD2,0          TEST REAL P/B                                
         BE    PRT094              NO                                           
         CLI   SVCMLPIG,0          YES- TEST ONLY ONE FILM                      
         BNE   PRT100              P/B WITH 2 FILMS - LEN= NOT REQD             
*                                                                               
         LA    R1,PLCMLNAM                                                      
         MVC   0(4,R1),=C'LEN='    PRINT LEN FOR P/B WITH 1 FILM                
         LLC   R0,SVCMLSLN                                                      
         BAS   RE,EDTSLN                                                        
         MVC   4(3,R1),DUB                                                      
         B     PRT096                                                           
*                                                                               
PRT094   CLI   SVCMLPIG,1          NOT A P/B - TEST ONLY ONE FILM               
         BNE   PRT100              ONLY 1 OR 2 OF 2 - NO PRINTING               
*                                                                               
         LA    R1,PLCMLNAM                                                      
         MVC   0(4,R1),=C'LEN='    PRINT FOR FILM 1 OF 2 ONLY                   
         LLC   R0,SVCMLSLN                                                      
         BAS   RE,EDTSLN                                                        
         MVC   4(3,R1),DUB                                                      
*                                                                               
         LA    RE,SVCMLNXT         POINT TO PARTNER                             
         CLI   SVCMLPIG-SVCMLD(RE),2                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LLC   R0,SVCMLSLN-SVCMLD(RE)                                           
         BAS   RE,EDTSLN                                                        
         LA    R1,6(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   1(R1),C'/'                                                       
         MVC   2(3,R1),DUB                                                      
*                                                                               
PRT096   LA    R4,132(R4)                                                       
*                                                                               
PRT100   MVC   PLCMLNAM(15),SVCMLNAM                                            
         LA    R4,132(R4)                                                       
*                                                                               
         CLC   SVCMLNM2,SPACES                                                  
         BNH   *+14                                                             
         MVC   PLCMLNAM(20),SVCMLNM2                                            
         LA    R4,132(R4)                                                       
*                                                                               
         CLC   SVCMLNM3,SPACES                                                  
         BNH   *+14                                                             
         MVC   PLCMLNAM(20),SVCMLNM3                                            
         LA    R4,132(R4)                                                       
*                                                                               
         L     R4,FULL             BACK TO FIRST PRINT LINE                     
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A11'  GET UNTIME ADDRESS                    
         L     RF,0(R1)                                                         
         ST    RF,FULL             SAVE UNTIME ADDRESS                          
*                                                                               
         OC    SVCMLSTM,SVCMLSTM   TEST TIMES GIVEN                             
         BZ    PRT110              NO                                           
         CLI   SVT3PROF+2,C'Y'     PRINT TIMES ON INST?                         
         BNE   PRT110              NO                                           
         TM    SVCMLST,X'80'       TEST TIMES ARE DAILY                         
         BZ    PRT104                                                           
         GOTO1 (RF),(R1),SVCMLSTM,PLTIME                                        
         MVC   PLTIME+132(5),=C'DAILY'                                          
         B     PRT110                                                           
*                                                                               
PRT104   MVC   PLTIME(8),=C'RUN FROM'                                           
         GOTO1 DATCON,DMCB,(2,SVCMLFTD),(4,PLTIME+9)                            
         MVI   PLTIME+14,C'-'                                                   
         XC    DUB,DUB                                                          
         MVC   DUB(2),SVCMLSTM     PASS START TIME ONLY                         
         L     RF,FULL                                                          
         GOTO1 (RF),DMCB,DUB,PLTIME+15                                          
*                                                                               
         MVC   PLTIME+132(8),=C'   UNTIL'                                       
         GOTO1 DATCON,DMCB,(2,SVCMLLTD),(4,PLTIME+132+9)                        
         MVI   PLTIME+132+14,C'-'                                               
         MVC   DUB(2),SVCMLETM                                                  
         L     RF,FULL                                                          
         GOTO1 (RF),DMCB,DUB,PLTIME+132+15                                      
*                                                                               
PRT110   OC    SVCMLHDF,SVCMLHDF                                                
         BZ    PRT112                                                           
         MVC   PLOTHER(7),=C'HIDEF ='                                           
         MVC   PLOTHER+8(12),SVCMLHDF                                           
         LA    R4,132(R4)                                                       
*                                                                               
PRT112   OC    SVCMLCTR,SVCMLCTR                                                
         BZ    PRT114                                                           
         MVC   PLOTHER(7),=C'CTRCUT='                                           
         MVC   PLOTHER+8(12),SVCMLCTR                                           
         LA    R4,132(R4)                                                       
                                                                                
PRT114   OC    SVCMLCLT,SVCMLCLT    ANY CLT CML #                               
         BZ    PRT116               NO                                          
         CLI   SVT1PR7,C'A'        TEST TO PRINT CLT CML #                      
         BE    PRT116              NOT FOR A                                    
         CLI   SVT1PR7,C'N'        TEST TO PRINT CLT CML #                      
         BE    PRT116              NOT FOR N                                    
         MVC   PLOTHER(6),=C'CLT# ='    ELSE PRINT IT!                          
         MVC   PLOTHER+7(20),SVCMLCLT    CLT CML #                              
         LA    R4,132(R4)          NEXT PRINT LINE                              
*                                                                               
PRT116   DS    0H                                                               
         OC    SVCMLSTM,SVCMLSTM   TEST TIMES GIVEN                             
         BZ    PRT116C             NO                                           
         CLC   3(110,R4),SPACES                                                 
         BNH   PRT116C                                                          
         LA    R4,132(R4)                                                       
         CLC   3(110,R4),SPACES                                                 
         BNH   PRT116C                                                          
         LA    R4,132(R4)                                                       
                                                                                
PRT116C  OC    SVCMLTHD,SVCMLTHD   IS THERE ANY TLCSTR HIDEF                    
         BZ    PRT116D                                                          
         MVC   PLOTHER(7),=C'TC/HD#='                                           
         MVC   PLOTHER+8(L'SVCMLTHD),SVCMLTHD                                   
         LA    R4,132(R4)                                                       
*                                                                               
PRT116D  DS    0H                                                               
         OC    SVCMLTLX,SVCMLTLX   IS THERE ANY TLCSTR                          
         BZ    PRT116E                                                          
         MVC   PLOTHER(7),=C'TC#   ='                                           
         MVC   PLOTHER+8(L'SVCMLTLX),SVCMLTLX                                   
         LA    R4,132(R4)                                                       
         B     PRT116F                                                          
*                                                                               
PRT116E  DS    0H                                                               
         OC    SVCMLTEL,SVCMLTEL   IS THERE ANY TLCSTR                          
         BZ    PRT116F                                                          
         MVC   PLOTHER(7),=C'TC#   ='                                           
         MVC   PLOTHER+8(L'SVCMLTEL),SVCMLTEL                                   
         LA    R4,132(R4)                                                       
*                                                                               
PRT116F  DS    0H                                                               
         OC    SVCMLTC1,SVCMLTC1   IS THERE ANY CBC/SRC #                       
         BZ    PRT116G                                                          
         MVC   PLOTHER(7),=C'TALCYC='                                           
         MVC   PLOTHER+8(L'SVCMLTC1),SVCMLTC1                                   
         MVC   PLOTHER+8+L'SVCMLTC1+2(L'SVCMLTC2),SVCMLTC2                      
         LA    R4,132(R4)                                                       
*                                                                               
PRT116G  DS    0H                                                               
         OC    SVCMLCBC,SVCMLCBC   IS THERE ANY CBC/SRC #                       
         BZ    PRT118                                                           
         MVC   PLOTHER(9),=C'CBC/SRC#='                                         
         MVC   PLOTHER+10(L'SVCMLCBC),SVCMLCBC                                  
         LA    R4,132(R4)                                                       
*                                                                               
PRT118   BRAS  RE,CHKLINES         SET SEEMORE FLAG                             
*                                                                               
PRT120   L     R4,APRTBUFF         FIND LAST PRINT LINE                         
         SR    R5,R5                                                            
*                                                                               
PRT122   CLI   0(R4),0             MAYBE FORCING A LINE TO PRINT                
         BE    *+14                                                             
         CLC   0(132,R4),SPACES                                                 
         BNH   PRT124                                                           
         LA    R4,132(R4)                                                       
         BCT   R5,PRT122                                                        
*                                                                               
PRT124   LPR   R5,R5                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   SVCMLPIG,1          TEST FIRST OF PIGGYBACK                      
         BE    PRT136              YES - GO DO PARTNER                          
*                                                                               
         CLI   DOPCT,C'Y'          TEST ROT PRINTS AS PCTS                      
         BE    PRT130              YES                                          
         CLI   SVT1PR12,C'Y'       TEST SUPPRESS PATTERN ROT                    
         BE    PRT130                                                           
*                                                                               
         CLI   SVCMLNXT,0          TEST ANY MORE COMMERCIALS                    
         BNE   PRT132              YES - DEFER ROTATION PRINTING                
         BRAS  RE,FMTROT           ELSE FORMAT ROTATION LINE                    
         LA    R4,132(R4)                                                       
         LA    R5,1(R5)                                                         
*                                                                               
PRT130   CLI   SVCMLNXT,0          TEST ANY MORE COMMERCIALS                    
         BNE   PRT132              YES                                          
         TM    RUNFLAG,RUNFLMOR    ANY ADDITIONAL DATA TO PRINT?                
         BZ    PRT132              NO                                           
         NI    RUNFLAG,X'FF'-RUNFLMOR   RESET FLAG NOW                          
*                                                                               
         MVI   0(R4),0                                                          
         LA    R4,132(R4)                                                       
         MVC   0(SEEMOREX-SEEMORE,R4),SEEMORE                                   
         LA    R5,2(R5)            AND SEE MORE TOOK 2 LINES                    
*                                                                               
PRT132   STC   R5,ALLOWLIN                                                      
         C     R2,ASVCMLS          TEST FIRST COMMERCIAL                        
         BNE   PRT132A                                                          
         CLI   ALLOWLIN,6                                                       
         BH    PRT132A                                                          
         MVI   ALLOWLIN,6                                                       
*                                                                               
PRT132A  BRAS  RE,PRTBUFF          PRINT AND CLEAR BUFFER                       
*                                                                               
PRT134   CLI   SVCMLNXT,0          TEST ANY MORE COMMERCIALS                    
         BE    PRT150              NO                                           
*                                                                               
PRT136   LA    R2,SVCMLNXT         POINT TO NEXT COMMERCIAL                     
         CLI   SVCMLPIG,2                                                       
         BE    *+8                                                              
         L     R4,APRTBUFF         RESET BUFFER POINTER                         
*                                                                               
         MVI   0(R4),0             SET TO SKIP A LINE                           
         LA    R4,132(R4)                                                       
         B     PRT080                                                           
         EJECT                                                                  
*==========================================================                     
* DEAL WITH SPECIAL PRINT ITEMS                                                 
*==========================================================                     
                                                                                
PRT150   L     R4,APRTBUFF         START OF PRINT BUFFER                        
*                                                                               
         CLI   SVPROF11,C'P'       USING 1ST CHAR OF PROG NAME                  
         BNE   PRT152              NO                                           
         BAS   RE,FMTCOD                                                        
         B     PRT160                                                           
*                                                                               
PRT152   DS    0H                                                               
         MVI   0(R4),0             SKIP A LINE                                  
         LA    R4,132(R4)                                                       
         EJECT                                                                  
*==========================================================                     
* FORMAT PATTERN COMMENT (IF ANY) ON SAME LINES                                 
*==========================================================                     
                                                                                
PRT160   MVI   ELCODE,X'40'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   PRT166                                                           
*                                                                               
         USING PATCMTEL,R6                                                      
*                                                                               
         CLC   =C'BOX=',3(R6)                                                   
         BE    PRT164                                                           
         CLI   SVPROF3,C'Y'        TEST AUTO BOX                                
         BE    PRT164                                                           
                                                                                
*=========================================================                      
* NO BOXES                                                                      
*=========================================================                      
                                                                                
PRT162   LLC   RE,1(R6)                                                         
         AHI   RE,-4                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),3(R6) *EXECUTED*                                         
         LA    R4,132(R4)                                                       
         BRAS  RE,NEXTEL                                                        
         BE    PRT162                                                           
         B     PRT166                                                           
                                                                                
*=========================================================                      
* BOXES                                                                         
*=========================================================                      
                                                                                
PRT164   GOTOR BOXER,DMCB,(132,(R4))                                            
*                                                                               
PRT166   BRAS  RE,PRTBUFF                                                       
*                                                                               
PRT170   TM    PTNFLAG,PTNFIPR     TEST INVERTED PRODUCTS                       
         BZ    PRT172                                                           
*                                                                               
         IC    R1,PTNBPRD2                                                      
         MVC   PTNBPRD2,PTNBPRD                                                 
         STC   R1,PTNBPRD                                                       
         IC    RE,PTNSLN                                                        
         MVC   PTNSLN,PTNSLN2                                                   
         STC   RE,PTNSLN2                                                       
                                                                                
PRT172   LA    R3,PTNEXT           NEXT PATTERN ENTRY                           
         LH    R0,SVPTNSIZ                                                      
         AHI   R0,-L'PTNENT                                                     
         BNP   PRT174              NO MORE PATTERNS                             
         STH   R0,SVPTNSIZ                                                      
         ST    R3,ACURRPTN                                                      
         B     PRT030             YES - GO PROCESS                              
                                                                                
*=================================================================              
* IF DOING POL, MAY NEED TO KEEP GOING                                          
*=================================================================              
                                                                                
PRT174   CLI   BPRD,X'FF'                                                       
         BNE   PRT178                                                           
         MVC   CURTABNO,SVCRTBNO                                                
         BRAS  RE,FNDCUR           FIND REAL CURTABNO FOR PTN PTR               
         BE    PRT178               NO, ALL DONE                                
*                                                                               
         L     R3,APATTAB                                                       
         USING PTNTABD,R3                                                       
         SR    RE,RE                                                            
         ICM   RE,3,CURTABNO                                                    
         B     *+8                                                              
*                                                                               
PRT176   AH    R3,SVPTNSIZ                                                      
         LH    R1,0(R3)                                                         
         AHI   R1,-HDRLEN                                                       
         STH   R1,SVPTNSIZ                                                      
         AHI   R3,HDRLEN                                                        
         BCT   RE,PRT176                                                        
*                                                                               
         ST    R3,ACURRPTN                                                      
         B     PRT030                                                           
                                                                                
PRT178   BRAS  RE,PRTADDL          PRINT ADDITIONAL CMML DATA                   
*                                                                               
         BRAS  RE,RDSTEXT          GO PRINT SPECIAL TEXT                        
*                                                                               
         MVI   CONTINUE,C'N'       NOW FORCE LAST FOOTLINES                     
         MVI   FORCEFUT,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)     GOTO SPOOL RTN                               
*                                                                               
         CLI   SVFAXREQ,C'2'      SEE IF DOING FAX REPORT                       
         BNE   PRT180                                                           
         TM    WHEN,X'20'          IS THIS SOON                                 
         BZ    PRT184                                                           
         B     PRT182                                                           
*                                                                               
PRT180   CLI   SVFAXREQ,C'Y'      SEE IF DOING FAX REPORT                       
         BNE   PRT184                                                           
PRT182   MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,2                                                           
         MVC   P1(26),=C'*** END OF DDS MESSAGE ***'                            
         GOTO1 SPOOL,DMCB,(R8)       GOTO SPOOL RTN                             
*                                                                               
PRT184   ICM   R1,3,CURTABNO                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,CURTABNO                                                    
*                                                                               
         L     R5,ASTATAB                                                       
         USING STATABD,R5                                                       
*                                                                               
PRT186   CLC   CURTABNO,STAPTR                                                  
         BE    PRT004                                                           
         LA    R5,STANEXT                                                       
         OC    STAPTR,STAPTR                                                    
         BNZ   PRT186                                                           
*                                                                               
PRT188   CLI   SVFAXREQ,C'2'       DON'T UPDATE ON FIRST PASS                   
         BE    *+8                                                              
         BRAS  RE,UPDRCP           UPDATE INST RECAP RECS                       
*                                                                               
         CLI   SVFAXREQ,C'2'       TEST NEED SECOND REPORT                      
         BNE   PRT200                                                           
         TM    WHEN,X'20'          IS THIS A SOON RUN                           
         BO    PRT240               YES                                         
*                                                                               
         TM    SVOPT,OPTCOPY       THIS AN AGENCY COPY RUN                      
         BO    PRT200               YES                                         
*                                                                               
         MVI   SPMODE,X'FF'        CLOSE PRINT QUE                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   PQSW,1                SO I WILL REOPEN THE PRINTQ                
*                                                                               
         MVI   SVFAXREQ,C'Y'         SET FOR FAX REPORT                         
         OI    SVOPT,OPTTEST         SO I WON'T REMARK FILE                     
         MVC   CURTABNO,=H'1'                                                   
*                                                                               
         L     R0,APATTAB          CLEAR THE PATTERN TABLE                      
         L     R1,APATTABX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,ASTATAB                                                       
         L     R1,ASTATABX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R5,ASTATAB                                                       
         XC    STAPTR,STAPTR                                                    
         B     PRT002                                                           
*                                                                               
PRT200   TM    WHEN,X'20'          IS THIS A SOON RUN                           
         BO    PRT240               YES                                         
*                                                                               
         CLI   SVFAXREQ,C'Y'      SEE IF DOING FAX REPORT                       
         BNE   PRTX                NO                                           
         CLI   OFFLINE,C'Y'       IS THIS OFFLINE                               
         BNE   PRTX                NO                                           
*                                                                               
         L     RE,TWAMASTC                                                      
         L     RF,MCVREMOT-MASTD(RE)                                            
         USING REMOTED,RF                                                       
         XC    REMOTKEY,REMOTKEY                                                
*                                                                               
* SEE IF ORIGINALLY PRINTING DDS SHOP                                           
*                                                                               
         OC    MCREMOTE-MASTD(80,RE),MCREMOTE-MASTD(RE)                         
         BZ    PRTX                YES                                          
*                                                                               
         LA    R1,MCREMOTE-MASTD(RE)                                            
         MVC   REMOTSYS,MCS2OSYS-MASTD(RE)                                      
         MVC   REMOTPRG,REMOTPRG-REMOTED(R1)                                    
         MVC   REMOTFRM,REMOTFRM-REMOTED(R1)                                    
         MVC   REMOTJID,REMOTJID-REMOTED(R1)                                    
         MVC   REMOTDST,REMOTDST-REMOTED(R1)                                    
         MVC   REMOTCLS,REMOTCLS-REMOTED(R1)                                    
         MVC   REMOTCPY,REMOTCPY-REMOTED(R1)                                    
*                                                                               
         B     PRTX                                                             
         DROP  RF                                                               
*                                                                               
* GO TO CONTROLLER TO USE LOCKET TO UNLOCK INST RECAP RECS FOR THIS             
* MEDIA AND CLIENT                                                              
                                                                                
PRT240   TM    SVOPT,OPTTEST      THIS TEST MODE                                
         BO    PRTX                YES                                          
*                                                                               
         MVI   DUB,X'E4'                     U=UNLOCK                           
         MVC   DUB+1(7),=X'030A220A240A25'   03=3 ENTRIES                       
*                                            0A22,0A24,0A25 RECS                
         GOTO1 VALILOC,0                                                        
*                                                                               
         CLI   SVFAXREQ,C'2'                                                    
         BNE   PRTX                                                             
*                                                                               
         BRAS  RE,REQ                                                           
*                                                                               
PRTX     B     EXIT                                                             
         DROP  R5                                                               
         LTORG                                                                  
INSBSCEL EQU   15                                                               
INSSUBEL EQU   7                                                                
         EJECT                                                                  
FMTROT   NTR1                                                                   
         MVC   0(ROTMSGX-ROTMSG,R4),ROTMSG                                      
         LA    R5,L'ROTMSG+1(R4)                                                
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'32'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LLC   R0,1(R6)                                                         
         AHI   R0,-2               NUMBER OF LETTERS                            
         LA    RF,2                SET FOR ONE SPACE BETWEEN LETTERS            
         CHI   R0,37               IF MORE THAN 37 ENTRIES                      
         BNH   *+8                                                              
         LA    RF,1                THEN NO SPACES                               
         LA    R1,ROTMSGX-ROTMSG+1(R4) FIRST OUTPUT POSN                        
         LA    R6,2(R6)                                                         
*                                                                               
FMTROT2  MVC   0(1,R1),0(R6)                                                    
         AR    R1,RF                                                            
         LA    R6,1(R6)                                                         
         BCT   R0,FMTROT2                                                       
         J     EXIT                                                             
*                                                                               
ROTMSG   DC    C'*****'                                                         
         DC    C' ROTATE COMMERCIALS AS FOLLOWS:'                               
ROTMSGX  EQU   *                                                                
*                                                                               
FMTSLN   NTR1                                                                   
         MVC   0(4,R4),=C'LEN='                                                 
         LLC   R0,PTNSLN                                                        
         BAS   RE,EDTSLN                                                        
         MVC   4(3,R4),DUB                                                      
         LLC   R0,PTNSLN2                                                       
         LTR   R0,R0                                                            
         JZ    EXIT                                                             
         BAS   RE,EDTSLN                                                        
         LA    RE,6(R4)            POINT TO LAST CHAR OF LEN=999                
         CLI   0(RE),C' '                                                       
         BH    *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'/'                                                       
         MVC   2(3,RE),DUB                                                      
         J     EXIT                                                             
*                                                                               
EDTSLN   CVD   R0,DUB              RETURN PRINTABLE SLN IN DUB(3)               
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(2),DUB+6(2)                                                  
         MVI   DUB+2,C' '                                                       
         CHI   R0,100                                                           
         BLR   RE                                                               
         UNPK  DUB(3),DUB+6(2)                                                  
         BR    RE                                                               
*                                                                               
         USING PLINED,R4                                                        
FMTCOD   NTR1                                                                   
         LA    R0,FMTCODS                                                       
         L     R1,=A(FMTCODT)                                                   
         A     R1,SPTR34RR                                                      
*                                                                               
FMTCOD2  CLC   PTNCOPY,0(R1)                                                    
         BE    FMTCOD4                                                          
         LA    R1,L'FMTCODT(R1)   NEXT ENTRY                                    
         BCT   R0,FMTCOD2                                                       
*                                                                               
         MVC   PLCODE(1),PTNCOPY                                                
         MVC   PLCODE+1(L'FMTCODT-1),0(R1)                                      
         J     EXIT                                                             
*                                                                               
FMTCOD4  MVC   PLCODE(L'FMTCODT),0(R1)                                          
         J     EXIT                                                             
         DROP  R4                                                               
                                                                                
FMTCODT  DS   0CL42                                                             
         DC    CL42'A/=ACTION ADVENTURE                       '                 
         DC    CL42'C/=CARTOONS && ANIMATION                  '                 
         DC    CL42'E/=TEEN TALK AND ENTERTAINMENT SHOWS      '                 
         DC    CL42'K/=SITCOMS FOR KIDS                       '                 
         DC    CL42'M/=TEEN MINORITY AUDIENCES                '                 
         DC    CL42'N/=NEWS AND ENTERTAINMENT                 '                 
         DC    CL42'O/=OTHER                                  '                 
         DC    CL42'P/=ADULT PROGRAMS                         '                 
         DC    CL42'R/=TEEN TALK SHOWS                        '                 
         DC    CL42'S/=SPORTS                                 '                 
         DC    CL42'T/=PROGRAMS FOR GENERAL TEENS             '                 
         DC    CL42'W/=DRAMAS FOR TEENS AND OLDER TEEN SITCOMS'                 
FMTCODS  EQU   (*-FMTCODT)/L'FMTCODT                                            
         DC    CL42' /=UNKNOWN  '                                               
                                                                                
*==============================================================                 
* FIND CURRENT ENTRY IN TABLE FOR POL PROCESSING                                
*==============================================================                 
                                                                                
FNDCUR   L     R1,ACURPRPT        GET LAST USED POS IN SVCLIST                  
*                                                                               
FCR10    LLC   R0,3(R1)           GET BINARY PRD                                
         L     RF,ACURPRDT        CURR PRDTAB ENTRY                             
         AR    RF,R0                                                            
         CLI   0(RF),0            WAS THIS PRODUCT ACTIVE                       
         JNE   FCR20              YES                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),0            END OF SVCLIST                                
         BER   RE                 YES                                           
         J     FCR10                                                            
*                                                                               
FCR20    MVC   CURTABNO+1(1),0(RF)                                              
         LA    R1,4(R1)                                                         
         ST    R1,ACURPRPT                                                      
         ST    RF,FULL             RETURN POINTER                               
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* SEE IF ANY ADDITIONAL LINES ARE REQUIRED FOR THIS COMMERCIAL                  
*====================================================================           
                                                                                
CHKLINES NTR1                                                                   
*                                                                               
         TM    SVCMLST,X'40'       TEST COMMERCIAL TEXT REQUIRED                
         BO    CHKLIN10                                                         
*                                                                               
         OC    SVCMLDDT,SVCMLDDT   TEST DESTROY DATE                            
         BNZ   CHKLIN10                                                         
*                                                                               
         CLI   SVT2PR02,C'Y'       SUPPRESS COMML TYPE                          
         BE    *+12                                                             
         CLI   SVCMLTYP,C' '       TEST COMMERCIAL TYPE TO PRINT                
         BH    CHKLIN10                                                         
*                                                                               
         B     CHKLINX             IDIOT!                                       
*                                                                               
CHKLIN10 OI    RUNFLAG,RUNFLMOR                                                 
*                                                                               
CHKLINX  J     EXIT                                                             
*                                                                               
SEEMORE  DC    C'+++++'                                                         
         DC    C' SEE ADDITIONAL COMMERCIAL INFORMATION BELOW '                 
         DC    C'+++++'                                                         
SEEMOREX EQU   *                                                                
         EJECT                                                                  
* FORMAT STATION FOR PRINTING (EG WABC-FM) *                                    
*                                                                               
FSTA     NTR1                                                                   
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),QSTA                                                  
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
         LA    RF,STAPRNT+3                                                     
         CLI   0(RF),C' '                                                       
         BNE   *+6                                                              
         BCTR  RF,0                                                             
         MVI   1(RF),C'-'                                                       
         MVC   2(1,RF),QSTA+4      MOVE SUB-MEDIA                               
         MVI   3(RF),C'V'          ASSUME TV                                    
         CLI   QMED,C'T'                                                        
         BE    FSTA10                                                           
         MVI   3(RF),C'M'          ASSUME RADIO                                 
         CLI   QMED,C'R'                                                        
         BE    FSTA10                                                           
         MVI   3(RF),C' '                                                       
*                                                                               
FSTA10   CLI   SVT1PR11,C'Y'       USE STATION CMML TYPE                        
         BNE   EXIT                                                             
*                                                                               
         CLI   SVT2PR02,C'Y'       SUPPRESS COMML TYPE                          
         BE    EXIT                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A28'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(5),QSTA                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   EXIT                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING STADTAEL,R6                                                      
         CLI   STADTALN,119                                                     
         BE    EXIT                                                             
         MVC   P+76(4),STACMLT                                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*==============================================================                 
* SUBROUTINE TO FIND EBCDIC PRODUCT IN CLIENT LIST                              
*==============================================================                 
                                                                                
GETPRD   L     RF,ASVCLIST                                                      
*                                                                               
GETPRD2  CLC   3(1,RF),0(R1)                                                    
         BER   RE                                                               
         LA    RF,4(RF)                                                         
         CLI   0(RF),0                                                          
         JNE   GETPRD2                                                          
         BRAS  RF,*+8              POINT RF TO ****                             
         DC    C'****'                                                          
         BR    RE                                                               
                                                                                
*==============================================================                 
* SUBROUTINE TO GET PRODUCT NAME                                                
*==============================================================                 
                                                                                
GPRD     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LLC   R0,0(R1)            SAVE BINARY PRODUCT CODE                     
                                                                                
*==============================================================                 
* SEE IF PRODUCT ALREADY IN SVPRD BUFFER                                        
*==============================================================                 
                                                                                
         L     R4,ASVPRDS                                                       
         L     R5,ASVPRDSX                                                      
         USING SVPRDD,R4                                                        
*                                                                               
GPRD2    CLI   0(R4),0                                                          
         BE    GPRD4                                                            
         CLM   R0,1,SVPRDCD                                                     
         BE    GPRD10                                                           
         LA    R4,L'SVPRDDTA(R4)                                                
         CR    R4,R5                                                            
         BL    GPRD2                                                            
         DC    H'0'                                                             
*                                                                               
GPRD4    STC   R0,SVPRDCD                                                       
*                                                                               
         BRAS  RE,GETPRD           GET EBCDIC PRD                               
         MVC   SVPRDEBC,0(RF)                                                   
*                                                                               
         L     R6,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD       A-M/CLT                                    
         MVC   KEY+4(3),0(RF)                                                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT                             
         GOTO1 GETREC                                                           
*                                                                               
         USING PRDHDRD,R6                                                       
         MVC   SVPRDNM,PNAME                                                    
         OC    SVPRDNM,SPACES                                                   
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
GPRD10   LR    R1,R4               POINT R1 TO SVPRD ENTRY                      
         XIT1  REGS=(R1)                                                        
         LTORG                                                                  
         DROP  R4,R6                                                            
         EJECT                                                                  
NOTXTERR XC    CONHEAD,CONHEAD                                                  
         BRAS  RE,*+28             POINT RE TO DC                               
         DC    CL24'ERROR * NO TEXT FOUND '                                     
         MVC   CONHEAD(24),0(RE)                                                
         GOTO1 ERREX2                                                           
         EJECT                                                                  
MISTAER  XC    CONHEAD,CONHEAD                                                  
         BRAS  RE,*+36                                                          
         DC    CL32'* ERROR * NO STATION MASTER FOR'                            
         MVC   CONHEAD(32),0(RE)                                                
         MVC   CONHEAD+32(4),QSTA                                               
         MVI   CONHEAD+36,C'-'                                                  
         MVC   CONHEAD+37(1),QSTA+4                                             
         MVI   CONHEAD+39,C'*'                                                  
         LA    R2,TRAMEDH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
TCPROFER L     R1,=A(TCPROFMS)                                                  
         B     ERREXIT                                                          
*                                                                               
ROTSIZER L     R1,=A(ROTSIZMS)                                                  
         LA    R2,TRAMEDH                                                       
         B     ERREXIT                                                          
*                                                                               
POLPTRER L     R1,=A(POLPTRMS)                                                  
*                                                                               
ERREXIT  A     R1,SPTR34RR                                                      
         MVC   CONHEAD,0(R1)                                                    
         GOTO1 ERREX2                                                           
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         LTORG                                                                  
NOCMLSCH DC    CL30'** NO COMMERCIALS SCHEDULED **'                             
ROTSIZMS DC    CL60'* ERROR * TOO LARGE FOR NOW-RUN SOON OR OFFLINE *'          
TCPROFMS DC    CL60'* ERROR * CAN''T DO BOTH MGROUP AND MARKET, ONLY 1 C        
               *'                                                               
POLPTRMS DC    CL60'* ERROR * CAN''T DO PROD POL AND LIMIT PARTNER PRODC        
               UCT *'                                                           
         EJECT                                                                  
* VALIDATE KEY FIELDS ROUTINE                                                   
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         LA    R0,MYSTOR                                                        
         LHI   R1,MYSTORX-MYSTOR                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*MN                                                                             
*        MVI   TWAFIRST,2          SEND RUN LAST MODE/GLOBAL FILES              
*MN                                                                             
*                                                                               
         LA    R2,TRAMEDH          FIELD PTR FOR MEDIA                          
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,TRACLTH                                                       
         GOTO1 VALICLT                                                          
*                                                                               
         BAS   RE,RDPROF           READ PROFILES                                
*                                                                               
         LA    R2,TRAPRDH          PRODUCT CODE                                 
         GOTO1 VALIPRD                                                          
*                                                                               
         CLC   =C'POL',WORK        PRODUCT POL ENTERED                          
         BNE   VK04                                                             
*                                                                               
         TM    WHEN,X'38'          ONLY ALLOWED OFFLINE                         
         B     *+8           <<<<< NOP - ALLOW ONLY POL REQUEST                 
         BZ    INVPOLER                                                         
*                                                                               
         CLI   SVT1PR16,C'Y'       IS SPECIFIC PRODUCT REQUIRED                 
         BE    POLINVER             YES                                         
*                                                                               
VK04     MVC   QPRD,WORK           SAVE EBCIC PRODUCT                           
         MVC   BPRD,WORK+3         SAVE BINARY PRODUCT                          
         MVC   PRDNM,WORK+5        SAVE PRODUCT NAME                            
*                                                                               
         LA    R2,TRAPTRH          PARTNER CODE                                 
         XC    QPRD2,QPRD2                                                      
         MVI   BPRD2,0                                                          
         XC    PRD2NM,PRD2NM                                                    
         CLI   5(R2),0                                                          
         BE    VK06                                                             
         CLC   =C'NONE',8(R2)                                                   
         BNE   *+12                                                             
         MVI   BPRD2,X'FF'                                                      
         B     VK06                                                             
*                                                                               
         CLI   BPRD,X'FF'          WAS POL SELECTED                             
         BE    POLPTRER                                                         
*                                                                               
         GOTO1 VALIPRD                                                          
*                                                                               
         CLC   =C'POL',WORK        PRODUCT POL INVALID                          
         BE    INVPRDER                                                         
         MVC   QPRD2,WORK           SAVE EBCIC PRODUCT                          
         MVC   BPRD2,WORK+3         SAVE BINARY PRODUCT                         
         MVC   PRD2NM,WORK+5        SAVE PRODUCT NAME                           
*                                                                               
VK06     LA    R2,TRAESTH          EDIT ESTIMATE                                
         BRAS  RE,VEST                                                          
*                                                                               
         LA    R2,TRAPERH          EDIT PERIOD                                  
         BRAS  RE,VPER                                                          
*                                                                               
         LA    R2,TRAFAXH                                                       
         CLC   TWAORIG,=X'335D'                                                 
         BNE   VK07                                                             
         CLI   TRLSTPGM,X'34'                                                   
         BNE   VK08                                                             
*                                                                               
VK07     CLI   5(R2),0             ANY ENTRY?                                   
         BNE   VF10                                                             
*                                                                               
VK08     MVC   8(1,R2),SVTWPR04    USE PROFILE DEFAULT                          
         OI    6(R2),X'80'         FORCE XMIT                                   
         MVI   5(R2),1                                                          
         MVI   TRLSTPGM,X'34'                                                   
         B     VFX                                                              
*                                                                               
VF10     CLI   8(R2),C'Y'                                                       
         BE    VFX                                                              
         CLI   8(R2),C'2'                                                       
         BE    VFX                                                              
         CLI   8(R2),C'N'                                                       
         BE    VFX                                                              
         MVC   GERROR,=Y(BADFAX)                                                
         GOTO1 VTRAERR                                                          
*                                                                               
VFX      OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
         MVC   SVFAXREQ,8(R2)                                                   
*                                                                               
         LA    R2,TRAOPTH          OPTION VALIDATION                            
*                                                                               
         BRAS  RE,VOPT             VALIDATE OPTIONS                             
         LA    R2,TRACONTH         CONTACT IS REQUIRED BUT NOT EDITED           
         GOTO1 ANY                                                              
         XC    QUESTOR,QUESTOR                                                  
         XC    CONTEL,CONTEL                                                    
         XC    CONFAX,CONFAX                                                    
         MVC   QUESTOR(L'TRACONT),WORK                                          
*                                                                               
         CLC   =C'CON=',WORK       THIS AGY CONTACT KEY                         
         BNE   VK20                                                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CNTKEY,R4                                                        
         MVC   CNTKID,=X'0A36'                                                  
         MVC   CNTKAM(3),BAGYMD                                                 
*                                                                               
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),4             TEST DATA AFTER CON=                         
         BNH   TRAPERR                                                          
*                                                                               
         CLI   5(R2),12            NO MORE THAN 8 CHARS ALLOWED                 
         BNH   *+12                                                             
         LA    R1,7                                                             
         B     VFX20                                                            
*                                                                               
         LLC   R1,5(R2)            INPUT LENGTH                                 
         AHI   R1,-5               MINUS 4 (CON=) 1 (FOR EX)                    
*                                                                               
VFX20    EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CNTKNAME(0),12(R2)                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VK10                                                             
         MVC   KEY,KEYSAVE                                                      
         XC    CNTKCLT,CNTKCLT                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VK10                                                             
         MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
VK10     L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CNTDTAEL,R6                                                      
         MVC   QUESTOR,CNTNAME                                                  
         MVC   CONTEL,CNTTEL                                                    
*                                                                               
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   VK12                                                             
         USING CNTFAXEL,R6                                                      
         MVC   CONFAX,CNTFTEL                                                   
*                                                                               
VK12     L     R6,AIO1                                                          
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VK20                                                             
         USING CNTEMLEL,R6                                                      
         MVC   CONEMAIL,CNTEMLAD                                                
         DROP  R4,R6                                                            
*                                                                               
VK20     DS    0H                                                               
         OC    QUESTOR,SPACES      NEEDED IF FAXING                             
*                                                                               
         LA    R2,TRAMKTSH         OPTION VALIDATION                            
*                                                                               
         BRAS  RE,VMKT                                                          
*                                                                               
         BRAS  RE,RDHOUSE            READ PRODUCTION HOUSE                      
*                                                                               
         MVI   ERROR,0                                                          
*                                                                               
         TM    SVOPT,OPTCOPY       THIS AN AGENCY COPY RUN                      
         BO    VK30                                                             
*                                                                               
         TM    WHEN,X'20'          THIS A SOON REQUEST                          
         BZ    VK24                                                             
         CLI   SVFAXREQ,C'2'    SEE IF ONLY FAXING                              
         BE    VK26                                                             
*                                                                               
VK24     CLI   SVFAXREQ,C'Y'    SEE IF ONLY FAXING                              
         BNE   VK30                                                             
*                                                                               
VK26     MVI   PQSW,1           TO SUPPRESS AUTO PRINTQ OPEN                    
*                                                                               
         TM    WHEN,X'20'          THIS A SOON REQUEST                          
         BZ    VK30                                                             
         MVC   REMUSER,=C'COV'                                                  
         MVI   SPOOLKEY+PLCLASS-PQPLD,C'G'                                      
*AR                                                                             
         OC    SPOOLKEY+QLTYP1-PQPLD(1),SVFAXARC                                
*AR                                                                             
         ICM   RE,15,TWAMASTC                                                   
         BZ    VK30                                                             
         MVI   MCREQREP-MASTD(RE),C'N'                                          
*                                                                               
VK30     CLI   OFFLINE,C'Y'        IS THIS OFFLINE                              
         BE    VKX                  YES, NO TEST OR LOCK                        
*                                                                               
* GO TO CONTROLLER TO USE LOCKET TO SEE IF INST RECAP RECS LOCKED *             
* IF THEY ARE LOCKED, CONTROLLER WILL GIVE ERROR MESSAGE          *             
*                                                                               
         MVC   DUB,=X'E3010A2400000000'  T=TEST, 01=1 ENTRY, 0A24 RECS          
         GOTO1 VALILOC,0                                                        
*                                                                               
         TM    WHEN,X'20'          IS THIS A SOON RUN                           
         BZ    VKX                  NO                                          
*                                                                               
* GO TO CONTROLLER TO USE LOCKET TO LOCK INST RECAP RECS FOR THIS MEDIA         
* AND CLIENT, IF THEY ARE LOCKED, CONTROLLER WILL GIVE ERROR MESSAGE            
*                                                                               
         MVC   DUB,=X'D3010A2400000000'  L=LOCK, 01=1 ENTRY, 0A24 RECS          
         GOTO1 VALILOC,0                                                        
*                                                                               
         MVI   TWAWHEN,5           FRED'S NEW UPDATIVE SOONS                    
*                                                                               
VKX      XIT1                                                                   
         EJECT                                                                  
*===============================================================                
* READ TRAFFIC PROFILES                                                         
*===============================================================                
                                                                                
RDPROF   NTR1                                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         MVI   WORK+3,C'1'         READ T1 PROFILE                              
         GOTO1 (RF),(R1),WORK,SVT1PROF                                          
*                                                                               
         MVI   WORK+3,C'2'         READ T2 PROFILE                              
         GOTO1 (RF),(R1),WORK,ELEM                                              
         MVC   SVT2PR02,ELEM+1                                                  
         MVC   SVT2PR04,ELEM+3                                                  
*                                                                               
         MVI   WORK+3,C'3'         READ T3 PROFILE                              
         GOTO1 (RF),(R1),WORK,SVT3PROF,DATAMGR                                  
*                                                                               
         MVI   WORK+3,C'C'         READ TC PROFILE                              
         GOTO1 (RF),(R1),WORK,SVTCPROF                                          
*                                                                               
         CLI   SVTCPR02,C'0'                                                    
         BNE   *+8                                                              
         MVI   SVTCPR02,0                                                       
         CLI   SVTCPR03,C'0'                                                    
         BNE   *+8                                                              
         MVI   SVTCPR03,0                                                       
*                                                                               
         MVI   WORK+3,C'W'         READ TW PROFILE                              
         GOTO1 GETPROF,DMCB,WORK,SVTWPROF,DATAMGR                               
*                                                                               
         CLI   SVTWPR04,0          2=FAX+PRINT,Y=FAX,N=PRINT                    
         BNE   *+8                                                              
         MVI   SVTWPR04,C'N'       SET DEFAULT VALUE                            
*                                                                               
         CLI   QMGRID,C' '         TEST PROCESSING BY MKTGRP                    
         BNH   RDPRX               NO                                           
         CLI   SVTCPR01,C'Y'                                                    
         BE    TCPROFER                                                         
*                                                                               
RDPRX    XIT1                                                                   
*                                                                               
POLINVER MVC   CONHEAD,POLINVMS                                                 
         B     VKEREX2                                                          
*                                                                               
INVPOLER MVC   CONHEAD,INVPOLMS                                                 
*                                                                               
VKEREX2  GOTO1 ERREX2                                                           
*                                                                               
INVPRDER MVI   ERROR,INVPROD                                                    
         B     TRAPERR                                                          
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
NUMERR   MVI   ERROR,NOTNUM                                                     
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
POLINVMS DC    CL60'* ERROR * POL INVALID, ENTER SPECIFIC PRODUCT, T1 PC        
               ROF 16 *'                                                        
INVPOLMS DC    CL60'* ERROR * CAN''T DO PROD POL NOW, ONLY DDS, SOON, OC        
               R OV *'                                                          
*                                                                               
HEADING  SSPEC H1,3,AGYNAME                                                     
         SSPEC H2,3,AGYADD                                                      
*                                                                               
*NOTE*   SSPEC H1,39,C'SPOT TELEVISION COMMERCIAL INSTRUCTIONS'                 
*NOTE*   SSPEC H2,39,C'---------------------------------------'                 
         SSPEC H3,46,PERIOD                                                     
*                                                                               
         SSPEC H1,85,REPORT                                                     
         SSPEC H2,85,RUN                                                        
         SSPEC H5,85,PAGE                                                       
         SSPEC H5,93,REQUESTOR                                                  
*                                                                               
         DC    X'00'               END MARKER FOR SSPECS                        
         DROP  R7                                                               
         EJECT                                                                  
*==============================================                                 
* VALIDATE ESTIMATE                                                             
*==============================================                                 
                                                                                
VEST     NTR1  BASE=*,LABEL=*                                                   
         MVI   BEST,0                                                           
         MVI   QBEST,0                                                          
*                                                                               
         CLI   5(R2),0             ANY INPUT                                    
         BNE   VEST20                                                           
VEST10   CLI   SVPROF11,C'E'       IF COPY CODE = EST, MUST ENTER EST           
         BE    VESTER10                                                         
         B     VESTX                                                            
VEST20   GOTO1 ANY                                                              
         CLC   =C'NO',WORK                                                      
         BE    VEST10                                                           
*                                                                               
         CLI   SVPROF11,C'E'       IF COPY CODE NE EST, NO EST ALLOWED          
         BNE   VESTER20                                                         
         GOTO1 VALINUM                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),ACTUAL                                                  
         MVC   BEST,ACTUAL                                                      
         MVC   QBEST,ACTUAL                                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERROR,NOESTS                                                     
         CLC   KEY(13),KEYSAVE     SEE IF ESTIMATE FOUND                        
         BNE   VESTER                                                           
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT MEDIA                       
         GOTO1 GETREC                                                           
*                                                                               
         USING ESTHDRD,R6                                                       
*                                                                               
         CLI   ECOPY,0             COPY CODE ILLEGAL                            
         BNE   VESTER30                                                         
         GOTO1 DATCON,DMCB,(0,ESTART),(3,SVESTSTR)                              
         GOTO1 (RF),(R1),(0,EEND),(3,SVESTEND)                                  
         MVC   QESTDESC,EDESC                                                   
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
VESTX    XIT1                                                                   
VESTER   GOTO1 ERREX                                                            
VESTER10 MVC   CONHEAD,=CL60'* ERROR * COPY CODE = EST REQUIRES EST *'          
         B     VESTERX                                                          
*                                                                               
VESTER20 MVC   CONHEAD,=CL60'* ERROR * EST NOT ALLOWED *'                       
         B     VESTERX                                                          
*                                                                               
VESTER30 MVC   CONHEAD,=CL60'* ERROR * EST IS COPY CODED *'                     
VESTERX  GOTO1 ERREX2                                                           
         DROP  R6,RB,RC                                                         
         EJECT                                                                  
* VALIDATE PERIOD                                                               
*                                                                               
VPER     DS    0H                                                               
         NMOD1 0,**VPER**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSERRA                                                         
         GOTO1 ANY                                                              
*                                                                               
         CLI   8(R2),C'?'            IF QUESTION MK, TELL MEL FLT DATES         
         BNE   VPER30                                                           
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER26                                                           
*                                                                               
         CLI   5(R2),1                 SEE IF DATE ENTERED TOO                  
         BE    VPER10                  NO                                       
*                                                                               
         GOTO1 DATVAL,DMCB,9(R2),SVQSTART                                       
         L     R4,DMCB                 GET LENGTH OF FIELD                      
         LTR   R4,R4                                                            
         BZ    BDATERR                                                          
         GOTO1 DATCON,(R1),(0,SVQSTART),(3,SVFLTSTR)                            
         B     VPER12                                                           
*                                                                               
VPER10   GOTO1 DATCON,DMCB,(5,0),(3,SVFLTSTR)                                   
*                                                                               
VPER12   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(4),BAGYMD     BCLT, BPRD (TRY PRD SPECIFIC REC)            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER14                                                           
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(6),KEYSAVE                                                   
         BNE   NOFLTER                                                          
VPER14   CLC   SVFLTSTR,KEY+6      FIRST TLCST DATE TO RECORD END DATE          
         BNH   VPER16                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER14                                                           
         MVC   KEY,KEYSAVE         GET LAST DATE BEFORE TODAY                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
VPER16   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(5),=C'*END='                                             
         GOTO1 DATCON,DMCB,(3,KEY+6),(5,CONHEAD+5)                              
         LA    R4,4                                                             
         LA    R5,CONHEAD+14                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
VPER20   BRAS  RE,NEXTEL                                                        
         BNE   VPER24                                                           
         USING FLTDTAEL,R6                                                      
         CLC   SVFLTSTR,FLTEND                                                  
         BNH   VPER22                                                           
         CLC   SVFLTSTR,FLTSTART                                                
         BH    VPER20                                                           
VPER22   GOTO1 DATCON,DMCB,(3,FLTSTART),(4,0(R5))                               
         MVI   5(R5),C'-'                                                       
         GOTO1 (RF),(R1),(3,FLTEND),(4,6(R5))                                   
         LA    R5,11(R5)                                                        
         BCT   R4,VPER20                                                        
VPER24   MVI   0(R5),C'*'                                                       
         GOTO1 ERREX2                                                           
*                                                                               
VPER26   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(8),=CL8'EST FROM'                                        
         GOTO1 DATCON,DMCB,(3,SVESTSTR),(5,CONHEAD+9)                           
         MVC   CONHEAD+18(2),=C'TO'                                             
         GOTO1 (RF),(R1),(3,SVESTEND),(5,CONHEAD+21)                            
         GOTO1 ERREX2                                                           
*                                                                               
VPER30   CLI   5(R2),8             TEST ONE DATE ENTERED                        
         BH    VPER34                                                           
*                                                                               
* ACCEPT ONE DATE AS FLIGHT START DATE *                                        
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   VPER32                                                           
*                                                                               
         CLC   =C'ES',8(R2)        USE EST DATES                                
         BNE   VPER32                                                           
         MVC   SVFLTDTS,SVESTSTR                                                
         GOTO1 DATCON,DMCB,(3,SVFLTSTR),(5,TRAPER)                              
         MVI   TRAPER+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,SVFLTEND),(5,TRAPER+9)                              
         GOTO1 (RF),(R1),(3,SVFLTSTR),SVQSTART                                  
         GOTO1 (RF),(R1),(3,SVFLTEND),SVQEND                                    
         OI    TRAPERH+6,X'80'                                                  
         MVI   TRAPERH+5,17        RESET LENGTH                                 
         B     VPER46                                                           
*                                                                               
VPER32   GOTO1 VALIDATE,DMCB,SVQSTART                                           
         GOTO1 DATCON,(R1),SVQSTART,(3,SVFLTSTR)                                
         XC    SVFLTEND,SVFLTEND                                                
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER40                                                           
*                                                                               
         BAS   RE,CHKFLT             FILLS IN SVQEND/SVFLTEND                   
         B     VPERX                                                            
         EJECT                                                                  
* VALIDATE TWO DATES *                                                          
*                                                                               
VPER34   DS    0H                                                               
         GOTO1 VALPER,DMCB,SVQSTART                                             
*                                                                               
         GOTO1 DATCON,DMCB,SVQSTART,(3,SVFLTSTR)                                
         GOTO1 (RF),(R1),SVQEND,(3,SVFLTEND)                                    
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER40                                                           
*                                                                               
         BAS   RE,CHKFLT             FILLS IN SVQEND/SVFLTEND                   
*                                                                               
         CLC   SVFLTEND,SVGENEND                                                
         BNE   FLTDTER                                                          
*                                                                               
         B     VPERX                                                            
*                                                                               
* PERIOD DATES SHOULD FALL ENTIRELY WITHIN THIS ESTIMATE *                      
*                                                                               
VPER40   CLC   SVFLTSTR,SVESTEND    PER START AFTER EST END                     
         BH    ESTDTERR                                                         
         CLC   SVFLTSTR,SVESTSTR    PER START BEFORE EST STR                    
         BL    ESTDTERR                                                         
*                                                                               
         OC    SVFLTEND,SVFLTEND    ANY END DATE ENTERED                        
         BNZ   VPER44                                                           
         MVC   SVFLTEND,SVESTEND    USE EST END DATE                            
         GOTO1 DATCON,DMCB,(3,SVFLTEND),SVQEND                                  
         B     VPER46                                                           
*                                                                               
* BOTH DATES GIVEN, END MUST MATCH ESTIMATE END *                               
*                                                                               
VPER44   CLC   SVFLTEND,SVESTEND    LAST TLCST MUST BE EST END                  
         BNE   ESTDTERR                                                         
*                                                                               
VPER46   MVC   SVGENDTS,SVFLTDTS                                                
*                                                                               
VPERX    DS  0H                                                                 
         GOTO1 DATCON,DMCB,(3,SVFLTSTR),(2,SVFLTSTP)                            
         GOTO1 (RF),(R1),(3,SVFLTEND),(2,SVFLTEDP)                              
         GOTO1 (RF),(R1),(3,SVGENST),(2,SVGENSTP)                               
         GOTO1 (RF),(R1),(3,SVGENEND),(2,SVGENEDP)                              
*                                                                               
         XIT1                                                                   
*                                                                               
ESTDTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=CL37'* ERROR * DATE(S) NOT IN EST PERIOD *'         
         GOTO1 ERREX2                                                           
         EJECT                                                                  
***************************************************************                 
* SUBROUTINE DETERMINES FLIGHT DATES FOR GIVEN TELECAST DATES *                 
***************************************************************                 
*                                                                               
CHKFLT   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         CLI   BPRD,X'FF'          TEST POL INST                                
         BE    *+10                                                             
         MVC   KEY+5(1),BPRD       TRY FOR PRODUCT SPECIFIC RECORD              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    CHKF2                                                            
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
CHKF2    CLC   KEY(6),KEYSAVE                                                   
         BNE   NOFLTER                                                          
         CLC   SVFLTSTR,KEY+6      FIRST TLCST DATE TO RECORD END DATE          
         BNH   CHKF4                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     CHKF2                                                            
*                                                                               
CHKF4    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
CHKF6    BRAS  RE,NEXTEL                                                        
         BNE   FLTELER                                                          
*                                                                               
         USING FLTDTAEL,R6                                                      
*                                                                               
         OC    SVFLTEND,SVFLTEND   TEST END DATE GIVEN                          
         BZ    CHKF10                                                           
*                                                                               
         CLC   SVFLTSTR,FLTEND     FIRST TLCST AFTER FLIGHT END                 
         BH    CHKF6                                                            
         CLC   SVFLTEND,FLTSTART   LAST TLCST BEFORE FLIGHT START               
         BL    CHKF6                                                            
         EJECT                                                                  
* TELECAST DATES SHOULD FALL ENTIRELY WITHIN THIS FLIGHT *                      
*                                                                               
         CLC   SVFLTEND,FLTEND     LAST TLCST DATE TO FLT END                   
         BH    FLTOVLER                                                         
         CLC   SVFLTSTR,FLTSTART                                                
         BL    FLTOVLER                                                         
         MVC   SVGENDTS,FLTSTART   SAVE FLT START/END DATES                     
         CLC   SVFLTEND,FLTEND       TEST LAST TLCST = FLIGHT END               
         BNE   VPERX                                                            
         MVC   SVGENST,SVFLTSTR    SAVE FLIGHT START                            
         B     VPERX                                                            
*                                                                               
* ONLY ONE DATE GIVEN - MATCH FLIGHT START DATE *                               
*                                                                               
CHKF10   CLC   SVFLTSTR,FLTSTART                                                
         BNE   CHKF6                                                            
*                                                                               
         MVC   SVGENDTS,FLTSTART                                                
*                                                                               
         MVC   SVFLTEND,SVGENEND                 FORCE END DATE                 
         GOTO1 DATCON,DMCB,(3,SVFLTEND),SVQEND   AND REQ END DATE               
         B     VPERX                                                            
BDATERR  MVI   ERROR,INVDATE                                                    
         J     TRAPERR                                                          
FLTDTER  MVI   ERROR,BADFLT                                                     
         J     TRAPERR                                                          
FLTELER  MVI   ERROR,NOFLTEL                                                    
         J     TRAPERR                                                          
FLTOVLER MVI   ERROR,FLTOVLAP                                                   
         J     TRAPERR                                                          
NOFLTER  MVI   ERROR,NOFLTREC                                                   
         J     TRAPERR                                                          
MISSERRA MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* VALIDATE OPTIONS                                                              
*                                                                               
* VALID OPTIONS ARE   'MGRP'   NEEDED FOR OPTION MARKET GROUP                   
*                     'TEST'   (X80)                                            
*                     'RERUN'  (X40) RERUN ALL COVER LETTERS                    
*                              (X02) (ONE MKT REQUEST)                          
*                              (X01) (ONE STA REQUEST)                          
*        SVOPT2                                                                 
*                                                                               
*                    'CMT'     (X20) USE SVPROF16 SPEC CMT, NOT 12              
*==================================================================             
                                                                                
VOPT     NTR1  BASE=*,LABEL=*                                                   
         CLI   8(R2),C'?'         HELP                                          
         BE    VOPTH                                                            
*                                                                               
         XC    OPTIONS,OPTIONS                                                  
         XC    SVMGR,SVMGR                                                      
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VOPT4               NO MUST GO CHECK PROFILE                     
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(7,BLOCK)                                      
         LA    R4,BLOCK                                                         
*                                                                               
VOPT2    CLI   0(R4),0             TEST FOR MORE DATA                           
         BNE   VOPT16                                                           
*                                                                               
VOPT4    MVC   HDSVOPT,SVOPT       SAVE TEST OPTION FOR HEADS                   
         TM    SOXSW,SOXOKFLG      IF DDS, AND FACTEST, OKAY TO GO              
         BO    VOPT6                                                            
*                                                                               
         TM    SOXSW,SOXERFLG      IF RD ONLY/RD ONLY MODE/WRONG ADV            
         BZ    VOPT6                                                            
*                                                                               
         TM    SVOPT,OPTTEST                                                    
         BO    VOPT6                                                            
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VOPT6    CLI   SVFAXREQ,C'N'       SEE IF FAXING                                
         BNE   VOPT8                                                            
         TM    SVOPT2,OP2MLBOX     NOT FAXING                                   
         BO    FAXMBXER                                                         
         B     VOPT10                                                           
*                                                                               
VOPT8    TM    SVOPT,OPTTEST       IF FAX, CAN'T USE 'TEST'                     
         BNO   VOPT10                                                           
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=CL32'* USE FAX=NO WITH TEST REQUEST *'              
         B     VOPTERRX                                                         
*                                                                               
VOPT10   CLI   SVPROF16,0          ANY SPECIAL COMMENT REQUIRED                 
         BE    VOPT14              NO                                           
         CLI   SVPROF16,C'0'       ANY SPECIAL COMMENT REQUIRED                 
         BE    VOPT14              NO                                           
         CLI   SVPROF16,C'N'       ANY SPECIAL COMMENT REQUIRED                 
         BE    VOPT14              NO                                           
         TM    SVOPT2,OP2SPCMT     WAS CMT= ENTERED                             
         BO    VOPT14              YES                                          
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(24),=CL24'* ERROR * CMT=REQUIRED *'                      
         B     VOPTERRX                                                         
*                                                                               
VOPT14   CLI   SVTCPR02,0         TEST MKTGRP IN PROFILE                        
         BE    *+12               NO                                            
         CLI   QMGRID,C' '        WAS MARKET GROUP ENTERED                      
         BNH   MGRMISER            NO, ERROR                                    
         B     VOPTX                                                            
*                                                                               
VOPT16   CLI   0(R4),1                                                          
         BNE   VOPT20                                                           
         CLI   12(R4),C'#'                                                      
         BNE   VOPT20                                                           
         CLI   OFFLINE,C'Y'                                                     
         BNE   VOPT90                                                           
         OI    SVOPT,OPTCOPY                                                    
         B     VOPT80                                                           
*                                                                               
VOPT20   CLI   0(R4),4                                                          
         BNE   VOPT25                                                           
         CLC   =C'TEST',12(R4)                                                  
         BNE   VOPT50                                                           
         OI    SVOPT,OPTTEST                                                    
         B     VOPT80                                                           
*                                                                               
VOPT25   CLC   =C'ISCII',12(R4)                                                 
         BNE   VOPT30                                                           
         CLI   TWAOFFC,C'*'        ONLY DDS TERMINALS                           
         BNE   VOPT90                                                           
         MVI   SVT1PR7,C'N'        PRINT CML ISCII NOT CLT CML #                
         B     VOPT80                                                           
*                                                                               
VOPT30   CLI   0(R4),3                                                          
         BNE   VOPT50                                                           
         CLC   =C'MGR',12(R4)                                                   
         BNE   VOPT40                                                           
*                                                                               
         CLI   SVTCPR02,0          MUST HAVE MGROUP PROFILE ON                  
         BE    OPTCPRER                                                         
*                                                                               
         CLI   1(R4),2                                                          
         BL    MGRENTER                                                         
         CLI   1(R4),6                                                          
         BH    MGRENTER                                                         
*                                                                               
         MVC   QMGRID,22(R4)                                                    
         CLI   QMGRID+1,C'Z'                                                    
         BNH   *+8                                                              
         MVI   QMGRID+1,C' '                                                    
*                                                                               
         CLC   SVTCPR02(2),QMGRID  MUST BE SAME AS PROFILE                      
         BNE   MGRPROER                                                         
*                                                                               
         BRAS  RE,VMG             YES, GET MGROUP TITLE                         
*                                                                               
         LLC   R0,1(R4)            GET FIELD LEN                                
         BCTR  R0,0                                                             
         LA    R1,23(R4)           AND POINT TO MGRP CHAR 2                     
         CLI   0(R1),C'Z'          A NUMBER IS GROUP CODE                       
         BH    *+10                                                             
         LA    R1,1(R1)                                                         
         BCTR  R0,0                                                             
         MVC   QMGRCD,0(R1)        SAVE MKTGRP AS ENTERED                       
*                                                                               
VOPT32   LA    RE,DUB                                                           
         MVC   DUB(4),=C'0000'                                                  
*                                                                               
VOPT34   CLI   0(R1),C'0'                                                       
         BL    MGRENTER                                                         
         CLI   0(R1),C'9'                                                       
         BH    MGRENTER                                                         
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,VOPT34                                                        
*                                                                               
         PACK  WORK(3),DUB(5)                                                   
         MVC   SVMGRCD,WORK        PWOS GROUP CODE                              
         B     VOPT80                                                           
*                                                                               
VOPT40   CLC   =C'ROT',12(R4)                                                   
         BNE   VOPT50                                                           
         MVI   SVT1PR12,C'N'       SET TO PRINT PATTERN ROTATION                
         B     VOPT80                                                           
*                                                                               
VOPT50   CLI   0(R4),5                                                          
         BNE   VOPT60                                                           
         CLC   =C'RERUN',12(R4)                                                 
         BNE   VOPT54                                                           
         TM    SVOPT,X'38'                                                      
         BNZ   KEYWRDER                                                         
         OI    SVOPT,OPTRERUN                                                   
         B     VOPT80                                                           
VOPT54   CLC   =C'NOROT',12(R4)                                                 
         BNE   VOPT60                                                           
         MVI   SVT1PR12,C'Y'       SET TO SUPPRESS PATTERN ROTATION             
         B     VOPT80                                                           
*                                                                               
VOPT60   CLI   0(R4),3                                                          
         BNE   VOPT70                                                           
         CLC   =C'CMT',12(R4)      SPECIAL COMMENT CODES                        
         BNE   VOPT70                                                           
         CLI   SVPROF16,C'N'                                                    
         BE    VOPT90                                                           
         CLI   SVPROF16,0                                                       
         BE    VOPT90                                                           
         CLC   22(1,R4),SVPROF12                                                
         BE    VOPT62                                                           
         CLI   SVPROF16,C'A'       ALL SPEC CMT CODES ALLOWED                   
         BE    VOPT64                                                           
         CLC   22(1,R4),SVPROF16                                                
         BE    VOPT62                                                           
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SPECMTER),SPECMTER                                     
         MVC   CONHEAD+27(1),SVPROF16                                           
         B     VOPTERRX                                                         
*                                                                               
VOPT62   MVC   SVPROF16(1),22(R4)  SAVE SPECIAL COMMENT CODE                    
         OI    SVOPT2,OP2SPCMT                                                  
         B     VOPT80                                                           
*                                                                               
VOPT64   LA    R0,7                                                             
         LA    R1,=C'$&&@#/*N'                                                  
         CLC   22(1,R4),0(R1)                                                   
         BE    VOPT62                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=CL31'* ERROR * CMT MUST BE $&&@#/*N *'              
         GOTO1 ERREX2                                                           
*                                                                               
VOPT70   CLI   0(R4),2                                                          
         BNE   VOPT90                                                           
         CLC   =C'MB',12(R4)       MAILBOX? (INITIALLY FOR DGS ONLY)            
         BNE   VOPT90                                                           
*                                                                               
         LA    R0,MAILTABN         # ENTRIES                                    
         LA    R1,MAILTAB                                                       
         OC    22(4,R4),SPACES                                                  
*                                                                               
VOPT72   CLC   22(4,R4),0(R1)      CK IF VALID ENTRY                            
         BE    VOPT74                                                           
         LA    R1,MAILNEXT(R1)     NEXT ENTRY                                   
         BCT   R0,VOPT72                                                        
         B     MLBOXER                                                          
*                                                                               
VOPT74   CLC   MAILTAB(4),0(R1)    DEST=DDS                                     
         BNE   VOPT76                                                           
         CLI   TWAOFFC,C'*'        THIS A DDS TERM                              
         BNE   MLBOXER              NOT OKAY                                    
*                                                                               
VOPT76   MVC   SVMLBOX,4(R1)                                                    
         OI    SVOPT2,OP2MLBOX                                                  
*                                                                               
VOPT80   LA    R4,32(R4)                                                        
         B     VOPT2                                                            
*                                                                               
VOPTH    MVC   CONHEAD,VOPTHELP                                                 
         LA    R1,CONHEAD+33                                                    
         CLI   SVTCPR02,C' '       MARKET GROUP ON                              
         BNH   VOPTH10              NO                                          
         MVC   0(7,R1),=C'/MGR= *'                                              
         LA    R1,4(R1)                                                         
*                                                                               
VOPTH10  CLI   SVT1PR12,C'Y'       SUPPRESS PRINT PATTERN ROTATION              
         BE    VOPTH20                                                          
         MVC   0(9,R1),=C'/NOROT= *'                                            
         B     VOPTERRX                                                         
*                                                                               
VOPTH20  MVC   0(7,R1),=C'/ROT= *'                                              
         B     VOPTERRX                                                         
*                                                                               
FAXMBXER MVC   CONHEAD,FAXMBXMS                                                 
         B     VOPTERRX                                                         
*                                                                               
MLBOXER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(33),=CL33'* ERROR * ENTER MB=DESTINATION *'              
         B     VOPTERRX                                                         
*                                                                               
VOPT90   MVC   CONHEAD,SPACES                                                   
         MVC   CONHEAD(25),=C'* ERROR * - UNKNOWN INPUT'                        
         MVC   CONHEAD+26(8),12(R4)                                             
VOPTERRX GOTO1 ERREX2                                                           
*                                                                               
MGRENTER MVC   CONHEAD,MGRENTMS                                                 
         B     VOPTERRX                                                         
*                                                                               
DUPMGERR XC    CONHEAD,CONHEAD                                                  
         LA    R2,TRAOPTH          OPTION VALIDATION                            
         MVC   CONHEAD(34),=CL34'REQUEST 1 MARKET GROUP AT A TIME *'            
         B     VOPTERRX                                                         
*                                                                               
MGRMISER DS    0H                                                               
         LA    R2,TRAOPTH          OPTION VALIDATION                            
         MVC   CONHEAD,MGRMSG                                                   
         B     VOPTERRX                                                         
*                                                                               
NOMGRPER MVC   CONHEAD,NOMGRPMS                                                 
         MVC   CONHEAD+20(6),QMGRID                                             
         B     VOPTERRX                                                         
*                                                                               
*                                                                               
MGRPROER MVC   CONHEAD,MGRPROMS                                                 
         B     VOPTERRX                                                         
*                                                                               
OPTCPRER MVC   CONHEAD,OPTCPRMS                                                 
         B     VOPTERRX                                                         
*                                                                               
VOPTX    XIT1                                                                   
*                                                                               
KEYWRDER MVI   ERROR,BADKEYWD                                                   
         GOTO1 ERREX                                                            
*                                                                               
MGRMSG   DC    CL60'ENTER 1-2 CHAR SCHEME AND 1-4 DIGIT MARKET GROUP'           
MAILTAB  DC    C'DDS ',CL16'FAXW212 633-5676'                                   
MAILNEXT EQU   *-MAILTAB                                                        
         DC    C'DGS ',CL16'62010135'    DGS MAILBOX NUMBER (DROP FAX)          
MAILTABN EQU   (*-MAILTAB)/20                                                   
FAXMBXMS DC    CL60'* ERROR * MB (MAILBOX) REQUESTED, BUT NOT FAXING'           
SPECMTER DC    CL30'* ERROR * CMT=MUST BE * OR X *'                             
VOPTHELP DC    CL60'* VALID OPTIONS=TEST/RERUN/CMT/MB= *'                       
OPTCPRMS DC    CL60'* ERROR * MUST HAVE TC PROFILE MARKET GROUP ON *'           
MGRENTMS DC    CL60'* ERROR * MGROUP MUST BE 1-2 CHAR AND 1-4 DIGITS *'         
NOMGRPMS DC    CL60'* ERROR * NO MKTGRP XX0000 FOUND *'                         
MGRPROMS DC   CL60'* ERROR * MGROUP SCHEME DOES NOT MATCH TC PROFILE 2 C        
               AND 3 *'                                                         
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* VALIDATE MARKET LIST (IF NONE, WILL TRY TO DO ALL                             
*============================================================                   
                                                                                
VMKT     NTR1  BASE=*,LABEL=*                                                   
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VMKTX                                                            
         L     R5,AIO2                                                          
         GOTO1 SCANNER,DMCB,(R2),(11,(R5)),0                                    
         LLC   R4,DMCB+4           COUNT OF BLOCKS FOUND                        
*                                                                               
         CHI   R4,10   '          ONLY ALLOW 10 MARKET ENTRIES                  
         BH    EXTRAMER                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(8),0(R2)                                                    
         LA    R2,ELEM                                                          
         MVI   ERROPT,C'Y'         SET ERROPT FOR RETURN HERE                   
         LA    R3,MKTLIST                                                       
         MVI   BYTE,1                                                           
*                                                                               
VMKT10   MVC   ELEM+5(1),0(R5)     DATA LENGTH                                  
         PACK  ELEM+4(1),2(1,R5)   INVERT BITS                                  
         MVC   ELEM+8(10),12(R5)      MOVE IN MARKET                            
         GOTO1 VALIMKT                                                          
         CLI   ERROR,0             IF NON-ZERO, ERROR                           
         BNE   BADMKT              BAD MARKET/STATION                           
         MVC   0(2,R3),BMKT                                                     
         LLC   R1,BYTE             COUNTER                                      
         LA    R1,1(R1)            UPDATE # VALID ENTRIES                       
         STC   R1,BYTE             COUNTER                                      
         LA    R1,MKTLIST          START OF LIST                                
VMKT20   CR    R1,R3               AT END OF LIST?                              
         BNL   VMKT30              YES                                          
         CLC   0(2,R1),0(R3)       EQUAL ENTRY?                                 
         BE    VMKT34              YES, IGNORE                                  
         LA    R1,2(R1)            NEXT SLOT                                    
         B     VMKT20                                                           
*                                                                               
VMKT30   LA    R3,2(R3)                                                         
*                                                                               
VMKT34   LA    R5,32(R5)                                                        
         BCT   R4,VMKT10                                                        
*                                                                               
* SORT INTO MARKET ORDER *                                                      
*                                                                               
         LLC   R4,BYTE                                                          
         BCTR  R4,0                                                             
         MVC   0(2,R3),=X'FFFF'                                                 
         GOTO1 =V(XSORT),DMCB,MKTLIST,(R4),2,2,0,RR=SPTR34RR                    
*                                                                               
         MVI   ERROPT,0            RESET ERROPT                                 
VMKTX    XIT1                                                                   
*                                                                               
BADMKT   MVC   CONHEAD,BADMKTMS                                                 
         LA    R4,CONHEAD+16                                                    
         EDIT  (B1,BYTE),(2,(R4))                                               
         B     VMKTEREX                                                         
*                                                                               
EXTRAMER MVC   CONHEAD,EXTRAMMS                                                 
*                                                                               
VMKTEREX LA    R2,TRAMKTSH                                                      
         GOTO1 ERREX2                                                           
BADMKTMS DC    CL60'* ERROR * ENTRY XX IS A BAD MARKET *'                       
EXTRAMMS DC    CL60'* ERROR * MORE THAN 10 MARKETS ENTERED *'                   
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* VALIDATE MARKET GROUP IN TC PROFILE                                           
* SAVE THE TITLES OF THE BREAKS                                                 
* AND BUILD TABLE OF MARKET GROUP NAMES                                         
*=========================================================                      
                                                                                
VMG      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
VMG02    CLI   QMGRID,C'A'                                                      
         BL    MGRPERR                                                          
         CLI   QMGRID,C'Z'                                                      
         BH    MGRPERR                                                          
*                                                                               
         CLI   QMGRID+1,0                                                       
         BNE   *+12                                                             
         MVI   QMGRID+1,C' '                                                    
         B     VMG04                                                            
*                                                                               
         CLI   QMGRID+1,C'A'                                                    
         BL    MGRPERR                                                          
         CLI   QMGRID+1,C'Z'                                                    
         BH    MGRPERR                                                          
                                                                                
* FIND THE GROUP ID IN THE TABLE                                                
                                                                                
VMG04    L     R1,=A(SPMGRTAB)                                                  
         A     R1,SPTR34RR                                                      
         LHI   R0,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
VMG06    CLC   QMGRID,0(R1)         IS THIS IT                                  
         BE    VMG10                                                            
         LA    R1,3(R1)                                                         
         BCT   R0,VMG06                                                         
         B     MGRPERR                                                          
*                                                                               
VMG10    MVC   SVMGRID,2(R1)        SAVE 1 CHAR MKTGRP ID                       
*                                                                               
         XC    SVMGRKEY,SVMGRKEY                                                
         MVC   SVMGRTY,=X'0D02'                                                 
         MVC   SVMGRAM,BAGYMD        A-M                                        
         MVC   SVMGRCL,BCLT          CLT                                        
         MVC   SVMGRMGR(1),2(R1)     MGRPID                                     
*                                                                               
         MVC   KEY,SVMGRKEY                                                     
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT                             
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(9),KEYSAVE      0D02/A-M/CLT/00../MGID                       
         BE    VMG20                                                            
*                                                                               
         MVC   KEY,SVMGRKEY                                                     
         XC    KEY+3(2),KEY+3      CLEAR THE CLIENT FIELD                       
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE      0D02/A-M/00./00../MGID                       
         BNE   BDMGRPER                                                         
*                                                                               
VMG20    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT                             
         GOTO1 GETREC                                                           
         LA    R6,24(R6)                                                        
         USING MKGEL01,R6                                                       
         CLI   0(R6),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVMGEL01,MKGEL01    SAVE 01 ELEMENT                              
         XC    FILENAME,FILENAME                                                
*                                                                               
         XIT1                                                                   
*                                                                               
MGRPERR  SR    RE,RE                                                            
         BRAS  R1,VMGERRX                                                       
         DC    CL42'MARKET GROUP IN TC PROFILE MUST BE A-Z *'                   
*                                                                               
BDMGRPER LA    RE,QMGRID                                                        
         BRAS  R1,VMGERRX                                                       
         DC    CL42'MKTGRP X  FOUND, CHANGE TC PROF 2,3 *'                      
*                                                                               
VMGERRX  MVC   CONHEAD+10(42),0(R1)  MOVE MESSAGE                               
*                                                                               
         LTR   RE,RE                 TEST VARIABLE TEXT                         
         JZ    *+10                  NO                                         
         MVC   CONHEAD+17(2),0(RE)   ELSE MOVE IT IN                            
*                                                                               
         BRAS  R1,*+14               POINT TO ERROR TEXT                        
         DC    CL10'* ERROR * '                                                 
         MVC   CONHEAD(10),0(R1)                                                
*                                                                               
         GOTO1 ERREX2                                                           
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* READ COMMERCIAL RECORDS NEEDED FOR A PATTERN AND SAVE                         
* ON ENTRY PATTERN RECORD IS IN IO1                                             
*===================================================================            
                                                                                
BLDCMLS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,ASVCMLS                                                       
         USING SVCMLD,R2                                                        
         LA    R0,L'SVCMLDTA                                                    
         LR    R1,R2                                                            
         MVI   0(R1),0                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,*-8                                                           
         MVI   THISCML,1          SET CMML ROTATION POSN                        
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ADIDFLAG,C'N'                                                    
         TM    PATSTAT1-PATDTAEL(R6),PATSADID                                   
         BZ    *+8                                                              
         MVI   ADIDFLAG,C'Y'                                                    
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LLC   R0,1(R6)                                                         
         SRL   R0,4                16 BYTES PER PATTERN ENTRY                   
         LA    R5,2(R6)            R5 POINTS TO PATTERN ENTRY                   
*                                                                               
BLDCML2  CLC   =X'5C00',0(R5)      TEST COMMERCIAL DELETED                      
         BE    BLDCML34            YES - NEXT COMMERCIAL                        
         CLC   =X'5C00',8(R5)      JUST IN CASE ONLY P/B IS DELETED             
         BE    BLDCML34                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AC1'                                                  
         CLI   ADIDFLAG,C'Y'                                                    
         BE    *+10                                                             
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(8),0(R5)                                                   
*                                                                               
BLDCML4  GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO2             USE IO2 FOR COMMERCIALS                      
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    BLDCML6                                                          
*                                                                               
         L     RE,ASVCMLSX         GET A(END OF SVCMLS)                         
         AHI   RE,-L'SVCMLDTA                                                   
         CR    R2,RE                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDCML6  DS    0H                                                               
         LA    R4,L'SVCMLDTA                                                    
         AH    R4,=H'1'                                                         
         LR    R1,R2                                                            
         MVI   0(R1),0                                                          
         LA    R1,1(R1)                                                         
         BCT   R4,*-8                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
*                                                                               
         TM    CMLSTAT,X'80'       TEST DELETED COMMERCIAL                      
         BZ    BLDCML8             NO                                           
         MVC   0(2,R5),=X'5C00'    SET FLAG IN PTTN CMML LIST ELEM              
*                                                                               
         AHI   R2,-L'SVCMLDTA      BACK UP TO PREVIOUS ENTRY                    
         CLI   SVCMLPIG,1          WAS THIS 2ND OF PIGGYBACK PAIR?              
         BNE   BLDCML32            NO                                           
         LA    R4,L'SVCMLDTA                                                    
         LR    R1,R2                                                            
         MVI   0(R1),0                                                          
         LA    R1,1(R1)                                                         
         BCT   R4,*-8                                                           
         B     BLDCML34            AND REUSE PREVIOUS ENTRY                     
*                                                                               
BLDCML8  DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,PTNFTD),(3,DUB)                                   
         GOTO1 (RF),(R1),(2,PTNLTD),(3,DUB+3)                                   
*                                                                               
         CLC   CMLRLSE,DUB         SEE IF THIS CMML WILL START IN TIME          
         BH    CMLDTERA             NO, CMLRLSE AFTER PAT START                 
         CLC   CMLRCL,DUB          IS CML RECALL BEFORE PAT START               
         BL    CMLDTERC             YES, ERROR                                  
         CLC   CMLRCL,DUB+3        SEE IF THIS CMML WILL LAST THRU PAT          
         BNL   *+14                YES, OK                                      
         CLC   PTNLTD,=XL2'FFFF'   IS PAT UFN                                   
         BNE   CMLDTERB                                                         
*                                                                               
         MVC   SVCMLCOD(8),KEY+5                                                
         MVC   SVCMLCOD+8(4),SPACES  UNTIL CML IS ADID!                         
         MVC   SVCMLNAM,CMLTITLE                                                
         CLI   SVT2PR02,C'Y'       TEST TO SUPPRESS CMML TYPE                   
         BE    *+10                                                             
         MVC   SVCMLTYP,CMLTYPE                                                 
*                                                                               
BLDCML10 MVC   SVCMLADI,SVCMLCOD   USE ISCI AS ADID IN CASE NO ADID             
         MVC   SVCMLADI+8(4),SPACES                                             
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   BLDCML12                                                         
         GOTO1 VTRPACK,DMCB,(C'U',SVCMLCOD),SVCMLADI                            
*                                                                               
BLDCML12 MVC   SVCMLSLN,CMLSLN                                                  
         MVC   SVCMLOV1,CMLOVRD1                                                
         MVC   SVCMLOV2,CMLOVRD2                                                
         MVC   SVCMLSEQ,CMLSEQ+1                                                
         MVC   SVCMLPOS,THISCML    SAVE POSITION NUMBER                         
         MVC   SVCMLST,CMLSTAT     SAVE STATUS (POSSIBLE TEXT)                  
         MVC   SVCMLCLT,CMLCLTNO                                                
         MVC   SVCMLFTD,PTNFTD                                                  
         MVC   SVCMLLTD,PTNLTD                                                  
         MVC   SVCMLPRD,PTNBPRD                                                 
         MVI   SVCMLPIG,0                                                       
         OC    8(8,R5),8(R5)       TEST PIGGYBACK                               
         BZ    BLDCML20                                                         
         MVI   SVCMLPIG,1          SET PRD 1                                    
         CLC   0(8,R5),KEY+5       TEST THIS IS PRD1                            
         BE    BLDCML20                                                         
         MVC   SVCMLPRD,PTNBPRD2                                                
         MVI   SVCMLPIG,2          SET PRD 2                                    
*                                                                               
BLDCML20 MVI   ELCODE,X'30'        GET TITLES 2/3                               
         BRAS  RE,NEXTEL                                                        
         BNE   BLDCML22                                                         
         MVC   SVCMLNM2,3(R6)                                                   
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   BLDCML22                                                         
         MVC   SVCMLNM3,3(R6)                                                   
*                                                                               
BLDCML22 MVI   ELCODE,X'40'        LOOK FOR TELCASTER                           
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SVCMLTEL,2(R6)                                                   
*                                                                               
         MVI   ELCODE,X'45'        LOOK FOR AD-ID                               
         USING CMLXTLEL,R6                                                      
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   *+16                                                             
         MVC   SVCMLTLX,CMLXTLNM                                                
         MVC   SVCMLTHD,CMLXTLHD                                                
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'46'        LOOK FOR AD-ID                               
         USING CMLCBCEL,R6                                                      
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SVCMLCBC,CMLCBCNM                                                
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'47'        LOOK FOR AD-ID                               
         USING CMLTCYEL,R6                                                      
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   *+16                                                             
         MVC   SVCMLTC1,CMLTCYC1                                                
         MVC   SVCMLTC2,CMLTCYC2                                                
         DROP  R6                                                               
*                                                                               
BLDCML24 MVI   ELCODE,X'A0'        LOOK FOR AD-ID                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   BLDCML26                                                         
         USING CMLADIEL,R6                                                      
         MVC   SVCMLADI,CMLADID                                                 
         DROP  R6                                                               
*                                                                               
BLDCML26 MVI   ELCODE,X'24'        LOOK FOR EXTENDED DATA EL                    
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   BLDCML28                                                         
         USING CMLXDTEL,R6                                                      
*                                                                               
         MVC   SVCMLHDF,CMLXHDEF                                                
         MVC   SVCMLCTR,CMLXCNTR                                                
         CLI   CMLXSWAP,C'Y'                                                    
         BNE   *+16                                                             
         MVC   SVCMLHDF,CMLXCNTR                                                
         MVC   SVCMLCTR,CMLXHDEF                                                
*                                                                               
         MVC   SVCMLPRN,CMLXPRNT                                                
         MVC   SVCMLHSE,CMLXPRHS                                                
         MVC   SVCMLDDT,CMLXDSDT                                                
         MVC   SVCMLDTM,CMLXDSTM                                                
         DROP  R6                                                               
*                                                                               
BLDCML28 MVI   ELCODE,X'B0'        MATCHING DATA ELEMENT                        
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   BLDCML30                                                         
         USING CMLMATEL,R6                                                      
         MVC   SVCMLSTM,CMLMSTIM                                                
         MVC   SVCMLETM,CMLMETIM                                                
         TM    CMLMFLAG,CMLMFDAY   TEST DAILY TIMES FLAG                        
         BZ    *+8                                                              
         OI    SVCMLST,X'80'       SAVE FLAG                                    
         DROP  R6                                                               
*                                                                               
BLDCML30 CLI   SVCMLPIG,1          TEST PIGGYBACK                               
         BNE   BLDCML32                                                         
         LA    R2,L'SVCMLDTA(R2)   POINT TO NEXT ENTRY                          
         MVC   KEY+5(8),8(R5)      SET TO PROCESS SECOND CMML                   
         B     BLDCML4                                                          
*                                                                               
BLDCML32 LA    R2,L'SVCMLDTA(R2)   POINT TO NEXT ENTRY                          
*                                                                               
BLDCML34 LA    R5,16(R5)           16 BYTES PER ENTRY                           
         LLC   RE,THISCML                                                       
         AHI   RE,1                                                             
         STC   RE,THISCML                                                       
         BCT   R0,BLDCML2                                                       
         EJECT                                                                  
*================================================================               
* ADD NEW CMMLS TO SVADDL TABLE IF THEY HAVE ADDL DATA                          
*================================================================               
                                                                                
         L     R2,ASVCMLS                                                       
*                                                                               
BLDCML40 OC    SVCMLSEQ,SVCMLSEQ   TEST ANY CMML TEXT                           
         BNZ   BLDCML42                                                         
         OC    SVCMLDDT,SVCMLDDT   TEST ANY DESTROY DATE                        
         BNZ   BLDCML42                                                         
         CLC   SVCMLHSE,SPACES     TEST ANY PROD HOUSE                          
         BH    BLDCML42                                                         
         CLI   SVT2PR02,C'Y'       TEST SUPPRESS TYPE                           
         BE    *+12                                                             
         CLI   SVCMLTYP,C' '       TEST ANY CMML TYPE                           
         BH    BLDCML42                                                         
         B     BLDCML50            CMML HAS NO ADDL DATA                        
*                                                                               
BLDCML42 L     R4,ASVADDL          ADD ENTRY TO TABLE                           
         USING SVADDLD,R4                                                       
*                                                                               
BLDCML44 CLI   0(R4),0             TEST FREE ENTRY                              
         BE    BLDCML46                                                         
         CLC   SVADDCML,SVCMLCOD   SAME CMML                                    
         BE    BLDCML50                                                         
         LA    R4,SVADDNXT                                                      
         B     BLDCML44                                                         
*                                                                               
BLDCML46 MVC   SVADDCML,SVCMLCOD                                                
         MVC   SVADDADI,SVCMLADI                                                
         MVC   SVADDPRD,SVCMLPRD                                                
         MVC   SVADDSEQ,SVCMLSEQ                                                
         MVC   SVADDDDT,SVCMLDDT                                                
         MVC   SVADDDTM,SVCMLDTM                                                
         MVC   SVADDHSE,SVCMLHSE                                                
         CLI   SVT2PR02,C'Y'       TEST SUPPRESS TYPE                           
         BE    *+10                                                             
         MVC   SVADDTYP,SVCMLTYP                                                
*                                                                               
BLDCML50 LA    R2,SVCMLNXT                                                      
         CLI   0(R2),0                                                          
         BNE   BLDCML40                                                         
         J     EXIT                                                             
         DROP  R2,R4                                                            
*                                                                               
CMLDTERA MVI   ERROR,BDCMLDTS      CML RELEASE DTE AFTER PAT START              
         B     CMLDTERX                                                         
*                                                                               
CMLDTERB MVI   ERROR,BDCMLDTE      CML RECALL DTE BEFORE PAT END                
         B     CMLDTERX                                                         
*                                                                               
CMLDTERC MVI   ERROR,BDCMLRCD      CML RECALL DTE BEFORE PAT START              
*                                                                               
CMLDTERX CLI   ERROPT,C'Y'                                                      
         JE    EXIT                                                             
         GOTO1 ERREX                                                            
         EJECT                                                                  
*===================================================================            
* HEADHOOK ROUTINE                                                              
*===================================================================            
                                                                                
HDHK     NTR1 BASE=*,LABEL=*                                                    
                                                                                
         EDIT  (B2,SVCRTBNO),(4,H1+101)     PRINT TABLE NUMER                   
         MVI   H1+105,C','                                                      
         LH    R1,SEQNUM                    AND SEQNUM                          
         LA    R1,1(R1)                                                         
         STH   R1,SEQNUM                                                        
         EDIT  (R1),(3,H1+106),ALIGN=LEFT                                       
*                                                                               
         CLI   HEADSW,C'C'         TEST COVER LETTER PRINTING                   
         BNE   HDHK06                                                           
         MVC   H1+38(39),=CL39'******* INSTRUCTION COVER LETTER ******'         
         B     HDHK10                                                           
*                                                                               
HDHK06   MVC   H1+38(40),SPACES                                                 
         MVC   H1+39(10),=C'SPOT RADIO'                                         
         CLI   QMED,C'R'                                                        
         BE    HDHK08                                                           
         MVC   H1+38(13),=C'NETWORK RADIO'                                      
         CLI   QMED,C'X'                                                        
         BE    HDHK08                                                           
         MVC   H1+38(15),=C'SPOT TELEVISION'                                    
         CLI   QMED,C'T'                                                        
         BE    HDHK08                                                           
         MVC   H1+36(18),=C'NETWORK TELEVISION'                                 
         CLI   QMED,C'N'                                                        
         BE    HDHK08                                                           
         DC    H'0'                                                             
*                                                                               
HDHK08   MVC   H1+55(23),=C'COMMERCIAL INSTRUCTIONS'                            
         GOTO1 SQUASHER,DMCB,H1+38,40                                           
         GOTO1 CENTER,DMCB,,40                                                  
* PRINTS FOR INSTRUCTIOS, NOT COVER PAGE                                        
         MVC   H4+38(22),=C'(FROM INSTRUCTIONS OF '                             
         GOTO1 DATCON,DMCB,(2,SVSTAIDT),(8,H4+60)                               
         MVI   H4+68,C')'                                                       
         GOTO1 CENTER,DMCB,H4+38,40                                             
*                                                                               
HDHK10   GOTO1 UNDERLIN,DMCB,(40,H1+38),H2+38                                   
*&&DO                                                                           
         CLI   HEADSW,C'C'         TEST COVER LETTER                            
         BE    HDHK11X                                                          
         CLI   SVINSRCT,0          TEST ALL SAME REVISION                       
         BNE   HDHK11X             NO                                           
*                                                                               
         MVC   H4+84(8),=C'REVISION'                                            
         LLC   R0,SVINSREV                                                      
         AHI   R0,1                                                             
HDHK11   DS    0H                                                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  H4+94(3),DUB                                                     
*                                                                               
HDHK11X  CLI   SVINSREV,0          TEST ORIGINAL INSTRUCTIONS                   
         BNE   HDHK12               THESE ARE ORIGINAL                          
         TM    SVOPT,OPTRERUN      TEST THIS IS A RERUN                         
         BZ    HDHK14               NO - TREAT THESE AS REVISIONS               
*                                                                               
HDHK12   MVC   H4+38(40),SPACES    OVERWRITE SUPERSEDES LINE                    
*                                                                               
HDHK13   MVC   H4+84(21),=C'ORIGINAL INSTRUCTIONS'                              
*&&                                                                             
HDHK14   DS   0H                                                                
         TM    HDSVOPT,OPTTEST       TEST OPTION TEST                           
         BZ    *+10                                                             
         MVC   H5+50(16),=C'*** TEST RUN ***'                                   
*                                                                               
         TM    SVOPT,OPTRERUN                                                   
         BZ    *+10                                                             
         MVC   H4+106(3),=C'(R)'                                                
*                                                                               
         TM    SVOPT,OPTREV        TEST REVISIONS ONLY                          
         BZ    *+10                                                             
         MVC   H4+106(3),=C'(V)'                                                
*                                                                               
         TM    SVOPT,OPTCOPY                                                    
         BO    *+12                                                             
         CLI   SVFAXREQ,C'2'        TEST AGENCY COPY                            
         BNE   HDHK16                                                           
         MVC   H6+50(17),=C'** AGENCY COPY **'                                  
*                                                                               
         CLI   SVPROF5,C'N'        TEST SUPPRESS REQUESTOR/REPORT               
         BNE   HDHK16                                                           
         MVC   H1+84(14),SPACES    REPORT (LEAVE SEQUENCE NUMBER)               
         MVC   H5+92(18),SPACES    REQUESTOR                                    
*                                                                               
HDHK16   CLC   PAGE,=H'1'                                                       
         BE    *+10                                                             
         MVC   H6+85(21),=C'***** CONTINUED *****'                              
*                                                                               
         MVC   H7+2(6),=C'CLIENT'                                               
         MVC   H7+11(3),QCLT                                                    
         MVC   H7+16(20),CLTNM                                                  
*                                                                               
         MVC   H8+2(7),=C'PRODUCT'                                              
*                                                                               
         L     RE,ASVPRDS                                                       
         USING SVPRDD,RE                                                        
*                                                                               
HDHK20   CLI   BPRD,X'FF'          TEST POL REQUEST                             
         BNE   HDHK22                                                           
         MVC   H8+11(3),QPRD       POOL REQUEST                                 
         MVC   H8+16(20),PRDNM                                                  
         B     HDHK26                                                           
*                                                                               
HDHK22   CLC   BPRD,SVPRDCD        NON-POL REQUEST                              
         BE    HDHK24                                                           
         LA    RE,L'SVPRDDTA(RE)                                                
         CLI   0(RE),0                                                          
         BNE   HDHK22                                                           
         DC    H'0'                                                             
*                                                                               
HDHK24   MVC   H8+11(3),SVPRDEBC                                                
         MVC   H8+16(20),SVPRDNM                                                
         DROP  RE                                                               
*                                                                               
HDHK26   MVC   H9+2(8),=C'ESTIMATE'                                             
         CLI   SVPROF11,C'E'       COPY CODE=ESTIMATE                           
         BNE   HDHK28                                                           
         EDIT (B1,QBEST),(3,H9+11),ALIGN=LEFT                                   
         CLI   SVT1PR14,C'Y'       PRINT EST HDR DESC                           
         BNE   *+10                                                             
         MVC   H9+16(20),QESTDESC                                               
         B     HDHK30                                                           
*                                                                               
HDHK28   MVC   H9+11(7),=C'VARIOUS'                                             
*                                                                               
HDHK30   CLI   QMGRID,C' '         COVER LETTERS BY MARKET GROUP                
         BNH   HDHK40               NO                                          
*                                                                               
         MVC   H11(12),SVMGEL01    MOVE TITLE FROM BREAK 1                      
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,SVCRTBNO                                                    
         L     R1,APATTAB                                                       
         SR    RF,RF                                                            
         B     HDHK32X                                                          
*                                                                               
HDHK32   ICM   RF,3,0(R1)                                                       
         AR    R1,RF                                                            
HDHK32X  BCT   R0,HDHK32                                                        
*                                                                               
**NOP**  MVC   H11+13(2),QMGRID                                                 
         MVC   H11+16(13),=C'** UNKNOWN **'                                     
         CLC   5(2,R1),=XL2'9999'                                               
         BE    HDHK40                                                           
*                                                                               
         L     RE,AMGRTAB                                                       
         LA    RF,MAXMGR                                                        
*                                                                               
HDHK34   CLC   2(3,R1),0(RE)       THIS SAME MARKET GROUP                       
         BE    HDHK36                                                           
         LA    RE,27(RE)                                                        
         BCT   RF,HDHK34                                                        
         DC    H'0'                                                             
*                                                                               
HDHK36   DS    0H                                                               
**NOP**  MVC   H11+13(2),QMGRID                                                 
**NOP**  UNPK  DUB(5),2(3,R1)      UNPACK 1 EXTRA BYTE                          
**NOP**  MVC   H11+15(4),DUB                                                    
**NOP**  MVC   H11+13(6),QMGRID    ELSE OUTPUT GROUP AS INPUT                   
         MVC   H11+16(24),3(RE)    OUTPUT GROUP NAME                            
*                                                                               
HDHK40   MVC   H7+50(7),=C'CONTACT'                                             
         MVC   H7+58(L'QUESTOR),QUESTOR                                         
*                                                                               
         OC    CONEMAIL,SPACES          FORCE NULLS TO SPACES                   
         CLC   CONEMAIL,SPACES          ANY E-MAIL                              
         BNH   *+10                      NO                                     
         MVC   H8+58(50),CONEMAIL                                               
*                                                                               
         MVC   H9+50(5),=C'PHONE'                                               
         MVC   H9+58(12),CONTEL                                                 
         OC    CONTEL+13(5),CONTEL+13   ANY EXTENSION                           
         BZ    HDHK37                    NO                                     
         MVC   H9+71(3),=C'EXT'                                                 
         MVC   H9+75(5),CONTEL+13                                               
*                                                                               
HDHK37   OC    CONFAX(18),CONFAX        ANY FAX                                 
         BZ    HDHK38                    NO                                     
         MVC   H10+50(3),=C'FAX'                                                
         MVC   H10+58(12),CONFAX                                                
         OC    CONFAX+13(5),CONFAX+13   ANY EXTENSION                           
         BZ    HDHK38                    NO                                     
         MVC   H10+71(3),=C'EXT'                                                
         MVC   H10+75(5),CONFAX+13                                              
*                                                                               
HDHK38   CLI   PIGSW,C'Y'          IS THERE A PIGGYBACK PRODUCT                 
         BNE   *+10                 NO                                          
         MVC   H12+67(26),=C'*** PIGGYBACKS PRESENT ***'                        
*                                                                               
HDHKX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* MIDLINE HOOK                                                                  
*=============================================================                  
                                                                                
MIDHK    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
         CLI   MIDHOOK,C'R'        TEST RETURN CALL                             
         BE    MIDHK22                                                          
*                                                                               
MIDHK4   CLI   HEADSW,C'A'         TEST PRINTING ADD'L DATA                     
         BE    MIDHK10                                                          
         CLI   HEADSW,C'T'         IF HEADSW = T, PRINTING PATTERN TEXT         
         BE    MIDHKX              SO NO MIDLINES                               
         CLI   HEADSW,C'Y'         Y MEANS TO PRINT INST MIDLINES               
         BE    MIDHK6                                                           
         CLI   HEADSW,C'C'         C MEANS TO PRINT COVER MIDLINES              
         BE    MIDHK20                                                          
         B     MIDHKX                                                           
*                                                                               
MIDHK6   LA    RE,MIDLINEX-MIDLINE   SET LEN OF ENTIRE MIDLINE                  
         CLI   SVT3PROF+2,C'Y'       TEST PRINT CMML RUN TIMES                  
         BE    *+8                                                              
         LA    RE,MIDLINE1-MIDLINE   SET LEN WITHOUT RUN TIMES                  
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     MIDHKX                                                           
         MVC   MID1(0),MIDLINE                                                  
*                                                                               
MIDHK10  MVC   MID1(33),=C'ADDITIONAL COMMERCIAL INFORMATION'                   
         MVC   MID2(33),=C'---------------------------------'                   
         CLI   CONTINUE,C'Y'                                                    
         BE    MIDHK12                                                          
         MVI   CONTINUE,C'Y'       SET TO PRINT CONT'D NEXT TIME                
         B     MIDHKX                                                           
*                                                                               
MIDHK12  MVC   MID1+33(10),=C' CONTINUED'                                       
         MVC   MID2+33(10),=C'----------'                                       
         B     MIDHKX                                                           
*                                                                               
MIDHK20  MVC   MID1+2(MIDLNC1X-MIDLNC1),MIDLNC1                                 
         MVI   MID2,0              FORCE A SPACE BETWEEN                        
         MVI   MIDHOOK,C'R'        SET FOR RETURN CALL                          
         B     MIDHKX                                                           
*                                                                               
MIDHK22  MVI   MIDHOOK,0           RESET RETURN CALL                            
         MVC   MID1+2(6),=C'MARKET'                                             
         MVC   MID2+2(MIDLNC2X-MIDLNC2),MIDLNC2                                 
*                                                                               
         CLI   SVT2PR02,C'Y'       TEST TO SUPPRESS TYPE                        
         BE    *+10                                                             
         MVC   MID2+2+73(4),SPACES                                              
*                                                                               
MIDHKX   XIT1                                                                   
*                                                                               
MIDLINE  DC    C'---------- ROTATION  COMMERCIAL ---- COMMERCIAL'               
         DC    C' TITLE --- --------- OTHER ----------'                         
MIDLINE1 EQU   *                                                                
         DC    C' CMML RUN TIMES'                                               
MIDLINEX EQU   *                                                                
*                                                                               
MIDLNC1  DC    C'COMMERCIALS TO BE AIRED BY THE FOLLOWING LIST OF '             
         DC    C'STATIONS'                                                      
MIDLNC1X EQU   *                                                                
MIDLNC2  DC    C'NUMBER NAME                   STATION'                         
         DC    CL11' '                                                          
         DC    C'TELECAST DATES     REV   TYPE'                                 
MIDLNC2X EQU   *                                                                
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* FOOTLINE HOOK ROUTINE                                                         
* PUT FOOTLINE DATA IN P WHICH HAS BEEN SAVED BY SPOOL                          
* WHEN THIS ROUTINE RETURNS P=SPACES, IT WILL NOT BE CALLED AGAIN               
*=================================================================              
                                                                                
FTHK     NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         CLI   FOOTSW,5            TEST DONE 5 LINES YET                        
         BE    FTHK10              YES - DONE                                   
         LLC   RE,FOOTSW                                                        
         LA    RE,1(RE)                                                         
         STC   RE,FOOTSW                                                        
         MVI   HEADFLAG,C'F'       SET FOOTLINES PRINTING                       
*                                                                               
FTHK1    MVC   P,SPACES            P DOES NOT GET CLEARED                       
         MVI   P,0                 FORCE RETURN                                 
*                                                                               
         CLI   CONTINUE,C'N'       TEST FINISHED                                
         BE    FTHK1C              YES                                          
         CLI   FOOTSW,1                                                         
         BNE   FTHK1C                                                           
         MVI   P,0                 FORCE RETURN                                 
         MVC   P(40),=C'** INSTRUCTIONS CONTINUE ON NEXT PAGE **'               
*                                                                               
FTHK1C   CLI   FOOTSW,3            REACHED LINE 3 YET                           
         BL    FTHK4               NO - SKIP FOOTNOTE                           
                                                                                
* CHECK FOR FOOTNOTE LINE *                                                     
                                                                                
         L     R2,ASVFTNT                                                       
         LLC   R0,FOOTSW                                                        
         AHI   R0,-2                                                            
         B     FTHK2X                                                           
FTHK2    CLI   0(R2),0             TEST MORE DATA                               
         BE    FTHK4                                                            
         LA    R2,60(R2)                                                        
FTHK2X   BCT   R0,FTHK2                                                         
         CLI   0(R2),0                                                          
         BE    FTHK4                                                            
         MVC   P(60),0(R2)                                                      
*                                                                               
FTHK4    CLI   CONTINUE,C'N'       TEST LAST PAGE                               
         BE    FTHK6               YES - PRINT STATION ADDRESS                  
         CLI   FOOTSW,2            ONLY PRINT 2 STATION ADDRESS LINES           
         BH    FTHK20              IF NOT LAST PAGE                             
         EJECT                                                                  
*===========================================================                    
* FORMAT PRODUCTION HOUSE ADDRESS AND FAX                                       
*===========================================================                    
                                                                                
FTHK6    LA    R2,PRDHSEL1                                                      
         LLC   R0,FOOTSW                                                        
         BCTR  R0,0                                                             
         MHI   R0,24                                                            
         AR    R2,R0                                                            
         MVC   P+75(24),0(R2)                                                   
         B     FTHKX                                                            
*                                                                               
FTHK10   MVI   FOOTSW,0                                                         
         MVC   P,SPACES                                                         
         B     FTHKX                                                            
*                                                                               
FTHK20   MVI   P+75,C'*'                                                        
         MVC   P+76(23),P+75                                                    
*                                                                               
FTHKX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*====================================================                           
* SUBROUTINE TO PRINT BOXES AROUND TEXT                                         
* AIO MUST HAVE RECORD ADDRESS                                                  
* ELCODE MUST CONTAIN COMMENT ELEM CODE                                         
* P1  (1) = MAXIMUM LEN OF EXPANDED COMMENT                                     
* P1+1(3) = EXPANDED COMMENT OUTPUT AREA                                        
*====================================================                           
                                                                                
BOXER    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* FIRST - FIND LENGTH OF LONGEST COMMENT *                                      
*                                                                               
         L     R3,0(R1)            GET OUTPUT AREA ADDRESS                      
         LLC   R4,0(R1)            GET OUTPUT RECORD SIZE                       
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R5,R5                                                            
*                                                                               
BOX2     LLC   RE,1(R6)                                                         
         CLC   =C'BOX=',3(R6)                                                   
         BNE   *+8                                                              
         SH    RE,=H'4'                                                         
         CR    R5,RE                                                            
         BH    *+6                                                              
         LR    R5,RE                                                            
         BRAS  RE,NEXTEL                                                        
         BE    BOX2                                                             
*                                                                               
* LENGTH IN R5 INCLUDES 3 FOR ELCODE/LEN/SEQ - NOW ADJUST FOR                   
* '*/SP' AND 'SP/*' AT EITHER END                                               
*                                                                               
         LA    R5,1(R5)                                                         
*                                                                               
* CREATE ROW OF *'S THIS LENGTH                                                 
*                                                                               
         EX    R4,BOXSPC                                                        
         MVI   0(R3),C'*'                                                       
         BCTR  R5,0                ADJUST FOR FIRST *                           
         BCTR  R5,0                SET FOR EX                                   
         EX    R5,BOXR7                                                         
         AR    R3,R4               POINT TO NEXT OUTPUT LINE                    
         LA    R5,2(R5)            RESTORE LENGTH                               
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
BOX4     EX    R4,BOXSPC                                                        
*                                                                               
         LLC   RE,1(R6)                                                         
         LA    RF,3(R6)                                                         
*                                                                               
         CLC   =C'BOX=',0(RF)                                                   
         BNE   *+12                                                             
         SH    RE,=H'4'                                                         
         LA    RF,4(RF)                                                         
*                                                                               
         MVI   0(R3),C'*'                                                       
         SH    RE,=H'4'            SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R3),0(RF) *EXECUTED*                                         
*                                                                               
         LA    RE,0(R3,R5)         POINT TO END OF LINE                         
         BCTR  RE,0                BACK UP                                      
         MVI   0(RE),C'*'                                                       
         AR    R3,R4               POINT TO NEXT LINE                           
         BRAS  RE,NEXTEL                                                        
         BE    BOX4                                                             
*                                                                               
         EX    R4,BOXSPC                                                        
         MVI   0(R3),C'*'                                                       
         BCTR  R5,0                ADJUST FOR FIRST *                           
         BCTR  R5,0                SET FOR EX                                   
         EX    R5,BOXR7                                                         
         AR    R3,R4                                                            
         MVI   0(R3),0             SET END OF BUFFER FLAG                       
         XIT1                                                                   
*                                                                               
BOXR7    MVC   1(0,R3),0(R3)  *EXECUTED*                                        
BOXSPC   MVC   0(0,R3),SPACES *EXECUTED*                                        
         EJECT                                                                  
*===============================================================                
* READ PRODUCTION HOUSE ADDRESS                                                 
*===============================================================                
                                                                                
RDHOUSE  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,TRAHSEH                                                       
         CLI   5(R2),0             CHECK FOR HOUSE OVERRIDE                     
         BE    RDHSE10             NO                                           
*                                                                               
         GOTO1 ANY                                                              
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A29'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+3(6),WORK                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PRDHSER                                                          
         B     RDHSE50                  READ HOUSE AND SAVE ADDRESS             
*                                                                               
RDHSE10  XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(8),=C'99999999'                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOPRDHSE                                                         
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,24(R6)                                                        
         CLI   0(R6),X'10'                                                      
         BNE   NOPRDHSE            MUST BE 9'S KEY PTR TO PROD HSE              
*                                                                               
         USING CMLDTAEL,R6                                                      
*                                                                               
* BUILD PRODUCTION HOUSE KEY *                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A29'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(6),CMLTITLE   PROD HOUSE ID IS IN TITLE                    
         OC    KEY+3(6),SPACES                                                  
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PRDHSER                                                          
*                                                                               
RDHSE50  L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   HOUSECD,3(R6)                                                    
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PRDHSELS(96),2(R6)     SAVE ADDRESS                              
*                                                                               
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   RDHSE60                                                          
*                                                                               
         USING PRHFAXEL,R6                                                      
         LA    R1,PRDHSEFX                                                      
         MVC   0(4,R1),=C'FAX '                                                 
         LA    R1,4(R1)                                                         
*                                                                               
         CLI   PRHFAXLN,20                                                      
         BE    RDHSE54                                                          
         CLI   PRHFTEL1,0                                                       
         BE    RDHSE54                                                          
         MVC   PRDHFAX(1),PRHFTEL1                                              
         LA    R1,1(R1)                                                         
*                                                                               
RDHSE54 MVC    0(18,R1),PRHFTEL                                                 
         B     RDHSEX                                                           
         DROP  R6                                                               
*                                                                               
RDHSE60  CLI   SVFAXREQ,C'N'         FAX REQUIRED FOR FAX REPORT                
         BE    RDHSEX                                                           
         B     NOFAXERR                                                         
*                                                                               
RDHSEX   XIT1                                                                   
*                                                                               
NOPRDHSE MVI   ERROR,NOPRDHS                                                    
         B     PRDHSERA                                                         
*                                                                               
PRDHSER  MVI   ERROR,INVPRHSE                                                   
         B     ERRXIT                                                           
*                                                                               
PRDHSERA LA    R2,TRAMEDH          POSITION CURSOR                              
*                                                                               
ERRXIT   GOTO1 ERREX                                                            
*                                                                               
NOFAXERR MVC   CONHEAD,NOFAXMSG                                                 
         GOTO1 ERREX2                                                           
*                                                                               
NOFAXMSG DC    CL60'* ERROR * PRODUCTION HOUSE MUST HAVE FAX NUMBER'            
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* SET UP FOR INSTRUCTIONS.                                                      
* FIRST FIND COMMON REV, INST, AND TLCST DATES                                  
*================================================================               
                                                                                
         USING STATABD,R5                                                       
SETUP    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
SETUP2   XC    SVBMKT,SVBMKT                                                    
         SR    R4,R4                                                            
         SR    R6,R6                                                            
         XC    ELEM,ELEM                                                        
         XC    BLOCK(256),BLOCK                                                 
         MVC   SVTCDTS,=X'FFFF0000'                                             
         XC    SEQNUM,SEQNUM                                                    
         MVC   SVCRTBNO,CURTABNO                                                
*                                                                               
SETUP10  LA    R2,ELEM                                                          
         LA    R3,BLOCK                                                         
         SR    R1,R1                                                            
*                                                                               
SETUP12  CLI   0(R2),0                                                          
         BE    SETUP14                                                          
         CLC   STAREV,0(R2)                                                     
         BE    SETUP14                                                          
         LA    R2,3(R2)                                                         
         B     SETUP12                                                          
*                                                                               
SETUP14  MVC   0(1,R2),STAREV                                                   
         ICM   R1,3,1(R2)                                                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,1(R2)                                                       
*                                                                               
SETUP16  GOTO1 DATCON,DMCB,(2,STAINSDT),(3,DUB)                                 
         CLI   0(R3),0                                                          
         BE    SETUP18                                                          
         CLC   DUB(3),0(R3)                                                     
         BE    SETUP18                                                          
         LA    R3,5(R3)                                                         
         B     SETUP16                                                          
*                                                                               
SETUP18  MVC   0(3,R3),DUB          SAVE STAINSDT                               
         SR    R1,R1                                                            
         ICM   R1,3,4(R3)                                                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,4(R3)                                                       
                                                                                
*====================================================                           
* FIND WIDEST SPREAD OF TELECAST DATES                                          
* FOR ALL STATIONS ON AMS LETTER                                                
*====================================================                           
                                                                                
         CLC   SVSTAFTD,STAFTD                                                  
         BNH   *+10                                                             
         MVC   SVSTAFTD,STAFTD                                                  
         CLC   SVSTALTD,STALTD                                                  
         BNL   *+10                                                             
         MVC   SVSTALTD,STALTD                                                  
*                                                                               
         LA    R5,STANEXT                                                       
         CLC   CURTABNO,STAPTR                                                  
         BE    SETUP10                                                          
         DROP  R5                                                               
                                                                                
*===================================================                            
* FIND MOST COMMON REVISION/INSTR DATE BY                                       
* SORTING ON CT FLDS                                                            
*===================================================                            
                                                                                
         LA    R1,ELEM                                                          
         SR    R2,R2                                                            
         OC    0(3,R1),0(R1)                                                    
         BZ    *+12                                                             
         LA    R1,3(R1)                                                         
         BCT   R2,*-14                                                          
         LPR   R2,R2                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SVINSRCT,0                                                       
         CHI   R2,1                                                             
         BE    SETUP20                                                          
         MVI   SVINSRCT,1                                                       
*                                                                               
         GOTO1 VXSORT,DMCB,(C'N',ELEM),(R2),3,2,1                               
*                                                                               
SETUP20  MVC   SVINSREV,ELEM                                                    
*                                                                               
         LA    R1,BLOCK                                                         
         SR    R2,R2                                                            
*                                                                               
SETUP22  OC    0(5,R1),0(R1)                                                    
         BZ    SETUP24                                                          
         LA    R1,5(R1)                                                         
         BCT   R2,SETUP22                                                       
*                                                                               
SETUP24  LPR   R2,R2                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CHI   R2,1                                                             
         BE    SETUP32                                                          
         GOTO1 VXSORT,DMCB,(C'N',BLOCK),(R2),5,2,3                              
*                                                                               
SETUP32  GOTO1 DATCON,DMCB,(3,BLOCK),(2,SVSTAIDT)                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
SETPQ    NTR1  BASE=*,LABEL=*                                                   
                                                                                
SETPQ02  CLI   SVFAXREQ,C'2'      SEE IF DOING FAX REPORT                       
         BNE   SETPQ04                                                          
         TM    WHEN,X'20'          IS THIS SOON                                 
         BZ    SETPQ20                                                          
         B     SETPQ06                                                          
*                                                                               
SETPQ04  CLI   SVFAXREQ,C'Y'      SEE IF DOING FAX REPORT                       
         BNE   SETPQ20                                                          
*                                                                               
SETPQ06  CLI   PQSW,1             SEE IF I MUST OPEN PRINTQ                     
         BNE   SETPQ16            NO - BUT SEND SPCL PRINT LINE(S)              
                                                                                
*================================================================               
*  NEED TO OPENPQ SINCE AUTO OPEN WAS SUPPRESSED IF FAXING                      
*================================================================               
                                                                                
         TM    WHEN,X'20'          IS THIS SOON                                 
         BO    SETPQ10              YES                                         
         MVI   SPMODE,X'FF'        CLOSE PRINT QUE                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
SETPQ10  CLI   OFFLINE,C'Y'                                                     
         BNE   SETPQ12                                                          
*                                                                               
         ICM   RE,15,TWAMASTC                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,MCVREMOT-MASTD(RE)                                            
         USING REMOTED,RF                                                       
         MVC   REMOTSYS,MCS2OSYS-MASTD(RE)                                      
         MVI   REMOTCPY,1                                                       
         MVI   REMOTCLS,C'G'                                                    
         MVC   REMOTJID,=C'COV'                                                 
         MVC   REMOTDST,TWAORIG                                                 
*AR                                                                             
         OC    REMOTTY1,SVFAXARC                                                
*AR                                                                             
*                                                                               
         OC    TWADEST,TWADEST                                                  
         BZ    *+10                                                             
         MVC   REMOTDST,TWADEST                                                 
         DROP  RF                                                               
*                                                                               
SETPQ12  MVC   REMUSER,=C'COV'                                                  
*                                                                               
         LA    R1,ELEM                                                          
         ST    R1,SPOOLQLK                                                      
         USING PQPLD,R1                                                         
*                                                                               
         XC    ELEM(128),ELEM                                                   
*AR                                                                             
         OC    QLTYP1,SVFAXARC                                                  
*AR                                                                             
         MVC   QLDESC(1),QMED                                                   
         MVC   QLDESC+3(3),QCLT                                                 
         MVC   QLDESC+6(3),QPRD                                                 
         MVI   PLCLASS,C'G'        WESTERN UNION                                
         MVI   QLCLASS,C'G'        ALSO                                         
*                                                                               
         MVI   QLEXTRA,X'FF'       NEW PARM LIST                                
         OI    SPOOLIND,X'40'      USER VALUES PRESENT                          
         GOTO1 OPENPQ                                                           
         EJECT                                                                  
*============================================================                   
* SET UP PRINTING ADDRESSABILITY (AFTER PRTQUE OPEN)                            
* MAXLINES IS SET TO 40 AS A FLAG FOR FIRST PAGE ONLY                           
* WILL BE RESET TO 48 IN HDHK                                                   
*============================================================                   
                                                                                
SETPQ14  MVI   MAXLINES,40                                                      
                                                                                
*==================================================================             
* SEE IF FAXING AMS LETTER TO WESTERN UNION                                     
* IF SO - MUST USE CLASS G AND SEND SPECIAL PRINT LINE FIRST                    
*==================================================================             
                                                                                
SETPQ16  MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,2                                                           
*                                                                               
         MVC   EDIORIG,AGYORIG                                                  
         MVC   EDIHDR,=CL5'*HDR*'                                               
*                                                                               
         MVC   EDIDESID(4),=C'FAXW'                                             
*                                                                               
         MVI   EDIWIDE,C'L'                                                     
         MVI   EDIPAGE,C'P'                                                     
*                                                                               
         TM    SVOPT2,OP2MLBOX     SEND TO MAILBOX (DGS)                        
         BZ    SETPQ18                                                          
         MVC   EDIDESID(L'SVMLBOX),SVMLBOX  MAILBOX NUMBER (DROP FAX)           
         MVC   EDIFDEST(16),SVMLBOX                                             
         B     SETPQ18X                                                         
         SPACE                                                                  
SETPQ18  MVC   EDIDESID+5(19),PRDHFAX       PRODUCTION HOUSE FAX NUMBER         
         MVC   EDIFDEST(16),PRDHFAX                                             
*                                                                               
SETPQ18X MVC   EDIBILL(1),QMED      MEDIA                                       
         MVC   EDIBILL+1(3),QCLT    CLIENT                                      
         MVC   EDIBILL+4(3),QPRD    PRODUCT                                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
*                                                                               
         MVC   EDIDDSID(14),=C'++DDS SPCRATRN'                                  
         MVC   EDISTCMD(4),QMED     MED/CLT                                     
         MVC   EDISTCPR,QPRD                                                    
         MVC   EDISTCP2,QPRD2                                                   
         LLC   R0,BEST                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  EDISTCES,DUB                                                     
         GOTO1 DATCON,DMCB,(0,SVQSTART),(X'20',WORK)                            
         GOTO1 DATCON,DMCB,(0,SVQEND),(X'20',WORK+6)                            
         MVC   EDISTCDT,WORK                                                    
         MVC   EDISTCCT,QUESTOR                                                 
         MVC   EDISTCHS,HOUSECD      PRODUCTION HOUSE CODE                      
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
*                                                                               
* MUST RESET AS OPENPQ XC'S                                                     
*                                                                               
SETPQ20  LM    R0,R2,=A(HEADING,HDHK,FTHK) SPECS, HEADHOOK, FOOTHOOK            
         A     R0,SPTR34RR                                                      
         ST    R0,SPECS            STORE FOR CONTROLLER                         
         A     R1,SPTR34RR                                                      
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
         A     R2,SPTR34RR                                                      
         ST    R2,FOOTHOOK         STORE FOR CONTROLLER                         
         MVI   FOOTLNS,5                                                        
         MVI   FOOTSW,0                                                         
         MVI   HEADSW,0                                                         
*                                                                               
         L     R0,=A(MIDHK)                                                     
         A     R0,SPTR34RR                                                      
         ST    R0,MIDHOOK                                                       
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* PRINT LIST OF STATIONS AND COVER LETTER COMMENTS IF ENTERED                   
*================================================================               
                                                                                
PRTSTA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   LINE,99             FORCE HEADS W/O FOOTLINES                    
         MVI   HEADSW,0            SUPPRESS MIDS FOR NOW                        
                                                                                
         LA    R4,P1+2                                                          
         LA    R2,TRACMT1H                                                      
         LA    R5,4                                                             
         LR    RE,R2                                                            
         LR    RF,R5                                                            
*                                                                               
PRTSTA2  CLI   5(RE),0             ANY DATA                                     
         BNE   PRTSTA4              YES                                         
         LLC   R0,0(RE)                                                         
         AR    RE,R0                                                            
         IC    R0,0(RE)                                                         
         AR    RE,R0                                                            
         BCT   RF,PRTSTA2                                                       
         B     PRTST10             NO ENTRIES, BYPASS                           
*                                                                               
PRTSTA4  CLI   5(R2),0             ANY DATA                                     
         BE    PRTSTA6              NO                                          
         CLI   8(R2),C'='          SPECIAL TEXT (PRODUCTION TEXT)               
         BNE   *+12                                                             
         BAS   RE,GPHTXT           GET PRODUCTION HOUSE TEXT                    
         B     PRTST10                                                          
*                                                                               
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,PRTMVC                                                        
*                                                                               
PRTSTA6  LA    R4,132(R4)          NEXT PRINT LINE                              
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R5,PRTSTA4                                                       
*                                                                               
         LA    R5,P1+2                                                          
         CR    R4,R5               ANYTHING TO PRINT                            
         BNH   PRTST10                                                          
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)       GOTO SPOOL RTN                             
         B     PRTST10                                                          
PRTMVC   MVC   0(0,R4),8(R2)                                                    
*                                                                               
PRTST10  MVI   FORCEMID,C'Y'                                                    
         MVI   HEADSW,C'C'         SET PRINTING COVER PAGE                      
                                                                                
* FIND FIRST STATION FOR THIS TABLE                                             
                                                                                
         L     R5,ASTATAB                                                       
         USING STATABD,R5                                                       
         CLC   CURTABNO,STAPTR                                                  
         BE    *+12                                                             
         LA    R5,STANEXT                                                       
         B     *-14                                                             
*                                                                               
         SR    R4,R4               SET STATION CT TO ZERO                       
PRTSTA12 GOTO1 MSUNPK,DMCB,(X'80',STAMS),QMKT,DUB                               
         MVC   QSTA,DUB                                                         
*                                                                               
         CLI   QSTA+4,C' '                                                      
         BH    *+8                                                              
         MVI   QSTA+4,C'T'                                                      
*                                                                               
         LA    R4,1(R4)            BUMP COUNTER                                 
*                                                                               
         XC    STANET,STANET                                                    
         CLC   DUB+5(3),SPACES                                                  
         BE    PRTSTA14                                                         
         MVC   STANET(4),DUB                                                    
         MVI   STANET+4,C'/'                                                    
         MVC   STANET+5(3),DUB+5                                                
*                                                                               
PRTSTA14 CLC   SVBMKT,STAMS        TEST SAME MARKET                             
         BE    PRTSTA30                                                         
*                                                                               
         MVC   SVBMKT,STAMS                                                     
         MVC   P1+3(4),QMKT                                                     
*                                                                               
         CLC   AGENCY,=C'BS'       THIS BACKER                                  
         BNE   PRTSTA22                                                         
*                                                                               
         CLC   QCLT,=C'MT2'        AND CLIENT MT2                               
         BE    PRTSTA20                                                         
         CLC   QCLT,=C'MT3'        AND CLIENT MT3                               
         BE    PRTSTA20                                                         
         CLC   QCLT,=C'MT4'        AND CLIENT MT4                               
         BE    PRTSTA20                                                         
         CLC   QCLT,=C'MT5'        AND CLIENT MT5                               
         BE    PRTSTA20                                                         
         CLC   QCLT,=C'MT6'        AND CLIENT MT6                               
         BNE   PRTSTA22                                                         
*                                                                               
PRTSTA20 L     R6,AIO1                                                          
         USING STARECD,R6                                                       
*                                                                               
         MVI   0(R6),C'0'                                                       
         MVC   1(14,R6),0(R6)                                                   
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,QSTA                                                    
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,QCLT                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(15),0(R6)                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R6)                     
*                                                                               
         CLC   KEY(9),0(R6)                                                     
         JNE   MISTAER                                                          
         MVC   QMKT,SMKT                                                        
         PACK  DUB,QMKT                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,SVBMKT         FORCE CHANGE IN MARKET                       
         DROP  R6                                                               
*                                                                               
PRTSTA22 L     R6,AIO1                                                          
         XC    KEY,KEY                                                          
         USING MKTRECD,R6                                                       
         MVI   0(R6),C'0'                                                       
         MVC   1(14,R6),0(R6)                                                   
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,QMKT                                                     
         MVC   MKTKAGY,AGENCY                                                   
         MVC   KEY(15),0(R6)                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R6)                     
*                                                                               
         MVC   P1+9(20),=C'** UNKNOWN MARKET **'                                
         CLC   KEY(15),0(R6)                                                    
         BNE   *+10                                                             
         MVC   P1+9(20),MKTNAME                                                 
         OC    P1+9(20),SPACES     NEEDED IF FAXING                             
         DROP  R6                                                               
*                                                                               
PRTSTA30 BAS   RE,FMTSTA           FORMAT STATION FOR PRINTING                  
*                                                                               
         MVC   P1+32(7),STAPRNT                                                 
*                                                                               
         OC    STANET,STANET       THIS A CABLE HEAD                            
         BZ    *+10                                                             
         MVC   P1+32(L'STANET),STANET                                           
*                                                                               
         GOTO1 DATCON,DMCB,(2,STAFTD),(5,P1+48)                                 
         GOTO1 (RF),(R1),(2,STALTD),(5,P1+58)                                   
*                                                                               
         EDIT  (B1,STAREV),(3,P+69)                                             
*                                                                               
         CLI   STAREV,0            THIS ORIGINAL                                
         BNE   PRTSTA32            NO                                           
         MVC   P+69(3),=C'ORG'                                                  
*                                                                               
PRTSTA32 GOTO1 SPOOL,DMCB,(R8)       GOTO SPOOL RTN                             
*                                                                               
         LA    R5,STANEXT                                                       
         CLC   CURTABNO,STAPTR                                                  
         BE    PRTSTA12                                                         
*                                                                               
         EDIT  (R4),(4,P1+32)                                                   
         MVC   P1+39(8),=C'STATIONS'                                            
         CHI   R4,1                                                             
         BNE   *+8                                                              
         MVI   P1+46,C' '                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     EXIT                                                             
         EJECT                                                                  
*======================================================                         
* FORMAT STATION FOR PRINTING (EG WABC-FM)                                      
*======================================================                         
                                                                                
FMTSTA   NTR1                                                                   
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),QSTA                                                  
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
*                                                                               
         LA    RF,STAPRNT+3                                                     
         CLI   0(RF),C' '                                                       
         BNE   *+6                                                              
         BCTR  RF,0                                                             
         MVI   1(RF),C'-'                                                       
         MVC   2(1,RF),QSTA+4      MOVE SUB-MEDIA                               
*                                                                               
         MVI   3(RF),C'V'          ASSUME TV                                    
         CLI   QMED,C'T'                                                        
         BE    FMTSTA10                                                         
         MVI   3(RF),C'M'          ASSUME RADIO                                 
         CLI   QMED,C'R'                                                        
         BE    FMTSTA10                                                         
         MVI   3(RF),C' '                                                       
*                                                                               
FMTSTA10 CLI   SVT1PR11,C'Y'       USE STATION CMML TYPE                        
         JNE   EXIT                                                             
*                                                                               
         CLI   SVT2PR02,C'Y'       SUPPRESS COMML TYPE                          
         JE    EXIT                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A28'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(5),QSTA                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   EXIT                                                             
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING STADTAEL,R6                                                      
         CLI   STADTALN,119                                                     
         JE    EXIT                                                             
         MVC   P+76(4),STACMLT                                                  
         J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*============================================================                   
* GET SPECIAL TEXT RECORD (PRODUCTION HOUSE TEXT)                               
*============================================================                   
                                                                                
GPHTXT   NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2D'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVI   KEY+5,C'='                                                       
         MVC   KEY+6(6),9(R2)                                                   
         OC    KEY+6(6),SPACES                                                  
         MVI   KEY+12,C'L'                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         JNE   NOTXTERR                                                         
*                                                                               
         BRAS  RE,PSTXT            PRINT SPECIAL TEXT                           
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==========================================================                     
* READ INST RECAP RECORDS FOR INSTS FOR REQ PERIOD                              
*                                                                               
* REGISTER USAGE                                                                
* R2 - SUBEL POINTER                                                            
* R3 - POINTER WITHIN BLOCK FOR PATTERN TABLE BUILD                             
* R4 - INSTR RECAP KEY                                                          
* R5 - STATION POINTER TABLE                                                    
* R6 - ELEM PTR                                                                 
*                                                                               
*==========================================================                     
                                                                                
RDRECAP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,ASTATAB                                                       
         USING STATABD,R5                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              BUILD KEY FOR READHI                         
         USING INSKEY,R4                                                        
         MVC   INSKID(2),=XL2'0A24'                                             
         MVC   INSKAM(4),BAGYMD    A-M/CLT/PRD                                  
         CLI   BPRD,X'FF'          POL                                          
         BNE   *+8                                                              
         MVI   INSKPRD,0                                                        
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(6),KEY      ANY RECS FOR THIS A-M/CLT/PRD                
         BE    RDRC02                                                           
         CLI   BPRD,X'FF'          POL                                          
         BNE   NOINSER                                                          
         CLC   KEYSAVE(5),KEY      ANY RECS FOR THIS A-M/CLT                    
         BNE   NOINSER                                                          
         B     *+14                                                             
*                                                                               
RDRC02   CLC   KEY(6),KEYSAVE      AT END OF THIS A-M/CLT/PRD                   
         BE    RDRC04              NO                                           
         CLI   BPRD,X'FF'          POL                                          
         BNE   RDRC150             NO, SET UP FOR REPORT                        
         CLC   KEYSAVE(5),KEY      AT END OF THIS A/M, BCLT                     
         BNE   RDRC150             YES, SET UP FOR REPORT                       
                                                                                
* INITIALIZE BLOCK FOR NEW MARKET                                               
                                                                                
RDRC04   L     RE,ABLOCK                                                        
         L     RF,ABLOCKX                                                       
         SR    RF,RE               GIVES LEN                                    
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R7,ABLOCK                                                        
         MVC   0(2,R7),=Y(HDRLEN)                                               
         LA    R3,HDRLEN(R7)                                                    
         USING PTNTABD,R3                                                       
*                                                                               
         MVI   SVINSREV,0                                                       
         MVC   SVSTAPRD,KEY+INSKPRD-INSKEY                                      
         MVC   SVTCDTS,=X'FFFF0000'                                             
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   *+14                                                             
         CLC   BEST,KEY+11         THIS FOR THIS EST                            
         BNE   RDRC100                                                          
*                                                                               
         OC    MKTLIST,MKTLIST    ONLY SELECTIVE MARKETS                        
         BZ    RDRC20                                                           
         LA    R0,10                                                            
         LA    R1,MKTLIST                                                       
*                                                                               
RDRC12   CLC   0(2,R1),KEY+6      MARKET SELECTED                               
         BE    RDRC20                                                           
         BH    RDRC14                                                           
         CLI   2(R1),X'FF'        AT END OF TABLE                               
         BE    RDRC150                                                          
         LA    R1,2(R1)                                                         
         BCT   R0,RDRC12                                                        
         B     RDRC102            NO MORE                                       
                                                                                
* SKIP TO NEXT MARKET IN TABLE *                                                
                                                                                
RDRC14   CLI   0(R1),X'FF'        AT END OF TABLE                               
         BE    RDRC102                                                          
         MVC   KEY(5),KEYSAVE                                                   
         CLI   BPRD,X'FF'          POL                                          
         BE    *+10                 YES, LEAVE WHAT PROD WE FOUND               
         MVC   KEY+5(1),KEYSAVE+5                                               
         MVC   KEY+6(2),0(R1)                                                   
         XC    KEY+8(5),KEY+8                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     RDRC02                                                           
*                                                                               
RDRC20   CLI   QMGRID,C' '         TEST MKTGRPS ACTIVE                          
         BNH   RDRC50                                                           
                                                                                
*===============================================================                
* TEST IF MARKET IN REQUESTED MKTGRP                                            
*===============================================================                
                                                                                
         BRAS  RE,GETMKA           CHECK MKT ASSIGNMENT REC                     
         BE    RDRC50              MKT IN REQUESTED GROUP                       
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,KEY+6          BUMP TO NEXT MARKET                          
         LA    RE,1(RE)                                                         
         STCM  RE,3,KEY+6                                                       
         XC    KEY+8(5),KEY+8                                                   
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     RDRC02                                                           
         EJECT                                                                  
*===========================================================                    
* BUILD ALL PATTERNS FOR THIS STATION WITHIN PERIOD                             
*===========================================================                    
                                                                                
RDRC50   L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING INSKEY,R4                                                        
         MVC   BMKTSTA,INSKMKT                                                  
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   RDRC100                                                          
         USING INSDTAEL,R6                                                      
                                                                                
*=================================================================              
* BYPASS EITHER COPY CODE=EST INSTR OR COPY CODE NOT EST INSTR                  
*=================================================================              
                                                                                
RDRC60   MVC   MYKEYSV,KEY         SAVE CURRENT KEY (SO CAN READ PTTN)          
         ST    R6,MYLASTEL         AND ELEMENT ADDRESS                          
         SR    R2,R2               CLEAR SUBEL POINTER                          
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODED EST ON                            
         BNE   RDRC61                                                           
         TM    INSFLAG,INSFLEST    IF COPY CODE = EST                           
         BO    RDRC62               USE IT                                      
         B     RDRC90                                                           
*                                                                               
RDRC61   TM    INSFLAG,INSFLEST    IF NOT COPY CODE = EST                       
         BO    RDRC90               BYPASS                                      
*                                                                               
RDRC62   CLI   BPRD2,0                                                          
         BE    RDRC64                                                           
         CLI   BPRD2,X'FF'         PARTNER NONE                                 
         BNE   RDRC63                                                           
         CLI   INSPRD2,0                                                        
         BNE   RDRC90                                                           
         B     RDRC66                                                           
*                                                                               
RDRC63   CLC   BPRD2,INSPRD2       REQUESTED PARTNER                            
         BNE   RDRC90                                                           
*                                                                               
RDRC64   TM    INSFLAG,INSFLCGR    THIS AMS GROUP CODE                          
         BZ    RDRC65                                                           
         CLI   SVTCPR04,C'Y'       SHOW CABLE STA                               
         BNE   RDRC90              NO                                           
         TM    INSFLAG,INSFLMAR    DID AMS RUN                                  
         BZ    RDRC90              NO,BYPASS                                    
*                                                                               
RDRC65   TM    SVOPT,OPTRERUN      THIS RERUN                                   
         BO    *+12                                                             
         TM    INSFLAG,INSFLCOV    WERE COVER LETTERS ALREADY RUN               
         BO    RDRC90               BYPASS                                      
*                                                                               
RDRC66   LA    R2,INSPTTN                                                       
         LLC   R0,INSDTALN                                                      
         AHI   R0,-INSBSCEL                                                     
*                                                                               
RDRC68   CLC   SVFLTSTP,5(R2)      FLIGHT START TO LTD                          
         BH    RDRC92              NOT IN FLIGHT                                
         CLC   SVFLTEDP,3(R2)      FLIGHT END TO FTD                            
         BL    RDRC92              NOT IN FLIGHT                                
         OI    TABSW,FOUNDENT      FOUND ENTRY                                  
         EJECT                                                                  
*====================================================                           
* BUILD PATTERN ENTRY                                                           
*====================================================                           
                                                                                
RDRC70   MVC   SVINSREV,INSREV                                                  
         MVC   SVSTAIDT,INSDATE                                                 
         MVC   PTNBPRD(2),INSPRD1                                               
         LA    R1,PTNBPRD                                                       
         BRAS  RE,GETPRD                                                        
*                                                                               
         CLI   INSPRD2,0                                                        
         BE    RDRC80                                                           
*                                                                               
         MVC   PTNBPRD2,INSPRD2                                                 
         MVC   PTNPRD2,0(RF)                                                    
         MVC   PTNSLN2,INSSLN2                                                  
         LA    R1,PTNBPRD2                                                      
         BRAS  RE,GETPRD                                                        
*                                                                               
RDRC80   MVC   PTNREF,0(R2)        PATTERN REF/SUB                              
         MVC   PTNFTD,3(R2)                                                     
         MVC   PTNLTD,5(R2)                                                     
         MVC   PTNCOPY,INSKCOPY                                                 
                                                                                
*=========================================================                      
* FIND FIRST/LAST TELECAST DATES FOR THIS STATION                               
*=========================================================                      
                                                                                
         CLC   SVSTAFTD,3(R2)                                                   
         BNH   *+10                                                             
         MVC   SVSTAFTD,3(R2)                                                   
*                                                                               
         CLC   SVSTALTD,5(R2)                                                   
         BNL   *+10                                                             
         MVC   SVSTALTD,5(R2)                                                   
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(2,SVSTAFTD),(3,WORK+16)                             
         GOTO1 (RF),(R1),(2,SVSTALTD),(3,WORK+19)                               
                                                                                
* NOW WE NEED TO READ THE PATTERN REC TO GET TIME AND DAYPART                   
                                                                                
         MVC   KEY(2),=X'0A22'     READ PATTERN RECORD                          
         MVC   KEY+5(2),PTNBPRD    PRD/SLN                                      
         MVC   KEY+7(1),PTNBPRD2   PRD2                                         
         MVC   KEY+8(1),PTNSLN2    SLN2                                         
         MVC   KEY+9(1),PTNCOPY    COPY CODE                                    
         MVC   KEY+10(3),PTNREF    REF/SUB                                      
*                                                                               
         OC    PTNREF,PTNREF       TEST HIATUS                                  
         BZ    RDRC88                                                           
*                                                                               
         CLC   =X'FFFFFF',PTNREF   THIS A TBA PATTERN                           
         BNE   RDRC81                                                           
         CLI   SVT1PR3,C'S'        SUPPRESS TBA'S?                              
         BE    RDRC90               YES                                         
         B     RDRC88                                                           
*                                                                               
RDRC81   GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO2             USE I/O 1                                    
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
                                                                                
* FIND A PATTERN ELEM THAT OVERLAPS THE PERIOD                                  
                                                                                
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
*                                                                               
RDRC82   CLC   WORK+16(3),PATEND     INST START AFTER PATTERN END               
         BH    RDRC84                                                           
         CLC   WORK+19(3),PATSTART   INST END BEFORE PATTERN START              
         BL    RDRC84                                                           
         B     RDRC86                USE THIS ELEMENT                           
*                                                                               
RDRC84   BRAS  RE,NEXTEL                                                        
         BE    RDRC82                                                           
         DC    H'0'                                                             
                                                                                
RDRC86   MVC   PTNDPT,PATDPT                                                    
         OC    PATSTIM,PATSTIM       TEST START TIME PRESENT                    
         BZ    RDRC88                                                           
         MVC   PTNSTIM(4),PATSTIM    MOVE START/END TIME                        
         TM    PATSTAT1,PATSDLY                                                 
         BZ    *+8                                                              
         OI    PTNFLAG1,PTNFDLY                                                 
*                                                                               
*                                                                               
RDRC88   LH    R1,0(R7)                                                         
         LA    R1,L'PTNENT(R1)                                                  
         STH   R1,0(R7)                                                         
         LA    R3,PTNEXT                                                        
*                                                                               
RDRC90   MVC   KEY,MYKEYSV         REREAD LAST 0A24 KEY                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RDRC92   L     R6,AIO1                                                          
         ST    R6,AIO              RESTORE AIO                                  
*                                                                               
         L     R6,MYLASTEL         RESTORE ELEM POINTER IN RCP RECAP            
*                                                                               
RDRC96   LTR   R2,R2               TEST SUBEL IN PROCESS                        
         BZ    RDRC98              NO                                           
         LA    R2,INSSUBEL(R2)                                                  
         AHI   R0,-INSSUBEL                                                     
         BP    RDRC68                                                           
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RDRC98   BRAS  RE,NEXTEL                                                        
         BE    RDRC60                                                           
*                                                                               
RDRC100  LA    R4,KEY                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
         CLC   =X'0A24',KEY        RAN OUT OF INST REC                          
         BNE   RDRC102                                                          
*                                                                               
         CLC   INSKAM(3),BAGYMD    SAME MEDIA/AGENCY & BCLT                     
         BNE   RDRC102                                                          
*                                                                               
         CLI   BPRD,X'FF'          POL                                          
         BE    *+14                 ALL PRODUCTS OK                             
         CLC   INSKPRD,BPRD        SAME PRODUCT                                 
         BNE   RDRC100                                                          
*                                                                               
         CLC   KEY(11),KEYSAVE     ALL SAME BUT COPY CODE                       
         BNE   RDRC102                                                          
*                                                                               
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BE    RDRC100              NO CABLE                                    
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODED EST ON                            
         BNE   RDRC50                                                           
         CLC   BEST,KEY+11         ONLY THIS EST                                
         BE    RDRC50                                                           
         B     RDRC100                                                          
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
RDRC102 CLC    =AL2(HDRLEN),0(R7)   ANY ENTRIES                                 
         BE    RDRC02               NO                                          
         EJECT                                                                  
         SR    R0,R0                                                            
         ICM   R0,3,0(R7)                                                       
         AHI   R0,-HDRLEN                                                       
         SRDL  R0,32                                                            
         LHI   RE,L'PTNENT                                                      
         DR    R0,RE                                                            
         LR    R0,R1               SAVE NUMBER OF ENTRIES                       
                                                                                
*==========================================================                     
* SORT BY DATES                                                                 
*==========================================================                     
                                                                                
         GOTO1 VXSORT,DMCB,HDRLEN(R7),(R0),L'PTNENT,4,                 C        
               PTNFTD-PTNENT                                                    
                                                                                
*==========================================================                     
* SORT BY PRD/SLN/PRD2/SLN2                                                     
*==========================================================                     
                                                                                
         GOTO1 (RF),(R1),,,,L'PTNSRT,0                                          
                                                                                
*==========================================================                     
* NOW CHECK AGAINST EXISTING TABLE                                              
*==========================================================                     
                                                                                
         MVC   CURTABNO,=H'1'                                                   
         L     R2,APATTAB                                                       
         OC    0(2,R2),0(R2)       ANY ENTRIES                                  
         BZ    RDRC130              NO                                          
*                                                                               
RDRC112 CLC    0(HDRLEN,R7),0(R2) SAME SIZE PATTERN TABLE/MKTGRP                
         BNE   RDRC128                                                          
         LA    R1,HDRLEN(R7)                                                    
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         SRDL  RE,32                                                            
         LA    R0,L'PTNENT                                                      
         DR    RE,R0                                                            
         LR    RE,RF               SAVE NUMBER OF ENTRIES                       
         LA    RF,HDRLEN(R2)                                                    
         XC    LSTPTNED,LSTPTNED                                                
         NI    TABSW,X'FF'-CHAENT                                               
         EJECT                                                                  
*======================================================                         
*  R1 - PTR TO NEW PATTERNS IN 0(R7)                                            
*  R2 - PTR TO CURRENT PATTERN ENTRY IN TABLE                                   
*  RE - CT OF PATTERNS IN THIS ENTRY                                            
*  RF - PTR TO PATTERN IN EXISTING PATTERN ENTRY                                
*======================================================                         
                                                                                
RDRC120 CHI    RE,1                THIS LAST PATTERN                            
         BNH   *+14                 YES                                         
         CLC   PTNEXT-PTNENT(L'PTNSRT,R1),0(R1) NXT PTTN DIFF PRD/SLN           
         BE    *+14                              NO, USE DATES                  
         MVC   NXTPTNST,=X'FFFF'                                                
         B     RDRC122                                                          
*                                                                               
         MVC   NXTPTNST,PTNFTDN-PTNENT(RF) NEXT PTTN START DATE                 
         CLC   PTNFTDN-PTNENT(,R1),PTNFTDN-PTNENT(RF)                           
         BNL   *+10                                                             
         MVC   NXTPTNST,PTNFTDN-PTNENT(R1)                                      
*                                                                               
RDRC122 CLC    0(9,R1),0(RF)       SAME PRD/SLN/PRD2/SLN2 REF/SUB               
         BNE   RDRC128              NO, LOOK AT NEXT TABLE                      
         CLC   PTNCOPY-PTNENT(1,R1),PTNCOPY-PTNENT(RF) SAME COPY CODE           
         BNE   RDRC128              NO, LOOK AT NEXT TABLE                      
         CLC   PTNFTD-PTNENT(,R1),PTNFTD-PTNENT(RF) CK START TO START           
         BNL   RDRC124                                                          
         CLC   LSTPTNED,PTNFTD-PTNENT(R1) CK LAST PTTN END TO START             
         BNL   RDRC128                                                          
         OI    TABSW,CHAENT                                                     
*                                                                               
RDRC124 CLC    PTNLTD-PTNENT(,R1),PTNLTD-PTNENT(RF) SAME END DATES              
         BNH   RDRC126                                                          
         CLC   NXTPTNST,PTNLTD-PTNENT(R1) CK NEXT PTTN START TO END             
         BNH   RDRC128                                                          
         OI    TABSW,CHAENT                                                     
RDRC126 MVC    LSTPTNED,PTNLTD-PTNENT(R1)                                       
         CLC   PTNLTD-PTNENT(,R1),PTNLTD-PTNENT(RF)                             
         BNL   *+10                                                             
         MVC   LSTPTNED,PTNLTD-PTNENT(RF)                                       
         CLC   PTNEXT-PTNENT(L'PTNSRT,R1),0(R1) NEXT PTTN DIFF PRD/SLN          
         BE    *+10                              SAME                           
         XC    LSTPTNED,LSTPTNED   RESET LAST PTTN END DATE                     
         LA    R1,PTNEXT-PTNENT(R1)                                             
         LA    RF,PTNEXT-PTNENT(RF)                                             
         BCT   RE,RDRC120                                                       
         B     RDRC132                                                          
*                                                                               
RDRC128  SR    R0,R0                                                            
         ICM   R0,3,0(R2)                                                       
         AR    R2,R0                                                            
         SR    RF,RF                                                            
         ICM   RF,3,CURTABNO                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,CURTABNO                                                    
         OC    0(2,R2),0(R2)       AT END OF TABLE                              
         BNZ   RDRC112             NO                                           
                                                                                
* MOVE TABLE IN BLOCK TO NEXT EMPTY SLOT *                                      
                                                                                
RDRC130  SR    R1,R1                                                            
         ICM   R1,3,0(R7)          GET TABLE LEN                                
                                                                                
* SEE IF ROOM FOR THIS STATION PATTERN TABLE *                                  
                                                                                
         LA    RE,HDRLEN(R1,R2)         END OF THIS ENTRY + 6                   
*                                                                               
         C     RE,APATTABX         END OF PTTN TABLE                            
         BNL   PTNSIZER                                                         
*                                                                               
         L     R0,ABLOCK                                                        
         LR    RE,R2                                                            
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         XC    0(L'PTNSRT,RE),0(RE)                                             
         B     RDRC144                                                          
*                                                                               
* BUILD STATION TABLE ENTRY                                                     
*                                                                               
RDRC132 TM     TABSW,CHAENT                                                     
         BZ    RDRC144                                                          
         LA    R1,HDRLEN(R7)                                                    
         SR    RE,RE                                                            
         ICM   RE,3,0(R2)                                                       
         SRDL  RE,32                                                            
         LA    R0,L'PTNENT                                                      
         DR    RE,R0                                                            
         LR    RE,RF               SET RE FOR NUMBER OF ENTRIES                 
         LA    RF,HDRLEN(R2)                                                    
         XC    LSTPTNED,LSTPTNED                                                
*                                                                               
RDRC134 CHI    RE,1                THIS LAST PATTERN                            
         BNH   *+14                 YES                                         
         CLC   PTNEXT-PTNENT(L'PTNSRT,R1),0(R1) NXT PTTN SAME PRD/SLN           
         BE    *+14                              YES, USE DATES                 
         MVC   NXTPTNST,=3X'FF'                                                 
         B     RDRC138                                                          
         MVC   NXTPTNST,PTNFTDN-PTNENT(RF)     NEXT PATTERN START DATE          
         CLC   PTNFTDN-PTNENT(,R1),PTNFTDN-PTNENT(RF)                           
         BNL   *+10                                                             
         MVC   NXTPTNST,PTNFTDN-PTNENT(R1)                                      
*                                                                               
RDRC138 CLC    0(9,R1),0(RF)       SAME PRD/SLN/PRD2/SLN2 REF/SUB               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PTNCOPY-PTNENT(1,R1),PTNCOPY-PTNENT(RF) SAME COPY CODE           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PTNFTD-PTNENT(,R1),PTNFTD-PTNENT(RF) CK START TO START           
         BNL   RDRC140                                                          
         CLC   LSTPTNED,PTNFTD-PTNENT(R1) CK LST PTN END TO THIS START          
         BL    *+6                         OK                                   
         DC    H'0'                                                             
         MVC   PTNFTD-PTNENT(,RF),PTNFTD-PTNENT(R1)                             
         NI    TABSW,X'FF'-CHAENT                                               
*                                                                               
RDRC140 CLC    PTNLTD-PTNENT(,R1),PTNLTD-PTNENT(RF) SAME END DATES              
         BNH   RDRC142                                                          
         CLC   NXTPTNST,PTNLTD-PTNENT(R1) CK NEXT PTN STRT TO THIS END          
         BH    *+6                         OK                                   
         DC    H'0'                                                             
         MVC   PTNLTD-PTNENT(,RF),PTNLTD-PTNENT(R1)                             
         NI    TABSW,X'FF'-CHAENT                                               
*                                                                               
RDRC142 MVC    LSTPTNED,PTNLTD-PTNENT(R1)                                       
         CLC   PTNLTD-PTNENT(,R1),PTNLTD-PTNENT(RF)                             
         BNL   *+10                                                             
         MVC   LSTPTNED,PTNLTD-PTNENT(RF)                                       
         CLC   PTNEXT-PTNENT(L'PTNSRT,R1),0(R1) NEXT PTTN DIFF PRD/SLN          
         BE    *+10                       SAME                                  
         XC    LSTPTNED,LSTPTNED   RESET LAST PTTN END DATE                     
         LA    R1,L'PTNENT(R1)                                                  
         LA    RF,L'PTNENT(RF)                                                  
         BCT   RE,RDRC134                                                       
         TM    TABSW,CHAENT        MUST HAVE SET OFF CHANGE                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* BUILD STATION TABLE ENTRY                                                     
*                                                                               
RDRC144 BRAS   RE,BLDST            GO BUILD STATION TABLE ENTRY                 
         B     RDRC02                                                           
         DROP  R3,R5                                                            
         EJECT                                                                  
*=======================================================                        
* SEE IF ANY INSTRUCTIONS TO PRINT                                              
*=======================================================                        
                                                                                
RDRC150 L      R1,ASTATAB                                                       
         OC    0(L'STAENT,R1),0(R1)      END OF TABLE                           
         BZ    NOINSER                                                          
*                                                                               
         BRAS  RE,RDHOUSE          READ PRODUCTION HOUSE RECORD                 
*                                                                               
         BRAS  RE,GETFOOT          GET FOOTNOTE COMMENTS                        
*                                                                               
RDRC160 L      R1,ASTATAB                                                       
         SR    R2,R2                                                            
*                                                                               
RDRC162 OC     0(L'STAENT,R1),0(R1)      END OF TABLE                           
         BZ    RDRC164                                                          
         LA    R1,L'STAENT(R1)                                                  
         BCT   R2,RDRC162                                                       
*                                                                               
RDRC164 LTR    R2,R2                                                            
         BZ    NOINSER                                                          
         LPR   R2,R2                                                            
*                                                                               
         L     R5,ASTATAB                                                       
         USING STATABD,R5                                                       
         GOTO1 VXSORT,DMCB,(R5),(R2),L'STAENT,7,0                               
         MVC   CURTABNO,=H'1'                                                   
         XC    QMKT,QMKT                                                        
         J     EXIT                                                             
*                                                                               
PTNSIZER L     R1,=A(PTNSIZMS)                                                  
         B     LERREXIT                                                         
*                                                                               
NOINSER  TM    WHEN,X'20'          IS THIS A SOON RUN                           
         BZ    NOINSERA             NO                                          
*                                                                               
* CALL CONTROLLER FOR LOCKET TO UNLOCK INST RECAP RECS                          
* FOR THIS MEDIA AND CLIENT                                                     
*                                                                               
         TM    SVOPT,OPTTEST      THIS TEST MODE                                
         BO    NOINSERA             YES                                         
*                                                                               
         MVC   DUB,=X'E4010A2400000000' U=UNLK/01=1 ENTRY/0A24 RECS             
         GOTO1 VALILOC,0                                                        
*                                                                               
NOINSERA L     R1,=A(NOINSMSA)                                                  
         LA    R2,TRAMEDH                                                       
         TM    TABSW,FOUNDENT     ANY DATA FOUND                                
         BZ    LERREXIT                                                         
         L     R1,=A(NOINSMSB)                                                  
*                                                                               
LERREXIT A     R1,SPTR34RR                                                      
         MVC   CONHEAD(8),=C'* NOTE *'                                          
         MVC   CONHEAD+9(42),0(R1)                                              
*                                                                               
         LA    R1,CONHEAD+10       LEAVE FIRST CHAR UC                          
         LA    R0,41                                                            
LERREX2  CLI   0(R1),X'C1'         TEST ALPHA                                   
         BL    *+8                                                              
         XI    0(R1),X'40'                                                      
         LA    R1,1(R1)                                                         
         BCT   R0,LERREX2                                                       
         GOTO1 ERREX2                                                           
         LTORG                                                                  
PTNSIZMS DC    CL42'TOO MANY PATTERNS FOR ONLINE, RUN OFFLINE *'                
NOINSMSA DC    CL42'NO INST RUN FOR MED/CLT/PRD/PERIOD *'                       
NOINSMSB DC    CL42'NO INST RUN SINCE LAST COV/GEN *'                           
         EJECT                                                                  
*===============================================================                
* INITIALIZE STORAGE                                                            
* STORAGE    SIZE      ONLINE        OFFLINE                                    
*                                                                               
* BLOCK       5300      TWA            TWA                                      
* PRTBUFF     4K        TWA            TWA                                      
* SVCMLS      8K        T2168C         DUMMY                                    
* PRDTAB      12K       T2168C          ***                                     
* SVADDL      2K        T2168C         DUMMY                                    
* PATTAB      6K        AIO3           DUMMY                                    
* STATAB      1K        SYSD           DUMMY                                    
* PRDTAB      50200     ***            DUMMY                                    
*===============================================================                
                                                                                
SETSTOR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R0,=V(XSORT)                                                     
         A     R0,SPTR34RR                                                      
         ST    R0,VXSORT                                                        
*                                                                               
         LHI   R1,STADRTAB-T216FFD                                              
         AR    R1,RA                    RA POINTS TO TWA                        
         MVC   0(8,R1),=C'**BLOCK*'     NOTE BLOCK IN TWA                       
         LA    R1,8(R1)                                                         
         ST    R1,ABLOCK                                                        
         AHI   R1,ENDUNTBL-ENDSTADR                                             
         ST    R1,ABLOCKX                                                       
                                                                                
         LR    R0,R9               POINT TO SYSD                                
         AHI   R0,FOOTCMT-SYSD                                                  
         ST    R0,ASVFTNT                                                       
         LHI   R1,FOOTCMTX-FOOTCMT                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LR    R0,R9               POINT TO SYSD                                
         AHI   R0,MGRTAB-SYSD                                                   
         ST    R0,AMGRTAB                                                       
         LR    R1,R9                                                            
         AHI   R1,MGRTABX-SYSD                                                  
         ST    R1,AMGRTABX                                                      
         LHI   R1,MGRTABX-MGRTAB   GIVES LEN OF MGRTAB                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
*==============================================================                 
* USE T2168C TO HOLD COMMERCIAL DATA (ONLINE ONLY)                              
*==============================================================                 
                                                                                
         CLI   OFFLINE,C'Y'                                                     
         BE    SETST22                                                          
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB,X'8C'          SET OVERLAY NUMBER                           
         GOTO1 CALLOV,DMCB,,ATWA                                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,0(R1)                                                         
         LA    RE,16(RE)            DO NOT OVERWRITE OVLY/LEN                   
         MVC   0(8,RE),=CL8'*PRTBUFF'                                           
         LA    RE,8(RE)                                                         
         ST    RE,APRTBUFF                                                      
         AHI   RE,4000                                                          
*                                                                               
         MVC   0(8,RE),=CL8'*SVADDL'                                            
         LA    RE,8(RE)                                                         
         ST    RE,ASVADDL                                                       
         AHI   RE,2048                                                          
         ST    RE,ASVADDLX                                                      
*                                                                               
         MVC   0(8,RE),=CL8'*SVCMLS*'  (PHASE LEN IS AT +8)                     
         LA    RE,8(RE)                                                         
         ST    RE,ASVCMLS          AND ALLOCATE SVCML AREA                      
         AHI   RE,8*1024                                                        
         ST    RE,ASVCMLSX                                                      
*                                                                               
         MVC   0(8,RE),=CL8'*SVPRDS*'  (PHASE LEN IS AT +8)                     
         LA    RE,8(RE)                                                         
         ST    RE,ASVPRDS          AND ALLOCATE SVPRD AREA                      
         AHI   RE,1024                                                          
         ST    RE,ASVPRDSX                                                      
*                                                                               
         MVC   0(8,RE),=CL8'*PRDTAB*'                                           
         LA    RE,8(RE)                                                         
         ST    RE,APRDTAB                                                       
         L     RE,0(R1)            GET ADDRESS OF T2168C                        
         LA    RE,0(RE)            CLEAR HOB                                    
         A     RE,8(RE)            ADD LENGTH AT +8 IN PHASE                    
         ST    RE,APRDTABX         SET EOT ADDR                                 
*                                                                               
         L     R1,AIO3                                                          
         ST    R1,APATTAB                                                       
         AHI   R1,6000-L'PTNENT                                                 
         ST    R1,APATTABX                                                      
*                                                                               
         LA    R1,STATAB                                                        
         ST    R1,ASTATAB                                                       
         LA    R1,L'STATAB-L'STAENT(R1)                                         
         ST    R1,ASTATABX                                                      
         B     SETST24                                                          
                                                                                
*====================================================                           
* OFFLINE STORAGE ALLOCATIONS                                                   
*====================================================                           
                                                                                
SETST22  L     R1,VADUMMY                                                       
         MVC   0(8,R1),=CL8'*STATAB*'                                           
         LA    R1,8(R1)                                                         
         ST    R1,ASTATAB                                                       
         AHI   R1,21760           ROOM FOR 1360 STATIONS                        
         LR    R0,R1                                                            
         AHI   R0,L'STAENT                                                      
         ST    R0,ASTATABX                                                      
*                                                                               
         MVC   0(8,R1),=CL8'*PTNTBL*'                                           
         LA    R1,8(R1)                                                         
         ST    R1,APATTAB                                                       
         AH    R1,=AL2(30000-L'PTNENT)                                          
         ST    R1,APATTABX                                                      
*                                                                               
         LA    R1,L'PTNENT(R1)                                                  
         MVC   0(8,R1),=CL8'*PRDTAB*'                                           
         LA    R1,8(R1)                                                         
         ST    R1,APRDTAB                                                       
         A     R1,=F'51200'        50 ENTRIES (50*1024)                         
         ST    R1,APRDTABX                                                      
*                                                                               
         MVC   0(8,R1),=C'*PRTBUFF'                                             
         LA    R1,8(R1)                                                         
         ST    R1,APRTBUFF                                                      
         AHI   R1,4000                                                          
*                                                                               
         MVC   0(8,R1),=CL8'*SVCMLS*'                                           
         LA    R1,8(R1)                                                         
         ST    R1,ASVCMLS                                                       
         AHI   R1,24000                                                         
         ST    R1,ASVCMLSX                                                      
*                                                                               
         MVC   0(8,R1),=CL8'*SVADDL*'                                           
         LA    R1,8(R1)                                                         
         ST    R1,ASVADDL                                                       
         AHI   R1,2048                                                          
         ST    R1,ASVADDLX                                                      
*                                                                               
         MVC   0(8,R1),=CL8'*SVPRDS*'  (PHASE LEN IS AT +8)                     
         LA    R1,8(R1)                                                         
         ST    R1,ASVPRDS          AND ALLOCATE SVCML AREA                      
         AHI   R1,1024                                                          
         ST    R1,ASVPRDSX                                                      
                                                                                
SETST24  L     RE,APATTAB          CLEAR PATTERN TABLE                          
         L     RF,APATTABX                                                      
         SR    RF,RE                                                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,ASTATAB          CLEAR STATION TABLE                          
         L     RF,ASTATABX                                                      
         SR    RF,RE                                                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,APRDTAB          CLEAR PRODUCT TABLE                          
         L     RF,APRDTABX                                                      
         SR    RF,RE               GIVES TABLE LEN                              
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,ASVCMLS          CLEAR COMMERCIAL TABLE                       
         L     RF,ASVCMLSX                                                      
         SR    RF,RE                                                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,ASVPRDS          CLEAR PRDNAME TABLE                          
         L     RF,ASVPRDSX                                                      
         SR    RF,RE                                                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,ASVPRDS          PUT PRODUCT NAMES IN TABLE                   
         USING SVPRDD,RE                                                        
         CLI   BPRD,X'FF'                                                       
         BE    SETSTX                                                           
         MVC   SVPRDCD,BPRD                                                     
         MVC   SVPRDEBC,QPRD                                                    
         MVC   SVPRDNM,PRDNM                                                    
         CLI   BPRD2,0                                                          
         BE    SETSTX                                                           
         LA    RE,L'SVPRDDTA(RE)                                                
         MVC   SVPRDCD,BPRD2                                                    
         MVC   SVPRDEBC,QPRD2                                                   
         MVC   SVPRDNM,PRD2NM                                                   
         DROP  RE                                                               
*                                                                               
SETSTX   BRAS  RE,CLRBUFF          CLEAR PRINT BUFFER                           
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* SET PERCENTAGES IN COMMERCIAL BUFFER                                          
* PATTERN RECORD IS IN IO1                                                      
*==============================================================                 
                                                                                
         USING SVCMLD,R2                                                        
SETPCTS  NTR1  BASE=*,LABEL=*                                                   
         MVI   ELCODE,X'32'        FIRST SEE IF ONLY ONE CMML                   
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BNE   SETPCT2                                                          
         CLI   1(R6),3             IF ONLY 1 CMML                               
         BNE   SETPCT2             THEN FORCE 100 PCT ELEMENT                   
         MVI   ELEM,X'34'                                                       
         MVI   ELEM+1,5                                                         
         MVI   ELEM+2,C'A'                                                      
         MVC   ELEM+3(2),=H'100'                                                
         B     SETPCT6                                                          
*                                                                               
SETPCT2  MVI   ELCODE,X'36'        LOOK FOR A PERCENTAGE ELEMENT                
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BE    SETPCT4                                                          
*                                                                               
         MVI   ELCODE,X'34'                                                     
         L     R6,AIO1                                                          
         BRAS  RE,GETEL                                                         
         BE    SETPCT4                                                          
         MVI   DOPCT,C'N'                                                       
         J     EXIT                                                             
*                                                                               
SETPCT4  MVC   ELEM,0(R6)          SAVE PERCENTAGE ELEMENT                      
*                                                                               
SETPCT6  L     R2,ASVCMLS                                                       
         USING SVCMLD,R2                                                        
         OC    0(8,R2),0(R2)                                                    
         BNZ   SETPCT10                                                         
         MVI   DOPCT,C'N'                                                       
         J     EXIT                                                             
*                                                                               
SETPCT10 LA    R6,ELEM                                                          
         LLC   R0,1(R6)                                                         
         AHI   R0,-2                                                            
         SRDL  R0,32                                                            
         D     R0,=F'3'            GET NUMBER OF ENTRIES                        
         LR    R0,R1                                                            
         LA    R6,2(R6)                                                         
*                                                                               
SETPCT12 MVC   SVCMLPCT,1(R6)                                                   
*                                                                               
         LA    R2,SVCMLNXT                                                      
         CLI   SVCMLPIG,2          SKIP PIGGYBACK ENTRY                         
         BE    *-8                                                              
         LA    R6,3(R6)                                                         
         BCT   R0,SETPCT12                                                      
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* PRINT COMMERCIAL TEXT AND ANY OTHER ADDITIONAL DATA                           
*===================================================================            
                                                                                
PRTADDL  NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         L     R2,ASVADDL                                                       
         USING SVADDLD,R2                                                       
*                                                                               
         MVI   HEADSW,C'A'         SET MIDLINES PRINTING                        
         MVI   FORCEMID,C'Y'                                                    
         MVI   CONTINUE,C'N'                                                    
                                                                                
*=============================================================                  
* READ COMMERCIAL TEXT RECORD                                                   
*=============================================================                  
                                                                                
PRTA40   LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
*                                                                               
         OC    SVADDSEQ,SVADDSEQ    TEST TEXT FOR THIS CMML                     
         BZ    PRTA44                                                           
*                                                                               
         USING CMTKEY,R4                                                        
         MVC   CMTKID,=X'0A35'                                                  
         MVC   CMTKAM(3),BAGYMD     A-M/CLT                                     
         MVC   CMTKPRD,SVADDPRD     PRD                                         
         MVC   CMTKSEQ,SVADDSEQ     SEQ                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRTA42                                                           
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVI   CMTKPRD,0                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PRTA42                                                           
         XC    KEY,KEY             INDICATE NO TEXT FOUND                       
         B     PRTA44                                                           
*                                                                               
PRTA42   L     R6,AIO2             USE IO2                                      
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
PRTA44   BRAS  RE,CLRBUFF                                                       
*                                                                               
         L     R4,APRTBUFF                                                      
         USING PLINED,R4                                                        
*                                                                               
         MVI   ALLOWLIN,0          CLEAR LINE COUNTER                           
*                                                                               
         CLI   SVT2PR02,C'Y'       TEST TO SUPPRESS TYPE                        
         BE    PRTA46                                                           
         CLI   SVADDTYP,C' '                                                    
         BNH   PRTA46                                                           
         MVC   15(5,R4),=C'TYPE='                                               
         MVC   20(4,R4),SVADDTYP                                                
         LA    R4,132(R4)                                                       
         IC    R0,ALLOWLIN                                                      
         AHI   R0,1                                                             
         STC   R0,ALLOWLIN                                                      
*                                                                               
PRTA46   DS    0H                                                               
         CLI   KEY,0               TEST ANY TEXT THIS CMML                      
         BE    PRTA50                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BNE   PRTA50                                                           
*                                                                               
PRTA48   LLC   RE,1(R6)                                                         
         AHI   RE,-4                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   15(0,R4),3(R6)      DO NOT MOVE COMMENT NUMBER                   
         CLC   0(132,R4),SPACES                                                 
         BNE   *+8                                                              
         MVI   0(R4),0             FORCE LINE TO PRINT                          
         LA    R4,132(R4)                                                       
         IC    R0,ALLOWLIN                                                      
         AHI   R0,1                                                             
         STC   R0,ALLOWLIN                                                      
         BRAS  RE,NEXTEL                                                        
         BE    PRTA48                                                           
*                                                                               
PRTA50   CLI   SVADDHSE,C' '       TEST ANY PROD HOUSE                          
         BH    PRTA56                                                           
         CLI   SVADDDDT,0          TEST ANY DESTROY DATE                        
         BE    PRTA58              NOS                                          
*                                                                               
PRTA56   IC    R0,ALLOWLIN         THEN NEED ONE MORE LINE                      
         AHI   R0,1                                                             
         STC   R0,ALLOWLIN                                                      
         LA    R4,15(R4)                                                        
*                                                                               
PRTA58   CLI   SVADDHSE,C' '       ANY PROD HOUSE                               
         BNH   PRTA60                                                           
         MVC   0(11,R4),=C'PROD HOUSE:'                                         
         MVC   12(6,R4),SVADDHSE                                                
         LA    R4,20(R4)                                                        
*                                                                               
PRTA60   CLI   SVADDDDT,0          ANY DESTROY DATE                             
         BE    PRTA62                                                           
         MVC   0(14,R4),=C'PLEASE DESTROY'                                      
         GOTO1 DATCON,DMCB,(3,SVADDDDT),(8,16(R4))                              
*                                                                               
         CLI   SVADDDTM,0          ANY DESTROY TIME                             
         BE    PRTA62              NO                                           
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(2),SVADDDTM                                                  
         GOTO1 CALLOV,DMCB,0,X'D9000A11'  GET UNTIME ADDRESS                    
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,DUB,26(R4)                                             
*                                                                               
PRTA62   DS    0H                                                               
*&&DO                                                                           
         OC    SVINSDT,SVINSDT     IF ORIGINAL, NO CHANGE POSSIBLE              
         BZ    PRTA64                                                           
         CLI   SVINSREV,0          IF REVSION ZERO                              
         BNE   *+12                                                             
         TM    SVOPT,OPTRERUN      AND RERUN                                    
         BO    PRTA64                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'F1'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   SVINSDT,2(R6)                                                    
         BNL   PRTA64                                                           
         MVC   PLPRD+29(6),=C'(ADDED'                                           
         MVC   PLPRD+36(24),=C'SINCE LAST INSTRUCTIONS)'                        
         B     PRTA70                                                           
*                                                                               
PRTA64   CLC   SVINSDT,8(R6)                                                    
         BNL   PRTA70                                                           
         MVC   PLPRD+29(8),=C'(CHANGED'                                         
         MVC   PLPRD+38(24),=C'SINCE LAST INSTRUCTIONS)'                        
*&&                                                                             
PRTA70   CLI   ALLOWLIN,0          TEST ANYTHING TO PRINT                       
         BE    PRTA80              NO                                           
*                                                                               
PRTA72   L     R4,APRTBUFF                                                      
         MVC   0(8,R4),SVADDCML   MOVE ISCI CMML TO PRINT                       
         CLI   SVADDADI,C' '                                                    
         BNH   *+10                                                             
         MVC   0(12,R4),SVADDADI  USE ADID IF IT'S THERE                        
*                                                                               
         BRAS  RE,PRTBUFF                                                       
*                                                                               
PRTA80   LA    R2,SVADDNXT                                                      
         CLI   0(R2),0                                                          
         BNE   PRTA40                                                           
         MVI   HEADSW,C'X'         SUPPRESS MIDLINES AGAIN                      
         XIT1                                                                   
         LTORG                                                                  
*          DATA SET SPTRA36    AT LEVEL 069 AS OF 05/06/08                      
         EJECT                                                                  
*================================================================               
* FIND, SAVE, AND PRINT ANY PATTERN SPECIAL TEXT                                
*================================================================               
                                                                                
         USING PTNTABD,R3                                                       
RDSTEXT  NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         L     R3,APATTAB                                                       
         USING PTNTABD,R3                                                       
         SR    RE,RE                                                            
         ICM   RE,3,CURTABNO                                                    
         B     *+8                                                              
*                                                                               
RDTX10   AH    R3,SVPTNSIZ                                                      
         LH    R1,0(R3)                                                         
         AHI   R1,-HDRLEN                                                       
         STH   R1,SVPTNSIZ                                                      
         LR    RF,R3                                                            
         AHI   R3,HDRLEN                                                        
         BCT   RE,RDTX10                                                        
         ST    R3,ACURRPTN                                                      
*                                                                               
         LR    R0,R3                                                            
         AH    R0,SVPTNSIZ                                                      
         ST    R0,ACURRPTX         SAVE END ADDRESS                             
*                                                                               
         L     R4,AIO1                                                          
         XC    0(256,R4),0(R4)                                                  
*                                                                               
RDTX20   OC    PTNSTXT,PTNSTXT     ANY SPECIAL TEXT FOR PATTERN                 
         BZ    RDTX36                                                           
         LA    R0,42                                                            
         L     R4,AIO1                                                          
*                                                                               
RDTX30   OC    0(6,R4),0(R4)       EMPTY ENTRY                                  
         BZ    RDTX34                                                           
         CLC   PTNSTXT,0(R4)                                                    
         BE    RDTX36                                                           
         LA    R4,6(R4)                                                         
         BCT   R0,RDTX30                                                        
         DC    H'0'                                                             
*                                                                               
RDTX34   MVC   0(6,R4),PTNSTXT                                                  
*                                                                               
RDTX36   LA    R3,PTNEXT           NEXT PATTERN                                 
         C     R3,ACURRPTX                                                      
         BL    RDTX20                                                           
*                                                                               
RDTX44   L     R4,AIO1                                                          
         OC    0(6,R4),0(R4)       EMPTY ENTRY                                  
         BZ    RDTX80                                                           
*                                                                               
         MVI   HEADSW,C'X'         SUPPRESS MIDLINES                            
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         L     R6,AIO2             USE AIO2 FOR GPRD CALL, NOT AIO1!            
         ST    R6,AIO                                                           
*                                                                               
RDTX50   L     R3,ACURRPTN                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2D'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVI   KEY+5,C'-'                                                       
         MVC   KEY+6(6),0(R4)                                                   
         MVI   KEY+12,C'L'                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   RDTX70                                                           
*                                                                               
RDTX52   MVC   P+25(23),=C'** SPECIAL COMMENTS FOR'                             
         MVC   MYKEYSV,KEY         SAVE TRFDIR KEY                              
*                                                                               
RDTX54   CLC   0(6,R4),PTNSTXT     TEST RIGHT ENTRY                             
         BNE   RDTX58                                                           
*                                                                               
         LA    R1,PTNBPRD                                                       
         BRAS  RE,GPRD                                                          
         MVC   P+53(3),1(R1)                                                    
         MVC   P+58(20),4(R1)                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(2,PTNFTD),(8,P+80)                                  
         MVI   P+88,C'-'                                                        
         GOTO1 (RF),(R1),(2,PTNLTD),(8,P+89)                                    
*                                                                               
         LA    R1,PTNBPRD2                                                      
         CLI   0(R1),0                                                          
         BE    RDTX56                                                           
         BRAS  RE,GPRD                                                          
         MVC   P+53+132(3),1(R1)                                                
         MVC   P+58+132(20),4(R1)                                               
*                                                                               
RDTX56   MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   SPACING,1           RESTORE                                      
*                                                                               
RDTX58   LA    R3,PTNEXT           NEXT PATTERN                                 
         C     R3,ACURRPTX         ANY MORE                                     
         BL    RDTX54               YES - GO PROCESS                            
*                                                                               
RDTX62   MVC   KEY,MYKEYSV         RESTORE TRFDIR FOR READ SEQ                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,PSTXT                                                         
*                                                                               
RDTX70   LA    R4,6(R4)            NEXT STEXT KEY                               
         CLI   0(R4),0                                                          
         JE    EXIT                                                             
         MVI   P,0                                                              
         GOTO1 SPOOL,DMCB,(R8)       GOTO SPOOL RTN                             
         B     RDTX50                                                           
                                                                                
*==============================================                                 
* PRINT END OF INSTRUCTIONS TEXT                                                
*==============================================                                 
                                                                                
RDTX80   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2D'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVI   KEY+3,C'*'                                                       
         MVC   KEY+4(1),SVCLTOFF                                                
         MVI   KEY+12,C'L'                                                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    RDTX90                                                           
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3                                                   
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   RDTXX                                                            
*                                                                               
RDTX90   L     R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
* SEE HOW MANY LINES WE NEED FOR COMMENTS *                                     
                                                                                
         SR    R5,R5               CLEAR COUNTER                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
*                                                                               
         BRAS  RE,GETEL                                                         
         BNE   RDTXX                                                            
*                                                                               
         AHI   R5,1                                                             
         BRAS  RE,NEXTEL                                                        
         BE    *-8                                                              
*                                                                               
         MVI   SPACING,1                                                        
         LLC   R0,LINE                                                          
         LLC   RE,FOOTLNS                                                       
         AR    R0,RE                                                            
         AR    R0,R5                                                            
         CLM   R0,1,MAXLINES                                                    
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
*                                                                               
         BRAS  RE,PSTXT                                                         
*                                                                               
RDTXX    DS    0H                                                               
         MVI   FORCEHED,C'N'       RESET IN CASE NOTHING PRINTED                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* PRINT SPECIAL TEXT FOR RECORD IN KEY                                          
*===========================================================                    
                                                                                
PSTXT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
PSTX2    L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
PSTX4    BRAS  RE,NEXTEL                                                        
         BNE   PSTX6                                                            
         LLC   RE,1(R6)                                                         
         AHI   RE,-4                                                            
         EX    RE,PSTMVC           MOVE COMMENT TO PRINT                        
         GOTO1 SPOOL,DMCB,(R8)     GOTO SPOOL RTN                               
         B     PSTX4                                                            
PSTMVC   MVC   P+25(0),3(R6)                                                    
*                                                                               
PSTX6    LLC   R1,KEY+12           BUMP TYP (PAGE NUMBER)                       
         LA    R1,1(,R1)                                                        
         STC   R1,KEY+12                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PSTX2                                                            
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=======================================================                        
* READ AND SAVE FOOTNOTE TEXT                                                   
*=======================================================                        
                                                                                
GETFOOT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A23'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
*                                                                               
         CLI   BPRD,X'FF'          TEST POL REQUEST                             
         BE    *+10                YES - USE CLT TEXT                           
         MVC   KEY+5(3),QPRD                                                    
*                                                                               
         MVI   KEY+8,C'F'                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST PRD TEXT FOUND                          
         BE    GETFT2                                                           
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         XC    KEY+5(3),KEY+5      CLEAR PRODUCT                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GETFT2                                                           
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         XC    KEY+3(2),KEY+3      CLEAR CLIENT                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   EXIT                                                             
*                                                                               
GETFT2   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'NONE ',2(R6)     TEST SUPPRESS COMMENT                        
         JE    EXIT                                                             
*                                                                               
         LA    R0,MAXFOOT          SET MAX COMMENT LINES                        
         LA    R4,FOOTCMT                                                       
*                                                                               
GETFT4   MVC   0(60,R4),SPACES                                                  
         LLC   RE,1(R6)                                                         
         AHI   RE,-4                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),3(R6) *EXECUTED*                                         
         LA    R4,60(R4)                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   GETFTX                                                           
         BCT   R0,GETFT4                                                        
*                                                                               
GETFTX   MVI   0(R4),0                                                          
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* BUILD STATION TABLE ENTRIES - EXTRA ENTRY IF POL *                            
*================================================================               
                                                                                
         USING STATABD,R5                                                       
BLDST    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   BPRD,X'FF'          THIS A POL REQUEST                           
         BE    BLDST10              YES, BUILD EXTRA ENTRY                      
         MVC   STAPTR,CURTABNO                                                  
         MVC   STAMS,BMKTSTA                                                    
         MVC   STADTS,SVTCDTS      FIRST/LAST TELECAST DATES FOR STA            
         MVC   STAREV,SVINSREV                                                  
*                                                                               
         MVC   STAINSDT,SVSTAIDT                                                
         LA    R5,STANEXT                                                       
         C     R5,ASTATABX                                                      
         BNL   BSTSIZER                                                         
         LTR   RB,RB               SET CC NE                                    
BLDSTX   XIT1  REGS=(R5)                                                        
                                                                                
*=================================================================              
* THERE WILL ONLY BE 1 NEW ENTRY IN ANY TABLE IN ONE PASS                       
*                                                                               
* REGS 0 COUNT OF BYTES LEFT IN EACH ENTRY (STARTS AT 256)                      
*      1 CURR ENTRY IN PRD TABLE                                                
*      2 CT OF CURR PRD TABLE ENTRY                                             
*      5 CURRENT ENTRY IN STATION TABLE                                         
*      E PTR IN ELEM                                                            
*      F PTR IN CURR PRD ENTRY                                                  
*=================================================================              
                                                                                
BLDST10  L     R5,ASTATAB          SEE IF THIS STATION IN TABLE                 
         XC    ELEM,ELEM                                                        
         MVI   BYTE,0                                                           
*                                                                               
BLDST14  OC    STAENT,STAENT       AT END OF ENTRIES                            
         BZ    BLDST20                                                          
         CLC   STAMS,BMKTSTA       FOUND THIS STATION                           
         BE    BLDST16              YES                                         
         LA    R5,STANEXT                                                       
         C     R5,ASTATABX                                                      
         BL    BLDST14                                                          
         B     BSTSIZER                                                         
*                                                                               
BLDST16  SR    RF,RF                                                            
         ICM   RF,3,STAPTR                                                      
         BCTR  RF,0                                                             
         SLL   RF,8                X 256                                        
         A     RF,APRDTAB                                                       
         MVC   ELEM,0(RF)                                                       
*                                                                               
BLDST20  LLC   RE,SVSTAPRD                                                      
         LA    R1,ELEM(RE)                                                      
         MVC   0(1,R1),CURTABNO+1                                               
         MVC   STAMS,BMKTSTA                                                    
         MVC   STADTS,SVTCDTS      FIRST/LAST TELECAST DATES FOR STA            
         MVC   STAREV,SVINSREV                                                  
*                                                                               
         MVC   STAINSDT,SVSTAIDT                                                
         L     R1,APRDTAB                                                       
         LA    R2,1                                                             
BLDST30  OC    0(256,R1),0(R1)     AT END OF TABLE                              
         BZ    BLDST50                                                          
         LA    R0,256                                                           
         LA    RE,ELEM                                                          
         LR    RF,R1                                                            
         MVI   BYTE,0                                                           
*                                                                               
BLDST32  CLC   0(1,RE),0(RF)       BOTH ENTRIES THE SAME                        
         BE    BLDST36                                                          
         CLI   0(RF),0             IF OLD IS ZERO, SET FLAG                     
         BE    BLDST34                                                          
         LA    R1,256(R1)                                                       
         LA    R2,1(R2)                                                         
         C     R1,APRDTABX                                                      
         BL    BLDST30                                                          
         DC    H'0'                                                             
*                                                                               
BLDST34  MVI   BYTE,1              SET FLAG                                     
*                                                                               
BLDST36  LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,BLDST32                                                       
*                                                                               
* EXISTING AND NEW TABLES ARE EQUAL, NOW SET ANY MISSING FROM NEW *             
*                                                                               
         CLI   BYTE,0              WAS FLAG SET                                 
         BE    BLDST54              NO, OLD AND NEW EQUAL                       
*                                                                               
         LA    R0,256                                                           
         LA    RE,ELEM                                                          
         LR    RF,R1                                                            
*                                                                               
BLDST40  CLC   0(1,RE),0(RF)       BOTH ENTRIES THE SAME                        
         BE    BLDST44                                                          
         CLI   0(RF),0             IF OLD IS ZERO, MOVE JUST THIS ENTRY         
         BE    BLDST46                                                          
BLDST44  LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,BLDST40                                                       
         DC    H'0'                BUG-MUST BE 1 BYTE DIFFERENT TO MOVE         
*                                                                               
BLDST46  MVC   0(1,RF),0(RE)       MOVE USED BYTE FROM NEW TO OLD               
         B     BLDST54                                                          
*                                                                               
BLDST50  MVC   0(256,R1),ELEM                                                   
*                                                                               
BLDST54  STCM  R2,3,STAPTR                                                      
         LA    R5,STANEXT                                                       
         C     R5,ASTATABX                                                      
         BNL   BSTSIZER                                                         
         CR    RB,RB               SET CC EQ                                    
         J     EXIT                                                             
*                                                                               
BSTSIZER CLI   OFFLINE,C'Y'        IF OFFLINE, BIG TROUBLE                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         BRAS  R1,*+64                                                          
         DC    CL60'* ERROR * TOO MANY STATIONS - PLEASE RUN OVERNITE'          
         MVC   CONHEAD(60),0(R1)                                                
         XC    CONHEAD+11(49),SPACES                                            
         MVI   CONHEAD+28,C'-'     RESTORE THE -                                
         LA    R2,TRAMEDH                                                       
         GOTO1 ERREX2                                                           
         LTORG                                                                  
         DROP  R5                                                               
         EJECT                                                                  
*===============================================================                
* READ MKT ASSIGNMENT REC TO SEE IF MKT INCLUDED IN REQUEST                     
*================================================================               
                                                                                
GETMKA   NTR1  BASE=*,LABEL=*                                                   
         MVC   SVKEY,KEY                                                        
*                                                                               
         LA    R4,KEY                                                           
         USING MKAKEY,R4                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   MKAKTYP,=X'0D03'                                                 
         MVC   MKAKAGMD,BAGYMD      A-M                                         
         MVC   MKAKCLT,BCLT                                                     
         MVC   MKAKMKT,SVKEY+6     MARKET                                       
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GETMKA2                                                          
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    MKAKCLT,MKAKCLT      TRY ALL CLIENT                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BE    GETMKA2                                                          
         XC    FILENAME,FILENAME                                                
         B     GETMKA10                                                         
         DROP  R4                                                               
*                                                                               
GETMKA2  L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'SPTFILE'                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         LA    R6,24(R6)                                                        
         MVI   ELCODE,5                                                         
         BRAS  RE,FIRSTEL                                                       
         BNE   GETMKA10                                                         
         B     GETMKA4A                                                         
*                                                                               
GETMKA4  BRAS  RE,NEXTEL                                                        
         BNE   GETMKA10                                                         
*                                                                               
         USING MKAEL05,R6                                                       
GETMKA4A OC    MKAPGRP(3),MKAPGRP  SKIP PRDGRP ASSIGNS                          
         BNZ   GETMKA4                                                          
*                                                                               
         CLC   MKAMGRP(1),SVMGRID  RIGHT MGRID                                  
         BNE   GETMKA4                                                          
         OC    SVMGRCD,SVMGRCD     ONE MKTGRP REQUESTED                         
         BZ    GETMKA6             NO                                           
         CLC   MKAMGRP(3),SVMGRID  MGRID/MGRCD                                  
         BNE   GETMKA4                                                          
*                                                                               
GETMKA6  MVC   FULL(3),MKAMGRP     SAVE MATCHING MGID/MGCD                      
         DROP  R6                                                               
         BRAS  RE,GETMGR           GET MKTGRP NAME IF NEEDED                    
         L     R1,ABLOCK                                                        
         MVC   2(3,R1),FULL        MOVE SAVED MGID/MGCD                         
         MVC   KEY,SVKEY           RESTORE KEY                                  
         J     EQXIT                                                            
*                                                                               
GETMKA10 L     R1,ABLOCK                                                        
         MVC   2(1,R1),SVMGRID                                                  
         MVC   3(2,R1),=XL2'9999'                                               
         MVC   KEY,SVKEY                                                        
         CLI   SVTCPR02,C'*'       TEST MULTIPLE MKTGRPS                        
         J     EXIT                AND SET CC                                   
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* LOOK THROUGH BUFFER AND SEE IF MKTGRP NAME SAVED ALREADY                      
* IF NOT, READ THIS MKTGRP RECORD AND SAVE NAMES                                
* ON ENTRY KEY HAS MARKET ASSIGNMENT RECORD KEY                                 
*      AND R6 HAS ELEMENT IN MARKET ASSIGN RECORD                               
*==============================================================                 
                                                                                
GETMGR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R0,MAXMGR           MAX ENTRIES IN TABLE                         
         L     R4,AMGRTAB                                                       
         USING MGRTABD,R4                                                       
*                                                                               
         USING MKAEL05,R6                                                       
GETMGR02 OC    MGRID(3),MGRID      EMPTY SLOT                                   
         BZ    GETMGR04                                                         
         CLC   MKAMGRP(3),MGRID    TEST ELEM TO TABLE                           
         BE    GETMGRX                                                          
         LA    R4,L'MGRTAB(R4)                                                  
         BCT   R0,GETMGR02                                                      
         DC    H'0'                                                             
*                                                                               
GETMGR04 MVC   KEYSAVE,KEY         SAVE MKA KEY                                 
         XC    KEY,KEY                                                          
*                                                                               
         LA    R5,KEY              A-M/CLT ARE PRESET                           
         USING MKGRECD,R5                                                       
*                                                                               
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD(3),(MKAKAGMD-MKAKEY)+KEYSAVE   A-M/CLT                  
         XC    MKGKPID(3),MKGKPID     CLEAR PRDGRP                              
         MVC   MKGKMID(3),MKAMGRP     SET MKTGRP FROM MKAEL05                   
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         DROP  R6                     (NEW RECORD READ OVER IT)                 
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT                             
         GOTO1 GETREC                                                           
         L     RE,AIO                                                           
         CLI   24(RE),X'10'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   MGRID(3),FULL       MOVE SAVED MGID/MGCD TO TABLE                
         MVC   MGRNAME,26(RE)      MOVE BREAK 1 NAME                            
         DROP  R4,R5                                                            
*                                                                               
GETMGRX  XC    FILENAME,FILENAME                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===================================================                            
* GENERATE REQUEST FOR AGENCY COPY                                              
*===================================================                            
                                                                                
REQ      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   CONWHEN(2),=C'OV'        CHANGE TO OV(ERNIGHT) REQ               
*                                                                               
         MVC   CONWHEN+2(4),CONWHEN+4                                           
         MVI   CONWHEN+6,0                                                      
         MVI   CONWHEN+7,0                                                      
         LLC   RF,CONWHENH+5                                                    
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         STC   RF,CONWHENH+5                                                    
*                                                                               
         LLC   R2,TRAOPTH+5                                                     
         LA    R3,TRAOPT(R2)                                                    
         LR    R6,R3                                                            
         SR    R4,R4                                                            
*                                                                               
         LTR   R2,R2                                                            
         BZ    REQ10                                                            
         MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
         LA    R4,1                                                             
*                                                                               
REQ10    MVI   0(R6),C'#'                                                       
         LA    R4,1(R4)                                                         
         LA    RF,0(R2,R4)                                                      
         STC   RF,TRAOPTH+5                                                     
*                                                                               
         XC    REQHDR,REQHDR                                                    
         MVC   REQUEST,SPACES                                                   
         MVI   REQHDR+15,X'01'     GENERATE LINKED REQUESTS                     
         MVC   REQUEST(2),=C'TV'                                                
         MVC   REQUEST+2(2),AGENCY                                              
*                                                                               
         L     RE,TWAMASTC                                                      
         MVC   REQUEST+5(6),MCREQREC+5-MASTD(RE) USE THIS REQ SIN               
*                                                                               
         XC    REQSPOOK,REQSPOOK   NEED TO PASS A SPOOK                         
         GOTO1 REQTWA,DMCB,(0,ATWA),REQHDR,DATAMGR,RCCOMFAC,REQSPOOK            
*                                                                               
         MVC   WORK(L'CONWHEN-4),CONWHEN+2                                      
         MVC   CONWHEN+4(L'CONWHEN-4),WORK                                      
         MVC   CONWHEN(4),=C'SOON'                                              
         LLC   RF,CONWHENH+5                                                    
         LA    RF,2(RF)                                                         
         STC   RF,CONWHENH+5                                                    
         XC    CONHEAD,CONHEAD                                                  
*                                                                               
         EX    R4,REQCLR                                                        
         STC   R2,TRAOPTH+5                                                     
         XIT1                                                                   
*                                                                               
REQCLR   MVC   0(0,R3),SPACES                                                   
         LTORG                                                                  
         EJECT                                                                  
PRTEXT   NTR1  BASE=*,LABEL=*                                                   
         USING GEND,RC                                                          
*                                                                               
         L     RE,AIO1                                                          
         AHI   RE,3000                                                          
         XC    0(24,RE),0(RE)      CLEAR IN CASE NO TEXT                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A23'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         CLI   BPRD,X'FF'          TEST POL REQUEST                             
         BE    *+10                IF SO READ CLIENT TEXT                       
         MVC   KEY+5(3),QPRD                                                    
*                                                                               
         MVI   KEY+8,C'H'                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST PRD TEXT FOUND                          
         BE    PTX10                                                            
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         XC    KEY+5(3),KEY+5      CLEAR PRODUCT                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PTX10                                                            
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         XC    KEY+3(2),KEY+3      CLEAR CLIENT                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PTX36                                                            
*                                                                               
PTX10    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   HALF,0                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   HALF,2(R6)          SAVE IND BYTE                                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   =C'NONE ',3(R6)     TEST SUPPRESS COMMENT                        
         BE    PTX36                                                            
*                                                                               
         TM    HALF,X'80'          TEST TO BOX COMMENT                          
         BZ    PTX16               NO                                           
*                                                                               
         L     R0,AIO1                                                          
         AHI   R0,3000             FORMAT TO AIO1+3000                          
         GOTO1 =A(BOXER),DMCB,(60,(R0)),RR=SPTR34RR                             
         B     PTX20                                                            
         EJECT                                                                  
*======================================================                         
* FORMAT UNBOXED COMMENT                                                        
*======================================================                         
                                                                                
PTX16    L     R4,AIO1                                                          
         AHI   R4,3000                                                          
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
*                                                                               
PTX18    MVC   0(60,R4),SPACES                                                  
         LLC   RE,1(R6)                                                         
         AHI   RE,-4                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),3(R6) *EXECUTED*                                         
         LA    R4,60(R4)                                                        
         BRAS  RE,NEXTEL                                                        
         BE    PTX18                                                            
         MVI   0(R4),0             SET EOL FLAG                                 
                                                                                
*=================================================================              
* FORMAT STANDARD TEXT TO PRINT LINES                                           
*=================================================================              
                                                                                
PTX20    MVI   HEADSW,C'X'         SET FLAG TO SUPPRESS MIDLINES                
         L     R4,AIO1             FORMATTED COMMENTS ARE HERE                  
         AHI   R4,3000                                                          
         MVI   SPACING,1                                                        
*                                                                               
PTX30    LA    R0,4                                                             
         LA    R1,P1                                                            
*                                                                               
PTX32    CLI   0(R4),0             TEST REACHED END OF COMMENTS                 
         BE    PTX34                                                            
         MVC   0(60,R1),0(R4)      MOVE COMMENT TO PRINT LINE                   
*                                                                               
         LA    R1,132(R1)                                                       
         LA    R4,60(R4)                                                        
         BCT   R0,PTX32                                                         
*                                                                               
PTX34    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CLI   0(R4),0                                                          
         BNE   PTX30                                                            
         EJECT                                                                  
*=================================================================              
* FORMAT STEXT TO HEADLINES                                                     
*=================================================================              
                                                                                
PTX36    TM    SVOPT2,OP2SPCMT     TEST SPECIAL COMMENTS                        
         BZ    PTX38                                                            
         CLI   SVPROF16,C'N'       NULL (NO SPECIAL TEXT)                       
         JE    EXIT                                                             
         B     PTX40                                                            
*                                                                               
PTX38    CLI   SVPROF12,C'*'       TEST SPECIAL MKT/STA COMMENTS                
         JNE   EXIT                                                             
                                                                                
*====================================================================           
* FIND AND PRINT SPECIAL TEXT FOR MKTGRP OR MARKET                              
*====================================================================           
                                                                                
PTX40    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2D'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(1),SVPROF16                                                
         TM    SVOPT2,OP2SPCMT     TEST SPECIAL COMMENTS                        
         BO    *+10                                                             
         MVC   KEY+5(1),SVPROF12                                                
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   PTX42                                                            
*                                                                               
         LLC   R0,BEST                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   KEY+6(3),=C'ES='                                                 
         UNPK  KEY+9(3),DUB                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    PTX60                                                            
*                                                                               
         MVC   KEY(13),KEYSAVE     RESTORE KEY                                  
         MVI   KEY+11,0                                                         
*                                                                               
         CLI   SVTCPR02,C'0'       COVER LETTER BY MARKET GROUP                 
         BE    PTX42                NO                                          
*                                                                               
         MVC   KEY+6(3),=C'MG='                                                 
         MVC   KEY+9(1),SVMGRID                                                 
         LR    R1,R3                                                            
         AHI   R1,-2                                                            
         MVC   KEY+10(2),0(R1)     MOVE MGRCD                                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(12),KEYSAVE                                                  
         BE    PTX60                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(6),KEYSAVE                                                   
*                                                                               
PTX42    CLI   SVTCPR01,C'Y'       COVER LETTER BY MARKET                       
         BNE   PTX44                                                            
*                                                                               
         MVC   KEY+6(4),QMKT                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(12),KEYSAVE                                                  
         BE    PTX60                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(6),KEYSAVE                                                   
*                                                                               
PTX44    MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE    TEST ALL MKT/STA STEXT FOUND                  
         BNE   PTX92                                                            
*                                                                               
PTX60    MVI   P1,0                FORCE A BLANK LINE                           
         LA    R4,P2               FIRST PRINT LINE                             
         LA    R5,3                                                             
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
PTX70    BRAS  RE,NEXTEL                                                        
         BNE   PTX75                                                            
*                                                                               
PTX72    SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-4                                                            
         LTR   RE,RE                                                            
         BNZ   PTX74                                                            
         CLI   3(R6),C' '                                                       
         BNE   PTX74                                                            
         MVI   0(R4),0                                                          
         B     PTX74A                                                           
*                                                                               
PTX74    EX    RE,MOVETEXT                                                      
PTX74A   LA    R4,132(R4)          NEXT PRINT LINE                              
         BCT   R5,PTX70                                                         
*                                                                               
PTX75    GOTO1 SPOOL,DMCB,(R8)     PRINT THE LINES                              
         MVI   SPACING,1           RESET                                        
*                                                                               
         CLI   0(R6),0             TEST REACHED END                             
         BE    PTX80               YES                                          
*                                                                               
         LA    R4,P1               IF NOT, PRINT 4 MORE LINES                   
         LA    R5,4                                                             
         B     PTX70                                                            
*                                                                               
PTX80    LLC   R1,KEY+12           BUMP TYP (PAGE NUMBER)                       
         LA    R1,1(,R1)                                                        
         STC   R1,KEY+12                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    PTX60                                                            
*                                                                               
PTX90    LLC   RE,MAXLINES         FORCE A BLANK LINE                           
         LLC   R0,LINE                                                          
         SR    RE,R0                                                            
         CHI   RE,10               IF DON'T HAVE 10 LINES                       
         BNL   PTX92                                                            
         MVI   FORCEHED,C'Y'       START ON A NEW PAGE                          
*                                                                               
PTX92    MVI   HEADSW,C'Y'         GET THE MIDLINES PRINTING                    
         MVI   FORCEMID,C'Y'       NEED TO PRINT W/O DATA IN P                  
         XIT1                                                                   
MOVETEXT MVC   0(0,R4),3(R6)                                                    
         EJECT                                                                  
*================================================================               
* TURN ON MARKET LETTER INSTR RUN FLAG IN INSTR RECAP RECORDS                   
* USES MARKET BECAUSE TRUE MARKET GEN CAN'T RUN ON CABLE, SO                    
* THERE IS NO CONFLICT                                                          
*================================================================               
                                                                                
UPDRCP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,ASTATAB                                                       
         USING STATABD,R5                                                       
         OC    STAPTR,STAPTR       IF NO ENTRIES, EXIT NOW                      
         JZ    EXIT                                                             
*                                                                               
UPDRCP2  MVC   CURTABNO,STAPTR                                                  
         CLI   BPRD,X'FF'          WE DOING POL INSTR                           
         BNE   UPDRCP4                                                          
*                                                                               
         L     R1,APRDTAB          GET PATTERN TABLE ENTRY ADDR                 
         ICM   R0,3,CURTABNO                                                    
         BCTR  R0,0                                                             
         SLL   R0,8                TIMES 256                                    
         AR    R1,R0                                                            
         OC    0(256,R1),0(R1)     BETTER BE AN ENTRY                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R1,ACURPRDT                                                      
         L     R0,ASVCLIST                                                      
         ST    R0,ACURPRPT                                                      
         BRAS  RE,FNDCUR           FIND REAL CURTABNO FOR PTN PTR               
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDRCP4  L     R3,APATTAB                                                       
         USING PTNTABD,R3                                                       
         SR    RE,RE                                                            
         ICM   RE,3,CURTABNO                                                    
         B     *+8                                                              
*                                                                               
         AH    R3,SVPTNSIZ                                                      
         LH    R1,0(R3)                                                         
         AHI   R1,-HDRLEN                                                       
         STH   R1,SVPTNSIZ                                                      
         AHI   R3,HDRLEN                                                        
         BCT   RE,*-20                                                          
*                                                                               
         USING PTNTABD,R3                                                       
UPDRCP6  LA    R4,KEY                                                           
         USING INSKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVC   INSKID(2),=XL2'0A24'                                             
         MVC   INSKAM(3),BAGYMD        BCLT                                     
         MVC   INSKPRD,PTNBPRD         BPRD                                     
         MVC   INSKMKT(5),STAMS        MKT/STA                                  
         MVC   INSKCOPY,PTNCOPY        COPY CODE                                
         MVC   INSKDPT,PTNDPT          DPT W/IN EST                             
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   UPDRCP22                                                         
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
UPDRCP8  L     R6,AIO1                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING INSDTAEL,R6                                                      
UPDRCP10 CLC   INSPRD1(2),PTNBPRD                                               
         BNE   UPDRCP20                                                         
         CLC   INSPRD2,PTNBPRD2                                                 
         BNE   UPDRCP20                                                         
         CLC   INSSLN2(1),PTNSLN2                                               
         BNE   UPDRCP20                                                         
         CLC   SVFLTEDP,INSFTD                                                  
         BL    UPDRCP20                                                         
         LLC   RE,1(R6)                                                         
         LA    RF,0(R6,RE)                                                      
         AHI   RF,-2                                                            
         CLC   SVFLTSTP,0(RF)                                                   
         BH    UPDRCP20                                                         
         OI    INSFLAG,INSFLCOV   SET ON MKT/AMS LETTERS RUN                    
         OI    TABSW,UPREC        SET UPDATE NEEDED ON                          
*                                                                               
         CLI   SVFAXREQ,C'2'      SEE IF DOING FAX REPORT                       
         BE    *+12                                                             
         CLI   SVFAXREQ,C'Y'                                                    
         BNE   UPDRCP20                                                         
         OI    INSFLAG,INSFLFAX   SET ON MKT/AMS LETTERS FAXED                  
*                                                                               
UPDRCP20 BRAS  RE,NEXTEL                                                        
         BE    UPDRCP10                                                         
*                                                                               
UPDRCP22 LA    R3,L'PTNENT(R3)                                                  
         LH    R1,SVPTNSIZ                                                      
         AHI   R1,-L'PTNENT                                                     
         BNP   UPDRCP26                                                         
         STH   R1,SVPTNSIZ                                                      
         CLC   INSKCOPY,PTNCOPY                                                 
         BE    UPDRCP8                                                          
*                                                                               
         TM    TABSW,UPREC                                                      
         BZ    UPDRCP24                                                         
*                                                                               
         TM    SVOPT,OPTTEST      THIS TEST MODE                                
         BO    UPDRCP24                                                         
         GOTO1 PUTREC                                                           
*                                                                               
UPDRCP24 NI    TABSW,X'FF'-UPREC                                                
         B     UPDRCP6                                                          
*                                                                               
UPDRCP26 TM    TABSW,UPREC                                                      
         BZ    UPDRCP30                                                         
         TM    SVOPT,OPTTEST      THIS TEST MODE                                
         BO    UPDRCP30                                                         
         GOTO1 PUTREC                                                           
*                                                                               
UPDRCP30 NI    TABSW,X'FF'-UPREC                                                
*                                                                               
         CLI   BPRD,X'FF'          DOING POL REQUEST                            
         BNE   UPDRCP40                                                         
         MVC   CURTABNO,STAPTR                                                  
         BRAS  RE,FNDCUR           FIND REAL CURTABNO FOR PTN PTR               
         BNE   UPDRCP4              NO, ALL DONE                                
*                                                                               
UPDRCP40 LA    R5,STANEXT                                                       
         OC    STAPTR,STAPTR                                                    
         BNZ   UPDRCP2                                                          
         J     EXIT                                                             
         EJECT                                                                  
*==========================================================                     
* COUNT NUMBER OF 132 CHAR PRINT LINES IN PRTBUFF                               
* AND PRINT THEM                                                                
*==========================================================                     
                                                                                
         USING SPOOLD,R8                                                        
PRTBUFF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,APRTBUFF         COUNT NUMBER OF PRINT LINES                  
         SR    R5,R5                                                            
*                                                                               
PRTBUFF2 CLI   0(R4),0             TEST FORCE PRINT                             
         BE    *+14                                                             
         CLC   0(132,R4),SPACES                                                 
         BNH   PRTBUFF4                                                         
         LA    R4,132(R4)                                                       
         BCT   R5,PRTBUFF2                                                      
*                                                                               
PRTBUFF4 LPR   R0,R5                                                            
         JZ    EXIT                                                             
*                                                                               
         L     R6,APRTBUFF                                                      
         MVC   SVSPCING,SPACING    SAVE SPACING VALUE                           
         MVI   SPACING,1                                                        
*                                                                               
PRTBUFF6 LA    R4,P                                                             
         LA    R5,4                                                             
         CR    R5,R0               TEST MORE THAN 4 LINES REMAIN                
         BL    PRTBUFF8                                                         
         LR    R5,R0               DO THE NUMBER THAT REMAIN                    
         MVC   SPACING,SVSPCING    AND RESTORE ORIGINAL SPACING                 
*                                                                               
PRTBUFF8 MVC   0(132,R4),0(R6)                                                  
         MVC   0(132,R6),SPACES    BLANK THE SOURCE NOW                         
         LA    R6,132(R6)                                                       
         LA    R4,132(R4)                                                       
         BCT   R5,PRTBUFF8                                                      
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT 4 LINES                                
         AHI   R0,-4                                                            
         BP    PRTBUFF6                                                         
         J     EXIT                                                             
*                                                                               
CLRBUFF  SR    R0,R0               R4 HAS A(BUFF), R5 HAS LEN                   
         LA    R1,X'40'                                                         
         SLL   R1,24               SET TO BLANK FILL                            
         L     R4,APRTBUFF                                                      
         LA    R5,4000                                                          
         MVCL  R4,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPMGRTAB                                                       
         EJECT                                                                  
         PRINT ON                                                               
PLINED   DSECT                                                                  
*                                                                               
PLSLN    DS    CL9                                                              
         DS    CL2                                                              
PLROT    DS    CL8                                                              
         DS    CL2                                                              
PLCML    DS    CL12                                                             
         DS    CL4                                                              
PLCMLNAM DS    CL20                                                             
         DS    CL1                                                              
PLOTHER  DS    CL27                                                             
         DS    CL1                                                              
PLTIME   DS    CL23                                                             
         ORG                                                                    
         ORG   PLINED+132                                                       
PLPRD    DS    CL7                                                              
*                                                                               
PLSEEMOR EQU   PLROT                                                            
         ORG   PLCMLNAM+8                                                       
PLCOM    DS    CL80                                                             
         ORG   PLROT                                                            
PLCODE   DS    CL16                                                             
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPTRINST                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRFLT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRSTA                                                        
         EJECT                                                                  
       ++INCLUDE SPTRPRH                                                        
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRCMLTXT                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAGYCON                                                     
         EJECT                                                                  
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRDTXT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DDREPMASTD                                                     
         EJECT                                                                  
       ++INCLUDE DDREMOTED                                                      
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAD9D                                                       
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
HDRLEN   EQU   5                   2 LEN/3 MKTGRP AT TOP OF ABLOCK              
         ORG   SVSPAREX                                                         
SPTR34RR DS    A                                                                
PQPROF   DS    A                   ADDRESS OF GET PQ PROFILE ROUTINE            
ASTATAB  DS    A                                                                
ASTATABX DS    A                                                                
APATTAB  DS    A                                                                
APATTABX DS    A                                                                
APRDTAB  DS    A                                                                
APRDTABX DS    A                                                                
ASVCMLS  DS    A                                                                
ASVCMLSX DS    A                                                                
ASVPRDS  DS    A                                                                
ASVPRDSX DS    A                                                                
ASVADDL  DS    A                                                                
ASVADDLX DS    A                                                                
                                                                                
APTNEXP  DS    A                                                                
ASVFTNT  DS    A                                                                
ABLOCK   DS    A                                                                
ABLOCKX  DS    A                                                                
APRTBUFF DS    A                                                                
AMGRTAB  DS    A                                                                
AMGRTABX DS    A                                                                
VTRPACK  DS    A                                                                
                                                                                
VXSORT   DS    A                                                                
ACURRPTN DS    A                   ADDRESS OF CURR PATTERN LIST                 
ACURRPTX DS    A                   ADDRESS OF END OF PTTN LIST ENTRY            
ACURPRDT DS    A                   ADDRESS OF CURR PRD TABLE ENTRY              
ACURPRPT DS    A                   ADDRESS (IN SVCLIST) OF CURR PROD            
SVTCPROF DS    CL16                                                             
SVTCPR01 EQU   SVTCPROF+0          COVER LETTERS BY MARKET                      
SVTCPR02 EQU   SVTCPROF+1          COVER LETTERS BY MARKET GROUP                
SVTCPR03 EQU   SVTCPROF+2          BY MARKET GROUP 2ND CHARACTER                
SVTCPR04 EQU   SVTCPROF+3          SHOW CABLE STA ON COV GEN                    
SVTWPROF DS    CL16                                                             
SVTWPR01 EQU   SVTWPROF+0                                                       
SVTWPR02 EQU   SVTWPROF+1                                                       
SVTWPR03 EQU   SVTWPROF+2                                                       
SVTWPR04 EQU   SVTWPROF+3          FAX COVER LETTERS                            
SVTWPR05 EQU   SVTWPROF+4          FAX SHIPPING                                 
SVT2PR02 DS    CL1                 SUPPRESS COMML TYPE                          
SVT2PR04 DS    CL1                 PRINT PATTERN PERCENTS                       
*                                                                               
PROFKEY  DS    CL5                 READ PROGRAM PROFILE RECORD                  
SVQLTYP1 DS    CL1                 STORE ARCHIVE SETTING                        
SVFAXARC DS    CL1                 ARCHIVE SETTING FOR FAX COPIES               
MYSTOR   EQU   *                   START OF CLEARED STORAGE                     
SVPTNSIZ DS    F                                                                
SEQNUM   DS    H                                                                
SVBMKT   DS    XL2                                                              
SVESTSTR DS    XL3                 ESTIMATE START                               
SVESTEND DS    XL3                          END                                 
*                                                                               
SVFLTDTS DS    0XL6                                                             
SVFLTSTR DS    XL3                 FLIGHT START                                 
SVFLTEND DS    XL3                        END                                   
SVFLTSTP DS    XL2                 FLIGHT START                                 
SVFLTEDP DS    XL2                        END                                   
*                                                                               
SVGENDTS DS    0XL6                                                             
SVGENST  DS    XL3                                                              
SVGENEND DS    XL3                                                              
SVGENDTP DS    0XL4                                                             
SVGENSTP DS    XL2                                                              
SVGENEDP DS    XL2                                                              
*                                                                               
SVSTAIDT DS    XL2                                                              
*                                                                               
SVTCDTS  DS   0XL4                                                              
SVSTAFTD DS    XL2                                                              
SVSTALTD DS    XL2                                                              
*                                                                               
SVSTAPRD DS    XL1                                                              
SVFAXREQ DS    C                                                                
HDSVOPT  DS    X                                                                
*                                                                               
LSTPTNED DS    XL2                 LAST PATTERN END DATE                        
NXTPTNST DS    XL2                 NEXT PATTN START DATE                        
*                                                                               
SVPATST  DS    XL1                 SAVED PATTERN STATUS                         
SVINSREV DS    XL1                 MOST COMMON REV NUMBER                       
SVINSRCT DS    XL1                 COUNT OF REV NUMS FOR THIS LETTER            
SVBPRD   DS    XL1                                                              
CURTABNO DS    XL2                                                              
SVCRTBNO DS    XL2                                                              
QUESTOR  DS    CL24                                                             
CONTEL   DS    CL18                                                             
CONFAX   DS    CL18                                                             
CONEMAIL DS    CL60                                                             
MYKEYSV  DS    XL16                                                             
MYLASTEL DS    A                                                                
PRDHSELS DS   0CL120                                                            
PRDHSEL1 DS    CL24                                                             
PRDHSEL2 DS    CL24                                                             
PRDHSEL3 DS    CL24                                                             
PRDHSEL4 DS    CL24                                                             
PRDHSEFX DS    CL24                                                             
*                                                                               
HOUSECD  DS    CL6                                                              
PRDHFAX  DS    CL19    HOUSE FAX NUMBER                                         
SVMLBOX  DS    CL16    MAIL BOX FAX                                             
TABSW    DS    XL1                                                              
CHAENT   EQU   X'80' - TABLE ENTRY (IF EQUAL) NEEDS DATES CHANGED               
FOUNDENT EQU   X'40' - INSTR RECAPS FOUND FOR MEDIA/CLT/PRD/PER                 
UPREC    EQU   X'08' - UPDATED ELEMENT, NEED TO PUTREC                          
*                                                                               
THISCML  DS    X                                                                
SVSPCING DS    C                                                                
PIGSW    DS    C                                                                
HEADSW   DS    XL1                 80=HEAD HOOK JUST EXECUTED                   
*                                  08=USE COVER LETTER MIDLINES                 
*                                  04=PRINTING PATTERN SPECIAL TEXT,            
*                                      DO NOT PRINT MIDLINES                    
DOPCT    DS    CL1                                                              
FOOTSW   DS    CL1                                                              
CONTINUE DS    CL1                                                              
RUNFLAG  DS    XL1                                                              
RUNFLMOR EQU   X'80'                                                            
*                                                                               
MKTLIST  DS    CL22                                                             
*                                                                               
SVMGR    DS    0XL9                                                             
SVMGRID  DS    XL1                 MGRID                                        
SVMGRCD  DS    XL2                 MGRCD                                        
QMGRID DS      CL2                 2-CHAR MKTGRP ID                             
QMGRCD DS      CL4                 4 DIGIT MKTGRP NUMBER                        
*                                                                               
SVMGEL01 DS    CL41                CD/LEN/TITLES AND BREAKLENS                  
*                                                                               
SVMGRKEY DS    0XL13                                                            
SVMGRTY  DS    XL2                                                              
SVMGRAM  DS    XL1                                                              
SVMGRCL  DS    XL2                                                              
SVMGRPGR DS    XL3                                                              
SVMGRMGR DS    XL3                                                              
SVMGRMKT DS    XL2                                                              
*                                                                               
OPTIONS  DS    0CL20                                                            
*                                                                               
SVOPT    DS    XL1                                                              
*                                                                               
OPTTEST  EQU   X'80'                                                            
OPTRERUN EQU   X'40'                                                            
OPTREV   EQU   X'20'                                                            
OPTNEW   EQU   X'10'                                                            
OPTDEL   EQU   X'08'                                                            
OPTCOPY  EQU   X'04'                                                            
OPT1MKT  EQU   X'02'                                                            
OPT1STA  EQU   X'01'                                                            
*                                                                               
SVOPT2   DS    XL1                                                              
OP2NOADR EQU   X'80'               IGNORE MISSING STATION ADDRESS               
OP2TRBUY EQU   X'40'               USE TRAFFIC BUYS, NOT SPOT BUYS              
OP2SPCMT EQU   X'20'               USE ALT SPEC CMT CODE-SVPROF16               
OP2MLBOX EQU   X'10'               SEND TO MAILBOX (DGS FOR NOW)                
*                                                                               
         DS    0D                                                               
FOOTCMT  DS    5CL60               FOOTLINE COMMENTS                            
         DS    CL1                 ROOM FOR EOT FLAG                            
FOOTCMTX EQU   *                                                                
MAXFOOT  EQU   5                                                                
*                                                                               
MYSTORX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
STATAB   DS    XL1024                                                           
STATABX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
MGRTAB   DS    30XL27              ROOM FOR 30 27 BYTE ENTRIES                  
         DS    XL1                 ROOM FOR EOL FLAG                            
MGRTABX  EQU   *                   SEE MGRTABD BELOW                            
MAXMGR   EQU   (MGRTABX-MGRTAB)/27                                              
*                                                                               
ENDSYSD  EQU   *                   CHECKED AT RUN TIME FOR LSYSD                
*                                                                               
SVPRDD   DSECT                                                                  
SVPRDDTA DS    0CL24                                                            
SVPRDCD  DS    X                                                                
SVPRDEBC DS    CL3                                                              
SVPRDNM  DS    CL20                                                             
*                                                                               
MGRTABD  DSECT                                                                  
MGRID    DS    XL1                                                              
MGRGRP   DS    XL2                                                              
MGRNAME  DS    XL24                                                             
         EJECT                                                                  
* STATION TABLE DSECT *                                                         
*                                                                               
STATABD DSECT                                                                   
STAENT   DS    0XL14                                                            
STAPTR   DS    XL2                                                              
STAMS    DS    0XL5                                                             
STAMKT   DS    XL2                                                              
STASTA   DS    XL3                                                              
STADTS   DS    0XL4                                                             
STAFTD   DS    XL2                                                              
STALTD   DS    XL2                                                              
STAREV   DS    XL1                                                              
STAINSDT DS    XL2                                                              
STANEXT  EQU   *                                                                
                                                                                
* PATTERN TABLE DSECT *                                                         
                                                                                
PTNTABD  DSECT                                                                  
PTNENT   DS   0XL32                                                             
PTNSRT   DS   0XL6                                                              
PTNBPRD  DS    XL1                                                              
PTNSLN   DS    XL1                                                              
PTNPRD2  DS    CL3                 SORT ON EBC PRD2                             
PTNSLN2  DS    XL1                                                              
*                                                                               
PTNREF   DS    XL3                                                              
PTNFTD   DS    XL2                                                              
PTNLTD   DS    XL2                                                              
PTNCOPY  DS    CL1                                                              
PTNBPRD2 DS    XL1                                                              
PTNSTXT  DS    CL6          SPECIAL TEXT KEY                                    
PTNSTIM  DS    XL2                                                              
PTNETIM  DS    XL2                                                              
PTNDPT   DS    XL1                                                              
*                                                                               
PTNFLAG  DS    XL1                                                              
PTNFHIA  EQU   X'80'               PATTERN IS AN HIATUS                         
PTNFUFN  EQU   X'40'               PATTERN RUNS UFN                             
PTNFIPR  EQU   X'20'               INVERT PRODUCT ORDER                         
PTNFCMT  EQU   X'08'               PATTERN HAS NEW COMML TEXT                   
PTNFSTX  EQU   X'04'               PATTERN HAS NEW SPECIAL TEXT                 
PTNFCHGE EQU   X'02'               PATTERN HAS CHANGED                          
PTNFNEW  EQU   X'01'               NEW PATTERN                                  
*                                                                               
PTNFTDN  DS    XL2                                                              
PTNLTDN  DS    XL2                                                              
PTNFLAG1 DS    XL1                                                              
PTNFDLY  EQU   X'40'               PATTERN TIMES ARE DAILY                      
PTNEXT   EQU   *                                                                
         EJECT                                                                  
SVCMLD   DSECT                                                                  
SVCMLDTA DS    0CL(SVCMLNXT-SVCMLCOD) (184)                                     
SVCMLCOD DS    CL12                                                             
SVCMLSEQ DS    XL2                                                              
SVCMLPOS DS    XL1                 CMML POSITION  (FOR ROT LETTER)              
SVCMLNAM DS    CL15                                                             
SVCMLNM2 DS    CL20                DECSRIPTION LINE 2                           
SVCMLNM3 DS    CL20                DESCRIPTION LINE 3                           
SVCMLTYP DS    CL4                                                              
SVCMLPIG DS    CL1                 0 FOR SOLO - 1 OR 2 FOR P/B                  
SVCMLPRD DS    XL1                                                              
SVCMLSLN DS    XL1                                                              
SVCMLOV1 DS    XL1                                                              
SVCMLOV2 DS    XL1                                                              
SVCMLST  DS    XL1                 CMML STATUS BYTE (X'40' = CML TEXT)          
*                                  X'80' = START/END TIMES ARE DAILY            
*                                  X'40' = CML TEST                             
SVCMLFTD DS    XL2                 FIRST TELECAST DATE                          
SVCMLLTD DS    XL2                 LAST TELECAST DATE                           
SVCMLSTM DS    XL2                 START TIME                                   
SVCMLETM DS    XL2                 END TIME                                     
SVCMLCLT DS    CL20                CLIENT COMMERCIAL #                          
SVCMLTEL DS    CL8                 CANADIAN TELECASTER #                        
SVCMLTLX DS    CL27                CANADIAN TELECASTER # NEW                    
SVCMLTHD DS    CL27                CANADIAN TELECASTER # HD                     
SVCMLCBC DS    CL9                 CANADIAN CBS/SRC #                           
SVCMLTC1 DS    CL17                CANADIAN TALENT CYCLE 1                      
SVCMLTC2 DS    CL17                CANADIAN TALENT CYCLE 2                      
SVCMLADI DS    CL12                AD-ID                                        
SVCMLHDF DS    CL12                HI-DEF                                       
SVCMLPRN DS    CL12                PARENT                                       
SVCMLCTR DS    CL12                CENTERCUT                                    
SVCMLHSE DS    CL6                 PROD HOUSE                                   
SVCMLDDT DS    XL3                 DESTROY DATE                                 
SVCMLDTM DS    XL2                 DESTROY TIME (2400=12A,0=NONE)               
SVCMLPCT DS    XL2                 ROTATION PERCENTAGE (FROM PTTN)              
         DS    CL7                                                              
SVCMLNXT EQU   *                                                                
*                                                                               
SVADDLD  DSECT                                                                  
SVADDENT DS   0CL48                                                             
*                                                                               
SVADDCML DS    CL12                                                             
*                                                                               
SVADDADI DS    CL12                                                             
SVADDSEQ DS    XL2                 ZERO FOR NO TEXT REC                         
SVADDPRD DS    XL1                                                              
SVADDTYP DS    CL4                 COMMERCIAL TYPE                              
SVADDHSE DS    CL6                 PROD HOUSE                                   
SVADDDDT DS    XL3                 DESTROY DATE                                 
SVADDDTM DS    XL2                 DESTROY TIME (2400=12A,0=NONE)               
         DS    CL7                                                              
SVADDNXT EQU   *                                                                
         EJECT                                                                  
*                                                                               
GEND     DSECT                                                                  
         ORG   ELEM                                                             
REQHDR   DS    CL26                                                             
REQUEST  DS    CL80                                                             
REQUEST2 DS    CL80                                                             
REQSPOOK DS    CL64                                                             
         EJECT                                                                  
* OFFLINE REPORT LINE                                                           
*                                                                               
SPOOLD   DSECT                                                                  
*                                                                               
       ++INCLUDE EDIDESTD                                                       
         ORG   P                                                                
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035SPTRA34   11/19/15'                                      
         END                                                                    
