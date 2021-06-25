*          DATA SET REINF00S   AT LEVEL 008 AS OF 05/01/02                      
*PHASE T80B00A,*                                                                
         TITLE 'REINF00 - T80B00 - INFO BASE MODULE, VALIDATING'                
*                                                                               
*********************************************************************           
*                                                                   *           
*        REINF00 --- REP INFO PROGRAM BASE MODULE                   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* JUN08/89 (MRR) --- HISTORY LOST                                   *           
*                    ADD 'ACE', 'FAX', 'COMBO' AND 'INTER' OPTIONS  *           
*                     FOR STATION USE ONLY                          *           
*                                                                   *           
* 07/19/89  PJS  ADDED NEW RECORDS:                                 *           
*                'DMENU' - X'23' DEMO MENU RECORDS                  *           
*                                                                   *           
*                'COMMENT' OR 'CMT' - X'2E' STANDARD COMMENT RECS   *           
*                                                                   *           
* AUG23/89  PJS  CHANGE DATAMGR CALL TO KEYSAVE,KEY FORMAT          *           
*                                                                   *           
* 09/05/89  PJS  ADDED REP LISTING FOR DDS TERMINALS                *           
*                                                                   *           
* 11/09/89  PJS  ADDED '@@**' SPECIAL RECORD ACTION (FIXER)         *           
*                                                                   *           
* 01/24/90  PJS  ADDED POINT PERSON (X'31') AND                     *           
*                CONTRACT TYPE (X'32') LISTINGS                     *           
*                                                                   *           
*07/25/90   BU   STATION RECORD:  EI OPTION, INACTIVE FILTER/ACTIVE *           
*                DEFAULT.                                           *           
*                AGENCY RECORD:   EI OPTION.                        *           
*                PRODUCT RECORD:  FILTER FOR POINT PERSON, NETWORK  *           
*                CONTRACT, OPTION OF "NETWORK" .                    *           
*                                                                   *           
* 01/23/92  SKU  PRODUCT RECORD: OPTION OF "SPOT" AND DISPLAY SCREEN*           
*                POINT PERSON MIN LEN CHANGED TO 2                  *           
*                                                                   *           
* 04/15/92  SKU  AGY RECORD: RISK OPTION, LIAB OPTION               *           
*                                                                   *           
* 04/23/92  SKU  ADJUST PP DISPLAY FOR SALESPERSON NAME             *           
*                                                                   *           
* 10/28/92  BU   NEW COLUMN HEADINGS FOR STATION/COMBO OPTION       *           
*                                                                   *           
* 11/10/92  SKU  ADD FAX DISPLAY OPTION FOR OFFICE and agency rec.  *           
*                ADD FAX DISPLAY FOR SALESPERSON REC.               *           
*                                                                   *           
* 11/22/93  BU   NEW COLUMN HEADINGS FOR INTER OPTION               *           
*                NEW 'COMBO+' ACTION                                *           
*                                                                   *           
* 12/07/93  BU   NEW SALESPERSON OPTIONS, HEADINGS                  *           
*                                                                   *           
* 02/08/94  BU   ADDED DEVELOPMENTAL SALESPERSON RECORDS (X'3A')    *           
*                AND CONTRACT TYPE RECORDS (x'3B')                  *           
*                                                                   *           
* FEB17/95  BU   DROP REGION HQ FIELDS                              *           
*                                                                   *           
* FEB22/95  BU   ADD LEAVE DATE TO POINT PERSON, DEV S/P            *           
*                                                                   *           
* FEB22/95  BU   ACCESS PROFILE RECORD, SET TWA VALUES              *           
*                                                                   *           
* FEB23/95  BU   SET OPTION FOR DATE/COMP STATION DISPLAYS          *           
*                                                                   *           
* FEB23/95  BU   TEAM/OFFTEAM FILTERS FOR STATION DISPLAYS          *           
*                                                                   *           
* NOV08/95  RHV  ROUTINE TO TEST IF REP IS KATZ OR SUBSIDIARY       *           
*                                                                   *           
* NOV13/95  RHV  PARENT/MASTER REP FILTER FOR REP RECORDS           *           
*                                                                   *           
* FEB20/96  RHV  REMOVE CHECKING FOR KATZ FOR EQUIV OPTION          *           
*                                                                   *           
* 10APR96  (RHV) SUPPORT DISPLAY OF 34 BYTE AGY ADDRESS FIELDS      *           
*                                                                   *           
* MAY01/96  WSB  ADD 'REP' OPT FOR STA TO DISPLAY FORMER/NEW REPS   *           
*                                                                   *           
* AUG09/96  SEP  ADD 'TER' FILTERS FOR TERRITORY DISPLAY            *           
*                                                                   *           
* FEB11/97 (DBU) ADD 'AGY' OPTION (AGENCY/FLIGHT DATES)             *           
*                                                                   *           
* JAN09/98 (JRD) ADD 'PARENT' 'PARENT+' 'MARKET' & 'OWNER' TO       *           
*                 STATION.                                          *           
*                                                                   *           
* JUN23/98 (RHV) ALLOW AC=BO W/STATION REC BY MARKET                *           
*                                                                   *           
* AUG12/98 (AST) SCOPE FILTER FOR NATIONAL/LOCAL OFFICES, AGY + ADV *           
*                                                                   *           
* SEP11/98 (AST) OFFICE FILTER FOR AGY & ADV RECS                   *           
*                                                                   *           
* JAN28/99 (BU ) STATION MASTER DISPLAY                             *           
*                                                                   *           
* MAY12/98 (BU ) AGENCY+TERRITORY DISPLAY                           *           
*                                                                   *           
* JUN28/00 (BU ) REMOVE REFERENCES TO GLV1GOTO PER MEL HERTZIG      *           
*                                                                   *           
*                                                                   *           
*                ***  END TOMBSTONE  ***                            *           
*********************************************************************           
*                                                                               
T80B00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWORK,**8B00**,R8,R9,RR=R5,CLEAR=YES                             
*                                                                               
         USING GENOLD,RC                                                        
*                                                                               
         ST    R5,RELO             ROOT PHASE RELO FACTOR                       
         ST    R1,SYSFAC           SAVE A(FACILITIES)                           
         L     RE,16(R1)           A(COMFACS)                                   
         ST    RE,ACOMFACS                                                      
*                                                                               
         BAS   RE,INITL                                                         
         USING T80BFFD,RA                                                       
*                                                                               
         ST    RD,BASERD           SAVE RD FOR MAIN EXIT                        
*                                                                               
         L     RF,0(R1)            A(FATIOB)                                    
         ST    RF,ATIOB                                                         
*                                                                               
*                                                                               
         BAS   RE,CHKGLOB          CHECK FOR GLOBBER RETURN                     
*                                                                               
         XC    INFMESS,INFMESS     CLEAR MESSAGE SPACE                          
         FOUT  INFMESSH            AND TRANSMIT                                 
*                                                                               
         BAS   RE,CHKPFK           CHECK AND HANDLE PF KEYS                     
         B     MAIN0010            CONTINUE IF NO SWAP                          
         EJECT                                                                  
CHKGLOB  NTR1                                                                   
*                                                                               
*    CHECK FOR GLOBBER LOADER VARIABLES - USE MYP                               
*                                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         GOTO1 CGLOBBER,DMCB,=C'GETD',WORK,24,GLVXCTL   XCTL ELEM?              
         TM    DMCB+8,X'10'        NO VARIABLES FOUND, SKIP                     
         BNZ   CKGL0100                                                         
         GOTO1 (RF),DMCB,=C'DELE',,,GLVXCTL                                     
*                                                                               
         LA    R3,WORK                                                          
         USING GLFINEL,R3                                                       
*                                                                               
         XC    INFSVC,INFSVC         CLEAR SERVICE REQUEST FIELD                
         LA    R2,INFSVCH                                                       
         MVC   8(3,R2),=C'=RE'                                                  
*                                    FORCE SCREEN REFRESH                       
         MVI   5(R2),4                                                          
         OI    6(R2),X'80'                                                      
         XC    INFSTRT,INFSTRT       CLEAR START AT FIELD                       
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'GETD',WORK,GLFINLNQ,GLRFLINF                    
         TM    DMCB+8,X'10'        NO VARIABLES FOUND, SKIP                     
         BNZ   MAINXIT                                                          
         GOTO1 (RF),DMCB,=C'DELE',,,GLFINEL                                     
         DROP  R4                                                               
         CLC   GLFINSTA,SPACES       IS THIS A MASTER ADD OR CHANGE?            
         BNH   CKGL0020                                                         
         LA    R2,INFSTRTH                                                      
         MVC   8(4,R2),GLFINSTA      INSERT 1ST FOUR CHARS OF STN               
         DROP  R3                                                               
*                                    FORCE SCREEN REFRESH                       
         MVI   5(R2),4                                                          
         OI    6(R2),X'80'                                                      
         MVI   NEXTBYTE,0          CLEAR 'SHOW NEXT SCREEN'                     
         B     CKGL0100            POP BACK ONLY ONCE                           
CKGL0020 EQU   *                                                                
         L     RD,4(RD)              POP BACK TWICE                             
         B     CKGL0100                                                         
CKGL0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
CHKPFK   NTR1                                                                   
*                                                                               
*  HANDLE PFKEYS                                                                
*                                  GET PFKEY NUMBER AND ADJUST                  
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID          NUMBER OF PFKEY PRESSED                      
         C     R0,=F'12'                                                        
         BNH   *+8                                                              
         S     R0,=F'12'                                                        
         STC   R0,PFKEY            PFKEY ADJUSTED TO 1..12 (0 = ENTER)          
         DROP  RF                                                               
*                                                                               
         CLI   LINK,X'5C'          IF STATION MASTER TYPE                       
         BE    CHKX                THEN EXIT (HANDLED BY 04 OVERLAY)            
*                                                                               
         CLI   SVPGP#,RREPQINF     IF PROFILE INFO NOT IN TWA                   
         BNE   CHKX                THEN EXIT                                    
         TM    SVPGPBIT,SVSWAPQ    ELSE,IF PROF NOT SET TO ALLOW SWAP           
         BZ    CHKX                THEN EXIT                                    
*                                                                               
         CLI   PFKEY,2             PF TO 'DISPLAY'?                             
         BNE   CHKCHA               NO, CHECK NEXT                              
         BAS   RE,SWAP              YES                                         
         B     EXIT                                                             
*                                                                               
CHKCHA   CLI   PFKEY,3             PF TO 'CHANGE'?                              
         BNE   CHKADD               NO, CHECK NEXT                              
         BAS   RE,SWAP              YES                                         
         B     EXIT                                                             
*                                                                               
CHKADD   CLI   PFKEY,4             PF TO 'ADD'?                                 
         BNE   CHKX                 NO,PFKEY NOT FOR GLOBBER/SWAP               
         BAS   RE,SWAP              YES                                         
         B     EXIT                                                             
*                                                                               
CHKX     XIT1                                                                   
         EJECT                                                                  
*                                                                               
SWAP     NTR1                                                                   
*                                                                               
*    HANDLES REQUESTS TO SWAP TO FILE PROGRAM                                   
*                                                                               
         CLI   OPTNBYTE,0                     IF OPTIONS CHOSEN                 
         BE    SWAP010                                                          
         MVC   INFMESS(L'NOTAVAIL),NOTAVAIL   THEN ERROR, AND EXIT              
         B     SWAPX                                                            
*                                                                               
SWAP010  BAS   RE,CHKCURSR                    GET CURSOR POSITION               
         BE    SWAP015                        IF POS VALID, CONTINUE            
         MVC   INFMESS(L'INVPOSIT),INVPOSIT   ELSE, ERROR, AND EXIT             
         B     SWAPX                                                            
*                                                                               
SWAP015  XC    KEYHOLD,KEYHOLD              CLEAR KEY HOLDER                    
         BAS   RE,GETKEY                    GET KEY FOR REC AT CURSR            
         BNZ   SWAP020                      IF FOUND, CONTINUE                  
         B     SWAPX                        ELSE, EXIT                          
*                                                                               
SWAP020  L     R5,ACOMFACS                                                      
         USING COMFACSD,R5                                                      
         XC    BLOCK1(256),BLOCK1                                               
         LA    R1,BLOCK1                                                        
*                                                                               
         USING GLVXFRSY,R1            SET TO SWAP                               
         MVC   GLVXFRSY,=C'REP'       FROM REP SYSTEM                           
         MVC   GLVXFRPR,=C'INF'         INFO PROGRAM                            
         MVC   GLVXTOSY,=C'REP'       TO REP SYSTEM                             
         MVC   GLVXTOPR,=C'FIL'         FILE PROGRAM                            
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
         DROP  R1                                                               
*                                                                               
*                  SET TRANSFER CONTROL BLOCK                                   
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',BLOCK1,14,GLVXCTL                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                  SET GLOBBER ELEMENT FOR FILE DISPLAY/ADD/CHANGE              
*                                                                               
         XC    TEMPWORK,TEMPWORK                                                
         LA    R1,TEMPWORK                                                      
         USING GLFILEL,R1                                                       
*                                                                               
         CLI   PFKEY,2             DISPLAY REQUEST?                             
         BNE   SWAP030             NO                                           
         MVC   GLFILFA(3),=C'DIS'   YES                                         
         B     SWAP100                                                          
*                                                                               
SWAP030  CLI   PFKEY,3             CHANGE  REQUEST?                             
         BNE   SWAP040             NO                                           
         MVC   GLFILFA(3),=C'CHA'   YES                                         
         B     SWAP100                                                          
*                                                                               
SWAP040  CLI   PFKEY,4             ADD     REQUEST?                             
         BNE   SWAP050             NO                                           
         MVC   GLFILFA(3),=C'ADD'   YES                                         
         B     SWAP100                                                          
*                                                                               
SWAP050  DC    H'0'                BAD PFKEY VALUE                              
*                                                                               
*                                                                               
SWAP100  MVC   GLFILREC,RECTYPE    MOVE IN RECORD TYPE                          
*                                                                               
         CLI   PFKEY,4             IF 'ADD'                                     
         BE    SWAP150             THEN DO NOT ADD KEY                          
         MVC   GLFILKEY,KEYHOLD    ELSE, MOVE IN KEYTAB ENTRY                   
SWAP150  GOTO1 CGLOBBER,DMCB,=C'PUTD',TEMPWORK,GLFILLNQ,GLRINFIL                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SWAPX    B     MAINXIT             EXIT PROGRAM                                 
         EJECT                                                                  
*                                                                               
GETKEY   NTR1                                                                   
*                                                                               
*          RETURNS ADDRESS OF KEYTABLE ENTRY IF FOUND                           
*          SETS CONDITION CODE TO SIGNAL FOUND/NOT FOUND                        
*                                                                               
         LA    R3,SWAPTAB                                                       
         USING SWAPTABD,R3                                                      
GETK10   CLI   SWAPLNK,X'FF'           IF NO ENTRY FOUND                        
         BE    GETKMISS                  SWAP UNAVAILABLE                       
         CLC   SWAPLNK,LINK            IF MATCH FOUND                           
         BE    GETK20                    CONTINUE                               
         LA    R3,L'SWAPTAB(R3)        ELSE, NEXT ENTRY                         
         B     GETK10                                                           
*                                                                               
GETK20   MVC   RECTYPE,SWAPREC         SAVE RECORD TYPE                         
         TM    SWAPFLG,SWMULTQ         IF MULTIPLE LINE DISP                    
         BO    GETK50                    HANDLE DIFFERENTLY                     
         ZIC   R5,SWAPELP              ELSE,GET #OF ENTS PER LINE               
         SR    R6,R6                   CLEAR EVEN MEMBER                        
         ZIC   R7,OUTLN#               GET NUMBER OF OUTPUT LINE                
         BCTR  R7,0                    DECREMENT                                
         MR    R6,R5                   GET NUMBER OF ENTS UP TO LINE            
         LR    R4,R7                   KEEP IN R4                               
*                                                                               
         SR    R6,R6                                                            
         ZIC   R7,SWAPDISP             GET DISPLACEMENT OF ENTRY                
         ZIC   R5,OUTCOL#              GET CURSOR COLUMN                        
GETK30   AR    R6,R7                   INCREASE DISP FOR NEXT ENTR              
         CR    R5,R6                   IF CURSR POS LESS THAN DISP              
         BL    GETK100                                                          
         AHI   R4,1                    INCREMENT PRECEDING ENTS #               
         B     GETK30                                                           
*                                                                               
*        ROUTINE FOR ENTRIES WITH MULTIPLE LINES                                
*                                                                               
GETK50   ZIC   R5,SWAPELP              ELSE,GET #OF LINES PER ENTRY             
         SR    R6,R6                   CLEAR EVEN MEMBER                        
         ZIC   R7,OUTLN#               GET NUMBER OF OUTPUT LINE                
         DR    R6,R5                   OUTPUT LINE / LINES PER ENTRY            
         CHI   R6,0                    IF THERE IS A REMAINDER                  
         BNH   *+8                                                              
         AHI   R7,1                      THEN ADD 1                             
         BCTR  R7,0                    DECREMENT R7 FOR FIND LOOP               
         LR    R4,R7                   R4= NUMBER OF ENTRIES TO LINE            
         B     GETK100                                                          
         DROP R3                                                                
*                                                                               
*                                                                               
GETK100  LA    R3,KEYTAB               POINT TO BEGINNING OF KEYTAB             
         CHI   R4,0                    IF THIS IS 1ST ENTRY                     
         BNH   GETK200                   THEN WE HAVE RIGHT ADDRESS             
GETK110  LA    R3,L'KEYTAB(R3)         ELSE, ADVANCE ADDRESS                    
         BCT   R4,GETK110              FOR THE ENTRY COUNT                      
         B     GETK200                                                          
*                                                                               
GETKMISS MVC   INFMESS(L'FUNCTREC),FUNCTREC  FUNCTION NOT DEFINED FOR           
         SR    R4,R4                                                            
         LTR   R4,R4                                                            
         B     GETKX                                                            
*                                                                               
GETK200  MVC   KEYHOLD,0(R3)           SAVE KEY ENTRY                           
         OC    KEYHOLD,KEYHOLD         CHECK IF ENTRY WAS EMPTY                 
         BNZ   *+10                                                             
         MVC   INFMESS(L'INVPOSIT),INVPOSIT  NO KEY FOR THIS SCRN POS           
GETKX    XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
**********************************************************************          
*  RETURNS CURSOR POSITION BY OUTPUT LINE AND COLUMN#                           
*    OR ERROR IF ABOVE VALID SCREEN LOCATION                                    
*                                                                               
CHKCURSR NTR1                                                                   
         MVI   OUTLN#,0                                                         
         MVI   OUTCOL#,0                                                        
         L     R2,ATIOB                                                         
         USING TIOBD,R2                                                         
         LH    R3,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         AR    R3,RA               RA=TWA                                       
         LHI   R5,1                R5 = OUTPUT LINE #                           
         LA    R4,INFOUTH          FIRST OUT LINE ON SCRREN                     
         CR    R3,R4                                                            
         BL    CCNO                CURSOR CAN'T BE LESS THAN 1ST LINE           
         BE    CC20                FOUND                                        
         BH    CC15                GET NEXT LINE                                
*                                                                               
CC10     CR    R3,R4                                                            
         BL    CC20                                                             
         AHI   R5,1                INCREMENT OUTPUT LINE NUMBER                 
*                                                                               
                                                                                
CC15     ZIC   R1,0(R4)                                                         
         C     R1,=F'9'            END OF SCREEN                                
         BNH   CCNO                                                             
         AR    R4,R1               NO,BUMP TO NEXT SCRN OUTPUT LINE             
         B     CC10                                                             
                                                                                
* - IF CURSOR FOUND ON VALID OUTPUT LINE                                        
CC20     STC   R5,OUTLN#           STORE OUTPUT LINE NUMBER                     
         LH    R7,TIOBCURS         PICK UP ABSOLUTE CURSOR DISP                 
         SR    R6,R6               CLEAR EVEN MEMBER OF PAIR                    
         LHI   R4,80                 SCREEN LINE LENGTH                         
         DR    R6,R4                                                            
         STC   R6,OUTCOL#          REMAINDER IS COLUMN NUMBER                   
*                                                                               
CCYES    SR    R3,R3                                                            
CCNO     LTR   R3,R3                                                            
CCX      XIT1                                                                   
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
MAIN0010 EQU   *                                                                
         ST    RB,BASERB                                                        
         XC    INFMESS,INFMESS                                                  
*                                                                               
*  IF NOT ALREADY DONE, READ IN REP RECORD FOR PGM PROFILE                      
         CLI   SVPGP#,RREPQINF                                                  
         BE    VALRCRD             IN TWA FROM PRIOR HIT                        
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'                                                        
         MVC   KEY+25(2),REPALPHA                                               
         GOTO1 READ                                                             
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                REP RECORD NOT ON FILE?   HOW?               
*                                                                               
         GOTO1 GETREC                                                           
         MVC   REPMAST,RREPMAST    SAVE MASTER/SUBSIDIARY FLAG                  
         XC    SVPGMPRF,SVPGMPRF   ASSUME NO ELEMENT                            
         MVI   SVPGP#,RREPQINF     SET PROGRAM NUMBER                           
*                                                                               
         L     RE,AIOAREA          RECORD IS HERE                               
         SR    RF,RF                                                            
         ICM   RF,3,RREPLEN                                                     
         AR    RF,RE                                                            
         MVI   0(RF),0             FORCE 0 AT END OF RECORD                     
*                                                                               
         LA    RE,34(RE)           A(1ST ELEMENT)                               
MAIN020  EQU   *                                                                
         CLI   0(RE),0             END OF RECORD W/O MATCH?                     
         BE    VALRCRD             YES - NO PROFILES SET                        
*                                                                               
         CLI   0(RE),X'04'         PROGRAM PROFILE ELEMENT?                     
         BE    MAIN040                                                          
*                                                                               
*****>   CLI   0(RE),X'01'         REP ELEMENT?                                 
*****>   BE    CK                  CHECK KATZ ROUTINE                           
*                                                                               
MAIN030  ZIC   RF,1(RE)            GET NEXT ELEMENT                             
         AR    RE,RF                                                            
         B     MAIN020             GO BACK FOR NEXT                             
*                                                                               
*   FIND INF PROGRAM UNIT WITHIN PROGRAM PROFILE ELEMENT                        
MAIN040  EQU   *                                                                
         USING RREPPGMP,RE                                                      
         ZIC   R0,RREPPGM#         NUMBER OF PROGRAM UNITS                      
         LA    RE,RREPPGM1         A(1ST UNIT)                                  
         DROP  RE                                                               
*                                                                               
MAIN050  CLI   0(RE),RREPQINF      LOOKING FOR INF                              
         BE    MAIN060                                                          
         LA    RE,RREPPGML(RE)     NEXT UNIT                                    
         BCT   R0,MAIN050                                                       
         B     VALRCRD             NOT FOUND - NO PROFILES SET                  
*                                                                               
MAIN060  MVC   SVPGMPRF,0(RE)      SAVE UNIT IN TWA                             
*                                                                               
VALRCRD  LA    R2,INFRCRDH         RECORD                                       
         OI    1(R2),X'01'         TURN ON MODIFIED BIT                         
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         TM    4(R2),X'20'         TEST ALREADY VALID                           
         BO    VALSTRT                                                          
         BAS   RE,CLRRCRD                                                       
*                                                                               
         LA    R3,RCRDERR                                                       
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         CLI   5(R2),0             TEST ANY INPUT                               
         BE    ERRFLD                                                           
*                                                                               
         CLI   5(R2),2             AT LEAST 2 CHARACTERS INPUT                  
         BL    ANYERR                                                           
         IC    R4,5(R2)            SET COMPARE LENGTH ACCRDING TO INPUT         
         BCTR  R4,0                                                             
         LA    R7,TABLE1                                                        
*                                                                               
VALRCRD1 EX    R4,CLCTBL                                                        
         BNE   VALRCRD2                                                         
         MVC   LINK,0(R7)          SET UP FOR TITLES AND READING                
         B     VALSTRT                                                          
*                                                                               
VALRCRD2 IC    R5,1(R7)                                                         
         LA    R7,0(R5,R7)                                                      
         OC    0(2,R7),0(R7)                                                    
         BNZ   VALRCRD1                                                         
         SPACE 1                                                                
         LA    R3,RCRDERR                                                       
         B     ANYERR                                                           
         SPACE 1                                                                
CLCTBL   CLC   8(0,R2),2(R7)                                                    
         EJECT                                                                  
VALSTRT  OI    4(R2),X'20'         SET RECORD FIELD VALID                       
         LA    R2,INFSTRTH                                                      
         TM    4(R2),X'20'                                                      
         BO    VALFLTR                                                          
         BAS   RE,CLRSTRT                                                       
         OI    4(R2),X'20'                                                      
         SPACE 2                                                                
VALFLTR  LA    R2,INFFLTRH                                                      
         TM    4(R2),X'20'                                                      
         BO    VALOPTN                                                          
         BAS   RE,CLRFLTR                                                       
*                                                                               
         CLI   5(R2),0                                                          
         BE    VALOPTN                                                          
*                                                                               
         CLI   LINK,X'40'                                                       
         BE    REPFLTR                                                          
*                                                                               
         LA    R3,FLTRERR1                                                      
         L     RF,ACOMFACS                                                      
         L     RF,CSCANNER-COMFACSD(RF) GET A(SCANNER)                          
         GOTO1 (RF),DMCB,(R2),IOAREA1,0                                         
         CLI   DMCB+4,0            NO INPUT STRING                              
         BE    ANYERR                                                           
         LA    R5,IOAREA1          SCAN OUTPUT BLK                              
         USING SCAND,R5                                                         
         XR    R1,R1                                                            
         IC    R1,DMCB+4          NUMBER OF FILTERS                             
BGLOP    LA    R4,TABLE3                                                        
         L     R0,0(R4)            NO. OF ENTRIES                               
         LA    R4,4(R4)            CODE LIST                                    
LKCDE    CLC   3(1,R4),SCLIN       INPUT CODE LENGTH                            
         BNE   NXTABL              NEXT TABLE ENTRY                             
         XR    R6,R6                                                            
         IC    R6,3(R4)            CODE LENGTH                                  
         BCTR  R6,0                                                             
         EX    R6,COMPCD           COMPARE CODE                                 
         BNE   NXTABL              NO MATCH NEXT TABLE ITEM                     
         CLC   SCLOUT,5(R4)       MIN. LENGTH                                   
         BL    ANYERR                                                           
         CLC   SCLOUT,6(R4)        MAX LENGTH                                   
         BH    ANYERR                                                           
         NI    SCVLOUT,X'C0'       CLEAR HEX BIT                                
         MVC   TSTBYTE,8(R4)       ALPHA NUMERIC BIT                            
         NC    TSTBYTE,SCVLOUT     LEAVE ON RETURN                              
         CLC   TSTBYTE,SCVLOUT                                                  
         BNE   ANYERR                                                           
         OC    FLTRBYTE,7(R4)      SET FILTER SELECTION INDICATORS              
         OC    FLTRBYT2,10(R4)     SECOND FILTER INDICATORS                     
         OC    FLTRBYT3,11(R4)     THIRD  FILTER INDICATORS                     
         IC    R6,4(R4)            DISPLACEMENT IN FIL KEY                      
         LA    R7,MEDFLTR(R6)                                                   
         IC    R6,SCLOUT           SECOND HALF LENGTH                           
         BCTR  R6,0                                                             
         EX    R6,MVDAT            MOVE DATA TO FLTR KEY                        
         IC    R6,6(R4)                                                         
         BCTR  R6,0                                                             
         EX    R6,OCDAT                                                         
         IC    R6,9(R4)            BRANCH INDEX                                 
         B     BRANCH(R6)                                                       
BRANCH   B     TABLOOP                                                          
         B     MEDIT                                                            
         B     AFFEDIT                                                          
         B     ACEDIT              STATION ACTIVITY FILTER                      
*                                                                               
TABLOOP  LA    R5,32(R5)                                                        
         BCT   R1,BGLOP            VALIDATE OPTION                              
         B     VALOPTN                                                          
*                                                                               
NXTABL   LA    R4,L'TAB3LEN(R4)           NEXT TABLE OPTION                     
         BCT   R0,LKCDE            LOOK AT NEXT CODE                            
         B     ANYERR              END OF TABLE                                 
*                                                                               
COMPCD   CLC   SCFDIN(0),0(R4)                                                  
MVDAT    MVC   0(0,R7),SCFFOUT                                                  
OCDAT    OC    0(0,R7),SPACES                                                   
*                                                                               
MEDIT    CLC   MEDFLTR,=C'TV'                                                   
         BE    TABLOOP                                                          
         CLC   MEDFLTR,=C'FM'                                                   
         BE    TABLOOP                                                          
         CLC   MEDFLTR,=C'AM'                                                   
         BE    TABLOOP                                                          
         CLC   MEDFLTR,=C'CM'      COMBINED STATION                             
         BE    TABLOOP                                                          
         B     ANYERR                                                           
*                                                                               
ACEDIT   CLC   ACFLTR,=C'BO'       'BOTH' ACTIVE/INACTIVE                       
         BE    TABLOOP                                                          
*                                                                               
AFFEDIT  EQU   *                                                                
         LA    R1,#AFFILS          NUMBER OF TABLE ENTRIES                      
         BAS   RE,AFFED20                                                       
AFFEDTBL EQU   *                   LIST OF 3 BYTE AFFILIATE CODES               
         DC    CL3'NBC'                                                         
         DC    CL3'ABC'                                                         
         DC    CL3'CBS'                                                         
         DC    CL3'IND'                                                         
         DC    CL3'UNI'                                                         
         DC    CL3'TEL'                                                         
         DC    CL3'FOX'                                                         
         DC    CL3'GAL'                                                         
         DC    CL3'UPN'                                                         
         DC    CL3'WBT'                                                         
#AFFILS  EQU   (*-AFFEDTBL)/3                                                   
*                                                                               
AFFED20  CLC   AFFLTR,0(RE)                                                     
         BE    TABLOOP                                                          
         LA    RE,3(RE)                                                         
         BCT   R1,AFFED20                                                       
         B     ANYERR                                                           
         EJECT                                                                  
*                                                                               
REPFLTR  EQU   *                   VALIDATE FILTER FOR REP RECORDS              
         LA    R3,70               ERR MSG NUM                                  
         CLC   =C'MRP=',8(R2)      MASTER REP FILTER?                           
         BE    RF50                                                             
         CLI   5(R2),2                                                          
         BNE   ANYERR                                                           
         MVI   FLTRBYTE,X'FF'      PARENT REP FILTER FLAG                       
         MVC   MEDFLTR,8(R2)                                                    
         B     VALOPTN                                                          
RF50     CLI   5(R2),6                                                          
         BNE   ANYERR                                                           
         MVI   FLTRBYTE,X'80'      MASTER REP FILTER FLAG                       
         MVC   MEDFLTR,12(R2)                                                   
         EJECT                                                                  
*                                                                               
VALOPTN  OI    4(R2),X'20'         SET FILTER FIELD VALID                       
         LA    R2,INFOPTNH                                                      
         TM    4(R2),X'20'                                                      
         BO    TESTNEXT                                                         
*                                                                               
         MVI   STAMUSED,C'N'       SET 'STATION MASTER' OFF                     
*                                                                               
         BAS   RE,CLROPTN                                                       
         LA    R3,OPTNERR1                                                      
         CLI   5(R2),0                                                          
         BE    VOPT0060                                                         
*                                                                               
         LA    R6,8(R2)                                                         
         BAS   RE,TSTOPTN                                                       
         CLI   OPTNBYTE,13         STATION DATE OPTION?                         
         BE    VOPT0020            YES - CHECK FOR STATION RECORD               
         CLI   OPTNBYTE,15         STATION FORMER REP/NEW REP OPTION?           
         BE    VOPT0020            YES - CHECK FOR STATION RECORD               
         CLI   OPTNBYTE,14         COMPETITIVE OPTION?                          
         BE    VOPT0020            YES - CHECK FOR STATION RECORD               
         CLI   OPTNBYTE,17         COMBO PARENT OPTION?                         
         BE    VOPT0020            YES - CHECK FOR STATION RECORD               
         CLI   OPTNBYTE,18         MULTIPLE COMBO PARENT OPTION?                
         BE    VOPT0020            YES - CHECK FOR STATION RECORD               
         CLI   OPTNBYTE,19         MARKET LIST OPTION?                          
         BE    VOPT0020            YES - CHECK FOR STATION RECORD               
         CLI   OPTNBYTE,20         OWNER LIST OPTION?                           
         BNE   VOPT0040            NO                                           
*                                  YES - CHECK FOR STATION RECORD               
VOPT0020 EQU   *                                                                
         CLI   LINK,X'18'          STATION RECORD?                              
         BNE   ERROPTN             NO  - DON'T PERMIT THIS OPTION               
VOPT0040 EQU   *                                                                
         CLI   5(R2),6                                                          
         BL    VOPT0060                                                         
         CLI   OPTNBYTE,7          THESE OPTIONS ARE TOO LONG                   
         BE    VOPT0060              ACCEPT THEM AS IS:                         
         CLI   OPTNBYTE,17           - COMBO+                                   
         BE    VOPT0060              - PARENT                                   
         CLI   OPTNBYTE,18           - PARENT+                                  
         BE    VOPT0060              - MARKET                                   
         CLI   OPTNBYTE,19                                                      
         BE    VOPT0060                                                         
*                                                                               
         CLI   4(R6),C','                                                       
         BNE   ANYERR                                                           
         LA    R6,5(R6)                                                         
         BAS   RE,TSTOPTN                                                       
VOPT0060 OI    4(R2),X'20'         SET VALID                                    
         EJECT                                                                  
TESTNEXT CLI   NEXTBYTE,0                                                       
         BE    ROUTER                                                           
*                                                                               
CALLOVER BAS   RE,PROTFLD          PROTECT/UNPROTECT SCRN                       
         LA    R1,1                ASSUME PHASE 1                               
         CLI   LINK,X'15'          LINK 0-14 IN PHASE 1                         
         BL    COVE0060                                                         
*                                                                               
         LA    R1,2                LINK 15+ IN OVERLAY 2                        
*                                                                               
         CLI   LINK,X'44'          SPECIAL FIXER?                               
         BNE   COVE0020                                                         
         LA    R1,X'99'            OVERLAY 99 (FIXER)                           
         B     COVE0060                                                         
COVE0020 EQU   *                                                                
         CLI   LINK,X'24'          AGY/AOF RECORD?                              
         BNE   COVE0040                                                         
         LA    R1,3                LINK AGY/AOF IN OVERLAY 3                    
         B     COVE0060                                                         
COVE0040 EQU   *                                                                
         CLI   LINK,X'5C'          STATION MASTER?                              
         BNE   COVE0060            NO  - GOES TO OVERLAY 2                      
         LA    R1,4                YES - STATION MASTER IN OVERLAY 4            
         B     COVE0060                                                         
*                                                                               
COVE0060 EQU   *                                                                
         XC    DMCB(4),DMCB                                                     
         STC   R1,DMCB             PASS OVERLAY NUMBER                          
*                                                                               
         GOTO1 VCALLOV,DMCB,,(RA)                                               
         CLI   DMCB+4,X'FF'                                                     
         BNE   COVE0080                                                         
         DC    H'0'                                                             
         SPACE 2                                                                
COVE0080 L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC),LINK                                              
*                                  NO ERROR RETURN                              
         CLI   LINK,X'5C'          STATION MASTER?                              
         BNE   COVE0100            NO  -                                        
**       L     R2,DUB              YES - SET CURSOR POS FROM MASTER             
**       OI    6(R2),OI1C          SET CURSOR POSITION                          
COVE0100 EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* CHECK IF REP IS KATZ OR SUBSIDIARY ROUTINE                                    
*                                                                               
*CK      EQU   *                                                                
*****    MVI   ISKATZ,X'FF'        DEFAULT                                      
*****    L     RF,AIOAREA                                                       
*****    USING RREPREC,RF                                                       
*****    CLC   RREPMAST,=C'B4'     OPTION OK FOR 'BIL' TEST REC SUBSID.         
*****    BE    CKEND                                                            
*****    CLC   RREPKREP,=C'B4'     OPTION OK FOR 'BIL' TST RECS                 
*****    BE    CKEND                                                            
*****    CLC   RREPMAST,=C'K3'     OPTION OK FOR KATZ SUBSIDIARY                
*****    BE    CKEND                                                            
*****    CLC   RREPKREP,=C'K3'     OPTION OK FOR KATZ                           
*****    BE    CKEND                                                            
*****    MVI   ISKATZ,0                                                         
*CKEND   B MAIN030                                                              
*****    DROP  RF                                                               
*                                                                               
         EJECT                                                                  
ROUTER   XC    INFTITL,INFTITL                                                  
         LA    R3,FLTRERR2                                                      
         LA    R2,INFFLTRH                                                      
         SR    R5,R5                                                            
         IC    R5,LINK                                                          
         B     TABLE2(R5)                                                       
         SPACE 2                                                                
TABLE2   B     ADV10                                                            
         B     AOF10               MAKE AGY=AOF                                 
         B     DIV10                                                            
         B     TEM10                                                            
         B     SAL10                                                            
         B     REG10                                                            
         B     STAT0020                                                         
         B     OFF10                                                            
         B     PRD10                                                            
         B     AOF10                                                            
         B     CAT10                                                            
         B     CLS10                                                            
         B     GRP10                                                            
         B     OWN10                                                            
         B     DMN10               DEMO MENU                                    
         B     CMT10               STANDARD COMMENT                             
         B     REP10               REP LISTING                                  
         B     FIX10               RECORD CODE @@**                             
         B     PTP10               POINT PERSON                                 
         B     CTY10               CONTRACT TYPE                                
         B     DSP10               DEVELOPMENTAL SALESPERSON                    
         B     DCT10               DEVELOPMENTAL CONTRACT TYPE                  
         B     TER10               TERRITORY CODE DISPLAY                       
         B     STAM0020            STATION MASTER DISPLAY                       
         EJECT                                                                  
****************************************************************                
*  FILTER TABLE                                                                 
*                                                                               
*                                                                               
*TABLE3                                                                         
*                                                                               
* FIELD FIELD                           FIELD                                   
*  NO.DEF                             POS                                       
*                                                                               
*   1      CODE                            BYTE 0-2                             
*   2      CODE LENGTH                     BYTE 3                               
*   3      DISP IN FILTER STORE TABLE      BYTE 4                               
*   4      MIN INPUT LENGTH                BYTE 5                               
*   5      MAX INPUT LENGTH                BYTE 6                               
*   6      FILTER BIT                      BYTE 7                               
*   7      ALPHA X'40', NUMERIC X'80'      BYTE8                                
*   8      SPECIAL EDIT                    BYTE 9                               
*   9      SECOND FILTER BIT               BYTE 10                              
*  10      THIRD  FILTER BIT               BYTE 11                              
*                                                                               
*                                                                               
TABLE3   DS    0F                                                               
         DC    A(#TABLE3)    **** NUMBER OF TABLE ENTRIES ****                  
*                                                                   1           
*FIELD NO.        1     2     3     4 5     6     7     8     9     0           
*                                                                               
TAB3LEN  DS    0CL12                                                            
         DC    C'M  ',X'01',X'00',X'0202',X'01',X'40',X'04',X'00',X'00'         
         DC    C'AGY',X'03',X'02',X'0104',X'02',X'C0',X'00',X'00',X'00'         
         DC    C'ADV',X'03',X'06',X'0104',X'04',X'C0',X'00',X'00',X'00'         
         DC    C'R  ',X'01',X'0A',X'0102',X'08',X'C0',X'00',X'00',X'00'         
         DC    C'D  ',X'01',X'0C',X'0102',X'10',X'C0',X'00',X'00',X'00'         
         DC    C'C  ',X'01',X'0E',X'0102',X'40',X'C0',X'00',X'00',X'00'         
         DC    C'G  ',X'01',X'10',X'0102',X'80',X'C0',X'00',X'00',X'00'         
         DC    C'AFF',X'03',X'12',X'0303',X'20',X'40',X'08',X'00',X'00'         
         DC    C'O  ',X'01',X'15',X'0102',X'00',X'C0',X'00',X'01',X'00'         
         DC    C'TVB',X'03',X'17',X'0102',X'00',X'C0',X'00',X'02',X'00'         
         DC    C'OWN',X'03',X'19',X'0103',X'00',X'C0',X'00',X'04',X'00'         
         DC    C'RNK',X'03',X'1C',X'0101',X'00',X'C0',X'00',X'08',X'00'         
         DC    C'TRA',X'03',X'1D',X'0101',X'00',X'C0',X'00',X'10',X'00'         
         DC    C'PP ',X'02',X'1E',X'0203',X'00',X'C0',X'00',X'20',X'00'         
         DC    C'NET',X'03',X'21',X'0108',X'00',X'80',X'00',X'40',X'00'         
         DC    C'AC ',X'02',X'29',X'0202',X'00',X'C0',X'0C',X'80',X'00'         
         DC    C'OFT',X'03',X'2B',X'0404',X'00',X'C0',X'00',X'00',X'01'         
         DC    C'TEM',X'03',X'2B',X'0202',X'00',X'C0',X'00',X'00',X'02'         
         DC    C'KEY',X'03',X'2F',X'020A',X'00',X'40',X'00',X'00',X'04'         
         DC    C'NL ',X'02',X'39',X'0101',X'00',X'40',X'00',X'00',X'08'         
*                                                                               
*                                                                               
#TABLE3  EQU   (*-TAB3LEN)/12          SELF-ADJUSTING # ENTRIES IN TBL          
         EJECT                                                                  
**********************************************************************          
*        TABLE FOR GLOBBER SWAP INFO (DISPLACEMENTS,ETC)                        
*                                                                               
*                          1 = LINK# (ALL SWAP CAPABLE RECORDS)                 
*                          2 = RECORD TYPE                                      
*                          3 = FLAGS                                            
*                              X'01'=SINGLE ENTRY TAKES MULT LINES              
*                          4 = #ENTRIES PER LINE IF 3(X'01' OFF)                
*                              #LINES PER ENTRY IF 3(X'01' ON)                  
*                          5 = DISPLACEMENT OF ENTRY                            
*                                                                               
SWAPTAB  DS    0CL10                                                            
*              1     2           3     4      5                                 
         DC    X'00',CL6'ADVERT',X'00',AL1(3),AL1(27)                           
         DC    X'08',CL6'TEAM  ',X'00',AL1(3),AL1(27) 'DIV'IN TEAM REC          
         DC    X'0C',CL6'TEAM  ',X'00',AL1(3),AL1(27)                           
         DC    X'10',CL6'SALESM',X'00',AL1(1),AL1(79)                           
         DC    X'14',CL6'REGION',X'00',AL1(1),AL1(44)                           
         DC    X'18',CL6'STATIO',X'00',AL1(1),AL1(79)                           
         DC    X'1C',CL6'OFFICE',X'00',AL1(1),AL1(79)                           
         DC    X'20',CL6'PRODUC',X'00',AL1(2),AL1(41)                           
         DC    X'24',CL6'AGENCY',X'00',AL1(2),AL1(40)                           
         DC    X'5C',CL6'STAM  ',X'00',AL1(0),AL1(0)  'DONE EXTERNALLY'         
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                    *          
**********************************************************************          
*                                  ALL FILTERS AND OPTIONS CHECKED FOR          
*                                  VALIDITY                                     
*                                                                               
ADV10    TM    FLTRBYTE,X'FF'      NO FILTERS                                   
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FE'      ONLY OFFICE FILTER                           
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'-X'04'-X'08' ONLY KEYWORD AND SCOPE FILT'S         
         BNZ   ANYERR                                                           
*                                                                               
         LA    R3,FLTRERR1                                                      
         CLI   REPBYTE,0                                                        
         BNE   *+8                                                              
         MVI   REPBYTE,2           PRESET FOR REP RECS ONLY                     
         CLI   OPTNBYTE,15         DISPLAY KATZ RECORDS OPTION                  
         BE    ADV30                                                            
         CLI   OPTNBYTE,2                                                       
         BH    ERROPTN                                                          
         BE    ADV20                                                            
         MVC   INFTITL(4),=C'CODE'                                              
         MVC   INFTITL+7(15),ADVNAME                                            
         MVC   INFTITL+27(49),INFTITL                                           
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
*                                                                               
ADV20    DS    0H                                                               
         TM    FLTRBYT3,X'04'      KEYWORD FILTER NOT ALLOWED HERE              
         BO    ANYERR                                                           
         MVC   INFTITL(4),=C'CODE'                                              
         MVC   INFTITL+7(15),ADVNAME                                            
         MVC   INFTITL+28(30),ADVTTL                                            
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
*                                                                               
ADV30    DS    0H                                                               
         TM    FLTRBYT3,X'04'      KEYWORD FILTER NOT ALLOWED HERE              
         BO    ANYERR                                                           
         MVC   INFTITL(4),=C'CODE'                                              
         MVC   INFTITL+6(3),=C'EQV'                                             
         MVC   INFTITL+14(15),ADVNAME                                           
         MVC   INFTITL+35(30),ADVTTL                                            
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
         SPACE 2                                                                
***12/87 *** USE AOF CODE INSTEAD OF AGENCY CODE                                
         SPACE 2                                                                
AGY10    TM    FLTRBYTE,X'FF'      NO FILTERS                                   
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
*                                                                               
         CLI   REPBYTE,0                                                        
         BNE   *+8                                                              
         MVI   REPBYTE,2           PRESET FOR REP RECS ONLY                     
         CLI   OPTNBYTE,2                                                       
         BH    ERROPTN                                                          
         BE    AGY20                                                            
AGY15    MVC   INFTITL(4),=C'CODE'                                              
         MVC   INFTITL+8(4),AGYNAME                                             
         MVC   INFTITL+27(48),INFTITL                                           
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
*                                                                               
AGY20    MVC   INFTITL(4),=C'CODE'                                              
         MVC   INFTITL+8(L'AGYNAME),AGYNAME                                     
         MVC   INFTITL+29(L'AGYTTL),AGYTTL                                      
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
         EJECT                                                                  
DIV10    TM    FLTRBYTE,X'FF'      NO FILTERS                                   
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         CLI   OPTNBYTE,0                                                       
         BNE   ERROPTN                                                          
         CLI   REPBYTE,0                                                        
         BNE   ERROPTN                                                          
*                                                                               
         MVC   INFTITL(3),=C'DIV'                                               
         MVC   INFTITL+5(L'DIVNAME),DIVNAME                                     
         MVC   INFTITL+27(48),INFTITL                                           
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
         SPACE 2                                                                
TEM10    TM    FLTRBYTE,X'EF'      ALLOW DIV FILTER                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         CLI   OPTNBYTE,0                                                       
         BNE   ERROPTN                                                          
         CLI   REPBYTE,0                                                        
         BNE   ERROPTN                                                          
*                                                                               
         MVC   INFTITL(L'TEMTTL),TEMTTL                                         
         MVC   INFTITL+27(46),INFTITL                                           
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
         SPACE 2                                                                
SAL10    TM    FLTRBYTE,X'EF'      ALLOW DIV FILTER                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'7E'      ALLOW ACT + OFFICE FILTER                    
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'-X'04'  ALLOW KEYWORD FILTER                       
         BNZ   ANYERR                                                           
         CLI   REPBYTE,0                                                        
         BNE   ERROPTN                                                          
         CLI   OPTNBYTE,1          name OPTION ALLOWED                          
         BE    SAL20                                                            
         CLI   OPTNBYTE,12         MANAGER OPTION ALLOWED                       
         BE    SAL20                                                            
         CLI   OPTNBYTE,0                                                       
         BNE   ERROPTN                                                          
SAL20    EQU   *                                                                
*                                                                               
         MVC   INFTITL(L'SALTTL),SALTTL                                         
         MVC   INFTITL+49(L'SALTTL2),SALTTL2                                    
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
         EJECT                                                                  
REG10    TM    FLTRBYTE,X'FF'      NO FILTERS                                   
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         CLI   OPTNBYTE,0                                                       
         BNE   ERROPTN                                                          
         CLI   REPBYTE,0                                                        
         BNE   ERROPTN                                                          
*                                                                               
         MVC   INFTITL(4),=C'CODE'                                              
         MVC   INFTITL+6(L'REGTTL),REGTTL                                       
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
         EJECT                                                                  
         SPACE 2                                                                
STAT0020 EQU   *                                                                
         CLC   =X'FFFF',REPMAST    MASTER CALLING FACILITY?                     
         BNE   STAT0030            NO                                           
         SR    R3,R3                                                            
         MVC   INFMESS(L'YESMASTR),YESMASTR                                     
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
YESMASTR DC    C'Signed On As MASTER REP.  REJECTED.'                           
         DS    0F                                                               
STAT0030 EQU   *                                                                
         TM    FLTRBYTE,X'5E'      ALLOW MEDIA,GROUP,AFF FILTERS                
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'61'      TVB,OWNR,RNK,TRFC,ACT FILTS                  
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FC'      OFFTEAM,TEAM FILTS                           
         BNZ   ANYERR                                                           
         CLI   REPBYTE,0                                                        
         BNE   ERROPTN                                                          
*                                                                               
*                                  DATE/COMP OPTIONS HAVE PRIORITY              
         MVC   INFTITL(L'STATTL),STATTL                                         
*                                                                               
         CLI   OPTNBYTE,14         COMPETITIVE STATION OPTION?                  
         BNE   STAT0040            YES                                          
         MVC   INFTITL+52(L'STATTL2),STATTL2                                    
         B     STAT0220                                                         
STAT0040 EQU   *                                                                
         CLI   OPTNBYTE,15         FORMER REP/NEW REP OPTION?                   
         BNE   STAT0050            NO                                           
         MVC   INFTITL+52(L'STATTL10),STATTL10                                  
         B     STAT0220                                                         
STAT0050 EQU   *                                                                
         CLI   OPTNBYTE,13         JOIN/LEAVE DATE OPTION?                      
         BNE   STAT0060            NO                                           
         MVC   INFTITL+52(L'STATTL2A),STATTL2A                                  
         B     STAT0220                                                         
STAT0060 EQU   *                                                                
         CLI   OPTNBYTE,17         COMBO PARENTS OPTION?                        
         BNE   STAT0062            NO                                           
*                                                                               
         TM    FLTRBYTE,X'FF'      NO FILTERS                                   
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
*                                                                               
         XC    INFTITL,INFTITL                                                  
         MVC   INFTITL(L'STATTL31),STATTL31                                     
         B     STAT0220                                                         
STAT0062 EQU   *                                                                
         CLI   OPTNBYTE,18         Multiple COMBO PARENTS OPTION?               
         BNE   STAT0064            NO                                           
*                                                                               
         TM    FLTRBYTE,X'FF'      NO FILTERS                                   
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
*                                                                               
         XC    INFTITL,INFTITL                                                  
         MVC   INFTITL(L'STATTL31),STATTL31                                     
         B     STAT0220                                                         
STAT0064 EQU   *                                                                
         CLI   OPTNBYTE,19         MARKET LIST OPTION?                          
         BNE   STAT0066            NO                                           
*                                                                               
         TM    FLTRBYTE,X'FF'      NO FILTERS                                   
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'-X'80' AC= FILTER  ALLOWED                         
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
*                                                                               
         XC    INFTITL,INFTITL                                                  
         MVC   INFTITL(L'STATTL32),STATTL32                                     
         B     STAT0220                                                         
STAT0066 EQU   *                                                                
         CLI   OPTNBYTE,20         OWNER LIST OPTION?                           
         BNE   STAT0068            NO                                           
*                                                                               
         TM    FLTRBYTE,X'FF'      NO FILTERS                                   
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
*                                                                               
         XC    INFTITL,INFTITL                                                  
         MVC   INFTITL(L'STATTL33),STATTL33                                     
         B     STAT0220                                                         
STAT0068 EQU   *                                                                
*                                                                               
         CLI   OPTNBYTE,2                                                       
         BE    STAT0100                                                         
         CLI   OPTNBYTE,3                                                       
         BE    STAT0120                                                         
         CLI   OPTNBYTE,4                                                       
         BE    STAT0140                                                         
         CLI   OPTNBYTE,5                                                       
         BE    STAT0160                                                         
         CLI   OPTNBYTE,7                                                       
         BE    STAT0200                                                         
         CLI   OPTNBYTE,6                                                       
         BE    STAT0180                                                         
*                                                                               
         LA    RF,SVPGPBIT         CHECK FOR JOIN/LEAVE DATE                    
         TM    0(RF),X'80'         INF PROF # 1 ON?                             
         BO    STAT0080                                                         
         MVC   INFTITL+52(L'STATTL2),STATTL2                                    
         B     STAT0220                                                         
STAT0080 EQU   *                                                                
         MVC   INFTITL+52(L'STATTL2A),STATTL2A                                  
         B     STAT0220                                                         
*                                                                               
STAT0100 EQU   *                                                                
         MVC   INFTITL+49(L'STATTL3),STATTL3                                    
         B     STAT0220                                                         
*                                                                               
STAT0120 EQU   *                                                                
         MVC   INFTITL+49(L'STATTL4),STATTL4                                    
         B     STAT0220                                                         
*                                                                               
STAT0140 EQU   *                                                                
         MVC   INFTITL+49(L'STATTL5),STATTL5                                    
         B     STAT0220                                                         
*                                                                               
STAT0160 EQU   *                                                                
         MVC   INFTITL,SPACES                                                   
         MVC   INFTITL(L'STATTL6),STATTL6                                       
         MVC   INFTITL+38(L'STATTL6A),STATTL6A                                  
         B     STAT0220                                                         
*                                                                               
STAT0180 EQU   *                                                                
         MVC   INFTITL(L'STATTL),SPACES                                         
*                                       CLEAR TITLE TO PERMIT NEW ONE           
         MVC   INFTITL(L'STATTL9),STATTL9                                       
         MVC   INFTITL+25(L'STATTL7),STATTL7                                    
         B     STAT0220                                                         
*                                                                               
STAT0200 EQU   *                                                                
         MVC   INFTITL+29(L'STATTL8),STATTL8                                    
         B     STAT0220                                                         
*                                                                               
STAT0220 EQU   *                                                                
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
**>>>>                                                                          
*                                                                               
STAM0020 EQU   *                                                                
         CLC   =X'FFFF',REPMAST    MASTER CALLING FACILITY?                     
         BE    STAM0040            YES                                          
         SR    R3,R3                                                            
         MVC   INFMESS(L'NOTMASTR),NOTMASTR                                     
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
NOTMASTR DC    C'Not Signed On As MASTER REP.  REJECTED.'                       
         DS    0F                                                               
STAM0040 EQU   *                                                                
         TM    FLTRBYTE,X'5E'      ALLOW MEDIA,GROUP,AFF FILTERS                
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'61'      TVB,OWNR,RNK,TRFC,ACT FILTS                  
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FC'      OFFTEAM,TEAM FILTS                           
         BNZ   ANYERR                                                           
         CLI   REPBYTE,0                                                        
         BNE   ERROPTN                                                          
*                                                                               
*                                  STATION MASTER HEADING                       
         MVC   INFTITL(LSTAMTTL),STAMTTL                                        
*                                                                               
*                                                                               
STAM0220 EQU   *                                                                
         MVI   STAMUSED,C'Y'       SET 'STAM SCREEN WAS LAST'                   
         B     CALLOVER                                                         
**>>>>                                                                          
         EJECT                                                                  
OFF10    TM    FLTRBYTE,X'F7'      ALLOW REGION FILTER                          
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'F7'      ALLOW SCOPE FILTER                           
         BNZ   ANYERR                                                           
         CLI   REPBYTE,0                                                        
         BNE   ERROPTN                                                          
         CLI   OPTNBYTE,4          FAX OPTION?                                  
         BE    OFF30                                                            
*                                                                               
OFF20    DS    0H                                                               
         MVC   INFTITL(L'OFFTTL),OFFTTL                                         
         MVC   INFTITL+66(L'OFFTTL2),OFFTTL2                                    
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
*                                                                               
OFF30    DS    0H                                                               
         MVC   INFTITL(L'OFFAXTTL),OFFAXTTL                                     
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
*****                                                                           
TER10    TM    FLTRBYTE,X'FF'      ALLOW REGION FILTER                          
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         CLI   REPBYTE,0                                                        
         BNE   ERROPTN                                                          
*                                                                               
TER20    DS    0H                                                               
         MVC   INFTITL(L'TERTTL),TERTTL                                         
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
         SPACE 2                                                                
PRD10    TM    FLTRBYTE,X'FB'      ALLOW ADVERTISER FILTER                      
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'9F'      ALLOW POINT PERSON, NET CON#                 
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         CLI   REPBYTE,0                                                        
         BNE   *+8                                                              
         MVI   REPBYTE,2           PRESET FOR THIS REPS RECORDS ONLY            
         CLI   OPTNBYTE,0                                                       
         BE    PRD12               NO OPTION SELECTED                           
         CLI   OPTNBYTE,8          NETWORK DISPLAY OPTION?                      
         BE    PRD14                                                            
         CLI   OPTNBYTE,9          SPOT DISPLAY OPTION?                         
         BE    PRD15                                                            
         CLI   OPTNBYTE,16         AGENCY FLIGHT DATES OPTION?                  
         BE    PRD16                                                            
         B     ERROPTN             INVALID OPTION CHOSEN                        
*                                                                               
PRD12    MVC   INFTITL(4),=C'CODE'                                              
         MVC   INFTITL+9(L'PRDTTL),PRDTTL                                       
         MVC   INFTITL+41(38),INFTITL                                           
         B     PRD17                                                            
*                                                                               
PRD14    MVC   INFTITL(4),=C'CODE'                                              
         MVC   INFTITL+5(L'NETTTL),NETTTL                                       
         MVC   INFTITL+39(33),INFTITL                                           
         B     PRD17                                                            
*                                                                               
PRD15    MVC   INFTITL(4),=C'CODE'                                              
         MVC   INFTITL+5(LSPTTTL),SPTTTL                                        
         B     PRD17                                                            
*                                                                               
PRD16    EQU   *                                                                
         MVC   INFTITL(LDATEFLT),DATEFLT                                        
*                                                                               
PRD17    FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
         EJECT                                                                  
***THIS CODE IS USED FOR AGY AND AOF***                                         
         SPACE 2                                                                
AOF10    DS    0H                                                               
         TM    FLTRBYTE,X'FD'      ALLOW AGENCY FILTER                          
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FE'      ONLY OFFICE FILTER                           
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'F3'      ALLOW KEYWORD SEARCH AND SCOPE FLT           
         BNZ   ANYERR                                                           
         CLI   REPBYTE,0                                                        
         BNE   *+8                                                              
         MVI   REPBYTE,2           PRESET FOR REP RECS ONLY                     
         CLI   OPTNBYTE,10         RISK OPTION                                  
         BE    AOF30                                                            
         CLI   OPTNBYTE,11         LIAB OPTION                                  
         BE    AOF40                                                            
         CLI   OPTNBYTE,4          FAX OPTION                                   
         BE    AOF50                                                            
         CLI   OPTNBYTE,21         TERRITORY (x'aa') OPTION                     
         BNE   AOF14                                                            
*                                                                               
AOF12    EQU   *                                                                
         TM    FLTRBYTE,X'FF'      NO FILTERS                                   
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'                                                   
         BNZ   ANYERR                                                           
         B     AOF60                                                            
*                                                                               
AOF14    DS    0H                                                               
         CLI   OPTNBYTE,22         AGENCY W/TERR                                
         BE    AOF16                                                            
         CLI   OPTNBYTE,23         AGENCY W/O TERR                              
         BE    AOF16                                                            
         CLI   OPTNBYTE,24         AGENCY W OR W/O TERR                         
         BE    AOF16                                                            
         CLI   OPTNBYTE,2                                                       
         BH    ERROPTN                                                          
         BE    AOF20                                                            
         B     AOF18                                                            
AOF16    EQU   *                                                                
         MVC   INFTITL+34(4),AGYTERR                                            
*                                  INSERT 'TERR'                                
*                                                                               
AOF18    MVC   INFTITL(7),=C'AGY/OFF'                                           
         MVC   INFTITL+9(4),AGYNAME                                             
         MVC   INFTITL+40(38),INFTITL                                           
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
*                                                                               
AOF20    MVC   INFTITL(7),=C'AGY/OFF'                                           
         MVC   INFTITL+9(L'AGYNAME),AGYNAME                                     
         MVC   INFTITL+31(L'AGYTTL2),AGYTTL2                                    
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
*                                                                               
AOF30    MVC   INFTITL(LAGYRTTL),AGYRTTL                                        
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
*                                                                               
AOF40    MVC   INFTITL(LAGYLTTL),AGYLTTL                                        
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
*                                                                               
AOF50    MVC   INFTITL(LAGYXTTL),AGYXTTL                                        
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
*                                                                               
AOF60    MVC   INFTITL(L'AGYTTLAA),AGYTTLAA                                     
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
         EJECT                                                                  
CAT10    TM    FLTRBYTE,X'BF'      ALLOW CLASS FILTER                           
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         CLI   REPBYTE,0                                                        
         BNE   ERROPTN                                                          
         CLI   OPTNBYTE,0                                                       
         BNE   ERROPTN                                                          
*                                                                               
         MVC   INFTITL(4),=C'CODE'                                              
         MVC   INFTITL+10(L'CATTTL),CATTTL                                      
         MVC   INFTITL+35(3),=C'CLS'                                            
         MVC   INFTITL+41(38),INFTITL                                           
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
         SPACE 2                                                                
CLS10    CLI   FLTRBYTE,0          NO FILTERS                                   
         BNE   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         CLI   OPTNBYTE,0                                                       
         BNE   ERROPTN                                                          
         CLI   REPBYTE,0                                                        
         BNE   ERROPTN                                                          
*                                                                               
         MVC   INFTITL(4),=C'CODE'                                              
         MVC   INFTITL+10(L'CLSTTL),CLSTTL                                      
         MVC   INFTITL+41(38),INFTITL                                           
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
         SPACE 3                                                                
GRP10    TM    FLTRBYTE,X'FF'      NO FILTERS                                   
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         CLI   OPTNBYTE,0                                                       
         BNE   ERROPTN                                                          
         CLI   REPBYTE,0                                                        
         BNE   ERROPTN                                                          
*                                                                               
         MVC   INFTITL(4),=C'CODE'                                              
         MVC   INFTITL+6(L'GRPTTL),GRPTTL                                       
         MVC   INFTITL+41(38),INFTITL                                           
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
         EJECT                                                                  
OWN10    TM    FLTRBYTE,X'FF'      NO FILTERS                                   
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         CLI   OPTNBYTE,0                                                       
         BNE   ERROPTN                                                          
         CLI   REPBYTE,0                                                        
         BNE   ERROPTN                                                          
*                                                                               
         MVC   INFTITL(4),=C'CODE'                                              
         MVC   INFTITL+6(L'OWNNAME),OWNNAME                                     
         MVC   INFTITL+27(49),INFTITL                                           
         FOUT  INFTITLH                                                         
         B     CALLOVER                                                         
         EJECT                                                                  
*                                                                               
*- DEMO MENU RECORD                                                             
*                                                                               
*  NO VALID FILTERS OR OPTIONS                                                  
*                                                                               
DMN10    TM    FLTRBYTE,X'FF'      NO FILTERS                                   
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         CLI   OPTNBYTE,0          NO VALID OPTIONS                             
         BNE   ANYERR                                                           
         CLI   REPBYTE,0           REP NOT ALLOWED                              
         BNE   ERROPTN                                                          
*                                                                               
*- MOVE TITLE LITERALS INTO SCREEN                                              
         MVC   INFTITL(L'DMNTTL),DMNTTL                                         
         FOUT  INFTITLH            TRANSMIT SCREEN FIELD                        
*                                                                               
         B     CALLOVER                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
*- STANDARD COMMENT RECORD                                                      
*                                                                               
*  NO FILTERS OR OPTIONS                                                        
*                                                                               
CMT10    TM    FLTRBYTE,X'FF'      NO FILTERS                                   
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         CLI   OPTNBYTE,0          NO OPTIONS ALLOWED                           
         BNZ   ANYERR                                                           
         CLI   REPBYTE,0           REP NOT ALLOWED                              
         BNE   ERROPTN                                                          
*                                                                               
*- MOVE TITLE LITERALS INTO SCREEN                                              
         MVC   INFTITL(L'CMTTTL),CMTTTL                                         
         FOUT  INFTITLH            TRANSMIT SCREEN FIELD                        
         B     CALLOVER                                                         
         SPACE 2                                                                
*                                                                               
*- REP RECORD                                                                   
*                                                                               
*  NO FILTERS OR OPTIONS                                                        
*  DDS TERMINALS ONLY                                                           
*                                                                               
REP10    TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         CLI   OPTNBYTE,0          NO OPTIONS ALLOWED                           
         BNZ   ANYERR                                                           
         CLI   REPBYTE,0           REP NOT ALLOWED                              
         BNE   ERROPTN                                                          
         L     RE,VTWA             A(TWA)                                       
         USING TWAD,RE                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    REP20                                                            
         DROP  RE                                                               
*                                                                               
         LA    R3,RCRDERR          INVALID REC MSG                              
         LA    R2,INFRCRDH         RECORD FIELD                                 
         B     ANYERR                                                           
*                                                                               
*- MOVE TITLE LITERALS INTO SCREEN                                              
REP20    MVC   INFTITL(LREPTTL),REPTTL                                          
         FOUT  INFTITLH            TRANSMIT SCREEN FIELD                        
         B     CALLOVER                                                         
         SPACE                                                                  
*                                                                               
*- @@** RECORD (SPECIAL FIX OVERLAY)                                            
*                                                                               
*  DDS TERMINALS ONLY                                                           
*                                                                               
FIX10    EQU   *                                                                
         L     RE,VTWA             A(TWA)                                       
         USING TWAD,RE                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    FIX20                                                            
         DROP  RE                                                               
*                                                                               
         LA    R3,RCRDERR          INVALID REC MSG                              
         LA    R2,INFRCRDH         RECORD FIELD                                 
         B     ANYERR                                                           
*                                                                               
*- MOVE TITLE LITERALS INTO SCREEN                                              
FIX20    MVC   INFTITL(LFIXTTL),FIXTTL                                          
         FOUT  INFTITLH            TRANSMIT SCREEN FIELD                        
         B     CALLOVER                                                         
         SPACE 2                                                                
*                                                                               
*- POINT PERSON RECORD                                                          
*                                                                               
*  ACTIVE/BOTH FILTER                                                           
*                                                                               
PTP10    TM    FLTRBYTE,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'7F'      PERMIT AC= FILTER                            
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      no extra filters                             
         BNZ   ANYERR                                                           
         CLI   OPTNBYTE,0          NO OPTIONS ALLOWED                           
         BNZ   ANYERR                                                           
         CLI   REPBYTE,0           REP NOT ALLOWED                              
         BNE   ERROPTN                                                          
*                                                                               
*- MOVE TITLE LITERALS INTO SCREEN                                              
         MVC   INFTITL(LPTPTTL),PTPTTL                                          
         FOUT  INFTITLH            TRANSMIT SCREEN FIELD                        
         B     CALLOVER                                                         
         SPACE 2                                                                
*                                                                               
*- CONTRACT TYPE RECORD                                                         
*                                                                               
*  NO FILTERS OR OPTIONS                                                        
*                                                                               
CTY10    TM    FLTRBYTE,X'FF'      NO FILTERS                                   
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         CLI   OPTNBYTE,0          NO OPTIONS ALLOWED                           
         BNZ   ANYERR                                                           
         CLI   REPBYTE,0           REP NOT ALLOWED                              
         BNE   ERROPTN                                                          
*                                                                               
*- MOVE TITLE LITERALS INTO SCREEN                                              
         MVC   INFTITL(L'CTYTTL),CTYTTL                                         
         FOUT  INFTITLH            TRANSMIT SCREEN FIELD                        
         B     CALLOVER                                                         
*                                                                               
*- DEVELOPMENTAL SALESPERSON TYPE RECORD                                        
*                                                                               
*  ACTIVE/BOTH FILTER                                                           
*                                                                               
DSP10    TM    FLTRBYTE,X'FF'      NO FILTERS                                   
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'7F'      PERMIT AC= FILTER                            
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      no extra filters                             
         BNZ   ANYERR                                                           
         CLI   OPTNBYTE,0          NO OPTIONS ALLOWED                           
         BNZ   ANYERR                                                           
         CLI   REPBYTE,0           REP NOT ALLOWED                              
         BNE   ERROPTN                                                          
*                                                                               
*- MOVE TITLE LITERALS INTO SCREEN                                              
         MVC   INFTITL(LDSPTTL),DSPTTL                                          
         FOUT  INFTITLH            TRANSMIT SCREEN FIELD                        
         B     CALLOVER                                                         
*                                                                               
*- DEVELOPMENTAL CONTRACT TYPE RECORD                                           
*                                                                               
*  NO FILTERS OR OPTIONS                                                        
*                                                                               
DCT10    TM    FLTRBYTE,X'FF'      NO FILTERS                                   
         BNZ   ANYERR                                                           
         TM    FLTRBYT2,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         TM    FLTRBYT3,X'FF'      NO EXTRA FILTERS                             
         BNZ   ANYERR                                                           
         CLI   OPTNBYTE,0          NO OPTIONS ALLOWED                           
         BNZ   ANYERR                                                           
         CLI   REPBYTE,0           REP NOT ALLOWED                              
         BNE   ERROPTN                                                          
*                                                                               
*- MOVE TITLE LITERALS INTO SCREEN                                              
         MVC   INFTITL(L'DCTTTL),DCTTTL                                         
         FOUT  INFTITLH            TRANSMIT SCREEN FIELD                        
         B     CALLOVER                                                         
         EJECT                                                                  
CLRRCRD  EQU   *                                                                
CLRSTRT  EQU   *                                                                
         MVI   STRTBYTE,0                                                       
         NI    INFSTRTH+4,X'DF'    TURN OFF VALIDATED PREVIOUSLY                
CLRFLTR  XC    FLTRBYTE(LFLTRS),FLTRBYTE                                        
         NI    INFFLTRH+4,X'DF'    TURN OFF VALIDATED PREVIOUSLY                
CLROPTN  MVI   OPTNBYTE,0                                                       
         MVI   REPBYTE,0                                                        
         MVI   OPT2BYTE,0                                                       
         NI    INFOPTNH+4,X'DF'    TURN OFF VALIDATED PREVIOUSLY                
         MVI   NEXTBYTE,0                                                       
         BR    RE                                                               
         SPACE 2                                                                
ERRFLD   LA    R3,FLDERR                                                        
         B     ANYERR                                                           
         SPACE 2                                                                
ERROPTN  LA    R3,OPTNERR2                                                      
         LA    R2,INFOPTNH                                                      
         B     ANYERR                                                           
         SPACE 2                                                                
ANYERR   LR    R4,R2                                                            
         LA    R2,INFTITLH                                                      
         BAS   RE,CLEAR                                                         
         LR    R2,R4                                                            
         B     ERROR                                                            
         SPACE 2                                                                
CLEAR    CLI   0(R2),0                                                          
         BER   RE                                                               
         SR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,OCLINE                                                        
         BZ    CLEAR2                                                           
         EX    R1,XCLINE                                                        
         FOUT  (R2)                                                             
CLEAR2   LA    R2,9(R2,R1)                                                      
         B     CLEAR                                                            
         SPACE 1                                                                
OCLINE   OC    8(0,R2),8(R2)                                                    
*                                                                               
XCLINE   XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
TSTOPTN  CLC   0(4,R6),=C'NAME'                                                 
         BNE   *+10                                                             
         MVI   OPTNBYTE,1                                                       
         BR    RE                                                               
*                                                                               
         CLC   0(4,R6),=C'CODE'                                                 
         BNE   *+10                                                             
         MVI   OPTNBYTE,2                                                       
         BR    RE                                                               
*                                                                               
         CLC   0(3,R6),=C'ACE'                                                  
         BNE   *+10                                                             
         MVI   OPTNBYTE,3                                                       
         BR    RE                                                               
*                                                                               
         CLC   0(3,R6),=C'FAX'                                                  
         BNE   *+10                                                             
         MVI   OPTNBYTE,4                                                       
         BR    RE                                                               
*                                                                               
         CLC   0(6,R6),=C'COMBO+'  COMBO+ MUST BE CHECKED                       
         BNE   *+10                   BEFORE COMBO -                            
         MVI   OPTNBYTE,7                                                       
         BR    RE                                                               
*                                                                               
         CLC   0(5,R6),=C'COMBO'                                                
         BNE   *+10                                                             
         MVI   OPTNBYTE,5                                                       
         BR    RE                                                               
*                                                                               
         CLC   0(5,R6),=C'INTER'                                                
         BNE   *+10                                                             
         MVI   OPTNBYTE,6                                                       
         BR    RE                                                               
*                                                                               
         CLC   0(2,R6),=C'EI'                                                   
         BNE   *+10                                                             
         MVI   OPT2BYTE,7          SET A SEPARATE OPT BYTE                      
         BR    RE                                                               
*                                                                               
         CLC   0(3,R6),=C'NET'                                                  
         BNE   *+10                                                             
         MVI   OPTNBYTE,8                                                       
         BR    RE                                                               
*                                                                               
         CLC   0(4,R6),=C'SPOT'                                                 
         BNE   *+10                                                             
         MVI   OPTNBYTE,9                                                       
         BR    RE                                                               
*                                                                               
         CLC   0(4,R6),=C'RISK'                                                 
         BNE   *+10                                                             
         MVI   OPTNBYTE,10                                                      
         BR    RE                                                               
*                                                                               
         CLC   0(4,R6),=C'LIAB'                                                 
         BNE   *+10                                                             
         MVI   OPTNBYTE,11                                                      
         BR    RE                                                               
*                                                                               
         CLC   0(3,R6),=C'MGR'                                                  
         BNE   *+10                                                             
         MVI   OPTNBYTE,12                                                      
         BR    RE                                                               
*                                                                               
         CLC   0(4,R6),=C'DATE'                                                 
         BNE   *+10                                                             
         MVI   OPTNBYTE,13                                                      
         BR    RE                                                               
*                                                                               
         CLC   0(4,R6),=C'COMP'                                                 
         BNE   *+10                                                             
         MVI   OPTNBYTE,14                                                      
         BR    RE                                                               
*                                                                               
         CLC   0(3,R6),=C'AGY'                                                  
         BNE   *+10                                                             
         MVI   OPTNBYTE,16                                                      
         BR    RE                                                               
*                                                                               
         CLC   0(7,R6),=C'PARENT+'                                              
         BNE   *+10                                                             
         MVI   OPTNBYTE,18                                                      
         BR    RE                                                               
*                                                                               
         CLC   0(6,R6),=C'PARENT'                                               
         BNE   *+10                                                             
         MVI   OPTNBYTE,17                                                      
         BR    RE                                                               
*                                                                               
         CLC   0(6,R6),=C'MARKET'                                               
         BNE   *+10                                                             
         MVI   OPTNBYTE,19                                                      
         BR    RE                                                               
*                                                                               
         CLC   0(5,R6),=C'OWNER'                                                
         BNE   *+10                                                             
         MVI   OPTNBYTE,20                                                      
         BR    RE                                                               
*                                                                               
*                                                                               
         CLC   0(4,R6),=C'TER+'    AGENCIES WITH TERRITORY                      
         BNE   *+10                                                             
         MVI   OPTNBYTE,22                                                      
         BR    RE                                                               
*                                                                               
         CLC   0(4,R6),=C'TER-'    AGENCIES W/O  TERRITORY                      
         BNE   *+10                                                             
         MVI   OPTNBYTE,23                                                      
         BR    RE                                                               
*                                                                               
         CLC   0(4,R6),=C'TERB'    AGENCIES + TERR/GENERAL                      
         BNE   *+10                                                             
         MVI   OPTNBYTE,24                                                      
         BR    RE                                                               
*                                                                               
         CLC   0(3,R6),=C'TER'                                                  
         BNE   *+10                                                             
         MVI   OPTNBYTE,21                                                      
         BR    RE                                                               
*                                                                               
*** OPTNBYTE = 15 IS DONE BELOW REPBYTE = 3 (SINCE 1ST 3 LETTERS ON             
*** BOTH ARE 'REP')                                                             
*                                                                               
         CLC   0(4,R6),=C'STND'                                                 
         BNE   *+10                                                             
         MVI   REPBYTE,1                                                        
         BR    RE                                                               
*                                                                               
         CLC   0(4,R6),=C'BOTH'                                                 
         BNE   *+10                                                             
         MVI   REPBYTE,3                                                        
         BR    RE                                                               
*                                                                               
         CLC   0(4,R6),=C'REPS'                                                 
         BNE   *+10                                                             
         MVI   REPBYTE,2                                                        
         BR    RE                                                               
*                                                                               
         CLC   0(3,R6),=C'REP'                                                  
         BNE   *+10                                                             
         MVI   OPTNBYTE,15                                                      
         BR    RE                                                               
                                                                                
         CLC   0(5,R6),=C'EQUIV'    DISPLAY KATZ CODES OPTION                   
         BNE   ANYERR                                                           
****>    CLI   ISKATZ,X'FF'         ONLY FOR KATZ OR SUBSIDIARIES               
****>    BNE   ANYERR                                                           
         CLI   LINK,0               ONLY FOR ADV RECORDS                        
         BNE   ANYERR                                                           
         MVI   OPTNBYTE,15                                                      
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*    PROTECTS/UNPROTECTS OUTPUT FIELDS  BASED ON SWAP CAPABILITY *              
******************************************************************              
PROTFLD  NTR1                                                                   
         OI    INFPLEGH+1,X'04'    INIT LEGEND TO ZERO INTENSITY                
         OI    INFPLEGH+6,X'80'    TRANSMIT IT                                  
         SR    R0,R0               INIT R0 FLAG (REC NOT SWAP ABLE)             
*                                                                               
         CLI   LINK,X'5C'          IF STATION MASTER                            
         BNE   PROT05                                                           
         LHI   R0,1                ALWAYS UNPROTECT FIELDS                      
         B     PROT50               (SPECIAL SWAP RULES)                        
*                                                                               
PROT05   CLI   SVPGP#,RREPQINF     IF PROFILE INFO NOT IN TWA                   
         BNE   PROT50              THEN DON'T ENABLE SWAP                       
*                                   ELSE,                                       
         TM    SVPGPBIT,SVSWAPQ    IF PROF NOT SET TO ALLOW SWAP                
         BZ    PROT50              THEN DON'T ENABLE SWAP                       
*                                                                               
         CLI   OPTNBYTE,0          IF OPTIONS ENABLED                           
         BNE   PROT50              SKIP TABLE SEARCH BECAUSE SWAP               
*                                  CAN NOT BE ENABLED                           
         LA    R1,SWAPTAB                                                       
PROT10   CLI   0(R1),X'FF'                                                      
         BE    PROT50              NO ENTRY FOUND FOR THIS REC TYPE             
         CLC   LINK,0(R1)                                                       
         BE    PROT45                                                           
         LA    R1,L'SWAPTAB(R1)                                                 
         B     PROT10                                                           
*                                                                               
PROT45   LHI   R0,1                MARK FLAG  FOR RECORD SWAP CAPABLE           
         NI    INFPLEGH+1,X'FF'-X'04' PFKEY LEGEND SET TO NORM INTENS.          
*                                                                               
PROT50   FOUT  INFTITLH                                                         
         LA    R2,INFOUTH                                                       
         LA    R3,15                                                            
PROT100  CHI   R0,1                IF RECORD SWAP CAPABLE                       
         BNE   PROT120                                                          
*                                                                               
         NI    1(R2),X'FF'-X'20'   UNPROTECT ALL LINES  -4/4/00 CHANGE          
*                                  ALLOWS USER TO TAB THROUGH SCREEN            
         B     PROT140                                                          
*                                                                               
PROT120  OI    1(R2),X'20'         ELSE,TURN ON  'PROTECTED' BIT                
*                                                                               
PROT140  FOUT  (R2)                                                             
         ZIC   R5,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R5                                                            
         BCT   R3,PROT100                                                       
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*****                    *****                                                  
*         MAIN EXIT          *                                                  
*****                    *****                                                  
*                                                                               
MAINXIT  OI    INFNEXTH+6,OI1C      POSITION CURSOR                             
         OI    INFRCRDH+6,X'81'     FORCE INP FLG/AVOID NO INP ERR              
         L     RD,BASERD            RESTORE MAIN PRG RD                         
         XIT1                       AND EXIT                                    
         EJECT                                                                  
*                                                                               
*                                                                               
ADVNAME  DC    C'ADVERTISER NAME'                                               
ADVTTL   DC    C'ADVERTISER CITY    CLASS  CATG'                                
AGYNAME  DC    C'NAME'                                                          
AGYTERR  DC    C'TERR'                                                          
AGYTTL   DC    C'ADDRESS                                   ST ZIP'              
AGYTTL2  DC    C'ADDRESS               ST  ZIP'                                 
AGYTTLAA DC    C'TERRITORY  AGENCIES'                                           
AGYRTTL  DC    C'CODE  NAME                               RISK'                 
         DC    C' CATEGORY/DESCRIPTION'                                         
LAGYRTTL EQU   *-AGYRTTL                                                        
AGYLTTL  DC    C'CODE  NAME                               LIAB'                 
         DC    C' POSITION/DESCRIPTION'                                         
LAGYLTTL EQU   *-AGYLTTL                                                        
AGYXTTL  DC    C'CODE  NAME                               PHONE'                
         DC    C' NUMBER   FAX NUMBER'                                          
LAGYXTTL EQU   *-AGYXTTL                                                        
DIVNAME  DC    C'DIVISION NAME'                                                 
TEMTTL   DC    C'TM  DIV NAME  TEAM NAME'                                       
SALTTL   DC    C'INTL    SALESPERSON         TELEPHONE  TEAM  OFF'              
SALTTL2  DC    C'FAX NUMBER    LEAVE DATE  MGR?'                                
REGTTL   DC    C'REGION NAME'                                                   
***REGTTL   DC    C'REGION NAME           TV HQ   RADIO HQ'                     
STATTL   DC    C'STATION         MARKET        CHANL AFF ACT G/S'               
STATTL2  DC    C'COMPETING STATIONS'                                            
STATTL2A DC    C'JOIN DATE LEAVE DATE'                                          
STATTL3  DC    C'OWNER  TVB  RANK  TRAFFIC'                                     
STATTL4  DC    C' REC ID /SIGN ON /DEST ID'                                     
STATTL5  DC    C'     FAX           TWX'                                        
STATTL6  DC    C'STATION         MARKET       ACT G/S'                          
STATTL6A DC    C'COMBO STATIONS'                                                
STATTL7  DC    C'ACT G/S INTERFACE  COMBO PARTICIPANTS'                         
STATTL8  DC    C'ACT G/S  COMBO PARENT'                                         
STATTL9  DC    C'STATION      MARKET '                                          
STATTL10 DC    C'FORMER REP   NEW REP'                                          
*                                                                               
STATTL31 DC    C'STATION     COMBO PARENTS'                                     
STATTL32 DC    C'MARKET NAME              STATIONS'                             
STATTL33 DC    C'OWNER CODE/NAME              STATIONS'                         
*                                                                               
STAMTTL  DC    C'ADDTO STATION         MARKET        AFF OWN G/S '              
STAMTTL2 DC    C'JOINED   LEFT     USERID  ALPHA'                               
LSTAMTTL EQU   *-STAMTTL                                                        
*                                                                               
OFFTTL   DC    C'OF RG NAME             N/L ADDRESS'                            
OFFTTL2  DC    C'ST ZIP'                                                        
OFFAXTTL DC    C'OF RG NAME             N/L FAX NUMBER'                         
TERTTL   DC    C'CODE  TERRITORY'                                               
PRDTTL   DC    C'PRODUCT NAME     ADV  CLS CTG'                                 
NETTTL   DC    C'ADV  POINT NET CON#  DESCRIPTION'                              
SPTTTL   DC    C'PRODUCT NAME          ADV  POINT NET CON#'                     
         DC    C'  DESCRIPTION   SPTCLT PRD(S) EST'                             
LSPTTTL  EQU   *-SPTTTL                                                         
CATTTL   DC    C'CATEGORY NAME'                                                 
CLSTTL   DC    C'CLASS NAME'                                                    
GRPTTL   DC    C'GROUP NAME SUBGROUP NAME'                                      
OWNNAME  DC    C'OWNERSHIP NAME'                                                
DATEFLT  DC    C'CODE PRODUCT NAME         ADV  AGENCY'                         
         DC    C'                         FLIGHT DATES'                         
LDATEFLT EQU   *-DATEFLT                                                        
*                                                                               
DMNTTL   DC    C'Code  Description'                                             
*                                                                               
CMTTTL   DC    C'Comment Code    First Comment Line'                            
*                                                                               
REPTTL   DC    C'Id  Full Name                          Short Name    '         
         DC    C'       Prnt'                                                   
LREPTTL  EQU   *-REPTTL                                                         
*                                                                               
FIXTTL   DC    C'RECORD FIXER'                                                  
LFIXTTL  EQU   *-FIXTTL                                                         
*                         1         2         3                                 
*                1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.                       
PTPTTL   DC    C'CODE NAME                 TELEPHONE    '                       
         DC    C'REP OFF SALESPERSON           LV DATE'                         
LPTPTTL  EQU   *-PTPTTL                                                         
*                                                                               
CTYTTL   DC    C'Code  Description           Created   Last Changed'            
*                                                                               
DSPTTL   DC    C'Code  Salesperson Name      Telephone      Fax    '            
         DC    C'       Leave Date'                                             
LDSPTTL  EQU   *-DSPTTL                                                         
*                                                                               
DCTTTL   DC    C'Code  Contract Type Description                   '            
*                                                                               
INVPOSIT DC    C'CURSOR NOT IN VALID POSITION FOR ACTION'                       
*                                                                               
NOTAVAIL DC    C'FUNCTION NOT AVAILABLE WITH OPTIONS'                           
*                                                                               
FUNCTREC DC    C'FUNCTION NOT AVAILABLE FOR THIS RECORD'                        
*                                                                               
         EJECT                                                                  
*                                                                               
*TABLE 1                                                                        
*  LINK                           BYTE 0                                        
*      (DISPLACEMENT INTO BRANCH TABLE)                                         
*  LENGTH OF TABLE ENTRY          BYTE 1                                        
*  NAME OF RECORD                 BYTE 2+                                       
TABLE1   DC    X'000C',C'ADVERTISER'                                            
         DC    X'2408',C'AGENCY'   USE AOF CODE                                 
         DC    X'2405',C'AGY'      USE AOF CODE                                 
         DC    X'080A',C'DIVISION'                                              
         DC    X'0C06',C'TEAM'                                                  
         DC    X'100A',C'SALESMAN'                                              
         DC    X'1408',C'REGION'                                                
         DC    X'180A',C'STATIONS'                                              
         DC    X'5C0B',C'STAMASTER'                                             
         DC    X'1C09',C'OFFICES'                                               
         DC    X'200A',C'PRODUCTS'                                              
         DC    X'2005',C'PRD'                                                   
         DC    X'2408',C'AGYOFF'                                                
         DC    X'2406',C'AOFF'                                                  
         DC    X'280A',C'CATEGORY'                                              
         DC    X'2C07',C'CLASS'                                                 
         DC    X'3007',C'GROUP'                                                 
         DC    X'3005',C'GRP'                                                   
         DC    X'340B',C'OWNERSHIP'                                             
         DC    X'3807',C'DMENU'    DEMO MENU                                    
         DC    X'3C09',C'COMMENT'  STANDARD COMMENT                             
         DC    X'3C05',C'CMT'      ALSO COMMENT                                 
         DC    X'4005',C'REP'      REP RECORD                                   
         DC    X'4406',C'@@**'     SPECIAL FIXER                                
         DC    X'480A',C'POINTPER' POINT PERSON                                 
         DC    X'4C09',C'CONTYPE'  CONTRACT TYPE                                
         DC    X'5009',C'DEVSALE'  DEVELOPMENTAL SALESMAN                       
         DC    X'5409',C'DEVTYPE'  DEVELOPMENTAL CONTRACT TYPE                  
         DC    X'5805',C'TER'      TERRITORY RECORD                             
         DC    2X'00'              END OF LIST                                  
         SPACE 1                                                                
SPACES   DC    CL80' '                                                          
*                                                                               
         EJECT                                                                  
       ++INCLUDE REGENINT                                                       
         EJECT                                                                  
       ++INCLUDE REINFWRKA                                                      
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
       ++INCLUDE FATWA                                                          
       ++INCLUDE REGLFINF                                                       
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE REGLFILEA                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008REINF00S  05/01/02'                                      
         END                                                                    
