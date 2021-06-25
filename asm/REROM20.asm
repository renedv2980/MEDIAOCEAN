*          DATA SET REROM20    AT LEVEL 236 AS OF 04/01/05                      
*PHASE T83F20B                                                                  
*                                                                               
         TITLE 'T83F20 - REROM20 - DARE ORDER OPEN/REJECT'                      
***********************************************************************         
*                                                                     *         
*  REROM20 (T83F20) --- DARE ORDER OPEN/REJECT                        *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* JUN28/04 SKU REMOVE UID SUPPORT                                     *         
* AUG24/04 HQ  FIX CONFLICT END DATE NO 03 ELEMENT BUG                *         
* APR28/04 HQ  STOP ALL ACTIONS ON NON-UPDATEABLE SYSTEM              *         
* FEB20/04 HQ  HANDLE CONFLICT END DATE                               *         
* FEB09/04 SKU PRESERVE 1D FOR UNDARE                                 *         
* OCT03/03 HQ  POPULATE COST OVERRIDE FLAG ON OPEN                    *         
* MAR10/03 HQ  CALLED FROM OVERLAY 22 TO PROCESS RADIO UNDARE         *         
* NOV11/02 SKU ADD REJECT TO BUYER PFKEY                              *         
* JUN10/02 SKU FIX DEMO RATING VALUE STORAGE                          *         
*              STORE DEMO RATING CATETORIES TO CONTRACT HEADER        *         
* MAY20/02 SKU RECORD BUYLINE 1 ADD DATE                              *         
* MAR08/02 SKU FIX UNDARE BUG                                         *         
* JUN01/01 BU  DAILY PACING                                           *         
* JAN04/02 SKU CHANGE APPROVE TO OPEN                                 *         
* NOV07/01 SKU HANDLE ORDER WITH NO BUYS                              *         
* OCT17/01 SKU ADD AUDIT TRAIL                                        *         
* AUG28/01 SKU MOVE EST TO EXPANDED EST LOCATION                      *         
*              REMOVE ALL REFERENCES TO POEM                          *         
* AUG23/01 SKU UNPOEM SHOULD BE UNDARE WITHIN DARE TRANSACTION        *         
* AUG17/01 SKU GET CORRECT START/END DAYS IF ORBITS ARE PRESENT       *         
* JAN31/00 SKU SUPPORT NEW PROGRAM NAME ELEMENT                       *         
* JAN12/01 SKU COMPRESS TIME IF START AND END TIME ARE SAME           *         
* DEC27/00 SKU CHECK FOR NON-BLANK OR NON-NULL IN BUY ROTATION FIELD  *         
* DEC11/00 SKU SET TRADE FLAG                                         *         
* AUG15/00 BU  SET 'BUY ENTERED' FLAG IN RCONMODR                     *         
* JUN28/00 BU  REMOVE REFERENCES TO GLV1GOTO PER MEL HERTZIG          *         
* NOV15/99 SKU FIX TAKEOVER 8D/8E PASSIVE KEY BUG                     *         
* JAN19/98 SKU REMOVE LINK FOR UNDARE ORDERS                          *         
* JUN04/98 SKU SAVE DARE DEMO VALUES                                  *         
* FEB25/98 BU  ALTERNATE CALENDAR UPGRADE                             *         
* FEB04/98 SKU YEAR 2000 COMPLIANT                                    *         
* JUL22/97 SKU AGENCY CHANGES/REVISION SUPPORT. DARE HISTORY          *         
* MAR10/97 SKU SUPPORT VARIOUS/BRAND ORDERS                           *         
*              SUPPRESS STATION BUY ORDER CMT PER PETRY               *         
* OCT07/96 SKU LOW POWER STATION                                      *         
* AUG21/96 SKU TRAP BAD CONTRACT NUM TO GLOBBER                       *         
* JUL23/96 SKU FIX OOWR BUG                                           *         
* JUN03/96 SKU RE-INIT HIATUS COMMENT ELEMENT                         *         
* APR02/96 SKU MISSING X'1D' FIX FOR PETRY ORDERS                     *         
* MAR16/96 SKU UPDATE X'8D', X'8E' KEYS IF FLIGHT DATES DIFFER        *         
* FEB20/96 SKU SHOW POEM INSTEAD OF DARE FOR PETRY (PV)               *         
* JAN30/96 SKU KATZ EDI SUPPORT                                       *         
* JAN17/96 SKU FIX REJ BUG OF WRITING OUT A NULL KEY                  *         
* DEC14/95 SKU ADJUST FOR DDS TIME CHANGE                             *         
* JUN26/95 SKU MARK IF DAILY IN CONTRACT HEADER IN X'1D' ELEMT        *         
* JUN08/95 SKU WIP SUPPORT                                            *         
* MAY11/95 BU  SET 'DAILY ORDER' FLAG IN BUY RECORD                   *         
* MAY03/95 BU  PROCESS HIATUS RECORDS - ADD ELEMENT TO                *         
*              CONTRACT RECORD                                        *         
* APR11/95 SKU MOVE PRD1/PRD2 CODES TO EI PRD FIELDS                  *         
*              MOVE PRD1/PRD2 NAMES TO CONTRACT PROD FIELD            *         
* MAR21/95 SKU TRUNCATE BUY ORDER COMMENT AT 60 CHARS.                *         
* FEB28/95 SKU DON'T DISPLAY REJECTED MESSAGE                         *         
* FEB14/95 SKU DISPLAY REC FULL ERR MSG INTEAD OF ABEND IN RECUP      *         
* JAN30/95 SKU POST UNDARED MESSAGE.                                  *         
* JAN26/95 BU  TRUNCATE BUY COMMENT AT 60 CHARS.  MOVED FROM          *         
*              SERVICE REQUEST.                                       *         
* JAN03/95 SKU PASS 'NOTDARE' FOR REJECTED ORDERS                     *         
* DEC28/94 BU  FACILITATE 'DAILY' ORDERS                              *         
* DEC22/94 SKU STUFF UNDARE IN FIRST LINE IF NOTDARE ORDER            *         
* DEC21/94 BU  OVERRIDE CONTRACT BUYER WITH AGY ORDER BUY NAME        *         
* DEC20/94 BU  PAD END OF PRINT LINE                                  *         
*              CHECK FOR 'NOTDARE' ORDER:  ONLY PERMIT 'REJECT'       *         
*              AND 'UNDARE'                                           *         
* DEC15/94 BU  FIX ERROR: # SPOTS/WEEK:  FIX IT RIGHT!                *         
* DEC08/94 BU  FIX ERROR: # SPOTS/WEEK                                *         
* NOV17/94 SKU ADD EDICT INFORMATIONAL CHUNK                          *         
* OCT27/94 SKU VERSION NUMBER FIX                                     *         
* JUL28/94 BU  INCORPORATE 'SOFT DELETE' FEATURE                      *         
* JUL21/94 SKU REVAMP SCREEN FIELDS                                   *         
* JUL19/94 BU  SAVE REJECT COMMENTS IN AGENCY ORDER HEADER            *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
T83F20   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T83F20*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         MVC   MYLABEL,=C'*MYAREA*'                                             
         ST    R3,RELO                                                          
         MVC   MYFLAG,4(R1)        SAVE OFF CALLING PARAMETER                   
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL - ALLOW ACTION                  
         BE    MAIN0005                                                         
         GOTO1 (RFCHKSYS,REPFACS),DMCB,ACOMFACS                                 
         BE    MAIN0005                                                         
         MVC   RERROR,=AL2(991)    SET ERROR TYPE = NOT UPDATEABLE              
         LA    R2,AORHDLNH         SET CURSOR HERE                              
         GOTO1 MYERROR                                                          
*                                                                               
MAIN0005 DS    0H                                                               
*                                                                               
         TM    MYFLAG,X'80'                                                     
         BZ    MAIN0010                                                         
         GOTO1 CALLOV,DMCB,CONTAGH,X'D9083FF9'                                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AORREAS(6),=C'UNDARE'                                            
         OI    AORREASH+6,X'80'                                                 
         MVI   AORREASH+5,6                                                     
*                                                                               
MAIN0010 DS    0H                                                               
         MVC   AORLAST+1(2),=X'0101'                                            
         MVI   MYSCRNUM,X'F9'      SET SCREEN NUMBER                            
         BAS   RE,SETPFKYS         SETUP THE PFKEYS                             
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                  YES                                          
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VALRECRD            YES                                          
*                                  NO OTHER ACTIONS RECOGNIZED                  
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         CLI   CALLSP,0            MUST BE CALLED TO GET HERE                   
         BNE   VK10                                                             
         LA    R2,CONRECH                                                       
         B     INVLRCAC            INVALID REC/ACTION                           
                                                                                
VK10     DS    0H                  CHECK IF NOTDARE ORDER                       
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         OI    DMINBTS,X'08'       IN CASE ORDER WAS UNDARED                    
         GOTO1 HIGH                                                             
         OI    DMINBTS,X'08'       IN CASE ORDER WAS UNDARED                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         TM    RDARMISC,X'20'      IS ORDER 'NOTDARE'?                          
         BNO   VKX                 NO, SKIP                                     
         MVC   AORREAS(6),=C'UNDARE'  STUFF UNDARE IN IF IT IS                  
*                                                                               
VK20     DS    0H                                                               
         MVI   AORREASH+5,6                                                     
         DROP  R6                                                               
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VALRECRD DS    0H                                                               
*                                                                               
         CLC   =C'REJ',CONACT                                                   
         BNE   VREC0010                                                         
         MVC   AORPFKY+16(30),=C'3 REJECT to Buyer  PF12 Return'                
*                                                                               
VREC0010 DS    0H                                                               
         CLI   PFKEY,2             USER WANTS TO SWITCH TO CONTRACT             
         BNE   VREC0030                                                         
                                                                                
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         OI    DMINBTS,X'08'       IN CASE ORDER WAS UNDARED                    
         GOTO1 HIGH                                                             
         OI    DMINBTS,X'08'       IN CASE ORDER WAS UNDARED                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         OC    RDARREP#,RDARREP#                                                
         BNZ   VREC0013                                                         
         LA    R2,AORHDLNH         SET CURSOR TO 'ACTION' FIELD                 
         MVC   RERROR,=AL2(423)    SET ERROR MESSAGE                            
         GOTO1 MYERROR             EXIT WITH ERROR                              
         DROP  R6                                                               
*                                                                               
VREC0013 DS    0H                                                               
         XC    BLOCK(256),BLOCK                                                 
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'    FROM THE REP SYSTEM                          
         MVC   GLVXFRPR,=C'ROM'    DARE PROGRAM                                 
         MVC   GLVXTOSY,=C'REP'    TO THE REP SYSTEM                            
         MVC   GLVXTOPR,=C'CON'    CONTRACT PROGRAM                             
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
         DROP  R1                                                               
*&&DO                                                                           
         LR    R2,RA                                                            
         AHI   R2,DARPROFS-CONHEADH                                             
         USING SVDSECT,R2                                                       
         TM    DMISFLGX,X'40'                                                   
         BZ    VREC0015            MASTER REP SIGNED ON?                        
         DROP  R2                                                               
*                                                                               
         OI    BLOCK+16,X'80'      CALL BY MASTER FLAG                          
K        USING RDARKEY,SELECTKY    PASS THE SUBREP CODE FOR MASTER CF           
         MVC   BLOCK+17(2),K.RDARKREP                                           
         DROP  K                                                                
*&&                                                                             
*                                                                               
VREC0015 DS    0H                                                               
*                                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',BLOCK,14,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*&&DO                                                                           
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   FULL,RDARREP#                                                    
         OC    RDARREP#,RDARREP#                                                
         BNZ   VREC0020                                                         
         MVC   FULL,CCONKNUM                                                    
         DROP  R6                                                               
*                                                                               
VREC0020 DS    0H                                                               
         ZAP   WORK+20(5),=P'0'    EDIT USES FIRST 17 BYTES OF WORK             
         MVO   WORK+20(5),FULL                                                  
         EDIT  (P5,WORK+20),(8,WORK+30),ALIGN=LEFT                              
*                                                                               
         LA    R2,AORHDLNH         ANY CONTRACT NUMBER?                         
         GOTO1 CGLOBBER,DMCB,=C'PUTD',FULL,L'FULL,GLRCONNO                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 CGLOBBER,DMCB,=C'PUTD',=CL1'D',1,GLRPFKEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
**********                                                                      
         XC    WORK,WORK                                                        
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   FULL,RDARREP#                                                    
         OC    RDARREP#,RDARREP#                                                
         BNZ   *+10                                                             
         MVC   FULL,CCONKNUM                                                    
         DROP  R6                                                               
*                                                                               
         LA    R6,WORK                                                          
         USING GLCONNUM,R6                                                      
         GOTO1 (RFCONNUM,REPFACS),DMCB,(1,FULL),(5,GLCONNUM)                    
         MVC   GLCONCA(3),=C'DIS'                                               
         MVC   GLCONBA(3),=C'DSM'                                               
*                                                                               
         LR    R2,RA                                                            
         AHI   R2,DARPROFS-CONHEADH                                             
         USING SVDSECT,R2                                                       
         TM    DMISFLGX,X'40'                                                   
         BZ    VREC0025            MASTER REP SIGNED ON?                        
         DROP  R2                                                               
*                                                                               
         OI    GLCONRPY+2,X'80'    CALL BY MASTER FLAG                          
K        USING RDARKEY,SELECTKY    PASS THE SUBREP CODE FOR MASTER CF           
         MVC   GLCONRPY+3(2),K.RDARKREP                                         
         DROP  K                                                                
*                                                                               
VREC0025 DS    0H                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',WORK,GLCONLNQ,GLRKACT                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
**********                                                                      
         DROP  R4                                                               
                                                                                
*        LA    R2,CONSERVH         POINT AT SVC REQ FLD HDR                     
*        OI    6(R2),X'C0'         XMIT FIELD                                   
*        XC    CONSERV,CONSERV                                                  
*        MVC   CONSERV(10),=C'+C,SV     '                                       
         B     EXIT                                                             
         EJECT                                                                  
VREC0030 DS    0H                                                               
         MVI   DELAGORD,C'N'       SET 'DELETE AGY ORDER' FLAG = NO             
         CLI   NEEDSCRN,C'Y'                                                    
         BNE   VREC0040                                                         
         MVI   NEEDSCRN,C'Y'       SET 'NEED SCREEN' FLAG                       
                                                                                
VREC0040 DS    0H                                                               
         LA    R2,CONACTH          DETERMINE ACTION                             
         MVI   BUYLINE#,0          CLEAR BUYLINE NUMBER                         
         XC    IFELCODE(IFELLEN),IFELCODE                                       
*                                  CLEAR SPOTPAK XFER ELEMENT STORE             
         MVI   ACTCODE,C'A'                                                     
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                SET FOR EX STATEMENT                         
         EX    RF,VREC1200         COMPARE TO 'OPEN'                            
         BE    VREC0080            FOUND - PROCEED                              
         MVI   ACTCODE,C'R'                                                     
         EX    RF,VREC1240         COMPARE TO 'REJECT'                          
         BE    VREC0080            FOUND - PROCEED                              
         LA    R3,INVRCACT         ACTION NOT RECOGNIZED                        
         DC    H'0'                DUMP FOR NOW                                 
         B     VREC0880            TRANSFER TO WHERE???                         
VREC0080 EQU   *                                                                
         MVC   AORRMSG(21),=C'REASON FOR REJECTION:'                            
         CLI   ACTCODE,C'R'        REJECT REQUEST?                              
         BE    VREC0120            YES                                          
         XC    AORRMSG,AORRMSG     NO  - APPROVAL - CLEAR IT                    
VREC0120 EQU   *                                                                
         LA    R2,AORRMSGH         SET TRANSMIT BIT                             
         OI    6(R2),X'80'            FOR WHEN SCREEN ALREADY UP                
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
                                                                                
         L     R4,AIO                                                           
         USING RDARREC,R4                                                       
*                                                                               
*                                  INSERT SENDER/RECEIVER                       
         GOTO1 HEXOUT,DMCB,RDARREP#,RETCON#,4,=C'TOG'                           
*                                  INSERT CONTRACT # (REP)                      
         GOTO1 HEXOUT,DMCB,RDARKORD,RETORD#,4,=C'TOG'                           
*                                                                               
         OC    RDARREP#,RDARREP#                                                
         BZ    *+10                                                             
         MVC   AORHDLN,RETCON#     PUT CONTRACT NUMBER ON SCREEN                
         LA    R2,AORHDLNH                                                      
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         MVC   AORAORD,RETORD#     PUT AGY ORDER NUMBER ON SCREEN               
         LA    R2,AORAORDH                                                      
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         NI    MISCFLAG,X'FF'-X'20' INIT TRADE FLAG                             
         CLI   RDARCORT,C'T'                                                    
         BNE   *+8                                                              
         OI    MISCFLAG,X'20'      THIS IS A TRADE ORDER                        
*                                                                               
         MVI   KATZEDI,C'N'        SET KATZ EDI ORDER TO 'NO'                   
         CLC   =C'$EDI$',RDARAGAD  SCAN FOR KATZ EDI SPECIAL                    
         BNE   *+8                                                              
         MVI   KATZEDI,C'Y'                                                     
*                                                                               
         MVI   RESENT,C'N'         SET RESENT FLAG TO 'NO'                      
         TM    RDARMISC,X'80'      ORDER RESENT?                                
         BNO   VREC0160            NO                                           
         MVI   RESENT,C'Y'         YES - NEED VERSION BUMP                      
VREC0160 EQU   *                                                                
         TM    RDARMISC,X'20'      IS ORDER 'NOTDARE'?                          
         BNO   VREC0180            NO                                           
         CLI   ACTCODE,C'R'        YES - IS ACTION CODE 'REJECT'?               
         BNE   VREC0170            NO  - ERROR                                  
         CLC   AORREAS(6),=C'UNDARE'                                            
*                                  YES - IS REASON 'UNDARE'?                    
         BE    VREC0180            YES - PROCEED                                
VREC0170 EQU   *                                                                
         LA    R2,CONACTH          SET CURSOR TO 'ACTION' FIELD                 
         MVC   RERROR,=AL2(451)    SET ERROR MESSAGE                            
         GOTO1 MYERROR             EXIT WITH ERROR                              
*                                                                               
VREC0180 EQU   *                                                                
         CLC   AORREAS(6),=C'UNDARE'  UNDARE?                                   
         BNE   VREC0185                                                         
         CLI   RDARRNUM,0          YES - IS THIS A REVISION                     
         BE    VREC0185            NO - PROCEED                                 
         MVC   RERROR,=AL2(958)    YES - NOT ALLOW TO UNDARE REVISION           
         GOTO1 MYERROR                                                          
*                                                                               
VREC0185 EQU   *                                                                
         LA    R2,AORHDLNH         ANY CONTRACT NUMBER?                         
*        CLI   5(R2),0                                                          
*        BNE   VREC0320            YES - CONTINUE                               
         OC    RDARREP#,RDARREP#   IS ORDER LINKED?                             
         BNZ   VREC0200            YES                                          
         CLI   ACTCODE,C'R'        'REJECT' REQUEST?                            
         BE    VREC0320            YES - SKIP CONTRACT # OUTPUT                 
         MVC   RERROR,=AL2(435)    NO  - UNLINKED: ILLEGAL APPROVAL             
         B     VREC1000                                                         
VREC0200 EQU   *                                                                
         GOTO1 HEXOUT,DMCB,RDARREP#,WORK,4,=C'TOG'                              
*                                  GET REP CONTRACT NUMBER                      
         LA    R3,WORK                                                          
         LA    RF,8                LOOP CONTROL                                 
VREC0240 EQU   *                                                                
         CLI   0(R3),C'0'          LEADING ZERO?                                
         BNE   VREC0280            NO  - MOVE IT                                
         LA    R3,1(R3)            YES - BUMP TO NEXT POSITION                  
         BCT   RF,VREC0240         GO BACK FOR NEXT                             
         DC    H'0'                SHOULDN'T HAPPEN                             
VREC0280 EQU   *                                                                
         BCTR  RF,0                DECREMENT FOR EX                             
         EX    RF,VREC1280         MOVE BY LENGTH TO SCREEN                     
VREC0320 EQU   *                                                                
         TM    CCONFLAG,CCONFSWP   CANNOT OPEN IF CONTRACT IS IN                
         BZ    VREC0325            STATION'S WORK-IN-PROGRESS                   
         MVC   RERROR,=AL2(656)    SET ERROR TYPE = STATION IN WIP              
         B     VREC1000                                                         
*                                                                               
*    ACTIVITY DECISION TABLE:                                                   
*        AGENCY ORDER        TYPE OF        PERMITTED/                          
*        PRIOR STATUS        REQUEST        PROHIBITED                          
*        ------------        -------        ----------                          
*        OPEN                OPEN           PERMIT                              
*        OPEN                REJECT         PERMIT (SHOULDN'T OCCUR)            
*        OPENED              OPEN           PROHIBIT                            
*        OPENED              REJECT         PERMIT                              
*        REJECTED            OPEN           PROHIBIT                            
*        REJECTED            REJECT         PROHIBIT                            
*        NOTDARED            OPEN           PROHIBIT                            
*        NOTDARED            REJECT         PERMIT (WITH UNDARE AS CMT)         
*                                                                               
VREC0325 EQU   *                                                                
         TM    RDARMISC,X'20'      ORDER NOTDARED FROM AGENCY SIDE?             
         BZ    VREC0330                                                         
         CLI   ACTCODE,C'R'        YES - THIS REQUEST = REJECT/UNDARE?          
         BNE   VREC0328                                                         
         CLI   RDARBSTS,C'R'       ALREADY REJECTED                             
         BNE   VREC0440                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VREC0440                                                         
         CLC   =C'UNDARE',2(R6)                                                 
         BE    VREC0360                                                         
         B     VREC0440                                                         
VREC0328 EQU   *                                                                
         MVI   RMSGTYPE,C'E'                                                    
         LA    R2,CONSERVH         SET A(SERVICE REQUEST)                       
         MVC   RERROR,=AL2(450)    SET ERROR TYPE = ONLY ACTION REJECT          
*                                     IS ALLOWED FOR NOTDARE ORDER              
         B     VREC1000                                                         
                                                                                
VREC0330 EQU   *                                                                
*                                                                               
         CLI   RDARBSTS,X'00'      AGY ORDER PRIOR STATUS?                      
         BE    VREC0440            NO  - DO OPEN OR REJECT                      
         CLI   RDARBSTS,C'A'       PRIOR STATUS = OPENED?                       
         BNE   VREC0360            NO  - REJECTED - NOT ALLOWED                 
         CLI   ACTCODE,C'R'        YES - THIS REQUEST = REJECT?                 
         BE    VREC0440            YES - PERMIT IT                              
         B     VREC0400            NO  - TRYING TO OPEN TWICE                   
VREC0360 EQU   *                                                                
         CLC   =C'UNDARE',AORREAS  ALLOW UNDARE ON AMENDED ORDER                
         BE    VREC0440                                                         
*                                                                               
         MVI   RMSGTYPE,C'E'                                                    
         LA    R2,CONSERVH         SET A(SERVICE REQUEST)                       
         MVC   RERROR,=AL2(425)    SET ERROR TYPE = REJECTED:                   
*                                     THIS ACTION IGNORED                       
         CLI   RDARBSTS,C'M'                                                    
         BNE   VREC1000                                                         
         MVC   RERROR,=AL2(942)    SET ERROR TYPE = AMENDED:                    
         B     VREC1000                                                         
VREC0400 EQU   *                                                                
         MVI   RMSGTYPE,C'E'                                                    
         LA    R2,CONSERVH         SET A(SERVICE REQUEST)                       
         MVC   RERROR,=AL2(424)    SET ERROR TYPE = OPENED:                     
*                                     CAN'T OPEN TWICE                          
         B     VREC1000                                                         
VREC0440 EQU   *                                                                
         CLI   ACTCODE,C'R'        REJECT REQUEST?                              
         BNE   VREC0480            NO                                           
*                                                                               
         CLI   RDARKTYP,X'51'                                                   
         BNE   VREC0450                                                         
         MVI   RMSGTYPE,C'E'                                                    
         LA    R2,CONSERVH         SET A(SERVICE REQUEST)                       
         MVC   RERROR,=AL2(919)    SET ERROR TYPE = CONFIRMED                   
         B     VREC1000                                                         
*                                                                               
VREC0450 EQU   *                                                                
         LA    R6,AORREASH         YES - MUST HAVE AT LEAST 1 LINE              
         CLI   5(R6),0             ANYTHING ON FIRST LINE?                      
         BNE   VREC0480            YES                                          
         MVI   RMSGTYPE,C'E'                                                    
         LR    R2,R6               SET A(CURSOR)                                
         MVC   RERROR,=AL2(433)    SET ERROR TYPE = REJECTED:                   
*                                     REQUIRES SOME INPUT                       
         GOTO1 MYERROR                                                          
VREC0480 EQU   *                                                                
*                                                                               
*   SAVE EASI CODES BEFORE ANYTHING ELSE.  AGENCY HEADER RECORD                 
*     CONTAINS X'01' AND X'02' ELEMENT, SO ALL LABELS APPLY AT                  
*     THIS TIME.                                                                
*                                                                               
         MVC   EASIADV(4),RDARCLI  INSERT CLIENT/ADV CODE                       
*                                     CHOPPED TO 4 CHARS                        
         MVC   EASIPROD,RDARPRD1   INSERT PRODUCT CODE                          
         MVC   EASIPRD2,RDARPRD2   INSERT PRODUCT CODE 2                        
*                                                                               
         MVC   SAVEPROD,RDARPRN1   SAVE PRODUCT NAME(S)                         
         CLC   =C'$EDI$',RDARAGAD  KATZ EDI ONLY HAS ONE PRODUCT                
         BE    VREC0490                                                         
         CLC   RDARPRN2,SPACES                                                  
         BE    VREC0490                                                         
         MVI   SAVEPROD+10,C'/'                                                 
         MVC   SAVEPROD+11(9),RDARPRN2                                          
                                                                                
VREC0490 EQU   *                                                                
         EDIT  RDAREST#,(4,EASIEST#),FILL=0,ZERO=NOBLANK                        
*                                  INSERT ESTIMATE NUMBER, 3 CHARS              
         MVC   FLTDATES,RDARESST   SAVE FLIGHT START/END                        
         MVC   SAVEBUYR,RDARBUYR   SAVE BUYER NAME                              
*                                                                               
* SAVE OFF DEMO CATEGORIES TO LATER STORE IN CONTRACT RECORD                    
*                                                                               
         XC    SVDEMCAT,SVDEMCAT                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VREC0495                                                         
         USING RDARDMEL,R6                                                      
         MVC   SVDEMCAT(16),RDARDEM1                                            
         DROP  R6                                                               
*                                                                               
VREC0495 EQU   *                                                                
         L     R6,AIO                                                           
         USING RDARELEM,R6                                                      
*                                                                               
*   INSERT INFORMATION FROM KEY SECTION INTO RETURN MESSAGE                     
*      SAVE AREA                                                                
*                                                                               
*        GOTO1 HEXOUT,DMCB,RDARKORD,RETORD#,4,=C'TOG'                           
*                                  INSERT ORDER #                               
         MVC   RETSTAT,RDARKSTA    INSERT STATION                               
         CLI   RETSTAT+4,C'L'      IS IT A TV STATION?                          
         BE    VREC0500            YES - LEAVE AS IS                            
         MVI   RETSTAT+5,C'V'      INSERT LAST CHAR OF MEDIA                    
         CLI   RETSTAT+4,C'T'      IS IT A TV STATION?                          
         BE    VREC0500            YES - LEAVE AS IS                            
         MVI   RETSTAT+5,C'M'      NO  - INSERT RADIO MEDIA                     
*                                                                               
VREC0500 EQU   *                                                                
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
*   SAVE HEADER INFORMATION FOR RETURN MESSAGE                                  
*                                                                               
         MVC   RETFROM(20),RDARSNDR                                             
*                                  INSERT SENDER/RECEIVER                       
*&&DO                                                                           
         GOTO1 HEXOUT,DMCB,RDARREP#,RETCON#,4,=C'TOG'                           
*                                  INSERT CONTRACT # (REP)                      
*                                                                               
         MVC   AORHDLN,RETCON#     PUT CONTRACT NUMBER ON SCREEN                
         LA    R2,AORHDLNH                                                      
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         MVC   AORAORD,RETORD#     PUT AGY ORDER NUMBER ON SCREEN               
         LA    R2,AORAORDH                                                      
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*&&                                                                             
*                                                                               
         MVC   RETSENDR,RDARRTS    INSERT 'RETURN TO SENDER' INFO               
*                                                                               
         OC    RDARREP#,RDARREP#                                                
         BZ    VREC0520                                                         
*                                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RDARREP#                                                 
         EDIT  (P5,WORK),(8,AORHDLN),ALIGN=LEFT                                 
         STC   R0,AORHDLNH+5       SET LENGTH OF DESTINATION                    
         MVI   AORHDLNH+4,X'08'    SET VALID NUMERIC                            
*                                                                               
         CLI   NEEDSCRN,C'Y'       IS SCREEN NEEDED?                            
         BNE   VREC0520            NO                                           
         CLI   ACTCODE,C'R'        YES - IS IT 'REJECT' ACTION?                 
         BNE   VREC0520            NO  - PROCEED                                
         LA    R2,AORREASH         SET CURSOR                                   
         MVC   RERROR,=AL2(112)    ERROR MESSAGE                                
         MVI   RMSGTYPE,C'I'                                                    
         GOTO1 MYERROR             EXIT VIA MYERROR                             
****     B     EXIT                                                             
         DROP  R6                                                               
                                                                                
VREC0520 DS    0H                                                               
         LA    R2,AORHDLNH         CONTRACT# ON SCREEN?                         
         CLI   5(R2),0                                                          
         BNE   VREC0560            YES                                          
         CLI   ACTCODE,C'R'        NO  - IS IT AN UNLINKED REJECT?              
         BE    VREC0600            YES                                          
         B     VREC0760            NO                                           
VREC0560 EQU   *                                                                
         GOTO1 VALICON,DMCB,(R2)   GET CONTRACT INFO AND STORE IT               
         GOTO1 =A(GETCON),DMCB,(RC),RR=RELO RETRIEVE CONTRACT RECORD            
         BZ    VREC0600            CONTRACT OKAY TO PROCESS                     
*                                                                               
*                                  ERROR IN GETCON:  MESSAGE CODE               
*                                     ALREADY SET IN RERROR                     
         LA    R2,CONACTH          SET CURSOR TO 'ACTION' FIELD                 
         GOTO1 MYERROR             EXIT WITH ERROR                              
*                                                                               
*   SET REPORT ID/CLASS FOR SPOOLING                                            
*                                                                               
VREC0600 EQU   *                                                                
*                                                                               
         CLI   ACTCODE,C'R'        REJECTION?                                   
         BNE   VREC0630                                                         
         CLI   PFKEY,3             YES, MUST PRESS PF3 TO SEND REJECT           
         BE    VREC0630            NOTICE TO AGENCY                             
         MVC   RERROR,=AL2(932)    SET ERROR TYPE = UNDARED                     
         LA    R2,AORREASH         SET CURSOR HERE                              
         GOTO1 MYERROR                                                          
*                                                                               
VREC0630 EQU   *                                                                
         CLI   KATZEDI,C'Y'        SKIP SENDING APPROVAL NOTICE BACK TO         
         BE    VREC0690            THE AGENCY FOR KATZ EDI ORDERS               
*                                                                               
         MVC   REMUSER,=C'DAR'                                                  
         LA    RF,SPOOLKEY                                                      
         USING PQPLD,RF                                                         
*                                                                               
         XC    SPOOLKEY,SPOOLKEY                                                
         CLI   ACTCODE,C'A'        APPROVAL?                                    
         BNE   VREC0640            NO                                           
         MVC   PLDESC,=CL11'DARE APPRVL'                                        
         MVC   EDICTACT,=C'APP'    SET EDICT 'ACTION'                           
         B     VREC0680                                                         
VREC0640 EQU   *                                                                
         MVC   PLDESC,=CL11'DARE REJECT'                                        
VREC0650 EQU   *                                                                
         MVC   EDICTACT,=C'REJ'    SET EDICT 'ACTION'                           
VREC0680 EQU   *                                                                
         OI    GENSTAT3,NOCLRSPK                                                
         MVI   PLCLASS,C'G'        CLASS G                                      
         OI    SPOOLIND,SPUINIT    PERMITS SETTING OF CLASS                     
*                                                                               
         DROP  RF                                                               
*                                                                               
         LA    RE,SPLKEYAD         SET EXTENDED KEY ADDRESS                     
*                                                                               
*                                                                               
         ST    RE,SPOOLQLK         SAVE EXTENDED KEY ADDRESS                    
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
*                                                                               
         DROP  RE                                                               
*                                                                               
         GOTO1 OPENPQ                                                           
*                                                                               
         GOTO1 =A(EDICT),DMCB,(RC),(R8),(R5),RR=RELO                            
*                                  PUT OUT EDICT HEADER                         
VREC0690 EQU   *                                                                
         GOTO1 =A(DATETIME),DMCB,(RC),(R5),RR=RELO                              
*                                  SET UP DATE/TIMES FOR STAMP                  
         LA    R2,CONACTH          DETERMINE ACTION                             
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                SET FOR EX STATEMENT                         
         EX    RF,VREC1200         COMPARE TO 'OPEN'                            
         BE    VREC0720            OPEN:     PROCESS ORDER                      
         B     VREC0840            REJECT:   CYCLE SCREEN ENTRIES               
VREC0720 EQU   *                                                                
*                                                                               
         XC    SPOTCTR,SPOTCTR     CLEAR ACCUMULATORS                           
         XC    WEEKCTR,WEEKCTR                                                  
         XC    BUYCOST,BUYCOST                                                  
         XC    ORDTOT$$,ORDTOT$$                                                
         XC    ORDTOTSP,ORDTOTSP                                                
*                                                                               
         GOTO1 AGYPROC,DMCB,(RC)   CYCLE AGENCY RECORDS,                        
*                                     GENERATING BUYLINES                       
*                                                                               
* IF BRAND, NEED TO CLEAR BUCKETS IN VARIOUS' CONTRACT                          
*                                                                               
         GOTO1 =A(VARIOUS),DMCB,(RC),RR=RELO                                    
*                                                                               
         EDIT  ORDTOT$$,(10,AORCTOT),2                                          
*                                  INSERT TOTAL $$ ON SCREEN                    
         LA    R2,AORCTOTH                                                      
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         EDIT  ORDTOTSP,(6,AORTSPT)                                             
*                                  INSERT TOTAL SPOTS ON SCREEN                 
         LA    R2,AORTSPTH                                                      
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         B     VREC0800            EXIT:  BUYS GENERATED                        
                                                                                
                                                                                
VREC0760 DS    0H                                                               
*                                  SEND MESSAGE: NO CONTRACT                    
         B     VREC0920                                                         
VREC0800 DS    0H                                                               
         CLI   KATZEDI,C'Y'        SKIP MESSAGE FOR KATZ EDI ORDERS             
         BE    VREC0880                                                         
*                                  SEND MESSAGE: ORDER OPENED                   
         GOTO1 =A(APPRREJC),DMCB,(RC),(R8),(R5),RR=RELO                         
         L     R6,AIO1                                                          
         USING RDARREC,R6                                                       
         CLI   RDARKSTA+4,C'A'     FOR RADIO EDI, SEND SALESPERSON              
         BE    VREC0850            AND TRAILER RECORDS                          
         CLI   RDARKSTA+4,C'F'                                                  
         BE    VREC0850                                                         
         B     VREC0880            FINISHED - EXIT                              
*                                                                               
VREC0840 EQU   *                                                                
*                                  SEND MESSAGE: ORDER REJECTED                 
         GOTO1 =A(APPRREJC),DMCB,(RC),(R8),(R5),RR=RELO                         
VREC0850 EQU   *                                                                
         GOTO1 =A(REJCMSGS),DMCB,(RC),(R8),(R5),RR=RELO                         
*                                  REJECT ACTIVITY                              
VREC0880 DS    0H                                                               
         DROP  R6                                                               
*                                                                               
*                                  RETRIEVE HEADER OF AGY ORD                   
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
                                                                                
         L     R4,AIO                                                           
         USING RDARREC,R4                                                       
*                                                                               
         MVC   RDARBSTS,ACTCODE    SET STATUS TO ACTCODE                        
         CLI   ACTCODE,C'A'        ORDER OPENED?                                
         BNE   VREC0900            NO  -                                        
         OI    RDARMISC,X'40'      YES - SET 'OPENED AT LEAST ONCE'             
         DROP  R4                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VREC0900            NO FLAG ELEMENT, SKIP                        
         USING RDARFLEM,R6         TURN OFF -S FLAG                             
         NI    RDARFLG1,X'FF'-X'40'                                             
         DROP  R6                                                               
*                                                                               
VREC0900 EQU   *                                                                
         CLI   ACTCODE,C'R'        ORDER REJECTED?                              
         BNE   VREC0910            NO  -                                        
         GOTO1 =A(ADDREJS),DMCB,(RC),(R5),(R4),RR=RELO                          
*                                                                               
VREC0910 EQU   *                                                                
*        GOTO1 PUTREC              REWRITE RECORD WITH NEW STATUS               
         GOTO1 =A(WRITEREC),RR=RELO                                             
*                                                                               
         GOTO1 =A(DOAUDIT),DMCB,(RC),(R5),(R4),RR=RELO                          
*                                                                               
         CLI   KATZEDI,C'Y'        SKIP MESSAGE FOR KATZ EDI ORDERS             
         BE    VREC0960                                                         
*                                  SEND MESSAGE: ORDER OPENED                   
*                       1.3.5.7.9.1.3.5.7.9.1.3.5.                              
         MVC   P(26),=C'*** END OF DDS MESSAGE ***'                             
*                                  SEND SPECIAL PRINT LINE                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
*                                                                               
VREC0920 DS    0H                                                               
         MVI   SPMODE,X'FF'        CLOSE THE PRINT QUEUE                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
*                                                                               
* FOR REJECT, AUTOMATICALLY GENERATE WORKSHEET                                  
*                                                                               
         CLI   ACTCODE,C'R'        REJECT?                                      
         BNE   VREC0960                                                         
         GOTO1 VLOAD,DMCB,(X'30',0),('QPRONE',0)                                
*        GOTO1 VREDAR30,DMCB,(RC),('QPRONE',0)                                  
         CLI   DELAGORD,C'Y'       DELETE AGENCY ORDER?                         
         BNE   EXIT                                                             
         GOTO1 =A(DELORD),DMCB,(RC),RR=RELO                                     
         B     EXIT                WANT TO SHOW REPORT ID ON CONMSG             
*                                  INSTEAD OF REJECT MESSAGE                    
VREC0960 DS    0H                                                               
*                                                                               
* SPECIAL FOR KATZ EDI ORDERS. IF ACTION IS OPEN, ADD A PASSIVE KEY             
* X'E1' WITH CONTRACT # AND DATE/TIME STAMP INFO TO BE PICKED UP LATER          
* AT NIGHT BY A REPORT THAT WILL WRITE THESE ORDERS OUT TO TAPE                 
*                                                                               
         CLI   ACTCODE,C'A'        FOR ACTION OPEN                              
         BNE   VREC0970                                                         
         CLI   KATZEDI,C'Y'        AND KATZ EDI ORDERS                          
         BNE   VREC0970                                                         
         GOTO1 =A(GENEDIKY),DMCB,(RC),RR=RELO                                   
*                                                                               
VREC0970 DS    0H                                                               
         MVI   RMSGTYPE,C'I'                                                    
         LA    R2,CONSERVH         SET A(SERVICE REQUEST)                       
         MVC   RERROR,=AL2(113)    SET ERROR TYPE = OPENED                      
         CLI   ACTCODE,C'A'        OPENED?                                      
         BE    VREC1000            YES                                          
         MVC   RERROR,=AL2(114)    SET ERROR TYPE = REJECTED                    
         CLC   =C'UNDARE',AORREAS  REJECT AS 'UNDARE'?                          
         BNE   VREC1000            NO                                           
         MVC   RERROR,=AL2(117)    SET ERROR TYPE = UNDARED                     
*                                                                               
VREC1000 EQU   *                                                                
         LA    R2,AORHDLNH         SET CURSOR HERE                              
         GOTO1 MYERROR                                                          
*                                                                               
VREC1200 CLC   8(0,R2),=C'OPEN'                                                 
VREC1240 CLC   8(0,R2),=C'REJECT'                                               
VREC1280 MVC   8(0,R2),0(R3)                                                    
*                                                                               
         EJECT                                                                  
*                                                                               
*   GETCON:  RETRIEVE THE CONTRACT RECORD FOR PROCESSING.                       
*        STORE IT IN ALTERNATE IO AREA 3 (AIO3).                                
*                                                                               
NOCONREC EQU   82                                                               
*                                                                               
GETCON   NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE) IF NMOD NEEDED            
         LA    R2,AORHDLNH         CONVERT HEADLINE #                           
         LA    R3,INVALID          INVALID INPUT ERROR MSG                      
         MVC   AIO,AIO3            SET ALT IO AREA 3                            
         L     R4,AIO3                                                          
         USING RCONREC,R4                                                       
         GOTO1 SCANNER,DMCB,(R2),(1,RCONREC),0                                  
         CLI   4(R1),0                                                          
         BE    ERREXIT                                                          
         LA    R4,RCONREC          POINT TO SCANNER BLOCK                       
         CLI   0(R4),0                                                          
         BE    ERREXIT                                                          
         CLI   0(R4),8             K NUMBER UP TO 8 DIGITS                      
         BH    ERREXIT                                                          
         TM    2(R4),X'80'         TEST FOR NUMERIC INPUT                       
         BZ    ERREXIT                                                          
         ICM   R0,15,4(R4)                                                      
         CVD   R0,DUB                                                           
         SPACE                                                                  
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
*                                  CALCULATE 9'S COMP                           
         MVO   WORK(5),WORK+10(5)                                               
         XC    RCONREC(32),RCONREC                                              
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,TWAAGY     INSERT POWER CODE                            
         MVC   RCONPCON,WORK       CHECK IF NUMBER IS ON FILE                   
         MVC   COMPCON,WORK        REVERSE THE COMPLEMENT                       
         PACK  COMPCON+0(1),WORK+3(1)                                           
         PACK  COMPCON+1(1),WORK+2(1)                                           
         PACK  COMPCON+2(1),WORK+1(1)                                           
         PACK  COMPCON+3(1),WORK+0(1)                                           
         MVC   KEY,RCONREC                                                      
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    GETC0040            RECORD FOUND                                 
         MVC   RERROR,=AL2(031)    SET ERROR MESSAGE                            
         GOTO1 MYERROR             EXIT WITH ERROR                              
***>>>   LA    R3,NOCONREC         NO CONTRACT RECORD FOUND!!                   
***>>>   B     GETC0370            OK EXIT ANYWAY, IN CASE WE DO UNDARE         
*                                  FOR CONTRACTS THAT HAVE BEEN PURGED          
GETC0040 EQU   *                                                                
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 GETREC                                                           
         B     GETC0120                                                         
GETC0080 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
GETC0120 EQU   *                                                                
         L     R6,AIO1             DARE RECORD                                  
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GETC0130                                                         
R        USING RDARFLEM,R6                                                      
         TM    R.RDARFLG1,X'01'      UNWIRE?                                    
         BZ    GETC0130            NO                                           
         DROP  R                                                                
*                                                                               
         NI    MISCFLAG,X'FF'-MF1TKO                                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'1C'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GETC0130                                                         
         OI    MISCFLAG,MF1TKO                                                  
*                                                                               
         CLI   ACTCODE,C'A'                                                     
         BNE   GETC0130                                                         
         MVC   AIO,AIOAREA                                                      
         L     R2,AIO                                                           
         USING RPRDREC,R2                                                       
         XC    RPRDKEY,RPRDKEY                                                  
         MVI   RPRDKEY,9           INSERT RECORD CODE                           
         MVC   RPRDKADV,RCONKADV   INSERT ADVERTISER CODE                       
         MVC   RPRDKPRD,RCONPRD    INSERT PRODUCT CODE                          
         MVC   RPRDKREP,TWAAGY     INSERT POWER CODE                            
         MVC   KEY,RPRDKEY         LOAD KEY                                     
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                 RECORD FOUND                                 
         DC    H'0'                ??                                           
*                                                                               
         MVI   RDUPDATE,C'Y'       SET UPDATE TO NO                             
         GOTO1 GETREC                                                           
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'04'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GETC0130                                                         
*                                                                               
         LR    R3,R6                                                            
         USING RPRDAGFL,R3                                                      
*                                                                               
         L     R6,AIO1                                                          
DARREC   USING RDARREC,R6                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(2,DARREC.RDARESST),(3,ELTBUILD)                     
         GOTO1 DATCON,DMCB,(2,DARREC.RDARESEN),(3,ELTBUILD+3)                   
*                                                                               
         CLC   ELTBUILD(3),RPRDAGDF                                             
         BL    GETC0125                                                         
         CLC   ELTBUILD+3(3),RPRDAGDT                                           
         BH    GETC0125                                                         
         B     GETC0130                                                         
*                                                                               
GETC0125 DS    0H                                                               
         OC    RPRDAGDF(L'RPRDAGDF+L'RPRDAGDT),RPRDAGDF                         
         BNZ   GETC0130            EMPTY?                                       
         MVC   RPRDAGDF,ELTBUILD   COPY START-END DATE FROM ORDER               
         MVC   RPRDAGDT,ELTBUILD+3                                              
         GOTO1 PUTREC                                                           
*                                                                               
GETC0130 DS    0H                                                               
         DROP  R3,DARREC                                                        
                                                                                
***>>>                                                                          
*                                                                               
*        DROP ALL EST/INV ELEMENTS FROM ORDER                                   
*                                                                               
****     GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'03',RCONREC),0,0                
*                                  DELETE OLD ESTIMATE ELTS                     
****     GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'04',RCONREC),0,0                
*                                  DELETE OLD INVOICE  ELTS                     
****     GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'53',RCONREC),0,0                
*                                  DELETE OLD ALT CAL EST ELTS                  
****     GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'54',RCONREC),0,0                
*                                  DELETE OLD ALT CAL INV  ELTS                 
****     GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'A4',RCONREC),0,0                
*                                  DELETE  ALT CAL CTL  ELT                     
         XC    WORK,WORK                                                        
         MVC   WORK(4),ACOMFACS                                                 
         MVC   WORK+4(2),TWAAGY                                                 
*                                                                               
         GOTOX (RFVALTCL,REPFACS),DMCB,(X'FF',RCONREC),GETBROAD,0,WORK          
         BE    GETC0140            ALT CALENDAR(S) TEST PASSED                  
         MVC   RERROR,=AL2(780)    SET ERROR MESSAGE                            
         B     GETC0400            EXIT CC NOT ZERO                             
*                                                                               
GETC0140 EQU   *                                                                
         GOTOX (RFCHKALT,REPFACS),DMCB,(0,RCONREC),ACOMFACS                     
         MVC   BUCKFLGS(1),0(R1)                                                
*                                  SET ALT CAL FLAGS FOR STATION                
***>>>                                                                          
         CLI   ACTCODE,C'R'        'REJECT' ACTION?                             
         BE    GETC0360            YES - DON'T DUMP BUYS                        
         BAS   RE,GETVERSN         RETRIEVE VERSION NUMBER                      
*                                                                               
*   RETRIEVE REP RECORD IN ALL CASES TO GET DAILY PACING PROFILE                
*                                                                               
         MVC   AIO,AIOAREA         SET ALTERNATE READ AREA                      
*                                     FOR SPOT CODES                            
         L     R2,AIO                                                           
         USING RREPRECD,R2                                                      
         XC    RREPKEY,RREPKEY                                                  
         MVI   RREPKEY,1           INSERT RECORD CODE                           
         MVC   RREPKREP,TWAAGY     INSERT POWER CODE                            
         MVC   KEY,RREPKEY         LOAD KEY                                     
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                 RECORD FOUND                                 
         DC    H'0'                ??                                           
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 GETREC                                                           
         MVC   DAILYFLG,RREPPROF+27                                             
*                                  SAVE DAILY PACING FLAG                       
         BAS   RE,DUMPBUYS                                                      
*                                                                               
* SKIP VALIDATION FOR TRAINING IDS                                              
*                                                                               
         CLC   =C'MS',AGENCY                                                    
         BE    GETC0360                                                         
         CLC   =C'U1',AGENCY                                                    
         BE    GETC0360                                                         
         CLC   =C'U2',AGENCY                                                    
         BE    GETC0360                                                         
         CLC   =C'UR',AGENCY                                                    
         BE    GETC0360                                                         
*                                                                               
         CLI   RCONTYPE,C'N'       REP-TO-SPOT TRANSFER NEEDED?                 
         BE    GETC0160            YES                                          
         CLI   RCONTYPE,C'X'       REP-TO-SPOT TRANSFER NEEDED?                 
         BNE   GETC0360            NO                                           
GETC0160 EQU   *                                                                
         MVC   IFELCODE,=X'0830'   INSERT CODE/LENGTH                           
         MVC   RERROR,=AL2(940)    SET ERROR MESSAGE                            
         LA    R3,RREPELEM         FIND X'05' ELEMENT                           
GETC0200 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BNE   GETC0210                                                         
         LA    R2,AORHDLNH         SET CURSOR HERE                              
         GOTO1 MYERROR             EXIT WITH ERROR                              
GETC0210 EQU   *                                                                
         CLI   0(R3),X'05'         SPOT INTERFACE ELEMENT?                      
         BE    GETC0240            YES                                          
         ZIC   RF,1(R3)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R3,RF                                                            
         B     GETC0200            GO BACK FOR NEXT                             
GETC0240 EQU   *                                                                
         MVC   IFSPAG,RREPSPPC-RREPSPOT(R3)                                     
*                                  INSERT SPOT AGENCY CODE                      
         MVC   IFSPMD,RREPMED-RREPSPOT(R3)                                      
*                                  INSERT SPOT MEDIA                            
         DROP R2                                                                
*                                                                               
         L     R2,AIO                                                           
         USING RPRDREC,R2                                                       
         XC    RPRDKEY,RPRDKEY                                                  
         MVI   RPRDKEY,9           INSERT RECORD CODE                           
         MVC   RPRDKADV,RCONKADV   INSERT ADVERTISER CODE                       
         MVC   RPRDKPRD,RCONPRD    INSERT PRODUCT CODE                          
         MVC   RPRDKREP,TWAAGY     INSERT POWER CODE                            
         MVC   KEY,RPRDKEY         LOAD KEY                                     
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                 RECORD FOUND                                 
         DC    H'0'                ??                                           
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 GETREC                                                           
         MVC   RERROR,=AL2(940)    SET ERROR MESSAGE                            
         LA    R3,RPRDELEM         FIND X'03' ELEMENT                           
GETC0280 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BNE   GETC0290                                                         
         LA    R2,AORHDLNH         SET CURSOR HERE                              
         GOTO1 MYERROR             EXIT WITH ERROR                              
GETC0290 EQU   *                                                                
         CLI   0(R3),X'03'         SPOT INTERFACE ELEMENT?                      
         BE    GETC0320            YES                                          
         ZIC   RF,1(R3)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R3,RF                                                            
         B     GETC0280            GO BACK FOR NEXT                             
GETC0320 EQU   *                                                                
         MVC   IFSPCL,RPRDSPCL-RPRDSPOT(R3)                                     
*                                  INSERT SPOT CLIENT CODE                      
         MVC   IFSPPRD,RPRDSPP1-RPRDSPOT(R3)                                    
*                                  INSERT SPOT PRODUCT CODE                     
         MVC   IFSPES,RPRDSPES-RPRDSPOT(R3)                                     
*                                  INSERT SPOT ESTIMATE NUMBER                  
         MVC   IFSPPP,RPRDSPP2-RPRDSPOT(R3)                                     
*                                  INSERT SPOT PRODUCT PIGGY                    
         MVC   IFSPP1,RPRDSPS1-RPRDSPOT(R3)                                     
*                                  INSERT SPOT PRODUCT PIGGY SPLIT 1            
         MVC   IFSPP2,RPRDSPS2-RPRDSPOT(R3)                                     
*                                  INSERT SPOT PRODUCT PIGGY SPLIT 2            
         MVC   IFSPST,RCONKSTA     INSERT STATION CALL LETTERS                  
         MVC   IFSPADV,RCONKADV    INSERT REP ADVERTISER                        
         MVC   IFSPRD,RCONPRD      INSERT REP PRODUCT CODE                      
GETC0360 EQU   *                                                                
         MVC   CONMOD#,RCONMOD     SAVE MOD #                                   
*                                                                               
GETC0370 EQU   *                                                                
         SR    RF,RF               SET CC ZERO - CLEAN END                      
         LTR   RF,RF                                                            
         B     ERREXIT                                                          
GETC0400 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO                              
ERREXIT  EQU   *                   ERROR EXIT???                                
*                                                                               
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*   DUMPBUYS: AS A WHOLE ORDER IS BEING PROCESSED AT ONE TIME, THE              
*        BUYS FOR IT HAVE TO BE DELETED.  ALSO, THE DOLLARS FOR                 
*        THOSE BUYS HAVE TO BE BACKED OUT OF THE CONTRACT, FOR                  
*        PACING PURPOSES.  AT A LATER DATE, WHEN ''CHANGES'' ARE                
*        PROCESSED, THIS ROUTINE WILL HAVE TO BE MODIFIED, OR                   
*        DROPPED COMPLETELY.                                                    
*                                                                               
DUMPBUYS NTR1                                                                   
         MVC   AIO,AIO2            SET IO AREA FOR READING                      
         L     R4,AIO2             SET WORK AREA FOR BUYS                       
         USING RBUYREC,R4                                                       
*                                                                               
         NI    DMINBTS,X'FF'-X'08' TURN OFF 'RETURN DELETES'                    
         XCEFL RBUYREC,1024        CLEAR BUY BUILD AREA                         
         MVI   RBUYKTYP,X'0B'      INSERT RECORD TYPE                           
         MVC   RBUYKREP,TWAAGY     INSERT REP CODE                              
         MVC   RBUYKCON,COMPCON    INSERT CONTRACT #, 9/COMP/REV                
         MVC   KEY(27),RBUYREC     RETRIEVE KEY: ACTIVE ONLY                    
         GOTO1 HIGH                                                             
         B     DBUY0040                                                         
DBUY0020 EQU   *                                                                
         GOTO1 SEQ                 RETRIEVE NEXT BUY                            
DBUY0040 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     SAME ORDER?                                  
         BNE   DBUY0400            NO  - FINISHED                               
         GOTO1 GETREC              RETRIEVE RECORD                              
         OI    RBUYCNTL,X'80'      MARK RECORD DELETED                          
*                                                                               
*   MUST MARK BUY AS DELETED, OR, IF SENT TO STATION, AS CANCELLED.             
*     TEST OF VERSION NUMBERS DETERMINES WHETHER BUY SENT OR NOT.               
*                                                                               
         MVI   RBUYCHGI,C'X'       SET CHANGE TO 'DELETED'                      
         CLC   RBUYVER,VERDFLT     BUY VERSION VS CONTRACT VERSION              
         BNL   DBUY0060            BUY VER LESS THAN CONTRACT VERSION           
         MVI   RBUYCHGI,C'C'       SET CHANGE TO 'CANCELLED'                    
DBUY0060 EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,0),(3,RBUYCHGD)  UPDATE LAST CHANGE DATE          
                                                                                
         L     RF,AIO3             GET CONTRACT RECORD IOAREA                   
         USING RCONREC,RF                                                       
         TM    RCONMODR+1,X'C0'    ACE OR GRAPHNET                              
         BZ    DBUY0070                                                         
         DROP  RF                                                               
                                                                                
         MVC   RBUYVER,VERDFLT     UPDATE VERSION NUMBER                        
                                                                                
DBUY0070 EQU   *                                                                
         GOTO1 PUTREC              REWRITE BUY AS DELETED                       
         MVI   BUCKFLAG,X'FF'      SET TO BACK OUT FIGURES                      
         OI    BUCKFLGS,X'10'      DON'T IGNORE CANCELLED BUYS                  
         MVC   REJMESS(7),=C'BILLUHR'                                           
         LA    RF,1                                                             
         GOTO1 =A(BUCKUPDT),DMCB,(RC),(R5),(RF),RR=RELO                         
*                                  BACK OUT BUCKETS                             
         NI    BUCKFLGS,X'FF'-X'10' RESET                                       
         OI    KEY+27,X'80'        SET KEY TO DELETED                           
         GOTO1 WRITE               REWRITE DELETED KEY                          
         B     DBUY0020            GO BACK FOR NEXT                             
DBUY0400 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*    GETVERSN:  RETRIEVE X'20' ELEMENT FROM CONTRACT, SAVE THE                  
*        REP VERSION NUMBER.  BUMP THE NUMBER IF CONTRACT IS                    
*       'RESENT'.                                                               
*        TO SUPPORT WIP IN CONTRACT, THE NEXT REP VERSION NUMBER MUST           
*        BE THE HIGHER OF CURRENT REP NUMBER PLUS 2 *OR* CURRENT STA            
*        NUMBER PLUS 1.                                                         
*                                                                               
GETVERSN NTR1                                                                   
         L     R4,AIO3             SET WORK AREA FOR BUYS                       
         USING RCONREC,R4                                                       
*                                                                               
         MVI   VERDFLT,1           SET REP VERSION DEFAULT                      
         LA    R3,RCONELEM                                                      
GETV0020 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    GETVX               YES - FINISHED - NOT FOUND                   
         CLI   0(R3),X'20'         SEND INFO ELEMENT?                           
         BE    GETV0040            YES                                          
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         B     GETV0020            GO BACK FOR NEXT                             
GETV0040 EQU   *                                                                
         MVC   VERDFLT,RCONSRV-RCONSEND(R3)                                     
*                                  SAVE THE REP VERSION NUMBER                  
         CLI   RESENT,C'Y'         CONTRACT IS RESENT?                          
         BE    GETV0045            YES - NEED VERSION BUMP                      
         L     R6,AIO1                                                          
         USING RDARREC,R6                                                       
         TM    RDARMISC,X'10'      IF VARIOUS, THIS MIGHT BE A                  
         BZ    GETVX               POOL RESENT AFTER ALL                        
         DROP  R6                                                               
                                                                                
* BUMP REP VERSION IF REP VERSION WAS NOT ADVANCED, ELSE EXIT                   
GETV0045 EQU   *                                                                
         TM    RCONSENF-RCONSEND(R3),X'20'                                      
         BNO   GETVX                                                            
                                                                                
         LA    RF,2                                                             
         ZIC   RE,VERDFLT                                                       
         AR    RE,RF               INCREASE VERSION NUMBER                      
         STC   RE,VERDFLT          REPLACE NEW VERSION NUMBER                   
         STC   RE,RCONSRV-RCONSEND(R3)                                          
*                                                                               
* NEXT REP VERSION SHOULD BE THE GREATER OF REP VERSION + 2 OR                  
* STA VERSION + 1                                                               
*                                                                               
         CLC   RCONSRV-RCONSEND(1,R3),RCONSSV-RCONSEND(R3)                      
         BH    GETV0050                                                         
         ZIC   RF,RCONSSV-RCONSEND(R3)                                          
         LA    RF,1(RF)                                                         
         STC   RF,RCONSRV-RCONSEND(R3)                                          
         STC   RF,VERDFLT                                                       
*                                                                               
GETV0050 DS    0H                                                               
         NI    RCONSENF-RCONSEND(R3),X'FF'-X'20'                                
*                                  TURN OFF 'REP VERS# NOT ADVANCED'            
*                                     MEANING:  IT WAS ADVANCED                 
*                                                                               
*                                                                               
GETVX    EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
*   AGYPROC:  PROCESS THE AGENCY ORDER.  FIRST RECORD HAS BEEN                  
*        DELIVERED INTO THE PRIMARY IOAREA (AIO1).                              
*        CONSTRUCT BUYLINES IN ALTERNATE IO AREA 2 (AIO2).                      
*        UPDATE CONTRACT HEADER BUCKETS.  CONTRACT RECORD IS IN                 
*            ALTERNATE IO AREA 3 (AIO3).                                        
*                                                                               
AGYPROC  NTR1                                                                   
         L     R4,AIO2             SET WORK AREA FOR BUYS                       
         USING RBUYREC,R4                                                       
*                                                                               
         MVC   AIO,AIO1            SET IO AREA FOR X'41' RECS                   
         L     R3,AIO                                                           
         USING RDARREC,R3                                                       
*                                                                               
         GOTO1 =A(SETTRADE),DMCB,(RC),RR=RELO                                   
*                                                                               
         GOTO1 =A(SETUPHIA),DMCB,(RC),RR=RELO                                   
*                                  RESET HIATUS DATA, IF ANY                    
         MVI   RDARKRT,X'40'       SET REC TYPE TO 'BUY'                        
         XC    RDARKSEQ(2),RDARKSEQ                                             
*                                  CLEAR SEQ # AND SUB TYPE                     
         MVC   KEY,RDARKEY         RETRIEVE FIRST BUY HEADER                    
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     SAME KEY? THROUGH REC TYPE                   
         BNE   APRO0400            YES                                          
APRO0020 EQU   *                                                                
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         MVI   BUYUPDAT,C'Y'       SET 'WRITE OUTPUT RECORD'                    
         GOTO1 GETREC              RETRIEVE RECORD                              
         GOTO1 =A(CHKDAREC),DMCB,(RC),(R3),(R5),RR=RELO                         
*                                  ROUTINE SETS 'BUYUPDAT' TO NO                
*                                     WHEN HEADER IS SOFT-DELETED.              
*                                     THIS WILL PREVENT OUTPUT.                 
*                                  ALSO SETS 'SKIP RECORD' FLAG FOR             
*                                     OTHER RECORD TYPES                        
APRO0040 EQU   *                                                                
         MVI   ORBSTDAY,0          CLEAR 'ORBIT START DAY'                      
         MVI   ORBFLAG,C'N'        SET   'ORBIT NOT PRESENT'                    
         MVI   DETLFLAG,C'N'       CLEAR COUNT OF DETAILS                       
         XCEFL RBUYREC,1024        CLEAR BUY BUILD AREA                         
         MVI   RBUYKTYP,X'0B'      INSERT RECORD TYPE                           
         MVC   RBUYKREP,TWAAGY     INSERT REP CODE                              
         MVC   RBUYKCON,COMPCON    INSERT CONTRACT #, 9/COMP/REV                
         MVC   RBUYKPLN,=X'FFFFFF' INSERT PLAN CODE                             
         ZIC   RF,BUYLINE#         GET NEXT BUYLINE NUMBER                      
         LA    RF,1(RF)                                                         
         STC   RF,BUYLINE#         PUT IT BACK                                  
         STC   RF,RBUYKMLN         INSERT MASTER LINE NUMBER                    
         STC   RF,RBUYKLIN         INSERT LINE NUMBER                           
         MVC   RBUYLEN,=X'004D'    INSERT RECORD LENGTH:                        
*                                     34+43 = 77 = X'4D'                        
         MVC   RBUYELEM(2),=X'012B'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         LA    R6,RDARELEM                                                      
         USING RDARBYEL,R6                                                      
         MVC   PROGNAME,RDARBYPN   SAVE BUY HDR PROGRAM NAME                    
         MVC   RBUYCOS,RDARBYCO    INSERT COST                                  
         MVC   BUYCOST,RDARBYCO    SAVE COST FOR CALCULATIONS                   
         MVC   RBUYCLS(6),SPACES   SET CLASS/SECTION TO SPACES                  
         GOTO1 DATCON,DMCB,(5,WORK),(3,RBUYCREA)                                
*                                  INSERT CREATION DATE                         
         MVC   RBUYKMOD,CONMOD#    INSERT CONTRACT MOD #                        
         MVI   RBUYCHGI,C'A'       INSERT CHANGE INDICATOR                      
         MVC   RBUYAGBL,RDARKSEQ   INSERT AGENCY BUY LINE NUMBER                
         XC    STARTDAY(2),STARTDAY                                             
*                                  CLEAR START/END DAYS OF WEEK                 
         GOTO1 =A(STARTEND),DMCB,(RC),RDARBYRO,RBUYSTED,(R5),RR=RELO            
*                                  CONVERT ONE-BYTE START/END DATE              
         GOTO1 EFFDATEL,DMCB,(RC),RDARREC                                       
*                                                                               
         MVC   RBUYDUR,RDARBYSL    INSERT TOTAL SPOT LENGTH                     
         CLI   RDARBYSL,C'M'       LENGTH IN MINUTES?                           
         BNE   APRO0060            NO                                           
         OI    RBUYDUR,X'80'       YES - SET 'MINUTES' INDICATOR                
APRO0060 EQU   *                                                                
         TM    MISCFLAG,X'20'      TRADE ORDER?                                 
         BZ    *+8                                                              
         OI    RBUYFLG2,X'02'      MARK TRADE BUY                               
*                                                                               
         MVC   RBUYVER,VERDFLT     INSERT VERSION NUMBER                        
*                                     FROM REP VERSION NUMBER                   
         OC    IFELCODE(IFELLEN),IFELCODE SPOTPAK XFER ELEMENT?                 
         BZ    APRO0070            NO                                           
         GOTO1 LOADELT,DMCB,(R4),IFELCODE,=C'ADD=CODE'                          
*                                                                               
APRO0070 EQU   *                   SAVE OFF DEMO VALUES, IF ANY                 
         GOTO1 =A(SAVEDEMO),RR=RELO                                             
*                                                                               
APRO0080 EQU   *                                                                
*                                                                               
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 SEQ                 ACCESS NEXT X'41' RECORD                     
         CLC   KEY(25),KEYSAVE     SAME KEY? THROUGH RECORD TYPE?               
         BE    APRO0120            YES - CONTINUE                               
         CLI   BUYUPDAT,C'Y'       WRITE THIS RECORD?                           
         BNE   APRO0400            NO  - SKIP THE REWRITE                       
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BO    APRO0400            YES - ALREADY WRITTEN                        
         TM    MISCFLAG,MF1CDTE    CONFLICT END DATE                            
         BO    APRO0400            YES-ALREADY WRITTEN                          
         GOTO1 GENREC,DMCB,(RC),RBUYREC                                         
*                                  NO  - OUTPUT PREVIOUS RECORD                 
         B     APRO0400            FINISHED WITH GENERATION                     
APRO0120 EQU   *                                                                
         CLC   KEY+25(1),KEYSAVE+25                                             
*                                  SAME BUYLINE #?                              
         BE    APRO0140            YES - CONTINUE                               
         CLI   BUYUPDAT,C'Y'       WRITE THIS RECORD?                           
         BNE   APRO0140            NO  - SKIP THE REWRITE                       
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BO    APRO0126            NO  - GO PUT IT OUT                          
         TM    MISCFLAG,MF1CDTE    CONFLICT END DATE BUYS                       
         BZ    APRO0130            NO  - GO PUT IT OUT                          
*                                                                               
APRO0126 DS    0H                                                               
*                                                                               
*                                                                               
*   NOTE:  'DAILY'S ARE GENERATED FROM WITHIN THE BUYDETL ROUTINE.              
*      AS SUCH, THE BUYLINE# IS BUMPED AFTER THE RECORD IS WRITTEN.             
*      WHEN THE AGENCY BUY IS COMPLETED, THE BUYLINE # IS SET TO                
*      THE NEXT EXPECTED BUYLINE.  TO ENSURE THAT IT IS NOT DOUBLE-             
*      INCREMENTED, IT IS BACKED OFF HERE.                                      
*                                                                               
         ZIC   RF,BUYLINE#         YES - DROP BUYLINE # BY 1                    
         BCTR  RF,0                                                             
         STC   RF,BUYLINE#                                                      
         B     APRO0140            DON'T PUT IT OUT AGAIN                       
APRO0130 EQU   *                                                                
         GOTO1 GENREC,DMCB,(RC),RBUYREC                                         
*                                  NO  - OUTPUT PREVIOUS RECORD                 
APRO0140 EQU   *                                                                
         MVC   AIO,AIO1            SET A(IO AREA 1)                             
         GOTO1 GETREC              RETRIEVE RECORD                              
         GOTO1 =A(CHKDAREC),DMCB,(RC),(R3),(R5),RR=RELO                         
*                                  ROUTINE SETS 'BUYUPDAT' TO NO                
*                                     WHEN HEADER IS SOFT-DELETED.              
*                                     THIS WILL PREVENT OUTPUT.                 
*                                  ALSO SETS 'SKIP RECORD' FLAG FOR             
*                                     OTHER RECORD TYPES                        
*                                                                               
         CLI   KEY+26,X'00'        BUY HEADER?                                  
         BE    APRO0040            YES - START NEXT HEADER                      
         CLI   KEY+26,X'10'        BUY ORBIT?                                   
         BNE   APRO0160            NO                                           
         CLI   SKIPRECS,C'Y'       SKIP THIS RECORD?                            
         BE    APRO0080            YES - GO BACK FOR NEXT                       
         GOTO1 BUYORBIT,DMCB,(RC),(R3)                                          
*                                  PROCESS BUY ORBIT RECORD                     
         B     APRO0080            GO BACK FOR NEXT RECORD                      
APRO0160 EQU   *                                                                
         CLI   KEY+26,X'20'        BUY COMMENT?                                 
         BNE   APRO0200            NO                                           
         CLI   SKIPRECS,C'Y'       SKIP THIS RECORD?                            
         BE    APRO0080            YES - GO BACK FOR NEXT                       
         GOTO1 BUYCOMMT,DMCB,(RC),(R3)                                          
*                                  PROCESS BUY COMMENT RECORD                   
         B     APRO0080            GO BACK FOR NEXT RECORD                      
APRO0200 EQU   *                                                                
         CLI   KEY+26,X'30'        BUY DETAIL?                                  
         BNE   APRO0240            NO  - ONLY TRAILER LEFT                      
         CLI   SKIPRECS,C'Y'       SKIP THIS RECORD?                            
         BE    APRO0080            YES - GO BACK FOR NEXT                       
         CLC   PROGNAME,SPACES     ANY PROGRAM NAME?                            
         BE    APRO0220            NO  - DON'T NEED COMMENT                     
         CLI   KATZEDI,C'Y'        KATZ/EDI USES ONLY THE FIRST 32              
         BNE   APRO0210            WITH THE LAST 2 FOR DAYPART                  
         CLC   PROGNAME(32),SPACES ANY PROGRAM NAME?                            
         BE    APRO0220            NO  - DON'T NEED COMMENT                     
APRO0210 DS    0H                                                               
         GOTO1 =A(GENCOMMT),DMCB,(RC),RBUYREC,(R5),RR=RELO                      
APRO0220 EQU   *                                                                
         GOTO1 =A(BUYDETL),DMCB,(R3),RR=RELO                                    
*                                  PROCESS BUY DETAIL RECORD                    
         B     APRO0080            GO BACK FOR NEXT RECORD                      
APRO0240 EQU   *                                                                
         DC    H'0'                UNKNOWN RECORD SUBTYPE                       
APRO0400 EQU   *                                                                
*                                  PREPARE TO REWRITE CONTRACT RECORD           
         MVC   AIO,AIOAREA         SET IO AREA TO READ OLD RECORD               
         L     R2,AIO3                                                          
         USING RCONREC,R2                                                       
*                                                                               
         MVC   KEY(27),RCONREC     GET KEY FROM NEW RECORD                      
         MVC   KEYSAVE,KEY                                                      
         OI    DMINBTS,X'08'       RETURN DELETED KEY ALSO                      
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY     REDUNDANT CHECK: KEY FOUND?                  
         BE    *+6                 YES                                          
         DC    H'0'                ???                                          
         NI    KEY+27,X'FF'-X'80'  RESTORE KEY                                  
         OI    DMINBTS,X'08'       RETURN DELETED RECORDS ALSO                  
         GOTO1 GETREC              READ INTO AIOAREA                            
         MVC   AIO,AIO3            RESET TO UPDATED CON RECORD                  
*                                                                               
*                                  UPDATE CONTRACT STATUS ELEMENT               
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        RETRIEVE                                     
         BRAS  RE,GETEL                                                         
         BE    APRO0520            FOUND: UPDATE IT                             
         XC    ELTBUILD,ELTBUILD   NOT FOUND: BUILD IT                          
         MVI   ELTBUILD,X'1D'      INSERT ELEMENT CODE                          
         LA    RF,RCONDL2Q                                                      
         STC   RF,ELTBUILD+1       INSERT ELEMENT LENGTH                        
*                                  SET 'OPENED' AND LINKED FLAG                 
         OI    ELTBUILD+2,X'40'+X'80'                                           
                                                                                
         TM    MISCFLAG,X'80'      DAILY ORDER?                                 
         BZ    *+8                                                              
         OI    ELTBUILD+2,X'08'    SET DAILY FLAG                               
*                                                                               
         CLI   KATZEDI,C'Y'                                                     
         BNE   *+8                                                              
         OI    ELTBUILD+2,X'04'    SET KATZ EDI ORDER                           
*                                                                               
         GOTO1 HEXIN,DMCB,RETORD#,ELTBUILD+3,8                                  
*                                  INSERT HEX VALUE OF AGY ORD #                
         MVC   ELTBUILD+RCONDRDA-RCONDREL(4),ACTDATE                            
*                                  YES - MOVE DATE+TIME TO OPENED               
APRO0480 EQU   *                                                                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBUILD,         X        
               =C'ADD=CODE'                                                     
         B     APRO0560                                                         
APRO0520 EQU   *                                                                
                                                                                
         MVI   2(R6),X'C0'         SET 'OPENED+LINKED' FLAGS, CLEAR             
*                                     ANY PREVIOUS VALUE                        
         TM    MISCFLAG,X'80'      DAILY ORDER?                                 
         BZ    *+8                                                              
         OI    2(R6),X'08'         SET DAILY FLAG                               
*                                                                               
         CLI   KATZEDI,C'Y'                                                     
         BNE   *+8                                                              
         OI    2(R6),X'04'         SET KATZ EDI ORDER                           
*                                                                               
         MVC   RCONDRDA-RCONDREL(4,R6),ACTDATE                                  
*                                  YES - MOVE DATE+TIME TO OPENED               
APRO0560 EQU   *                   UPDATE EASI CODES, IF NOT ALREADY            
*                                     PRESENT IN RECORD                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'        RETRIEVE                                     
         BRAS  RE,GETEL                                                         
         BE    APRO0600            FOUND: JUST REPLACE THE CODES                
         XC    ELTBUILD,ELTBUILD   NOT FOUND: BUILD IT                          
         MVC   ELTBUILD(2),=X'A220'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         MVC   ELTBUILD+2(L'SAVEEASI),SAVEEASI                                  
*                                  INSERT EASI CODES FROM AGY HEADER            
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBUILD,         X        
               =C'ADD=CODE'                                                     
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'        RETRIEVE                                     
         BRAS  RE,GETEL                                                         
         BE    APRO0605            MUST BE THERE!                               
         DC    H'0'                                                             
APRO0600 EQU   *                                                                
         MVC   RCONIADV-RCONIEL(L'SAVEEASI,R6),SAVEEASI                         
*                                  REPLACE EXISTING CODES WITH NEW              
*                                                                               
* MIGRATE TO NEW EXPANDED ESTIMATE FIELD                                        
*                                                                               
APRO0605 EQU   *                                                                
EASID    USING RCONIEL,R6                                                       
         XC    EASID.RCONXEST,EASID.RCONXEST                                    
         MVC   EASID.RCONXEST(L'RCONIEST),EASID.RCONIEST                        
         XC    EASID.RCONIEST,EASID.RCONIEST                                    
         DROP  EASID                                                            
*                                                                               
APRO0610 EQU   *                                                                
         MVI   FLTKEYFG,C'N'       DEFAULT DON'T REWRITE 8D/8E POINTERS         
*                                  OVERRIDE REP CON'S FLIGHT DATES              
*                                     WITH AGENCY ORDER'S DATES                 
         GOTO1 DATCON,DMCB,(2,FLTDATES),(3,WORK)                                
         GOTO1 DATCON,(R1),(2,FLTDATES+2),(3,WORK+3)                            
*                                                                               
*                                  SAVE ORIGINAL CONTRACT DATES                 
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,CONFLTDT)                            
         GOTO1 DATCON,(R1),(3,RCONDATE+3),(2,CONFLTDT+2)                        
*                                                                               
         CLC   RCONDATE,WORK       IF DIFFERENT, FLAG TO UPDATE                 
         BE    APRO0620            8E/8D KEYS TO CONTRACT                       
         MVC   RCONDATE,WORK                                                    
         MVI   FLTKEYFG,C'Y'                                                    
*                                                                               
APRO0620 EQU   *                                                                
         CLC   RTKODATE,RCONDATE   DATE CHOPPING??                              
         BNH   APRO0630                                                         
         MVC   RCONDATE(3),RTKODATE                                             
         MVI   FLTKEYFG,C'Y'                                                    
*                                                                               
APRO0630 EQU   *                                                                
         MVC   RCONBUYR(20),SAVEBUYR                                            
*                                  OVERRIDE BUYER NAME WITH A/O NAME            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'05'        PROD EXPANSION ELEMENT                       
         BRAS  RE,GETEL                                                         
         BNE   APRO0650                                                         
         USING RCONEXEL,R6                                                      
         MVC   RCONEXPR,SAVEPROD   OVERRIDE REP'S PRODUCT NAME(S)               
         DROP  R6                                                               
*                                                                               
* IF CONFIRMED, MARK PREVIOUSLY CONFIRMED AND UNCONFIRMED                       
* ONLY VARIOUS ORDERS WILL BE AFFECTED                                          
*                                                                               
APRO0650 EQU   *                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'        EXTENDED DESCRIPTION ELEMENT                 
         BRAS  RE,GETEL                                                         
         BNE   APRO0655                                                         
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BZ    APRO0655                                                         
         NI    RCONCONF,X'FF'-X'40'                                             
         OI    RCONCONF,X'80'+X'20'                                             
         DROP  R6                                                               
*                                                                               
APRO0655 EQU   *                                                                
         GOTO1 =A(SAVEDCAT),RR=RELO                                             
*                                                                               
         GOTO1 PUTREC              WRITE UPDATED RECORD TO FILE                 
         MVC   RECADDR,KEY+28      SAVE DISK ADDRESS OF RECORD                  
         GOTO1 WRITE               REWRITE CLEARED KEY FOR RECORD               
*                                                                               
         CLI   FLTKEYFG,C'Y'       NEED TO REFRESH 8D/E POINTER?                
         BNE   APRO0660                                                         
*                                  YES! CONFLTDT HAS OLD DATES                  
         GOTO1 =A(GEN8DEKY),DMCB,(RC),(R5),RR=RELO                              
*                                                                               
APRO0660 EQU   *                                                                
         CLI   TRUFLAG,C'Y'        EC/SAR KEY NEEDED?                           
         BNE   APRO0680            NO                                           
         GOTO1 GENECKEY            YES - GENERATE KEY                           
APRO0680 EQU   *                                                                
         B     EXIT                                                             
*                                                                               
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*                                                                               
*   GENREC:  OUTPUTS THE NEWLY CREATED BUY RECORD.  THEN RE-ESTAB-              
*        LISHES THE SEQUENCE FOR READING THE X'41' RECORDS.                     
*                                                                               
GENREC   NTR1                                                                   
         MVC   SAVEKEY,KEY         SAVE CURRENT KEY FOR RESTART                 
         MVC   AIO,AIO2            SET IO AREA = IOAREA2                        
         L     R2,AIO                                                           
         USING RBUYREC,R2                                                       
*                                                                               
*   INSERT TOTAL SPOTS, TOTAL COST, AND TOTAL WEEKS FIGURES                     
*                                                                               
         MVC   RBUYTSPT,SPOTCTR+2  LAST TWO POSITIONS ONLY                      
         L     RF,SPOTCTR          ACCUMULATE TOTAL SPOTS                       
         A     RF,ORDTOTSP                                                      
         ST    RF,ORDTOTSP         SAVE ORDER TOTAL SPOTS                       
         MVC   RBUYTWKS,WEEKCTR+3  LAST POSITION ONLY                           
         ZICM  RF,RBUYTSPT,2       LOAD TOTAL SPOTS                             
         SR    R0,R0                                                            
         L     R1,BUYCOST          LOAD COST PER SPOT                           
         MR    R0,RF               # SPOTS X COST/SPOT                          
         STCM  R1,15,RBUYTCOS      INSERT TOTAL COST                            
         A     R1,ORDTOT$$         ACCUMULATE ORDER TOTAL DOLLARS               
         ST    R1,ORDTOT$$         SAVE ORDER TOTAL DOLLARS                     
         XC    SPOTCTR,SPOTCTR     CLEAR ACCUMULATORS                           
         XC    WEEKCTR,WEEKCTR                                                  
         XC    BUYCOST,BUYCOST                                                  
         MVC   KEY,RBUYKEY         GET KEY OF BUY                               
*                                  LOOK FOR OLD RECORD                          
*                                     GENCON DOESN'T LIKE 'ADDREC'              
         MVC   KEYSAVE,KEY                                                      
         OI    DMINBTS,X'08'       RETURN DELETED KEYS ALSO                     
         GOTO1 HIGH                READ THE KEY                                 
         CLC   KEYSAVE(27),KEY     KEY ALREADY ON FILE?                         
         BE    GENR0040            YES - RECORD/KEY MUST BE REPLACED            
*                                                                               
         MVC   KEY(27),KEYSAVE     NO  - RESET NEW KEY                          
         CLI   RBUYVER,1           VERSION = 1?                                 
         BE    GENR0020            YES - ORIGINAL CREATION                      
         MVI   RBUYCHGI,C'A'       NO  - ADDED ON THIS PASS                     
         MVI   RBUYCHGI+1,0        CLEAR SECOND CHG BYTE                        
GENR0020 EQU   *                                                                
         GOTO1 ADDREC              ADD THE RECORD/KEY                           
*                                     GENCON TRAPS ERRORS                       
         B     GENR0080            RECORD ADDED SUCCESSFULLY                    
GENR0040 EQU   *                                                                
         MVC   AIO,AIOAREA         SET ALTERNATE IO AREA                        
         GOTO1 GETREC              READ RECD INTO AIOAREA                       
         MVC   AIO,AIO2            RESET IOAREA TO NEW BUY REC                  
         GOTO1 =A(GENREC2),DMCB,(RC),(R5),RR=RELO                               
*                                  COMPARE OLD/NEW FOR CHANGE CODE              
*                                                                               
*                                     OLD RECORD MAY HAVE BEEN DELETED:         
*                                     STILL MUST ACCESS KEY/RECORD              
*                                        FOR UPDATE                             
*                                                                               
*   GENERAL NOTE:  AT PRESENT, THE BUYS ARE ALL BLOWN AWAY BY A                 
*      GENERAL SUBROUTINE AT THE START OF THE JOB.  THERE IS NO                 
*      WAY TO DETERMINE WHICH BUYS ARE TO REMAIN, WHICH TO BE                   
*      DELETED ANY OTHER WAY.  THE FOLLOWING ITEMS OF CODE ARE                  
*      INCLUDED FOR THAT TIME WHEN ''CHANGES'' ARE PROCESSED.                   
*      AT THAT TIME, BUYS WILL BE PROCESSED SINGLY.                             
*                                                                               
         MVI   DELBUKTS,0          CLEAR 'DELETE BUCKET' FLAG                   
         TM    KEY+27,X'80'        KEY DELETED?                                 
         BO    GENR0050            YES - NOTHING TO CLEAR                       
         MVI   DELBUKTS,1          SET 'DELETE BUCKET' FLAG                     
GENR0050 EQU   *                                                                
         NI    KEY+27,X'FF'-X'80'  RESTORE KEY                                  
         MVC   AIO,AIOAREA         SET ALTERNATE IO AREA                        
         OI    DMINBTS,X'08'       RETURN DELETED KEYS ALSO                     
         GOTO1 GETREC              RETRIEVE ORIGINAL RECORD                     
         CLI   DELBUKTS,0          DELETED RECORD IN PROCESS?                   
         BE    GENR0060            YES - DON'T BACK BUCKETS OUT                 
         MVI   BUCKFLAG,X'FF'      NO  - SET TO BACK OUT FIGURES                
         MVC   REJMESS(7),=C'BILLUHR'                                           
         LA    RF,2                                                             
         GOTO1 =A(BUCKUPDT),DMCB,(RC),(R5),(RF),RR=RELO                         
*                                  BACK OUT BUCKETS                             
GENR0060 EQU   *                                                                
         MVC   AIO,AIO2            SET IO AREA FOR NEW BUY                      
         GOTO1 PUTREC              WRITE UPDATED RECORD TO FILE                 
         GOTO1 WRITE               REWRITE CLEARED KEY FOR RECORD               
GENR0080 EQU   *                                                                
         MVI   BUCKFLAG,X'00'      SET TO ADD NEW FIGURES                       
         MVC   REJMESS(7),=C'BILLUHR'                                           
         LA    RF,3                                                             
         GOTO1 =A(BUCKUPDT),DMCB,(RC),(R5),(RF),RR=RELO                         
*                                  ADD NEW BUCKETS                              
GENR0100 EQU   *                                                                
         MVC   AIO,AIO1            RESET A(X'41' RECORD AREA)                   
         MVC   KEY,SAVEKEY         RESET KEY FOR X'41' RECS                     
         GOTO1 HIGH                                                             
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   GENECKEY:  GENERATE AN EC KEY FOR SAR ACTIVITY                              
*                                                                               
GENECKEY NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'EC'           INSERT KEY ID                                
         MVC   KEY+23(4),COMPCON   INSERT CON# 9S COMP/REV                      
         MVC   KEY+21(2),TWAAGY                                                 
         OI    DMINBTS,X'08'       RETURN DELETED KEYS ALSO                     
         MVI   DMOUTBTS,0                                                       
         GOTO1 HIGH                READ KEY                                     
         BAS   RE,CHECK                                                         
         CLC   KEY(27),KEYSAVE                                                  
         BE    GENEXIT             ALREADY THERE - DON'T READD                  
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+28(4),RECADDR   INSERT RECORD ADDRESS                        
*                                                                               
*                                                                               
         GOTO1 ADD                                                              
         BAS   RE,CHECK                                                         
GENEXIT  EQU   *                                                                
         XIT1                                                                   
*                                                                               
CHECK    TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*   BUYORBIT:  BUILDS DAY-TIME ELEMENT FROM THE ORBIT ELEMENTS                  
*        IN THE RECORD, INSERTS THEM INTO THE NEW BUY RECORD.                   
*                                                                               
BUYORBIT NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R3,4(R1)            RESET A(X'41' RECORD)                        
         USING RDARREC,R3                                                       
         LA    R8,RDARELEM         SET A(X'01' ELEMENT)                         
         ZIC   RF,1(R8)            GET LENGTH                                   
         AR    R8,RF               BUMP TO 1ST ORBIT ELEMENT                    
         MVI   ORBFLAG,C'Y'        SET 'ORBIT PRESENT' INDICATOR                
*                                     ORBIT TAKES PRIORITY OVER                 
*                                     BUY DAY/TIME ELTS                         
BORB0040 EQU   *                                                                
*                                  CLEAR START/END DAYS                         
         CLI   0(R8),0             END OF RECORD?                               
         BE    BORB0400            YES - FINISHED                               
*                                                                               
         USING RDAROEEL,R8                                                      
*                                                                               
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         XC    STARTDAY(2),STARTDAY                                             
*                                  CLEAR START/END DAYS                         
         MVC   ELTBUILD(2),=X'0209'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         GOTO1 =A(STARTEND),DMCB,(RC),RDAROERO,ELTBUILD+2,(R5),RR=RELO          
*                                  INSERT START/END DAY                         
         CLI   ORBSTDAY,0          ANY ENTRY IN ORBIT START DAY?                
         BNE   BORB0060            YES - DON'T REPLACE IT                       
         MVC   ORBSTDAY,STARTDAY   NO  - SAVE FIRST ORBIT START DAY             
*                                                                               
* EARLIEST ORBIT START DAY MIGHT BE LATER THAN CURRENT STAY DAY AS              
* SPECIFIED IN THE BUY HEADER. WE'LL TAKE THE FIRST ORBIT START DAY             
* AS THE OVERALL BUY START DAY                                                  
*                                                                               
* NOTE THAT THIS ASSUMES DAILYS SHOULD NEVER BE ACCOMPANIED WITH                
* ORBITS!!                                                                      
*                                                                               
         L     R2,AIO2             A(BUY RECORD IN PROGRESS)                    
         USING RBUYREC,R2                                                       
         ZIC   RF,ORBSTDAY                                                      
         SLL   RF,4                SHIFT TO LOW ORDER, HI NIBBLE                
         ZIC   RE,RBUYSTED                                                      
         SLL   RE,8+8+8+4          SHIFT OFF HI NIBBLE OR CURRENT               
         SRL   RE,8+8+8+4          START DAY                                    
         AR    RE,RF                                                            
         STC   RE,RBUYSTED                                                      
         DROP  R2                                                               
*                                                                               
BORB0060 EQU   *                                                                
         SR    RE,RE               INITIALIZE FINAL OUTPUT                      
         LA    RF,7                SET LOOP CONTROL                             
         LA    R1,RDAROERO         SET A(ROTATION ARRAY)                        
BORB0080 EQU   *                                                                
         CLI   0(R1),C' '          DAY SET?                                     
         BE    BORB0120            NO                                           
         CLI   0(R1),0             DAY SET?                                     
         BE    BORB0120            NO                                           
         LA    RE,1(RE)            YES - TURN ON LOW-ORDER BIT                  
BORB0120 EQU   *                                                                
         SLL   RE,1                SHIFT UP 1 'DAY'                             
         LA    R1,1(R1)            BUMP TO NEXT ARRAY POSITION                  
         BCT   RF,BORB0080         GO BACK AND TEST NEXT                        
         SRL   RE,1                SHIFT BACK 1 'DAY'                           
         STC   RE,ELTBUILD+3       INSERT DAYS INTO ELEMENT                     
         MVC   ELTBUILD+4(4),RDAROEST                                           
*                                  INSERT START/END TIMES INTO ELEMENT          
         MVI   ELTBUILD+8,1        INSERT WEIGHT OF 1                           
*                                  THIS DOESN'T SEEM TO CHANGE                  
*                                     NO IDEA WHY.....                          
         L     R2,AIO2             A(BUY RECORD IN PROGRESS)                    
         GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
*                                                                               
*   NOTE:  DAY/TIME ELEMENTS ENTERED VIA ORBITS WILL BE DELETED IF              
*      BUY HAS BEEN ENTERED AS A 'DAILY' BUY.  THIS SHOULD NOT HAPPEN,          
*      BUT HAS BEEN HANDLED IF IT DOES.                                         
*                                                                               
         ZIC   RF,1(R8)                                                         
         AR    R8,RF                                                            
         B     BORB0040            GO BACK FOR NEXT ELEMENT                     
BORB0400 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3,R8                                                            
         EJECT                                                                  
*                                                                               
*   BUYCOMMT:  BUILDS BUY COMMENT ELEMENT FOR THE FIRST TWO COMMENTS            
*        IN THE RECORD, INSERTS THEM INTO THE NEW BUY RECORD.                   
*                                                                               
BUYCOMMT NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R3,4(R1)            RESET A(X'41' RECORD)                        
         USING RDARREC,R3                                                       
*                                                                               
         L     R2,AIO2             A(BUY RECORD IN PROGRESS)                    
         LA    R4,2                LOOP CONTROL:                                
*                                     FIRST TWO ARE BUY COMMENTS                
*                                     NEXT  TWO ARE BUY ORDER COMMENTS          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         CLC   PROGNAME,SPACES     ANY PROGRAM NAME?                            
         BE    BCOM0020            NO  - PROCEED                                
         CLI   KATZEDI,C'Y'        KATZ/EDI USES ONLY THE FIRST 32              
         BNE   BCOM0005            WITH THE LAST 2 FOR DAYPART                  
*                                                                               
*                                  REMOVE OLD X'ED' DAYPART CODE                
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'ED',(R2)),0,0                   
         XC    ELEM,ELEM           FOR KATZ/EDI ADD THE DAYPART CODE            
         LA    R6,ELEM             ELEMENT                                      
         USING RBUYEDEL,R6                                                      
         MVI   RBUYEDCD,X'ED'                                                   
         MVI   RBUYEDLN,RBUYEDLQ                                                
         MVC   RBUYEDDP,PROGNAME+32                                             
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R2),ELEM,0                        
         DROP  R6                                                               
*                                                                               
         CLC   PROGNAME(32),SPACES ANY PROGRAM NAME?                            
         BE    BCOM0020            NO  - DON'T NEED COMMENT                     
         DROP  R8                                                               
*                                                                               
BCOM0005 EQU   *                                                                
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         MVI   ELTBUILD,X'04'      INSERT ELEMENT CODE                          
*                                  CLEAR WORKSPACE                              
         LA    R6,PROGNAME+33      SCAN PROGRAM NAME FOR BLANKS                 
         LA    RF,34               LOOP CONTROL                                 
         CLI   KATZEDI,C'Y'        KATZ/EDI USES ONLY THE FIRST 32              
         BNE   BCOM0010            WITH THE LAST 2 FOR DAYPART                  
         LA    R6,PROGNAME+31      SCAN PROGRAM NAME FOR BLANKS                 
         LA    RF,32               LOOP CONTROL                                 
BCOM0010 EQU   *                                                                
         CLI   0(R6),C' '          CHARACTER = SPACE?                           
         BNE   BCOM0015            NO  - LAST CHARACTER FOUND                   
         BCTR  R6,0                YES - BACK UP 1 SPACE                        
         BCT   RF,BCOM0010         LOOP THROUGH ALL                             
         DC    H'0'                SHOULDN'T HAPPEN:  SPACES CHECKED            
BCOM0015 EQU   *                                                                
         LA    RF,1(RF)            ADD 1 FOR KEYWORD (+2 -1 FOR EX)             
         MVC   ELTBUILD+2(2),=C'P='                                             
*                                  INSERT KEYWORD                               
         EX    RF,BCOM0505         MOVE PROGRAM NAME BY LENGTH                  
         LA    RF,3(RF)            RESTORE LENGTH + L(CONTROLS)                 
         STC   RF,ELTBUILD+1       INSERT LENGTH INTO ELEMENT                   
*                                                                               
* SKIP BUILDING P= COMMENT PROGRAM NAME ELEMENT                                 
*                                                                               
*        GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
*                                                                               
* BUILD DEDICATED PROGRAM NAME ELEMENT                                          
*                                                                               
         MVI   ELTBUILD,X'21'      PROGRAM NAME ELEMENT                         
         XC    ELTBUILD+2(L'ELTBUILD-2),ELTBUILD+2                              
         ZIC   RF,ELTBUILD+1       REUSE LENGTH FROM THE P= COMMENT             
         SHI   RF,2                ELEMENT TO BUILD LENGTH OF                   
         STC   RF,ELTBUILD+1       PROGRAM NAME ELEMENT                         
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELTBUILD+2(0),PROGNAME                                           
         GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
*                                                                               
         MVI   PROGNAME,C' '       CLEAR THE PROGRAM NAME                       
         MVC   PROGNAME+1(L'PROGNAME-1),PROGNAME                                
*        BCTR  R4,0                SUBTRACT 1 FROM COMMENT COUNT                
BCOM0020 EQU   *                                                                
         LA    R8,RDARELEM         SET A(X'01' ELEMENT)                         
         ZIC   RF,1(R8)            GET LENGTH                                   
         AR    R8,RF               BUMP TO 1ST COMMT ELEMENT                    
BCOM0040 EQU   *                                                                
         CLI   0(R8),0             END OF RECORD?                               
         BE    BCOM0400            YES - FINISHED                               
         CLI   0(R8),2             COMMENT ELEMENT?                             
         BE    BCOM0050                                                         
         BH    BCOM0400                                                         
         ZIC   RF,1(R8)                                                         
         AR    R8,RF                                                            
         B     BCOM0040                                                         
*                                                                               
BCOM0050 EQU   *                                                                
         USING RDARCTEL,R8                                                      
*                                                                               
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         MVI   ELTBUILD,X'04'      INSERT ELEMENT CODE                          
         ZIC   R1,1(R8)            GET ELEMENT LENGTH                           
         LA    RF,3                DECREMENT FOR EX + CTRL                      
         SR    R1,RF                                                            
         LA    RE,59               MAX SIZE FOR REP COMMENTS = 60               
         CR    R1,RE               NEW INPUT VS REP MAX                         
         BNH   BCOM0060            ACCEPTABLE                                   
         LR    R1,RE               NOT ACCEPTABLE                               
BCOM0060 EQU   *                                                                
         EX    R1,BCOM0500         MOVE ELEMENT TO BUILT AREA                   
         LA    R1,3(R1)            SET ELEMENT LENGTH                           
         STC   R1,ELTBUILD+1       INSERT ELEMENT LENGTH                        
         GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
*                                                                               
         ZIC   RF,1(R8)                                                         
         AR    R8,RF                                                            
         BCT   R4,BCOM0040         GO BACK FOR NEXT ELEMENT                     
*                                     IF COUNT < 2                              
*                                                                               
* IF MORE COMMENTS, SKIP PUTTING IT IN BUY ORDER COMMENT AREA AND EXIT          
*                                                                               
*&&DO                                                                           
         LA    R4,2                IF MORE COMMENTS, PUT TO                     
*                                     BUY ORDER COMMENT RECORDS                 
BCOM0080 EQU   *                                                                
         CLI   0(R8),0             END OF RECORD?                               
         BE    BCOM0400            YES - FINISHED                               
         CLI   0(R8),2             PROCESS ONLY FOR COMMENTS                    
         BE    BCOM0085                                                         
         BH    BCOM0400                                                         
         ZIC   RF,1(R8)                                                         
         AR    R8,RF                                                            
         B     BCOM0080                                                         
*                                                                               
*                                                                               
BCOM0085 EQU   *                                                                
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         MVI   ELTBUILD,X'84'      INSERT ELEMENT CODE                          
         ZIC   R1,1(R8)            GET ELEMENT LENGTH                           
         LA    RF,3                DECREMENT FOR EX + CTRL                      
         SR    R1,RF                                                            
*                                                                               
         CH    R1,=H'60'           TRUNCATE IF MORE THAN 60 CHARS.              
         BL    BCOM0090                                                         
         LA    R1,59               MAX - 1 FOR EX                               
*                                                                               
BCOM0090 EQU   *                                                                
         EX    R1,BCOM0510         MOVE ELEMENT TO BUILD AREA                   
         LA    R1,4(R1)            RESET ELEMENT LENGTH                         
         STC   R1,ELTBUILD+1       INSERT ELEMENT LENGTH                        
         MVI   ELTBUILD+2,X'80'    TURN ON 'SENT BY REP'                        
         GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
*                                                                               
         ZIC   RF,1(R8)                                                         
         AR    R8,RF                                                            
         BCT   R4,BCOM0080         GO BACK FOR NEXT ELEMENT                     
*                                     IF COUNT < 2                              
*&&                                                                             
BCOM0400 EQU   *                                                                
         XIT1                                                                   
*                                                                               
BCOM0500 MVC   ELTBUILD+2(0),2(R8) SET UP BUY COMMENT                           
*                                                                               
BCOM0505 MVC   ELTBUILD+4(0),PROGNAME                                           
*                                  SET UP PROGRAM NAME COMMENT                  
BCOM0510 MVC   ELTBUILD+3(0),2(R8) SET UP BUY ORDER COMMENT                     
*                                                                               
         DROP  R3,R8                                                            
         EJECT                                                                  
*                                                                               
*   EFFDATEL:  BUILD AN ALTERNATE X'02' ELEMENT IN EVENT THERE                  
*        ARE NO ORBIT RECORDS.                                                  
*                                                                               
EFFDATEL NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R3,4(R1)            RESET A(X'41' RECORD)                        
         USING RDARREC,R3                                                       
         LA    R8,RDARELEM         SET A(X'01' ELEMENT)                         
         USING RDARBYEL,R8                                                      
*                                                                               
         XC    STARTDAY(2),STARTDAY                                             
*                                  CLEAR START/END DAY WORK AREA                
         XC    ELTBILD2,ELTBILD2   CLEAR ELEMENT BUILD AREA                     
         MVC   ELTBILD2(2),=X'0209'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         GOTO1 =A(STARTEND),DMCB,(RC),RDARBYRO,ELTBILD2+2,(R5),RR=RELO          
*                                  INSERT START/END DAY                         
                                                                                
         SR    RE,RE               INITIALIZE FINAL OUTPUT                      
         LA    RF,7                SET LOOP CONTROL                             
         LA    R1,RDARBYRO         SET A(ROTATION ARRAY)                        
EFFD0080 EQU   *                                                                
         CLI   0(R1),C' '          DAY SET?                                     
         BE    EFFD0120            NO                                           
         CLI   0(R1),0             DAY SET?                                     
         BE    EFFD0120            NO                                           
         LA    RE,1(RE)            YES - TURN ON LOW-ORDER BIT                  
EFFD0120 EQU   *                                                                
         SLL   RE,1                SHIFT UP 1 'DAY'                             
         LA    R1,1(R1)            BUMP TO NEXT ARRAY POSITION                  
         BCT   RF,EFFD0080         GO BACK AND TEST NEXT                        
         SRL   RE,1                SHIFT DOWN 1 'DAY' FOR                       
*                                     PROPER ALIGNMENT                          
         STC   RE,ELTBILD2+3       INSERT DAYS INTO ELEMENT                     
         ZICM  RF,RDARBYST,2       CHECK START TIME                             
         C     RF,=F'2400'         AFTER MIDNIGHT?                              
         BNH   EFFD0160            NO  - LEAVE AS IS.....                       
         S     RF,=F'2400'         YES - SUBTRACT 2400 FROM FIGURE              
EFFD0160 EQU   *                                                                
         STCM  RF,3,ELTBILD2+4     SAVE START TIME                              
*                                                                               
         CLC   RDARBYST,RDARBYET   IF SAME, SKIP END TIME                       
         BE    EFFD0210                                                         
*                                                                               
         ZICM  RF,RDARBYET,2       CHECK END   TIME                             
         C     RF,=F'2400'         AFTER MIDNIGHT?                              
         BNH   EFFD0200            NO  - LEAVE AS IS.....                       
         S     RF,=F'2400'         YES - SUBTRACT 2400 FROM FIGURE              
EFFD0200 EQU   *                                                                
         STCM  RF,3,ELTBILD2+6     SAVE END   TIME                              
*                                  INSERT START/END TIMES INTO ELEMENT          
EFFD0210 EQU   *                                                                
         MVI   ELTBILD2+8,1        INSERT WEIGHT OF 1                           
*                                  THIS DOESN'T SEEM TO CHANGE                  
*                                     NO IDEA WHY.....                          
         XIT1                                                                   
*                                                                               
         DROP  R3,R8                                                            
         EJECT                                                                  
*                                                                               
*   LOADELT :  LOADS ELEMENT IN WORKSPACE (ELTBUILD) TO RECORD.                 
*                                                                               
LOADELT  NTR1                                                                   
         L     R2,0(R1)            RESET A(TARGET RECORD)                       
         L     R3,4(R1)            RESET A(ELEMENT BUILD AREA)                  
         L     R4,8(R1)            RESET A(COMMAND EXTENSION)                   
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R2),(R3),(R4)                     
*                                  ADD ELT TO RECORD                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DELELT :  DROPS X'03' ELEMENT FROM BUYRECS                                  
*                                                                               
DELELT   NTR1                                                                   
         L     R2,0(R1)            RESET A(TARGET RECORD)                       
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(3,(R2)),0,0                       
*                                  DROP X'03' ELTS FROM BUYRECORD               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   DELELT02 :  DROPS X'02' ELEMENT(S) (DAY/TIME) FROM BUYRECS                  
*        WHEN THEY WERE GENERATED BY AN ORBIT RECORD, AND THE BUYS              
*        ARE 'DAILY'                                                            
*                                                                               
DELELT02 NTR1                                                                   
         L     R2,0(R1)            RESET A(TARGET RECORD)                       
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(2,(R2)),0,0                       
*                                  DROP X'02' ELTS FROM BUYRECORD               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SET THE PFKEY INFORMATION                                                     
***********************************************************************         
SETPFKYS NTR1                                                                   
         SR    R2,R2               NO PFKEY AT TABLE FIRST                      
***************                                                                 
* FOR ACTION OPEN/REJECT                                                        
***************                                                                 
         CLI   ACTNUM,ACTAPP       ACTION OPEN?                                 
         BE    STPFK10                                                          
         CLI   ACTNUM,ACTREJ       ACTION REJECT?                               
         BNE   STPFINIT                                                         
                                                                                
STPFK10  LA    R2,SPFTABLE         YES, USE LIST PFKEY TABLE                    
         TM    CTLRFLG1,CF1BRDQ                                                 
         BZ    STPFINIT                                                         
         LA    R2,BPFTABLE                                                      
*                                                                               
STPFINIT GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
*                                                                               
STPFX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OPEN/REJECT PFKEY TABLE DEFINITIONS                                           
***********************************************************************         
SPFTABLE  DS    0C                                                              
*                                                                               
* JUMP TO THE CONTRACT PROGRAM                                                  
         DC    AL1(SPF02X-*,02,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF02X   EQU   *                                                                
*                                                                               
* SEND REJECTION TO BUYER                                                       
         DC    AL1(SPF03X-*,03,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF03X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(SPF12X-*,12,PFTRPROG,0,0,0)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
SPF12X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
***********************************************************************         
* BRAND OPEN/REJECT PFKEY TABLE DEFINITIONS                                     
***********************************************************************         
BPFTABLE  DS    0C                                                              
*                                                                               
* JUMP TO THE CONTRACT PROGRAM                                                  
         DC    AL1(BPF02X-*,02,0,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8' ',CL8' '                                             
BPF02X   EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
*        DC    AL1(BPF12X-*,12,0,0,0,PFTRETRN)                                  
*        DC    CL3' ',CL8' ',CL8' '                                             
         DC    AL1(BPF12X-*,12,0,0,0,0)                                         
         DC    CL3' ',CL8'BRAND',CL8'SELECT'                                    
BPF12X   EQU   *                                                                
*                                                                               
* ACTUAL RETURN TO CALLER                                                       
*        DC    AL1(BPF24X-*,24,0,0,0,0)                                         
*        DC    CL3' ',CL8'BRAND',CL8'SELECT'                                    
BPF24X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVC   RERROR,=AL2(1)                                                   
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(2)                                                   
         B     ERREND                                                           
*                                                                               
INVLRCAC MVC   RERROR,=AL2(INVRCACT)                                            
         B     ERREND                                                           
*                                                                               
RECFULL  DS    0H                  FORCE ABEND AND REVERSE TRANSACTIONS         
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(RECFULLQ),RECFULLM                                       
         OI    CONHEADH+6,X'80'    XMIT                                         
         OI    CONACTH+6,X'40'     FORCE CURSOR HERE                            
         L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBALRM   SOUND A BEEP                                 
         DROP  RF                                                               
         DC    H'0',C'$ABEND'                                                   
*                                                                               
RECNTFND MVI   GERROR1,53          RECORD NOT FOUND                             
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 MYERROR                                                          
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
RECFULLM DC    C'RECORD FULL - ACTION NOT PROCESSED - CALL DDS'                 
RECFULLQ EQU   *-RECFULLM                                                       
         EJECT                                                                  
**********RINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE REPFACSQ                                                       
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE REROMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE REROMF9D                                                       
         EJECT                                                                  
       ++INCLUDE REDARTW2                                                       
         EJECT                                                                  
       ++INCLUDE REDARWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REDARDSECT                                                     
         EJECT                                                                  
RSTARECD DSECT                                                                  
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
RSALRECD DSECT                                                                  
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
RREPRECD DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
REDIRECD DSECT                                                                  
       ++INCLUDE REGENEDI                                                       
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
         EJECT                                                                  
       ++INCLUDE EDILINKD                                                       
         EJECT                                                                  
       ++INCLUDE REGLCON                                                        
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
* APPLICATION STORAGE AREA                                                      
*                                                                               
MYAREAD  DSECT                                                                  
ACTAPP   EQU   15                                                               
ACTREJ   EQU   16                                                               
MYLABEL  DS    CL8                 FOR IDENTIFIER                               
SAVEROTE DS    A                   SAVE A(ROTATION FIELD)                       
COMPCON  DS    CL4                 CONTRACT # COMP/REVERSED                     
ELTBUILD DS    CL96                ELEMENT BUILD AREA                           
ELTBILD2 DS    CL64                SECOND BUILD AREA                            
SVDEMCAT DS    XL16                SAVED DEMO CATEGORIES                        
PROGNAME DS    CL34                SAVE BUY PROGRAM NAME                        
REVNUM   DS    X                                                                
DETLFLAG DS    XL2                 TOTAL SPOT LENGTH                            
BUYUPDAT DS    CL1                 BUY WRITE FLAG                               
SKIPRECS DS    CL1                 GENERAL RECORD SKIP FLAG                     
TOTSPTUN DS    XL1                 TOTAL SPOT UNITS                             
ACTCODE  DS    CL1                 ACTION CODE:                                 
*                                     A  =  OPEN                                
*                                     R  =  REJECT                              
MISCFLAG DS    XL1                 MESSAGE FLAGS                                
*                                     X'80'  =  DAILY ORDER                     
*                                     X'40'  =  ORBIT DROPPED                   
*                                     X'20'  =  TRADE ORDER                     
MF1TKO   EQU   X'10'                  X'10'  =  TAKE OVER ORDER                 
MF1CDTE  EQU   X'08'                  X'08'  =  CONFLICT END DATE ORDER         
*                                                                               
ORBFLAG  DS    CL1                 ORBIT PRESENCE INDICATOR                     
*                                     Y  =  ORBIT PRESENT                       
BUCKFLGS DS    CL1                 INDICATOR FOR BUCKUP ROUTINE                 
EDICTACT DS    CL3                 EDICT 'ACTION'                               
RESENT   DS    CL1                 RESENT FLAG                                  
DELAGORD DS    CL1                 DELETE AGENCY ORDER FLAG                     
ACTDATE  DS    XL2                 DATE FOR STAMPING                            
ACTTIME  DS    XL2                 TIME FOR STAMPING                            
VERDFLT  DS    CL1                 VERSION NUMBER (REP)                         
CONMOD#  DS    CL1                 SAVE AREA FOR MOD NUMBER                     
BUYLINE# DS    X                   BUYLINE NUMBER BEING GENERATED               
NEWVER#  DS    CL1                 NEW RECORD VERSION NUMBER                    
NEEDSCRN DS    CL1                                                              
HDRDA    DS    XL4                 DISK ADDRESS OF DARE HEADER RECORD           
SAVEKEY  DS    CL27                KEY SAVE AREA FOR RESTARTS                   
SAVEPROD DS    CL20                PROD NAME(S) FROM AGENCY ORDER               
SAVEEASI DS    0CL16               SAVE AREA FOR EASI CODES                     
EASIADV  DS    CL4                 CLIENT CODE: 6 CHARS ON AGENCY HDR           
EASIPROD DS    CL4                                                              
EASIEST# DS    CL4                 EST#: 3 CHARS ON AGENCY HDR                  
EASIPRD2 DS    CL4                 PIGGY/PARTNER PRODUCT                        
FLTDATES DS    CL4                 AGENCY ORDER FLIGHT DATES                    
SAVEBUYR DS    CL24                BUYER NAME FROM AGENCY ORDER                 
STARTDAY DS    XL1                 START DAY FOR BUY                            
ENDDAY   DS    XL1                 END DAY FOR BUY                              
ORBSTDAY DS    XL1                 ORBIT START DAY                              
ROTATDAY DS    XL1                 ROTATION START DAY                           
DELBUKTS DS    CL1                 DELETE BUCKET FLAG                           
BUCKFLAG DS    CL1                 ADD/SUBTRACT FLAG FOR BUCKETS                
TRUFLAG  DS    CL1                 GENERATE EC/SAR KEY IF 'Y'                   
FLTKEYFG DS    CL1                 GENERATE 8E AND 8D KEY IF 'Y'                
CONFLTDT DS    CL4                 ORIGINAL CONTRACT FLIGHT DATES               
NEWFLTDT DS    CL4                 NEW CONTRACT FLIGHT DATES                    
RECADDR  DS    CL4                 DISK ADDRESS OF CONTRACT RECORD              
SAVEBU$$ DS    CL4                 BUY BREAK SAVE AREA                          
SENDID   DS    CL2                                                              
INTTYPE  DS    CL1                 INPUT TYPE                                   
SPLKEYAD DS    133C                EXTENDED SPOOL AREA                          
ORDRETRN EQU   *                   INFO FOR RETURN MESSAGES                     
RETORD#  DS    CL8                                                              
RETFROM  DS    CL10                                                             
RETTO    DS    CL10                                                             
RETSTAT  DS    CL6                                                              
RETCON#  DS    CL8                                                              
RETSENDR DS    CL16                                                             
*                                                                               
*   SPOTPAK TRANSFER ELEMENT                                                    
*                                                                               
IFELCODE DS    CL2                 CODE/LENGTH = X'0830'                        
IFSPAG   DS    CL2                 SPOT AGENCY POWER CODE                       
IFSPMD   DS    CL1                 SPOT MEDIA CODE                              
IFSPCL   DS    CL3                 SPOT CLIENT CODE                             
IFSPPRD  DS    CL3                 SPOT PRODUCT CODE                            
IFSPES   DS    CL1                 SPOT ESTIMATE NUMBER                         
IFSPPP   DS    CL3                 SPOT PRODUCT PIGGY                           
IFSPP1   DS    CL1                 SPOT PRODUCT 1 SPLIT                         
IFSPP2   DS    CL1                 SPOT PRODUCT 2 SPLIT                         
IFSPL#   DS    CL1                 SPOT BUYLINE NUMBER                          
IFSPST   DS    CL5                 STATION CALL LETTERS                         
IFSPADV  DS    CL4                 REP ADVERTISER CODE                          
IFSPRD   DS    CL3                 REP PRODUCT CODE                             
IFSPDT   DS    CL3                 SPOT TRANSFER DATE                           
IFSPTM   DS    CL4                 SPOT TRANSFER TIME                           
         DS    CL12                SPARE                                        
IFELLEN  EQU   *-IFELCODE          ELEMENT LENGTH                               
*                                                                               
WORK2    DS    300C                WORK SPACE FOR BUCKET BUILD                  
*                                     AND GENERAL COMPARISONS                   
         DS    0F                                                               
BUYCOST  DS    F                   COST OF SPOTS IN BUY                         
SPOTCTR  DS    F                   NUMBER SPOTS IN BUY                          
WEEKCTR  DS    F                   NUMBER WEEKS IN BUY                          
ORDTOT$$ DS    F                   ORDER TOTAL DOLLARS                          
ORDTOTSP DS    F                   ORDER TOTAL SPOTS                            
AREJMESS DS    A                   A(NEXT REJ MESS SPACE)                       
KATZEDI  DS    C                   Y/N KATZ EDI ORDER?                          
DAILYFLG DS    C                   DAILY PACING FLAG                            
MYSVAIO  DS    F                                                                
MYFLAG   DS    X                   MY PARAMETER FLAG                            
REJMESS  DS    600C                REJECTION MESSAGE BUILD AREA                 
*                                                                               
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTRHDLN DS    CL8                                                              
         DS    CL1                                                              
LSTRAORN DS    CL8                                                              
         DS    CL1                                                              
LSTRSTA  DS    CL6                                                              
         DS    CL1                                                              
LSTRAGY  DS    CL5                                                              
         EJECT                                                                  
ORDAPREJ DSECT                                                                  
ARTRANID DS    CL6                                                              
ARORDNUM DS    CL8                                                              
ARFROM   DS    CL10                                                             
ARTO     DS    CL10                                                             
ARDATE   DS    CL6                                                              
ARTIME   DS    CL4                                                              
ARSTAT   DS    CL6                                                              
ARCON#   DS    CL8                                                              
ARRETSND DS    CL16                                                             
ARDDS    DS    CL3                                                              
         SPACE 4                                                                
ORDSAL   DSECT                                                                  
OSTRANID DS    CL6                                                              
OSORDNUM DS    CL8                                                              
OSSPPCDE DS    CL3                                                              
OSSPPNME DS    CL20                                                             
         SPACE 4                                                                
ORDCOM   DSECT                                                                  
OCTRANID DS    CL6                                                              
OCORDNUM DS    CL8                                                              
OCCONTIN DS    CL1                                                              
OCBUYLIN DS    CL4                                                              
OCCOMMNT DS    CL70                                                             
OCDDS    DS    CL3                                                              
         SPACE 4                                                                
ORDTRLR  DSECT                                                                  
OTTRANID DS    CL6                                                              
OTORDNUM DS    CL8                                                              
OTCOUNT  DS    CL6                                                              
         EJECT                                                                  
**********************************************************************          
* BUILDS DEMO VALUES ELEMENT                                                    
**********************************************************************          
T83F20   CSECT                                                                  
SAVEDEMO NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   SVDEMX                                                           
         USING RDARBMEL,R6                                                      
         LA    R6,RDARBDM1                                                      
         DROP  R6                                                               
*                                                                               
         XC    ELEM,ELEM                                                        
ELEMD    USING RBUYDMEL,ELEM                                                    
         MVI   ELEMD.RBUYDMCD,RBUYDMCQ                                          
         MVI   ELEMD.RBUYDMLN,18                                                
         LA    R4,ELEMD.RBUYDMCV                                                
         LA    R3,4                MAX 4 DEMOS                                  
*                                                                               
         DROP  ELEMD                                                            
*                                                                               
SVDEM10  DS    0H                                                               
         MVC   0(4,R4),=4X'FF'     -1 MEANS VALUE NOT PROVIDED                  
         CLC   0(L'RDARBDM1,R6),SPACES                                          
         BE    SVDEM15                                                          
         PACK  DUB(8),0(7,R6)                                                   
         L     RF,DUB+4                                                         
         SRL   RF,4                DUMP THE SIGN                                
         STCM  RF,15,0(R4)                                                      
*                                                                               
SVDEM15  DS    0H                                                               
         AHI   R6,L'RDARBDM1                                                    
         AHI   R4,L'RBUYDMDM                                                    
         BCT   R3,SVDEM10                                                       
*                                                                               
SVDEM20  DS    0H                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO2,ELEM,=C'ADD=CODE'             
*                                                                               
SVDEMX   DS    0H                                                               
         XIT1                                                                   
         DROP  R8                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* BUILDS DEMO CATEGORIES ELEMENT                                                
**********************************************************************          
SAVEDCAT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'DD',AIO3),0,0                   
*                                                                               
         XC    ELEM,ELEM                                                        
ELEMD    USING RCONDDEL,ELEM                                                    
         MVI   ELEMD.RCONDDCD,X'DD'                                             
         MVI   ELEMD.RCONDDLN,18                                                
         MVC   ELEMD.RCONDDCT(16),SVDEMCAT                                      
         DROP  ELEMD                                                            
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO3,ELEM,=C'ADD=CODE'             
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   BUYDETL:  BUILDS BUY EFFECTIVE DATE ELEMENTS, THEN INSERTS                  
*        THEM INTO THE NEW BUY RECORD.                                          
*                                                                               
BUYDETL  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*BUYDET*'                                                    
*                                                                               
         L     R3,0(R1)            RESET A(X'41' RECORD)                        
         USING RDARREC,R3                                                       
*******                                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(2,FLTDATES),(3,WORK)                                
         CLC   RTKODATE,WORK                                                    
         BNH   BDET0010                                                         
*** TESTING                                                                     
*        GOTO1 DATCON,DMCB,(0,=C'970428'),(3,RTKODATE)                          
*                                                                               
         ST    R3,DMCB             SETUP CHOPPING CALL                          
         MVC   DMCB+4(3),RTKODATE                                               
         MVC   DMCB+8(1),STARTDAY                                               
         MVC   DMCB+9(1),ENDDAY                                                 
         MVC   DMCB+10(1),ROTATDAY                                              
         MVC   DMCB+11(1),ORBSTDAY                                              
         XC    WORK,WORK                                                        
         MVC   WORK(4),DATCON                                                   
         MVC   WORK+4(4),GETDAY                                                 
         MVC   WORK+8(4),ADDAY                                                  
         MVC   WORK+12(4),PERVERT                                               
         LA    RE,WORK                                                          
         ST    RE,DMCB+12                                                       
*                                                                               
         GOTO1 VREDARTK,DMCB                                                    
         BNE   BDETNO              BUY DELETED, EXIT                            
*                                                                               
BDET0010 EQU   *                                                                
*******                                                                         
         LA    R8,RDARELEM         SET A(X'01' ELEMENT)                         
*                                                                               
         DROP  R3                                                               
         USING RDARBDEL,R8         BUY DETAIL DESCRIP ELEMENT                   
*                                                                               
         L     R2,AIO2             A(BUY RECORD IN PROGRESS)                    
         USING RBUYREC,R2                                                       
         NI    MISCFLAG,X'FF'-MF1CDTE                                           
         TM    RDARBDFL,X'10'                                                   
         BNO   BDET0015                                                         
         OI    MISCFLAG,MF1CDTE    MARK COST OVERRIDE FLAG                      
*                                                                               
BDET0015 EQU   *                                                                
         NI    MISCFLAG,X'7F'      TURN OFF 'DAILY' BUY FLAG                    
         TM    RDARBDFL,X'80'      'DAILY' BUY?                                 
         BNO   BDET0020            NO                                           
         OI    MISCFLAG,X'80'      SET FLAG FOR 'DAILY' BUY                     
BDET0020 EQU   *                                                                
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BNO   BDET0030            NO                                           
         CLI   ORBFLAG,C'Y'        ANY ORBIT RECORDS?                           
         BNE   BDET0040            NO                                           
         GOTO1 DELELT02,DMCB,RBUYREC                                            
*                                  CLEAR ORBIT X'02' DAY/TIME ELTS              
         MVI   ORBFLAG,C'N'        SET ORBIT FLAG TO NO                         
         OI    MISCFLAG,X'40'      SET 'ORBIT DROPPED' FLAG                     
         B     BDET0040                                                         
BDET0030 EQU   *                                                                
         CLI   ORBFLAG,C'Y'        ANY ORBIT RECORDS?                           
         BE    BDET0060            YES - DON'T NEED SECONDARY -                 
*                                  ORBIT HAS ALREADY ADDED THE X'02'            
*                                     ELT.  SHOULD NEVER ABE ORB W/             
*                                        'DAILY' RECORDS                        
BDET0040 EQU   *                                                                
         GOTO1 LOADELT,DMCB,(R2),ELTBILD2,=C'ADD=CODE'                          
         XC    ELTBILD2,ELTBILD2                                                
BDET0060 EQU   *                                                                
*                                  MOVE FROM DESCRIP ELT TO DETAIL              
         ZIC   RF,1(R8)            GET LENGTH                                   
         AR    R8,RF               BUMP TO 1ST DETAIL ELEMENT                   
BDET0080 EQU   *                                                                
         CLI   0(R8),0             END OF RECORD?                               
         BE    BDETYES             YES - FINISHED                               
*                                                                               
         USING RDARBUEL,R8                                                      
*                                                                               
         CLI   RDARBULN,RDARBUL2   CONFLICT END DATE?                           
         BNE   BDET0090                                                         
*                                                                               
* DEAL WITH CONFLICT END DATE!!! -- START                                       
*                                                                               
         XC    RBUYSTED,RBUYSTED                                                
         XC    STARTDAY(2),STARTDAY                                             
         GOTOR STARTEND,DMCB,(RC),RDARBURO,RBUYSTED,(R5)                        
*                                                                               
         SR    RE,RE               INITIALIZE FINAL OUTPUT                      
         LA    RF,7                SET LOOP CONTROL                             
         LA    R1,RDARBURO         SET A(ROTATION ARRAY)                        
BDETCD30 EQU   *                                                                
         CLI   0(R1),C' '          DAY SET?                                     
         BE    BDETCD80            NO                                           
         CLI   0(R1),0             DAY SET?                                     
         BE    BDETCD80            NO                                           
         LA    RE,1(RE)            YES - TURN ON LOW-ORDER BIT                  
BDETCD80 EQU   *                                                                
         SLL   RE,1                SHIFT UP 1 'DAY'                             
         LA    R1,1(R1)            BUMP TO NEXT ARRAY POSITION                  
         BCT   RF,BDETCD30         GO BACK AND TEST NEXT                        
         SRL   RE,1                SHIFT DOWN 1 'DAY' FOR                       
*                                     PROPER ALIGNMENT                          
         STC   RE,BYTE             INSERT DAYS INTO ELEMENT                     
         LR    R6,R2                                                            
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
R        USING RBUYDYEL,R6                                                      
         MVC   R.RBUYDAYS,BYTE                                                  
         DROP  R                                                                
*                                                                               
         DC    X'0770'                                                          
*                                                                               
* DEAL WITH CONFLICT END DATE!!!  -- END                                        
*                                                                               
BDET0090 EQU   *                                                                
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BO    BDET0100            YES - SKIP CHECK FOR COST BREAK ->           
*                                     EACH ELEMENT BECOMES A BUY REC            
         GOTO1 DETLBRAK,DMCB,(RC),(R8),(R2)                                     
*                                  CHECK FOR COST BREAK                         
BDET0100 EQU   *                                                                
         MVI   DETLFLAG,C'Y'       SET DETAILS TO YES                           
         OC    RBUYNW,RBUYNW       NUMBER/WEEK FILLED IN?                       
         BNZ   BDET0120            YES                                          
*                                                                               
         MVC   RBUYNW,RDARBUSW     NO  - INSERT NUMBER PER WEEK                 
BDET0120 EQU   *                                                                
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         MVC   ELTBUILD(2),=X'030B'                                             
*                                  INSERT ELEMENT CODE/LENGTH                   
         GOTO1 DATCON,DMCB,(2,RDARBUSD),(0,WORK)                                
*                                  CONVERT START DATE TO EBCDIC                 
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BNO   BDET0130            NO  -                                        
         MVC   WORK+12(6),WORK     YES - USE START AS IS, THEN                  
*                                     SET END TO START                          
         B     BDET0190                                                         
BDET0130 EQU   *                                                                
         ZIC   RF,RDARBUWK         CALCULATE NUMBER OF DAYS                     
         BCTR  RF,0                MAKE WEEKS ZERO RELATIVE                     
         MH    RF,=H'7'            MULTIPLY WEEKS BY 7                          
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
*                                                                               
         GOTO1 GETDAY,DMCB,WORK,WORK+12                                         
*                                  GET DAY OF START WEEK                        
         ZIC   RF,DMCB             GET DAY OF WEEK                              
         BCTR  RF,0                MAKE DAY ZERO RELATIVE                       
         LTR   RF,RF               MONDAY?                                      
         BZ    BDET0140            YES - LEAVE                                  
         LNR   RF,RF               NEGATE REGISTER                              
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(RF)                                     
*                                  BACK UP TO MONDAY                            
*                                                                               
* IN CASE OF OUT OF WEEK ROTATORS, COMPARE BUY START DAY AND ROTATION           
* START DAY TO GET THE CORRECT MONDAY DATE                                      
*                                                                               
         CLC   STARTDAY,ROTATDAY                                                
         BNL   BDET0135                                                         
         MVC   WORK(6),WORK+12                                                  
         LA    RF,7                                                             
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(RF)                                     
*                                                                               
BDET0135 DS    0H                                                               
         MVC   WORK(6),WORK+12                                                  
*                                                                               
BDET0140 EQU   *                                                                
         CLI   ORBSTDAY,0          ANY ORBIT START DAY?                         
         BZ    BDET0160            NO  - USE HEADER START DAY                   
         ZIC   RF,ORBSTDAY         YES - USE IT                                 
         B     BDET0180                                                         
BDET0160 EQU   *                                                                
         ZIC   RF,STARTDAY         ADD START DAY                                
BDET0180 EQU   *                                                                
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         GOTO1 ADDAY,DMCB,WORK,WORK+12,(RF)                                     
*                                  BUMP TO START DAY IN WEEK                    
BDET0190 EQU   *                                                                
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,ELTBUILD+2)                           
*                                  INSERT START DATE INTO ELEMENT               
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BNO   BDET0200            NO                                           
         MVC   ELTBUILD+5(3),ELTBUILD+2                                         
*                                  YES - SET END DATE = START DATE              
         B     BDET0260                                                         
BDET0200 EQU   *                                                                
*                                                                               
         GOTO1 GETDAY,DMCB,WORK+6,WORK+12                                       
*                                  GET DAY OF END   WEEK                        
         ZIC   RF,DMCB             GET DAY OF WEEK                              
         BCTR  RF,0                MAKE DAY ZERO RELATIVE                       
         LTR   RF,RF               MONDAY?                                      
         BZ    BDET0240            YES - LEAVE                                  
         LNR   RF,RF               NEGATE REGISTER                              
         CLC   STARTDAY,ENDDAY     START/END ON SAME DAY?                       
*                                     (SINGLE-DAY BUY?)                         
         BE    BDET0220            YES - DON'T BUMP TO NEXT WEEK                
         BL    BDET0220            START < END DAY:  NOT AN                     
*                                     OOWR - DON'T BUMP                         
         LA    RF,7(RF)            BUMP IT INTO NEXT WEEK                       
BDET0220 EQU   *                                                                
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12,(RF)                                   
*                                  BACK UP TO MONDAY                            
*                                                                               
* IN CASE OF OUT OF WEEK ROTATORS, COMPARE BUY START DAY AND ROTATION           
* START DAY TO GET THE CORRECT MONDAY DATE                                      
*                                                                               
         CLC   STARTDAY,ROTATDAY                                                
         BNL   BDET0230                                                         
         MVC   WORK+6(6),WORK+12                                                
         LA    RF,7                                                             
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12,(RF)                                   
*                                                                               
BDET0230 DS    0H                                                               
         MVC   WORK+6(6),WORK+12                                                
*                                                                               
BDET0240 EQU   *                                                                
         ZIC   RF,ENDDAY           ADD END   DAY                                
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         GOTO1 ADDAY,DMCB,WORK+6,WORK+12,(RF)                                   
*                                  BUMP TO END   DAY IN WEEK                    
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,ELTBUILD+5)                           
*                                  INSERT END   DATE INTO ELEMENT               
*                                                                               
BDET0260 EQU   *                                                                
         MVI   ELTBUILD+8,X'80'    SET TO 'EVERY WEEK'                          
         CLC   RBUYNW,RDARBUSW     HEADER #/WK = DETAIL #/WK?                   
         BE    BDET0280            YES                                          
         OI    ELTBUILD+8,X'01'    NO  - PUT IN 'OVERRIDE' FLAG                 
BDET0280 EQU   *                                                                
         MVC   ELTBUILD+9(1),RDARBUSW                                           
*                                  INSERT NUMBER PER WEEK                       
         MVC   ELTBUILD+10(1),RDARBUWK                                          
*                                  INSERT NUMBER OF WEEKS                       
         ZIC   RF,RDARBUWK         ACCUMULATE NUMBER OF WEEKS                   
         SR    R0,R0                  AND CALC # SPOTS                          
         ZIC   R1,RDARBUSW         NUMBER SPOTS PER WEEK                        
         MR    R0,RF               # SPOTS X # WEEKS                            
         L     RE,SPOTCTR                                                       
         AR    RE,R1               ACCUMULATE NUMBER OF SPOTS                   
         ST    RE,SPOTCTR          STORE IT BACK                                
         A     RF,WEEKCTR          ACCUMULATE NUMBER OF WEEKS                   
         ST    RF,WEEKCTR          STORE IT BACK                                
         L     R2,AIO2             A(BUY RECORD IN PROGRESS)                    
         GOTO1 LOADELT,DMCB,(R2),ELTBUILD,=C'ADD=CODE'                          
*                                                                               
         TM    MISCFLAG,X'80'      'DAILY' BUY?                                 
         BNO   BDET0290            NO  - GO BACK FOR NEXT ELEMENT               
         BAS   RE,GENDAILY         YES - GENERATE A BUY RECORD                  
         B     BDET0300                                                         
*                                                                               
BDET0290 EQU   *                                                                
         TM    MISCFLAG,MF1CDTE    CONFLICT END DATE?                           
         BZ    BDET0300                                                         
         BAS   RE,GENCDATE         YES, GENERATE A BUY RECORD                   
*                                                                               
BDET0300 EQU   *                                                                
         ZIC   RF,1(R8)                                                         
         AR    R8,RF                                                            
         B     BDET0080            GO BACK FOR NEXT ELEMENT                     
*                                                                               
BDETYES  SR    RC,RC                                                            
BDETNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   GENDAILY:  FOR 'DAILY' BUYS, EACH DETAIL IS TO BE A SEPARATE BUY            
*        RECORD.  THE DETAIL'S DETAILS ARE INSERTED INTO THE HEADER,            
*        AND THE RECORD WRITTEN TO THE FILE.                                    
*                                                                               
GENDAILY NTR1                                                                   
         OI    RBUYFLG2,X'80'      SET 'DAILY ORDER' FLAG                       
         MVC   RBUYNW,RDARBUSW     INSERT NUMBER SPOTS/WEEK                     
         MVC   RBUYTSPT,RDARBUSW   INSERT TOTAL SPOT: SAME AS SPTS/WK           
         MVC   RBUYCOS,RDARBU$$    INSERT SPOT COST                             
         MVC   BUYCOST,RDARBU$$    SAVE   SPOT COST FOR CALCULATION             
*                                  INSERT ELEMENT CODE/LENGTH                   
         GOTO1 DATCON,DMCB,(2,RDARBUSD),(0,WORK)                                
*                                  CONVERT START DATE TO EBCDIC                 
         GOTO1 GETDAY,DMCB,WORK,WORK+12                                         
*                                  GET DAY OF WEEK OF START DAY                 
         ZIC   RF,DMCB             GET DAY OF WEEK:  START DAY                  
         SLL   RF,4                SHIFT TO LOW ORDER, HI NYBBLE                
         ZIC   RE,DMCB             GET DAY OF WEEK:  END   DAY                  
         AR    RF,RE               ADD END DAY TO START DAY                     
         STC   RF,RBUYSTED         INSERT START/END DAYS                        
         LA    RF,RBUYELEM         SET A(01 ELEMENT)                            
GDAI0020 EQU   *                                                                
         ZIC   RE,1(RF)            GET LENGTH                                   
         AR    RF,RE               BUMP TO NEXT                                 
         CLI   0(RF),0             END OF RECORD?                               
         BNE   *+6                 NO                                           
         DC    H'0'                YES - SHOULDN'T HAPPEN                       
         CLI   0(RF),2             DAY/TIME ELEMENT?                            
         BNE   GDAI0020            NO  - GO BACK FOR NEXT                       
         MVC   RBUYDYIN-RBUYDYEL(1,RF),RBUYSTED                                 
*                                  YES - INSERT START/END FROM HDR              
         ZIC   RE,RBUYSTED         GET ST/END DAYS                              
         SRL   RE,4                DROP THE END DAY                             
         LA    R3,DAYTABLE                                                      
         AR    R3,RE               ADD DAY TO TABLE ADDR                        
         MVC   RBUYDAYS-RBUYDYEL(1,RF),0(R3)                                    
*                                  INSERT DAYS INTO ELEMENT -                   
*                                     ALWAYS A SINGLE DAY                       
         GOTO1 GENREC,DMCB,(RC),RBUYREC                                         
*                                  OUTPUT THE BUY RECORD                        
         ZIC   RF,BUYLINE#         BUMP BUYLINE #                               
         LA    RF,1(RF)                                                         
         STC   RF,BUYLINE#                                                      
         STC   RF,RBUYKMLN         INSERT NEW NUMBER IN MASTER LINE#            
         STC   RF,RBUYKLIN         INSERT NEW NUMBER IN LINE #                  
         GOTO1 DELELT,DMCB,RBUYREC                                              
         MVI   DETLFLAG,C'N'       RESET DETAIL COUNT TO NONE                   
         XIT1                                                                   
*                                                                               
DAYTABLE DC    X'8040201008040201'                                              
*                                                                               
         EJECT                                                                  
*                                                                               
*   GENCDATE:  FOR 'CONFLICT END DATE' BUYS, THE LAST BUY SHOULD                
*              BECOME A SEPERATE BUY LINE                                       
*                                                                               
GENCDATE NTR1                                                                   
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'        MISC FLAG ELEMENT                            
         BRAS  RE,GETEL                                                         
         BE    CDTE0050                                                         
*                                                                               
WKD      USING RBUYXXEL,ELEM                                                    
         XC    ELEM,ELEM                                                        
         MVI   WKD.RBUYXXCD,RBUYXXCQ                                            
         MVI   WKD.RBUYXXLN,RBUYXXLQ                                            
         OI    WKD.RBUYXXFG,X'10'  MARK CONFLICT END DATE FLAG                  
         DROP  WKD                                                              
         GOTO1 LOADELT,DMCB,(R4),ELEM,=C'ADD=CODE'                              
         B     CDTE0055                                                         
*                                                                               
CDTE0050 EQU   *                                                                
         USING RBUYXXEL,R6                                                      
         OI    RBUYXXFG,X'10'      MARK CONFLICT END DATE FLAG                  
         DROP  R6                                                               
*                                                                               
CDTE0055 EQU   *                                                                
         MVC   RBUYNW,RDARBUSW     INSERT NUMBER SPOTS/WEEK                     
         MVC   RBUYTSPT,RDARBUSW   INSERT TOTAL SPOT: SAME AS SPTS/WK           
         MVC   RBUYCOS,RDARBU$$    INSERT SPOT COST                             
         MVC   BUYCOST,RDARBU$$    SAVE   SPOT COST FOR CALCULATION             
*                                  INSERT ELEMENT CODE/LENGTH                   
                                                                                
         CLI   RDARBULN,RDARBUL2                                                
         BNE   *+10                                                             
         MVC   RBUYDYIN,RBUYSTED                                                
*                                                                               
         GOTO1 GENREC,DMCB,(RC),RBUYREC                                         
*                                  OUTPUT THE BUY RECORD                        
         ZIC   RF,BUYLINE#         BUMP BUYLINE #                               
         LA    RF,1(RF)                                                         
         STC   RF,BUYLINE#                                                      
         STC   RF,RBUYKMLN         INSERT NEW NUMBER IN MASTER LINE#            
         STC   RF,RBUYKLIN         INSERT NEW NUMBER IN LINE #                  
         GOTO1 DELELT,DMCB,RBUYREC                                              
         MVI   DETLFLAG,C'N'       RESET DETAIL COUNT TO NONE                   
         XIT1                                                                   
         DROP  R2,R8                                                            
*                                                                               
*   DETLBRAK:  IF DETAIL COST IS DIFFERENT THAN BUYHDR COST, AND                
*      PREVIOUS DETAILS HAVE BEEN ENCOUNTERED WITHIN THE BUYHDR                 
*      GROUP, THE FOLLOWING STEPS MUST BE TAKEN:                                
*         1.  THE BUY MUST BE OUTPUT                                            
*         2.  THE NEXT BUYLINE NUMBER MUST BE CALCULATED                        
*         3.  THE RECORD MUST BE CLEARED OF DETAIL INFORMATION                  
*         4.  PROCESSING OF THE NEW DETAIL WILL THEN CONTINUE                   
*                                                                               
DETLBRAK NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R3,4(R1)            RESET A(DETAIL ELEMENT)                      
         USING RDARBUEL,R3         BUY DETAIL ELEMENT                           
*                                                                               
         L     R4,8(R1)            RESET A(RBUYREC)                             
         USING RBUYREC,R4                                                       
*                                                                               
         CLI   DETLFLAG,C'N'       ANY DETAILS ENCOUNTERED?                     
         BNE   DBRA0040            YES - CHECK FOR $$ CHANGE                    
         MVC   RBUYCOS,RDARBU$$    NO  - INSERT BUY COST IN CASE                
*                                     DIFFERENT FROM HEADER COST                
         MVC   BUYCOST,RBUYCOS     SAVE COST FOR CALCULATION                    
         MVC   RBUYNW,RDARBUSW     INSERT SPOTS/WK IN CASE DIFFERENT            
*                                                                               
         B     DBRA0200            EXIT                                         
DBRA0040 EQU   *                                                                
         CLC   RBUYCOS,RDARBU$$    BUY COST = DETAIL COST?                      
         BE    DBRA0200            YES - FINISHED                               
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'        MISC FLAG ELEMENT                            
         BRAS  RE,GETEL                                                         
         BE    DBRA0050                                                         
*                                                                               
WKD      USING RBUYXXEL,ELEM                                                    
         XC    ELEM,ELEM                                                        
         MVI   WKD.RBUYXXCD,RBUYXXCQ                                            
         MVI   WKD.RBUYXXLN,RBUYXXLQ                                            
         OI    WKD.RBUYXXFG,X'40'  MARK COST OVERRIDE FLAG                      
         DROP  WKD                                                              
         GOTO1 LOADELT,DMCB,(R4),ELEM,=C'ADD=CODE'                              
         B     DBRA0055                                                         
*                                                                               
DBRA0050 EQU   *                                                                
         USING RBUYXXEL,R6                                                      
         OI    RBUYXXFG,X'40'      MARK COST OVERRIDE FLAG                      
         DROP  R6                                                               
*                                                                               
DBRA0055 EQU   *                                                                
         GOTO1 GENREC,DMCB,(RC),RBUYREC                                         
*                                  NO  - OUTPUT PREVIOUS BUY RECORD             
         MVC   RBUYCOS,RDARBU$$    INSERT NEW COST                              
         MVC   BUYCOST,RBUYCOS     SAVE COST FOR CALCULATION                    
         MVC   RBUYNW,RDARBUSW     INSERT SPOTS/WK IN CASE DIFFERENT            
         ZIC   RF,BUYLINE#         BUMP BUYLINE #                               
         LA    RF,1(RF)                                                         
         STC   RF,BUYLINE#                                                      
         STC   RF,RBUYKMLN         INSERT NEW NUMBER IN MASTER LINE#            
         STC   RF,RBUYKLIN         INSERT NEW NUMBER IN LINE #                  
         GOTO1 DELELT,DMCB,RBUYREC                                              
         MVI   DETLFLAG,C'N'       RESET DETAIL COUNT TO NONE                   
DBRA0200 EQU   *                                                                
         XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
*  VARIOUS: IF ORDER IS BRAND, WE NEED TO FIND THE CONTRACT LINKED TO           
*     THE BRAND'S VARIOUS ORDER AND CLEAR THE ESTIMATE DOLLAR BUCKETS           
*     X'03' TO PREVENT DOUBLE BOOKING. IN ADDITION, WE NEED TO MARK             
*     ALL OF THE VARIOUS CONTRACT'S BUY LINES AS CANCELLED AND FLAGGED          
*     FOR DELETION.                                                             
*                                                                               
T83F20   CSECT                                                                  
VARIOUS  NMOD1 0,*VARI*                                                         
         L     RC,0(R1)                                                         
*                                                                               
         OC    SELECTKY,SELECTKY                                                
         BZ    VARX                                                             
         MVC   AIO,AIO1                                                         
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         TM    RDARMISC,X'08'      BRAND ORDER?                                 
         BZ    VARX                                                             
         DROP  R6                                                               
*                                                                               
         OC    SVSELKEY,SVSELKEY                                                
         BZ    VARX                                                             
         MVC   KEY(L'SVSELKEY),SVSELKEY                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         MVC   WORK+55(4),RDARREP# SAVE OFF DAR REP #                           
         DROP  R6                  FOR VARIOUS ORDER                            
*                                                                               
         L     R2,AIO                                                           
         USING RCONREC,R2                                                       
*                                                                               
         XC    RCONREC(32),RCONREC                                              
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,TWAAGY     INSERT POWER CODE                            
         ZAP   WORK+5(5),=P'99999999'                                           
         ZAP   WORK+10(5),=P'0'                                                 
         MVO   WORK+10(5),WORK+55(4)                                            
         SP    WORK+5(5),WORK+10(5)                                             
         MVO   WORK(5),WORK+5(5)                                                
         MVC   RCONPCON,WORK       CHECK IF NUMBER IS ON FILE                   
*                                                                               
         MVC   KEY,RCONREC                                                      
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VARX                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'03',RCONREC),0,0                
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'53',RCONREC),0,0                
*                                  DELETE OLD ALT CAL EST  ELTS                 
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'54',RCONREC),0,0                
*                                  DELETE OLD ALT CAL INV  ELTS                 
         GOTO1 PUTREC                                                           
         MVC   WORK(4),KEY+23      SAVE OFF FOR LOOKING FOR BUYS                
         DROP  R2                                                               
*                                                                               
         MVC   AIO,AIO3                                                         
         L     R2,AIO                                                           
         USING RBUYREC,R2                                                       
*                                                                               
         XC    RBUYREC(32),RBUYREC                                              
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,TWAAGY     INSERT POWER CODE                            
         MVC   RBUYKCON,WORK       CHECK IF NUMBER IS ON FILE                   
         PACK  RBUYKCON+0(1),WORK+3(1)                                          
         PACK  RBUYKCON+1(1),WORK+2(1)                                          
         PACK  RBUYKCON+2(1),WORK+1(1)                                          
         PACK  RBUYKCON+3(1),WORK+0(1)                                          
*                                                                               
         MVC   KEY(27),RBUYREC                                                  
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
VAR10    DS    0H                                                               
         CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   VARX                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         OI    RBUYCNTL,X'80'                                                   
         MVI   RBUYCHGI,C'C'       MARK BUY AS CANCELLED                        
         GOTO1 PUTREC                                                           
         OI    KEY+27,X'80'        MARK KEY AS DELETED, TOO                     
         GOTO1 WRITE                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                                                              
         B     VAR10                                                            
*                                                                               
VARX     DS    0H                                                               
         XMOD1                                                                  
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*  TRUDATE:  FOR CONTRACTS WHERE THE BUYLINES HAVE RESULTED IN BUCKET           
*     CHANGES, FIND (ADD IF NOT PRESENT) THE X'08' ELEMENT, AND                 
*     UPDATE THE TRUE ACTIVITY DATE FOR SAR REPORTING                           
*                                                                               
T83F20   CSECT                                                                  
TRUDATE  NMOD1 0,*TRUD*                                                         
         L     RC,0(R1)                                                         
         L     R8,4(R1)                                                         
*                                                                               
         USING RCONREC,R8                                                       
*                                                                               
         LA    R2,RCONELEM         A(1ST ELEMENT)                               
TDAT0010 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         CLI   0(R2),0             END OF RECORD?                               
         BE    TDAT0030            YES - NO X'08' FOUND - ADD IT                
         CLI   0(R2),X'08'         X'08'?                                       
         BNE   TDAT0010            NO  - GO BACK FOR NEXT                       
TDAT0020 EQU   *                   YES - ADD TODAYS DATE                        
         USING RCONACEL,R2                                                      
         GOTO1 DATCON,DMCB,(5,RCONACTA),(3,RCONACTA)                            
*                                  TODAY'S DATE INTO TRUE ACT DATE              
         DROP  R2                                                               
*                                                                               
         B     TDAT0040            FINISHED                                     
TDAT0030 EQU   *                                                                
         GOTO1 DATCON,DMCB,(5,RCONDATE),(3,TDATELT+5)                           
*                                  TODAY'S DATE INTO TRUE ACT DATE              
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,TDATELT,          X        
               =C'ADD=CODE'                                                     
TDAT0040 EQU   *                                                                
         MVI   TRUFLAG,C'Y'        SET 'NEED EC KEY' FLAG                       
         XIT1                                                                   
         DROP  R8                                                               
*                                                                               
*                   .1.2.3.4.5.6.7.8.9.0.1.2                                    
TDATELT  DC    XL12'080C00000000000000000000'                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*              PARAMETER 1 =       A(RECORD) BYTE 0=X'FF' FOR BUFFERS           
*                                            INSTEAD OF RECORDS WHERE           
*                                            1ST 2 BYTES = LENGTH OF            
*                                            BUFFER                             
*              PARAMETER 2 =       A(BUCKET TO BE INSERTED)                     
*              PARAMETER 3 =       RC                                           
*                                                                               
ADDBUCK  NMOD1 0,*ADDB*                                                         
*                                  SET UP BXLE                                  
         L     RC,8(R1)                                                         
         L     R2,0(R1)            A(REC)                                       
         L     R6,4(R1)            A(ELEM)                                      
         MVC   HALF,27(R2)                                                      
         LA    R3,34(R2)           1ST ELEM                                     
         MVI   BYTE,2              REPPAK IND FOR RECUP                         
*                                                                               
         CLI   0(R1),X'FF'         BUFFER (NOT RECORD)?                         
         BNE   *+18                                                             
         MVC   HALF,0(R2)          BUFFER LENGTH                                
         LA    R3,2(R2)            1ST ELEMENT IN BUFFER                        
         MVI   BYTE,X'FF'          BUFFER IND TO RECUP                          
*                                                                               
         LH    R5,HALF             REC LEN                                      
         LA    R5,0(R5,R2)         REC END                                      
         BCTR  R5,R0                                                            
         BCTR  R5,R0               REC END-2 IN CASE ONLY 1 ELEM                
         SR    R4,R4                                                            
         CR    R3,R5               NO ELEMENTS YET?                             
         BH    ADDB100                                                          
*                                                                               
         CLC   0(6,R6),0(R3)       NEW ELEM V OLD                               
         BL    ADDB100             LOW-ADD NEW ELEM                             
         BE    ADDB200             EQUAL-ADD TO OLD ELEM                        
         IC    R4,1(R3)            ELEM LEN                                     
         BXLE  R3,R4,*-18          NEXT ELEM                                    
*                                                                               
*              ADD ELEMENT                                                      
ADDB100  GOTO1 VRECUP,PARAS,(BYTE,(R2)),(R6),(C'R',(R3))                        
         CLI   PARAS+8,0                                                        
         BE    RECFULL                                                          
*                                                                               
BUCKXIT  XIT1                                                                   
*                                                                               
*              EQUAL BUCKETS                                                    
ADDB200  MVC   FULL,6(R6)          NEW AMOUNT                                   
         L     R6,FULL                                                          
         MVC   FULL,6(R3)          OLD AMOUNT                                   
         A     R6,FULL                                                          
         ST    R6,FULL                                                          
         MVC   6(4,R3),FULL        NEW AMOUNT                                   
*                                                                               
         LTR   R6,R6                                                            
         BNZ   BUCKXIT                                                          
*                                                                               
*              DELETE ZERO BUCKET                                               
         GOTO1 VRECUP,PARAS,(BYTE,(R2)),(R3),(C'R',(R3))                        
         CLI   PARAS+8,0                                                        
         BE    RECFULL                                                          
         B     BUCKXIT                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   SETPQUE:  ESTABLISH PRINT QUEUE FOR NOTIFICATIONS                           
*                                                                               
SETPQUE  NMOD1 0,*STPQ*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,AIO3             SET A(CONTRACT RECORD)                       
         USING RCONREC,R4                                                       
*                                                                               
         L     R5,4(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         L     R8,8(R1)            RESET A(SPOOLD)                              
         USING SPOOLD,R8                                                        
*                                                                               
*                                                                               
         GOTO1 EDICT,DMCB,(RC),(R8),RCONREC                                     
*                                  CREATE EDICT HEADER CARDS                    
         XIT1                                                                   
*                                                                               
         DROP  R4,R5,R8                                                         
         EJECT                                                                  
*                                                                               
* CREATES EDICT HEADER CARDS FOR EDICT PROCESSING                               
*                                                                               
         DS    0F                                                               
EDICT    NMOD1 0,*EDIC*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R8,4(R1)            RESET A(SPOOLWORK)                           
         USING SPOOLD,R8                                                        
*                                                                               
*                                                                               
*                                                                               
         L     R5,8(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,0                                                           
         MVC   P+4(5),=C'*HDR*'                                                 
                                                                                
         MVC   P+9(14),=C'EDICT=*DDSDARR'                                       
                                                                                
*                                                                               
         MVI   P+34,C'W'           WIDE REPORT - 132 CHARS                      
*                                  SEND SPECIAL PRINT LINE                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* PRINT A ++DDS CARD                                                            
*                                                                               
         LA    R3,P                                                             
         USING EDICTD,R3                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVI   EDISYST,C'D'        UPPER CASE 'D'                               
         MVC   EDIPROG,EDICTACT    INSERT EDICT 'ACTION'                        
         MVC   EDIIDEN,=C'TRN'     TRANSACTION DATA                             
                                                                                
*                                                                               
* INFORMATION CHUNK FOR ETI REPORTING                                           
*                                                                               
         L     R6,AIO1             DARE RECORD                                  
         USING RDARREC,R6                                                       
         MVC   EDIRDRRP,RDARKREP   REP CODE                                     
         MVC   EDIRDRAG,RDARKAGY   AGENCY CODE                                  
         MVC   EDIRDRST,RDARKSTA   STATION CODE                                 
         MVC   EDIRDRMD,RDARMEDI   MEDIA CODE                                   
                                                                                
         EDIT  RDAREST#,(3,EDIRDRES),ALIGN=LEFT                                 
                                                                                
* AGENCY ORDER #                                                                
         ZAP   WORK2(5),=P'0'                                                   
         MVO   WORK2(5),RDARKORD                                                
         EDIT  (P5,WORK2),(8,EDIRDRAN),ALIGN=LEFT                               
                                                                                
* CONTRACT #                                                                    
         OC    RDARREP#,RDARREP#                                                
         BZ    EDICT10                                                          
         ZAP   WORK2(5),=P'0'                                                   
         MVO   WORK2(5),RDARREP#                                                
         EDIT  (P5,WORK2),(8,EDIRDRCN),ALIGN=LEFT                               
                                                                                
*                                                                               
         L     R4,AIO3             RESET A(CONTRACT RECORD)                     
         USING RCONREC,R4                                                       
         MVC   EDIRDRSP,RCONSAL    SALESMAN CODE                                
         DROP  R4                                                               
                                                                                
EDICT10  DS    0H                                                               
         MVC   EDIRDRBY,RDARBUYC   BUYER CODE                                   
                                                                                
         MVI   ELCODE,X'02'        DESCRIPTIVE ELEMENT #2                       
         BRAS  RE,GETEL                                                         
         BNE   EDICT50                                                          
         USING RDARCLEM,R6                                                      
         MVC   EDIRDRCL,RDARCLI    CLIENT CODE                                  
         MVC   EDIRDRP1,RDARPRD1   PRODUCT CODE 1                               
         MVC   EDIRDRP2,RDARPRD2   PRODUCT CODE 2                               
         DROP  R3,R5,R6                                                         
*                                  SEND SPECIAL PRINT LINE                      
EDICT50  DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
                                                                                
EDICTX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   APPRREJC:  APPROVAL/REJECTION HEADER OUTPUT                                 
*                                                                               
         DS    0F                                                               
APPRREJC NMOD1 0,*APRJ*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R8,4(R1)            RESET A(SPOOLD)                              
         USING SPOOLD,R8                                                        
*                                                                               
         L     R5,8(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         LA    R4,P                SET PRINT OUTPUT                             
         USING ORDAPREJ,R4                                                      
*                                                                               
         CLI   ACTCODE,C'A'        APPROVAL?                                    
         BNE   APRJ0020            NO  -  REJECT                                
         MVC   ARTRANID,=C'ORDAPP'                                              
         B     APRJ0040                                                         
APRJ0020 EQU   *                                                                
         MVC   ARTRANID,=C'ORDREJ'                                              
APRJ0040 EQU   *                                                                
*                                  INSERT IDENTIFIER                            
         MVC   ARORDNUM,RETORD#    INSERT ORDER NUMBER                          
         MVC   ARFROM,RETTO        INSERT FROM CODE                             
         MVC   ARTO,RETFROM        INSERT TO CODE                               
*                                     NOTE:  CODES ARE REVERSED                 
         GOTO1 DATCON,DMCB,(5,WORK),(X'20',ARDATE)                              
*                                  INSERT TODAY'S DATE                          
*&&DO                                                                           
         TIME  DEC                 RETRIEVE TIME:  RETURNED IN R0               
*                                     AS HH:MM:SS:TT                            
         LR    R2,R0               MOVE TO A 'REAL' REGISTER                    
         SRDL  R2,24               SHIFT MM:SS:TT INTO R1                       
         LA    R2,6(R2)            ADD 6 TO HOURS                               
         SLDL  R2,8                SHIFT MM BACK TO R0                          
         STCM  R2,3,WORK+20        MOVE HH:MM TO WORK                           
         STCM  R2,3,WORK+32        SAVE FOR TEST DISPLAY                        
*                                                                               
*   NOTE ON EC TIME DISPLAY:                                                    
*     TIME IS NOW A HEX REPRESENTATION OF HH:MM.  THEREFORE,                    
*     IT MUST BE CONVERTED TO A MEANINGFUL DISPLAY FORMAT.  FOR                 
*     EXAMPLE, 6:15 AM WAS ORIGINALLY TAKEN FROM THE SYSTEM AS                  
*     TWO BYTES OF 0015 (DDS TIME: 6AM=0000).  ABOVE CODE ADDED                 
*     A VALUE OF 6 TO THE HOURS BYTE, PRODUCING 0615.  AT 11:15 AM,             
*     SYSTEM TIME WAS 0515.  ADDING 6 PRODUCED 0B15, WHICH MUST BE              
*     PROPERLY INTERPRETED FOR HOURS.                                           
*                                                                               
*     FIRST, INTERPRET THE HOURS BYTE...                                        
*                                                                               
         ZIC   R1,WORK+20          EXTRACT HOURS BYTE                           
         LA    RF,36               HOUR ADJUSTMENT                              
         CLI   WORK+20,X'28'       4AM-7AM?                                     
         BNL   APRJ0120            YES - SUBTRACT 36                            
         LA    RF,30               HOUR ADJUSTMENT                              
         CLI   WORK+20,X'1F'       1AM-3AM?                                     
         BNL   APRJ0120            YES - SUBTRACT 20                            
         LA    RF,6                HOUR ADJUSTMENT                              
         CLI   WORK+20,X'18'       6PM-MID?                                     
         BNL   APRJ0120            YES - SUBTRACT 6                             
         LA    RF,0                NO ADJUSTMENT                                
APRJ0120 EQU   *                                                                
         SR    R1,RF               SUBTRACT ADJUSTMENT FROM HOUR                
         EDIT  (R1),(2,WORK+24),FILL=0,ZERO=NOBLANK                             
*                                                                               
*     NEXT, DISPLAY THE MINUTES BYTE                                            
*                                                                               
         GOTO1 HEXOUT,DMCB,WORK+21,WORK+26,1,=C'TOG'                            
         MVC   ARTIME,WORK+24      INSERT INTO OUTPUT                           
*&&                                                                             
*                                                                               
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,WORK                                                        
         GOTO1 HEXOUT,DMCB,WORK,ARTIME,2,0                                      
*                                                                               
         MVC   ARSTAT(6),RETSTAT   INSERT STATION                               
*&&DO                                                                           
         CLI   ARSTAT+4,C'A'       GET UID FROM STATION RECORD FOR              
         BE    APRJ0125            RADIO EDI                                    
         CLI   ARSTAT+4,C'F'       GET UID FROM STATION RECORD FOR              
         BNE   APRJ0130            RADIO EDI                                    
*                                                                               
APRJ0125 EQU   *                                                                
         GOTO1 =A(GETUID),DMCB,ARSTAT,RR=RELO                                   
*&&                                                                             
*                                                                               
APRJ0130 EQU   *                                                                
         CLI   ACTCODE,C'R'        REJECTION?                                   
         BNE   APRJ0140            NO                                           
         CLC   =C'UNDARE',AORREAS  REJECT AS 'UNDARE'?                          
         BNE   APRJ0140            NO                                           
         MVC   ARCON#,SPACES       YES - CLEAR CONTRACT NUMBER                  
         MVC   ARCON#(6),=C'UNDARE' INSERT 'UNDARE' AS REP CON#                 
         MVI   DELAGORD,C'Y'       SET 'DELETE AGENCY ORDER' FLAG               
         B     APRJ0150                                                         
APRJ0140 EQU   *                                                                
         MVC   ARCON#,RETCON#      INSERT CONTRACT #                            
APRJ0150 EQU   *                                                                
         MVC   ARRETSND,RETSENDR   INSERT 'RET TO SENDER' INFO                  
****>>>> MVC   ARDDS,=C'DDS'       INSERT LINE DELIMITER                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     SEND THE MESSAGE                             
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
*                                                                               
         CLI   ACTCODE,C'A'        APPROVAL?                                    
         BE    APRJ0320            OPENED:    DON'T UPDATE CONTRACT             
*                                     STATUS FLAG HERE...                       
*                                                                               
*                                  SET 'ORDER REJECTED' FLAG                    
*                                     IN CONTRACT RECORD AND REWRITE            
         MVC   AIO,AIO3            SET A(IO AREA)                               
         L     R2,AIO3                                                          
         USING RCONREC,R2                                                       
*                                                                               
         OC    RCONREC(L'RCONKEY),RCONREC                                       
         BZ    APRJ0320            ORDER IS UNLINKED                            
*                                                                               
         MVC   KEY(27),RCONREC     GET KEY FROM CONTRACT RECORD                 
         MVC   KEYSAVE,KEY         DON'T RETURN DELETED RECORDS                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY     REDUNDANT CHECK: KEY FOUND?                  
         BNE   APRJ0320                       NO  - EXIT                        
         GOTO1 GETREC              READ CONTRACT                                
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        RETRIEVE                                     
         BRAS  RE,GETEL                                                         
         BE    APRJ0160            FOUND: UPDATE IT                             
         XC    ELTBUILD,ELTBUILD   NOT FOUND: BUILD IT                          
         MVI   ELTBUILD,X'1D'      INSERT ELEMENT CODE                          
         MVI   ELTBUILD+1,RCONDL2Q INSERT ELEMENT LENGTH                        
         OI    ELTBUILD+2,X'20'    SET 'REJECTED' FLAG                          
*                                                                               
         CLI   AORHDLNH+5,0        ANY CONTRACT NUMBER ON SCREEN?               
         BE    *+8                                                              
         OI    ELTBUILD+2,X'80'    YES, SET 'LINKED' FLAG                       
*                                                                               
         GOTO1 HEXIN,DMCB,RETORD#,ELTBUILD+3,8                                  
*                                  INSERT HEX VALUE OF AGY ORD #                
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,ELTBUILD,         X        
               =C'ADD=CODE'                                                     
         B     APRJ0200                                                         
APRJ0160 EQU   *                                                                
         MVI   2(R6),X'A0'         SET 'REJECTED+LINKED' FLAG, CLEAR            
*                                     ANY PREVIOUS VALUE                        
APRJ0200 EQU   *                                                                
         CLI   DELAGORD,C'Y'       DELETE AGENCY ORDER?                         
         BNE   APRJ0240            NO                                           
*                                  YES - BLOW AWAY 'DARE' ELEMENT,              
*                                     FREEING THIS ORDER                        
*        GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'1D',RCONREC),0,0                
*                                                                               
* UNDARE WILL LEAVE THE 1D ELEMENT BUT REMOVE LINK TO ORDER                     
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        RETRIEVE                                     
         BRAS  RE,GETEL                                                         
         BNE   APRJ0240                                                         
         USING RCONDREL,R6                                                      
         MVI   RCONDRFG,0                                                       
         DROP  R6                                                               
*                                                                               
APRJ0240 EQU   *                                                                
         GOTO1 PUTREC              WRITE UPDATED RECORD TO FILE                 
APRJ0320 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R2,R4,R5,R8                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   REJCMSGS:  REJECTION MESSAGES                                               
*                                                                               
         DS    0F                                                               
REJCMSGS NMOD1 0,*REJC*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R8,4(R1)            RESET A(SPOOLD)                              
         USING SPOOLD,R8                                                        
*                                                                               
         L     R6,8(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R6                                                       
*                                                                               
         LA    RF,REJMESS          SET A(REJECTION MESSAGES)                    
         ST    RF,AREJMESS                                                      
         SR    R4,R4                                                            
*                                                                               
         GOTO1 =A(SENDSAL),RR=RELO                                              
*                                                                               
         CLI   ACTCODE,C'A'        IF APPROVE/OPEN                              
         BE    REMS1100            PRINT TRAILER AND EXIT                       
*                                                                               
*   LINES ON SCREEN ARE COUNTED, TO DETERMINE WHERE '*' CONTINUATION            
*     MARKS ARE TO BE PLACED.  BLANK LINES WITHIN THE BODY OF THE               
*     REJECTION COMMENTS ARE SKIPPED.  THESE ARE THEN SKIPPED OVER              
*     WHEN THE SCREEN IS FORMATTED INTO THE OUTPUT MESSAGES.                    
*                                                                               
         LA    R2,AORREASH         MESSAGE HEADER                               
         SR    R3,R3                                                            
REMS0020 EQU   *                                                                
         CLI   5(R2),0             ANYTHING ON LINE?                            
         BE    REMS0040            NO  - DON'T COUNT LINE                       
         LA    R3,1(R3)            YES - COUNT LINE                             
REMS0040 EQU   *                                                                
         LA    R2,AORREA2H-AORREASH(R2)                                         
*                                  BUMP TO NEXT LINE                            
         LA    RE,AORENDH          LAST                                         
         CR    R2,RE               END OF SCREEN REACHED?                       
         BNH   REMS0020            NO  - GO BACK FOR NEXT                       
REMS0060 EQU   *                                                                
         XC    P,P                 CLEAR PRINT LINE                             
         LA    R5,P                                                             
         USING ORDCOM,R5                                                        
*                                                                               
         LTR   R3,R3               ANY REJECT LINES?                            
         BZ    REMS0100            NO                                           
         LA    R2,AORREASH                                                      
REMS0080 EQU   *                                                                
         BAS   RE,MSGCHECK         OUTPUT PREVIOUS LINE?                        
         MVC   OCTRANID,=C'ORDCOM'                                              
         MVC   OCORDNUM,RETORD#                                                 
REMS0090 EQU   *                                                                
         ZIC   RF,5(R2)            GET LENGTH OF LINE                           
         LTR   RF,RF               ANYTHING ON LINE?                            
         BNZ   REMS0095            YES                                          
         ST    RF,DMCB+8           INSERT LENGTH (ZERO) INTO P3                 
         GOTO1 REJMSGS,DMCB,(RC),0                                              
*                                  NO  - BLANK LINE IN MESS AREA                
         LA    R4,1(R4)            COUNT BLANK LINE IN                          
*                                     TOTAL LINES                               
         LA    R2,AORREA2H-AORREASH(R2)                                         
*                                  BUMP TO NEXT LINE                            
         B     REMS0080            GO BACK FOR NEXT                             
*                                     WITHOUT CHANGING COUNTER                  
REMS0095 EQU   *                                                                
         BCTR  RF,0                DECREMENT FOR MOVE                           
         EX    RF,REMS0950         MOVE BY LENGTH                               
****>>>  MVC   OCDDS,=C'DDS'       INSERT LINE DELIMITER                        
         ST    RF,DMCB+8           INSERT LENGTH INTO P3                        
         GOTO1 REJMSGS,DMCB,(RC),(R2)                                           
         LA    R4,1(R4)                                                         
*                                                                               
         LA    R2,AORREA2H-AORREASH(R2)                                         
*                                  BUMP TO NEXT LINE                            
         BCT   R3,REMS0080         GO BACK FOR NEXT                             
*                                                                               
* SUPPRESS STATION ORDER COMMENTS PER PETRY AND ELLEN WEINSTEIN                 
*                                                                               
REMS0100 EQU   *                                                                
         B     REMS1000                                                         
*        MVC   AIO,AIO2            SET IO AREA FOR READING                      
*        L     R2,AIO2             SET WORK AREA FOR BUYS                       
*        USING RBUYREC,R2                                                       
*                                                                               
*        XCEFL RBUYREC,1024        CLEAR BUY BUILD AREA                         
*        MVI   RBUYKTYP,X'0B'      INSERT RECORD TYPE                           
*        MVC   RBUYKREP,TWAAGY     INSERT REP CODE                              
*        MVC   RBUYKCON,COMPCON    INSERT CONTRACT #, 9/COMP/REV                
*        MVC   KEY(27),RBUYREC     RETRIEVE KEY: ACTIVE ONLY                    
*        MVI   RDUPDATE,C'N'                                                    
*        GOTO1 HIGH                                                             
*        B     REMS0140                                                         
*REMS0120 EQU   *                                                               
*        MVI   RDUPDATE,C'N'                                                    
*        GOTO1 SEQ                 RETRIEVE NEXT BUY                            
*REMS0140 EQU   *                                                               
*        CLC   KEY(22),KEYSAVE     SAME ORDER?                                  
*        BNE   REMS1000            NO  - FINISHED                               
*        MVI   RDUPDATE,C'N'                                                    
*        GOTO1 GETREC              RETRIEVE RECORD                              
*        LA    R3,RBUYELEM                                                      
*REMS0160 EQU   *                                                               
*        CLI   0(R3),0             END OF RECORD?                               
*        BE    REMS0120            YES                                          
*        CLI   0(R3),X'84'         BY ORDER COMMENT ELEMENT?                    
*        BNE   REMS0200            NO  -                                        
*REMS0180 EQU   *                                                               
*        USING RBUYOCEL,R3                                                      
*        CLI   RBUYOCID,0          ZERO = STATION INPUT                         
*        BNE   REMS0120            COMMENT IS REP SIDE - IGNORE                 
*        BAS   RE,MSGCHECK                                                      
*        BZ    REMS0190            NO OUTPUT:  DON'T ADD TO LINECT              
*        LA    R4,1(R4)            OUTPUT:  BUMP COUNT                          
*REMS0190 EQU   *                                                               
*        MVC   OCTRANID,=C'ORDCOM' STATION SIDE:  SEND IT OUT                   
*        MVC   OCORDNUM,RETORD#                                                 
*        EDIT  RBUYAGBL,(4,OCBUYLIN),FILL=0,ZERO=NOBLANK                        
*                                                                               
*  INSERT AGENCY ORDER BUYLINE NUMBER INTO ORDER COMMENT                        
*                                                                               
*        ZIC   RF,RBUYOCLN         GET LENGTH OF LINE                           
*        LA    RE,4                DECREMENT FOR MOVE:  SKIP                    
*                                     CODE/LENGTH                               
*                                     COMMENT ENTRY INDICATOR                   
*                                     1 FOR EXEC STATEMENT                      
*        SR    RF,RE               SUBTRACT 3                                   
*        EX    RF,REMS0960         MOVE BY LENGTH                               
****>>>> MVC   OCDDS,=C'DDS'       INSERT LINE DELIMITER                        
*REMS0200 EQU   *                                                               
*        ZIC   RF,1(R3)                                                         
*        AR    R3,RF               BUMP TO NEXT ELEMENT                         
*        B     REMS0160            GO BACK FOR NEXT ELEMENT                     
*                                                                               
REMS0950 MVC   OCCOMMNT(0),8(R2)   MOVE BY LENGTH                               
*REMS0960 MVC   OCCOMMNT(0),RBUYOCNT                                            
*                                  MOVE BY LENGTH                               
*                                                                               
REMS1000 EQU   *                                                                
*                                                                               
*   DETERMINE IF LAST BUYLINE COMMENT ENTRY IS STILL WAITING TO                 
*      SPOOL.                                                                   
*                                                                               
         CLC   P,SPACES            ANYTHING ON PRINT LINE?                      
         BE    REMS1100                                                         
         OC    P,P                 ANYTHING ON PRINT LINE?                      
         BZ    REMS1100            NO  - NO OUTPUT                              
         GOTO1 SPOOL,DMCB,(R8)     SPOOL THE LINE                               
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
         OC    OCBUYLIN,OCBUYLIN   BUYLINE NUMBER IN LINE?                      
         BZ    REMS1100            NO                                           
         CLC   OCBUYLIN,SPACES     BUYLINE NUMBER IN LINE?                      
         BE    REMS1100                                                         
*                                                                               
*   ONLY BUMP THE LINECOUNT FOR COMMENTS FROM BUYLINE RECORDS.                  
*      THE SCREEN MESSAGE LINES ARE ALREADY COUNTED.                            
*                                                                               
*        LA    R4,1(R4)            BUMP THE LINECOUNT                           
*                                                                               
*        DROP  R2,R3,R5                                                         
         DROP  R5                                                               
*                                                                               
REMS1100 EQU   *                                                                
         LA    R5,P                                                             
         USING ORDTRLR,R5                                                       
         MVC   OTTRANID,=C'ORDTLR'                                              
         MVC   OTORDNUM,RETORD#                                                 
         LA    R4,2(R4)            ADD 1 EACH FOR HDR, TRLR                     
         EDIT  (R4),(6,OTCOUNT),FILL=0,ZERO=NOBLANK                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
         CLI   ACTCODE,C'A'        IF APPROVE/OPEN                              
         BE    REMS1300            PRINT TRAILER AND EXIT                       
*                                                                               
*   INSERT REJECT DATE/TIME STAMP                                               
*                                                                               
         LA    R2,AORHDLNH         ANY CONTRACT NUMBER ON SCREEN?               
         CLI   5(R2),0                                                          
         BE    REMS1300            NO  - DON'T GET CONTRACT NUMBER              
         CLC   =C'UNDARE',AORREAS  REJECT AS 'UNDARE'?                          
         BE    REMS1300            YES, X'1D' ELEMENT DELETED                   
*                                  PREPARE TO REWRITE CONTRACT RECORD           
         MVC   AIO,AIOAREA         SET IO AREA TO READ OLD RECORD               
         L     R2,AIO3                                                          
         USING RCONREC,R2                                                       
*                                                                               
         MVC   KEY(27),RCONREC     GET KEY FROM NEW RECORD                      
         MVC   KEYSAVE,KEY                                                      
         OI    DMINBTS,X'08'       RETURN DELETED KEY ALSO                      
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY     REDUNDANT CHECK: KEY FOUND?                  
         BNE   REMS1300            YES                                          
         NI    KEY+27,X'FF'-X'80'  RESTORE KEY                                  
         OI    DMINBTS,X'08'       RETURN DELETED RECORDS ALSO                  
         GOTO1 GETREC              READ INTO AIOAREA                            
         MVC   AIO,AIO3            RESET TO UPDATED CON RECORD                  
*                                                                               
*                                  UPDATE CONTRACT STATUS ELEMENT               
*                                                                               
*                                                                               
         LA    R2,RCONELEM         FIND X'1D' ELEMENT                           
REMS1120 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    REMS1140            YES - NOT FOUND - BUILD IT                   
         CLI   0(R2),X'1D'         DARE ELEMENT?                                
         BE    REMS1200            FOUND: UPDATE IT                             
         ZIC   RF,1(R2)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R2,RF                                                            
         B     REMS1120            GO BACK FOR NEXT                             
REMS1140 EQU   *                                                                
         XC    ELTBUILD,ELTBUILD   NOT FOUND: BUILD IT                          
         MVI   ELTBUILD,X'1D'      INSERT ELEMENT CODE                          
         MVI   ELTBUILD+1,RCONDL2Q INSERT ELEMENT LENGTH                        
         OI    ELTBUILD+2,X'20'    SET 'REJECTED' FLAG                          
*                                                                               
         CLI   AORHDLNH+5,0        ANY CONTRACT NUMBER ON SCREEN?               
         BE    *+8                                                              
         OI    ELTBUILD+2,X'80'    YES, SET 'LINKED' FLAG                       
*                                                                               
         TM    MISCFLAG,X'80'      DAILY ORDER?                                 
         BZ    *+8                                                              
         OI    ELTBUILD+2,X'08'    SET DAILY FLAG                               
                                                                                
         GOTO1 HEXIN,DMCB,RETORD#,ELTBUILD+3,8                                  
*                                  INSERT HEX VALUE OF AGY ORD #                
         MVC   ELTBUILD+RCONDRDR-RCONDREL(4),ACTDATE                            
*                                  YES - MOVE DATE+TIME TO REJECTED             
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO,ELTBUILD,=C'ADD=CODE'          
         B     REMS1250                                                         
REMS1200 EQU   *                                                                
                                                                                
         MVI   2(R2),X'A0'         SET 'REJECTED+LINKED' FLAGS, CLEAR           
*                                     ANY PREVIOUS VALUE                        
         TM    MISCFLAG,X'80'      DAILY ORDER?                                 
         BZ    *+8                                                              
         OI    2(R2),X'08'         SET DAILY FLAG                               
                                                                                
         MVC   RCONDRDR-RCONDREL(4,R2),ACTDATE                                  
*                                  YES - MOVE DATE+TIME TO REJECTED             
REMS1250 EQU   *                                                                
         GOTO1 PUTREC              WRITE UPDATED RECORD TO FILE                 
         GOTO1 WRITE               REWRITE CLEARED KEY FOR RECORD               
REMS1300 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R2,R5                                                            
         EJECT                                                                  
*                                                                               
*   MSGCHECK:  DETERMINE IF A LINE HAS TO BE SPOOLED.  THIS IS TO               
*      ENABLE THE '*' TO BE INSERTED FOR ANOTHER LINE FOLLOWING                 
*      INDICATOR.  THIS ROUTINE WILL ONLY BE CALLED WHEN A NEW LINE             
*      IS TO BE CONSTRUCTED.  IF OLD LINE IS WAITING TO SPOOL, IT               
*      MUST BE FLAGGED.                                                         
*   NOTE:  CONDITION CODE ON EXIT DETERMINES WHETHER LINE COUNT IS              
*      TO BE INCREMENTED.                                                       
*                                                                               
MSGCHECK NTR1                                                                   
         CLC   P,SPACES            ANYTHING ON PRINT LINE?                      
         BE    MCHE0100            NO  - NO OUTPUT                              
         OC    P,P                 ANYTHING ON PRINT LINE?                      
         BZ    MCHE0100            NO  - NO OUTPUT                              
         USING ORDCOM,R5                                                        
         MVI   OCCONTIN,C'*'       YES - INSERT INDICATOR                       
         GOTO1 SPOOL,DMCB,(R8)     SPOOL THE LINE                               
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
MCHE0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R5,R8                                                            
         EJECT                                                                  
*                                                                               
*   REJMSGS:  INSERTS MESSAGE INTO HOLD AREA, FOR LATER INCLUSION               
*        IN THE AGENCY ORDER HEADER, SO THAT MESSAGES CAN BE RE-                
*        CALLED DURING WORKSHEET PRINTING.                                      
*                                                                               
REJMSGS  NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET A(ERROR MESSAGE LINE)                  
         L     RF,8(R1)            RESET L(ERROR MESSAGE LINE)                  
         L     R3,AREJMESS         SET A(NEXT MESSAGE SLOT)                     
         LTR   RF,RF               ANY LENGTH IN LINE?                          
         BNZ   RMSG0040            YES - PROCESS                                
         MVC   0(2,R3),=X'1002'    NO  - PUT IN EMPTY ELEMENT                   
         LA    R3,2(R3)                                                         
         B     RMSG0200            EXIT                                         
RMSG0040 EQU   *                                                                
         LR    RE,RF               SET NEW ELEMENT LENGTH                       
         LA    RE,3(RE)            ADD FOR ELTID+CTRL+EX DEC                    
         MVI   0(R3),X'10'         INSERT ELEMENT CODE                          
         STC   RE,1(R3)            INSERT ELEMENT LENGTH                        
         EX    RF,RMSG0080         MOVE BY LENGTH                               
         AR    R3,RE               ADD LENGTH TO SLOT ADDR                      
         B     RMSG0200                                                         
*                                                                               
RMSG0080 MVC   2(0,R3),8(R2)       INSERT MESSAGE BY LENGTH                     
RMSG0200 EQU   *                                                                
         ST    R3,AREJMESS         REPLACE A(NEXT MESSAGE SLOT)                 
         XC    0(4,R3),0(R3)       CLEAR NEXT SLOT                              
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
*                                                                               
*   GETUID:  RETRIEVE UID FROM STATION RECORD                                   
*                                                                               
*   PARM 1: STATION CALL. WILL BE REPLACED WITH UID ON EXIT                     
*                                                                               
         DS    0F                                                               
GETUID   NTR1  BASE=*,WORK=(R4,500),LABEL=*                                     
         L     R3,0(R1)                                                         
         L     R2,AIO              SAVE CURRENT AIO                             
         ST    R4,AIO                                                           
*                                                                               
         LA    R6,KEY                                                           
         USING RSTAKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   RSTAKREP,AGENCY                                                  
         MVC   RSTAKSTA,0(R3)                                                   
         DROP  R6                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING RSTAUIEL,R6                                                      
         MVI   ELCODE,X'2A'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   0(L'ARSTAT,R3),RSTAUIST                                          
*                                                                               
         ST    R2,AIO              RESTORE AIO                                  
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*                                                                               
* FOR RADIO EDI, WE NEED TO SEND BACK THE SALESPERSON CODE/NAME                 
*                                                                               
SENDSAL  NTR1  BASE=*,LABEL=*                                                   
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R6,AIO1             CHECK IF WE NEED TO SEND                     
         MVI   ELCODE,X'0A'        SALESPERSON/POINTPERSON                      
         BRAS  RE,GETEL            FOR BOTH OPEN/REJECT                         
         BNE   SSALX                                                            
         USING RDARPPEL,R6                                                      
         CLC   RDARPPSP,SPACES                                                  
         BE    SSALX                                                            
*                                                                               
         GOTO1 =A(GETSALNM),RR=RELO                                             
*                                                                               
         XC    P,P                 CLEAR PRINT LINE                             
         LA    R2,P                                                             
         USING ORDSAL,R2                                                        
         MVC   OSTRANID,=C'ORDSAL'                                              
         MVC   OSORDNUM,RETORD#                                                 
         MVC   OSSPPCDE,RDARPPSP                                                
         MVC   OSSPPNME,WORK                                                    
         DROP  R2,R6                                                            
*                                                                               
         LA    R4,1(R4)            COUNT LINE                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              FORCE TO SINGLE PAGE                         
*                                                                               
SSALX    DS    0H                                                               
         XIT1  REGS=(R4)                                                        
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   GETSALNM :  RETRIEVE SALESPERSON NAME FOR UNLINKED REJECTIONS               
*               ON EXIT, WORK HAS EXPANDED NAME                                 
*                                                                               
*                                                                               
         DS    0F                                                               
GETSALNM NTR1  BASE=*,WORK=(R4,500),LABEL=*                                     
         L     R2,AIO              SAVE CURRENT AIO                             
         ST    R4,AIO                                                           
*                                                                               
         XC    KEY,KEY             GET SALESPERSON NAME                         
         XC    WORK,WORK                                                        
*                                                                               
         LA    R3,KEY                                                           
         USING RSALREC,R3                                                       
*                                                                               
         L     R6,AIO1             CHECK IF WE NEED TO SEND                     
         MVI   ELCODE,X'0A'        SALESPERSON                                  
         BRAS  RE,GETEL            FOR BOTH OPEN/REJECT                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RDARPPEL,R6                                                      
*                                                                               
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,AGENCY                                                  
         MVC   RSALKSAL,RDARPPSP                                                
         DROP  R6                                                               
*                                                                               
         L     R6,AIO1             CHECK IF UNWIRED                             
         MVI   ELCODE,X'0F'        YES, NEED TO GET POINTPERSON                 
         BRAS  RE,GETEL            FOR BOTH OPEN/REJECT                         
         BNE   GSAL10                                                           
         USING RDARFLEM,R6                                                      
         TM    RDARFLG1,X'01'      UNWIRED?                                     
         BZ    GSAL10                                                           
         DROP  R6                                                               
*                                                                               
         MVI   RSALKTYP,X'31'                                                   
*                                                                               
GSAL10   DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVC   WORK(L'RSALNAME),RSALNAME                                        
         DROP  R3                                                               
         ST    R2,AIO              RESTORE AIO                                  
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   STARTEND:  CONVERTS ROTATION MATRIX AND START DAY TO ONE-BYTE               
*        START/END DAY, INSERTS INTO ADDRESS                                    
*                                                                               
STARTEND NMOD1 0,*STEN*                                                         
         L     RC,0(R1)                                                         
         L     R2,4(R1)            A(INPUT: ROTATION+START DAY)                 
         L     R3,8(R1)            A(RECEIVING FIELD)                           
         L     R5,12(R1)           A(MYAREAD)                                   
         USING MYAREAD,R5                                                       
*                                                                               
         ZIC   RF,7(R2)            ROTATION START DAY                           
         SLL   RF,28               STRIP OFF ZONE BITS                          
         SRL   RF,28               SHIFT START DAY BACK                         
         STC   RF,ROTATDAY         SAVE ROTATION START DAY                      
*                                                                               
         LA    R8,0(R2)            A(ROTATION FIELD)                            
         LR    RE,R8               CALCULATE END OF ROTATION FIELD              
         LA    RE,7(RE)                                                         
         AR    R8,RF               GET A(1ST DAY+1)                             
         BCTR  R8,0                BACK OFF TO A(1ST ENTRY)                     
         LR    RF,R8               SAVE A(1ST ENTRY)                            
*                                     FOR WHEN 1ST ENTRY IS ONLY ENTRY          
         LA    R0,7                SET LOOP CONTROL TO SIX DAYS                 
STEX0040 EQU   *                                                                
         CR    R8,RE               END OF ROTATION FIELD REACHED?               
         BNE   STEX0080            NO                                           
         LA    R8,0(R2)            YES  - GO BACK TO FIRST LOCATION             
STEX0080 EQU   *                                                                
         CLI   0(R8),C' '          ARRAY POSITION USED?                         
         BE    STEX0200            NO  - SKIP IT                                
         CLI   0(R8),0             ARRAY POSITION USED?                         
         BE    STEX0200            NO  - SKIP IT                                
         CLI   STARTDAY,0          ANYTHING IN START DAY?                       
         BNE   STEX0120            YES - DON'T REPLACE                          
         LA    R4,7                NO  - CALCULATE STARTDAY                     
         SR    R4,R0               SUBTRACT REMAINING DAYS                      
         ZIC   R1,ROTATDAY         OFFSET BY ROTATION START DAY                 
         AR    R4,R1                                                            
         CH    R4,=H'7'            WRAPAROUND?                                  
         BNH   STEX0100            NO                                           
         SH    R4,=H'7'            YES - SUBTRACT 7                             
STEX0100 EQU   *                                                                
         STC   R4,STARTDAY         SAVE CALCULATED STARTDAY                     
STEX0120 EQU   *                                                                
         OC    0(1,R3),0(R3)       RECEIVING FIELD ENTRY MADE?                  
         BNZ   STEX0160            YES - START DAY ENTERED                      
         SLL   R4,4                NO  - MOVE START DAY TO HIGH NYBBLE          
         STC   R4,0(R3)            INSERT INTO RECORD                           
STEX0160 EQU   *                                                                
         LR    RF,R8               YES - SAVE NEW ARRAY POSITION                
STEX0200 EQU   *                                                                
         LA    R8,1(R8)            BUMP TO NEXT ARRAY LOCATION                  
         BCT   R0,STEX0040         GO BACK AND CHECK NEXT                       
         LA    R8,0(R2)            A(START OF ARRAY)                            
         BCTR  R8,0                BACK UP 1 POSITION                           
         SR    RF,R8               CALCULATED DISPLACEMENT                      
         STC   RF,ENDDAY           SAVE END DAY FOR EFF DATE SETTING            
         ZIC   RE,0(R3)            RETRIEVE START DAY                           
         AR    RF,RE               ADD START TO END                             
         STC   RF,0(R3)            PUT IT BACK IN RECORD                        
         LA    R8,0(R2)            COUNT NUMBER OF DAYS                         
         SR    RF,RF                                                            
         LA    R0,7                                                             
STEX0240 EQU   *                                                                
         CLI   0(R8),C' '          DAY ACTIVE?                                  
         BE    STEX0280            NO                                           
         CLI   0(R8),0             DAY ACTIVE?                                  
         BE    STEX0280            NO                                           
         LA    RF,1(RF)            YES - ADD 1                                  
STEX0280 EQU   *                                                                
         LA    R8,1(R8)            BUMP TO NEXT POSITION                        
         BCT   R0,STEX0240         GO BACK AND CHECK NEXT                       
         C     RF,=F'1'            COUNT = 1?                                   
         BH    STEX0320            NO  - HIGHER - EXIT                          
         BE    *+6                 YES - SET START=END IN RECORD                
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                     MEANS EMPTY ARRAY!!!                      
         ZIC   RF,0(R3)            RETRIEVE START/END DAY                       
         SLL   RF,28               DROP START DAY                               
         SRL   RF,24               MOVE END DAY BACK TO HI NYBBLE               
         NI    0(R3),X'0F'         CLEAR START DAY                              
         ZIC   RE,0(R3)            RETRIEVE 0/END DAY                           
         AR    RE,RF               ADD NEW START DAY                            
         STC   RE,0(R3)            MOVE IT BACK                                 
STEX0320 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   ADDREJS :  REJECTION MESSAGES                                               
*                                                                               
ADDREJS  NMOD1 0,*AREJ*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         L     R4,8(R1)            RESET A(IO AREA)                             
         USING RDARREC,R4                                                       
*                                                                               
         LA    R3,REJMESS          SET A(REJECT MSG AREA)                       
*                                                                               
ARJS0020 EQU   *                                                                
         CLI   0(R3),0             ANY ENTRY?                                   
         BE    ARJS0080            NO  - FINISHED                               
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R4),(R3),=C'ADD=CODE'             
*                                  YES - ADD ELEMENT TO AGY HDR REC             
         ZIC   RF,1(R3)            BUMP TO NEXT ENTRY IN TABLE                  
         AR    R3,RF                                                            
         B     ARJS0020            GO BACK FOR NEXT                             
ARJS0080 EQU   *                                                                
         XC    ELTBUILD,ELTBUILD   INSERT DATE/TIME ELEMENT                     
         MVC   ELTBUILD(2),=X'2006'                                             
         MVC   ELTBUILD+2(4),ACTDATE                                            
*                                  MOVE DATE+TIME TO OPENED                     
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R4),ELTBUILD,            X        
               =C'ADD=CODE'                                                     
         XIT1                                                                   
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   WRITE OUT DARE RECORD                                                       
*   UPDATE ALL APPROPRIATE PASSIVE KEYS IF RADIO EDI ORDER                      
*   AIO/AIO3 HAS DARE RECORD TO PUT TO FILE                                     
*                                                                               
WRITEREC NTR1  BASE=*,WORK=(R3,500),LABEL=*                                     
         USING MYAREAD,R5                                                       
*                                                                               
         L     R6,AIO                                                           
         USING RDARREC,R6                                                       
         CLI   RDARMEDI,C'R'       DO ONLY FOR RADIO                            
         BE    WR10                                                             
         GOTO1 PUTREC                                                           
         B     WRX                                                              
         DROP  R6                                                               
*                                                                               
WR10     DS    0H                                                               
         L     R6,AIO                                                           
         MVC   KEY(27),0(R6)                                                    
*                                                                               
         L     R2,AIO                                                           
         MVC   AIO,AIOAREA         SET ALTERNATE IO AREA                        
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                READ THE KEY                                 
         CLC   KEYSAVE(27),KEY     KEY MUST BE ON FILE?                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   HDRDA,KEY+28                                                     
*                                                                               
         GOTO1 GETREC              READ ORIGINAL REC INTO AIOAREA               
         ST    R2,AIO              RESTORE                                      
*                                                                               
*   PULL OLD KEYS PRE-P/P-S/P CHANGE:                                           
*          R3:  KEY BUILD AREA                                                  
*     AIOAREA:  CURRENT LOCATION OF OLD AGENCY ORDER RECORD                     
*        AIO2:  IO AREA                                                         
*                                                                               
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'41',ACOMFACS),(R3),AIOAREA,  X        
               AIO2                                                             
*                                                                               
         GOTO1 PUTREC              WRITE OUT CHANGED DARE RECORD                
*                                                                               
*   PULL NEW KEYS PRE-P/P-S/P CHANGE:                                           
*          R3:  KEY BUILD AREA                                                  
*         AIO:  CURRENT LOCATION OF NEW AGENCY ORDER RECORD                     
*        AIO2:  IO AREA                                                         
*                                                                               
         LA    R6,800(R3)          ADD 800 TO KEY BUILD AREA                    
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'41',ACOMFACS),(R6),AIO,AIO2           
*                                                                               
*   PROCESS OLD VS NEW PASSIVE POINTERS                                         
*                                                                               
         LA    R6,800(R3)          R6->NEW PASSIVE POINTERS                     
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'02',ACOMFACS),(R3),(R6),HDRDA         
*                                                                               
*                                                                               
WRX      DS    0H                                                               
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DOAUDIT :  ADD AUDIT TRAIL                                                  
*                                                                               
         DS    0F                                                               
DOAUDIT  NTR1  BASE=*,LABEL=*                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         L     R4,8(R1)            RESET A(IO AREA)                             
         USING RDARREC,R4                                                       
*                                                                               
         MVC   REVNUM,RDARRNUM                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),RDARKEY                                     
         MVI   KEY+RDARKRT-RDARKEY,X'70'                                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BNE   DOAUDX                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVC   WORK(4),HELLO       RECORD DARE HISTORY                          
         MVC   WORK+4(4),DATCON                                                 
         XC    DMCB+4(4),DMCB+4                                                 
         MVI   DMCB+4,X'FF'        VALID ACTION                                 
         MVI   DMCB+5,DHAPPROQ     ACTION OPEN                                  
         CLI   ACTCODE,C'A'        APPROVAL?                                    
         BE    *+8                                                              
         MVI   DMCB+5,DHREJECQ     ACTION REJECT                                
         MVC   DMCB+6(1),REVNUM    REVISION NUMBER                              
         GOTO1 VREGENDH,DMCB,AIO,,WORK                                          
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
DOAUDX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DATETIME:  DEVELOPS THE DATE AND TIME FOR STAMPING                          
*                                                                               
         DS    0F                                                               
DATETIME NMOD1 0,*DTTM*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,WORK),(2,ACTDATE)                                 
*                                  FETCH TODAY'S DATE                           
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STCM  R1,3,ACTTIME                                                     
                                                                                
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   GEN8DEKY:  DELETE OLD 8D/8E KEY AND ADD NEW ONES                            
*                                                                               
* CONFLTDT = OLD/ORIGINAL CONTRACT FLIGHT DATES                                 
* FLTDATES = NEW/DARE ORDER FLIGHT DATES                                        
*                                                                               
         DS    0F                                                               
GEN8DEKY NTR1  BASE=*,WORK=(R2,G8WORKQ),LABEL=*                                 
         USING G8WORKD,R2                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            SET MYAREA                                   
         USING MYAREAD,R5                                                       
*                                                                               
         MVC   G8SVAIO,AIO                                                      
*                                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         GOTO1 DATCON,DMCB,(3,RCONDATE),(2,NEWFLTDT)                            
         GOTO1 DATCON,(R1),(3,RCONDATE+3),(2,NEWFLTDT+2)                        
         CLC   CONFLTDT,NEWFLTDT   NO CHANGES, SHOULDN'T BE IN HERE             
         BE    GENKEYX                                                          
*                                                                               
         MVC   WORK(5),RCONKSTA    SAVE OFF STATION AND CON# BEFORE             
         MVC   WORK+5(4),RCONKCON  AIO GETS USED BY DATAMGR                     
         DROP  R6                                                               
*                                                                               
         LA    RE,G8IO                                                          
         ST    RE,AIO                                                           
*                                                                               
* DELETE OLD SET OF 8D/8E RIS KEYS FIRST                                        
*                                                                               
         XC    KEY,KEY                                                          
KYD      USING RCONKEY,KEY                                                      
         MVI   KYD.RCON8TYP,X'8D'  INSERT KEY ID                                
*                                                                               
GENKEY10 DS    0H                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         MVC   KYD.RCON8REP,AGENCY                                              
         MVC   KYD.RCON8FST(4),CONFLTDT                                         
         MVC   KYD.RCON8CON,WORK+5                                              
         LA    R3,1                                                             
         STC   R3,KYD.RCON8RID                                                  
         NI    DMINBTS,X'FF'-X'08' TURN OFF 'RETURN DELETES'                    
         GOTO1 HIGH                READ HIGH AND GET X'01' KEY                  
*                                  READ ACTIVE KEYS ONLY!                       
GENKEY20 DS    0H                                                               
         MVC   0(32,R4),KEY        SAVE OFF KEY                                 
*                                                                               
         CLC   KEY(16),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                DIE FOR NOW                                  
*                                                                               
         ZIC   R1,KYD.RCON8RID     SEQUENCE MUST MATCH                          
         CR    R3,R1                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    KEY+27,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
         GOTO1 SEQ                                                              
         AHI   R3,1                                                             
         LA    R4,32(R4)                                                        
         CHI   R3,4                                                             
         BL    GENKEY20                                                         
*                                                                               
* ADD NEW SET OF 8D/8E RIS KEYS WITH NEW FLIGHT DATES                           
*                                                                               
GENKEY30 DS    0H                                                               
*                                                                               
         LA    R3,1                                                             
         LA    R4,BLOCK                                                         
*                                                                               
GENKEY40 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(32),0(R4)                                                    
*                                  ADD NEW KEYS WITH NEW FLIGHT DATES           
         MVC   KYD.RCON8FST(4),NEWFLTDT                                         
         STC   R3,KYD.RCON8RID                                                  
         OI    DMINBTS,X'08'       RETURN DELETED KEY                           
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     CHECK IF NEW KEY EXISTS                      
         BNE   GENKEY50                                                         
         MVI   KEY+27,0            YES, RESTORE IT                              
         GOTO1 WRITE                                                            
         B     GENKEY60                                                         
*                                                                               
GENKEY50 DS    0H                                                               
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+27,0                                                         
         GOTO1 ADD                                                              
*                                                                               
GENKEY60 DS    0H                                                               
         AHI   R3,1                                                             
         LA    R4,32(R4)                                                        
         CHI   R3,4                                                             
         BL    GENKEY40                                                         
*                                                                               
         CLI   KEYSAVE,X'8E'       HAVE DONE PROCESSING 8D AND 8E KEYS?         
         BE    GENKEYX             YES, EXIT                                    
         XC    KEY,KEY             NO, SET TO PROCESS 8E KEYS                   
         MVI   KYD.RCON8TYP,X'8E'  INSERT KEY ID                                
         MVC   KYD.RCON8EST,WORK                                                
         B     GENKEY10                                                         
*                                                                               
GENKEYX  EQU   *                                                                
         MVC   AIO,G8SVAIO                                                      
         XIT1                                                                   
         DROP  KYD,R2                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   GENREC2:  RETRIEVE ORIGINAL RECORD, COMPARE AGAINST NEW RECORD              
*        TO DETERMINE CHANGE, IF ANY.                                           
*                                                                               
         DS    0F                                                               
GENREC2  NMOD1 0,*GRE2*                                                         
*                                                                               
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R5,4(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         L     R2,AIO              SET A(NEW BUY RECORD)                        
         L     R3,AIOAREA          SET A(OLD BUY RECORD)                        
*                                                                               
*   CERTAIN FIELDS MUST BE RESET BEFORE OLD VS NEW COMPARE CAN BE               
*      DONE.  INCLUDED IN THESE FIELDS ARE CLASS AND SECTION, WHICH             
*      CAN ONLY BE ENTERED FROM THE REP SIDE, SO WE DON'T WANT TO               
*      LOSE THEM.                                                               
*                                                                               
         MVC   RBUYCREA-RBUYREC(3,R2),RBUYCREA-RBUYREC(R3)                      
*                                  INSERT OLD CREATE DATE IN NEW                
         MVC   RBUYCHGD-RBUYREC(3,R2),RBUYCHGD-RBUYREC(R3)                      
*                                  INSERT OLD CHANGE DATE IN NEW                
         MVC   RBUYCHGI-RBUYREC(2,R2),RBUYCHGI-RBUYREC(R3)                      
*                                  INSERT OLD CODES IN NEW RECORD               
         MVC   NEWVER#,RBUYVER-RBUYREC(R2)                                      
*                                  SAVE NEW VERSION #                           
         MVC   RBUYVER-RBUYREC(1,R2),RBUYVER-RBUYREC(R3)                        
*                                  MOVE OLD VERSION NUMBER TO                   
*                                     NEW RECORD                                
         MVC   RBUYCLS-RBUYREC(6,R2),RBUYCLS-RBUYREC(R3)                        
*                                  INSERT OLD CLASS/SECTION IN NEW              
*                                                                               
*   COMPARE TOTAL RECORDS:  IF SAME, CODE IS OKAY                               
*                                                                               
         LR    R0,R2               SET LENGTH AND ADDRESS OF RECORDS            
         ZICM  R1,RBUYLEN-RBUYREC(R2),2                                         
         LR    RE,R3                                                            
         ZICM  RF,RBUYLEN-RBUYREC(R3),2                                         
         CLCL  R0,RE                                                            
         BE    GRE20200            RECORDS EQUAL:  OLD CODES WHICH              
*                                     WERE INSERTED ARE USED.                   
*                                                                               
GRE20005 EQU   *                   RECORDS NOT EQUAL                            
         MVC   RBUYVER-RBUYREC(1,R2),NEWVER#                                    
*                                  REINSERT NEW VERSION NUMBER                  
         LA    R8,RBUYCHGD-RBUYREC(R2)                                          
*                                  INSERT CHANGE DATE IN NEW RECORD             
         GOTO1 DATCON,DMCB,(5,FULL),(3,(R8))                                    
*                                  USE TODAYS DATE IN NEW RECORD                
         SR    R8,R8               SET COUNTER                                  
         XC    DUB,DUB             CLEAR STORAGE AREA                           
         LA    R4,DUB                                                           
         CLC   RBUYFLT-RBUYREC(1,R2),RBUYFLT-RBUYREC(R3)                        
*                                  SAME FLIGHT?                                 
         BE    GRE20010            YES                                          
         MVI   0(R4),C'F'          NO  - INSERT 'FLIGHT CHANGE'                 
         LA    R4,1(R4)            BUMP TO NEXT POSITION                        
         LA    R8,1(R8)            INCREMENT COUNTER                            
GRE20010 EQU   *                                                                
         CLC   RBUYDUR-RBUYREC(2,R2),RBUYDUR-RBUYREC(R3)                        
*                                  SAME DURATION?                               
         BE    GRE20020            YES                                          
         MVI   0(R4),C'L'          NO  - INSERT 'LENGTH CHANGE'                 
         LA    R4,1(R4)            BUMP TO NEXT POSITION                        
         LA    R8,1(R8)            INCREMENT COUNTER                            
GRE20020 EQU   *                                                                
         CLC   RBUYKPLN-RBUYREC(3,R2),RBUYKPLN-RBUYREC(R3)                      
*                                  SAME PLAN?                                   
         BE    GRE20030            YES                                          
         MVI   0(R4),C'P'          NO  - INSERT 'PLAN/CLS/SEC CHG'              
         LA    R4,1(R4)            BUMP TO NEXT POSITION                        
         LA    R8,1(R8)            INCREMENT COUNTER                            
         C     R8,=F'2'            BOTH SLOTS FULL?                             
         BNH   GRE20030            NO  - CONTINUE                               
         MVC   DUB(2),=C'* '       INSERT 'MORE THAN TWO CHANGES'               
         B     GRE20150            INSERT CODES AND EXIT                        
GRE20030 EQU   *                                                                
*                                                                               
*   CLASS AND SECTION ARE ILLOGICAL TESTS HERE.  NEITHER FIELD CAN BE           
*      ENTERED VIA AN AGENCY ORDER.  THEY MAY BE ENTERED ONLY VIA THE           
*      REP CONTRACT BUY SCREEN.  AS SUCH, THEY WILL ALWAYS BE ON THE            
*      'OLD' RECORD AND NEVER ON THE 'NEW' RECORD.  A PREVIOUSLY-               
*      ENTERED CODE IS TO BE CARRIED OVER.  CODING DURING FULL RECORD           
*      COMPARISONS TAKES CARE OF THAT.                                          
*                                                                               
         CLC   RBUYCLS-RBUYREC(3,R2),RBUYCLS-RBUYREC(R3)                        
*                                  SAME CLASS?                                  
         BE    GRE20040            YES                                          
         MVI   0(R4),C'P'          NO  - INSERT 'PLAN/CLS/SEC CHG'              
         LA    R4,1(R4)            BUMP TO NEXT POSITION                        
         LA    R8,1(R8)            INCREMENT COUNTER                            
         C     R8,=F'2'            BOTH SLOTS FULL?                             
         BNH   GRE20040            NO  - CONTINUE                               
         MVC   DUB(2),=C'* '       INSERT 'MORE THAN TWO CHANGES'               
         B     GRE20150            INSERT CODES AND EXIT                        
GRE20040 EQU   *                                                                
         CLC   RBUYSEC-RBUYREC(3,R2),RBUYSEC-RBUYREC(R3)                        
*                                  SAME SECTION?                                
         BE    GRE20050            YES                                          
         MVI   0(R4),C'P'          NO  - INSERT 'PLAN/CLS/SEC CHG'              
         LA    R4,1(R4)            BUMP TO NEXT POSITION                        
         LA    R8,1(R8)            INCREMENT COUNTER                            
         C     R8,=F'2'            BOTH SLOTS FULL?                             
         BNH   GRE20050            NO  - CONTINUE                               
         MVC   DUB(2),=C'* '       INSERT 'MORE THAN TWO CHANGES'               
         B     GRE20150            INSERT CODES AND EXIT                        
GRE20050 EQU   *                                                                
         CLC   RBUYCOS-RBUYREC(3,R2),RBUYCOS-RBUYREC(R3)                        
*                                  SAME SECTION?                                
         BE    GRE20060            YES                                          
         MVI   0(R4),C'R'          NO  - INSERT 'RATE CHG'                      
         LA    R4,1(R4)            BUMP TO NEXT POSITION                        
         LA    R8,1(R8)            INCREMENT COUNTER                            
         C     R8,=F'2'            BOTH SLOTS FULL?                             
         BNH   GRE20060            NO  - CONTINUE                               
         MVC   DUB(2),=C'* '       INSERT 'MORE THAN TWO CHANGES'               
         B     GRE20150            INSERT CODES AND EXIT                        
GRE20060 EQU   *                                                                
         BAS   RE,CHKBCOMS         CHECK BUY COMMENTS                           
         BZ    GRE20070            NO CHANGE TO COMMENTS                        
         MVI   0(R4),C'Z'          NO  - INSERT 'BUY COMMENT CHG'               
         LA    R4,1(R4)            BUMP TO NEXT POSITION                        
         LA    R8,1(R8)            INCREMENT COUNTER                            
         C     R8,=F'2'            BOTH SLOTS FULL?                             
         BNH   GRE20070            NO  - CONTINUE                               
         MVC   DUB(2),=C'* '       INSERT 'MORE THAN TWO CHANGES'               
         B     GRE20150            INSERT CODES AND EXIT                        
GRE20070 EQU   *                                                                
         BAS   RE,CHKOCOMS         CHECK ORDER COMMENTS                         
         BZ    GRE20080            NO CHANGE TO COMMENTS                        
         MVI   0(R4),C'O'          NO  - INSERT 'ORDER COMMENT CHG'             
         LA    R4,1(R4)            BUMP TO NEXT POSITION                        
         LA    R8,1(R8)            INCREMENT COUNTER                            
         C     R8,=F'2'            BOTH SLOTS FULL?                             
         BNH   GRE20080            NO  - CONTINUE                               
         MVC   DUB(2),=C'* '       INSERT 'MORE THAN TWO CHANGES'               
         B     GRE20150            INSERT CODES AND EXIT                        
GRE20080 EQU   *                                                                
         BAS   RE,CHKDTIMS         CHECK DAY/TIME ELEMENTS                      
         BZ    GRE20090            NO CHANGE TO DAY/TIME ELEMENTS               
         MVC   0(1,R4),DUB+4       NO  - INSERT 'DAY/TIME CHG'                  
         LA    R4,1(R4)            BUMP TO NEXT POSITION                        
         LA    R8,1(R8)            INCREMENT COUNTER                            
         C     R8,=F'2'            BOTH SLOTS FULL?                             
         BNH   GRE20090            NO  - CONTINUE                               
         MVC   DUB(2),=C'* '       INSERT 'MORE THAN TWO CHANGES'               
         B     GRE20150            INSERT CODES AND EXIT                        
GRE20090 EQU   *                                                                
         BAS   RE,CHKEFDTS         CHECK EFFECTIVE DATE ELEMENTS                
         BZ    GRE20100            NO CHANGE TO EFF DATE ELEMENTS               
         MVC   0(1,R4),DUB+4       NO  - INSERT 'EFF DATE CHG'                  
         LA    R4,1(R4)            BUMP TO NEXT POSITION                        
         LA    R8,1(R8)            INCREMENT COUNTER                            
         C     R8,=F'2'            BOTH SLOTS FULL?                             
         BNH   GRE20100            NO  - CONTINUE                               
         MVC   DUB(2),=C'* '       INSERT 'MORE THAN TWO CHANGES'               
         B     GRE20150            INSERT CODES AND EXIT                        
GRE20100 EQU   *                                                                
GRE20150 EQU   *                                                                
         MVC   RBUYCHGI-RBUYREC(2,R2),DUB                                       
*                                  INSERT DEVELOPED CHANGE CODES                
GRE20200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CHKBCOMS:  COMPARE OLD BUY COMMENT TO NEW BUY COMMENTS.                     
*      RECORD CAN CONTAIN ONLY TWO COMMENT LINES.                               
*                                                                               
CHKBCOMS NTR1                                                                   
         XCEFL WORK2,300           CLEAR THE WORK SPACE                         
         LA    R1,WORK2            RETRIEVE NEW RECORD X'04' ELTS               
         LA    R2,RBUYELEM-RBUYREC(R2)                                          
*                                  RESET A(NEW RECORD) TO X'01' ELT             
CHKB0020 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    CHKB0060            YES                                          
         CLI   0(R2),X'04'         BUY COMMENT ELEMENT?                         
         BNE   CHKB0050            NO  - BUMP TO NEXT ELEMENT                   
CHKB0040 EQU   *                                                                
         ZIC   RF,1(R2)            YES - SET TO MOVE ELEMENT BY LENGTH          
         BCTR  RF,0                                                             
         EX    RF,CHKB0900                                                      
         LA    RF,1(RF)                                                         
         AR    R1,RF               BUMP WORK AREA TO NEXT SLOT                  
CHKB0050 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         B     CHKB0020            GO BACK FOR NEXT ELEMENT                     
CHKB0060 EQU   *                                                                
         LA    R1,WORK2+150        RETRIEVE OLD RECORD X'04' ELTS               
         LA    R3,RBUYELEM-RBUYREC(R3)                                          
*                                  RESET A(OLD RECORD) TO X'01' ELT             
CHKB0120 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    CHKB0160            YES                                          
         CLI   0(R3),X'04'         BUY COMMENT ELEMENT?                         
         BNE   CHKB0150            NO  - BUMP TO NEXT ELEMENT                   
CHKB0140 EQU   *                                                                
         ZIC   RF,1(R3)            YES - SET TO MOVE ELEMENT BY LENGTH          
         BCTR  RF,0                                                             
         EX    RF,CHKB0910                                                      
         LA    RF,1(RF)                                                         
         AR    R1,RF               BUMP WORK AREA TO NEXT SLOT                  
CHKB0150 EQU   *                                                                
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         B     CHKB0120            GO BACK FOR NEXT ELEMENT                     
CHKB0160 EQU   *                                                                
         CLC   WORK2(150),WORK2+150 WORK AREAS EQUAL?                           
         BE    CHKB0240            YES - EXIT CC = ZERO                         
         LTR   RB,RB               NO  - SET CC NOT = ZERO                      
CHKB0240 EQU   *                                                                
         XIT1                                                                   
*                                                                               
CHKB0900 MVC   0(0,R1),0(R2)       MOVE COMMENT BY LENGTH                       
CHKB0910 MVC   0(0,R1),0(R3)       MOVE COMMENT BY LENGTH                       
*                                                                               
         EJECT                                                                  
*                                                                               
*   CHKOCOMS:  COMPARE OLD ORDER COMMENT TO NEW ORDER COMMENTS.                 
*      RECORD CAN CONTAIN ONLY TWO COMMENT LINES.                               
*                                                                               
CHKOCOMS NTR1                                                                   
         XCEFL WORK2,300           CLEAR THE WORK SPACE                         
         LA    R1,WORK2            RETRIEVE NEW RECORD X'84' ELTS               
         LA    R2,RBUYELEM-RBUYREC(R2)                                          
*                                  RESET A(NEW RECORD) TO X'01' ELT             
CHKO0020 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    CHKO0060            YES                                          
         CLI   0(R2),X'84'         ORDER COMMENT ELEMENT?                       
         BNE   CHKO0050            NO  - BUMP TO NEXT ELEMENT                   
CHKO0040 EQU   *                                                                
         ZIC   RF,1(R2)            YES - SET TO MOVE ELEMENT BY LENGTH          
         BCTR  RF,0                                                             
         EX    RF,CHKO0900                                                      
         LA    RF,1(RF)                                                         
         AR    R1,RF               BUMP WORK AREA TO NEXT SLOT                  
CHKO0050 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         B     CHKO0020            GO BACK FOR NEXT ELEMENT                     
CHKO0060 EQU   *                                                                
         LA    R1,WORK2+150        RETRIEVE OLD RECORD X'84' ELTS               
         LA    R3,RBUYELEM-RBUYREC(R3)                                          
*                                  RESET A(OLD RECORD) TO X'01' ELT             
CHKO0120 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    CHKO0160            YES                                          
         CLI   0(R3),X'84'         BUY COMMENT ELEMENT?                         
         BNE   CHKO0150            NO  - BUMP TO NEXT ELEMENT                   
CHKO0140 EQU   *                                                                
         ZIC   RF,1(R3)            YES - SET TO MOVE ELEMENT BY LENGTH          
         BCTR  RF,0                                                             
         EX    RF,CHKO0910                                                      
         LA    RF,1(RF)                                                         
         AR    R1,RF               BUMP WORK AREA TO NEXT SLOT                  
CHKO0150 EQU   *                                                                
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         B     CHKO0120            GO BACK FOR NEXT ELEMENT                     
CHKO0160 EQU   *                                                                
         CLC   WORK2(150),WORK2+150 WORK AREAS EQUAL?                           
         BE    CHKO0240            YES - EXIT CC = ZERO                         
         LTR   RB,RB               NO  - SET CC NOT = ZERO                      
CHKO0240 EQU   *                                                                
         XIT1                                                                   
*                                                                               
CHKO0900 MVC   0(0,R1),0(R2)       MOVE COMMENT BY LENGTH                       
CHKO0910 MVC   0(0,R1),0(R3)       MOVE COMMENT BY LENGTH                       
*                                                                               
         EJECT                                                                  
*                                                                               
*   CHKDTIMS:  COMPARE OLD DAY/TIME ELEMENTS TO NEW.                            
*                                                                               
CHKDTIMS NTR1                                                                   
         XCEFL WORK2,300           CLEAR THE WORK SPACE                         
         LA    R1,WORK2            RETRIEVE NEW RECORD X'02' ELTS               
         LA    R2,RBUYELEM-RBUYREC(R2)                                          
*                                  RESET A(NEW RECORD) TO X'01' ELT             
         XC    FULL,FULL           SET UP COUNTERS                              
CHKD0020 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    CHKD0080            YES                                          
         CLI   0(R2),X'02'         DAY/TIME ELEMENT?                            
         BNE   CHKD0060            NO  - BUMP TO NEXT ELEMENT                   
CHKD0040 EQU   *                                                                
         ZIC   RF,1(R2)            YES - SET TO MOVE ELEMENT BY LENGTH          
         BCTR  RF,0                                                             
         EX    RF,CHKD0900                                                      
         LA    RF,1(RF)                                                         
         AR    R1,RF               BUMP WORK AREA TO NEXT SLOT                  
         LH    RF,FULL                                                          
         LA    RF,1(RF)            INCREMENT NEW ELEMENT COUNT                  
         STH   RF,FULL             SAVE IT BACK                                 
CHKD0060 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         B     CHKD0020            GO BACK FOR NEXT ELEMENT                     
CHKD0080 EQU   *                                                                
         LA    R1,WORK2+150        RETRIEVE OLD RECORD X'02' ELTS               
         LA    R3,RBUYELEM-RBUYREC(R3)                                          
*                                  RESET A(OLD RECORD) TO X'01' ELT             
CHKD0120 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    CHKD0180            YES                                          
         CLI   0(R3),X'02'         DAY TIME ELEMENT?                            
         BNE   CHKD0160            NO  - BUMP TO NEXT ELEMENT                   
CHKD0140 EQU   *                                                                
         ZIC   RF,1(R3)            YES - SET TO MOVE ELEMENT BY LENGTH          
         BCTR  RF,0                                                             
         EX    RF,CHKD0910                                                      
         LA    RF,1(RF)                                                         
         AR    R1,RF               BUMP WORK AREA TO NEXT SLOT                  
         LH    RF,FULL+2                                                        
         LA    RF,1(RF)            INCREMENT OLD ELEMENT COUNT                  
         STH   RF,FULL+2           SAVE IT BACK                                 
CHKD0160 EQU   *                                                                
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         B     CHKD0120            GO BACK FOR NEXT ELEMENT                     
CHKD0180 EQU   *                                                                
         CLC   WORK2(150),WORK2+150                                             
*                                  WORK AREAS EQUAL?                            
         BE    CHKD0300            YES - NO CHANGE                              
*                                  NO  - COMPARE                                
         CLC   FULL(2),FULL+2      COMPARE OLD/NEW ELEMENT COUNTS               
         BE    CHKD0200            EQUAL: COMPARE ELEMENTS                      
         MVI   DUB+4,C'D'          SET 'CHANGE TO DAYS'                         
         B     CHKD0280            EXIT                                         
CHKD0200 EQU   *                                                                
         LH    RF,FULL             LOAD COUNTER FOR LOOP                        
         LA    R2,WORK2            SET A(1ST NEW D/T ELEMENT)                   
         LA    R3,WORK2+150        SET A(1ST NEW D/T ELEMENT)                   
CHKD0220 EQU   *                                                                
         CLC   RBUYDYIN-RBUYDYEL(2,R2),RBUYDYIN-RBUYDYEL(R3)                    
*                                  OLD=NEW DAYS IN ELEMENTS?                    
         BE    CHKD0240            YES                                          
         MVI   DUB+4,C'D'          NO  - SET 'CHG TO DAYS'                      
         B     CHKD0280            EXIT                                         
CHKD0240 EQU   *                                                                
         CLC   RBUYDYT1-RBUYDYEL(4,R2),RBUYDYT1-RBUYDYEL(R3)                    
*                                  OLD=NEW TIMES IN ELEMENTS?                   
         BE    CHKD0260            YES                                          
         MVI   DUB+4,C'T'          NO  - SET 'CHG TO TIMES'                     
         B     CHKD0280            EXIT                                         
CHKD0260 EQU   *                                                                
         ZIC   RE,1(R2)            BUMP TO NEXT ELEMENTS                        
         AR    R2,RE                                                            
         AR    R3,RE                                                            
         BCT   RF,CHKD0220         GO BACK FOR NEXT                             
         B     CHKD0300            EXIT CC = ZERO                               
CHKD0280 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     CHKD0320                                                         
CHKD0300 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
CHKD0320 EQU   *                                                                
         XIT1                                                                   
*                                                                               
CHKD0900 MVC   0(0,R1),0(R2)       MOVE COMMENT BY LENGTH                       
CHKD0910 MVC   0(0,R1),0(R3)       MOVE COMMENT BY LENGTH                       
*                                                                               
         EJECT                                                                  
*                                                                               
*   CHKEFDTS:  COMPARE OLD EFFECTIVE DATE ELEMENTS TO NEW.                      
*                                                                               
CHKEFDTS NTR1                                                                   
         XCEFL WORK2,300           CLEAR THE WORK SPACE                         
         LA    R1,WORK2            RETRIEVE NEW RECORD X'03' ELTS               
         LA    R2,RBUYELEM-RBUYREC(R2)                                          
*                                  RESET A(NEW RECORD) TO X'01' ELT             
         XC    FULL,FULL           SET UP COUNTERS                              
CHKE0020 EQU   *                                                                
         CLI   0(R2),0             END OF RECORD?                               
         BE    CHKE0080            YES                                          
         CLI   0(R2),X'03'         EFF DATE ELEMENT?                            
         BNE   CHKE0060            NO  - BUMP TO NEXT ELEMENT                   
CHKE0040 EQU   *                                                                
         ZIC   RF,1(R2)            YES - SET TO MOVE ELEMENT BY LENGTH          
         BCTR  RF,0                                                             
         EX    RF,CHKE0900                                                      
         LA    RF,1(RF)                                                         
         AR    R1,RF               BUMP WORK AREA TO NEXT SLOT                  
         LH    RF,FULL                                                          
         LA    RF,1(RF)            INCREMENT NEW ELEMENT COUNT                  
         STH   RF,FULL             SAVE IT BACK                                 
CHKE0060 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         B     CHKE0020            GO BACK FOR NEXT ELEMENT                     
CHKE0080 EQU   *                                                                
         LA    R1,WORK2+150        RETRIEVE OLD RECORD X'03' ELTS               
         LA    R3,RBUYELEM-RBUYREC(R3)                                          
*                                  RESET A(OLD RECORD) TO X'01' ELT             
CHKE0120 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    CHKE0180            YES                                          
         CLI   0(R3),X'03'         EFF DATE ELEMENT?                            
         BNE   CHKE0160            NO  - BUMP TO NEXT ELEMENT                   
CHKE0140 EQU   *                                                                
         ZIC   RF,1(R3)            YES - SET TO MOVE ELEMENT BY LENGTH          
         BCTR  RF,0                                                             
         EX    RF,CHKE0910                                                      
         LA    RF,1(RF)                                                         
         AR    R1,RF               BUMP WORK AREA TO NEXT SLOT                  
         LH    RF,FULL+2                                                        
         LA    RF,1(RF)            INCREMENT OLD ELEMENT COUNT                  
         STH   RF,FULL+2           SAVE IT BACK                                 
CHKE0160 EQU   *                                                                
         ZIC   RF,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,RF                                                            
         B     CHKE0120            GO BACK FOR NEXT ELEMENT                     
CHKE0180 EQU   *                                                                
         CLC   WORK2(150),WORK2+150                                             
*                                  WORK AREAS EQUAL?                            
         BE    CHKE0300            YES - NO CHANGE                              
*                                  NO  - COMPARE                                
         CLC   FULL(2),FULL+2      COMPARE OLD/NEW ELEMENT COUNTS               
         BE    CHKE0200            EQUAL: COMPARE ELEMENTS                      
         MVI   DUB+4,C'E'          SET 'CHANGE TO EFF DATES'                    
         B     CHKE0280            EXIT                                         
CHKE0200 EQU   *                                                                
         LH    RF,FULL             LOAD COUNTER FOR LOOP                        
         LA    R2,WORK2            SET A(1ST NEW D/T ELEMENT)                   
         LA    R3,WORK2+150        SET A(1ST NEW D/T ELEMENT)                   
CHKE0220 EQU   *                                                                
         CLC   RBUYDTST-RBUYDTEL(6,R2),RBUYDTST-RBUYDTEL(R3)                    
*                                  OLD=NEW EFF DATES IN ELEMENTS?               
         BE    CHKE0240            YES                                          
         MVI   DUB+4,C'E'          NO  - SET 'CHG TO EFF DATES'                 
         B     CHKE0280            EXIT                                         
CHKE0240 EQU   *                                                                
         CLC   RBUYDTNW-RBUYDTEL(2,R2),RBUYDTNW-RBUYDTEL(R3)                    
*                                  OLD=NEW SPOTS/NO. WKS IN ELTS                
         BE    CHKE0260            YES                                          
         MVI   DUB+4,C'S'          NO  - SET 'CHG TO SPOTS/WEEK'                
         B     CHKE0280            EXIT                                         
CHKE0260 EQU   *                                                                
         ZIC   RE,1(R2)            BUMP TO NEXT ELEMENTS                        
         AR    R2,RE                                                            
         AR    R3,RE                                                            
         BCT   RF,CHKE0220         GO BACK FOR NEXT                             
         B     CHKE0300            EXIT CC = ZERO                               
CHKE0280 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     CHKE0320                                                         
CHKE0300 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
CHKE0320 EQU   *                                                                
         XIT1                                                                   
*                                                                               
CHKE0900 MVC   0(0,R1),0(R2)       MOVE COMMENT BY LENGTH                       
CHKE0910 MVC   0(0,R1),0(R3)       MOVE COMMENT BY LENGTH                       
*                                                                               
         EJECT                                                                  
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   GENCOMMT:  GENERATE A COMMENT RECORD IN THOSE INSTANCES WHEN                
*      A PROGRAM NAME IS PRESENT, AND NO DARE COMMENTS HAVE BEEN                
*      FOUND.  OTHERWISE, THE PROGRAM NAME AS A COMMENT WILL BE                 
*      SKIPPED.                                                                 
*                                                                               
GENCOMMT NMOD1 0,*CMMT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET A(BUYREC)                              
         USING RBUYREC,R2                                                       
*                                                                               
         L     R5,8(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         CLI   KATZEDI,C'Y'        KATZ/EDI USES ONLY THE FIRST 32              
         BNE   GCOM0005            WITH THE LAST 2 FOR DAYPART                  
*                                                                               
*                                  REMOVE OLD X'ED' DAYPART CODE                
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'ED',(R2)),0,0                   
         XC    ELEM,ELEM           FOR KATZ/EDI ADD THE DAYPART CODE            
         LA    R6,ELEM             ELEMENT                                      
         USING RBUYEDEL,R6                                                      
         MVI   RBUYEDCD,X'ED'                                                   
         MVI   RBUYEDLN,RBUYEDLQ                                                
         MVC   RBUYEDDP,PROGNAME+32                                             
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),(R2),ELEM,0                        
         DROP  R6                                                               
*                                                                               
GCOM0005 EQU   *                                                                
         XC    ELTBUILD,ELTBUILD   CLEAR ELEMENT BUILD AREA                     
         MVI   ELTBUILD,X'04'      INSERT ELEMENT CODE                          
*                                  CLEAR WORKSPACE                              
         LA    R6,PROGNAME+33      SCAN PROGRAM NAME FOR BLANKS                 
         LA    RF,34               LOOP CONTROL                                 
         CLI   KATZEDI,C'Y'        KATZ/EDI USES ONLY THE FIRST 32              
         BNE   GCOM0010            WITH THE LAST 2 FOR DAYPART                  
         LA    R6,PROGNAME+31      SCAN PROGRAM NAME FOR BLANKS                 
         LA    RF,32               LOOP CONTROL                                 
GCOM0010 EQU   *                                                                
         CLI   0(R6),C' '          CHARACTER = SPACE?                           
         BNE   GCOM0015            NO  - LAST CHARACTER FOUND                   
         BCTR  R6,0                YES - BACK UP 1 SPACE                        
         BCT   RF,GCOM0010         LOOP THROUGH ALL                             
         DC    H'0'                SHOULDN'T HAPPEN:  SPACES CHECKED            
GCOM0015 EQU   *                                                                
         LA    RF,1(RF)            ADD 1 FOR KEYWORD (+2 -1 FOR EX)             
         MVC   ELTBUILD+2(2),=C'P='                                             
*                                  INSERT KEYWORK                               
         EX    RF,GCOM0505         MOVE PROGRAM NAME BY LENGTH                  
         LA    RF,3(RF)            RESTORE LENGTH + L(CONTROLS)                 
         STC   RF,ELTBUILD+1       INSERT LENGTH INTO ELEMENT                   
*                                                                               
* SKIP BUILDING P= COMMENT PROGRAM NAME ELEMENT                                 
*                                                                               
*        GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBUILD,         X        
               =C'ADD=CODE'                                                     
*                                                                               
* BUILD DEDICATED PROGRAM NAME ELEMENT                                          
*                                                                               
         MVI   ELTBUILD,X'21'      PROGRAM NAME ELEMENT                         
         XC    ELTBUILD+2(L'ELTBUILD-2),ELTBUILD+2                              
         ZIC   RF,ELTBUILD+1       REUSE LENGTH FROM THE P= COMMENT             
         SHI   RF,2                ELEMENT TO BUILD LENGTH OF                   
         STC   RF,ELTBUILD+1       PROGRAM NAME ELEMENT                         
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELTBUILD+2(0),PROGNAME                                           
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RBUYREC,ELTBUILD,         X        
               =C'ADD=CODE'                                                     
*                                                                               
         MVI   PROGNAME,C' '       CLEAR THE PROGRAM NAME                       
         MVC   PROGNAME+1(L'PROGNAME-1),PROGNAME                                
         XIT1                                                                   
*                                                                               
GCOM0505 MVC   ELTBUILD+4(0),PROGNAME                                           
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DELORD:  AN 'UNDARE' ACTION REQUIRES THAT THE AGENCY ORDER                  
*        RECORDS BE DELETED AFTER THE WORKSHEET HAS BEEN PRODUCED.              
*                                                                               
DELORD   NMOD1 0,*DEOR*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   AIO,AIO2            SET IO AREA FOR READING                      
         L     R4,AIO2             SET WORK AREA FOR AGY ORDER RECS             
         USING RDARREC,R4                                                       
*                                                                               
         XCEFL RDARREC,1024        CLEAR BUILD AREA                             
*                                  RETRIEVE HEADER OF AGENCY ORDER              
         MVC   KEY(L'SELECTKY),SELECTKY                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   KEY,RDARKEY         LOAD KEY AFTER READ                          
         GOTO1 HIGH                READ RECORD FOR REWRITE                      
         B     DEOR0040            SKIP RECORD REREAD                           
DEOR0020 EQU   *                                                                
         GOTO1 SEQ                 RETRIEVE NEXT BUY                            
DEOR0040 EQU   *                                                                
         CLC   KEY(24),KEYSAVE     SAME ORDER?                                  
         BNE   DEOR0400            NO  - FINISHED                               
         GOTO1 GETREC              RETRIEVE RECORD                              
         CLI   KEY+RDARKRT-RDARKEY,X'10'                                        
         BNE   DEOR0060                                                         
         MVC   HDRDA,KEY+28                                                     
         GOTOR DELPAS              DELETE PASSIVE PTS                           
*                                                                               
DEOR0060 EQU   *                                                                
         OI    RDARCNTL,X'80'      MARK RECORD DELETED                          
         GOTO1 PUTREC              REWRITE BUY AS DELETED                       
         OI    KEY+27,X'80'        SET KEY TO DELETED                           
         GOTO1 WRITE               REWRITE DELETED KEY                          
         B     DEOR0020            GO BACK FOR NEXT                             
DEOR0400 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP R4                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* SPECIAL FOR KATZ EDI ORDERS. IF ACTION IS OPEN, ADD A PASSIVE KEY             
* X'E1' WITH CONTRACT # AND DATE/TIME STAMP INFO TO BE PICKED UP LATER          
* AT NIGHT BY A REPORT THAT WILL WRITE THESE ORDERS OUT TO TAPE                 
*                                                                               
GENEDIKY NMOD1 0,*EDIK*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING REDIKEY,R6                                                       
         MVI   REDIKTYP,REDIKTYQ                                                
         MVC   REDIKREP,AGENCY                                                  
         MVI   REDIKACT,C'A'       ACTION IF OPEN                               
         MVC   REDIKCON,CCONKNUM   CONTRACT NUMBER IN PWOS                      
*                                  DATE IN YYMMDD                               
         GOTO1 DATCON,DMCB,(5,WORK),(3,REDIKDTE)                                
*                                  FETCH TODAY'S DATE                           
         THMS  DDSTIME=YES                                                      
         ST    R0,DUB              ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB+4            DDS TIME                                     
         AP    DUB(4),DUB+4(4)                                                  
         ICM   R1,15,DUB                                                        
         SRL   R1,4                SHIFT OFF SIGN                               
         STCM  R1,7,REDIKTIM       TIME IN HHMMSS                               
         DROP  R6                                                               
*                                                                               
         L     RE,AIOAREA                                                       
         LA    RF,LIOS                                                          
         XCEF                                                                   
         L     R6,AIOAREA                                                       
         MVC   0(27,R6),KEY                                                     
*                                                                               
         MVC   KEY+28(4),RECADDR   CONTRACT RECORD ADDRESS                      
*                                                                               
         GOTO1 ADD                 ADD THE KEY                                  
         TM    DMCB+8,X'FD'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIOAREA                                                       
         USING REDBKEY,R6                                                       
*                                                                               
         MVI   REDBKTYP,REDBKTYQ   ADD X'0E' RECORD AS A BACKUP                 
         MVI   REDBLEN+1,34+REDBELLQ                                            
         MVI   REDBCODE,1          TO THE X'E1' KEYS                            
         MVI   REDBELLN,REDBELLQ                                                
         DROP  R6                                                               
*                                                                               
         MVC   MYSVAIO,AIO                                                      
         MVC   AIO,AIOAREA         SET ALTERNATE READ AREA                      
         GOTO1 ADDREC                                                           
*                                                                               
         MVC   AIO,MYSVAIO         RESTORE AIO BEFORE EXIT                      
*                                                                               
GENEDIX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   SETTRADE:  SETUP TRADE FLAG IN CONTRACT RECORD                              
*                                                                               
SETTRADE NTR1  BASE=*,LABEL=*                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         TM    MISCFLAG,X'20'      TRADE ORDER?                                 
         BZ    SETTRX                                                           
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'1E'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
         USING RCONRFEL,R6                                                      
         OI    RCONRF1,X'08'       MARK CONTRACT AS TRADE ORDER                 
         DROP  R6                                                               
*                                                                               
SETTRX   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DELPAS:  DELETE PASSIVE POINTERS FOR UNDARE                                 
*                                                                               
DELPAS   NTR1  BASE=*,WORK=(R3,500),LABEL=*                                     
*   PULL OLD KEYS PRE-P/P-S/P CHANGE:                                           
*        R3: KEY BUILD AREA                                                     
*        AIO2: CURRENT LOCATION OF AGENCY ORDER RECORD                          
*                                                                               
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'81',ACOMFACS),(R3),AIO2               
*                                                                               
         LA    R4,800(R3)          R4->ALL NULL                                 
         LHI   RF,800              THIS WILL DELETE ALL THE OLD PTS             
         XCEF  (R4)                                                             
         GOTO1 (RFGENDTR,REPFACS),DMCB,(X'02',ACOMFACS),(R3),(R4),     >        
               HDRDA                                                            
*                                                                               
DELPASX  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   SETUPHIA:  DELETE ANY HIATUS ELEMENT FROM THE CONTRACT RECORD.              
*        ACCESS THE TYPE 15 RECORD.  IF FOUND, ADD ITS X'02' ELEMENT            
*        TO THE CONTRACT RECORD AS AN X'25' ELEMENT.                            
*                                                                               
SETUPHIA NMOD1 0,*HIAT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,AIO3                                                          
         USING RCONREC,R4                                                       
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'25',RCONREC),0,0                
*                                  DELETE HIATUS ELEMENT, IF ANY                
         GOTO1 HELLO,DMCB,(C'D',=C'REPFILE'),(X'26',RCONREC),0,0                
*                                  DELETE HIATUS COMMENT, IF ANY                
         MVC   AIO,AIO1            SET IO AREA FOR X'15' RECORD                 
         L     R3,AIO                                                           
         USING RDARREC,R3                                                       
         MVI   RDARKRT,X'15'       SET REC TYPE TO 'HIATUS'                     
         XC    RDARKSEQ(2),RDARKSEQ                                             
*                                  CLEAR SEQ # AND SUB TYPE                     
         MVC   KEY,RDARKEY         RETRIEVE HIATUS RECORD, IF ANY               
         MVI   RDUPDATE,C'N'       SET UPDATE TO NO                             
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     SAME KEY? THROUGH REC TYPE                   
         BNE   SETH0100            NO  - NO HIATUS RECORD                       
         GOTO1 GETREC              RETRIEVE RECORD                              
         GOTO1 =A(CHKDAREC),DMCB,(RC),(R3),(R5),RR=RELO                         
         BNZ   SETH0100            SOFT/HARD DELETED:  SKIP IT                  
         LA    R3,RDARELEM         SET A(01 ELEMENT)                            
         ZIC   RF,1(R3)                                                         
         AR    R3,RF               BUMP TO X'02' ELEMENT                        
         USING RDARHIE2,R3         SET A(HIATUS COMMENT DATES)                  
         MVI   RDARHIC2,X'25'      RESET ELEMENT CODE TO X'25'                  
         PRINT GEN                                                              
         GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),RCONREC,RDARHIC2,         X        
               =C'ADD=CODE'                                                     
         PRINT NOGEN                                                            
*                                  ADD HIATUS ELEMENT TO CONTRACT               
SETH0100 EQU   *                                                                
         XIT1                                                                   
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CHKDAREC:  CHECKS FOR TYPE OF DARE RECORD.  IF SOFT DELETE                  
*        BIT OR HARD DELETE BIT IS SET, CC IS SET TO NON-ZERO,                  
*        INDICATING THAT RECORD IS TO BE SKIPPED/NOT PROCESSED.                 
*        SOFT DELETE BIT (X'80') WILL ALWAYS BE SET WHEN HARD BIT               
*        (X'40') IS SET.  THEREFORE, ONLY SOFT BIT IS TESTED.                   
*                                                                               
CHKDAREC NMOD1 0,*DARE*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            RESET A(DARE RECORD IN PROCESS)              
         USING RDARREC,R2                                                       
*                                                                               
         L     R5,8(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         MVI   SKIPRECS,C'N'       SET 'SKIP RECORD' TO NO                      
*                                                                               
         CLI   RDARKRT,X'10'       AGENCY ORDER HEADER?                         
         BNE   CDAR0040            NO                                           
         LA    R3,RDARELEM         YES -                                        
         USING RDARELEM,R3                                                      
*                                                                               
         TM    RDARDELS,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0900            NO  - EXIT WITH 'PROCESS RECORD'             
         ZIC   RF,BUYLINE#         YES - BUMP LINE NUMBER                       
         LA    RF,1(RF)                                                         
         STC   RF,BUYLINE#         PUT IT BACK                                  
         B     CDAR0800            EXIT WITH 'SKIP RECORD'                      
         DROP  R3                                                               
CDAR0040 EQU   *                                                                
         CLI   RDARKRT,X'40'       BUY?                                         
         BNE   CDAR0080            NO                                           
         CLI   RDARKSRT,X'00'      BUY HEADER?                                  
         BNE   CDAR0120            NO  - ORB/COMMT/DETAIL                       
*                                                                               
         LA    R3,RDARELEM         YES -                                        
         USING RDARBYEL,R3                                                      
*                                                                               
         MVI   BUYUPDAT,C'Y'       SET WRITE FLAG TO YES                        
         ZIC   RF,RDARBYLN         CHECK LENGTH                                 
         LA    RE,RDARBYOL         SET OLD ELEMENT LENGTH                       
         CR    RF,RE               COMPARE OLD VS NEW                           
         BE    CDAR0900            EQUAL: OLD LENGTH FOUND -                    
*                                     NOT KEY-DELETED:  MUST NOT                
*                                     BE SOFT DELETED.                          
CDAR0050 EQU   *                                                                
         TM    RDARBYDL,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0900            NO  - EXIT WITH 'PROCESS RECORD'             
         MVI   BUYUPDAT,C'N'       YES - SET WRITE FLAG TO NO                   
         B     CDAR0800            EXIT WITH 'SKIP RECORD'                      
         DROP  R3                                                               
CDAR0080 EQU   *                                                                
         BH    CDAR0800            SKIP OVER RECORD TYPES                       
*                                     50 (TRAILER) + 60 (EQUIVS)                
*                                        THESE AREN'T SOFT-DELETED.             
CDAR0120 EQU   *                                                                
*                                                                               
*                                  ONLY BUYS WILL HAVE RDARKSRT                 
*                                     SET - STANDARD AND ORDER                  
*                                        COMMENTS WILL NOT                      
         CLI   RDARKSRT,X'30'      BUY DETAIL?                                  
         BL    CDAR0280            NO  - ORB/COMMT                              
         LA    R3,RDARELEM         YES - CHECK FOR MULTI RATES                  
         USING RDARBDEL,R3                                                      
*                                                                               
         TM    RDARBDDL,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0900            NO  - EXIT WITH 'PROCESS RECORD'             
         MVI   SKIPRECS,C'Y'       YES - SET SKIP RECORD TO YES                 
         B     CDAR0800            FINISHED                                     
         DROP  R3                                                               
*                                                                               
CDAR0280 EQU   *                                                                
*                                                                               
*   REMAINING RECORD TYPES ARE 15, 20, 30, 40/10, 40/20, AND                    
*        SOFT/HARD DELETE BYTE IS IN SAME PLACE IN ALL ELEMENTS.                
*        STANDARD COMMENT ELEMENT FORMAT USED, ARBITRARILY.                     
*                                                                               
         LA    R3,RDARELEM                                                      
         USING RDARELE2,R3                                                      
*                                                                               
         TM    RDARELDL,X'80'      SOFT DELETE SET?                             
         BNO   CDAR0900            NO  - EXIT WITH 'PROCESS RECORD'             
         MVI   SKIPRECS,C'Y'       YES - SET SKIP RECORD TO YES                 
         B     CDAR0800            EXIT WITH 'SKIP RECORD'                      
         DROP  R3                                                               
CDAR0800 EQU   *                                                                
         LTR   RB,RB               SET CC NOT = ZERO                            
         B     CDAR1000                                                         
CDAR0900 EQU   *                                                                
         SR    R0,R0               SET CC = ZERO                                
CDAR1000 EQU   *                                                                
         XIT1                                                                   
         DROP  R2,R5                                                            
         EJECT                                                                  
*                                                                               
*   BUCKUPDT: EITHER DELETE OR ADD BUCKET DOLLARS TO CONTRACT RECORD,           
*        BASED ON BUCKFLAG VALUE.                                               
*                                                                               
BUCKUPDT NMOD1 BUCKWRKX-BUCKWORK,*BUCKUP*,CLEAR=Y                               
         LR    R2,RC               SET A(BUCKWORK AREA)                         
         L     RC,0(R1)            RESET A(WORKAREA)                            
         L     R5,4(R1)            RESET A(MYAREAD)                             
         USING MYAREAD,R5                                                       
*                                                                               
         L     R8,AIO3             SET A(CONTRACT RECORD)                       
         USING RCONREC,R8                                                       
         CLI   BUCKFLAG,X'00'      ADDING NEW FIGURES?                          
         BNE   BUUP0020            NO                                           
         OI    RCONMODR,X'10'      YES - SET 'NOT PENDING/BUY ADDED'            
*                                                                               
         TM    RCONMODR,X'20'      BUYLINE 1 ADDED?                             
         BO    BUUP0020                                                         
         MVC   RCONCREA,BTODAY                                                  
         OI    RCONMODR,X'20'      SET BUYLINE 1 ADDED                          
*                                                                               
BUUP0020 EQU   *                                                                
         CLI   DAILYFLG,C'Y'       DAILY PACING?                                
         BNE   BUUP0040            NO                                           
         OI    BUCKFLGS,X'08'      YES - SET DAILY PACING CALL                  
BUUP0040 EQU   *                                                                
         MVC   DMCB,AIO            A(BUYREC)                                    
         MVC   DMCB(1),BUCKFLAG                                                 
         L     R0,VRECUP                                                        
         GOTOX (RFBUCKUP,REPFACS),DMCB,,(BUCKFLGS,RCONREC),ACOMFACS,   +        
               GETBROAD,(R0),(R2)                                               
         BNE   BUUP0100                                                         
*        BAS   RE,TRUDATE          UPDATE TRUE ACTIVITY DATE                    
         GOTO1 =A(TRUDATE),DMCB,(RC),(R8),RR=RELO                               
         XIT1                                                                   
*                                                                               
BUUP0100 EQU   *                                                                
         LA    R2,CONACTH                                                       
         L     R3,0(R1)                                                         
         GOTO1 GETTXT,DMCB+12,(R3),0,(C'E',DMCB),0,0,0                          
         DC    H'0',C'$ABEND'                                                   
*                                                                               
         XIT1                                                                   
         DROP  R5,R8                                                            
         LTORG                                                                  
         EJECT                                                                  
BUCKWORK DSECT                                                                  
BUCKSPAC DS    2000C                 WORKAREA FOR BUCKUP ROUTINE                
BUCKWRKX EQU   *                                                                
*                                                                               
G8WORKD  DSECT                                                                  
G8SVAIO  DS    A                                                                
G8IO     DS    XL1000                                                           
G8WORKQ  EQU   *-G8WORKD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'236REROM20   04/01/05'                                      
         END                                                                    
