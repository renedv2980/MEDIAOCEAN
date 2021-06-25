*          DATA SET RELFM08    AT LEVEL 218 AS OF 01/09/13                      
*PHASE T80408A                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE MEDGET                                                                 
*INCLUDE GETBROAD                                                               
         PRINT NOGEN                                                            
         TITLE 'RELFM08 (T80408) - REP/FILE REP AND PRODUCT RECORDS'            
*                                                                               
*********************************************************************           
*                                                                   *           
*        RELFM08 (T80408) --- REP AND PRODUCT RECORDS               *           
*                                                                   *           
* ----------------------------------------------------------------- *           
*                                                                   *           
*        THIS OVERLAY WAS CREATED FOR THE REP AND PRODUCT RECORDS   *           
*        BECAUSE THESE TWO RECORDS NEED TO NOW PERFORM VALIDATION   *           
*        OF FIELDS FROM SPOTPAK.  SPOT TO REP SWITCH, ET AL, IS     *           
*        CONTAINED IN THE MODULE.                                   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* JUN17/91 (MRR) --- INITIAL                                        *           
*                                                                   *           
* JUL31/91 (MRR) --- >SPOTPAK CODES STEPED ON BY NETWORK DATA       *           
*                                                                   *           
* DEC23/91 (BU ) --- MAINTENANCE FOR ALT KEY FOR PRODUCT RECORD     *           
*                    X'89' NETWORK CONTRACT # KEY                   *           
*                                                                   *           
* JAN02/92 (BU ) --- FIX BUG IN ALT KEY, WHERE PREVIOUSLY VALID WAS *           
*                    BEING TESTED FOR, BUT NOT SET.  CHANGE TO TEST *           
*                    'ENTERED THIS TIME' BIT.                       *           
*                                                                   *           
* JAN24/92 (BU ) --- INSTALL LOCKOUT TEST TO PROHIBIT UPDATE OF     *           
*                    SPOTPAK INFO IF PRODUCT REFERENCED BY CONTRACT *           
*                                                                   *           
* FEB10/92 (BU ) --- INCLUDE PRODUCT IN USE COUNT ON SCREEN FOR DDS *           
*                    TERMINALS ONLY.  SCREEN RELFMF6 CHANGED TO HAVE*           
*                    NEW FIELD.                                     *           
*                                                                   *           
* MAR23/92 (SKU) --- FIX BUG TO ALLOW TO EDIT FIELDS THAT ARE NOT   *           
*                    SPOTPAK DATA                                   *           
*                                                                   *           
* MAR09/94 (BU ) --- ADD DEVSAL/DEVTYP TO MASTER FILE ACCESS LIST   *           
*                                                                   *           
* FEB23/95 (BU ) --- DROP REP/OFF/SPER SHARES                       *           
*                                                                   *           
* FEB27/95 (BU ) --- FIX BUG IN ABOVE CHANGE                        *           
*                                                                   *           
* MAR24/95 (BU ) --- PRODUCT SPOTPAK FIELDS:  CHECK FOR 3 INSTEAD   *           
*                    OF 5 BECAUSE SPLIT FIELDS REMOVED.             *           
*                                                                   *           
* JUL25/96 (SKU) --- NETWORK CONTRACT NUMBER VALIDATION BUG FIX     *           
*                                                                   *           
* DEC05/96 (RHV) --- HANDLE REP LOGO FILENAME FIELD                 *           
*                                                                   *           
* DEC23/96 (BU ) --- CATEGORY PRIORITY SETTING                      *           
*                                                                   *           
* JAN30/97 (DBU) --- ADD AGENCY CODE AND FLIGHT DATES               *           
*                                                                   *           
* APR07/00 (BU ) --- ADD PROFILE ELEMENT TO NEW REP RECORD          *           
*                                                                   *           
* NOV16/01 (RHV) --- LAST UPDATE FIELD                              *           
*                                                                   *           
* FEB14/02 (BU ) --- REDISPLAY OF SPOTPAK CODES NOT ON FILE         *           
*                                                                   *           
* FEB19/02 (BU ) --- PERMIT RMR TO CHANGE SPOTPAK CODES             *           
*                                                                   *           
* MAY19/03 (BU ) --- FIX 'ADD PROD' BUG                             *           
*                                                                   *           
* NOV05/03 (BU ) --- PRODUCT FLIGHT DATES VS MANDATORY PROFILES     *           
*                                                                   *           
* MAY13/04 (BU ) --- LOCAL INVOICE FLAG                             *           
*                                                                   *           
* OCT07/04 (BU ) --- PRODUCT:  'NOT ON FILE' WHEN AGY IN FILTER     *           
*                    NOT ON FILE                                    *           
*                                                                   *           
* FEB01/07 (BU ) --- URL PROCESSING: EXPAND FIELD TO 72 CHARS       *           
*                                                                   *           
* FEB01/07 (BU ) --- SECOND SCREEN FOR 'REPURL' ACTION              *           
*                                                                   *           
* MAY16/07 (BU ) --- EZPOST URL: MAX LENGTH NOT CHECKED ANY LONGER  *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
T80408   CSECT                                                                  
         NMOD1 0,T80408,R7,R9,R8,RR=R3                                          
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T804FFD,RA                                                       
*                                                                               
         ST    R3,RELO             SAVE RELOCATION FACTOR                       
*                                                                               
         MVC   KEY,BKEY                                                         
         MVI   KEY+27,0                                                         
         MVC   KEY+28(4),BSVDA                                                  
*                                                                               
         LA    R2,LFMLAST          POINT TO FIRST TITLE                         
*                                                                               
*- IF THIS IS FORMAT MODE, BLANK OUT SCREEN FIRST.                              
         CLI   BFMTSW,0                                                         
         BNE   INIT100                                                          
*                                                                               
         XC    DMCB(24),DMCB       0 PARAMS                                     
         GOTO1 RECLRFLD,DMCB,(R2)                                               
*                                                                               
INIT100  EQU   *                                                                
         CLI   BREC,1                                                           
         BE    REP                                                              
         CLI   BREC,9                                                           
         BE    PRODDISP                                                         
         CLI   BREC,X'17'                                                       
         BNE   INIT120                                                          
         GOTO1 =A(URLPROC),RR=Y                                                 
         CLI   BFMTSW,0            TEST FORMAT OR EDIT                          
         BE    EXXMOD              FORMAT - FINISHED                            
         B     FLFILE                                                           
INIT120  EQU   *                                                                
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*         REP RECORDS                                                           
*   - CHECK FOR 'PROF' RECORD -- AN ELEMENT ON THE REP RECORD                   
*                                                                               
REP      EQU   *                                                                
         CLI   BFMTSW,0            TEST FORMAT OR EDIT                          
         BNE   REPEDT                                                           
* FORMAT ROUTINE                                                                
*                                                                               
         XC    RREPREC(256),RREPREC                                             
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         LA    R2,RPMNMH                                                        
         MVC   8(L'RREPNAME,R2),RREPNAME                                        
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,RPMADH                                                        
         MVC   8(L'RREPADDR,R2),RREPADDR                                        
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,RPMSNH                                                        
         MVC   8(L'RREPSHRT,R2),RREPSHRT                                        
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,RPMABH                                                        
         MVC   8(L'RREPABBR,R2),RREPABBR                                        
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,RPMSPLMH         TO SPL MONTH                                 
         MVC   8(7,R2),=8C' '                                                   
         OC    RREPSPLM,RREPSPLM                                                
         BZ    RDIS0020            NO OVERRIDE                                  
         GOTO1 VDATCON,DMCB,(3,RREPSPLM),(6,8(R2))                              
RDIS0020 FOUT  (R2)                                                             
*                                                                               
         LA    R2,RPMEFFDH                                                      
         MVC   8(8,R2),=8C' '                                                   
         OC    RREPSPLE,RREPSPLE                                                
         BZ    RDIS0040                                                         
         GOTO1 VDATCON,DMCB,(3,RREPSPLE),(8,8(R2))                              
RDIS0040 FOUT  (R2)                                                             
*                                                                               
         LA    R2,RPMPARH                                                       
         MVC   8(2,R2),RREPPAR                                                  
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,RPMFISMH         FISCAL START MONTH                           
         OC    RREPFMON,RREPFMON                                                
         BZ    RDIS0100                                                         
         LA    R4,MONTHS                                                        
RDIS0060 CLC   0(1,R4),RREPFMON                                                 
         BE    RDIS0080                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R4,4(R4)                                                         
         B     RDIS0060                                                         
RDIS0080 MVC   8(3,R2),1(R4)                                                    
         FOUT  (R2)                                                             
         SPACE 1                                                                
*                                                                               
*- DISPLAY 30 PROFILES                                                          
RDIS0100 LA    R4,RREPPROF                                                      
         LA    R1,30               LOOP COUNTER                                 
         LA    R2,RPMPROFH                                                      
RDIS0120 EQU   *                                                                
         MVC   8(1,R2),0(R4)                                                    
         FOUT  (R2)                                                             
         LA    R4,1(R4)                                                         
         BAS   RE,NEXTUF                                                        
         BCT   R1,RDIS0120                                                      
         SPACE 1                                                                
RDIS0140 EQU   *                                                                
         XC    KEY2(32),KEY2                                                    
         CLC   RREPMAST(2),=X'0000'                                             
         BE    RDIS0160                                                         
         CLC   RREPMAST(2),=C'  '                                               
         BE    RDIS0160                                                         
         CLC   RREPMAST(2),=X'FFFF'                                             
         BE    RDIS0160                                                         
         FOUT  RPMMRPH,RREPMAST,2                                               
         MVC   KEY2(32),KEY                                                     
         XC    KEY(32),KEY                                                      
         MVI   KEY+00,X'01'                                                     
         MVC   KEY+25(2),RREPMAST                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RDIS0160                                                         
         L     R6,AIOAREA                                                       
         LA    R0,REC2                                                          
         ST    R0,AIOAREA                                                       
         GOTO1 GETREC                                                           
         ST    R6,AIOAREA                                                       
         FOUT  RPMMRPXH,REC2+(RREPSHRT-RREPREC),20                              
         MVC   KEY(32),KEY2                                                     
         GOTO1 HIGH                                                             
         XC    KEY2(32),KEY2                                                    
         B     RDIS0180                                                         
RDIS0160 EQU   *                                                                
         FOUT  RPMMRPH,SPACES,2                                                 
         FOUT  RPMMRPXH,SPACES,20                                               
         CLI   KEY2,0                                                           
         BE    RDIS0180                                                         
         MVC   KEY(32),KEY2                                                     
         XC    KEY2(32),KEY2                                                    
         GOTO1 HIGH                                                             
RDIS0180 EQU   *                                                                
*                                                                               
RDIS0200 EQU   *                                                                
         CLI   RREPSCDE,X'02'             SUBSIDIARY REP LIST?                  
         BNE   RDIS0240                                                         
         LA    R2,RPMSRP1H                                                      
         LA    R3,RREPSCOD                                                      
         ZIC   R4,RREPSCNT                                                      
RDIS0220 EQU   *                                                                
         MVC   8(2,R2),0(R3)                                                    
         FOUT  (R2)                                                             
         LA    R3,2(R3)                                                         
         BAS   RE,NEXTUF                                                        
         BCT   R4,RDIS0220                                                      
RDIS0240 EQU   *                                                                
*                                                                               
*- SPECIAL ADJUSTMENT FOR FOUL X'01' ELEMENT LENGTH ON OLD RECORDS              
RDIS0260 EQU   *                                                                
         LA    R3,RREPCODE                                                      
*                                                                               
         CLI   1(R3),X'90'                                                      
         BNE   RDIS0280            LENGTH IS OK                                 
         MVI   1(R3),X'91'         CORRECT ELEMENT LENGTH                       
*                                                                               
RDIS0280 EQU   *                                                                
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    RDIS0380                                                         
         CLI   0(R3),3                                                          
         BE    RDIS0300                                                         
         CLI   0(R3),5             SPOTPAK INTERFACE CODES                      
         BE    RDIS0340                                                         
         BNE   RDIS0280                                                         
*                                                                               
RDIS0300 EQU   *                                                                
         LA    R2,RPMAAGYH                                                      
         LA    R4,2(R3)                                                         
         LA    R5,12               # ACCESS CODES TO DISPLAY                    
RDIS0320 EQU   *                                                                
         MVC   8(1,R2),0(R4)                                                    
         FOUT  (R2)                                                             
         LA    R4,1(R4)                                                         
         BAS   RE,NEXTUF                                                        
         BCT   R5,RDIS0320                                                      
         B     RDIS0280             LOOK FOR A DIFFERENT EL                     
*                                                                               
RDIS0340 EQU   *                                                                
         LA    R2,RPMSPAGH                                                      
         MVC   8(2,R2),2(R3)                                                    
         MVI   5(R2),2                                                          
         FOUT  (R2)                                                             
         GOTO1 SPOTCT,DMCB,RPMSPAG                                              
         BNZ   MYERR                                                            
         BAS   RE,SWISPOT                                                       
         BNZ   MYERR                                                            
         BAS   RE,SPOTAGY                                                       
         BNZ   MYERR                                                            
*                                                                               
         LA    R2,RPMSPCNH                                                      
         MVC   8(8,R2),SPACES                                                   
         OC    4(4,R3),4(R3)                                                    
         BZ    RDIS0360                                                         
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),4(4,R3)                                                  
         EDIT  (P5,WORK),(8,8(R2)),ALIGN=LEFT                                   
RDIS0360 EQU   *                                                                
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,RPMSPMH                                                       
         MVC   8(1,R2),8(R3)                                                    
         MVI   5(R2),1                                                          
         FOUT  (R2)                                                             
         GOTO1 SPOTMED,DMCB,RPMSPM,RPMSPAG,WORK2+950                            
         BNZ   MYERR                                                            
         MVC   RPMSPMX(10),WORK2+951                                            
         FOUT  RPMSPMXH                                                         
         BAS   RE,SWIREP                                                        
         BNZ   MYERR                                                            
RDIS0380 EQU   *                   GET LOGO FILENAME FROM '06' ELEM             
         LA    R2,RPMLOGOH                                                      
         XC    RPMLOGO,RPMLOGO                                                  
         FOUT  (R2)                                                             
         GOTO1 VGETEL,DMCB,(X'06',IOAREA),DMCB+4                                
         CLI   DMCB,X'FF'                                                       
         BE    RDIS0400            NO ELEMENT FOUND                             
         L     R6,DMCB+4           A(ELEM)                                      
         MVC   RPMLOGO,2(R6)                                                    
*                                                                               
RDIS0400 EQU   *                                                                
         LA    R2,RPMILOCH                                                      
         MVI   RPMILOC,C'N'                                                     
         TM    RREPFLGS,X'80'      LOCAL INVOICE SET?                           
         BNO   RDIS0420            NO                                           
         MVI   RPMILOC,C'Y'        YES - SET FLAG                               
RDIS0420 EQU   *                                                                
         OI    RPMILOCH+6,X'80'    TURN ON TRANSMIT BIT                         
         FOUT  (R2)                                                             
*                                                                               
*                                                                               
         LR    R1,RA                                                            
*                                                                               
         USING TWAD,R1                                                          
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   RDIS0480            NO  - NO DISPLAY                             
*                                                                               
         DROP  R1                                                               
*                                                                               
         LA    R2,RPMURLH          LOOK FOR URL ELEMENT                         
         MVC   RPMURL,SPACES       CLEAR URL LINE                               
         GOTO1 VGETEL,DMCB,(X'11',IOAREA),DMCB+4                                
         CLI   DMCB,X'FF'                                                       
         BE    RDIS0480            NO ELEMENT FOUND                             
         L     R6,DMCB+4           A(ELEM)                                      
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         BCTR  RF,0                BACK OFF 1 FOR EX                            
         EX    RF,RDIS0440                                                      
         B     RDIS0460                                                         
RDIS0440 EQU   *                                                                
         MVC   RPMURL,2(R6)        MOVE URL BY LENGTH                           
*                                                                               
RDIS0460 EQU   *                                                                
         OI    RPMURLH+6,X'80'     TURN ON TRANSMIT BIT                         
         FOUT  (R2)                                                             
RDIS0480 EQU   *                                                                
         B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
*        EDIT REP REC DATA                                                      
*                                                                               
REPEDT   EQU   *                                                                
         MVC   REC+34(2),=X'0191'                                               
         MVC   REC+27(2),=Y(179)                                                
         MVC   RREPPROF,ZEROS                                                   
         XC    RREPSPLM(6),RREPSPLM                                             
*                                                                               
         LA    R3,REC                                                           
         AH    R3,=Y(179)                                                       
         CLI   0(R3),X'02'                                                      
         BNE   REPE05                                                           
         GOTO1 VRECUP,DMCB,(C'R',REC),(R3)                                      
REPE05   EQU   *                                                                
         LA    R3,REC                                                           
         AH    R3,=Y(179)                                                       
         CLI   0(R3),X'03'                                                      
         BNE   REPE06                                                           
         GOTO1 VRECUP,DMCB,(C'R',REC),(R3)                                      
REPE06   EQU   *                                                                
*                                                                               
         LA    R2,RPMNMH                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RREPNAME,WORK                                                    
*                                                                               
         LA    R2,RPMADH                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RREPADDR,WORK                                                    
*                                                                               
         LA    R2,RPMSNH                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RREPSHRT,WORK                                                    
*                                                                               
         LA    R2,RPMABH                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RREPABBR,WORK                                                    
*                                                                               
         LA    R2,RPMSPLMH              SPL MONTH                               
         CLI   5(R2),0                                                          
         BE    REPE10                                                           
         GOTO1 VDATVAL,DMCB,(2,8(R2)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    FLERR2                                                           
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 VDATCON,DMCB,(0,WORK),(3,RREPSPLM)                               
*                                                                               
REPE10   LA    R2,RPMEFFDH              LAST EFFECTIVE DATE                     
         OC    RREPSPLM,RREPSPLM                                                
         BZ    REPE30                                                           
         GOTO1 VDATCON,DMCB,(3,TODAY),(0,WORK)                                  
         CLI   5(R2),0                                                          
         BE    REPE20                                                           
         GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    FLERR2                                                           
*                                                                               
REPE20   GOTO1 VDATCON,DMCB,(0,WORK),(3,RREPSPLE)                               
*                                                                               
REPE30   MVC   8(8,R2),SPACES                                                   
         OC    RREPSPLE,RREPSPLE                                                
         BZ    REPE40                                                           
         GOTO1 VDATCON,DMCB,(3,RREPSPLE),(8,8(R2))                              
REPE40   FOUT  (R2)                                                             
*                                                                               
         LA    R2,RPMPARH                                                       
         MVC   RREPPAR,8(R2)                                                    
*                                                                               
         LA    R2,RPMFISMH         FISCAL START MONTH                           
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
*                                                                               
         LA    R4,MONTHS                                                        
REPE50   CLI   0(R4),X'FF'                                                      
         BE    FLERR2                                                           
         CLC   1(3,R4),8(R2)                                                    
         BE    REPE60                                                           
         LA    R4,4(R4)                                                         
         B     REPE50                                                           
REPE60   MVC   RREPFMON(1),0(R4)                                                
         SPACE 1                                                                
*                                                                               
*- EDIT 30 PROFILES.  DEFAULT TO C'0' IF NOTHING ENTERED                        
         LA    R4,RREPPROF                                                      
         LA    R2,RPMPROFH                                                      
         LA    R1,30                                                            
REPE70   EQU   *                                                                
         MVI   0(R4),C'0'          SEED DEFAULT                                 
         CLI   5(R2),0                                                          
         BE    REPE75              DEFAULT TO C'0'                              
         MVC   0(1,R4),8(R2)                                                    
REPE75   LA    R4,1(R4)                                                         
         BAS   RE,NEXTUF           NEXT INPUT FIELD                             
         BCT   R1,REPE70                                                        
*                                                                               
REPE80   EQU   *                                                                
         MVC   RREPMAST,=X'0000'                                                
         FOUT  RPMMRPXH,SPACES,20                                               
         LA    R2,RPMMRPH                                                       
         CLI   5(R2),0                                                          
         BE    REPE90                                                           
         MVC   KEY2(32),KEY                                                     
         XC    KEY(32),KEY                                                      
         MVI   KEY+00,X'01'                                                     
         MVC   KEY+25(2),8(R2)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   FLERR2                                                           
*                                                                               
*- PREVENT PUTTING YOUR OWN REP CODE IN AS MASTER REP                           
         CLC   8(2,R2),LFMKEY                                                   
         BE    FLERR4                                                           
*                                                                               
         MVC   RREPMAST(2),8(R2)                                                
         L     R6,AIOAREA                                                       
         LA    R0,REC2                                                          
         ST    R0,AIOAREA                                                       
         GOTO1 GETREC                                                           
         ST    R6,AIOAREA                                                       
         FOUT  RPMMRPXH,REC2+(RREPSHRT-RREPREC),20                              
         MVC   KEY(32),KEY2                                                     
         GOTO1 HIGH                                                             
         XC    KEY2(32),KEY2                                                    
*                                                                               
REPE90   EQU   *                                                                
         XC    WORK,WORK           PROCESS SUBSIDIARY REP LIST                  
         LA    R2,RPMSRP1H                                                      
         LA    R3,WORK                                                          
         MVI   0(R3),X'02'                                                      
         MVI   1(R3),10                                                         
         LA    R4,10(R3)                                                        
         LA    R5,15                                                            
REPE91   EQU   *                                                                
         CLI   5(R2),0                                                          
         BE    REPE92                                                           
*                                                                               
*- PREVENT PUTTING YOUR OWN REP CODE IN AS SUBSIDIARY                           
         CLC   8(2,R2),LFMKEY                                                   
         BE    FLERR4                                                           
*                                                                               
         MVC   0(2,R4),8(R2)                                                    
         LA    R4,2(R4)                                                         
         ZIC   RF,1(R3)                                                         
         LA    RF,2(RF)                                                         
         STC   RF,1(R3)                                                         
         ZIC   RF,2(R3)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,2(R3)                                                         
REPE92   EQU   *                                                                
         BAS   RE,NEXTUF                                                        
         BCT   R5,REPE91                                                        
         CLI   WORK+1,10                                                        
         BE    REPE99              NO SUBSIDIARIES                              
*                                                                               
*- CAN'T BE BOTH MASTER & SUBSIDIARY                                            
         LA    R2,RPMMRPH                                                       
         CLC   RREPMAST(2),=X'0000'                                             
         BNE   FLERR5              MASTER ENTERED. ERROR.                       
*                                                                               
*- MARK THIS REP AS THE MASTER                                                  
         MVC   RREPMAST(2),=H'-1'                                               
*                                                                               
         LA    R3,REC                                                           
         SR    R0,R0                                                            
         ICM   R0,3,RREPLEN                                                     
         AR    R3,R0                                                            
         GOTO1 VRECUP,DMCB,(C'R',REC),WORK,(R3)                                 
REPE99   EQU   *                                                                
*                                                                               
*- MASTER FILE ACCESS ONLY APPLIES TO MASTER REP.                               
*  IGNORE INPUT FOR SUBSIDIARY/OTHER REPS                                       
REPE100  EQU   *                                                                
         CLC   RREPMAST(2),=H'-1'                                               
         BNE   REPE150             NOT THE MASTER                               
*                                                                               
         XC    WORK,WORK           PROCESS MASTER FILE ACCESS LIST              
         LA    R2,RPMAAGYH                                                      
         LA    R3,WORK                                                          
         MVI   0(R3),X'03'                                                      
         MVI   1(R3),16                                                         
         LA    R3,2(R3)                                                         
         LA    R5,12               LOOP 12 TIMES                                
REPE101  EQU   *                                                                
         CLI   5(R2),0                                                          
         BE    FLERR2              FIELD MUST HAVE INPUT                        
         CLI   8(R2),C'Y'          FULL ACCESS                                  
         BE    REPE102                                                          
         CLI   8(R2),C'D'          DISPLAY ONLY ACCESS                          
         BE    REPE102                                                          
         CLI   8(R2),C'N'          NO ACCESS                                    
         BNE   FLERR2              INPUT CAN ONLY BE 'Y', 'D' OR 'N'            
REPE102  EQU   *                                                                
         MVC   0(1,R3),8(R2)                                                    
         LA    R3,1(R3)                                                         
         BAS   RE,NEXTUF                                                        
         BCT   R5,REPE101                                                       
*                                                                               
         LA    R3,REC                                                           
         SR    R0,R0                                                            
         ICM   R0,3,RREPLEN                                                     
         AR    R3,R0                                                            
         GOTO1 VRECUP,DMCB,(C'R',REC),WORK,(R3)                                 
*                                                                               
*- PROCESS SPOTPAK CODES                                                        
REPE150  EQU   *                                                                
         LA    R2,RPMSPAGH                                                      
         CLI   5(R2),0                                                          
         BNE   REPE160                                                          
         LA    R2,RPMSPCNH                                                      
         CLI   5(R2),0                                                          
         BNE   REPE151                                                          
         LA    R2,RPMSPMH                                                       
         CLI   5(R2),0                                                          
         BE    REPE200                                                          
REPE151  EQU   *                                                                
         MVC   LFMMSG(L'NEEDAGY),NEEDAGY                                        
         B     MYERR                                                            
REPE160  EQU   *                                                                
         XC    WORK(20),WORK                                                    
         MVC   WORK(2),=X'0514'             ELEMENT ID/LEN                      
         MVC   WORK+2(2),8(R2)              LOAD SPOTPAK AGENCY ID              
*                                                                               
         GOTO1 SPOTCT,DMCB,RPMSPAG                                              
         BNZ   MYERR                                                            
         BAS   RE,SWISPOT                                                       
         BNZ   MYERR                                                            
         BAS   RE,SPOTAGY                                                       
         BNZ   MYERR                                                            
*                                                                               
         LA    R2,RPMSPCNH               FIRST CONTRACT NUMBER FOR SPOT         
         CLI   5(R2),0                                                          
         BE    REPE170                                                          
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    ERROR               LENGTH ERROR                                 
         TM    4(R2),X'08'                                                      
         BZ    ERROR               NON-NUMERIC                                  
         BCTR  R1,0                                                             
         EX    R1,VARPACK                                                       
         SRP   DUB+3(5),1,0                                                     
         MVC   WORK+4(4),DUB+3                                                  
REPE170  EQU   *                                                                
         LA    R2,RPMSPMH                                                       
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         GOTO1 SPOTMED,DMCB,RPMSPM,RPMSPAG,WORK2+950                            
         BNZ   MYERR                                                            
         MVC   WORK+8(1),RPMSPM                                                 
         MVC   RPMSPMX(10),WORK2+951                                            
         FOUT  RPMSPMXH                                                         
REPE190  EQU   *                                                                
         BAS   RE,SWIREP                                                        
         BNZ   MYERR                                                            
         LA    R3,REC                                                           
         SR    R0,R0                                                            
         ICM   R0,3,RREPLEN                                                     
         AR    R3,R0                                                            
         GOTO1 VRECUP,DMCB,(C'R',REC),WORK,(R3)                                 
*                                                                               
*- PRESERVE PROGRAM RESTRICTION ELEMENT ON CHANGE.                              
*                                                                               
REPE200  EQU   *                                                                
         CLI   BACT,C'A'           ADD?                                         
         BE    REPE210                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),RREPKEY                                                  
         BAS   RE,READ                                                          
*                                                                               
         L     R3,AIOAREA          READ OLD REC INTO REC2                       
         LA    RF,REC2                                                          
         ST    RF,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         ST    R3,AIOAREA                                                       
*                                                                               
         GOTO1 VGETEL,DMCB,(X'04',REC2),DMCB+4                                  
         CLI   DMCB,X'FF'                                                       
         BE    REPE210             NO ELEMENT FOUND                             
*                                                                               
         L     R3,DMCB+4           A(ELEM)                                      
         CLI   1(R3),10                                                         
         BE    REPE20              THROW OUT OLD (BAD) ELEMS                    
         GOTO1 VADDELEM,DMCB,RREPREC,(R3)                                       
*                                                                               
* VALIDATE LOGO FILENAME FIELD & UPDATE/CREATE/DELETE X'06' ELEM                
*                                                                               
REPE210  EQU   *                                                                
         CLI   RPMLOGOH+5,0                                                     
         BNE   REPE220                                                          
         GOTO1 VGETEL,DMCB,(X'06',REC),DMCB+4                                   
         CLI   DMCB,X'FF'                                                       
         BE    REPE250             NO ELEMENT FOUND                             
         L     R6,DMCB+4                                                        
         GOTO1 VRECUP,DMCB,(C'R',REC),(R6),REC  DELETE ELEM                     
         B     REPE250                                                          
REPE220  EQU   *                                                                
         LA    R3,637              INVALID LOGO FILENAME MSG                    
         CLI   RPMLOGOH+5,12                                                    
         BNE   LOGOERR                                                          
         CLC   RPMLOGO+8(4),=C'.PCX'                                            
         BNE   LOGOERR                                                          
         B     REPE225                                                          
LOGOERR  EQU   *                                                                
         OI    RPMLOGOH+6,X'40'                                                 
         GOTO1 VGETTXT,DMCB,637,0,(C'E',0),0,X'44',0                            
         XMOD1  2                                                               
REPE225  EQU   *                                                                
         GOTO1 VGETEL,DMCB,(X'06',REC),DMCB+4                                   
         CLI   DMCB,X'FF'                                                       
         BE    REPE230             NO ELEMENT FOUND                             
         L     R6,DMCB+4                                                        
         MVC   2(12,R6),RPMLOGO                                                 
         B     REPE250                                                          
REPE230  EQU   *                                                                
         XC    WORK,WORK                                                        
         MVC   WORK(2),=X'060E'                                                 
         MVC   WORK+2(12),RPMLOGO                                               
         LA    R6,REC                                                           
         ZICM  R0,27(R6),2                                                      
         AR    R6,R0                                                            
         GOTO1 VRECUP,DMCB,(C'R',REC),WORK,(R6)                                 
REPE250  EQU   *                                                                
         NI    RREPFLGS,X'FF'-X'80' RESET 'INVOICE LOCAL' FLAG                  
         LA    R2,RPMILOCH                                                      
         LA    R3,2                SET ERROR MESSAGE                            
         CLI   RPMILOCH+5,0        ANY INVOICE LOCAL FLAG?                      
         BE    REPE270             NO  - ACCEPT AS 'NO'                         
         CLI   RPMILOC,C'Y'        ENTERED AS 'YES'?                            
         BNE   REPE260             NO                                           
         OI    RREPFLGS,X'80'      YES - SET FLAG TO 'YES'                      
         B     REPE270                                                          
REPE260  EQU   *                                                                
         CLI   RPMILOC,C'N'        ENTERED AS 'NO'?                             
         BNE   ERROR                                                            
REPE270  EQU   *                                                                
***>>>  URL PROCESSING                                                          
*                                                                               
*                                                                               
         LR    R1,RA                                                            
*                                                                               
         USING TWAD,R1                                                          
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   REPEX               NO  - NO EDIT                                
*                                                                               
*   MAY HAVE TO REINSERT EXISTING DATA                                          
*                                                                               
         DROP  R1                                                               
*                                                                               
         LA    R6,RPMURLH                                                       
*                                                                               
*   THIS IS AN UNEDITED FIELD, AND IS THE RESPONSIBILITY OF THE USER.           
*                                                                               
         CLI   5(R6),0             ANY DATA IN FIELD?                           
         BE    REPEX               NO  - NOTHING TO INPUT                       
*                                                                               
         XC    URLSTORE,URLSTORE                                                
         MVI   URLSTORE,X'11'      INSERT ELEMENT TYPE                          
         ZIC   RF,5(R6)            GET DATA LENGTH                              
         LA    RF,2(RF)            SET LENGTH FOR ELEMENT                       
         STC   RF,URLSTORE+1                                                    
         SH    RF,=H'3'            SET UP FOR MOVE BY LENGTH                    
         EX    RF,REPE272          MOVE BY LENGTH                               
         B     REPE280                                                          
REPE272  EQU   *                                                                
         MVC   URLSTORE+2(0),8(R6) MOVE URL BY LENGTH                           
REPE280  EQU   *                                                                
*                                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CHELLO,DMCB,(C'P',REPFILE),REC,URLSTORE                          
*                                  ADD ELEMENT TO RECORD                        
         DROP  R4                                                               
***>>>  URL PROCESSING                                                          
*                                                                               
REPEX    EQU   *                                                                
         CLI   BACT,C'A'           ADD OF NEW RECORD?                           
         BNE   FLFILE              NO  - CONTINUE                               
         XC    REC2(200),REC2      CLEAR SPACE FOR NEW 04 ELT                   
         MVC   REC2(3),=X'049A0D'  SET CODE/LENGTH/COUNTER                      
*                                  INITIALIZE PROFILES IN RECORD                
         LA    R2,REC2+3           SET A(1ST PROFILE IN RECORD)                 
         LA    RE,13               SET LOOP CONTROL                             
         LA    RF,1                                                             
REPE0300 EQU   *                                                                
         STCM  RF,3,0(R2)          STORE TWO CHARACTER COUNTER                  
         LA    R2,10(R2)           BUMP TO NEXT PROFILE                         
         LA    RF,1(RF)            BUMP TO NEXT COUNT                           
         BCT   RE,REPE0300         DO NEXT                                      
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         GOTO1 CHELLO,DMCB,(C'P',REPFILE),REC,REC2                              
*                                  ADD ELEMENT TO RECORD                        
         DROP  R4                                                               
         B     FLFILE                                                           
*                                                                               
REPFILE  DC    C'REPFILE'                                                       
         DS    H                                                                
*                                                                               
*        PRODUCT RECORD                                                         
*                                                                               
PRODDISP EQU   *                                                                
         CLI   BFMTSW,0            TEST FORMAT OR EDIT                          
         BNE   PRODEDIT                                                         
* FORMAT ROUTINE                                                                
         BAS   RE,GETREC                                                        
*                                                                               
         XC    PRDPCEX,PRDPCEX     CLEAR PRODUCT CATGY EXPANSION                
         FOUT  PRDPCEXH            SET TO TRANSMIT                              
         XC    PRDPCFC,PRDPCFC     CLEAR PRODUCT CATGY MESSAGE                  
         FOUT  PRDPCFCH            SET TO TRANSMIT                              
*                                                                               
         LA    R2,PRDPNAMH                                                      
         MVC   8(L'RPRDNAME,R2),RPRDNAME                                        
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,PRDPCATH                                                      
         MVC   8(L'RPRDCATG,R2),RPRDCATG                                        
         FOUT  (R2)                                                             
*                                                                               
         LR    R1,RA                                                            
*                                                                               
         USING TWAD,R1                                                          
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   PRDI0020            NO  - NO DISPLAY                             
*                                                                               
         DROP  R1                                                               
*                                                                               
         LA    R2,PRDPCNTH         A(PRODUCT IN USE CNTR FIELD)                 
         EDIT  (2,RPRDLOCK),(4,8(R2)),ZERO=BLANK                                
         FOUT  (R2)                                                             
PRDI0020 EQU   *                                                                
*                                                                               
*- CLEAR SPOTPAK CLIENT THRU END OF SCREEN                                      
*                                                                               
         GOTO1 VFOUTBLK,DMCB,PRDSPCH,PRDLAST                                    
         FOUT  PRDSPCNH,SPACES,20                                               
         FOUT  PRDSP1NH,SPACES,20                                               
         FOUT  PRDSP2NH,SPACES,20                                               
         FOUT  PRDSPENH,SPACES,20                                               
*                                                                               
*DISPLAY CONTRACT TYPES                                                         
*                                                                               
         XC    PRDCTP1,PRDCTP1        DISPPLAY FIRST TYPE                       
         LA    R2,PRDCTP1H                                                      
         MVC   PRDCTP1,RPRDCTY1                                                 
         FOUT  PRDCTP1H                                                         
*                                                                               
         XC    PRDCTP2,PRDCTP2        DISPPLAY SECOND TYPE                      
         LA    R2,PRDCTP2H                                                      
         MVC   PRDCTP2,RPRDCTY2                                                 
         FOUT  PRDCTP2H                                                         
*                                                                               
         XC    PRDCTP3,PRDCTP3        DISPPLAY THIRD TYPE                       
         LA    R2,PRDCTP3H                                                      
         MVC   PRDCTP3,RPRDCTY3                                                 
         FOUT  PRDCTP3H                                                         
*                                                                               
         XC    PRDCTP4,PRDCTP4        DISPPLAY FORTH TYPE                       
         LA    R2,PRDCTP4H                                                      
         MVC   PRDCTP4,RPRDCTY4                                                 
         FOUT  PRDCTP4H                                                         
*                                                                               
*- DISPLAY SPOTPAK INTERFACE DATA                                               
*                                                                               
         LA    R6,RPRDELEM                                                      
PRDI0040 EQU   *                                                                
         CLI   0(R6),0                                                          
         BE    PRDI0120                                                         
         CLI   0(R6),X'03'                                                      
         BE    PRDI0060                                                         
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         B     PRDI0040                                                         
*                                                                               
PRDI0060 EQU   *                                                                
*                                             SET UP FOR SPOT                   
         BAS   RE,GETREP                                                        
         BNZ   MYERR                                                            
*                                                                               
         MVC   PRDSPC(3),RPRDSPCL-RPRDSPOT(R6)                                  
         FOUT  PRDSPCH                                                          
         GOTO1 SPOTCLI,DMCB,PRDSPC,PRDSPCN                                      
***>>>   BNZ   MYERR                                                            
         BZ    PRDI0070                                                         
         MVC   PRDSPCN(19),=C'CODE NOT ON SPOTPAK'                              
PRDI0070 EQU   *                                                                
         FOUT  PRDSPCNH                                                         
*                                                                               
         MVC   PRDSP1(3),RPRDSPP1-RPRDSPOT(R6)                                  
         FOUT  PRDSP1H                                                          
         GOTO1 SPOTPRD,DMCB,PRDSP1,PRDSP1N                                      
***>>>   BNZ   MYERR                                                            
         BZ    PRDI0072                                                         
         MVC   PRDSP1N(19),=C'CODE NOT ON SPOTPAK'                              
PRDI0072 EQU   *                                                                
         FOUT  PRDSP1NH                                                         
*                                                                               
         OC    RPRDSPP2-RPRDSPOT(3,R6),RPRDSPP2-RPRDSPOT(R6)                    
         BZ    PRDI0080                                                         
         MVC   PRDSP2(3),RPRDSPP2-RPRDSPOT(R6)                                  
         FOUT  PRDSP2H                                                          
         GOTO1 SPOTPRD,DMCB,PRDSP2,PRDSP2N                                      
***>>>   BNZ   MYERR                                                            
         BZ    PRDI0074                                                         
         MVC   PRDSP2N(19),=C'CODE NOT ON SPOTPAK'                              
PRDI0074 EQU   *                                                                
         FOUT  PRDSP2NH                                                         
PRDI0080 EQU   *                                                                
*                                                                               
         LA    R3,RPRDSPS1-RPRDSPOT(R6)                                         
         OC    0(1,R3),0(R3)                                                    
         BZ    PRDI0100                                                         
         EDIT  (1,0(R3)),(3,PRDSPS1),ZERO=NOBLANK,ALIGN=LEFT                    
         FOUT  PRDSPS1H                                                         
         LA    R3,1(R3)                            BUMP TO NEXT VALUE           
         EDIT  (1,0(R3)),(3,PRDSPS2),ZERO=NOBLANK,ALIGN=LEFT                    
         FOUT  PRDSPS2H                                                         
PRDI0100 EQU   *                                                                
*                                                                               
         LA    R3,RPRDSPES-RPRDSPOT(R6)                                         
         EDIT  (1,0(R3)),(3,PRDSPE),ZERO=NOBLANK,ALIGN=LEFT                     
         FOUT  PRDSPEH                                                          
         GOTO1 SPOTEST,DMCB,PRDSP1,0(R3),PRDSPEN                                
***>>>   BNZ   MYERR                                                            
         BZ    PRDI0102                                                         
         MVC   PRDSP2N(19),=C'CODE NOT ON SPOTPAK'                              
PRDI0102 EQU   *                                                                
         FOUT  PRDSPENH                                                         
         BAS   RE,SWIREP                                                        
*                                                                               
*- DISPLAY NETWORK CONTRACT DATA                                                
*                                                                               
PRDI0120 EQU   *                                                                
*  SET PREVIOUSLY VALID BITS FOR SPOTPAK FIELDS ONLY                            
*                                                                               
         LA    R2,PRDSPCH          SET PREVIOUSLY VALID BIT                     
         OI    4(R2),X'20'                                                      
         LA    R2,PRDSP1H          SET PREVIOUSLY VALID BIT                     
         OI    4(R2),X'20'                                                      
         LA    R2,PRDSP2H          SET PREVIOUSLY VALID BIT                     
         OI    4(R2),X'20'                                                      
         LA    R2,PRDSPS1H         SET PREVIOUSLY VALID BIT                     
         OI    4(R2),X'20'                                                      
         LA    R2,PRDSPS2H         SET PREVIOUSLY VALID BIT                     
         OI    4(R2),X'20'                                                      
         LA    R2,PRDSPEH          SET PREVIOUSLY VALID BIT                     
         OI    4(R2),X'20'                                                      
*                                                                               
         LA    R2,PRDPPEXH                                                      
         MVC   8(L'PRDPPEX,R2),SPACES                                           
         FOUT  (R2)                                                             
*                                                                               
***>>>   CLC   =XL2'FFFF',SVREPMST                                              
***>>>   BNE   PRDI0140            NOT A MASTER                                 
         OC    RPRDNET#,RPRDNET#   NETWORK CONTRACT?                            
         BZ    PRDI0140            NO. MOVE ON.                                 
*                                                                               
         LA    R2,PRDNCNOH                                                      
         MVC   8(L'RPRDNET#,R2),RPRDNET#                                        
         FOUT  (R2)                                                             
*                                                                               
PRDI0140 EQU   *                                                                
         GOTO1 VGETEL,DMCB,(X'02',RPRDREC),DMCB+4                               
         CLI   DMCB,X'FF'                                                       
         BE    PRDI0160            NO ELEMENT?                                  
         L     R3,DMCB+4                                                        
         USING RPRDNELM,R3                                                      
*                                                                               
         LA    R2,PRDNDESH         NET CON DESCRIPTION                          
         MVC   8(L'RPRDNDES,R2),RPRDNDES                                        
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,PRDPPERH         NET CON POINT PERSON                         
         MVC   8(L'RPRDNPNT,R2),RPRDNPNT                                        
         FOUT  (R2)                                                             
*                                                                               
         GOTO1 =A(GETPOINT),RR=Y   POINT PERSON EXPANSION                       
         DROP  R3                                                               
*                                                                               
PRDI0160 EQU   *                                                                
*                                                                               
         XC    PRDAGY(L'PRDAGY),PRDAGY    CLEAR FIELDS                          
         OI    PRDAGYH+6,X'80'                                                  
         XC    PRDAEXP(L'PRDAEXP),PRDAEXP                                       
         OI    PRDAEXPH+6,X'80'                                                 
         XC    PRDDATE(L'PRDDATE),PRDDATE                                       
         OI    PRDDATEH+6,X'80'                                                 
         MVC   SVAGYOFF,SPACES                                                  
         XC    SVDATEF,SVDATEF                                                  
         XC    SVDATET,SVDATET                                                  
*                                                                               
         GOTO1 VGETEL,DMCB,(X'04',RPRDREC),DMCB+4                               
         CLI   DMCB,X'FF'          NO ELEMENT?                                  
         BE    PRDI0175            NO - SKIP IT                                 
         L     R3,DMCB+4                                                        
AGYL     USING RPRDAGFL,R3         AGENCY/FLIGHT ELEMENT                        
*                                                                               
         LA    R2,PRDAGYH          AGENCY CODE                                  
*                                                                               
         OC    AGYL.RPRDAGAG,AGYL.RPRDAGAG   ANY AGENCY?                        
         BZ    PRDI0168            NO - DISPLAY FLIGHT DATES                    
         MVC   8(L'AGYL.RPRDAGAG,R2),AGYL.RPRDAGAG                              
*                                                                               
         CLC   AGYL.RPRDAGAO,SPACES          AGENCY OFFICE?                     
         BE    PRDI0165            NO                                           
         OC    AGYL.RPRDAGAO,SPACES                                             
         MVI   L'AGYL.RPRDAGAG+8(R2),C'-'                                       
         MVC   L'AGYL.RPRDAGAG+9(L'AGYL.RPRDAGAO,R2),AGYL.RPRDAGAO              
*                                                                               
PRDI0165 EQU   *                                                                
         FOUT  (R2)                AGENCY CODE/OFFICE                           
         MVC   SVAGYOFF(7),8(R2)                                                
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'0A'           AGENCY RECORD                                
         MVC   KEY+25(2),REPALPHA  REP CODE                                     
         MVC   KEY+19(4),AGYL.RPRDAGAG  AGENCY                                  
         MVC   KEY+23(2),AGYL.RPRDAGAO  OFFICE                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     HAS AGY BEEN FOUND?                          
         BE    PRDI0164            YES                                          
         MVC   PRDAEXP(20),=C'AGENCY NOT ON FILE  '                             
         OI    PRDAEXPH+6,X'80'    TURN ON TRANSMIT BIT                         
         B     PRDI0168                                                         
*                                                                               
* AGY HAS BEEN FOUND - DISPLAY AGENCY NAME                                      
*                                                                               
PRDI0164 EQU   *                                                                
         L     R5,AIOAREA          SAVE A(CURRENT IO AREA)                      
         LA    R6,REC2             SET A(IOAREA2)                               
         ST    R6,AIOAREA                                                       
         GOTO1 GETREC              RETRIEVE CATEGORY RECORD                     
         ST    R5,AIOAREA          RESET IO AREA                                
         USING RAGYREC,R6          AGENCY RECORD                                
         MVC   PRDAEXP(20),RAGYNAM1   MOVE AGY NAME TO THE SCREEN               
         OI    PRDAEXPH+6,X'80'    TURN ON TRANSMIT BIT                         
         DROP  R6                                                               
*                                                                               
PRDI0168 EQU   *                                                                
         LA    R2,PRDDATEH         FLIGHT DATES                                 
         GOTO1 VDATCON,DMCB,(3,AGYL.RPRDAGDF),(11,8(R2))                        
         MVC   SVDATEF(L'AGYL.RPRDAGDF),AGYL.RPRDAGDF                           
         OC    AGYL.RPRDAGDT,AGYL.RPRDAGDT   DATE TO?                           
         BZ    PRDI0170            NO                                           
         MVI   16(R2),C'-'                                                      
         GOTO1 VDATCON,DMCB,(3,AGYL.RPRDAGDT),(11,17(R2))                       
         MVC   SVDATET(L'AGYL.RPRDAGDT),AGYL.RPRDAGDT                           
*                                                                               
         DROP  AGYL                                                             
*                                                                               
*   RETRIEVE CATEGORY EXPANSION FROM CATEGORY RECORD                            
*                                                                               
PRDI0170 EQU   *                                                                
         FOUT  (R2)                                                             
PRDI0175 EQU   *                                                                
         XC    KEY,KEY                                                          
         MVI   KEY,X'0F'                                                        
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),PRDPCAT   INSERT CATEGORY CODE                         
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                 MUST BE FOUND                                
         DC    H'0'                                                             
         L     R5,AIOAREA          SAVE A(CURRENT IO AREA)                      
         LA    R6,REC2             SET A(IOAREA2)                               
         ST    R6,AIOAREA                                                       
         GOTO1 GETREC              RETRIEVE CATEGORY RECORD                     
         ST    R5,AIOAREA          RESET IO AREA                                
         USING RCTGREC,R6                                                       
         MVC   PRDPCEX,RCTGNAME AMEINSERT NAME ON SCREEN                        
*                                                                               
         DROP  R6                                                               
*                                                                               
         FOUT  PRDPCEXH                                                         
PRDI0180 EQU   *                   DISPLAY LAST UPDATE DATE                     
         XC    PRDUPD,PRDUPD                                                    
         OI    PRDUPDH+6,X'80'                                                  
         OC    RPRDUPD,RPRDUPD                                                  
         BZ    PRDI0190                                                         
         GOTO1 VDATCON,DMCB,(2,RPRDUPD),(11,PRDUPD)                             
PRDI0190 EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
PRODEDIT EQU   *                                                                
         XC    SPOTLOCK,SPOTLOCK                                                
         CLI   BACT,C'A'                                                        
         BE    PRED0020                                                         
         L     R5,AIOAREA          RETRIEVE ORIGINAL PROD RECORD                
         LA    R6,REC2             TO CHECK COUNTER                             
         ST    R6,AIOAREA                                                       
         GOTO1 GETREC                                                           
         ST    R5,AIOAREA          RESET IO AREA                                
         MVC   SPOTLOCK,RPRDLOCK-RPRDREC(R6)    SAVE LOCKOUT CTR                
*                                                                               
*                                                                               
PRED0020 EQU   *                                                                
         MVC   REC+34(2),=X'012E'                                               
         MVC   REC+27(2),=Y(80)                                                 
*                                                                               
         MVC   RPRDLOCK,SPOTLOCK   REINSERT COUNTER                             
*                                                                               
         MVC   RPRDCLSS,SPACES                                                  
         MVC   RPRDCATG,SPACES                                                  
*                                                                               
         LA    R2,PRDPNAMH                                                      
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RPRDNAME,WORK                                                    
*                                                                               
         LA    R2,PRDPCATH                                                      
         CLI   5(R2),0                                                          
         BNE   PRED0030                                                         
         TM    SVPGPBIT+1,X'08'    ADV CATEGORY O'RIDE ON?                      
         BNO   FLERR1              NO  - NEED CATEGORY CODE                     
         XC    RPRDCATG,RPRDCATG   YES - CLEAR CATEGORY CODE IN REC             
         B     PRED0040            LOAD CODE FROM RECORD                        
PRED0030 EQU   *                                                                
         BAS   RE,MOVE                                                          
         MVC   RPRDCATG,WORK                                                    
*                                                                               
PRED0040 EQU   *                                                                
         XC    PRDPCEX,PRDPCEX     CLEAR PRODUCT CATGY EXPANSION                
         FOUT  PRDPCEXH            SET TO TRANSMIT                              
         XC    PRDPCFC,PRDPCFC     CLEAR PRODUCT CATGY MESSAGE                  
         FOUT  PRDPCFCH            SET TO TRANSMIT                              
*                                                                               
*-----CONTRACT TYPE ---------                                                   
* ABOB ADDED                                                                    
*                                                                               
*VALIDATES ENTERED CONTRACT TYPES AGAINST EXISTING CODES                        
         LA    R2,PRDCTP1H         POINT TO FIRST SCREEN FIELD                  
         LA    R4,4                SET COUNTER (NUMBER OF FIELDS)               
*                                                                               
PRED0051 CLI   5(R2),0             CHECK IF ANYTHING ENTERED                    
         BE    PRED0052                                                         
         MVC   ONECHAR,8(R2)       MOVE FOR VALIDATION SUB INPUT                
         GOTO1 =A(VALCTYPE),RR=Y   CALL VALIDATION SUB                          
         BNE   FLERR7                                                           
PRED0052 ZIC   RF,0(R2)           GET SCREEN FEILD LENGTH                       
         AR    R2,RF              BUMP TO NEXT SCREEN FIELD                     
         BCT   R4,PRED0051                                                      
*                                                                               
*VALIDATE CONTRACT TYPES FOR DUPLICATES                                         
*                                                                               
         LA    R3,PRDCTP1H        POINT TO 1ST SCREEN FIELD                     
         LA    R2,PRDCTP2H        POINT TO 2ND SCREEN FIELD                     
         LA    R4,3               SET COUNTER (ONE LESS OF FIELDS)              
*                                                                               
PRED0054 CLI   5(R2),0             CHECK IF ANYTHING ENTERED                    
         BE    PRED0055                                                         
         CR    R2,R3               CHECK POINTERS                               
         BE    PRED0055                                                         
         CLC   8(1,R2),8(R3)       CHEC IF CONTRACT TYPES =                     
         BE    FLERR8                                                           
         ZIC   RF,0(R3)           GET SCREEN FIELD LENGTH                       
         AR    R3,RF              BUMP TO NEXT SCREEN FIELD                     
         B     PRED0054                                                         
PRED0055 LA    R3,PRDCTP1H         RESET POINTER                                
         ZIC   RF,0(R2)          GET SCREEN FIELD                               
         AR    R2,RF             BUMP TO NEXT SCREEN FIELD                      
         BCT   R4,PRED0054                                                      
*                                                                               
*STORE VALID CONTRACT TYPES                                                     
         XC    RPRDCTY1(4),RPRDCTY1   CLEAR STORAGE                             
         LA    R3,RPRDCTY1         POINT TO FIRST REC FIELD                     
         LA    R2,PRDCTP1H         POINT TO FIRST SCREEN FIELD                  
         LA    R4,4                SET COUNTER (NUMBER OF FIELDS)               
*                                                                               
PRED0056 CLI   5(R2),0             CHECK IF ANYTHING ENTERED                    
         BE    PRED0057                                                         
         MVC   0(1,R3),8(R2)      STORE VALID CONTRACT TYPE                     
         LA    R3,1(R3)           BUMP TO NEXT STORAGE                          
         FOUT  (R2)               SET TO TRANSMIT                               
PRED0057 ZIC   RF,0(R2)           GET SCREEN FEILD LENGTH                       
         AR    R2,RF              BUMP TO NEXT SCREEN FIELD                     
         BCT   R4,PRED0056                                                      
*--------------------                                                           
*                                                                               
*   CHECK FILE PROFILE:  IF BYTE2 BIT X'08' ON, OVERRIDE ANY ENTRY              
*        WITH ADVERTISER CATEGORY CODE.                                         
*        KEY CONTAINS PRODUCT KEY AT THIS POINT,FROM WHICH ADV CODE             
*           MAY BE TAKEN.                                                       
*                                                                               
         TM    SVPGPBIT+1,X'08'    ADV CATEGORY O'RIDE ON?                      
         BNO   PRED0080            NO                                           
         XC    WORK,WORK                                                        
         MVC   WORK(27),KEY        SAVE CURRENT VALUE OF KEY                    
         L     R5,AIOAREA          RETRIEVE ADVERTISER RECORD                   
         LA    R6,REC2                TO GET ADVERTISER CATEGORY                
         ST    R6,AIOAREA                                                       
         XC    KEY,KEY             READ FOR ADVERTISER                          
         MVI   KEY,X'08'           SET KEY FOR ADVERTISER                       
         MVC   KEY+21(4),WORK+18   GET ADV CODE FROM PROD KEY SAVED             
         MVC   KEY+25(2),REPALPHA  INSERT REP CODE                              
*                                                                               
         BAS   RE,HIGH                                                          
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    PRED0060                                                         
         LA    R3,NOADVERR                                                      
         LA    R2,LFMKEYH          SET A(CURSOR)                                
         B     ERROR                                                            
*                                     CAN'T BE - ALREADY CHECKED                
PRED0060 EQU   *                                                                
         GOTO1 GETREC              RETRIEVE ADVERT RECORD                       
         ST    R5,AIOAREA          RESET IO AREA                                
         CLC   RPRDCATG,RADVCATG   CODE ENTERED = ADVERTISER'S?                 
         BE    PRED0080            YES - NO RESET, NO MESSAGE                   
         MVC   RPRDCATG,RADVCATG   INSERT ADVERTISER'S CATEGORY INTO            
*                                     PRODUCT RECORD BEING BUILT                
         MVC   PRDPCAT(2),RADVCATG INSERT ADVERTISER'S CATEGORY BACK            
*                                     TO SCREEN                                 
         FOUT  PRDPCATH            SET TO TRANSMIT                              
         MVC   PRDPCFC(17),=C'<-ADVERT CATEGORY'                                
         FOUT  PRDPCFCH            SET TO TRANSMIT                              
*                                                                               
PRED0080 EQU   *                                                                
         XC    KEY,KEY             READ FOR CATEGORY KEY                        
         MVI   KEY,X'0F'                                                        
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),RPRDCATG                                               
*                                                                               
         BAS   RE,HIGH                                                          
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    PRED0100                                                         
         LA    R3,NOCTGERR                                                      
         B     ERROR                                                            
*                                                                               
PRED0100 EQU   *                                                                
*                                                                               
*   RETRIEVE CATEGORY RECORD TO DISPLAY CATEGORY EXPANSION                      
*                                                                               
         L     R5,AIOAREA          SAVE A(CURRENT IO AREA)                      
         LA    R6,REC2             SET A(IOAREA2)                               
         ST    R6,AIOAREA                                                       
         GOTO1 GETREC              RETRIEVE CATEGORY RECORD                     
         ST    R5,AIOAREA          RESET IO AREA                                
         USING RCTGREC,R6                                                       
         MVC   PRDPCEX,RCTGNAME AMEINSERT NAME ON SCREEN                        
*                                                                               
         DROP  R6                                                               
*                                                                               
         FOUT  PRDPCEXH                                                         
*                                                                               
*- AGENCY CODE                                                                  
*                                                                               
         XC    ELEM,ELEM                                                        
         XC    PRDAEXP(L'PRDAEXP),PRDAEXP   CLEAR PRD EXP NAME FLD              
         OI    PRDAEXPH+6,X'80'                                                 
         MVI   ELEM,X'04'                                                       
         MVI   ELEM+1,X'0E'        LENGTH                                       
         LA    R2,PRDAGYH          AGENCY CODE                                  
         CLI   5(R2),0             ANY AGENCY ENTERED?                          
         BE    PRED0125            NO - CHECK FLIGHT DATES                      
*                                                                               
         OC    SPOTLOCK(2),SPOTLOCK   IS COUNTER ZERO?                          
         BZ    PRED0115               YES - ALLOWE CHANGES                      
         OC    8(4,R2),SPACES                                                   
         CLC   SVAGYOFF(7),8(R2)                                                
         BNE   NOCHANGE            CAN'T CHANGE THE FIELD                       
*                                                                               
PRED0115 EQU   *                                                                
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,X'0A'           AGENCY RECORD                                
         MVC   KEY+19(6),SPACES    MOVE SPACES INTO AGY/OFFICE SPOTS            
         MVC   KEY+25(2),REPALPHA  REP CODE                                     
*                                                                               
         XC    BLOCK,BLOCK                                                      
         L     R6,ACOMFACS                                                      
         USING COMFACSD,R6                                                      
         GOTO1 CSCANNER,DMCB,(R2),(1,BLOCK),C',=,-'                             
         DROP  R6                                                               
*                                                                               
         LA    R6,ELEM             BUILD AGY/OFF PART OF DATE VAL ELEM          
         USING RPRDAGFL,R6                                                      
         CLI   BLOCK,4             IS AGY LENGTH BIGGER THAN 4 BYTES?           
         BH    AGYERROR            YES - ERROR                                  
         ZIC   RF,BLOCK            LENGTH OF AGENCY CODE                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   KEY+19(0),BLOCK+12  MOVE AGY CODE INTO KEY                       
         MVC   RPRDAGAG(L'RPRDAGAG),SPACES                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   RPRDAGAG(0),BLOCK+12   MOVE AGENCY INTO THE ELEMENT              
*                                                                               
         MVC   RPRDAGAO(L'RPRDAGAO),SPACES                                      
         CLI   BLOCK+1,0           WAS THERE OFFICE?                            
         BE    PRED0120            NO                                           
         MVC   KEY+23(2),BLOCK+22  MOVE OFFICE INTO THE KEY                     
         MVC   RPRDAGAO(2),BLOCK+22   MOVE OFFICE INTO THE ELEMENT              
*                                                                               
PRED0120 EQU   *                                                                
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     HAS AGY BEEN FOUND?                          
         BNE   AGYERROR            NO - ERROR                                   
*                                                                               
* AGY HAS BEEN FOUND - DISPLAY AGENCY NAME                                      
*                                                                               
         L     R5,AIOAREA          SAVE A(CURRENT IO AREA)                      
         LA    R6,REC2             SET A(IOAREA2)                               
         ST    R6,AIOAREA                                                       
         GOTO1 GETREC              RETRIEVE CATEGORY RECORD                     
         ST    R5,AIOAREA          RESET IO AREA                                
         USING RAGYREC,R6          AGENCY RECORD                                
         MVC   PRDAEXP(20),RAGYNAM1   MOVE AGY NAME TO THE SCREEN               
         OI    PRDAEXPH+6,X'80'    TURN ON TRANSMIT BIT                         
         DROP  R6                                                               
*                                                                               
*                                                                               
* FLIGHT DATES                                                                  
*                                                                               
PRED0125 EQU   *                                                                
         LA    R2,PRDDATEH         FLIGHT DATES FIELD                           
         CLI   5(R2),0             ANY ENTRY?                                   
         BNE   PRED0128            YES                                          
*                                                                               
***      CLI   BACT,C'A'           ADD?                                         
***      BNE   PRED0145            NO  - DON'T CHECK PROFILES                   
*                                                                               
*   CHECK FILE PROFILE:                                                         
*        IF BYTE3 BIT X'10' ON, FLIGHT DATES ARE MANDATORY.                     
*        IF BYTE3 BIT X'08' ON, FLIGHT DATES ARE MANDATORY                      
*              <IF CLI/PROD/EST ARE PRESENT>                                    
*                                                                               
         TM    SVPGPBIT+2,X'10'    FLIGHT DATES MANDATORY?                      
         BO    FLERR9              YES - RETURN AN ERROR                        
         TM    SVPGPBIT+2,X'08'    FLIGHT DATES MANDATORY                       
*                                     IF CLI/PRD/EST ENTERED?                   
         BNO   PRED0145            NO  - NOT REQUIRED: ACCEPTED                 
         CLI   PRDSPCH+5,0         YES - SPOT CLI CODE ENTERED?                 
         BNE   FLERR9              YES - FLIGHT NEEDED                          
         CLI   PRDSP1H+5,0         NO  - SPOT PROD CODE ENTERED?                
         BNE   FLERR9              YES - FLIGHT NEEDED                          
         CLI   PRDSPEH+5,0         NO  - SPOT EST ENTERED?                      
         BNE   FLERR9              YES - FLIGHT NEEDED                          
         B     PRED0145            NO CODES: NOT NEEDED                         
PRED0128 EQU   *                                                                
*                                                                               
* VALIDATE DATES ( ALLOWED FORMAT MMMDD/YY-MMMDD/YY OR                          
* MM/DD/YY-MM/DD/YY )                                                           
*                                                                               
         XC    BLOCK,BLOCK                                                      
         XC    WORK,WORK                                                        
         L     R6,ACOMFACS                                                      
         USING COMFACSD,R6                                                      
         GOTO1 CSCANNER,DMCB,(R2),(2,BLOCK),C',=-='                             
         DROP  R6                                                               
         CLI   DMCB+4,2            IF NOT 2 DATES, ERROR                        
         BNE   FLTERR                                                           
PRED0130 EQU   *                                                                
         GOTO1 VDATVAL,DMCB,(0,BLOCK+12),WORK                                   
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    FLTERR                                                           
*                                                                               
         GOTO1 VDATVAL,DMCB,(0,BLOCK+44),WORK+6                                 
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    FLTERR                                                           
*                                                                               
* CHECK THE DATES, THEY SHOULD BE WITHIN ONE BROADCAST YEAR                     
* CALL PERVAL AGAIN BECAUSE IT WOULD GIVE US THE DATES IN A COUPLE              
* OF DIFFERENT FORMATS (EX: 6 BYTES AND 3 BYTES)                                
*                                                                               
         XC    BLOCK,BLOCK                                                      
         L     R6,ACOMFACS                                                      
         USING COMFACSD,R6                                                      
         GOTO1 CPERVAL,DMCB,(5(R2),8(R2)),BLOCK                                 
         DROP  R6                                                               
         CLI   DMCB+4,0                                                         
         BNE   FLTERR                                                           
PRED0135 EQU   *                                                                
         GOTO1 =V(GETBROAD),DMCB,(1,BLOCK+44),BRDATE1,VGETDAY,VADDAY,  +        
               RR=RELO                                                          
         GOTO1 =V(GETBROAD),DMCB,(1,BLOCK+50),BRDATE2,VGETDAY,VADDAY,  +        
               RR=RELO                                                          
         GOTO1 VDATCON,DMCB,(0,BRDATE1+6),(5,BRDATE)                            
         GOTO1 VDATCON,DMCB,(0,BRDATE2+6),(5,BRDATE+9)                          
         MVI   BRDATE+8,C'-'                                                    
         L     R6,ACOMFACS                                                      
         USING COMFACSD,R6                                                      
         GOTO1 CPERVAL,DMCB,(17,BRDATE),BLOCK1                                  
         DROP  R6                                                               
         CLC   BLOCK1+2(2),=X'0035' MORE THAN 53 WEEKS??                        
         BH    BIGERR              YES                                          
*                                                                               
         OC    SPOTLOCK(2),SPOTLOCK   IS COUNTER ZERO?                          
         BZ    PRED0140               YES - ALLOW CHANGES                       
*                                                                               
* IF THERE ARE DATES IN CONTRACT RECORD AND NONE IN PRODUCT RECORD              
* DO NOT ALLOW TO ENTER PRODUCT DATES                                           
*                                                                               
         OC    SVDATEF,SVDATEF     TRY TO ADD PRD DATES?                        
         BZ    NOCHDT1             YES - BUT CONTRACT EXISTS - ERROR            
         CLC   BLOCK+28(3),SVDATEF    IS NEW START DATE BIGGER?                 
         BH    NOCHDATE               CHANGES ARE NOT ALLOWED                   
         CLC   BLOCK+31(3),SVDATET    IS NEW END DATE BIGGER?                   
         BL    NOCHDATE               CHANGES ARE NOT ALLOWED                   
*                                                                               
PRED0140 EQU   *                                                                
         LA    R6,ELEM             BUILD DATE PART OF DATE VAL ELEM             
         USING RPRDAGFL,R6                                                      
         MVC   RPRDAGDF(3),BLOCK+28   START DATE BINARY                         
         MVC   SVDATEF(3),BLOCK+28    START DATE BINARY                         
         TM    DMCB+4,X'04'           ONLY ONE INPUT?                           
         BO    PRED0145               YES                                       
         MVC   RPRDAGDT(3),BLOCK+31   END DATE BINARY                           
         MVC   SVDATET(3),BLOCK+31    END DATE BINARY                           
         DROP  R6                                                               
*                                                                               
PRED0145 EQU   *                                                                
         GOTO1 VGETEL,DMCB,(X'04',REC),DMCB+4                                   
         CLI   DMCB,X'FF'                                                       
         BE    PRED0150            NO ELEMENT FOUND                             
*                                                                               
         L     R3,DMCB+4           A(ELEM)                                      
         GOTO1 VRECUP,DMCB,(C'R',REC),(R3)     REMOVE AN ELEMENT                
*                                                                               
PRED0150 EQU   *                                                                
         OC    ELEM+2(12),ELEM+2   ANY OFFICE OR FLIGHT DATES?                  
         BZ    PRED0155            NO - DON'T ADD AN ELEMENT                    
         GOTO1 VADDELEM,DMCB,REC,ELEM  ADD AN ELEMENT                           
*                                                                               
*- NETWORK CONTRACT NUMBER                                                      
*  ** NOTE ** NETWORK INFO ONLY ALLOWED FOR MASTER REPS                         
*                                                                               
PRED0155 EQU   *                                                                
         XC    RPRDNET#,RPRDNET#   ASSUME NONE                                  
         LA    R2,PRDNCNOH                                                      
*                                                                               
         LA    R3,MASTONLY         FLD RESERVED FOR MASTER                      
         LA    R4,L'MASTONLY                                                    
*                                                                               
         CLC   =XL2'FFFF',SVREPMST                                              
         BNE   PRED0180            NOT A MASTER - PERMIT FIELD                  
*                                     ENTRY, ACCEPT WHAT IS INPUT               
*                                                                               
         LA    R3,NETCONLY         FLD RESERVED FOR NETWORK CONTRACT            
         LA    R4,L'NETCONLY                                                    
*                                                                               
         CLI   5(R2),0                                                          
         BNE   PRED0180                                                         
         LA    R1,SVPGPBIT         PROGRAM PROFILE BITS                         
         TM    0(R1),X'20'         3RD BIT ON = NET CONT# REQUIRED              
         BNO   PRED0160                                                         
*                                                                               
*  IF BIT ON, NETWORK CONTRACT # IS REQUIRED.  IF PRESENT, DESCRIPT             
*     AND POINT PERSON ARE ALSO REQUIRED (THIS IS AS ALWAYS.)                   
*                                                                               
         LR    R1,RA                                                            
         USING TWAD,R1                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   FLERR1              NO  - TREAT AS ERROR                         
         DROP  R1                                                               
*                                                                               
*  IF DDS TERMINAL, IGNORE THE TEST                                             
*                                                                               
*- NOT A NETWORK PRODUCT.  NO OTHER INPUT ALLOWED ON SCREEN                     
*                                                                               
PRED0160 EQU   *                                                                
*                                                                               
*   WILL PERMIT ADDITIONAL INPUT ON SCREEN FOR MASTER.  SAME AS                 
*     NON-MASTER SETUP.                                                         
*                                                                               
*                                                                               
         B     PRED0180                                                         
*                                                                               
***>>    CLI   5(R2),0                                                          
***>>    BNE   PRED0165                                                         
***>>    BAS   RE,NEXTUF                                                        
***>>    LA    RF,PRDLAST                                                       
***>>    CR    R2,RF                                                            
***>>    BL    PRED0160                                                         
***>>    B     PRED0340            MOVE ON TO SPOTPAK CODES                     
*                                                                               
PRED0165 EQU   *                                                                
         BCTR  R4,0                PUT OUT ERROR MESSAGE                        
         EX    R4,PRED0170                                                      
         B     MYERR                                                            
*                                                                               
PRED0170 MVC   LFMMSG(0),0(R3)     R3 = A(MSG TEXT)                             
         SPACE 2                                                                
PRED0180 EQU   *                                                                
*                                                                               
*   REVISED PROCESSING FOR 'NETWORK CON#/DESCRIPTION/POINT PERSON:'             
*     0. MASTER:  WILL BUILD ELEMENT WITH MINIMUM OF DESC/PP                    
*        (ITEM 1 IS NOW INVALID).                                               
****>>1. MASTER:  REQUIRES THAT ALL THREE FIELDS BE PRESENT                     
*     2. NON-MASTER:  WILL BUILD AN ELEMENT FOR NETWORK CON#/DES/PP             
*        REQUIRING ONLY PP.  IF PP IS NOT INPUT, NO ELEMENT IS ADDED            
*        TO THE RECORD.  OTHER FIELDS MAY BE BLANK.                             
*                                                                               
****     CLC   =XL2'FFFF',SVREPMST                                              
****     BE    PRED0200            MASTER                                       
         LA    R2,PRDNCNOH         NOT MASTER - ANY DATA IN FIELD?              
         CLI   5(R2),0                                                          
         BE    PRED0220            NO  - SKIP OVER IT                           
*                                  YES - MUST CONFORM TO STANDARDS              
PRED0200 EQU   *                                                                
         MVC   RPRDNET#,8(R2)      PICK UP NETWORK CON #                        
         OC    RPRDNET#,SPACES                                                  
*                                                                               
         TM    4(R2),X'80'         FIELD INPUT THIS TIME?                       
         BNO   PRED0220            NO  - SKIP EXISTENCE CHECK                   
         XC    KEY,KEY             READ FOR NETWORK CONTRACT KEY                
         MVI   KEY,X'89'           SET PASSIVE POINTER ID                       
         MVC   KEY+10(2),REC+25    SET REP CODE                                 
         MVC   KEY+12(8),RPRDNET#  INSERT NETWORK CONTRACT #                    
         BAS   RE,HIGH             SKIP DELETED CODES                           
         CLC   KEYSAVE(20),KEY     ALREADY ON FILE?                             
         BNE   PRED0220            NO  - CONTINUE                               
         MVC   LFMMSG(L'BADNETCN),BADNETCN                                      
         B     MYERR                                                            
*                                                                               
PRED0220 EQU   *                                                                
*                                                                               
*                                                                               
*- START BUILDING NETWORK CONTRACT ELEMENT                                      
PRED0235 EQU   *                                                                
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING RPRDNELM,R3                                                      
         MVI   0(R3),X'02'         ELEMENT CODE                                 
*                                                                               
*- NETWORK CONTRACT DESCRIPTION.  REQUIRED.                                     
         LA    R2,PRDNDESH                                                      
         CLC   =XL2'FFFF',SVREPMST                                              
         BE    PRED0240            MASTER                                       
         CLI   5(R2),0             NOT MASTER - ANY DATA?                       
         BE    PRED0260            NO                                           
PRED0240 EQU   *                                                                
         CLI   5(R2),0             MASTER REQUIRES DATA                         
         BE    FLERR1              MISSING                                      
PRED0260 EQU   *                                                                
         MVC   RPRDNDES,8(R2)                                                   
         OC    RPRDNDES,SPACES     BLANK PADDED                                 
*                                                                               
         LA    R2,PRDPPERH         POINT PERSON                                 
         CLC   =XL2'FFFF',SVREPMST                                              
         BE    PRED0280            MASTER                                       
         CLI   5(R2),0             NOT MASTER - ANY DATA?                       
         BE    PRED0340            NO - DON'T ADD ELEMENT                       
         B     PRED0300                                                         
PRED0280 EQU   *                                                                
         CLI   5(R2),0                                                          
         BE    FLERR1              MISSING                                      
PRED0300 EQU   *                                                                
         OC    8(3,R2),SPACES                                                   
         MVC   RPRDNPNT,8(R2)                                                   
         GOTO1 =A(GETPOINT),RR=Y   VALIDATE/DISPLAY EXPAN                       
         BNZ   FLERR2                                                           
*                                                                               
         LA    R4,RPRDN#+1         SET END OF ELEMENT ADDRESS                   
*                                     NEEDED FOR LEN CAL                        
         MVI   RPRDN#,0            SET # OF SHARE GROUPS                        
*                                  SHARE GROUPS NO LONGER USED.                 
PRED0320 EQU   *                                                                
         SR    R4,R3                                                            
         STC   R4,RPRDNLEN                                                      
*                                                                               
*- WHOLE-ELEMENT CROSS EDITS                                                    
         DROP  R3                                                               
PRED0340 EQU   *                                                                
*                                                                               
*- SPOTPAK INTERFACE CODES                                                      
*                                                                               
         MVI   SPOTCHNG,C'N'       SET SPOTPAK CHANGE FLAG NO                   
         LA    R2,PRDSPCH          CLIENT FIELD CHANGED?                        
         TM    4(R2),X'20'         FIELD PREVIOUSLY VALID?                      
         BO    PRED0360            YES                                          
         MVI   SPOTCHNG,C'Y'       SET SPOTPAK CHANGE FLAG YES                  
PRED0360 EQU   *                                                                
         LA    R2,PRDSP1H          PRODUCT 1 FIELD CHANGED?                     
         TM    4(R2),X'20'         FIELD PREVIOUSLY VALID?                      
         BO    PRED0380            YES                                          
         MVI   SPOTCHNG,C'Y'       SET SPOTPAK CHANGE FLAG YES                  
PRED0380 EQU   *                                                                
         LA    R2,PRDSP2H          PRODUCT 2 FIELD CHANGED?                     
         TM    4(R2),X'20'         FIELD PREVIOUSLY VALID?                      
         BO    PRED0400            YES                                          
         MVI   SPOTCHNG,C'Y'       SET SPOTPAK CHANGE FLAG YES                  
PRED0400 EQU   *                                                                
         LA    R2,PRDSPS1H         SPLIT 1 FIELD CHANGED?                       
         TM    4(R2),X'20'         FIELD PREVIOUSLY VALID?                      
         BO    PRED0420            YES                                          
         MVI   SPOTCHNG,C'Y'       SET SPOTPAK CHANGE FLAG YES                  
PRED0420 EQU   *                                                                
         LA    R2,PRDSPS2H         SPLIT 2 FIELD CHANGED?                       
         TM    4(R2),X'20'         FIELD PREVIOUSLY VALID?                      
         BO    PRED0440            YES                                          
         MVI   SPOTCHNG,C'Y'       SET SPOTPAK CHANGE FLAG YES                  
PRED0440 EQU   *                                                                
         LA    R2,PRDSPEH          ESTIMATE FIELD CHANGED?                      
         TM    4(R2),X'20'         FIELD PREVIOUSLY VALID?                      
         BO    PRED0460            YES                                          
         MVI   SPOTCHNG,C'Y'       SET SPOTPAK CHANGE FLAG YES                  
PRED0460 EQU   *                                                                
         CLC   LFMACT(3),=C'CHA'      CAN'T CHANGE SPOT INFO IF                 
*                                     PRODUCT CODE USED BY CONTRACT             
         BNE   PRED0480                                                         
         CLI   SPOTCHNG,C'Y'          HAS SPOTPAK INFO CHANGED?                 
         BNE   PRED0480               NO  - DON'T CHECK FURTHER                 
*                                                                               
         LR    R1,RA                                                            
         USING TWAD,R1                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    PRED0480            YES - DON'T PROHIBIT CHANGE                  
         DROP  R1                                                               
*                                                                               
         CLC   =C'RA',REPALPHA     TEMPORARY TEST                               
         BE    PRED0480            RMR: PERMIT CHANGE                           
*                                                                               
         OC    SPOTLOCK(2),SPOTLOCK   IS COUNTER ZERO?                          
         BZ    PRED0480               YES - ALLOW CHANGES                       
         MVC   LFMMSG(L'ALREADY),ALREADY                                        
         LA    R2,PRDSPCH             NO  - PRODUCT CODE IN USE                 
         B     MYERR                  CHANGES DISALLOWED                        
PRED0480 EQU   *                                                                
         FOUT  PRDSPCNH,SPACES,20                                               
         FOUT  PRDSP1NH,SPACES,20                                               
         FOUT  PRDSP2NH,SPACES,20                                               
         FOUT  PRDSPENH,SPACES,20                                               
*                                                                               
         LA    R2,PRDSPCH                                                       
         CLI   5(R2),0             ANYTHING IN CLIENT FIELD?                    
         BNE   PRED0540            YES - VALIDATE OTHER SPOTPAKS                
****>>>> LA    R3,5                NO  - OTHERS MUST BE EMPTY                   
         LA    R3,3                NO  - OTHERS MUST BE EMPTY                   
*                                  THREE FIELDS WITH SPLIT PRODS OUT            
PRED0500 EQU   *                                                                
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BNE   PRED0520            DATA FOUND - ERROR                           
         BCT   R3,PRED0500                                                      
         B     PRED0740                                                         
PRED0520 EQU   *                                                                
         MVC   LFMMSG(L'NEEDCLI),NEEDCLI                                        
         B     MYERR                                                            
PRED0540 EQU   *                                                                
         BAS   RE,GETREP                                                        
         BNZ   MYERR                                                            
*                                                                               
         LA    R2,PRDSPCH                                                       
         BAS   RE,MOVE                                                          
         MVC   PRDSPC(3),WORK                                                   
*                                                                               
         CLI   5(R2),2                                                          
         BL    PRED0560                                                         
         CLI   5(R2),3                                                          
         BNH   PRED0570                                                         
PRED0560 EQU   *                                                                
         MVC   LFMMSG(L'BADCLI),BADCLI                                          
         B     MYERR                                                            
PRED0570 EQU   *                                                                
         GOTO1 SPOTCLI,DMCB,PRDSPC,PRDSPCN                                      
         BNZ   MYERR                                                            
         FOUT  PRDSPCNH                                                         
*                                                                               
         LA    R2,PRDSP1H                                                       
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   PRDSP1(3),WORK                                                   
         GOTO1 SPOTPRD,DMCB,PRDSP1,PRDSP1N                                      
         BNZ   MYERR                                                            
         FOUT  PRDSP1NH                                                         
*                                                                               
         LA    R2,PRDSP2H                                                       
         CLI   5(R2),0                                                          
         BE    PRED0580                                                         
         BAS   RE,MOVE                                                          
         MVC   PRDSP2(3),WORK                                                   
         GOTO1 SPOTPRD,DMCB,PRDSP2,PRDSP2N                                      
         BNZ   MYERR                                                            
         FOUT  PRDSP2NH                                                         
         CLC   PRDSP1(3),PRDSP2                                                 
         BNE   PRED0580                                                         
         MVC   LFMMSG(L'PRDSAME),PRDSAME                                        
         B     MYERR                                                            
PRED0580 EQU   *                                                                
*                                                                               
         LA    R2,PRDSPS1H                                                      
         CLI   5(R2),0                                                          
         BE    PRED0600                                                         
         BAS   RE,MOVE                                                          
         MVC   PRDSPS1(3),WORK                                                  
         GOTO1 =A(TESTEST),DMCB,PRDSPS1H,SPOTSPL1,RR=Y                          
         BNZ   MYERR                                                            
         LA    R2,PRDSPS2H                                                      
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   PRDSPS2(3),WORK                                                  
         GOTO1 =A(TESTEST),DMCB,PRDSPS2H,SPOTSPL2,RR=Y                          
         BNZ   MYERR                                                            
PRED0600 EQU   *                                                                
*                                                                               
         LA    R2,PRDSPEH                                                       
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   PRDSPE(3),WORK                                                   
         GOTO1 =A(TESTEST),DMCB,PRDSPEH,SPOTEST#,RR=Y                           
         BNZ   MYERR                                                            
         GOTO1 SPOTEST,DMCB,PRDSP1,SPOTEST#,PRDSPEN                             
         BNZ   MYERR                                                            
         FOUT  PRDSPENH                                                         
*                                                                               
         LA    R2,PRDSPS1H                SET CURSOR                            
         CLI   PRDSP2H+5,0                TWO PRODUCT CODES?                    
         BE    PRED0620                   NO, CHECK FOR SPLIT                   
         MVC   LFMMSG(L'PRDSPLIT),PRDSPLIT                                      
         CLI   PRDSPS1H+5,0                                                     
         BE    MYERR                                                            
         CLI   PRDSPS2H+5,0                                                     
         BE    MYERR                                                            
         B     PRED0640                                                         
PRED0620 EQU   *                                                                
         MVC   LFMMSG(L'NOSPLIT),NOSPLIT                                        
         CLI   PRDSPS1H+5,0                                                     
         BNE   MYERR                                                            
         CLI   PRDSPS2H+5,0                                                     
         BNE   MYERR                                                            
PRED0640 EQU   *                                                                
         MVC   LFMMSG(L'LFMMSG),SPACES                                          
*                                                                               
PRED0660 EQU   *                                                                
         BAS   RE,SWIREP                                                        
         BNZ   MYERR                                                            
         XC    WORK(32),WORK                                                    
         LA    R4,WORK                                                          
         MVC   0(2,R4),=X'0320'                                                 
         MVC   RPRDSPCL-RPRDSPOT(3,R4),PRDSPC                                   
         MVC   RPRDSPP1-RPRDSPOT(3,R4),PRDSP1                                   
         CLI   PRDSP2H+5,0                                                      
         BE    PRED0680                                                         
         MVC   RPRDSPP2-RPRDSPOT(3,R4),PRDSP2                                   
PRED0680 EQU   *                                                                
         CLI   PRDSPS1H+5,0                                                     
         BE    PRED0700                                                         
         MVC   RPRDSPS1-RPRDSPOT(1,R4),SPOTSPL1                                 
PRED0700 EQU   *                                                                
         CLI   PRDSPS2H+5,0                                                     
         BE    PRED0720                                                         
         MVC   RPRDSPS2-RPRDSPOT(1,R4),SPOTSPL2                                 
PRED0720 EQU   *                                                                
         MVC   RPRDSPES-RPRDSPOT(1,R4),SPOTEST#                                 
PRED0730 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,REC,WORK    ADD 03 ELEMENT                         
PRED0740 EQU   *                                                                
         CLI   ELEM+1,0                                                         
         BZ    PRED0750                                                         
         GOTO1 VADDELEM,DMCB,REC,ELEM    ADD 02 ELEMENT                         
PRED0750 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(5,0),(2,RPRDUPD) SAVE UPDATE DATE                  
         B     FLFILE                                                           
         EJECT                                                                  
*                                                                               
*        CHECK NEW TO OLD ON A CHANGE                                           
*                                                                               
FLFILE   CLI   BACT,C'A'           TEST ADD                                     
         BE    FLADD                                                            
* CHANGE - READ REC THEN WRITE NEW                                              
         LA    R4,REC2                                                          
         LA    R5,REC                                                           
         BAS   RE,MOVEREC          MOVE REC TO REC2                             
         MVC   KEY(28),REC                                                      
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,GETREC                                                        
         LA    R4,REC                                                           
         LA    R5,REC2                                                          
         BAS   RE,XCREC                                                         
         BAS   RE,PUTREC                                                        
         B     FLFILE2                                                          
*                                                                               
FLADD    BAS   RE,ADDREC                                                        
         MVC   BSVDA,KEY           SAVE DISK ADDRESS                            
*                                                                               
FLFILE2  EQU   *                                                                
         CLI   BREC,9              PRODUCT RECORD IN PROGRESS?                  
         BE    PRDPSV              YES - NETWORK CONTRACT # PASSIVE             
*                                                                               
         B     EXXMOD              NO  -                                        
         EJECT                                                                  
*                                                                               
*   NETWORK CONTRACT # ALTERNATE KEY PROCESSING                                 
*                                                                               
PRDPSV   CLI   BACT,C'A'           TEST ADD/CHANGE                              
         BE    PRDPSVA             ADD                                          
         LA    R2,PRDNCNOH         HAS FIELD CHANGED?                           
         TM    4(R2),X'80'         FIELD INPUT THIS TIME?                       
         BNO   EXXMOD              NO CHANGE - OKAY AS IS                       
*                                                                               
*  DELETE OLD PASSIVE POINTER, IF ONE EXISTS                                    
*                                                                               
         OC    RPRDNET#-RPRDREC+REC2(8),SPACES                                  
         CLC   RPRDNET#-RPRDREC+REC2(8),SPACES                                  
         BE    PRDPSVA             NO OLD NETWORK CONTRACT #                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'89'           SET RECORD ID                                
         MVC   KEY+10(2),REC2+25   SET REP CODE                                 
         MVC   KEY+12(8),RPRDNET#-RPRDREC+REC2   SET NET CON#                   
         MVC   KEY+20(7),REC2+18   SET ADV/PRODUCT CODES                        
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BNE   PRDPSVA             NO  -                                        
*                                                                               
*   IF NETCON# ONLY ENTERED, THE PRD NET MAY NOT HAVE A PASSIVE.                
*                                                                               
****>>>  BE    *+6                 YES                                          
****>>>  DC    H'0'                NOT FOUND - SHOULDN'T HAPPEN                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'        SET DELETE BIT                               
         BAS   RE,WRITE            REWRITE KEY AS DELETE                        
         BAS   RE,CHECK                                                         
PRDPSVA  EQU   *                                                                
         OC    RPRDNET#-RPRDREC+REC(8),SPACES                                   
         CLC   RPRDNET#-RPRDREC+REC(8),SPACES                                   
         BE    EXXMOD              NO NEW NETWORK CONTRACT #                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'89'           SET RECORD ID                                
         MVC   KEY+10(2),REC+25    SET REP CODE                                 
         MVC   KEY+12(8),RPRDNET#-RPRDREC+REC    SET NET CON#                   
         MVC   KEY+20(7),REC+18    SET ADV/PRODUCT CODES                        
         MVC   KEY+28(4),BSVDA     SET DISK ADDRESS                             
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TOTS                                
         BAS   RE,HIGH             CHECK FOR ADD/REWRITE                        
         LA    RF,ADD              SET FOR ADD                                  
         CLC   KEYSAVE(27),KEY                                                  
         BNE   *+8                 NOT EQUAL IS 'ADD'                           
         LA    RF,WRITE            EQUAL IS 'REWRITE'                           
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         BASR  RE,RF               PERFORM THE FUNCTION                         
         BAS   RE,CHECK                                                         
         B     EXXMOD                                                           
*                                                                               
CHECK    EQU   *                                                                
         TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
* SUBROUTINE TO POINT R2 TO NEXT UNPROTECTED FIELD                              
NEXTUF   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BCR   8,RE                                                             
         CLI   0(R2),9                                                          
         BE    NEXTUF2                                                          
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NEXTUF                                                           
*                                                                               
         B     NEXTUF4             WE HAVE A FIELD.                             
*                                                                               
NEXTUF2  CLI   9(R2),0             CHECK FOR LAST                               
         BNE   NEXTUF4                                                          
         CR    R2,R2               IF LAST, SET CC=                             
         BR    RE                                                               
NEXTUF4  LTR   R2,R2               NOT LAST, SET CC NOT=                        
         BR    RE                                                               
         SPACE 2                                                                
         SPACE 2                                                                
* SUBROUTINE TO MOVE 1000 BYTES TO R4 FROM R5                                   
MOVEREC  MVC   000(250,R4),000(R5)                                              
         MVC   250(250,R4),250(R5)                                              
         MVC   500(250,R4),500(R5)                                              
         MVC   750(250,R4),750(R5)                                              
         BR    RE                                                               
         SPACE 2                                                                
XCREC    LA    R0,4                                                             
         XC    0(250,R4),0(R5)                                                  
         XC    0(250,R5),0(R4)                                                  
         XC    0(250,R4),0(R5)                                                  
         LA    R4,250(R4)                                                       
         LA    R5,250(R5)                                                       
         BCT   R0,XCREC+4                                                       
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        SPOTEST --- VALIDATE (FILE) AND EXPAND SPOTPAK ESTIMATE                
*                                                                               
*        P1      =   A(INPUT PRODUCT CODE)                                      
*        P2      =   A(INPUT ESTIMATE CODE (BINARY))                            
*        P3      =   A(OUTPUT ESTIMATE NAME, ZERO = NO EXPAND)                  
*                                                                               
*                                                                               
SPOTEST  NTR1                                                                   
*                                                                               
         L     R2,0(R1)                  GET ADDR OF INPUT PRD CODE             
         L     R3,4(R1)                  GET ADDR OF INPUT EST CODE             
         L     R4,8(R1)                  GET ADDR OF ESTIMATE NAME              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING EKEY,R5                                                          
         MVC   EKEYAM,SPOTMED#                                                  
         MVC   EKEYCLT,SPOTCLI#                                                 
         MVC   EKEYPRD,0(R2)                                                    
         MVC   EKEYEST,0(R3)                                                    
         BAS   RE,SPOTHIGH                                                      
         CLC   KEY(13),KEYSAVE                                                  
         BE    SEST20                                                           
         MVC   LFMMSG(L'NOEST),NOEST                                            
         B     SESTBAD                                                          
SEST20   EQU   *                                                                
         LTR   R4,R4                                                            
         BZ    SESTGOOD                                                         
         L     R6,AIOAREA                                                       
         LA    R5,WORK3                                                         
         ST    R5,AIOAREA                                                       
         BAS   RE,SPOTGET                                                       
         ST    R6,AIOAREA                                                       
         MVC   0(20,R4),EDESC                                                   
         DROP  R5                                                               
*                                                                               
SESTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     SESTEXIT                                                         
SESTBAD  EQU   *                                                                
         LA    R0,1                                                             
SESTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        SPOTPRD --- VALIDATE AND EXPAND SPOTPAK PRODUCT                        
*                                                                               
*        P1      =   A(INPUT PRODUCT CODE)                                      
*        P2      =   A(OUTPUT PRODUCT NAME, ZERO = NO EXPAND)                   
*                                                                               
*                                                                               
SPOTPRD  NTR1                                                                   
*                                                                               
         L     R2,0(R1)                  GET ADDR OF INPUT PRD CODE             
         L     R3,4(R1)                  GET ADDR OF PRODUCT NAME               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PKEY,R4                                                          
         MVC   PKEYAM,SPOTMED#                                                  
         MVC   PKEYCLT,SPOTCLI#                                                 
         MVC   PKEYPRD,0(R2)                                                    
         BAS   RE,SPOTHIGH                                                      
         CLC   KEY(13),KEYSAVE                                                  
         BE    SPRD20                                                           
         MVC   LFMMSG(L'NOPRD),NOPRD                                            
         B     SPRDBAD                                                          
SPRD20   EQU   *                                                                
         LTR   R3,R3                                                            
         BZ    SPRDGOOD                                                         
         L     R5,AIOAREA                                                       
         LA    R4,WORK3                                                         
         ST    R4,AIOAREA                                                       
         BAS   RE,SPOTGET                                                       
         ST    R5,AIOAREA                                                       
         MVC   0(20,R3),PNAME                                                   
         DROP  R4                                                               
*                                                                               
SPRDGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     SPRDEXIT                                                         
SPRDBAD  EQU   *                                                                
         LA    R0,1                                                             
SPRDEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        SPOTCLI --- VALIDATE AND EXPAND SPOTPAK CLIENT                         
*                                                                               
*        P1      =   A(INPUT CLIENT CODE)                                       
*        P2      =   A(OUTPUT CLIENT NAME, ZERO = NO EXPAND)                    
*                                                                               
*                                                                               
SPOTCLI  NTR1                                                                   
*                                                                               
         L     R2,0(R1)                  GET ADDR OF INPUT CLI CODE             
         L     R3,4(R1)                  GET ADDR OF CLIENT NAME                
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A14'                                           
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,0(R2),SPOTCLI#                                         
         CLI   DMCB,0                                                           
         BE    SCLI10                                                           
         MVC   LFMMSG(L'BADCLI),BADCLI                                          
         B     SCLIBAD                                                          
SCLI10   EQU   *                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CKEY,R4                                                          
         MVC   CKEYAM,SPOTMED#                                                  
         MVC   CKEYCLT,SPOTCLI#                                                 
         BAS   RE,SPOTHIGH                                                      
         CLC   KEY(13),KEYSAVE                                                  
         BE    SCLI20                                                           
         MVC   LFMMSG(L'NOCLI),NOCLI                                            
         B     SCLIBAD                                                          
SCLI20   EQU   *                                                                
         LTR   R3,R3                                                            
         BZ    SCLIGOOD                                                         
         L     R5,AIOAREA                                                       
         LA    R4,WORK3                                                         
         ST    R4,AIOAREA                                                       
         BAS   RE,SPOTGET                                                       
         ST    R5,AIOAREA                                                       
         MVC   0(20,R3),CNAME                                                   
         DROP  R4                                                               
*                                                                               
SCLIGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     SCLIEXIT                                                         
SCLIBAD  EQU   *                                                                
         LA    R0,1                                                             
SCLIEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        GETREP --- GET THE REP AND SPOTPAK INTERFACE INFO                      
*                                                                               
GETREP   NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'                                                        
         MVC   KEY+25(2),REPALPHA                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIOAREA                                                       
         LA    R4,REC2                                                          
         ST    R4,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         ST    R3,AIOAREA                                                       
*                                                                               
         LA    R2,REC2+34                                                       
GREP10   EQU   *                                                                
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),5                       SPOTPAK CODES                      
         BE    GREP20                                                           
         CLI   0(R2),0                       END OF RECORD                      
         BNE   GREP10                                                           
         MVC   LFMMSG(L'BADREP),BADREP                                          
         B     GREPBAD                                                          
GREP20   EQU   *                                                                
         GOTO1 SPOTCT,DMCB,2(R2)                                                
         BNZ   GREPBAD                                                          
         BAS   RE,SWISPOT                                                       
         BNZ   GREPBAD                                                          
         GOTO1 SPOTMED,DMCB,8(R2),2(R2),WORK2+950                               
         BNZ   GREPBAD                                                          
*                                                                               
GREPGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     GREPEXIT                                                         
GREPBAD  EQU   *                                                                
         LA    R0,1                                                             
GREPEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        SPOTAGY --- VALIDATE AND EXPAND SPOTPAK AGY CODE                       
*                                                                               
SPOTAGY  NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AGYHDR,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,RPMSPAG                                                  
         BAS   RE,SPOTHIGH                                                      
         CLC   KEY(13),KEYSAVE                                                  
         BE    SAGY100                                                          
         MVC   LFMMSG(L'SPAKAGY),SPAKAGY   AGY NOT ON SPOTPAK                   
         B     SAGYBAD                                                          
SAGY100  EQU   *                                                                
         L     R5,AIOAREA                                                       
         LA    R4,WORK3                                                         
         ST    R4,AIOAREA                                                       
         BAS   RE,SPOTGET                                                       
         ST    R5,AIOAREA                                                       
         MVC   RPMSPAX(20),AGYNAME       LOAD SPOTPAK AGY NAME                  
         FOUT  RPMSPAXH                                                         
         DROP  R4                                                               
*                                                                               
SAGYGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     SAGYEXIT                                                         
SAGYBAD  EQU   *                                                                
         LA    R0,1                                                             
SAGYEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        SPOTMED --- VALIDATE AND EXPAND SPOTPAK MEDIA CODE                     
*                                                                               
*        P1      =   A(SPOTPAK MEDIA CODE)                                      
*        P2      =   A(SPOTPAK AGENCY CODE)                                     
*        P3      =   A(40 BYTE WORK AREA)                                       
*                      ON RETURN, BYTE 1 = SPOTPAK AGY/MEDIA CODE               
*                                 BYTES 2-11 = MEDIA NAME                       
*                                                                               
SPOTMED  NTR1                                                                   
*                                                                               
         LM    R2,R4,0(R1)         GET PARAMS                                   
*                                                                               
         XC    DMCB(12),DMCB                                                    
         ST    R3,DMCB             LOAD A(AGENCY CODE)                          
         MVC   DMCB(1),0(R2)       LOAD (MEDIA CODE)                            
         MVC   DMCB+4(4),VDATAMGR                                               
         ST    R4,DMCB+8           LOAD A(WORK AREA)                            
         GOTO1 =V(MEDGET),DMCB,RR=RELO                                          
         CLI   8(R1),X'FF'                                                      
         BNE   SMED150                                                          
         MVC   LFMMSG(L'SPAKMED),SPAKMED         INVALID MEDIA                  
         B     SMEDBAD                                                          
SMED150  EQU   *                                                                
         MVC   SPOTMED#(1),0(R4)                                                
*                                                                               
SMEDGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     SMEDEXIT                                                         
SMEDBAD  EQU   *                                                                
         LA    R0,1                                                             
SMEDEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        SPOTCT --- LOOK AT THE CONTROL FILE FOR SPOTPAK INFO                   
*                                                                               
*        P1     =   A(SPOTPAK POWER CODE)                                       
*                                                                               
*                                                                               
SPOTCT   NTR1                                                                   
*                                                                               
         L     R2,0(R1)                                                         
*                                                                               
         LA    R5,WORK3                                                         
         XC    0(25,R5),0(R5)                                                   
         MVI   0(R5),C'5'                    SYSTEM ACCESS RECORD               
         MVC   23(2,R5),0(R2)                                                   
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R5),(R5),0                  
         CLI   DMCB+8,0                                                         
         BE    SPCT20                                                           
         MVC   LFMMSG(L'SYSID),SYSID         NO ID RECORD ERROR                 
         B     SPCTBAD                                                          
SPCT20   EQU   *                                                                
         LA    R5,28(R5)                     POINT TO THE START OF REC          
SPCT50   EQU   *                                                                
         CLI   0(R5),0                                                          
         BNE   SPCT60                                                           
         MVC   LFMMSG(L'NOSPOT),NOSPOT       NOT CLEARED FOR SOT                
         B     SPCTBAD                                                          
SPCT60   EQU   *                                                                
         CLI   0(R5),X'21'                    SYSTEM ELEMENT?                   
         BNE   SPCT70                                                           
         CLI   2(R5),X'02'                    SPOT?                             
         BE    SPCT100                                                          
SPCT70   EQU   *                                                                
         ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     SPCT50                                                           
SPCT100  EQU   *                                                                
         MVC   SPOTSYS#(1),3(R5)             FACPAK SYSTEM NUMBER               
         MVC   SPOTAGY#(1),4(R5)             SPOTPAK AGY NUMBER                 
*                                                                               
SPCTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     SPCTEXIT                                                         
SPCTBAD  EQU   *                                                                
         LA    R0,1                                                             
SPCTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        SWITCH TO SPOT                                                         
*                                                                               
SWISPOT  NTR1                                                                   
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SPOTSYS#    SPOT SYSTEM NUMBER                           
         GOTO1 (RF),DMCB                                                        
         SPACE 1                                                                
         CLI   4(R1),2             TEST FOR SYSTEM NOT OPERATIONAL              
         BNE   *+14                                                             
         MVC   LFMMSG(L'SYSNOTUP),SYSNOTUP                                      
         B     SWISBAD                                                          
         SPACE 1                                                                
         CLI   4(R1),0             ALL OTHER ERRORS ARE FATAL                   
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*                                                                               
SWISGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     SPCTEXIT                                                         
SWISBAD  EQU   *                                                                
         LA    R0,1                                                             
SWISEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        SWITCH BACK TO REP                                                     
*                                                                               
SWIREP   NTR1                                                                   
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         L     RF,CSWITCH                                                       
         DROP  RE                                                               
*                                                                               
         GOTO1 (RF),DMCB,=C'REP ',0                                             
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SWIRGOOD EQU   *                                                                
         SR    R0,R0                                                            
         LTR   R0,R0                                                            
         XIT1                                                                   
         EJECT                                                                  
*        SPOTPAK COMMUNICATION WITH DATA MANAGER (DIRECTORY)                    
         SPACE 3                                                                
SPOTREAD MVC   COMMAND,=C'DMREAD'                                               
         B     SPOTDIR                                                          
         SPACE 2                                                                
SPOTSEQ  MVC   COMMAND,=C'DMRSEQ'                                               
         B     SPOTDIR                                                          
         SPACE 2                                                                
SPOTHIGH MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     SPOTDIR                                                          
         SPACE 2                                                                
SPOTDIR  NTR1                                                                   
         IC    R4,DMINBTS                                                       
         IC    R3,TERMNAL                                                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,((R4),COMMAND),=C'SPTDIR  ',KEYSAVE,KEY,  +        
               ((R3),0),0                                                       
         B     DMCHECK                                                          
         EJECT                                                                  
*        SPOTPAK COMMUNICATION WITH DATA MANAGER (FILE)                         
         SPACE 3                                                                
SPOTGET  MVC   COMMAND,=C'GETREC'                                               
         NTR1                                                                   
         LA    R2,KEY+14                                                        
         IC    R3,TERMNAL                                                       
         IC    R4,DMINBTS                                                       
         GOTO1 VDATAMGR,DMCB,((R4),COMMAND),=C'SPTFIL  ',              X        
               (R2),AIOAREA,((R3),DMWORK),0                                     
         SPACE 2                                                                
         B     DMCHECK                                                          
         EJECT                                                                  
       ++INCLUDE RECLRFLD                                                       
         SPACE 2                                                                
AGYERROR LA    R3,AGYERR           INVLAID AGENCY                               
         B     ERROR                                                            
BIGERR   LA    R3,TOOBIG           DATES CAN NOT EXCEED 1 YEAR                  
         B     ERROR                                                            
FLTERR   EQU   *                   INVALID DATE                                 
         MVC   LFMMSG(L'DATEERR),DATEERR                                        
         B     MYERR                                                            
NOCHANGE EQU   *                                                                
         LA    R3,30                                                            
         B     ERROR                                                            
NOCHDATE MVC   LFMMSG(L'CHERR),CHERR                                            
         B     MYERR                                                            
NOCHDT1  EQU   *                                                                
         MVC   LFMMSG(L'CONDEX),CONDEX                                          
         B     MYERR                                                            
FLERR1   LA    R3,MSSNGERR                                                      
         B     ERROR                                                            
FLERR2   LA    R3,INVERR                                                        
         B     ERROR                                                            
FLERR3   LA    R3,NUMERR                                                        
         B     ERROR                                                            
FLERR4   EQU   *                   CAN'T ADD YOURSELF TO LIST                   
         MVC   LFMMSG(L'NOTSELF),NOTSELF                                        
         B     MYERR                                                            
FLERR5   EQU   *                   CAN'T HAVE BOTH MASTER & SUBSIDIARY          
         MVC   LFMMSG(L'NOTBOTH),NOTBOTH                                        
         B     MYERR                                                            
FLERR6   EQU   *                   CAN'T INSERT (SCREEN FULL)                   
         MVC   LFMMSG(L'NOINSERT),NOINSERT                                      
         B     MYERR                                                            
FLERR7   EQU   *                   CONTRACT TYPE DOES NOT EXIST                 
         MVC   LFMMSG(L'NOCTYPE),NOCTYPE                                        
         B     MYERR                                                            
FLERR8   EQU   *                   SAME CONTRACT TYPE CODE                      
         MVC   LFMMSG(L'SAMECTP),SAMECTP                                        
         B     MYERR                                                            
FLERR9   EQU   *                   SAME CONTRACT TYPE CODE                      
         MVC   LFMMSG(L'FLITEMAN),FLITEMAN                                      
         B     MYERR                                                            
FLERR10  EQU   *                   URL ENTERED IS TOO LONG                      
         MVC   LFMMSG(L'URL2LONG),URL2LONG                                      
         B     MYERR                                                            
*                                                                               
MYERR    EQU   *                   MESSAGE ALREADY IN HEADER                    
         FOUT  LFMMSGH                                                          
         MVI   ERRAREA,X'FF'       ERRORS FOUND SWITCH                          
         B     EXIT                                                             
         SPACE 2                                                                
         DS    0F                                                               
RELO     DS    A                                                                
ZEROS    DC    30C'0'                                                           
BLANKS   DC    CL24' '                                                          
BLOCK    DS    CL64                TEMP BLOCK FOR SCANNER/PERVAL                
BLOCK1   DS    CL56                TEMP BLOCK FOR SCANNER/PERVAL                
*                                                                               
BRDATE1  DS    CL12                BROADCAST DATES FOR START DATE               
BRDATE2  DS    CL12                BROADCAST DATES FOR END DATE                 
BRDATE   DS    CL17                BROADCAST MONTH DATES                        
*                                                                               
ELEM     DS    CL256                                                            
         ORG   ELEM                                                             
ELEMCODE DS    XL1                                                              
ELEMLEN  DS    XL1                                                              
ELEMUDTC DS    XL3                 DATE CHANGED                                 
ELEMLUID DS    CL8                 LUID MAKING CHANGE                           
ELEMURL  DS    CL75                                                             
*                                                                               
LELEM    EQU   *-ELEM                                                           
*                                                                               
         ORG                                                                    
CHADATE  DS    XL3                 DATE OF CHANGE                               
USERLUID DS    CL8                                                              
*                                                                               
EZDALUID DS    CL11                                                             
PRDALUID DS    CL11                                                             
U1DALUID DS    CL11                UNASSIGNED TO APPLICATION                    
U2DALUID DS    CL11                UNASSIGNED TO APPLICATION                    
U3DALUID DS    CL11                UNASSIGNED TO APPLICATION                    
*                                                                               
*                                                                               
ONECHAR  DS    C                                                                
*                                                                               
MONTHS   DC    X'01',CL3'JAN'                                                   
         DC    X'02',CL3'FEB'                                                   
         DC    X'03',CL3'MAR'                                                   
         DC    X'04',CL3'APR'                                                   
         DC    X'05',CL3'MAY'                                                   
         DC    X'06',CL3'JUN'                                                   
         DC    X'07',CL3'JUL'                                                   
         DC    X'08',CL3'AUG'                                                   
         DC    X'09',CL3'SEP'                                                   
         DC    X'0A',CL3'OCT'                                                   
         DC    X'0B',CL3'NOV'                                                   
         DC    X'0C',CL3'DEC'                                                   
         DC    X'FF'                                                            
         SPACE 2                                                                
DATEER   EQU   359                                                              
TOOBIG   EQU   49                                                               
*                                                                               
CHERR    DC    C'CONTRACT(S) EXIST. FLIGHT DATES CAN BE REMOVED OR EXPA+        
               NDED'                                                            
*                                                                               
NOTSELF  DC    C'CAN''T ENTER YOUR OWN REP CODE IN THIS FIELD.'                 
*                                                                               
CONEX    DC    C'FIELD CAN NOT BE CHANGED. CONTRACT(S) EXIST.'                  
*                                                                               
CONDEX   DC    C'FIELD CAN NOT BE CHANGED. CONTRACT(S) DATE(S) EXIST.'          
*                                                                               
DATEERR  DC    C'FORMAT IS MMMDD/YY-MMMDD/YY.'                                  
*                                                                               
NOTBOTH  DC    C'CAN''T ENTER BOTH MASTER AND SUBSIDIARY REP FIELDS.'           
*                                                                               
NOINSERT DC    C'RECORD IS FULL. COMMENTS NOT ALLOWED BEYOND THIS LINE'         
*                                                                               
SALOFFER DC    C'SALESPERSON NOT ASSIGNED TO SPECIFIED OFFICE.'                 
*                                                                               
MBMASTER DC    C'FIELD RESERVED FOR MASTER REP USE ONLY.'                       
*                                                                               
NOTASUB  DC    C'REP NOT IN SUBSIDIARY REP LIST.'                               
*                                                                               
NOT100   DC    C'SHARE ALLOCATIONS MUST SUM TO 100 PERCENT.'                    
*                                                                               
OFFCONF  DC    C'SALESPERSON NOT ASSIGNED TO SPECIFIED OFFICE.'                 
*                                                                               
NOTSUB   DC    C'REP CODE NOT A VALID SUBSIDIARY REP.'                          
*                                                                               
BADNETCN DC    C'NETWORK CONTRACT NUMBER ALREADY ASSIGNED.'                     
*                                                                               
MASTONLY DC    C'FIELD RESERVED FOR MASTER REP USE ONLY.'                       
*                                                                               
NETCONLY DC    C'FIELD RESERVED FOR NETWORK CONTRACT USE ONLY.'                 
*                                                                               
NEEDAGY  DC    C'SPOTPAK AGENCY MUST BE ENTERED WITH THIS FIELD.'               
*                                                                               
NEEDCLI  DC    C'SPOTPAK CLIENT MUST BE ENTERED WITH THIS FIELD.'               
*                                                                               
SPAKAGY  DC    C'THIS AGENCY CODE CANNOT BE FOUND IN SPOTPAK.'                  
*                                                                               
SPAKMED  DC    C'THIS MEDIA CODE CANNOT BE FOUND IN SPOTPAK.'                   
*                                                                               
SYSID    DC    C'THIS AGENCY CANNOT BE FOUND IN THE CONTROL SYSTEM.'            
*                                                                               
NOSPOT   DC    C'THIS AGENCY HAS NOT BEEN CLEARED FOR SPOTPAK.'                 
*                                                                               
SYSNOTUP DC    C'YOUR SPOTPAK SPOT SYSTEM IS NOT OPERATIONAL.'                  
*                                                                               
BADREP   DC    C'THIS REP HAS NOT BEEN SET-UP FOR SPOTPAK TRANSFER.'            
*                                                                               
BADCLI   DC    C'INVALID SPOTPAK CLIENT CODE.'                                  
*                                                                               
BADEST   DC    C'INVALID SPOTPAK ESTIMATE CODE.'                                
*                                                                               
NOCLI    DC    C'SPOTPAK CLIENT CODE NOT ON FILE.'                              
*                                                                               
NOPRD    DC    C'SPOTPAK PRODUCT CODE NOT ON FILE.'                             
*                                                                               
PRDSAME  DC    C'SPOTPAK PRODUCT PAIR CANNOT BE THE SAME PRODUCT.'              
*                                                                               
NOSPLIT  DC    C'PRODUCT SPLIT REQUIRES TWO PRODUCTS.'                          
*                                                                               
PRDSPLIT DC    C'PRODUCT SPLIT REQUIRED WITH TWO PRODUCTS.'                     
*                                                                               
NOEST    DC    C'SPOTPAK ESTIMATE CODE NOT ON FILE.'                            
*                                                                               
ALREADY  DC    C'CAN NOT CHANGE SPOTPAK INFO, DATA ALREADY USED.'               
*                                                                               
NOCTYPE  DC    C'CONTRACT TYPE DOES NO EXIST.'                                  
*                                                                               
SAMECTP  DC    C'SAME CONTRACT TYPE.'                                           
*                                                                               
FLITEMAN DC    C'FLIGHT DATE(S) REQUIRED.'                                      
*                                                                               
URL2LONG DC    C'ER#***: EZPOST URL MAX LENGTH = 67 CHARS.'                     
*                                                                               
         SPACE 2                                                                
*                                                                               
*- ERROR MESSAGE EQUATES                                                        
*                                                                               
NOCTGERR EQU   126                                                              
NOADVERR EQU   110                                                              
         EJECT                                                                  
*        LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RELFMINC                                                       
       ++INCLUDE FLDIND                                                         
       ++INCLUDE RGENEROL                                                       
         LTORG                                                                  
       ++INCLUDE RGENOLD                                                        
       ++INCLUDE RELFMWRK                                                       
         EJECT                                                                  
*        WORK AREA FOR RELFM08                                                  
*                                                                               
         ORG   WORK2                                                            
KEY2     DS    CL32                EXTRA KEY HOLDER                             
SPOTSYS# DS    CL1                 FACPACK SYSTEM NUMBER FOR SPOT               
SPOTAGY# DS    CL1                 SPOTPAK AGY NUMBER                           
SPOTMED# DS    CL1                 SPOTPAK AGY/MEDIA NUMBER                     
SPOTCLI# DS    CL2                 SPOTPAK CLIENT CODE PACKED                   
SPOTPRD1 DS    CL3                 SPOTPAK PRODUCT CODE                         
SPOTPRD2 DS    CL3                 SPOTPAK PRODUCT PIGGY CODE                   
SPOTSPL1 DS    CL1                 SPOTPAK PRODUCT SPLIT (BINARY)               
SPOTSPL2 DS    CL1                 SPOTPAK PRODUCT SPLIT (BINARY)               
SPOTESTT DS    CL3                 SPOTPAK ESTIMATE CODE (TEXT)                 
SPOTEST# DS    CL1                 SPOTPAK ESTIMATE CODE (BINARY)               
SPOTLOCK DS    CL2                 SPOTPAK LOCK COUNTER                         
SPOTCHNG DS    CL1                 SPOTPAK CHANGE FLAG                          
*                                  N  =  NO    Y  =  YES                        
URLSTORE DS    CL64                BUILD / STORE AREA FOR URL ELEMENT           
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENREG                                                       
         ORG                                                                    
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENPRD                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENURL                                                       
         EJECT                                                                  
         ORG   REC2                                                             
       ++INCLUDE REGENPTP                                                       
         EJECT                                                                  
         ORG   REC2                                                             
       ++INCLUDE REGENCTG                                                       
         EJECT                                                                  
         ORG   REC2                                                             
       ++INCLUDE REGENADV                                                       
         EJECT                                                                  
         ORG   REC2                                                             
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
         ORG   REC2                                                             
       ++INCLUDE REGENAGY                                                       
         EJECT                                                                  
*              DDCOMFACS                                                        
*        PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
*        PRINT ON                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE FATWA                                                          
       ++INCLUDE RELFMTWA                                                       
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMFED                                                       
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMF6D                                                       
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMD4D                                                       
         ORG   PRDWORK                                                          
SVAGYOFF DS    CL7                 SAVED AGENCY/OFFICE CODE                     
SVDATEF  DS    CL3                 SAVED START DATE                             
SVDATET  DS    CL3                 SAVED END DATE                               
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
SPOTPAK1 DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
SPOTPAK2 DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
SPOTPAK3 DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
SPOTPAK4 DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
*                                                                               
T80408   CSECT                                                                  
*--------------------------------------------------------------------|          
*VALCTYPE - SUBROUTINE VALIDATE CONTRACT TYPE AGAINST REGENCTY X'32' |          
*           RECORD.                                                  |          
*      INPUT-ONECHAR WITH CONTRACT TYPE FROM SCREEN                  |          
*      OUTPUT-CONDITION CODE SET EQUAL FOR VALID CONTRACT TYPE       |          
*             CONDITION CODE SET NOT EQUAL FOR INVALID CONTRACT TYPE |          
*--------------------------------------------------------------------|          
VALCTYPE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   KEY2,KEY        SAVE OFF KEY                                     
*                                                                               
*BUILD CONTRACT TYPE KEY                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'32'        MOVE RECORD TYPE X'32'                          
         MVC   KEY+24(2),REPALPHA  MOVE REP                                     
         MVC   KEY+26(1),ONECHAR   MOVE CONTRACT TYPE TO BE VALIDATED           
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY    SET CONDITION CODE                            
         MVC   KEY,KEY2           RESTORE ORIG KEY                              
         XIT1                                                                   
* END VALCTYPE SUBROUTINE                                                       
*---------------------------------------------------------------------          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   URLPROC:  PROCESS URL ENTRY SCREEN                                          
*                                                                               
URLPROC  NTR1  LABEL=*,BASE=*                                                   
*                                                                               
*        URL RECORD                                                             
*                                                                               
         LR    R1,RA                                                            
*                                                                               
         USING TWAD,R1                                                          
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    URLP0020            NO  - NO DISPLAY                             
         DC    H'0'                ONLY PERMIT FROM A DDS TERMINAL              
         DROP  R1                                                               
*                                                                               
URLP0020 EQU   *                                                                
*                                                                               
         CLI   BFMTSW,0            TEST FORMAT OR EDIT                          
         BNE   URLEDIT                                                          
* FORMAT ROUTINE                                                                
         BAS   RE,GETREC                                                        
*                                                                               
*   START POINT FOR REDISPLAY AFTER UPDATE.                                     
*                                                                               
URLP0025 EQU   *                                                                
         LA    R2,URLEZPNH         SET A(EZPOST URL)                            
         MVC   8(L'URLEZPN,R2),SPACES                                           
         GOTO1 VGETEL,DMCB,(X'10',RURLREC),DMCB+4                               
         CLI   DMCB,X'FF'          FIND EZPOST URL                              
         BE    URLP0045            NO ELEMENT FOUND                             
         L     R6,DMCB+4                                                        
         USING RURLDETL,R6                                                      
         ZIC   RF,1(R6)            GET LENGTH FOR MOVE                          
         SH    RF,=H'14'           SUBTRACT FOR EX                              
         EX    RF,URLP0030                                                      
         B     URLP0040                                                         
URLP0030 EQU   *                                                                
         MVC   8(0,R2),RURLURL                                                  
URLP0040 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(3,RURLDDTC),(5,URLEDTC)                            
         FOUT  URLEDTCH                                                         
         MVC   URLELUI,RURLDLUI                                                 
         FOUT  URLELUIH                                                         
URLP0045 EQU   *                                                                
         OI    4(R2),X'20'         TURN ON PREVIOUSLY VALID                     
         FOUT  (R2)                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    R2,URLPRPNH         SET A(PROPOSER URL)                          
         MVC   8(L'URLPRPN,R2),SPACES                                           
         GOTO1 VGETEL,DMCB,(X'12',RURLREC),DMCB+4                               
         CLI   DMCB,X'FF'          FIND PROPOSER URL                            
         BE    URLP0065            NO ELEMENT FOUND                             
         L     R6,DMCB+4                                                        
         USING RURLDETL,R6                                                      
         ZIC   RF,1(R6)            GET LENGTH FOR MOVE                          
         SH    RF,=H'14'           SUBTRACT FOR EX                              
         EX    RF,URLP0050                                                      
         B     URLP0060                                                         
URLP0050 EQU   *                                                                
         MVC   8(0,R2),RURLURL                                                  
URLP0060 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(3,RURLDDTC),(5,URLPDTC)                            
         FOUT  URLPDTCH                                                         
         MVC   URLPLUI,RURLDLUI                                                 
         FOUT  URLPLUIH                                                         
URLP0065 EQU   *                                                                
         OI    4(R2),X'20'         TURN ON PREVIOUSLY VALID                     
         FOUT  (R2)                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
*                                                                               
****UNUSED 1 DISPLAY                                                            
*                                                                               
         LA    R2,URLUNU1H         SET A(UNUSED#1 URL)                          
         MVC   8(L'URLUNU1,R2),SPACES                                           
         GOTO1 VGETEL,DMCB,(X'14',RURLREC),DMCB+4                               
         CLI   DMCB,X'FF'          FIND UNUSED#1 URL                            
         BE    URLP0085            NO ELEMENT FOUND                             
         L     R6,DMCB+4                                                        
         USING RURLDETL,R6                                                      
         ZIC   RF,1(R6)            GET LENGTH FOR MOVE                          
         SH    RF,=H'14'           SUBTRACT FOR EX                              
         EX    RF,URLP0070                                                      
         B     URLP0080                                                         
URLP0070 EQU   *                                                                
         MVC   8(0,R2),RURLURL                                                  
URLP0080 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(3,RURLDDTC),(5,URL1DTC)                            
         FOUT  URL1DTCH                                                         
         MVC   URL1LUI,RURLDLUI                                                 
         FOUT  URL1LUIH                                                         
URLP0085 EQU   *                                                                
         OI    4(R2),X'20'         TURN ON PREVIOUSLY VALID                     
         FOUT  (R2)                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
****UNUSED 1 DISPLAY                                                            
****UNUSED 2 DISPLAY                                                            
*                                                                               
         LA    R2,URLUNU2H         SET A(UNUSED#2 URL)                          
         MVC   8(L'URLUNU2,R2),SPACES                                           
         GOTO1 VGETEL,DMCB,(X'16',RURLREC),DMCB+4                               
         CLI   DMCB,X'FF'          FIND UNUSED#2 URL                            
         BE    URLP0105            NO ELEMENT FOUND                             
         L     R6,DMCB+4                                                        
         USING RURLDETL,R6                                                      
         ZIC   RF,1(R6)            GET LENGTH FOR MOVE                          
         SH    RF,=H'14'           SUBTRACT FOR EX                              
         EX    RF,URLP0090                                                      
         B     URLP0100                                                         
URLP0090 EQU   *                                                                
         MVC   8(0,R2),RURLURL                                                  
URLP0100 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(3,RURLDDTC),(5,URL2DTC)                            
         FOUT  URL2DTCH                                                         
         MVC   URL2LUI,RURLDLUI                                                 
         FOUT  URL2LUIH                                                         
URLP0105 EQU   *                                                                
         OI    4(R2),X'20'         TURN ON PREVIOUSLY VALID                     
         FOUT  (R2)                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
****UNUSED 2 DISPLAY                                                            
****UNUSED 3 DISPLAY                                                            
*                                                                               
         LA    R2,URLUNU3H         SET A(UNUSED#3 URL)                          
         MVC   8(L'URLUNU3,R2),SPACES                                           
         GOTO1 VGETEL,DMCB,(X'18',RURLREC),DMCB+4                               
         CLI   DMCB,X'FF'          FIND UNUSED#3 URL                            
         BE    URLP0125            NO ELEMENT FOUND                             
         L     R6,DMCB+4                                                        
         USING RURLDETL,R6                                                      
         ZIC   RF,1(R6)            GET LENGTH FOR MOVE                          
         SH    RF,=H'14'           SUBTRACT FOR EX                              
         EX    RF,URLP0110                                                      
         B     URLP0120                                                         
URLP0110 EQU   *                                                                
         MVC   8(0,R2),RURLURL                                                  
URLP0120 EQU   *                                                                
         GOTO1 VDATCON,DMCB,(3,RURLDDTC),(5,URL3DTC)                            
         FOUT  URL3DTCH                                                         
         MVC   URL3LUI,RURLDLUI                                                 
         FOUT  URL3LUIH                                                         
URLP0125 EQU   *                                                                
         OI    4(R2),X'20'         TURN ON PREVIOUSLY VALID                     
         FOUT  (R2)                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
****UNUSED 3 DISPLAY                                                            
         XIT1                                                                   
         EJECT                                                                  
URLEDIT  EQU   *                                                                
         MVC   EZDALUID(2),=X'FFFF' SET 'NO OLD DATA'                           
         MVC   PRDALUID(2),=X'FFFF' SET 'NO OLD DATA'                           
         MVC   U1DALUID(2),=X'FFFF' SET 'NO OLD DATA'                           
         MVC   U2DALUID(2),=X'FFFF' SET 'NO OLD DATA'                           
         MVC   U3DALUID(2),=X'FFFF' SET 'NO OLD DATA'                           
*                                                                               
*   RETRIEVE LUID OF CHANGING TERMINAL                                          
*                                                                               
         L     R7,ACOMFACS         A(COMFACS)                                   
         USING COMFACSD,R7                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         LA    R0,WORK2            SAVE RETURNED FAFACTS AREA                   
         L     RE,0(R1)                                                         
         LHI   R1,L'WORK2                                                       
         LHI   RF,L'WORK2                                                       
         MVCL  R0,RE                                                            
         DROP  R7                                                               
*                                                                               
         LA    RF,WORK2                                                         
         USING FACTSD,RF                                                        
         MVC   USERLUID,FASYM      GET LUID                                     
         DROP  RF                                                               
*                                                                               
         GOTO1 VDATCON,DMCB,(5,WORK),(3,CHADATE)                                
         CLI   BACT,C'A'                                                        
         BE    URLE0010                                                         
         L     R5,AIOAREA          RETRIEVE ORIGINAL URL RECORD                 
         LA    R6,REC2                                                          
         ST    R6,AIOAREA                                                       
         GOTO1 GETREC                                                           
         BAS   RE,GETOLD           SET OLD DATE/LUID                            
*                                                                               
         ST    R5,AIOAREA          RESET IO AREA                                
*                                                                               
URLE0010 EQU   *                                                                
         MVC   REC+34(2),=X'0140'                                               
         MVC   REC+27(2),=Y(98)                                                 
*                                                                               
         LA    R2,URLEZPNH         SET A(EZPOST URL ADDRESS)                    
         CLI   5(R2),0             ANY INPUT?                                   
         BE    URLE0040            NO                                           
*                                                                               
*   EZPOST URL LENGTH NO LONGER CHECKED.                                        
*                                                                               
****     CLI   5(R2),67            YES - MORE THAN 67 CHARS?                    
****     BH    FLERR10             YES - ERROR                                  
         XC    ELEM(LELEM),ELEM    CLEAR NEW ELEMENT AREA                       
*                                                                               
*   BOTH OF THESE FIELDS SHOULD ONLY BE CHANGED IF INPUT ON                     
*        THIS PASS.                                                             
*                                                                               
         TM    4(R2),X'20'         PREVIOUSLY VALID ON?                         
         BO    URLE0016            YES                                          
         MVC   ELEMUDTC,CHADATE    SET CHANGE DATE                              
         MVC   ELEMLUID,USERLUID   SET USER LUID                                
         B     URLE0018                                                         
URLE0016 EQU   *                                                                
         CLC   =X'FFFF',EZDALUID   ANY PREVIOUS VALUE?                          
         BE    URLE0018            NO  -                                        
         MVC   ELEMUDTC(11),EZDALUID  USE PREVIOUS VALUE                        
URLE0018 EQU   *                                                                
*                                                                               
         MVI   ELEMCODE,X'10'      SET ELEMENT CODE                             
         ZIC   RF,5(R2)            GET L(INPUT)                                 
         LA    RF,13(RF)           ADD FOR ELEM CODE/LEN,DATE,LUID              
         STC   RF,ELEMLEN          SET ELEMENT LEN                              
         ZIC   RF,5(R2)            GET L(INPUT)                                 
         BCTR  RF,0                SET FOR MOVE BY LENGTH                       
         EX    RF,URLE0020         MOVE BY LENGTH                               
         B     URLE0030                                                         
URLE0020 EQU   *                                                                
         MVC   ELEMURL,8(R2)       MOVE URL BY LENGTH                           
*                                                                               
URLE0030 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,REC,ELEM  ADD AN ELEMENT                           
*                                                                               
URLE0040 EQU   *                                                                
         LA    R2,URLPRPNH         SET A(PROPOSER URL ADDRESS)                  
         CLI   5(R2),0             ANY INPUT?                                   
         BE    URLE0080            NO                                           
         XC    ELEM(LELEM),ELEM    CLEAR NEW ELEMENT AREA                       
*                                                                               
*   BOTH OF THESE FIELDS SHOULD ONLY BE CHANGED IF INPUT ON                     
*        THIS PASS.                                                             
*                                                                               
         TM    4(R2),X'20'         PREVIOUSLY VALID ON?                         
         BO    URLE0056            YES                                          
         MVC   ELEMUDTC,CHADATE    SET CHANGE DATE                              
         MVC   ELEMLUID,USERLUID   SET USER LUID                                
         B     URLE0058                                                         
URLE0056 EQU   *                                                                
         CLC   =X'FFFF',PRDALUID   ANY PREVIOUS VALUE?                          
         BE    URLE0058            NO  -                                        
         MVC   ELEMUDTC(11),PRDALUID  USE PREVIOUS VALUE                        
URLE0058 EQU   *                                                                
*                                                                               
         MVI   ELEMCODE,X'12'      SET ELEMENT CODE                             
         ZIC   RF,5(R2)            GET L(INPUT)                                 
         LA    RF,13(RF)           ADD FOR ELEM CODE/LEN,DATE,LUID              
         STC   RF,ELEMLEN          SET ELEMENT LEN                              
         ZIC   RF,5(R2)            GET L(INPUT)                                 
         BCTR  RF,0                SET FOR MOVE BY LENGTH                       
         EX    RF,URLE0060         MOVE BY LENGTH                               
         B     URLE0070                                                         
URLE0060 EQU   *                                                                
         MVC   ELEMURL,8(R2)       MOVE URL BY LENGTH                           
*                                                                               
URLE0070 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,REC,ELEM  ADD AN ELEMENT                           
URLE0080 EQU   *                                                                
*** UNUSED1                                                                     
         LA    R2,URLUNU1H         SET A(UNUSED#1 URL ADDRESS)                  
         CLI   5(R2),0             ANY INPUT?                                   
         BE    URLE0130            NO                                           
         XC    ELEM(LELEM),ELEM    CLEAR NEW ELEMENT AREA                       
*                                                                               
*   BOTH OF THESE FIELDS SHOULD ONLY BE CHANGED IF INPUT ON                     
*        THIS PASS.                                                             
*                                                                               
         TM    4(R2),X'20'         PREVIOUSLY VALID ON?                         
         BO    URLE0090            YES                                          
         MVC   ELEMUDTC,CHADATE    SET CHANGE DATE                              
         MVC   ELEMLUID,USERLUID   SET USER LUID                                
         B     URLE0100                                                         
URLE0090 EQU   *                                                                
         CLC   =X'FFFF',U1DALUID   ANY PREVIOUS VALUE?                          
         BE    URLE0100            NO  -                                        
         MVC   ELEMUDTC(11),U1DALUID  USE PREVIOUS VALUE                        
URLE0100 EQU   *                                                                
*                                                                               
         MVI   ELEMCODE,X'14'      SET ELEMENT CODE                             
         ZIC   RF,5(R2)            GET L(INPUT)                                 
         LA    RF,13(RF)           ADD FOR ELEM CODE/LEN,DATE,LUID              
         STC   RF,ELEMLEN          SET ELEMENT LEN                              
         ZIC   RF,5(R2)            GET L(INPUT)                                 
         BCTR  RF,0                SET FOR MOVE BY LENGTH                       
         EX    RF,URLE0110         MOVE BY LENGTH                               
         B     URLE0120                                                         
URLE0110 EQU   *                                                                
         MVC   ELEMURL,8(R2)       MOVE URL BY LENGTH                           
*                                                                               
URLE0120 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,REC,ELEM  ADD AN ELEMENT                           
URLE0130 EQU   *                                                                
*** UNUSED1                                                                     
*** UNUSED2                                                                     
         LA    R2,URLUNU2H         SET A(UNUSED#2 URL ADDRESS)                  
         CLI   5(R2),0             ANY INPUT?                                   
         BE    URLE0180            NO                                           
         XC    ELEM(LELEM),ELEM    CLEAR NEW ELEMENT AREA                       
*                                                                               
*   BOTH OF THESE FIELDS SHOULD ONLY BE CHANGED IF INPUT ON                     
*        THIS PASS.                                                             
*                                                                               
         TM    4(R2),X'20'         PREVIOUSLY VALID ON?                         
         BO    URLE0140            YES                                          
         MVC   ELEMUDTC,CHADATE    SET CHANGE DATE                              
         MVC   ELEMLUID,USERLUID   SET USER LUID                                
         B     URLE0150                                                         
URLE0140 EQU   *                                                                
         CLC   =X'FFFF',U2DALUID   ANY PREVIOUS VALUE?                          
         BE    URLE0150            NO  -                                        
         MVC   ELEMUDTC(11),U2DALUID  USE PREVIOUS VALUE                        
URLE0150 EQU   *                                                                
*                                                                               
         MVI   ELEMCODE,X'16'      SET ELEMENT CODE                             
         ZIC   RF,5(R2)            GET L(INPUT)                                 
         LA    RF,13(RF)           ADD FOR ELEM CODE/LEN,DATE,LUID              
         STC   RF,ELEMLEN          SET ELEMENT LEN                              
         ZIC   RF,5(R2)            GET L(INPUT)                                 
         BCTR  RF,0                SET FOR MOVE BY LENGTH                       
         EX    RF,URLE0160         MOVE BY LENGTH                               
         B     URLE0170                                                         
URLE0160 EQU   *                                                                
         MVC   ELEMURL,8(R2)       MOVE URL BY LENGTH                           
*                                                                               
URLE0170 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,REC,ELEM  ADD AN ELEMENT                           
URLE0180 EQU   *                                                                
*** UNUSED2                                                                     
*** UNUSED3                                                                     
         LA    R2,URLUNU3H         SET A(UNUSED#3 URL ADDRESS)                  
         CLI   5(R2),0             ANY INPUT?                                   
         BE    URLE0230            NO                                           
         XC    ELEM(LELEM),ELEM    CLEAR NEW ELEMENT AREA                       
*                                                                               
*   BOTH OF THESE FIELDS SHOULD ONLY BE CHANGED IF INPUT ON                     
*        THIS PASS.                                                             
*                                                                               
         TM    4(R2),X'20'         PREVIOUSLY VALID ON?                         
         BO    URLE0190            YES                                          
         MVC   ELEMUDTC,CHADATE    SET CHANGE DATE                              
         MVC   ELEMLUID,USERLUID   SET USER LUID                                
         B     URLE0200                                                         
URLE0190 EQU   *                                                                
         CLC   =X'FFFF',U3DALUID   ANY PREVIOUS VALUE?                          
         BE    URLE0200            NO  -                                        
         MVC   ELEMUDTC(11),U3DALUID  USE PREVIOUS VALUE                        
URLE0200 EQU   *                                                                
*                                                                               
         MVI   ELEMCODE,X'18'      SET ELEMENT CODE                             
         ZIC   RF,5(R2)            GET L(INPUT)                                 
         LA    RF,13(RF)           ADD FOR ELEM CODE/LEN,DATE,LUID              
         STC   RF,ELEMLEN          SET ELEMENT LEN                              
         ZIC   RF,5(R2)            GET L(INPUT)                                 
         BCTR  RF,0                SET FOR MOVE BY LENGTH                       
         EX    RF,URLE0210         MOVE BY LENGTH                               
         B     URLE0220                                                         
URLE0210 EQU   *                                                                
         MVC   ELEMURL,8(R2)       MOVE URL BY LENGTH                           
*                                                                               
URLE0220 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,REC,ELEM  ADD AN ELEMENT                           
URLE0230 EQU   *                                                                
*** UNUSED3                                                                     
         B     URLP0025            GO BACK AND REDISPLAY NEW DATA               
         EJECT                                                                  
GETOLD   NTR1                                                                   
         GOTO1 VGETEL,DMCB,(X'10',REC2),DMCB+4                                  
*                                  RETRIEVE EZPOST ELEMENT                      
         CLI   DMCB,X'FF'          FIND PROPOSER URL                            
         BE    GETO0020            NO ELEMENT FOUND                             
         L     R6,DMCB+4                                                        
         USING RURLDETL,R6                                                      
         MVC   EZDALUID(11),RURLDDTC                                            
*                                  SAVE DATE & LUID FOR EZPOST                  
         DROP  R6                                                               
GETO0020 EQU   *                                                                
         GOTO1 VGETEL,DMCB,(X'12',REC2),DMCB+4                                  
*                                  RETRIEVE EZPOST ELEMENT                      
         CLI   DMCB,X'FF'          FIND PROPOSER URL                            
         BE    GETO0040            NO ELEMENT FOUND                             
         L     R6,DMCB+4                                                        
         USING RURLDETL,R6                                                      
         MVC   PRDALUID(11),RURLDDTC                                            
*                                  SAVE DATE & LUID FOR PROPOSER                
         DROP  R6                                                               
GETO0040 EQU   *                                                                
         GOTO1 VGETEL,DMCB,(X'14',REC2),DMCB+4                                  
*                                  RETRIEVE UNUSED#1 ELEMENT                    
         CLI   DMCB,X'FF'          FIND UNUSED#1 URL                            
         BE    GETO0060            NO ELEMENT FOUND                             
         L     R6,DMCB+4                                                        
         USING RURLDETL,R6                                                      
         MVC   U1DALUID(11),RURLDDTC                                            
*                                  SAVE DATE & LUID FOR UNUSED#1                
         DROP  R6                                                               
GETO0060 EQU   *                                                                
         GOTO1 VGETEL,DMCB,(X'16',REC2),DMCB+4                                  
*                                  RETRIEVE UNUSED#2 ELEMENT                    
         CLI   DMCB,X'FF'          FIND UNUSED#2 URL                            
         BE    GETO0080            NO ELEMENT FOUND                             
         L     R6,DMCB+4                                                        
         USING RURLDETL,R6                                                      
         MVC   U2DALUID(11),RURLDDTC                                            
*                                  SAVE DATE & LUID FOR UNUSED#2                
         DROP  R6                                                               
GETO0080 EQU   *                                                                
         GOTO1 VGETEL,DMCB,(X'18',REC2),DMCB+4                                  
*                                  RETRIEVE UNUSED#3 ELEMENT                    
         CLI   DMCB,X'FF'          FIND UNUSED#3 URL                            
         BE    GETO0100            NO ELEMENT FOUND                             
         L     R6,DMCB+4                                                        
         USING RURLDETL,R6                                                      
         MVC   U3DALUID(11),RURLDDTC                                            
*                                  SAVE DATE & LUID FOR UNUSED#3                
         DROP  R6                                                               
GETO0100 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
*- POINT PERSON VALIDATION AND EXPANSION DISPLAY                                
*                                                                               
*  INPUT: R2 = A(FIELD HEADER WITH POINT PERSON CODE FOR VAL)                   
*              1-3 CHARACTER ALPHA                                              
*         NEXT FIELD ON SCREEN = 20 CHAR EXPANSION.                             
*                                                                               
*  RETURN CC: 0 = VALID CODE                                                    
*                                                                               
*  NOTE:  POINT PERSON RECORD READ INTO REC2.                                   
*                                                                               
GETPOINT NTR1  LABEL=*,BASE=*                                                   
         ZIC   R3,0(R2)                                                         
         AR    R3,R2               R3 = A(EXPANSION FIELD)                      
         MVC   8(20,R3),SPACES                                                  
         FOUT  (R3)                CLEAR J.I.C. BAD CODE                        
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'31'                                                        
         MVC   KEY+22(2),REPALPHA                                               
         MVC   KEY+24(3),8(R2)                                                  
         OC    KEY+24(3),SPACES    BLANK PADDED                                 
         GOTO1 READ                                                             
         TM    DMCB+8,X'10'                                                     
         BO    GPNTBAD             BAD CODE                                     
*                                                                               
         L     R4,AIOAREA          DON'T TRASH CALLERS AIOAREA                  
         LA    R5,REC2                                                          
         ST    R5,AIOAREA                                                       
         GOTO1 GETREC              RECORD IN REC2                               
         ST    R4,AIOAREA                                                       
         USING RPTPREC,R5                                                       
         MVC   8(20,R3),RPTPNAME                                                
         DROP  R5                                                               
GPNTOK   SR    R0,R0               GOOD CC                                      
         B     GPNTEXT                                                          
GPNTBAD  LTR   RD,RD               ^0 = BAD                                     
GPNTEXT  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        TESTEST --- VALIDATE (SCREEN) SPOTPAK ESTIMATE                         
*                                                                               
*        P1      =   A(INPUT ESTIMATE CODE (SCREEN HEADER))                     
*        P2      =   A(OUTPUT ESTIMATE CODE (BINARY))                           
*                                                                               
*                                                                               
TESTEST  NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         L     R2,0(R1)                  GET ADDR OF INPUT EST CODE             
         L     R3,4(R1)                  GET ADDR OF OUTPUT EST CODE            
*                                                                               
*        CONVERT INPUT TO 1 BYTE BINARY                                         
*                                                                               
         XC    DMCB(8),DMCB                                                     
         ZIC   R4,5(R2)                  GET I/P LENGTH                         
         ST    R4,DMCB+4                                                        
         LA    R4,8(R2)                  GET ADDR OF I/P                        
         ST    R4,DMCB                                                          
         GOTO1 =V(NUMVAL),DMCB,RR=RELO                                          
         CLI   DMCB,0                                                           
         BE    TEST10                                                           
         MVC   LFMMSG(L'BADEST),BADEST                                          
         B     TESTBAD                                                          
TEST10   EQU   *                                                                
         MVC   0(1,R3),DMCB+7                                                   
*                                                                               
TESTGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     TESTEXIT                                                         
TESTBAD  EQU   *                                                                
         LA    R0,1                                                             
TESTEXIT EQU   *                                                                
         LTR   R0,R0                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'218RELFM08   01/09/13'                                      
         END                                                                    
