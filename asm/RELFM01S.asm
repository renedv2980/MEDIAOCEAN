*          DATA SET RELFM01S   AT LEVEL 238 AS OF 12/16/02                      
*PHASE T80401A,*                                                                
         TITLE 'T80401 - REGION/OWNERSHIP/CMT/PROF/P.PERS RECS'                 
*                                                                               
*********************************************************************           
*                                                                   *           
*        RELFM01 --- REP FILE REGION/OWNERSHIP/CMT/PROF/P.PERS      *           
*        PHASE T80401                                               *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* JUN21/89 (MRR) --- ADD MASTER/SUBSIDIARY REP STUFF TO THE REP     *           
*                     RECORD.  ADD MASTER FILE DISSEMINATION CONTROL*           
*                     TO THE REP RECORD.                            *           
*                                                                   *           
*  07/21/89  PJS  ADDED STANDARD COMMENT RECORD (X'2E' KEY)         *           
*                 USES SCREEN T804E4                                *           
*                                                                   *           
*                                                                   *           
*  08/02/89  PJS  REP REC: CHANGED 'DISSEMINATION' TO 'ACCESS'      *           
*                                                                   *           
*                 FIXED PROFILE DISPLAY/EDIT                        *           
*                                                                   *           
*  01/15/90  PJS  ADDED PROF RECORD.  MODIFY REP RECORD CODE TO     *           
*                 PASS PROFILE UNCHANGED.                           *           
*                                                                   *           
*  01/24/90  PJS  ADDED X'31' POINT PERSON RECORD                   *           
*                                                                   *           
*  01/25/90  PJS  ADDED X'32' CONTRACT TYPE RECORD                  *           
*                                                                   *           
*  JUN12/90 (MRR) --- ADD 1 MASTER FILE RECORD TYPE                 *           
*                                                                   *           
*  06MAR91  (EFJ) --- CHANGE FORMAT OF COMMENT RECS                 *           
*                                                                   *           
*  JUL03/91 (MRR) --- MOVE REP TO RELFM08 BUT LEAVE 'PROF' HERE     *           
*                                                                   *           
*  JAN27/94 (BU ) --- ADD X'3B' DEVELOPMENT CONTRACT TYPE RECORD    *           
*                                                                   *           
*  FEB17/95 (BU ) --- DROP REGION HQ FIELDS                         *           
*                                                                   *           
*  FEB21/95 (BU ) --- ADD DATE LEAVE TO POINT PERSON RECORD         *           
*                                                                   *           
*  NOV07/95 (SKU) --- ADD STANDARD COMMENT CODE TO CONTRACT TYPE    *           
*                                                                   *           
*  APR23/96 (RHV) --- UPGRADE CONTYPE REC TO PROVIDE WORKSHEET &    *           
*                     CONTRACT FORMAT INFO                          *           
*                                                                   *           
*  APR24/96 (BU ) --- USE ALTERNATE SVREPSB2 AREA FOR SVREPSUB      *           
*                     (INSUFFICIENT SPACE IN ORIGINAL)              *           
*                                                                   *           
*  JUN05/96 RHV  ADDITONAL DDS ONLY OPTION BITS ON CONTYPE REC      *           
*                                                                   *           
*  JAN15/98 ASTE -- OWNER CHANGE RECORD - CHANGE STATIONS RECORDS   *           
*                    WITH OWNER #1 TO OWNER #2                      *           
*                                                                   *           
*  FEB19/98 ASTE -- DISPLAY NEW SECURITY PROFILE ELEMENT,CHANGED    *           
*                    SCREEN (RELFME0)                               *           
*                                                                   *           
*  MAY04/99 JRD  -- DON'T ALLOW OWNER CHANGE WHEN PROF 14 IS ON     *           
*                                                                   *           
*  OCT21/99 SKU  -- ADD DARE TO TABLE, INCREASE TABLE SIZE TO 15    *           
*                   AND REMOVE SECURITY ENTRY FROM 2/19/98          *           
*  FEB01/01  ABOB --ADD MASTER CODE TO CONTRACT TYPE                *           
*                                                                   *           
*  FEB19/02 BU   -- UNPROTECT CONTRACT TYPE ADDITIONAL OPTIONS      *           
*                                                                   *           
*  NOV05/02 BU   -- RADIO EDI CHANGES                               *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*                     ***  END TOMBSTONE  ***                       *           
*********************************************************************           
*                                                                               
T80401   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80401,R9,R8,RR=R3                                             
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T804FFD,RA                                                       
*                                                                               
         ST    R3,RELO             SAVE RELOCATION FACTOR                       
*                                                                               
         MVI   UPDATCON,0          SET 'UPDATE CONTROL FILE' OFF                
*                                                                               
         MVC   KEY,BKEY                                                         
         MVI   KEY+27,0                                                         
         MVC   KEY+28(4),BSVDA                                                  
*                                                                               
         LA    R2,LFMLAST          POINT TO FIRST TITLE                         
*                                                                               
         CLI   LFMRECH+5,3         IF NOT MORE THAN 3, SKIP                     
         BNH   INIT001                                                          
*                                                                               
         ZIC   R1,LFMRECH+5        LENGTH OF RECORD INPUT                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LFMREC(0),=C'OWNCHG'  OWNER CHANGE SCREEN?                       
         BNE   *+8                                                              
         MVI   BFMTSW,1                                                         
*                                                                               
*- IF THIS IS FORMAT MODE, BLANK OUT SCREEN FIRST.                              
INIT001  CLI   BFMTSW,0                                                         
         BNE   INIT100                                                          
*                                                                               
         XC    DMCB(24),DMCB       0 PARAMS                                     
         GOTO1 RECLRFLD,DMCB,(R2)                                               
*                                                                               
INIT100  EQU   *                                                                
         CLI   BREC,1                                                           
         BE    REP                                                              
         CLI   BREC,3                                                           
         BE    RGN                                                              
         CLI   BREC,X'2A'                                                       
         BE    OWN                                                              
         CLI   BREC,X'2E'          COMMENT REC?                                 
         BE    CMT                                                              
         CLI   BREC,X'31'          POINT PERSON                                 
         BE    PTP                                                              
         CLI   BREC,X'32'          CONTRACT TYPE                                
         BE    CTY                                                              
         CLI   BREC,X'3B'          DEVELOPMENT CONTRACT TYPE                    
         BE    DEVLCTYP                                                         
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*         REP RECORDS                                                           
*   - CHECK FOR 'PROF' RECORD -- AN ELEMENT ON THE REP RECORD                   
*                                                                               
REP      EQU   *                                                                
         CLC   =C'PROF',LFMREC                                                  
         BE    PROF                REALLY THE PROFILE 'RECORD'                  
*                                                                               
         DC    H'0'                SHOULD NOT BE HERE                           
         TITLE 'PROFILE ELEMENT ON REP RECORD'                                  
*                                                                               
*         PROFILE ELEMENT ON REP RECORD.                                        
*                                                                               
PROF     EQU   *                                                                
         CLI   BFMTSW,0            TEST FORMAT OR EDIT                          
         BNE   PRFEDT                                                           
*                                                                               
* FORMAT ROUTINE                                                                
*                                                                               
         XC    RREPREC(256),RREPREC                                             
*                                                                               
*- BUILD PGM PROFILE ELEMENT IN REC2 WITH ALL ACCESS OFF.                       
         XC    REC2(256),REC2                                                   
         LA    R1,REC2+4                                                        
         LA    RF,PRFTBL                                                        
PRFF020  CLI   0(RF),X'FF'                                                      
         BE    PRFF040                                                          
         MVC   0(1,R1),0(RF)                                                    
         LA    R1,RREPPGML(R1)     NEXT PGM IN 'ELEMENT'                        
         LA    RF,9(RF)            NEXT PRFTBL ENTRY                            
         B     PRFF020                                                          
*                                                                               
PRFF040  EQU   *                                                                
         BAS   RE,GETREC                                                        
*                                                                               
         LA    R2,PROPGM1H         1ST LINE                                     
*                                                                               
*- FIND PGM PROFILE ELEM IN REP REC.  NO ELEMENT = ALL OFF.                     
         GOTO1 VGETEL,DMCB,(X'04',RREPREC),DMCB+4                               
*                                                                               
         LA    R5,#PRFTBL          PROGRAMS TO DISPLAY                          
*                                                                               
         LA    R6,REC2+4           ASSUME NO ELEMENT                            
*                                                                               
         CLI   DMCB,X'FF'          NO ELEM?                                     
         BE    PRFF100             TREAT AS ALL OFF                             
*                                                                               
         L     R6,DMCB+4           A(ELEMENT)                                   
         USING RREPPGMP,R6                                                      
         ZIC   R5,RREPPGM#         # UNITS                                      
         LA    R6,RREPPGM1         A(1ST PGM UNIT)                              
         DROP  R6                                                               
*                                                                               
*- TOP OF DISPLAY LOOP.                                                         
*  R2 = A(CURRENT LINE)   PGM NAME, BITS                                        
*  R5 = LOOP COUNTER                                                            
*  R6 = A(PROFILE ELEMENT DATA). 10 BYTES/PROGRAM                               
*                                                                               
         LA    R3,PRFTBL           FIND PGM NAME FOR DISPLAY                    
PRFF100  EQU   *                                                                
*                                                                               
PRFF110  LA    RF,=CL8'????????'                                                
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    PRFF120                                                          
*                                                                               
PRFF115  EQU   *                                                                
         LA    RF,1(R3)                                                         
         CLC   0(1,R3),0(R6)       TBL -VS- ELEM PROGRAM NUMBER                 
         BE    PRFF120                                                          
*                                                                               
         LA    R3,9(R3)            NEXT TBL ENTRY                               
         B     PRFF110                                                          
*                                                                               
PRFF120  EQU   *                                                                
         MVC   8(8,R2),0(RF)       MOVE NAME TO SCREEN                          
         FOUT  (R2)                                                             
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               R2 = A(BIT DISPLAY FIELD)                    
         FOUT  (R2)                                                             
*                                                                               
         LA    R4,2(R6)            A(PROFILE BITS)                              
         GOTO1 BITOUT,DMCB,(R4)                                                 
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT LINE ON SCREEN                          
*                                                                               
         LA    R6,10(R6)           NEXT PGM IN ELEMENT                          
         BCT   R5,PRFF100                                                       
*                                                                               
PRFF130  EQU   *                                                                
         LA    R3,9(R3)            NEXT TBL ENTRY                               
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    PRFFX                                                            
*                                                                               
         MVC   8(8,R2),1(R3)       MOVE NAME TO SCREEN                          
         FOUT  (R2)                                                             
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               R2 = A(BIT DISPLAY FIELD)                    
         FOUT  (R2)                                                             
*                                                                               
         XC    DUB,DUB                                                          
         GOTO1 BITOUT,DMCB,DUB                                                  
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT LINE ON SCREEN                          
*                                                                               
         B     PRFF130                                                          
*                                                                               
PRFFX    DS    0H                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*- BITOUT -- DISPLAY 64 Y/N BYTES                                               
*  P1 = A(DBLWD DATA BITS)                                                      
*  R2 = A(FLD HEADER FOR OUTPUT)                                                
BITOUT   NTR1                                                                   
         LA    R0,64               64 BITS TO DISPLAY                           
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
         SPACE 2                                                                
*                                                                               
*- PROFILE TABLE FOR PROGRAMS.  END WITH X'FF'                                  
*                                                                               
*  AL1(PROGRAM NUMBER EQUATE)                                                   
*  CL8'PROGRAM NAME FOR SCREEN DISPLAY)                                         
*                                                                               
PRFTBL   EQU   *                                                                
         DC    AL1(RREPQCNT),CL8'CONTRACT'                                      
         DC    AL1(RREPQLFM),CL8'FILE'                                          
         DC    AL1(RREPQINF),CL8'INFO'                                          
         DC    AL1(RREPQINV),CL8'INVOICE'                                       
         DC    AL1(RREPQREQ),CL8'REQUEST'                                       
         DC    AL1(RREPQRMP),CL8'RMP'                                           
         DC    AL1(RREPQRIS),CL8'RIS'                                           
         DC    AL1(RREPQRRG),CL8'RRGON'                                         
         DC    AL1(RREPQSFM),CL8'SFM'                                           
         DC    AL1(RREPQSIN),CL8'SIN'                                           
         DC    AL1(RREPQSEL),CL8'SEL WKSH'                                      
         DC    AL1(RREPQDAR),CL8'DARE'                                          
         DC    AL1(RREPQSEC),CL8'SECURITY'                                      
#PRFTBL  EQU   (*-PRFTBL)/9        # PROGRAMS                                   
         DC    AL1(-1)                     EOT                                  
         DS    0H                                                               
*                                                                               
*        EDIT PROFILE DATA                                                      
*                                                                               
PRFEDT   EQU   *                                                                
         BAS   RE,GETREC           READ REP RECORD                              
*                                                                               
*- DELETE OLD PROFILE ELEMENT IF ANY.                                           
         GOTO1 VDELELEM,DMCB,(X'04',RREPREC)                                    
*                                                                               
*- BUILD NEW ELEMENT                                                            
         XC    REC2(256),REC2                                                   
         MVI   REC2,X'04'          EL CODE                                      
         MVI   REC2+1,RREPPGMX     EL LEN                                       
         MVI   REC2+2,RREPPGMQ     # UNITS                                      
         MVI   REC2+3,0            SPARE                                        
*                                                                               
         LA    R2,PROPGM1H         1ST SCREEN LINE                              
*                                                                               
         ZIC   R5,REC2+2           LOOP COUNTER                                 
*                                                                               
         LA    R6,REC2+4           A(PGM PROFILE IN ELEM)                       
*                                                                               
*- EDIT LOOP.                                                                   
*  FIND PROGRAM NUMBER EQU VIA PRFTBL LOOKUP                                    
*  EDIT 64 INPUT CHARS.  Y/N, BLANK=N                                           
*                                                                               
PRFE100  EQU   *                                                                
         OC    8(8,R2),SPACES      CONVERT NULLS TO BLANKS                      
         LA    R3,PRFTBL                                                        
PRFE110  EQU   *                                                                
         CLI   0(R3),X'FF'                                                      
         BNE   *+6                                                              
         DC    X'0'                HOW DID THIS DISPLAY?                        
         CLC   1(8,R3),8(R2)                                                    
         BE    PRFE120             A HIT!                                       
         LA    R3,9(R3)                                                         
         B     PRFE110                                                          
*                                                                               
PRFE120  EQU   *                                                                
         MVC   0(1,R6),0(R3)       PGM EQU # FROM TBL                           
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               A(BIT INPUT FIELD)                           
*                                                                               
         LA    R3,2(R6)                                                         
         GOTO1 BITIN,DMCB,(R3)     EDIT BITS                                    
         BNZ   MYERR                                                            
*                                                                               
         LA    R6,RREPPGML(R6)     NEXT PGM UNIT IN ELEMENT                     
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT LINE                                    
         BCT   R5,PRFE100                                                       
*                                                                               
*- ADD ELEMENT TO RECORD                                                        
         GOTO1 VADDELEM,DMCB,RREPREC,REC2                                       
*                                                                               
         B     FLFILE                                                           
         SPACE 2                                                                
*                                                                               
*- BITIN -- EDIT 64 Y/N/BLANKS AND RETURN DBLWD BITS.                           
*  P1 = A(DOUBLEWORD OUTPUT AREA)                                               
*  R2 = A(INPUT FIELD HEADER)                                                   
*  CC: ^0 = ERROR.  MSG ALREADY SET.                                            
*  NOTE: BLANK = 'N'                                                            
BITIN    NTR1                                                                   
         LA    R0,64               64 BITS TO DO                                
         SR    R4,R4               R4-R5 = DBLWD OUTPUT                         
         SR    R5,R5                                                            
         L     RE,=XL4'80000000'   RE-RF = ROTATING BIT                         
         SR    RF,RF                                                            
         LA    R2,8(R2)                                                         
         B     BIN30                                                            
*                                                                               
BIN20    SRDL  RE,1                ROTATE BIT RIGHT BY 1                        
BIN30    EQU   *                                                                
         OI    0(R2),C' '          CONVERT NULL TO BLANK                        
         CLI   0(R2),C'N'                                                       
         BE    BIN40                                                            
         CLI   0(R2),C' '          BLANK = N                                    
         BE    BIN40                                                            
         CLI   0(R2),C'Y'          ONLY OTHER VALUE                             
         BNE   BINERR                                                           
*                                                                               
         OR    R4,RE               TURN ON BIT IN OUTPUT REGS                   
         OR    R5,RF                                                            
*                                                                               
BIN40    LA    R2,1(R2)            NEXT A(IN)                                   
         BCT   R0,BIN20                                                         
*                                                                               
         STM   R4,R5,4(R1)         CLOBBER USER'S P2,P3                         
         L     R2,0(R1)            A(USER'S OUTPUT AREA)                        
         MVC   0(8,R2),4(R1)                                                    
         SR    R0,R0               GOOD CC                                      
BINEXT   XIT1                                                                   
         SPACE                                                                  
*                                                                               
*- ERROR.  PUT UP VERY SPECIFIC MESSAGE.                                        
BINERR   EQU   *                                                                
         LA    R2,65                                                            
         SR    R2,R0               R2=PROFILE IN ERROR                          
*                                                                               
         XC    LFMMSG,LFMMSG                                                    
         MVC   LFMMSG(8),=CL8'PROFILE '                                         
         EDIT  (R2),(2,LFMMSG+8)                                                
         MVC   LFMMSG+10(29),=CL29' IS INVALID.  MUST BE Y OR N.'               
         LTR   RD,RD                                                            
         B     BINEXT                                                           
         TITLE 'REGION RECORD'                                                  
*                                                                               
*        REGION RECORDS                                                         
*                                                                               
RGN      CLI   BFMTSW,0            TEST FORMAT OR EDIT                          
         BNE   RGNEDT                                                           
* FORMAT ROUTINE                                                                
         BAS   RE,GETREC                                                        
*                                                                               
         BAS   RE,NEXTUF           POINT TO UNP FIELD                           
         MVC   8(L'RREGNAME,R2),RREGNAME                                        
         FOUT  (R2)                                                             
*                                                                               
*   REGION HQ FIELDS ARE DISCONTINUED AS OF FEB17/95 (BU)                       
*                                                                               
*        BAS   RE,NEXTUF           TV HEADQUARTERS                              
*        MVC   8(L'RREGTHQ,R2),SPACES                                           
*        FOUT  (R2)                                                             
*        OC    RREGTHQ,RREGTHQ                                                  
*        BZ    RGNF10                                                           
*        MVC   8(L'RREGTHQ,R2),RREGTHQ                                          
*        FOUT  (R2)                                                             
*                                                                               
RGNF10   EQU   *                                                                
*        BAS   RE,NEXTUF           RADIO HEADQUARTERS                           
*        MVC   8(L'RREGRHQ,R2),SPACES                                           
*        FOUT  (R2)                                                             
*        OC    RREGRHQ,RREGRHQ                                                  
*        BZ    RGNF20                                                           
*        MVC   8(L'RREGRHQ,R2),RREGRHQ                                          
*        FOUT  (R2)                                                             
*                                                                               
RGNF20   LA    R4,RREGPROF                                                      
RGNF30   BAS   RE,NEXTUF                                                        
         BE    RGNFX                                                            
         MVC   8(1,R2),0(R4)                                                    
         FOUT  (R2)                                                             
         LA    R4,1(R4)                                                         
         B     RGNF30                                                           
RGNFX    B     EXXMOD                                                           
         EJECT                                                                  
* EDIT RGNREC DATA                                                              
*                                                                               
RGNEDT   MVC   REC+34(2),=X'0148'                                               
         MVC   REC+27(2),=Y(106)                                                
         MVC   RREGPROF,ZEROS                                                   
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RREGNAME,WORK                                                    
*                                                                               
*   REGION HQ FIELDS ARE DISCONTINUED AS OF FEB17/95 (BU)                       
*                                                                               
*                                                                               
*        BAS   RE,NEXTUF           TV HEADQUARTERS                              
*        CLI   5(R2),0                                                          
*        BE    FLERR1                                                           
*        BAS   RE,MOVE                                                          
*        MVC   RREGTHQ,WORK                                                     
*        BAS   RE,GETID                                                         
*        MVC   RREGTHID,HALF                                                    
*                                                                               
*        BAS   RE,NEXTUF           RADIO HEADQUARTERS                           
*        CLI   5(R2),0                                                          
*        BE    RGNE10                                                           
*        BAS   RE,MOVE                                                          
*        MVC   RREGRHQ,WORK                                                     
*        BAS   RE,GETID                                                         
*        MVC   RREGRHID,HALF                                                    
*                                                                               
RGNE10   LA    R4,RREGPROF                                                      
RGNE20   BAS   RE,NEXTUF                                                        
         BE    RGNEX                                                            
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         MVC   0(1,R4),8(R2)                                                    
         LA    R4,1(R4)                                                         
         B     RGNE20                                                           
*                                                                               
RGNEX    B     FLFILE                                                           
         EJECT                                                                  
*        SUBROUTINE TO EXTRACT 2 BYTE ID                                        
*                                                                               
GETID    NTR1                                                                   
         LA    R4,REC2                                                          
         XC    0(25,R4),0(R4)      BUILD CONTROL FILE KEY                       
         MVI   0(R4),C'I'                                                       
         MVC   15(10,R4),SPACES                                                 
         MVC   15(8,R4),WORK                                                    
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R4),(R4),0                  
         CLI   DMCB+8,0                                                         
         BNE   NOID                                                             
         LA    R4,28(R4)                                                        
         SR    R5,R5                                                            
*                                                                               
TESTELE  CLI   0(R4),0                                                          
         BE    NOID                                                             
         CLI   0(R4),2                                                          
         BNE   NEXTELE                                                          
         MVC   HALF,2(R4)          ID FOUND                                     
         B     EXXMOD                                                           
*                                                                               
NEXTELE  IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     TESTELE                                                          
*                                                                               
NOID     MVC   HALF,=H'43'      USE DDS ID AS DEFAULT FOR REGIONAL HQ           
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*        OWNERSHIP RECORDS                                                      
*                                                                               
OWN      DS    0H                                                               
* CHECK RECORD FIELD TO SEE IF 'OWNCHG' WAS ENTERED                             
         CLI   LFMRECH+5,3         IF NOT MORE THAN 3, SKIP                     
         BNH   OWNF10                                                           
*                                                                               
         ZIC   R1,LFMRECH+5        LENGTH OF RECORD INPUT                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LFMREC(0),=C'OWNCHG'  OWNER CHANGE SCREEN?                       
         BE    OWNCHG                                                           
*                                                                               
OWNF10   CLI   BFMTSW,0            TEST FORMAT OR EDIT                          
         BNE   OWNEDT                                                           
* FORMAT ROUTINE                                                                
         BAS   RE,GETREC                                                        
*                                                                               
         BAS   RE,NEXTUF           POINT TO UNPROTECTED FIELD                   
         MVC   8(L'ROWNNAME,R2),ROWNNAME                                        
         FOUT  (R2)                                                             
*                                                                               
OWNFX    B     EXXMOD                                                           
         EJECT                                                                  
* EDIT OWNERSHIP DATA                                                           
*                                                                               
OWNEDT   MVC   REC+34(2),=X'0120'  FIRST ELEMENT                                
         MVC   REC+27(2),=Y(66)    TOTAL LENGTH OF RECORD                       
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   ROWNNAME,WORK                                                    
*                                                                               
OWNEX    B     FLFILE                                                           
         EJECT                                                                  
* CHANGE OWNER1 TO OWNER2 FOR ALL STATION RECORDS WITH OWNER1                   
*                                                                               
OWNCHG   DS    0H                                                               
         TM    SVPGPBIT+1,X'04'    PROFILE = CHANGE ONLY ON FRIDAY?             
         BNO   OWNCHG0             NO  - OKAY AS ENTERED                        
*                                                                               
         GOTO1 VDATCON,DMCB,(5,WORK),(0,WORK)                                   
*                                  INSERT TODAY'S DATE EFFECTIVE                
         GOTO1 VGETDAY,DMCB,WORK,DUB                                            
         CLI   DMCB,5              FRIDAY RETURNED?                             
         BE    OWNCHG0                                                          
*                                                                               
         LA    R2,LASNEWH                                                       
         MVC   LFMMSG(L'FRIERR),FRIERR                                          
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
FRIERR   DC    C'** ERROR - OWNER MAY ONLY BE CHANGED ON FRIDAY'                
*                                                                               
OWNCHG0  DS    0H                                                               
         XC    STACNT,STACNT       COUNTER FOR # STA'S CHANGED                  
         XC    SVSTA,SVSTA                                                      
         XC    SVDSKDA,SVDSKDA                                                  
         XC    KEY2,KEY2                                                        
*                                                                               
         BAS   RE,CLRBOTSC         CLEAR BOTTOM OF SCREEN                       
         CLC   LFMACT(3),=C'CHA'                                                
         BE    OWCH05                                                           
*                                                                               
* CLEAR FIELDS ON DISPLAY                                                       
         LA    R2,LASNEWH                                                       
         XC    LASNEW,LASNEW                                                    
         MVI   LASNEWH+5,0                                                      
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,LASNENH                                                       
         XC    LASNEN,LASNEN                                                    
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,LASNUMH                                                       
         XC    LASNUM,LASNUM                                                    
         FOUT  (R2)                                                             
*                                                                               
OWCH05   LA    R2,LASOLDH                                                       
         MVC   8(L'LASOLD,R2),LFMKEY                                            
         FOUT  (R2)                                                             
         BAS   RE,GETREC                                                        
*                                                                               
         LA    R2,LASOLNH                                                       
         MVC   8(L'ROWNNAME,R2),ROWNNAME                                        
         FOUT  (R2)                                                             
*                                                                               
         CLC   LFMACT(3),=C'CHA'                                                
         BNE   OWCHXX                                                           
*                                                                               
         LA    R2,LASNEWH                                                       
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         CLC   LASOLD,LASNEW            CAN'T COPY TO SAME OWNER                
         BE    FLERR2                                                           
*                                                                               
         XC    KEY,KEY                                                          
         USING ROWNREC,R4                                                       
         LA    R4,KEY                                                           
         MVI   KEY,X'2A'                                                        
         MVC   ROWNKREP,REPALPHA                                                
         MVC   ROWNKOWN,LASNEW                                                  
         DROP  R4                                                               
*                                                                               
         GOTO1 READ                     GET NEW OWNER RECORD                    
         BAS   RE,GETREC           DON'T NEED OLD OWNER REC ANYMORE             
         LA    R2,LASNENH                                                       
         MVC   8(L'ROWNNAME,R2),ROWNNAME                                        
         FOUT  (R2)                                                             
*                                                                               
* BUILD P-KEY                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RST5KEY,R4                                                       
         MVI   RST5KTYP,X'83'                                                   
         MVI   RST5KSTP,X'03'                                                   
         MVC   RST5KREP,REPALPHA                                                
         MVC   RST5KOWN,LASOLD                                                  
*                                                                               
         LA    R2,LASBEGH          TO PRINT CHANGED STATIONS                    
         LA    R5,LASENDH          MARK LAST PRINT LINE                         
         LA    R3,8(R2)            TO BUMP THROUGH LINE                         
         ZAP   HALF,=P'9'                                                       
         GOTO1 HIGH                                                             
         B     OWCH11                                                           
*                                                                               
OWCH10   GOTO1 SEQ                                                              
OWCH11   CLC   KEY(RST5KSTA-RST5KEY),KEYSAVE                                    
         BNE   OWCHX               NO STATIONS FOUND W/ OLD OWNER               
*                                                                               
         MVC   SVDSKDA,KEY+28       SAVE D/A                                    
         MVC   SVSTA,RST5KSTA       SAVE STATION                                
         DROP  R4                                                               
*                                                                               
         MVC   KEY2,KEY                 SAVE OLD P-KEY TO RESTORE SEQ           
         BAS   RE,GETREC                                                        
         LA    R4,REC                                                           
         USING RSTAREC,R4                                                       
         MVC   RSTAOWN,LASNEW      CHANGE TO NEW OWNER                          
         BAS   RE,PUTREC                                                        
         DROP  R4                                                               
*                                                                               
* DELETE OLD P-KEY                                                              
         OI    KEY+27,X'80'             TURN DELETE BIT ON                      
         GOTO1 WRITE                                                            
*                                                                               
         ZICM  R1,STACNT,2              INCREMENT # OF P-KEYS DELETED           
         LA    R1,1(R1)                                                         
         STCM  R1,3,STACNT                                                      
*                                                                               
* ADD NEW P-KEY                                                                 
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RST5KEY,R4                                                       
         MVI   RST5KTYP,X'83'                                                   
         MVI   RST5KSTP,X'03'                                                   
         MVC   RST5KREP,REPALPHA                                                
         MVC   RST5KOWN,LASNEW                                                  
         MVC   RST5KSTA,SVSTA                                                   
         DROP  R4                                                               
*                                                                               
         OI    DMINBTS,X'08'       EXAMINE DELETED RECS                         
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    OWCH20              CHANGE P-KEY                                 
*                                                                               
         MVC   KEY,KEYSAVE              ADD P-KEY                               
         MVC   KEY+28(4),SVDSKDA                                                
         GOTO1 ADD                                                              
         B     OWCH25                                                           
*                                                                               
OWCH20   NI    KEY+27,X'FF'-X'80'  TURN OFF DELETE BIT                          
         GOTO1 WRITE                                                            
*                                                                               
OWCH25   DS    0H                                                               
*                                                                               
* DISPLAY STATION JUST CHANGED                                                  
OWCH30   DS    0H                                                               
         CR    R2,R5               PAST LAST PRINT LINE?                        
         BH    OWCH45              YES, DON'T DISPLAY ANYMORE                   
*                                                                               
         MVC   0(4,R3),SVSTA       PRINT STATION                                
         MVI   4(R3),C'-'                                                       
         CLI   SVSTA+4,X'40'       SPACE=TV, IS IT TV?                          
         BNE   *+12                NO, PRINT MEDIA                              
         MVI   5(R3),C'T'          YES, SO PRINT T                              
         B     *+10                                                             
         MVC   5(1,R3),SVSTA+4     NOT TV, MOVE IN MEDIA                        
*                                                                               
         LA    R3,8(R3)                                                         
         SP    HALF,=P'1'          SUBTRACT ONE FROM # ON LINE                  
         BP    OWCH50              IF ZERO, DO STUFF FOR NEXT REC               
         FOUT  (R2)                                                             
         ZIC   R0,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,R0                                                            
         CR    R2,R5               AT LAST PRINT LINE?                          
         BNE   OWCH40              NOT AT END LINE,MUST BE BEFORE               
         ZAP   HALF,=P'8'          AT END LINE, ONE LESS PRINT SPACE            
         B     *+10                                                             
OWCH40   ZAP   HALF,=P'9'                                                       
         LA    R3,8(R2)                                                         
         B     OWCH50                                                           
*                                                                               
OWCH45   DS    0H                                                               
         MVC   LASMOR,=C'MORE--'                                                
         FOUT  (R2)                                                             
OWCH50   DS    0H                                                               
         MVC   KEY,KEY2        GET P-KEY JUST DELETED (OF OLD OWNER)            
         OI    DMINBTS,X'08'       EXAMINE DELETED RECS                         
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08' GET NEXT UNDELETED OLD OWNER P-KEY           
         B     OWCH10                                                           
*                                                                               
OWCHX    DS    0H                  DONE CHANGING STATION REC'S & P-KEYS         
         EDIT  STACNT,LASNUM,ALIGN=LEFT,ZERO=NOBLANK                            
         LA    R2,LASNUMH                                                       
         FOUT  (R2)                                                             
OWCHXX   B     EXXMOD                                                           
*                                                                               
*- STANDARD COMMENT RECORD (X'2E')                                              
*                                                                               
CMT      EQU   *                                                                
         CLI   BFMTSW,0            0=FORMAT (DISPLAY)                           
         BNE   CMTEDT                                                           
*                                                                               
*- STANDARD COMMENT RECORD FORMAT DISPLAY                                       
CMTFMT   EQU   *                                                                
         BAS   RE,GETREC           READ IN RECORD                               
*                                                                               
         BAS   RE,DOCMTF           CALL FORMAT ROUTINE                          
*                                                                               
         B     EXXMOD              EXIT                                         
         SPACE                                                                  
*                                                                               
*- ACTUAL CMT REC FORMAT ROUTINE.  CALLED BY 'DIS' ACTION                       
*  AND AFTER EDIT ROUTINE                                                       
DOCMTF   NTR1                                                                   
*                                                                               
*- BLANK OUT SCREEN                                                             
         XC    DMCB(24),DMCB                                                    
         GOTO1 RECLRFLD,DMCB,(R2)  BLANK TO END OF SCREE                        
*                                                                               
*- FIND FIRST ELEMENT (IF ANY)                                                  
*  ON RETURN   DMCB+08 = A(ELEMENT)                                             
*                                                                               
         GOTO1 VGETEL,DMCB,(X'02',AIOAREA),DMCB+8                               
         CLI   DMCB,X'FF'                                                       
         BE    CMTF900             NO ELEMENTS IN RECORD                        
*                                                                               
         L     R5,DMCB+8           A(1ST ELEMENT)                               
         LA    R7,REC                                                           
         SR    RE,RE                                                            
         ICM   RE,3,RCMTLEN                                                     
         AR    R7,RE               R7=A(END OF RECORD)                          
*                                                                               
*- LOOP THRU FOR EACH ELEMENT ON RECORD & MOVE TO SCREEN.                       
*  R5 = A(ELEMENT).  R2 = A(SCREEN FIELD HEADER)                                
CMTF020  EQU   *                                                                
         BAS   RE,NEXTUF           POINT TO NEXT LINE                           
         BZ    CMTF900             END OF SCREEN                                
*                                                                               
         ZIC   RE,1(R5)            ELEMENT LENGTH IN BYTES                      
         LA    R0,3                                                             
         SR    RE,R0               LESS EL CODE/LEN/1 FOR THE 'EX'              
         EX    RE,CMTOUT           MOVE TEXT TO SCREEN LINE                     
         FOUT  (R2)                                                             
*                                                                               
*- FIND NEXT ELEMENT.                                                           
         ZIC   R6,1(R5)                                                         
         AR    R5,R6                                                            
         CR    R5,R7               PAST END?                                    
         BNL   CMTF900                                                          
*                                                                               
         CLI   0(R5),X'02'         SAME ELEM CODE?                              
         BE    CMTF020             PROCESS THIS ELEMENT                         
*                                                                               
*- END OF FORMATTING.                                                           
CMTF900  XIT1                                                                   
*                                                                               
CMTOUT   MVC   8(0,R2),2(R5)       MOVE TEXT TO SCREEN                          
         EJECT                                                                  
*                                                                               
*- STANDARD COMMENT EDITING ROUTINE                                             
*                                                                               
*  SPECIAL SCREEN EDIT FEATURES: (MUST BE IN 1ST 2 CHARACTERS IN FIELD)         
*        @I = INSERT 1 BLANK LINE (PREVIOUS LINE INFO UNCHANGED)                
*        @D = DELETE 1 LINE                                                     
*                                                                               
*  BUILD NEW RECORD IN 'REC'.                                                   
*  FOR CHANGES, READ OLD RECORD INTO 'REC2' J.I.C. USER IS                      
*      INSERTING BLANK LINES WITH THE '@I' FEATURE                              
*                                                                               
*  'WORK2' USED AS ELEMENT BUILD AREA                                           
*  USE 'WORK3' AS INTERIM WORK AREA AS NEEDED                                   
*                                                                               
*  WORK3 MAP                                                                    
*        +00   A(LAST LINE OF SCREEN INPUT)                                     
* +04   RETURN ADDRESS FOR TEXT ADDING ROUTINE                                  
*                                                                               
CMTEDT   EQU   *                                                                
         CLI   BACT,C'X'           DELETE?                                      
         BNE   CMTE0010                                                         
*                                                                               
         BAS   RE,READ                                                          
         OI    KEY+27,X'80'        SET DELETE BIT                               
         BAS   RE,WRITE                                                         
         B     EXXMOD                                                           
*                                                                               
CMTE0010 EQU   *                                                                
         BAS   RE,NEXTUF           FIND 1ST FIELD                               
*                                                                               
         CLI   BACT,C'A'           ADD?                                         
         BE    CMTE0020            YES                                          
*                                                                               
         L     R5,AIOAREA          SAVE A(IOAREA)                               
         LA    RE,REC2                                                          
         ST    RE,AIOAREA                                                       
         BAS   RE,GETREC           READ OLD REC INTO REC2                       
*                                                                               
         ST    R5,AIOAREA          POINT BACK TO REC.                           
*                                                                               
*- BUILD FIXED PART OF RECORD                                                   
CMTE0020 EQU   *                                                                
         LA    R4,27+2+1+4+RCMTELMX    KEY/LEN/CONTROL/LINK/X'01' EL            
         STCM  R4,3,RCMTLEN        SET UP RECORD LEN   (..LL)                   
*                                                                               
         MVI   RCMT1CDE,X'01'      EL CODE                                      
         MVI   RCMT1LEN,RCMTELMX   EL LEN                                       
         XC    RCMT#LIN,RCMT#LIN   START WITH 0 LINES                           
         XC    RCMTSPAR,RCMTSPAR   0 OUT SPARE                                  
*                                                                               
*- BUILD ACTIVITY DATE ELEMENT                                                  
         XC    WORK2(20),WORK2                                                  
         MVC   WORK2(2),=X'EF0C'   EL CODE & LENGTH                             
         MVC   WORK2+2(3),TODAY    CREATED TODAY                                
         MVC   WORK2+5(3),TODAY    UPDATED TODATE                               
         MVC   WORK2+8(1),BACT     REASON CODE                                  
*                                                                               
         CLI   BACT,C'A'           ADD?                                         
         BE    CMTE0040                                                         
*                                                                               
         GOTO1 VGETEL,DMCB,(X'EF',REC2),DMCB+8                                  
         CLI   DMCB,X'FF'                                                       
         BE    CMTE0040            NO ELEM. TREAT AS ADD                        
*                                                                               
         L     RF,DMCB+8           A(ELEMENT)                                   
         MVC   WORK2+2(3),2(RF)    KEEP CREATE DATE ON CHANGE                   
*                                                                               
CMTE0040 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,RCMTREC,WORK2 ADD ACTIVITY ELEMENT                 
*                                                                               
*- LOOP THRU SCREEN LINES & BUILD X'02' TEXT ELEMENTS.                          
*                                                                               
*  NOTES: -IMEBEDDED BLANK LINES ARE OK...TRAILING ONES OMITTED                 
*                                                                               
*         -INSERT FEATURE - IF 1ST 2 BYTES = '@I' WILL BE INSERTED              
*          ABOVE MARKED LINE.  MARKED LINE WILL BE PRESERVED                    
*          FROM OLD RECORD (IE - LINE WON'T BE LOST)                            
*                                                                               
*         -DELETE FEATURE - IF 1ST 2 BYTES = '@D' LINE WILL BE DELETED          
*                                                                               
*          ** INSERT & DELETE WORK FOR CHANGES ONLY **                          
*                                                                               
*                                                                               
*  DETERMINE WHICH IS LAST INPUT LINE SO WE KNOW WHICH ARE                      
*  IMBEDDED AND WHICH ARE TRAILING BLANKS.                                      
*  PUT A(LAST INPUT LINE) IN WORK3+0                                            
         SR    R4,R4               PUT A(LAST LINE HERE)                        
         LR    R5,R2               1ST INPUT LINE                               
CMTE0050 CLI   0(R5),0             END OF SCREEN?                               
         BE    CMTE0080                                                         
         CLI   5(R5),0             ANY INPUT?                                   
         BZ    *+6                 NO INPUT                                     
         LR    R4,R5               INPUT ON THIS LINE                           
         ZIC   RE,0(R5)                                                         
         AR    R5,RE               NEXT FIELD                                   
         B     CMTE0050                                                         
*                                                                               
CMTE0080 LA    R3,INVERR           AT LEAST 1 LINE REQ'D                        
         ST    R4,WORK3                                                         
         LTR   R4,R4                                                            
         BZ    ERROR               BUT NONE ENTERED                             
*                                                                               
*- LOOP THRU SCREEN                                                             
         LA    R4,1                LINE # WE ARE PROCESSING                     
*                                                                               
CMTE0100 EQU   *                                                                
         CLI   0(R2),0             END OF SCREEN?                               
         BE    CMTE0900            YES                                          
         C     R2,WORK3+00         PAST LAST INPUT FLD?                         
         BH    CMTE0900            YES. STOP.                                   
*                                                                               
* NEXT LINE COMMENTED OUT IN THE BELIEF THAT IT SERVES NO                       
* NO PURPOSE, AND COULD BE POTENTIANLLY DESTRUCTIVE --- EJOR                    
*         XC    0(100,R5),0(R5)     CLEAAR ELEMENT BUILD AREA                   
*                                                                               
*- CHANGES ONLY:                                                                
*  SCAN FOR '@I' OR '@D'....THEY NEED SPECIAL PROCESSING                        
         CLI   BACT,C'A'           ADD?                                         
         BE    CMTE0300                                                         
*                                                                               
         CLC   8(2,R2),=C'@D'      DELETE?                                      
         BE    CMTE0500            YES. SKIP THIS ELEMENT                       
*                                                                               
         CLC   8(2,R2),=C'@I'      INSERT?                                      
         BNE   CMTE0300            NEITHER...PROCESS AS NORMAL                  
*                                                                               
*- INSERT LINE                                                                  
*  1) BUILD ELEMENT WITH 1 BLANK FOR INSERT                                     
*  2) COPY ELEMENT FROM OLD RECORD (IF ANY)                                     
*                                                                               
         MVC   WORK2(3),=X'020340'  ELEM W/1 BYTE BLANK                         
         BAS   RE,ADDTXT                                                        
*                                                                               
*- FIND FIRST ELEMENT (IF ANY)                                                  
*  ON RETURN   DMCB+08 = A(ELEMENT)                                             
*              DMCB+16 = A(END OF RECORD)                                       
*                                                                               
         GOTO1 VGETEL,DMCB,(X'02',REC2),DMCB+8                                  
*                                                                               
         CLI   DMCB,X'FF'                                                       
         BE    CMTE0500            NO ELEMENTS IN OLD RECORD?                   
*                                                                               
         L     R1,DMCB+8           A(1ST ELEMENT)                               
         SR    RE,RE                                                            
         L     RF,DMCB+16          RF=A(END OF REC)                             
*                                                                               
         LR    R0,R4               ELEMENT # TO GET                             
         BCTR  R0,0                                                             
         LTR   R0,R0                                                            
         BZ    CMTE0160            R1=A(ELEM)                                   
*                                                                               
*- FIND THE 'N-TH' ELEMENT                                                      
CMTE0150 EQU   *                                                                
         IC    RE,1(R1)                                                         
         BXH   R1,RE,CMTE0500      OUT OF ELEMENTS. NOTHING TO INSERT           
         CLI   0(R1),X'02'         SAME ELEM CODE?                              
         BNE   CMTE0500            END OF TEXT ELEMENTS                         
         BCT   R0,CMTE0150                                                      
*                                                                               
*- MOVE OLD ELEM INTO NEW RECORD (R1=A(OLD ELEM))                               
CMTE0160 EQU   *                                                                
         ZIC   RE,1(R1)            EL LEN                                       
         BCTR  RE,0                                                             
         EX    RE,CMTCOPY          FROM (R1) TO WORK2                           
         B     CMTE0350            GO ADD ELEM TO RECORD                        
*                                                                               
CMTCOPY  MVC   WORK2(0),0(R1)      COPY ELEMENT FROM OLD REC                    
*                                                                               
CMTBUILD MVC   WORK2+2(0),8(R2)    BUILD ELEMENT FROM SCREEN IPT                
         SPACE                                                                  
*                                                                               
*- REGULAR ELEMENT.                                                             
*  BUILD ELEMENT IN REC                                                         
CMTE0300 ZIC   RE,5(R2)            INPUT LENGTH                                 
         LTR   RE,RE                                                            
         BNZ   CMTE0320                                                         
         MVI   8(R2),C' '          MAKE NO INPUT LOOK LIKE 1 BLANK              
         LA    RE,1                                                             
*                                                                               
CMTE0320 MVI   WORK2,X'02'         ELEMENT CODE                                 
         LA    RF,2(RE)            +2 FOR EL CODE/LEN                           
         STC   RF,WORK2+1          EL CODE TO RECORD                            
*                                                                               
         BCTR  RE,0                TEXT LEN -1 FOR 'EX'                         
         EX    RE,CMTBUILD                                                      
*                                                                               
CMTE0350 BAS   RE,ADDTXT           ADD ELEMENT TO RECORD                        
*                                                                               
*- DONE WITH THIS ELEMENT.                                                      
*  BUMP SCREEN LINE COUNT & LOOP BACK FOR NEXT FIELD                            
CMTE0500 LA    R4,1(R4)            LINE COUNT                                   
         BAS   RE,NEXTUF           R2=A(NEXT FIELD)                             
         B     CMTE0100                                                         
*                                                                               
*- RE-DISPLAY RECORD J.I.C. @I OR @D WERE USED                                  
CMTE0900 EQU   *                   ALL DONE                                     
         LA    R2,LFMLAST                                                       
         BAS   RE,DOCMTF                                                        
         B     FLFILE              UPDATE THE FILE                              
         SPACE 2                                                                
*                                                                               
*- ADDTXT -- ADD ELEMENT FROM WORK2 TO RECORD.                                  
*            BUMP UP ELEMENT COUNT IN RECORD                                    
ADDTXT   ST    RE,WORK3+4          SAVE RETURN ADDRESS                          
         ZIC   RE,RCMT#LIN                                                      
         LA    RE,1(RE)            BUMP LINE COUNT                              
         STC   RE,RCMT#LIN                                                      
*                                                                               
*- FLAG AS ERROR IF INSERT CAUSES LINES TO DROP OFF END OFF SCREEN              
         CLI   RCMT#LIN,12                                                      
         BH    FLERR6              TOO MANY LINES                               
*                                                                               
         GOTO1 VADDELEM,DMCB,RCMTREC,WORK2                                      
         L     RE,WORK3+4                                                       
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*- POINT PERSION RECORD (X'31')                                                 
*                                                                               
PTP      EQU   *                                                                
*                                                                               
*- CLEAR REP, OFFICE, SALESPERSON EXPANSIONS (PREVENT RESIDUAL DATA)            
         FOUT  FPPREPXH,SPACES,20                                               
         FOUT  FPPOFFXH,SPACES,20                                               
         FOUT  FPPSPEXH,SPACES,20                                               
*                                                                               
         CLI   BFMTSW,0            0=FORMAT (DISPLAY)                           
         BNE   PTPEDT                                                           
*                                                                               
*- FORMAT DISPLAY                                                               
PTPFMT   EQU   *                                                                
         BAS   RE,GETREC           READ IN RECORD                               
*                                                                               
         LA    R2,FPPNAMEH         DISPLAY NAME                                 
         FOUT  (R2)                                                             
         MVC   8(L'RPTPNAME,R2),RPTPNAME                                        
*                                                                               
         LA    R2,FPPEDIH          DISPLAY EDI FLAG                             
         MVI   8(R2),C'Y'          SET DEFAULT                                  
         TM    RPTPFLG,X'20'       BLOCK FROM EDI?                              
         BNO   PTPF010             NO                                           
         MVI   8(R2),C'N'          SET BLOCK                                    
PTPF010  EQU   *                                                                
         LA    R2,FPPFONEH         DISPLAY TELEPHONE #                          
         FOUT  (R2)                                                             
         MVC   8(L'RPTPFONE,R2),RPTPFONE                                        
*                                                                               
         OC    RPTPREP,RPTPREP     OPTIONAL REP?                                
         BZ    PTPF020                                                          
         LA    R2,FPPREPH                                                       
         FOUT  (R2)                                                             
         MVC   8(L'RPTPREP,R2),RPTPREP                                          
         BAS   RE,VALREP           DISPLAY EXPANSION                            
*                                                                               
PTPF020  OC    RPTPOFF,RPTPOFF     OPTIONAL OFFICE?                             
         BZ    PTPF040                                                          
         LA    R2,FPPOFFH                                                       
         FOUT  (R2)                                                             
         MVC   8(L'RPTPOFF,R2),RPTPOFF                                          
         BAS   RE,VALOFF           DISPLAY EXPANSION                            
*                                                                               
PTPF040  OC    RPTPSPER,RPTPSPER   OPTIONAL SALESPERSON CODE?                   
         BZ    PTPF060                                                          
         LA    R2,FPPSPERH                                                      
         FOUT  (R2)                                                             
         MVC   8(L'RPTPSPER,R2),RPTPSPER                                        
         BAS   RE,VALSPER          DISPLAY EXPANSION                            
*                                                                               
PTPF060  EQU   *                                                                
         LA    R2,FPPLDATH         A(LEAVE DATE FIELD)                          
*                                                                               
         ZIC   RF,0(R2)            LENGTH OF FIELD                              
         LA    RE,9                                                             
         SR    RF,RE               SUB HEADER + 1 FROM TOTAL LENGTH             
         EX    RF,PTPF0087         CLEAR FIELD                                  
         OC    RPTPLDAT,RPTPLDAT   OPTIONAL LEAVE DATE?                         
         BZ    PTPF080             NO                                           
         GOTO1 VDATCON,DMCB,(2,RPTPLDAT),(5,8(R2))                              
PTPF080  EQU   *                                                                
         FOUT  (R2)                                                             
*                                                                               
PTPF085  DS    0H                                                               
         LA    R2,FPPEMALH         SET A(SCREEN EMAIL ADDRESS)                  
         MVC   FPPEMAL,SPACES                                                   
*                                                                               
         GOTO1 VGETEL,DMCB,(X'20',RPTPREC),DMCB+8                               
         CLI   DMCB,X'FF'                                                       
         BE    PTPF090             NO ELEMENTS                                  
*                                                                               
         USING RPTPEMEM,R6         SALESPERSON EMAIL ADDRESS                    
         L     R6,DMCB+8           A(1ST ELEMENT)                               
*                                                                               
         CLI   1(R6),2             NO EMAIL?                                    
         BNH   PTPF090                                                          
*                                                                               
         ZIC   RF,1(R6)            GET LENGTH OF ELEMENT                        
         SH    RF,=H'3'            SUBTRACT 3 FOR MOVE                          
         EX    RF,PTPF0088         MOVE EMAIL BY LENGTH                         
         B     PTPF090                                                          
*                                                                               
         DROP  R6                                                               
PTPF0087 XC    8(0,R2),8(R2)       CLEAR FIELD BY LENGTH                        
PTPF0088 MVC   8(0,R2),2(R6)       INSERT EMAIL ADDRESS                         
*                                                                               
PTPF090  DS    0H                                                               
         LA    R2,FPPFAXH          SET A(SCREEN FAX ADDRESS)                    
         MVC   FPPFAX,SPACES                                                    
*                                                                               
         GOTO1 VGETEL,DMCB,(X'21',RPTPREC),DMCB+8                               
         CLI   DMCB,X'FF'                                                       
         BE    PTPF100             NO ELEMENTS                                  
*                                                                               
         USING RPTPFXEM,R6         FAX ADDRESS                                  
         L     R6,DMCB+8           A(1ST ELEMENT)                               
*                                                                               
         MVC   FPPFAX,RPTPFXFX     MOVE FAX NUMBER TO SCREEN                    
*                                                                               
         MVC   FPPFAXP,SPACES      CLEAR FIELD                                  
         LA    R2,FPPFAXPH         SET FAX VS EMAIL FLAG                        
*                                                                               
         TM    RPTPFXFG,X'40'      EMAIL PREFERENCE?                            
         BO    PTPF100             YES                                          
         MVI   FPPFAXP,C'Y'        NO  - SET PREF = FAX                         
         DROP  R6                                                               
*                                                                               
PTPF100  DS    0H                                                               
         B     EXXMOD              EXIT                                         
*                                                                               
         SPACE                                                                  
*                                                                               
*- EDIT ROUTINE (ADD/CHANGE)                                                    
PTPEDT   EQU   *                                                                
*                                                                               
         TM    SVPGPBIT+2,X'20'    PROFILE = UPDATE CONTROL FILE?               
         BNO   PTPE0010            NO  -                                        
*                                                                               
         MVI   UPDATCON,1          SET 'UPDATE CONTROL FILE' ON                 
*                                                                               
         L     R7,ACOMFACS         LOAD A(COMFACS)                              
         USING COMFACSD,R7                                                      
         MVC   VSWITCH,CSWITCH     SET A(SWITCH ROUTINE)                        
*                                                                               
*                                  SWITCH TO CONTROL SYSTEM                     
*                                                                               
         GOTO1 VSWITCH,DMCB,(X'0A',X'FFFFFFFF'),0                               
****     GOTO1 VSWITCH,DMCB,(X'0A',X'00000000')                                 
         CLI   4(R1),0             SWITCHED OKAY?                               
         BNE   FLERR8              NO  - NO ACCESS TO CONTROL SYSTEM            
*                                                                               
*   THIS IS SUPPOSED TO BE THE CONTROL ASPECT:  LACK OF ACCESS SHOULD           
*        RETURN AN ERROR MESSAGE.  THIS ISN'T WORKING.                          
*                                                                               
*   NOW SWITCH BACK TO REP IN PROCESS                                           
*                                                                               
         GOTO1 VSWITCH,DMCB,=C'REP ',0                                          
         CLI   4(R1),0             SWITCHED BACK OKAY?                          
         BE    *+6                 YES -                                        
         DC    H'0'                NO  - ABORT                                  
*                                                                               
PTPE0010 EQU   *                                                                
         XC    GOLDKEYS,GOLDKEYS   CLEAR KEYS                                   
         XC    GNEWKEYS,GNEWKEYS                                                
*                                                                               
         CLI   BACT,C'A'           ADD?                                         
         BE    PTPE0020            YES                                          
*                                                                               
         L     R5,AIOAREA          SAVE A(IOAREA)                               
         LA    RE,REC2                                                          
         ST    RE,AIOAREA                                                       
         BAS   RE,GETREC           READ OLD REC INTO REC2                       
*                                                                               
         ST    R5,AIOAREA          POINT BACK TO REC.                           
*                                                                               
         GOTO1 =A(PASSKEYS),DMCB,GOLDKEYS,REC2,RR=Y                             
*                                                                               
PTPE0015 EQU   *                                                                
*                                                                               
*- BUILD FIXED PART OF RECORD                                                   
PTPE0020 EQU   *                                                                
         LA    R4,27+2+1+4+RPTPELMX    KEY/LEN/CONTROL/LINK/X'01' EL            
         STCM  R4,3,RPTPLEN        SET UP RECORD LEN   (..LL)                   
*                                                                               
         XC    RPTP1CDE(RPTPELMX),RPTP1CDE    START CLEAN                       
*                                                                               
         MVI   RPTP1CDE,X'01'      EL CODE                                      
         MVI   RPTP1LEN,RPTPELMX   EL LEN                                       
*                                                                               
*- POINT PERSON NAME...REQUIRED                                                 
         LA    R2,FPPNAMEH                                                      
         CLI   5(R2),0                                                          
         BE    FLERR1              REQUIRED INPUT NOT GIVEN                     
*                                                                               
         MVC   RPTPNAME,8(R2)      NAME TO RECORD                               
         OC    RPTPNAME,SPACES                                                  
*                                                                               
*- EDI USE:  YES (DEFAULT) OR NO                                                
         LA    R2,FPPEDIH                                                       
         CLI   5(R2),0                                                          
         BE    PTPE0030            NO INPUT:  CONSIDER AS YES                   
         CLI   5(R2),1             MORE THAN ONE CHAR?                          
         BH    FLERR2              YES - ERROR                                  
         CLI   8(R2),C'Y'          SET TO 'YES'?                                
         BE    PTPE0030            YES - ALREADY DONE                           
         CLI   8(R2),C'N'          SET TO 'NO '?                                
         BNE   FLERR2              NO  - UNRECOGNIZED VALUE                     
         OI    RPTPFLG,X'20'       YES - SET FLAG TO 'BLOCK'                    
PTPE0030 EQU   *                                                                
*                                                                               
*- TELEPHONE....OPTIONAL                                                        
         LA    R2,FPPFONEH                                                      
         CLI   5(R2),0             ANY INPUT?                                   
         BZ    PTPE0040                                                         
         MVC   RPTPFONE,8(R2)      TELEPHONE TO RECORD (MAY BE BLANK)           
         OC    RPTPFONE,SPACES                                                  
*                                                                               
*- SUBSIDIARY REP CODE...OPTIONAL                                               
*  IF GIVEN:   1. SESSION REP (REPALPHA) MUST BE A MASTER REP                   
*              2. GIVEN REP MUST BE IN THE SUBSIDIARY REP LIST.                 
PTPE0040 EQU   *                                                                
         LA    R2,FPPREPH                                                       
         CLI   5(R2),0             ANY INPUT?                                   
         BZ    PTPE0060                                                         
         MVC   RPTPREP,8(R2)       REP CODE.                                    
         OC    RPTPREP,SPACES                                                   
         BAS   RE,VALREP           VALIDATE/DISPLAY EXPANSION                   
         BNZ   FLERR2                                                           
*                                                                               
         CLC   =H'-1',SVREPMST     SESSION REP MUST BE MASTER                   
         BE    PTPE0045                                                         
*                                                                               
         MVC   LFMMSG(L'MBMASTER),MBMASTER  MUST BE MASTER                      
         B     MYERR                                                            
*                                                                               
PTPE0045 LA    R1,SVREPSB2         SUBSIDIARY REP ELEMENT                       
         USING RREPSUB,R1                                                       
         ZIC   R0,RREPSCNT         # SUB REPS                                   
         LA    R1,RREPSCOD                                                      
         DROP  R1                                                               
PTPE0050 CLC   RPTPREP,0(R1)       PPERS REP IN SUBLIST?                        
         BE    PTPE0060                                                         
         LA    R1,2(R1)                                                         
         BCT   R0,PTPE0050                                                      
*                                                                               
         MVC   LFMMSG(L'NOTASUB),NOTASUB  NOT A SUBSID REP                      
         B     MYERR                                                            
*                                                                               
*- OFFICE....OPTIONAL                                                           
PTPE0060 EQU   *                                                                
         LA    R2,FPPOFFH                                                       
         CLI   5(R2),0             ANY INPUT?                                   
         BZ    PTPE0080                                                         
         MVC   RPTPOFF,8(R2)       OFFICE CODE                                  
         OC    RPTPOFF,SPACES                                                   
         BAS   RE,VALOFF           VALIDATE/DISPLAY EXPANSION                   
         BNZ   FLERR2                                                           
*                                                                               
*- LOCAL SALESPERSON CODE....OPTIONAL                                           
*  IF OFFICE GIVEN, SALESPERSON OFFICE MUST MATCH GIVEN OFFICE.                 
PTPE0080 EQU   *                                                                
         LA    R2,FPPSPERH                                                      
         CLI   5(R2),0             ANY INPUT?                                   
         BZ    PTPE0090                                                         
         MVC   RPTPSPER,8(R2)      SALESPERSON CODE                             
         OC    RPTPSPER,SPACES                                                  
         BAS   RE,VALSPER          VALIDATE/DISPLAY EXPANSION                   
         BNZ   FLERR2                                                           
*                                                                               
         OC    RPTPOFF,RPTPOFF     OFFICE GIVEN?                                
         BZ    PTPE0090                                                         
*                                                                               
         LA    R1,RPTPOFF          WATCH OUT FOR THE USING                      
         LA    R3,WORK2                                                         
         USING RSALREC,R3                                                       
         CLC   RSALOFF,0(R1)       MUST MATCH                                   
         BE    PTPE0090                                                         
         DROP  R3                                                               
*                                                                               
         MVC   LFMMSG(L'SALOFFER),SALOFFER  SALESPERS. OFF CONFLICT             
         B     MYERR                                                            
*                                                                               
*                                                                               
*- LEAVE DATE....OPTIONAL                                                       
*                                                                               
PTPE0090 EQU   *                                                                
         XC    LEAVDATE,LEAVDATE   CLEAR LEAVE DATE                             
         LA    R2,FPPLDATH         A(LEAVE DATE FIELD)                          
         CLI   5(R2),0             ANY INPUT?                                   
         BZ    PTPE0100            NO                                           
         XC    WORK,WORK                                                        
         GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK                                      
         OC    0(4,R1),0(R1)       ERROR?                                       
         BZ    ERROR               YES                                          
         GOTO1 VDATCON,DMCB,WORK,(2,RPTPLDAT)                                   
*                                  INSERT DATE INTO RECORD                      
         MVC   LEAVDATE,RPTPLDAT   SAVE LEAVE DATE                              
*                                                                               
*                                                                               
*- BUILD ACTIVITY DATE ELEMENT                                                  
PTPE0100 EQU   *                                                                
         XC    WORK2(20),WORK2                                                  
         MVC   WORK2(2),=X'EF0C'   EL CODE & LENGTH                             
         MVC   WORK2+2(3),TODAY    CREATED TODAY                                
         MVC   WORK2+5(3),TODAY    UPDATED TODATE                               
         MVC   WORK2+8(1),BACT     REASON CODE                                  
*                                                                               
         CLI   BACT,C'A'           ADD?                                         
         BE    PTPE0340                                                         
*                                                                               
         GOTO1 VGETEL,DMCB,(X'EF',REC2),DMCB+8                                  
         CLI   DMCB,X'FF'                                                       
         BE    PTPE0340            NO ELEM. TREAT AS ADD                        
*                                                                               
*                                                                               
         L     RF,DMCB+8           A(ELEMENT)                                   
         MVC   WORK2+2(3),2(RF)    KEEP CREATE DATE ON CHANGE                   
*                                                                               
PTPE0340 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,RPTPREC,WORK2     ADD ACTIVITY ELEMENT             
*                                                                               
         TM    RPTPFLG,X'20'       'BLOCK FROM EDI' SET TO YES?                 
         BO    PTPE0400            YES - NO NEW PASSIVE KEYS                    
*                                                                               
         CLC   RPTPOFF,SPACES      ANY OFFICE ENTERED?                          
         BNH   PTPE0400            NO  - NO NEW PASSIVE KEYS                    
*                                                                               
         OC    LEAVDATE,LEAVDATE   ANY LEAVE DATE?                              
         BZ    PTPE0360            NO                                           
         GOTO1 VDATCON,DMCB,(5,WORK),(2,WORK)                                   
*                                  GET TODAY'S DATE                             
         CLC   LEAVDATE,WORK                                                    
         BNH   PTPE0400            LEAVE DATE NOT IN FUTURE                     
PTPE0360 EQU   *                                                                
         GOTO1 =A(PASSKEYS),DMCB,GNEWKEYS,REC,RR=Y                              
*                                                                               
PTPE0400 EQU   *                                                                
         GOTO1 VDELELEM,DMCB,(X'20',RPTPREC)                                    
*                                                                               
         XC    WORK2(2+L'FPPEMAL),WORK2                                         
         MVI   WORK2,X'20'                                                      
*                                                                               
         ZIC   RE,FPPEMALH+5                                                    
         CHI   RE,0                                                             
         BE    PTPE0410                                                         
*                                                                               
         AHI   RE,-1                                                            
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+2(0),FPPEMAL                                               
*                                                                               
         AHI   RE,3                LENGTH OF ELEM                               
         STC   RE,WORK2+1                                                       
*                                                                               
         GOTO1 VADDELEM,DMCB,RPTPREC,WORK2                                      
*                                                                               
PTPE0410 DS    0H                                                               
         GOTO1 VDELELEM,DMCB,(X'21',RPTPREC)                                    
*                                                                               
         XC    WORK2(4+L'FPPFAX),WORK2                                          
         MVI   WORK2,X'21'         INSERT ELEMENT CODE                          
         MVI   WORK2+1,16          INSERT ELEMENT LENGTH                        
*                                                                               
         ZIC   RE,FPPFAXH+5        GET LENGTH OF INPUT                          
         CHI   RE,0                NO INPUT/NO ELEMENT                          
         BE    PTPE0420                                                         
*                                                                               
         BCTR  RE,0                BACK OFF 1 FOR EX                            
         EX    RE,PTPE0412         MOVE BY LENGTH                               
         B     PTPE0420                                                         
PTPE0412 MVC   WORK2+2(0),FPPFAX                                                
PTPE0420 EQU   *                                                                
         OC    WORK2+2(12),SPACES  SET BINARY TO SPACES                         
*                                                                               
         CLI   FPPFAXPH+5,0        ANY FAX PREF ENTERED?                        
         BE    PTPE0425            NO  - SET TO EMAIL ADDR                      
         CLI   FPPFAXP,C'Y'        PREF = FAX?                                  
         BE    PTPE0430            YES - SET TO FAX PREFERENCE                  
PTPE0425 EQU   *                                                                
         OI    WORK2+14,X'40'      NO  - SET TO EMAIL PREFERENCE                
         B     PTPE0435                                                         
PTPE0430 EQU   *                                                                
         OI    WORK2+14,X'20'      SET TO FAX   PREFERENCE                      
PTPE0435 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,RPTPREC,WORK2                                      
*                                                                               
PTPE0440 DS    0H                                                               
         GOTO1 =A(NEW71REC),DMCB,GNEWKEYS,REC,RR=Y                              
*                                                                               
         B     FLFILE              UPDATE THE FILE                              
         EJECT                                                                  
*                                                                               
*- VALREP -- VALIDATE RPTPREP. DISPLAY EXPANSION IF VALID.                      
*  CC: 0 = GOOD; ^0 = BAD.                                                      
*  NOTE: THIS USES WORK2                                                        
VALREP   NTR1                                                                   
         LA    R2,FPPREPXH         CLEAR EXPANSION J.I.C. ERROR                 
         FOUT  (R2)                                                             
         MVC   8(20,R2),SPACES                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           REC ID                                       
         MVC   KEY+25(2),RPTPREP                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VREPERR                                                          
*                                                                               
         L     R4,AIOAREA          READ RECORD INTO WORK2                       
         LA    R3,WORK2                                                         
         ST    R3,AIOAREA                                                       
         GOTO1 GETREC                                                           
         ST    R4,AIOAREA                                                       
*                                                                               
         USING RREPREC,R3                                                       
         MVC   8(20,R2),RREPSHRT   SHORT NAME EXPANSION                         
         DROP  R3                                                               
VREPOK   SR    R0,R0               0 CC                                         
         B     VREPEXT                                                          
VREPERR  LTR   RD,RD               ^0                                           
VREPEXT  XIT1                                                                   
         EJECT                                                                  
*                                                                               
*- VALOFF -- VALIDATE RPTPOFF. DISPLAY EXPANSION IF VALID.                      
*  CC: 0 = GOOD; ^0 = BAD.                                                      
*  NOTE: THIS USES WORK2                                                        
VALOFF   NTR1                                                                   
         LA    R2,FPPOFFXH         CLEAR EXPANSION J.I.C. ERROR                 
         FOUT  (R2)                                                             
         MVC   8(20,R2),SPACES                                                  
*                                                                               
*- USE SUBSIDIARY REP IF GIVEN, ELSE USE SIGN-ON REP                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'04'           REC ID                                       
         MVC   KEY+23(2),REPALPHA  ASSUME NO SUB REP                            
         OC    RPTPREP,RPTPREP                                                  
         BZ    VOFF20                                                           
         MVC   KEY+23(2),RPTPREP   USE SUB REP                                  
VOFF20   MVC   KEY+25(2),RPTPOFF                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VREPERR                                                          
*                                                                               
         L     R4,AIOAREA          READ RECORD INTO WORK2                       
         LA    R3,WORK2                                                         
         ST    R3,AIOAREA                                                       
         GOTO1 GETREC                                                           
         ST    R4,AIOAREA                                                       
*                                                                               
         USING ROFFREC,R3                                                       
         MVC   8(20,R2),ROFFNAME   NAME EXPANSION                               
         DROP  R3                                                               
VOFFOK   SR    R0,R0               0 CC                                         
         B     VOFFEXT                                                          
VOFFERR  LTR   RD,RD               ^0                                           
VOFFEXT  XIT1                                                                   
         EJECT                                                                  
*                                                                               
*- VALSPER -- VALIDATE RPTPSPER. DISPLAY EXPANSION IF VALID.                    
*  CC: 0 = GOOD; ^0 = BAD.                                                      
*  NOTE: THIS USES WORK2                                                        
VALSPER  NTR1                                                                   
         LA    R2,FPPSPEXH         CLEAR EXPANSION J.I.C. ERROR                 
         FOUT  (R2)                                                             
         MVC   8(20,R2),SPACES                                                  
*                                                                               
*- USE SUBSIDIARY REP IF GIVEN, ELSE USE SIGN-ON REP                            
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'           REC ID                                       
         MVC   KEY+22(2),REPALPHA  ASSUME NO SUB REP                            
         OC    RPTPREP,RPTPREP                                                  
         BZ    VSPE20                                                           
         MVC   KEY+22(2),RPTPREP   USE SUB REP                                  
VSPE20   MVC   KEY+24(3),RPTPSPER                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VREPERR                                                          
*                                                                               
         L     R4,AIOAREA          READ RECORD INTO WORK2                       
         LA    R3,WORK2                                                         
         ST    R3,AIOAREA                                                       
         GOTO1 GETREC                                                           
         ST    R4,AIOAREA                                                       
*                                                                               
         USING RSALREC,R3                                                       
         MVC   8(20,R2),RSALNAME   NAME EXPANSION                               
         DROP  R3                                                               
VSPEOK   SR    R0,R0               0 CC                                         
         B     VOFFEXT                                                          
VSPEERR  LTR   RD,RD               ^0                                           
VSPEEXT  XIT1                                                                   
         EJECT                                                                  
*                                                                               
*- CONTRACT TYPE RECORD (X'32')                                                 
*                                                                               
CTY      EQU   *                                                                
         CLI   BFMTSW,0            0=FORMAT (DISPLAY)                           
         BNE   CTYEDT                                                           
*                                                                               
*- FORMAT DISPLAY                                                               
CTYFMT   EQU   *                                                                
         BAS   RE,GETREC           READ IN RECORD                               
*                                                                               
         LA    R2,CTYDESCH         DISPLAY DESCRIPTION                          
         FOUT  (R2)                                                             
         MVC   8(L'RCTYDESC,R2),RCTYDESC                                        
*                                                                               
         CLI   RCTY1LEN,RCTYELMX                                                
         BL    CTYF0010                                                         
*                                                                               
         LA    R2,CTYCMMTH         DISPLAY STANDARD COMMENT CODE                
         FOUT  (R2)                                                             
         MVC   8(L'RCTYCMMT,R2),RCTYCMMT                                        
*                                                                               
CTYF0010 DS    0H                                                               
         SR    R0,R0                                                            
         XC    WORK2(255),WORK2                                                 
         GOTO1 VGETEL,DMCB,(X'10',RCTYREC),DMCB+8                               
         L     R6,DMCB+8                                                        
         CLI   DMCB,X'FF'                                                       
         BNE   *+10                                                             
         LA    R6,WORK2                 IF NO ELEM POINT TO NULLS               
         AR    R0,RB                    IF NO ELEM SET FLAG                     
         LR    R4,R6                                                            
*                                                                               
         LA    R3,FLDTAB                FIELD TABLE                             
         LA    R2,CTYRNMH               FIRST FIELD                             
         LA    R4,2(R4)                 FIRST FIELD STATUS BYTE                 
CTYF0020 DS    0H                                                               
         CLI   0(R3),X'FF'              END OF FIELDS ON SCREEN?                
         BE    CTYF0100                                                         
         GOTO1 VGETEL,DMCB,(X'12',RCTYREC),DMCB+8                               
         L     R6,DMCB+8                                                        
         CLI   DMCB,X'FF'               NO ELEM?                                
         BE    CTYF0050                                                         
CTYF0030 DS    0H                                                               
         CLC   2(1,R6),0(R3)            SAME FIELD LABEL AS THIS FIELD?         
         BNE   CTYF0040                                                         
         ZIC   R1,1(R6)                 YES - MOVE IT IN                        
         SH    R1,=H'3'                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),3(R6)                                                    
         FOUT  (R2)                                                             
         B     CTYF0050                                                         
CTYF0040 DS    0H                                                               
         ZIC   R1,1(R6)                 BUMP TO NEXT ELEM                       
         AR    R6,R1                                                            
         CLI   0(R6),X'12'                                                      
         BE    CTYF0030                                                         
CTYF0050 DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                    FIRST Y/N FIELD                         
         TM    1(R2),X'20'              PROTECTED?                              
         BO    CTYF0060                 YES - SKIP                              
         MVI   8(R2),C'N'                                                       
         LA    RF,CTYRRLCH                                                      
         CR    R2,RF                                                            
         BNE   CTYF0051                                                         
         LTR   R0,R0                                                            
         BZ    CTYF0051                                                         
         MVI   8(R2),C'Y'                                                       
         B     CTYF0052                                                         
CTYF0051 TM    0(R4),X'80'              REPLACE FIELD?                          
         BNO   *+8                                                              
         MVI   8(R2),C'Y'                                                       
CTYF0052 FOUT  (R2)                                                             
*                                                                               
CTYF0060 DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                    SECOND Y/N FIELD                        
         TM    1(R2),X'20'              PROTECTED?                              
         BO    CTYF0070                 YES - SKIP                              
         MVI   8(R2),C'N'                                                       
         TM    0(R4),X'40'              REPLACE FIELD?                          
         BNO   *+8                                                              
         MVI   8(R2),C'Y'                                                       
         FOUT  (R2)                                                             
*                                                                               
CTYF0070 DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                    NEXT LINE                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                    SKIP LABEL                              
         LA    R3,L'FLDTAB(R3)          NEXT FIELD IN TABLE                     
         LA    R4,1(R4)                 NEXT STATUS BYTE IN ELEM                
         B     CTYF0020                                                         
*                                                                               
CTYF0100 DS    0H                       DISPLAY PROFILE BITS                    
         LR    RF,RA                                                            
         USING TWAD,RF                                                          
*                                                                               
*   FIELD IS NOW AVAILABLE TO EVERYONE.  PER B. POTASKY/K FAHEY.                
*                                        FEB 19/02.   BILL UHR                  
***      CLI   TWAOFFC,C'*'             DDS TERMINAL?                           
***      BNE   *+8                      NO - SKIP                               
         NI    CTYPRFAH+1,X'FF'-X'20'   YES - UNPROTECT ADDT'L OPTIONS          
         DROP  RF                                                               
         LA    R2,CTYPRFKH                                                      
         LA    R3,8(R2)                                                         
         BAS   RE,DOPROF                                                        
         FOUT  (R2)                                                             
         LA    R4,1(R4)                                                         
         LA    R2,CTYPRFWH                                                      
         LA    R3,8(R2)                                                         
         BAS   RE,DOPROF                                                        
         FOUT  (R2)                                                             
         LA    R4,1(R4)                                                         
         LA    R2,CTYPRFAH                                                      
         LA    R3,8(R2)                                                         
         BAS   RE,DOPROF                                                        
         LA    R4,1(R4)                                                         
         LA    R3,8(R3)                                                         
         BAS   RE,DOPROF                                                        
         FOUT  (R2)                                                             
*                                                                               
CTYFMTX  DS    0H                                                               
         B     EXXMOD              EXIT                                         
*                                                                               
* ROUTINE TO DISPLAY PROFILE BITS                                               
* NOTE: R4 MUST ADDRESS BYTE TO BE DISPLAYED (8 OPTION BITS/BYTE)               
*       R3 MUST ADDRESS 8-BYTE DISPLAY AREA ON SCREEN                           
*                                                                               
DOPROF   DS    0H                                                               
         MVC   0(8,R3),=C'NNNNNNNN'                                             
         TM    0(R4),X'80'                                                      
         BZ    *+8                                                              
         MVI   0(R3),C'Y'                                                       
         TM    0(R4),X'40'                                                      
         BZ    *+8                                                              
         MVI   1(R3),C'Y'                                                       
         TM    0(R4),X'20'                                                      
         BZ    *+8                                                              
         MVI   2(R3),C'Y'                                                       
         TM    0(R4),X'10'                                                      
         BZ    *+8                                                              
         MVI   3(R3),C'Y'                                                       
         TM    0(R4),X'08'                                                      
         BZ    *+8                                                              
         MVI   4(R3),C'Y'                                                       
         TM    0(R4),X'04'                                                      
         BZ    *+8                                                              
         MVI   5(R3),C'Y'                                                       
         TM    0(R4),X'02'                                                      
         BZ    *+8                                                              
         MVI   6(R3),C'Y'                                                       
         TM    0(R4),X'01'                                                      
         BZ    *+8                                                              
         MVI   7(R3),C'Y'                                                       
DOPROFX  BR    RE                                                               
*                                                                               
*- EDIT ROUTINE (ADD/CHANGE)                                                    
CTYEDT   EQU   *                                                                
         CLI   BACT,C'A'           ADD?                                         
         BE    CTYE0020            YES                                          
*                                                                               
         L     R5,AIOAREA          SAVE A(IOAREA)                               
         LA    RE,REC2                                                          
         ST    RE,AIOAREA                                                       
         BAS   RE,GETREC           READ OLD REC INTO REC2                       
*                                                                               
         ST    R5,AIOAREA          POINT BACK TO REC.                           
*                                                                               
*- BUILD FIXED PART OF RECORD                                                   
CTYE0020 EQU   *                                                                
         LA    R4,27+2+1+4+RCTYELMX    KEY/LEN/CONTROL/LINK/X'01' EL            
         STCM  R4,3,RCTYLEN        SET UP RECORD LEN   (..LL)                   
*                                                                               
         XC    RCTY1CDE(RCTYELMX),RCTY1CDE    START CLEAN                       
*                                                                               
         MVI   RCTY1CDE,X'01'      EL CODE                                      
         MVI   RCTY1LEN,RCTYELMX   EL LEN                                       
*                                                                               
         LA    R2,CTYDESCH                                                      
         CLI   5(R2),0                                                          
         BE    FLERR1              REQUIRED INPUT NOT GIVEN                     
*                                                                               
         MVC   RCTYDESC,8(R2)      NAME TO RECORD                               
         OC    RCTYDESC,SPACES                                                  
*                                                                               
* VALIDATE STANDARD COMMENT                                                     
*                                                                               
         LA    R2,CTYCMMTH                                                      
         CLI   5(R2),0                                                          
         BE    CTYE0030            OPTIONAL                                     
         XC    KEY,KEY                                                          
         MVI   KEY,X'2E'           STANDARD COMMENT                             
         MVC   KEY+15(2),RCTYKREP                                               
         MVC   KEY+17(2),=X'FFFF'                                               
         MVC   KEY+19(8),8(R2)                                                  
         OC    KEY+19(8),SPACES                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   FLERR7                                                           
*                                                                               
         MVC   RCTYCMMT,8(R2)      NAME TO RECORD                               
         OC    RCTYCMMT,SPACES                                                  
*                                                                               
*-VALIDATE FORMAT FIELDS & BUILD FORMAT/TEXT ELEMENTS                           
*                                                                               
CTYE0030 DS    0H                                                               
         LA    R3,FLDTAB                FIELD TABLE                             
         LA    R2,CTYRNMH               FIRST FIELD                             
         LA    R4,WORK2                 BUILD '10' ELEMENT HERE                 
         MVI   0(R4),X'10'              ELEM CODE                               
         MVI   1(R4),RCTYFLQ            ELEM LENGTH                             
         LA    R4,2(R4)                 FIRST FIELD STATUS BYTE                 
CTYE0040 DS    0H                                                               
         CLI   0(R3),X'FF'              END OF FIELDS?                          
         BE    CTYE0050                 YES                                     
         CLI   5(R2),0                  DO WE HAVE REPLACEMNT TEXT              
         BE    *+8                      NO - SKIP                               
         BAS   RE,TXTEL                 YES - CREATE TEXT ELEM                  
         BAS   RE,VALYN                 VALIDATE Y/N & GET STATUS               
         MVC   0(1,R4),HALF             PUT STAUS INTO ELEM                     
         LA    R3,L'FLDTAB(R3)          NEXT FIELD IN TABLE                     
         LA    R4,1(R4)                 NEXT BYTE IN ELEMENT                    
         B     CTYE0040                 DO IT AGAIN                             
*                                                                               
**** NOTE: CONTYPE RECORD OPTION BITS DOCUMENTED IN REGENCTY ****               
*                                                                               
CTYE0050 DS    0H                       VALIDATE PROFILE BITS                   
         LA    R2,CTYPRFKH                                                      
         LA    R6,CTYPRFK                                                       
         BAS   RE,PUTPROF                                                       
         LA    R4,1(R4)                                                         
         LA    R2,CTYPRFWH                                                      
         LA    R6,CTYPRFW                                                       
         BAS   RE,PUTPROF                                                       
         LA    R4,1(R4)                                                         
         LA    R2,CTYPRFAH                                                      
         LA    R6,CTYPRFA                                                       
         BAS   RE,PUTPROF                                                       
         LA    R4,1(R4)                                                         
         LA    R6,CTYPRFA+8                                                     
         BAS   RE,PUTPROF                                                       
*                                                                               
         GOTO1 VADDELEM,DMCB,RCTYREC,WORK2  ADD FORMAT ELEMENT                  
*                                                                               
*                                                                               
*- BUILD ACTIVITY DATE ELEMENT                                                  
         XC    WORK2(20),WORK2                                                  
         MVC   WORK2(2),=X'EF0C'   EL CODE & LENGTH                             
         MVC   WORK2+2(3),TODAY    CREATED TODAY                                
         MVC   WORK2+5(3),TODAY    UPDATED TODATE                               
         MVC   WORK2+8(1),BACT     REASON CODE                                  
*                                                                               
         CLI   BACT,C'A'           ADD?                                         
         BE    CTYE0340                                                         
*                                                                               
         GOTO1 VGETEL,DMCB,(X'EF',REC2),DMCB+8                                  
         CLI   DMCB,X'FF'                                                       
         BE    CTYE0340            NO ELEM. TREAT AS ADD                        
*                                                                               
         L     RF,DMCB+8           A(ELEMENT)                                   
         MVC   WORK2+2(3),2(RF)    KEEP CREATE DATE ON CHANGE                   
*                                                                               
CTYE0340 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,RPTPREC,WORK2     ADD ACTIVITY ELEMENT             
*                                                                               
         B     FLFILE              UPDATE THE FILE                              
*                                                                               
*                                                                               
* ROUTINE TO CONVERT 8 Y/N TYPE OPTIONS ON SCREEN TO 1 BYTE IN BINARY           
* NOTE: R6 ADDRESSES 8-BYTE SCREEN FIELD WITH OPTIONS                           
*       R4 ADDRESSES BYTE TO HAVE BINARY WRITTEN TO                             
PUTPROF  DS    0H                                                               
         LA    R1,8                     8 BITS                                  
         ZIC   R3,=X'80'                FIRST BIT HEX VALUE                     
PUTP10   CLI   0(R6),C'N'                                                       
         BE    PUTP20                                                           
         CLI   0(R6),C'Y'                                                       
         BNE   FLERR2                                                           
         STC   R3,HALF                                                          
         OC    0(1,R4),HALF             BIT VALUE INTO RECORD                   
PUTP20   SRL   R3,1                     NEXT BIT VALUE                          
         LA    R6,1(R6)                 NEXT BIT ON SCREEN                      
         BCT   R1,PUTP10                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*- DEVELOPMENT CONTRACT TYPE RECORD (X'3B')                                     
*                                                                               
DEVLCTYP EQU   *                                                                
         CLI   BFMTSW,0            0=FORMAT (DISPLAY)                           
         BNE   DEVLEDT                                                          
*                                                                               
*- FORMAT DISPLAY                                                               
DCTFMT   EQU   *                                                                
         BAS   RE,GETREC           READ IN RECORD                               
*                                                                               
         LA    R2,DCTDESCH         DISPLAY DESCRIPTION                          
         FOUT  (R2)                                                             
         MVC   8(L'RDCTDESC,R2),RDCTDESC                                        
*                                                                               
         B     EXXMOD              EXIT                                         
         SPACE                                                                  
*                                                                               
*- EDIT ROUTINE (ADD/CHANGE)                                                    
DEVLEDT  EQU   *                                                                
         CLI   BACT,C'A'           ADD?                                         
         BE    DCTE0020            YES                                          
*                                                                               
         L     R5,AIOAREA          SAVE A(IOAREA)                               
         LA    RE,REC2                                                          
         ST    RE,AIOAREA                                                       
         BAS   RE,GETREC           READ OLD REC INTO REC2                       
*                                                                               
         ST    R5,AIOAREA          POINT BACK TO REC.                           
*                                                                               
*- BUILD FIXED PART OF RECORD                                                   
DCTE0020 EQU   *                                                                
         LA    R4,27+2+1+4+RDCTELMX    KEY/LEN/CONTROL/LINK/X'01' EL            
         STCM  R4,3,RDCTLEN        SET UP RECORD LEN   (..LL)                   
*                                                                               
         XC    RDCT1CDE(RDCTELMX),RDCT1CDE    START CLEAN                       
*                                                                               
         MVI   RDCT1CDE,X'01'      EL CODE                                      
         MVI   RDCT1LEN,RDCTELMX   EL LEN                                       
*                                                                               
         LA    R2,DCTDESCH                                                      
         CLI   5(R2),0                                                          
         BE    FLERR1              REQUIRED INPUT NOT GIVEN                     
*                                                                               
         MVC   RDCTDESC,8(R2)      NAME TO RECORD                               
         OC    RDCTDESC,SPACES                                                  
*                                                                               
*- BUILD ACTIVITY DATE ELEMENT                                                  
         XC    WORK2(20),WORK2                                                  
         MVC   WORK2(2),=X'EF0C'   EL CODE & LENGTH                             
         MVC   WORK2+2(3),TODAY    CREATED TODAY                                
         MVC   WORK2+5(3),TODAY    UPDATED TODATE                               
         MVC   WORK2+8(1),BACT     REASON CODE                                  
*                                                                               
         CLI   BACT,C'A'           ADD?                                         
         BE    DCTE0340                                                         
*                                                                               
         GOTO1 VGETEL,DMCB,(X'EF',REC2),DMCB+8                                  
         CLI   DMCB,X'FF'                                                       
         BE    DCTE0340            NO ELEM. TREAT AS ADD                        
*                                                                               
         L     RF,DMCB+8           A(ELEMENT)                                   
         MVC   WORK2+2(3),2(RF)    KEEP CREATE DATE ON CHANGE                   
*                                                                               
DCTE0340 EQU   *                                                                
         GOTO1 VADDELEM,DMCB,RDCTREC,WORK2     ADD ACTIVITY ELEMENT             
*                                                                               
         B     FLFILE              UPDATE THE FILE                              
         EJECT                                                                  
*                                                                               
*        CHECK NEW TO OLD ON A CHANGE                                           
*                                                                               
FLFILE   CLI   BACT,C'A'           TEST ADD                                     
         BE    FLADD                                                            
*                                                                               
* CHANGE - READ REC THEN WRITE NEW                                              
*                                                                               
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
         B     FLFLEXT                                                          
*                                                                               
FLADD    BAS   RE,ADDREC                                                        
         MVC   BSVDA,KEY           SAVE DISK ADDRESS                            
*                                                                               
         CLI   BREC,X'32'          CONTRACT TYPE                                
         BNE   FLFLEXT                                                          
         BAS   RE,CTYMAST                                                       
*                                                                               
FLFLEXT  DS    0H                                                               
         CLI   UPDATCON,1          UPDATE CONTROL FILE?                         
         BNE   FLFLEXT2            NO  - DON'T UPDATE CONTROL FILE              
*                                                                               
         TM    SVPGPBIT+2,X'20'    PROFILE = UPDATE CONTROL FILE?               
         BNO   FLFLEXT2            NO  - SKIP UPDATE PHASE                      
*                                                                               
         GOTO1 VSWITCH,DMCB,(X'0A',X'FFFFFFFF'),0                               
         CLI   4(R1),0             SWITCHED OKAY?                               
         BE    *+6                 YES - ACCESS TO CONTROL SYSTEM               
         DC    H'0'                GRANTED: SHOULD STILL BE OKAY                
*                                                                               
         XC    GDSKADDR,GDSKADDR   CLEAR DISK ADDRESS                           
         XC    DMCB+8(4),DMCB+8    CLEAR P3                                     
         CLI   BACT,C'A'           ADD IN PROGRESS?                             
         BE    FLFL0100            YES -                                        
         MVI   DMCB+11,X'FF'       NO  - CHANGE: SET INDICATOR                  
FLFL0100 EQU   *                                                                
         GOTO1 =A(UPCONFIL),DMCB,GOLDKEYS,GNEWKEYS,,RR=Y                        
*                                                                               
*   NOW SWITCH BACK TO REP IN PROCESS                                           
*                                                                               
         GOTO1 VSWITCH,DMCB,=C'REP ',0                                          
         CLI   4(R1),0             SWITCHED BACK OKAY?                          
         BE    *+6                 YES -                                        
         DC    H'0'                NO  - ABORT                                  
*                                                                               
FLFLEXT2 EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
***************************************************************                 
* CTYMAST-SUBROUTINE                                                            
*              ADD MASTER RECORD FOR CONTRACT TYPE                              
***************************************************************                 
CTYMAST  NTR1                                                                   
*                                                                               
         OC    SVREPSUB,SVREPSUB    MASTER RECORD?                              
         BZ    CTMSEXT                                                          
*                                                                               
         MVC   KEY(27),REC          RESTORE KEY FROM REC                        
         MVC   KEY+24(2),SVREPMST   PUT MASTER CODE                             
         MVC   KEY2,KEY             SAVE OFF KEY                                
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY2(27),KEY       IS MASTER RECOR ON FILE?                      
         BE    CTMSEXT            YES  EXIT                                     
*ELSE                                                                           
         MVC   REC(27),KEY2       RESTORE KEY                                   
*                                                                               
         BAS   RE,ADDREC          ADD RRECORD                                   
*                                                                               
CTMSEXT  DS    0H                                                               
         XIT1                                                                   
* END OF CTYMAST                                                                
***************************************************************                 
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
*                                                                               
* ROUTINE TO CREATE X'12' REPLACEMENT TEXT ELEMENTS                             
*   R2 = CORRESPONDING TEXT FIELD HEADER                                        
*   R3 = FLD TAB ENTRY                                                          
*                                                                               
TXTEL    NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         ZIC   R1,1(R3)                 LENGTH OF FIELD FROM TABLE              
         LTR   R1,R1                                                            
         BZ    TXTELX                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R6),8(R2)            PUT TEXT IN ELEM                        
         MVI   0(R6),X'12'              ELEMENT CODE                            
         LA    R1,4(R1)                                                         
         STC   R1,1(R6)                 ELEMENT LENGTH                          
         MVC   2(1,R6),0(R3)            FIELD LABEL                             
         GOTO1 VADDELEM,DMCB,RCTYREC,WORK  ADD REPLACEMENT TEXT ELEMENT         
TXTELX   XIT1                                                                   
*                                                                               
* ROUTINE TO VALIDATE Y/N FIELDS FOR CTY RECORD REPLACEMENT TEXT FIELDS         
*   R2 = CORRESPONDING TEXT FIELD HEADER                                        
*   RETURNS APPROPRIATE HEX FLAG VALUE IN HIGH BYTE OF HALF                     
*                                                                               
VALYN    DS    0H                                                               
         MVI   HALF,0                                                           
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                    FIRST Y/N FIELD                         
         TM    1(R2),X'20'              PROTECTED?                              
         BO    VALYN100                 YES - SKIP                              
         CLI   5(R2),0                  ZERO LEN?                               
         BE    FLERR1                   NOT ALLOWED                             
         CLI   8(R2),C'Y'                                                       
         BE    VALYN50                                                          
         CLI   8(R2),C'N'                                                       
         BE    VALYN100                                                         
         B     FLERR2                                                           
VALYN50  OI    HALF,X'80'               FLAG REPL FIELD (CONTRACT)              
VALYN100 ZIC   R1,0(R2)                                                         
         AR    R2,R1                    SECOND Y/N FIELD                        
         TM    1(R2),X'20'              PROTECTED?                              
         BO    VALYNX                                                           
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         CLI   8(R2),C'Y'                                                       
         BE    VALYN120                                                         
         CLI   8(R2),C'N'                                                       
         BE    VALYNX                                                           
         B     FLERR2                                                           
VALYN120 OI    HALF,X'40'               FLAG REPL FIELD (WRKSHT)                
VALYNX   DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                    NEXT ROW OF FIELDS                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                    SKIP FIELD LABEL                        
         BR    RE                                                               
*                                                                               
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
* ROUTINE TO CLEAR STATION LIST FROM SCREEN                                     
*                                                                               
CLRBOTSC NTR1                                                                   
         LA    R2,LASBEGH          FIRST PRINT LINE FOR STATIONS                
         LA    R3,LASMORH          LAST SCREEN FIELD                            
*                                                                               
CLRSCR10 ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE?                                 
         BNH   CLRSCR10            NO, BEFORE OR ON LAST FIELD                  
CBSX     XIT1                                                                   
         EJECT                                                                  
       ++INCLUDE RECLRFLD                                                       
         SPACE 2                                                                
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
*                                                                               
FLERR7   EQU   *                   CAN'T INSERT (SCREEN FULL)                   
         MVC   LFMMSG(L'INVLSCMT),INVLSCMT                                      
         B     MYERR                                                            
*                                                                               
FLERR8   EQU   *                   CONTROL SYSTEM NOT AVAILABLE                 
         MVC   LFMMSG(L'CTLNOAVL),CTLNOAVL                                      
         B     MYERR                                                            
*                                                                               
MYERR    EQU   *                   MESSAGE ALREADY IN HEADER                    
         FOUT  LFMMSGH                                                          
         MVI   ERRAREA,X'FF'       ERRORS FOUND SWITCH                          
         B     EXIT                                                             
***>>>>>>>>>>>  GENDIR/GENFIL IO CONTROL                                        
*                  COMMUNICATION WITH DATA MANAGER (GENDIR)                     
*                                                                               
GREAD    MVC   COMMAND,=C'DMREAD'                                               
         B     GDIRCTRY                                                         
GSEQ     MVC   COMMAND,=C'DMRSEQ'                                               
         B     GDIRCTRY                                                         
GHIGH    MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     GDIRCTRY                                                         
GADD     MVC   COMMAND,=C'DMADD '                                               
         B     GDIRCTRY                                                         
GWRITE   MVC   COMMAND,=C'DMWRT '                                               
         B     GDIRCTRY                                                         
GDIRCTRY NTR1                                                                   
         IC    R4,DMINBTS                                                       
         IC    R3,TERMNAL                                                       
         MVC   GKEYSAVE,GKEY                                                    
         GOTO1 VDATAMGR,DMCB,((R4),COMMAND),=C'GENDIR',GKEYSAVE,GKEY            
         B     GDMCHECK                                                         
*                  COMMUNICATION WITH DATA MANAGER (GENFIL)                     
*                                                                               
GGETREC  MVC   COMMAND,=C'GETREC'                                               
         B     GFILE                                                            
GPUTREC  MVC   COMMAND,=C'PUTREC'                                               
         B     GFILE                                                            
GADDREC  MVC   COMMAND,=C'ADDREC'                                               
         B     GFILE                                                            
GFILE    NTR1                                                                   
         LA    R2,GKEY+36                                                       
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,GKEY                                                          
         IC    R3,TERMNAL                                                       
         IC    R4,DMINBTS                                                       
         GOTO1 VDATAMGR,DMCB,((R4),COMMAND),=C'GENFIL',                X        
               (R2),AIOAREA,((R3),DMWORK),0                                     
*                  DATA MANAGER ERRORS AND EXIT                                 
                                                                                
GDMCHECK MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   GDMERRS                                                          
         XIT1                                                                   
GDMERRS  L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
***>>>>>>>>>>>  GENDIR/GENFIL IO CONTROL                                        
*                                                                               
         SPACE 2                                                                
ZEROS    DC    30C'0'                                                           
BLANKS   DC    CL24' '                                                          
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
NOTSELF  DC    C'CAN''T ENTER YOUR OWN REP CODE IN THIS FIELD.'                 
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
INVLSCMT DC    C'STANDARD COMMENT CODE NOT FOUND.'                              
CTLNOAVL DC    C'CONTROL SYSTEM ACCESS NOT ALLOWED'                             
         DS    0H                                                               
         SPACE 2                                                                
RELO     DS    A                   RELOCATION FACTOR                            
KEY2     DS    CL32                EXTRA KEY HOLDER                             
GKEY     DS    CL40                EXTRA GENDIR KEY HOLDER                      
GKEYSAVE DS    CL40                EXTRA GENDIR KEY SAVE                        
SVDSKDA  DS    F                        SAVED D/A                               
SVSTA    DS    CL5                      SAVED STATION                           
STACNT   DS    XL2                      # STATIONS CHANGED                      
LEAVDATE DS    XL2                                                              
UPDATCON DS    XL1                                                              
*                                                                               
       ++INCLUDE REGENFLD                                                       
         EJECT                                                                  
       ++INCLUDE RELFMGEN                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENREG                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENOWN                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENCMT                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENPTP                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENCTY                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENDCT                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENOFF                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENAGY                                                       
         ORG   REC                                                              
       ++INCLUDE REGENAGY2                                                      
         ORG                                                                    
*              DDCOMFACS                                                        
         PRINT OFF                                                              
       ++INCLUDE FATWA                                                          
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE RELFMTWA                                                       
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMFED                                                       
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFME0D                                                       
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFME3D                                                       
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMDFD                                                       
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMDED                                                       
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMD5D                                                       
SPSAL    DSECT                                                                  
       ++INCLUDE GEGENSPSAL                                                     
         EJECT                                                                  
T80401   CSECT                                                                  
PASSKEYS NTR1  LABEL=*,BASE=*                                                   
         L     R3,0(R1)            SET A(KEY STORAGE AREA)                      
         L     R4,4(R1)            SET A(RECORD)                                
         USING RPTPREC,R4                                                       
*                                                                               
*   BUILD PRIMARY KEY FOR THIS RECORD                                           
*                                                                               
         XC    0(GSPLDLEN,R3),0(R3)  GENERATE PRIMARY KEY                       
         MVI   GSPLKTYP-GSPLKEY(R3),GSPLRECQ                                    
         MVI   GSPLKSTP-GSPLKEY(R3),X'02'                                       
*                                  INSERT SUBREC TYPE INTO KEY                  
         MVC   GSPLKREP-GSPLKEY(02,R3),RPTPKREP                                 
*                                  INSERT REP CODE    INTO KEY                  
         MVC   GSPLKPP-GSPLKEY(03,R3),RPTPKREC                                  
*                                  INSERT P-P CODE INTO PASSIVE                 
*                                                                               
         LA    R3,KEYLEN(R3)       R3=A(NEXT PASSIVE POINTER)                   
         XC    0(GSPLDLEN,R3),0(R3)  GENERATE P/P PASSIVES                      
         MVI   GSPLPTYP-GSPLKEY(R3),GSPLPTYQ                                    
         MVI   GSPLPSTP-GSPLKEY(R3),X'02'                                       
*                                  INSERT SUBREC TYPE INTO KEY                  
         MVC   GSPLPREP-GSPLKEY(02,R3),RPTPKREP                                 
*                                  INSERT REP CODE    INTO KEY                  
         MVC   GSPLPNAM-GSPLPKEY(20,R3),RPTPNAME                                
*                                  INSERT P-P NAME INTO PASSIVE                 
         MVC   GSPLPCOD-GSPLPKEY(03,R3),RPTPKREC                                
*                                  INSERT P-P CODE INTO PASSIVE                 
         MVC   GSPLPOFF-GSPLPKEY(02,R3),RPTPOFF                                 
*                                  INSERT P-P OFFC INTO PASSIVE                 
*                                                                               
         LA    R3,KEYLEN(R3)       R3=A(NEXT PASSIVE POINTER)                   
         XC    0(KEYLEN,R3),0(R3)                                               
*                                                                               
*   GENERATE SECOND PASSIVE KEY FOR THIS RECORD                                 
*                                                                               
         XC    0(GSPLDLEN,R3),0(R3)  GENERATE P/P PASSIVES                      
         MVI   GSPLPTYP-GSPLKEY(R3),GSPLPTYQ                                    
         MVI   GSPLPSTP-GSPLKEY(R3),X'04'                                       
*                                  INSERT SUBREC TYPE INTO KEY                  
         MVC   GSPLPREP-GSPLKEY(02,R3),RPTPKREP                                 
*                                  INSERT REP CODE    INTO KEY                  
         MVC   GSP2POFF-GSPLPKEY(02,R3),RPTPOFF                                 
*                                  INSERT P-P OFFC INTO PASSIVE                 
         MVC   GSP2PNAM-GSPLPKEY(20,R3),RPTPNAME                                
*                                  INSERT P-P NAME INTO PASSIVE                 
         MVC   GSP2PCOD-GSPLPKEY(03,R3),RPTPKREC                                
*                                  INSERT P-P CODE INTO PASSIVE                 
         XIT1                                                                   
KEYLEN   EQU   40                                                               
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
NEW71REC NTR1  LABEL=*,BASE=*                                                   
         LA    R4,REC                                                           
         USING RPTPREC,R4                                                       
         XC    GNEWREC,GNEWREC     CLEAR RECORD AREA                            
         MVC   GNEWREC(32),GNEWKEYS                                             
*                                  INSERT NEW PRIMARY KEY                       
         MVI   GNEWREC+33,138      SET RECORD LENGTH                            
         MVI   GNEWREC+42,X'01'                                                 
*                                  SET ELEMENT CODE                             
         MVI   GNEWREC+43,GSPLPPL                                               
*                                  SET ELEMENT LENGTH                           
         MVC   GNEWREC+44(20),RPTPNAME                                          
*                                  INSERT POINT PERSON NAME                     
         MVC   GNEWREC+64(2),RPTPOFF                                            
*                                  INSERT POINT PERSON OFFICE                   
         OC    GNEWREC+81(50),SPACES                                            
*                                  SET EMAIL FIELD TO SPACE                     
         LA    R6,RPTPREC                                                       
         GOTO1 VGETEL,DMCB,(X'20',RPTPREC),DMCB+8                               
         CLI   DMCB,X'FF'                                                       
         BE    NW710020            NO ELEMENT                                   
*                                                                               
U        USING RPTPEMEM,R6         POINTPERSON EMAIL ADDRESS                    
         L     R6,DMCB+8           A(1ST ELEMENT)                               
         ZIC   RF,1(R6)            GET L(ELEMENT)                               
         SH    RF,=H'3'            MINUS CONTROL + 1 FOR EX                     
         EX    RF,NW710010         MOVE EMAIL ADDR BY LENGTH                    
         B     NW710020                                                         
NW710010 MVC   GNEWREC+81(0),U.RPTPEMAL                                         
         DROP  U                                                                
NW710020 EQU   *                                                                
         OC    GNEWREC+69(12),SPACES                                            
*                                  SET FAX   FIELD TO SPACE                     
         LA    R6,RPTPREC                                                       
         GOTO1 VGETEL,DMCB,(X'21',RPTPREC),DMCB+8                               
         CLI   DMCB,X'FF'                                                       
         BE    NW710040            NO ELEMENT                                   
*                                                                               
         L     R6,DMCB+8           A(1ST ELEMENT)                               
U        USING RPTPFXEM,R6         POINTPERSON FAX   ADDRESS                    
         MVC   GNEWREC+69(12),U.RPTPFXFX                                        
*                                  INSERT FAX NUMBER INTO ELEMENT               
NW710040 EQU   *                                                                
         TM    U.RPTPFXFG,X'40'    EMAIL PREFERENCE?                            
         BNO   NW710060            NO                                           
         OI    GNEWREC+68,X'40'    YES - SET EMAIL PREFERENCE                   
         B     NW710080                                                         
NW710060 EQU   *                                                                
         OI    GNEWREC+68,X'20'    SET FAX PREFERENCE                           
NW710080 EQU   *                                                                
         B     NW710900                                                         
         DROP  U                                                                
*                                                                               
NW710900 EQU   *                                                                
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*    P1 / R2  ->  OLD KEYS                                                      
*    P2 / R3  ->  NEW KEYS                                                      
*    P3 / R4  ->  FLAG: NON-ZERO = CHANGE.  ZERO = ADD.                         
*                                                                               
UPCONFIL NTR1  LABEL=*,BASE=*                                                   
         LM    R2,R4,0(R1)         LOAD A(OLD/NEW KEYS, FLAG)                   
*                                                                               
         OC    0(3,R3),0(R3)       KEY IN NEW SLOT?                             
         BZ    UCON0020            NO  -                                        
*                                  YES - ADD NEW SPSAL REC IF NEEDED            
         MVC   GKEY(32),0(R2)      TRY TO FIND OLD KEY                          
         GOTO1 GHIGH                                                            
***      CLC   GKEY(32),GKEYSAVE   FOUND?                                       
***      BE    UCON0020            YES - DON'T TRY TO READD                     
*                                  NO  - MUST BE ADDED                          
         BAS   RE,ADDSPSAL                                                      
*                                                                               
UCON0020 EQU   *                                                                
*                                                                               
         MVI   DMOUTBTS,0          FOR PROPER RECOVERY                          
         OI    DMINBTS,X'08'       PASS DELETES                                 
UCON0040 EQU   *                                                                
*                                                                               
*   1ST TEST:  IF OLD KEY NOT EQUAL NEW KEY:                                    
*        A.  OLD KEY IS MARKED FOR DELETE                                       
*        B.  NEW KEY IS EITHER ADDED OR REINSTATED WITH D/A                     
*                                                                               
         CLC   0(32,R2),0(R3)      SAME?                                        
         BNE   UCON0080            NO  -                                        
*                                  YES - CAN'T BE DELETED                       
         MVC   GKEY,0(R2)                                                       
         OI    DMINBTS,X'08'       PASS DELETES                                 
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
         GOTO1 GHIGH                                                            
*                                                                               
*   2ND TEST:  OLD KEY EQUAL NEW KEY: IS KEY ON FILE?                           
*        A.  IF ON FILE, KEY HAS TO BE ACTIVATED WITH D/A                       
*        B.  IF NOT ON FILE, KEY IS CONSIDERED AN 'ADD'                         
*                                                                               
         CLC   GKEY(32),GKEYSAVE   KEY FOUND?                                   
         BNE   UCON0080            NO  - CONSIDER IT AN 'ADD'                   
*                                                                               
         OC    GDSKADDR,GDSKADDR   DISK ADDRESS SET?                            
         BNZ   UCON0060            YES                                          
         MVC   GDSKADDR,GKEY+36    NO  - SAVE THE DISK ADDRESS                  
UCON0060 EQU   *                                                                
         MVI   GKEY+32,0           CLEAR STATUS OF KEY                          
         GOTO1 GWRITE              REWRITE KEY WITH NEW STATUS                  
         BAS   RE,UCON0800                                                      
         B     UCON0180                                                         
UCON0080 EQU   *                                                                
*                                                                               
* DIFFERENT                                                                     
         OC    0(3,R2),0(R2)       ADD?  NO KEY IN OLD SLOT?                    
         BZ    UCON0120            WRITE/RESTORE NEW KEY                        
*                                                                               
*                                  NO  - CHANGE.                                
*                                     FIRST DELETE OLD PTR                      
         MVC   GKEY,0(R2)                                                       
         OI    DMINBTS,X'08'       PASS DELETES                                 
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
         GOTO1 GHIGH                                                            
         CLC   GKEY(32),GKEYSAVE   KEY FOUND?                                   
         BNE   UCON0120            NO  - NOT ON FILE                            
         OC    GDSKADDR,GDSKADDR   DISK ADDRESS SET?                            
         BNZ   UCON0100            YES                                          
         MVC   GDSKADDR,GKEY+36    NO  - SAVE THE DISK ADDRESS                  
         OC    GDSKADDR,GDSKADDR   MUST BE A VALID DISK ADDRESS                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,UCON0800                                                      
UCON0100 EQU   *                                                                
         CLC   GKEY(32),GKEYSAVE                                                
         BNE   UCON0120                                                         
         MVI   GKEY+32,X'FF'                                                    
         GOTO1 GWRITE                                                           
         BAS   RE,UCON0800                                                      
* ADD NEW PTR                                                                   
UCON0120 DS    0H                                                               
*                                                                               
         OC    0(32,R3),0(R3)      ANY NEW KEY?                                 
         BZ    UCON0180            NO  - THIS IS A 'INACTIVATE'                 
         MVC   GKEY,0(R3)                                                       
         OI    DMINBTS,X'08'       PASS DELETES                                 
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
         GOTO1 GHIGH                                                            
         BAS   RE,UCON0800                                                      
         CLC   GKEY(32),GKEYSAVE                                                
*                                  KEY ON FILE?                                 
         BE    UCON0140            YES                                          
         MVC   GKEY,GKEYSAVE       NO  - RESET KEY SOUGHT                       
         MVC   GKEY+36(4),GDSKADDR INSERT DISK ADDRESS                          
         MVI   GKEY+32,0           CLEAR STATUS                                 
         GOTO1 GADD                                                             
         BAS   RE,UCON0800                                                      
         B     UCON0180                                                         
*                                                                               
UCON0140 EQU   *                                                                
         MVI   GKEY+32,0           CLEAR STATUS OF KEY                          
         MVC   GKEY+36(4),GDSKADDR SET DISK ADDR                                
         GOTO1 GWRITE              REWRITE KEY WITH NEW STATUS                  
         BAS   RE,UCON0800                                                      
*                                  UNDELETE KEY                                 
*                                  ADD NEW PTR                                  
*                                                                               
* NEXT POINTER                                                                  
UCON0180 LA    R2,40(R2)                                                        
         LA    R3,40(R3)                                                        
         OC    0(3,R2),0(R2)       ANY MORE 'OLD KEYS'?                         
         BNZ   UCON0040            YES - CONTINUE TO LOOP                       
         OC    0(3,R3),0(R3)       NO  - ANY MORE 'NEW KEYS'?                   
         BNZ   UCON0040            YES - CONTINUE TO LOOP                       
         MVI   DMOUTBTS,X'FD'      BOTH OLD AND NEW PROCESSED                   
         B     EXXIT                                                            
*                                                                               
UCON0800 TM    DMCB+8,X'FD'        ALL BUT DELETE                               
         BCR   8,RE                                                             
         DC    H'0'                                                             
*                                                                               
EXXIT    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
ADDSPSAL NTR1                                                                   
         XC    GKEY,GKEY                                                        
         MVC   GKEY(32),GNEWREC    MOVE KEY FROM RECORD                         
         OI    DMINBTS,X'08'       PASS DELETES                                 
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
         GOTO1 GHIGH               READ FOR THIS KEY                            
         CLC   GKEY(32),GKEYSAVE   KEY FOUND?                                   
         BE    ASPS0040            YES - GET ITS D/A                            
         LA    R2,REC2             NO  - SET A(IOAREA)                          
         ST    R2,AIOAREA                                                       
         XC    REC2(192),REC2                                                   
         MVC   REC2(144),GNEWREC   MOVE IN NEW RECORD                           
         GOTO1 GADDREC             ADD NEW RECORD                               
         MVC   GDSKADDR,GKEY       SAVE NEW REC D/A                             
         B     ASPS0100                                                         
ASPS0040 EQU   *                                                                
         MVC   GDSKADDR,GKEY+36                                                 
         MVI   GKEY+32,0           CLEAR STATUS OF KEY                          
         GOTO1 GWRITE              REWRITE KEY WITH NEW STATUS                  
         OI    DMINBTS,X'08'       PASS DELETES                                 
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
         GOTO1 GREAD               REREAD KEY FOR UPDATE                        
         LA    R2,REC2             NO  - SET A(IOAREA)                          
         ST    R2,AIOAREA                                                       
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
         GOTO1 GGETREC             RETRIEVE THE RECORD                          
         MVC   REC2(144),GNEWREC                                                
         GOTO1 GPUTREC             REWRITE NEW VERSION OF REC                   
         LTR   RB,RB                                                            
ASPS0100 EQU   *                                                                
         LA    R2,REC              NO  - RESET A(IOAREA)                        
         ST    R2,AIOAREA                                                       
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'238RELFM01S  12/16/02'                                      
         END                                                                    
