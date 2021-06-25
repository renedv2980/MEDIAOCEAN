*          DATA SET RELFM03    AT LEVEL 036 AS OF 10/26/09                      
*PHASE T80403A                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE REGENSTC                                                               
         TITLE 'RELFM03 (T80403) --- ADV/AGY RECORDS'                           
*                                                                               
*********************************************************************           
*                                                                   *           
*- RELFM03 -- PHASE T80403                                          *           
*                                                                   *           
* ----------------------------------------------------------------- *           
*  MOD LOG                                                          *           
*  -------                                                          *           
*  08/24/89  PJS  CHANGE PHASE CARD TO 'A' LEVEL                    *           
*                                                                   *           
*  02/02/90  PJS  ADDED NETWORK CONTRACT, POINT PERS, ETC TO PRD.   *           
*                                                                   *           
*  01MAR90   EFJ  ADDED PROFILES TO AGY REC                         *           
*                                                                   *           
*  AUG06/90  BU   THIRD BIT (BIT 2) OF PROFILE USED TO MAKE NETWORK *           
*                 CONTRACT, DESCRIPTION, AND POINT PERSON MANDATORY.*           
*                                                                   *           
*  JUL03/91 (MRR) >MOVE PRODUCT RECORD TO RELFM08                   *           
*                                                                   *           
*  MAR16/92 (MRR) --- SUPPORT CREDIT RATING FIELD ON AGY SCREEN     *           
*                     SUPPORT LIABILITY POSITION COMMENT # FIELD    *           
*  APR02/92 (BU ) --- CHANGE HARDCODED LITERALS PER KH              *           
*                     (IN RERISKTAB)                                *           
*                                                                   *           
*  APR09/92 (SKU) --- DISPLAY FIRST LINE OF AGY LIAB REC            *           
*                                                                   *           
*  APR28/92 (SKU) --- SUPPORT CAT CODE IN ADV REC                   *           
*                                                                   *           
*  JUN04/92 (SKU) --- SUPPORT EASYLINK AGY CONTRACT COPY PROFILE    *           
*                     AND FAX NUMBER FIELD (PROF 2)                 *           
*                     USES NEW AGENCY RECORD (REGENAGY2)            *           
*                     ADD AGENCY PHONE NUMBER SUPPORT               *           
*                                                                   *           
*  NOV02/92 (SKU) --- PROFILE FOR CATEGORY CODE REQUIRED WHEN       *           
*                     ADDING/CHANGING ADVERTISER RECORD             *           
*                                                                   *           
*  APR26/93 (SKU) --- FIX AGY2 REC ADD BUG                          *           
*                                                                   *           
*  JUN17/94 (SKU) --- ADD DARE AGENCY AND PASSIVE POINTER           *           
*                                                                   *           
*  FEB09/95 (BU ) --- AUTO-ASSIGN CODES.                            *           
*                                                                   *           
*  MAY08/95 (SKU) --- SET TRAP FOR AGENCY REC WRITE WITH WRONG KEY  *           
*                                                                   *           
*  AUG07/95 (SKU) --- PROF TO USE CREDIT RATING 2 AS DEFAULT        *           
*                                                                   *           
*  SEP08/95 (JR ) --- PROF ON ADD THE AGENCY EQUIV. FIELD TO AGY    *           
*                                                                   *           
*  OCT09/95 (SKU) --- PERMIT AUTO-ASSIGN FOR ACTION ADD ONLY        *           
*                                                                   *           
*  DEC20/95 (WSB) --- ADD TERRITORY FIELD PROCESSING                *           
*                                                                   *           
*  FEB13/96 (SKU) --- ALLOW 5 CHAR DARE AGENCY EQUIVALENT FOR TV    *           
*                                                                   *           
*  FEB15/96 (SKU) --- PROHIBIT ADDRESS CHANGE IF PROF +8 IS SET TO  *           
*                     Y                                             *           
*                                                                   *           
*  FEB26/96 (BU ) --- INCREASE EQUIV FIELD TO INCLUDE OFFICE        *           
*                                                                   *           
*  MAR09/96 (BU ) --- AGENCY: PETRY - DISPLAY SECONDARY ADDRESS,    *           
*                     AND CONTROL FLAG FOR DISPLAY ON UPDATE        *           
*                                                                   *           
*  APR03/96 (RHV) --- EXPAND PREVIOUS PETRY CHANGE TO ALLOW 34 BYTE *           
*                     ADDRESS FIELDS FOR ALL USERS & ADD CITY FIELD *           
*                                                                   *           
*  AUG01/96 (BU ) --- FIX 'REQUIRES CATEGORY CODE' ON DDS TERMINAL  *           
*                                                                   *           
*  JUN20/97 (RHV) --- SUPPORT AGY COMMENT                           *           
*                                                                   *           
*  SEP23/97 (BU ) --- FOX TV:  PERMIT EQUIV AGY IN DIFF FORMAT      *           
*                                                                   *           
*  DEC12/97 (JRD) --- IN CARE OF FLAG(BUYING SERVICE)               *           
*                                                                   *           
*  DEC18/97 (BU ) --- ADD 'AA' PASSIVE KEY FOR AGENCY (TERRITORY)   *           
*                                                                   *           
*  SEP15/98 (AST) --- PAXSON OFFICE ELEMENTS                        *           
*                                                                   *           
*  OCT28/98 (RHV) --- REVISE AGY2 REC                               *           
*                                                                   *           
*  JAN28/99 (AST) --- FIX PAXSON ELEMENTS ON ADDREC -- ADDAGY2 PROC *           
*                                                                   *           
*  MAR04/99 (AST) --- ADD X'20' ELEM FOR ALTERNATE AGENCY ADDRESS   *           
*                                                                   *           
*  MAR10/99 (RHV) --- REVISE AGY2 REC --> ADD 'BA' PASSIV PTR       *           
*                                                                   *           
*  SEP28/00 (BU ) --- PROHIBIT ENTRY OF 'ALL ' AS A CODE            *           
*                                                                   *           
*  FEB12/01 (RHV) --- AGY2 'BA' KEY MAINTENANCE FIX                 *           
*                                                                   *           
*  AUG06/01 (BU ) --- ACTIVATE EQUIV FIELDS FOR AGY/ADV             *           
*                                                                   *           
*  OCT24/01 (BU ) --- ADD DO NOT USE/DROP FLAGS FOR AGY/ADV         *           
*                                                                   *           
*  AUG22/06 (BU ) --- ABC (NTVSNY / CV) AGENCY EQUIV CODE           *           
*                                                                   *           
*  OCT31/06 (BU ) --- ADD 'SJ' TO AGENCY EQUIV CODE TESTING         *           
*                                                                   *           
*  AUG21/07 (BU ) --- CORRECT RAGYFLAG ORIGINAL SETTING ERROR       *           
*                                                                   *           
*  OCT23/09 (SMY) --- ADD "LOCKED" (Y/N) AGY SCREEN FIELD FOR       *           
*                       DDS TERMINALS ONLY - CAN BE USED TO TURN    *           
*                       OFF "CONTRACT ADDED" FLAG IN RAGYFLAG       *           
*                                                                   *           
*                                                                   *           
*                     ***  END TOMBSTONE  ***                       *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
T80403   CSECT                                                                  
         NMOD1 126,T80403,R9,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T804FFD,RA                                                       
*                                                                               
         ST    R3,RELO                                                          
*                                                                               
         MVC   KEY,BKEY                                                         
         MVI   KEY+27,0                                                         
         MVC   KEY+28(4),BSVDA                                                  
*                                                                               
         LA    R2,LFMLAST          POINT TO FIRST TITLE                         
         CLI   BREC,8                                                           
         BE    ADV                                                              
         CLI   BREC,10                                                          
         BE    AGY                                                              
         DC    H'0'                                                             
         SPACE                                                                  
RELO     DS    F                                                                
         TITLE 'T80403 - ADVERTISER RECORDS'                                    
ADV      CLI   BFMTSW,0            TEST FORMAT OR EDIT                          
         BNE   ADVEDT                                                           
*                                                                               
         BAS   RE,GETREC                                                        
         GOTO1 =A(ADVFRMAT),DMCB,(RC),RR=Y                                      
         B     EXXMOD                                                           
*                                                                               
***                          DATCON  FIELDS                                     
                                                                                
DCP1B0   DS    X             X 05   TODAYS DATE                                 
DATEIN   DS    CL8           MMMDD/YY                                           
CDATEIN  DC    XL2'0000'     COMPRESSED                                         
*                                                                               
DCP2B0   DS    X             X 02   TO COMPRESS OUTPUT                          
DATEOUT  DS    CL8           MMMDD/YY                                           
CDATEOUT DC    XL2'0000' COMPRESSED                                             
***                          DATCON FIELDS END                                  
                                                                                
                                                                                
         EJECT                                                                  
ADVEDT   MVC   REC+34(2),=X'0142'                                               
         MVC   REC+27(2),=Y(100)                                                
*                                                                               
         MVC   RADVCITY,SPACES                                                  
         MVC   RADVCLSS,SPACES                                                  
         MVC   RADVCATG,SPACES                                                  
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         LR    R1,RA                CHECK FOR DDS TERMINAL                      
         USING TWAD,R1                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   ADVE0060            ONLY DDS CAN SET CONTROL NUMBER              
*                                                                               
         DROP  R1                                                               
*                                                                               
         TM    SVPGPBIT,X'01'      AUTO-ASSIGN USER?                            
         BNO   ADVE0060            NO                                           
         LA    R5,LFMKEYH          A(KEY FIELD)                                 
         CLI   5(R5),4             INPUT = 4 CHARS?                             
         BNE   ADVE0060            NO  - CAN'T BE CONTROL                       
         CLC   LFMKEY(4),=C'0000'  CONTROL RECORD UPDATE?                       
         BNE   ADVE0060            NO                                           
         CLC   8(8,R2),=C'CONTROL='                                             
*                                  CONTROL SET REQUEST?                         
         BNE   ADVE0060            NO                                           
         ZIC   RF,5(R2)            YES - GET LENGTH OF INPUT                    
         SH    RF,=H'8'            SUBTRACT L(CONTROL= )                        
         LA    R6,16(R2)           SET A(INPUT)                                 
ADVE0020 EQU   *                                                                
         CLI   0(R6),C'0'          VALIDATE NUMBER                              
         BL    FLERR11             NOT NUMERIC                                  
         CLI   0(R6),C'9'          VALIDATE NUMBER                              
         BH    FLERR11             NOT NUMERIC                                  
         LA    R6,1(R6)            BUMP TO NEXT POSITION                        
         BCT   RF,ADVE0020         GO BACK FOR NEXT                             
         ZIC   RF,5(R2)            OK  - GET LENGTH OF INPUT AGAIN              
         SH    RF,=H'9'            SUBTRACT L(CONTROL= + 1 FOR EX)              
         LA    R6,16(R2)           SET A(INPUT) AGAIN                           
         EX    RF,ADVE0040         PACK INPUT BY LENGTH                         
         B     ADVE0050                                                         
ADVE0040 EQU   *                                                                
         PACK  DUB(8),0(0,R6)      PACK INPUT                                   
ADVE0050 EQU   *                                                                
         CVB   RF,DUB              CONVERT VALUE TO BINARY                      
         STCM  RF,3,RADVASSN       INSERT VALUE INTO RECORD                     
         LA    RF,7                RESET LENGTH OF INPUT FIELD                  
         STC   RF,5(R2)            RESET HEADER LENGTH                          
ADVE0060 EQU   *                                                                
         BAS   RE,MOVE                                                          
         MVC   RADVNAME,WORK                                                    
*                                                                               
         B     ADVE0080                                                         
*                                                                               
*  SPECIAL CODE TO DELETE AN ADV RECORD AND PASSIVE POINTER                     
*        ADVERTISER CODES CANNOT AND MUST NOT BE DELETED THIS WAY!!             
*                                                                               
*        CLI   BACT,C'A'           MUST BE ACTION CHANGE                        
*        BE    ADVE0080                                                         
*        CLC   RADVNAME(6),=C'DELETE'                                           
*        BNE   ADVE0080                                                         
*                                                                               
*        BAS   RE,READ                                                          
*        OI    KEY+27,X'80'        MARK DELETED                                 
*        BAS   RE,WRITE                                                         
*                                                                               
*        BAS   RE,GETREC           DEAL WITH PASSIVE POINTER                    
*        XC    KEY,KEY                                                          
*        MVI   KEY,X'88'                                                        
*        MVC   KEY+1(20),RADVNAME                                               
*        MVC   KEY+21(6),REC+21                                                 
*        BAS   RE,HIGH                                                          
*        CLC   KEYSAVE(27),KEY                                                  
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
*        MVI   DMOUTBTS,0          NO ERROR TESTS                               
*        OI    KEY+27,X'80'                                                     
*        BAS   RE,WRITE                                                         
*        BAS   RE,CHECK                                                         
*                                                                               
*        MVC   LFMMSG(20),=C'** RECORD DELETED **'                              
*        MVI   ERRAREA,X'FF'                                                    
*        LA    R2,LFMKEYH                                                       
*        B     EXIT                                                             
*                                                                               
ADVE0080 BAS   RE,NEXTUF                                                        
         BAS   RE,MOVE                                                          
         MVC   RADVCITY,WORK                                                    
*                                                                               
         BAS   RE,NEXTUF                                                        
         BAS   RE,MOVE                                                          
         MVC   RADVCATG,WORK                                                    
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,RADVLCD)                                   
*                                  INSERT NEW LAST CHANGED DATE                 
         GOTO1 VDATCON,DMCB,(5,0),(5,LADAVLC)                                   
         FOUT  LADAVLCH                                                         
*                                                                               
         GOTO1 =A(SETDONT),RR=Y    SET DO NOT FLAGS                             
         BZ    ADVE0090            NO ERROR                                     
         L     R2,DUB              ERROR - RESET A(FIELD IN ERROR)              
         B     FLERR2              EXIT WITH ERROR                              
ADVE0090 EQU   *                                                                
*                                                                               
         MVC   RADVKATZ,ORIGADV    REINSERT LAST ADVERTISER, IF ANY             
*                                                                               
         CLC   RADVCATG,SPACES                                                  
         BNE   ADVE0120                                                         
*                                                                               
         LA    R1,SVPGPBIT         PROGRAM PROFILE BITS                         
         TM    0(R1),X'02'         6TH BIT ON = CATEGORY CODE REQUIRED          
         BZ    ADVE0100            NOT REQUIRED                                 
**       LR    R1,RA                CHECK FOR DDS TERMINAL                      
**       USING TWAD,R1                                                          
**       CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
**       BE    ADVE0100            YES - IGNORE TEST                            
**       DROP  R1                                                               
         B     FLERR1              REQUIRED = ERROR                             
*                                                                               
ADVE0100 DS    0H                                                               
         ZIC   R0,0(R2)            BUMP TO CAT EXPANDED NAME                    
         AR    R2,R0                                                            
         MVC   8(L'RCTGNAME,R2),SPACES                                          
         FOUT  (R2)                CLEAR IT!                                    
         B     ADVE0140                                                         
*                                                                               
ADVE0120 EQU   *                                                                
         LA    R4,REC2             SAVE THE ADVERTISER RECORD                   
         LA    R5,REC                                                           
         BAS   RE,MOVEREC          MOVE REC TO REC2                             
*                                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0F'           CATEGORY RECORD                              
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),RADVCATG                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   FLERR2                                                           
*                                                                               
         BAS   RE,GETREC           EXPAND CATEGORY NAME                         
*                                                                               
         ZIC   R0,0(R2)            BUMP TO CAT EXP NAME                         
         AR    R2,R0                                                            
         MVC   8(L'RCTGNAME,R2),RCTGNAME                                        
         FOUT  (R2)                                                             
*                                                                               
         LA    R4,REC                                                           
         LA    R5,REC2                                                          
         BAS   RE,MOVEREC          MOVE REC2 BACK TO REC                        
*                                                                               
ADVE0140 EQU   *                                                                
         TM    SVPGPBIT+1,X'01'    CONVERT FLAG IS ON??                         
         BNO   ADVE0160            NO                                           
         GOTO1 =A(CONADV),DMCB,(RC),RR=Y                                        
         BZ    ADVE0160            OKAY RETURN                                  
         L     R2,DUB              A(ERROR FIELD)                               
         B     MYERR               ERROR RETURN - MESSAGE ALREADY SET           
*                                                                               
ADVE0160 EQU   *                                                                
         GOTO1 =A(PAXOFF),DMCB,(RC),LADOFC1H,RADVREC,RR=Y                       
*                                                                               
         BZ    ADVE0180            OKAY RETURN                                  
         L     R2,DUB              A(ERROR FIELD)                               
         B     MYERR               ERROR RETURN - MESSAGE ALREADY SET           
*                                                                               
* VALIDATE ALTERNATE ADDRESS AND CONTYPE                                        
*                                                                               
ADVE0180 GOTO1 =A(VALALTAD),DMCB,(RC),LADCTR1H,RADVREC,RR=Y                     
         BZ    ADVE0200            OKAY RETURN                                  
         L     R2,DUB              A(ERROR FIELD)                               
         B     MYERR               ERROR RETURN - MESSAGE ALREADY SET           
ADVE0200 EQU   *                                                                
*                                                                               
         CLI   BACT,C'A'           'ADD' ACTION?                                
         BNE   ADVE0220            NO                                           
         TM    SVPGPBIT,X'01'      AUTO-ASSIGN CODE NUMBER?                     
         BNO   ADVE0220            NO  - KEY FULLY SET UP                       
         GOTO1 =A(AUTOASSN),DMCB,(RC),RR=Y                                      
***      BAS   RE,AUTOASSN         YES - RETRIEVE NEXT CODE NUMBER              
ADVE0220 EQU   *                                                                
         B     FLFILE                                                           
         EJECT                                                                  
ADVE0240 CLI   BACT,C'A'           TEST ADD                                     
         BE    ADVE0260                                                         
         LA    R2,LFMLAST                                                       
         BAS   RE,NEXTUF           POINT TO NAME FIELD                          
         TM    4(R2),X'20'         TEST CHANGED                                 
         BO    EXXMOD              NO.                                          
* DELETE OLD PASSIVE POINTER                                                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'88'                                                        
         MVC   KEY+1(20),RADVNAME-RADVREC+REC2                                  
         MVC   KEY+21(6),REC2+21                                                
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'                                                     
         BAS   RE,WRITE                                                         
         BAS   RE,CHECK                                                         
*                                                                               
ADVE0260 XC    KEY,KEY             BUILD NEW PASSIVE POINTER                    
         MVI   KEY,X'88'                                                        
         MVC   KEY+1(20),RADVNAME                                               
         MVC   KEY+21(6),REC+21                                                 
         MVC   KEY+28(4),BSVDA                                                  
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         BAS   RE,HIGH                                                          
         BAS   RE,CHECK                                                         
         LA    RF,ADD                                                           
         CLC   KEYSAVE(27),KEY                                                  
         BNE   *+8                                                              
         LA    RF,WRITE                                                         
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         BASR  RE,RF                                                            
         BAS   RE,CHECK                                                         
         B     EXXMOD                                                           
         TITLE 'T80403 - AGENCY RECORDS'                                        
AGY      CLI   BFMTSW,0            TEST FORMAT OR EDIT                          
         BNE   AGYEDT                                                           
* FORMAT ROUTINE                                                                
         XC    ORIGFLAG,ORIGFLAG   CLEAR ORIGINAL FLAG AREA                     
         BAS   RE,GETREC                                                        
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RAGYNAM1,R2),RAGYNAM1                                        
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET VALIDATED BIT                            
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RAGYNAM2,R2),RAGYNAM2                                        
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET VALIDATED BIT                            
*                                                                               
         BAS   RE,NEXTUF                                                        
         XC    LAGAGY1,LAGAGY1                                                  
         MVC   8(L'RAGYADD1,R2),RAGYADD1                                        
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET VALIDATED BIT                            
*                                                                               
         BAS   RE,NEXTUF                                                        
         XC    LAGAGY2,LAGAGY2                                                  
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET VALIDATED BIT                            
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RAGYCITY,R2),RAGYCITY                                        
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET VALIDATED BIT                            
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RAGYSTAT,R2),RAGYSTAT                                        
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET VALIDATED BIT                            
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RAGYZIP,R2),RAGYZIP                                          
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET VALIDATED BIT                            
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RAGYPROS,R2),RAGYPROS                                        
         OC    RAGYPROS,RAGYPROS                                                
         BNZ   *+10                                                             
         MVC   8(L'RAGYPROS,R2),SPACES                                          
         FOUT  (R2)                                                             
*                                                                               
***      BAS   RE,NEXTUF           BUMP UP TO 'DO NOT DROP' FIELD               
***      MVI   8(R2),C'N'          SET 'DO NOT USE' TO NO                       
***      TM    RAGYFLAG,X'01'      'DO NOT DROP' SET?                           
***      BNO   AGYD020             NO                                           
***      MVI   8(R2),C'Y'          YES - SET TO YES                             
AGYD020  EQU    *                                                               
**       FOUT (R2)                 TRANSMIT 'DO NOT DROP' FIELD                 
*                                                                               
*   THE DO NOT SWITCH FIELD IS INACTIVE AT THIS TIME.  WHEN IT IS               
*       WANTED/NEEDED, REACTIVATE THE FIELD ON THE SCREEN, AND                  
*       REACTIVE THIS CODE AS WELL.                                             
*                                                                               
         BAS   RE,NEXTUF           BUMP UP TO 'DO NOT USE' FIELD                
         MVI   8(R2),C'N'          SET 'DO NOT USE' TO NO                       
         TM    RAGYFLAG,X'02'      'DO NOT USEP' SET?                           
         BNO   AGYD040             NO                                           
         MVI   8(R2),C'Y'          YES - SET TO YES                             
AGYD040  EQU    *                                                               
         FOUT (R2)                 TRANSMIT 'DO NOT USE' FIELD                  
*                                                                               
         BAS   RE,NEXTUF           BUMP UP TO 'CREDIT RISK' FIELD               
         MVC   8(1,R2),SPACES                                                   
         OC    RAGYRISK,RAGYRISK                                                
         BZ    AGYD100                                                          
         MVC   8(1,R2),RAGYRISK                                                 
         OI    8(R2),X'F0'                                                      
AGYD100  EQU   *                                                                
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         MVC   8(40,R2),SPACES                                                  
         OC    RAGYRISK,RAGYRISK                                                
         BZ    AGYD110                                                          
         LA    RE,RISKTAB                                                       
         ZIC   RF,RAGYRISK                                                      
         BCTR  RF,0                                                             
         MH    RF,=H'40'                                                        
         AR    RE,RF                                                            
         MVC   8(40,R2),0(RE)                                                   
AGYD110  EQU   *                                                                
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(3,R2),SPACES                                                   
         OC    RAGYLIAB,RAGYLIAB                                                
         BNZ   AGYD115                                                          
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)            BUMP TO DISPLAY FIELD                        
         AR    R2,RF                                                            
         MVC   8(50,R2),SPACES                                                  
         B     AGYD120                                                          
AGYD115  EQU   *                                                                
         EDIT  (B1,RAGYLIAB),(2,8(R2)),FILL=0                                   
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,DISPFC           DISPLAY LIAB REC                             
*                                                                               
AGYD120  EQU   *                                                                
         FOUT  (R2)                                                             
*                                                                               
         MVC   ORIGFLAG,RAGYFLAG   SAVE AGENCY RECORD FLAG BYTE                 
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         TWAXC (R2)                CLEAR FAX AND PHONE NUMBER                   
*                                                                               
         XC    LAGAGLC,LAGAGLC     CLEAR THE DATE                               
         FOUT  LAGAGLCH                                                         
*                                                                               
         XC    LAGABYS,LAGABYS     CLEAR BUYING SERVICE FLAG                    
         MVI   LAGABYS,C'N'                                                     
         TM    ORIGFLAG,X'20'      BUYING SERVICE?                              
         BZ    *+8                                                              
         MVI   LAGABYS,C'Y'        YES                                          
*                                                                               
         LR    R1,RA                CHECK FOR DDS TERMINAL                      
         USING TWAD,R1                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   AGYD124             ONLY DDS CAN SEE "LOCKED"                    
*                                                                               
         DROP  R1                                                               
*                                                                               
         XC    LAGDLOK,LAGDLOK     CLEAR CONTRACT ADDED FIELD                   
         FOUT  LAGDLOKH                                                         
*                                                                               
         MVI   LAGDLOK,C'N'                                                     
         TM    ORIGFLAG,X'04'      CONTRACT ADDED ? "LOCKED"                    
         BZ    *+8                                                              
         MVI   LAGDLOK,C'Y'        YES                                          
*                                                                               
AGYD124  EQU   *                                                                
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,RAGK2TYQ        FAX INFO KEPT IN REGENAGY2                   
         MVC   KEY+19(8),RAGYKAGY                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   AGYDX                                                            
*                                                                               
         MVC   AIOSV,AIOAREA       SAVE OFF REC, SO WE WON'T CLOBBER IT         
         LA    R4,MYIOAREA                                                      
         ST    R4,AIOAREA                                                       
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         GOTO1 =A(TRADATA),RR=Y    GET THE TRADE DATA                           
*                                                                               
         TM    ORIGFLAG,X'80'      DO WE HAVE A LONG ADDRESS                    
         BZ    AGYD130             NO - DON'T BOTHER READING THE ELEM           
*                                                                               
         L     R6,AIOAREA          RETRIEVE SECONDARY ADDRESS                   
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNZ   AGYD130             NOT FOUND                                    
         USING RAGY2AE1,R6                                                      
         MVC   LAGAGY1,RAGY2AD1    OVERWRITE SCREEN WITH LONG ADDRESS           
         MVC   LAGAGY2,RAGY2AD2                                                 
*                                                                               
* NOTE: THE NEXT 3 LINES ARE TO FIX THE PROBLEM OF THE CITY BEING               
*       DUPLICATED IN THE 2ND ADDRESS FIELD IN PETRY CONVERTED RECS             
*                                                                               
         CLC   LAGAGYI,RAGY2AD2                                                 
         BNE   *+10                                                             
         XC    LAGAGYI,LAGAGYI                                                  
         DROP  R6                                                               
*                                                                               
AGYD130  EQU   *                                                                
         L     R6,AIOAREA          FAX/PHONE NUMBER?                            
         USING RAGY2FXE,R6                                                      
         MVI   ELCODE,RAGY2CDQ                                                  
         BAS   RE,GETEL                                                         
*                                                                               
         MVC   ORIGAGY,RAGY2EQU    SAVE LAST AGENCY IN TWA                      
*                                                                               
AGYD140  DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(3,R2),RAGY2FAX    AREA CODE                                    
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(3,R2),RAGY2FAX+3  PREFIX                                       
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(4,R2),RAGY2FAX+6  SUFFIX                                       
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         ZIC   R0,0(R2)            AGENCY PHONE NUMBER                          
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(3,R2),RAGY2FON    AREA CODE                                    
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(3,R2),RAGY2FON+3  PREFIX                                       
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(4,R2),RAGY2FON+6  SUFFIX                                       
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         LA    R2,LAGTCODH         TERRITORY CODE                               
         MVC   LAGTCOD,RAGY2TER    TERRITORY                                    
         FOUT  (R2)                                                             
*                                                                               
         LA    R2,LAGTNAMH         DISPLAY TERRITORY NAME                       
*                                                                               
         OC    RAGY2TER,RAGY2TER   CHECK IF NO TERR CODE IN RECORD              
         BNZ   *+14                                                             
         XC    LAGTNAM,LAGTNAM     IF NO, CLEAR OUT TERR NAME FIELD             
         B     AGYD148                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RTERKEY,R4          READ TERRITORY RECORD                        
         MVI   RTERKTYP,X'3D'                                                   
         MVC   RTERKREP,REPALPHA                                                
         MVC   RTERKTER,RAGY2TER                                                
         DROP  R4                                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+14                                                             
         MVC   LAGTNAM(19),=C'***** MISSING *****'                              
         B     AGYD148                                                          
*                                                                               
         MVC   AIOSV2,AIOAREA      SAVE OFF REC, SO WE WON'T CLOBBER IT         
         LA    R4,WORK2                                                         
         ST    R4,AIOAREA                                                       
*                                                                               
         BAS   RE,GETREC                                                        
         STCM  R6,15,MYWORK        SAVE R6 SINCE USING TO PT TO ELEMENT         
         DROP  R6                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'01'        TERRITORY ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF X'01' ELEMENT NOT THERE               
         USING RTERELEM,R6                                                      
         MVC   LAGTNAM,RTERNAME                                                 
         DROP  R6                                                               
         ICM   R6,15,MYWORK        RESTORE R6                                   
         USING RAGY2FXE,R6                                                      
         MVC   AIOAREA,AIOSV2      RESTORE OLD RECORD                           
AGYD148  FOUT  (R2)                                                             
*                                                                               
*                                                                               
         MVC   DAREAGY,RAGY2DAR                                                 
*                                                                               
         LA    R2,LAGDAREH                                                      
*                                                                               
         LA    R4,RAGY2DAR                                                      
         LA    R5,4                UP TO 4 DARE AGENCY EQUIVALENTS              
         MVI   EQUVFLAG,C'N'       SET 'NOT EQUIV: ABC'                         
*                                                                               
AGYD150  DS    0H                                                               
         OC    0(5,R4),0(R4)       SKIP IF NO DARE DATA                         
         BZ    AGYD160                                                          
         CLC   0(5,R4),SPACES                                                   
         BE    AGYD160                                                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         CLC   REPALPHA,=C'B3'     YES - IS THIS EJOR (FOR TESTING)             
         BE    AGYD152             YES                                          
         CLC   REPALPHA,=C'SJ'     YES - IS THIS SJR  (FOR TESTING)             
         BE    AGYD152             YES                                          
         CLC   REPALPHA,=C'CV'     NO  - IS THIS ABC?                           
         BNE   AGYD154             NO                                           
AGYD152  EQU   *                                                                
         CLI   EQUVFLAG,C'Y'       YES - EQUIVALENCE ENTERED?                   
         BE    AGYD153             YES                                          
         CLC   =C'EQUIV',0(R4)     NO  - EQUIVALENCE KEYWORD FOUND?             
         BNE   AGYD154             NO  - TREAT AS REGULAR DARE                  
         MVI   EQUVFLAG,C'Y'       YES - SET 'EQUIV: ABC'                       
AGYD153  EQU   *                                                                
         MVC   8(5,R2),0(R4)       MOVE CODE TO SCREEN                          
         B     AGYD155                                                          
AGYD154  EQU   *                                                                
         MVC   8(3,R2),0(R4)       DARE AGENCY                                  
         OC    3(2,R4),3(R4)                                                    
         BZ    AGYD155                                                          
         CLC   3(2,R4),SPACES                                                   
         BE    AGYD155                                                          
         LA    RE,8(R2)                                                         
         MVI   11(R2),C' '                                                      
         CLI   0(RE),C' '                                                       
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),C'-'                                                       
         MVC   1(2,RE),3(R4)       DARE AGENCY OFFICE                           
                                                                                
AGYD155  DS    0H                                                               
         OI    6(R2),X'80'         XMIT                                         
         LA    R4,5(R4)                                                         
         BCT   R5,AGYD150                                                       
****************************************************                            
AGYD160  EQU   *                                                                
*              ABOUT TO DISPLAY  WHEN AGENCY BIT IS ON                          
         TM    SVPGPBIT+1,X'01'    CONVERT FLAG IS ON                           
*                                                                               
*   TEST  -  FORCE 'ON'                                                         
*                                                                               
         B     AGYD161             DISPLAY THE AGENCY EQUIV. SCREEN             
         BO    AGYD161             DISPLAY THE AGENCY EQUIV. SCREEN             
         TM    SVPGPBIT+1,X'40'                                                 
         BNO   AGYD163             DISPLAY THE AGENCY EQUIV. SCREEN             
*                                  BIT IS ON                                    
AGYD161  EQU   *                                                                
*                                  NOW MOVE THE AGENCY EQUIV . FIELD            
*                                          TO THE SCREEN                        
         MVC   LAGAGEQ,RAGY2EQU    INSERT AGENCY                                
         MVC   LAGAGEO,RAGY2EQO    INSERT AGENCY OFFICE, IF ANY                 
*                                                                               
AGYD163  EQU   *                                                                
         XC    LAGAGLC,LAGAGLC     CLEAR THE DATE                               
         OC    RAGY2LCD,RAGY2LCD   ANY DATE INFIELD?                            
         BZ    AGYD165             NO                                           
         CLC   RAGY2LCD,=C'00'      DITTO                                       
         BE    AGYD165                                                          
         GOTO1 VDATCON,DMCB,(2,RAGY2LCD),(5,LAGAGLC)                            
*                                  TAKE LAST CHANGE DATE FROM FILE              
AGYD165  EQU   *                                                                
         FOUT  LAGAGLCH                                                         
         L     R4,AIOSV            RESTORE                                      
         ST    R4,AIOAREA                                                       
*                                                                               
         GOTO1 =A(AGYCMT),DMCB,(RC),MYIOAREA,RR=Y DISPLAY AGY COMMENTS          
*                                                                               
         GOTO1 =A(AGYDIS),DMCB,(RC),MYIOAREA,RR=Y                               
*                                  DISPLAY 'PAXSON' OFFICES                     
*                                                                               
AGYDX    DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
AGYEDT   DS    0H                                                               
* SAVE TO CHECK LATER WHEN WE WRITE TO FILE                                     
         MVC   TRAPKEY,KEY                                                      
*                                                                               
         MVI   PROFNINE,C'N'       SET PROFILE NINE TO 'NO'                     
         CLI   BACT,C'A'           NEW RECORD?                                  
         BE    AGED0020            YES                                          
         GOTO1 GETREC              RETRIEVE THE AGENCY RECORD                   
         MVC   PROFNINE,RAGYPROS+8 SAVE NINTH PROFILE BYTE                      
         XCEFL REC+36,120          CLEAR REMAINDER OF RECORD                    
AGED0020 EQU   *                                                                
*                                                                               
         MVC   REC+34(2),=X'017A'                                               
         MVC   REC+27(2),=Y(156)                                                
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         LR    R1,RA                CHECK FOR DDS TERMINAL                      
         USING TWAD,R1                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   AGED0100            ONLY DDS CAN SET CONTROL NUMBER              
*                                                                               
         DROP  R1                                                               
*                                                                               
         TM    SVPGPBIT,X'01'      AUTO-ASSIGN USER?                            
         BNO   AGED0100            NO                                           
         LA    R5,LFMKEYH          A(KEY FIELD)                                 
         CLI   5(R5),4             INPUT = 4 CHARS?                             
         BNE   AGED0100            NO  - CAN'T BE CONTROL                       
         CLC   LFMKEY(4),=C'0000'  CONTROL RECORD UPDATE?                       
         BNE   AGED0100            NO                                           
         CLC   8(8,R2),=C'CONTROL='                                             
*                                  CONTROL SET REQUEST?                         
         BNE   AGED0100            NO                                           
         ZIC   RF,5(R2)            YES - GET LENGTH OF INPUT                    
         SH    RF,=H'8'            SUBTRACT L(CONTROL= )                        
         LA    R6,16(R2)           SET A(INPUT)                                 
AGED0040 EQU   *                                                                
         CLI   0(R6),C'0'          VALIDATE NUMBER                              
         BL    FLERR11             NOT NUMERIC                                  
         CLI   0(R6),C'9'          VALIDATE NUMBER                              
         BH    FLERR11             NOT NUMERIC                                  
         LA    R6,1(R6)            BUMP TO NEXT POSITION                        
         BCT   RF,AGED0040         GO BACK FOR NEXT                             
         ZIC   RF,5(R2)            OK  - GET LENGTH OF INPUT AGAIN              
         SH    RF,=H'9'            SUBTRACT L(CONTROL= + 1 FOR EX)              
         LA    R6,16(R2)           SET A(INPUT) AGAIN                           
         EX    RF,AGED0060         PACK INPUT BY LENGTH                         
         B     AGED0080                                                         
AGED0060 EQU   *                                                                
         PACK  DUB(8),0(0,R6)      PACK INPUT                                   
AGED0080 EQU   *                                                                
         CVB   RF,DUB              CONVERT VALUE TO BINARY                      
         STCM  RF,3,RAGYASSN       INSERT VALUE INTO RECORD                     
         LA    RF,7                RESET LENGTH OF INPUT FIELD                  
         STC   RF,5(R2)            RESET HEADER LENGTH                          
AGED0100 EQU   *                                                                
         MVC   OLDNAME,RAGYNAM1    SAVE OLD NAME                                
         BAS   RE,MOVE                                                          
         MVC   RAGYNAM1,WORK                                                    
*                                                                               
         B     AGED0140                                                         
*&&DO                                                                           
*                                                                               
*  SPECIAL CODE TO DELETE AN AGY RECORD AND PASSIVE POINTER                     
*        AGENCY CODES CANNOT AND MUST NOT BE DELETED THIS WAY!!                 
*        TERRITORY PASSIVE POINTER NOT IN THIS CODE:  MUST BE ADDED             
*                                                                               
*        CLI   BACT,C'A'           MUST BE ACTION CHANGE                        
*        BE    AGED0140                                                         
*        CLC   RAGYNAM1(6),=C'DELETE'                                           
*        BNE   AGED0140                                                         
*                                                                               
*        BAS   RE,READ                                                          
*        OI    KEY+27,X'80'        MARK DELETED                                 
*        BAS   RE,WRITE                                                         
*                                                                               
*        BAS   RE,GETREC           DEAL WITH PASSIVE POINTER                    
*        XC    KEY,KEY                                                          
*        MVI   KEY,X'8A'                                                        
*        MVC   KEY+1(18),RAGYNAM1                                               
*        MVC   KEY+19(8),REC+19                                                 
*        BAS   RE,HIGH                                                          
*        CLC   KEYSAVE(27),KEY                                                  
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
*        MVI   DMOUTBTS,0          NO EROR TESTS                                
*        OI    KEY+27,X'80'                                                     
*        BAS   RE,WRITE                                                         
*        BAS   RE,CHECK                                                         
*                                                                               
* DELETE REGENAGY2 FAX RECORD AS WELL                                           
*                                                                               
*        XC    KEY,KEY                                                          
*        MVI   KEY,RAGK2TYQ                                                     
*        MVC   KEY+19(8),RAGYKAGY                                               
*        BAS   RE,HIGH                                                          
*        CLC   KEYSAVE(27),KEY                                                  
*        BNE   AGED0120                                                         
*        OI    KEY+27,X'80'        MARK DELETED                                 
*        BAS   RE,WRITE                                                         
*                                                                               
*        MVC   AIOSV,AIOAREA       SAVE OFF REC, SO WE WON'T CLOBBER IT         
*        LA    R4,MYIOAREA                                                      
*        ST    R4,AIOAREA                                                       
*        USING RAGY2REC,R4                                                      
*                                                                               
*        BAS   RE,GETREC                                                        
*        OI    RAGY2CTL,X'80'                                                   
*        BAS   RE,PUTREC                                                        
*        DROP  R4                                                               
*                                                                               
*        L     R4,AIOSV            RESTORE                                      
*        ST    R4,AIOAREA                                                       
*                                                                               
AGED0120 DS    0H                                                               
*        MVC   LFMMSG(20),=C'** RECORD DELETED **'                              
*        MVI   ERRAREA,X'FF'                                                    
*        LA    R2,LFMKEYH                                                       
*        B     EXIT                                                             
*&&                                                                             
AGED0140 EQU   *                                                                
         MVC   RAGYFLAG,ORIGFLAG   RESET FLAG FROM STORAGE                      
         CLI   BACT,C'A'           'ADD' ACTION?                                
         BNE   AGED0150            NO                                           
*                                                                               
*   FOR AN 'ADD', THIS FLAG SHOULD BE OFF.  FLAG ONLY HAS MEANING FOR           
*        A 'CHANGE' ACTION.                                                     
****     NI    RAGYFLAG,X'FF'-X'04'                                             
****                               YES - DON'T CARRY 'CONTRACT' FLAG            
         MVI   RAGYFLAG,0          CLEAR THE FLAG BYTE FOR 'ADD'                
AGED0150 EQU   *                                                                
*                                                                               
         CLI   LAGABYSH+5,0        INPUT?                                       
         BE    AGED0160            NO                                           
*                                                                               
         NI    RAGYFLAG,X'FF'-X'20'                                             
         CLI   LAGABYS,C'Y'        BUYING SERVICE?                              
         BNE   *+12                                                             
         OI    RAGYFLAG,X'20'      YES                                          
         B     AGED0160                                                         
*                                                                               
         CLI   LAGABYS,C'N'                                                     
         BE    AGED0160            NOT Y OR N                                   
*                                                                               
         LA    R2,LAGABYSH                                                      
         B     FLERR2              INVALID                                      
*                                                                               
AGED0160 DS    0H                                                               
         MVI   LAGABYS,C'N'        REDISPLAY FIELD                              
         TM    RAGYFLAG,X'20'      BUYING SERVICE?                              
         BZ    *+8                                                              
         MVI   LAGABYS,C'Y'        YES                                          
         OI    LAGABYSH+6,X'80'                                                 
*                                                                               
         LR    R1,RA                CHECK FOR DDS TERMINAL                      
         USING TWAD,R1                                                          
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   AGED0175            ONLY DDS CAN "UNLOCK" RECORD                 
*                                    (AND SEE THE "LOCKED" FIELD)               
         DROP  R1                                                               
*                                                                               
         TM    RAGYFLAG,X'04'      LOCKED FOR "CONTRACT ADDED"?                 
         BZ    AGED0170            NO                                           
*                                                                               
         CLI   LAGDLOK,C'Y'        MUST BE Y OR N                               
         BE    AGED0170            YES                                          
*                                                                               
         NI    RAGYFLAG,X'FF'-X'04'                                             
         CLI   LAGDLOK,C'N'        NOT LOCKED FOR "CONTRACT ADDED"              
         BE    AGED0170            YES                                          
*                                                                               
         LA    R2,LAGDLOKH                                                      
         B     FLERR2              INVALID                                      
*                                                                               
AGED0170 DS    0H                                                               
         MVI   LAGDLOK,C'N'        REDISPLAY FIELD                              
         TM    RAGYFLAG,X'04'      LOCKED FOR "CONTRACT ADDED"?                 
         BZ    *+8                                                              
         MVI   LAGDLOK,C'Y'        YES                                          
         OI    LAGDLOKH+6,X'80'                                                 
*                                                                               
AGED0175 DS    0H                                                               
         OI    RAGYFLAG,X'80'      USE EXPANDED ADDRESS FIELDS                  
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RAGYNAM2,WORK                                                    
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   PROFNINE,C'Y'       PROHIBIT CHANGE TO ADDRESS?                  
         BNE   AGED0180            NO                                           
         TM    4(R2),X'20'         YES - PREVALID NOW OFF?                      
         BNO   FLERR15             YES - ATTEMPT TO CHANGE                      
AGED0180 CLI   5(R2),0             ANY DATA?                                    
         BE    FLERR1              NO  - NEEDS DATA HERE                        
****>    BAS   RE,MOVE             DON'T SAVE THE DATA YET, THIS                
****>    MVC   RAGYADD1,WORK       WILL BE DONE LATER                           
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   PROFNINE,C'Y'       PROHIBIT CHANGE TO ADDRESS?                  
         BNE   AGED0200            NO                                           
         TM    4(R2),X'20'         YES - PREVALID NOW OFF?                      
         BNO   FLERR15             YES - ATTEMPT TO CHANGE                      
AGED0200 DS    0H                                                               
****>    BAS   RE,MOVE             DON'T SAVE THE DATA YET, THIS                
****>    MVC   RAGYADD2,WORK       WILL BE DONE LATER                           
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   PROFNINE,C'Y'       PROHIBIT CHANGE TO ADDRESS?                  
         BNE   AGED0220            NO                                           
         TM    4(R2),X'20'         YES - PREVALID NOW OFF?                      
         BNO   FLERR15             YES - ATTEMPT TO CHANGE                      
AGED0220 CLI   5(R2),0             ANY DATA?                                    
         BE    FLERR1              NO  - NEEDS DATA HERE                        
         MVC   RAGYCITY,8(R2)                                                   
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   PROFNINE,C'Y'       PROHIBIT CHANGE TO ADDRESS?                  
         BNE   AGED0240            NO                                           
         TM    4(R2),X'20'         YES - PREVALID NOW OFF?                      
         BNO   FLERR15             YES - ATTEMPT TO CHANGE                      
AGED0240 CLI   5(R2),0             ANY DATA?                                    
         BE    FLERR1              NO  - NEEDS DATA HERE                        
         CLI   5(R2),2                                                          
         BNE   FLERR2                                                           
         MVC   RAGYSTAT,8(R2)                                                   
*                                                                               
         BAS   RE,NEXTUF           ZIP CAN BE ALPHA (CANADIAN)/NUMERIC          
*                                     AND 1-10 CHARACTERS                       
         CLI   PROFNINE,C'Y'       PROHIBIT CHANGE TO ADDRESS?                  
         BNE   AGED0260            NO                                           
         TM    4(R2),X'20'         YES - PREVALID NOW OFF?                      
         BNO   FLERR15             YES - ATTEMPT TO CHANGE                      
AGED0260 CLI   5(R2),0             ANY DATA?                                    
         BE    FLERR1              NO  - NEEDS DATA HERE                        
         MVC   RAGYZIP,8(R2)                                                    
*                                                                               
         BAS   RE,NEXTUF           PROFILE                                      
         MVI   RAGYPROS,C'N'                                                    
         MVC   RAGYPROS+1(L'RAGYPROS-1),RAGYPROS                                
         ZIC   R1,5(R2)                                                         
         CH    R1,=H'10'           MAX 10 OPTIONS                               
         BH    FLERR1                                                           
         OR    R1,R1                                                            
         BZ    AGED0320            NO PROFILES, DEFAULT N'S                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RAGYPROS(0),8(R2)                                                
         LA    R1,9                MAX NUMBER OF OPTIONS - 1                    
         LA    R4,RAGYPROS                                                      
AGED0280 EQU   *                                                                
         CLI   0(R4),C'N'          ONLY 'N' OR 'Y' VALID                        
         BE    AGED0300                                                         
         CLI   0(R4),C'Y'                                                       
         BNE   FLERR2                                                           
AGED0300 EQU   *                                                                
         LA    R4,1(R4)            NEXT OPT                                     
         BCT   R1,AGED0280                                                      
         CLI   0(R4),C'N'          N/Y/F/L VALID FOR 10TH OPTION                
         BE    AGED0320                                                         
         CLI   0(R4),C'Y'          N/Y/F/L VALID FOR 10TH OPTION                
         BE    AGED0320                                                         
         CLI   0(R4),C'F'          N/Y/F/L VALID FOR 10TH OPTION                
         BE    AGED0320                                                         
         CLI   0(R4),C'L'                                                       
         BNE   FLERR2A                                                          
AGED0320 EQU   *                                                                
         MVC   8(L'RAGYPROS,R2),RAGYPROS                                        
         FOUT  (R2)                                                             
*                                                                               
*   DO NOT SWITCH IS NOT PRESENTLY USED.  SEE OTHER NOTES.                      
*                                                                               
**       BAS   RE,NEXTUF           DO NOT DROP FIELD                            
**       NI    RAGYFLAG,X'FF'-X'01'   TURN OFF 'DO NOT DROP' FLAG               
**       CLI   5(R2),0             ANY DATA?                                    
**       BE    AGED0330            NO                                           
**       CLI   8(R2),C'N'          YES - ONLY 'N' OR 'Y' VALID                  
**       BE    AGED0330                                                         
**       CLI   8(R2),C'Y'                                                       
**       BNE   FLERR2                                                           
**       OI    RAGYFLAG,X'01'      SET 'DO NOT DROP' FLAG                       
AGED0330 DS    0H                                                               
         BAS   RE,NEXTUF           DO NOT USE FIELD                             
         NI    RAGYFLAG,X'FF'-X'02'   TURN OFF 'DO NOT USE' FLAG                
         CLI   5(R2),0             ANY DATA?                                    
         BE    AGED0340            NO                                           
         CLI   8(R2),C'N'          YES - ONLY 'N' OR 'Y' VALID                  
         BE    AGED0340                                                         
         CLI   8(R2),C'Y'                                                       
         BNE   FLERR2                                                           
         OI    RAGYFLAG,X'02'      SET 'DO NOT USE' FLAG                        
AGED0340 DS    0H                                                               
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BNE   AGED0360                                                         
         MVI   8(R2),C'1'                                                       
         TM    SVPGPBIT+1,X'80'    USE RATING 2: INSUFF. CREDIT INFO            
         BZ    AGED0360            INSTEAD OF 1                                 
         MVI   8(R2),C'2'                                                       
AGED0360 EQU   *                                                                
         CLI   8(R2),C'1'                                                       
         BL    FLERR4                                                           
         CLI   8(R2),C'6'                                                       
         BH    FLERR4                                                           
         MVC   RAGYRISK,8(R2)                                                   
         NI    RAGYRISK,X'0F'                                                   
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         MVC   8(40,R2),SPACES                                                  
         LA    RE,RISKTAB                                                       
         ZIC   RF,RAGYRISK                                                      
         BCTR  RF,0                                                             
         MH    RF,=H'40'                                                        
         AR    RE,RF                                                            
         MVC   8(40,R2),0(RE)                                                   
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         XC    RAGYLIAB,RAGYLIAB                                                
         CLI   5(R2),0                                                          
         BNE   AGED0380                                                         
         ZIC   RF,0(R2)            BUMP TO DISPLAY FIELD                        
         AR    R2,RF                                                            
         MVC   8(50,R2),SPACES                                                  
         FOUT  (R2)                                                             
         B     AGED0420                                                         
*                                                                               
AGED0380 CLI   5(R2),2                                                          
         BNE   FLERR5                                                           
         LA    RE,8(R2)                                                         
         ZIC   RF,5(R2)                                                         
         STM   RE,RF,DMCB                                                       
         GOTO1 =V(NUMVAL),DMCB,RR=RELO                                          
         CLI   DMCB,0                                                           
         BNE   FLERR5                                                           
         CLC   DMCB+4(4),=F'0'                                                  
         BL    FLERR5                                                           
         CLC   DMCB+4(4),=F'100'                                                
         BH    FLERR5                                                           
         MVC   RAGYLIAB,DMCB+4+3                                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING RCMTD,R3                                                         
         MVI   RCMTKTYP,X'2E'      RECORD TYPE                                  
         MVC   RCMTKREP,REPALPHA   REP CODE                                     
         MVC   RCMTKOFF,=X'FFFF'   OFFICE CODE                                  
         MVC   RCMTKCDE(4),=C'LIAB'  LIABILITY COMMENT CODE                     
         MVC   RCMTKCDE+4(2),8(R2)                                              
         OC    RCMTKCDE,SPACES     BLANK PADDED                                 
         DROP  R3                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BE    AGED0400                                                         
         XC    RAGYLIAB,RAGYLIAB                                                
         B     FLERR6                                                           
*                                                                               
AGED0400 EQU   *                                                                
         BAS   RE,DISPFCED         DISPLAY LIAB REC                             
         FOUT  (R2)                                                             
*                                                                               
AGED0420 EQU   *                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         XC    KEY,KEY             GET DARE AGENCY INFO, IF ANY                 
         LA    R4,KEY                                                           
         USING RAGY2KEY,R4                                                      
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY,RAGYKAGY                                                
         MVC   RAGK2AOF,RAGYKAOF                                                
         MVC   RAGK2REP,RAGYKREP                                                
         DROP  R4                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   AGED0440                                                         
         MVC   AIOSV,AIOAREA       SAVE OFF REC, SO WE WON'T CLOBBER IT         
         LA    R4,MYIOAREA                                                      
         ST    R4,AIOAREA                                                       
         GOTO1 GETREC                                                           
         USING RAGY2REC,R4                                                      
         MVC   DAREAGY,RAGY2DAR                                                 
         MVC   OLDTERR,RAGY2TER    SAVE OLD TERRITORY CODE                      
         DROP  R4                                                               
         L     R4,AIOSV            RESTORE                                      
         ST    R4,AIOAREA                                                       
*                                                                               
* BUILD FAX ELEMENT IN ELEM                                                     
*                                                                               
AGED0440 DS    0H                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING RAGY2FXE,R6                                                      
         CLI   5(R2),0             AREA CODE                                    
         BNE   AGED0460                                                         
         CLI   RAGYPRO2,C'Y'       EASYLINK CONTRACT COPY SET?                  
         BE    FLERR7                                                           
         B     AGED0480                                                         
AGED0460 TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    FLERR3                                                           
AGED0480 MVC   RAGY2FAX(3),8(R2)                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0             PREFIX                                       
         BNE   AGED0500                                                         
         CLI   RAGYPRO2,C'Y'       EASYLINK CONTRACT COPY SET?                  
         BE    FLERR7                                                           
         B     AGED0520                                                         
AGED0500 TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    FLERR3                                                           
AGED0520 MVC   RAGY2FAX+3(3),8(R2)                                              
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0             SUFFIX                                       
         BNE   AGED0540                                                         
         CLI   RAGYPRO2,C'Y'       EASYLINK CONTRACT COPY SET?                  
         BE    FLERR7                                                           
         B     AGED0560                                                         
AGED0540 TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    FLERR3                                                           
AGED0560 MVC   RAGY2FAX+6(4),8(R2)                                              
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         EJECT                                                                  
*                                                                               
* BUILD PHONE NUMBER IN ELEM                                                    
*                                                                               
         CLI   5(R2),0             PHONE AREA CODE                              
         BE    AGED0580                                                         
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    FLERR3                                                           
         MVC   RAGY2FON(3),8(R2)                                                
AGED0580 ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0             PREFIX                                       
         BE    AGED0600                                                         
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    FLERR3                                                           
         MVC   RAGY2FON+3(3),8(R2)                                              
AGED0600 ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0             SUFFIX                                       
         BE    AGED0620                                                         
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    FLERR3                                                           
         MVC   RAGY2FON+6(4),8(R2)                                              
*                                                                               
AGED0620 DS    0H                                                               
         LA    R2,LAGTCODH         GET TERRITORY CODE                           
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
         XC    LAGTNAM,LAGTNAM     IF NOT THERE, CLEAR OUT TERR NAME            
         B     AGED0640                                                         
*                                                                               
         OC    LAGTCOD,SPACES                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RTERKEY,R4          READ TERRITORY REC TO VALIDATE CODE          
         MVI   RTERKTYP,X'3D'                                                   
         MVC   RTERKREP,REPALPHA                                                
         MVC   RTERKTER,LAGTCOD                                                 
         DROP  R4                                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   FLERR2              DOESN'T MATCH--INVALID CODE                  
*                                                                               
         MVC   RAGY2TER,LAGTCOD    STORE CODE IN RECORD                         
         MVC   NEWTERR,LAGTCOD     SAVE NEW CODE                                
         MVC   AIOSV,AIOAREA       SAVE OFF REC, SO WE WON'T CLOBBER IT         
         LA    R4,WORK2                                                         
         ST    R4,AIOAREA                                                       
*                                                                               
         BAS   RE,GETREC                                                        
         DROP  R6                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'01'        TERRITORY ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF X'01' ELEMENT NOT THERE               
         USING RTERELEM,R6                                                      
         MVC   LAGTNAM,RTERNAME    DISPLAY THE NAME                             
         DROP  R6                                                               
         LA    R6,ELEM             RESTORE R6 TO POINT TO ELEM                  
         USING RAGY2FXE,R6                                                      
         MVC   AIOAREA,AIOSV       RESTORE OLD RECORD                           
AGED0640 FOUT  LAGTNAMH                                                         
*                                                                               
*                                                                               
* GET DARE AGENCY EQUIVALENCY CODE                                              
         MVI   EQUVFLAG,C'N'       SET 'NOT EQUIVALENT: ABC'                    
*                                                                               
         LA    R2,LAGDAG1H                                                      
*                                                                               
         XC    RAGY2DAR(20),RAGY2DAR                                            
         LA    R5,4                MAX 4 DARE AGENCIES                          
                                                                                
AGED0660 DS    0H                                                               
         CLI   5(R2),0             SKIP IF NO DATA                              
         BE    AGED0820                                                         
         XC    MYWORK,MYWORK                                                    
         XC    WORK,WORK                                                        
         MVC   WORK(6),8(R2)                                                    
         OC    WORK,SPACES         USE SPACE AS SENTINEL                        
         LA    R4,WORK                                                          
*                                                                               
* CHECK FOR AGENCY OFFICE                                                       
*                                                                               
AGED0680 DS    0H                                                               
         CLI   0(R4),C'-'                                                       
         BE    AGED0700                                                         
         CLI   0(R4),C' '                                                       
         BE    AGED0740                                                         
         LA    R4,1(R4)                                                         
         B     AGED0680                                                         
*                                                                               
AGED0700 DS    0H                                                               
*                                                                               
* ALLOW XXX-XX                                                                  
*                                                                               
         B     AGED0720                                                         
*                                                                               
* SKIP. XXX-XX SHOULD BE ALLOWED                                                
*                                                                               
         CLI   10(R2),C'-'         FORMAT MUST BE XXXX OR XX-XX                 
         BNE   FLERR10                                                          
         CLI   5(R2),5                                                          
         BNE   FLERR10                                                          
                                                                                
AGED0720 DS    0H                                                               
         MVC   MYWORK+3(2),1(R4)    AGENCY OFFICE                               
         CLI   MYWORK+3,C' '        OFFICE MUST BE 2 CHARS                      
         BE    FLERR8                                                           
         CLI   MYWORK+4,C' '        OFFICE MUST BE 2 CHARS                      
         BE    FLERR8                                                           
         LA    RF,WORK+1                                                        
         SR    R4,RF                                                            
         EX    R4,*+8                                                           
         B     AGED0760                                                         
         MVC   MYWORK(0),WORK                                                   
*                                                                               
AGED0740 DS    0H                                                               
*                                                                               
*   ABC MAY HAVE 3, 4, OR 5 CHARACTER EQUIV CODES. THE 5-CHAR TEST              
*        WILL BE NEEDED TO PICK UP THE KEYWORD 'EQUIV'                          
*                                                                               
         CLI   EQUVFLAG,C'Y'       'EQUIV' KEYWORK FOUND?                       
         BE    AGED0745            YES - ACCEPT ANY INPUT                       
         CLI   5(R2),5             NO  - ABC 5 CHARACTER INPUT?                 
         BNE   AGED0755            NO                                           
         CLC   REPALPHA,=C'B3'     YES - IS THIS EJOR (FOR TESTING)             
         BE    AGED0742            YES                                          
         CLC   REPALPHA,=C'SJ'     YES - IS THIS SJR  (FOR TESTING)             
         BE    AGED0742            YES                                          
         CLC   REPALPHA,=C'CV'     NO  - IS THIS ABC?                           
         BNE   AGED0755            NO                                           
AGED0742 EQU   *                                                                
         CLI   EQUVFLAG,C'Y'       'EQUIV' KEYWORD FOUND?                       
         BE    AGED0745            YES                                          
         CLC   =C'EQUIV',8(R2)     NO  - IS THIS KEYWORD?                       
         BNE   FLERR10             NO  - MUST BE AN ERROR                       
         MVI   EQUVFLAG,C'Y'       YES - SET ABC EQUIV ENTERED                  
AGED0745 EQU   *                                                                
         MVC   MYWORK(5),8(R2)     THIS IS EITHER KEYWORD OR EQUIV              
         OC    MYWORK(5),SPACES    SET SMALL TO SPACES                          
         B     AGED0760                                                         
AGED0755 DS    0H                                                               
         CLI   5(R2),4             MUST BE IN FORMAT XXXX OR XX-XX              
         BNE   FLERR10                                                          
         MVC   MYWORK(2),WORK                                                   
         MVC   MYWORK+3(2),WORK+2                                               
*                                                                               
AGED0760 DS    0H                                                               
         OC    MYWORK(5),SPACES                                                 
         LA    RF,RAGY2DAR                                                      
                                                                                
AGED0780 OC    0(5,RF),0(RF)                                                    
         BZ    AGED0800                                                         
         CLC   0(5,RF),MYWORK                                                   
         BE    FLERR9              DUPLICATE ERROR                              
         LA    RF,5(RF)                                                         
         B     AGED0780                                                         
                                                                                
AGED0800 MVC   0(5,RF),MYWORK                                                   
                                                                                
AGED0820 ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R5,AGED0660                                                      
*                                                                               
*                                                                               
**********************************************************JR                    
*     WHEN PROFILE POSITION 10 IS  "ON"                                         
*        ON AN ADD FUNCTION THEN :                                              
*        _   CHECK  HEADER SCREEN  KEY FOR AGY AND THIS                         
*          MUST HAVE AN A/N FIELD.                                              
*        _   CHECK NEW AGEQ FIELD AND MUST BE 4 DIGIT NUMERIC                   
*          AND WHEN VALID MOVE AGEQ TO RAGY2EQU                                 
*     WHEN PROFILE POSITION 10 IS  "OFF"                                        
*        MAKE SURE THAT THE AGEQ FIELD IS PROTECTED AND                         
*        CANNOT BE ENTERED.                                                     
*                                                                               
****          CHECK NEW AGENCY EQUIV. FIELD                                     
                                                                                
***>>>   MVC   RAGY2EQU,ORIGAGY    REINSERT LAST AGENCY, IF ANY                 
*                                                                               
         TM    SVPGPBIT+1,X'01'    CONVERT BIT ON??                             
*                                                                               
*   TEST  -  FORCE 'ON'                                                         
*                                                                               
         B     AGED0830            YES                                          
         BO    AGED0830            YES                                          
         TM    SVPGPBIT+1,X'40'    AGENCY BIT ON ??                             
         BNO   AGED0880            CONTINUE ORIG CODE                           
AGED0830 EQU   *                                                                
         LA    RF,LAGAGEQH         AGENCY EQUIV CODE INPUT                      
         CLI   5(RF),0             ANYTHING ENTERED ?                           
         BNE   AGED0835            YES - PROCESS EQUIV AND OFF EQUIV            
         CLC   REPALPHA,=C'B3'     YES - IS THIS EJOR (FOR TESTING)             
         BE    AGED0860            YES                                          
         CLC   REPALPHA,=C'SJ'     YES - IS THIS SJR  (FOR TESTING)             
         BE    AGED0860            YES                                          
         CLC   REPALPHA,=C'CV'     NO  - IS THIS ABC?                           
         BNE   AGED0880            NO  - SKIP EQUIV AND OFFICE EQUIV            
         B     AGED0860            YES - PROCESS OFFICE EQUIV                   
AGED0835 EQU   *                                                                
*                                                                               
*   LEN/NUM TESTS IGNORED FOR ALL USERS NOW! BUHR (MAR11/06)                    
*                                                                               
*&&DO                                                                           
         CLC   =C'B3',REPALPHA     HARD-CODED FOR EJOR                          
         BE    AGED0840            YES - SKIP LEN/NUM TESTS                     
         CLC   =C'UT',REPALPHA     HARD-CODED FOR UTS                           
         BE    AGED0840            YES - SKIP LEN/NUM TESTS                     
         CLC   =C'FB',REPALPHA     HARD-CODED FOR FOX STATION                   
         BE    AGED0840            YES - SKIP LEN/NUM TESTS                     
         CLC   =C'PV',REPALPHA     HARD-CODED FOR PETRY                         
         BE    AGED0840            YES - SKIP LEN/NUM TESTS                     
         CLC   =C'FN',REPALPHA     HARD-CODED FOR FOX TV                        
         BE    AGED0840            YES - SKIP LEN/NUM TESTS                     
         CLI   5(RF),4             LENGTH 4?                                    
         BNE   FLERR12             ERR MESSAGE TO PUT OUT                       
         TM    4(RF),X'08'         NUMERIC ?                                    
         BNO   FLERR12             ERR MESSAGE TO PUT OUT                       
*&&                                                                             
AGED0840 EQU   *                                                                
         MVC   RAGY2EQU,LAGAGEQ    EQUIV TO STORAGE                             
*                                                                               
*   OFFICE MAY BE ENTERED FOR ALL USERS !  BUHR (MAR11/06)                      
*                                                                               
*&&DO                                                                           
         CLC   =C'B3',REPALPHA     HARD-CODED FOR EJOR                          
         BE    AGED0860            YES -                                        
         CLC   =C'UT',REPALPHA     HARD-CODED FOR UTS                           
         BE    AGED0860            YES                                          
         CLC   =C'FB',REPALPHA     HARD-CODED FOR FOX STATION                   
         BE    AGED0860            YES                                          
         CLC   =C'PV',REPALPHA     HARD-CODED FOR PETRY                         
         BNE   AGED0880            NO  -                                        
*&&                                                                             
AGED0860 EQU   *                                                                
         LA    RF,LAGAGEOH                                                      
         CLI   5(RF),0             NOTHING ENTERED ?                            
         BE    AGED0880                                                         
         CLI   5(RF),2             LENGTH 2?                                    
         BNE   FLERR16             ERR MESSAGE TO PUT OUT                       
         MVC   RAGY2EQO,LAGAGEO    INSERT EQUIV AGY OFFICE                      
                                                                                
AGED0880 EQU   *                                                                
         GOTO1 =A(GAINSPC1),DMCB,(R6),RR=Y                                      
         MVI   RAGY2CDE,RAGY2CDQ   ELEMENT CODE                                 
         MVI   RAGY2FXL,RAGY2FLQ   ELEMENT LENGTH                               
         DROP  R6                                                               
*                                                                               
* ADD OR CHANGE THE REGENAGY2 RECORD                                            
*                                                                               
         MVC   AIOSV,AIOAREA       SAVE OFF REC, SO WE WON'T CLOBBER IT         
         LA    R4,MYIOAREA                                                      
         ST    R4,AIOAREA                                                       
         USING RAGY2REC,R4                                                      
*                                                                               
         XCEF  MYIOAREA,1000       CLEAR MY IO AREA                             
*                                                                               
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY,RAGYKAGY                                                
         MVC   RAGK2AOF,RAGYKAOF                                                
         MVC   RAGK2REP,RAGYKREP                                                
         MVC   RAGY2LEN,=Y(86)                                                  
         MVC   RAGY2FXE(RAGY2FLQ),ELEM                                          
*                                                                               
         MVC   KEY(L'RAGYKEY),RAGY2REC                                          
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   AGED1000                                                         
         MVC   RAGY2DA,KEY+28      SAVE DISK ADDRESS OF RAGY2REC                
*                                                                               
         NI    KEY+27,X'FF'-X'80'  RESTORE KEY                                  
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,GETREC                                                        
         NI    RAGY2CTL,X'FF'-X'80' RESTORE RECORD, IF EXISTED                  
         MVC   RAGY2FXE(RAGY2FLQ),ELEM                                          
*                                                                               
AGED0940 LA    R6,RAGY2REC                                                      
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            IS ELEMENT THERE?                            
         BE    AGED0960            YES - GO UPDATE IT                           
         USING RAGY2AE1,R6                                                      
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   RAGY2AC1,X'20'                                                   
         MVI   RAGY2AX1,RAGY2FL1                                                
         GOTO1 VADDELEM,DMCB,RAGY2REC,ELEM                                      
         B     AGED0940                                                         
AGED0960 MVC   RAGY2AD1,LAGAGY1                                                 
         MVC   RAGY2AD2,LAGAGY2                                                 
         MVC   RAGY2CTY,RAGYCITY                                                
         MVC   RAGY2STE,RAGYSTAT                                                
         MVC   RAGY2ZIP,RAGYZIP                                                 
         DROP  R6                                                               
*                                                                               
* WRITE AGENCY COMMENTS TO RAGY2REC HERE                                        
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'40',RAGY2REC) DELETE OLD COMMENTS               
         LA    R6,WORK3            CMT ELEMENT SAVE AREA                        
         CLI   0(R6),X'40'         HAVE A COMMENT ELEM?                         
         BNE   AGED0980            NO                                           
         GOTO1 VADDELEM,DMCB,RAGY2REC,0(R6)                                     
         LA    R6,62(R6)           2ND COMMENT AREA                             
         CLI   0(R6),X'40'         HAVE A COMMENT ELEM?                         
         BNE   AGED0980            NO                                           
         GOTO1 VADDELEM,DMCB,RAGY2REC,0(R6)                                     
*                                                                               
AGED0980 DS    0H                                                               
         GOTO1 VDELELEM,DMCB,(X'50',RAGY2REC) DEL OFFICES                       
         GOTO1 =A(PAXOFF),DMCB,(RC),LAGOFC1H,MYIOAREA,RR=Y                      
         BZ    AGED0990            OKAY RETURN                                  
         L     R2,DUB              A(ERROR FIELD)                               
         B     MYERR               ERROR RETURN - MESSAGE ALREADY SET           
*                                                                               
AGED0990 EQU   *                 ** BUILD '1F' ELEMENT **                       
         GOTO1 =A(ADAGY21F),RR=Y                                                
         GOTO1 VDELELEM,DMCB,(X'1F',RAGY2REC) DEL OLD AGY ELEM                  
         GOTO1 VADDELEM,DMCB,RAGY2REC,ELEM                                      
*                                                                               
         BAS   RE,PUTREC           WRITE THE RECORD                             
         BAS   RE,WRITE            WRITE THE KEY                                
         B     AGED1040                                                         
*                                                                               
AGED1000 DS    0H                                                               
         GOTO1 =A(ADDAGY2),DMCB,(RC),MYIOAREA,RR=Y                              
*                                                                               
         DROP  R4                                                               
*                                                                               
* ADD PAXSON ELEMENTS                                                           
*                                                                               
AGED1020 DS    0H                                                               
         GOTO1 =A(PAXOFF),DMCB,(RC),LAGOFC1H,MYIOAREA,RR=Y                      
         BZ    AGED1030            OKAY RETURN                                  
         L     R2,DUB              A(ERROR FIELD)                               
         B     MYERR               ERROR RETURN - MESSAGE ALREADY SET           
*                                                                               
AGED1030 BAS   RE,ADDREC           ADD RAGY2 REC                                
         MVC   KEY+28(4),KEY                                                    
         MVC   RAGY2DA,KEY+28      SAVE D/A OF AGY2 RECORD                      
*                                                                               
AGED1040 DS    0H                  PROCESS PASSIVE POINTER(S)                   
         MVC   DAREDKAD,KEY+28                                                  
         GOTO1 =A(DAREKEYS),RR=Y                                                
*                                                                               
AGED1060 DS    0H                                                               
         GOTO1 =A(AGYCMT),DMCB,(RC),AIOAREA,RR=Y REDISP AGY COMMENTS            
*                                                                               
         L     R4,AIOSV            RESTORE                                      
         ST    R4,AIOAREA                                                       
*                                                                               
AGED1080 EQU   *                                                                
* DIE IF KEY CHANGED FOR ANY REASON!!                                           
         CLI   BACT,C'A'           FOR ACTION CHANGE ONLY                       
         BE    FLFILE                                                           
         CLC   TRAPKEY,REC                                                      
         BE    *+6                                                              
         DC    H'0'                DIE, DIE, DIE!                               
*                                                                               
         B     FLFILE                                                           
         EJECT                                                                  
AGYPSV   CLI   BACT,C'A'           TEST ADD                                     
         BE    AGYPSVA                                                          
         LA    R2,LFMLAST                                                       
         BAS   RE,NEXTUF           POINT TO NAME FIELD                          
         TM    4(R2),X'20'         TEST CHANGED                                 
         BO    AGPSV020            NO  - DON'T DO ANYTHING HERE                 
* DELETE OLD PASSIVE POINTER                                                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'8A'                                                        
         MVC   KEY+1(18),RAGYNAM1-RAGYREC+REC2                                  
         MVC   KEY+19(8),REC2+19                                                
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'                                                     
         BAS   RE,WRITE                                                         
         BAS   RE,CHECK                                                         
*                                                                               
AGYPSVA  XC    KEY,KEY             BUILD NEW PASSIVE POINTER                    
         MVI   KEY,X'8A'                                                        
         MVC   KEY+1(18),RAGYNAM1                                               
         MVC   KEY+19(8),REC+19                                                 
         MVC   KEY+28(4),BSVDA                                                  
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         BAS   RE,HIGH                                                          
         LA    RF,ADD                                                           
         CLC   KEYSAVE(27),KEY                                                  
         BNE   *+8                                                              
         LA    RF,WRITE                                                         
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         BASR  RE,RF                                                            
         BAS   RE,CHECK                                                         
*                                                                               
AGPSV020 EQU   *                                                                
         LA    R2,LAGTCODH         SET A(TERRITORY HDR)                         
         TM    4(R2),X'20'         CHANGED?                                     
         BO    AGPSV100            NO  - LEAVE KEYS ALONE                       
* DELETE OLD PASSIVE POINTER                                                    
         OC    OLDTERR,OLDTERR     ANY TERRITORY ENTERED?                       
         BZ    AGPSV040            NO  - NOTHING TO DELETE                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'AA'                                                        
         MVC   KEY+17(2),REPALPHA  INSERT REP CODE                              
         MVC   KEY+19(2),OLDTERR   INSERT OLD TERRITORY CODE                    
         MVC   KEY+21(6),REC2+19   INSERT OLD AGENCY/AGYOFF                     
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   AGPSV040            NOT ON FILE: DON'T DELETE                    
*                                                                               
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         OI    KEY+27,X'80'                                                     
         BAS   RE,WRITE                                                         
         BAS   RE,CHECK                                                         
*                                                                               
AGPSV040 EQU   *                                                                
         OC    NEWTERR,NEWTERR     ANY NEW TERRITORY CODE?                      
         BZ    AGPSV100            NO  - DON'T PUT OUT KEY                      
         XC    KEY,KEY             BUILD NEW PASSIVE POINTER                    
         MVI   KEY,X'AA'                                                        
         MVC   KEY+17(2),REPALPHA  INSERT REP CODE                              
         MVC   KEY+19(2),NEWTERR   INSERT OLD TERRITORY CODE                    
         MVC   KEY+21(6),REC+19                                                 
*                                  INSERT NEW TERRITORY CODE                    
         MVC   KEY+28(4),RAGY2DA   INSERT AGENCY2 RECORD D/A                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         BAS   RE,HIGH                                                          
         LA    RF,ADD                                                           
         CLC   KEYSAVE(27),KEY                                                  
         BNE   *+8                                                              
         LA    RF,WRITE                                                         
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         BASR  RE,RF                                                            
         BAS   RE,CHECK                                                         
AGPSV100 EQU   *                                                                
         GOTO1 =A(AGY2PTR),RR=Y                                                 
         B     EXXMOD                                                           
*                                                                               
CHECK    TM    DMCB+8,X'FD'                                                     
         BCR   8,RE                                                             
         DC    H'0'                                                             
         TITLE 'T80403 - ADV/PRD/AGY RECORDS'                                   
FLFILE   CLI   BACT,C'A'           TEST ADD                                     
         BE    FLADD                                                            
* CHANGE - READ REC THEN WRITE NEW                                              
         LA    R4,REC2                                                          
         LA    R5,REC                                                           
         BAS   RE,MOVEREC          MOVE REC TO REC2                             
         MVC   KEY(28),REC                                                      
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,GETREC                                                        
         LA    R4,REC2                                                          
         LA    R5,REC                                                           
         BAS   RE,XCREC            SWAP REC AND REC2                            
         BAS   RE,PUTREC                                                        
         B     FLFILE2                                                          
*                                                                               
FLADD    EQU   *                                                                
         LA    RF,REC                                                           
         CLI   REC,X'08'           ADVERTISER RECORD?                           
         BNE   FLADD020            NO  - CHECK AGY                              
         CLC   =C'ALL ',RADVKADV   PROHIBITED ADVERTISER?                       
         BE    FLERR17             YES                                          
         B     FLADD040                                                         
FLADD020 EQU   *                                                                
         CLI   REC,X'0A'           AGENCY RECORD?                               
         BNE   FLADD040            NO  - PROCEED                                
         CLC   =C'ALL ',RAGYKAGY   PROHIBITED AGENCY?                           
         BE    FLERR17             YES                                          
FLADD040 EQU   *                                                                
         BAS   RE,ADDREC                                                        
*                                                                               
         MVC   BSVDA,KEY           SAVE DISK ADDRESS                            
*                                                                               
FLFILE2  CLI   BREC,8                                                           
         BE    ADVE0240                                                         
         CLI   BREC,10                                                          
         BE    AGYPSV                                                           
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY LIABILITY COMMENT RECORDS                                  
***********************************************************************         
DISPFC   DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCMTD,R6                                                         
         MVI   RCMTKTYP,X'2E'      RECORD TYPE                                  
         MVC   RCMTKREP,REPALPHA   REP CODE                                     
         MVC   RCMTKOFF,=X'FFFF'   OFFICE CODE                                  
         MVC   RCMTKCDE(4),=C'LIAB'  COMMENT CODE                               
         MVC   RCMTKCDE+4(2),8(R2)                                              
         OC    RCMTKCDE,SPACES     BLANK PADDED                                 
         DROP  R6                                                               
*                                                                               
DISPFCED LR    R5,RE               SAVE RETURN ADDRESS                          
         ZIC   RF,0(R2)            BUMP TO DISPLAY FIELD                        
         AR    R2,RF                                                            
         MVC   8(50,R2),SPACES                                                  
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BNE   DISPFCX                                                          
*                                                                               
         L     R4,AIOAREA          SAVE OFF REC, SO WE WON'T                    
         ST    R4,AIOSV            CLOBBER IT                                   
         LA    R4,MYIOAREA                                                      
         ST    R4,AIOAREA                                                       
         BAS   RE,GETREC                                                        
         L     R6,AIOAREA                                                       
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   DISPFC50                                                         
         USING RCMTELM2,R6                                                      
*                                                                               
DISPFC10 DS    0H                                                               
         CLI   RCMT2LEN,3          GET FIRST NON-BLANK COMMT LINE               
         BH    DISPFC20                                                         
         CLI   RCMT2TXT,C' '                                                    
         BNE   DISPFC20                                                         
         BAS   RE,NEXTEL           R4 HAS ADDRESS OF FIRST ELEMENT              
         BE    DISPFC10                                                         
         B     DISPFC50                                                         
*                                                                               
DISPFC20 DS    0H                                                               
         CLI   RCMT2LEN,50         COMMT FIELD HAS THIS MUCH ROOM               
         BH    DISPFC45                                                         
         ZIC   R1,RCMT2LEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     DISPFC50                                                         
         MVC   8(0,R2),RCMT2TXT                                                 
         B     DISPFC50                                                         
*                                                                               
DISPFC45 MVC   8(50,R2),RCMT2TXT                                                
         DROP  R6                                                               
*                                                                               
DISPFC50 L     R4,AIOSV            RESTORE                                      
         ST    R4,AIOAREA                                                       
*                                                                               
DISPFCX  DS    0H                                                               
         LR    RE,R5                                                            
         BR    RE                                                               
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
         BR    RE                                                               
NEXTUF2  CLI   9(R2),0             CHECK FOR LAST                               
         BNE   NEXTUF4                                                          
         CR    R2,R2               IF LAST, SET CC=                             
         BR    RE                                                               
NEXTUF4  LTR   R2,R2               NOT LAST, SET CC NOT=                        
         BR    RE                                                               
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
         SPACE 2                                                                
FLERR1   LA    R3,MSSNGERR                                                      
         B     ERROR                                                            
FLERR2   LA    R3,INVERR                                                        
         B     ERROR                                                            
FLERR2A  EQU   *                                                                
         MVC   LFMMSG(L'PROFERR),PROFERR                                        
         B     MYERR                                                            
FLERR3   LA    R3,NUMERR                                                        
         B     ERROR                                                            
FLERR4   EQU   *                                                                
         MVC   LFMMSG(L'RISKERR),RISKERR                                        
         B     MYERR                                                            
FLERR5   EQU   *                                                                
         MVC   LFMMSG(L'LIABERR),LIABERR                                        
         B     MYERR                                                            
FLERR6   EQU   *                                                                
         MVC   LFMMSG(L'LIABRERR),LIABRERR                                      
         B     MYERR                                                            
FLERR7   EQU   *                                                                
         MVC   LFMMSG(L'FAXERR),FAXERR                                          
         B     MYERR                                                            
*                                                                               
FLERR8   EQU   *                                                                
         MVC   LFMMSG(L'DOFFERR),DOFFERR                                        
         B     MYERR                                                            
*                                                                               
FLERR9   EQU   *                                                                
         MVC   LFMMSG(L'DDUPERR),DDUPERR                                        
         B     MYERR                                                            
*                                                                               
FLERR10  EQU   *                                                                
         MVC   LFMMSG(L'DAGYERR),DAGYERR                                        
         B     MYERR                                                            
*                                                                               
FLERR11  EQU   *                                                                
         MVC   LFMMSG(L'CTRLERR),CTRLERR                                        
         B     MYERR                                                            
*********************************************JR**                               
FLERR12  EQU   *                                                                
         MVC   LFMMSG(L'AEQNERR),AEQNERR                                        
         B     MYERR                                                            
FLERR13  EQU   *                                                                
         MVC   LFMMSG(L'AEQFERR),AEQFERR                                        
         B     MYERR                                                            
FLERR14  LA    R3,547                                                           
         B     ERROR                                                            
FLERR15  EQU   *                                                                
         MVC   LFMMSG(L'NOCHANG),NOCHANG                                        
         B     MYERR                                                            
FLERR16  EQU   *                                                                
         MVC   LFMMSG(L'AEQOERR),AEQOERR                                        
         B     MYERR                                                            
FLERR17  EQU   *                                                                
         LA    R2,LFMKEYH          SET A(ERROR)                                 
         MVC   LFMMSG(L'ALLNGCOD),ALLNGCOD                                      
         B     MYERR                                                            
****                                                                            
*                                                                               
*- MSG ALREADY SET UP IN HEADER                                                 
MYERR    MVI   ERRAREA,X'FF'       MSG ALREADY DONE                             
         B     EXIT                                                             
*                                                                               
ZEROS    DC    30C'0'                                                           
BLANKS   DC    CL30' '                                                          
*                                                                               
LIABERR  DC    C'LIABILITY POSITION COMMENT MUST BE 01 THRU 99.'                
RISKERR  DC    C'CREDIT RATING MUST BE 1 THRU 6.'                               
PROFERR  DC    C'10TH PROFILE MAY BE N/Y/L/F ONLY'                              
LIABRERR DC    C'LIABILITY RECORD NOT FOUND'                                    
FAXERR   DC    C'NEED VALID FAX NUMBER FOR EASYLINK CONTRACT COPY'              
DOFFERR  DC    C'INVALID DARE AGENCY OFFICE'                                    
DDUPERR  DC    C'DUPLICATE DARE AGENCY ASSIGNMENT'                              
DAGYERR  DC    C'FORMAT MUST BE XXXX OR XX-XX (IE. AB-NY)'                      
CTRLERR  DC    C'RESET VALUE FOR CONTROL= IS NOT NUMERIC '                      
AEQNERR  DC    C'AGENCY EQUIV MUST BE 4 NUMERIC CHARACTERS'                     
AEQOERR  DC    C'EQUIV OFFICE MUST BE 2 CHARACTERS'                             
AEQFERR  DC    C'AGENCY EQUIV FIELD EXISTS IN THE FILE   '                      
NOCHANG  DC    C'ADDRESS CHANGE IS PROHIBITED FOR AGENCY '                      
ALLNGCOD DC    C'"ALL" IS NOT PERMITTED AS A CODE '                             
*                                                                               
         GETEL R6,34,ELCODE                                                     
*                                                                               
                                                                                
                                                                                
                                                                                
                                                                                
                                                                                
*** STORAGE AREA                                                                
ELCODE   DS    X                                                                
AIOSV    DS    A                                                                
AIOSV2   DS    A                                                                
MYWORK   DS    CL64                MY WORK AREA                                 
DAREAGY  DS    CL20                4 * DARE AGENCY+OFFICE                       
DAREDKAD DS    XL4                 D/A OF X'1A' REC                             
OLDTERR  DS    CL2                 OLD TERRITORY CODE                           
NEWTERR  DS    CL2                 NEW TERRITORY CODE                           
ELEM     DS    CL256                                                            
OLDNAME  DS    CL20                OLD SCREEN NAME                              
*                                                                               
TRAPKEY  DS    CL27                TEMPORARY KEY TO TRAP WRONG AGENCY           
PROFNINE DS    CL1                 HOLD PROFILE BYTE NINE                       
*                                  REC PUTREC                                   
EQUVFLAG DS    CL1                                                              
RAGY2DA  DS    A                                                                
*                                                                               
*                                                                               
       ++INCLUDE RERISKTAB                                                      
         EJECT                                                                  
       ++INCLUDE RELFMGEN                                                       
         EJECT                                                                  
*                                                                               
T80403   CSECT                                                                  
         LTORG                                                                  
MYIOAREA DS    1000C               FOR STORED COMMENTS                          
         DROP  RB                                                               
         EJECT                                                                  
SETDONT  NTR1  BASE=*,LABEL=*                                                   
**       LA    R2,LADADDDH         SET A(DO NOT DROP) FIELD                     
**       NI    RADVFLGS,X'FF'-X'01'   TURN OFF 'DO NOT DROP' FLAG               
**       CLI   5(R2),0             ANY DATA IN FIELD?                           
**       BE    SETD0040            NO                                           
**       CLI   8(R2),C'N'          YES - ONLY 'N' OR 'Y' VALID                  
**       BE    SETD0040                                                         
**       CLI   8(R2),C'Y'                                                       
**       BNE   SETD0800                                                         
**       OI    RADVFLGS,X'01'      SET 'DO NOT DROP' FLAG                       
SETD0040 DS    0H                                                               
         LA    R2,LADADDUH         SET A(DO NOT USE) FIELD                      
         NI    RADVFLGS,X'FF'-X'02'   TURN OFF 'DO NOT USE' FLAG                
         CLI   5(R2),0             ANY DATA IN FIELD?                           
         BE    SETD0060            NO                                           
         CLI   8(R2),C'N'          YES - ONLY 'N' OR 'Y' VALID                  
         BE    SETD0060                                                         
         CLI   8(R2),C'Y'                                                       
         BNE   SETD0800                                                         
         OI    RADVFLGS,X'02'      SET 'DO NOT USE' FLAG                        
SETD0060 DS    0H                                                               
         B     SETD0820            EXIT CC ZERO                                 
SETD0800 DS    0H                                                               
         ST    R2,DUB              SAVE A(FIELD IN ERROR)                       
         LTR   RB,RB               SET CC NOT ZERO                              
         B     SETD0900            EXIT                                         
SETD0820 EQU   *                                                                
         SR    R0,R0               SET CC ZERO                                  
SETD0900 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
TRADATA  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIOAREA                                                       
         USING RAGY2REC,R6                                                      
         MVC   DUB(2),RAGK2AOF     SAVE AGENCY OFFICE, IF ANY                   
         DROP  R6                                                               
*                                                                               
         XC    LAGATRD(20),LAGATRD CLEAR OLD FIELD                              
         L     R6,AIOAREA          RETRIEVE 1F ELEMENT                          
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNZ   TRAD0040            NOT FOUND                                    
         USING RAG2ELEM,R6                                                      
         CLI   RAG2TRAD,C' '       ANY TRADE INFORMATION?                       
         BNH   TRAD0040            NO                                           
         LA    RF,LAGATRD          SET A(TRADE INFO)                            
         MVI   0(RF),C'#'          INSERT FIRST INDICATOR                       
         CLC   DUB(2),=C'  '       ANY AGENCY OFFC IN RECORD?                   
         BH    TRAD0020            YES                                          
         MVI   1(RF),C'0'          NO  - INSERT CORPORATE ZERO                  
         MVC   3(2,RF),=C'OF'                                                   
         MVI   6(RF),C'#'          INSERT 2ND   INDICATOR                       
         MVC   7(1,RF),RAG2TRAD    INSERT HIGH AGY OFF CODE                     
         B     TRAD0040                                                         
TRAD0020 EQU   *                                                                
         MVC   1(1,RF),RAG2TRAD    INSERT AGENCY OFFICE CODE                    
TRAD0040 EQU   *                                                                
         FOUT  LAGATRDH                                                         
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
GAINSPC1 NTR1  BASE=*,LABEL=*                                                   
         L     R6,0(R1)                                                         
         USING RAGY2FXE,R6                                                      
*                                                                               
* VALIDATE & BUILD AGENCY COMMENTS                                              
*                                  SAVE A(1A AGENCY2 RECORD)                    
         GOTO1 =A(CMTRNT),DMCB,(RC),AIOAREA,RR=Y                                
*                                                                               
*                                  DISPLAY DATE FROM FILE                       
         GOTO1 VDATCON,DMCB,(5,0),(2,RAGY2LCD)                                  
*                                  INSERT CHANGE DATE INTO RECORD               
         GOTO1 VDATCON,DMCB,(2,RAGY2LCD),(5,LAGAGLC)                            
*                                  INSERT CHANGE DATE INTO SCREEN               
         FOUT  LAGAGLCH                                                         
*                                  ADD CHA SCREEN                               
*                                                                               
* VALIDATE & BUILD PAXSON OFFICE ELEMENTS                                       
*                                                                               
         CLI   BACT,C'A'           'ADD' ACTION?                                
         BNE   AGED0920            NO                                           
         TM    SVPGPBIT,X'01'      AUTO-ASSIGN CODE NUMBER?                     
         BNO   AGED0920            NO  - KEY FULLY SET UP                       
         GOTO1 =A(AUTOASSN),DMCB,(RC),RR=Y                                      
*                                  YES - RETRIEVE NEXT CODE NUMBER              
AGED0920 EQU   *                                                                
         DROP  R6                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE X'BA' AGY2 PASSIVE PTR                                                 
***********************************************************************         
AGY2PTR  NTR1  BASE=*,LABEL=*                                                   
         CLI   BACT,C'A'           TEST ADD                                     
         BE    AGY2P10             NEED PTR                                     
         CLC   RAGYNAM1(18),RAGYNAM1-RAGYREC+REC2                               
         BE    AGY2PX              AGY NAME NOT CHANGED SIGNIFICANTLY           
*                                                                               
         XC    KEY,KEY             DELETE OLD PTR                               
         MVI   KEY,X'BA'                                                        
         MVC   KEY+1(18),RAGYNAM1-RAGYREC+REC2                                  
         MVC   KEY+19(8),REC2+19                                                
         MVI   DMOUTBTS,0          NO ERROR INTERRUPT                           
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   AGY2P10                                                          
         OI    KEY+27,X'80'                                                     
         BAS   RE,WRITE                                                         
*                                                                               
AGY2P10  DS    0H                                                               
         XC    KEY,KEY             BUILD NEW PASSIVE POINTER                    
         MVI   KEY,X'BA'                                                        
         MVC   KEY+1(18),RAGYNAM1                                               
         MVC   KEY+19(8),REC+19                                                 
         MVI   DMOUTBTS,0          NO ERROR INTERRUPT                           
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BNE   AGY2P20                                                          
         NI    KEY+27,X'FF'-X'80'  RESTORE KEY                                  
         MVC   KEY+28(4),RAGY2DA                                                
         BAS   RE,WRITE                                                         
         B     AGY2PX                                                           
*                                                                               
AGY2P20  DS    0H                  WRITE NEW PTR                                
         MVC   KEY,KEYSAVE         RESTORE KEY WE BUILT                         
         MVC   KEY+28(4),RAGY2DA                                                
         BAS   RE,ADD                                                           
AGY2PX   DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
********************************************************************            
* ADD AGY2 '1F' ELEMENT - BUILDS 1F ELEM IN 'ELEM'                              
********************************************************************            
ADAGY21F NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIOAREA          RETRIEVE AGY2 REC 1F ELEMENT                 
         MVI   HALF2,0             CLEAR TEMPORARY STORAGE                      
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNZ   ADAG0020            NOT FOUND                                    
         USING RAG2ELEM,R6                                                      
         MVC   HALF2(1),RAG2TRAD   SAVE TRADE/AGENCY BYTE                       
         DROP  R6                                                               
ADAG0020 EQU   *                                                                
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING RAG2ELEM,R6                                                      
         MVI   RAG2CODE,X'1F'                                                   
         MVI   RAG2ELLN,RAG2ELLQ                                                
         MVC   RAG2NAM1,RAGYNAM1                                                
         MVC   RAG2NAM2,RAGYNAM2                                                
         MVC   RAG2PROS,RAGYPROS                                                
         MVC   RAG2RISK,RAGYRISK                                                
         MVC   RAG2LIAB,RAGYLIAB                                                
         MVC   RAG2ASSN,RAGYASSN                                                
         MVC   RAG2TRAD,HALF2      RESTORE TRADE/AGENCY BYTE                    
*                                                                               
         MVC   RAG2FLAG,RAGYFLAG   SET NEW FLAG BYTE TO OLD                     
         NI    RAG2FLAG,X'FF'-X'C0'   TURN OFF X'80' & X'40' FLAGS              
**                                                                              
**       TM    RAGYFLAG,X'20'      PROPAGATE 20,10,08,04,02,01 FLAGS            
**       BZ    *+8                                                              
**       OI    RAG2FLAG,X'20'                                                   
**       TM    RAGYFLAG,X'10'                                                   
**       BZ    *+8                                                              
**       OI    RAG2FLAG,X'10'                                                   
**       TM    RAGYFLAG,X'08'                                                   
**       BZ    *+8                                                              
**       OI    RAG2FLAG,X'08'                                                   
         XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
* PROCESS PASSIVE KEYS FOR DARE AGENCY ASSIGNMENTS                              
* DAREAGY HAS ORIGINAL LIST OF DARE AGENCY ASSIGNMENTS BEFORE UPDATE            
***********************************************************************         
DAREKEYS NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIOAREA                                                       
         USING RAGY2REC,R6                                                      
                                                                                
         MVC   ELEM(20),RAGY2DAR   SAVE OFF NEW AGENCY ASSIGNMENTS              
         OC    ELEM,SPACES                                                      
         OC    DAREAGY,SPACES                                                   
         CLC   DAREAGY,SPACES      NO OLD POINTERS TO DELETE                    
         BE    DAREK30                                                          
         CLC   DAREAGY,ELEM        DIDN'T CHANGE, DONT ADD/DEL ANYTHING         
         BE    DAREKX                                                           
                                                                                
         LA    R4,DAREAGY                                                       
         LA    R5,4                MAX 4 DARE AGENCIES                          
         DROP  R6                                                               
                                                                                
DAREK10  DS    0H                                                               
         CLC   0(5,R4),SPACES      SKIP IF NO MORE                              
         BE    DAREK30                                                          
                                                                                
         LA    R6,MYIOAREA                                                      
         USING RAGKDKEY,R6                                                      
         XC    MYIOAREA(32),MYIOAREA                                            
         MVI   RAGKDTYP,RAGKDTYQ                                                
         MVC   RAGKDREP,RAGYKREP                                                
         MVC   RAGKDDAG(5),0(R4)                                                
         MVC   RAGKDAGY,RAGYKAGY                                                
         MVC   RAGKDAOF,RAGYKAOF                                                
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(L'RAGKDKEY),RAGKDKEY                                         
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   DAREK20             NONE FOUND                                   
         MVC   KEY(27),KEYSAVE     RESTORE KEY                                  
         OI    KEY+27,X'80'        MARK FOR DELETION                            
         GOTO1 WRITE                                                            
         DROP  R6                                                               
*                                                                               
DAREK20  DS    0H                                                               
         LA    R4,5(R4)                                                         
         BCT   R5,DAREK10                                                       
*                                                                               
DAREK30  DS    0H                                                               
         LA    R4,ELEM             NEW AGENCIES                                 
         LA    R5,4                                                             
                                                                                
DAREK40  DS    0H                                                               
         CLC   0(5,R4),SPACES      SKIP IF NO MORE                              
         BE    DAREKX                                                           
                                                                                
         LA    R6,MYIOAREA                                                      
         USING RAGKDKEY,R6                                                      
                                                                                
         XC    MYIOAREA(32),MYIOAREA                                            
         MVI   RAGKDTYP,RAGKDTYQ                                                
         MVC   RAGKDREP,RAGYKREP                                                
         MVC   RAGKDDAG(5),0(R4)                                                
         MVC   RAGKDAGY,RAGYKAGY                                                
         MVC   RAGKDAOF,RAGYKAOF                                                
                                                                                
         MVC   KEY,RAGKDKEY                                                     
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,0          NO ERROR TESTS                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   DAREK45                                                          
         MVC   KEY(27),KEYSAVE     RESTORE KEY + CTRL BYTE                      
         MVI   KEY+27,0                                                         
         BAS   RE,WRITE                                                         
         B     DAREK48                                                          
                                                                                
DAREK45  DS    0H                  NOT FOUND, ADD NEW                           
         MVC   KEY(28),KEYSAVE                                                  
         MVC   KEY+28(4),DAREDKAD                                               
         BAS   RE,ADD                                                           
                                                                                
DAREK48  DS    0H                                                               
         BAS   RE,CHECK                                                         
         DROP  R6                                                               
*                                                                               
DAREK50  DS    0H                                                               
         LA    R4,5(R4)                                                         
         BCT   R5,DAREK40                                                       
*                                                                               
DAREKX   DS    0H                                                               
         B     EXXMOD                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*  AUTOASSN:  RETRIEVE CODE 0000 AGY OR ADV KEY.  DERIVE NEXT AVAIL-            
*        ABLE CODE.  UPDATE THE CONTROL, AND REWRITE IT.                        
***********************************************************************         
AUTOASSN NMOD1 0,*AUTO*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         MVC   REC+25(2),REPALPHA  INSERT AGENCY CODE INTO KEY                  
         MVC   REC(1),BREC         INSERT RECORD TYPE INTO KEY                  
         LA    R4,REC2                                                          
         LA    R5,REC                                                           
         BAS   RE,MOVEREC          MOVE NEW REC TO REC2                         
         MVC   KEY(27),REC         ESTABLISH CONTROL RECORD KEY                 
         CLI   BREC,8              ADVERTISER RECORD?                           
         BNE   AASS0020            NO                                           
         MVC   KEY+21(4),=C'0000'                                               
*                                  YES - INSERT CONTROL KEY CODE                
         B     AASS0040                                                         
AASS0020 EQU   *                   NO  - AGENCY RECORD                          
         MVC   KEY+19(6),=C'0000  '                                             
*                                  INSERT CONTROL KEY CODE W/OFFICE             
AASS0040 EQU   *                   NO  - AGENCY RECORD                          
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    *+6                                                              
         DC    H'0'                NOT FOUND                                    
         BAS   RE,GETREC                                                        
         CLI   BREC,8              ADVERTISER RECORD?                           
         BNE   AASS0060            NO                                           
         ZICM  R2,RADVASSN,2       YES - GET ADVERTISER ASSIGN NUMBER           
         LA    R2,1(R2)            BUMP TO NEXT NUMBER                          
         STCM  R2,3,RADVASSN       PUT BACK NEXT NUMBER                         
         B     AASS0080                                                         
AASS0060 EQU   *                                                                
         ZICM  R2,RAGYASSN,2       NO  - GET AGENCY     ASSIGN NUMBER           
         LA    R2,1(R2)            BUMP TO NEXT NUMBER                          
         STCM  R2,3,RAGYASSN       PUT BACK NEXT NUMBER                         
AASS0080 EQU   *                                                                
         BAS   RE,PUTREC           REWRITE CONTROL RECORD                       
         LA    R4,REC                                                           
         LA    R5,REC2             MOVE NEW RECORD BACK INTO PLACE              
         BAS   RE,MOVEREC          MOVE OLD REC2 TO REC                         
         CLI   BREC,8              ADVERTISER RECORD?                           
         BNE   AASS0100            NO                                           
         EDIT  (R2),(4,RADVKADV),FILL=0                                         
*                                  YES - EDIT CODE INTO KEY                     
         B     AASS0120                                                         
AASS0100 EQU   *                   NO  - AGENCY KEY                             
         EDIT  (R2),(4,RAGYKAGY),FILL=0                                         
*                                  EDIT CODE INTO KEY                           
         MVC   RAGYKAOF,SPACES     SET AGENCY OFFICE TO SPACES                  
AASS0120 EQU   *                                                                
         XC    LFMKEY(L'LFMKEY),LFMKEY                                          
*                                  CLEAR THE KEY                                
         EDIT  (R2),(4,LFMKEY),FILL=0                                           
         LA    R2,LFMKEYH          A(KEY FIELD)                                 
         OI    6(R2),X'80'         SET TRANSMIT BIT                             
         MVC   KEY,REC             INSERT REBUILT KEY                           
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   DISPLAY ROUTINE FOR ADVERTISER DATA                                         
*                                                                               
ADVFRMAT NMOD1 0,*ADVF*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RADVNAME,R2),RADVNAME                                        
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET VALIDATED BIT                            
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RADVCITY,R2),RADVCITY                                        
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RADVCATG,R2),RADVCATG                                        
         FOUT  (R2)                                                             
*                                                                               
         MVC   LADAVEQ,SPACES      CLEAR CONVERT ADVERTISER                     
*                                                                               
         MVC   ORIGADV,RADVKATZ    SAVE LAST ADVERT IN TWA                      
         TM    SVPGPBIT+1,X'01'    CONVERT BIT ON??                             
         BO    ADV060              YES                                          
         TM    SVPGPBIT+1,X'40'                                                 
         BNO   ADV080                                                           
ADV060   EQU   *                                                                
         MVC   LADAVEQ(5),RADVKATZ                                              
*                                  INSERT ORIGINAL ADVERTISER                   
ADV080   EQU   *                                                                
         FOUT  LADAVEQH                                                         
*                                                                               
*        LA    R2,LADADDDH         SET A(DO NOT DROP) FIELD                     
*        MVI   8(R2),C'N'          SET 'DO NOT DROP' TO NO                      
*        TM    RADVFLGS,X'01'      'DO NOT DROP' SET?                           
*        BNO   ADV085              NO                                           
*        MVI   8(R2),C'Y'          YES - SET TO YES                             
ADV085   EQU    *                                                               
*        FOUT (R2)                 TRANSMIT 'DO NOT DROP' FIELD                 
*                                                                               
*  DO NOT SWITCH FIELD IS INACTIVE AT THIS TIME.                                
*     WHEN IT IS NEEDED/WANTED, THE FIELD MUST BE ACTIVATED ON THE              
*     SCREEN, AND THIS CODE REACTIVATED.                                        
*                                                                               
         LA    R2,LADADDUH         SET A(DO NOT USE) FIELD                      
         MVI   8(R2),C'N'          SET 'DO NOT USE' TO NO                       
         TM    RADVFLGS,X'02'      'DO NOT USE' SET?                            
         BNO   ADV090              NO                                           
         MVI   8(R2),C'Y'          YES - SET TO YES                             
ADV090   EQU    *                                                               
         FOUT (R2)                 TRANSMIT 'DO NOT USE' FIELD                  
*                                                                               
         XC    LADAVLC,LADAVLC     CLEAR THE DATE                               
         OC    RADVLCD,RADVLCD     ANY LAST CHANGED DATE?                       
         BZ    ADV100              NO  - SKIP IT                                
*                                                                               
         GOTO1 VDATCON,DMCB,(2,RADVLCD),(5,LADAVLC)                             
ADV100   EQU   *                                                                
         FOUT  LADAVLCH                                                         
*                                                                               
         GOTO1 =A(ADVDISP),DMCB,(RC),RR=Y                                       
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   RELOCATED DISPLAY CODE FOR ADVERTISER RECORD                                
*                                                                               
ADVDISP  NMOD1 0,**ADVD**                                                       
         L     RC,0(R1)                                                         
         LA    R4,REC2             SAVE THE ADVERTISER RECORD                   
         LA    R5,REC                                                           
         BAS   RE,MOVEREC          MOVE REC TO REC2                             
*                                                                               
         LA    R2,LADCATCH         SET A(CATEGORY NAME FIELD)                   
         MVC   8(L'RCTGNAME,R2),SPACES                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'0F'           CATEGORY RECORD                              
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),RADVCATG                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   ADDI0020                                                         
*                                                                               
         BAS   RE,GETREC           EXPAND CATEGORY NAME                         
         MVC   8(L'RCTGNAME,R2),RCTGNAME                                        
*                                                                               
ADDI0020 FOUT  (R2)                                                             
*                                                                               
         LA    R4,REC              RESTORE THE ADVERTISER RECORD                
         LA    R5,REC2                                                          
         BAS   RE,MOVEREC          MOVE REC2 TO REC                             
*                                                                               
         LA    R2,LADOFC1H         SET A(1ST PAXSON OFFICE DISPLAY)             
         LA    R0,6                CLEAR ALL OFFICE FIELDS                      
ADDI0040 EQU   *                                                                
         MVC   8(2,R2),SPACES      CLEAR TO SPACES                              
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO NEXT FIELD                           
         BCT   R0,ADDI0040         GO BACK FOR NEXT                             
*                                                                               
         LA    R2,LADOFC1H         SET A(1ST PAXSON OFFICE DISPLAY)             
         LA    R6,RADVREC          RETRIEVE 'PAXSON' OFFICE(S)                  
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL                                                         
         B     ADDI0080                                                         
ADDI0060 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
ADDI0080 EQU   *                                                                
         BNZ   ADDI0100            NOT FOUND                                    
         MVC   8(2,R2),2(R6)       INSERT OFFICE FROM ADV RECORD                
         FOUT  (R2)                SET TO TRANSMIT                              
         BAS   RE,NEXTUF                                                        
         B     ADDI0060            GO BACK FOR NEXT ELEMENT                     
ADDI0100 EQU   *                                                                
*          DATA SET RELFM03A   AT LEVEL 017 AS OF 03/11/99                      
*                                                                               
* NEW FOR ALTERNATE AGENCY ADDRESS                                              
*                                                                               
*  -  CLEAR SCREEN OF CTYPE, OFFICE AND ADDRESSES                               
*                                                                               
         TWAXC LADCTR1H            CLEAR SCREEN                                 
*                                                                               
         XC    BYTE4,BYTE4                                                      
         MVI   BYTE4,1              SET COUNTER TO ONE                          
         LA    R2,LADCTR1H         SET A(1ST ADDRESS LINE)                      
         LA    R4,LADADD1H                                                      
         LA    R6,RADVREC                                                       
*                                                                               
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         B     ADDI0110                                                         
         USING RADVAGEL,R6                                                      
*                                                                               
ADDI0105 BAS   RE,NEXTEL                                                        
*                                                                               
ADDI0110 BNE   ADDI00XX                                                         
*                                                                               
** SHOW CONTYPE                                                                 
*                                                                               
         CLC   RADVAGTY,SPACES     IS CONTYPE BLANK OR NULLS?                   
         BNH   ADDI0115            YES, SKIP TO OFFICE                          
         MVC   8(1,R2),RADVAGTY                                                 
         FOUT  (R2)                SET TO TRANSMIT                              
*                                                                               
ADDI0115 DS    0H                                                               
         BAS   RE,NEXTUF           POINT TO OFFICE                              
         CLC   RADVAGOF,SPACES     IS CONTYPE BLANK OR NULLS?                   
         BNH   ADDI0120            YES, SKIP TO ADDRESS                         
         MVC   8(2,R2),RADVAGOF                                                 
         FOUT  (R2)                SET TO TRANSMIT                              
*                                                                               
ADDI0120 DS    0H                                                               
         LR    R2,R4               POINT TO ADDRESS                             
         MVC   8(L'LADADD1,R2),RADVAGA1                                         
         FOUT  (R2)                SET TO TRANSMIT                              
         BAS   RE,NEXTUF                                                        
         BAS   RE,NEXTUF                                                        
         MVC   8(L'LADADD1,R2),RADVAGA2                                         
         FOUT  (R2)                SET TO TRANSMIT                              
         BAS   RE,NEXTUF                                                        
         BAS   RE,NEXTUF                                                        
         MVC   8(L'LADADD3,R2),RADVAGA3                                         
         FOUT  (R2)                SET TO TRANSMIT                              
         DROP  R6                                                               
*                                                                               
* CHECK FOR NEXT ALTERNATE ADDRESS BLOCK                                        
*                                                                               
         ZIC   R1,BYTE4                                                         
         LA    R1,1(R1)            ADD ONE                                      
         STC   R1,BYTE4                                                         
         CLI   BYTE4,2             IS IT SECOND ADD?                            
         BE    DISNXT2                                                          
         SR    R0,R0               SET CC TO ZERO                               
         B     ADDI00XX                                                         
*                                                                               
DISNXT2  DS    0H                  CHECK NEXT BLOCK AND ELEMENT                 
         LA    R2,LADCTR2H                                                      
         LA    R4,LADADDAH                                                      
         B     ADDI0105                                                         
*                                                                               
ADDI00XX FOUT  (R2)                SET TO TRANSMIT                              
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   RELOCATED DISPLAY CODE FOR AGENCY RECORD                                    
*                                                                               
AGYDIS   NMOD1 0,**AGYD**                                                       
         L     RC,0(R1)                                                         
         L     R3,4(R1)            SET A(MYIOAREA)                              
         USING RAGY2REC,R3                                                      
*                                                                               
         LA    R2,LAGOFC1H         SET A(1ST PAXSON OFFICE DISPLAY)             
         LA    R0,6                CLEAR ALL OFFICE FIELDS                      
AGDI0020 EQU   *                                                                
         MVC   8(2,R2),SPACES      CLEAR TO SPACES                              
         FOUT  (R2)                                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO NEXT FIELD                           
         BCT   R0,AGDI0020         GO BACK FOR NEXT                             
*                                                                               
         LA    R2,LAGOFC1H         SET A(1ST PAXSON OFFICE DISPLAY)             
*                                                                               
         LA    R6,RAGY2REC         RETRIEVE 'PAXSON' OFFICE(S)                  
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL                                                         
         B     AGDI0060                                                         
AGDI0040 EQU   *                                                                
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
AGDI0060 EQU   *                                                                
         BNZ   AGDI0080            NOT FOUND                                    
         MVC   8(2,R2),2(R6)       INSERT OFFICE FROM ADV RECORD                
         FOUT  (R2)                SET TO TRANSMIT                              
*                                                                               
AGDI0070 BAS   RE,NEXTUF                                                        
         B     AGDI0040            GO BACK FOR NEXT ELEMENT                     
AGDI0080 EQU   *                                                                
*                                                                               
AGDI00XX FOUT  (R2)                SET TO TRANSMIT                              
         XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   CHECK FOR CONVERSION VALUES FOR ADVERTISER                                  
*                                                                               
CONADV   NMOD1 0,*CADV*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R2,LADAVEQH         SET A(ADVERTISER CONVERT VALUE)              
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    CADV0800            NO  - EXIT                                   
         CLC   REPALPHA,=C'B3'     IS THIS TEST/EJOR?                           
         BE    CADV0010            NO                                           
         CLC   REPALPHA,=C'SJ'     IS THIS TEST/SJR?                            
         BE    CADV0010            NO                                           
         CLC   REPALPHA,=C'CV'     IS THIS ABC?                                 
         BNE   CADV0020            NO                                           
CADV0010 EQU   *                                                                
         CLI   5(R2),5             YES - MORE THAN FIVE CHARACTERS?             
         BH    CADV0760            YES - EXIT WITH ERROR                        
         B     CADV0040                                                         
CADV0020 EQU   *                                                                
         CLI   5(R2),4             MORE THAN FOUR CHARACTERS?                   
         BH    CADV0780            YES - EXIT WITH ERROR                        
CADV0040 EQU   *                                                                
         MVC   RADVKATZ,8(R2)      MOVE 5 CHARS FROM SCREEN                     
         OC    RADVKATZ,SPACES     SET BIN ZERO TO SPACES                       
         SR    R0,R0               SET CC ZERO                                  
         B     CADV0800            EXIT                                         
CADV0760 EQU   *                                                                
         MVC   LFMMSG(L'CNBABC),CNBABC                                          
         ST    R2,DUB              SAVE ERROR ADDRESS                           
         LTR   RB,RB               SET CC NOT ZERO                              
         B     CADV0800                                                         
CADV0780 EQU   *                                                                
         MVC   LFMMSG(L'CONVNG),CONVNG                                          
         ST    R2,DUB              SAVE ERROR ADDRESS                           
         LTR   RB,RB               SET CC NOT ZERO                              
CADV0800 EQU   *                                                                
*                                                                               
         XIT1                                                                   
CONVNG   DC    C'INVALID: 4 CHARACTERS MAX'                                     
CNBABC   DC    C'INVALID: ABC = 5 CHARACTERS MAX'                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   TEST 'PAXSON' OFFICE INPUT FIELDS FOR VALIDITY                              
*                                                                               
PAXOFF   NMOD1 0,*PAXO*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            SET A(1ST PAXSON OFFICE)                     
         L     R3,8(R1)            SET A(OUTPUT RECORD)                         
         LA    R0,6                SET LOOP CONTROL                             
PAXO0020 EQU     *                                                              
         CLI   5(R2),0             FIELD EMPTY?                                 
         BE    PAXO0030            YES - SKIP TO NEXT                           
         XC    KEY,KEY                                                          
         MVI   KEY,4               SET OFFICE RECORD TYPE                       
         MVC   KEY+23(2),REPALPHA  INSERT REP CODE                              
         MVC   KEY+25(2),8(R2)     INSERT CODE FROM SCREEN                      
         GOTO1 HIGH                CHECK FOR CODE EXISTENCE                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   PAXO0060            NO  - ERROR                                  
*                                                                               
         MVC   NEWOFELT+2(2),8(R2)                                              
         GOTO1 VADDELEM,DMCB,(R3),NEWOFELT                                      
PAXO0030 EQU     *                                                              
         ZIC   RF,0(R2)              FIND NEXT OFFICE FIELD                     
         AR    R2,RF                                                            
         BCT   R0,PAXO0020         GO BACK FOR NEXT                             
         B     PAXO0040                                                         
NEWOFELT DC    X'50044040'         NEW ELEMENT FOR OFFICES                      
PAXO0040 DS    0H                                                               
         SR    R0,R0               SET CC = ZERO                                
         B     PAXO0080                                                         
PAXO0060 EQU   *                                                                
         MVC   LFMMSG(L'BADOFFC),BADOFFC                                        
         ST    R2,DUB              SAVE ERROR ADDRESS                           
         LTR   RB,RB               SET CC NOT ZERO                              
PAXO0080 EQU   *                                                                
*                                                                               
         XIT1                                                                   
BADOFFC  DC    C'OFFICE DOES NOT EXIST'                                         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* VALIDATE ALTERNATE ADDRESS AND CONTYPE                                        
*                                                                               
VALALTAD NMOD1 0,*ALTA*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R2,4(R1)            SET A(1ST ALT ADDRESS)                       
         L     R3,8(R1)            SET A(OUTPUT RECORD)                         
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'20',RADVREC) DELETE OLD ALT ADD'S               
*                                                                               
         XC    BYTE4,BYTE4                                                      
         MVI   BYTE4,1              SET COUNTER TO ONE                          
         LA    R2,LADCTR1H         CHECK CONTYPE                                
         LA    R4,LADADD1H         CHECK ADDRESS                                
*                                                                               
* VALIDATE CONTYPE                                                              
*                                                                               
*                                                                               
VAL01    DS    0H                                                               
         XC    ELEM,ELEM           CLEAR ELEMENT                                
         LA    R6,ELEM                                                          
         USING RADVAGEL,R6                                                      
         MVI   RADVAGCD,X'20'                                                   
         MVI   RADVAGLN,RADVAGLQ                                                
*                                                                               
         CLI   5(R2),0             CONTYPE BLANK?                               
         BE    VAL10               YES, SKIP                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'32'           CONTYPE RECORD                               
         MVC   KEY+24(2),REPALPHA                                               
         MVC   KEY+26(1),8(R2)     GET CONTYPE FROM SCREEN                      
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   VALER02             CONTRACT TYPE NOT FOUND                      
*                                                                               
         MVC   RADVAGTY,8(R2)                                                   
         B     VAL11                                                            
*                                                                               
* VALIDATE OFFICE                                                               
*                                                                               
VAL10    DS    0H                  CONTYP BLANK                                 
         BAS   RE,NEXTUF           GOTO OFFICE FIELD                            
         CLI   5(R2),0             OFFICE INPUT?                                
         BNE   VALER04             MUST BOTH BE ENTERED TOGETHER                
         B     VAL20               CHECK ADDRESS                                
*                                                                               
VAL11    BAS   RE,NEXTUF           GOTO OFFICE FIELD                            
         CLI   5(R2),0             OFFICE INPUT?                                
         BE    VALER04             BLANK, ERROR                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,4               SET OFFICE RECORD TYPE                       
         MVC   KEY+23(2),REPALPHA  INSERT REP CODE                              
         MVC   KEY+25(2),8(R2)     INSERT CODE FROM SCREEN                      
         GOTO1 HIGH                CHECK FOR CODE EXISTENCE                     
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   VALER03             NO  - ERROR                                  
*                                                                               
         MVC   RADVAGOF,8(R2)                                                   
*                                                                               
* VALIDATE ADDRESS                                                              
*                                                                               
VAL20    DS    0H                                                               
*                                                                               
         CLI   5(R2),0             IS CONTYPE BLANK?                            
         BE    VAL21               BLANK, CHECK OFFICE                          
         CLI   5(R4),0             IS ADDRESS BLANK?                            
         BE    VALER01             ADDRESS MUST BE INPUT IF CTYP INPUT          
*                                                                               
VAL21    SR    R0,R0               SET CC TO ZERO                               
         LR    R2,R4               POINT TO ALTERNATE ADDRESS                   
         CLI   5(R2),0             ADDRESS INPUT?                               
         BE    VALNXT              BLANK, CHECK NEXT ENTRIES                    
*                                                                               
         MVC   RADVAGA1,8(R2)                                                   
         OC    RADVAGA1,SPACES                                                  
         BAS   RE,NEXTUF           GOTO NEXT ADDRESS                            
         BAS   RE,NEXTUF           GOTO NEXT ADDRESS                            
         MVC   RADVAGA2,8(R2)                                                   
         OC    RADVAGA2,SPACES                                                  
         BAS   RE,NEXTUF           GOTO NEXT ADDRESS                            
         BAS   RE,NEXTUF           GOTO NEXT ADDRESS                            
         MVC   RADVAGA3,8(R2)                                                   
         OC    RADVAGA3,SPACES                                                  
         DROP  R6                                                               
*                                                                               
VALADD   GOTO1 VADDELEM,DMCB,RADVREC,ELEM                                       
*                                                                               
VALNXT   DS    0H                                                               
         ZIC   R1,BYTE4                                                         
         LA    R1,1(R1)            ADD ONE                                      
         STC   R1,BYTE4                                                         
         CLI   BYTE4,2             IS IT SECOND ADD?                            
         BE    VALNXT2                                                          
         SR    R0,R0               SET CC TO ZERO                               
         B     VALEX                                                            
*                                                                               
VALNXT2  DS    0H                                                               
         LA    R2,LADCTR2H         CHECK CONTYPE                                
         LA    R4,LADADDAH         CHECK ADDRESS                                
         B     VAL01                                                            
*                                                                               
VALER01  EQU   *                                                                
         LR    R2,R4               LOAD ADDRESS                                 
         MVC   LFMMSG(L'NEEDADD),NEEDADD                                        
         B     VALEREND                                                         
VALER02  EQU   *                                                                
         MVC   LFMMSG(L'NOCONTY),NOCONTY                                        
         B     VALEREND                                                         
VALER03  EQU   *                                                                
         MVC   LFMMSG(L'BADOFF2),BADOFF2                                        
         B     VALEREND                                                         
VALER04  EQU   *                                                                
         MVC   LFMMSG(L'CONTOFF),CONTOFF                                        
VALEREND ST    R2,DUB              SAVE ERROR ADDRESS                           
         LTR   RB,RB               SET CC NOT ZERO                              
*                                                                               
VALEX    XIT1                                                                   
*                                                                               
NEEDADD  DC    C'NEED ALTERNATE ADDRESS IF CONTRACT TYPE INPUT'                 
NOCONTY  DC    C'CONTRACT TYPE NOT FOUND'                                       
BADOFF2  DC    C'OFFICE DOES NOT EXIST'                                         
CONTOFF  DC    C'CONTYPE AND OFFICE MUST BE BOTH INPUT OR BOTH BLANK'           
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   HAVE TO ADD THE AGY2 RECORD - EXCEPT PAXSON!! (PAXSON DONE AFTER)           
*                                                                               
ADDAGY2  NMOD1 0,*AGY2*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,4(R1)            SET A(OUTPUT RECORD)                         
******                                                                          
         USING RAGY2REC,R4                                                      
         MVC   RAGY2FXE(RAGY2FLQ),ELEM                                          
*                                                                               
         USING RAGY2AE1,R6                                                      
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   RAGY2AC1,X'20'                                                   
         MVI   RAGY2AX1,RAGY2FL1                                                
         MVC   RAGY2AD1,LAGAGY1                                                 
         MVC   RAGY2AD2,LAGAGY2                                                 
         MVC   RAGY2CTY,RAGYCITY                                                
         MVC   RAGY2STE,RAGYSTAT                                                
         MVC   RAGY2ZIP,RAGYZIP                                                 
         GOTO1 VADDELEM,DMCB,RAGY2REC,ELEM                                      
         DROP  R6                                                               
*                                                                               
* WRITE AGENCY COMMENTS TO RAGY2REC HERE                                        
*                                                                               
         GOTO1 VDELELEM,DMCB,(X'40',RAGY2REC) DELETE OLD COMMENTS               
         LA    R6,WORK3            CMT ELEMENT SAVE AREA                        
         CLI   0(R6),X'40'         HAVE A COMMENT ELEM?                         
         BNE   ADAG210             NO                                           
         GOTO1 VADDELEM,DMCB,RAGY2REC,0(R6)                                     
         ZIC   R0,1(R6)                                                         
         AR    R6,R0               NEXT CMT ELEM                                
         CLI   0(R6),X'40'         HAVE A COMMENT ELEM?                         
         BNE   ADAG210             NO                                           
         GOTO1 VADDELEM,DMCB,RAGY2REC,0(R6)                                     
*                                                                               
ADAG210  DS    0H                                                               
         GOTO1 =A(ADAGY21F),RR=Y    BUILD 1F ELEM IN 'ELEM'                     
         GOTO1 VDELELEM,DMCB,(X'1F',RAGY2REC) DEL OLD AGY ELEM                  
         GOTO1 VADDELEM,DMCB,RAGY2REC,ELEM                                      
ADAG2EX  XIT1                                                                   
         DROP  R4                                                               
******                                                                          
*                                                                               
*********************************************************************           
* DISPLAY AGENCY COMMENTS                                                       
*********************************************************************           
AGYCMT   DS    0H                                                               
         NMOD1 0,*CMTD*                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     R5,4(R1)                                                         
*                                                                               
         XC    LAGCMT1,LAGCMT1     CLEAR COMMENT LINES                          
         FOUT  LAGCMT1H                                                         
         XC    LAGCMT2,LAGCMT2                                                  
         FOUT  LAGCMT2H                                                         
*                                                                               
         LA    R2,LAGCMT1H                                                      
         LR    R6,R5                                                            
         MVI   ELCODE,X'40'                                                     
         BAS   RE,GETEL                                                         
         BNE   AGYD200                                                          
*                                                                               
         XC    WORK,WORK           CREATE A FAKE 0C CONTRACT KEY TO             
         MVC   WORK+2(2),REPALPHA  SATISFY REGENSTC                             
*                                                                               
AGYD170  CLI   1(R6),3                                                          
         BL    AGYD180                                                          
         ZIC   R4,1(R6)            ELEM LEN                                     
         SH    R4,=H'3'                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),2(R6)                                                    
*                                                                               
         GOTO1 =V(REGENSTC),DMCB,(1,(R2)),WORK2,VDATAMGR,WORK,VGETTXT, +        
               RR=Y                                                             
         BZ    AGYD180                                                          
         MVC   22(24,R2),=C'***CMNT REC NOT FOUND***'                           
*                                                                               
AGYD180  DS    0H                  NEXT COMMENT?                                
         LA    RF,LAGCMT2H                                                      
         CR    R2,RF                                                            
         BNL   AGYD200                                                          
         LA    R2,LAGCMT2H                                                      
         MVI   ELCODE,X'40'        RESTORE ELCODE                               
         BAS   RE,NEXTEL                                                        
         BE    AGYD170                                                          
AGYD200  DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
*********************************************************************           
* VALIDATE FILE COMMENTS & FREE FORM COMMENTS                                   
*********************************************************************           
CMTRNT   DS    0H                                                               
         NMOD1 0,*CMTE*                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     R4,4(R1)                                                         
         USING RAGY2REC,R4                                                      
*                                                                               
         XC    WORK3(256),WORK3    BUILD CMT ELEMENTS HERE                      
*                                                                               
         XC    WORK,WORK           CREATE A FAKE 0C CONTRACT KEY TO             
         MVC   WORK+2(2),REPALPHA  SATISFY REGENSTC                             
*                                                                               
         LA    R2,LAGCMT1H         1ST COMMENT                                  
         CLI   5(R2),0             HAVE COMMENT?                                
         BE    CMTR20                                                           
*                                                                               
         LA    R3,711                                                           
         CLC   =C'SC=',LAGCMT1                                                  
         BE    ERROR                                                            
         GOTO1 =V(REGENSTC),DMCB,(0,LAGCMT1H),0,VDATAMGR,WORK,VGETTXT, +        
               RR=Y                                                             
         BZ    *+16                                                             
         L     RD,4(RD)            ERROR, RETURN CONTROL TO USER                
         L     RD,4(RD)            ERROR, RETURN CONTROL TO USER                
         B     EXXMOD                                                           
*                                                                               
         MVC   WORK2+2(60),LAGCMT1                                              
         MVI   WORK2,X'40'         COMMENT ELEM CODE                            
*                                                                               
         IC    RE,5(R2)            LENGTH                                       
         LA    RE,2(RE)                                                         
         STC   RE,WORK2+1          ELEM LEN                                     
*                                                                               
*              ADD 1ST COMMENT ELEMENT                                          
         ZIC   R1,WORK2+1          CMT LEN                                      
         LA    R1,1(R1)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK3(0),WORK2                                                   
*                                                                               
CMTR20   DS    0H                                                               
         CLI   LAGCMT2H+5,0        2D COMMENT?                                  
         BE    CMTRNTX                                                          
*                                                                               
         LA    R3,711                                                           
         CLC   =C'SC=',LAGCMT2                                                  
         BE    ERROR                                                            
         GOTO1 =V(REGENSTC),DMCB,(0,LAGCMT2H),0,VDATAMGR,WORK,VGETTXT, +        
               RR=Y                                                             
         BZ    *+16                                                             
         L     RD,4(RD)            ERROR, RETURN CONTROL TO USER                
         L     RD,4(RD)            ERROR, RETURN CONTROL TO USER                
         B     EXXMOD                                                           
*                                                                               
         IC    RE,LAGCMT2H+5       LENGTH                                       
         LA    RE,2(RE)                                                         
         STC   RE,WORK2+1          2D COMMENT LENGTH                            
         MVC   WORK2+2(60),LAGCMT2                                              
         MVI   WORK2,X'40'         COMMENT ELEM CODE                            
*                                                                               
*              ADD 2D COMMENT                                                   
         ZIC   RF,WORK2+1          2ND CMT ELEM LEN                             
         LA    RF,1(RF)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK3+62(0),WORK2                                                
*                                                                               
CMTRNTX  XMOD1                                                                  
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
GENOLD   DSECT                                                                  
*                                                                               
         ORG   REC                                                              
       ++INCLUDE REGENADV                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENPRD                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENAGY                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENCTG                                                       
         EJECT                                                                  
*                                                                               
*- POINTPERSON, REP, SALESPERSON RECS FOLLOW (FOR NETWORK CONTRACT)             
         PRINT OFF                                                              
         ORG   REC2                                                             
       ++INCLUDE REGENPTP                                                       
         ORG   REC2                                                             
       ++INCLUDE REGENREPA                                                      
         ORG   REC2                                                             
       ++INCLUDE REGENSAL                                                       
         PRINT ON                                                               
         ORG                                                                    
*                                                                               
         EJECT                                                                  
       ++INCLUDE FATWA                                                          
       ++INCLUDE RELFMTWA                                                       
         EJECT                                                                  
         ORG   LFMLAST                                                          
*                                  AGENCY SCREEN                                
       ++INCLUDE RELFMF5D                                                       
         ORG   LAGWORK+4                                                        
ORIGAGY  DS    CL4                 SAVE AREA FOR LAST AGENCY                    
*                                  ADVERTISER SCREEN                            
ORIGFLAG DS    CL1                 AGENCY FLAG SAVE AREA                        
SCPFLG   DS    CL1                 AGENCY/ADV SCOPE FLAG                        
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMF7D                                                       
         ORG   LADWORK+4                                                        
ORIGADV  DS    CL5                 SAVE AREA FOR LAST ADVERTISER                
*                                                                               
RCMTD    DSECT                                                                  
       ++INCLUDE REGENCMT                                                       
RAGY2D   DSECT                                                                  
       ++INCLUDE REGENAGY2                                                      
         EJECT                                                                  
RTERD    DSECT                                                                  
       ++INCLUDE REGENTER                                                       
         EJECT                                                                  
ROFF2D   DSECT                                                                  
       ++INCLUDE REGENOFF2                                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036RELFM03   10/26/09'                                      
         END                                                                    
