*          DATA SET RELFM02    AT LEVEL 166 AS OF 08/08/06                      
*PHASE T80402A,*                                                                
***********************************************************************         
*- RELFM02 -- PHASE T80402                                            *         
*---------------------------------------------------------------------*         
*  MOD LOG                                                            *         
*  -------                                                            *         
*  08/24/89  PJS  CHANGE PHASE CARD TO 'A' LEVEL                      *         
*                                                                     *         
*  02/08/90  PJS  SALESPERSON: PREVENT CHANGE TO OFFICE (LIKE TEAM)   *         
*                                                                     *         
*  11/03/92  SKU  ADD FAX NUMBER FIELD TO OFFICE AND SALESPERSON RECS *         
*                                                                     *         
*  09/14/93  BU   SALESPERSON MERGER CODE ADDITION                    *         
*                                                                     *         
*  10/25/93  BU   CHANGE BASIS FOR SALESPERSON MERGER ERROR           *         
*                                                                     *         
*  12/03/93  BU   DELETE REFERENCES TO MERGER, ADD S/P LEAVE DATE.    *         
*                                                                     *         
*  12/06/93  BU   ADD MANAGER FLAG                                    *         
*                                                                     *         
*  JAN27/94 (BU) --- ADD DEVELOPMENT SALESPERSON                      *         
*                                                                     *         
*  FEB21/95 (BU) --- ADD DATE LEAVE TO DEVELOPMENT SALESPERSON RECORD *         
*                                                                     *         
*  SEP22/95 (BU) --- ADD SALES PERSON EQUIV FOR THE KATZ REQUIREMENT  *         
*                                                                     *         
*  SEP26/95 (BU) --- UNLOCK TEAM BASED ON BIT 64.                     *         
*                                                                     *         
*  FEB13/96 (BU) --- EXPAND AND UNLOCK SALESPERSON EQUIV FIELD,       *         
*                    BASED ON                                         *         
*                                                                     *         
*  FEB27/96 (RV) --- ADD VALIDATION OF PROF BITS 4&5 - CAN'T BOTH     *         
*                    BE ON AT ONCE                                    *         
*                                                                     *         
*  JUN19/96 (BU) --- DISPLAY MERGE CODE AS ALFANUM IF RSALFLG = X'80' *         
*                                                                     *         
*  DEC24/96 (DB) --- ADD STATION EXCLUSIONS FIELDS                    *         
*                                                                     *         
*  JAN22/97 (DB) --- ADD OFFICE RECORDS ON MASTER LEVEL               *         
*                                                                     *         
*  AUG08/98 (AST) -- ADD ROUTINE FOR NATIONAL/LOCAL OFFICE            *         
*                                                                     *         
*  MAY26/99 (BU ) -- ADD SALESPERSON EMAIL ADDRESS                    *         
*                                                                     *         
*  JUN22/99 (SKU) -- ADD SALESPERSON X'8601' PASSIVE KEY              *         
*                                                                     *         
*  JUN29/99 (SKU) -- EMAIL BUG FIX                                    *         
*                                                                     *         
*  AUG08/01 (BU ) -- PERMIT S/P IF CONVERT OPTION SET                 *         
*                                                                     *         
*  MAR25/02 (BU ) -- ADD SALES ASSISTANT NAME, E-MAIL ADDRESS, FLAG   *         
*                                                                     *         
*  NOV05/02 (BU ) -- ADD RADIO EDI UPGRADES                           *         
*                                                                     *         
*  NOV24/03 (BU ) -- MEDIA OCEAN S/P NAME                             *         
*                                                                     *         
*  APR22/04 (BU ) -- FIX 'DUPE KEY ON ADD' SITUATION FOR OFFICE       *         
*                                                                     *         
*  MAY10/04 (HQ ) -- EXPAND OFFICE PROFILE BIT FROM 8 TO 16           *         
*                                                                     *         
*                                                                     *         
*                 ***  END TOMBSTONE  ***                             *         
***********************************************************************         
RELFM02  TITLE 'T80402 - OFC/TM/SL/DEV SP/GRP RECORDS'                          
T80402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80402,R9                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T804FFD,RA                                                       
*                                                                               
         MVI   UPDATCON,0          SET 'UPDATE CONTROL FILE' OFF                
*                                                                               
         LA    R2,LFMLAST                                                       
         CLI   BREC,4                                                           
         BE    OFC                                                              
         CLI   BREC,5                                                           
         BE    TM                                                               
         CLI   BREC,6              SALESPERSON                                  
         BE    SL                                                               
         CLI   BREC,7                                                           
         BE    GRP                                                              
         CLI   BREC,X'3A'          DEVELOPMENTAL SALESPERSON                    
         BE    DEVLSALE                                                         
         DC    H'0'                                                             
NOREGERR EQU   118                                                              
NOTMERR  EQU   119                                                              
NOCHGERR EQU   123                                                              
MRGSALNG EQU   69                                                               
MANAGRNG EQU   408                                                              
INVSTAT  EQU   150                                                              
         TITLE 'T80402 - OFFICE RECORDS'                                        
* OFFICE RECORDS                                                                
*                                                                               
OFC      CLI   BFMTSW,0                                                         
         BNE   OFCEDT                                                           
* FORMAT ROUTINE                                                                
         GOTO1 =A(OFFCADDR),RR=Y                                                
         B     EXXMOD                                                           
         EJECT                                                                  
* EDIT OFCREC DATA                                                              
*                                                                               
OFCEDT   MVC   REC+34(2),=X'014A'                                               
         MVC   REC+27(2),=Y(108)                                                
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   ROFFNAME,WORK                                                    
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   ROFFADD1,WORK                                                    
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   ROFFADD2,WORK                                                    
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         CLI   5(R2),2             STATE SHOULD BE 2 CHARACTERS                 
         BNE   FLERR2                                                           
         BAS   RE,MOVE                                                          
         MVC   ROFFSTT,WORK                                                     
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   ROFFZIP,WORK                                                     
*                                                                               
         BAS   RE,NEXTUF                                                        
         BAS   RE,MOVE                                                          
         MVC   ROFFREG,WORK                                                     
*                                                                               
         OC    ROFFREG,ROFFREG     TEST REGION ENTERED                          
         BZ    OFCEDT10            NO                                           
         CLC   ROFFREG,SPACES                                                   
         BE    OFCEDT10                                                         
* TEST REGION ON FILE                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,3                                                            
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),ROFFREG                                                
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    OFCEDT10                                                         
* POSITION TO REGION FIELD                                                      
         LA    R2,LFMLAST                                                       
         LA    R3,6                                                             
         BAS   RE,NEXTUF                                                        
         BCT   R3,*-4                                                           
         LA    R3,NOREGERR                                                      
         B     ERROR                                                            
*                                                                               
* BUILD PROFILE AND FAX ELEMENT IN WORK                                         
*                                                                               
OFCEDT10 DS    0H                                                               
         BAS   RE,NEXTUF                                                        
*                                                                               
         CLC   =C'NY',8(R2)        OPTION 2 REQUIRES OPTION 1 BE SET            
         BE    FLERR4                                                           
         CLC   =C' Y',8(R2)                                                     
         BE    FLERR4                                                           
         CLC   =C'YY',11(R2)       BITS 4&5 CAN'T BOTH BE ON                    
         BE    FLERR7                                                           
*                                                                               
         GOTO1 =A(BITIN),DMCB,PROFBITS,8(R2),RR=Y                               
*                                                                               
         GOTO1 =A(BITIN),DMCB,PROFBIT2,16(R2),RR=Y                              
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         XC    WORK,WORK                                                        
         CLI   5(R2),0             AREA CODE                                    
         BE    OFCEDT20                                                         
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    FLERR3                                                           
OFCEDT20 MVC   WORK+6(3),8(R2)     FIRST 6 BYTES ARE CODE/LEN/PROF              
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0             PREFIX                                       
         BE    OFCEDT30                                                         
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    FLERR3                                                           
OFCEDT30 MVC   WORK+9(3),8(R2)                                                  
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0             SUFFIX                                       
         BE    OFCEDT40                                                         
         TM    4(R2),X'08'         MUST BE NUMERIC                              
         BZ    FLERR3                                                           
OFCEDT40 MVC   WORK+12(4),8(R2)                                                 
*                                                                               
         MVI   WORK,ROFF2CDQ       ELEMENT CODE                                 
         MVI   WORK+1,ROFF2FLQ     ELEMENT LENGTH                               
         MVC   WORK+2(1),PROFBITS  PROFILE                                      
         MVC   WORK+3(1),PROFBIT2  MORE PROFILE                                 
*                                                                               
* SELECT NATIONAL OR LOCAL OFFICE - DEFAULT IS NATIONAL                         
*                                                                               
OFCEDT41 DS    0H                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0             HAS USER SELECTED NATIONAL/LOCAL?            
         BNE   *+12                YES, SKIP...                                 
         NI    WORK+3,X'FF'-X'80'  DEFAULT TO NATIONAL, BYTE2 OF PROF           
         B     OFCEDT49                                                         
         CLI   8(R2),C'L'                                                       
         BNE   *+12                                                             
         OI    WORK+3,X'80'                                                     
         B     OFCEDT49                                                         
         CLI   8(R2),C'N'                                                       
         BNE   FLERR2              INVALID                                      
         NI    WORK+3,X'FF'-X'80'  NATIONAL                                     
*                                                                               
* ADD OR CHANGE THE REGENOFF2 RECORD                                            
*                                                                               
OFCEDT49 MVC   AIOSV,AIOAREA       SAVE OFF REC, SO WE WON'T CLOBBER IT         
         LA    R4,MYIOAREA                                                      
         ST    R4,AIOAREA                                                       
MYIOD    USING ROFF2REC,MYIOAREA                                                
*                                                                               
         XCEF  MYIOAREA,1000       CLEAR MY IO AREA                             
*                                                                               
         MVC   MYIOD.ROFF2KEY,ROFFKEY                                           
         MVI   MYIOD.ROFF2TYP,ROFF2TYQ                                          
         MVC   MYIOD.ROFF2LEN,=Y(90)                                            
         MVC   MYIOD.ROFF2FXE(16),WORK                                          
*                                                                               
***      CLI   BACT,C'A'           TEST ADD                                     
***      BE    OFCEDT50                                                         
*                                                                               
         MVC   KEY(L'ROFFKEY),MYIOD.ROFF2REC                                    
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   OFCEDT50                                                         
*                                                                               
         NI    KEY+27,X'FF'-X'80'  RESTORE KEY                                  
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,GETREC                                                        
         NI    MYIOD.ROFF2CTL,X'FF'-X'80' RESTORE RECORD, IF EXISTED            
         MVC   MYIOD.ROFF2FXE(16),WORK                                          
         BAS   RE,PUTREC           WRITE THE RECORD                             
         BAS   RE,WRITE            WRITE THE KEY                                
         B     OFCEDTX                                                          
*                                                                               
OFCEDT50 DS    0H                                                               
         BAS   RE,ADDREC           ADD ROFF2 REC                                
*                                                                               
OFCEDT60 DS    0H                                                               
         OC    SVREPSUB,SVREPSUB   MASTER RECORD?                               
         BZ    OFCEDTX             NO                                           
*                                                                               
         MVC   KEY(27),0(R4)                                                    
         MVC   KEY+23(2),SVREPMST  MASTER REP CODE                              
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    OFCEDT80                                                         
*                                                                               
         MVC   MYIOD.ROFF2REP,SVREPMST    REP CODE                              
         BAS   RE,ADDREC           ADD ROFF2 MASTER REC                         
*                                                                               
OFCEDT80 DS    0H                                                               
         L     R5,AIOSV            RESTORE                                      
         ST    R5,AIOAREA                                                       
         MVC   MYKEY(25),0(R5)     SAVE TYPE/REP                                
*                                                                               
         MVC   KEY(27),0(R5)                                                    
         MVC   KEY+23(2),SVREPMST  MASTER REP CODE                              
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    OFCEDTX                                                          
*                                                                               
         USING ROFFREC,R5                                                       
         MVC   ROFFKREP,SVREPMST       REP CODE                                 
         BAS   RE,ADDREC           ADD ROFF MASTER REC                          
         MVC   0(25,R5),MYKEY      RESTORE TYPE/REP                             
         DROP  R5                                                               
*                                                                               
OFCEDTX  EQU   *                                                                
         DROP  MYIOD                                                            
         L     R4,AIOSV            RESTORE                                      
         ST    R4,AIOAREA                                                       
         B     FLFILE                                                           
         EJECT                                                                  
         TITLE 'T80402 - TEAM RECORDS'                                          
TM       CLI   BFMTSW,0                                                         
         BNE   TMEDT                                                            
* FORMAT ROUTINE                                                                
         GOTO1 =A(TEAMADDR),RR=Y                                                
         B     EXXMOD                                                           
         EJECT                                                                  
TMEDT    MVC   REC+34(2),=X'0148'                                               
         MVC   REC+27(2),=Y(106)                                                
         MVC   RTEMPROF,ZEROS                                                   
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RTEMDVNM,WORK                                                    
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   REC+26,C' '         NO TEAM NAME ON DIV RECORD                   
         BNE   TMEDT1                                                           
         CLI   5(R2),0                                                          
         BNE   FLERR1                                                           
         B     TMEDT1A                                                          
         SPACE 1                                                                
TMEDT1   CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RTEMNAME,WORK                                                    
*                                                                               
TMEDT1A  LA    R4,RTEMPROF                                                      
TMEDT2   BAS   RE,NEXTUF                                                        
         BE    TMEDTX                                                           
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         MVC   0(1,R4),8(R2)                                                    
         LA    R4,1(R4)                                                         
         B     TMEDT2                                                           
*                                                                               
TMEDTX   B     FLFILE                                                           
         TITLE 'T80402 - SALESMAN RECORDS'                                      
SL       CLI   BFMTSW,0                                                         
         BNE   SPEREDIT                                                         
* FORMAT ROUTINE                                                                
         GOTO1 =A(SALEADDR),RR=Y                                                
*                                                                               
* ---- DISPLAY STATION EXCLUSIONS ---->                                         
                                                                                
         BAS   RE,EXCCLEAR         CLEAR ALL STATION EXCLUSION FIELDS           
*                                                                               
         BAS   RE,SETSAL2                                                       
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   SPDSEX                                                           
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIOAREA                                                       
         USING RSALEXEM,R6         STATION EXCLUSION ELEMENT                    
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL            DOES EXCLUSION ELEMENT EXIST?                
         BNE   SPDS0120            NO                                           
*                                                                               
         ZIC   RF,RSALEXLN         LENGTH OF ELEMENT                            
         LA    R2,LF9EXLFH         FIRST STATION EXCLUSION FIELD                
         LA    R4,RSALEXST                                                      
         LA    R7,7                INITIALIZE LENGTH                            
*                                                                               
SPDS0100 DS    0H                                                               
         BAS   RE,DISSTAT          DISPLAY EXCLUSION STATION                    
         CR    R7,RF               END OF ELEMENT?                              
         BNL   SPDS0120                                                         
         LA    R4,5(R4)                                                         
         BAS   RE,NEXTUF                                                        
         LA    R7,5(R7)            INCREMENT CURRENT LENGTH                     
         B     SPDS0100                                                         
*                                                                               
         DROP  R6                                                               
SPDS0120 DS    0H                                                               
         LA    R2,LF9MAILH         SET A(SCREEN EMAIL ADDRESS)                  
         L     R6,AIOAREA                                                       
         USING RSALEMEM,R6         SALESPERSON EMAIL ADDRESS                    
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            DOES EMAIL ADDRESS EXIST?                    
         BNE   SPDS0130            NO                                           
         CLI   1(R6),2             NO EMAIL?                                    
         BNH   SPDS0130                                                         
         ZIC   RF,1(R6)            GET LENGTH OF ELEMENT                        
         SH    RF,=H'3'            SUBTRACT 3 FOR MOVE                          
         EX    RF,SPDS0800         MOVE EMAIL BY LENGTH                         
SPDS0130 DS    0H                                                               
         FOUT  (R2)                                                             
         L     R6,AIOAREA                                                       
         USING RSASEMEM,R6         S/A EMAIL ADDRESS ELT                        
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL            DOES S/A ADDRESS EXIST?                      
         BNE   SPDS0140            NO                                           
         CLI   1(R6),2             NO DATA IN ELEMENT?                          
         BNH   SPDS0140                                                         
         LA    R2,LF9SMALH         SET A(SCREEN S/A MAIL ADDRESS)               
         ZIC   RF,1(R6)            GET LENGTH OF ELEMENT                        
         SH    RF,=H'23'           SUBTRACT 23 FOR MOVE: CONTROL,               
*                                     LEN, S/A NAME, FLAG                       
         LTR   RF,RF               ANYTHING LEFT OF ELEMENT?                    
         BZ    SPDS0132            NO  - THERE IS NO S/A EMAIL ADDR             
         BCTR  RF,0                YES - SUBTRACT 1 FOR EX                      
         EX    RF,SPDS0131         MOVE EMAIL BY LENGTH                         
         B     SPDS0132                                                         
SPDS0131 EQU   *                                                                
         MVC   8(00,R2),RSASEAML   INSERT S/A EMAIL ADDRESS                     
SPDS0132 DS    0H                                                               
         TM    RSASEMFL,X'80'      FLAG SET?                                    
         BNO   SPDS0134            NO                                           
         MVI   LF9SONL,C'Y'        YES - SET ON SCREEN                          
SPDS0134 DS    0H                                                               
         CLC   RSASEMNM,SPACES     S/A NAME?                                    
         BNH   SPDS0140            NO                                           
         MVC   LF9SAST,RSASEMNM    YES - SET ON SCREEN                          
SPDS0140 DS    0H                                                               
         FOUT LF9SASTH                                                          
         FOUT LF9SONLH                                                          
         FOUT LF9SMALH                                                          
*                                                                               
         L     R6,AIOAREA                                                       
         USING RSASMOEM,R6         MO S/P ELT                                   
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL            DOES MO S/P ELT  EXIST?                      
         BNE   SPDS0160            NO                                           
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         SHI   RF,3                BACK OFF 3 CHAR: CTL + 1 FOR EX              
         EX    RF,SPDS0150                                                      
         B     SPDS0155                                                         
SPDS0150 MVC   LF9MONM(0),RSASMONM                                              
SPDS0155 EQU   *                   YES - INSERT NAME ON SCREEN                  
         FOUT  LF9MONMH                                                         
SPDS0160 DS    0H                                                               
*                                                                               
SPDSEX   DS    0H                                                               
         BAS   RE,SETSAL                                                        
         B     EXXMOD                                                           
SPDS0800 EQU   *                                                                
         MVC   8(00,R2),2(R6)      INSERT EMAIL ADDRESS                         
         DROP  R6                                                               
         EJECT                                                                  
********************************************************************            
*  CLEAR STATION EXCLUSIONS FIELDS                                              
********************************************************************            
EXCCLEAR NTR1                                                                   
         LA    R2,LF9EXLFH         FIRST EXCLUSION FIELD                        
         LA    R7,LF9EXLLH         LAST EXCLUSION FIELD                         
*                                                                               
EXCL10   CR    R2,R7                                                            
         BH    EXCLEX                                                           
         XC    8(6,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXTUF           NEXT FIELD                                   
         B     EXCL10                                                           
*                                                                               
EXCLEX   XIT1                                                                   
         EJECT                                                                  
********************************************************************            
* DISPLAY EXCLUSION STATION                                                     
********************************************************************            
DISSTAT  NTR1                                                                   
         MVC   8(4,R2),0(R4)                                                    
         CLI   4(R4),C' '                                                       
         BE    DSTAT40                                                          
*                                                                               
DSTAT30  MVI   12(R2),C'-'                                                      
         MVC   13(1,R2),4(R4)      MOVE MEDIA ON THE SCREEN                     
*                                                                               
DSTAT40  DS    0H                                                               
         CLI   11(R2),C' '                                                      
         BNE   DSTATEX                                                          
         MVC   11(2,R2),12(R2)                                                  
         MVI   13(R2),C' '                                                      
*                                                                               
DSTATEX  DS    0H                                                               
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         XIT1                                                                   
         EJECT                                                                  
********************************************************************            
SPEREDIT EQU   *                                                                
*                                                                               
         TM    SVPGPBIT+2,X'20'    PROFILE = UPDATE CONTROL FILE?               
         BNO   SPER0002            NO  -                                        
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
SPER0002 EQU   *                                                                
         XC    GOLDKEYS,GOLDKEYS   CLEAR KEYS                                   
         XC    GNEWKEYS,GNEWKEYS                                                
*                                                                               
         MVC   REC+34(2),=X'0156'                                               
         MVC   REC+27(2),=Y(120)                                                
         MVC   RSALPROF,ZEROS                                                   
*                                                                               
         BAS   RE,NEXTUF           NAME.  REQUIRED                              
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RSALNAME,WORK                                                    
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    SPER0005            NO INPUT:  CONSIDER AS YES                   
         CLI   5(R2),1             MORE THAN ONE CHAR?                          
         BH    FLERR2              YES - ERROR                                  
         CLI   8(R2),C'Y'          SET TO 'YES'?                                
         BE    SPER0005            YES - ALREADY DONE                           
         CLI   8(R2),C'N'          SET TO 'NO '?                                
         BNE   FLERR2              NO  - UNRECOGNIZED VALUE                     
         OI    RSALFLG,X'20'       YES - SET FLAG TO 'BLOCK'                    
*                                                                               
SPER0005 EQU   *                                                                
         BAS   RE,NEXTUF           TEAM.  MAY NOT BE CHANGED                    
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         LA    R3,NOCHGERR                                                      
         TM    SVPGPBIT+7,X'01'    PERMIT TEAM CHANGE?                          
         BO    SPER0010            YES                                          
         CLI   BACT,C'A'           TEST ADD                                     
         BE    *+14                YES                                          
         CLC   SVTEAM,WORK       TEST SAME DIV/TEAM                             
         BNE   ERROR                                                            
SPER0010 EQU   *                                                                
         MVC   RSALTEAM,WORK                                                    
         MVC   SVTEAM,RSALTEAM                                                  
*                                                                               
         XC    RSALPFLG,RSALPFLG   CLEAR OUT POWER CODE FLAG                    
         XC    RSALPOWR,RSALPOWR   CLEAR OUT POWER CODE                         
*                                                                               
         BAS   RE,NEXTUF           REP CODE                                     
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    SPER0012            NO  - ALSO SKIP MINUS FIELD                  
*                                  YES - VALIDATE FOR EXISTENCE                 
         GOTO1 =A(VALIREP),DMCB,(R2),RR=Y                                       
         BNZ   FLERR2              DOESN'T EXIST:  ERROR                        
*                                                                               
         BAS   RE,MOVE                                                          
         MVC   RSALPOWR,WORK       REP CODE.   OPTIONAL                         
*                                                                               
         BAS   RE,NEXTUF           MINUS FIELD                                  
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    SPER0013            NO  -                                        
         CLI   5(R2),1             MUST BE ONLY A SINGLE CHARACTER              
         BH    FLERR2                                                           
         CLI   8(R2),C'N'          "NO"?                                        
         BE    SPER0013                                                         
         CLI   8(R2),C'Y'          "YES"?                                       
         BNE   FLERR2              NO  - ERROR: NOT Y OR N                      
         OI    RSALPFLG,X'80'      TURN ON 'MINUS' FLAG                         
         B     SPER0013                                                         
*                                                                               
SPER0012 EQU   *                                                                
         BAS   RE,NEXTUF           SKIP MINUS FIELD: NO POWER CODE              
SPER0013 EQU   *                                                                
*                                                                               
         BAS   RE,NEXTUF           PHONE NUMBER.  OPTIONAL                      
         BAS   RE,MOVE                                                          
         MVC   RSALTEL,WORK                                                     
*                                                                               
         BAS   RE,NEXTUF           OFFICE. REQUIRED.                            
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         MVC   RSALPROF(2),8(R2)                                                
*                                                                               
         LA    R3,NOCHGERR                                                      
         CLI   BACT,C'A'           CAN'T CHANGE OFFICE                          
         BE    SPER0020            SKIP TEST ON ADD.                            
*                                                                               
         CLC   SVOFFICE,RSALPROF                                                
         BNE   ERROR                                                            
SPER0020 EQU   *                                                                
         MVC   SVOFFICE,RSALPROF                                                
         SPACE 2                                                                
         XC    KEY,KEY             VALIDATE OFFICE                              
         MVI   KEY,4                                                            
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),RSALPROF                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    SPER0040                                                         
         LA    R3,151              INVALID OFFICE                               
         B     ERROR                                                            
         SPACE 1                                                                
SPER0040 EQU   *                                                                
         XC    MOSPNAME,MOSPNAME   CLEAR MEDIA OCEAN S/P NAME                   
*                                                                               
         TM    SVPGPBIT+2,X'04'    PROFILE = DISPLAY/ACCEPT MO S/P?             
         BNO   SPER0042            NO  -                                        
         BAS   RE,NEXTUF           YES - MO S/P NAME: OPTIONAL                  
         CLI   5(R2),0             ANYTHING IN FIELD?                           
         BE    SPER0042            NO  - DON'T SET ANYTHING UP                  
         MVI   MOSPNAME,X'22'      SET ELEMENT CODE                             
         ZIC   RF,5(R2)            GET DATA LENGTH                              
         BCTR  RF,0                SUBTRACT 1 FOR EX                            
         EX    RF,SPER0041         MOVE BY LENGTH                               
         AH    RF,=H'3'            COMPUTE ELEMENT LENGTH                       
         STC   RF,MOSPNAME+1       INSERT LENGTH                                
         B     SPER0042                                                         
*                                                                               
SPER0041 EQU   *                                                                
         MVC   MOSPNAME+2(0),8(R2) INSERT NAME BY LENGTH                        
*                                                                               
SPER0042 EQU   *                                                                
         BAS   RE,NEXTUF           FAX   NUMBER.  OPTIONAL                      
         BAS   RE,MOVE                                                          
         MVC   RSALFAX,WORK                                                     
*                                                                               
         BAS   RE,NEXTUF           FAX PREF                                     
         CLI   8(R2),C'Y'          FAX PREFERENCE SET?                          
         BNE   SPER0043            NO                                           
         OI    RSALFLG,X'02'       SET FAX    PREFERENCE                        
         B     SPER0044                                                         
SPER0043 EQU   *                                                                
         OI    RSALFLG,X'04'       SET EMAIL  PREFERENCE                        
SPER0044 EQU   *                                                                
*                                                                               
         BAS   RE,NEXTUF           EMAIL ADDRESS. OPTIONAL                      
         CLI   5(R2),0             ANY DATA?                                    
         BE    SPER0045                                                         
         BAS   RE,MOVE                                                          
         XC    EMAIL,EMAIL                                                      
         MVI   EMAIL,X'20'         INSERT ELEMENT ID                            
         ZIC   RF,5(R2)            GET LENGTH OF EMAIL INPUT                    
         LA    RF,2(RF)            ADD 2 FOR CONTROL                            
         STC   RF,EMAIL+1          INSERT LENGTH INTO ELEMENT                   
         MVC   EMAIL+2(48),WORK          SAVE EMAIL ADDR TEMPORARILY            
*                                                                               
SPER0045 EQU   *                                                                
*                                                                               
*   ALWAYS SET UP X'21' ELEMENT.  MAY NOT GET ADDED TO RECORD.                  
*                                                                               
         XC    SANAME,SANAME       ALWAYS CLEAR S/A NAME                        
         BAS   RE,NEXTUF           S/A NAME. OPTIONAL                           
         CLI   5(R2),0             ANY DATA?                                    
         BE    SPER0047            NO                                           
         BAS   RE,MOVE             YES                                          
         MVC   SANAME(20),WORK     SAVE S/A ELT TEMPORARILY                     
*                                                                               
SPER0047 EQU   *                                                                
*                                                                               
         MVI   SAFLAG,0            CLEAR S/A FLAG                               
         BAS   RE,NEXTUF           S/A FLAG:  OPTIONAL                          
         CLI   5(R2),0             ANY DATA?                                    
         BE    SPER0048            NO                                           
         CLI   8(R2),C'Y'          SET TO YES?                                  
         BNE   SPER0048            NO                                           
         ST    R2,FULL             SAVE A(FLAG FIELD)                           
         OI    SAFLAG,X'80'        YES - TURN FLAG ON                           
SPER0048 EQU   *                                                                
         XC    SAEMAL,SAEMAL       CLEAR S/A E-MAIL ADDR                        
         BAS   RE,NEXTUF           S/A EMAIL ADDRESS. OPTIONAL                  
         CLI   5(R2),0             ANY DATA?                                    
         BNE   SPER0049            YES                                          
         TM    SAFLAG,X'80'        NO  - S/A FLAG ON?                           
         BNO   SPER0050            NO  - PROCEED                                
         L     R2,FULL             YES - SET A(FLAG FIELD)                      
         B     FLERR10             ERROR: NO FLAG IF NO S/A ADDR                
SPER0049 EQU   *                                                                
         BAS   RE,MOVE                                                          
         ZIC   RF,5(R2)            GET LENGTH OF EMAIL INPUT                    
         ZIC   RE,SAELEM+1         GET CURRENT LENGTH OF ELEMENT                
         AR    RF,RE                                                            
         STC   RF,SAELEM+1         INSERT TOTAL LENGTH INTO ELEMENT             
         MVC   SAEMAL(48),WORK     SAVE S/A EMAIL ELT TEMPORARILY               
*                                                                               
SPER0050 EQU   *                                                                
         BAS   RE,NEXTUF           FIND NEXT FIELD FOR LEAVE DATE               
         XC    LEAVDATE,LEAVDATE   CLEAR LEAVE DATE                             
         CLI   5(R2),0             ANY DATA?                                    
         BE    SPER0060            NO  - SKIP FIELD                             
         XC    WORK,WORK                                                        
         LA    R3,13               INVALID DATE MESSAGE                         
         GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK                                      
         OC    0(4,R1),0(R1)       ERROR?                                       
         BZ    ERROR               YES                                          
         GOTO1 VDATCON,DMCB,WORK,(3,RSALLEAV)                                   
*                                  INSERT DATE INTO RECORD                      
         GOTO1 VDATCON,DMCB,(3,RSALLEAV),(2,LEAVDATE)                           
*                                  SAVE LEAVE DATE                              
*                                                                               
SPER0060 EQU   *                                                                
         MVI   RSALMGR,C'N'        SET MANAGER FLAG TO 'NO'                     
         BAS   RE,NEXTUF           FIND NEXT FIELD FOR MANAGER FLAG             
         CLI   5(R2),0             ANY DATA?                                    
         BE    SPER0080            NO  - SKIP FIELD                             
         LA    R3,MANAGRNG                                                      
         CLI   8(R2),C'N'                                                       
         BE    SPER0080                                                         
         CLI   8(R2),C'Y'                                                       
         BNE   ERROR                NOT 'Y' AND NOT 'N'                         
         MVI   RSALMGR,C'Y'                                                     
SPER0080 EQU   *                                                                
*                                   SCREEN TO FILE                              
         MVC   RSALMRG,ORIGSAL     RESET ORIG S/P, IF ANY                       
         TM    SVPGPBIT+1,X'10'    CONVERT S/P EQUIV ALLOWED?                   
         BO    SPER0082            YES - TREAT AS 3-CHAR ALPHANUM               
         TM    SVPGPBIT+1,X'40'    S/P EQUIV ALLOWED?                           
         BO    SPER0082            NOW TREAT A 3 CHARS                          
***      BNO   SPER0087            NO                                           
***      TM    SVPGPBIT+1,X'20'    YES - KATZ OR SELTEL FORMAT?                 
***      BNO   SPER0084            NOT ON = KATZ FORMAT                         
         LA    R2,LF9SPEQH         SET A(INPUT FIELD)                           
         CLI   5(R2),0             ANY INPUT?                                   
         BE    SPER0086            NO                                           
         CLI   5(R2),8             DATA LENGTH 8 CHARACTERS?                    
         BNE   FLERR8              NO                                           
         MVC   RSALEQU,8(R2)       INSERT EQUIVALENCY CODE                      
         B     SPER0086                                                         
SPER0082 EQU   *                                                                
         LA    R2,LF9SPEQH         SET A(INPUT FIELD)                           
         CLI   5(R2),0             ANY INPUT?                                   
         BE    SPER0086            NO                                           
         CLI   5(R2),3             DATA LENGTH 3 CHARACTERS?                    
         BNE   FLERR9              NO                                           
         MVC   RSALMRG,8(R2)       INSERT EQUIVALENCY CODE                      
         B     SPER0086                                                         
SPER0084 EQU   *                                                                
         LA    R2,LF9SPEQH                                                      
         CLI   5(R2),0                                                          
         BE    SPER0086                                                         
         CLI   5(R2),5                                                          
         BNE   FLERR5                                                           
         TM    4(R2),X'08'                                                      
         BNO   FLERR5                                                           
SPER0085 EQU   *                                                                
         PACK  DUB(8),LF9SPEQ(5)   PACK EQUIV CODE                              
         CVB   RF,DUB              CONVERT IT TO BINARY                         
         STCM  RF,7,RSALMRG        STORE IN S/P RECORD                          
SPER0086 EQU   *                                                                
         CLI   BACT,C'D'                                                        
         BNE   SPER0087                                                         
         OC    RSALLCD,RSALLCD     ANY LAST CHANGE DATE?                        
         BZ    SPER0090            NO                                           
         CLC   RSALLCD,=C'00'      DITTO                                        
         BE    SPER0090            NO                                           
                                                                                
SPER0087 GOTO1 VDATCON,DMCB,(5,0),(2,RSALLCD)                                   
*                                  INSERT LAST CHANGE DATE INTO RECORD          
         GOTO1 VDATCON,DMCB,(5,0),(5,LF9SPLC)                                   
*                                  INSERT LAST CHANGE DATE TO SCREEN            
                                                                                
         FOUT  LF9SPLCH                                                         
                                                                                
SPER0090 EQU   *                                                                
                                                                                
*--- VALIDATE STATION EXCLUSIONS FIELDS -------->                               
                                                                                
         GOTO1 =A(DUPLICST),RR=Y   ARE THERE ANY DUPLICATE STATIONS?            
*                                                                               
         LA    R2,LF9EXLFH         FIRST EXCLUSION FIELD                        
         XC    ELEM,ELEM                                                        
         LR    R5,R2                                                            
         LA    R7,LF9LAST          LAST EXCLUSION FIELD                         
*                                                                               
SPREP100 DS    0H                                                               
         CR    R2,R7               ALL FIELD HAVE BEEN VALIDATED?               
         BNL   SPREP150                                                         
         CLI   5(R2),0             ANY ENTRIES?                                 
         BE    SPREP140            NO                                           
*                                                                               
         OI    FLAG,ANYENTR        THERE ARE ENTRIES                            
         OC    8(6,R2),SPACES                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSTAKEY,R4          STATION RECORD                               
         MVI   RSTAKTYP,X'02'                                                   
*SVREPSUB CONTAINS WHOLE SUBSIDIARY ELEMENT                                     
         LA    R8,SVREPSUB+10      POINT TO REP CODES                           
*                                                                               
         OC    SVREPSUB,SVREPSUB   MASTER RECORD?                               
         BNZ   SPREP110            YES                                          
         MVC   RSTAKREP,REPALPHA   REP CODE                                     
*                                                                               
         BAS   RE,PRSTAT           MOVE STATION INTO THE KEY                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BNE   STERROR                                                          
         B     SPREP130                                                         
*                                                                               
SPREP110 DS    0H                                                               
         CLI   0(R8),0                                                          
         BE    SPER0120                                                         
         MVC   RSTAKREP,0(R8)      SUBSIDIARY REP CODE                          
         BAS   RE,PRSTAT           MOVE STATION INTO THE KEY                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    SPREP130                                                         
*                                                                               
         LA    R8,2(R8)            NEXT REP CODE                                
         B     SPREP110                                                         
*                                                                               
SPER0120 DS    0H                                                               
         B     STERROR                                                          
*                                                                               
SPREP130 DS    0H                                                               
         GOTO1 =A(BUILD10),RR=Y    BUILD OR CHANGE AN EXCLUSION ELEM            
*                                                                               
SPREP140 DS    0H                                                               
         BAS   RE,NEXTUF           FIND NEXT STATION                            
         LA    R8,SVREPSUB+10                                                   
         B     SPREP100                                                         
*                                                                               
SPREP150 DS    0H                                                               
         DROP  R4                                                               
                                                                                
         BAS   RE,SETSAL2                                                       
*                                                                               
         L     R6,AIOAREA                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     DOES SECONDARY SALESMAN REC EXIST?           
         BE    SPREP160            YES                                          
* -- SECONDARY RECORD DOES NOT EXIST --->                                       
*                                                                               
         TM    FLAG,ANYENTR        ANY ENTRIES?                                 
         BNZ   SPREP152            YES - SET UP NEW RECORD                      
         OC    EMAIL+2(48),EMAIL+2                                              
*                                  NO  - ANY EMAIL ADDRESS ENTERED?             
         BNZ   SPREP152                                                         
*                                  YES - SET UP NEW RECORD                      
         OC    MOSPNAME(22),MOSPNAME                                            
*                                  NO  - ANY MEDIA OCEAN NAME ENTERED?          
         BNZ   SPREP152                                                         
*                                  YES - SET UP NEW RECORD                      
         OC    SAELEM+2(24),SAELEM+2                                            
*                                  NO  - ANY S/A EMAIL DATA ENTERED?            
         BZ    SPREP190            NO  -                                        
*                                  YES - SET UP NEW RECORD                      
*                                                                               
SPREP152 EQU   *                                                                
         XC    0(250,R6),0(R6)     CLEAR AIO AREA                               
         XC    250(250,R6),250(R6)                                              
         XC    500(250,R6),500(R6)                                              
         XC    750(250,R6),750(R6)                                              
         MVC   0(L'RSA2KEY,R6),MYKEY MOVE KEY                                   
         MVI   0(R6),X'46'           SECONDARY SALESMAN RECORD                  
         MVI   L'RSA2KEY+1(R6),50    RECORD LENGTH+LENGTH OF 01 ELEM            
*                                                                               
         LA    R6,34(R6)                                                        
         MVI   0(R6),X'01'                                                      
         MVI   1(R6),16              LENGTH                                     
         GOTO1 VDATCON,DMCB,(5,0),(3,2(R6))                                     
*                                                                               
         L     R6,AIOAREA                                                       
         CLI   ELEM,0              HAVE AN ELEM TO ADD?                         
         BE    SPREP155            NO                                           
         GOTO1 VADDELEM,DMCB,(R6),ELEM                                          
SPREP155 GOTO1 ADDREC                                                           
         MVC   KEY(27),0(R6)      RESET KEY                                     
         GOTO1 HIGH               RETRIEVE KEY FOR POSSIBLE REWRITE             
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
         BAS   RE,GETREC           GET RECORD AGAIN FOR UPDATE                  
         OC    EMAIL+2(48),EMAIL+2 ANY EMAIL ADDRESS?                           
         BZ    SPREP157            NO  - ANY S/A DATA?                          
         L     R6,AIOAREA                                                       
         GOTO1 VADDELEM,DMCB,(R6),EMAIL                                         
*                                                                               
SPREP157 EQU   *                                                                
         OC    SAELEM+2(22),SAELEM+2                                            
*                                  ANY S/A DATA TO ADD?                         
         BZ    SPREP159            NO  - NOTHING TO ADD                         
         L     R6,AIOAREA                                                       
         GOTO1 VADDELEM,DMCB,(R6),SAELEMNT                                      
*                                                                               
SPREP159 EQU   *                                                                
         OC    MOSPNAME(22),MOSPNAME                                            
         BZ    SPREP187            NO  - NOTHING TO ADD                         
         L     R6,AIOAREA                                                       
         GOTO1 VADDELEM,DMCB,(R6),MOSPNAME                                      
*                                                                               
         B     SPREP187            WRITE NEW RECORD                             
* -- RECORD ALREADY EXISTS ------>                                              
SPREP160 DS    0H                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL            DOES EXCLUSION ELEMENT EXIST?                
         BNE   SPREP170            NO                                           
         L     R6,AIOAREA                                                       
         GOTO1 VDELELEM,DMCB,(X'10',(R6))                                       
*                                                                               
         TM    FLAG,ANYENTR        ANY ENTRIES?                                 
         BZ    SPREP174            NO - DON'T ADD ELEMENT                       
*                                                                               
SPREP170 DS    0H                                                               
         L     R6,AIOAREA                                                       
         CLI   ELEM,0              HAVE AN ELEM TO ADD?                         
         BE    SPREP174            NO                                           
         GOTO1 VADDELEM,DMCB,(R6),ELEM                                          
*                                                                               
SPREP174 DS    0H                                                               
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL            DOES EMAIL ADDRESS EXIST?                    
         BNE   SPREP176            NO                                           
         L     R6,AIOAREA          YES - DELETE IT                              
         GOTO1 VDELELEM,DMCB,(X'20',(R6))                                       
*                                                                               
SPREP176 EQU   *                                                                
         OC    EMAIL+2(48),EMAIL+2 NEW EMAIL ADDRESS?                           
         BZ    SPREP178            NO - DON'T ADD ELEMENT                       
*                                                                               
         L     R6,AIOAREA                                                       
         GOTO1 VADDELEM,DMCB,(R6),EMAIL                                         
***>>>                                                                          
SPREP178 EQU   *                                                                
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL            DOES S/A EMAIL ADDR ELT EXIST?               
         BNE   SPREP180            NO                                           
         L     R6,AIOAREA          YES - DELETE IT                              
         GOTO1 VDELELEM,DMCB,(X'21',(R6))                                       
*                                                                               
SPREP180 EQU   *                                                                
         OC    SAELEM+2(22),SAELEM+2    NEW EMAIL ADDRESS?                      
         BZ    SPREP181            NO - DON'T ADD ELEMENT                       
*                                                                               
         L     R6,AIOAREA                                                       
         GOTO1 VADDELEM,DMCB,(R6),SAELEMNT                                      
***>>>                                                                          
SPREP181 EQU   *                                                                
         L     R6,AIOAREA                                                       
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL            DOES MO S/P NAME ELT EXIST?                  
         BNE   SPREP182            NO                                           
         L     R6,AIOAREA          YES - DELETE IT                              
         GOTO1 VDELELEM,DMCB,(X'22',(R6))                                       
*                                                                               
SPREP182 EQU   *                                                                
         OC    MOSPNAME(22),MOSPNAME    NEW MO S/P NAME?                        
         BZ    SPREP187            NO - DON'T ADD ELEMENT                       
*                                                                               
         L     R6,AIOAREA                                                       
         GOTO1 VADDELEM,DMCB,(R6),MOSPNAME                                      
***>>>                                                                          
SPREP187 DS    0H                                                               
         GOTO1 VDATCON,DMCB,(5,0),(3,36(R6))                                    
         GOTO1 PUTREC                                                           
*                                                                               
SPREP190 DS    0H                                                               
         CLI   BACT,C'A'           TEST ADD                                     
         BE    SPREP195                                                         
         BAS   RE,SETSAL                                                        
         GOTO1 =A(PASSKEYS),DMCB,GOLDKEYS,AIOAREA,RR=Y                          
*                                                                               
SPREP195 MVC   AIOAREA,AIOSV                                                    
         XC    KEY,KEY             VALIDATE TEAM                                
         MVI   KEY,5                                                            
         MVC   KEY+23(2),REPALPHA                                               
         MVC   KEY+25(2),RSALTEAM                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BE    SPER0200                                                         
* POSITION TO TEAM FIELD                                                        
         LA    R2,LFMLAST                                                       
         LA    R3,2                                                             
         BAS   RE,NEXTUF                                                        
         BCT   R3,*-4                                                           
         LA    R3,NOTMERR                                                       
         B     ERROR                                                            
*                                                                               
SPER0200 DS    0H                                                               
         NI    FLAG,X'FF'-ANYENTR                                               
*                                                                               
         TM    RSALFLG,X'20'       'BLOCK FROM EDI' SET TO YES?                 
         BO    SPER0400            YES - NO NEW PASSIVE KEYS                    
         OC    LEAVDATE,LEAVDATE   ANY LEAVE DATE?                              
         BZ    SPREP220            NO                                           
         GOTO1 VDATCON,DMCB,(5,WORK),(2,WORK)                                   
*                                  GET TODAY'S DATE                             
         CLC   LEAVDATE,WORK                                                    
         BNH   SPREP240            LEAVE DATE NOT IN FUTURE                     
*                                                                               
SPREP220 DS    0H                                                               
*                                                                               
         GOTO1 =A(PASSKEYS),DMCB,GNEWKEYS,REC,RR=Y                              
         GOTO1 =A(NEW71REC),DMCB,GNEWKEYS,REC,RR=Y                              
SPREP240 DS    0H                                                               
*                                                                               
SPER0400 EQU   *                                                                
         B     FLFILE                                                           
         EJECT                                                                  
*******************************************************************             
SETSAL2  NTR1                                                                   
         MVC   MYKEY,REC           SAVE THE KEY                                 
         MVC   KEY(27),REC                                                      
         MVI   KEY,X'46'                                                        
*                                                                               
         MVC   AIOSV,AIOAREA       SAVE OFF REC, SO WE WON'T CLOBBER IT         
         LA    RF,MYIOAREA                                                      
         ST    RF,AIOAREA                                                       
         XIT1                                                                   
         EJECT                                                                  
*******************************************************************             
SETSAL   NTR1                                                                   
         MVC   KEY(27),MYKEY                                                    
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*        MVC   AIOAREA,AIOSV                                                    
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*******************************************************************             
*  MOVE STATION+MEDIA INTO THE KEY                                              
*******************************************************************             
PRSTAT   NTR1                                                                   
         LA    R4,KEY                                                           
         USING RSTAKEY,R4          STATION RECORD                               
         MVC   RSTAKSTA,SPACES                                                  
         CLI   11(R2),C'-'         3 LETTERS STAT CODE?                         
         BE    PRSTAT5                                                          
         CLI   11(R2),C' '         3 LETTERS AND IS IT TV?                      
         BE    PRSTAT10            MAYBE                                        
         CLI   12(R2),C'-'         4 LETTERS STAT CODE?                         
         BE    PRSTAT20                                                         
         CLI   12(R2),C' '         4 LETTERS AND IS IT TV?                      
         BE    PRSTAT25            MAYBE                                        
         B     STERROR                                                          
*                                                                               
PRSTAT5  DS    0H                                                               
         CLI   12(R2),C' '                                                      
         BE    STERROR                                                          
         B     PRSTAT15                                                         
*                                                                               
PRSTAT10 DS    0H                                                               
         CLI   12(R2),C'-'                                                      
         BE    STERROR                                                          
*                                                                               
PRSTAT15 DS    0H                                                               
         CLI   13(R2),C' '                                                      
         BNE   STERROR                                                          
         MVC   RSTAKSTA(3),8(R2)                                                
         LA    R8,12(R2)           R8 POINTS TO MEDIA                           
         B     PRSTAT30                                                         
*                                                                               
PRSTAT20 DS    0H                                                               
         CLI   13(R2),C' '                                                      
         BE    STERROR                                                          
         B     PRSTAT28                                                         
*                                                                               
PRSTAT25 DS    0H                                                               
         CLI   13(R2),C' '                                                      
         BNE   STERROR                                                          
*                                                                               
PRSTAT28 DS    0H                                                               
         MVC   RSTAKSTA(4),8(R2)                                                
         LA    R8,13(R2)           R8 POINTS TO MEDIA                           
*                                                                               
*CHECK IF VALID MEDIA                                                           
*                                                                               
PRSTAT30 DS    0H                                                               
         CLI   0(R8),C' '          TV?                                          
         BE    PRSTATEX            YES                                          
         CLI   0(R8),C'T'          TV?                                          
         BE    PRSTATEX            YES                                          
         CLI   0(R8),C'L'                                                       
         BE    PRSTAT50                                                         
         CLI   0(R8),C'A'                                                       
         BE    PRSTAT60                                                         
         CLI   0(R8),C'F'                                                       
         BE    PRSTAT70                                                         
         B     STERROR                                                          
*                                                                               
PRSTAT50 DS    0H                                                               
         MVI   RSTAKSTA+4,C'L'                                                  
         B     PRSTATEX                                                         
*                                                                               
PRSTAT60 DS    0H                                                               
         MVI   RSTAKSTA+4,C'A'                                                  
         B     PRSTATEX                                                         
*                                                                               
PRSTAT70 DS    0H                                                               
         MVI   RSTAKSTA+4,C'F'                                                  
*                                                                               
PRSTATEX DS    0H                                                               
         MVC   STATION,RSTAKSTA                                                 
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*******************************************************************             
         TITLE 'T80402 - SALESMAN RECORDS'                                      
DEVLSALE CLI   BFMTSW,0                                                         
         BNE   DSPSEDIT                                                         
* FORMAT ROUTINE                                                                
         BAS   RE,GETREC                                                        
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RDSPNAME,R2),RDSPNAME                                        
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RDSPTEL,R2),RDSPTEL                                          
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RDSPFAX,R2),RDSPFAX                                          
         FOUT  (R2)                                                             
*                                                                               
                                                                                
         BAS   RE,NEXTUF                                                        
         ZIC   RF,0(R2)            LENGTH OF FIELD                              
         LA    RE,9                                                             
         SR    RF,RE               SUB HEADER + 1 FROM TOTAL LENGTH             
         EX    RF,DEVS0060         CLEAR FIELD                                  
         OC    RDSPLEAV,RDSPLEAV   ANY LEAVE DATE?                              
         BZ    DEVS0020            NO                                           
         GOTO1 VDATCON,DMCB,(2,RDSPLEAV),(5,8(R2))                              
DEVS0020 EQU   *                                                                
         FOUT  (R2)                                                             
*                                                                               
*                                                                               
         B     EXXMOD                                                           
*                                                                               
DEVS0060 XC    8(0,R2),8(R2)       CLEAR BY LENGTH                              
*                                                                               
         EJECT                                                                  
DSPSEDIT MVC   REC+34(2),=X'0156'                                               
         MVC   REC+27(2),=Y(120)                                                
         MVC   RDSPPROF,ZEROS                                                   
*                                                                               
         BAS   RE,NEXTUF           NAME.  REQUIRED                              
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RDSPNAME,WORK                                                    
*                                                                               
         BAS   RE,NEXTUF           PHONE NUMBER.  OPTIONAL                      
         BAS   RE,MOVE                                                          
         MVC   RDSPTEL,WORK                                                     
*                                                                               
         BAS   RE,NEXTUF           FAX   NUMBER.  OPTIONAL                      
         BAS   RE,MOVE                                                          
         MVC   RDSPFAX,WORK                                                     
*                                                                               
         BAS   RE,NEXTUF           FIND NEXT FIELD FOR LEAVE DATE               
         CLI   5(R2),0             ANY DATA?                                    
         BE    DSPS0100            NO  - SKIP FIELD                             
         XC    WORK,WORK                                                        
         GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK                                      
         OC    0(4,R1),0(R1)       ERROR?                                       
         BZ    ERROR               YES                                          
         GOTO1 VDATCON,DMCB,WORK,(2,RDSPLEAV)                                   
*                                  INSERT DATE INTO RECORD                      
*                                                                               
*                                                                               
DSPS0100 B     FLFILE                                                           
         TITLE 'T80402 - GROUP RECORDS'                                         
GRP      CLI   BFMTSW,0                                                         
         BNE   GRPEDT                                                           
* FORMAT ROUTINE                                                                
         GOTO1 =A(GRPADDR),RR=Y                                                 
         B     EXXMOD                                                           
         EJECT                                                                  
GRPEDT   MVC   REC+34(2),=X'0148'                                               
         MVC   REC+27(2),=Y(106)                                                
         MVC   RGRPPROF,ZEROS                                                   
*                                                                               
         BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RGRPNAME,WORK                                                    
         BAS   RE,NEXTUF                                                        
         CLI   REC+26,C' '         NO SUB NAME ON GROUP RECORD                  
         BNE   GRPEDT1                                                          
         CLI   5(R2),0                                                          
         BNE   FLERR1                                                           
         B     GRPEDT1A                                                         
         SPACE 1                                                                
GRPEDT1  CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         BAS   RE,MOVE                                                          
         MVC   RGRPSBNM,WORK                                                    
*                                                                               
GRPEDT1A LA    R4,RGRPPROF                                                      
GRPEDT2  BAS   RE,NEXTUF                                                        
         BE    GRPEDTX                                                          
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
         MVC   0(1,R4),8(R2)                                                    
         LA    R4,1(R4)                                                         
         B     GRPEDT2                                                          
*                                                                               
GRPEDTX  B     FLFILE                                                           
         TITLE 'T80402 - OFC/TM/SL/GRP RECORDS'                                 
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
         B     FLX                                                              
*                                                                               
FLADD    BAS   RE,ADDREC                                                        
         MVC   BSVDA,KEY           SAVE DISK ADDRESS                            
*                                                                               
FLX      DS    0H                                                               
         CLI   BREC,6              SALESPERSON                                  
         BNE   EXXMOD                                                           
         GOTO1 =A(SALPTR),RR=Y                                                  
         GOTO1 =A(SALPTR2),RR=Y                                                 
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
FLERR3   LA    R3,NUMERR                                                        
         B     ERROR                                                            
FLERR4   EQU   *                                                                
         MVC   LFMMSG(L'OPTNERR),OPTNERR                                        
         B     MYERR                                                            
FLERR5   EQU   *                                                                
         MVC   LFMMSG(L'SPEQERR),SPEQERR                                        
         B     MYERR                                                            
*                                                                               
DUPERR   EQU   *                   DUPLICATE ENTRY ERROR MESSAGE                
         MVC   LFMMSG(L'DUPENTRY),DUPENTRY                                      
         MVI   ERRAREA,X'FF'       MSG ALREADY DONE                             
         OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
         XMOD1 2                                                                
*        B     MYERR                                                            
*                                                                               
FLERR6   EQU   *                                                                
         MVC   LFMMSG(L'SPOTERR),SPOTERR                                        
         B     MYERR                                                            
*                                                                               
FLERR7   EQU   *                                                                
         MVC   LFMMSG(L'BITERR),BITERR                                          
         B     MYERR                                                            
*                                                                               
FLERR8   EQU   *                                                                
         MVC   LFMMSG(L'LENERR),LENERR                                          
         B     MYERR                                                            
*                                                                               
FLERR9   EQU   *                                                                
         MVC   LFMMSG(L'LENER2),LENER2                                          
         B     MYERR                                                            
*                                                                               
FLERR10  EQU   *                                                                
         MVC   LFMMSG(L'SAFLGERR),SAFLGERR                                      
         B     MYERR                                                            
*                                                                               
* MSG ALREADY SET UP IN HEADER                                                  
MYERR    MVI   ERRAREA,X'FF'       MSG ALREADY DONE                             
         B     EXIT                                                             
*                                                                               
STERROR  LA    R3,INVSTAT                                                       
         B     ERROR                                                            
*                                                                               
ZEROS    DC    30C'0'                                                           
BLANKS   DC    CL30' '                                                          
*                                                                               
OPTNERR  DC    C'OPTION 2 NOT ALLOWED UNLESS OPTION 1 IS SET'                   
*                                                                               
SPEQERR  DC    C'CODE MUST BE 5 DIGITS W/LEADING ZEROS'                         
*                                                                               
SPOTERR  DC    C'FORMAT OF CODE MUST BE    SSSSTTOO'                            
*                                                                               
BITERR   DC    C'PROFILE BITS 4 AND 5 CANNOT BOTH BE ''Y'''                     
*                                                                               
DUPENTRY DC    C'DUPLICATE ENTRY NOT ALLOWED'                                   
*                                                                               
LENERR   DC    C'LENGTH MUST BE 8 CHARACTERS'                                   
*                                                                               
LENER2   DC    C'LENGTH MUST BE 3 CHARACTERS'                                   
*                                                                               
SAFLGERR DC    C'NO S/A EMAIL ADDRESS: "Y" IS INVALID'                          
*                                                                               
         GETEL R6,34,ELCODE                                                     
*** LOCAL STORAGE AREA                                                          
ELCODE   DS    X                                                                
AIOSV    DS    A                                                                
PROFBITS DS    X                                                                
PROFBIT2 DS    X                                                                
FOXZEROS DC    C'0000000000000000'                                              
MYIOAREA DS    1000C               FOR REGENOFF2                                
ELEM     DS    CL256                                                            
BUFFER   DS    CL180                                                            
ELLENGTH DS    X                                                                
ADDPUT   DS    CL1                                                              
STATION  DS    CL5                                                              
MYKEY    DS    CL27                                                             
FLAG     DS    X                                                                
EMAILLEN DS    X                   LENGTH OF EMAIL INPUT                        
EMAIL    DS    CL50                EMAIL ADDRESS: TEMP STORAGE                  
MOSPNAME DS    CL22                MO S/P NAME  : TEMP STORAGE                  
*                                                                               
SAELEMNT DS    0CL73                                                            
SAELEM   DC    XL2'2117'           INITIALIZE ELEMENT TO 23                     
SANAME   DS    CL20                S/A NAME                                     
SAFLAG   DS    CL1                 FLAGS                                        
*                                  X'80' = EMAIL TO S/A ONLY                    
SAEMAL   DS    CL50                S/A EMAIL ADDR: TEMP STORAGE                 
*                                                                               
ANYENTR  EQU   X'80'               ANY ENTRIES                                  
FRSTADDR DS    A                   ADDRESS OF THE FIRST EXCL FIELD              
GKEY     DS    CL40                EXTRA GENDIR KEY HOLDER                      
GKEYSAVE DS    CL40                EXTRA GENDIR KEY SAVE                        
UPDATCON DS    XL1                                                              
LEAVDATE DS    XL2                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE RELFMGEN                                                       
         EJECT                                                                  
T80402   CSECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
GENOLD   DSECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENOFF                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENTEM                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENSAL2                                                      
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENSTA                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENDSP                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENGRP                                                       
         SPACE 2                                                                
         ORG                                                                    
*                                                                               
       ++INCLUDE RELFMTWA                                                       
         EJECT                                                                  
       ++INCLUDE REGENOFF2                                                      
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMF9D                                                       
         ORG   LF9WORK+4                                                        
ORIGSAL  DS    CL3                                                              
         EJECT                                                                  
*              DDCOMFACS                                                        
       ++INCLUDE FATWA                                                          
       ++INCLUDE DDCOMFACS                                                      
GSPSAL   DSECT                                                                  
       ++INCLUDE GEGENSPSAL                                                     
T80402   CSECT                                                                  
*          DATA SET REDAR04    AT LEVEL 221 AS OF 01/28/03                      
***********************************************************************         
* VALIDATE REP                                                                  
*                                                                               
***********************************************************************         
VALIREP  NTR1  LABEL=*,BASE=*                                                   
         L     R2,0(R1)            RESET A(FIELD BEING TESTED)                  
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,1               SET REP RECORD TYPE                          
         MVC   KEY+25(2),8(R2)     INSERT ALPHA ID INTO KEY                     
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     REP CODE FOUND?                              
         BNE   VREP0080            NO  - NOT ON FILE                            
VREP0060 EQU   *                   YES - ACCEPT IT                              
         SR    R0,R0               SET CC = ZERO                                
         B     VREP0100                                                         
VREP0080 EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO:  ERROR                      
VREP0100 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***>>>                                                                          
*******************************************************************             
*    MOVED FOR ADDRESSABILITY                                     *             
*******************************************************************             
GRPADDR  NTR1  LABEL=*,BASE=*                                                   
         BAS   RE,GETREC                                                        
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RGRPNAME,R2),RGRPNAME                                        
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RGRPSBNM,R2),RGRPSBNM                                        
         FOUT  (R2)                                                             
*                                                                               
         LA    R4,RGRPPROF                                                      
GRPFMT2  BAS   RE,NEXTUF                                                        
         BE    GRPFMTX                                                          
         MVC   8(1,R2),0(R4)                                                    
         LA    R4,1(R4)                                                         
         B     GRPFMT2                                                          
*                                                                               
GRPFMTX  EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*******************************************************************             
*    BUILD EXCLUSION ELEMENT *                                                  
*******************************************************************             
BUILD10  NTR1  LABEL=*,BASE=*                                                   
         LA    R6,ELEM                                                          
         USING RSALEXEM,R6         STATION EXCLUSION ELEMENT                    
*                                                                               
         OC    ELEM,ELEM           FIRST TIME THROUGH?                          
         BZ    BL10                                                             
*                                                                               
         ZIC   RF,RSALEXLN         ELEMENT LENGTH                               
         SH    RF,=H'2'                                                         
         LA    R7,RSALEXST                                                      
         AR    RF,R7                                                            
         MVC   0(L'STATION,RF),STATION                                          
         ZIC   RF,RSALEXLN                                                      
         LA    RF,5(RF)                                                         
         STC   RF,RSALEXLN         INCREMENT LENGTH OF ELEMENT                  
         B     BUILDEX                                                          
*                                                                               
BL10     DS    0H                                                               
         MVI   RSALEXCD,X'10'                                                   
         MVI   RSALEXLN,7                                                       
         MVC   RSALEXST,STATION                                                 
*                                                                               
BUILDEX  DS    0H                                                               
         MVC   ELLENGTH,RSALEXLN                                                
         XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   RELOCATED CODE TO GAIN SOME ADDRESS SPACE                                   
*                                                                               
SALEADDR NTR1  LABEL=*,BASE=*                                                   
         XC    LF9MAIL,LF9MAIL     CLEAR S/P EMAIL ADDRESS                      
         FOUT  LF9MAILH                                                         
         XC    LF9SMAL,LF9SMAL     CLEAR S/A EMAIL ADDR                         
         FOUT  LF9SMALH                                                         
         XC    LF9SONL,LF9SONL     CLEAR S/A FLAG                               
         FOUT  LF9SONLH                                                         
         XC    LF9SAST,LF9SAST     CLEAR S/A NAME                               
         FOUT  LF9SASTH                                                         
         XC    LF9RCOD,LF9RCOD     CLEAR REP CODE                               
         FOUT  LF9RCODH                                                         
         XC    LF9MINU,LF9MINU     CLEAR MINUS FLAG                             
         FOUT  LF9MINUH                                                         
         XC    LF9FPRF,LF9FPRF     CLEAR FAX PREFERENCE                         
         FOUT  LF9FPRFH                                                         
         XC    LF9MONM,LF9MONM     CLEAR MO S/P NAME                            
         FOUT  LF9MONMH                                                         
         BAS   RE,GETREC                                                        
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RSALNAME,R2),RSALNAME                                        
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVI   8(R2),C'Y'          SET 'FOR EDI USE = YES'                      
         TM    RSALFLG,X'20'       BLOCK EDI USE?                               
         BNO   SPDS0020            NO                                           
         MVI   8(R2),C'N'          SET 'FOR EDI USE = NO '                      
SPDS0020 EQU   *                                                                
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RSALTEAM,R2),RSALTEAM                                        
         MVC   SVTEAM,RSALTEAM                                                  
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF           REP CODE                                     
         CLC   RSALPOWR,SPACES     ANYTHING IN FIELD?                           
         BNH   SPDS0030            NO                                           
         CLC   RSALPOWR,=C'00'     ANYTHING IN FIELD?                           
         BE    SPDS0030            NO                                           
         MVC   8(L'RSALPOWR,R2),RSALPOWR                                        
         BAS   RE,NEXTUF           BUMP TO 'MINUS' FIELD                        
         TM    RSALPFLG,X'80'      'MINUS' FLAG SET?                            
         BNO   SPDS0035            NO                                           
         MVI   8(R2),C'Y'          YES - SET SCREEN                             
         B     SPDS0035                                                         
SPDS0030 EQU   *                                                                
         BAS   RE,NEXTUF           SKIP 'MINUS' FIELD                           
SPDS0035 EQU   *                                                                
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RSALTEL,R2),RSALTEL                                          
         FOUT  (R2)                                                             
*                                                                               
         LA    R4,RSALOFF          INSERT SALESPERSON OFFICE                    
         BAS   RE,NEXTUF                                                        
         MVC   8(2,R2),0(R4)                                                    
         FOUT  (R2)                                                             
         MVC   SVOFFICE,RSALOFF    SAVE S/P OFFICE                              
*                                                                               
*                                                                               
         TM    SVPGPBIT+2,X'04'    PROFILE = DISPLAY/ACCEPT MO S/P?             
         BNO   SPDS0036            NO  -                                        
*                                                                               
         BAS   RE,NEXTUF           YES - SKIP MO S/P FOR NOW                    
*                                                                               
SPDS0036 EQU   *                                                                
*                                                                               
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RSALFAX,R2),RSALFAX                                          
         FOUT  (R2)                                                             
*                                                                               
*                                                                               
         BAS   RE,NEXTUF           FAX PREF                                     
         XC    8(2,R2),8(R2)       CLEAR FAX PREFERENCE FIELD                   
         TM    RSALFLG,X'04'       E-MAIL PREFERENCE SET?                       
         BO    SPDS0037            YES - LEAVE FIELD BLANK                      
         TM    RSALFLG,X'02'       FAX    PREFERENCE SET?                       
         BNO   SPDS0037            NO  - LEAVE FIELD BLANK                      
         MVI   8(R2),C'Y'          YES - SET FAX PREFERENCE                     
SPDS0037 EQU   *                                                                
         FOUT  (R2)                                                             
         BAS   RE,NEXTUF           SKIP EMAIL FIELD FOR NOW                     
         BAS   RE,NEXTUF           SKIP S/A   FIELD FOR NOW                     
         BAS   RE,NEXTUF           SKIP S/A FLAG  FIELD FOR NOW                 
         BAS   RE,NEXTUF           SKIP S/A EMAIL FIELD FOR NOW                 
*                                     NOT ELEGANT, BUT WORKS                    
         ZIC   RF,0(R2)            LENGTH OF FIELD                              
         LA    RE,9                                                             
         SR    RF,RE               SUB HEADER + 1 FROM TOTAL LENGTH             
         EX    RF,SPDS0060         CLEAR FIELD                                  
         FOUT  (R2)                                                             
         BAS   RE,NEXTUF                                                        
         ZIC   RF,0(R2)            LENGTH OF FIELD                              
         LA    RE,9                                                             
         SR    RF,RE               SUB HEADER + 1 FROM TOTAL LENGTH             
         EX    RF,SPDS0060         CLEAR FIELD                                  
         OC    RSALLEAV,RSALLEAV   ANY DATE?                                    
         BZ    SPDS0040            NO                                           
         GOTO1 VDATCON,DMCB,(3,RSALLEAV),(5,8(R2))                              
SPDS0040 EQU   *                                                                
         FOUT  (R2)                                                             
         BAS   RE,NEXTUF                                                        
         MVI   8(R2),C'N'          SET MANAGER FLAG TO 'NO'                     
         FOUT  (R2)                                                             
         CLI   RSALMGR,X'00'       ANY MANAGER FLAG?                            
         BZ    SPDS0070            NO  - LEAVE AS 'N'                           
         MVC   8(1,R2),RSALMGR     YES - MOVE IT TO SCREEN                      
         B     SPDS0070                                                         
*                                                                               
SPDS0060 XC    8(0,R2),8(R2)       CLEAR BY LENGTH                              
*                                                                               
SPDS0070 DS    0H                                                               
*                                  FILE TO SCREEN                               
         XC    LF9SPEQ,LF9SPEQ     CLEAR THE EQUIV FIELD                        
         MVC   ORIGSAL,RSALMRG     SAVE ORIGINAL SALESPERSON                    
         TM    SVPGPBIT+1,X'10'    CONVERT PROFILE SET FOR S/P EQUIV?           
         BO    SPDS0072            YES - USE 3-CHAR S/P FORMAT                  
         TM    SVPGPBIT+1,X'40'    PROFILE SET FOR S/P EQUIV?                   
         BNO   SPDS0085            NO  - DON'T DISPLAY ANYTHING                 
*                                                                               
*   CONSIDER ALL S/P EQU AS ALPHANUM                                            
***      TM    RSALFLG,X'80'       MERGE VALUE STORED AS ALPHANUM?              
***      BNO   SPDS0082            NO                                           
*                                                                               
         MVC   LF9SPEQ(3),RSALMRG  YES - DISPLAY IT AS SUCH                     
         LA    RF,LF9SPEQH                                                      
****     OI    1(RF),X'20'         DATA FIELD PROTECTED                         
         B     SPDS0085                                                         
SPDS0072 DS    0H                                                               
         MVC   LF9SPEQ(3),RSALMRG  YES - DISPLAY IT AS SUCH                     
         LA    RF,LF9SPEQH                                                      
         NI    1(RF),X'FF'-X'20'   DATA FIELD UNPROTECTED                       
         B     SPDS0085                                                         
SPDS0082 EQU   *                                                                
*                                                                               
*   KATZ FORMAT NO LONGER RECOGNIZED.  BUHR (MAR11/06)                          
*                                                                               
*&&DO                                                                           
         TM    SVPGPBIT+1,X'20'    YES - KATZ OR SELTEL FORMAT?                 
         BNO   SPDS0084            NOT ON = KATZ                                
*&&                                                                             
SPDS0083 EQU   *                                                                
         CLC   RSALEQU,FOXZEROS    IS FIELD ALL ZERO?                           
         BE    SPDS0085            YES - DON'T DISPLAY ANYTHING                 
         CLC   RSALEQU,SPACES      ALL SPACES?                                  
         BE    SPDS0085            YES - DON'T DISPLAY ANYTHING                 
         OC    RSALEQU,RSALEQU     BINARY ZEROS?                                
         BZ    SPDS0085            YES - DON'T DISPLAY ANYTHING                 
         MVC   LF9SPEQ(8),RSALEQU  NO  - LOAD EQUIVALENCY VALUE                 
         B     SPDS0085                                                         
SPDS0084 EQU   *                                                                
         OC    RSALMRG,RSALMRG     ANYTHING IN FIELD?                           
         BZ    SPDS0085            NO                                           
         EDIT  RSALMRG,(5,LF9SPEQ),FILL=0                                       
*                                  EXPAND SALESPERSON EQUIVALENCE               
SPDS0085 EQU   *                                                                
         FOUT  LF9SPEQH                                                         
*                                                                               
         XC    LF9SPLC,LF9SPLC     CLEAR THE DATE                               
         OC    RSALLCD,RSALLCD                                                  
         BZ    SPDS0090                                                         
         CLC   RSALLCD,=C'00'                                                   
         BE    SPDS0090                                                         
         GOTO1 VDATCON,DMCB,(2,RSALLCD),(5,LF9SPLC)                             
SPDS0090 EQU   *                                                                
         FOUT  LF9SPLCH                                                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   RELOCATED CODE TO GAIN SOME ADDRESS SPACE                                   
*                                                                               
TEAMADDR NTR1  LABEL=*,BASE=*                                                   
         BAS   RE,GETREC                                                        
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RTEMDVNM,R2),RTEMDVNM                                        
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'RTEMNAME,R2),RTEMNAME                                        
         FOUT  (R2)                                                             
*                                                                               
         LA    R4,RTEMPROF                                                      
TMFMT2   BAS   RE,NEXTUF                                                        
         BE    TMFMTX                                                           
         MVC   8(1,R2),0(R4)                                                    
         B     TMFMT2                                                           
*                                                                               
TMFMTX   EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   RELOCATED CODE TO GAIN SOME ADDRESS SPACE                                   
*                                                                               
OFFCADDR NTR1  LABEL=*,BASE=*                                                   
         BAS   RE,GETREC                                                        
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'ROFFNAME,R2),ROFFNAME                                        
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'ROFFADD1,R2),ROFFADD1                                        
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'ROFFADD2,R2),ROFFADD2                                        
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'ROFFSTT,R2),ROFFSTT                                          
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'ROFFZIP,R2),ROFFZIP                                          
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF                                                        
         MVC   8(L'ROFFREG,R2),ROFFREG                                          
         FOUT  (R2)                                                             
* PROFILE AND FAX NUMBER KEPT IN REGENOFF2                                      
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         TWAXC (R2)                CLEAR PROFILE AND FAX NUMBER                 
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(16,R2),=C'NNNNNNNNNNNNNNNN' DEFAULT SETTINGS                   
         OI    6(R2),X'80'                                                      
*                                                                               
*******************************************************************             
* THIS IS ALL TO DISPLAY 'N' FOR DEFAULT NATIONAL OFFICE!                       
*                                                                               
* POINT TO FAX NUMBER                                                           
         LR    R1,R2               PNT TO PROFILE OUTPUT                        
         ZIC   R0,0(R1)            POINT TO 'FAX NUMBER'                        
         AR    R1,R0                                                            
         ZIC   R0,0(R1)            PNT TO '('                                   
         AR    R1,R0                                                            
         ZIC   R0,0(R1)            PNT TO AREA CODE INPUT                       
         AR    R1,R0                                                            
         ZIC   R0,0(R1)            PNT TO ')'                                   
         AR    R1,R0                                                            
         ZIC   R0,0(R1)            PNT TO PREFIX INPUT                          
         AR    R1,R0                                                            
         ZIC   R0,0(R1)            PNT TO '-'                                   
         AR    R1,R0                                                            
         ZIC   R0,0(R1)            PNT TO SUFFIX INPUT                          
         AR    R1,R0                                                            
* POINT TO NATIONAL/LOCAL OFFICE SCOPE FIELD                                    
         ZIC   R0,0(R1)                                                         
         AR    R1,R0                                                            
         ZIC   R0,0(R1)                                                         
         AR    R1,R0                                                            
         MVI   8(R1),C'N'          DISPLAY NATIONAL OFFICE (DEFAULT)            
         OI    6(R1),X'80'                                                      
*                                                                               
* END DISPLAY OF DEFAULT NATIONAL OFFICE                                        
*********************************************************************           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'ROFFKEY),ROFFKEY                                           
         MVI   KEY,ROFF2TYQ        PROFILE/FAX INFO KEPT IN REGENOFF2           
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   OFCFMTX                                                          
*                                                                               
         MVC   AIOSV,AIOAREA       SAVE OFF REC, SO WE WON'T CLOBBER IT         
         LA    RF,MYIOAREA                                                      
         ST    RF,AIOAREA                                                       
*                                                                               
         BAS   RE,GETREC                                                        
         L     R6,AIOAREA                                                       
         USING ROFF2FXE,R6                                                      
         MVI   ELCODE,ROFF2CDQ                                                  
         BAS   RE,GETEL                                                         
*                                                                               
* PRINT PROFILES, WE ARE ONLY USING THE FIRST BYTE (8 BITS) FOR NOW             
* THIS CAN BE EXPANDED TO ACCOMODATE UP TO 32 BITS                              
*                                                                               
         GOTO1 BITOUT,DMCB,ROFF2PRF,8(R2)                                       
         GOTO1 BITOUT,DMCB,ROFF2PRF+1,16(R2)                                    
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
* PRINT FAX NUMBER                                                              
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         MVC   8(3,R2),ROFF2FAX    AREA CODE                                    
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(3,R2),ROFF2FAX+3  PREFIX                                       
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(4,R2),ROFF2FAX+6  SUFFIX                                       
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
* PRINT NATIONAL OR LOCAL OFFICE                                                
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    ROFF2PRF+1,X'80'    LOCAL OFFICE?                                
         BZ    OFC10               NO, SKIP                                     
         MVI   8(R2),C'L'          LOCAL OFFICE DISPLAY                         
         DROP  R6                                                               
*                                                                               
OFC10    OI    6(R2),X'80'         XMIT                                         
         L     RF,AIOSV            RESTORE IOAREA                               
         ST    RF,AIOAREA                                                       
*                                                                               
OFCFMTX  EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  BITOUT -- DISPLAY 8 Y/N BYTES                                                
*  P1 = A(BYTE DATA BITS)                                                       
*  R2 = A(FLD HEADER FOR OUTPUT)                                                
***********************************************************************         
BITOUT   NTR1                                                                   
         LA    R0,8                8 BITS TO DISPLAY                            
         L     R3,4(R1)                                                         
         L     RE,0(R1)            A(DATA BITS)                                 
         L     R4,0(RE)                                                         
         B     BOUT30                                                           
*                                                                               
BOUT20   SLDL  R4,1                                                             
BOUT30   MVI   0(R3),C'Y'                                                       
         LTR   R4,R4               HI-ORDER BIT ON?                             
         BM    BOUT40              YES. LEAVE AS 'Y'                            
         MVI   0(R3),C'N'                                                       
BOUT40   LA    R3,1(R3)            NEXT A(OUT)                                  
         BCT   R0,BOUT20                                                        
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*******************************************************************             
*  SEE IF STATION TO BE ADDED IS A DUPLICATE ENTRY                              
*******************************************************************             
DUPLICST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    BUFFER,BUFFER                                                    
         LA    R2,LF9EXLFH         FIRST EXCLUSION FIELD                        
         LA    R7,LF9LAST          LAST EXCLUSION FIELD                         
         LA    R5,BUFFER                                                        
*                                                                               
DUPST05  EQU   *                                                                
         CR    R2,R7               ALL ENTRIES HAVE BEEN VALIDATED?             
         BNL   DUPSTEX             YES                                          
*                                                                               
         CLI   5(R2),0             ANY ENTRIES?                                 
         BE    NEXT                NO                                           
*                                                                               
DUPST10  EQU   *                                                                
         OC    8(6,R2),SPACES                                                   
         OI    6(R2),X'80'                                                      
         OC    0(6,R5),0(R5)       ANY ENTRY IN BUFFER?                         
         BZ    DUPST20             NO  - ADD SCREEN ENTRY TO TABLE              
         CLC   8(6,R2),0(R5)       YES - SCREEN ALREADY IN TABLE?               
         BE    DUPERR              YES                                          
         LA    R5,6(R5)            NO  - BUMP TO NEXT ENTRY                     
         B     DUPST10             GO BACK AND CHECK NEXT                       
*                                                                               
DUPST20  EQU   *                                                                
         MVC   0(6,R5),8(R2)                                                    
         LA    R5,BUFFER           RESET A(BUFFER)                              
*                                                                               
NEXT     EQU   *                                                                
         ZIC   R4,0(R2)            BUMP BY L(EXCLUDED STATION FIELD)            
         AR    R2,R4                                                            
         B     DUPST05             GO CHECK NEXT FIELD                          
*                                                                               
DUPSTEX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* SALPTR - GENERATES SALESMAN PASSIVE PTR                                       
*********************************************************************           
SALPTR   NTR1  BASE=*,LABEL=*                                                   
         CLI   BACT,C'A'           ADD?                                         
         BE    SALPTR20                                                         
         CLC   REC+(RSALNAME-RSALREC)(20),REC2+(RSALNAME-RSALREC)               
         BE    SALPTRX                                                          
         XC    KEY,KEY             DELETE OLD KEY                               
         MVI   KEY,X'86'                                                        
         MVC   KEY+2(2),REPALPHA                                                
         MVC   KEY+4(20),REC2+(RSALNAME-RSALREC)                                
         MVC   KEY+24(3),REC2+(RSALKSAL-RSALREC)                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'REPDIR',KEY,KEY                      
         TM    DMCB+8,X'10'                                                     
         BO    SALPTR20                                                         
         OI    KEY+27,X'80'                                                     
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'REPDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
SALPTR20 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'86'                                                        
         MVC   KEY+2(2),REPALPHA                                                
         MVC   KEY+4(20),REC+(RSALNAME-RSALREC)                                 
         MVC   KEY+24(3),REC+(RSALKSAL-RSALREC)                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'08',=C'DMREAD'),=C'REPDIR',KEY,KEY              
         TM    DMCB+8,X'02'                                                     
         BZ    SALPTR30                                                         
         NI    KEY+27,X'FF'-X'80'                                               
         MVC   KEY+28,BSVDA                                                     
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'REPDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    SALPTRX                                                          
         DC    H'0'                                                             
SALPTR30 DS    0H                                                               
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+28,BSVDA                                                     
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REPDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    SALPTRX                                                          
         DC    H'0'                                                             
SALPTRX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* SALPTR2 - GENERATES SALESMAN PASSIVE PTR X'8601'                              
*********************************************************************           
SALPTR2  NTR1  BASE=*,LABEL=*                                                   
         CLI   BACT,C'A'           ADD?                                         
         BE    SALP210                                                          
         CLC   REC+(RSALTEAM-RSALREC)(2),REC2+(RSALTEAM-RSALREC)                
         BE    SALP2X                                                           
         XC    KEY,KEY             DELETE OLD KEY                               
         MVC   KEY(2),=X'8601'                                                  
         MVC   KEY+2(2),REPALPHA                                                
         MVC   KEY+22(2),REC2+(RSALTEAM-RSALREC)                                
         MVC   KEY+24(3),REC2+(RSALKSAL-RSALREC)                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'REPDIR',KEY,KEY                      
         TM    DMCB+8,X'10'                                                     
         BO    SALP210                                                          
         OI    KEY+27,X'80'                                                     
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'REPDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
SALP210  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'8601'                                                  
         MVC   KEY+2(2),REPALPHA                                                
         MVC   KEY+22(2),REC+(RSALTEAM-RSALREC)                                 
         MVC   KEY+24(3),REC+(RSALKSAL-RSALREC)                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(X'08',=C'DMREAD'),=C'REPDIR',KEY,KEY              
         TM    DMCB+8,X'02'                                                     
         BZ    SALP220                                                          
         NI    KEY+27,X'FF'-X'80'                                               
         MVC   KEY+28,BSVDA                                                     
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'REPDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    SALP2X                                                           
         DC    H'0'                                                             
SALP220  DS    0H                                                               
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+28,BSVDA                                                     
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REPDIR',KEY,KEY                       
         CLI   DMCB+8,0                                                         
         BE    SALP2X                                                           
         DC    H'0'                                                             
SALP2X   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
*  BITIN -- EDIT 8 Y/N/BLANKS AND RETURN BYTE BITS.                             
*  P1 = A(BYTE OUTPUT AREA)                                                     
*  R2 = A(INPUT FIELD HEADER)                                                   
*  CC: ^0 = ERROR.  MSG ALREADY SET.                                            
*  NOTE: BLANK = 'N'                                                            
***********************************************************************         
BITIN    NTR1  BASE=*,LABEL=*                                                   
         LA    R0,8                8 BITS TO DO                                 
         SR    R4,R4               R4 = BYTE OUTPUT                             
         L     RE,=XL4'80000000'   RE = ROTATING BIT                            
         L     R3,4(R1)                                                         
         B     BIN30                                                            
*                                                                               
BIN20    SRDL  RE,1                ROTATE BIT RIGHT BY 1                        
BIN30    EQU   *                                                                
         OI    0(R3),C' '          CONVERT NULL TO BLANK                        
         CLI   0(R3),C'N'                                                       
         BE    BIN40                                                            
         CLI   0(R3),C' '          BLANK = N                                    
         BE    BIN40                                                            
         CLI   0(R3),C'Y'          ONLY OTHER VALUE                             
         BNE   BINERR                                                           
*                                                                               
         OR    R4,RE               TURN ON BIT IN OUTPUT REGS                   
*                                                                               
BIN40    LA    R3,1(R3)            NEXT A(IN)                                   
         BCT   R0,BIN20                                                         
*                                                                               
         L     R3,0(R1)                                                         
         STCM  R4,15,0(R3)         A(USER'S OUTPUT AREA)                        
         SR    R0,R0               GOOD CC                                      
BINEXT   XIT1                                                                   
         EJECT                                                                  
*                                                                               
*- ERROR.  PUT UP VERY SPECIFIC MESSAGE.                                        
*                                                                               
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
         LTORG                                                                  
         EJECT                                                                  
PASSKEYS NTR1  LABEL=*,BASE=*                                                   
         L     R3,0(R1)            SET A(KEY STORAGE AREA)                      
         L     R4,4(R1)            SET A(RECORD)                                
         USING RSALREC,R4                                                       
*                                                                               
*   BUILD PRIMARY KEY FOR THIS RECORD                                           
*                                                                               
         XC    0(GSPLDLEN,R3),0(R3)  GENERATE PRIMARY KEY                       
         MVI   GSPLKTYP-GSPLKEY(R3),GSPLRECQ                                    
         MVI   GSPLKSTP-GSPLKEY(R3),X'01'                                       
*                                  INSERT SUBREC TYPE INTO KEY                  
         MVC   GSPLKREP-GSPLKEY(02,R3),RSALKREP                                 
*                                  INSERT REP CODE    INTO KEY                  
         MVC   GSPLKSAL-GSPLKEY(03,R3),RSALKSAL                                 
*                                  INSERT S-P CODE INTO PASSIVE                 
*                                                                               
         LA    R3,KEYLEN(R3)       R3=A(NEXT PASSIVE POINTER)                   
         XC    0(GSPLDLEN,R3),0(R3)  GENERATE S/P PASSIVES                      
         MVI   GSPLPTYP-GSPLKEY(R3),GSPLPTYQ                                    
         MVI   GSPLPSTP-GSPLKEY(R3),X'01'                                       
*                                  INSERT SUBREC TYPE INTO KEY                  
         MVC   GSPLPREP-GSPLKEY(02,R3),RSALKREP                                 
*                                  INSERT REP CODE    INTO KEY                  
         MVC   GSPLPNAM-GSPLPKEY(20,R3),RSALNAME                                
*                                  INSERT S-P NAME INTO PASSIVE                 
         MVC   GSPLPCOD-GSPLPKEY(03,R3),RSALKSAL                                
*                                  INSERT S-P CODE INTO PASSIVE                 
         MVC   GSPLPOFF-GSPLPKEY(02,R3),RSALOFF                                 
*                                  INSERT S-P OFFC INTO PASSIVE                 
*                                                                               
         LA    R3,KEYLEN(R3)       R3=A(NEXT PASSIVE POINTER)                   
         XC    0(KEYLEN,R3),0(R3)                                               
*                                                                               
*   GENERATE SECOND PASSIVE KEY FOR THIS RECORD                                 
*                                                                               
         XC    0(GSPLDLEN,R3),0(R3)  GENERATE S/P PASSIVES                      
         MVI   GSPLPTYP-GSPLKEY(R3),GSPLPTYQ                                    
         MVI   GSPLPSTP-GSPLKEY(R3),X'03'                                       
*                                  INSERT SUBREC TYPE INTO KEY                  
         MVC   GSPLPREP-GSPLKEY(02,R3),RSALKREP                                 
*                                  INSERT REP CODE    INTO KEY                  
         MVC   GSP2POFF-GSPLPKEY(02,R3),RSALOFF                                 
*                                  INSERT S-P OFFC INTO PASSIVE                 
         MVC   GSP2PNAM-GSPLPKEY(20,R3),RSALNAME                                
*                                  INSERT S-P NAME INTO PASSIVE                 
         MVC   GSP2PCOD-GSPLPKEY(03,R3),RSALKSAL                                
*                                  INSERT S-P CODE INTO PASSIVE                 
         XIT1                                                                   
KEYLEN   EQU   40                                                               
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*   NEW RECORD EQUATES:                                                         
*                                                                               
DNEWLEN  EQU   GSPLFLEN-GSPLRECD+1                                              
*                                  MOVING TO 2ND BYTE ONLY                      
DNEWCODE EQU   GSPLSPCD-GSPLRECD                                                
DNEWELEN EQU   GSPLSPLN-GSPLRECD                                                
DNEWNAME EQU   GSPLSPNM-GSPLRECD                                                
DNEWOFFC EQU   GSPLSPOF-GSPLRECD                                                
DNEWTEAM EQU   GSPLSPTM-GSPLRECD                                                
DNEWFLAG EQU   GSPLSPFL-GSPLRECD                                                
DNEWFAX  EQU   GSPLSPFX-GSPLRECD                                                
DNEWMAIL EQU   GSPLSPEM-GSPLRECD                                                
DNEWPOWR EQU   GSPLSPPC-GSPLRECD                                                
*                                                                               
NEW71REC NTR1  LABEL=*,BASE=*                                                   
         LA    R4,REC                                                           
         USING RSALREC,R4                                                       
         XC    GNEWREC,GNEWREC     CLEAR RECORD AREA                            
         MVC   GNEWREC(32),GNEWKEYS                                             
*                                  INSERT NEW PRIMARY KEY                       
         MVI   GNEWREC+DNEWLEN,GSPLFRST+GSPLSALL                                
*                                  LENGTH: CONTROL + DATA ELEMENTS              
         MVI   GNEWREC+DNEWCODE,X'01'                                           
*                                  SET ELEMENT CODE                             
         MVI   GNEWREC+DNEWELEN,GSPLSALL                                        
*                                  SET ELEMENT LENGTH                           
         MVC   GNEWREC+DNEWNAME(20),RSALNAME                                    
*                                  INSERT SALES PERSON NAME                     
         MVC   GNEWREC+DNEWOFFC(2),RSALOFF                                      
*                                  INSERT SALES PERSON OFFICE                   
         MVC   GNEWREC+DNEWTEAM(2),RSALTEAM                                     
*                                  INSERT SALES PERSON TEAM                     
         MVC   GNEWREC+DNEWFAX(12),RSALFAX                                      
*                                  INSERT SALES PERSON FAX #                    
         MVC   GNEWREC+DNEWMAIL(48),EMAIL+2                                     
*                                  INSERT SALES PERSON EMAIL                    
         OC    GNEWREC+DNEWMAIL(50),SPACES                                      
*                                                                               
         CLC   RSALPOWR(2),=C'00'  ORIGINAL VALUE?                              
         BE    NW710020            YES - DON'T USE                              
         CLC   RSALPOWR(2),SPACES  SPACE OR BINARY ZERO FILLED?                 
         BNH   NW710020            YES - DON'T USE                              
         MVC   GNEWREC+DNEWPOWR(2),RSALPOWR                                     
*                                  INSERT REDI POWER CODE                       
         TM    RSALPFLG,X'80'      POWER CODE A 'MINUS'?                        
         BNO   NW710020            NO                                           
         OI    GNEWREC+DNEWFLAG,X'10'                                           
*                                  TURN ON 'MINUS' FLAG                         
NW710020 EQU   *                                                                
         TM    RSALFLG,X'04'       EMAIL PREFERENCE SET?                        
         BNO   NW710040            NO                                           
         OI    GNEWREC+DNEWFLAG,X'40'                                           
*                                  TURN ON 'EMAIL' FLAG                         
         B     NW710060                                                         
NW710040 EQU   *                                                                
         TM    RSALFLG,X'02'       FAX   PREFERENCE SET?                        
         BNO   NW710060            NO                                           
         OI    GNEWREC+DNEWFLAG,X'20'                                           
*                                  TURN ON 'FAX  ' FLAG                         
NW710060 EQU   *                                                                
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
         XC    DMCB(4),DMCB        SET P1 TO ZERO                               
         CLC   GKEY(32),GKEYSAVE   FOUND?                                       
         BE    UCON0010            YES - REPLACE ORIGINAL W/THIS                
         MVI   DMCB+3,X'FF'        NO  - SET P1 TO NON-ZERO                     
*                                        MUST BE ADDED                          
UCON0010 EQU   *                                                                
         GOTO1 ADDSPSAL,DMCB,,                                                  
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
*                                                                               
*   FLAG FOR ADD/REWRITE IS NOT USED.  ALL UPDATES COME THROUGH THE             
*        ADDSPSAL LOGIC.  IF A RECORD IS PREVIOUSLY ON FILE, IT IS              
*        REWRITTEN RATHER THAN ADDED.                                           
*                                                                               
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
         XC    REC2(256),REC2                                                   
         MVC   REC2(GSPLFRST+GSPLSALL),GNEWREC  MOVE IN NEW RECORD              
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
         LA    R2,REC2                                                          
         ST    R2,AIOAREA                                                       
         OI    DMINBTS,X'80'       READ FOR UPDATE                              
         GOTO1 GGETREC             RETRIEVE THE RECORD                          
         MVC   REC2(GSPLFRST+GSPLSALL),GNEWREC  MOVE IN NEW RECORD              
         GOTO1 GPUTREC             REWRITE NEW VERSION OF REC                   
         LTR   RB,RB                                                            
ASPS0100 EQU   *                                                                
         LA    R2,REC              NO  - RESET A(IOAREA)                        
         ST    R2,AIOAREA                                                       
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
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
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'166RELFM02   08/08/06'                                      
         END                                                                    
