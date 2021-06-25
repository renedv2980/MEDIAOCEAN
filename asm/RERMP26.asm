*          DATA SET RERMP26    AT LEVEL 034 AS OF 04/13/09                      
*PHASE T81026C,*                                                                
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS'                       
*                                                                               
***********************************************************************         
*                                                                     *         
*- RERMP26 -- OVERNIGHT PROJECTION CREATION/REPORT                    *         
*                                                                     *         
*  MOD LOG:                                                           *         
*  --------                                                           *         
*                                                                     *         
*  11/07/89  PJS  DO NOT USE 'PJVAL' SWITCH FOR 2 THINGS (SAVING      *         
*                 PROJECTION VALUE AND PROJECTION FOUND SWITCH)       *         
*                                                                     *         
*                 PGM WAS NOT CREATING PROJECTIONS IF T.V. HOMES      *         
*                 VALUE WAS 0  (LAST VALUE SAVED IN PJVAL BY 'FORMDEM'*         
*                                                                     *         
*  OCT26/90 (MRR) --- CHANGE DBLOCK AND PRINTING FOR 1 DECIMAL        *         
*                                                                     *         
*  DEC14/90 (MRR) --- SWITCH COMPARE OF PROJECTION FROM HOMES TO      *         
*                      RTG HOMES                                      *         
*                                                                     *         
*  DEC10/92 (BU ) --- CHANGE X'5E' ELEMENT VALUE FROM '5202' TO '520A'*         
*                                                                     *         
*  DEC19/06 (BU ) --- CHANGE TABLE SIZE                               *         
*                                                                     *         
*  APR13/09 (KUI) --- SUPPORT NEW INVENTORY KEY                       *         
***********************************************************************         
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - INIT'                
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - GENERAL INITIALIZATION     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T81026   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81026**,RR=RE                                                 
*                                                                               
         L     RC,0(R1)            ESTABLISH GENCON WORKAREA                    
         USING GEND,RC                                                          
*                                                                               
         L     RA,ATWA             ESTABLISH SCREEN                             
         USING CONHEADH-64,RA                                                   
*                                                                               
         L     R8,ASPOOLD          ESTABLISH SPOOL WORKAREA                     
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD            ESTABLISH SYSTEM WORKAREA                    
         USING SYSD,R9                                                          
*                                                                               
         ST    RE,RELO26           SAVE RELOCATION FACTOR                       
*                                                                               
         L     R7,=A(SUBROUTS)     ESTABLLISH COMMON SUBROUTINES                
         A     R7,RELO26                                                        
         USING SUBROUTS,R7                                                      
*                                                                               
*        INIT PRINT LINES                                                       
*                                                                               
         LA    R0,9                MAX 9 PRINT LINES                            
         LA    R3,PL1              FIRST PRINT LINE                             
*                                                                               
         MVC   0(132,R3),SPACES    INIT PRINT LINES                             
         LA    R3,132(R3)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - PRMODE'              
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - DETERMINE MODE FOR PROGRAM *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRMODE   DS    0H                                                               
*                                                                               
         CLI   MODE,PRINTREP       ONLY ACCEPT PRTREP                           
         BE    PREP                                                             
*                                                                               
         XIT1                      PROGRAM EXIT                                 
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - PREP'                
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PREP     DS    0H                                                               
*                                                                               
         XC    LYVAL,LYVAL                                                      
         XC    PJVAL,PJVAL                                                      
*                                                                               
         LA    R2,HOOK             SET HEADLINE HOOK                            
         ST    R2,HEADHOOK                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,CMPTODAY)  COMPRESS TODAY'S DT         
*                                                                               
         MVI   RCSUBPRG,0          INNIT                                        
*                                                                               
         MVC   REP,AGENCY          SAVE REP ID                                  
*                                                                               
*        FIND PARENT REP ID IN REP RECORD                                       
*                                                                               
         LA    R4,KEY              GET PARENT REP FROM REP RECORD               
         USING RREPKEY,R4          ESTABLISH REPREC KEY                         
         XC    KEY,KEY                                                          
*                                                                               
         MVI   RREPKTYP,X'01'      RECORD ID                                    
         MVC   RREPKREP,REP        REP ID                                       
*                                                                               
         DROP  R4                                                               
*                                                                               
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'01'        FIND '01' ELEMENT                            
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
*                                                                               
         USING RREPELEM,R6         ESTABLISH REP MASTER ELEM                    
*                                                                               
         MVC   REP,RREPPAR         SWITCH TO PARENT REP                         
*                                                                               
         TITLE 'RERMP26 - T81026 - OVERNIGHT PROJECTIONS - GTPROF'              
********************************************************************            
*                                                                  *            
*        GTPROF --- GET AND SET SFM/REP PROFILES FROM THE REP REC  *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
GTPROF   DS    0H                                                               
*                                                                               
         XC    RMPPROF,RMPPROF     INIT RMP PROGRAM PROFILE                     
*                                                                               
         MVI   ELCODE,X'04'        SET TO FIND PROGRAM PROFILE ELM              
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         BNE   GTPROFX             NO PROFILE FOUND                             
*                                                                               
*- FIND RMP PROGRAM PROFILE WITHIN PROGRAM PROFILE ELEMENT                      
*                                                                               
         USING RREPPGMP,R6         ESTABLISH PROGRAM PROFILE ELEMENT            
*                                                                               
         ZIC   R0,RREPPGM#         NUMBER OF PROGRAM UNITS                      
         LA    RE,RREPPGM1         A(1ST UNIT)                                  
*                                                                               
         CLI   0(RE),RREPQRMP      LOOKING FOR RMP PROGRAM PROFILE              
         BE    *+16                                                             
         LA    RE,RREPPGML(RE)     NEXT UNIT                                    
         BCT   R0,*-12                                                          
         B     GTPROFX             NO MATCH                                     
*                                                                               
         MVC   RMPPROF,2(RE)       SAVE PROFILE                                 
*                                                                               
GTPROFX DS     0H                                                               
*                                                                               
         L     R2,=A(STACK)        RELOCATE ADCONS                              
         A     R2,RELO26                                                        
         ST    R2,ASTACK                                                        
*                                                                               
         L     R2,=A(PJBUFF)                                                    
         A     R2,RELO26                                                        
         ST    R2,APJBUFF                                                       
*                                                                               
         L     R2,=A(LYBUFF)                                                    
         A     R2,RELO26                                                        
         ST    R2,ALYBUFF                                                       
*                                                                               
         MVC   LASTBK,TO           SET UP LAST YEARS BOOK                       
         ZIC   R1,LASTBK+1                                                      
         BCTR  R1,0                BACK UP TO LAST YEAR                         
         STC   R1,LASTBK+1                                                      
*                                                                               
         EDIT  (R1),(2,LASTCD+3)                                                
*                                                                               
         MVC   LASTCD1,LASTCD+3    SAVE YEAR                                    
*                                                                               
         ZIC   R1,LASTBK+2                                                      
         BCTR  R1,0                                                             
         MHI   R1,3                INDEXING FACTOR                              
         LA    R1,MONCOD(R1)       PICK UP MONTH CODE                           
         MVC   LASTCD(3),0(R1)     PRINT MONTH                                  
*                                                                               
         ZIC   R1,LASTBK+2                                                      
         BCTR  R1,0                                                             
         LA    R1,MONCOD1(R1)       PICK UP MONTH CODE                          
         MVC   LASTCD1(1),0(R1)     PRINT MONTH                                 
*                                                                               
         L     R2,ACOMFACS         COMFACS ODDMENTS                             
         USING COMFACSD,R2                                                      
*                                                                               
         XC    DBLOCK,DBLOCK       INITIALIZE DBLOCK                            
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
*                                                                               
         XC    0(128,R1),0(R1)                                                  
         MVC   DBXTID(4),=C'SPOT'                                               
         MVI   DBXTTRP,X'01'      RTG = 1 DECIMAL                               
         MVI   DBXTTSP,X'01'      SHR = 1 DECIMAL                               
         MVI   DBXTTIP,X'02'      IMP = 00'S                                    
*                                                                               
         DROP  R1                                                               
*                                                                               
         MVC   DBFILE,=C'INV'                                                   
*                                                                               
         ST    R2,DBCOMFCS                                                      
*                                                                               
         TM    RMPPROF+RMPIMPSB,RMPIMPSA                                        
         BNO   *+8                                                              
         MVI   DBTAPEP,C'Y'           TURN ON FLAG                              
*                                                                               
         DROP  R2                                                               
*                                                                               
         LA    R2,IO                                                            
         ST    R2,DBAREC                                                        
*                                                                               
         LA    R2,34(R2)                                                        
         ST    R2,DBAQUART                                                      
*                                                                               
         XC    DMCB(12),DMCB       NEED A (DEMUP)                               
         MVC   DMCB+4(4),=X'D9000A08'                                           
         GOTO1 CALLOV,DMCB                                                      
         MVC   VDEMUP,DMCB                                                      
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - PRMAIN'              
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              MAIN PRINT LOOP                                        *         
*              REPEAT FOR EACH DAYPART                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRMAIN   DS    0H                                                               
*                                                                               
         LA    R3,STLIST           POINT TO LIST OF STATIONS                    
*                                                                               
PRPSTALP DS    0H                                                               
*                                                                               
         USING STLISTD,R3          ESTABLISH ENTRY IN STATION LIST              
*                                                                               
         ST    R3,ASTA             SET A(STATION)                               
*                                                                               
         OC    STLISTD(STLISTL),STLISTD CHECK FOR END OF LIST                   
         BZ    PRPSTADN                                                         
*                                                                               
         LA    R2,DPLIST           POINT TO DAYPART LIST                        
         LA    R4,L'DPLIST         MAX NUMBER OF DAYPARTS                       
*                                                                               
PRMDPTLP DS    0H                                                               
*                                                                               
         CLI   0(R2),0             DONE IF NO MORE DAYPARTS                     
         BE    PRMDPTDN                                                         
*                                                                               
         ST    R2,ADPT             SAVE LIST POINTER                            
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE NEW HEADLINES                          
         MVC   PAGE,=H'1'          FORCE NEW PAGE                               
*                                                                               
         GOTO1 =A(PRDPT),RR=RELO26                                              
*                                                                               
PRMDPTCN DS    0H                                                               
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT DAYPART                         
         BCT   R4,PRMDPTLP                                                      
*                                                                               
PRMDPTDN DS    0H                                                               
*                                                                               
*        CHECK IF A LOCK NEEDS TO BE RELEASED                                   
*                                                                               
         TM    WHEN,X'20'          SKIP IF NOT SOON                             
         BNO   PRLCKX                                                           
*                                                                               
         MVC   CSTAT,STLSSTAC      PASS STATION CALL LETTERS                    
*                                                                               
         GOTO1 CHKLOCK,DMCB,('LKUNLKQ',0)  UNLOCK STATION                       
*                                                                               
         BE    *+6                 TEST FOR ERRORS                              
         DC    H'0'                                                             
*                                                                               
PRLCKX   DS    0H                                                               
*                                                                               
PRPSTACN DS    0H                                                               
*                                                                               
         LA    R3,STLISTL(R3)      BUMP TO NEXT STATION IN LIST                 
         B     PRPSTALP                                                         
*                                                                               
PRPSTADN DS    0H                                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
         OC    ABOX(4),ABOX                                                     
         BZ    PRBOXX              ON-LINE, NO BOX TO SET                       
*                                                                               
         L     R4,ABOX             ESTABLISH BOX AREA                           
         USING BOXD,R4                                                          
*                                                                               
         CLI   BOXYORN,C'Y'        SKIP UNLESS USING BOXES                      
         BNE   PRBOXX                                                           
*                                                                               
         CLI   BOXSTAT,C'I'        SKIP UNLESS INSIDE A BOX                     
         BNE   PRBOXX                                                           
*                                                                               
         MVI   BOXREQ,C'C'         CLOSE ANY BOXES                              
         MVI   FORCEHED,C'N'       NO NEW PAGE AND HEADLINES                    
*                                                                               
         BAS   RE,SPLAT                                                         
*                                                                               
PRBOXX   DS    0H                                                               
*                                                                               
PRMAINX  CLI   TITDET,C'Y'         PRINT DETAILS OPTION                         
         BE    PRMDETX                                                          
*                                                                               
         MVI   RCSUBPRG,1          IF NO DETAILS PRINTED                        
*                                     PRINT END OF RUN MESSAGE                  
         MVC   PL1(132),SPACES     CLEAR LINE                                   
         MVC   PL1(55),=C'***** OVERNIGHT PROJECTION SUCCESSFULLY COMPLX        
               ETED *****'                                                      
         MVC   PL2(132),SPACES     CLEAR LINE                                   
         BAS   RE,SPLAT            PRINT MESSAGE                                
*                                                                               
PRMDETX  DS    0H                                                               
*                                                                               
XIT      DS    0H                                                               
         XIT1                      PROGRAM EXIT                                 
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - PRDPT'               
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              PRINT LOOP FOR A DAYPART                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRDPT    NTR1                      BUILD A STACK OF D/A                         
*                                                                               
*        BUILD STACK OF DISK ADDRESSES OF INVENTORY RECORDS                     
*        TO BE INCLUDED IN REPORT                                               
*                                                                               
         L     R5,ASTACK           POINT TO START OF STACK                      
         SR    R6,R6               INIT MEMBER COUNTER                          
*                                                                               
         LA    R4,KEY              ESTABLISH KEY AS INVREC DPT                  
         USING RIDPKEY,R4          PASSIVE POINTER                              
         XC    KEY,KEY                                                          
*                                                                               
         MVI   RIDPKTYP,RIDPKTYQ   RECORD ID                                    
         MVC   RIDPKREP,REP        REP ID                                       
*                                                                               
         L     R3,ASTA             POINT TO CURRENT STATION                     
         USING STLISTD,R3          ESTABLISH LIST ENTRY                         
*                                                                               
         MVC   RIDPKSTA,STLSSTAC   SET STATION                                  
         CLI   RIDPKSTA+4,C' '     BLANK MEANS TV                               
         BH    *+8                                                              
         MVI   RIDPKSTA+4,C'T'                                                  
*                                                                               
         L     RF,ADPT                                                          
         MVC   RIDPKDPT,0(RF)      CURRENT DAYPART                              
*                                                                               
         GOTO1 HIGH                READ FIRST POINTER                           
*                                                                               
PRDSTKLP DS    0H                                                               
*                                                                               
         CLC   KEYSAVE(RIDPKDAY-RIDPKEY),KEY DONE ON REP/STA/DPT CHANGE         
         BNE   PRDSTKDN                                                         
*                                                                               
*        FILTER ON INVENTORY NUMBERS                                            
*                                                                               
         LA    RF,INVLIST          POINT TO INVENTORY NO. FILTERS               
*                                                                               
         CLC   0(L'RIDPKINV,RF),SPACES  SKIP FILTER IF NONE                     
         BNH   PRDIVLOK                                                         
*                                                                               
PRDIVLLP DS    0H                                                               
*                                                                               
         CLC   0(L'RIDPKINV,RF),SPACES  CHECK FOR EOL                           
         BNH   PRDIVLDN                                                         
*                                                                               
         CLC   RIDPKINV,0(RF)      INVENTORY NUMBER MUST LIE IN RANGE           
         BL    PRDIVLCN                                                         
         CLC   RIDPKINV,L'RIDPKINV(RF)                                          
         BH    PRDIVLCN                                                         
*                                                                               
         B     PRDIVLOK                                                         
*                                                                               
PRDIVLCN DS    0H                                                               
*                                                                               
         LA    RF,2*L'RIDPKINV(RF) BUMP INVENTORY LIST POINTER                  
         B     PRDIVLLP                                                         
*                                                                               
PRDIVLDN DS    0H                                                               
         B     PRDSTKCN            FAILS INVENTORY LIST FILTER                  
*                                                                               
PRDIVLOK DS    0H                                                               
*                                                                               
PRDSTK20 DS    0H                                                               
*                                                                               
         CLC   RIDPKSTD,STRTOPT    DATES MUST LIE IN REQUEST PERIOD             
         BL    PRDSTKCN                                                         
         CLC   RIDPKSTD,ENDOPT                                                  
         BH    PRDSTKCN                                                         
*                                                                               
         CLI   RIDPKINV,C' '       MUST BE NEW INVENTORY RECORD                 
         BL    PRDSTKCN                                                         
*                                                                               
         MVC   0(4,R5),KEY+28      SAVE DISK ADDRESS                            
*                                                                               
         LA    R5,4(R5)            BUMP STACK POINTERS                          
         LA    R6,1(R6)                                                         
*                                                                               
PRDSTKCN DS    0H                                                               
*                                                                               
         GOTO1 SEQ                                                              
         B     PRDSTKLP                                                         
*                                                                               
PRDSTKDN DS    0H                                                               
*                                                                               
         LTR   R6,R6               DONE IF STACK IS EMPTY                       
         BZ    PRDPTX                                                           
*                                                                               
         STM   R5,R6,SAVESTAK      SAVE STACK POINTERS                          
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - PRDPT'               
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              READ INVENTORY RECORDS USING DISK ADDRESSES IN STACK   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         L     R5,ASTACK           POINT TO FIRST DISK ADDR IN STACK            
         L     R6,SAVESTAK+4       NUMBER OF ITEMS IN STACK                     
*                                                                               
PRDINVLP DS    0H                                                               
*                                                                               
         STM   R5,R6,SAVESTAK      SAVE STACK POINTERS                          
*                                                                               
         MVC   KEY+28(4),0(R5)     READ INV REC BY DISAK ADDRESS                
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   KEY(27),IO          COPY KEY OF READ RECORD                      
*                                                                               
         USING RINVKEY,R4          ESTABLISH AS REP INV REC KEY                 
*                                                                               
         MVI   ELCODE,X'01'        FIND INVENTORY ELEMENT                       
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
*                                                                               
         USING RINVPEL,R6          ESTABLISH INVENTORY ELEMENT                  
*                                                                               
         MVC   SVTIMCHG,RINVTCHG   SAVE TIME CHANGE FROM HEADER                 
         TM    RINVGPRO,X'40'      SKIP IF PROTECTED FROM CHANGE                
         BO    PRDINVCN                                                         
*                                                                               
*        SUPPRESS PRINT IN MULTIPLE DPTS                                        
*                                                                               
         CLI   DPLIST+1,0         NO CHANCE OF DUPLICATE IF ONLY 1 DPT          
         BE    PRINVDPD              REQUESTED                                  
*                                                                               
         GOTO1 =A(CHKDPT),RR=RELO26  CHECK DAYPART LISTS FOR DUPES              
         BNE   PRDINVCN              DROP INVENTORY RECORD                      
*                                                                               
PRINVDPD DS    0H                                                               
*                                                                               
*        TEST IF INVENTORY FILTER COMPATIBLE WITH ASKED FOR FILTER              
*                                                                               
         LA    R1,RINVPFLT         POINT TO INVENTORY FILTER                    
         LA    R5,TITFILT          ENTERED FILTER                               
         LA    R0,6                MAX 6 BYTES TO FILTER                        
*                                                                               
PRDIFLTL DS    0H                                                               
*                                                                               
         CLI   0(R5),C'A'          IF ANYTHING IS SPECIFIED                     
         BL    PRDIFLTC                                                         
         CLC   0(1,R5),0(R1)       IT MUST MATCH                                
         BNE   PRDINVCN               GO TO NEXT INVOICE RECORD                 
*                                                                               
PRDIFLTC DS    0H                                                               
*                                                                               
         LA    R1,1(R1)            BUMP FILTERS POINTERS                        
         LA    R5,1(R5)                                                         
         BCT   R0,PRDIFLTL                                                      
*                                  FILTERS ARE COMPATABLE                       
*                                                                               
*        DROP IF IT ENDS BEFORE TODAY                                           
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2  SKIP IF NO END DATE GIVEN              
         BZ    *+14                                                             
         CLC   RINVPEFF+2(2),CMPTODAY  ENDS BEFORE TODAY                        
         BL    PRDINVCN                                                         
*                                                                               
         GOTO1 =A(HEADER),RR=RELO26  FORMAT INVENTORY DETAILS                   
*                                                                               
         CLI   TITACT,C'Y'         Y = USE ACTUALS IF HIGHER                    
         BNE   PRDINV16                                                         
*                                                                               
         GOTO1 =A(LAST),RR=RELO26                                               
*                                                                               
PRDINV16 GOTO1 =A(PROJECT),RR=RELO26                                            
*                                                                               
         GOTO1 =A(CHOOSE),RR=RELO26                                             
*                                                                               
         CLI   TITDET,C'Y'         Y = PRINT DETAILS                            
         BNE   PRDINVCN                                                         
*                                                                               
         BAS   RE,SPLAT            PRINT LINES                                  
*                                                                               
PRDINVCN DS    0H                                                               
*                                                                               
         LM    R5,R6,SAVESTAK      RESTORE STACK POINTERS                       
         LA    R5,4(R5)            BUMP TO NEXT DISK ADDR IN STACK              
         BCT   R6,PRDINVLP                                                      
*                                                                               
PRDPTX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - HEADER'              
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              FORMAT HEADLINES FOR SPOOL                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HEADER   NTR1                                                                   
*                                                                               
         LA    R4,IO               ESTABLISH INVENTORY RECORD                   
         USING RINVKEY,R4                                                       
*                                                                               
*        PROGRAM DAY/TIMES                                                      
*                                                                               
         LA    R3,PL1              STARTING PRINT POSITION                      
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'02'        SET TO FIND A DAY/TIME ELEMENTS              
*                                                                               
         BAS   RE,GETEL            FIND FIRST DAY/TIME ELEMENT                  
         BNE   HDNWDTDN            NONE FOUND - SKIP PRINTING                   
*                                                                               
HDNWDTLP DS    0H                                                               
*                                                                               
         USING RIDTELEM,R6         ESTABLISH DAY/TIME ELEMENT                   
*                                                                               
         GOTO1 UNDAY,PARAS,RIDTDAY,1(R3)      DAY                               
*                                                                               
         GOTO1 UNTIME,PARAS,RIDTTIME,10(R3)    TIME                             
*                                                                               
HDNWDTCN DS    0H                                                               
*                                                                               
         LA    R3,132(R3)          BUMP TO NEXT LINE                            
         BAS   RE,NEXTEL           FIND NEXT ELEMENT                            
         BE    HDNWDTLP                                                         
*                                                                               
HDNWDTDN DS    0H                  END OF DAY-TIME ELEMENTS                     
*                                                                               
*                                                                               
*        PRINT PROGRAM NAMES                                                    
*                                                                               
         LA    R3,PL1              RESET LINE POINTER TO FIRST                  
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'03'        FIND A PROGRAM ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   HDNWPGX                                                          
*                                                                               
HDNWPGLP DS    0H                                                               
*                                                                               
         USING RIPGELEM,R6         ESTABLISH PROGRAM ELEMENT                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RIPGLEN          ELEMENT LENGTH                               
         SH    RF,=Y(RIPGNAME-RIPGELEM)  PROGRAM NAME LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   21(0,R3),RIPGNAME   PRINT PROGRAM NAME                           
*                                                                               
HDNWPGCN DS    0H                                                               
*                                                                               
         LA    R3,132(R3)          BUMP POINTER                                 
         BAS   RE,NEXTEL           NEXT PROGRAM ELEMENT                         
         BE    HDNWPGLP            ONE FOUND                                    
*                                                                               
HDNWPGDN DS    0H                                                               
*                                                                               
HDNWPGX  DS    0H                                                               
*                                                                               
         MVC   PL1+49(4),RINVKINV    PRINT INVENTORY NUMBER                     
*                                                                               
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(8,PL1+54) PRINT EFFECTIVE DATE         
*                                                                               
         XIT1                                                                   
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - LAST'                
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              CHECK LAST YEAR'S RECORD                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LAST     NTR1                                                                   
*                                                                               
         XC    LYVAL,LYVAL         INIT RETURNED VALUE                          
*                                                                               
         LA    R2,FROM             DEFAULT TO FROM BOOK                         
*                                                                               
         OC    CHKBOOK,CHKBOOK     IF REFERENCE BOOK ENTERED                    
         BZ    *+8                                                              
         LA    R2,CHKBOOK             USE IT                                    
*                                                                               
         LA    R4,KEY              ESTABLISH INVENTORY KEY                      
         USING RINVKEY,R4                                                       
*                                                                               
         MVC   RINVKRSR,TITSRCE    RATING SOURCE                                
         MVC   RINVKQLF,0(R2)      QUALIFIER                                    
         MVC   RINVKBTP,3(R2)      BOOKTYPE                                     
*                                                                               
         MVC   RINVKBK,LASTBK+1    TACK ON LAST YEARS BOOK                      
*                                                                               
         OC    CHKBOOK,CHKBOOK     IF REFERENCE BOOK ENTERED                    
         BZ    *+10                                                             
         MVC   RINVKBK,CHKBOOK+1      TACK ON ITS YEAR                          
*                                                                               
         GOTO1 HIGH                READ LAST YEAR'S INVENTORY RECORD            
*                                                                               
         CLC   KEYSAVE(27),KEY     DONE IF THERE WASN'T ONE                     
         MVC   KEY(27),KEYSAVE       RESTORE STARTING KEY                       
         BNE   LASTX                                                            
*                                                                               
         GOTO1 GETREC              READ RECORD                                  
*                                                                               
         LA    R4,IO               GOT A HIT LAST YEAR                          
         GOTO1 =A(GETHPT),RR=RELO26  MAKE SURE REC HAS OLD AND NEW HPT          
*                                                                               
         MVC   RINVKBK,TO+1        SET TO BOOK IN KEY                           
*                                                                               
         OC    CHKBOOK,CHKBOOK     IF REFERENCE BOOK ENTERED                    
         BZ    LAST10                                                           
*                                                                               
         EDIT  (1,CHKBOOK+1),(2,LASTCD+3)    YEAR                               
*                                                                               
         MVC   LASTCD1,LASTCD+3    SAVE YEAR                                    
*                                                                               
         ZIC   R1,CHKBOOK+2                                                     
         BCTR  R1,0                                                             
         MHI   R1,3                INDEXING FACTOR                              
         LA    R1,MONCOD(R1)       PICK UP MONTH CODE                           
         MVC   LASTCD(3),0(R1)     PRINT MONTH                                  
*                                                                               
         ZIC   R1,CHKBOOK+2                                                     
         BCTR  R1,0                                                             
         LA    R1,MONCOD1(R1)       PICK UP MONTH CODE                          
         MVC   LASTCD1(1),0(R1)     PRINT MONTH                                 
*                                                                               
LAST10   DS    0H                                                               
*                                                                               
         MVC   PL1+63(5),LASTCD      PRINT CODE                                 
*                                                                               
         LA    R3,PL1                                                           
         GOTO1 =A(FORMDEM),RR=RELO26    PRINT DEMOS                             
*                                                                               
         MVC   LYVAL,THISRTG                                                    
         MVC   THISCD,LASTCD                                                    
         MVC   THISCD1,LASTCD1                                                  
         GOTO1 =A(MAINTCOD),RR=RELO26                                           
*                                                                               
         MVC   RINVKRSR,TITSRCE    RATING SOURCE                                
         MVC   RINVKQLF,TO         QUALIFIER                                    
         MVC   RINVKBTP,TO+3       BOOKTYPE                                     
*                                                                               
LAST20   L     R5,ALYBUFF                                                       
         MOVE  ((R5),2000),IO      SAVE LAST YEAR'S BOOK                        
*                                                                               
LASTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - PROJECT'             
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              PROJECT DEMOS                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PROJECT  NTR1                                                                   
*                                                                               
         XC    PJVAL,PJVAL                                                      
         XC    FOUNDPJ,FOUNDPJ     ASSUME NO PROJECTION                         
*                                                                               
         LA    R4,KEY              ESTABLISH INVENTORY KEY                      
         USING RINVKEY,R4                                                       
*                                                                               
         MVC   RINVKRSR,TITSRCE    RATING SOURCE                                
         MVC   RINVKQLF,FROM       QUALIFIER                                    
         MVC   RINVKBTP,FROM+3     BOOKTYPE                                     
*                                                                               
PROJ5    MVC   RINVKBK,FROM+1      TRY AND READ FROM BOOK                       
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY                                                  
         MVC   KEY(27),KEYSAVE                                                  
         BNE   PROJX                                                            
*                                                                               
         MVI   FOUNDPJ,X'FF'       PROJECTION FOUND                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R4,IO                                                            
         GOTO1 =A(GETHPT),RR=RELO26 MAKE SURE REC HAS OLD AND NEW HPT           
*                                                                               
         MVC   RINVKRSR,TITSRCE    RATING SOURCE                                
         MVC   RINVKQLF,TO         QUALIFIER                                    
         MVC   RINVKBTP,TO+3       BOOKTYPE                                     
*                                                                               
         MVC   RINVKBK,TO+1                                                     
         MVC   THISCD,=C' PJ  '                                                 
         MVC   THISCD1,=C'PJ'                                                   
         MVC   PL2+63(5),=C' PJ  '                                              
******                                                                          
******   CLI   GSOQLF,C'E'         IF ESTIMATED BOOK                            
******   BNE   *+16                                                             
******   MVC   THISCD,=C'ES'                                                    
******   MVC   PL2+63(5),=C' ES  '                                              
*                                                                               
         LA    R3,PL2                                                           
         NI    RINVKREP+1,X'FF'-X'40'                                           
*                                  BECAUSE OF UT/TP PROBLEM                     
*                                                                               
         SR    R0,R0                                                            
*                                                                               
         TM    RMPPROF+RMPIMPSB,RMPIMPSA   IF DEMOS TO BE BASED ON IMPS         
         BNO   *+8                                                              
         ICM   R0,1,=C'I'               TURN ON FLAG                            
*                                                                               
         MVC   DEMODUB(4),=C'RI2=' PASS ID TO DEMUP                             
         MVC   DEMODUB+4(2),REP                                                 
         MVC   DEMODUB+6(1),SVTIMCHG                                            
*                                                                               
         GOTO1 VDEMUP,DMCB,(C'I',RINVPEL),((R0),UPEL),ACOMFACS,DEMODUB,X        
               0                                                                
         OI    RINVKREP+1,X'40'                                                 
*                                                                               
         GOTO1 =A(FORMDEM),RR=RELO26   PRINT DEMOS                              
*                                                                               
         MVC   PJVAL,THISRTG                                                    
*                                                                               
         GOTO1 =A(MAINTCOD),RR=RELO26                                           
*                                                                               
         L     R5,APJBUFF                                                       
         MOVE  ((R5),2000),IO      SAVE PROJECTED RECORD                        
*                                                                               
PROJX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - GETHPT'              
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*         ROUTINE TO GET OLD HPT'S AND DEAL WITH UNCONVERTED RECS     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETHPT   NTR1                                                                   
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'5E'        LOOK FOR A BOOK ELEMENT                      
         BAS   RE,GETEL                                                         
         ST    R6,ABOOKEL          SAVE ELEMENT ADDRESS                         
         BE    GETHPT2             FOUND AN ELEMENT                             
*                                                                               
         USING RIBELEM,R6          ELSE BUILD ONE IN RECORD                     
*                                                                               
         LA    R6,DUB                                                           
         XC    DUB,DUB             CLEAR ELEMENT AREA                           
         MVC   RIBELEM(2),=X'5E07' FORCE AN ELEMENT IN                          
         MVC   RIBFILE(3),=C'PTN'                                               
         MVC   RIBBOOK,=X'520A'                                                 
         GOTO1 VRECUP,DMCB,(2,(R4)),DUB,ABOOKEL                                 
*                                                                               
GETHPT2  L     R6,ABOOKEL          POINT TO BOOK ELEMENT                        
*                                                                               
         MVI   IUNSW,C'Y'                                                       
*                                                                               
         CLC   RIBFILE(3),=C'IUN'                                               
         BE    *+8                                                              
         MVI   IUNSW,C'N'          NOT IN IUN FORMAT                            
*                                                                               
         XC    DEMODUB,DEMODUB     CLEAR EXTRA STORAGE FOR DEMUP                
         XC    TOTSHR(12),TOTSHR                                                
*                                                                               
GETHPT4  XC    WORK,WORK           BUILD DUMMY INDEX FF UPGRADE EL              
         LA    RE,WORK                                                          
         USING RAVLNEL,RE                                                       
*                                                                               
         MVI   RAVLNCOD,X'05'                                                   
         MVI   RAVLNLEN,14                                                      
         MVI   RAVLNTYP,4                                                       
         MVI   RAVLNCAT,C'I'       INVENTORY TO INVENTORY TRANSFER              
         MVC   RAVLNOP1,=X'FFFF'                                                
         MVC   RAVLNOP2,=H'1'      SET WEIGHTING TO ONE                         
*                                                                               
         NI    11(R4),X'FF'-X'40'                                               
*                                  BECAUSE OF UT/TP PROBLEM                     
         SR    R0,R0                                                            
*                                                                               
         TM    RMPPROF+RMPIMPSB,RMPIMPSA   IF DEMOS TO BE BASED ON IMPS         
         BNO   *+8                                                              
         ICM   R0,1,=C'I'               TURN ON FLAG                            
*                                                                               
         MVC   DEMODUB(4),=C'RI2=' PASS ID TO DEMUP                             
         MVC   DEMODUB+4(2),REP                                                 
         MVC   DEMODUB+6(1),SVTIMCHG                                            
*                                                                               
         GOTO1 VDEMUP,DMCB,34(R4),((R0),WORK),ACOMFACS,DEMODUB,TOTSHR           
         OI    11(R4),X'40'                                                     
*                                                                               
         CLI   IUNSW,C'Y'          TEST FOR ORIGINAL REC IN IUN FORMAT          
         BE    GETHPTX                                                          
*                                                                               
GETHPT6  XC    WORK,WORK           ADD NEW HPT'S TO NON-IUN RECORD              
         LA    RE,WORK             BY MEANS OF INDEX 100 UPGRADE                
         MVC   RAVLNCOD(2),=X'050E'   BUILD DUMMY UPGRADE ELEMENT               
         MVI   RAVLNTYP,4                                                       
         MVC   RAVLNOP1,=H'100'                                                 
*                                                                               
         MVI   ELCODE,X'05'                                                     
         LR    R6,R4                                                            
         BAS   RE,GETEL                                                         
*                                                                               
         GOTO1 VRECUP,DMCB,(2,(R4)),WORK,(R6)                                   
*                                                                               
         NI    11(R4),X'FF'-X'40'  BECAUSE OF UT/TP PROBLEM                     
*                                                                               
         SR    R0,R0                                                            
*                                                                               
         TM    RMPPROF+RMPIMPSB,RMPIMPSA   IF DEMOS TO BE BASED ON IMPS         
         BNO   *+8                                                              
         ICM   R0,1,=C'I'               TURN ON FLAG                            
*                                                                               
         MVC   DEMODUB(4),=C'RI2=' PASS ID TO DEMUP                             
         MVC   DEMODUB+4(2),REP                                                 
         MVC   DEMODUB+6(1),SVTIMCHG                                            
*                                                                               
         GOTO1 VDEMUP,DMCB,(C'I',34(R4)),((R0),WORK),ACOMFACS,DEMODUB           
         OI    11(R4),X'40'                                                     
*                                                                               
GETHPTX  DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R6,RE                                                            
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - CHOOSE'              
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              ROUTINE TO CHOOSE ONE OF THE RECORDS                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CHOOSE   NTR1                                                                   
*                                                                               
         L     R5,ALYBUFF          SELECT RECORD WITH HIGHER HOMES              
*                                                                               
         CLC   LYVAL,PJVAL                                                      
         BH    CH2                                                              
*                                                                               
         L     R5,APJBUFF                                                       
         MVC   PL1+63(69),PL2+63   IF PJ WINS, PRINT PJ LINE                    
*                                                                               
*- EXIT WITHOUT UPDATE IF NO PROJECTION FOUND (IN 'PROJECT' RTN)                
*                                                                               
         OC    FOUNDPJ,FOUNDPJ     NO HITS - NOT TOO INTERESTED                 
         BZ    CHOOSEX                                                          
*                                                                               
CH2      MVC   PL2,SPACES          ONLY PRINT 1 LINE, NOT PJ AND ACTUAL         
*                                                                               
         TM    RMPPROF+RMPR_41B,RMPR_41A                                        
*                                  CHANGE IDENTITY FOR (P/I)?                   
         BNO   CH2A0060            NO                                           
*                                                                               
         CLI   RINVKRSR-RINVREC(R5),C'N'                                        
         BNE   CH2A0060                                                         
         LA    RF,KSRCTAB                                                       
CH2A0020 EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    CH2A0060            YES                                          
         CLC   0(2,RF),RINVKQLF-RINVREC(R5)                                     
*                                  TABLE ENTRY = RECORD SOURCE?                 
         BNE   CH2A0040            NO  - BUMP TO NEXT TABLE ENTRY               
         MVC   RINVKQLF-RINVREC(2,R5),2(RF)                                     
*                                  YES - CHANGE RECORD IDENTITY                 
         B     CH2A0060            FINISHED                                     
CH2A0040 EQU   *                                                                
         LA    RF,4(RF)                                                         
         B     CH2A0020            GO BACK FOR NEXT                             
KSRCTAB  EQU   *                                                                
*                                  PEOPLE METER: REGULAR                        
*                                  NSI PROJECTED                                
         DC    X'44',C'P',X'44',X'00'                                           
*                                  NSI ESTIMATED                                
         DC    X'60',C'P',X'60',X'00'                                           
*                                  PEOPLE METER: HISPANIC                       
*                                  NSI PROJECTED                                
         DC    X'44',C'I',X'44',C'H'                                            
*                                  NSI ESTIMATED                                
         DC    X'60',C'I',X'60',C'H'                                            
         DC    X'00'                                                            
*                                                                               
*   ONLY TRANSLATE / CONVERT PROJECTED / ESTIMATED CODES                        
*                                                                               
         DS    0H                                                               
CH2A0060 EQU   *                                                                
         MVC   KEY,0(R5)           MOVE IN KEY OF NEW RECORD                    
*                                                                               
CH3      DS    0H                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETED RECORDS                  
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                                                               
         CLC   KEY(27),KEYSAVE     IS IT THERE ALREADY                          
         MVC   KEY(27),KEYSAVE                                                  
         BE    CH4                 YES-GO TO CHANGE CODE                        
*                                                                               
         MOVE  (IO,2000),(R5)                                                   
*                                                                               
CH3C     LA    R4,IO                                                            
         LR    R6,R4                                                            
         MVI   ELCODE,X'EF'        IS THERE AN ACTIVITY ELEMENT YET             
         BAS   RE,GETEL                                                         
         BNE   CH3G                                                             
*                                                                               
         GOTO1 VRECUP,DMCB,(2,(R4)),(R6),0     YES - JUNK IT                    
*                                                                               
CH3G     XC    WORK,WORK           ADD AN ACTIVITY ELEMENT                      
         LA    RE,WORK                                                          
         USING RINVAEL,RE                                                       
*                                                                               
         MVC   RINVACOD(2),=X'EF0C'                                             
         MVC   RINVAFST,BTODAY     TODAY'S DATE - BINARY                        
         MVC   RINVALST,BTODAY                                                  
         MVI   RINVAWHY,C'A'                                                    
*                                                                               
         DROP  RE                                                               
*                                                                               
         GOTO1 VRECUP,DMCB,(2,(R4)),WORK,(R6)                                   
*                                                                               
CH3L     EQU   *                                                                
         GOTO1 ADDREC              NO - ADD NEW ONE                             
*&&DO                                                                           
         MVC   P+1(03),=C'ADDREC'                                               
         GOTO1 HEXOUT,DMCB,IO,P+11,27,=C'TOG'                                   
         GOTO1 SPOOL,PARAS,(R8)   PRINT THE DATA                                
*&&                                                                             
*                                                                               
         B     CHOOSEX                                                          
*                                                                               
CH4      DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETED RECORDS                  
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         NI    DMINBTS,X'FF'-X'08' RESET                                        
*                                                                               
         LA    R6,IO                                                            
         MOVE  (IO,2000),(R5)                                                   
*                                                                               
         LA    R4,IO                                                            
         LR    R6,R4                                                            
*                                                                               
         MVI   ELCODE,X'EF'        UPDATE ACTIVITY ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   CH6                                                              
*                                                                               
         LR    RE,R6               ESTABLISH ADDRESSABILITY TO ELEMENT          
         USING RINVAEL,RE                                                       
         MVC   RINVALST,BTODAY                                                  
         MVI   RINVAWHY,C'C'                                                    
*                                                                               
CH6      DS    0H                                                               
*                                                                               
*&&DO                                                                           
         MVC   P+1(06),=C'PUTREC'                                               
         TM    RMPPROF+RMPR_41B,RMPR_41A                                        
*                                  CHANGE IDENTITY?                             
         BNO   TEST0020            NO                                           
         MVC   P+8(3),=C'YES'                                                   
TEST0020 EQU   *                                                                
         GOTO1 HEXOUT,DMCB,KEY,P+11,27,=C'TOG'                                  
         GOTO1 SPOOL,PARAS,(R8)   PRINT THE DATA                                
*&&                                                                             
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
         TM    KEY+27,X'80'        IF RECORD WAS DELETED                        
         BNO   CH7                                                              
*                                                                               
         NI    KEY+27,X'FF'-X'80'     UNDELETE                                  
         GOTO1 WRITE                                                            
*                                                                               
CH7      DS    0H                                                               
*                                                                               
CHOOSEX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RE                                                               
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - MAINTCOD'            
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              ROUTINE TO MAINTAIN CODE & UPGRADE ELEMENTS            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MAINTCOD NTR1                                                                   
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'CD'        IS THERE A CD ELEMENT YET                    
         BAS   RE,GETEL                                                         
         BNE   MC2                                                              
*                                                                               
         GOTO1 VRECUP,DMCB,(2,IO),(R6),0     YES - JUNK IT                      
*                                                                               
MC2      LA    R2,WORK             BUILD A NEW ELEMENT IN WORK                  
         XC    WORK,WORK                                                        
         USING RINVCEL,R2                                                       
*                                                                               
         MVI   RINVCCOD,X'CD'                                                   
         MVI   RINVCLEN,10                                                      
         MVC   RINVCODE,THISCD1     FOR 'PJ'                                    
         OI    RINVCTYP,X'40'       PRODUCED BY OP                              
*                                                                               
         TM    TO,X'20'            IS 'TO BOOK' ESTIMATED                       
         BNO   *+8                                                              
         MVI   RINVCSET,C'E'       MOVE IN 'E'                                  
*                                                                               
         TM    TO,X'04'            IS 'TO BOOK' PROJECTED                       
         BNO   *+8                                                              
         MVI   RINVCSET,C'P'       MOVE IN 'P'                                  
*                                                                               
         TM    TO,X'02'            IS 'TO BOOK' SPECIAL SURVEY                  
         BNO   *+8                                                              
         MVI   RINVCSET,C'S'       MOVE IN 'S'                                  
*                                                                               
         GOTO1 VRECUP,DMCB,(2,IO),(R2),(R6)                                     
*                                                                               
         LA    R6,IO               LOOK FOR UPGRADE ELEMENT                     
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   MC4                                                              
*                                                                               
         GOTO1 VRECUP,DMCB,(2,IO),(R6),0     AND DELETE                         
*                                                                               
MC4      CLC   THISCD1,=C'PJ'     ADD AN UPGRADE ELM FOR PROJECTION             
******   BE    *+10                                                             
******   CLC   THISCD,=C'ES'       OR ESTIMATE                                  
         BNE   MAINTX                                                           
*                                                                               
         GOTO1 VRECUP,DMCB,(2,IO),UPEL,(R6)                                     
*                                                                               
MAINTX   DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - HOOK'                
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              HEADLINE ROUTINES                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         EJECT                                                                  
HOOK     NTR1  BASE=*,LABEL=*                                                   
****     L     RE,4(RD)                                                         
****     CLC   0(4,RE),=C'SPUL'                                                 
****     BE    *+12                                                             
****     L     RE,4(RE)                                                         
****     B     *-14                                                             
****     LM    RE,RC,12(RE)                                                     
****                                                                            
         L     R3,ASTA             POINT TO CURRENT STATION                     
         USING STLISTD,R3          ESTABLISH STATION LIST ENTRY                 
*                                                                               
         MVC   H4+10(4),STLSSTAC     STATION CALL LETTERS                       
*                                                                               
         CLI   STLSSTAC+4,C'T'       PRINT BAND IF NOT TV                       
         BE    HOOKBNDX                                                         
*                                                                               
         CLI   STLSSTAC+4,C' '       PRINT BAND IF NOT TV                       
         BNH   HOOKBNDX                                                         
*                                                                               
         LA    RF,H4+10+3          POINT TO LAST CALL LETTER                    
*                                                                               
         CLI   0(RF),C' '          BACK UP IF BLANK                             
         BH    *+6                                                              
         BCTR  RF,0                                                             
*                                                                               
         MVI   1(RF),C'-'          PRINT BAND                                   
*                                                                               
         MVC   2(1,RF),STLSSTAC+4                                               
*                                                                               
         CLI   2(RF),C'A'          IF AM/FM PRINT MODULATION                    
         BE    *+8                                                              
         CLI   2(RF),C'F'                                                       
         BNE   *+8                                                              
         MVI   3(RF),C'M'                                                       
*                                                                               
HOOKBNDX DS    0H                                                               
*                                                                               
         MVC   H4+19(24),TITMKT                                                 
*                                                                               
         CLI   TITDET,C'Y'         IF NO DETAILS, SKIP DAYPART                  
         BNE   HOOK4E                                                           
*                                                                               
         LA    R3,DPTBL            LOOK UP DAYPART                              
         USING DPTBLD,R3                                                        
*                                                                               
         L     R2,ADPT                                                          
*                                                                               
         MVC   DPBYTE,0(R2)                                                     
*                                                                               
HOOK2    CLC   DPBYTE,DPTBCODE     MATCH TO DAYPART CODE                        
         BE    HOOK4                                                            
*                                                                               
         LA    R3,DPTBLL(R3)                                                    
*                                                                               
         CLI   0(R3),X'00'         TEST EOT                                     
         BNE   HOOK2                                                            
*                                                                               
         B     *+10                NO MATCH                                     
*                                                                               
HOOK4    MVC   H4+54(15),DPTBLNAM  TRANSLATE DAYPART                            
*                                                                               
HOOK4E   MVC   H4+74(3),TITSRCE                                                 
*                                                                               
         LA    R2,FROM                                                          
         LA    R3,H4+83                                                         
*                                                                               
         BAS   RE,BOUT                                                          
*                                                                               
         CLI   8(R3),C' '                                                       
         BE    HOOK5                                                            
*                                                                               
         MVC   9(4,R3),=C' TO '                                                 
         LA    R3,1(R3)                                                         
*                                                                               
HOOK5    LA    R2,TO                                                            
         LA    R3,12(R3)                                                        
*                                                                               
         BAS   RE,BOUT                                                          
*                                                                               
         CLI   TITUPH+5,0                                                       
         BE    HOOK6                                                            
*                                                                               
         MVC   H5+74(7),=C'UPGRADE'                                             
         MVC   H5+82(25),TITUP                                                  
*                                                                               
HOOK6    CLI   TITDET,C'Y'         IF NO DETAILS, SKIP BOXES                    
         BNE   HOOKX                                                            
*                                                                               
         ICM   R4,15,ABOX             INITIALIZE BOXES                          
         BZ    HOOK7                                                            
*                                                                               
         USING BOXD,R4                                                          
*                                                                               
         MVI   BOXOFF,0                                                         
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+009,C'C'    DAY(S)                                       
         MVI   BOXCOLS+020,C'C'    TIME(S)                                      
         MVI   BOXCOLS+048,C'C'    PROGRAMMING                                  
         MVI   BOXCOLS+053,C'C'    INV NU                                       
         MVI   BOXCOLS+062,C'C'    START DATE                                   
         MVI   BOXCOLS+068,C'C'    CODE                                         
         MVI   BOXCOLS+075,C'C'    HUT/PUT                                      
         MVI   BOXCOLS+082,C'C'    SHR                                          
         MVI   BOXCOLS+089,C'C'    RTG                                          
         MVI   BOXCOLS+096,C'C'    W18+                                         
         MVI   BOXCOLS+103,C'C'    M18+                                         
         MVI   BOXCOLS+110,C'R'    IMPS                                         
         MVI   BOXROWS+06,C'T'                                                  
         MVI   BOXROWS+09,C'M'                                                  
         MVI   BOXROWS+56,C'B'                                                  
*                                                                               
HOOK7    EQU   *                                                                
*                                                                               
         OC    CHKDEMO,CHKDEMO     IF CHANGING REFERENCE DEMO                   
         BZ    HOOK20                                                           
*                                                                               
         MVC   H9+69(6),=CL6'  PUT'                                             
         MVC   H9+76(6),=CL6'  SHR'                                             
         MVC   H9+83(6),=CL6'  RTG'                                             
         MVC   H9+104(6),=CL6' IMPS'                                            
*                                                                               
         LA    R2,CHKDEMO                                                       
*                                                                               
         XC    DBLOCKW,DBLOCKW     INIT DBLOCK WORKAREA                         
         LA    RF,DBLOCKW          ESTABLISH DBLOCK                             
         USING DBLOCK,RF                                                        
*                                                                               
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
*                                                                               
         CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 DEMOCON,PARAS,(1,(R2)),(5,WORK),(0,DBLOCKW)                      
*                                                                               
         CLI   1(R2),C'I'          RESET FOR NEXT TIME                          
         BNE   *+8                                                              
         MVI   1(R2),C'T'                                                       
*                                                                               
         MVC   H8+70(5),WORK                                                    
         MVC   H8+77(5),WORK                                                    
         MVC   H8+84(5),WORK                                                    
         MVC   H8+105(5),WORK                                                   
*                                                                               
HOOK20   DS    0H                                                               
*                                                                               
HOOKX    EQU   *                                                                
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - SUBROUTS'            
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              COMMON SUBROUTINES                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SUBROUTS DS    0D                  COMMON SUBROUTINES                           
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - SPLAT'               
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              PRINT A CHUNK OF LINES                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPLAT    NTR1                                                                   
*                                                                               
*        PRINT DETAIL LINES - THERE COULD BE 8                                  
*                                                                               
         LA    R3,PL1              START OF PRINT LINES                         
*                                                                               
SPLTPR1L DS    0H                                                               
*                                                                               
         LA    R2,P                START OF PRINT AREA (4 LINES WORTH)          
         LA    R0,4                MAX 4 LINES AT A TIME                        
*                                                                               
SPLTPR2L DS    0H                                                               
*                                                                               
         MVC   0(132,R2),0(R3)                                                  
         MVC   0(132,R3),SPACES                                                 
*                                                                               
SPLTPR2C DS    0H                                                               
*                                                                               
         LA    R2,132(R2)                                                       
         LA    R3,132(R3)                                                       
         BCT   R0,SPLTPR2L                                                      
*                                                                               
SPLTPR2D DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,PARAS,(R8)   PRINT THE DATA                                
*                                                                               
SPLTPR1C DS    0H                                                               
*                                                                               
         CLC   0(132,R3),SPACES    DONE IF ONLY SPACES TO PRINT                 
         BH    SPLTPR1L            THERE IS EXTRA LINE TO STOP LOOP             
*                                                                               
         GOTO1 SPOOL,PARAS,(R8)   SPACING LINE                                  
*                                                                               
SPLATX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - BOUT'                
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              PRINT BOOK                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BOUT     NTR1                                                                   
*                                                                               
         LA    R1,SVCLST                                                        
*                                                                               
BOUT4    CLC   0(1,R2),3(R1)       CONVERT BOOKVAL TO PRINTABLE PREFIX          
         BE    BOUT7                                                            
*                                                                               
         LA    R1,L'SVCLST(R1)                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   BOUT4                                                            
         DC    H'0'                                                             
*                                                                               
BOUT7    CLI   1(R1),C' '                                                       
         BE    BOUT10                                                           
*                                                                               
         MVC   0(1,R3),1(R1)                                                    
         LA    R3,1(R3)                                                         
*                                                                               
BOUT10   SR    R1,R1                                                            
*                                                                               
         ICM   R1,1,2(R2)                                                       
         BNZ   BOUT15              HAVE A MONTH                                 
*                                                                               
         MVC   0(3,R3),=C'ST/'     'E' OF 'EST' ALREADY THERE                   
*                                                                               
         B     BOUT20                                                           
*                                                                               
BOUT15   DS    0H                                                               
*                                                                               
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   0(3,R3),0(R1)                                                    
*                                                                               
BOUT20   DS    0H                                                               
*                                                                               
         EDIT  (1,1(R2)),(2,3(R3))                                              
         LA    R3,5(R3)                                                         
*                                                                               
         CLI   3(R2),C' '           PRINT BOOK TYPE IF PRESENT                  
         BE    BOUTX                                                            
         CLI   3(R2),0              IF THERE IS A BOOKTYPE,                     
         BE    BOUTX                                                            
         GOTO1 GETBTYPE,DMCB,(3(R2),0)                                          
         CLI   DMCB,0                                                           
         BE    BOUTX                                                            
                                                                                
         MVI   0(R3),C'('                                                       
                                                                                
         ZIC   R1,DMCB                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),DMCB+2                                                   
                                                                                
         LA    R3,2(R1,R3)                                                      
         MVI   0(R3),C')'                                                       
*                                                                               
BOUTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - FORMDEM'             
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              ROUTINE TO FORMAT DEMO LINE                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FORMDEM  NTR1                                                                   
*                                                                               
         MVC   WDEMLIST,DEMLIST    COPY DEMLIST TO WORKING STORAGE              
*                                                                               
         LA    R2,WDEMLIST         POINT TO WORK COPY OF LIST                   
*                                                                               
         OC    CHKDEMO,CHKDEMO     IF REFERENCE DEMO ENTERERED                  
         BZ    FRDMDEMX                                                         
*                                                                               
         MVC   2(1,R2),CHKDEMO+2      REPLACE HH IN LIST                        
         MVC   5(1,R2),CHKDEMO+2      REPLACE HH IN LIST                        
         MVC   8(1,R2),CHKDEMO+2      REPLACE HH IN LIST                        
         MVC   17(1,R2),CHKDEMO+2     REPLACE HH IN LIST                        
*                                                                               
FRDMDEMX DS    0H                                                               
*                                                                               
         LA    R3,69(R3)           R3=A(PRINT LINE)                             
         LA    R4,6                                                             
*                                                                               
         XC    DEMAREA,DEMAREA                                                  
         LA    R6,DEMAREA          OUTPUT VALUES RETURNED TO DEMAREA            
*                                                                               
         GOTO1 DEMOUT,DMCB,(C'L',(R2)),DBLOCK,DEMAREA                           
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   R1,15,8(R6)         GET RTG                                      
*                                                                               
         CLI   CHKDEMO+1,C'S'      IF CHECKING ON SHARE                         
         BNE   *+8                                                              
         ICM   R1,15,4(R6)            GET SHARE                                 
*                                                                               
         B     *+6                                                              
         DC    H'0'                TESTING                                      
*                                                                               
         ST    R1,THISRTG         AND STORE IT                                  
         L     R1,20(R6)          GET HOMES                                     
         ST    R1,THISHMS         AND STORE IT                                  
*                                                                               
FORMDEM2 EQU   *                                                                
*                                                                               
         L     R1,0(R6)                                                         
         EDIT  (R1),(6,(R3)),1                                                  
         LA    R2,3(R2)                                                         
         LA    R3,7(R3)                                                         
         LA    R6,4(R6)            POINT TO NEXT OUTPUT VALUE                   
         BCT   R4,FORMDEM2                                                      
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - GETEL'               
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              FIND AN ELEMENT IN A RECORD                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         GETEL (R6),34,ELCODE      FIND AN ELEMENT IN A RECORD                  
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - TABLES'              
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              TABLES                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
*        MONTH CODE TABLE                                                       
*                                                                               
MONCOD   DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
MONCOD1  DC    C'JF3AM6Y8SOND'                                                  
*                                                                               
*        DEMO LIST TABLE                                                        
*                                                                               
****DEMLIST  DC    X'00',C'P',AL1(01)  HUT                                      
****         DC    X'00',C'S',AL1(01)  SHR                                      
****         DC    X'00',C'R',AL1(02)  MET                                      
****         DC    X'00',C'R',AL1(01)  RTG                                      
****         DC    X'00',C'R',AL1(45)  W18+                                     
****         DC    X'00',C'R',AL1(42)  W1849                                    
****         DC    X'00',C'R',AL1(41)  W1834                                    
****         DC    X'00',C'R',AL1(47)  W2549                                    
****         DC    X'00',C'R',AL1(48)  W2554                                    
****         DC    X'00',C'R',AL1(95)  M18+                                     
****         DC    X'00',C'R',AL1(92)  M1849                                    
****         DC    X'00',C'T',AL1(01)  HOMES                                    
*                                                                               
DEMLIST  DC    X'00',C'P',AL1(01)  HUT                                          
         DC    X'00',C'S',AL1(01)  SHR                                          
         DC    X'00',C'R',AL1(01)  RTG                                          
         DC    X'00',C'R',AL1(45)  W18+                                         
         DC    X'00',C'R',AL1(95)  M18+                                         
         DC    X'00',C'T',AL1(01)  HOMES                                        
         DC    X'FF'                                                            
         DC    XL32'00'            FILLER FOR TABLE                             
*                                                                               
*                                                                               
       ++INCLUDE RESVCTAB                                                       
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         TITLE 'RERMP26 - T81026 - OVERNIGHT PROJECTIONS - CHKDPT'              
*******************************************************************             
*                                                                 *             
*              COMPARE REQUESTED DAYPARTS TO INVENTORY RECORDS    *             
*              DROP INVENTORY IF PRIMARY DPT IS ONE OF THOSE      *             
*                REQUESTED BUT NOT THIS ONE                       *             
*                (IT WILL BE PROCESSED WHEN ITS PRIMARY DPT COMES *             
*                UP                                               *             
*              PROCESS INVENTORY THE FIRST TIME A NON-PRIMARY DPT *             
*                IS ENCOUNTERED                                   *             
*                                                                 *             
*                                                                 *             
*NTRY    R6==> RINVPEL                                            *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
         DS    0H                                                               
CHKDPT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RINVPEL,R6          ESTABLISH PROGRAM ELEMENT                    
*                                                                               
         L     R2,ADPT             POINT TO CURRENT DAYPART                     
         CLC   0(1,R2),RINVDP      OKAY IF CURRENT MATCHES PRIMARY              
         BE    CHKDPTOK                                                         
*                                                                               
*        DROP RECORD IF PRIMARY DAYPART IS AMONG THOSE REQUESTED                
*                                                                               
         LA    RE,DPLIST           REQUESTED DAYPARTS                           
         LA    RF,L'DPLIST                                                      
*                                                                               
         CLI   0(RE),C' '          DONE AT END OF LIST                          
         BNH   *+22                                                             
         CLC   0(1,RE),RINVDP      MATCH LIST TO PRIMARY DPT                    
         BE    CHKDPTEX            DROP IF MATCH FOUND                          
         LA    RE,1(RE)            BUMP LIST POINTER                            
         BCT   RF,*-22                                                          
*                                                                               
*        OKAY IF REQUESTED DPTS MATCH ONLY ONE IN RECORD                        
*                                                                               
         USING RINVPEL,R6          ESTABLISH PROGRAM ELEMENT                    
*                                                                               
         LA    RE,DPLIST           REQUESTED DAYPARTS                           
         LA    RF,L'DPLIST                                                      
*                                                                               
CHKDPT1L DS    0H                                                               
*                                                                               
         CLI   0(RE),C' '          DONE AT END OF LIST                          
         BNH   CHKDPT1D                                                         
*                                                                               
         LA    R1,RINVDP+1         INVENTORY DAYPARTS (FIRST ALREADY            
         LA    R0,L'RINVDP-1          CHECKED)                                  
*                                                                               
CHKDPT2L DS    0H                                                               
*                                                                               
         CLI   0(R1),C' '          CHECK FOR END OF LIST                        
         BNH   CHKDPT2D                                                         
*                                                                               
         CLC   0(1,RE),0(R1)       SKIP IF DAYPARTS DON'T MATCH                 
         BNE   CHKDPT2C                                                         
*                                                                               
         CR    RE,R2               IF MATCH BEFORE CURRENT DPT                  
         BL    CHKDPTEX               DROP INVENTORY AS DUPLICATE               
*                                                                               
         B     CHKDPTOK            ELSE PROCESS INVENTORY RECORD                
*                                                                               
CHKDPT2C DS    0H                                                               
*                                                                               
         LA    R1,1(R1)            BUMP LIST POINTER                            
         BCT   R0,CHKDPT2L                                                      
*                                                                               
CHKDPT2D DS    0H                                                               
*                                                                               
CHKDPT1C DS    0H                                                               
*                                                                               
         LA    RE,1(RE)            BUMP LIST POINTER                            
         BCT   RF,CHKDPT1L                                                      
*                                                                               
CHKDPT1D DS    0H                                                               
*                                                                               
CHKDPTOK DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     CHKDPTX                                                          
*                                                                               
CHKDPTEX DS    0H                                                               
         LTR   RB,RB               SET NE CC                                    
*                                                                               
CHKDPTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - WORKAREAS'           
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              WORKAREAS                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
       ++INCLUDE DEDBLOCK                                                       
         SPACE 2                                                                
DBEXTRA1 DS    CL128                                                            
         EJECT                                                                  
PL1      DS    CL132               PRINT WORKAREA                               
PL2      DS    CL132               PRINT WORKAREA                               
PL3      DS    CL132               PRINT WORKAREA                               
PL4      DS    CL132               PRINT WORKAREA                               
PL5      DS    CL132               PRINT WORKAREA                               
PL6      DS    CL132               PRINT WORKAREA                               
PL7      DS    CL132               PRINT WORKAREA                               
PL8      DS    CL132               PRINT WORKAREA                               
PL9      DS    CL132               PRINT WORKAREA                               
*                                                                               
         SPACE 2                                                                
STACK    DS    12000C                                                           
         SPACE 1                                                                
PJBUFF   DS    2000C                                                            
         SPACE 1                                                                
LYBUFF   DS    2000C                                                            
         SPACE 2                                                                
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - DSECTS'              
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              INCLUDED DSECTS                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT ON                                                               
* REGENREP                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENREPA                                                      
         PRINT ON                                                               
* RERMPPROF                                                                     
         PRINT OFF                                                              
       ++INCLUDE RERMPPROF                                                      
         PRINT ON                                                               
* REGENINV                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENINVA                                                      
         PRINT ON                                                               
* REGENAVL                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENAVL                                                       
         PRINT ON                                                               
* REGENRDP                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENRDP                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* FALOCKETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FALOCKETD                                                      
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DEDBEXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DEDBEXTRAD                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* RERMPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE RERMPWORKD                                                     
         PRINT ON                                                               
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - SCREEN'              
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              PROGRAM SCREEN                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
       ++INCLUDE RERMPFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPE2D                                                       
         TITLE 'T81026 - RERMP26 - OVERNIGHT PROJECTIONS - WORK'                
***********************************************************************         
*                                                                     *         
*        RERMP26 - OVERNIGHT PROJECTIONS - PRINT REPORT               *         
*              PROGRAM WORKAREA                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
STRTOPT  DS    CL3                *MUST BE IN SYNC WITH RERMP24                 
ENDOPT   DS    CL3                *                                             
FROM     DS    CL4                *                                             
TO       DS    CL4                *                                             
UPEL     DS    CL24               *                                             
DPLIST   DS    CL20               *                                             
BOOK     DS    XL4                *RATINGS BOOK WORKAREA                        
*                                                                               
RELO24   DS    A                  *OVLY 24 RELOCATION FACTOR                    
RELO26   DS    A                  *OVLY 26 RELOCATION FACTOR                    
FADDR    DS    A                  *A(FIELD IN ERROR)                            
*                                                                               
DPMENU   DS    CL4                *DAYPART MENU ID                              
*                                                                               
DPTBL    DS    XL(24*DPTBLL)      *DAYPART TABLE                                
*                                                                               
STMENU   DS    CL4                *STATION MENU CODE                            
STMENUNM DS    CL60               *STATION MENU NAME                            
*                                                                               
STLIST   DS    XL(24*STLISTL)     *STATIONS LIST                                
*                                                                               
INVLIST  DS    XL((INVMAX+1)*2*L'RINVKINV)  *INVENTORY NUMBERS LIST             
*                                                                               
INVMAX   EQU   30                  *MAXIMUM NUMBER OF INVENTORY NUMBERS         
*                                                                               
SBUFF    DS    A                   *A(BUFFER)                                   
*                                                                               
CHKBOOK  DS    XL4                 *CHECK BOOK                                  
CHKDEMO  DS    XL3                 *CHECK DEMO                                  
*                                                                               
ADPT     DS    F                                                                
ASTA     DS    F                                                                
SAVESTAK DS    2F                                                               
ASTACK   DS    A                                                                
         DS    A                   SPARE                                        
DPBYTE   DS    CL1                                                              
IUNSW    DS    C                                                                
DEMAREA  DS    CL60                                                             
VDEMUP   DS    V                                                                
APJBUFF  DS    A                                                                
ALYBUFF  DS    A                                                                
ABOOKEL  DS    A                                                                
TOTSHR   DS    3F                                                               
DEMODUB  DS    D                                                                
LASTBK   DS    CL3                                                              
LASTCD   DS    CL5                                                              
LASTCD1  DS    CL2                                                              
PJVAL    DS    F                                                                
LYVAL    DS    F                                                                
THISHMS  DS    F                                                                
THISRTG  DS    F                                                                
THISCD   DS    CL5                                                              
THISCD1  DS    CL2                                                              
FOUNDPJ  DS    CL2                 PROJECTION FOUND SWITCH                      
*                                  0 = NOT FOUND  ^0 = FOUND                    
REP      DS    H                                                                
CMPTODAY DS    H                   TODAY'S DATE COMPRESSED                      
RMPPROF  DS    XL8                 RMP PROGRAM PROFILE                          
SVTIMCHG DS    C                   TIME CHANGE FROM INV HEADER                  
*                                                                               
*        INPUT CONTROL BLOCK FOR GETKSRC                                        
*                                                                               
GSRCIN   DS    0C                  GETKSRC INPUT BLOCK                          
GSIRSVC  DS    CL1                 RATING SERVICE                               
GSIQLF   DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GSIKSRC  DS    CL1                 RINVKSRC FOR KEY                             
GSIBITS  DS    XL1                 BOOKVAL BITS                                 
GSIBKTYP DS    CL1                 BOOKTYPE                                     
GSRCINL  EQU   *-GSRCIN            INPUT BLOCK LENGTH                           
*                                                                               
*        OUTPUT CONTROL BLOCK FOR GETKSRC                                       
*                                                                               
GSRCOUT  DS    0C                  GETKSRC OUTPUT BLOCK                         
GSORSVC  DS    CL1                 RATING SERVICE                               
GSOQLF   DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GSOKSRC  DS    CL1                 RINVKSRC FOR KEY                             
GSOBITS  DS    XL1                 BOOKVAL BITS                                 
GSOBKTYP DS    CL1                 BOOKTYPE                                     
GSRCOUTL EQU   *-GSRCOUT           OUTPUT BLOCK LENGTH                          
*                                                                               
WDEMLIST DS    XL32                WORKING COPY OF DEMLIST                      
*                                                                               
DBLOCKW  DS    XL256               DBLOCK WORKAREA                              
*                                                                               
         TITLE 'T81905 --- RERES05 --- INVENTORY MASTER-DPTBL'                  
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF DAYPART CODES AND NAMES                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DPTBLD   DSECT                                                                  
DPTBCODE DS    CL(L'RRDPCODE)      DAYPART CODE                                 
DPTBSNAM DS    CL(L'RRDPSNAM)      DAYPART SHORT NAME                           
DPTBLNAM DS    CL(L'RRDPLNAM)      DAYPART LONG NAME                            
DPTBLL   EQU   *-DPTBLD            LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'T81026 --- RERMP26 --- OVERNIGHT PROJS - STLISTD'               
***********************************************************************         
*                                                                     *         
*        DSECT FOR LIST OF STATIONS                                   *         
*                                                                     *         
***********************************************************************         
STLISTD  DSECT                                                                  
STLSSTCD DS    CL1                 STATION SORT CODE                            
STLSSTAC DS    CL5                 STATION CALL LETTERS                         
STLISTL  EQU   *-STLISTD           LENGTH OF TABLE ENTRY                        
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034RERMP26   04/13/09'                                      
         END                                                                    
