*          DATA SET REREPSAP   AT LEVEL 145 AS OF 09/27/11                      
*PHASE RESAPB,*                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE ADDAY                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE GETDAY                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DATVAL                                                                 
*INCLUDE SORTER                                                                 
*ENTRY RESAP                                                                    
         TITLE 'REPPAK - SALESMAN/AGY/ADV/PROD/CTG PURGE'                       
****************************************************************                
*  HISTORY OF CHANGES:                                         *                
*  AUG09/90 (BU ) --- INITIAL ENTRY                            *                
*  SEP06/90 (BU ) --- SPLIT PROCESSING TO HANDLE INTEREP AND   *                
*                     LOCAL, WITH MASTER RECORD CONCEPT, AND   *                
*                     ALL OTHERS, AS SEPARATE JOB STEPS.       *                
*                                                              *                
*  !!!!  PLEASE SEE NOTE APPENDED TO TABLE 'IREP' !!!!!!       *                
*                                                              *                
*  JAN27/92 (BU ) --- ADD 'I8' (CABALLERO) AND 'I9' (SCHUBERT) *                
*                     TO TABLE IREP                            *                
*                                                              *                
*  JUL21/92 (BU ) --- ADD 'STATION' TO RECORDS PURGED          *                
*                                                              *                
*  FEB15/94 (SKU) --- ADD CATEGORY CODE TO RECORDS PURGED      *                
*                     ADD POINT PERSON TO RECORDS PURGED       *                
*                     UPDATE INTEREP LIST: DEL HN/DI, ADD D4   *                
*                     DISABLED 'STATION' PENDING FURTHER       *                
*                       EXAMINATION                            *                
*                                                              *                
*  FEB09/95 (BU ) --- KEEP AGY/ADV CONTROL RECORDS (CODE=0000) *                
*                                                              *                
*  SEP20/95 (BU ) --- ADD RM,IF,S1,CM TO INTEREP TABLE.        *                
*                                                              *                
*  JAN05/96 (BU ) --- ADD S3 TO KATZ TABLE                     *                
*                                                              *                
*  FEB26/96 (BU ) --- ADD CN TO INTR TABLE                     *                
*                                                              *                
*  JUL19/96 (BU ) --- ADD AQ TO INTR TABLE                     *                
*                                                              *                
*  MAR07/97 (BU ) --- ADD RS TO KATZ TABLE                     *                
*                                                              *                
*  MAR30/98 (BU ) --- ADD L7 TO INTEREP TABLE                  *                
*                                                              *                
*  MAY19/98 (BU ) --- ADD IB TO INTEREP TABLE                  *                
*                                                              *                
*  MAR25/99 (BU ) --- ADD NX TO INTEREP TABLE                  *                
*                                                              *                
*  APR13/99 (BU ) --- ADD UO TO INTEREP TABLE                  *                
*                                                              *                
*  MAY03/99 (BU ) --- ADD NPTV AS MASTER (NEEDS CODES!!)       *                
*                                                              *                
*  AUG16/99 (BU ) --- ADD QD TO KATZ RADIO TABLE               *                
*                                                              *                
*  DEC06/99 (BU ) --- ADD NU TO KATZ RADIO TABLE               *                
*                                                              *                
*  APR24/00 (BU ) --- ADD G8 TO KATZ RADIO TABLE               *                
*                                                              *                
*  MAR20/01 (BU ) --- ADD J0 TO KATZ RADIO TABLE               *                
*                                                              *                
*  SEP04/01 (BU ) --- ADD 46 (S/P) RECS TO PURGE               *                
*                                                              *                
*  SEP25/01 (BU ) --- ADD WC TO KATZ RADIO TABLE               *                
*                                                              *                
*  SEP27/11 (KUI) --- ADD V6 TO MASTER REP TABLE               *                
*                                                              *                
*                                                              *                
*                     ***  END TOMBSTONE  ***                  *                
****************************************************************                
*  ADDITIONAL BASE REGISTERS:  R9,R6                           *                
*  PURGE OF CATEGORY CODE CHECKS FOR THE EXISTENCE OF THE CAT  *                
*  CODE IN THE ADVERTISER, PRODUCT AND CONTRACT RECORDS.       *                
*  PURGE OF POINT PERSON CHECKS FOR THE EXISTENCE OF THE PTP   *                
*  CODE IN THE PRODUCT RECORD ONLY.                            *                
*                                                              *                
****************************************************************                
RESAP    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,RESAP,=V(REGSAVE),R9,R6                                        
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         USING RECD,R5                                                          
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         ST    RB,WORK                                                          
         L     RF,=V(STXITER)                                                   
         ST    RF,WORK+4                                                        
         OI    WORK+4,X'80'                                                     
         GOTO1 =V(STXITER),DMCB,WORK                                            
         XC    WORK,WORK                                                        
*                                                                               
         MVC   TITLE+10(38),=C'REP SAL/AGY/ADV/PRD/CTG/PTP PURGE'               
         SPACE 2                                                                
         BAS   R8,TABINIT          INITIALIZE TABLE OF REP SELECTIONS           
         SPACE 1                                                                
NEXTCARD GOTO1 CARDS,DMCB,CARDIN,=C'RE00'                                       
         MVC   P+1(80),CARDIN                                                   
         GOTO1 PRINTER                                                          
         CLC   CARDIN(2),=C'/*'                                                 
         BNE   VALCARD                                                          
         BAS   RE,MASTCHEK         FOR MASTREP, GENERATE TABLE                  
         B     PROCESS                                                          
         SPACE 3                                                                
VALCARD  EQU   *                                                                
*              VALIDATE INPUT CARDS                                             
*                                                                               
*  RULES FOR INPUT CARDS                                                        
*  1.    INPUT CARDS MAY BE ENTERED FOR 1 OR MORE REPS                          
*  2.    FIRST CARD MUST BE 'CONTROL=XXXXXX', WHERE XXXXXX IS EITHER            
*        'TEST  ' (NO UPDATE) OR 'UPDATE' (UPDATE).                             
*  3.    SECOND CARD MUST BE 'MASTREP=XXX', WHERE XXX IS EITHER 'YES'           
*        OR 'NO '.  IF 'YES', ONLY A 'REP=IR' CARD WILL BE ACCEPTED             
*        (SEE 4, BELOW).  IF 'NO ', 'IR' AND SUBSIDIARY REP CODES               
*        WILL BE REJECTED IN 'REP=' CARDS.                                      
*  4.    EACH SET MUST BEGIN WITH CARD WITH 'REP=XX', WHERE XX IS THE           
*        REP CODE.  ABSENCE OF 'REP=' CARD WILL TERMINATE INPUT WITH            
*        ERROR.                                                                 
*  5.    A SET OF INPUT CARDS WILL CONTROL A SPECIFIC REP.  IF DATA             
*        IS ENCOUNTERED FOR A REP CODE NOT IN THE TABLE, THE DATA               
*        WILL BE PASSED THROUGH.                                                
*                                                                               
*   NOTE: >>>> R7 CONTROLS THE TABLE INSERTION!!! <<<<                          
*              DON'T CHANGE IT IN THE VALIDATION                                
*                                                                               
         SPACE 1                                                                
         AP    CARDCTR(4),=P'1'    COUNT CARD INPUT                             
         CP    CARDCTR(4),=P'1'    FIRST CARD MUST BE CONTROL                   
         BNE   VC004                                                            
         CLC   CARDIN(8),=C'CONTROL='                                           
         BNE   CARDERR6            NO CONTROL - TERMINATE                       
         B     VALCTRL             VALIDATE CONTROL CARD                        
VC004    EQU   *                                                                
         CP    CARDCTR(4),=P'2'    SECOND CARD MUST BE MASTREP                  
         BNE   VC006                                                            
         CLC   CARDIN(8),=C'MASTREP='                                           
         BNE   CARDERR8            NO MASTREP - TERMINATE                       
         B     VALIREP                                                          
VC006    EQU   *                                                                
         CLC   CARDIN(4),=C'REP='                                               
         BE    VALREP                                                           
         L     R7,THISREP          A(SET OF INPUT CARDS)                        
         OC    DREP(2,R7),DREP(R7) 'REP=' CARD MUST BE FIRST                    
         BZ    CARDERR1            NOT PRESENT=ERROR                            
         CLC   CARDIN(11),=C'SALESPERSON'                                       
         BE    VALSAL                                                           
         CLC   CARDIN(6),=C'AGENCY'                                             
         BE    VALAGY                                                           
         CLC   CARDIN(10),=C'ADVERTISER'                                        
         BE    VALADV                                                           
         CLC   CARDIN(07),=C'PRODUCT'                                           
         BE    VALPROD                                                          
*        CLC   CARDIN(07),=C'STATION'                                           
*        BE    VALSTAT                                                          
         CLC   CARDIN(08),=C'CATEGORY'                                          
         BE    VALCTG                                                           
         CLC   CARDIN(11),=C'POINTPERSON'                                       
         BE    VALPTP                                                           
         CLC   CARDIN(09),=C'TRACE=YES'    TRACE FLAG                           
         BNE   CARDERR                                                          
         MVI   TESTYN,C'Y'         TURN ON TRACE TEST                           
         B     NEXTCARD            GO BACK FOR NEXT                             
         SPACE 2                                                                
VALCTRL  CLC   CARDIN+8(6),=C'UPDATE'                                           
         BNE   VCTRL002            NOT 'UPDATE' OPTION                          
         MVI   RUNOPT,C'H'         SET RUN-TIME OPTION = HARD                   
         B     NEXTCARD                                                         
VCTRL002 CLC   CARDIN+8(6),=C'TEST  '                                           
         BNE   CARDERR7            NOT 'SOFT' OPTION                            
         MVI   RUNOPT,C'S'         SET RUN-TIME OPTION = SOFT                   
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALIREP  CLC   CARDIN+8(3),=C'YES'                                              
         BNE   VIREP002            NOT 'MASTREP' OPTION                         
         MVI   MASTOPT,C'Y'        SET MASTREP OPTION TO YES                    
         B     NEXTCARD                                                         
VIREP002 CLC   CARDIN+8(3),=C'NO '                                              
         BNE   CARDERR9            MASTREP OPTION NOT YES OR NO                 
         MVI   MASTOPT,C'N'        SET MASTREP OPTION TO NO                     
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALREP   OC    NEXTREP,NEXTREP     ZERO=FIRST TIME                              
         BNZ   VR010                                                            
         LA    R2,REPNTRY+LREPNTRY SET A(TABLE) OF NEXTREP                      
         ST    R2,NEXTREP                                                       
         B     VR020                                                            
VR010    MVC   THISREP,NEXTREP     REPLACE CURRENT WITH NEXT                    
         L     R2,NEXTREP                                                       
         LA    R2,LREPNTRY(R2)     BUMP BY LENGTH                               
         ST    R2,NEXTREP                                                       
VR020    CLC   CARDIN+04(2),=C'  ' NO REP CODE ENTERED                          
         BE    CARDERR2                                                         
         CP    REPCTR(2),=P'50'    TABLE FULL? 50 MAX!                          
         BE    CARDERR5            YES - NOTIFY USER, STOP JOB                  
         AP    REPCTR(2),=P'1'     NO  - BUMP CTR                               
         CLI   MASTOPT,C'Y'        IS RUN FOR MASTREP & LOCALS?                 
         BNE   VR024               NO                                           
         CLC   CARDIN+04(2),=C'IR' YES - IS CODE INTEREP?                       
         BE    VR022                                                            
         CLC   CARDIN+04(2),=C'K3' YES - IS CODE KATZ RADIO?                    
         BE    VR022                                                            
         CLC   CARDIN+04(2),=C'MR' YES - IS CODE KATZ TV?                       
         BE    VR022                                                            
         CLC   CARDIN+04(2),=C'V6' YES - IS CODE NATIONAL PUBLIC BCAST          
         BNE   CARDER10            NO  - ERROR                                  
VR022    EQU   *                                                                
         CP    REPCTR(2),=P'1'     ONLY SINGLE CODE FOR IR/K3/MR                
         BNE   CARDER12            SHOULD HAVE BEEN ENTERED                     
         MVC   LIVEREP,CARDIN+04   SAVE REP INPUT AS MASTER                     
         B     VR030                                                            
VR024    EQU   *                                                                
         BAS   RE,VR0100           NOT MASTREP: VALIDATE REP                    
         BNZ   CARDER11            ERROR - MASTREP CODE FOUND                   
VR030    EQU   *                                                                
         L     R2,THISREP          LOAD A(REP IN TABLE)                         
         MVC   DREP(2,R2),CARDIN+4  SAVE INPUT - NO MORE VALIDATION             
         B     NEXTCARD                                                         
         SPACE 2                                                                
*                                                                               
*  OPTION IS TO PROCESS NON-MASTREP.  MUST CHECK TO ENSURE AN                   
*   MASTREP CODE IS NOT INCLUDED.                                               
*                                                                               
VR0100   EQU   *                                                                
         NTR1                                                                   
         LA    R3,IREPIR           LOAD A(CODE TABLE) AT IR                     
VR0105   EQU   *                                                                
         CLC   0(2,R3),=C'00'      LAST ENTRY FOUND?                            
         BE    VR0197              YES - BRANCH TO 'GOOD' EXIT                  
         CLC   0(2,R3),CARDIN+4    NO  - MATCH AGAINST CARD INPUT               
         BE    VR0198              ERROR IF FOUND                               
         LA    R3,L'IREP(R3)       BUMP BY TABLE ENTRY LENGTH                   
         B     VR0105                                                           
VR0197   EQU   *                                                                
         SR    R3,R3               SET CC = 0                                   
         B     VR0199                                                           
VR0198   EQU   *                                                                
         LTR   RB,RB               SET CC <> 0                                  
VR0199   EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
VALSAL   EQU   *                                                                
         MVI   DSAL(R7),C'Y'          FLAG REP/SALESPERSON PURGE                
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALAGY   EQU   *                                                                
         MVI   DAGY(R7),C'Y'          FLAG REP/AGENCY PURGE                     
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALADV   EQU   *                                                                
         MVI   DADV(R7),C'Y'          FLAG REP/ADVERTISER PURGE                 
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALPROD  EQU   *                                                                
         MVI   DPROD(R7),C'Y'         FLAG REP/ADVERTISER PURGE                 
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALSTAT  EQU   *                                                                
         MVI   DSTAT(R7),C'Y'         FLAG REP/STATION PURGE                    
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALCTG   EQU   *                                                                
         MVI   DCTG(R7),C'Y'          FLAG REP/CATEGORY PURGE                   
         B     NEXTCARD                                                         
         SPACE 2                                                                
VALPTP   EQU   *                                                                
         MVI   DPTP(R7),C'Y'          FLAG REP/POINT PERSON PURGE               
         B     NEXTCARD                                                         
         SPACE 2                                                                
CARDERR  MVC   P+1(26),=C'INVALID OR DUPLICATE CARD:'                           
         B     CERROR                                                           
CARDERR1 MVC   P+1(20),=C'NO REP= FOR CARD SET'                                 
         B     CERROR                                                           
CARDERR2 MVC   P+1(25),=C'NO REP CODE IN REP= CARD:'                            
         B     CERROR                                                           
CARDERR5 MVC   P+1(37),=C'TOO MANY SETS OF INPUT CARDS: 50 MAX!'                
         B     CERROR                                                           
CARDERR6 MVC   P+1(27),=C'NO CONTROL= FOR CARD INPUT:'                          
         B     CERROR                                                           
CARDERR7 MVC   P+1(28),=C'CONTROL= OPTION NOT CORRECT:'                         
         B     CERROR                                                           
CARDERR8 MVC   P+1(27),=C'NO MASTREP= FOR CARD INPUT:'                          
         B     CERROR                                                           
CARDERR9 MVC   P+1(28),=C'MASTREP= OPTION NOT CORRECT:'                         
         B     CERROR                                                           
CARDER10 MVC   P+1(39),=C'MASTREP RUN: ONLY "IR/K3/MR/V6" ALLOWED'              
         B     CERROR                                                           
CARDER11 MVC   P+1(36),=C'NON-MASTREP RUN: MASTREP CODE FOUND!'                 
         B     CERROR                                                           
CARDER12 MVC   P+1(38),=C'MASTREP RUN: ONLY SINGLE ENTRY ALLOWED'               
         B     CERROR                                                           
CERROR   GOTO1 PRINTER                                                          
         GOTO1 PRINTER                                                          
         MVC   P+1(20),CARDIN                                                   
         GOTO1 PRINTER                                                          
         GOTO1 PRINTER                                                          
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*  SET BASIC FLAGS.  PROPAGATE FIRST INITIALIZED TABLE ENTRY INTO               
*  50 OTHER ENTRIES.                                                            
*                                                                               
TABINIT  LA    R7,REPNTRY                                                       
         ST    R7,THISREP          SET 'THISREP'.'NEXTREP' STILL=0              
         LA    R2,MOREREPS         A(1ST UNINITIALIZED TABLE ENTRY)             
         LA    R3,50               TABLE ENTRIES                                
TAB010   MVC   0(LREPNTRY,R2),0(R7) PROPAGATE ENTRY                             
         LA    R2,LREPNTRY(R2)     BUMP TO NEXT ENTRY                           
         BCT   R3,TAB010           DO 50 ENTRIES                                
         BR    R8                  RETURN TO CALLING RTN                        
         EJECT                                                                  
*                                                                               
*  IF INTEREP RUN, INSERT LOCAL CODES INTO TABLE, PROPAGATE THE                 
*     INDIVIDUAL OPTIONS FROM THE 'IR' ENTRY                                    
*                                                                               
MASTCHEK EQU   *                                                                
         NTR1                                                                   
         CLI   MASTOPT,C'Y'        INTEREP RUN?                                 
         BNE   IC099               NO                                           
         LA    R3,IREP             A(INTEREP LOCAL CODE TABLE)                  
         CLC   LIVEREP,=C'IR'      INTEREP REQUEST?                             
         BE    IC004               YES                                          
         LA    R3,KATZRREP         A(KATZ RADIO LOCAL CODE TABLE)               
         CLC   LIVEREP,=C'K3'      KATZ RADIO REQUEST?                          
         BE    IC004               YES                                          
         LA    R3,KATZTREP         A(KATZ TV LOCAL CODE TABLE)                  
         CLC   LIVEREP,=C'MR'      KATZ TV REQUEST?                             
         BE    IC004               YES                                          
         LA    R3,NPTVREP          A(NATIONAL PUBLIC CODE TABLE)                
         CLC   LIVEREP,=C'V6'      NP TV/RADIO REQUEST?                         
         BE    IC004               YES                                          
         DC    H'0'                SHOULD NOT OCCUR                             
IC004    EQU   *                                                                
         CLC   0(2,R3),=X'FFFF'    END OF TABLE?                                
         BE    IC099               YES - FINISHED                               
         MVC   THISREP,NEXTREP     REPLACE CURRENT WITH NEXT                    
         L     R2,NEXTREP                                                       
         LA    R2,LREPNTRY(R2)     BUMP BY LENGTH                               
         ST    R2,NEXTREP                                                       
         L     R2,THISREP                                                       
         MVC   DREP(2,R2),0(R3)    MOVE CODE FROM MAST TO TABLE                 
         MVC   DSAL(NUMNTRY,R2),REPNTRY+2                                       
*                                  LOAD OPTIONS                                 
         LA    R3,L'IREP(R3)       BUMP TABLE                                   
         B     IC004                                                            
IC099    EQU   *                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*              BUILD FILE OF CODES IN USE                                       
         SPACE 1                                                                
*                                                                               
*   R7 IS USED THROUGHOUT THE REMAINDER OF THE PROGRAM TO CONTROL               
*      REP CODE TABLE SEARCH AND COUNTER UPDATE CONTROL.                        
*      >>>>>> PLEASE DON'T STEP ON R7!!!! <<<<<<                                
*                                                                               
PROCESS  LA    R7,REPNTRY          LOAD A(1ST TABLE ENTRY)                      
         MVI   PHASE,C'1'                                                       
         OPEN  (IN,(INPUT))                                                     
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         SPACE 1                                                                
GETR10   GET   IN,REC-4                                                         
         LH    RE,REC-4            SET EOR MARKER                               
         LA    RE,REC-4(RE)                                                     
         MVI   0(RE),0                                                          
         LA    R5,REC                                                           
         CLI   TESTYN,C'N'         TRACE TEST?                                  
         BE    GETR20              NO  - SKIP DISPLAY                           
         CP    TESTCTR1(4),TESTMAX MAX REACHED?                                 
         BH    GETR20              YES  - DON'T DO ANYTHING                     
         AP    TESTCTR1(4),=P'1'   NO  - ADD AND DISPLAY                        
         MVC   P+1(06),=C'RECIN:'                                               
         MVC   P+9(27),REC                                                      
         GOTO1 PRINTER                                                          
         EJECT                                                                  
*********************************************************************           
* CHECK STATION RECORD                                                          
*********************************************************************           
GETR20   EQU   *                                                                
         CLI   RSTAKTYP,X'02'      STATION RECORD?                              
         BNE   GETR30              NO  - CHECK FOR NEXT RECORD TYPE             
         LA    R2,RSTAKREP         FIND STATION'S REP IN TABLE                  
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'Y'       TEST FOUND FLAG                              
         BNE   GETR10              NOT FOUND - SKIP IT                          
         CLI   DSTAT(R7),C'Y'      PURGE STATIONS FOR REP?                      
         BNE   GETR10              NO  - SKIP THIS RECORD                       
         BAS   RE,SCANSTAT         GENERATE STA SORT RECS, IF NEEDED            
         B     GETR10              GO BACK FOR NEXT                             
         EJECT                                                                  
*********************************************************************           
* WITHIN THE ADVERTISER RECORD, CHECK FOR REFERENCE TO:                         
* - CATEGORY CODE                                                               
*********************************************************************           
GETR30   EQU   *                                                                
         CLI   RADVKTYP,X'08'      ADVERTISER RECORD?                           
         BNE   GETR40              NEED TO CHECK IF CTG CODE PRESENT            
*                                                                               
         LA    R2,RADVKREP         FIND ADVERTISER'S REP IN TABLE               
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'Y'       TEST FOUND FLAG                              
         BNE   GETR10              NOT FOUND - SKIP IT                          
         CLI   DCTG(R7),C'Y'       PURGE CATEGORY CODE?                         
         BNE   GETR10              NO - SKIP THIS RECORD                        
*                                                                               
         OC    RADVCATG,RADVCATG   IF CAT CODE IS NULL OR SPACES,               
         BZ    GETR10              DON'T BUILD A SORT RECORD                    
         CLC   RADVCATG,SPACES                                                  
         BE    GETR10                                                           
*                                                                               
         BAS   RE,BLDADCTG         GENERATE CTG SORT REC, IF NEEDED             
         B     GETR10              GO BACK FOR NEXT                             
         EJECT                                                                  
*********************************************************************           
* WITHIN THE PRODUCT RECORD, CHECK FOR REFERENCES OF:                           
* - CATEGORY CODE                                                               
* - POINT PERSON CODE                                                           
*********************************************************************           
GETR40   EQU   *                                                                
         CLI   RPRDKTYP,X'09'      PRODUCT RECORD?                              
         BNE   GETCON1             NO - CHECK FOR CONTRACT RECORD               
*                                                                               
         LA    R2,RPRDKREP         FIND PRODUCT'S REP IN TABLE                  
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'Y'       TEST FOUND FLAG                              
         BNE   GETR10              NOT FOUND - SKIP IT                          
                                                                                
         CLI   DCTG(R7),C'Y'       PURGE CATEGORY CODE?                         
         BNE   GETR45              NO - CHECK IF PURGE POINT PERSON             
*                                                                               
         OC    RPRDCATG,RPRDCATG   IF CTG CODE IS NULL OR SPACES,               
         BZ    GETR45              DON'T BUILD A SORT RECORD                    
         CLC   RPRDCATG,SPACES                                                  
         BE    GETR45                                                           
         BAS   RE,BLDPDCTG         GENERATE CTG SORT REC, IF NEEDED             
*                                                                               
GETR45   DS    0H                                                               
         CLI   DPTP(R7),C'Y'       PURGE POINT PERSON?                          
         BNE   GETR10                                                           
         BAS   RE,SCANPTP          GENERATE PTP SORT REC, IF NEEDED             
         B     GETR10              GO BACK FOR NEXT                             
         EJECT                                                                  
*********************************************************************           
* WITHIN THE CONTRACT RECORD, CHECK FOR ALL REFERENCES                          
*********************************************************************           
GETCON1  EQU   *                                                                
         CLI   RCONKTYP,X'0C'                                                   
         BH    END                                                              
         BL    GETR10                                                           
BUILDKEY EQU   *                                                                
         CLI   TESTYN,C'N'         TRACE TEST?                                  
         BE    BK003               NO  - SKIP DISPLAY                           
         CP    TESTCTR2(4),TESTMAX MAX REACHED?                                 
         BH    BK003               YES  - DON'T DO ANYTHING                     
         AP    TESTCTR2(4),=P'1'   NO  - ADD AND DISPLAY                        
         MVC   P+1(06),=C'CTRCT:'                                               
         MVC   P+9(27),REC                                                      
         GOTO1 PRINTER                                                          
BK003    EQU   *                                                                
         LA    R2,RCONKREP                                                      
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'Y'       TEST FOUND FLAG                              
         BE    BK0032              IN TABLE  - PROCESS                          
         CLI   TESTYN,C'N'         TRACE TEST?                                  
         BE    GETR10              NO  - GET NEXT RECORD                        
         CP    TESTCTR3(4),TESTMAX MAX REACHED?                                 
         BH    GETR10              YES  - GET NEXT RECORD                       
         AP    TESTCTR3(4),=P'1'   NO  - ADD AND DISPLAY                        
         MVC   P+1(06),=C'C NFD:'                                               
         MVC   P+9(27),REC                                                      
         GOTO1 PRINTER                                                          
         B     GETR10              GO BACK FOR NEXT RECORD                      
BK0032   CLI   TESTYN,C'N'         TRACE TEST?                                  
         BE    BK0033              NO  - SKIP DISPLAY                           
         CP    TESTCTR4(4),TESTMAX MAX REACHED?                                 
         BH    BK0033              YES  - DON'T DO ANYTHING                     
         AP    TESTCTR4(4),=P'1'   NO  - ADD AND DISPLAY                        
         MVC   P+1(06),=C'C FND:'                                               
         MVC   P+9(27),REC                                                      
         GOTO1 PRINTER                                                          
BK0033   EQU   *                                                                
         XC    SORTREC,SORTREC     BUILD THE SORT RECORD                        
*                                                                               
* BUILD AGENCY, ADVERTISER, PRODUCT, SALESPERSON KEYS IF NEEDED                 
*                                                                               
         CLI   DAGY(R7),C'Y'       PURGE AGENCY FOR REP?                        
         BNE   BK004               NO                                           
         BAS   RE,BLDAGY           YES - BUILD AGENCY SORT RECORD               
         SPACE 3                                                                
BK004    CLI   DADV(R7),C'Y'       PURGE ADVERTISER FOR REP?                    
         BNE   BK006               NO                                           
         BAS   RE,BLDADV           YES - BUILD ADVERTISER SORT RECORD           
         SPACE 3                                                                
BK006    CLI   DPROD(R7),C'Y'      PURGE PRODUCT FOR REP?                       
         BNE   BK008               NO                                           
         BAS   RE,BLDPROD          YES - BUILD PRODUCT SORT RECORD              
         SPACE 3                                                                
BK008    CLI   DSAL(R7),C'Y'       PURGE SALESMAN FOR REP                       
         BNE   BK010               NO                                           
         BAS   RE,BLDSAL           YES - BUILD SALESPERSON SORT REC             
         SPACE 3                                                                
BK010    CLI   DSTAT(R7),C'Y'      PURGE STATION FOR REP                        
         BNE   BK012               NO                                           
         BAS   RE,BLDSTAT          YES - BUILD STATION SORT REC                 
         SPACE 3                                                                
BK012    CLI   DCTG(R7),C'Y'       PURGE CATEGORY CODE FOR REP                  
         BNE   GETR10              NO                                           
         BAS   RE,BLDCTG           YES - BUILD STATION SORT REC                 
         B     GETR10                                                           
         EJECT                                                                  
*                                                                               
*  AGENCY SORT RECORD BUILD ROUTINE                                             
BLDAGY   NTR1                                                                   
         MVI   STYP,X'0A'          SET UP AGENCY KEY                            
         MVC   SORTREC+SAGYCODE(6),RCONKAGY                                     
         MVC   SORTREC+SAGYREP(2),RCONKREP                                      
         LA    R3,SORTREC+SAGYREP  FOR IREP TEST                                
         LA    R2,RCONKREP                                                      
         BAS   RE,MASTTEST         WILL REPLACE IF IR LOCAL                     
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVI   STYP,X'1A'          SET UP SECOND AGENCY RECORD                  
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*                                  PUT OUT SECOND RECORD                        
         MVI   STYP,X'0A'          RESET RECORD TYPE                            
         CLC   RCONKAOF(2),=C'  '  AGENCY OFFICE ENTERED                        
         BE    BAGY002             NO  - SKIP EXTRA RECORD                      
*                                                                               
*  CANNOT DELETE THE CORPORATE AGENCY RECORD IF AN AGENCY OFFICE                
*  IS ACTIVE.  IF AGENCY OFFICE NOT = SPACE, PUT OUT A CORPORATE                
*  SORT ENTRY, IN WHICH THE AGENCY OFFICE = SPACE.  THIS WILL                   
*  ENSURE THAT THE CORPORATE RECORD IS 'ACTIVE' DURING THE PURGE                
*  CYLE.  (I ALSO KNOW THIS GENERATES MANY ADDITIONAL RECORDS.)                 
*                                                                               
         MVC   SORTREC+SAGYOFF(2),SPACES                                        
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVI   STYP,X'1A'          SET UP SECOND AGENCY RECORD                  
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*                                  PUT OUT SECOND RECORD                        
BAGY002  EQU   *                                                                
         XC    SORTREC,SORTREC                                                  
         AP    RECSSORT(5),=P'1'                                                
         B     EXXMOD                                                           
         SPACE 3                                                                
*                                                                               
*  ADVERTISER SORT RECORD BUILD ROUTINE                                         
BLDADV   NTR1                                                                   
         MVI   STYP,X'08'          SET UP ADVERTISER KEY                        
         MVC   SORTREC+SADVCODE(4),RCONKADV                                     
         MVC   SORTREC+SADVREP(2),RCONKREP                                      
         LA    R3,SORTREC+SADVREP  FOR IREP TEST                                
         LA    R2,RCONKREP                                                      
         BAS   RE,MASTTEST         WILL REPLACE IF IR LOCAL                     
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         XC    SORTREC,SORTREC                                                  
         AP    RECSSORT(5),=P'1'                                                
         B     EXXMOD                                                           
*                                                                               
*  PRODUCT SORT RECORD BUILD ROUTINE                                            
BLDPROD  NTR1                                                                   
         MVI   STYP,X'09'          SET UP PRODUCT KEY                           
         MVC   SORTREC+SPRODADV(4),RCONKADV                                     
         MVC   SORTREC+SPRODPRD(3),RCONPRD                                      
         MVC   SORTREC+SPRODREP(2),RCONKREP                                     
         LA    R3,SORTREC+SPRODREP FOR IREP TEST                                
         LA    R2,RCONKREP                                                      
         BAS   RE,MASTTEST         WILL REPLACE IF IR LOCAL                     
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         XC    SORTREC,SORTREC                                                  
*                                                                               
*   THERE MAY BE A PRODUCT RECORD (KEY=ADVERTISER/PROD) WHOSE                   
*        ADVERTISER MIGHT BE INACTIVE.  TO ENSURE THAT THE                      
*        ADVERTISER IS NOT DELETED, AN ADDITIONAL RECORD IS                     
*        PUT OUT.                                                               
*                                                                               
         BAS   RE,BLDADV           ADVERTISER RECORD FOR PROD                   
         AP    RECSSORT(5),=P'1'                                                
         B     EXXMOD                                                           
*                                                                               
*  SALESPERSON SORT RECORD BUILD ROUTINE                                        
BLDSAL   NTR1                                                                   
         MVI   STYP,X'06'          SET UP SALESMAN KEY                          
         MVC   SORTREC+SSPREP(2),RCONKREP                                       
         CLC   =C'K3',LIVEREP      KATZ RADIO? S/P = MASTER!                    
         BE    BLDSAL02            YES - USE LIVEREP                            
         CLC   =C'IR',LIVEREP      INTEREP? S/P = MASTER!                       
         BE    BLDSAL02            YES - USE LIVEREP                            
         CLC   =C'MR',LIVEREP      KATZ TV? S/P = MASTER!                       
         BE    BLDSAL02            YES - USE LIVEREP                            
         CLC   =C'V6',LIVEREP      NP TV? S/P = MASTER!                         
         BNE   BLDSAL04            NO  - USE VALUE ALREADY LOADED               
BLDSAL02 EQU   *                                                                
         MVC   SORTREC+SSPREP(2),LIVEREP                                        
*                                  USE ALTERNATE MASTER REP!                    
BLDSAL04 EQU   *                                                                
         MVC   SORTREC+SSPSLS(3),RCONSAL                                        
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVI   STYP,X'46'          SET UP SECOND S/P    RECORD                  
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*                                  PUT OUT SECOND RECORD                        
         XC    SORTREC,SORTREC                                                  
         AP    RECSSORT(5),=P'1'                                                
EXXMOD   XMOD1 1                                                                
*                                                                               
*  STATION SORT RECORD BUILD ROUTINE                                            
BLDSTAT  NTR1                                                                   
         MVI   STYP,X'02'          SET UP STATION KEY                           
         MVC   SORTREC+SSTATREP(2),RCONKREP                                     
         MVC   SORTREC+SSTATSTA(5),RCONKSTA                                     
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         XC    SORTREC,SORTREC                                                  
         AP    RECSSORT(5),=P'1'                                                
         B     EXXMOD                                                           
*                                                                               
*  CATEGORY CODE SORT RECORD BUILD ROUTINE FROM A CONTRACT RECORD               
*                                                                               
BLDCTG   NTR1                                                                   
         MVI   STYP,X'0F'          SET UP CATEGORY CODE KEY                     
         MVC   SORTREC+SCTGREP(2),RCONKREP                                      
         MVC   SORTREC+SCTGCODE(2),RCONCTGY                                     
         LA    R3,SORTREC+SCTGREP                                               
         LA    R2,RCONKREP                                                      
         BAS   RE,MASTTEST         WILL REPLACE IF IR LOCAL                     
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         XC    SORTREC,SORTREC                                                  
         AP    RECSSORT(5),=P'1'                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*  POINT PERSON CODE SORT RECORD BUILD ROUTINE FROM A PRODUCT RECORD            
*                                                                               
SCANPTP  NTR1                                                                   
         XC    SORTREC,SORTREC     BUILD SORTREC(S) FOR COMPONENTS              
         LA    R1,RPRDREC          CALCULATE EOR                                
         ZICM  R0,RPRDLEN,2        INSERT RECORD LENGTH                         
         AR    R1,R0               CALCULATE END OF RECORD                      
         LA    R2,RPRDELEM         A(DESCRIP ELEMENT)                           
SCANP010 EQU   *                                                                
         ZIC   R0,1(R2)            BUMP BY LENGTH                               
         AR    R2,R0                                                            
         CR    R2,R1               END OF RECORD REACHED?                       
         BNL   EXXMOD              YES - FINISHED                               
         CLI   0(R2),X'02'         NETWORK CONTRACT ELT?                        
         BNE   SCANP010                                                         
*                                                                               
         MVI   STYP,X'31'                                                       
         MVC   SORTREC+SPTPREP(2),RPRDKREP                                      
         MVC   SORTREC+SPTPCODE(3),22(R2)                                       
         LA    R3,SORTREC+SPTPREP                                               
         LA    R2,RPRDKREP                                                      
         BAS   RE,MASTTEST         WILL REPLACE IF IR LOCAL                     
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         XC    SORTREC,SORTREC                                                  
         AP    RECSSORT(5),=P'1'                                                
                                                                                
         CLI   TESTYN,C'N'         TRACE TEST?                                  
         BE    EXXMOD              NO  - SKIP DISPLAY                           
         CP    TESTCTR7(4),TESTMAX MAX REACHED?                                 
         BH    EXXMOD              YES  - DON'T DO ANYTHING                     
         AP    TESTCTR7(4),=P'1'   NO  - ADD AND DISPLAY                        
         MVC   P+1(08),=C'PRD PTP:'                                             
         MVC   P+10(27),REC                                                     
         GOTO1 PRINTER                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*  CATEGORY CODE SORT RECORD BUILD ROUTINE FROM AN ADVERTISER RECORD            
*                                                                               
BLDADCTG NTR1                                                                   
         MVI   STYP,X'0F'          SET UP CATEGORY CODE KEY                     
         MVC   SORTREC+SCTGREP(2),RADVKREP                                      
         MVC   SORTREC+SCTGCODE(2),RADVCATG                                     
         LA    R3,SORTREC+SCTGREP                                               
         LA    R2,RADVKREP                                                      
         BAS   RE,MASTTEST         WILL REPLACE IF IR LOCAL                     
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         XC    SORTREC,SORTREC                                                  
         AP    RECSSORT(5),=P'1'                                                
                                                                                
         CLI   TESTYN,C'N'         TRACE TEST?                                  
         BE    EXXMOD              NO  - SKIP DISPLAY                           
         CP    TESTCTR5(4),TESTMAX MAX REACHED?                                 
         BH    EXXMOD              YES  - DON'T DO ANYTHING                     
         AP    TESTCTR5(4),=P'1'   NO  - ADD AND DISPLAY                        
         MVC   P+1(08),=C'ADV CAT:'                                             
         MVC   P+10(27),REC                                                     
         GOTO1 PRINTER                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*  CATEGORY CODE SORT RECORD BUILD ROUTINE FROM A PRODUCT RECORD                
*                                                                               
BLDPDCTG NTR1                                                                   
         MVI   STYP,X'0F'          SET UP CATEGORY CODE KEY                     
         MVC   SORTREC+SCTGREP(2),RPRDKREP                                      
         MVC   SORTREC+SCTGCODE(2),RPRDCATG                                     
         LA    R3,SORTREC+SCTGREP                                               
         LA    R2,RPRDKREP                                                      
         BAS   RE,MASTTEST         WILL REPLACE IF IR LOCAL                     
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         XC    SORTREC,SORTREC                                                  
         AP    RECSSORT(5),=P'1'                                                
                                                                                
         CLI   TESTYN,C'N'         TRACE TEST?                                  
         BE    EXXMOD              NO  - SKIP DISPLAY                           
         CP    TESTCTR6(4),TESTMAX MAX REACHED?                                 
         BH    EXXMOD              YES  - DON'T DO ANYTHING                     
         AP    TESTCTR6(4),=P'1'   NO  - ADD AND DISPLAY                        
         MVC   P+1(08),=C'PRD CAT:'                                             
         MVC   P+10(27),REC                                                     
         GOTO1 PRINTER                                                          
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* SCAN STATION RECORD FOR X'09' AND X'0A' ELEMENTS.  IF PRESENT,                
*    GENERATE A SORT RECORD FOR EACH STATION REPRESENTED.                       
*                                                                               
SCANSTAT NTR1                                                                   
         XC    SORTREC,SORTREC     BUILD SORTREC(S) FOR COMPONENTS              
         LA    R1,RSTAREC          CALCULATE EOR                                
         ZICM  R0,RSTALEN,2        INSERT RECORD LENGTH                         
         AR    R1,R0               CALCULATE END OF RECORD                      
         LA    R2,RSTAELEM         A(DESCRIP ELEMENT)                           
SCAN0010 EQU   *                                                                
         ZIC   R0,1(R2)            BUMP BY LENGTH                               
         AR    R2,R0                                                            
         CR    R2,R1               END OF RECORD REACHED?                       
         BNL   EXXMOD              YES - FINISHED                               
         CLI   0(R2),X'09'         COMBINED STATIONS ELT?                       
         BNE   SCAN0020                                                         
*                                                                               
*   NOTE!!! THE LAYOUT OF THE COMBINED STATION ELEMENT WILL BE                  
*    CHANGING FOR THE NEW FACILITIES BEING INSTALLED FOR INTEREP.               
*    THE NEW LAYOUT MUST BE REFLECTED HERE!!'                                   
*                                                                               
         MVI   STYP,X'02'          SET UP STATION KEY                           
         MVC   SORTREC+SSTATREP,RSTAKREP                                        
         MVC   SORTREC+SSTATSTA(4),2(R2)    SET UP AM STATION                   
         MVI   SORTREC+SSTATSTA+4,C'A'      INSERT 'AM' MEDIA                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVC   SORTREC+SSTATSTA(4),6(R2)    SET UP FM STATION                   
         MVI   SORTREC+SSTATSTA+4,C'F'      INSERT 'FM' MEDIA                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         XC    SORTREC,SORTREC                                                  
         AP    RECSSORT(5),=P'2'                                                
         B     SCAN0010            GO BACK FOR NEXT ELEMENT                     
SCAN0020 EQU   *                                                                
         CLI   0(R2),X'0A'         COMBINED STATIONS ELT?                       
         BNE   SCAN0010                                                         
         MVI   STYP,X'02'          SET UP STATION KEY                           
         MVC   SORTREC+SSTATREP,RSTAKREP                                        
         MVC   SORTREC+SSTATSTA(5),2(R2)    SET UP COMBO STATION                
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         XC    SORTREC,SORTREC                                                  
         AP    RECSSORT(5),=P'1'                                                
         B     SCAN0010            GO BACK FOR NEXT ELEMENT                     
         EJECT                                                                  
*                                                                               
*  IF REP CODE IS AN INTEREP LOCAL, MUST BE REPLACED BY MASTER                  
*     FILE CODE OF 'IR'                                                         
*        R2  = A(REP CODE IN KEY)                                               
*        R3  = A(REP CODE IN SORT RECORD, TO BE REPLACED)                       
*                                                                               
MASTTEST NTR1                                                                   
         CLI   MASTOPT,C'Y'          INTEREP RUN?                               
         BNE   MT099                 NO  - EXIT                                 
         MVC   0(2,R3),LIVEREP       YES - INSERT REP CODE                      
MT099    B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*  SCAN REP TABLE.  SET R7 TO A(REPCODE) IF CODE IS FOUND, OR IF                
*       DEFAULT ENTRY IS ENCOUNTERED (LAST ALLOWABLE ENTRY).  IF                
*       NO DEFAULT, SET 'REPFOUND' = NO!                                        
*                                                                               
*       R7  =  A(REPCODE IN TABLE ENTRY)                                        
*       R2  =  A(REPCODE IN TAPE RECORD)                                        
*                                                                               
FINDREP  MVI   REPFOUND,C'Y'       PRESET FLAG                                  
         CLC   0(2,R7),0(R2)       CHECK CURRENT POSITION                       
         BER   R8                                                               
         LA    R7,REPNTRY          RESET TO TOP OF TABLE                        
FR010    CLC   0(2,R7),0(R2)       CHECK REP CODE                               
         BER   R8                  FOUND - RETURN                               
         CLC   0(2,R7),=C'00'      END OF TABLE?                                
         BNE   FR016               NO                                           
         MVI   REPFOUND,C'N'       SET 'NO REP' FLAG                            
         BR    R8                  RETURN                                       
FR016    LA    R7,LREPNTRY(R7)     BUMP TABLE                                   
         B     FR010               GO BACK FOR NEXT ENTRY                       
         EJECT                                                                  
*              CREATE THE OUTPUT TAPE                                           
         SPACE 1                                                                
PHASE2   OPEN  (IN,(INPUT))                                                     
         CLI   RUNOPT,C'S'         SOFT RUN?                                    
         BE    P0000               YES - DON'T OPEN OUTPUT                      
         OPEN  (OUT,(OUTPUT))                                                   
P0000    EQU   *                                                                
         XC    SORTREC,SORTREC                                                  
*                                                                               
         CLI   TESTYN,C'N'         TRACE TEST?                                  
         BE    GETR2                                                            
         MVC   P+1(15),=C'PHASE 2 STARTED'                                      
         GOTO1 PRINTER                                                          
*                                                                               
GETR2    GET   IN,REC-4                                                         
         LH    RE,REC-4                                                         
         LA    RE,REC-4(RE)                                                     
         MVI   0(RE),0                                                          
         LA    R5,REC                                                           
         AP    RECIN(5),=P'1'                                                   
         SPACE 1                                                                
         SPACE 1                                                                
         CLI   RSALKTYP,X'06'                                                   
         BE    SALREC                                                           
         CLI   RSALKTYP,X'46'      SECOND S/P RECORD                            
         BE    SALREC              SAME CODE TO CHECK FOR PURGE                 
         CLI   RAGYKTYP,X'0A'      FIRST AGENCY RECORD                          
         BE    AGYREC                                                           
         CLI   RAGK2TYP,X'1A'      SECOND AGENCY RECORD                         
         BE    AGYREC2                                                          
         CLI   RADVKTYP,X'08'                                                   
         BE    ADVREC                                                           
         CLI   RPRDKTYP,X'09'                                                   
         BE    PRDREC                                                           
         CLI   RSTAKTYP,X'02'                                                   
         BE    STAREC                                                           
         CLI   RCTGKTYP,X'0F'                                                   
         BE    CTGREC                                                           
         CLI   RPTPKTYP,X'31'                                                   
         BE    PTPREC                                                           
         B     PUT                                                              
         EJECT                                                                  
*              DELETE SALESPERSON RECORDS                                       
         SPACE 1                                                                
SALREC   LA    R2,RSALKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         AP    DSALTOT(5,R7),=P'1'   REP'S TOTAL SALESPERSONS                   
         CLI   DSAL(R7),C'Y'       SALESPERSONS PURGED FOR REP?                 
         BNE   PUT                 NO  - KEEP ALL RECORDS                       
         MVC   WORK(1),RSALKTYP                                                 
         MVC   WORK+1(2),RSALKREP                                               
         MVC   WORK+3(3),RSALKSAL                                               
         SPACE 1                                                                
SALNXT   CLC   SORTREC(6),WORK     COMPARE SALESPERSON                          
         BL    SALSRT                                                           
         BH    DELETE                                                           
         B     PUT                                                              
         SPACE 1                                                                
SALSRT   BAS   R8,GETSORT                                                       
         B     SALNXT                                                           
         EJECT                                                                  
*              DELETE AGENCY RECORDS                                            
         SPACE 1                                                                
AGYREC   EQU   *                                                                
         CLC   =C'0000',RAGYKAGY   AGENCY = CONTROL RECORD                      
*                                     FOR AUTO-ASSIGNMENT?                      
         BE    PUT                 YES - KEEP THE RECORD                        
*                                                                               
         LA    R2,RAGYKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         AP    DAGYTOT(5,R7),=P'1'   REP'S TOTAL AGENCIES                       
         CLI   DAGY(R7),C'Y'       AGENCIES PURGED FOR REP?                     
         BNE   PUT                 NO  - KEEP ALL RECORDS                       
         MVC   WORK(1),RAGYKTYP                                                 
         MVC   WORK+1(4),RAGYKAGY                                               
         MVC   WORK+5(2),RAGYKAOF                                               
         MVC   WORK+7(2),RAGYKREP                                               
         SPACE 1                                                                
AGYNXT   CLC   SORTREC(9),WORK     COMPARE AGENIES                              
         BL    AGYSRT                                                           
         BH    DELETE                                                           
         B     PUT                                                              
         SPACE 1                                                                
AGYSRT   BAS   R8,GETSORT                                                       
         B     AGYNXT                                                           
         EJECT                                                                  
*              DELETE AGENCY2 RECORDS                                           
         SPACE 1                                                                
AGYREC2  EQU   *                                                                
         CLC   =C'0000',RAGK2AGY   AGENCY = CONTROL RECORD                      
*                                     FOR AUTO-ASSIGNMENT?                      
         BE    PUT                 YES - KEEP THE RECORD                        
*                                                                               
         LA    R2,RAGK2REP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         AP    DAGYTOT(5,R7),=P'1'   REP'S TOTAL AGENCIES                       
         CLI   DAGY(R7),C'Y'       AGENCIES PURGED FOR REP?                     
         BNE   PUT                 NO  - KEEP ALL RECORDS                       
         MVC   WORK(1),RAGK2TYP                                                 
         MVC   WORK+1(4),RAGK2AGY                                               
         MVC   WORK+5(2),RAGK2AOF                                               
         MVC   WORK+7(2),RAGK2REP                                               
         SPACE 1                                                                
AGYNXT2  CLC   SORTREC(9),WORK     COMPARE AGENIES                              
         BL    AGYSRT2                                                          
         BH    DELETE                                                           
         B     PUT                                                              
         SPACE 1                                                                
AGYSRT2  BAS   R8,GETSORT                                                       
         B     AGYNXT2                                                          
         EJECT                                                                  
*              DELETE ADVERTISER RECORDS                                        
         SPACE 1                                                                
ADVREC   EQU   *                                                                
         CLC   =C'0000',RADVKADV   ADVERT = CONTROL RECORD                      
*                                     FOR AUTO-ASSIGNMENT?                      
         BE    PUT                 YES - KEEP THE RECORD                        
*                                                                               
         LA    R2,RADVKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         AP    DADVTOT(5,R7),=P'1'   REP'S TOTAL ADVERTISERS                    
         CLI   DADV(R7),C'Y'       ADVERTISERS PURGED FOR REP?                  
         BNE   PUT                 NO  - KEEP ALL RECORDS                       
         MVC   WORK(1),RADVKTYP                                                 
         MVC   WORK+1(4),RADVKADV                                               
         MVC   WORK+5(2),RADVKREP                                               
         SPACE 1                                                                
ADVNXT   CLC   SORTREC(7),WORK     COMPARE ADVERTISERS                          
         BL    ADVSRT                                                           
         BH    DELETE                                                           
         B     PUT                                                              
         SPACE 1                                                                
ADVSRT   BAS   R8,GETSORT                                                       
         B     ADVNXT                                                           
         EJECT                                                                  
*              DELETE PRODUCT RECORDS                                           
         SPACE 1                                                                
PRDREC   LA    R2,RPRDKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         AP    DPRODTOT(5,R7),=P'1'  REP'S TOTAL PRODUCTS                       
         CLI   DPROD(R7),C'Y'      PRODUCTS PURGED FOR REP?                     
         BNE   PUT                 NO  - KEEP ALL RECORDS                       
         MVC   WORK(1),RPRDKTYP                                                 
         MVC   WORK+1(4),RPRDKADV                                               
         MVC   WORK+5(3),RPRDKPRD                                               
         MVC   WORK+8(2),RPRDKREP                                               
         SPACE 1                                                                
PRDNXT   CLC   SORTREC(10),WORK    COMPARE PRODUCTS                             
         BL    PRDSRT                                                           
         BH    DELETE                                                           
         B     PUT                                                              
         SPACE 1                                                                
PRDSRT   BAS   R8,GETSORT                                                       
         B     PRDNXT                                                           
         EJECT                                                                  
*              DELETE STATION RECORDS                                           
         SPACE 1                                                                
STAREC   LA    R2,RSTAKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         AP    DSTATTOT(5,R7),=P'1'  REP'S TOTAL STATIONS                       
         CLI   DSTAT(R7),C'Y'      STATIONS PURGED FOR REP?                     
         BNE   PUT                 NO  - KEEP ALL RECORDS                       
         MVC   WORK(1),RSTAKTYP                                                 
         MVC   WORK+1(2),RSTAKREP                                               
         MVC   WORK+3(5),RSTAKSTA                                               
         SPACE 1                                                                
STANXT   CLC   SORTREC(08),WORK    COMPARE STATIONS                             
         BL    STASRT                                                           
         BH    DELETE                                                           
         B     PUT                                                              
         SPACE 1                                                                
STASRT   BAS   R8,GETSORT                                                       
         B     STANXT                                                           
         EJECT                                                                  
*              DELETE CATEGORY RECORDS                                          
         SPACE 1                                                                
CTGREC   LA    R2,RCTGKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         AP    DCTGTOT(5,R7),=P'1' REP'S TOTAL CATEGORY CODES                   
         CLI   DCTG(R7),C'Y'       CATEGORY CODE PURGED FOR REP?                
         BNE   PUT                 NO  - KEEP ALL RECORDS                       
         MVC   WORK(1),RCTGKTYP                                                 
         MVC   WORK+1(2),RCTGKREP                                               
         MVC   WORK+3(2),RCTGKCTG                                               
         SPACE 1                                                                
CTGNXT   CLC   SORTREC(05),WORK    COMPARE CATEGORY CODES                       
         BL    CTGSRT                                                           
         BH    DELETE                                                           
         B     PUT                                                              
         SPACE 1                                                                
CTGSRT   BAS   R8,GETSORT                                                       
         B     CTGNXT                                                           
         EJECT                                                                  
*              DELETE POINT PERSON RECORDS                                      
         SPACE 1                                                                
PTPREC   LA    R2,RPTPKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         AP    DPTPTOT(5,R7),=P'1' REP'S TOTAL POINT PERSON CODES               
         CLI   DPTP(R7),C'Y'       POINT PERSON CODE PURGED FOR REP?            
         BNE   PUT                 NO  - KEEP ALL RECORDS                       
         MVC   WORK(1),RPTPKTYP                                                 
         MVC   WORK+1(2),RPTPKREP                                               
         MVC   WORK+3(3),RPTPKREC                                               
         SPACE 1                                                                
PTPNXT   CLC   SORTREC(06),WORK    COMPARE POINT PERSON CODES                   
         BL    PTPSRT                                                           
         BH    DELETE                                                           
         B     PUT                                                              
         SPACE 1                                                                
PTPSRT   BAS   R8,GETSORT                                                       
         B     PTPNXT                                                           
         EJECT                                                                  
*              PUT OUT NEW RECORD                                               
         SPACE 1                                                                
DELETE   LA    R3,DSALDEL(R7)                                                   
         CLI   RSALKTYP,X'06'                                                   
         BE    SALPRT                                                           
         CLI   RSALKTYP,X'46'                                                   
         BE    SALPRT2                                                          
         LA    R3,DAGYDEL(R7)                                                   
         CLI   RAGYKTYP,X'0A'      FIRST AGENCY RECORD                          
         BE    AGYPRT                                                           
         LA    R3,DAGYDEL(R7)                                                   
         CLI   RAGYKTYP,X'1A'      SECOND AGENCY RECORD                         
         BE    AGYPRT2                                                          
         LA    R3,DADVDEL(R7)                                                   
         CLI   RADVKTYP,X'08'                                                   
         BE    ADVPRT                                                           
         LA    R3,DPRODDEL(R7)                                                  
         CLI   RPRDKTYP,X'09'                                                   
         BE    PRDPRT                                                           
         LA    R3,DSTATDEL(R7)                                                  
         CLI   RSTAKTYP,X'02'                                                   
         BE    STAPRT                                                           
         LA    R3,DCTGDEL(R7)                                                   
         CLI   RCTGKTYP,X'0F'                                                   
         BE    CTGPRT              PRINT RECORD'S DETAIL                        
         LA    R3,DPTPDEL(R7)                                                   
         CLI   RPTPKTYP,X'31'                                                   
         BE    PTPPRT              PRINT RECORD'S DETAIL                        
         DC    H'0'                                                             
         SPACE 2                                                                
*                                                                               
*  SET UP SALESPERSON RECORD PRINTOUT                                           
*                                                                               
SALPRT   MVC   P+1(11),=C'SALESPERSON'                                          
         MVC   P+15(2),RSALKREP                                                 
         MVC   P+20(3),RSALKSAL                                                 
         GOTO1 PRINTER                                                          
         B     DEL1                                                             
         SPACE 2                                                                
*                                                                               
*  SET UP SALESPERSON RECORD PRINTOUT                                           
*                                                                               
SALPRT2  MVC   P+1(12),=C'SALESPERSON2'                                         
         MVC   P+15(2),RSALKREP                                                 
         MVC   P+20(3),RSALKSAL                                                 
         GOTO1 PRINTER                                                          
         B     DEL1                                                             
         SPACE 2                                                                
*                                                                               
*  SET UP AGENCY RECORD PRINTOUT                                                
*                                                                               
AGYPRT   MVC   P+1(11),=C'AGENCY     '                                          
         MVC   P+15(4),RAGYKAGY                                                 
         MVC   P+20(2),RAGYKAOF                                                 
         MVC   P+25(2),RAGYKREP                                                 
         GOTO1 PRINTER                                                          
         B     DEL1                                                             
         SPACE 2                                                                
*                                                                               
*  SET UP AGENCY2 RECORD PRINTOUT                                               
*                                                                               
AGYPRT2  MVC   P+1(11),=C'AGENCY2    '                                          
         MVC   P+15(4),RAGK2AGY                                                 
         MVC   P+20(2),RAGK2AOF                                                 
         MVC   P+25(2),RAGK2REP                                                 
         GOTO1 PRINTER                                                          
         B     DEL1                                                             
         SPACE 2                                                                
*                                                                               
*  SET UP ADVERTISER RECORD PRINTOUT                                            
*                                                                               
ADVPRT   MVC   P+1(11),=C'ADVERTISER '                                          
         MVC   P+15(4),RADVKADV                                                 
         MVC   P+25(2),RADVKREP                                                 
         GOTO1 PRINTER                                                          
         B     DEL1                                                             
         SPACE 2                                                                
*                                                                               
*  SET UP PRODUCT RECORD PRINTOUT                                               
*                                                                               
PRDPRT   MVC   P+1(11),=C'PRODUCT    '                                          
         MVC   P+15(4),RPRDKADV                                                 
         MVC   P+20(3),RPRDKPRD                                                 
         MVC   P+25(2),RPRDKREP                                                 
         GOTO1 PRINTER                                                          
         B     DEL1                                                             
         SPACE 2                                                                
*                                                                               
*  SET UP STATION RECORD PRINTOUT                                               
*                                                                               
STAPRT   MVC   P+1(11),=C'STATION    '                                          
         MVC   P+15(2),RSTAKREP                                                 
         MVC   P+20(5),RSTAKSTA                                                 
         GOTO1 PRINTER                                                          
         B     DEL1                                                             
         SPACE 2                                                                
*                                                                               
*  SET UP CATEGORY CODE RECORD PRINTOUT                                         
*                                                                               
CTGPRT   MVC   P+1(11),=C'CATEGORY   '                                          
         MVC   P+15(2),RCTGKREP                                                 
         MVC   P+20(2),RCTGKCTG                                                 
         GOTO1 PRINTER                                                          
         B     DEL1                                                             
         SPACE 2                                                                
*                                                                               
*  SET UP POINT PERSON CODE RECORD PRINTOUT                                     
*                                                                               
PTPPRT   MVC   P+1(11),=C'PT PERSON  '                                          
         MVC   P+15(2),RPTPKREP                                                 
         MVC   P+20(3),RPTPKREC                                                 
         GOTO1 PRINTER                                                          
         B     DEL1                                                             
         SPACE 2                                                                
DEL1     AP    0(5,R3),=P'1'                                                    
         AP    RECPURG,=P'1'       COUNT OF TOTAL RECORDS PURGED                
         B     GETR2               DELETED RECORD DROPPED                       
         SPACE 1                                                                
PUT      AP    RECOUT,=P'1'        COUNT OF TOTAL RECORDS WRITTEN               
         CLI   RUNOPT,C'S'         SOFT RUN?                                    
         BE    GETR2               YES - SKIP OUTPUT STEP                       
         PUT   OUT,REC-4                                                        
         B     GETR2                                                            
         SPACE 1                                                                
END      EQU   *                                                                
         MVC   P+1(17),=C'PHASE X COMPLETED'                                    
         MVC   P+07(1),PHASE                                                    
         GOTO1 PRINTER                                                          
*                                                                               
         CLOSE (IN,)                                                            
         CLI   PHASE,C'2'                                                       
         BE    CLSOUT                                                           
         MVI   PHASE,C'2'                                                       
         B     PHASE2                                                           
CLSOUT   EQU   *                                                                
         CLI   RUNOPT,C'S'         SOFT RUN?                                    
         BE    CLSOUT2             YES - DON'T CLOSE OUTPUT                     
         CLOSE (OUT,)                                                           
CLSOUT2  EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     PRNTCNTS                                                         
         SPACE 2                                                                
GETSORT  CLI   STYP,X'FF'                                                       
         BER   R8                                                               
         MVI   STYP,X'FF'                                                       
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     RE,DMCB+4                                                        
         LTR   RE,RE                                                            
         BZR   R8                                                               
         MVC   SORTREC,0(RE)                                                    
         BR    R8                                                               
         EJECT                                                                  
*              PRINT OUT THE ACCUMULATORS                                       
         SPACE 1                                                                
PRNTCNTS MVI   SPACING+3,C'3'                                                   
         GOTO1 PRINTER                                                          
         MVC   P+1(10),=C'RECORDS IN'                                           
         LA    R3,RECIN                                                         
         MVI   SPACING+3,C'2'                                                   
         BAS   R8,EDTP2                                                         
         SPACE 1                                                                
         MVC   P+1(11),=C'RECORDS OUT'                                          
         LA    R3,RECOUT                                                        
         BAS   R8,EDTP2                                                         
         SPACE 1                                                                
         MVI   SPACING+3,C'1'                                                   
         MVC   P+1(14),=C'RECORDS SORTED'                                       
         LA    R3,RECSSORT                                                      
         BAS   R8,EDTP2                                                         
         GOTO1 PRINTER                                                          
         MVI   P+1,C'-'                                                         
         MVC   P+2(13),P+1                                                      
         GOTO1 PRINTER                                                          
         SPACE 1                                                                
         LA    R7,REPNTRY          SET A(1ST TABLE ENTRY)                       
PRNTBCKT CLC   0(2,R7),=C'00'      END OF TABLE?                                
         BE    PRNTC10             YES - WRAP IT UP                             
         MVC   P+1(6),=C'REP=  '                                                
         MVC   P+8(02),0(R7)       PRINT REP CODE                               
         MVC   P+11(NUMNTRY),2(R7) PRINT 'Y/N' FLAGS                            
         GOTO1 PRINTER                                                          
         LA    R3,DSALDEL(R7)      POINT TO FIRST BUCKET                        
         LA    R4,PURGED                                                        
         LA    R2,NUMNTRY                                                       
         SPACE 1                                                                
PRNTC2   MVC   P+2(36),0(R4)       MOVE IN LINE LITERAL                         
         BAS   R8,EDTP                                                          
         LA    R4,L'PURGED(R4)                                                  
         LA    R3,DELSIZE(R3)      BUMP A(DELETE BUCKET)                        
         BCT   R2,PRNTC2                                                        
         LA    R7,LREPNTRY(R7)     BUMP TABLE ENTRY                             
         B     PRNTBCKT            CHECK NEXT TABLE ENTRY                       
         SPACE 1                                                                
PRNTC10  MVC   P+1(12),=C'TOTAL PURGED'                                         
         LA    R3,RECPURG                                                       
         BAS   R8,EDTP2                                                         
         B     ENDJOB                                                           
         SPACE 1                                                                
EDTP     EQU   *                                                                
         EDIT  (P5,5(R3)),(10,P+20),COMMAS=YES,ZERO=NOBLANK                     
EDTP2    EQU   *                                                                
         EDIT  (P5,0(R3)),(10,P+40),COMMAS=YES,ZERO=NOBLANK                     
         GOTO1 PRINTER                                                          
         BR    R8                                                               
         SPACE 1                                                                
ENDJOB   XBASE                                                                  
         EJECT                                                                  
*              CONSTANTS AND WORKING STORAGE                                    
         SPACE 1                                                                
DMCB     DC    6F'0'                                                            
WORK     DC    CL80' '                                                          
BYTE     DC    X'0'                                                             
FULL     DC    F'0'                                                             
DUB      DC    D'0'                                                             
HALF     DC    H'0'                                                             
*                                                                               
*  MASTER CODE TABLE (ASSUME MASTER POWER CODE = XX)                            
*       NOTE:  THIS TABLE CONTROLS WHICH REP CODES ARE ASSOCIATED               
*       WITH MASTER'S MASTER CODE SCHEME, WHERE ALL 'LOCALS' USE                
*       A SINGLE CODE UNDER REP CODE 'XX'.  THE ADDITION OR REMOVAL             
*       OF A REP CODE MUST BE REFLECTED IN THIS TABLE.  FAILURE TO              
*       DO SO CAN HAVE UNPLEASANT RESULTS.                                      
*                                                                               
*       SPECIAL NOTE:  THE TABLE SERVES TWO FUNCTIONS: FIRST, TO                
*       ENSURE THAT CODES ARE VALIDATED WITHIN THE MASTER CODE.                 
*       SECOND, TO GENERATE ENTRIES INTO THE TABLE FOR A MASTER                 
*       RUN.  TO ENSURE THAT CODES 'IR/K3/MR/V6' ARE NOT ENTERED                
*       TWICE, THE CODES FOR IR/K3/MR/V6 ARE SET AT A SEPARATE LABEL            
*       BEFORE THE MAIN SECTION OF THE TABLE.                                   
*                                                                               
*        TABLE IS SET IN TWO SECTIONS:                                          
*           BEGINNING WITH IREPIR, TABLE SERVES TO IDENTIFY ALL                 
*        SUBSIDIARY CODES OF MASTER REPS.  ENTIRE TABLE FOR THIS                
*        PURPOSE IS DELIMITED BY CL'0000'.                                      
*        FOUR SUBTABLES ARE WITHIN THE IREPIR TABLE: IREP,                      
*        KATZRREP (KATZ RADIO), KATZTREP (KATZ TV), AND NPTVREP                 
*        (NATIONAL PUBLIC TV/RADIO), KATZTREP (KATZ TV).  THESE                 
*        TABLES SERVE TO INSERT LOCAL CODES FOR THE INDIVIDUAL                  
*        MASTERS, AND ARE DELIMITED BY X'FFFF'                                  
*                                                                               
*                                                                               
IREPIR   DC    CL2'IR'             INTEREP MASTER                               
         DC    CL2'K3'             KATZ RADIO MASTER                            
         DC    CL2'MR'             KATZ TV MASTER                               
         DC    CL2'V6'             NATIONAL PUBLIC MASTER                       
IREP     DS    0CL2                INTEREP MASTER REP CODE                      
*                                  REPLACEMENT TABLE                            
         DC    CL2'TO'             TORBET                                       
         DC    CL2'I1'             MAJOR MARKET RADIO                           
         DC    CL2'MG'             MCGAVERN GUILD                               
         DC    CL2'D4'             D AND R RADIO                                
         DC    CL2'GP'             GROUP W                                      
         DC    CL2'I2'             NON-REP                                      
         DC    CL2'I8'             CABALLERO                                    
         DC    CL2'I9'             SCHUBERT                                     
         DC    CL2'RM'             RADIO MARKETING SPECIALISTS                  
         DC    CL2'IF'             INFINITY RADIO                               
         DC    CL2'S1'             SHAMROCK                                     
         DC    CL2'CM'             CONCERT MUSIC                                
         DC    CL2'CN'             CLEAR CHANNEL                                
         DC    CL2'AQ'             ALLIED RADIO PARTNERS                        
         DC    CL2'L7'             CABALLERO TV                                 
         DC    CL2'IB'             ABC RADIO                                    
         DC    CL2'NX'             PUBLIC RADIO                                 
         DC    CL2'UO'             CUMULUS RADIO                                
         DC    XL2'FFFF'           IREP DELIMITER                               
KATZRREP EQU   *                   KATZ RADIO MASTER TABLE                      
         DC    CL2'BF'             BANNER                                       
         DC    CL2'K4'             DIMENSIONS                                   
         DC    CL2'KU'             KATZ RADIO                                   
         DC    CL2'KF'             KATZ HISPANIC                                
         DC    CL2'EA'             EASTMAN                                      
         DC    CL2'CR'             CHRISTAL                                     
         DC    CL2'S3'             SENTRY                                       
         DC    CL2'RS'             ABC                                          
         DC    CL2'QD'             SPECTRUM                                     
         DC    CL2'NU'             CLEAR CHANNEL                                
         DC    CL2'G8'             INTERACTIVE MEDIA                            
         DC    CL2'J0'             DEDICATED   MEDIA                            
         DC    CL2'WC'             WEST SIDE   MEDIA                            
         DC    XL2'FFFF'           DELIMITER                                    
KATZTREP EQU   *                   KATZ TV MASTER TABLE                         
         DC    CL2'AM'             KATZ TV AMERICAN                             
         DC    CL2'CQ'             KATZ TV CONTINENTAL                          
         DC    CL2'NK'             KATZ TV NATIONAL                             
         DC    CL2'8K'             KATZ TV SYNDICATED                           
         DC    XL2'FFFF'           DELIMITER                                    
NPTVREP  EQU   *                   NATIONAL PUBLIC MASTER TABLE                 
         DC    CL2'NP'             NATIONAL PUBLIC TV                           
         DC    CL2'V5'             NATIONAL PUBLIC RADIO                        
         DC    XL2'FFFF'           DELIMITER                                    
         DC    CL2'00'             DELIMITER                                    
         SPACE 3                                                                
ADDAY    DC    V(ADDAY)                                                         
DATCON   DC    V(DATCON)                                                        
GETBROAD DC    V(GETBROAD)                                                      
GETDAY   DC    V(GETDAY)                                                        
RECUP    DC    V(RECUP)                                                         
PRINTER  DC    V(PRINTER)                                                       
PRNTBL   DC    V(PRNTBL)                                                        
CARDS    DC    V(CARDS)                                                         
DATVAL   DC    V(DATVAL)                                                        
SORTER   DC    V(SORTER)                                                        
         SPACE 1                                                                
RECIN    DC    PL5'0'                                                           
RECOUT   DC    PL5'0'                                                           
RECPURG  DC    PL5'0'                                                           
RECSSORT DC    PL5'0'                                                           
PURGED   DC    CL36'SALESPERSN TOTAL              PURGED'                       
         DC    CL36'AGENCIES   TOTAL              PURGED'                       
         DC    CL36'ADVRTISERS TOTAL              PURGED'                       
         DC    CL36'PRODUCTS   TOTAL              PURGED'                       
         DC    CL36'STATIONS   TOTAL              PURGED'                       
         DC    CL36'CATEGORIES TOTAL              PURGED'                       
         DC    CL36'PT PERSONS TOTAL              PURGED'                       
         SPACE 1                                                                
REPCTR   DC    PL2'00'                                                          
THISREP  DC    F'0'                                                             
NEXTREP  DC    F'0'                                                             
         SPACE 1                                                                
REPNTRY  EQU   *                                                                
REPCDE   DC    CL2'00'                                                          
SAL      DC    CL1'N'                                                           
AGY      DC    CL1'N'                                                           
ADV      DC    CL1'N'                                                           
PROD     DC    CL1'N'                                                           
STAT     DC    CL1'N'                                                           
CTG      DC    CL1'N'                                                           
PTP      DC    CL1'N'                                                           
NUMNTRY  EQU   *-SAL                                                            
SALDEL   DC    PL5'0'                                                           
SALTOT   DC    PL5'0'                                                           
AGYDEL   DC    PL5'0'                                                           
AGYTOT   DC    PL5'0'                                                           
ADVDEL   DC    PL5'0'                                                           
ADVTOT   DC    PL5'0'                                                           
PRODDEL  DC    PL5'0'                                                           
PRODTOT  DC    PL5'0'                                                           
STATDEL  DC    PL5'0'                                                           
STATTOT  DC    PL5'0'                                                           
CTGDEL   DC    PL5'0'                                                           
CTGTOT   DC    PL5'0'                                                           
PTPDEL   DC    PL5'0'                                                           
PTPTOT   DC    PL5'0'                                                           
DELSIZE  EQU   ADVDEL-AGYDEL       SIZE OF DELETE COUNTER                       
LREPNTRY EQU   *-REPNTRY                                                        
TABSIZE  EQU   (50*LREPNTRY)+2                                                  
MOREREPS DC    (TABSIZE)X'00'            TABLE SIZE= 51 ENTRIES                 
DREP     EQU   0                                                                
DSAL     EQU   SAL-REPNTRY                                                      
DAGY     EQU   AGY-REPNTRY                                                      
DADV     EQU   ADV-REPNTRY                                                      
DPROD    EQU   PROD-REPNTRY                                                     
DSTAT    EQU   STAT-REPNTRY                                                     
DCTG     EQU   CTG-REPNTRY                                                      
DPTP     EQU   PTP-REPNTRY                                                      
DSALDEL  EQU   SALDEL-REPNTRY                                                   
DAGYDEL  EQU   AGYDEL-REPNTRY                                                   
DADVDEL  EQU   ADVDEL-REPNTRY                                                   
DPRODDEL EQU   PRODDEL-REPNTRY                                                  
DSTATDEL EQU   STATDEL-REPNTRY                                                  
DCTGDEL  EQU   CTGDEL-REPNTRY                                                   
DPTPDEL  EQU   PTPDEL-REPNTRY                                                   
DSALTOT  EQU   SALTOT-REPNTRY                                                   
DAGYTOT  EQU   AGYTOT-REPNTRY                                                   
DADVTOT  EQU   ADVTOT-REPNTRY                                                   
DPRODTOT EQU   PRODTOT-REPNTRY                                                  
DSTATTOT EQU   STATTOT-REPNTRY                                                  
DCTGTOT  EQU   CTGTOT-REPNTRY                                                   
DPTPTOT  EQU   PTPTOT-REPNTRY                                                   
         SPACE 1                                                                
CARDIN   DS    CL80                                                             
         SPACE 2                                                                
SORTREC  DS    0CL10                                                            
STYP     DS    CL1                                                              
         DS    CL9                                                              
*                                                                               
         SPACE 1                                                                
*                                                                               
*  KEY BUILDING EQUATES: DISPLACEMENTS INTO SORTREC                             
*                                                                               
SSPREP   EQU   1                   SALESPERSON REP                              
SSPSLS   EQU   3                   SALESPERSON CODE                             
SAGYCODE EQU   1                   AGENCY CODE                                  
SAGYOFF  EQU   5                   AGENCY OFFICE CODE                           
SAGYREP  EQU   7                   AGENCY REP CODE                              
SADVCODE EQU   1                   ADVERTISER CODE                              
SADVREP  EQU   5                   ADVERTISER REP CODE                          
SPRODADV EQU   1                   PRODUCT ADVERTISER CODE                      
SPRODPRD EQU   5                   PRODUCT PRODUCT CODE                         
SPRODREP EQU   8                   PRODUCT REP CODE                             
SSTATREP EQU   1                   STATION REP                                  
SSTATSTA EQU   3                   STATION CALL LETTERS                         
SCTGREP  EQU   1                   CATEGORY REP CODE                            
SCTGCODE EQU   3                   CATEGORY CODE                                
SPTPREP  EQU   1                   POINT PERSON REP CODE                        
SPTPCODE EQU   3                   POINT PERSON CODE                            
         SPACE 3                                                                
PHASE    DC    C'2'                                                             
KEEPSW   DS    CL1                                                              
RUNOPT   DC    C'S'                RUN-TIME OPTION: DEFAULT = SOFT              
MASTOPT  DC    C'Y'                INTEREP SELECTION OPTION                     
REPFOUND DC    C'Y'                REP FOUND IN TABLE FLAG                      
LIVEREP  DS    CL2                 REP IN PROGRESS, IF MASTER                   
CARDCTR  DC    PL4'0'              CARD COUNTER                                 
*                                                                               
TESTCTR1 DC    PL4'0'                                                           
TESTCTR2 DC    PL4'0'                                                           
TESTCTR3 DC    PL4'0'                                                           
TESTCTR4 DC    PL4'0'                                                           
TESTCTR5 DC    PL4'0'                                                           
TESTCTR6 DC    PL4'0'                                                           
TESTCTR7 DC    PL4'0'                                                           
*                                                                               
TESTMAX  DC    PL4'100'                                                         
TESTYN   DC    C'N'                                                             
*                                                                               
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(1,10,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=10'                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
IN       DCB   DDNAME=IN,              DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04048,                                            X        
               MACRF=GM,                                               X        
               EODAD=END                                                        
         SPACE 2                                                                
OUT      DCB   DDNAME=OUT,             DOS SYS012                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04048,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=08200               X        
               MACRF=PM                                                         
         SPACE 2                                                                
         DS    D                                                                
REC      DS    6200C                                                            
         SPACE 1                                                                
         EJECT                                                                  
* DDDPRINT                                                                      
* REGENALL(X)                                                                   
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE REGENSAP                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'145REREPSAP  09/27/11'                                      
         END                                                                    
