*          DATA SET REREPCOM   AT LEVEL 109 AS OF 05/01/02                      
*          DATA SET REREPCOM   AT LEVEL 107 AS OF 09/21/95                      
*PHASE RECOMBA                                                                  
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
*ENTRY RECOM                                                                    
         TITLE 'REPPAK - SALESMAN/AGY/ADV/PROD/CTG COMPARE'                     
****************************************************************                
*  HISTORY OF CHANGES:                                         *                
*  SEP20/95 (BU ) --- INITIAL ENTRY                            *                
*                     ***  END TOMBSTONE  ***                  *                
****************************************************************                
*  ADDITIONAL BASE REGISTERS:  R9/R6                           *                
*  PURGE OF CATEGORY CODE CHECKS FOR THE EXISTENCE OF THE CAT  *                
*  CODE IN THE ADVERTISER, PRODUCT AND CONTRACT RECORDS.       *                
*  PURGE OF POINT PERSON CHECKS FOR THE EXISTENCE OF THE PTP   *                
*  CODE IN THE PRODUCT RECORD ONLY.                            *                
*                                                              *                
****************************************************************                
RECOM    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,RECOM,=V(REGSAVE),R9,R6                                        
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
         MVC   TITLE+10(38),=C'REP SAL/AGY/ADV/PRD/CTG/PTP COMP '               
         SPACE 2                                                                
         BAS   R8,TABINIT          INITIALIZE TABLE OF REP SELECTIONS           
         SPACE 1                                                                
NEXTCARD GOTO1 CARDS,DMCB,CARDIN,=C'RE00'                                       
         MVC   P+1(80),CARDIN                                                   
         GOTO1 PRINTER                                                          
         CLC   CARDIN(2),=C'/*'                                                 
         BNE   VALCARD                                                          
         BAS   RE,IREPCHEK         FOR INTEREP, GENERATE TABLE                  
         B     PROCESS                                                          
         SPACE 3                                                                
VALCARD  EQU   *                                                                
*              VALIDATE INPUT CARDS                                             
*                                                                               
*  RULES FOR INPUT CARDS                                                        
*  1.    INPUT CARDS MAY BE ENTERED FOR 1 OR MORE REPS                          
*  2.    FIRST CARD MUST BE 'CONTROL=XXXXXX', WHERE XXXXXX IS EITHER            
*        'TEST  ' (NO UPDATE) OR 'UPDATE' (UPDATE).                             
*  3.    SECOND CARD MUST BE 'INTEREP=XXX', WHERE XXX IS EITHER 'YES'           
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
         CP    CARDCTR(4),=P'2'    SECOND CARD MUST BE INTEREP                  
         BNE   VC006                                                            
         CLC   CARDIN(8),=C'INTEREP='                                           
         BNE   CARDERR8            NO INTEREP - TERMINATE                       
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
         BNE   VIREP002            NOT 'INTEREP' OPTION                         
         MVI   IREPOPT,C'Y'        SET INTEREP OPTION TO YES                    
         B     NEXTCARD                                                         
VIREP002 CLC   CARDIN+8(3),=C'NO '                                              
         BNE   CARDERR9            INTEREP OPTION NOT YES OR NO                 
         MVI   IREPOPT,C'N'        SET INTEREP OPTION TO NO                     
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
         CLI   IREPOPT,C'Y'        IS RUN FOR INTEREP & LOCALS?                 
         BNE   VR024               NO                                           
         CLC   CARDIN+04(2),=C'IR' YES - IS CODE INTEREP?                       
         BNE   CARDER10            NO  - ERROR                                  
         CP    REPCTR(2),=P'1'     ONLY SINGLE CODE FOR INTEREP                 
         BNE   CARDER12            SHOULD HAVE BEEN ENTERED                     
         B     VR030                                                            
VR024    EQU   *                                                                
         BAS   RE,VR0100           NOT INTEREP: VALIDATE REP                    
         BNZ   CARDER11            ERROR - INTEREP CODE FOUND                   
VR030    EQU   *                                                                
         L     R2,THISREP          LOAD A(REP IN TABLE)                         
         MVC   DREP(2,R2),CARDIN+4  SAVE INPUT - NO MORE VALIDATION             
         B     NEXTCARD                                                         
         SPACE 2                                                                
*                                                                               
*  OPTION IS TO PROCESS NON-INTEREP.  MUST CHECK TO ENSURE AN                   
*   INTEREP CODE IS NOT INCLUDED.                                               
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
CARDERR8 MVC   P+1(27),=C'NO INTEREP= FOR CARD INPUT:'                          
         B     CERROR                                                           
CARDERR9 MVC   P+1(28),=C'INTEREP= OPTION NOT CORRECT:'                         
         B     CERROR                                                           
CARDER10 MVC   P+1(35),=C'INTEREP RUN: ONLY CODE "IR" ALLOWED'                  
         B     CERROR                                                           
CARDER11 MVC   P+1(36),=C'NON-INTEREP RUN: INTEREP CODE FOUND!'                 
         B     CERROR                                                           
CARDER12 MVC   P+1(38),=C'INTEREP RUN: ONLY SINGLE ENTRY ALLOWED'               
         B     CERROR                                                           
CERROR   GOTO1 PRINTER                                                          
         GOTO1 PRINTER                                                          
         MVC   P+1(20),CARDIN                                                   
         GOTO1 PRINTER                                                          
         GOTO1 PRINTER                                                          
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
*  SET BASIC FLAGS.  PROPOGATE FIRST INITIALIZED TABLE ENTRY INTO               
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
IREPCHEK EQU   *                                                                
         NTR1                                                                   
         CLI   IREPOPT,C'Y'        INTEREP RUN?                                 
         BNE   IC099               NO                                           
         LA    R3,IREP             A(INTEREP LOCAL CODE TABLE)                  
IC004    EQU   *                                                                
         CLC   0(2,R3),=C'00'      END OF TABLE?                                
         BE    IC099               YES - FINISHED                               
         MVC   THISREP,NEXTREP     REPLACE CURRENT WITH NEXT                    
         L     R2,NEXTREP                                                       
         LA    R2,LREPNTRY(R2)     BUMP BY LENGTH                               
         ST    R2,NEXTREP                                                       
         L     R2,THISREP                                                       
         MVC   DREP(2,R2),0(R3)    MOVE CODE FROM IREP TO TABLE                 
         MVC   DSAL(NUMNTRY,R2),REPNTRY+2 LOAD OPTIONS FROM IR                  
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
         BAS   RE,IREPTEST         WILL REPLACE IF IR LOCAL                     
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
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
         BAS   RE,IREPTEST         WILL REPLACE IF IR LOCAL                     
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
         BAS   RE,IREPTEST         WILL REPLACE IF IR LOCAL                     
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
         MVC   SORTREC+SSPSLS(3),RCONSAL                                        
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
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
         BAS   RE,IREPTEST         WILL REPLACE IF IR LOCAL                     
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         XC    SORTREC,SORTREC                                                  
         AP    RECSSORT(5),=P'1'                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
GENSTA01 EQU   *                                                                
*                                                                               
*  IF REP CODE IS AN INTEREP LOCAL, MUST BE REPLACED BY MASTER                  
*     FILE CODE OF 'IR'                                                         
*        R2  = A(REP CODE IN KEY)                                               
*        R3  = A(REP CODE IN SORT RECORD, TO BE REPLACED)                       
*                                                                               
IREPTEST NTR1                                                                   
         CLI   IREPOPT,C'Y'          INTEREP RUN?                               
         BNE   IR099                 NO  - EXIT                                 
         MVC   0(2,R3),=C'IR'        YES - INSERT 'IR' REP CODE                 
IR099    B     EXXMOD                                                           
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
PHASE2   EQU   *                                                                
         CLI   RUNOPT,C'S'         SOFT RUN?                                    
         BE    P0000               YES - DON'T OPEN OUTPUT                      
P0000    EQU   *                                                                
         XC    SORTREC,SORTREC                                                  
*                                                                               
         CLI   TESTYN,C'N'         TRACE TEST?                                  
         BE    GETR2                                                            
         MVC   P+1(15),=C'PHASE 2 STARTED'                                      
         GOTO1 PRINTER                                                          
*                                                                               
GETR2    EQU   *                                                                
         BAS   R8,GETSORT                                                       
         CLI   STYP,X'FF'          END OF FILE ENCOUNTERED?                     
         BE    END                 YES                                          
         CLI   STYP,X'08'          ADVERT RECORD                                
         BE    AGYREC                                                           
         CLI   STYP,X'0A'          AGENCY RECORD                                
         BE    ADVREC                                                           
         CLI   STYP,X'09'          PROD   RECORD                                
         BE    PRDREC                                                           
         CLI   STYP,X'0F'          CATEGY RECORD                                
         BE    CTGREC                                                           
         CLI   STYP,X'31'          POINT  RECORD                                
         BE    PTPREC                                                           
         DC    H'0'                UNRECOGNIZED TYPE                            
*                                                                               
*              DELETE AGENCY RECORDS                                            
         EJECT                                                                  
         SPACE 1                                                                
AGYREC   EQU   *                                                                
         CLC   SORTREC,LASTSORT    PREVIOUSLY SEEN?                             
         BE    GETR2               YES - SKIP IT                                
         MVC   LASTSORT,SORTREC    NO  - SAVE AND CHECK                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'0A'           INSERT KEY                                   
         MVC   KEY+19(6),SORTREC+1                                              
         MVC   KEY+25(2),=C'IR'                                                 
         GOTO1 READ                READ THE KEY                                 
         CLC   KEY,KEYSAVE         KEY ON FILE?                                 
         BE    GETR2               YES - GO BACK AND DO NEXT                    
         MVC   P+1(06),=C'AGENCY'                                               
         MVC   P+10(10),SORTREC                                                 
         GOTO1 REPORT                                                           
         B     GETR2               GO BACK FOR NEXT                             
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
DELETE   EQU   *                                                                
         CLI   RSALKTYP,X'06'                                                   
         BE    SALPRT              PRINT RECORD'S DETAIL                        
         LA    R3,DAGYDEL(R7)                                                   
         CLI   RAGYKTYP,X'0A'                                                   
         BE    AGYPRT              PRINT RECORD'S DETAIL                        
         LA    R3,DADVDEL(R7)                                                   
         CLI   RADVKTYP,X'08'                                                   
         BE    ADVPRT              PRINT RECORD'S DETAIL                        
         LA    R3,DPRODDEL(R7)                                                  
         CLI   RPRDKTYP,X'09'                                                   
         BE    PRDPRT              PRINT RECORD'S DETAIL                        
         LA    R3,DSTATDEL(R7)                                                  
         CLI   RSTAKTYP,X'02'                                                   
         BE    STAPRT              PRINT RECORD'S DETAIL                        
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
END      CLOSE (IN,)                                                            
         CLI   PHASE,C'2'                                                       
         BE    CLSOUT                                                           
         MVI   PHASE,C'2'                                                       
         B     PHASE2                                                           
CLSOUT   EQU   *                                                                
         CLI   RUNOPT,C'S'         SOFT RUN?                                    
         BE    CLSOUT2             YES - DON'T CLOSE OUTPUT                     
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
*  INTEREP MASTER CODE TABLE                                                    
*       NOTE:  THIS TABLE CONTROLS WHICH REP CODES ARE ASSOCIATED               
*       WITH INTEREP'S MASTER CODE SCHEME, WHERE ALL 'LOCALS' USE               
*       A SINGLE CODE UNDER REP CODE 'IR'.  THE ADDITION OR REMOVAL             
*       OF A REP CODE MUST BE REFLECTED IN THIS TABLE.  FAILURE TO              
*       DO SO CAN HAVE UNPLEASANT RESULTS.                                      
*                                                                               
*       SPECIAL NOTE:  THE TABLE SERVES TWO FUNCTIONS: FIRST, TO                
*       ENSURE THAT NO INTEREP CODES ARE ENTERED FOR A NON-INTEREP              
*       RUN.  SECOND, TO GENERATE ENTRIES INTO THE TABLE FOR AN                 
*       INTEREP RUN.  TO ENSURE THAT CODE 'IR' IS NOT ENTERED                   
*       TWICE, THE CODE FOR IR IS SET AT A SEPARATE LABEL BEFORE THE            
*       MAIN SECTION OF THE TABLE.                                              
*                                                                               
IREPIR   DC    CL2'IR'                                                          
IREP     DS    0CL2                INTEREP MASTER REP CODE                      
*                                  REPLACEMENT TABLE                            
*        DC    CL2'TO'             TORBET                                       
*        DC    CL2'I1'             MAJOR MARKET RADIO                           
*        DC    CL2'MG'             MCGAVERN GUILD                               
*        DC    CL2'D4'             D AND R RADIO                                
*        DC    CL2'GP'             GROUP W                                      
*        DC    CL2'I2'             NON-REP                                      
*        DC    CL2'I8'             CABALLERO                                    
*        DC    CL2'I9'             SCHUBERT                                     
         DC    CL2'RM'             RADIO MARKETING SPECIALISTS                  
         DC    CL2'IF'             INFINITY RADIO                               
         DC    CL2'S1'             SHAMROCK                                     
         DC    CL2'CM'             CONCERT MUSIC                                
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
LASTSORT DS    CL10                                                             
*                                                                               
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
IREPOPT  DC    C'Y'                INTEREP SELECTION OPTION                     
REPFOUND DC    C'Y'                REP FOUND IN TABLE FLAG                      
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
         DS    D                                                                
REC      DS    1008C                                                            
         SPACE 1                                                                
IN       DCB   DDNAME=IN,              DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=01004,                                            X        
               MACRF=GM,                                               X        
               EODAD=END                                                        
         SPACE 2                                                                
OUT      DCB   DDNAME=OUT,             DOS SYS012                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=01004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=08200               X        
               MACRF=PM                                                         
         SPACE 2                                                                
         EJECT                                                                  
* DDDPRINT                                                                      
* REGENALLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE REGENALLD                                                      
         ORG   RECORD                                                           
       ++INCLUDE REGENPTP                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'109REREPCOM  05/01/02'                                      
         END                                                                    
