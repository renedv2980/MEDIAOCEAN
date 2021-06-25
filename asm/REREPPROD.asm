*          DATA SET REREPPROD  AT LEVEL 077 AS OF 05/01/02                      
*PHASE REPRODA,*                                                                
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
*ENTRY REPROD                                                                   
         TITLE 'REPPAK - PRODUCT LOCKOUT INITIALIZER   '                        
****************************************************************                
*  HISTORY OF CHANGES:                                         *                
*  JAN27/92 (BU ) --- INITIAL ENTRY                            *                
*                                                              *                
*  !!!!  PLEASE SEE NOTE APPENDED TO TABLE 'IREP' !!!!!!       *                
*                                                              *                
*                                                              *                
*                                                              *                
*                                                              *                
****************************************************************                
REPROD   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,REPROD,=V(REGSAVE),R9                                          
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
         MVC   TITLE+10(34),=C'REP PRODUCT LOCKOUT INITIALIZER   '              
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
         CLC   CARDIN(07),=C'PRODUCT'                                           
         BE    VALPROD                                                          
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
VALPROD  EQU   *                                                                
         MVI   DPROD(R7),C'Y'         FLAG REP/ADVERTISER PURGE                 
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
         MVC   DREP(2,R2),0(R3)      MOVE CODE FROM IREP TO TABLE               
         MVC   DPROD(1,R2),REPNTRY+2 LOAD OPTIONS FROM IR                       
         LA    R3,L'IREP(R3)         BUMP TABLE                                 
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
GETR1    GET   IN,REC-4                                                         
         LH    RE,REC-4            SET EOR MARKER                               
         LA    RE,REC-4(RE)                                                     
         MVI   0(RE),0                                                          
         LA    R5,REC                                                           
         CLI   TESTYN,C'N'         TRACE TEST?                                  
         BE    GETCON1             NO  - SKIP DISPLAY                           
         CP    TESTCTR1(4),TESTMAX MAX REACHED?                                 
         BH    GETCON1             YES  - DON'T DO ANYTHING                     
         AP    TESTCTR1(4),=P'1'   NO  - ADD AND DISPLAY                        
         MVC   P+1(06),=C'RECIN:'                                               
         MVC   P+9(27),REC                                                      
         GOTO1 PRINTER                                                          
         SPACE 1                                                                
GETCON1  CLI   RCONKTYP,X'0C'                                                   
         BH    END                                                              
         BL    GETR1                                                            
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
         BE    GETR1               NO  - GET NEXT RECORD                        
         CP    TESTCTR3(4),TESTMAX MAX REACHED?                                 
         BH    GETR1               YES  - GET NEXT RECORD                       
         AP    TESTCTR3(4),=P'1'   NO  - ADD AND DISPLAY                        
         MVC   P+1(06),=C'C NFD:'                                               
         MVC   P+9(27),REC                                                      
         GOTO1 PRINTER                                                          
         B     GETR1               GO BACK FOR NEXT RECORD                      
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
* BUILD PRODUCT KEYS                                                            
*                                                                               
BK006    CLI   DPROD(R7),C'Y'      COUNT PRODUCTS FOR REP?                      
         BNE   GETR1               NO                                           
*                                                                               
         BAS   RE,BLDPROD          YES - BUILD PRODUCT SORT RECORD              
*                                                                               
         B     GETR1                                                            
         EJECT                                                                  
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
         AP    RECSSORT(5),=P'1'                                                
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
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
PHASE2   OPEN  (IN,(INPUT))                                                     
         CLI   RUNOPT,C'S'         SOFT RUN?                                    
         BE    P0000               YES - DON'T OPEN OUTPUT                      
         OPEN  (OUT,(OUTPUT))                                                   
P0000    EQU   *                                                                
         XC    SORTREC,SORTREC                                                  
         SPACE 1                                                                
GETR2    GET   IN,REC-4                                                         
         LH    RE,REC-4                                                         
         LA    RE,REC-4(RE)                                                     
         MVI   0(RE),0                                                          
         LA    R5,REC                                                           
         AP    RECIN(5),=P'1'                                                   
         SPACE 1                                                                
         CLI   RPRDKTYP,X'09'                                                   
         BE    PRDREC                                                           
         B     PUT                                                              
         EJECT                                                                  
*   COUNT PRODUCT RECORDS                                                       
*      RECORDS ON SORT FILE REPRESENT CONTRACTS FOR EACH PROD CODE.             
*      FOR EACH MATCH, A COUNTER IS KEPT.  WHEN THE KEY OF THE SORT             
*      FILE EXCEEDS THAT OF THE PRODUCT RECORD ON TAPE, THE COUNT               
*      IS COMPLETE, AND THE FINAL COUNT IS INSERTED INTO THE PRODUCT            
*      RECORD BEFORE IT IS PUT ON THE OUTPUT TAPE.                              
*                                                                               
PRDREC   EQU   *                                                                
         CLI   TESTYN,C'N'         TRACE TEST?                                  
         BE    PRDREC01            NO  - SKIP DISPLAY                           
         CP    TESTCTR5(4),TESTMAX MAX REACHED?                                 
         BH    PRDREC01            YES - DON'T DO ANYTHING                      
         GOTO1 PRINTER             PRINT BLANK LINE BEFORE GROUP                
PRDREC01 EQU   *                                                                
         LA    R2,RPRDKREP         CHECK TABLE FOR REP CODE                     
         BAS   R8,FINDREP                                                       
         CLI   REPFOUND,C'N'       TEST FOUND FLAG                              
         BE    PUT                 REP NOT IN TABLE - KEEP RECORD               
         AP    DPRODTOT(5,R7),=P'1'  REP'S TOTAL PRODUCTS                       
         CLI   DPROD(R7),C'Y'      PRODUCTS COUNTED FOR REP?                    
         BNE   PUT                 NO  - JUST PUT IT OUT                        
         MVC   WORK(1),RPRDKTYP                                                 
         MVC   WORK+1(4),RPRDKADV                                               
         MVC   WORK+5(3),RPRDKPRD                                               
         MVC   WORK+8(2),RPRDKREP                                               
*                                                                               
         CLI   TESTYN,C'N'         TRACE TEST?                                  
         BE    PRDREC02            NO  - SKIP DISPLAY                           
         CP    TESTCTR5(4),TESTMAX MAX REACHED?                                 
         BH    PRDREC02            YES - DON'T DO ANYTHING                      
         MVC   P+1(08),=C'WORK:   '                                             
         MVC   P+12(10),WORK                                                    
         GOTO1 PRINTER                                                          
*                                                                               
PRDREC02 EQU   *                                                                
*                                                                               
         SR    R2,R2               INITIALIZE COUNTER                           
         SPACE 1                                                                
PRDNXT   CLC   SORTREC(10),WORK    COMPARE PRODUCTS                             
         BL    PRDSRT                                                           
         BH    PRDCTRIN            INSERT VALUE OF COUNTER                      
         LA    R2,1(R2)            ADD 1 TO COUNTER                             
         CLI   TESTYN,C'N'         TRACE TEST?                                  
         BE    PRDSRT              NO  - SKIP DISPLAY                           
         CP    TESTCTR5(4),TESTMAX MAX REACHED?                                 
         BH    PRDSRT              YES - DON'T DO ANYTHING                      
         MVC   P+1(08),=C'CON IN: '                                             
         MVC   P+12(10),SORTREC                                                 
         GOTO1 PRINTER                                                          
*                                                                               
PRDSRT   BAS   R8,GETSORT          GET NEXT CONTRACT INFO                       
         CLI   STYP,X'FF'          EOF ON CONTRACTS?                            
         BNE   PRDNXT              NO  - CONTINUE TO COUNT                      
*                                                                               
PRDCTRIN STCM  R2,3,RPRDLOCK       SAVE COUNTER IN RECORD                       
         CLI   TESTYN,C'N'         TRACE TEST?                                  
         BE    PRDCTR02            NO  - SKIP DISPLAY                           
         CP    TESTCTR5(4),TESTMAX MAX REACHED?                                 
         BH    PRDCTR02            YES - DON'T DO ANYTHING                      
         AP    TESTCTR5(4),=P'1'   ADD TO COUNTER                               
         MVC   P+1(10),=C'PROD OUT: '                                           
         MVC   P+12(36),RPRDELEM   '01' ELEM FROM O/P RECORD                    
         GOTO1 PRINTER             PRINT OUTPUT LINE                            
*                                                                               
PRDCTR02 EQU   *                                                                
         B     PUT                 OUTPUT THE RECORD                            
         EJECT                                                                  
*              PUT OUT NEW RECORD                                               
         SPACE 1                                                                
*                                                                               
*  SET UP PRODUCT RECORD PRINTOUT                                               
*                                                                               
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
         CLOSE (OUT,)                                                           
CLSOUT2  EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     PRNTCNTS                                                         
         SPACE 2                                                                
GETSORT  CLI   STYP,X'FF'                                                       
         BER   R8                                                               
         MVI   STYP,X'FF'                                                       
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZR   R8                                                               
         MVC   SORTREC,0(R6)                                                    
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
         DC    CL2'TO'             TORBET                                       
         DC    CL2'I1'             MAJOR MARKET RADIO                           
         DC    CL2'HN'             HNWH                                         
         DC    CL2'MG'             MCGAVERN GUILD                               
         DC    CL2'DI'             DURPETTI                                     
         DC    CL2'GP'             GROUP W                                      
         DC    CL2'I2'             NON-REP                                      
         DC    CL2'I8'             CABALLERO                                    
         DC    CL2'I9'             SCHUBERT                                     
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
         SPACE 1                                                                
REPCTR   DC    PL2'00'                                                          
THISREP  DC    F'0'                                                             
NEXTREP  DC    F'0'                                                             
         SPACE 1                                                                
REPNTRY  EQU   *                                                                
REPCDE   DC    CL2'00'                                                          
PROD     DC    CL1'N'                                                           
PRODDEL  DC    PL5'0'                                                           
PRODTOT  DC    PL5'0'                                                           
DELSIZE  EQU   *-PRODDEL           SIZE OF DELETE COUNTER                       
LREPNTRY EQU   *-REPNTRY                                                        
TABSIZE  EQU   (50*LREPNTRY)+2                                                  
MOREREPS DC    (TABSIZE)X'00'            TABLE SIZE= 51 ENTRIES                 
DREP     EQU   0                                                                
DPROD    EQU   PROD-REPNTRY                                                     
DPRODDEL EQU   PRODDEL-REPNTRY                                                  
DPRODTOT EQU   PRODTOT-REPNTRY                                                  
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
SPRODADV EQU   1                   PRODUCT ADVERTISER CODE                      
SPRODPRD EQU   5                   PRODUCT PRODUCT CODE                         
SPRODREP EQU   8                   PRODUCT REP CODE                             
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
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'077REREPPROD 05/01/02'                                      
         END                                                                    
