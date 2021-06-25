*          DATA SET REREP1K02S AT LEVEL 065 AS OF 05/01/02                      
*PHASE RE1K02C,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REREP1K02 - STRATEGY SEEDER'                                    
*********************************************************************           
*                                                                   *           
*        REREP1K02 --- STATEGY SEEDER                               *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY                                                    *           
*                                                                   *           
* MAR07/95 (BU ) --- INITIAL ENTRY - CLONED FROM REREP1B02          *           
*                                                                   *           
* MAR29/95 (BU ) --- FIX SHARE CALCULATION                          *           
*                                                                   *           
* JUN05/96 (BU ) --- KCVU PROBLEM RESOLUTION                        *           
*                                                                   *           
* MAY22/97 (BU ) --- UPGRADE FOR YR 2000                            *           
*                                                                   *           
* JAN23/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
* AUG10/98 (BU ) --- KEY RESTRUCTURING                              *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                    !!!  IMPORTANT  !!!                            *           
* THIS FACILITY IS SET UP TO HANDLE MULTIPLE STATIONS, VIA THE SUB- *           
* MISSION ON THE REQUEST OF A STATION GROUP/SUBGROUP.  BECAUSE IT   *           
* HAS BEEN DECIDED BY GEORGE DALLAS/KARI HAWKINS THAT THE REQUEST   *           
* ITSELF WILL BE CONSTRAINED TO A ->SINGLE STATION<-, THE REQUEST   *           
* ENTRY WILL VALIDATE THE EXISTENCE OF A 'SITUATION ANALYSIS' REC-  *           
* ORD FOR THE REPORT PERIOD.  THEREFORE, NO DUPLICATION OF EFFORT   *           
* WILL BE MADE TO ENSURE THAT THE DATA TO BE GENERATED SHOULD BE    *           
* PERMITTED.  SHOULD THIS FACILITY BE EXPANDED TO PERMIT GROUP/     *           
* SUBGROUP, THIS VALIDATION MUST BE INSERTED!!                      *           
*                                                                   *           
*                                                                   *           
*                                            BILL UHR - MAR 15, 95  *           
*********************************************************************           
* DISPLAY OPTIONS:                                                  *           
*   QUESTOR = $$        WILL SHOW CONTRACT #S, DATES, AND $$        *           
*      (SEE SPECIFIC CALLS TO SORTDISP FOR ADDITIONAL INFO.)        *           
*   QUESTOR+2  =  DISPLAY  'EX:' - DATA BEING EXTRACTED             *           
*   QUESTOR+3  =  DISPLAY  'RE:' - DATA RETURNED FROM 1ST SORT      *           
*   QUESTOR+4  =  DISPLAY  '2D:' - DATA SORTED WITH RANK VALUE      *           
*   QUESTOR+5  =  DISPLAY  'OP:' - DATA BEING SENT TO WORKER FILE   *           
*   QUESTOR+6  =  DISPLAY  'RG:' -                                  *           
*   QUESTOR+7  =  DISPLAY  'CF:' - CALCULATION OF FORECAST MKT $$   *           
*   QUESTOR+8  =  DISPLAY  'CR:' - VALUES FOR CALC OF FCST MKT $$   *           
*   QUESTOR+9  =  DISPLAY  PRNTBL VALUES                            *           
*   QUESTOR+10 =  DISPLAY  'CL:' - VALUES FOR CALC - FINAL VALUES   *           
*                                                                   *           
*********************************************************************           
*   REGISTER USAGE                                                  *           
*      R7  =  SECOND BASE REGISTER                                  *           
*      R9  =  THIRD  BASE REGISTER                                  *           
*                                                                   *           
*********************************************************************           
*                                                                               
RE1K02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE1K02,R7,R9,RR=RE                                           
         ST    RE,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     RC,FILEC                                                         
         USING FILED,RC                                                         
         STM   R2,RC,SAVEREGS      SAVE REGS 2 -> C                             
         EJECT                                                                  
*                                                                               
*        CHECK AND PROCESS MODE SETTINGS                                        
*                                                                               
         LA    R1,MODETAB          POINT TO MODE/PROC TABLE                     
         ZIC   R2,0(R1)            GET NUMBER OF ENTRIES                        
         ZIC   R3,1(R1)            GET LENGTH OF EACH ENTRY                     
         LA    R1,2(R1)            POINT TO 1ST ENTRY                           
         ZIC   R0,MODE             GET CURRENT MODE                             
MAIN10   EQU   *                                                                
         ZIC   RF,0(R1)            GET TABLE ENTRY MODE                         
         CR    R0,RF               GOT IT?                                      
         BNE   MAIN20              NO, CHECK NEXT                               
         ZICM  RF,1(R1),3          YES, GET THE ROUTINE ADDR                    
         GOTO1 (RF)                GO TO THE ROUTINE                            
         B     MAIN30              ZERO IS GOOD RETURN                          
MAIN20   EQU   *                                                                
         AR    R1,R3               POINT TO THE NEXT ENTRY                      
         BCT   R2,MAIN10           LOOP                                         
*                                                                               
MAIN30   EQU   *                                                                
*                                                                               
*        MAIN COMMON EXIT                                                       
*                                                                               
MAINGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     MAINEXIT                                                         
MAINBAD  EQU   *                                                                
         LA    R0,1                                                             
         B     MAINEXIT                                                         
MAINEXIT EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        MODE/PROCESS ROUTINE TABLE                                             
*                                                                               
*                                                                               
*        CL1  -  NUMBER OF ENTRIES                                              
*        CL1  -  LENGTH OF ONE ENTRY                                            
*        CL4  -  ENTRY:                                                         
*                  CL1 - MODE                                                   
*                  CL3 - ROUTINE ADDRESS                                        
*                                                                               
*                                                                               
MODETAB  EQU   *                                                                
         DC    AL1(NUMMDTAB)       NUMBER OF TABLE ENTRIES                      
         DC    AL1(MDTABLEN)       LENGTH OF A TABLE ENTRY                      
*                                                                               
MDENTRY  EQU   *                                                                
         DC    AL1(REQFRST),AL3(INITIAL)    REQUEST FIRST                       
MDENTRYX EQU   *                                                                
MDTABLEN EQU   MDENTRYX-MDENTRY                                                 
         DC    AL1(PROCCONT),AL3(POST)      PROCESS A CONTRACT                  
         DC    AL1(REQLAST),AL3(RPTDONE)    END OF REPORT                       
MODETABX EQU   *                                                                
NUMMDTAB EQU   (MODETABX-MDENTRY)/MDTABLEN                                      
         EJECT                                                                  
*   INITIAL:  REQUEST FIRST MODE SETTING                                        
*                                                                               
INITIAL  NTR1                                                                   
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK(4),QSTART                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFASTART)                                
         MVC   WORK(4),QEND                                                     
         GOTO1 DATCON,DMCB,(0,WORK),(0,QFAEND)                                  
*                                  CONVERT ORIGINAL EBCDIC DATE TO              
*                                     SPECIAL YR 2000 FORMAT                    
         MVI   DATAFLAG,C'N'       SET 'NO DATA FOUND'                          
         XC    SORTREC(LSORTREC),SORTREC                                        
         XC    SORTREC2(LSORTREC),SORTREC2                                      
         XC    SVSTAMKT,SVSTAMKT                                                
         XC    SVGRPNAM,SVGRPNAM                                                
         XC    TESTCTR,TESTCTR     CLEAR TEST COUNTER                           
         XC    RANKCTR,RANKCTR                                                  
         XC    RANKCTR2,RANKCTR2                                                
         XC    LASTRANK,LASTRANK                                                
         XC    RANKMAX,RANKMAX                                                  
         MVI   ACCTSEQ,0           CLEAR ACCOUNT SEQUENCE NUMBER                
         MVI   DELETFLG,C'N'       SET DELETE FLAG AT START                     
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   UNDRSCOR,X'6D'      SET FIELD TO UNDERSCORE                      
         MVI   CONFLAG,C'N'        SET CONTRACT FLAG TO NO                      
         MVC   UNDRSCOR+1(L'UNDRSCOR),UNDRSCOR                                  
         LA    R3,ALTHDR           STORE A(ALTERNATE HEADER)                    
         ST    R3,HEADHOOK                                                      
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         XC    SORTREC2,SORTREC2                                                
*                                  DERIVE REPORT PERIOD START AND               
*                                     END DATES FROM REQUEST CARD               
*   NOTE:  SPL SHARES ARE CALCULATED FOR PRIOR YEAR FIGURES.  TO                
*        PROPERLY ACCOUNT FOR LOSSES, THE REPORT PERIOD FOR TESTING             
*        MUST BE BACKED UP TO PRIOR YEAR ALSO.                                  
*                                                                               
         MVC   WORK+4(2),=C'15'                                                 
         MVC   WORK(4),QSTART      REPORT PERIOD START                          
         GOTO1 DATCON,DMCB,(0,WORK),(3,FULL)                                    
*                                  CONVERT TO BINARY FORMAT                     
         ZIC   RF,FULL             DECREMENT YEAR BY 1                          
         BCTR  RF,0                   FOR PRIOR LOSS TEST                       
         STC   RF,FULL                                                          
         GOTO1 DATCON,DMCB,(3,FULL),(0,WORK)                                    
*                                  CONVERT BACK TO EBCDIC                       
         GOTO1 GETBROAD,DMCB,WORK,WORK+6                                        
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,FULL)                                  
         MVC   STDATE,FULL         SAVE START DATE                              
         MVC   WORK(4),QEND        REPORT PERIOD END                            
         GOTO1 DATCON,DMCB,(0,WORK),(3,FULL)                                    
*                                  CONVERT TO BINARY FORMAT                     
         ZIC   RF,FULL             DECREMENT YEAR BY 1                          
         BCTR  RF,0                   FOR PRIOR LOSS TEST                       
         STC   RF,FULL                                                          
         GOTO1 DATCON,DMCB,(3,FULL),(0,WORK)                                    
*                                  CONVERT BACK TO EBCDIC                       
         GOTO1 GETBROAD,DMCB,WORK,WORK+6                                        
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,FULL)                                 
         MVC   ENDATE,FULL         SAVE END   DATE                              
         MVC   WORK+4(2),=C'01'    INSERT PADDER                                
         MVC   WORK(4),QSTART      INSERT START DATE FROM REQUEST               
         GOTO1 DATCON,DMCB,(0,WORK),(19,WORK+6)                                 
*                                  CONVERT DATE TO JULIAN 3 CHARS               
         ZAP   WORK+0(4),=P'0'                                                  
         MVO   WORK+0(4),WORK+6(3)                                              
*                                  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+14(4),=P'999999'                                            
         SP    WORK+14(4),WORK+0(4)                                             
*                                  GET 9'S COMPLEMENT                           
         MVO   WORK+10(4),WORK+14(4)                                            
*                                  CHANGE TO PWOS                               
         MVC   STDATE9S,WORK+10    INSERT START DATE 9S COMP                    
*                                                                               
         MVC   WORK+4(2),=C'01'    INSERT PADDER                                
         MVC   WORK(4),QEND        INSERT END   DATE FROM REQUEST               
         GOTO1 DATCON,DMCB,(0,WORK),(19,WORK+6)                                 
*                                  CONVERT DATE TO JULIAN 3 CHARS               
         ZAP   WORK+0(4),=P'0'                                                  
         MVO   WORK+0(4),WORK+6(3)                                              
*                                  CHANGE TO PACK WITH SIGN                     
         ZAP   WORK+14(4),=P'999999'                                            
         SP    WORK+14(4),WORK+0(4)                                             
*                                  GET 9'S COMPLEMENT                           
         MVO   WORK+10(4),WORK+14(4)                                            
*                                  CHANGE TO PWOS                               
         MVC   ENDATE9S,WORK+10    INSERT END DATE 9S COMP                      
         CLC   =C'$$',QUESTOR                                                   
         BNE   INIT0080                                                         
         MVC   P+2(10),=C'START/END='                                           
         MVC   P+14(3),STDATE                                                   
         MVC   P+19(3),ENDATE                                                   
         MVC   P+25(13),=C'START/END 9S='                                       
         MVC   P+40(3),STDATE9S                                                 
         MVC   P+45(3),ENDATE9S                                                 
         GOTO1 REPORT                                                           
INIT0080 EQU   *                                                                
         XC    DESCBILD,DESCBILD   CLEAR X'50' ELEMENT BUILD AREA               
         MVI   DESCBILD,X'50'      INSERT ELEMENT TYPE                          
         LA    RF,RSTRA5LQ         SET LENGTH                                   
         STC   RF,DESCBILD+1       INSERT ELEMENT LENGTH                        
         OI    DESCBILD+2,X'80'    INSERT 'BUILT BY SEEDER'                     
         CLI   QOPTION2,C'F'       RANKED ON PRIOR ACTUAL?                      
         BNE   INIT0120            NO                                           
         OI    DESCBILD+2,X'20'    YES - SET RANK FLAG                          
INIT0120 EQU   *                                                                
         L     RF,ANEWMON          SET UP ADDRESS WHICH                         
*                                     SERVES AS FLAG TO VALUENEW TO             
*                                     EXTRACT CONTRACT-SPECIFIC INFO            
         LA    RE,CONSPINF         SET A(CONTRACT SPECIFIC INFO)                
         SR    RF,RE                                                            
         MVC   0(4,RF),ACONSPEC    INSERT A(CON SPECIFIC INFO)                  
         XIT1                                                                   
         EJECT                                                                  
       ++INCLUDE REREPRGEQA                                                     
*              SHOW CONTRACT DETAILS                                            
         SPACE 3                                                                
POST     NTR1                                                                   
         XC    CONTOTS,CONTOTS                                                  
         MVI   CONFLAG,C'N'        TURN OFF CONTRACT FLAG                       
*                                                                               
         CLC   =C'$$',QUESTOR                                                   
         BNE   POST0020                                                         
         MVC   P+2(11),=C'CONTRACT = '                                          
         GOTO1 HEXOUT,DMCB,RCONKCON,P+13,4,=C'TOG'                              
         GOTO1 REPORT                                                           
POST0020 EQU   *                                                                
         SPACE 1                                                                
         AP    PROCCTR,=P'1'       ADD TO # CONTRACTS PROCESSED                 
         L     R4,ANEWMON          A(MONTH TABLE)                               
         LA    R3,TMONTBL          A(ACCUMULATORS)                              
*                                                                               
*   FIND REQUEST START DATE IN TABLE.                                           
*                                                                               
POST0040 EQU   *                                                                
         CLC   0(4,R4),QFASTART    TABLE ENTRY VS REQ START DATE                
         BE    POST0060            FOUND - BEGIN TO PULL FIGURES                
         BH    POST0180            NOT FOUND - SHOULDN'T HAPPEN                 
         LA    R4,NEXTBUCK(R4)     BUMP TO NEXT BUCKET                          
         B     POST0040            GO BACK FOR NEXT                             
*                                                                               
POST0060 EQU   *                                                                
         CLI   0(R4),0             ANY TABLE ENTRY?                             
         BE    POST0180            NO  - EXIT                                   
         CLC   0(4,R4),QFAEND      TABLE ENTRY VS REQ END DATE                  
         BH    POST0160            TABLE > END DATE - DISPLAY & EXIT            
*                                                                               
         LR    R6,R4               SET R6 TO A(BUCKETS IN MONTH)                
         LA    R6,BUCKDISP(R6)     PASS MONTH CONTROLS                          
         CLI   QOPTION2,C'B'       REQUEST FOR PRIOR BOOKED?                    
         BNE   POST0070            NO  -                                        
         MVC   CONTOTS(4),PRASATOR(R6)                                          
*                                  YES - USE PRIOR BOOKED FIGURE                
         B     POST0100                                                         
POST0070 EQU   *                                                                
         TM    FLAG6(R4),X'02'     ANY PRIOR INVOICED $ IN BUCKET?              
         BNO   POST0080            NO  - TAKE ORDERED $                         
         MVC   CONTOTS(4),PRTOTINV(R6)                                          
*                                  YES = INVOICED PRIOR YEAR                    
         B     POST0100                                                         
POST0080 EQU   *                                                                
         MVC   CONTOTS(4),PRTOTORD(R6)                                          
*                                  NO  = ORDERED PRIOR YEAR                     
POST0100 EQU   *                                                                
         L     RF,ACONSPEC         LOAD A(CONTRACT SPECIFIC AREA)               
         CLI   CSFRCAST(RF),C'Y'   FORECASTING CONTRACT?                        
         BNE   POST0110            NO  - DON'T EXTRACT FORECAST $$              
         MVC   CONTOTS+8(4),CURRFORC(R6)                                        
*                                  GET FORECAST DOLLARS FOR CONTRACT            
POST0110 EQU   *                                                                
*                                                                               
         OC    CONTOTS(16),CONTOTS ANY VALUE IN BUCKET?                         
         BZ    POST0140            NO  - DON'T SET CONFLAG                      
         MVI   CONFLAG,C'Y'        YES - SET CONFLAG                            
POST0120 EQU   *                                                                
         LA    R2,CONTOTS                                                       
         L     R0,0(R2)            ADD PRIOR $                                  
         A     R0,0(R3)            TO ACCUMULATOR PRIOR $                       
         ST    R0,0(R3)            STORE IT BACK                                
         L     R0,0(R2)            ADD PRIOR $                                  
         A     R0,12(R3)           TO ACCUMULATOR PRIOR STATION $$              
*                                     FOR SHARE CALCULATION                     
         ST    R0,12(R3)           STORE IT IN FOURTH BUCKET                    
         L     R0,8(R2)            ADD CURRENT FORECAST $                       
         A     R0,8(R3)            TO ACCUMULATOR CURR FCST$                    
         ST    R0,8(R3)            STORE IT BACK                                
POST0140 EQU   *                                                                
         LA    R4,NEXTBUCK(R4)     BUMP BUCKET ADDRESS                          
         B     POST0060            SWING BACK FOR NEXT BUCKET                   
         SPACE 2                                                                
POST0160 EQU   *                                                                
         XC    SORTREC(LSORTREC),SORTREC                                        
*                                  CLEAR THE SORTREC                            
         CLI   CONFLAG,C'Y'        ANY VALUE IN CONTRACT?                       
         BE    POST0170            YES -                                        
         L     RF,ACONSPEC         NO  - CHECK FOR SPL LOSS                     
         L     RE,CSSPLLSS(RF)     RETRIEVE LOSS FIGURE, IF ANY                 
         LTR   RE,RE               ANY VALUE THERE?                             
         BZ    POST0180            NO  - NOTHING IN THIS CONTRACT               
         STCM  RE,15,SPRIOR$$      LOAD LOSS TO MARKET DOLLARS                  
         B     POST0175                                                         
*                                                                               
POST0170 EQU   *                                                                
         L     RF,ACONSPEC         GET CONTRACT-SPECIFIC SPL %                  
         L     RE,CSSPLPER(RF)     RETRIEVE SPL %                               
         ST    RE,CONTOTS+12       SAVE IN BUCKET                               
         MVC   SSHRGOL,CSSHGOAL(RF)                                             
*                                  INSERT SHARE GOAL FOR THIS CONTRACT          
         GOTO1 CALCSHAR,DMCB,(R3)  CALCULATE MKT DOLLARS FOR SHARE              
         GOTO1 CALCFCST,DMCB,(R3)  CALCULATE FORECAST MARKET $$                 
*                                                                               
POST0175 EQU   *                                                                
         MVC   SGRPSUB,RCONKGRP    INSERT GROUP/SUBGROUP                        
         MVC   SSTATN,RCONKSTA     INSERT STATION                               
         MVC   SADVERT,RCONKADV    INSERT ADVERTISER                            
         MVC   SOFF,RCONKOFF       INSERT OFFICE                                
         MVC   SPRIOR$,0(R3)       INSERT PRIOR DOLLARS                         
         MVC   SPRISH$,12(R3)      INSERT PRIOR STATION DOLLARS                 
         MVC   SPRIOR$$,4(R3)      INSERT MARKET PRIOR $$                       
         MVC   SSPLPCT,CONTOTS+12  INSERT STATION SPL %                         
         MVC   SFORCAST,8(R3)      INSERT FORECAST $$                           
         MVC   SFORMKT,16(R3)      INSERT FORECAST MARKET $$                    
         MVC   SCONNUM,RCONKCON    INSERT CONTRACT NUMBER                       
         BAS   RE,SORTGEN          GENERATE THE SORTREC                         
         MVI   DATAFLAG,C'Y'       SET 'DATA FOUND FOR PROCESSING'              
         XC    TMONTBL,TMONTBL     CLEAR ACCUMULATORS                           
         CLC   =C'$$',QUESTOR                                                   
         BNE   POST0180                                                         
         CLI   QUESTOR+2,C'Y'      DISPLAY 'EX:'?                               
         BNE   POST0180            NO                                           
         GOTO1 SORTDISP,DMCB,0                                                  
POST0180 EQU   *                                                                
         LTR   R0,R0                                                            
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CALCSHAR:  USE SPL % AND SPRISH$ TO CALCULATE THE MARKET                    
*        DOLLARS TO PERMIT ACCUMULATION.                                        
*                                                                               
CALCSHAR NTR1                                                                   
*                                                                               
         L     R3,0(R1)            LOAD ACCUMULATOR ADDRESS                     
         L     R1,12(R3)           LOAD PRISH$ AMOUNT                           
*                                                                               
*   TEST                                                                        
*        L     RE,ACONSPEC         A(CONTRACT SPECIFIC DATA)                    
*        MVC   P+1(09),=C'CSSPLOSS='                                            
*        MVC   P+10(1),CSSPLOSS(RE)                                             
*        MVC   P+15(07),=C'CSSPCT='                                             
*        L     RF,CSSPLPCT(RE)     GET SPL PCT FOR CONTRACT                     
*        EDIT  (RF),(5,P+24),ZERO=NOBLANK                                       
*        GOTO1 REPORT                                                           
*   TEST END                                                                    
*                                                                               
         L     RE,ACONSPEC         A(CONTRACT SPECIFIC DATA)                    
         L     RF,CSSPLPCT(RE)     GET SPL PCT FOR CONTRACT                     
*                                                                               
*   CSSPLOSS IS SET IN CONTRACT ANALYZER MODULE.  IT IS 'N' ONLY IF             
*      CONTRACT HAS AN SPL ELEMENT, AND REP'D STATION HAS A PERCENT             
*      OF THE ACTION.  IF SPL ELEMENT EXISTS, AND REP'D STATION HAS             
*      NO PERCENT, VALUE IS 'Y' (LOSS).  IF NO SPL ELEMENT, VALUE IS            
*      X'00'.                                                                   
*   ONE FURTHER NOTE:  MANY CONTRACTS WERE ENTERED WITH 0% FOR ALL              
*      STATIONS, AND THE ORDER HAS DOLLARS.  THIS IS REPORTED HERE              
*      AS 'NOT A LOSS', BUT RESULTS IN DRASTICALLY INFLATING THE                
*      CALCULATED SHARE, BY ADDING THE ORDER DOLLARS TO THE NUMERATOR,          
*      AND $0 (ORDER DOLLARS / 0%) TO THE DENOMINATOR OF THE CALC.              
*      SUCH ORDERS WILL BE TREATED SAME AS 'CREDITS' - THEY WILL NOT            
*      BE CONSIDERED IN THE CALCULATION.                                        
*                                                                               
         CLI   CSSPLOSS(RE),C'N'   SPL NOT A LOSS?                              
         BNE   CSHR0240            NO  - LOSS OR NO SPL                         
         LTR   RF,RF               IT'S A WIN - ANY SPL PERCENT?                
         BZ    CSHR0040            NO  - TREAT SAME AS CREDIT                   
         LTR   R1,R1               CHECK FOR 'CREDIT': NEGATIVE                 
         BNM   CSHR0080            NOT NEGATIVE: PROCEED                        
CSHR0040 EQU   *                                                                
         SR    R1,R1               NEGATIVE: SET R1 TO ZERO                     
         XC    12(4,R3),12(R3)     CLEAR SHARE CALC BASIS                       
CSHR0080 EQU   *                                                                
         LR    RE,R1               LOAD SPRIOR $$ TO REGISTER                   
         SR    R0,R0                                                            
         M     R0,=F'10000'        MULT MONTHLY $ BY 10,000                     
*                                     PCT FIGURE IS XX.XX                       
         SLDA  R0,1                DOUBLE FOR ROUNDING                          
         LTR   R0,R0               VALUE NEEDS BOTH REGISTERS?                  
         BZ    CSHR0120            NO  - PROCEED                                
         C     RF,=F'4'            YES - CHECK DIVISOR SIGNIFICANCE             
         BH    CSHR0120            SIGNIFICANT ENOUGH TO PROCEED                
         SR    RF,RF               NOT SIGNIFICANT: DON'T DIVIDE                
CSHR0120 EQU   *                                                                
         AR    R1,RF               ADD PCT AMOUNT FOR ROUNDING                  
         LTR   RF,RF               ANY PERCENT?                                 
         BNZ   CSHR0160            YES                                          
         SR    R1,R1               NO % = NO MARKET $$                          
         B     CSHR0200                                                         
CSHR0160 EQU   *                                                                
         DR    R0,RF               AMOUNT/PERCENT=MARKET $$                     
         SRA   R1,1                DIVIDE BY 2                                  
CSHR0200 EQU   *                                                                
         A     R1,4(R3)            STORE AMOUNT IN BLANK BUCKET                 
         ST    R1,4(R3)            STORE IT BACK                                
         B     CSHR0360            FINISHED                                     
CSHR0240 EQU   *                   SPL WAS A LOSS                               
*                                                                               
*   A LOSS HAS BEEN IDENTIFIED.  CHECK TO ENSURE THAT THE                       
*      CONTRACT'S FLIGHT DATES FALL WITHIN THE REPORT PERIOD.                   
*                                                                               
         GOTO1 DATCON,DMCB,(3,RCONDATE),(0,WORK)   GET FLIGHT START/            
         GOTO1 GETBROAD,DMCB,WORK,WORK+6                      END DATES         
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,FULL)                                 
         MVC   FLIGHTST,FULL                                                    
         GOTO1 DATCON,DMCB,(3,RCONDATE+3),(0,WORK)                              
         GOTO1 GETBROAD,DMCB,WORK,WORK+6                                        
         GOTO1 DATCON,DMCB,(0,WORK+12),(3,FULL)                                 
         MVC   FLIGHTND,FULL                                                    
         CLC   FLIGHTND,STDATE     FLIGHT ENDS BEFORE REP PERIOD?               
         BL    CSHR0320            YES - SKIP IT                                
         CLC   ENDATE(2),FLIGHTST  FLIGHT STARTS AFTER REP PERIOD?              
         BL    CSHR0320            YES - SKIP IT                                
*                                                                               
*   FOR A LOSS, CSSPLPCT CONTAINED THE TOTAL MARKET $$, WHICH                   
*      ARE TO BE SAVED IN THE DENOMINATOR OF THE CALCULATION.                   
*      NOTE:  CSSPLPCT CONTAINS NO PENNIES, AND MUST BE ALIGNED.                
*                                                                               
CSHR0280 EQU   *                                                                
         L     RE,ACONSPEC         A(CONTRACT SPECIFIC DATA)                    
         L     RF,CSSPLPCT(RE)     GET SPL PCT FOR CONTRACT                     
         MH    RF,=H'100'          DECIMAL ALIGN                                
         ST    RF,4(R3)            LOSS - VALUE RETURNED FROM SPL WAS           
*                                     MARKET $$ - USE AS IS                     
CSHR0320 EQU   *                                                                
         XC    12(4,R3),12(R3)     CLEAR SHARE CALC BASIS                       
CSHR0360 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   CALCFCST:  USE SHARE GOAL AND SFORCAST TO CALCULATE THE MARKET              
*        FORECAST $$ TO PERMIT ACCUMULATION.                                    
*                                                                               
CALCFCST NTR1                                                                   
*                                                                               
         L     R3,0(R1)            LOAD ACCUMULATOR ADDRESS                     
         L     R1,8(R3)            LOAD SFORCAST AMOUNT                         
         ZICM  RF,SSHRGOL,4        LOAD SHARE GOAL FOR THIS CONTRACT            
*                                                                               
         CLC   =C'$$',QUESTOR                                                   
         BNE   CFCS0040                                                         
         CLI   QUESTOR+7,C'Y'      DISPLAY CF:?                                 
         BNE   CFCS0040            NO                                           
         ST    R1,DMCB+4           INSERT SFORCAST AMOUNT                       
         ST    RF,DMCB+8           INSERT SHARE GOAL                            
         GOTO1 SORTDIS3,DMCB,(0,0)                                              
CFCS0040 EQU   *                                                                
         LR    RE,R1               LOAD SFORCAST $$ TO REGISTER                 
         SR    R0,R0                                                            
         M     R0,=F'10000'        MULT MONTHLY $ BY 10,000                     
*                                     PCT FIGURE IS XX.XX                       
         SLDA  R0,1                DOUBLE FOR ROUNDING                          
         LTR   R0,R0               VALUE NEEDS BOTH REGISTERS?                  
         BZ    CFCS0120            NO  - PROCEED                                
         C     RF,=F'4'            YES - CHECK DIVISOR SIGNIFICANCE             
         BH    CFCS0120            SIGNIFICANT ENOUGH TO PROCEED                
         SR    RF,RF               NOT SIGNIFICANT: DON'T DIVIDE                
CFCS0120 EQU   *                                                                
         AR    R1,RF               ADD PCT AMOUNT FOR ROUNDING                  
         LTR   RF,RF               ANY SHARE GOAL?                              
         BNZ   CFCS0160            YES                                          
         SR    R1,R1               NO % = NO FORECAST MARKET $$                 
         B     CFCS0200                                                         
CFCS0160 EQU   *                                                                
         CLC   =C'$$',QUESTOR                                                   
         BNE   CFCS0180                                                         
         CLI   QUESTOR+8,C'Y'      DISPLAY CR:?                                 
         BNE   CFCS0180            NO                                           
         ST    R1,DMCB+4           INSERT SFORCAST AMOUNT                       
         ST    RF,DMCB+8           INSERT SHARE GOAL                            
         ST    R1,DUB              SAVE VALUES                                  
         ST    RF,DUB+4                                                         
         ST    R0,FULL                                                          
         GOTO1 SORTDIS3,DMCB,(1,0)                                              
         L     R1,DUB              RESET VALUES                                 
         L     RF,DUB+4                                                         
         L     R0,FULL                                                          
CFCS0180 EQU   *                                                                
         DR    R0,RF               AMOUNT/SHARE GOAL=MARKET $$                  
         SRA   R1,1                DIVIDE BY 2                                  
CFCS0200 EQU   *                                                                
         CLC   =C'$$',QUESTOR                                                   
         BNE   CFCS0240                                                         
         CLI   QUESTOR+10,C'Y'     DISPLAY CL:?                                 
         BNE   CFCS0240            NO                                           
         ST    R1,DMCB+4           INSERT FORECAST MKT $$                       
         MVC   DMCB+8(4),16(R3)    BASE VALUE                                   
         ST    R1,DUB              SAVE VALUES                                  
         GOTO1 SORTDIS3,DMCB,(2,0)                                              
         L     R1,DUB              RESET VALUES                                 
CFCS0240 EQU   *                                                                
         A     R1,16(R3)           STORE AMOUNT IN BLANK BUCKET                 
         ST    R1,16(R3)           STORE IT BACK                                
         XIT1                                                                   
         EJECT                                                                  
SORTDISP NTR1                                                                   
         MVC   P(3),=C'EX:'                                                     
         CLI   0(R1),0             FROM POST?                                   
         BE    SDIS0020            YES                                          
         MVC   P(3),=C'RE:'                                                     
         CLI   0(R1),1             FROM 1ST SORT?                               
         BE    SDIS0020            YES                                          
         MVC   P(3),=C'2D:'                                                     
         CLI   0(R1),2             FROM 2ND SORT?                               
         BE    SDIS0020            YES                                          
         MVC   P(3),=C'OP:'                                                     
         CLI   0(R1),3             FROM OUTPUT TO RGWORK?                       
         BE    SDIS0020            YES                                          
         DC    H'0'                UNKNOWN CALL                                 
SDIS0020 EQU   *                                                                
         MVC   P+20(5),SSTATN                                                   
         GOTO1 HEXOUT,DMCB,SCONNUM,P+28,4,=C'TOG'                               
         EDIT  (1,SZEROFLG),(1,P+37)                                            
         EDIT  (4,STOPN),(10,P+38),COMMAS=YES,MINUS=YES                         
         MVC   P+48(4),SADVERT                                                  
         MVC   P+53(2),SOFF                                                     
         EDIT  (4,SPRIOR$),(10,P+56),COMMAS=YES,MINUS=YES                       
         EDIT  (4,SPRISH$),(10,P+67),COMMAS=YES,MINUS=YES                       
         EDIT  (4,SPRIOR$$),(10,P+78),COMMAS=YES,MINUS=YES                      
         EDIT  (4,SSPLPCT),(6,P+87),2                                           
         EDIT  (4,SFORCAST),(10,P+98),COMMAS=YES,MINUS=YES                      
         EDIT  (4,SSHRGOL),(6,P+106),2                                          
         EDIT  (4,SFORMKT),(10,P+114),COMMAS=YES,MINUS=YES                      
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
SORTDIS2 NTR1                                                                   
         MVC   P(3),=C'RG:'                                                     
         CLI   0(R1),0             FROM RGWORK O/P?                             
         BE    SODI0020            YES                                          
         DC    H'0'                UNKNOWN CALL                                 
SODI0020 EQU   *                                                                
         MVC   P+20(5),SSTATN2                                                  
         GOTO1 HEXOUT,DMCB,SCONNUM2,P+28,4,=C'TOG'                              
         EDIT  (1,SZEROFL2),(1,P+37)                                            
         EDIT  (4,STOPN2),(10,P+38),COMMAS=YES,MINUS=YES                        
         MVC   P+48(4),SADVERT2                                                 
         MVC   P+53(2),SOFF2                                                    
         EDIT  (4,SPRIOR$2),(10,P+56),COMMAS=YES,MINUS=YES                      
         EDIT  (4,SPRISH$2),(10,P+67),COMMAS=YES,MINUS=YES                      
         EDIT  (4,SPRIOR2$),(10,P+78),COMMAS=YES,MINUS=YES                      
         EDIT  (4,SSPLPCT2),(6,P+87),2                                          
         EDIT  (4,SFORCST2),(10,P+98),COMMAS=YES,MINUS=YES                      
         EDIT  (4,SSHRGOL2),(6,P+106),2                                         
         EDIT  (4,SFORMKT2),(10,P+114),COMMAS=YES,MINUS=YES                     
         GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
SORTDIS3 NTR1                                                                   
         MVC   P(3),=C'CF:'                                                     
         CLI   0(R1),0             FROM CALCFCST?                               
         BE    SORD0020            YES                                          
         MVC   P(3),=C'CR:'                                                     
         CLI   0(R1),1             FROM CALCFCST: INPUT VALUES?                 
         BE    SORD0020            YES                                          
         MVC   P(3),=C'CL:'                                                     
         CLI   0(R1),2             FROM CALCFCST: FINAL VALUES?                 
         BE    SORD0040            YES                                          
         DC    H'0'                UNKNOWN CALL                                 
SORD0020 EQU   *                                                                
         L     R2,4(R1)            LOAD SFORECAST $$                            
         L     R3,8(R1)            LOAD SHARE GOAL                              
         MVC   P+4(14),=C'FCST$/SHRGOAL='                                       
         EDIT  (R2),(12,P+20),ZERO=NOBLANK,COMMAS=YES,MINUS=YES                 
         EDIT  (R3),(10,P+36),2                                                 
         GOTO1 REPORT                                                           
         B     SORD0200                                                         
SORD0040 EQU   *                                                                
         L     R2,4(R1)            LOAD SFORECAST $$                            
         L     R3,8(R1)            LOAD SHARE GOAL                              
         MVC   P+4(14),=C'FINL$/BUCKET ='                                       
         EDIT  (R2),(12,P+20),ZERO=NOBLANK,COMMAS=YES,MINUS=YES                 
         EDIT  (R3),(12,P+36),ZERO=NOBLANK,COMMAS=YES,MINUS=YES                 
         GOTO1 REPORT                                                           
SORD0200 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*   RPTDONE:  LAST REQUEST MODE SETTING:                                        
*         ALL INPUT COMPLETE - STILL MUST:                                      
*             SORT FILE                                                         
*             COMPRESS DATA FOR STATION/ADV/OFFICE                              
*             UPDATE FILE                                                       
*             PRODUCE REPORT, IF REQUIRED                                       
*                                                                               
*   DATA RETURN, FILE UPDATE AND REPORT GENERATOR:  EXPLANATION                 
*                                                                               
*   SORTED FILE WILL BE COMPRESSED ON STATION/ADV/OFF.  FIGURES                 
*        WILL BE ACCUMULATED.  THE SELECTED BASIS FOR RANKING WILL              
*        BE INSERTED INTO THE 'TOPN' FIELD OF THE RECORD, AND THE               
*        RECORD WILL BE WRITTEN TO A WORK FILE.  AFTER ALL RECORDS              
*        ARE PROCESSED, THE WORK FILE WILL BE RESORTED, AND THE                 
*        RECORDS PROCESSED TO ADD TO THE FILE.                                  
*                                                                               
*                                                                               
*                                                                               
MAXRKVAL EQU   80+80+6             DISPLACEMENT TO RANK MAX VAL                 
*                                                                               
RPTDONE  NTR1                                                                   
*                                                                               
         CLI   DATAFLAG,C'Y'       DATA FOR PROCESSING?                         
         BNE   RPTD0960            NO  - DON'T DO ANYTHING ELSE                 
         XC    RANKMAX,RANKMAX     CLEAR RANK MAX VALUE                         
         L     RF,AREQCRDS         ANY RANK MAX VALUE?                          
         CLC   =C'  ',MAXRKVAL(RF)                                              
*                                  RANK VALUE MAX EMPTY?                        
         BE    RPTD0020            YES                                          
         CLC   =X'0000',MAXRKVAL(RF)                                            
*                                  RANK VALUE MAX EMPTY?                        
         BE    RPTD0020            YES                                          
         PACK  DUB(8),MAXRKVAL(2,RF)                                            
*                                  NO  - PACK THE VALUE                         
         CVB   RF,DUB              CONVERT VALUE TO BINARY                      
         ST    RF,RANKMAX          SAVE THE VALUE                               
         CLC   =C'$$',QUESTOR                                                   
         BNE   RPTD0020                                                         
         MVC   P+1(14),=C'RANK MAXIMUM ='                                       
         EDIT  RANKMAX,(8,P+15)                                                 
         GOTO1 REPORT                                                           
RPTD0020 EQU   *                                                                
         XC    SORTREC2(LSORTREC),SORTREC2                                      
*                                  CLEAR SECOND SORT RECORD                     
*                                                                               
RPTD0040 EQU   *                                                                
         OPEN (RGWORK,OUTPUT)      PREPARE FOR A SECOND SORT                    
RPTD0080 EQU   *                                                                
         BAS   RE,GETSORT                                                       
         CLI   SORTREC,X'FF'       EOF                                          
         BE    RPTD0280            YES                                          
         CLC   =C'$$',QUESTOR                                                   
         BNE   RPTD0120                                                         
         CLI   QUESTOR+3,C'Y'      DISPLAY RE:?                                 
         BNE   RPTD0120            NO                                           
         GOTO1 SORTDISP,DMCB,(1,0)                                              
RPTD0120 EQU   *                                                                
         OC    SORTREC2,SORTREC2   FIRST PASS?                                  
         BZ    RPTD0200            YES                                          
         CLC   SORTREC(LSORTKEY),SORTREC2                                       
*                                  SAME STATION/ADV/OFF?                        
         BE    RPTD0240            YES - ACCUMULATE FIGURES                     
         L     RF,SORTACUM         NO  - INSERT SORT ACCUM VALUE                
         LTR   RF,RF               TEST FOR NEGATIVE VALUE                      
         BM    RPTD0200            ORIGINAL VALUE NEGATIVE:  SKIP IT...         
         LCR   RF,RF               NEGATE FOR DESCENDING SORT                   
         OC    SORTACUM,SORTACUM                                                
         BNZ   RPTD0160                                                         
         MVI   SZEROFL2,1          SET 'ZERO VALUE' FLAG                        
RPTD0160 EQU   *                                                                
         STCM  RF,15,STOPN2        INSERT VALUE INTO SORTFIELD                  
         PUT   RGWORK,SORTREC2     NO  - WRITE ACCUM REC TO WORKFILE            
         CLC   =C'$$',QUESTOR                                                   
         BNE   RPTD0200                                                         
         CLI   QUESTOR+6,C'Y'      DISPLAY RG:?                                 
         BNE   RPTD0200            NO                                           
         GOTO1 SORTDIS2,DMCB,(0,0)                                              
RPTD0200 EQU   *                                                                
         XC    SORTACUM,SORTACUM   CLEAR ACCUMULATOR                            
         MVC   SORTREC2(LSORTREC),SORTREC                                       
*                                  START A NEW KEY                              
         CLI   QOPTION2,C'C'       RANK ON CURRENT FORECAST?                    
         BNE   RPTD0220            NO  -                                        
         MVC   SORTACUM,SFORCST2   YES - FCAST $$ INTO RANK ACCUM               
         B     RPTD0080            GO BACK FOR NEXT                             
RPTD0220 EQU   *                                                                
         MVC   SORTACUM,SPRIOR$2   LOAD DOLLARS INTO RANK ACCUM                 
         B     RPTD0080            GO BACK FOR NEXT                             
RPTD0240 EQU   *                                                                
         ZICM  RF,SPRIOR$,4        ACCUMULATE FIGURES                           
         ZICM  RE,SPRIOR$2,4       ACCUMULATE FIGURES                           
         AR    RF,RE                                                            
         STCM  RF,15,SPRIOR$2                                                   
*                                                                               
         CLI   QOPTION2,C'C'       RANK ON CURRENT FORECAST?                    
         BNE   RPTD0260            NO  -                                        
         ZICM  RF,SFORCAST,4       YES - ACCUMUL FIGURES FOR RANKING            
         ZICM  RE,SORTACUM,4       ACCUMULATE FORECAST $$                       
         AR    RF,RE                                                            
         STCM  RF,15,SORTACUM      REPLACE RANK ACCUM DOLLARS                   
         B     RPTD0270                                                         
RPTD0260 EQU   *                                                                
         ZICM  RF,SPRIOR$,4        ACCUMULATE FIGURES FOR RANKING               
         ZICM  RE,SORTACUM,4       ACCUMULATE PRIOR $$                          
         AR    RF,RE                                                            
         STCM  RF,15,SORTACUM      REPLACE RANK ACCUM DOLLARS                   
RPTD0270 EQU   *                                                                
*                                                                               
         ZICM  RF,SPRISH$,4        ACCUMULATE FIGURES STATION $$                
         ZICM  RE,SPRISH$2,4       ACCUMULATE FIGURES                           
         AR    RF,RE                                                            
         STCM  RF,15,SPRISH$2      REPLACE MARKET $$                            
*                                                                               
         ZICM  RF,SPRIOR$$,4       ACCUMULATE FIGURES MARKET $$                 
         ZICM  RE,SPRIOR2$,4       ACCUMULATE FIGURES                           
         AR    RF,RE                                                            
         STCM  RF,15,SPRIOR2$      REPLACE MARKET $$                            
*                                                                               
         ZICM  RF,SFORCAST,4       ACCUMULATE FIGURES FORECAST $$               
         ZICM  RE,SFORCST2,4       ACCUMULATE FIGURES                           
         AR    RF,RE                                                            
         STCM  RF,15,SFORCST2      REPLACE FORECAST $$                          
*                                                                               
         ZICM  RF,SFORMKT,4        ACCUMULATE FORECAST MKT $$                   
         ZICM  RE,SFORMKT2,4       ACCUMULATE FIGURES                           
         AR    RF,RE                                                            
         STCM  RF,15,SFORMKT2      REPLACE FORECAST MKT $$                      
*                                                                               
         B     RPTD0080            GO BACK FOR NEXT                             
RPTD0280 EQU   *                                                                
         OC    SORTREC2(LSORTREC),SORTREC2                                      
*                                  ANY VALUE IN FIELD?                          
         BZ    RPTD0880            NO  - NOTHING IN REPORT                      
*                                     MUST END JOB CLEANLY                      
         L     RF,SORTACUM         YES - INSERT SORT ACCUM VALUE                
         LNR   RF,RF               NEGATIVE FOR DESCENDING SORT                 
         STCM  RF,15,STOPN2        INSERT VALUE INTO SORTFIELD                  
         OC    SORTACUM,SORTACUM                                                
         BNZ   RPTD0300                                                         
         MVI   SZEROFL2,1          SET 'ZERO VALUE' FLAG                        
RPTD0300 EQU   *                                                                
         XC    SORTACUM,SORTACUM   CLEAR ACCUMULATOR                            
         PUT   RGWORK,SORTREC2     WRITE LAST REC TO WORKFILE                   
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
*                                  RESET SORT FILE                              
         CLOSE RGWORK                                                           
         OPEN  (RGWORK,INPUT)                                                   
         LA    R0,RPTD0400         SET EOF ADDRESS                              
         STCM  R0,7,RGWORK+33                                                   
RPTD0320 EQU   *                                                                
         GET   RGWORK,SORTREC                                                   
         CLC   =C'$$',QUESTOR                                                   
         BNE   RPTD0360                                                         
         CLI   QUESTOR+5,C'Y'      DISPLAY OP:?                                 
         BNE   RPTD0360                                                         
         GOTO1 SORTDISP,DMCB,(3,0)                                              
RPTD0360 EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
*                                  WRITE SORTREC WITH $ VALUE IN KEY            
         B     RPTD0320            GO BACK FOR NEXT                             
RPTD0400 EQU   *                                                                
         CLOSE RGWORK              FINISHED WITH RGWORK                         
         XC    SORTREC2,SORTREC2                                                
*                                  CLEAR SECONDARY SORT AREA                    
         XC    ELTBUILD,ELTBUILD                                                
         MVI   ELTBUILD,X'20'      INSERT ELEMENT TYPE                          
         MVI   ELTBUILD+1,X'27'    INSERT ELEMENT LENGTH                        
         XCEFL RSTRREC,1000        CLEAR THE STRATEGY RECORD                    
         MVI   RSTRKTYP,X'39'      INSERT RECORD TYPE                           
         MVI   RSTRKSUB,5          INSERT SUBTYPE CODE                          
         MVC   RSTRKREP,RCREPFL    INSERT REP CODE                              
         MVC   RSTRKSTD,STDATE9S                                                
*                                  INSERT START DATE (9'S COMP)                 
         MVC   RSTRKEND,ENDATE9S                                                
*                                  INSERT END   DATE (9'S COMP)                 
         MVC   RSTRLEN,=H'34'      SET INITIAL LENGTH                           
RPTD0440 EQU   *                                                                
         BAS   RE,GETSORT                                                       
         CLI   SORTREC,X'FF'       EOF                                          
         BE    RPTD0800            YES                                          
         CLC   =C'$$',QUESTOR                                                   
         BNE   RPTD0480                                                         
         CLI   QUESTOR+4,C'Y'      DISPLAY 2D:?                                 
         BNE   RPTD0480            NO                                           
         GOTO1 SORTDISP,DMCB,(2,0)                                              
RPTD0480 EQU   *                                                                
         OC    SORTREC2,SORTREC2   FIRST PASS?                                  
         BNZ   RPTD0520            NO                                           
         MVC   SORTREC2(LSORTREC),SORTREC                                       
*                                  YES - LOAD 1ST RECORD                        
         MVC   RSTRKGRP(7),SGRPSUB                                              
*                                  INSERT FIRST GRP/STATION INTO KEY            
         MVI   PAGENUM,1           SET UP FOR 1ST PAGENUM                       
         MVI   RSTRKPG,1           INSERT 1ST PAGENUM IN 1ST REC                
         GOTO1 =A(SETHDGS),DMCB,(RC)                                            
*                                  SET HEADINGS ON FIRST PASS                   
RPTD0520 EQU   *                                                                
         CLC   SORTREC(7),SORTREC2 GROUP/STATION = LAST GRP/STN?                
         BE    RPTD0640            YES - INSERT ANOTHER ELEMENT                 
*                                  NO  -                                        
         GOTO1 =A(SETHDGS),DMCB,(RC)                                            
*                                  SET HEADINGS ON BREAK                        
         XC    RANKCTR,RANKCTR     CLEAR RANK MAX COUNTER                       
         XC    RANKCTR2,RANKCTR2   CLEAR RANK MAX COUNTER2                      
         XC    LASTRANK,LASTRANK   CLEAR LAST RANK VALUE                        
         MVI   PAGENUM,1           RESET TO PAGENUM 1                           
*                                  BREAK MAY HAVE COME JUST AFTER A             
*                                     FULL RECORD WAS WRITTEN - IF              
*                                     NO ELEMENTS, DON'T WRITE RECORD           
*   TEST                                                                        
*        MVC   P+1(08),=C'ACCTSEQ='                                             
*        MVC   P+10(1),ACCTSEQ                                                  
*        GOTO1 REPORT                                                           
*                                                                               
*                                                                               
         MVI   ACCTSEQ,0           RESET SEQ CTR TO 0                           
         CLC   RSTRLEN,=H'34'      ANY ELEMENTS IN RECORD?                      
         BNH   RPTD0560            NO  - DON'T WRITE IT                         
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RSTRREC,DESCBILD               
*                                  INSERT X'50' ELT INTO RECORD                 
         CLI   QOPTION1,C'T'       TEST RUN                                     
         BE    RPTD0560            YES - NO REWRITE OF RECORD                   
         CLI   QOPTION1,C'X'       SPECIAL TEST RUN                             
         BE    RPTD0560            YES - NO REWRITE OF RECORD                   
         GOTO1 =A(RECWRITE),DMCB,(RC)                                           
         B     RPTD0600                                                         
RPTD0560 EQU   *                                                                
         CLC   =C'$$',QUESTOR                                                   
         BNE   RPTD0600                                                         
         CLI   QUESTOR+9,C'Y'      PRNTBL ACTIVE?                               
         BNE   RPTD0600            NO                                           
         ZICM  RF,RSTRLEN,2                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,RSTRREC),RSTRREC,C'DUMP',(RF),=C'2D'          
RPTD0600 EQU   *                                                                
         GOTO1 =A(SITADATE),DMCB,(RC)                                           
*                                  UPDATE SITUATION ANALYSIS DATE               
         GOTO1 =A(DOREPORT),DMCB,(RC)                                           
         MVI   FORCEHED,C'Y'       FORCE PAGE BREAK                             
         MVC   RSTRKGRP(7),SGRPSUB                                              
*                                  INSERT NEW GRP/STATION INTO KEY              
         MVI   RSTRKPG,1           INSERT 1ST PAGENUM IN REC                    
         MVC   SORTREC2(LSORTREC),SORTREC                                       
*                                  SAVE NEW RECORD AS OLD RECORD                
         XCEFL RSTRELEM,960        CLEAR STRATEGY RECORD AFTER KEY              
         MVC   RSTRLEN,=H'34'      RESET LENGTH TO 'NO ELEMENTS'                
RPTD0640 EQU   *                                                                
         OC    RANKMAX,RANKMAX     ANY RANK MAX VALUE?                          
         BZ    RPTD0660            NO  - DON'T CHECK FOR RANKMAX                
         L     RF,RANKCTR2         INCREMENT RANK COUNTER2                      
         LA    RF,1(RF)                                                         
         ST    RF,RANKCTR2         PUT IT BACK                                  
         CLC   STOPN,LASTRANK      THIS RANK VALUE SAME AS LAST?                
         BE    RPTD0660            YES - DON'T INCREMENT RANK VALUE             
         MVC   RANKCTR,RANKCTR2    NO  - SET NEXT CONSECUTIVE RANK #            
         CLC   RANKCTR,RANKMAX     HAS COUNTER MAX'ED OUT?                      
         BH    RPTD0440            YES - DON'T OUTPUT RECORD                    
         MVC   LASTRANK,STOPN      NO  - SAVE THIS RANK VALUE                   
RPTD0660 EQU   *                                                                
         LA    R3,ELTBUILD                                                      
         USING RSTRACCD,R3         USE DSECT FOR ACCOUNT ELT                    
         ZIC   R4,ACCTSEQ          INCREMENT SEQUENCE NUMBER                    
         LA    R4,1(R4)                                                         
         CH    R4,=H'12'           12 = MAX ELTS IN RECORD                      
         BNH   RPTD0760            ROOM TO GROW                                 
*                                  NO ROOM:  PUT OUT RECORD                     
         LA    R4,1                RESET COUNTER TO 1                           
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RSTRREC,DESCBILD               
*                                  INSERT X'50' ELT INTO RECORD                 
         CLI   QOPTION1,C'T'       TEST RUN                                     
         BE    RPTD0680            YES - NO REWRITE OF RECORD                   
         CLI   QOPTION1,C'X'       SPECIAL TEST RUN                             
         BE    RPTD0680            YES - NO REWRITE OF RECORD                   
         GOTO1 =A(RECWRITE),DMCB,(RC)                                           
         B     RPTD0720                                                         
RPTD0680 EQU   *                                                                
         CLC   =C'$$',QUESTOR                                                   
         BNE   RPTD0720                                                         
         CLI   QUESTOR+9,C'Y'      PRNTBL ACTIVE?                               
         BNE   RPTD0720            NO                                           
         ZICM  RF,RSTRLEN,2                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,RSTRREC),RSTRREC,C'DUMP',(RF),=C'2D'          
RPTD0720 EQU   *                                                                
         GOTO1 =A(DOREPORT),DMCB,(RC)                                           
         ZIC   RF,PAGENUM                                                       
         LA    RF,1(RF)            BUMP PAGENUM                                 
         STC   RF,PAGENUM                                                       
         STC   RF,RSTRKPG          INSERT INTO RECORD                           
         XCEFL RSTRELEM,960        CLEAR STRATEGY RECORD AFTER KEY              
         MVC   RSTRLEN,=H'34'      RESET LENGTH TO 'NO ELEMENTS'                
RPTD0760 EQU   *                                                                
         STC   R4,ACCTSEQ          REPLACE ACCOUNT SEQ VALUE                    
         STC   R4,RSTRACSQ         INSERT SEQUENCE INTO RECORD                  
         MVC   RSTRACCT,SADVERT    INSERT ADVERTISER                            
         MVC   RSTRAOFF,SOFF       INSERT OFFICE                                
         EDIT  SPRIOR$,(12,RSTRAPDO)                                            
*                                  INSERT PRIOR DOLLARS                         
         EDIT  SFORCAST,(12,RSTRASDL)                                           
*                                  INSERT FORECAST DOLLARS                      
         LA    R4,SPRISH$          CALCULATE PRIOR SHARE                        
         LA    R5,SPRIOR$$                                                      
         LA    R6,SSPLPCT          A(RESULT)                                    
         GOTO1 SHARCALC,DMCB,(RC),(R4),(R5),(R6)                                
         EDIT  SSPLPCT,(3,RSTRAPSH)                                             
*                                  INSERT PRIOR SHARE                           
         LA    R4,SFORCAST         CALCULATE SHARE GOAL                         
         LA    R5,SFORMKT                                                       
         LA    R6,SSHRGOL          A(RESULT)                                    
         GOTO1 SHARCALC,DMCB,(RC),(R4),(R5),(R6)                                
         EDIT  SSHRGOL,(3,RSTRASGL)                                             
*                                  INSERT SHARE GOAL                            
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RSTRREC,ELTBUILD               
*                                  INSERT NEW ELEMENT INTO RECORD               
         XC    ELTBUILD+2(L'ELTBUILD-2),ELTBUILD                                
*                                                                               
         B     RPTD0440            GO BACK FOR NEXT RECORD                      
         DROP  R3                                                               
*                                                                               
RPTD0800 EQU   *                                                                
         CLC   RSTRLEN,=H'34'      ANY ELEMENTS IN RECORD?                      
         BNH   RPTD0920            NO  - DON'T WRITE IT                         
         GOTO1 =V(HELLO),DMCB,(C'P',=C'REPFILE'),RSTRREC,DESCBILD               
*                                  INSERT X'50' ELT INTO RECORD                 
         CLI   QOPTION1,C'T'       TEST RUN                                     
         BE    RPTD0840            YES - NO REWRITE OF RECORD                   
         CLI   QOPTION1,C'X'       SPECIAL TEST RUN                             
         BE    RPTD0840            YES - NO REWRITE OF RECORD                   
         GOTO1 =A(RECWRITE),DMCB,(RC)                                           
         B     RPTD0880                                                         
RPTD0840 EQU   *                                                                
         CLC   =C'$$',QUESTOR                                                   
         BNE   RPTD0880                                                         
         CLI   QUESTOR+9,C'Y'      PRNTBL ACTIVE?                               
         BNE   RPTD0880            NO                                           
         ZICM  RF,RSTRLEN,2                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,RSTRREC),RSTRREC,C'DUMP',(RF),=C'2D'          
RPTD0880 EQU   *                                                                
         GOTO1 =A(DOREPORT),DMCB,(RC)                                           
RPTD0920 EQU   *                                                                
         GOTO1 =A(SITADATE),DMCB,(RC)                                           
*                                  UPDATE SITUATION ANALYSIS DATE               
         B     RPTD1000                                                         
RPTD0960 EQU   *                                                                
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P+1(34),=C'NO DATA WAS FOUND FOR THIS REQUEST'                   
         GOTO1 REPORT                                                           
         GOTO1 SORTER,DMCB,=C'END'                                              
*                                  CLOSE THE SORT                               
RPTD1000 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*   SHARCALC:  CALCULATE THE SHARE VALUE FROM ITS COMPONENTS.  RETURN           
*       THE VALUE FOR INSERTION INTO RECORD                                     
*              R4  =  STATION VALUE                                             
*              R5  =  MARKET VALUE                                              
*              R6  =  RETURN ADDRESS                                            
*                                                                               
SHARCALC NTR1                                                                   
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         L     R4,4(R1)            A(STATION VALUE)                             
         L     R5,8(R1)            A(MARKET VALUE)                              
         L     R6,12(R1)           RETURN ADDRESS FOR RESULT                    
         ZICM  R1,0(R4),4          STATION VALUE                                
****>>   BAS   RE,QUIKDISP                                                      
         SR    R0,R0                                                            
         M     R0,=F'100'          MULTIPLY FOR DECIMAL ALIGNMENT               
         ZICM  RF,0(R5),4          MARKET VALUE                                 
         XC    0(4,R6),0(R6)       CLEAR RETURN VALUE AREA                      
         LTR   RF,RF               ANY DIVISOR?                                 
         BZ    SCAL0200            NO  - DON'T DIVIDE                           
         SLDA  R0,1                DOUBLE THE DIVIDEND                          
         AR    R1,RF               ADD DIVISOR FOR HALF-ROUND                   
         DR    R0,RF                                                            
         SRA   R1,1                DIVIDE RESULT BY 2                           
         STCM  R1,15,0(R6)         STORE RESULT                                 
****>>   BAS   RE,QUIKDIS3                                                      
SCAL0200 EQU   *                                                                
         XIT1                      EXIT ROUTINE                                 
QUIKDISP NTR1                                                                   
         MVC   P+1(09),=C'STATION$='                                            
         EDIT  (R1),(12,P+11),DUB=MYDUB                                         
         MVC   P+25(09),=C'MARKET $='                                           
         L     R1,0(R5)                                                         
         EDIT  (R1),(12,P+35),DUB=MYDUB                                         
         GOTO1 REPORT                                                           
         XIT1                                                                   
QUIKDIS2 NTR1                                                                   
         MVC   P+1(09),=C'STAT2  $='                                            
         EDIT  (R1),(12,P+11),DUB=MYDUB                                         
         L     R1,DUB              MARKET $$ IN DUB                             
         MVC   P+25(09),=C'MARKET2$='                                           
         L     R1,0(R5)                                                         
         EDIT  (R1),(12,P+35),DUB=MYDUB                                         
         GOTO1 REPORT                                                           
         XIT1                                                                   
QUIKDIS3 NTR1                                                                   
         MVC   P+1(09),=C'STAT3  $='                                            
         EDIT  (R1),(12,P+11),DUB=MYDUB                                         
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
MYDUB    DS    D                                                                
         EJECT                                                                  
*                                                                               
*   DOREPORT:  PRODUCE A LISTING OF RECORDS GENERATED.                          
*                                                                               
DOREPORT NTR1                                                                   
         LA    R3,RSTRELEM         SET TO 1ST ELEMENT IN REC                    
         USING RSTRACCD,R3                                                      
         LA    R4,P                SET PRINT DSECT                              
         USING ACCDSECT,R4                                                      
*                                                                               
DORE0020 EQU   *                                                                
         CLI   0(R3),0             END OF RECORD?                               
         BE    DORE0200            YES - FINISHED DISPLAYING                    
         CLI   0(R3),X'20'         ACCOUNT ELEMENT?                             
         BNE   DORE0040            NO  -                                        
         MVC   DCODE,RSTRACCT      INSERT ADVERTISER CODE                       
         BAS   RE,ACCNAME          GET ADVERT NAME                              
         MVC   DOFF,RSTRAOFF       INSERT OFFICE NAME                           
         MVC   DPRISHR,RSTRAPSH    INSERT PRIOR SHARE                           
         MVC   DPRI$$,RSTRAPDO     INSERT PRIOR DOLLARS                         
         MVC   DSHRGOL,RSTRASGL    INSERT SHARE GOAL                            
         MVC   DSHR$$,RSTRASDL     INSERT SHARE DOLLARS                         
         GOTO1 REPORT                                                           
DORE0040 EQU   *                                                                
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     DORE0020            GO BACK FOR NEXT ELEMENT                     
DORE0200 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
*  ACCNAME:  RETRIEVE ADVERT RECORD, GET ADVERT NAME FOR DISPLAY.               
*                                                                               
ACCNAME  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,8               INSERT RECORD TYPE                           
         MVC   KEY+21(4),RSTRACCT  INSERT ADVERTISER CODE                       
         MVC   KEY+25(2),RCREPFL   INSERT REP CODE                              
         GOTO1 HIGH                READ FOR KEY                                 
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BE    ANAM0040            YES                                          
         MVC   DACCT(16),=C'CODE NOT ON FILE'                                   
         B     ANAM0100            EXIT                                         
ANAM0040 EQU   *                                                                
         LA    R5,RADVREC          SET A(IOAREA)                                
         ST    R5,AIOAREA                                                       
         GOTO1 GREC                RETRIEVE ADVERTISER RECORD                   
         MVC   DACCT,RADVNAME      INSERT ADVERTISER NAME                       
ANAM0100 EQU   *                                                                
         XIT1                                                                   
*                                                                               
         DROP  R3,R4                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
*  GENERATE SORT RECORDS:  INSERT RATE INTO EACH UNIQUE STATION FOR             
*     THESE RECORDS, WHICH CONTAIN THE CONTRACT DOLLARS                         
*                                                                               
SORTGEN  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         XIT1                                                                   
         EJECT                                                                  
*   SORT RETURN AND END-OF-FILE TESTING                                         
*                                                                               
GETSORT  NTR1                                                                   
         CLI   SORTREC,X'FF'       EOF REACHED?                                 
         BE    GSOR0900            YES                                          
         MVI   SORTREC,X'FF'       SET 'EOF REACHED'                            
         GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,DMCB+4                                                        
         LTR   R6,R6                                                            
         BZ    GSOR0900            TEST RETURN ZERO=EOF                         
         MVC   SORTREC(LSORTREC),0(R6)                                          
*                                  LOAD RECORD TO SORTREC                       
GSOR0900 EQU   *                      OVERLAYS 'EOF REACHED' FLAG               
         XIT1                                                                   
         EJECT                                                                  
ALTHDR   EQU   *                                                                
         NTR1                                                                   
         LA    R1,SAVEREGS-ALTHDR                                               
         AR    R1,RF                                                            
         LM    R2,RC,0(R1)                                                      
*                                                                               
         MVC   HEAD4+10(2),QGROUP                                               
         MVC   HEAD5+14(5),SSTATN                                               
         MVC   HEAD4+24(20),SVGRPNAM                                            
         MVC   HEAD5+24(20),SVSTAMKT                                            
         XIT1                                                                   
         EJECT                                                                  
         DS    0H                                                               
         TITLE 'DATA MANAGER INTERFACE  -- INCLUDE (RGENIO) CODE'               
       ++INCLUDE RGENIO                                                         
         EJECT                                                                  
BLANKLIN DC    X'0000'                                                          
ERRORLIN DC    C'T',AL1(32),X'0000'                                             
ALLTEXT  DC    C'T',AL1(32),X'0000'                                             
STNTEXT  DC    C'T',AL1(07),C'O',AL1(01),C'T',AL1(20),X'0000'                   
*              WORK SPACE ETC                                                   
UNDRSCOR DS    XL20                 SET TO UNDERSCORE FOR PRINTING              
         SPACE 3                                                                
CONFLAG  DS    CL1                                                              
DATAFLAG DS    CL1                 Y = DATA FOUND FOR PROCESSING                
PRTFLAG  DC    CL1'X'                                                           
*                                                                               
*   PRTFLAG SETTING:  X  = NO: FIRST PASS                                       
*                     N  = NO: ALL OTHER PASSES                                 
*                     Y  = YES: PRINT TOTALS                                    
*                                                                               
ACCFLAG  DC    CL1'Y'              ACCUMULATE/DON'T ACCUMULATE                  
STARTMO  DS    XL1                 REQUEST START MONTH                          
ENDMO    DS    XL1                 REQUEST END   MONTH                          
NUMMOS   DS    XL1                 NUMBER OF MONTHS IN REQUEST                  
         DS    0F                                                               
CONTOTS  DS    0CL16                                                            
         DC    4F'0'                                                            
TMONTBL  DS    0CL20                                                            
         DC    5F'00'              BUCKET ACCUMULATOR                           
*  5 FULLWORDS, ONE WORD PER BUCKET                                             
*        1ST FULLWORD   = PRIOR BOOKING                                         
*        2ND FULLWORD   = PRIOR SHARE                                           
*        3RD FULLWORD   = FORECAST DOLLARS                                      
*        4TH FULLWORD   = PRIOR STATION $$ FOR SHARE CALC                       
*        5TH FULLWORD   = FORECAST MARKET DOLLARS                               
         SPACE 1                                                                
         DC    CL12'**SORTREC **'                                               
SORTREC  DS    0F                                                               
SGRPSUB  DS    CL2                 GROUP/SUBGROUP                               
SSTATN   DS    CL5                 STATION+MEDIA                                
SZEROFLG DS    CL1                 ZERO VALUE FLAG FOR                          
*                                     NEGATIVE SORTING                          
STOPN    DS    CL4                 TOP 'N' SORT VALUE                           
SADVERT  DS    CL4                 ADVERTISER CODE                              
SOFF     DS    CL2                 OFFICE CODE                                  
         DS    CL6                 SPARE WITHIN SORT KEY                        
LSORTKEY EQU   *-SORTREC           L(SORTKEY)                                   
SPRIOR$  DS    CL4                 PRIOR YEAR ACTUAL                            
SPRISH$  DS    CL4                 PRIOR YEAR SHARE STATION DOLLARS             
SPRIOR$$ DS    CL4                 ACTUAL / STATION SPL %                       
SSPLPCT  DS    CL4                 STATION SPL %                                
SFORCAST DS    CL4                 FORECAST DOLLARS                             
SFORMKT  DS    CL4                 PROJECTED FORECAST MARKET $$                 
SSHRGOL  DS    CL4                 SHARE GOAL                                   
SCONNUM  DS    CL4                 CONTRACT NUMBER                              
         DS    CL12                SPARE                                        
LSORTDTA EQU   *-SPRIOR$           L(SORT DATA)                                 
LSORTREC EQU   *-SORTREC           L(SORTREC)                                   
*                                                                               
*                                                                               
         DC    CL12'**SORTREC2**'                                               
SORTREC2 DS    0CL(LSORTREC)       SAVED SORTKEY                                
SGRPSUB2 DS    CL2                 GROUP/SUBGROUP                               
SSTATN2  DS    CL5                 STATION+MEDIA                                
SZEROFL2 DS    CL1                 ZERO VALUE FLAG FOR                          
*                                     NEGATIVE SORTING                          
STOPN2   DS    CL4                 TOP 'N' SORT VALUE                           
SADVERT2 DS    CL4                 ADVERTISER CODE                              
SOFF2    DS    CL2                 OFFICE CODE                                  
         DS    CL6                 SPARE WITHIN SORT KEY                        
*                                                                               
SPRIOR$2 DS    CL4                 PRIOR YEAR ACTUAL                            
SPRISH$2 DS    CL4                 PRIOR YEAR SHARE STATION DOLLARS             
SPRIOR2$ DS    CL4                 ACTUAL / STATION SPL %                       
SSPLPCT2 DS    CL4                 STATION SPL %                                
SFORCST2 DS    CL4                 FORECAST DOLLARS                             
SFORMKT2 DS    CL4                 PROJECTED FORECAST MARKET $$                 
SSHRGOL2 DS    CL4                 SHARE GOAL                                   
SCONNUM2 DS    CL4                 CONTRACT NUMBER                              
         DS    CL12                SPARE                                        
*                                                                               
RESETREC DS    CL(LSORTREC)                                                     
*                                                                               
*                                                                               
SUBPRG   DS    XL1                 SUBPROGRAM INCREMENT                         
*                                                                               
SAVESTA  DC    CL5'     '          SAVED STATION CALLS                          
SAVEGRUP DC    CL2'  '             SAVED GROUP CODE                             
KEYSAV3  DS    CL27                ALTERNATE KEY SAVE AREA                      
ELCODE   DS    X                   ELEMENT CODE FOR GETEL                       
TESTCNTR DS    XL1'00'             **TEST**                                     
SORTACUM DS    F                   SORT ACCUMULATOR                             
STDATE   DS    CL3      B          REPORT PERIOD START                          
ENDATE   DS    CL3      B          REPORT PERIOD END                            
STDATE9S DS    CL3      B          REPORT PERIOD START 9S COMP                  
ENDATE9S DS    CL3      B          REPORT PERIOD END   9S COMP                  
FLIGHTST DS    CL3      B          FLIGHT START OF CONTRACT                     
FLIGHTND DS    CL3      B          FLIGHT END OF CONTRACT                       
*                                                                               
GENWORK  EQU   *                                                                
*                                                                               
AIOAREA  DS    F                                                                
TESTCTR  DS    F                   TEST COUNTER                                 
RANKMAX  DS    F                   RANK MAXIMUM VALUE                           
RANKCTR  DS    F                   RANK COUNTER                                 
RANKCTR2 DS    F                   RANK COUNTER2                                
LASTRANK DS    F                   LAST RANK VALUE                              
*                                                                               
PROCCTR  DC    PL4'0'              CONTRACTS PROCESSED CTR                      
PAGENUM  DS    X                   PAGE NUMBER COUNTER                          
ACCTSEQ  DS    X                   ACCOUNT SEQUENCE COUNTER                     
DELETFLG DC    CL1'N'              DELETE FLAG - INITIALLY, NOT DONE            
*                                                                               
         DC    CL12'**ELTBUILD**'                                               
ELTBUILD DS    CL64                ELEMENT BUILD AREA                           
DESCBILD DS    CL12                X'50' ELEMENT BUILD AREA                     
COMMAND  DS    CL8                                                              
ACTIVE   DS    CL1                                                              
*                                                                               
QFASTART DS    CL6                                                              
QFAEND   DS    CL6                                                              
*                                                                               
MYHED    DS    CL132               FOR MARKET NAME AND OPTIONS                  
SVSTAMKT DS    CL20                                                             
SVGRPNAM DS    CL20                                                             
SORTCARD DC    CL80'SORT FIELDS=(1,24,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=68'                                    
         SPACE 2                                                                
RELO     DS    F                   RELOCATION ADDRESS                           
SAVEREGS DS    11F                                                              
*                                                                               
RGWORK   DCB   DDNAME=RGWORK,DSORG=PS,EODAD=*,LRECL=LSORTREC,          X        
               BLKSIZE=16*LSORTREC,MACRF=(GM,PM),RECFM=FB                       
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE REGENALL1A                                                     
         ORG   RCONREC                                                          
       ++INCLUDE REGENSTR                                                       
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE REXADDRD                                                       
ACCDSECT DSECT                                                                  
         DS    CL1                                                              
DCODE    DS    CL4                                                              
         DS    CL1                                                              
DACCT    DS    CL20                                                             
         DS    CL2                                                              
DOFF     DS    CL2                                                              
         DS    CL8                                                              
DPRISHR  DS    CL3                                                              
         DS    CL4                                                              
DPRI$$   DS    CL12                                                             
         DS    CL7                                                              
DSHRGOL  DS    CL3                                                              
         DS    CL4                                                              
DSHR$$   DS    CL12                                                             
         EJECT                                                                  
         CSECT                                                                  
*                                                                               
*   RECWRITE:  CHECKS FOR KEYS, DELETED RECORDS, ETC....                        
*                                                                               
RECWRITE NMOD1 0,*WRIT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         CLI   DELETFLG,C'Y'       THIS SET DELETED ALREADY?                    
         BE    RECW0040            YES - DON'T DO IT AGAIN                      
         BAS   RE,DELETREC         NO  - DELETE THE SET OF RECORDS              
         MVI   DELETFLG,C'Y'       SET DELETE FLAG TO 'YES'                     
RECW0040 EQU   *                                                                
         MVC   KEY,RSTRREC         SET KEY FROM NEW RECORD                      
         GOTO1 DATAMGR,DMCB,(X'88',ADDREC),=C'REPFILE',KEY,            X        
               RSTRREC,IOWORK                                                   
*                                  ATTEMPT TO ADD NEW RECORD                    
         CLI   P3,0                ERROR RETURN?                                
         BZ    RECW0400            NO  - SUCCESSFUL                             
         CLI   P3,X'20'            YES - DUPE KEY?                              
         BE    *+6                 YES -                                        
         DC    H'0'                NO  - CAN'T PROCESS: DUMP                    
*                                                                               
*                                  RESTORE THE KEY/OVERWRITE RECORD             
         MVC   KEY,RSTRREC         SET UP KEY AGAIN                             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'REPDIR',KEYSAVE,KEY               
*                                  READ FOR THE KEY                             
         CLC   KEYSAVE(27),KEY     REDUNDANT CHECK: KEY FOUND?                  
         BE    *+6                 YES                                          
         DC    H'0'                ???                                          
         NI    KEY+27,X'FF'-X'80'  RESTORE KEY                                  
         GOTO1 DATAMGR,DMCB,(X'88',GETREC),=C'REPFILE',KEY+28,         X        
               RBUYREC,IOWORK                                                   
*                                  RETRIEVE INTO 2ND IO AREA                    
         NI    RBUYREC+29,X'FF'-X'80'                                           
*                                  RESTORE CONTROL IN RECORD                    
         GOTO1 DATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,         X        
               RSTRREC,IOWORK                                                   
*                                  REWRITE FROM NEW REC: IO AREA # 1            
         GOTO1 DATAMGR,DMCB,(X'88',DMWRT),=C'REPDIR',KEYSAVE,KEY                
*                                  REWRITE CLEARED KEY                          
RECW0400 EQU   *                                                                
         XIT1                                                                   
*                                                                               
IOWORK   DS    12D                                                              
         EJECT                                                                  
*                                                                               
*   DELETREC:  MARKS EXISTING RECORDS FOR THIS SET AS 'DELETED'                 
*                                                                               
DELETREC NTR1                                                                   
         XC    KEY,KEY             CLEAR KEY AREA                               
         MVC   KEY(26),RSTRREC     INSERT KEY:  EXCEPT PAGE#                    
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'REPDIR',KEYSAVE,KEY               
*                                  READ FOR THE KEY                             
         B     DELE0040                                                         
DELE0020 EQU   *                                                                
         GOTO1 DATAMGR,DMCB,(X'88',DMRSEQ),=C'REPDIR',KEYSAVE,KEY               
*                                  READ FOR THE KEY                             
DELE0040 EQU   *                                                                
         CLC   KEYSAVE(26),KEY     SAME KEY, EXCEPT PAGE#?                      
         BNE   DELE0120            NO  - FINISHED                               
*                                                                               
DELE0060 EQU   *                                                                
         GOTO1 DATAMGR,DMCB,(X'88',GETREC),=C'REPFILE',KEY+28,         X        
               RBUYREC,IOWORK                                                   
*                                  YES - RETRIEVE RECORD                        
         OI    RBUYCNTL,X'80'      SET DELETE BIT IN RBUYREC!                   
         GOTO1 DATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,         X        
               RBUYREC,IOWORK                                                   
*                                  REWRITE DELETED REC                          
         OI    KEY+27,X'80'        SET DELETE BIT IN KEY                        
         GOTO1 DATAMGR,DMCB,(X'88',DMWRT),=C'REPDIR',KEYSAVE,KEY                
*                                  REWRITE DELETED KEY                          
         B     DELE0020            GO BACK FOR NEXT                             
DELE0120 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*   SITADATE:  INSERT UPDATE DATE INTO SITUATION ANALYSIS RECORD                
*                                                                               
         DS    0F                                                               
SITADATE NMOD1 0,*SITA*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    KEY,KEY                                                          
         MVC   KEY(26),RSTRREC     INSERT KEY FROM LAST ACCT REC                
         MVI   KEY+12,1            INSERT 'SITUATION ANALYSIS' TYPE             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'80',DMRDHI),=C'REPDIR',KEYSAVE,KEY               
*                                  READ FOR THE KEY FOR UPDATE                  
         CLC   KEYSAVE(27),KEY     SAME KEY?                                    
         BE    SITA0060            YES -                                        
         DC    H'0'                NO SITUATION ANALYSIS RECORD: ABORT          
*                                                                               
SITA0060 EQU   *                                                                
         GOTO1 DATAMGR,DMCB,(X'88',GETREC),=C'REPFILE',KEY+28,         X        
               RBUYREC,IOWRK2                                                   
*                                  RETRIEVE RECORD INTO RBUYREC                 
         GOTO1 DATCON,DMCB,(5,WORK),(2,WORK+6)                                  
*                                  GET TODAY'S DATE, COMPRESSED                 
         LA    RF,RBUYREC          INSERT DATE INTO RECORD                      
         MVC   RSTRDUPT-RSTRREC(2,RF),WORK+6                                    
*                                                                               
         CLI   QOPTION1,C'T'       TEST RUN                                     
         BE    SITA0080            YES - NO REWRITE OF RECORD                   
         CLI   QOPTION1,C'X'       SPECIAL TEST RUN                             
         BE    SITA0080            YES - NO REWRITE OF RECORD                   
         GOTO1 DATAMGR,DMCB,(X'88',PUTREC),=C'REPFILE',KEY+28,         X        
               RBUYREC,IOWRK2                                                   
*                                  REWRITE REC WITH NEW DATE                    
         B     SITA0120                                                         
SITA0080 EQU   *                                                                
         CLI   QUESTOR+9,C'Y'      PRNTBL ACTIVE?                               
         BNE   SITA0120            NO                                           
         ZICM  RF,RBUYLEN,2                                                     
         GOTO1 =V(PRNTBL),DMCB,(0,RBUYREC),RBUYREC,C'DUMP',(RF),=C'2D'          
*                                  PRNTBL RECORD FROM RBUYREC                   
SITA0120 EQU   *                                                                
         XIT1                                                                   
*                                                                               
IOWRK2   DS    12D                                                              
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*   SETHDGS:  RETRIEVE EXPANSIONS FOR GROUP, STATION RECORDS                    
*                                                                               
SETHDGS  NMOD1 0,*WRIT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,2               INSERT STATION RECORD TYPE                   
         MVC   KEY+20(2),RCREPFL   INSERT REP CODE                              
         MVC   KEY+22(5),SSTATN    INSERT STATION LETTERS                       
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                   
*                                  READ FOR THE STATION KEY                     
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NO  - ???                                    
         GOTO1 DATAMGR,DMCB,(0,GETREC),=C'REPFILE',KEY+28,             X        
               RSTAREC,IOWRK3                                                   
         MVC   SVSTAMKT,RSTAMKT    SAVE STATION MARKET NAME                     
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,7               INSERT GROUP RECORD TYPE                     
         MVC   KEY+23(2),RCREPFL   INSERT REP CODE                              
         MVC   KEY+25(2),QGROUP    INSERT GROUP/SUBGRP CODE                     
*                                     FROM REQUEST CARD                         
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),=C'REPDIR',KEYSAVE,KEY                   
*                                  READ FOR THE GROUP/SUBGROUP KEY              
         CLC   KEYSAVE(27),KEY     KEY FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                NO  - ???                                    
         GOTO1 DATAMGR,DMCB,(0,GETREC),=C'REPFILE',KEY+28,             X        
               RGRPREC,IOWRK3                                                   
         MVC   SVGRPNAM,RGRPNAME   SAVE GROUP/SUBGROUP NAME                     
         XIT1                                                                   
IOWRK3   DS    12D                                                              
         EJECT                                                                  
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065REREP1K02S05/01/02'                                      
         END                                                                    
