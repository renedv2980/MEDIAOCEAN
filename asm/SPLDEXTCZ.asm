*          DATA SET SPLDEXTCZ  AT LEVEL 084 AS OF 06/26/00                      
*PHASE SPEXTCZB                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE CLPACK                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'SPLDEXT -MOVE SPOT DATA TO ANOTHER FILE'                        
***********************************************************************         
*                                                                     *         
*        MOVE HEADERS,BUYS AND GOALS FROM ONE AGENCY TO ANOTHER       *         
*                                                                     *         
*        GENERALIZED TO READ INPUT FOR AGENCIES TO MOVE               *         
*                                                                     *         
*        BEFORE RUNNING COMPILE AND LINK STREPFXMVC                   *         
*              IN CASE PHASE SPFX02C HAS BEEN OVERWRITTEN             *         
*        ALSO RUN BOBY.DDS.JCL(SFXTO & SFXFROM)                       *         
*              CREATE STATION INPUT DATASETS                          *         
*                                                                     *         
*        PARAMETERS PASSED IN NORMAL SPOT REQUEST CARD                *         
*              QAGY SOURCE AGENCY                                     *         
*              QMED SOURCE MEDIA                                      *         
*              QPRD MUST BE 'ALL'                                     *         
*              QEST STARTING ESTIMATE TO MOVE                         *         
*              QESTEND END OF ESTIMATE RANGE OR BLANK FOR 1 ESTIMATE  *         
*              QOPT1 MOVE BUYS                                        *         
*              QOPT2 MOVE GOALS                                       *         
*              QOPT3 MOVE WORKSHEET RECORDS - NEEDS TO TESTED         *         
*              QOPT4 'C' - DO NOT MOVE CLIENT HEADERS                 *         
*                    'P' - DO NOT MOVE CLIENT/PRODUCT HEADERS         *         
*                    'E' - DO NOT MOVE CLIET/PRODUCT/ESTIMATE HDRS    *         
*              QOPT5 MOVE CABLE BUYS                                  *         
*              QUESTOR NEW CLIENT CODE - PRODUCTS REMAIN THE SAME     *         
*                                                                     *         
*        SEVERAL REQUEST CARDS CAN BE SUBMITTED                       *         
*        SEVERAL CLT/PRD/EST COMBINATIONS ALLOWED                     *         
*                                                                     *         
*              NEW AGENCY FOUND FROM 'TO' STATION FILE                *         
*                                                                     *         
*        DO NOT DELETE                                                *         
*        SAVE FOR POSTERITY                                           *         
*                                                                     *         
*        BIGGEST PROBLEM IS MATCHING MARKET NUMBERS. THIS IS DONE     *         
*        BY READING IN DATASETS WITH EXTRACTS FROM BOTH STATION FILES *         
*                                                                     *         
*        THIS MODULE RESTRICTED TO SPTDIR/FIL                         *         
*                                                                     *         
*        THIS MODULE DOES NOT MOVE LOCAL CABLE BUYS.                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
* PARAMETER LIST                                                      *         
*                                                                     *         
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                     *         
*                               X'01'= RECORD IN CORE                 *         
*                               X'FF'= END OF FILE                    *         
*               RETURN VALUE    X'00'= KEEP RECORD                    *         
*                               X'FF'= PURGE RECORD                   *         
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ        *         
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                     *         
*                               X'40'= TAPE OUTPUT                    *         
*                               X'20'= RECORD IS I/S FILE RECORD      *         
* P3=A(PARAM CARD)                                                    *         
* P4=A(FILE DEFN)                                                     *         
* P5=A(PRINTER)                                                       *         
* P6=A(CPRINT)                                                        *         
*                                                                     *         
***********************************************************************         
         TITLE 'SPLDEXTCZ - MOVE BETWEEN AGENCIES - INIT'                       
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 0,SPLDEXT                                                        
*                                                                               
         L     RC,=A(WORKD)        ESTABLISH WORKING STORAGE                    
         USING WORKD,RC                                                         
*                                                                               
         ST    R1,APARM            SAVE A(PARAMETER LIST)                       
         MVC   PLIST,0(R1)         SAVE PARAMETER LIST                          
*                                                                               
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
         L     R9,VLDDEFN          ESTABLISH LOAD CONTROLS                      
         USING LDDEFND,R9                                                       
*                                                                               
         L     RF,=V(HEXOUT)                                                    
         ST    RF,VHEXOUT                                                       
*                                                                               
         L     RF,=V(CLPACK)                                                    
         ST    RF,VCLPACK                                                       
*                                                                               
         B     INITX                                                            
*                                                                               
INITX    DS    0H                                                               
*                                                                               
         TITLE 'SPLDEXTCZ - MOVE BETWEEN AGENCIES - DMCTL'                      
***********************************************************************         
*                                                                     *         
*        CONTROL FLOW LOGIC                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXCTL   DS    0H                                                               
*                                                                               
         CLI   PRMMODE,PRMMINIQ                                                 
         BE    DMXINIT             INITIALIZE                                   
*                                                                               
         CLI   PRMMODE,PRMMRECQ    NEW RECORD IN CORE                           
         BE    DMXREC              PROCESS                                      
*                                                                               
         CLI   PRMMODE,PRMMEOFQ                                                 
         BE    DMXEOF              END-OF-FILE                                  
*                                                                               
         B     DMXIT                                                            
*                                                                               
*        EXITS                                                                  
*                                                                               
DMXEOF   DS    0H                                                               
*                                                                               
         CLOSE FILEOUT             CLOSE OUTPUT TAPE                            
*                                                                               
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         MVC   MID2,SPACES         CLEAR HEADLINES                              
         MVC   MID3,SPACES                                                      
*                                                                               
         GOTO1 VPRINTER            PRINT BLANK LINE                             
*                                                                               
         MVC   P(25),=C'RECORDS ADDED TO FILE = '                               
         EDIT  (P8,RECCNTR),(15,P+25),COMMAS=YES,ALIGN=LEFT                     
*                                                                               
         GOTO1 VPRINTER            PRINT RECORD COUNT                           
*                                                                               
         GOTO1 VPRINTER            PRINT BLANK LINE                             
*                                                                               
         B     DMXIT                                                            
*                                                                               
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),PRMRKPQ                                                    
         B     DMXIT                                                            
*                                                                               
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),PRMRPRGQ                                                   
         B     DMXIT                                                            
*                                                                               
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),PRMREOJQ                                                   
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
*                                                                               
DMXIT    XMOD1 1                                                                
*                                                                               
         TITLE 'SPLDEXTCZ - MOVE BETWEEN AGENCIES - DMXINIT'                    
***********************************************************************         
*                                                                     *         
*        PROGRAM INITIALIZATION                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXINIT  DS    0H                                                               
*                                                                               
         ZAP   RECCNTR,=P'0'                                                    
*                                                                               
         L     R4,=A(MVTAB)        POINT TO SELECTION TABLE                     
         USING MVTABD,R4           ESTABLISH TABLE                              
*                                                                               
         OPEN  (CARD,INPUT)        OPEN REQUEST CARD INPUT                      
*                                                                               
TABINILP DS    0H                                                               
*                                                                               
         XC    MVTENT(MVTENTL),MVTENT INIT TABLE ENTRY                          
*                                                                               
         L     R1,=A(CARD)                                                      
         GET   (1),QAREA           READ IN REQUEST CARD                         
*                                                                               
         MVC   MVTAGOLD,QAGY       SAVE OLD AGENCY CODE                         
*                                                                               
         MVI   MVTAMOLD,X'01'      ASSUME MEDIA TV                              
*                                                                               
         CLI   QMED,C'R'           IF MEDIA IS RADIO                            
         BNE   *+8                                                              
         MVI   MVTAMOLD,X'02'         RESET AGYMEDIA CODE                       
*                                                                               
         CLI   QMED,C'N'           IF MEDIA IS NETWORK                          
         BNE   *+8                                                              
         MVI   MVTAMOLD,X'03'         RESET AGYMEDIA CODE                       
*                                                                               
         CLI   QMED,C'C'           IF MEDIA IS CANADA                           
         BNE   *+8                                                              
         MVI   MVTAMOLD,X'08'         RESET AGYMEDIA CODE                       
*                                                                               
         MVC   MVTAMNEW,MVTAMOLD   INIT NEW AGENCY/MEDIA                        
*                                                                               
*        WILL GET AGENCY PART FROM STATION DATASET                              
*                                                                               
         MVC   MVTCLT,QCLT         GET CLIENT CODE                              
*                                                                               
         GOTO1 VCLPACK,DMCB,MVTCLT,MVTCLTPK   PACK CLIENT CODE                  
*                                                                               
         MVC   MVTCLTNW,MVTCLT     INIT NEW CLIENT                              
         MVC   MVTCLPKN,MVTCLTPK                                                
*                                                                               
         CLC   QNWCLT,SPACES       DONE IF NO NEW CLIENT                        
         BNH   TABINI05                                                         
*                                                                               
         MVC   MVTCLTNW,QNWCLT     SAVE NEW CLIENT                              
*                                                                               
         GOTO1 VCLPACK,DMCB,MVTCLTNW,MVTCLPKN   PACK CLIENT CODE                
*                                                                               
TABINI05 DS    0H                                                               
*                                                                               
         XC    MVTPRD,MVTPRD       ASSUME ALL PRODUCTS                          
         XC    MVTPRDCD,MVTPRDCD   ASSUME ALL PRODUCTS                          
*                                                                               
         CLC   =C'ALL',QPRD        OKAY IF ALL PRODUCTS                         
         BE    TABINI06                                                         
*                                                                               
         CLC   =C'POL',QPRD        OKAY IF PRODUCT POL                          
         BE    TABINI06                                                         
*                                                                               
         MVC   MVTPRD,QPRD            ELSE SAVE PRODUCT                         
*                                                                               
TABINI06 DS    0H                                                               
*                                                                               
         MVI   MVTESTST,1          ASSUME ALL ESTIMATES WANTED                  
         MVI   MVTESTEN,255                                                     
*                                                                               
         CLC   QEST,=C'   '        SKIP IF ALL ESTIMATES                        
         BE    TABINI10                                                         
         CLC   QEST,=C'ALL'        SKIP IF ALL ESTIMATES                        
         BE    TABINI10                                                         
*                                                                               
         PACK  DUB,QEST            PACK ESTIMATE NUMBER                         
         CVB   RF,DUB              CVB                                          
         STC   RF,MVTESTST                                                      
*                                                                               
         MVC   MVTESTEN,MVTESTST   INIT END ESTIMATE                            
*                                                                               
         CLC   QESTEND,=C'   '     IF NOT A RANGE                               
         BH    TABINI08                                                         
*                                                                               
         CLC   QNWEST,=C'   '      SKIP IF NOT NEW ESTIMATE                     
         BNH   TABINI10                                                         
*                                  ELSE PACK NEW ESTIMATE NUMBER                
         PACK  DUB,QNWEST                                                       
         CVB   RF,DUB                                                           
         STC   RF,MVTESTNW         SAVE IN TABLE                                
*                                                                               
         B     TABINI10                                                         
*                                                                               
TABINI08 DS    0H                                                               
*                                                                               
         PACK  DUB,QESTEND         PACK ESTIMATE NUMBER                         
         CVB   RF,DUB              CVB                                          
         STC   RF,MVTESTEN                                                      
*                                                                               
TABINI10 DS    0H                                                               
         CLC   QSTART,SPACES                                                    
         BE    TABINI12                                                         
         GOTO1 LDATCON,DMCB,QSTART,(2,MVTSTDTE)                                 
*                                                                               
TABINI12 CLC   QEND,SPACES                                                      
         BE    TABINI14                                                         
*                                                                               
         GOTO1 LDATCON,DMCB,QEND,(2,MVTNDDTE)                                   
*                                                                               
TABINI14 DS    0H                                                               
*                                                                               
         CLC   QMKT,SPACES         IF MARKET PRESENT                            
         BNH   *+18                                                             
         PACK  DUB,QMKT               PACK                                      
         CVB   RF,DUB                                                           
         STCM  RF,3,MVTMKT            SAVE                                      
*                                                                               
         CLI   MV1STTIM,C'N'       SKIP IF NOT FIRST TIME                       
         BE    TABINI90                                                         
*                                                                               
         MVI   MV1STTIM,C'N'       RESET FIRST TIME SWITCH                      
*                                                                               
         CLI   QOPT1,C'Y'          TEST FOR BUYS CONVERSION                     
         BNE   *+8                                                              
         MVI   MVBUYS,C'Y'                                                      
*                                                                               
         CLI   QOPT2,C'Y'          TEST FOR GOALS CONVERSION                    
         BNE   *+8                                                              
         MVI   MVGOALS,C'Y'                                                     
*                                                                               
         CLI   QOPT3,C'Y'          TEST FOR BUYERS WORKSHEET CONVERSION         
         BNE   *+8                                                              
         MVI   MVNWS,C'Y'                                                       
*                                                                               
         CLI   QOPT4,C'C'          TEST FOR CLIENT  RECORD SKIPPED              
         BE    TABINICL                                                         
         CLI   QOPT4,C'P'                                                       
         BE    TABINICL                                                         
         CLI   QOPT4,C'E'                                                       
         BNE   *+8                                                              
TABINICL MVI   MVCLT,C'Y'                                                       
*                                                                               
         CLI   QOPT4,C'P'          TEST FOR PRODUCT RECORD SKIPPED              
         BE    TABINIPR                                                         
         CLI   QOPT4,C'E'                                                       
         BNE   *+8                                                              
TABINIPR MVI   MVPRD,C'Y'                                                       
*                                                                               
         CLI   QOPT4,C'E'          TEST FOR ESTIMATE RECORD SKIPPED             
         BNE   *+8                                                              
TABINIES MVI   MVEST,C'Y'                                                       
*                                                                               
         CLI   QOPT5,C'Y'          MOVING CABLE BUYS?                           
         BNE   *+8                                                              
         MVI   MVCABLE,C'Y'                                                     
*                                                                               
TABINI90 DS    0H                                                               
*                                                                               
TABINICN DS    0H                                                               
*                                                                               
         LA    R4,MVTENTL(R4)      BUMP TO NEXT TABLE ENTRY                     
         B     TABINILP                                                         
*                                                                               
TABINIDN DS    0H                                                               
*                                                                               
         MVI   0(R4),X'FF'         SET END OF TABLE                             
*                                                                               
         CLOSE CARD                CLOSE INPUT FILE                             
*                                                                               
TABINITX DS    0H                                                               
*                                                                               
         MVI   EOFTOSW,0           INIT END OF FILE SWITCH                      
*                                                                               
         TITLE 'SPLDEXTCZ - MOVE BETWEEN AGENCIES - DMXISTA'                    
***********************************************************************         
*                                                                     *         
*        PROGRAM INITIALIZATION - STATION TABLE                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXISTA  DS    0H                                                               
*                                                                               
         OPEN  (FSTAFROM,(INPUT))    OPEN FROM AGENCY STATION FILE              
*                                                                               
         LTR   RF,RF               MUST HAVE A VALID READ                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (FSTATO,(INPUT))      OPEN TO   AGENCY STATION FILE              
*                                                                               
         LTR   RF,RF               MUST HAVE A VALID READ                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        ISSUE GETMAIN FOR STORAGE TO HOLD CONVERSION TABLE                     
*                                                                               
         LHI   R0,STANTRYL         RECORD LENGTH                                
         MHI   R0,STTRMAXQ         *MAXIMUM NUMBER OF RECORDS                   
*                                                                               
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STCM  R1,15,STATABA       SAVE A(GETMAIN AREA)                         
         LR    R3,R1               SAVE A(GETMAIN AREA)                         
         SR    R5,R5                                                            
*                                                                               
*        INITIALIZE BINSRCH PARAMETERS                                          
*                                                                               
         LA    R2,BSPSTA           POINT TO STATION BINSRCH PARAMETERS          
         USING BSRPRMD,R2          ESTABLISH BINSRCH PARAMETERES                
*                                                                               
         MVC   BSPATAB,STATABA     A(TABLE)                                     
*                                                                               
         LA    RF,STANTRYL         SET ENTRY LENGTH                             
         ST    RF,BSPLENR                                                       
*                                                                               
         XC    BSPNOR,BSPNOR       INIT RECORD COUNTER                          
*                                                                               
         LA    RF,STTKEYLQ         SET KEY LENGTH                               
         ST    RF,BSPLENK                                                       
         MVI   BSPKEYD,STTKEY-STANTRY KEY DISPLACEMENT                          
*                                                                               
         LHI   RF,STTRMAXQ         SET # OF AVAILABLE ENTRIES                   
         ST    RF,BSPMAX                                                        
*                                                                               
*        FILL CONVERSION TABLE FROM INPUT FILE                                  
*                                                                               
         LA    R5,FROMRECC         ESTABLISH FROM AGENCY STATION REC            
         USING FRMRECD,R5                                                       
*                                                                               
         LA    R6,TORECC           ESTABLISH TO   AGENCY STATION REC            
         USING TORECD,R6                                                        
*                                                                               
         LA    R7,STANTRYC         ESTABLISH TABLE RECORD                       
         USING STANTRYD,R7                                                      
*                                                                               
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         MVC   MID1+36(25),=CL25'CONVERSION TABLES'                             
         MVC   MID2+13(25),=CL25'OLD AGENCY DATA'                               
         MVC   MID2+47(25),=CL25'NEW AGENCY DATA'                               
*                                                                               
         LA    R1,MID3                                                          
         USING PLINED,R1           ESTABLISH PRINT LINE                         
*                                                                               
         MVC   POLDAGY,=C'AG'                                                   
         MVC   POLDAM,=C'AM'                                                    
         MVC   POLDMSTA,=CL10' MKT/STA'                                         
         MVC   POLDMK,=CL04'MKT '                                               
         MVC   POLDSTA,=CL05'STA '                                              
         MVC   POLDCLT,=CL03'CLT'                                               
         MVC   PNEWAGY,=C'AG'                                                   
         MVC   PNEWAM,=C'AM'                                                    
         MVC   PNEWMSTA,=CL10' MKT/STA'                                         
         MVC   PNEWMK,=CL04'MKT '                                               
         MVC   PNEWSTA,=CL05'STA '                                              
         MVC   PNEWCLT,=CL03'CLT'                                               
*                                                                               
         MVC   PLINED+90(18),=C'TABLE ENTRY IN HEX'                             
*                                                                               
         MVI   MID4,X'41'          FORCE BLANK LINE                             
*                                                                               
         DROP  R1                                                               
*                                                                               
DMXINLP  DS    0H                                                               
*                                                                               
         XC    STANTRY(STANTRYL),STANTRY   INIT TABLE ENTRY                     
*                                                                               
         L     R1,=A(FSTAFROM)                                                  
         GET   (1),FRMREC           READ NEXT RECORD                            
*                                                                               
*        COMPARE TO CURRENT TO RECORD                                           
*                                                                               
DMXINTLP DS    0H                                                               
*                                                                               
         CLC   FRMTYPE,TOTYPE      TYPES MUST MATCH                             
         BH    DMXINTCN                                                         
         BL    DMXINTDN                                                         
*                                                                               
         CLC   FRMMED,TOMED        MEDIA MUST MATCH                             
         BH    DMXINTCN                                                         
         BL    DMXINTDN                                                         
*                                                                               
         CLC   FRMCALL,TOCALL      STATIONS MUST MATCH                          
         BH    DMXINTCN                                                         
         BL    DMXINTDN                                                         
*                                                                               
         CLC   TOCLT,=C'000'       TOREC CAN BE MASTER STATION REC              
         BE    DMXINTFD                                                         
*                                                                               
*        TOREC CAN BE CLIENT SPECIFIC                                           
*                                                                               
         L     R4,=A(MVTAB)        POINT TO SELECTION TABLE                     
         USING MVTABD,R4           ESTABLISH TABLE                              
*                                                                               
DMXINTCL DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    DMXINTCD                                                         
*                                                                               
         CLC   FRMCLT,MVTCLT       MATCH ON OLD CLIENT                          
         BE    DMXINTCF                                                         
*                                                                               
DMXINTCC DS    0H                                                               
*                                                                               
         LA    R4,MVTENTL(R4)      BUMP TO NEXT TABLE ENTRY                     
         B     DMXINTCL                                                         
*                                                                               
DMXINTCD DS    0H                                                               
*                                                                               
         B     DMXINTCN            DROP                                         
*                                                                               
DMXINTCF DS    0H                                                               
*                                                                               
         CLC   MVTCLTNW,TOCLT      MATCH ON NEW CLIENT                          
         BNE   DMXINTCN                                                         
*                                                                               
         B     DMXINTFD            MATCH FOUND                                  
*                                                                               
DMXINTCN DS    0H                                                               
*                                                                               
         CLI   EOFTOSW,0           SKIP IF END OF FILE WAS REACHED              
         BNE   DMXINTDN                                                         
*                                                                               
         L     R1,=A(FSTATO)                                                    
         GET   (1),TOREC           READ NEXT TO RECORD                          
*                                                                               
         B     DMXINTLP                                                         
*                                                                               
DMXINTC1 DS    0H                                                               
*                                                                               
         MVI   EOFTOSW,X'FF'       INDICATE TO FILE EXHAUSTED                   
*                                                                               
DMXINTDN DS    0H                  NO MATCH                                     
*                                                                               
*        STATION NOT ON RECEIVING AGENCY FILE                                   
*                                                                               
*        SET AGENCY/MEDIA                                                       
*                                                                               
         MVC   STTAMOLD,FRMAGYMD   SET WITH FROM AGY/MED                        
         MVC   STTMSOLD,FRMMKSTA   SET WITH FROM MKT/STA                        
         MVC   STTMKOLD,FRMMKT     SET WITH FROM MARKET                         
         MVC   STTSTOLD,FRMCALL    SET WITH FROM STATION                        
         MVC   STTCLOLD,FRMCLT     SET WITH FROM CLIENT                         
         MVC   STTCPOLD,=X'FFFF'      FORCE HIGH                                
*                                                                               
*        PRINT FILE RECORD                                                      
*                                                                               
         LA    R4,P                ESTABLISH PRINT LINE                         
         USING PLINED,R4                                                        
*                                                                               
         GOTO1 VHEXOUT,DMCB,STTAMOLD,POLDAM,1,0,0 AGENCY/MEDIA - OLD            
*                                                                               
         GOTO1 VHEXOUT,DMCB,STTMSOLD,POLDMSTA,5,0,0  MKT/STA - OLD              
*                                                                               
         MVC   POLDMK,FRMMKT       NEW MARKET NUMBER                            
*                                                                               
         MVC   POLDSTA,FRMCALL     NEW STATION                                  
*                                                                               
         MVC   PNEWMK(15),=C'UNKNOWN STATION'                                   
*                                                                               
         B     DMXINTF2            ADD TO TABLE                                 
*                                                                               
DMXINTFD DS    0H                                                               
*                                                                               
*        FORMAT TABLE RECORD                                                    
*                                                                               
*        SET AGENCY/MEDIA                                                       
*                                                                               
         MVC   STTAMOLD,FRMAGYMD   SET WITH FROM AGY/MED                        
         MVC   STTMSOLD,FRMMKSTA   SET WITH FROM MKT/STA                        
         MVC   STTMKOLD,FRMMKT     SET WITH FROM MARKET                         
         MVC   STTSTOLD,FRMCALL    SET WITH FROM STATION                        
         MVC   STTCLOLD,FRMCLT     SET WITH FROM CLIENT                         
*                                                                               
         MVC   AGYOLD,FRMAGY       SAVE FROM AGENCY                             
*                                                                               
         CLC   FRMCLT,=C'000'      IF MASTER STATION REC                        
         BNE   *+14                                                             
         MVC   STTCPOLD,=X'FFFF'      FORCE HIGH                                
         B     DMXINTF1                                                         
*                                                                               
         GOTO1 VCLPACK,DMCB,FRMCLT,STTCPOLD   PACK CLIENT                       
*                                                                               
DMXINTF1 DS    0H                                                               
*                                                                               
         CLI   AGYMDOLD,0          IF OLD AGENCY/MEDIA NOT KNOWN YET            
         BNE   *+10                                                             
         MVC   AGYMDOLD,FRMAGYMD      SET FROM FROM RECORD                      
*                                                                               
         MVC   STTAMNEW,TOAGYMD    SET WITH TO   AGY/MED                        
         MVC   STTMSNEW,TOMKSTA    SET WITH TO   MKT/STA                        
         MVC   STTMKNEW,TOMKT      SET WITH TO   MARKET                         
         MVC   STTSTNEW,TOCALL     SET WITH TO   STATION                        
*                                                                               
         MVC   AGYNEW,TOAGY        SAVE TO   AGENCY                             
*                                                                               
         CLI   AGYMDNEW,0          IF NEW AGENCY/MEDIA NOT KNOWN YET            
         BNE   *+10                                                             
         MVC   AGYMDNEW,TOAGYMD       SET FROM TO   RECORD                      
*                                                                               
*        PRINT FILE RECORD                                                      
*                                                                               
         DROP  R4                                                               
*                                                                               
         LA    R3,P                ESTABLISH PRINT LINE                         
         USING PLINED,R3                                                        
*                                                                               
         GOTO1 VHEXOUT,DMCB,STTAMOLD,POLDAM,1,0,0 AGENCY/MEDIA - OLD            
*                                                                               
         GOTO1 VHEXOUT,DMCB,STTAMNEW,PNEWAM,1,0,0 AGENCY/MEDIA - NEW            
*                                                                               
         GOTO1 VHEXOUT,DMCB,STTMSOLD,POLDMSTA,5,0,0  MKT/STA - OLD              
*                                                                               
         MVC   POLDMK,FRMMKT       OLD MARKET NUMBER                            
*                                                                               
         MVC   POLDSTA,FRMCALL     OLD STATION                                  
         MVC   POLDCLT,FRMCLT      OLD CLIENT                                   
*                                                                               
         MVC   POLDAGY,AGYOLD                                                   
*                                                                               
         GOTO1 VHEXOUT,DMCB,STTMSNEW,PNEWMSTA,5,0,0  MKT/STA - NEW              
*                                                                               
         MVC   PNEWMK,TOMKT        NEW MARKET NUMBER                            
*                                                                               
         MVC   PNEWSTA,TOCALL      NEW STATION                                  
*                                                                               
         MVC   PNEWAGY,AGYNEW                                                   
*                                                                               
DMXINTF2 DS    0H                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',STANTRY) ADD REC TO TABLE          
*                                                                               
         OC    BSPAREC,BSPAREC     DIE IF TABLE FILLED                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   RF,15,BSPAREC                                                    
*                                                                               
         MVC   P+90(42),0(RF)      DISPLAY TABLE ENTRY                          
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
DMXINCN  DS    0H                                                               
*                                                                               
         B     DMXINLP                                                          
*                                                                               
DMXINDN  DS    0H                                                               
*                                                                               
         CLOSE FSTAFROM            CLOSE INPUT FILE                             
         CLOSE FSTATO              CLOSE INPUT FILE                             
*                                                                               
DMXISTAX DS    0H                                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
*        FILL IN AGENCY IN MOVE TABLE                                           
*                                                                               
         L     R4,=A(MVTAB)        POINT TO SELECTION TABLE                     
         USING MVTABD,R4           ESTABLISH TABLE                              
*                                                                               
         MVC   AGYAGOLD,AGYMDOLD    OLD AGENCY/MEDIA                            
         NI    AGYAGOLD,X'F0'       KILL MEDIA                                  
*                                                                               
         MVC   AGYAGNEW,AGYMDNEW    NEW AGENCY/MEDIA                            
         NI    AGYAGNEW,X'F0'       KILL MEDIA                                  
*                                                                               
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         LA    R5,MID3             ESTABLISH PRINT LINE                         
         USING PTBLINED,R5                                                      
         MVC   MID3,SPACES                                                      
*                                                                               
*        SET HEADLINES                                                          
*                                                                               
         MVC   PTBAMOLD,=C'AM'                                                  
         MVC   PTBCLT,=C'CLT'                                                   
         MVC   PTBCLTPK,=C'CLPK'                                                
         MVC   PTBPRD,=C'PRD'                                                   
         MVC   PTBESTST,=C'EST'                                                 
         MVC   PTBESTEN,=C'EST'                                                 
         MVC   PTBAMNEW,=C'AM'                                                  
         MVC   PTBCLTNW,=C'CLT'                                                 
         MVC   PTBCLPKN,=C'CLPK'                                                
         MVC   PTBPRDNW,=C'PRD'                                                 
         MVC   PTBESTNW,=C'EST'                                                 
         MVC   PTBMKT,=C'PRD '                                                  
*                                                                               
         LA    R5,P                ESTABLISH PRINT LINE                         
         USING PTBLINED,R5                                                      
*                                                                               
TABFLLLP DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    TABFLLDN                                                         
*                                                                               
         OC    MVTAMOLD,AGYAGOLD   FILL IN OLD AGENCY                           
         OC    MVTAMNEW,AGYAGNEW   FILL IN NEW AGENCY                           
*                                                                               
         GOTO1 VHEXOUT,DMCB,MVTAMOLD,PTBAMOLD,1,0,0 AGENCY/MEDIA - OLD          
*                                                                               
         GOTO1 VHEXOUT,DMCB,MVTAMNEW,PTBAMNEW,1,0,0 AGENCY/MEDIA - NEW          
*                                                                               
         MVC   PTBCLT,MVTCLT        CLIENT                                      
*                                                                               
         GOTO1 VHEXOUT,DMCB,MVTCLTPK,PTBCLTPK,2,0,0  PACKED CLIENT              
*                                                                               
         MVC   PTBCLTNW,MVTCLTNW    NEW CLIENT                                  
*                                                                               
         GOTO1 VHEXOUT,DMCB,MVTCLPKN,PTBCLPKN,2,0,0  PACKED CLIENT              
*                                                                               
         MVC   PTBPRD,MVTPRD       PRODUCT                                      
         OC    PTBPRD,=C'   '      MAKE PRINTABLE                               
*                                                                               
         MVC   PTBPRDNW,PTBPRD     PRODUCT NEVER CHANGED                        
*                                                                               
         EDIT  (B1,MVTESTST),(3,PTBESTST)   START ESTIMATE                      
*                                                                               
         CLC   MVTESTST,MVTESTEN   SKIP IF ONLY ONE ESTIMATE                    
         BE    TABFLL05                                                         
*                                                                               
         EDIT  (B1,MVTESTEN),(3,PTBESTEN)   END   ESTIMATE                      
*                                                                               
TABFLL05 DS    0H                                                               
*                                                                               
         CLI   MVTESTNW,0          IF NEW ESTIMATE AVAILABLE                    
         BE    TABFLL10                                                         
*                                                                               
         EDIT  (B1,MVTESTNW),(3,PTBESTNW)     PRINT IT                          
*                                                                               
TABFLL10 DS    0H                                                               
*                                                                               
         OC    MVTMKT,MVTMKT       IF MARKET FILTER PRESENT                     
         BZ    TABFLL20                                                         
*                                                                               
         EDIT  (B2,MVTMKT),(4,PTBMKT)   PRINT IT                                
*                                                                               
TABFLL20 DS    0H                                                               
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
TABFLLCN DS    0H                                                               
*                                                                               
         LA    R4,MVTENTL(R4)      BUMP TO NEXT TABLE ENTRY                     
         B     TABFLLLP                                                         
*                                                                               
TABFLLDN DS    0H                                                               
*                                                                               
         TITLE 'SPLDEXTCZ - MOVE BETWEEN AGENCIES - DMXIBYR'                    
***********************************************************************         
*                                                                     *         
*        PROGRAM INITIALIZATION - BUYER TABLE                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXIBYR  DS    0H                                                               
*                                                                               
         CLI   MVNWS,C'Y'          MOVE BUYSHEET IF ASKED                       
         BNE   DMXIBYRX                                                         
*                                                                               
         MVI   EOFTOSW,0           INIT EOF SWITCH                              
*                                                                               
         XC    FROMRECC,FROMRECC   INIT FILE READIN AREAS                       
         XC    TORECC,TORECC                                                    
*                                                                               
         OPEN  (FBYRFROM,(INPUT))    OPEN FROM AGENCY BUYER FILE                
*                                                                               
         LTR   RF,RF               MUST HAVE A VALID READ                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (FBYRTO,(INPUT))      OPEN TO   AGENCY BUYER FILE                
*                                                                               
         LTR   RF,RF               MUST HAVE A VALID READ                       
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        ISSUE GETMAIN FOR STORAGE TO HNEW CONVERSION TABLE                     
*                                                                               
         LHI   R0,BYRNTRYL         RECORD LENGTH                                
         MHI   R0,BYTRMAXQ         *MAXIMUM NUMBER OF RECORDS                   
*                                                                               
         GETMAIN RU,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STCM  R1,15,BYRTABA       SAVE A(GETMAIN AREA)                         
         LR    R3,R1               SAVE A(GETMAIN AREA)                         
         SR    R5,R5                                                            
*                                                                               
*        INITIALIZE BINSRCH PARAMETERS                                          
*                                                                               
         LA    R2,BSPBYR           POINT TO BUYER BINSRCH PARMS                 
         USING BSRPRMD,R2          ESTABLISH BINSRCH PARAMETERES                
*                                                                               
         MVC   BSPATAB,BYRTABA     A(TABLE)                                     
*                                                                               
         LA    RF,BYRNTRYL         SET ENTRY LENGTH                             
         ST    RF,BSPLENR                                                       
*                                                                               
         XC    BSPNOR,BSPNOR       INIT RECORD COUNTER                          
*                                                                               
         LA    RF,BYTKEYLQ         SET KEY LENGTH                               
         ST    RF,BSPLENK                                                       
         MVI   BSPKEYD,BYTKEY-BYRNTRY KEY DISPLACEMENT                          
*                                                                               
         LHI   RF,BYTRMAXQ         SET # OF AVAILABLE ENTRIES                   
         ST    RF,BSPMAX                                                        
*                                                                               
*        FILL CONVERSION TABLE FROM INPUT FILE                                  
*                                                                               
         LA    R5,FROMRECC         ESTABLISH FROM AGENCY BUYER REC              
         USING BYRFRMD,R5                                                       
*                                                                               
         LA    R6,TORECC           ESTABLISH TO   AGENCY BUYER REC              
         USING BYRTOD,R6                                                        
*                                                                               
         LA    R7,BYRNTRYC         ESTABLISH TABLE ENTRY                        
         USING BYRNTRYD,R7                                                      
*                                                                               
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         MVC   MID1+2(20),=CL20'CONVERSION FILE'  LABEL PRINT OUT               
*                                                                               
DMXIBYLP DS    0H                                                               
*                                                                               
         L     (R1),=A(FBYRFROM)                                                
         GET   (1),FROMRECC       READ NEXT RECORD                              
*                                                                               
*        COMPARE TO CURRENT TO RECORD                                           
*                                                                               
DMXIBY1L DS    0H                                                               
*                                                                               
         CLC   BFRKTYP,BTOKTYP     RECORD TYPES MUST MATCH                      
         BH    DMXIBY1C                                                         
         BL    DMXIBY1D                                                         
*                                                                               
         CLC   BFRKSUB,BTOKSUB     RECORD SUB-TYPES MUST MATCH                  
         BH    DMXIBY1C                                                         
         BL    DMXIBY1D                                                         
*                                                                               
         MVC   HALF(1),BFRKAM      DROP AGENCY CODE                             
         NI    HALF,X'0F'            TO GET MEDIA                               
         MVC   HALF+1(1),BTOKAM                                                 
         NI    HALF+1,X'0F'                                                     
*                                                                               
         CLC   HALF(1),HALF+1      MEDIA MUST MATCH                             
         BH    DMXIBY1C                                                         
         BL    DMXIBY1D                                                         
*                                                                               
         CLC   BFRKBYR,BTOKBYR     BUYERS MUST MATCH                            
         BH    DMXIBY1C                                                         
         BL    DMXIBY1D                                                         
*                                                                               
         B     DMXIBY1F            MATCH FOUND                                  
*                                                                               
DMXIBY1C DS    0H                                                               
*                                                                               
         CLI   EOFTOSW,0           SKIP IF END OF FILE WAS REACHED              
         BNE   DMXIBY1D                                                         
*                                                                               
         L     R1,=A(FBYRTO)                                                    
         GET   (1),TORECC          READ NEXT TO RECORD                          
*                                                                               
         B     DMXIBY1L                                                         
*                                                                               
DMXIBYC1 DS    0H                                                               
*                                                                               
         MVI   EOFTOSW,X'FF'       INDICATE TO FILE EXHAUSTED                   
*                                                                               
DMXIBY1D DS    0H                  NO MATCH                                     
*                                                                               
*        BUYER NOT ON RECEIVING AGENCY FILE                                     
*        DROP BUYER FROM TABLE                                                  
*                                                                               
*        SET AGENCY/MEDIA                                                       
*                                                                               
         MVC   BYTAMNEW,BFRKAM     SET WITH FROM AGY/MED                        
         MVC   BYTBCNEW,BFRBYRCD   SET WITH FROM BUYER CODE                     
         MVC   BYTBYNEW,BFRKBYR    SET WITH FROM BUYER ID                       
         MVC   BYTBNNEW,BFRBYRNM   SET WITH FROM BUYER NAME                     
*                                                                               
*        PRINT FILE RECORD                                                      
*                                                                               
         LA    R4,P                ESTABLISH PRINT LINE                         
         USING PBLINED,R4                                                       
*                                                                               
         GOTO1 VHEXOUT,DMCB,BYTAMNEW,PBAMNEW,1,0,0 AGENCY/MEDIA - NEW           
*                                                                               
         EDIT  (B1,BYTBCNEW),(3,PBNEWBC)   NEW BUYER CODE                       
*                                                                               
         MVC   PBNEWBYR,BYTBYNEW   NEW BUYER                                    
         MVC   PBNEWBN,BYTBNNEW    NEW BUYER NAME                               
*                                                                               
         MVC   PBNEWBN,=CL20'UNKNOWN BUYER'                                     
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIBYCN                                                         
*                                                                               
DMXIBY1F DS    0H                                                               
*                                                                               
*        FORMAT TABLE RECORD                                                    
*                                                                               
*        SET AGENCY/MEDIA                                                       
*                                                                               
         MVC   BYTAMNEW,BFRKAM     SET WITH FROM AGY/MED                        
         MVC   BYTBCNEW,BFRBYRCD   SET WITH FROM BUYER CODE                     
         MVC   BYTBYNEW,BFRKBYR    SET WITH FROM BUYER ID                       
         MVC   BYTBNNEW,BFRBYRNM   SET WITH FROM BUYER NAME                     
*                                                                               
         MVC   BYTAMNEW,BTOKAM     SET WITH TO   AGY/MED                        
         MVC   BYTBCNEW,BTOBYRCD   SET WITH TO   BUYER CODE                     
         MVC   BYTBYNEW,BTOKBYR    SET WITH TO   BUYER ID                       
         MVC   BYTBNNEW,BTOBYRNM   SET WITH TO   BUYER NAME                     
*                                                                               
*        PRINT FILE RECORD                                                      
*                                                                               
         LA    R4,P                ESTABLISH PRINT LINE                         
         USING PBLINED,R4                                                       
*                                                                               
         GOTO1 VHEXOUT,DMCB,BYTAMNEW,PBAMNEW,1,0,0 AGENCY/MEDIA - NEW           
*                                                                               
         EDIT  (B1,BYTBCNEW),(3,PBNEWBC)   NEW BUYER CODE                       
*                                                                               
         MVC   PBNEWBYR,BYTBYNEW   NEW BUYER                                    
         MVC   PBNEWBN,BYTBNNEW    NEW BUYER NAME                               
*                                                                               
         MVC   PBNEWBN,=CL20'UNKNOWN BUYER'                                     
*                                                                               
         GOTO1 VHEXOUT,DMCB,BYTAMNEW,PBAMNEW,1,0,0 AGENCY/MEDIA - NEW           
*                                                                               
         EDIT  (B1,BYTBCNEW),(3,PBNEWBC)   NEW BUYER CODE                       
*                                                                               
         MVC   PBNEWBYR,BYTBYNEW   NEW BUYER                                    
         MVC   PBNEWBN,BYTBNNEW    NEW BUYER NAME                               
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',BYRNTRY) ADD REC TO TABLE          
*                                                                               
         OC    BSPAREC,BSPAREC     DIE IF TABLE FILLED                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   RF,15,BSPAREC                                                    
*                                                                               
         MVC   P+80(BYRNTRYL),0(RF)      DISPLAY TABLE ENTRY                    
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
DMXIBYCN DS    0H                                                               
*                                                                               
         B     DMXIBYLP                                                         
*                                                                               
DMXIBYDN DS    0H                                                               
*                                                                               
         CLOSE FBYRFROM            CLOSE INPUT FILE                             
         CLOSE FBYRTO              CLOSE INPUT FILE                             
*                                                                               
DMXIBYRX DS    0H                                                               
*                                                                               
*        PRINT COLUMN TITLES                                                    
*                                                                               
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         MVC   MID2,SPACES                                                      
         MVC   MID3,SPACES                                                      
*                                                                               
         MVC   MID2+2(8),=CL8'OLD KEY'                                          
         MVC   MID2+32(8),=CL8'NEW KEY'                                         
         MVC   MID2+65(3),=C'OLD'                                               
         MVC   MID2+80(3),=C'NEW'                                               
*                                                                               
         OPEN  (FILEOUT,(OUTPUT))    OPEN OUTPUT FILE                           
*                                                                               
         B     DMXIT                                                            
*                                                                               
         DROP  R2                                                               
*                                                                               
         TITLE 'SPLDEXTCZ - MOVE BETWEEN AGENCIES - DMXREC'                     
***********************************************************************         
*                                                                     *         
*        PROCESS NEXT RECORD TO BE ADDED TO FILE                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DMXREC   DS    0H                                                               
*                                                                               
         SR    R6,R6               INIT ELEMENT POINTER                         
*                                                                               
         L     R3,AREC             POINT TO RECORD FOR PROCESSING               
*                                                                               
*        DETERMINE RECORD TYPE                                                  
*                                                                               
         CLI   0(R3),X'00'         TEST HEADER                                  
         BNE   *+12                                                             
         BRAS  RE,HDR                                                           
         B     DMXKEEP                                                          
*                                                                               
         CLI   0(R3),X'10'         TEST BUYREC                                  
         BNH   *+12                                                             
         BRAS  RE,BUY                                                           
         B     DMXKEEP                                                          
*                                                                               
         CLI   0(R3),X'02'         TEST GOALREC                                 
         BNE   *+12                                                             
         BRAS  RE,GOAL                                                          
         B     DMXKEEP                                                          
*                                                                               
DMXREC1  DS    0H                                                               
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
RECCTR   DC    PL2'100'            RECORD COUNTER                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* PRINT A LITTLE TRACE OF DELETED ELEMENTS                                      
***********************************************************************         
         SPACE 1                                                                
PRTDEL   NTR1  BASE=*,LABEL=*                                                   
         CP    PRTCOUNT,=P'50'                                                  
         BH    PRTDEL2                                                          
         AP    PRTCOUNT,=P'1'                                                   
         MVC   P(1),PRTFLAG                                                     
         MVC   P+1(3),=C'DEL'                                                   
         GOTO1 VHEXOUT,DMCB,(R3),P+10,13,0,0  PRINT KEY                         
         GOTO1 VHEXOUT,DMCB,(R6),P+40,16,0,0  PRINT REGULAR ELEMENT             
         GOTO1 VPRINTER                                                         
         MVI   PRTFLAG,C' '                                                     
PRTDEL2  GOTO1 =V(RECUP),DMCB,(R3),(R6)                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,13(R3)         GET NEW RECORD LENGTH                        
         LA    R1,0(RF,R3)         POINT TO END OF RECORD                       
         MVI   0(R1),0             FORCE NULL AFTER RECORD                      
*                                                                               
         CLI   0(R6),X'0D'                                                      
         BL    PRTDELX                                                          
         CLI   0(R6),X'20'                                                      
         BL    PRTDEL2                                                          
*                                                                               
PRTDELX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'STLDEXTCZ - MOVE BETWEEN AGENCIES - GETSTA'                     
***********************************************************************         
*                                                                     *         
*        FIND STATION IN CONVERSION TABLE                             *         
*                                                                     *         
*NTRY    R2==> BINARY MARKET/STATION CODE                             *         
*        R4==> ENTRY IN TABLE OF THINGS TO MOVE                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETSTA   NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         USING MVTABD,R4           ESTABLISH TABLE                              
*                                                                               
         LA    R7,STAEXCPS         POINT TO STATION EXCEPTIONS TABLE            
         USING STANTRYD,R7         ESTABLISH AS ENTRY IN STATION TABLE          
*                                                                               
GSTEXCLP DS    0H                                                               
*                                                                               
         CLI   STTAMOLD,X'00'      DONE AT END OF TABLE                         
         BE    GSTEXCDN                                                         
*                                                                               
         CLC   MVTAMOLD,STTAMOLD   MATCH ON AGENCY/MEDIA                        
         BNE   GSTEXCCN                                                         
*                                                                               
         CLC   STTMSOLD,0(R2)      MATCH ON INCOMING MKT/STATION                
         BE    GSTEXCFD                                                         
*                                                                               
GSTEXCCN DS    0H                                                               
*                                                                               
         LA    R7,STANTRYL(R7)     BUMP TO NEXT TABLE ENTRY                     
         B     GSTEXCLP                                                         
*                                                                               
GSTEXCFD DS    0H                                                               
*                                                                               
         L     R5,AREC             POINT TO INCOMING RECORD                     
         GOTO1 VHEXOUT,DMCB,(R5),P+2,13,0,0  PRINT NEW KEY                      
*                                                                               
         ZAP   CTR,=P'10'          FORCE TRACE                                  
*                                                                               
         B     GSTCLTFD                                                         
*                                                                               
GSTEXCDN DS    0H                                                               
*                                                                               
         LA    R3,BSPSTA           POINT TO STATION BINSRCH PARAMETERS          
         USING BSRPRMD,R3          ESTABLISH BINSRCH PARAMETERES                
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BNE   GETSTA1                                                          
*                                                                               
         L     R5,AREC             POINT TO INCOMING RECORD                     
         GOTO1 VHEXOUT,DMCB,(R5),P+2,13,0,0  PRINT NEW KEY                      
*                                                                               
GETSTA1  DS    0H                                                               
*                                                                               
         CLI   2(R2),X'F0'         NO STATION CHANGE FOR CABLE BUYS             
         BNL   GSTCLTF1                                                         
*                                                                               
         LA    R7,STANTRYC         ESTABLISH BINSRCH TABLE WORKAREA             
         USING STANTRYD,R7                                                      
*                                                                               
         XC    STANTRY(STANTRYL),STANTRY INIT WORKAREA                          
*                                                                               
         MVC   STTAMOLD,MVTAMOLD   SET OLD AGY/MD  IN KEY                       
         MVC   STTMSOLD,0(R2)      SET OLD MKT/STA IN KEY                       
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPRDHI',STTKEY)   FIND IN TABLE           
*                                                                               
         CLI   BSPCNTL,BSPNF       SKIP IF NOT FOUND                            
         BE    GETSTAER                                                         
*                                                                               
         ICM   R7,15,BSPAREC       POINT TO FOUND RECORD                        
         BZ    GETSTAER            SKIP IF NOT FOUND                            
*                                                                               
GSTCLTLP DS    0H                                                               
*                                                                               
         CLC   STTAMOLD,MVTAMOLD   MUST MATCH AGY/MED                           
         BNE   GETSTAER                                                         
*                                                                               
         CLC   STTMSOLD,0(R2)      MUST MATCH MKT/STA                           
         BNE   GETSTAER                                                         
*                                                                               
         CLC   STTCPOLD,=X'FFFF'   USE IF MASTER STATION                        
         BE    GSTCLTFD                                                         
*                                                                               
         CLC   STTCPOLD,WRKCLT     USE IF CLIENT MATCHES                        
         BE    GSTCLTFD                                                         
*                                                                               
GSTCLTCN DS    0H                                                               
*                                                                               
         LA    R7,STANTRYL(R7)     BUMP TO NEXT TABLE ENTRY                     
         B     GSTCLTLP                                                         
*                                                                               
GSTCLTFD DS    0H                                                               
*                                                                               
         OC    STTMSNEW,STTMSNEW   ERROR IF NO NEW MKT/STA                      
         BZ    GETSTAE1                                                         
*                                                                               
         MVC   0(5,R2),STTMSNEW    SET NEW MARKET/STATION                       
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BL    GETSTAXX                                                         
*                                                                               
         MVC   P+65(4),STTMKOLD       OLD MARKET                                
         MVC   P+70(5),STTSTOLD       OLD STATION                               
         MVC   P+76(3),STTCLOLD       OLD CLIENT                                
*                                                                               
         MVC   P+80(4),STTMKNEW       NEW MARKET                                
         MVC   P+85(5),STTSTNEW       NEW STATION                               
*                                                                               
GSTCLTF1 DS    0H                                                               
*                                                                               
         L     R3,=A(NEWREC)       POINT TO NEW RECORD                          
         GOTO1 VHEXOUT,DMCB,(R3),P+32,13,0,0  PRINT NEW KEY                     
*                                                                               
GETSTAX  DS    0H                                                               
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         B     GETSTAXX                                                         
*                                                                               
GETSTAER DS   0H                   PRINT TRACE IF NO MATCH                      
*                                                                               
         LA    R7,STANTRYC         ESTABLISH BINSRCH TABLE WORKAREA             
         USING STANTRYD,R7                                                      
*                                                                               
GETSTAE1 DS   0H                   HAVE STATION TABLE ENTRY                     
*                                                                               
         L     R5,AREC                                                          
         GOTO1 VHEXOUT,DMCB,(R5),P+2,13,0,0  PRINT OLD KEY                      
*                                                                               
         MVC   P+65(4),STTMKOLD       OLD MARKET                                
         MVC   P+70(5),STTSTOLD       OLD STATION                               
         MVC   P+76(3),STTCLOLD       OLD CLIENT                                
*                                                                               
         L     R3,=A(NEWREC)       POINT TO NEW RECORD                          
******   GOTO1 VHEXOUT,DMCB,(R3),P+32,13,0,0  PRINT NEW KEY                     
*                                                                               
         MVC   P+90(15),=CL15'UNKNOWN STATION'                                  
*                                                                               
GETSTAEX DS    0H                                                               
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         LTR   RB,RB               SET NON-ZERO CC                              
         B     GETSTAXZ                                                         
*                                                                               
GETSTAXX DS    0H                                                               
*                                                                               
         SP    CTR,=P'1'           DECREMENT COUNTER                            
         BP    *+10                                                             
         ZAP   CTR,=P'10'          RESET COUNTER                                
*                                                                               
         CR    RB,RB               SET =CC                                      
         B     GETSTAXZ                                                         
*                                                                               
GETSTAXZ DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
*        TABLE OF MKT/STATION EXCEPTIONS                                        
*        SAME FORMAT AS STATION TABLE                                           
*                                                                               
STAEXCPS DS    0D                  EXCEPTION STATIONS                           
******   DC    XL1'92'             NEW AGENCY/MEDIA                             
******   DC    XL5'00E8C68A61'     NEW MARKET/STATION                           
******   DC    XL2'AC01'           NEW CLIENT - PACKED                          
******   DC    CL4'0232'           NEW MARKET NUMBER                            
******   DC    CL5'WHFSF'          NEW STATION                                  
******   DC    CL3'LAB'            NEW CLIENT                                   
******   DC    XL1'12'             NEW AGENCY/MEDIA                             
******   DC    XL5'00E8C68A61'     NEW MARKET/STATION                           
******   DC    CL4'0232'           NEW MARKET NUMBER                            
******   DC    CL5'WHFSF'          NEW STATION                                  
*                                                                               
         DC    X'00'               EOT                                          
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         GETEL R6,42,ELCODE                                                     
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - GETMKT'                    
***********************************************************************         
*                                                                     *         
*        FIND MARKET  IN CONVERSION TABLE                             *         
*                                                                     *         
*NTRY    R2==> BINARY MARKET CODE                                     *         
*        R4==> ENTRY IN TABLE OF THINGS TO MOVE                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETMKT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,BSPSTA           POINT TO STATION BINSRCH PARAMETERS          
         USING BSRPRMD,R3          ESTABLISH BINSRCH PARAMETERES                
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BNE   GETMKT1                                                          
*                                                                               
         L     R5,AREC             POINT TO INCOMING RECORD                     
         GOTO1 VHEXOUT,DMCB,(R5),P+2,13,0,0  PRINT NEW KEY                      
*                                                                               
GETMKT1  DS    0H                                                               
*                                                                               
         LA    R7,STANTRYC         ESTABLISH BINSRCH TABLE WORKAREA             
         USING STANTRYD,R7                                                      
*                                                                               
         XC    STANTRY(STANTRYL),STANTRY INIT WORKAREA                          
*                                                                               
         MVC   STTAMOLD,MVTAMOLD   SET OLD AGY/MED IN KEY                       
*                                                                               
         TM    STTAMOLD,X'08'      IF MEDIA C                                   
         BNO   *+12                                                             
         NI    STTAMOLD,X'FF'-X'08'   CHANGE TO TV                              
         OI    STTAMOLD,X'01'         CHANGE TO TV                              
*                                                                               
         MVC   STTMSOLD(2),0(R2)   SET OLD MKT     IN KEY                       
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPRDHI',STTKEY)   FIND IN TABLE           
*                                                                               
         CLI   BSPCNTL,BSPNF       SKIP IF NOT FOUND                            
         BE    GETMKTER                                                         
*                                                                               
         ICM   R7,15,BSPAREC       POINT TO FOUND RECORD                        
         BZ    GETMKTER            SKIP IF NOT FOUND                            
*                                                                               
GMKCLTLP DS    0H                                                               
*                                                                               
         OC    STTMSNEW,STTMSNEW   SKIP IF NO NEW MKT/STA                       
         BZ    GMKCLTCN                                                         
*                                                                               
         CLC   STTMSOLD(2),0(R2)   FIND MARKET MATCH                            
         BNE   GETMKTER                                                         
*                                                                               
         CLC   STTCPOLD,=X'FFFF'   USE IF MASTER STATION                        
         BE    GMKCLTFD                                                         
*                                                                               
         CLC   STTCPOLD,WRKCLT     USE IF CLIENT MATCHES                        
         BE    GMKCLTFD                                                         
*                                                                               
GMKCLTCN DS    0H                                                               
*                                                                               
         LA    R7,STANTRYL(R7)     BUMP TO NEXT TABLE ENTRY                     
         B     GMKCLTLP                                                         
*                                                                               
GMKCLTFD DS    0H                                                               
*                                                                               
         MVC   0(2,R2),STTMSNEW    SET NEW MARKET                               
*                                                                               
         CP    CTR,=P'10'          PRINT WHEN COUNTER IS 10                     
         BL    GETMKTXX                                                         
*                                                                               
         MVC   P+65(4),STTMKOLD       OLD MARKET                                
*                                                                               
         L     R3,=A(NEWREC)       POINT TO NEW RECORD                          
         GOTO1 VHEXOUT,DMCB,(R3),P+32,13,0,0  PRINT NEW KEY                     
*                                                                               
         MVC   P+80(4),STTMKNEW       NEW MARKET                                
*                                                                               
GETMKTX  DS    0H                                                               
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         B     GETMKTXX                                                         
*                                                                               
GETMKTER DS   0H                   PRINT TRACE IF NO MATCH                      
*                                                                               
         L     R5,AREC                                                          
         GOTO1 VHEXOUT,DMCB,(R5),P+2,13,0,0  PRINT NEW KEY                      
*                                                                               
         EDIT  (B2,0(R2)),(4,P+65)   NEW MARKET                                 
*                                                                               
         L     R3,=A(NEWREC)       POINT TO NEW RECORD                          
*******  GOTO1 VHEXOUT,DMCB,(R5),P+32,13,0,0  PRINT NEW KEY                     
*                                                                               
         MVC   P+80(15),=CL15'UNKNOWN MARKET'                                   
*                                                                               
GETMKTEX DS    0H                                                               
*                                                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         LTR   RB,RB               SET NE CC                                    
         B     GETMKTXZ                                                         
*                                                                               
GETMKTXX DS    0H                                                               
*                                                                               
         SP    CTR,=P'1'           DECREMENT COUNTER                            
         BP    *+10                                                             
         ZAP   CTR,=P'10'          RESET COUNTER                                
*                                                                               
         CR    RB,RB               SET EQ CC                                    
*                                                                               
GETMKTXZ DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'STLDEXTCZ  - MOVE BETWEEN AGENCIES - COPY'                      
***********************************************************************         
*                                                                     *         
*        COPY CURRENT RECORD TO WORKAREA                              *         
*                                                                     *         
*NTRY    R3==> RECORD TO BE COPIED                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
COPY     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R0,R3               POINT TO INCOMING RECORD                     
         L     RE,=A(NEWREC)       POINT TO NEW RECORD                          
         SR    R1,R1                                                            
         ICM   R1,3,13(R3)         GET RECORD LENGTH                            
         LR    RF,R1               COPY LENGTH                                  
*                                                                               
         AHI   RF,4                ALLOW FOR LENGTH BYTES                       
         AHI   RE,-4               BACK UP TO RRLEN FIELD                       
         STCM  RF,3,0(RE)          SET OUTPUT FILE LENGTH                       
         AHI   RE,4                REPOINT TO RECORD                            
         AHI   RF,-4               RESTORE TRUE RECORD LENGTH                   
*                                                                               
         MVCL  RE,R0               COPY INCOMING RECORD                         
*                                                                               
         L     R3,=A(NEWREC)                                                    
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,13(R3)         GET RECORD LENGTH                            
*                                                                               
         LA    RF,0(R1,R3)         POINT TO END OF NEW RECORD                   
         MVI   0(RF),0             FORCE ENDING NULLS                           
*                                                                               
         CLC   AGYOLD,20(R3)     IF ALPHA AGENCY PRESENT                        
         BNE   *+10                                                             
         MVC   20(2,R3),AGYNEW                                                  
*                                                                               
COPYX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - WRITE'                     
***********************************************************************         
*                                                                     *         
*        WRITE WORKAREA RECORD TO OUTPUT DATASET                      *         
*                                                                     *         
*NTRY    NEWREC HAS RECORD TO GO                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WRITE    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R1,=A(FILEOUT)                                                   
         L     R2,=A(RRLEN)        POINT TO RECORD                              
*                                                                               
         CLC   SAVEKEY,4(R2)       SKIP IF DUPLICATE RECORD                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         BE    WRITEX                                                           
*                                                                               
         MVC   SAVEKEY,4(R2)       SAVE CURRENT KEY                             
*                                                                               
         PUT   (1),(R2)            WRITE TO OUTPUT FILE                         
*                                                                               
         AP    RECCNTR,=P'1'       COUNT NEW RECORDS                            
*                                                                               
WRITEX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'SPLDEXTCZ - MOVE BETWEEN AGENCIES - HDR'                        
***********************************************************************         
*                                                                     *         
*        HEADER RECORDS - CLIENT, PRODUCT, ESTIMATE - X'00'           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HDR      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING ESTHDRD,R3          ESTABLISH SPOT GENERIC HEADER RECORD         
*                                                                               
         OC    EKEYEST+1(5),EKEYEST+1 SKIP IF NOT ESTIMATE(IE.BILLREC)          
         BNZ   HDRKEYDN                                                         
*                                                                               
         L     R4,=A(MVTAB)        POINT TO SELECTION TABLE                     
         USING MVTABD,R4                                                        
*                                                                               
HDRKEYLP DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    HDRKEYDN                                                         
*                                                                               
         CLC   EKEYAM,MVTAMOLD     MATCH ON AGENCY/MEDIA                        
         BNE   HDRKEYCN                                                         
*                                                                               
         CLC   EKEYCLT,MVTCLTPK    MATCH ON PACKED CLIENT                       
         BNE   HDRKEYCN                                                         
*                                                                               
         OC    EKEYPRD(9),EKEYPRD  IF CLIENT HEADER                             
         BNZ   HDRCLTN                                                          
*                                                                               
         USING CLTHDRD,R3             ESTABLISH CLIENT HEADER                   
*                                                                               
         LA    R0,4                SAVE CLIST                                   
         LA    R1,SAVCLIST                                                      
         LA    RE,CLIST                                                         
*                                                                               
         MVC   0(220,R1),0(RE)                                                  
         AHI   R1,220                                                           
         AHI   RE,220                                                           
         BCT   R0,*-14                                                          
*                                                                               
         CLI   MVCLT,C'Y'             DROP IF NOT MOVING CLIENT HEADER          
         BE    HDRKEYDN                                                         
*                                                                               
         B     HDRKEYFD               ELSE KEEP CLIENT HEADER                   
*                                                                               
HDRCLTN  DS    0H                                                               
*                                                                               
         USING ESTHDRD,R3          ESTABLISH SPOT GENERIC HEADER RECORD         
*                                                                               
         OC    MVTPRD,MVTPRD       SKIP IF ALL PRODUCTS WANTED                  
         BZ    HDRPRD20                                                         
*                                                                               
         CLC   EKEYPRD,=C'POL'     IF NOT PRODUCT POL                           
         BE    HDRPRD20                                                         
*                                                                               
         CLC   EKEYPRD,MVTPRD         MATCH ON PRODUCT                          
         BNE   HDRKEYCN                                                         
*                                                                               
HDRPRD20 DS    0H                                                               
*                                                                               
         OC    EKEYEST(6),EKEYEST  IF PRODUCT HEADER                            
         BNZ   HDRPRDN                                                          
*                                                                               
         CLI   MVPRD,C'Y'             DROP IF NOT MOVING PRODUCT HEADER         
         BE    HDRKEYDN                                                         
*                                                                               
         B     HDRKEYFD               ELSE MOVE PRODUCT HEADER                  
*                                                                               
HDRPRDN  DS    0H                                                               
*                                                                               
         CLC   EKEYPRD,=C'POL'     SKIP IF PRODUCT POL                          
         BE    HDRPRDDN                                                         
*                                                                               
         LA    RF,SAVCLIST         FIND PRODUCT CODE IN CLIST                   
         LA    R0,L'CLIST/4        MAX ENTRIES IN LIST                          
*                                                                               
HDRPRDLP DS    0H                                                               
*                                                                               
         CLI   3(RF),0             CHECK FOR END OF LIST                        
         BE    HDRPRDDN            OKAY IF NOT IN LIST                          
*                                                                               
         CLC   EKEYPRD,0(RF)       MATCH ON PRODUCT ALPHA                       
         BE    HDRPRDFD                                                         
*                                                                               
HDRPRDCN DS    0H                                                               
*                                                                               
         LA    RF,4(RF)            NEXT PRODUCT                                 
         BCT   R0,HDRPRDLP                                                      
         B     HDRPRDDN            OKAY IF NOT IN PRODUCT LIST                  
*                                                                               
HDRPRDFD DS    0H                                                               
*                                                                               
         MVC   MVTPRDCD,3(RF)      SAVE PRODUCT CODE                            
*                                                                               
HDRPRDDN DS    0H                                                               
*                                                                               
         CLC   EKEYEST,MVTESTST    MATCH ON ESTIMATE RANGE                      
         BL    HDRKEYCN                                                         
         CLC   EKEYEST,MVTESTEN                                                 
         BH    HDRKEYCN                                                         
*                                                                               
         CLI   MVEST,C'Y'          DROP IF NOT MOVING ESTIMATE HEADER           
         BE    HDRKEYDN                                                         
         B     HDRKEYFD                                                         
*                                                                               
HDRKEYCN DS    0H                                                               
*                                                                               
         LA    R4,MVTENTL(R4)      POINT TO NEXT ENTRY IN TABLE                 
         B     HDRKEYLP                                                         
*                                                                               
HDRKEYFD DS    0H                                                               
*                                                                               
         BRAS  RE,COPY             COPY RECORD TO WORKAREA                      
*                                                                               
         L     R3,=A(NEWREC)       POINT TO NEW RECORD                          
         USING ESTHDRD,R3          ESTABLISH SPOT GENERIC HEADER RECORD         
*                                                                               
         NI    EKEYAM,X'0F'        KILL   OLD AGENCY NYBBLE                     
         OC    EKEYAM,AGYAGNEW     ADD IN NEW AGENCY NYBBLE                     
*                                                                               
         MVC   EKEYCLT,MVTCLPKN    SET NEW CLIENT                               
*                                                                               
         OC    EKEYEST,EKEYEST     IF ESTIMATE NUMBER EXISTS                    
         BZ    HDRKEYF1                                                         
         OC    MVTESTNW,MVTESTNW   AND NEW ESTIMATE NUMBER GIVEN                
         BZ    HDRKEYF1                                                         
*                                                                               
         MVC   EKEYEST,MVTESTNW       REPLACE ESTIMATE NUMBER                   
*                                                                               
         XC    EORDN,EORDN         CLEAR BUCKETS                                
         XC    EPAIDN,EPAIDN                                                    
*                                                                               
HDRKEYF1 DS    0H                                                               
*                                                                               
         BRAS  RE,WRITE            WRITE RECORD TO OUTPUT DATASET               
*                                                                               
         L     R5,AREC                                                          
         GOTO1 VHEXOUT,DMCB,(R5),P+2,13,0,0  PRINT NEW KEY                      
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+32,13,0,0  PRINT NEW KEY                     
*                                                                               
         MVC   P+80(MVTENTL),0(R4)     PRINT TABLE ENTRY                        
*                                                                               
         GOTO1 VPRINTER            PRINT KEYS                                   
*                                                                               
HDRKEYDN DS    0H                                                               
*                                                                               
HDRX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'SPLDEXTCZ - MOVE BETWEEN AGENCIES - BUY'                        
***********************************************************************         
*                                                                     *         
*        BUY RECORD - >X'10'                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BUY      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   MVBUYS,C'Y'         SKIP IF BUYS NOT WANTED                      
         BNE   BUYX                                                             
*                                                                               
         NOP   *+20                                                             
         OI    *-3,X'F0'                                                        
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
         ZAP   CTR,=P'1'           FORCE RECORD PRINTING                        
*                                                                               
         USING BUYRECD,R3          ESTABLISH SPOT BUY RECORD                    
*                                                                               
         TM    BUYKAM,X'03'        ACCEPT ONLY RADIO OR TV                      
         BNM   BUYX                                                             
*                                                                               
         TM    BUYKAM,X'08'        ACCEPT ONLY RADIO OR TV                      
         BO    BUYX                                                             
*                                                                               
         CLI   BUYMSTA+2,X'F0'     IF CABLE BUY                                 
         BL    BUYCBLN                                                          
*                                                                               
         CLI   MVCABLE,C'Y'           DROP IF NOT MOVING CABLE BUYS             
         BNE   BUYX                                                             
*                                                                               
BUYCBLN  DS    0H                                                               
*                                                                               
         L     R4,=A(MVTAB)        POINT TO SELECTION TABLE                     
         USING MVTABD,R4                                                        
*                                                                               
BUYKEYLP DS    0H                                                               
*                                                                               
         NOP   BUYLOOP1                                                         
         OI    *-3,X'F0'                                                        
         MVC   P(9),=C'GOT A BUY '                                              
         GOTO1 VPRINTER                                                         
         ZAP   PRTCOUNT,=P'0'      RESET PRINTING COUNTER                       
*                                                                               
BUYLOOP1 DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    BUYKEYDN                                                         
*                                                                               
         CLC   BUYKAM,MVTAMOLD     MATCH ON AGENCY/MEDIA                        
         BNE   BUYKEYCN                                                         
*                                                                               
         CLC   BUYKCLT,MVTCLTPK    MATCH ON PACKED CLIENT                       
         BNE   BUYKEYCN                                                         
*                                                                               
         CLC   BUYKEST,MVTESTST    MATCH ON ESTIMATE RANGE                      
         BL    BUYKEYCN                                                         
         CLC   BUYKEST,MVTESTEN    MATCH ON ESTIMATE                            
         BH    BUYKEYCN                                                         
*                                                                               
         CLI   MVTPRDCD,0          OKAY IF ALL PRDS WANTED                      
         BE    BUYKEY20                                                         
*                                                                               
         CLC   BUYKPRD,MVTPRDCD                                                 
         BE    BUYKEYFD            OKAY IF PRODUCT MATCHES                      
*                                                                               
         CLI   BUYKPRD,X'FF'       IF POL BUY                                   
         BNE   BUYKEYCN                                                         
*                                                                               
         CLC   MVTPRDCD,BDMASPRD      MATCH ON MASTER PRODUCT CODE              
         BNE   BUYKEYCN                                                         
*                                                                               
BUYKEY20 DS    0H                                                               
*                                                                               
         B     BUYKEYFD                                                         
*                                                                               
BUYKEYCN DS    0H                                                               
*                                                                               
         LA    R4,MVTENTL(R4)      POINT TO NEXT ENTRY IN TABLE                 
         B     BUYKEYLP                                                         
*                                                                               
BUYKEYFD DS    0H                                                               
*                                                                               
         NOP   BUYLOOP2                                                         
         OI    *-3,X'F0'                                                        
         MVC   P(9),=C'COPY BUY '                                               
         GOTO1 VPRINTER                                                         
                                                                                
BUYLOOP2 DS    0H                                                               
         BRAS  RE,COPY             COPY RECORD TO WORKAREA                      
*                                                                               
         L     R3,=A(NEWREC)       POINT TO NEW RECORD                          
         USING BUYRECD,R3          ESTABLISH SPOT BUY RECORD                    
*                                                                               
         MVC   WRKCLT,BUYKCLT      SAVE PACKED CLIENT                           
*                                                                               
         NI    BUYKAM,X'0F'        KILL OLD AGENCY NYBBLE                       
         OC    BUYKAM,AGYAGNEW     ADD IN NEW AGENCY NYBBLE                     
*                                                                               
         MVC   BUYKCLT,MVTCLPKN    NEW CLIENT                                   
*                                                                               
         LA    R2,BUYMSTA          R2 POINTS TO MARKET CODE                     
*                                  R4 POINTS TO CONVERSION TABLE ENTRY          
*                                                                               
         OC    MVTESTNW,MVTESTNW   IF NEW ESTIMATE NUMBER GIVEN                 
         BZ    *+10                                                             
         MVC   BUYKEST,MVTESTNW       REPLACE ESTIMATE NUMBER                   
*                                                                               
         MVI   SWITCH,C'B'         INDICATE BUY RECORD                          
         BRAS  RE,GETSTA           FIND NEW MARKET/STATION                      
         MVI   SWITCH,0            CLEAR SWITCH                                 
         BNZ   BUYKEYDN            SKIP WRITING RECORD                          
*                                                                               
         LA    R6,BDELEM           POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
BUYPAYLP DS    0H                                                               
*                                                                               
         USING REGELEM,R6          ESTABLISH AS REGULAR ELEMENT                 
*                                                                               
         CLI   RCODE,0             DONE AT RECORD END                           
         BE    BUYPAYDN                                                         
*                                                                               
         CLI   RCODE,X'06'         LOOK FOR REGULAR ELEMENTS                    
         BL    BUYPAYCN                                                         
         CLI   RCODE,X'0D'         LOOK FOR REGULAR ELEMENTS                    
         BH    BUYPAYCN                                                         
*                                                                               
         OC    MVTSTDTE,MVTSTDTE   ANY START DATE                               
         BZ    BUYPAYL2                                                         
         CLC   RDATE,MVTSTDTE                                                   
         BNL   BUYPAYL2            ACCEPT SPOT                                  
         MVI   PRTFLAG,C'S'                                                     
         BRAS  RE,PRTDEL                                                        
         B     BUYPAYLP                                                         
*                                                                               
BUYPAYL2 OC    MVTNDDTE,MVTNDDTE   ANY END DATE                                 
         BZ    BUYPAYL4                                                         
         CLC   RDATE,MVTNDDTE                                                   
         BNH   BUYPAYL4                                                         
         MVI   PRTFLAG,C'E'                                                     
         BRAS  RE,PRTDEL                                                        
         B     BUYPAYLP                                                         
*                                                                               
BUYPAYL4 OC    RPAY,RPAY           OKAY IF UNPAID                               
         BZ    BUYPAYCN                                                         
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R3),P+04,13,0,0  PRINT KEY                         
         GOTO1 VHEXOUT,DMCB,(R6),P+40,16,0,0  PRINT REGULAR ELEMENT             
         GOTO1 VPRINTER                                                         
*                                                                               
         XC    RPAY,RPAY           KILL CLEARANCE DATE                          
*                                                                               
BUYPAYCN DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RLEN             GET ELEMENT LENGTH                           
         LA    R6,REGELEM(RF)      BUMP TO NEXT ELEMENT                         
         B     BUYPAYLP                                                         
*                                                                               
BUYPAYDN DS    0H                                                               
*                                                                               
         BC    0,BUYLOOP3                                                       
         OI    *-3,X'F0'                                                        
         MVC   P(9),=C'WRITE BUY'                                               
         GOTO1 VPRINTER                                                         
                                                                                
BUYLOOP3 DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R0,3,13(R3)         GET RECORD LENGTH                            
         AHI   R0,4                                                             
         LR    RE,R3                                                            
         AHI   RE,-4                                                            
         STH   R0,0(RE)            SET LEN+4 AT REC-4                           
         BRAS  RE,WRITE            WRITE RECORD TO OUTPUT DATASET               
*                                                                               
         CLI   MVTESTNW,0          IF CHANGING ESTIMATE NUMBER                  
         BE    BUYKEYDN               GO CHECK IF BUY FITS ANOTHER              
******   B     BUYKEYDN  ********* NOP - NEVER DO TWICE **********              
*                                     TABLE ENTRY                               
         L     R3,AREC                RE-POINT TO INCOMING RECORD               
         B     BUYKEYCN                                                         
*                                                                               
BUYKEYDN DS    0H                                                               
*                                                                               
BUYX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'SPLDEXTCZ - MOVE BETWEEN AGENCIES - GOAL'                       
***********************************************************************         
*                                                                     *         
*        GOAL RECORD - X'02'                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GOAL     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   MVGOALS,C'Y'        SKIP IF GOALS NOT WANTED                     
         BNE   GOALX                                                            
*                                                                               
         USING GOALRECD,R3         ESTABLISH SPOT GOAL RECORD                   
*                                                                               
         TM    GKEYAM,X'08'        ONLY MEDIA C, R OT T                         
         BO    *+12                                                             
         TM    GKEYAM,X'03'                                                     
         BNM   GOALX                                                            
*                                                                               
         NOP   *+14                                                             
         OI    *-3,X'F0'                                                        
         ZAP   LINE,=P'100'        FORCE NEW PAGE                               
*                                                                               
         L     R4,=A(MVTAB)        POINT TO SELECTION TABLE                     
         USING MVTABD,R4                                                        
*                                                                               
GOALKYLP DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         DONE AT END OF TABLE                         
         BE    GOALKYDN                                                         
*                                                                               
         CLC   GKEYAM,MVTAMOLD     MATCH ON AGENCY/MEDIA                        
         BNE   GOALKYCN                                                         
*                                                                               
         CLC   GKEYCLT,MVTCLTPK    MATCH ON PACKED CLIENT                       
         BNE   GOALKYCN                                                         
*                                                                               
         CLI   MVTPRDCD,0          OKAY IF ALL PRDS WANTED                      
         BE    GOALKY20                                                         
*                                                                               
         CLI   GKEYPRD,X'FF'       KEEP BUYING GUIDELINES                       
         BE    *+14                                                             
         CLC   GKEYPRD,MVTPRDCD    ELSE MATCH ON PRODUCT CODE                   
         BNE   GOALKYCN                                                         
*                                                                               
GOALKY20 DS    0H                                                               
*                                                                               
         CLC   GKEYEST,MVTESTST    MATCH ON ESTIMATE RANGE                      
         BL    GOALKYCN                                                         
         CLC   GKEYEST,MVTESTEN    MATCH ON ESTIMATE                            
         BH    GOALKYCN                                                         
*                                                                               
         OC    MVTMKT,MVTMKT       IF MARKET FILTER EXISTS                      
         BZ    *+14                                                             
         CLC   GKEYMKT,MVTMKT         MATCH ON MARKET                           
         BNE   GOALKYCN                                                         
*                                                                               
         B     GOALKYFD            ACCEPT GOAL RECORD                           
*                                                                               
GOALKYCN DS    0H                                                               
*                                                                               
         LA    R4,MVTENTL(R4)      POINT TO NEXT ENTRY IN TABLE                 
         B     GOALKYLP                                                         
*                                                                               
GOALKYFD DS    0H                                                               
*                                                                               
         BRAS  RE,COPY             COPY RECORD TO WORKAREA                      
*                                                                               
         L     R3,=A(NEWREC)       POINT TO NEW RECORD                          
         USING GOALRECD,R3         ESTABLISH SPOT GOAL RECORD                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,13(R3)         GET NEW RECORD LENGTH                        
         LA    R1,0(RF,R3)         POINT TO END OF RECORD                       
         MVI   0(R1),0             FORCE NULL AFTER RECORD                      
*                                                                               
         MVC   WRKCLT,GKEYCLT      SAVE PACKED CLIENT CODE                      
*                                                                               
         NI    GKEYAM,X'0F'        KILL   OLD AGENCY NYBBLE                     
         OC    GKEYAM,AGYAGNEW     ADD IN NEW AGECNY NYBBLE                     
*                                                                               
         MVC   GKEYCLT,MVTCLPKN    SET NEW CLIENT CODE                          
*                                                                               
         OC    MVTESTNW,MVTESTNW   IF NEW ESTIMATE NUMBER GIVEN                 
         BZ    *+10                                                             
         MVC   GKEYEST,MVTESTNW       REPLACE ESTIMATE NUMBER                   
*                                                                               
         LA    R2,GKEYMKT          R2 POINTS TO MARKET CODE                     
*                                  R4 POINTS TO CONVERSION TABLE ENTRY          
*                                                                               
         BRAS  RE,GETMKT           FIND NEW MARKET                              
         BNZ   GOALKYDN            SKIP WRITING RECORD ON ERROR                 
*                                                                               
*        DROP ANY GOAL OR LOCKIN ELEMENTS OUTSIDE OF RANGE                      
*                                                                               
         OC    MVTSTDTE,MVTSTDTE   SKIP IF NO DATE RANGE SPECIFIED              
         BNZ   *+14                                                             
         OC    MVTNDDTE,MVTNDDTE                                                
         BZ    GLELMDN                                                          
*                                                                               
         LA    R6,GDELEM           POINT TO FIRST ELEMENT                       
*                                                                               
GLELMLP DS     0H                                                               
*                                                                               
         USING GLEMENT,R6          ESTABLISH AS GOAL WEEK ELEMENT               
*                                                                               
         CLI   GLCODE,0            DONE AT RECORD END                           
         BE    GLELMDN                                                          
*                                                                               
         CLI   GLCODE,X'21'        LOOK FOR GOAL ELEMENTS                       
         BE    GLELMFD                                                          
*                                                                               
         CLI   GLCODE,X'30'        LOOK FOR LOCKIN ELEMENTS                     
         BL    GLELMCN                                                          
         CLI   GLCODE,X'33'                                                     
         BH    GLELMCN                                                          
*                                                                               
GLELMFD  DS    0H                                                               
*                                                                               
*        BOTH TYPES OF ELEMENT HAVE DATES IN SAME POSITION                      
*                                                                               
         CLC   GLWEEK,MVTSTDTE                                                  
         BNL   GLELML2             ACCEPT SPOT                                  
         MVI   PRTFLAG,C'S'                                                     
         BRAS  RE,PRTDEL                                                        
         B     GLELMLP                                                          
*                                                                               
GLELML2  OC    MVTNDDTE,MVTNDDTE   ANY END DATE                                 
         BZ    GLELML4                                                          
         CLC   GLWEEK,MVTNDDTE                                                  
         BNH   GLELML4                                                          
         MVI   PRTFLAG,C'E'                                                     
         BRAS  RE,PRTDEL                                                        
         B     GLELMLP                                                          
*                                                                               
GLELML4  DS    0H                  ELEMENT ACCEPTED                             
*                                                                               
GLELMCN DS     0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,GLEN             GET ELEMENT LENGTH                           
         LA    R6,GLEMENT(RF)      BUMP TO NEXT ELEMENT                         
         B     GLELMLP                                                          
*                                                                               
GLELMDN DS     0H                                                               
*                                                                               
         NOP   GLLOOP3                                                          
         OI    *-3,X'F0'                                                        
         MVC   P(10),=C'WRITE GOAL'                                             
         GOTO1 VPRINTER                                                         
                                                                                
GLLOOP3  DS    0H                                                               
         BRAS  RE,WRITE            WRITE RECORD TO OUTPUT DATASET               
*                                                                               
         CLI   MVTESTNW,0          IF CHANGING ESTIMATE NUMBER                  
         BE    GOALKYDN               GO CHECK IF BUY FITS ANOTHER              
*                                     TABLE ENTRY                               
         L     R3,AREC                RE-POINT TO INCOMING RECORD               
         B     GOALKYCN                                                         
*                                                                               
GOALKYDN DS    0H                                                               
*                                                                               
GOALX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - WORKD'                     
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DS    0D                                                               
*                                                                               
DMCB     DS    6F                                                               
WORK     DS    CL128                                                            
APARM    DS    A                   A(PARAMETER LIST)                            
*                                                                               
SAVEKEY  DC    XL13'00'            LAST KEY WRITTEN                             
*                                                                               
BYTE     DS    X                                                                
SWITCH   DC    X'0'                C'B' - INDCATES MKT FROM BUY RECORD          
COUNT    DS    F                                                                
CTR      DC    PL2'10'             TRACE COUNTER                                
*                                                                               
RECCNTR  DS    PL8                 RECORD COUNTER                               
*                                                                               
FROMRECC DS    CL(FRMRECLQ)        FROM AGENCY STATION RECORD                   
TORECC   DS    CL(TORECLQ)         TO   AGENCY STATION RECORD                   
*                                                                               
STANTRYC DS    XL(STANTRYL)        STATION TABLE ENTRY BUILD AREA               
STATABA  DS    A                   A(STATION BINSRCH TABLE)                     
*                                                                               
BYRNTRYC DS    XL(BYRNTRYL)        BUYER   TABLE ENTRY BUILD AREA               
BYRTABA  DS    A                   A(BUYER   BINSRCH TABLE)                     
*                                                                               
DUB      DS    D                                                                
HALF     DS    H                                                                
KEY      DS    CL10                                                             
BAGYMD   DS    CL1                                                              
WAGYMD   DS    CL1                 MASTER AGY/MEDIA                             
PRINTSW  DC    XL1'00'             X'01' - PRINT TRACE                          
OLDKEY   DC    XL13'00'            OLD KEY SAVEAREA                             
NEWKEY   DC    XL13'00'            NEW KEY SAVEAREA                             
EOFTOSW  DC    X'00'               END OF FILE SWITCH FOR TO FILE               
*                                                                               
*                                                                               
*        PARAMETER LIST SAVE AREA                                               
*                                                                               
         DS    0D                  ALIGNMENT                                    
PLIST    DS    0CL24               PARAMETER LIST - SAVED                       
PRMMODE  DS    0XL1                CALLING MODE                                 
PRMMINIQ EQU   X'00'                 X'00'= INITIALISE                          
PRMMRECQ EQU   X'01'                 X'01'= RECORD IN CORE                      
PRMMEOFQ EQU   X'FF'                 X'FF'= END OF FILE                         
*                                                                               
PRMRTNCD DS    0XL1                RETURN CODE                                  
PRMRKPQ  EQU   X'00'               X'00'= KEEP RECORD                           
PRMRPRGQ EQU   X'FF'               X'FF'= PURGE RECORD                          
PRMREOJQ EQU   X'FF'               X'FF'/C'EOJ'=PURGE & CAUSE EOJ               
*                                                                               
AREC     DS    A                   A(CURRENT RECORD)                            
*                                                                               
VTAPEOUT DS    A                   V(TAPEOUT DCB)                               
APARAMC  DS    A                   A(PARAMETER CARD)                            
VLDDEFN  DS    A                   A(FILE DEFINITION)                           
VPRINTER DS    A                   V(PRINTER)                                   
VCPRINT  DS    A                   V(CPRINT)                                    
VHEXOUT  DS    A                   V(HEXOUT)                                    
VCLPACK  DS    A                   V(HEXOUT)                                    
*                                                                               
ELCODE   DS    X                                                                
WRKCLT   DS    XL2                 PACKED CLIENT CODE                           
*                                                                               
PRTFLAG  DC    C' '                                                             
PRTCOUNT DC    PL3'0'                                                           
*                                                                               
AGYOLD   DS    CL2                 AGENCY - OLD                                 
AGYMDOLD DC    XL1'00'             AGENCY/MEDIA -OLD                            
AGYAGOLD DC    XL1'00'             AGENCY       -OLD                            
AGYNEW   DS    CL2                 AGENCY - NEW                                 
AGYMDNEW DC    XL1'00'             AGENCY/MEDIA -NEW                            
AGYAGNEW DC    XL1'00'             AGENCY       -NEW                            
         EJECT                                                                  
         DS    D                                                                
QRECORD  DS    0CL80                                                            
QAREA    DS    0CL80   COLUMN                                                   
QPROG    DS    0CL2    ------                                                   
QCODE    DS    CL2        1        PROGRAM CODE                                 
QAGY     DS    CL2        3        AGENCY CODE                                  
QMED     DS    CL1        5        MEDIA CODE (R/T)                             
QCLT     DS    CL3        6        CLIENT CODE                                  
QPGR     DS    CL1        9        PROCESS BY DIVISION                          
QMGR     DS    CL1       10        PROCESS BY DISTRICT                          
QCLOFFC  DS    CL1       11        CLIENT OFFICE FILTER                         
QBYID    EQU   QCLOFFC             C'Y' IF BUYS PROCESSED BY ID                 
QPRD     DS    CL3       12        PRODUCT MNEMONIC                             
QMKT     DS    CL4       15        MARKET NUMBER                                
QSTA     DS    CL5       19        STATION CALL LETTERS                         
QEST     DS    CL3       24        ESTIMATE NUMBER                              
QESTEND  DS    CL3       27        LAST NUMBER IN ESTIMATE GROUP                
QDEMOVRD DS    CL1       30        Y=DEMO OVERRIDE ACTIVE                       
QCONTREQ DS    CL1       31        C'*' ==> DATA IN QAREA2                      
QSTAUTO  DS    CL3       32        AUTO REQUEST START DATE                      
QENDAUTO DS    CL3       35        AUTO REQUEST END DATE                        
         ORG   QSTAUTO+2                                                        
QDEMNOS  DS    CL4                 DEMO OVERRIDE NUMBERS                        
QSTART   DS    CL6       38        REQUEST START DATE                           
QEND     DS    0CL6      44        REQUEST END DATE                             
QTODAY   DS    CL6       44                                                     
QBOOK1   DS    CL4       50        RATING BOOK (YYMM) GOAL/ESTD DATA            
QHUT1    DS    CL2       54        HUT ADJUSTMENT MONTH                         
QRERATE  DS    CL1       56        RERATE TYPE  I=INVOICE                       
*                                               P=PURCHASED                     
*                                               A=ADJUST ONLY                   
*                                               U=UPGRADE (+Q2BOOK2)            
QCOMPARE DS    CL1       57        DATA COMPARE OPTION                          
*                                  A=GOAL V PURCHASED                           
*                                  B=GOAL V AFFIDAVIT                           
*                                  C=PURCHASED V PURCHASED (RERATED)            
*                                  D=PURCHASED V AFFIDAVIT                      
*                                  E=LOCKIN V PURCHASED                         
*                                  F=LOCKIN V AFFIDAVIT                         
*                                  L=GOAL V PURCHASED, LOCKIN PURCHASED         
QAFFIL   DS    CL1       58        AFFILIATION FILTER                           
QPRGTYPE DS    CL1       59        PROGRAM TYPE FILTER                          
QDPTDET  DS    CL1       60        DAYPART DETAIL CONTROL                       
*                                  A=SHOW FULL DETAIL (DEFAULT)                 
*                                  B=SUPPRESS SPOT-LENGTH                       
*                                  C=SUPPRESS SPOT-LENGTH & DAY-PART            
QDPTMENU DS    CL1       61        DAYPART MENU OVERRIDE                        
QOPT1    DS    CL1       62        OPTION 1                                     
QOPT2    DS    CL1       63        OPTION 2                                     
QOPT3    DS    CL1       64        OPTION 3                                     
QOPT4    DS    CL1       65        OPTION 4                                     
QOPT5    DS    CL1       66        OPTION 5                                     
QGRP     DS    CL2       67        GROUP                                        
QFILTER  EQU   QGRP                FILTER TYPE/VALUE                            
QUESTOR  DS    CL12      69        REQUESTOR NAME                               
         ORG   QUESTOR                                                          
QNWAGY   DS    CL2                 NEW AGENCY POWER CODE                        
QNWCLT   DS    CL3                 NEW CLIENT CODE                              
         DS    CL3                 SPARE                                        
QNWEST   DS    CL3                 NEW ESTIMATE CODE                            
         DS    CL1                 SPARE                                        
*                                                                               
         EJECT                                                                  
         ORG   QAREA+57                                                         
QCMRCL   DS    CL8       58        COMMERCIAL FILTER                            
         ORG   QAREA+29                                                         
QREP     DS    CL3       30        DISCREPANCY REP                              
QREPTYPE DS    CL1       33                                                     
QTIME    DS    CL2       34                                                     
QCONT    DS    CL1       36                                                     
         SPACE 2                                                                
         ORG   QAREA+49                                                         
QPRD2    DS    CL3       50        PRODUCT 2                                    
QPRD3    DS    CL3       53        PRODUCT 3                                    
QAMTTYPE DS    CL1       56        AMOUNT TYPE                                  
QAMT     DS    CL10      57        AMOUNT                                       
         DS    CL2       67                                                     
QIND     DS    CL12      69        INVOICE NUMBER                               
         SPACE 2                                                                
         DS    0A                  ALIGNMENT                                    
BSPSTA   DS    XL(BSPPRML)         STATION TABLE BINSRCH PARMS                  
         DS    0A                  ALIGNMENT                                    
BSPBYR   DS    XL(BSPPRML)         BUYER   TABLE BINSRCH PARMS                  
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - MVTAB'                     
***********************************************************************         
*                                                                     *         
*        DATA FOR THIS CONVERSION                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*        RECORD TYPES TO MOVE                                                   
*                                                                               
MV1STTIM DC    C'Y'                FIRST TIME SWITCH        - Y/N               
*                                                                               
MVBUYS   DC    C'N'                MOVE BUYS                - Y/N               
MVGOALS  DC    C'N'                MOVE GOALS               - Y/N               
MVNWS    DC    C'N'                MOVE BUYERS WORKSHEET    - Y/N               
MVCLT    DC    C'N'                DO NOT MOVE CLIENT   HDR - Y/N               
MVPRD    DC    C'N'                DO NOT MOVE PRODUCT  HDR - Y/N               
MVEST    DC    C'N'                DO NOT MOVE ESTIMATE HDR - Y/N               
MVCABLE  DC    C'N'                MOVE CABLE BUYS          - Y/N               
*                                                                               
         DS    0D                                                               
         DC    C'SAVCLIST'                                                      
SAVCLIST DS    XL1000              CLIENT PRODUCT LIST SAVEAREA                 
*                                                                               
*        TABLE OF WHAT TO MOVE                                                  
*                                                                               
RRLEN    DS    XL2                 OUTPUT RECORD LENGTH                         
         DS    XL2                 SPARE                                        
NEWREC   DS    XL4096              NEW RECORD BUILD AREA                        
*                                                                               
         DS    0D                                                               
         DC    C'**MVTAB*'                                                      
MVTAB    DS    0X                  TABLE ENTRY                                  
*                                                                               
         DS    1000XL(MVTENTL)     TRANSFER TABLE                               
*                                                                               
         DC    X'FF'               EOT                                          
*                                                                               
WORKLQ   EQU   *-WORKD             LENGTH OF WORKING STORAGE                    
         EJECT                                                                  
CARD     DCB   DDNAME=CARDIN,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,      X        
               EODAD=TABINIDN                                                   
*                                                                               
*        STATION FILES                                                          
*                                                                               
FSTAFROM DCB   DDNAME=FSTAFROM,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,    X        
               EODAD=DMXINDN                                                    
*                                                                               
FSTATO   DCB   DDNAME=FSTATO,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,      X        
               EODAD=DMXINTC1                                                   
*                                                                               
*        BUYER   FILES                                                          
*                                                                               
FBYRFROM DCB   DDNAME=FBYRFROM,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,    X        
               EODAD=DMXIBYDN                                                   
*                                                                               
FBYRTO   DCB   DDNAME=FBYRTO,DSORG=PS,RECFM=FB,LRECL=80,MACRF=GM,      X        
               EODAD=DMXIBYC1                                                   
*                                                                               
FILEOUT  DCB   DDNAME=TEMPOUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               BLKSIZE=25000                                                    
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - BSRPRMD'                   
***********************************************************************         
*                                                                     *         
*        DSECT FOR BINSRCH PARAMETERES                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BSRPRMD  DSECT                                                                  
       ++INCLUDE DDBSRPRMD                                                      
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - MVTABD'                    
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF DATA TO MOVE                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MVTABD   DSECT                                                                  
MVTENT   DS    0X                  ENTRY IN TABLE                               
MVTAGOLD DS    CL2                 AGENCY       - OLD                           
MVTAMOLD DS    XL1                 AGENCY/MEDIA - OLD                           
MVTAMNEW DS    XL1                 AGENCY/MEDIA - NEW                           
MVTCLT   DS    CL3                 CLIENT                                       
MVTCLTPK DS    XL2                 CLIENT - PACKED                              
MVTCLTNW DS    CL3                 CLIENT - NEW                                 
MVTCLPKN DS    XL2                 CLIENT - NEW PACKED                          
MVTPRD   DS    XL3                 PRODUCT  - ALL - NULLS MEANS ALL             
MVTPRDCD DS    XL1                 PRODUCT CODE                                 
MVTESTST DS    AL1                 START ESTIMATE - NULLS MEANS ALL             
MVTESTEN DS    AL1                 END   ESTIMATE - NULLS MEANS ALL             
MVTESTNW DS    AL1                 NEW ESTIMATE NUMBER (NO END EST)             
MVTSTDTE DS    XL2                 START DATE                                   
MVTNDDTE DS    XL2                 END DATE                                     
MVTMKT   DS    XL2                 MARKET                                       
MVTSTA   DS    XL3                 STATION                                      
MVTENTL  EQU   *-MVTENT            TABLE ENTRY LENGTH                           
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - STATABD'                   
***********************************************************************         
*                                                                     *         
*        DSECT FOR STATION CONVERSION TABLE                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STANTRYD DSECT                                                                  
STANTRY  DS    0XL1                CONVERSION RECORD                            
STTKEY   DS    0XL1                KEY FOR TABLE                                
STTAMOLD DS    XL1                 OLD AGENCY/MEDIA                             
STTMSOLD DS    XL5                 OLD MARKET/STATION                           
STTCPOLD DS    CL2                 OLD CLIENT - PACKED                          
STTKEYLQ EQU   *-STTKEY            KEY LENGTH                                   
*                                                                               
STTMKOLD DS    CL4                 OLD MARKET NUMBER                            
STTSTOLD DS    CL5                 OLD STATION                                  
STTCLOLD DS    CL3                 OLD CLIENT                                   
STTAMNEW DS    XL1                 NEW AGENCY/MEDIA                             
STTMSNEW DS    XL5                 NEW MARKET/STATION                           
STTMKNEW DS    CL4                 NEW MARKET NUMBER                            
STTSTNEW DS    CL5                 NEW STATION                                  
STANTRYL EQU   *-STANTRY           RECORD LENGTH                                
*                                                                               
STTRMAXQ EQU   20000               MAXIMUM NUMBER OF RECORDS IN FILE            
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - FROMRECD'                  
***********************************************************************         
*                                                                     *         
*        DSECT FOR FROM MARKET/STATION CONVERSION RECORD              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FRMRECD  DSECT                                                                  
FRMREC   DS    0XL1                CONVERSION RECORD                            
FRMKEY   DS    0XL15               STATION RECORD KEY                           
FRMTYPE  DS    CL1'S'              STATION RECORD TYPE                          
FRMMED   DS    CL1                 MEDIA                                        
FRMCALL  DS    CL5                 STATION CALL LETTERS                         
FRMAGY   DS    CL2                 AGENCY                                       
FRMCLT   DS    CL3                 CLIENT                                       
         DS    CL3'000'            FILL                                         
*                                                                               
FRMMKT   DS    CL4                 MARKET                                       
FRMMKSTA DS    XL5                 MKT/STA PACKED                               
FRMAGYMD DS    CL1                 AGENCY/MEDIA                                 
         DS    XL(80-(*-FRMRECD))  SPARE                                        
FRMRECLQ EQU   *-FRMREC            RECORD LENGTH                                
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - TORECD'                    
***********************************************************************         
*                                                                     *         
*        DSECT FOR TO   MARKET/STATION CONVERSION RECORD              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TORECD   DSECT                                                                  
TOREC    DS    0XL1                CONVERSION RECORD                            
TOKEY    DS    0XL15               STATION RECORD KEY                           
TOTYPE   DS    CL1'S'              STATION RECORD TYPE                          
TOMED    DS    CL1                 MEDIA                                        
TOCALL   DS    CL5                 STATION CALL LETTERS                         
TOAGY    DS    CL2                 AGENCY                                       
TOCLT    DS    CL3                 CLIENT                                       
         DS    CL3'000'            FILL                                         
*                                                                               
TOMKT    DS    CL4                 MARKET                                       
TOMKSTA  DS    XL5                 MKT/STA PACKED                               
TOAGYMD  DS    CL1                 AGENCY/MEDIA                                 
         DS    XL(80-(*-TORECD))   SPARE                                        
TORECLQ EQU    *-TOREC             RECORD LENGTH                                
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - BYRTABD'                   
***********************************************************************         
*                                                                     *         
*        DSECT FOR BUYER   CONVERSION TABLE                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BYRNTRYD DSECT                                                                  
BYRNTRY  DS    0XL1                CONVERSION RECORD                            
BYTKEY   DS    0XL1                KEY FOR TABLE                                
BYTAMOLD DS    XL1                 OLD AGENCY/MEDIA                             
BYTBCOLD DS    XL1                 OLD INTERNAL BUYER CODE                      
BYTKEYLQ EQU   *-BYTKEY            KEY LENGTH                                   
*                                                                               
BYTBYOLD DS    CL3                 OLD BUYER ID                                 
BYTBNOLD DS    CL20                OLD BUYER NAME                               
BYTAMNEW DS    XL1                 NEW AGENCY/MEDIA                             
BYTBYNEW DS    XL3                 NEW BUYER ID                                 
BYTBCNEW DS    XL1                 NEW INTERNAL BUYER CODE                      
BYTBNNEW DS    CL20                NEW BUYER NAME                               
BYRNTRYL EQU   *-BYRNTRY           RECORD LENGTH                                
*                                                                               
BYTRMAXQ EQU   600                 MAXIMUM NUMBER OF RECORDS IN FILE            
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - BYRFRMD'                   
***********************************************************************         
*                                                                     *         
*        DSECT FOR FROM BUYER CONVERSION RECORD                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BYRFRMD  DSECT                                                                  
BYRFRM   DS    0XL1                CONVERSION RECORD                            
BFRKEY   DS    0XL13               BUYER RECORD KEY                             
BFRKTYP  DS    XL1'0D'             STATION RECORD TYPE                          
BFRKSUB  DS    XL1'65'             STATION RECORD SUB TYPE                      
BFRKAM   DS    CL1                 AGENCY/MEDIA                                 
BFRKBYR  DS    CL3                 BUYER ID                                     
         ORG   BFRKEY+L'BFRKEY                                                  
*                                                                               
*                                                                               
BFRBYRCD DS    XL1                 BUYER CODE                                   
BFRBYRNM DS    CL20                BUYER NAME                                   
BFRAGYMD DS    CL1                 AGENCY/MEDIA                                 
         DS    XL(80-(*-BYRFRM))   SPARE                                        
BFRRECLQ EQU   *-BYRFRM            RECORD LENGTH                                
*                                                                               
         TITLE 'STLDEXTMVC - MOVE BETWEEN AGENCIES - BYRTOD'                    
***********************************************************************         
*                                                                     *         
*        DSECT FOR TO   BUYER          CONVERSION RECORD              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BYRTOD   DSECT                                                                  
BYRTO    DS    0XL1                CONVERSION RECORD                            
BTOKEY   DS    0XL13               BUYER RECORD KEY                             
BTOKTYP  DS    XL1'0D'             STATION RECORD TYPE                          
BTOKSUB  DS    XL1'65'             STATION RECORD SUB TYPE                      
BTOKAM   DS    CL1                 AGENCY/MEDIA                                 
BTOKBYR  DS    CL3                 BUYER ID                                     
         ORG   BTOKEY+L'BTOKEY                                                  
*                                                                               
*                                                                               
BTOBYRCD DS    XL1                 BUYER CODE                                   
BTOBYRNM DS    CL20                BUYER NAME                                   
BTOAGYMD DS    CL1                 AGENCY/MEDIA                                 
         DS    XL(80-(*-BYRTOD))   SPARE                                        
BTORECLQ EQU   *-BYRTO             RECORD LENGTH                                
*                                                                               
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
*SPSTAPACKD                                                                     
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
         DS    CL1                                                              
         DS    CL2                                                              
         DS    CL1                                                              
POLDAGY  DS    CL2                 AGENCY         - OLD                         
         DS    CL2                                                              
POLDAM   DS    CL2                 AGENCY/MEDIA   - OLD - HEX                   
         DS    CL2                                                              
         DS    CL2                                                              
POLDMSTA DS    CL10                MARKET/STATION - OLD - HEX                   
         DS    CL2                                                              
POLDMK   DS    CL4                 MARKET NUMBER  - OLD - DECIMAL               
         DS    CL2                                                              
POLDSTA  DS    CL5                 STATION CALL   - OLD - DECIMAL               
         DS    CL2                                                              
POLDCLT  DS    CL3                 CLIENT         - OLD                         
         DS    CL5                                                              
PNEWAGY  DS    CL2                 AGENCY         - NEW                         
         DS    CL2                                                              
PNEWAM   DS    CL2                 AGENCY/MEDIA   - NEW - HEX                   
         DS    CL2                                                              
PNEWMSTA DS    CL10                MARKET/STATION - NEW - HEX                   
         DS    CL2                                                              
PNEWMK   DS    CL4                 MARKET NUMBER  - NEW - DECIMAL               
         DS    CL2                                                              
PNEWSTA  DS    CL5                 STATION CALL   - NEW - DECIMAL               
         DS    CL2                                                              
PNEWCLT  DS    CL3                 CLIENT         - NEW                         
         DS    CL5                                                              
* DSECT FOR TITLE PRINT LINE                                                    
PTLINED  DSECT                                                                  
         DS    CL1                                                              
PTAMOLD  DS    CL6'AM-OLD'         AGENCY/MEDIA - OLD - HEX                     
         DS    CL1                                                              
PTAMNEW  DS    CL6'AM-NEW'         AGENCY/MEDIA - NEW - HEX                     
         DS    CL1                                                              
         DS    CL2                                                              
PTCLTOLD DS    CL3'CLT'            CLIENT - OLD                                 
         DS    CL2                                                              
PTCLPOLD DS    CL4'CLT'            PACKED CLIENT - OLD                          
         DS    CL2                                                              
PTAGYOLD DS    CL2'AG'             AGENCY - OLD                                 
         DS    CL1                                                              
PTCLTNEW DS    CL3'CLT'            CLIENT - NEW                                 
         DS    CL2                                                              
PTCLPNEW DS    CL4'CLT'            PACKED CLIENT - NEW                          
         DS    CL2                                                              
PTOLDMST DS    CL10'MKT/STA'       MARKET/STATION - OLD - HEX                   
         DS    CL2                                                              
PTOLDMKT DS    CL4'MKT'            MARKET NUMBER  - OLD - DECIMAL               
         DS    CL2                                                              
PTOLDSTA DS    CL5'STA'            STATION CALL   - OLD                         
         DS    CL2                                                              
PTNEWMST DS    CL10'MKT/STA'       MARKET/STATION - NEW - HEX                   
         DS    CL2                                                              
PTNEWMKT DS    CL4'MKT'            MARKET NUMBER  - NEW - DECIMAL               
         DS    CL2                                                              
PTNEWSTA DS    CL5'STA'            STATION CALL   - NEW                         
         DS    CL1                                                              
PTAGYNEW DS    CL2'AG'             AGENCY - NEW                                 
         DS    CL1                                                              
*                                                                               
PBLINED  DSECT                     BUYER PRINT LINE                             
         DS    CL1                                                              
         DS    CL2                                                              
         DS    CL1                                                              
         DS    CL2                                                              
PBAMOLD  DS    CL2                 AGENCY/MEDIA   - OLD - HEX                   
         DS    CL2                                                              
         DS    CL1                                                              
PBOLDBYR DS    CL3                 BUYER ID       - OLD                         
         DS    CL2                                                              
PBOLDBC  DS    CL3                 BUYER CODE     - OLD - DECIMAL               
         DS    CL2                                                              
PBOLDBN  DS    CL20                BUYER NAME     - OLD                         
         DS    CL2                                                              
PBAMNEW  DS    CL2                 AGENCY/MEDIA   - NEW - HEX                   
         DS    CL2                                                              
PBNEWBYR DS    CL3                 BUYER ID       - NEW                         
         DS    CL2                                                              
PBNEWBC  DS    CL3                 BUYER CODE     - NEW - DECIMAL               
         DS    CL2                                                              
PBNEWBN  DS    CL20                BUYER NAME     - NEW                         
         DS    CL2                                                              
* DSECT FOR TITLE PRINT LINE                                                    
PBTLINED  DSECT                     BUYER PRINT LINE                            
         DS    CL1                                                              
         DS    CL2                                                              
         DS    CL1                                                              
         DS    CL2                                                              
PBTAMOLD DS    CL2                 AGENCY/MEDIA   - OLD - HEX                   
         DS    CL2                                                              
         DS    CL1                                                              
PBTOLDBY DS    CL3                 BUYER ID       - OLD                         
         DS    CL2                                                              
PBTOLDBC DS    CL3                 BUYER CODE     - OLD - DECIMAL               
         DS    CL2                                                              
PBTOLDBN DS    CL20                BUYER NAME     - OLD                         
         DS    CL2                                                              
PBTAMNEW DS    CL2                 AGENCY/MEDIA   - NEW - HEX                   
         DS    CL2                                                              
PBTNEWBY DS    CL3                 BUYER ID       - NEW                         
         DS    CL2                                                              
PBTNEWBC DS    CL3                 BUYER CODE     - NEW - DECIMAL               
         DS    CL2                                                              
PBTNEWBN DS    CL20                BUYER NAME     - NEW                         
         DS    CL2                                                              
*                                                                               
*        MOVE TABLE PRINTOUT LINE                                               
*                                                                               
PTBLINED DSECT                                                                  
         DS    CL2                                                              
PTBAMOLD DS    CL2                 AGENCY/MEDIA - OLD                           
         DS    CL2                                                              
PTBCLT   DS    CL3                 CLIENT                                       
         DS    CL2                                                              
PTBCLTPK DS    XL4                 CLIENT PACKED                                
         DS    CL2                                                              
PTBPRD   DS    CL3                 PRODUCT                                      
         DS    CL2                                                              
PTBESTST DS    CL3                 START ESTIMATE                               
         DS    CL2                                                              
PTBESTEN DS    CL3                 END   ESTIMATE                               
         DS    CL17                                                             
PTBAMNEW DS    CL2                 AGENCY/MEDIA - NEW                           
         DS    CL2                                                              
PTBCLTNW DS    CL3                 CLIENT NEW                                   
         DS    CL2                                                              
PTBCLPKN DS    XL4                 CLIENT PACKED NEW                            
         DS    CL2                                                              
PTBPRDNW DS    CL3                 PRODUCT NEW                                  
         DS    CL2                                                              
PTBESTNW DS    CL3                 ESTIMATE NEW                                 
         DS    CL2                                                              
PTBMKT   DS    CL4                 MARKET FILTER                                
         DS    CL2                                                              
         DS    CL3                                                              
         DS    CL2                                                              
         EJECT                                                                  
*SPGENBUY                                                                       
         PRINT OFF                                                              
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENCLT                                                                       
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENEST                                                                       
         PRINT OFF                                                              
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENGOAL                                                                      
         PRINT OFF                                                              
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'084SPLDEXTCZ 06/26/00'                                      
         END                                                                    
*                                                                               
