*          DATA SET SPREP4802  AT LEVEL 048 AS OF 06/02/16                      
*PHASE SP4802A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE DLFLD                                                                  
*                                                                               
***********************************************************************         
* QOPT1 DETERMINES REQUEST TYPE                                       *         
* QOPT2 = Y GIVES AN ANALYSIS BY STATION SIZE                         *         
* QOPT3 = Y GIVES AN ANALYSIS BY AFFILIATE                            *         
* QOPT4 = Y SHOW DEACTIVATED ONLY                                     *         
* QOPT5 = Y SHOW EIX=Y STATIONS ONLY                                  *         
* QOPT6 = D = DOWNLOAD                                                *         
* QOPT7 = Y = DOWNLOAD FOR GROUPM                                     *         
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT DSSUP-7397  05/24/16 FIX DOWNLOAD ISSUES                       *         
*                                                                     *         
* 11/04/13 - DOWNLOAD OPTION (AKAT)                    *                        
* 12MAY03 PWE - FIX PRINT OF 'NET=' FOR CANADA STATION *                        
*               & SPACE PSTOUT (NOT XC)-NICER WHEN D/L *                        
* 06/20/02 - DOWNLOAD OPTION (AKAT)                    *                        
* 05/10/90 - REPLACE TELEGRAM NUMBER WITH FAX NUMBER   *                        
* 11/14/90 - ADD GST CODE                              *                        
* 01/29/91 - DON'T PACK REP, CAN BE ALPHA/CHAR         *                        
* 06/05/91 - ADD STATION BY AFFILIATE ANALYSIS         *                        
* 11/04/91 - FIX BINARY ZEROS STA PRINT BUG LEV 84     *                        
* 05/06/92 - FIX MISSING STATIONS BY SIZE/AFFIL LEV 85 *                        
* 02/22/93 - STOP KILLING REP FIELDS FROM STA MAS  101 *                        
* 05/04/93 - ADD PST FIELD                             *                        
* 07/22/93 - DEACTIVATED STATIONS                      *                        
* 09/16/94 - DON'T LET BAD STA'S GO THRU NEW STAPACK   *                        
* 10/11/94 - ALLOW 3 CHAR STATIONS - LEV 29            *                        
* 12/01/99 - OPTION TO REPORT STATIONS WITH EIX=Y ONLY *                        
********************************************************                        
         SPACE 1                                                                
         TITLE 'SP4802 - STATION FILE PRINT - APPLICATION'                      
SP4802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SP4802                                                         
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SP4802+4096,RC                                                   
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*        XC    SRREC,SRREC                                                      
         LA    R0,SRREC                                                         
         LHI   R1,L'SRREC                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE         CLEAR                                              
*                                                                               
         CLI   QOPT6,C'D'    DOWNLOADABLE?                                      
         BNE   CNTRL00       NO                                                 
         CLI   QOPT1,C'S'    STATIONS?                                          
         BNE   EXIT          NO...DOWNLOADABLE OPT ONLY FOR STATIONS            
         MVI   QOPT2,0       MAKE SURE THAT WE ONLY DO THE DOWNLOADABLE         
         MVI   QOPT3,0       REPORT                                             
*                                                                               
         SPACE 1                                                                
* CONTROL SECTION *                                                             
         SPACE 1                                                                
CNTRL00  CLI   MODE,STAFRST                                                     
         BE    SP200               MASTER STATION RECORDS                       
         BH    CNTRL10                                                          
         SPACE 1                                                                
* 'FRST' BREAKS *                                                               
         SPACE 1                                                                
         CLI   MODE,MKTFRST                                                     
         BE    SP100               MARKET RECORDS                               
         CLI   MODE,REQFRST                                                     
         BE    SP050               INITIALIZATION                               
         CLI   MODE,RUNFRST                                                     
         BE    SP000               INITIALIZE HEADHOOK & GET RELO               
         B     EXIT                                                             
         SPACE 1                                                                
* 'LAST' BREAKS *                                                               
         SPACE 1                                                                
CNTRL10  DS    0H                                                               
         CLI   MODE,PROCBUY                                                     
         BE    SP300               STATION ADDRESS RECORDS                      
         CLI   MODE,PROCGOAL                                                    
         BE    SP400               REP RECORDS                                  
         CLI   MODE,MKTLAST                                                     
         BE    SP700               PRINT STATION FILE LISTING                   
         CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
         GOTO1 VSORTER,DMCB,=C'END'                                             
         MVI   SORTSTAT,0                                                       
EXIT     XIT1                                                                   
         SPACE 1                                                                
         EJECT                                                                  
* SP000 - INITIALIZE HEADHOOK & CALCULATE THE RELOCATION FACTOR *               
         SPACE 1                                                                
SP000    DS    0H                                                               
         LA    RF,SP48HDHK                                                      
         ST    RF,HEADHOOK                                                      
         STM   R9,RC,SP48R9RC                                                   
         RELOC RELO                                                             
         SPACE 1                                                                
         L     RF,=A(MKTBUF)                                                    
         A     RF,RELO                                                          
         ST    RF,VMKTBUF                                                       
         L     RF,=V(SORTER)                                                    
         A     RF,RELO                                                          
         ST    RF,VSORTER                                                       
         L     RF,=V(DOWNLD)                                                    
         A     RF,RELO                                                          
         ST    RF,VDOWNLD                                                       
*                                                                               
         MVC   DUB,SPACES          GET PSTVAL                                   
         MVC   DUB(6),=C'T00A6B'                                                
         GOTO1 LOADER,DMCB,DUB,0                                                
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   VPSTVAL,4(R1)                                                    
*                                                                               
         L     RF,=V(DLFLD)                                                     
         A     RF,RELO                                                          
* THIS STORAGE IS SHARED BY ALL CSECTS                                          
         LA    R8,SPACEND                                                       
         USING SP48WRKD,R8                                                      
         ST    RF,VDLFLD                                                        
         DROP  R8                                                               
         LA    RF,BUFREC                                                        
         ST    RF,BUFFIO                                                        
         L     RF,=V(BUFFALOC)                                                  
         A     RF,RELO                                                          
         ST    RF,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         MVI   SORTSTAT,0                                                       
         B     EXIT                                                             
         SPACE 1                                                                
* SP050 - AT REQFRST, INITIALIZE SORTER AND THE BINSRCH TABLES *                
         SPACE 1                                                                
SP050    DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',BUFFBUFF                                  
         CLI   SORTSTAT,C'I'       HAS THE SORTER BEEN INITIALIZED?             
         BE    SP070                YES.                                        
         CLI   SORTSTAT,0          CAN WE INITIIALIZE THE SORTER?               
         BE    SP070                YES.                                        
         SPACE 1                                                                
SP060    DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         OC    DM2,DM2                                                          
         BNZ   SP060                                                            
         MVI   SORTSTAT,C'G'                                                    
SP070    DS    0H                                                               
         GOTO1 VSORTER,DMCB,SORT,RECCARD                                        
         MVI   SORTSTAT,C'I'       SORTER HAS BEEN INITIIALIZED                 
         SPACE 1                                                                
SP080    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R0,7                                                             
         LA    R2,CNTRS                                                         
         ZAP   0(4,R2),=P'0'       ZERO RECORD COUNTERS                         
         LA    R2,4(R2)                                                         
         BCT   R0,*-10                                                          
         XC    TMKT,TMKT                                                        
         MVI   OLDTYPE,X'FF'                                                    
         MVI   OLDMED,C'T'                                                      
*                                                                               
         CLI   QOPT2,C'Y'          TEST STATION BY SIZE ANALYSIS                
         BE    SP090                                                            
         CLI   QOPT3,C'Y'          TEST STATION BY AFFILIATE                    
         BNE   SP099                                                            
SP090    DS    0H                                                               
         OPEN  (WORKFIL,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SP099    CLI   QOPT6,C'D'             SPECIAL DOWNLOAD VERSION                  
         BNE   EXIT                                                             
         XC    HEADHOOK,HEADHOOK      MUST CLEAR HEADHOOK                       
         GOTO1 REPORT                                                           
         GOTO1 VDOWNLD,DMCB,(RA)                                                
         B     EXIT                                                             
         EJECT                                                                  
* SP100 - AT MKTFRST, BUILD MARKET SORTER RECORDS *                             
         SPACE 1                                                                
SP100    DS    0H                                                               
         L     R8,ADMARKET                                                      
         USING MKTREC,R8                                                        
         MVI   ERRCD,1                                                          
         MVI   SRTYPE,C'3'         TYPE IS MKT                                  
         MVC   SRMED,MKTKMED       MEDIA                                        
         MVC   SRMKT,MKTKMKT       MARKET NUMBER                                
         BAS   RE,SPUTSORT         SET SRDATA AND PUT RECORD TO SORTER          
         B     EXIT                                                             
         DROP  R8                                                               
         SPACE 1                                                                
*===================================================*                           
* SP200 - AT STAFRST, BUILD STATION SORTER RECORDS  *                           
*  AND MKT/STA,SIZE/STA AND REP/STA BINSRCH RECORDS *                           
*===================================================*                           
         SPACE 1                                                                
SP200    DS    0H                                                               
         L     R8,ADSTAT                                                        
         USING STAREC,R8                                                        
         CLI   QOPT4,C'Y'          DEACTIVATED ONLY                             
         BNE   SP201                                                            
         TM    SSYSDACT,X'FF'      YES - EXIT IF NOT DEACTIVATED                
         BNO   EXIT                                                             
         B     SP201A                                                           
*                                                                               
SP201    TM    SSYSDACT,X'FF'      NO - EXIT IF DEACTIVATED                     
         BO    EXIT                                                             
*                                                                               
SP201A   CLI   QOPT5,C'Y'          EIX=Y RECORDS ONLY?                          
         BNE   SP201B                                                           
         CLI   SEIXSTA,C'Y'        YES?                                         
         BNE   EXIT                NO !  THEN WE ARE EXITING                    
*                                                                               
SP201B   CLI   QOPT7,C'Y'          GROUPM REQUEST                               
         BNE   *+14                NO                                           
         CLC   STAKCLT,=C'000'     CLIENT/OFFICE SPECIFIC?                      
         BNE   EXIT                YES - DON'T WANT DUPS                        
         CLC   Q2AFLTR,SPACES                                                   
         BNH   SP202                                                            
         CLC   Q2AFLTR(3),SNETWRK  MATCH US NETWORK                             
         BE    SP202                                                            
         CLC   Q2AFLTR,SCANNTWK    MATCH CAN NET                                
         BE    SP202                                                            
         B     EXIT                                                             
SP202    XC    BUFREC,BUFREC                                                    
         MVC   BSMED,STAKMED       MEDIA                                        
         XC    WORK,WORK                                                        
         MVC   WORK(5),STAKCALL                                                 
*                                                                               
* MAKE SURE STATION IS EITHER ALL NUMERIC OR ALL ALPHA                          
* SINCE SF FCMSPACK BLOWS ON INVALID                                            
*                                                                               
         CLI   WORK,C'0'                                                        
         BL    SP203                                                            
         CLI   WORK,C'9'                                                        
         BH    EXIT                                                             
         CLI   WORK+1,C'0'                                                      
         BL    EXIT                                                             
         CLI   WORK+1,C'9'                                                      
         BH    EXIT                                                             
         CLI   WORK+2,C'0'                                                      
         BL    EXIT                                                             
         CLI   WORK+2,C'9'                                                      
         BH    EXIT                                                             
         CLI   WORK+3,C'0'                                                      
         BL    EXIT                                                             
         CLI   WORK+3,C'9'                                                      
         BH    EXIT                                                             
         B     SP204                                                            
*                                                                               
SP203    CLI   WORK,C'A'                                                        
         BL    EXIT                                                             
         CLI   WORK,C'Z'                                                        
         BH    EXIT                                                             
         CLI   WORK+1,C'A'                                                      
         BL    EXIT                                                             
         CLI   WORK+1,C'Z'                                                      
         BH    EXIT                                                             
         CLI   WORK+2,C'A'                                                      
         BL    EXIT                                                             
         CLI   WORK+2,C'Z'                                                      
         BH    EXIT                                                             
         CLI   WORK+3,C' '                                                      
         BE    SP204                                                            
         CLI   WORK+3,C'A'                                                      
         BL    EXIT                                                             
         CLI   WORK+3,C'Z'                                                      
         BH    EXIT                                                             
*                                                                               
SP204    DS    0H                                                               
         OC    SMKT,SMKT           PROTECT AGAINST X'00'                        
         BNZ   *+10                EXCLUDE FROM MKT/STA ANALYSIS                
         MVC   SMKT,=C'0000'                                                    
         GOTO1 MSPACK,DMCB,SMKT,WORK,BSMSTA                                     
                                                                                
         CLI   QOPT1,C'M'                                                       
         BE    SP205                                                            
         CLI   QOPT1,C'R'                                                       
         BE    SP206                                                            
         MVI   ERRCD,3                                                          
         MVI   SRTYPE,C'5'         TYPE IS STATION MASTER                       
         MVC   SRMED,STAKMED       MEDIA                                        
         MVC   SRSTA,STAKCALL      CALL LETTERS                                 
         MVI   SRSTYP,1            STATION MASTER TYPE                          
         MVC   SRSCLT,STAKCLT      CLIENT                                       
         BAS   RE,SPUTSORT         SET SRDATA AND PUT RECORD TO SORTER          
         CLI   STAKCLT,C'*'        TEST OFFICE LEVEL MASTER                     
         BE    SP205X                                                           
         EJECT                                                                  
SP205    DS    0H                                                               
         MVI   ERRCD,4                                                          
         MVI   BSTYPE,C'M'         MKT/STA RECORD                               
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
         CLI   QOPT2,C'Y'          TEST STATION BY SIZE ANALYSIS                
         BNE   SP205A                                                           
         MVI   BSTYPE,C'A'                                                      
         MVC   BSMED,STAKMED                                                    
         MVC   BSSIZE,SSIZE                                                     
         XC    BSSPARE,BSSPARE                                                  
         ICM   R0,3,BSMKT          SAVE MARKET                                  
         XC    BSMKT,BSMKT         CLEAR FOR SIZE RECORDS                       
         PUT   WORKFIL,BUFREC                                                   
         STCM  R0,3,BSMKT          AND RESTORE                                  
*                                                                               
SP205A   CLI   QOPT3,C'Y'          TEST STATION BY AFF ANALYSIS                 
         BNE   SP205X                                                           
         MVI   BSTYPE,C'B'                                                      
         MVC   BSMED,STAKMED                                                    
         MVC   BSAFF,SNETWRK                                                    
         ICM   R0,3,BSMKT          SAVE MARKET                                  
         XC    BSMKT,BSMKT         CLEAR FOR AFF RECORDS                        
         PUT   WORKFIL,BUFREC                                                   
         STCM  R0,3,BSMKT          AND RESTORE                                  
         SPACE 1                                                                
SP205X   CLI   QOPT1,C'M'                                                       
         BE    EXIT                                                             
SP206    DS    0H                                                               
         MVI   ERRCD,5                                                          
         MVI   BSTYPE,C'R'         REP/STA RECORD                               
         SPACE                                                                  
         LA    R0,3                                                             
         LA    R2,SREP                                                          
SP210    DS    0H                                                               
         OC    0(3,R2),0(R2)                                                    
         BZ    SP220                                                            
         CLC   0(3,R2),SPACES                                                   
         BE    SP220                                                            
         CLC   0(3,R2),=C'000'                                                  
         BE    SP220                                                            
         CLC   0(3,R2),=C'NCA'                                                  
         BE    SP220               TOO MANY OF THESE !                          
         MVC   BSREP(3),0(R2)                                                   
         SPACE                                                                  
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFFIO                             
SP220    DS    0H                                                               
         LA    R2,3(R2)                                                         
         BCT   R0,SP210                                                         
                                                                                
         B     EXIT                                                             
         DROP  R8                                                               
         SPACE 2                                                                
         EJECT                                                                  
* SP300 - AT PROCBUY, BUILD STATION ADDRESS SORTER RECORDS *                    
         SPACE 1                                                                
SP300    DS    0H                                                               
         L     R8,ADSTATAD                                                      
         USING ADDRREC,R8                                                       
         MVI   ERRCD,6                                                          
         MVI   SRTYPE,C'5'         TYPE IF STATION ADDRESS                      
         MVC   SRMED,ADDKMED       MEDIA                                        
         MVC   SRSTA,ADDKCALL      CALL LETTERS                                 
         MVI   SRSTYP,0            STATION ADDRESS TYPE                         
         BAS   RE,SPUTSORT         SET SRDATA AND PUT RECORD TO SORTER          
         B     EXIT                                                             
         DROP  R8                                                               
         SPACE 2                                                                
* SP400 - AT PROCGOAL, BUILD REP SORTER RECORDS *                               
         SPACE 1                                                                
SP400    DS    0H                                                               
         L     R8,ADREP                                                         
         USING REPREC,R8                                                        
         MVI   ERRCD,7                                                          
         MVI   SRTYPE,C'1'         TYPE IS REP                                  
         MVC   SRMED,REPKMED       MEDIA                                        
         MVC   SRREP,REPKREP       REP NUMBER                                   
         BAS   RE,SPUTSORT         SET SRDATA AND PUT RECORD TO SORTER          
         B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
* SP700 - AT MKTLAST, PRINT THE STATION FILE LISTING *                          
         SPACE 1                                                                
SP700    DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         OC    DM2,DM2             ANY MORE RECORDS?                            
         BZ    SP2000               NO.                                         
         MVI   SORTSTAT,C'G'                                                    
*        L     RF,DM2                                                           
*        MVC   SRREC,0(RF)                                                      
         L     R0,DM2              SET FROM ADDRESS                             
         LHI   R1,L'SRREC          SET FROM LEN                                 
         LA    RE,SRREC            SET TO ADDRESS                               
         LR    RF,R1               SET TO LENGTH                                
         MVCL  RE,R0                                                            
         MVC   OLDMED,SRMED        MEDIA                                        
         CLC   OLDTYPE,SRTYPE      IS THIS SAME TYPE OF RECORD?                 
         BE    SP710                YES.                                        
         SPACE 1                                                                
         MVC   OLDTYPE,SRTYPE       NO.                                         
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         CLI   OLDTYPE,C'1'        ARE THESE REP RECORDS?                       
         BE    SP710                YES.                                        
         MVI   RCSUBPRG,2                                                       
         CLI   OLDTYPE,C'3'        ARE THESE MARKET RECORDS?                    
         BE    SP710                YES.                                        
         MVI   RCSUBPRG,3           NO. MUST BE STATION RECORDS                 
SP710    DS    0H                                                               
         LA    R0,4                                                             
         LA    R2,RTNLIST                                                       
SP720    DS    0H                                                               
         CLC   0(1,R2),SRDATA      BRANCH TABLE SEARCH                          
         BE    SP730                                                            
         LA    R2,4(R2)                                                         
         BCT   R0,SP720                                                         
         DC    H'0'                ERROR - BAD RECORD TYPE                      
SP730    DS    0H                                                               
         XR    RF,RF                                                            
         ICM   RF,7,1(R2)                                                       
         A     RF,RELO                                                          
         AP    TOTREC,=P'1'                                                     
         BR    RF                                                               
         SPACE 2                                                                
* RECORD TYPE BRANCH TABLE *                                                    
         SPACE 1                                                                
         DS    0F                                                               
RTNLIST  DC    C'S'                                                             
         DC    AL3(SP1100)                                                      
         DC    C'A'                                                             
         DC    AL3(SP1000)                                                      
         DC    C'R'                                                             
         DC    AL3(SP800)                                                       
         DC    C'M'                                                             
         DC    AL3(SP900)                                                       
         EJECT                                                                  
* SP800 - REP RECORDS *                                                         
         SPACE 1                                                                
SP800    DS    0H                                                               
         AP    TOTREP,=P'1'                                                     
         L     R8,ADREP                                                         
         USING REPREC,R8                                                        
         LH    R1,SRDATA+15        GET L'RECORD                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   REPREC(0),SRDATA                                                 
         MVC   P+2(3),REPKREP      REP NUMBER                                   
         MVC   P+8(22),RNAME       REP NAME                                     
         MVC   P+34(24),R1LINE     STREET ADDRESS                               
         MVC   P+60(24),R2LINE     CITY                                         
         MVC   P+86(3),R3LINE      STATE                                        
         MVC   P+91(10),RBIGZIP    ZIP                                          
         SPACE 1                                                                
SP810    DS    0H                                                               
         MVI   ERRCD,9                                                          
         XC    BUFREC,BUFREC                                                    
         MVI   BSTYPE,C'R'                                                      
         MVC   BSMED,REPKMED       MEDIA                                        
         MVC   BSREP(3),REPKREP         REP NUMBER                              
         MVC   BUFGET,BUFREC                                                    
SP820    DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFFIO                            
         CLI   DM3,X'80'                                                        
         BE    NEXTREC              NO.                                         
         CLC   BUFGET(5),BUFREC    IS THIS RIGHT TYPE/MEDIA/REP?                
         BNE   NEXTREC              NO.                                         
         MVI   BSFORCE,X'FF'       FORCE READ HIGH FOR NEXT PASS                
         GOTO1 MSUNPK,DMCB,BSMSTA,P+104,P+109                                   
         CLI   P+108,C' '                                                       
         BE    SP830                                                            
         MVC   P+109(1),P+108                                                   
         MVI   P+108,C'-'                                                       
         CLI   P+109,C'N'                                                       
         BE    SP830                                                            
         CLI   P+109,C'X'                                                       
         BE    SP830                                                            
         MVI   P+110,C'M'                                                       
         CLI   P+109,C'T'                                                       
         BNE   SP830                                                            
         MVI   P+110,C'V'                                                       
SP830    DS    0H                                                               
         GOTO1 REPORT                                                           
         B     SP820                                                            
         DROP  R8                                                               
         EJECT                                                                  
* SP900 - MARKET RECORDS *                                                      
         SPACE 1                                                                
SP900    DS    0H                                                               
         AP    TOTMKT,=P'1'                                                     
         L     R8,ADMARKET                                                      
         USING MKTREC,R8                                                        
         LH    R1,SRDATA+15        GET L'RECORD                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MKTREC(0),SRDATA                                                 
         MVI   ERRCD,10                                                         
         MVC   P+2(4),MKTKMKT      MARKET NUMBER                                
         MVC   P+9(3),MKTRANK      MARKET RANK                                  
         MVC   P+15(24),MKTNAME    MARKET NAME                                  
         MVC   P+41(3),MKTALST     ALPHA MARKET CODE                            
         MVC   P+50(1),MKTZONE     TIME ZONE                                    
         MVC   P+93(3),MKTLTACC    LIMIT ACCESS CODE                            
         MVC   P+98(4),MKTWT       MARKET WEIGHTING FACTOR                      
         MVC   P+104(4),MKTSHR     MARKET SHARE                                 
         MVC   P+111(8),MKTHOMES   MARKET HOMES                                 
         MVC   P+121(3),MKTREG     REGION CODE                                  
         MVC   P+126(2),MKTNTA     NTA (1-29)                                   
         SPACE 1                                                                
         CLI   MKTRS1,0                                                         
         BNE   *+12                                                             
         CLI   MKTCLAS1,0                                                       
         BE    SP903                                                            
         SPACE 1                                                                
         MVC   FULL(3),MKTRS1                                                   
         MVC   FULL+3(1),MKTCLAS1                                               
         LA    R4,P2+16                                                         
         BAS   RE,FMTRS                                                         
         SPACE 1                                                                
         CLI   MKTRS2,0            IS THERE A SECOND RATING SERVICE             
         BE    SP903               NO                                           
         MVC   FULL(3),MKTRS2                                                   
         MVC   FULL+3(1),MKTCLAS2                                               
         BAS   RE,FMTRS                                                         
         AHI   R4,1                                                             
         B     SP903                                                            
         SPACE 1                                                                
* SUBROUTINE TO FORMAT RTG SVC MKT/SWEEP CLASS                                  
         SPACE 1                                                                
FMTRS    MVC   0(7,R4),=C'SWPCLS='                                              
         CLI   FULL,0              IS THERE A RATING SERVICE                    
         BE    FMTRS4                                                           
         MVC   0(4,R4),=C'NSI='                                                 
         CLI   FULL,C'0'                                                        
         BE    FMTRS2                                                           
         MVC   0(4,R4),=C'ARB='                                                 
         L     R6,ADAGY                                                         
         USING AGYHDRD,R6                                                       
         CLI   AGYPROF+7,C'C'      TEST CANADIAN                                
         BNE   FMTRS2                                                           
         MVC   0(4,R4),=C'BBM='                                                 
FMTRS2   SR    R0,R0               FORMAT RTG SVC MARKET                        
         ICM   R0,3,FULL+1                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R4),DUB                                                      
FMTRS4   MVI   7(R4),C'('          FORMAT SWEEP CLASS                           
         MVC   8(1,R4),FULL+3                                                   
         MVI   9(R4),C')'                                                       
         LA    R4,10(R4)                                                        
         BR    RE                                                               
         SPACE 1                                                                
*                                                                               
SP903    DS    0H                                                               
         OC    MKTALST,MKTALST     DISPLAY ALPHA MKTS IF ANY                    
         BZ    SP904                                                            
         MVC   0(5,R4),=C'MKTS='                                                
         L     R1,ADAGY                                                         
         CLI   AGYPROF-AGYHDRD+7(R1),C'C'   FOR CANADIAN AGY: ALPH=             
         BNE   *+10                                                             
         MVC   0(5,R4),=C'ALPH='                                                
         LA    R4,5(R4)                                                         
         LA    R6,MKTALST                                                       
         LA    R1,L'MKTALST(R6)                                                 
SP903A   MVC   0(3,R4),0(R6)                                                    
         CLI   2(R4),C' '                                                       
         BNE   *+6                                                              
         BCTR  R4,0                                                             
         LA    R4,3(R4)                                                         
         OC    3(3,R6),3(R6)                                                    
         BZ    SP904                                                            
         LA    R6,3(R6)                                                         
         CR    R6,R1                                                            
         BNL   SP904                                                            
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         B     SP903A                                                           
         SPACE 1                                                                
*                                                                               
SP904    DS    0H                                                               
         MVI   ERRCD,11                                                         
         XC    BUFREC,BUFREC                                                    
         MVI   BSTYPE,C'M'                                                      
         MVC   BSMED,MKTKMED       MEDIA                                        
         PACK  DUB,MKTKMKT         MARKET NUMBER                                
         CVB   RF,DUB                                                           
         STCM  RF,3,BSMKT                                                       
         MVC   BUFGET,BUFREC                                                    
SP905    DS    0H                                                               
         LA    R2,P+56                                                          
         LA    R4,14                                                            
SP910    DS    0H                                                               
         LA    R0,4                4 STATIONS/LINE                              
SP920    DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFFIO                            
         CLI   DM3,X'80'                                                        
         BE    SP940                                                            
         CLC   BUFGET,BUFREC       IS THIS SAME TYPE/MEDIA/REP/MKT?             
         BNE   SP940                NO.                                         
         MVI   BSFORCE,X'FF'       FORCE READ HIGH FOR NEXT PASS                
         SPACE                                                                  
         OC    BSCALL,BSCALL       IF BINARY ZEROS, BYPASS                      
         BZ    SP940                                                            
         SPACE                                                                  
         GOTO1 MSUNPK,DMCB,BSMSTA,FULL,0(R2)                                    
*******************************                                                 
         CLI   4(R2),C' '                                                       
         BE    SP930                                                            
         MVC   5(1,R2),4(R2)                                                    
         MVI   4(R2),C'-'                                                       
         CLI   5(R2),C'N'          IS THIS A NETWORK?                           
         BE    SP930                YES.                                        
         CLI   5(R2),C'X'                                                       
         BE    SP930                                                            
         MVI   6(R2),C'M'                                                       
         CLI   5(R2),C'T'                                                       
         BNE   SP930                                                            
         MVI   6(R2),C'V'                                                       
SP930    DS    0H                                                               
         LA    R2,9(R2)                                                         
         BCT   R0,SP920                                                         
         LA    R2,96(R2)                                                        
         BCT   R4,SP910                                                         
         GOTO1 REPORT                                                           
         B     SP905                                                            
         SPACE 1                                                                
SP940    DS    0H                                                               
         GOTO1 REPORT                                                           
         B     NEXTREC                                                          
         DROP  R8                                                               
         EJECT                                                                  
* SP1000 - STATION ADDRESS RECORD *                                             
         SPACE 1                                                                
SP1000   DS    0H                                                               
         AP    TOTADR,=P'1'                                                     
         L     R7,ADSTATAD                                                      
         USING ADDRREC,R7                                                       
         LH    R1,SRDATA+15        GET L'RECORD                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ADDRREC(0),SRDATA                                                
         B     SP700               GET THE NEXT RECORD                          
         SPACE 2                                                                
* SP1100 - STATION MASTER RECORDS *                                             
         SPACE 1                                                                
SP1100   DS    0H                                                               
         AP    TOTSTA,=P'1'                                                     
         L     R8,ADSTAT                                                        
         USING STAREC,R8                                                        
*                                                                               
         LA    R0,STAREC           CLEAR AREA BEFORE MOVE IN NEW REC            
         LHI   R1,L'STAREC                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R0,SRDATA           SET FROM ADDRESS                             
         SR    R1,R1                                                            
         ICM   R1,3,SRDATA+15      SET FROM LEN                                 
         LA    RE,STAREC           SET TO ADDRESS                               
         LR    RF,R1               SET TO LENGTH                                
         MVCL  RE,R0                                                            
*                                                                               
         MVC   P+2(4),STAKCALL     CALL LETTERS                                 
         MVI   P+6,C'-'                                                         
         MVC   P+7(1),STAKCALL+4                                                
         CLI   QOPT7,C'Y'          GROUPM REQUEST?                              
         BNE   SP1101              NO                                           
         MVI   P+13,C'N'           LOCK=N                                       
         TM    SFLAG1,SLOCK        LOCK=Y?                                      
         BZ    *+8                 NO                                           
         MVI   P+13,C'Y'           YES - LOCK=Y                                 
         B     SP1120              MKT NOT NEEDED                               
*                                                                               
SP1101   CLC   =CL3'000',STAKCLT   IS THIS A CLIENT-LINKED STATION?             
         BE    *+10                 NO.                                         
         MVC   P+9(3),STAKCLT      CLIENT                                       
         CLI   STAKCLT,C'*'        TEST OFFICE                                  
         BE    SP1120              YES - SKIP MARKET STUFF                      
         MVC   P+13(4),SMKT        MARKET NUMBER                                
         SPACE 1                                                                
         MVI   ERRCD,12                                                         
         XC    BINMKT,BINMKT                                                    
         MVC   BINNUM,SMKT                                                      
         OC    TMKT,TMKT           ARE THERE ANY MKT NAME ENTRIES?              
         BZ    SP1105               NO.                                         
         L     R3,TMKT                                                          
         GOTO1 BINSRCH,DMCB,(2,BINMKT),VMKTBUF,(R3),24,24,1000                  
         CLI   DM1,1                                                            
         BE    SP1105                                                           
         L     RF,DM1                                                           
         CLC   BINNUM,0(RF)                                                     
         BNE   SP1105                                                           
         MVC   P+18(20),4(RF)                                                   
         B     SP1120                                                           
         SPACE 1                                                                
SP1105   DS    0H                                                               
         L     R6,ADMARKET                                                      
         USING MKTREC,R6                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'            MARKET                                       
         MVC   KEY+1(1),STAKMED    MEDIA                                        
         MVC   KEY+2(4),SMKT       MARKET NUMBER                                
         MVC   KEY+6(2),QAGY       AGENCY                                       
         MVC   KEY+8(9),=9C'0'                                                  
         GOTO1 HIGHMKT                                                          
         CLC   MKTKEY,KEY          IS THIS THE CORRECT MARKET RECORD?           
         BE    SP1110               YES.                                        
         MVC   P+18(20),=CL22'** MARKET NOT FOUND **'                           
         AP    MKTERR,=P'1'                                                     
         B     SP1120                                                           
         SPACE 1                                                                
SP1110   DS    0H                                                               
         CLI   TMKT+3,50                                                        
         BL    *+10                                                             
         XC    TMKT,TMKT                                                        
         MVC   BINNAM,MKTNAME                                                   
         L     R3,TMKT                                                          
         GOTO1 BINSRCH,DMCB,(1,BINMKT),VMKTBUF,(R3),24,24,100                   
         MVC   TMKT,DM3                                                         
         MVC   P+18(20),MKTNAME                                                 
         DROP  R6                                                               
SP1120   DS    0H                                                               
         MVC   P+39(3),SPAYREP     PAYING REP                                   
*                                                                               
         CLI   STAKCLT,C'*'        TEST OFFICE RECORD                           
         BNE   SP1125                                                           
         MVI   P2,0                FORCE TO SKIP A LINE                         
         B     SP1260              YES - NO MORE TO PRINT                       
*                                                                               
SP1125   MVC   P+43(3),SCONREP     CONTRACT REP                                 
         MVC   P+47(3),STRFREP     TRAFFIC REP                                  
         MVC   P+51(1),STYPE       STATION TYPE                                 
         MVI   ERRCD,13                                                         
         CLC   ADDRREC+1(6),STAREC+1                                            
         BE    SP1130                                                           
         MVC   P+53(29),=CL29'** NO ADDRESS RECORD FOUND **'                    
         AP    ADRERR,=P'1'                                                     
         B     SP1140                                                           
SP1130   DS    0H                                                               
         MVC   P+53(20),ANAME      STATION NAME                                 
         MVC   P+74(24),A1LINE     STATION ADDRESS                              
         MVC   P+99(18),A2LINE     CITY                                         
         MVC   P+118(3),A3LINE     STATE                                        
         MVC   P+122(10),ABIGZIP   ZIP                                          
*                                                                               
SP1140   DS    0H                                                               
         XC    P2PROF,P2PROF       PROFILE OF P2 FOR DOWNLD OPTION              
         LA    R3,P2PROF                                                        
*                                                                               
         LA    R4,P2+39                                                         
         L     R1,ADAGY                                                         
         CLI   AGYPROF-AGYHDRD+7(R1),C'C'                                       
         BNE   SP1150                                                           
         MVI   1(R3),C'T'          TEXT FIELD WITH A LENGTH OF 0                
         CLC   SCANNTWK,=4C' '                                                  
         BNH   SP1145                                                           
         MVC   0(4,R4),=C'NET='                                                 
         MVC   4(4,R4),SCANNTWK    CANADIAN NETWORK                             
         LA    R4,9(R4)                                                         
         MVI   0(R3),8                                                          
*                                                                               
SP1145   LA    R3,2(R3)                                                         
         B     SP1160                                                           
*                                                                               
SP1150   MVI   1(R3),C'T'          TEXT FIELD WITH A LENGTH OF 0                
         CLC   SNETWRK,=3C' '                                                   
         BNH   SP1155                                                           
         MVC   0(4,R4),=C'NET='                                                 
         MVC   4(3,R4),SNETWRK     NETWORK                                      
         LA    R4,8(R4)                                                         
         MVI   0(R3),7                                                          
*                                                                               
SP1155   LA    R3,2(R3)                                                         
*                                                                               
SP1160   MVI   1(R3),C'T'          TEXT FIELD WITH A LENGTH OF 0                
         CLC   SCHNL,=4C'0'                                                     
         BE    SP1165                                                           
         OC    SCHNL,SCHNL                                                      
         BZ    SP1165                                                           
         MVC   0(3,R4),=C'CH='                                                  
         MVC   3(4,R4),SCHNL       CHANNEL                                      
         LA    R4,8(R4)                                                         
         MVI   0(R3),7                                                          
*                                                                               
SP1165   LA    R3,2(R3)                                                         
*                                                                               
SP1170   MVI   1(R3),C'T'          TEXT FIELD WITH A LENGTH OF 0                
         SR    R0,R0                                                            
         ICM   R0,3,SNEWTAX                                                     
         BZ    SP1171                                                           
         MVC   0(4,R4),=C'TAX='                                                 
         EDIT  (R0),(6,4(R4)),3,ALIGN=LEFT                                      
         AR    R4,R0                                                            
         LA    R4,5(R4)                                                         
         AHI   R0,4                                                             
         STC   R0,0(R3)                                                         
*                                                                               
SP1171   LA    R3,2(R3)                                                         
*                                                                               
SP1175   MVI   1(R3),C'T'          TEXT FIELD WITH A LENGTH OF 0                
         OC    STWIX,STWIX                                                      
         BZ    SP1176                                                           
         MVC   0(4,R4),=C'TWX='                                                 
         MVC   4(20,R4),STWIX      TWIX NUMBER                                  
         LA    R4,25(R4)                                                        
         MVI   0(R3),24                                                         
*                                                                               
SP1176   LA    R3,2(R3)                                                         
*                                                                               
SP1180   MVI   1(R3),C'T'          TEXT FIELD WITH A LENGTH OF 0                
         OC    SFAX,SFAX                                                        
         BZ    SP1185                                                           
         MVC   0(4,R4),=C'FAX='                                                 
         MVC   4(12,R4),SFAX       FAX NUMBER                                   
         LA    R4,17(R4)                                                        
         MVI   0(R3),16                                                         
*                                                                               
SP1185   LA    R3,2(R3)                                                         
*                                                                               
SP1190   MVI   1(R3),C'T'          TEXT FIELD WITH A LENGTH OF 0                
         OC    STIMEBK,STIMEBK                                                  
         BZ    SP1195                                                           
         MVC   0(5,R4),=C'TMBK='                                                
         ICM   R0,15,STIMEBK                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  5(9,R4),DUB                                                      
         LA    R4,15(R4)                                                        
         MVI   0(R3),14                                                         
         MVI   1(R3),C'N'                                                       
*                                                                               
SP1195   LA    R3,2(R3)                                                         
*                                                                               
SP1200   MVI   1(R3),C'T'          TEXT FIELD WITH A LENGTH OF 0                
         CLI   SSIZE,C' '                                                       
         BNH   SP1205                                                           
         MVC   0(3,R4),=C'SZ='                                                  
         MVC   3(1,R4),SSIZE                                                    
         LA    R4,5(R4)                                                         
         MVI   0(R3),4                                                          
*                                                                               
SP1205   LA    R3,2(R3)                                                         
*                                                                               
SP1210   MVI   1(R3),C'T'          TEXT FIELD WITH A LENGTH OF 0                
         CLI   SGSTCODE,0          GOODS AND SERVICE TAX                        
         BE    SP1215                                                           
         MVC   0(4,R4),=C'GST='                                                 
         MVC   4(1,R4),SGSTCODE                                                 
         LA    R4,6(R4)                                                         
         MVI   0(R3),5                                                          
*                                                                               
SP1215   LA    R3,2(R3)                                                         
*                                                                               
         L     R1,ADAGY                                                         
         CLI   AGYPROF-AGYHDRD+7(R1),C'C'   FOR CANADIAN AGY: CTRY=             
         BNE   SP1250                                                           
         MVI   1(R3),C'T'          TEXT FIELD WITH A LENGTH OF 0                
         CLI   SCOUNTRY,0                                                       
         BE    SP1216                                                           
         MVC   0(5,R4),=C'CTRY='                                                
         MVC   5(1,R4),SCOUNTRY                                                 
         LA    R4,7(R4)                                                         
         MVI   0(R3),6                                                          
*                                                                               
SP1216   LA    R3,2(R3)                                                         
*                                                                               
         MVI   1(R3),C'T'          TEXT FIELD WITH A LENGTH OF 0                
         CLI   SRS1CALL,0                                                       
         BE    SP1217                                                           
         MVC   0(4,R4),=C'NSI='                                                 
         MVC   4(4,R4),SRS1CALL                                                 
         LA    R4,9(R4)                                                         
         MVI   0(R3),8                                                          
*                                                                               
SP1217   LA    R3,2(R3)                                                         
*                                                                               
         MVI   1(R3),C'T'          TEXT FIELD WITH A LENGTH OF 0                
         CLI   SRS2CALL,0                                                       
         BE    SP1218                                                           
         MVC   0(4,R4),=C'BBM='                                                 
         MVC   4(4,R4),SRS2CALL                                                 
         LA    R4,9(R4)                                                         
         MVI   0(R3),8                                                          
*                                                                               
SP1218   LA    R3,2(R3)                                                         
*                                                                               
SP1220   MVI   1(R3),C'T'          TEXT FIELD WITH A LENGTH OF 0                
*                                                                               
         XR    R0,R0               FLAG THAT WE HAVEN'T USED P3                 
         OC    SPST,SPST           ANY PST?                                     
         BZ    SP1235              NO - JUST BUMP R3                            
*                                                                               
         LA    R1,P2+79            NEED 53 CHARS FOR PST                        
         CR    R4,R1               HAVE ENOUGH ROOM ON P2?                      
         BNH   SP1230              YES                                          
         LA    R4,P3+40            ELSE SKIP TO NEXT LINE                       
         LA    R0,1                FLAG THAT WE'RE USING P3                     
         MVI   0(R3),C'C'          INDICATE TO DOWNLD OPT TO PRINT              
         LA    R3,1(R3)            ON THIRD LINE                                
*                                                                               
SP1230   BAS   RE,DISPPST          DISPLAY PST                                  
*                                                                               
SP1235   LA    R3,2(R3)            BUMP R3                                      
*                                                                               
         LTR   R0,R0               DID WE USE P3?                               
         BZ    SP1250              NO                                           
         MVI   P3,0                                                             
         CLC   P3+1(131),SPACES                                                 
         BE    *+8                                                              
         MVI   P4,0                                                             
         B     SP1260                                                           
*                                                                               
SP1250   MVI   P2,0                                                             
         CLC   P2+1(131),SPACES                                                 
         BE    *+8                                                              
         MVI   P3,0                                                             
*                                                                               
SP1260   MVI   0(R3),X'FF'           INDICATE END OF P2 DATA                    
***                                                                             
* THE NEXT 4 LINES ARE IN CASE MORE DATA IS ADDED TO P2PROF IN THE              
* FUTURE. IT CURRENTLY ALLOWS UP TO 15 EXTRA FIELDS AND US HAS 8 AND            
* CANADA HAS 12                                                                 
***                                                                             
         LA    R1,P2PROF+L'P2PROF    P2PROF                                     
         CR    R3,R1                 DID WE BLOW PAST P2PROF?                   
         BNH   *+6                   NO                                         
         DC    H'0'                  YES - EXPAND P2PROF                        
*                                                                               
         CLI   QOPT6,C'D'            USER WANT A DOWNLOADABLE REPORT?           
         BNE   SP1270                NO                                         
         GOTO1 VDOWNLD,DMCB,(RA),P2PROF                                         
         B     SP1280                                                           
*                                                                               
SP1270   GOTO1 REPORT                                                           
*                                                                               
SP1280   B     SP700                                                            
         DROP  R7,R8                                                            
*                                                                               
* NEXTREC - SKIP LINE THEN GET NEXT RECORD *                                    
*                                                                               
NEXTREC  DS    0H                                                               
         MVI   P,0                                                              
         CLC   P+1(131),SPACES                                                  
         BE    *+8                                                              
         MVI   P2,0                                                             
         GOTO1 REPORT                                                           
         B     SP700                                                            
         EJECT                                                                  
*                                                                               
*        DISPLAY PST CODES                                                      
*                                                                               
         USING STAREC,R8                                                        
DISPPST  NTR1                                                                   
         LA    R2,PSTBLK                                                        
         USING PSTBLKD,R2                                                       
         XC    0(PSTLNQ,R2),0(R2)     CLEAR INTERFACE BLOCK                     
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,SPST                                                          
         ST    R1,PSTADIN          INPUT ADDRESS                                
         MVC   PSTOUT,SPACES                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
*                                                                               
         GOTO1 VPSTVAL,DMCB,(R2)                                                
         MVC   0(4,R4),=C'PST '                                                 
         MVC   4(49,R4),PSTOUT        OUTPUT                                    
         MVI   0(R3),53                                                         
         MVI   1(R3),C'T'          TEXT FIELD                                   
*                                                                               
DPX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*=====================================================*                         
* IF STATION BY SIZE OR AFFILIATE REQUESTED, READ DATA*                         
* FROM WORKFIL AND DO ANOTHER SORT                    *                         
*=====================================================*                         
         SPACE 1                                                                
SP2000   CLI   QOPT2,C'Y'                                                       
         BE    SP2002                                                           
         CLI   QOPT3,C'Y'                                                       
         BE    SP2002                                                           
         B     SP2010                                                           
*                                                                               
SP2002   DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'END'                                             
         CLOSE (WORKFIL)                                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OPEN  (WORKFIL,INPUT)                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VSORTER,DMCB,SORTCRD2,RECCARD2                                   
*                                                                               
SP2004   DS    0H                                                               
         GET   WORKFIL,BUFREC                                                   
         GOTO1 VSORTER,DMCB,=C'PUT',BUFREC                                      
         B     SP2004                                                           
*                                                                               
SP2006   CLOSE (WORKFIL)                                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SP2010   CLI   QOPT2,C'Y'          TEST STATION BY SIZE ANALYSIS                
         BNE   SP2200                                                           
         MVI   RCSUBPRG,5                                                       
         MVI   OLDMED,0                                                         
         MVI   OLDSIZE,X'FE'                                                    
*                                                                               
SP2012   DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         ICM   RE,15,4(R1)                                                      
         BZ    SP2050                                                           
         MVC   BUFREC,0(RE)                                                     
*                                                                               
         CLI   BSTYPE,C'A'         TEST STATION BY SIZE RECORD                  
         BNE   SP2050              NO                                           
*                                                                               
         CLC   OLDMED,BSMED        TEST SAME MEDIA                              
         BE    SP2030                                                           
         CLI   OLDSIZE,X'FE'       TEST FIRST TIME                              
         BE    SP2025                                                           
         GOTO1 REPORT              NO - PRINT PREVIOUS                          
*                                                                               
SP2025   MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         MVC   OLDMED,BSMED                                                     
*                                                                               
SP2030   CLC   OLDSIZE,BSSIZE      TEST SAME SIZE                               
         BE    SP2040                                                           
         CLI   OLDSIZE,X'FE'       TEST FIRST TIME                              
         BE    SP2035                                                           
         GOTO1 REPORT              NO - PRINT PREVIOUS                          
         MVI   P,0                                                              
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
SP2035   LA    R4,P+9              AND SET UP A NEW LINE                        
         LA    R5,17               17 STATIONS PER LINE                         
         SR    R6,R6               CLEAR LINE COUNTER                           
         MVC   P+4(1),BSSIZE       MOVE SIZE TO PRINT LINE                      
         MVC   OLDSIZE,BSSIZE      SAVE CURRENT SIZE                            
*                                                                               
SP2040   DS    0H                                                               
         GOTO1 MSUNPK,DMCB,BSMSTA,FULL,0(R4)                                    
**********************************8                                             
         MVC   5(1,R4),4(R4)                                                    
         CLI   5(R4),C' '                                                       
         BNE   *+8                                                              
         MVI   5(R4),C'T'                                                       
         MVI   4(R4),C'-'                                                       
         LA    R4,7(R4)                                                         
         BCT   R5,SP2012           DECREMENT REMAINING COUNT THIS LINE          
* NO MORE ROOM THIS LINE *                                                      
         LA    R6,1(R6)            BUMP LINE COUNTER                            
         CH    R6,=H'14'           HAVE WE REACHED MAX YET                      
         BL    SP2045               NO - RESET POINTERS                         
         MVI   OLDSIZE,X'FF'       AFTER 14 LINES, FORCE IT TO PRINT            
         B     SP2012                                                           
*                                                                               
SP2045   LR    R0,R6               ELSE SET TO PRINT ON NEXT LINE               
         MH    R0,=H'132'                                                       
         LA    R4,P+9                                                           
         AR    R4,R0               POINT TO FIRST PRINT POSN                    
         LA    R5,17                                                            
         B     SP2012                                                           
*                                                                               
SP2050   DS    0H                                                               
         GOTO1 REPORT              PRINT LAST TIME                              
         EJECT                                                                  
SP2200   CLI   QOPT3,C'Y'          TEST STATION BY AFF ANALYSIS                 
         BNE   SPTOTAL                                                          
         MVI   RCSUBPRG,6                                                       
         MVI   OLDMED,0                                                         
         MVI   OLDAFF,X'FE'                                                     
*                                                                               
         CLI   QOPT2,C'Y'          TEST STA BY SIZE PRECEDED                    
         BE    SP2215               YES - HAVE FIRST RECORD                     
*                                                                               
SP2210   DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         ICM   RE,15,4(R1)                                                      
         BZ    SP2250                                                           
         MVC   BUFREC,0(RE)                                                     
*                                                                               
SP2215   CLI   BSTYPE,C'B'                                                      
         BNE   SP2250                                                           
*                                                                               
         CLC   OLDMED,BSMED        TEST SAME MEDIA                              
         BE    SP2230                                                           
         CLI   OLDAFF,X'FE'        TEST FIRST TIME                              
         BE    SP2225                                                           
         GOTO1 REPORT              NO - PRINT PREVIOUS                          
*                                                                               
SP2225   MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         MVC   OLDMED,BSMED                                                     
*                                                                               
SP2230   CLC   OLDAFF,BSAFF        TEST SAME AFF                                
         BE    SP2240                                                           
         CLI   OLDAFF,X'FE'        TEST FIRST TIME                              
         BE    SP2235                                                           
         GOTO1 REPORT              NO - PRINT PREVIOUS                          
         MVI   P,0                                                              
         GOTO1 REPORT              SKIP A LINE                                  
*                                                                               
SP2235   LA    R4,P+9              AND SET UP A NEW LINE                        
         LA    R5,17               17 STATIONS PER LINE                         
         SR    R6,R6               CLEAR LINE COUNTER                           
         MVC   P+2(3),BSAFF        MOVE AFF TO PRINT LINE                       
         MVC   OLDAFF,BSAFF        SAVE CURRENT AFF                             
*                                                                               
SP2240   DS    0H                                                               
         GOTO1 MSUNPK,DMCB,BSMSTA,FULL,0(R4)                                    
****************************                                                    
         MVC   5(1,R4),4(R4)                                                    
         CLI   5(R4),C' '                                                       
         BNE   *+8                                                              
         MVI   5(R4),C'T'                                                       
         MVI   4(R4),C'-'                                                       
         LA    R4,7(R4)                                                         
         BCT   R5,SP2210           DECREMENT REMAINING COUNT THIS LINE          
* NO MORE ROOM THIS LINE *                                                      
         LA    R6,1(R6)            BUMP LINE COUNTER                            
         CH    R6,=H'14'           HAVE WE REACHED MAX YET                      
         BL    SP2245              NO - RESET POINTERS                          
         MVI   OLDAFF,X'FF'        AFTER 14 LINES, FORCE IT TO PRINT            
         B     SP2210                                                           
*                                                                               
SP2245   LR    R0,R6               ELSE SET TO PRINT ON NEXT LINE               
         MH    R0,=H'132'                                                       
         LA    R4,P+9                                                           
         AR    R4,R0               POINT TO FIRST PRINT POSN                    
         LA    R5,17                                                            
         B     SP2210                                                           
*                                                                               
SP2250   DS    0H                                                               
         GOTO1 REPORT              PRINT LAST TIME                              
         B     SPTOTAL                                                          
*                                                                               
         SPACE 2                                                                
* ERROR ROUTINE *                                                               
         SPACE 1                                                                
ERROR    DS    0H                                                               
         MVC   P+2(29),=C'ERROR - BINSRCH TABLE IS FULL'                        
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
* SPTOTAL - PRINT RECORD TOTALS *                                               
         SPACE 1                                                                
SPTOTAL  DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'END'                                             
*                                                                               
         CLI   QOPT6,C'D'    DOWNLOADABLE?                                      
         BE    SPT11         YES...DON'T PRINT THIS                             
*                                                                               
         MVC   OLDMED,QMED                                                      
         MVI   RCSUBPRG,4                                                       
         CP    TOTREC,=P'0'                                                     
         BE    EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         LA    R0,7                                                             
         LA    R2,CNTRS                                                         
         LA    R3,P+2                                                           
         LA    R4,CTRTITL                                                       
SPT10    DS    0H                                                               
         MVC   0(26,R3),0(R4)      COUNTER TITLE                                
         EDIT  (P4,0(R2)),(5,26(R3)),ZERO=BLANK                                 
         LA    R2,4(R2)                                                         
         LA    R3,132(R3)                                                       
         LA    R4,26(R4)                                                        
         BCT   R0,SPT10                                                         
         GOTO1 REPORT                                                           
SPT11    MVI   SORTSTAT,0                                                       
         B     EXIT                                                             
         SPACE 2                                                                
* SPUTSORT - SET SRDATA AND PUT THE RECORD TO SORTER *                          
         SPACE 1                                                                
SPUTSORT NTR1                                                                   
         LR    R0,R8               GET FROM ADDRESS                             
         SR    R1,R1                                                            
         ICM   R1,3,15(R8)         SET FROM LENGTH                              
*                                                                               
         LA    RE,SRDATA           SET TO ADDRESS                               
         LR    RF,R1               SET TO LENGTH                                
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SRREC                                       
         MVI   SORTSTAT,C'P'                                                    
         B     EXIT                                                             
         SPACE 2                                                                
* SP48HDHK - HEADHOOK ROUTINE *                                                 
         SPACE 1                                                                
         DS    0F                                                               
         USING *,RF                                                             
SP48HDHK NTR1                                                                   
         LM    R9,RC,SP48R9RC                                                   
         DROP  RF                                                               
         LA    R0,5                                                             
         LA    R2,MEDTITL                                                       
HD10     DS    0H                                                               
         CLC   OLDMED,0(R2)        IS THIS THE CORRECT MEDIA?                   
         BE    HD20                 YES.                                        
         LA    R2,14(R2)                                                        
         BCT   R0,HD10                                                          
         LA    R2,=CL11' *UNKNOWN*'                                             
         SPACE 1                                                                
HD20     DS    0H                                                               
         MVC   H1+2(13),1(R2)      MEDIA                                        
*                                                                               
         CLI   RCSUBPRG,3                                                       
         BNE   HD30                                                             
         CLC   Q2AFLTR,SPACES      TEST FILTER ON AFFILIATE                     
         BNH   HD30                                                             
         MVI   H4+74,C'-'                                                       
         MVC   H4+76(4),Q2AFLTR                                                 
         MVC   H4+81(15),=C'AFFILIATES ONLY'                                    
         GOTO1 SQUASHER,DMCB,H4,132                                             
         GOTO1 CENTER,DMCB,H4,132                                               
HD30     DS    0H                                                               
         CLI   QOPT4,C'Y'          DEACTIVATED ONLY                             
         BNE   HD40                                                             
         MVC   H3+58(16),=C'DEACTIVATED ONLY'                                   
HD40     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
* BINSRCH MARKET/STATION AND REP/STATION RECORD FORMATS *                       
         SPACE 1                                                                
BUFREC   DS    0CL11                                                            
BSTYPE   DS    CL1                 TYPE                                         
*                                   C'M' - MKT/STA                              
*                                   C'R' - REP/STA                              
BSMED    DS    CL1                 MEDIA                                        
BSREP    DS    CL3                 REP NUMBER - CHAR, WAS BINARY                
         ORG   BSREP                                                            
BSSIZE   DS    CL1                 SIZE                                         
BSSPARE  DS    CL2                                                              
         ORG   BSREP                                                            
BSAFF    DS    CL3                                                              
*                                                                               
BSMSTA   DS    0CL5                MARKET/STATION                               
BSMKT    DS    CL2                 MARKET NUMBER - BINARY                       
BSCALL   DS    CL3                 PACKED CALL LETTERS                          
BSFORCE  DS    CL1                                                              
         SPACE 2                                                                
* BINSRCH MARKET/MARKET NAME RECORD *                                           
         SPACE 1                                                                
BINMKT   DS    0CL24                                                            
BINNUM   DS    CL4                 MARKET                                       
BINNAM   DS    CL20                MARKET NAME                                  
         EJECT                                                                  
* SRREC - SORTER RECORD *                                                       
         SPACE 1                                                                
SORT     DC    C'SORT FIELDS=(1,11,BI,A),WORK=1 '                               
RECCARD  DC    C'RECORD TYPE=F,LENGTH=1024 '                                    
SORTCRD2 DC    C'SORT FIELDS=(1,11,BI,A),WORK=1 '                               
RECCARD2 DC    C'RECORD TYPE=F,LENGTH=24 '                                      
         DS    0F                                                               
*RREC    DS    0CL211                                                           
SRREC    DS    0CL363             MAX REC LEN (352,STATION) + L'SRKEY           
SRKEY    DS    0CL11                                                            
         SPACE 1                                                                
* COMMON KEY *                                                                  
         SPACE 1                                                                
SRTYPE   DS    C                   RECORD TYPE                                  
SRMED    DS    C                   MEDIA                                        
SRORG    DS    0C                                                               
         SPACE 1                                                                
* REP KEY *                                                                     
         SPACE 1                                                                
SRREP    DS    CL3                 REP                                          
         DS    CL6                 SPARE                                        
         SPACE 1                                                                
* MARKET KEY *                                                                  
         SPACE 1                                                                
         ORG   SRORG                                                            
SRMKT    DS    CL4                 MARKET                                       
         DS    CL5                 SPARE                                        
         SPACE 1                                                                
* STATION KEY *                                                                 
         SPACE 1                                                                
         ORG   SRORG                                                            
SRSTA    DS    CL5                 STATION CALL LETTERS                         
SRSTYP   DS    CL1                 STATION RECORD TYPE                          
*                                   X'00' - STATION ADDRESS                     
*                                   X'01' - STATION MASTER                      
SRSCLT   DS    CL2                 CLIENT                                       
         DS    CL1                 SPARE                                        
         SPACE 1                                                                
* DATA *                                                                        
         SPACE 1                                                                
SRDATA   DS    CL1024                                                           
         SPACE 2                                                                
         DS    0D                                                               
         DC    CL8'WORKFIL'                                                     
WORKFIL  DCB   DSORG=PS,EODAD=SP2006,MACRF=(GM,PM),RECFM=FB,           X        
               DDNAME=WORKFIL,BLKSIZE=7200,LRECL=24                             
         EJECT                                                                  
         DS    0F                                                               
VSORTER  DS    V                                                                
VMKTBUF  DS    V                   ADDRESS OF MARKET NAME BUFFER                
VDOWNLD  DS    A                   ADDRESS OF DOWNLOAD CODE                     
VPSTVAL  DS    A                   ADDRESS OF PSTVAL                            
TMKT     DS    F                                                                
RELO     DS    F                   RELOCATION FACTOR                            
SP48R9RC DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
CNTRS    DS    0F                                                               
TOTSTA   DS    PL4                 TOTAL STATION MASTER RECORDS                 
TOTADR   DS    PL4                 TOTAL STATION ADDRESS RECORDS                
TOTMKT   DS    PL4                 TOTAL MARKET RECORDS                         
TOTREP   DS    PL4                 TOTAL REP RECORDS                            
ADRERR   DS    PL4                 ADDRESS ERRORS                               
MKTERR   DS    PL4                 MARKET ERRORS                                
TOTREC   DS    PL4                                                              
BUFGET   DS    CL7                                                              
ERRCD    DS    C                   ERROR CODE                                   
OLDMED   DS    C                                                                
OLDTYPE  DS    C                                                                
OLDSIZE  DS    C                                                                
OLDAFF   DS    CL3                                                              
CANADA   DS    C                                                                
SORTSTAT DS    C                   SORTER STATUS                                
PSTBLK   DS    CL(PSTLNQ)                                                       
PSTOUT   DS    CL64                                                             
P2PROF   DS    CL30                PROFILE FOR SECOND PRINT LINE                
*                                                                               
MEDTITL  DS    0C                                                               
         DC    C'T'                                                             
         DC    CL13'SPOT TV'                                                    
         DC    C'R'                                                             
         DC    CL13'RADIO'                                                      
         DC    C'N'                                                             
         DC    CL13'NETWORK TV'                                                 
         DC    C'C'                                                             
         DC    CL13'COMBINED TV'                                                
         DC    C'X'                                                             
         DC    CL13'NETWORK RADIO'                                              
         SPACE 1                                                                
CTRTITL  DS    0C                                                               
         DC    CL26'STATION RECORDS...........'                                 
         DC    CL26'ADDRESS RECORDS...........'                                 
         DC    CL26'MARKET RECORDS............'                                 
         DC    CL26'REP ADDRESS RECORDS.......'                                 
         DC    CL26'MISSING ADDRESS RECORDS...'                                 
         DC    CL26'UNKNOWN MARKET CODES......'                                 
         DC    CL26'TOTAL RECORDS.............'                                 
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
         BUFF  LINES=10000,FLAVOR=DATA,KEYLIST=(11,A)                           
         SPACE 2                                                                
* MKTBUF - TEMPORARY MKT/MKT NAME BUFFER *                                      
         SPACE 1                                                                
MKTBUF   DS    24000C                                                           
         SPACE 2                                                                
*        PRINT OFF                                                              
         SPACE 1                                                                
***                                                                             
* DOWNLOAD CSECT GOES HERE                                                      
***                                                                             
DOWNLD   CSECT                                                                  
         NMOD1 0,DOWNLD                                                         
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,RC,RA                                                    
         LA    R8,SPACEND                                                       
         USING SP48WRKD,R8                                                      
         L     R3,4(R1)           P2PROF                                        
*                                                                               
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         OI    DLCBFLG1,DLCBFXTN                                                
         MVC   DLCXTND(7),MAXLINE                                               
         MVC   DLCBAPR,=A(DNPRINT)                                              
         LA    R0,P                                                             
         ST    R0,DLCBAPL                                                       
*                                                                               
         CLI   MODE,REQLAST       SEE IF END OF REPORT                          
         BE    DNP10                                                            
*                                                                               
         CLI   MODE,REQFRST       SEE IF I NEED TO INTIALIZE                    
         BE    DNP20                                                            
*                                                                               
         MVC   DNLINE,P           SAVE CONTENTS OF PRINTLINE                    
         MVC   P,SPACES                                                         
         MVC   DNLINE2,P2         SAVE CONTENTS OF PRINTLINE2                   
         MVC   P2,SPACES                                                        
         MVC   DNLINE3,P3         SAVE CONTENTS OF PRINTLINE2                   
         MVC   P3,SPACES                                                        
*                                                                               
         MVC   DLCBFLD(6),DNLINE+2       STATION CALL LETTERS                   
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   QOPT7,C'Y'                GROUPM REQUEST?                        
         BE    DNP01                     YES                                    
         MVC   DLCBFLD(3),DNLINE+9       CLIENT                                 
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(4),DNLINE+13      MARKET NUMBER                          
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(20),DNLINE+18     MARKET NAME                            
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP01    MVC   DLCBFLD(3),DNLINE+39      PAYING REP                             
         MVI   DLCBTYP,C'T'              TEST FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   QOPT7,C'Y'                GROUPM REQUEST?                        
         BE    DNP02                     YES                                    
         MVC   DLCBFLD(3),DNLINE+43      CONTRACT REP                           
         MVI   DLCBTYP,C'T'              NUMBER FIELD                           
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),DNLINE+47      TRAFFIC REP                            
         MVI   DLCBTYP,C'T'              NUMBER FIELD                           
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP02    MVC   DLCBFLD(1),DNLINE+51      STATION TYPE                           
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   DNLINE+53,C'*'            VALID ADDRESS?                         
         BNE   DNP02A                    YES                                    
         MVC   DLCBFLD(29),DNLINE+53     NO ADDRESS RECORD FOUND                
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
***      CLI   QOPT7,C'Y'                GROUPM REQUEST?                        
***      BNE   DNP02B                    NO                                     
*                                                                               
         LA    R2,4                      INSERT 4 BLANKS                        
                                                                                
DNP02AA  MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT           PUT BLANK TEXT FIELD SO                
         GOTO1 VDLFLD                    LOCK FIELD ALIGNS                      
         BCT   R2,DNP02AA                                                       
         B     DNP02B                                                           
*                                                                               
DNP02A   MVC   DLCBFLD(20),DNLINE+53     STATION NAME                           
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(24),DNLINE+74     STATION ADDRESS                        
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(18),DNLINE+99     CITY                                   
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),DNLINE+118     STATE                                  
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(10),DNLINE+122    ZIP                                    
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP02B   CLI   QOPT7,C'Y'                GROUPM REQUEST?                        
         BNE   DNP02C                    NO                                     
         MVC   DLCBFLD(5),=C'LOCK='      LOCK=                                  
         MVC   DLCBFLD+5(1),DNLINE+13    Y/N                                    
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVI   DLCBACT,DLCBEOL           SEND END OF LINE                       
         GOTO1 VDLFLD                                                           
         B     DNPX                      DONE FOR GROUPM                        
*                                                                               
DNP02C   OC    0(30,R3),0(R3)            HAVE MORE DATA TO PRINT ON P2          
         BZ    DNP05                     NO - DONE                              
                                                                                
         LA    R4,DNLINE2+39                                                    
         XR    R2,R2                     CLEAR R2                               
*                                                                               
DNP03    CLI   0(R3),X'FF'               END OF PRINTING?                       
         BE    DNP05                     YES                                    
         CLI   0(R3),C'C'                PRINT ON THE THIRD LINE?               
         BNE   DNP04                     NO                                     
         LA    R3,1(R3)                                                         
         LA    R4,DNLINE3+40                                                    
*                                                                               
DNP04    MVC   DLCBTYP(1),1(R3)          T/N                                    
         ICM   R2,1,0(R3)                HAVE ZERO LENGTH FIELD?                
         BZ    DNP04A                    YES - PRINT BLANK FIELD                
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R4)                                                 
*                                                                               
DNP04A   MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         LTR   R2,R2                     HAVE ZERO LENGTH FIELD?                
         BZ    *+8                       YES - PRINT DO NOT BUMP R4             
         LA    R4,2(R2,R4)                                                      
         LA    R3,2(R3)                                                         
         B     DNP03                                                            
*                                                                               
DNP05    MVI   DLCBACT,DLCBEOL           SEND END OF LINE                       
         GOTO1 VDLFLD                                                           
         B     DNPX                                                             
*                                                                               
DNP10    DS    0H                                                               
         MVC   P,SPACES                  JUST IN CASE                           
         MVI   DLCBACT,C'R'              SET END OF REPORT                      
         GOTO1 VDLFLD                                                           
         B     DNPX                                                             
*                                                                               
DNP20    DS    0H                                                               
         MVC   P,SPACES                  JUST IN CASE                           
         MVI   DLCBACT,C'I'              START AND INTIALIZE REPORT             
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),=C'STA'                                               
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   QOPT7,C'Y'                GROUPM REQUEST?                        
         BE    DNP25                     YES                                    
*                                                                               
         MVC   DLCBFLD(3),=C'CLT'                                               
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),=C'MKT'                                               
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(11),=C'MARKET NAME'                                      
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP25    MVC   DLCBFLD(3),=C'REP'                                               
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   QOPT7,C'Y'                GROUPM REQUEST?                        
         BE    DNP30                     YES                                    
*                                                                               
         MVC   DLCBFLD(12),=C'CONTRACT REP'                                     
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(11),=C'TRAFFIC REP'                                      
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP30    MVC   DLCBFLD(1),=C'T'                                                 
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(12),=C'STATION NAME'                                     
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(15),=C'STATION ADDRESS'                                  
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(4),=C'CITY'                                              
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(5),=C'STATE'                                             
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),=C'ZIP'                                               
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   QOPT7,C'Y'                GROUPM REQUEST?                        
         BNE   DNP40                     YES                                    
         MVC   DLCBFLD(4),=C'LOCK'                                              
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP50                     DONE FOR GROUPM                        
*                                                                               
DNP40    MVC   DLCBFLD(3),=C'NET'                                               
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(7),=C'CHANNEL'                                           
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),=C'TAX'                                               
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(4),=C'TWIX'                                              
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),=C'FAX'                                               
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(9),=C'TIME BANK'                                         
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(4),=C'SIZE'                                              
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),=C'GST'                                               
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         L     R6,ADAGY                  A(AGENCY RECORD)                       
         USING AGYHDRD,R6                AGENCY RECORD DSECT                    
         CLI   AGYPROF+7,C'C'            CANADIAN AGENCY?                       
         BNE   DNP50                     NO - DONE PRINTING HEADERS             
         DROP  R6                        DROP AGENCY RECORD USING               
*                                                                               
         MVC   DLCBFLD(7),=C'COUNTRY'                                           
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),=C'NSI'                                               
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),=C'BBM'                                               
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),=C'PST'                                               
         MVI   DLCBTYP,C'T'              TEXT FIELD                             
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP50    MVI   DLCBACT,DLCBEOL           SEND END OF LINE                       
         GOTO1 VDLFLD                                                           
*                                                                               
DNPX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
DNPRINT  NTR1                                                                   
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'N'                                                    
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
DLCB     DS    XL256                                                            
DNLINE   DS    CL132                                                            
DNLINE2  DS    CL132                                                            
DNLINE3  DS    CL132                                                            
         DS    0H                                                               
MAXLINE  DC    H'132'                                                           
DELIM    DC    C' '        FIELD DELIMITER                                      
EOTCHR   DC    C'"'        END OF TEXT FIELD DELIMITER                          
EOTALT   DC    C''''       END OF TEXT CHR ALTERNATE                            
EOLCHR   DC    X'5E'       END OF LINE CHAR - SEMI-COLON                        
EORCHR   DC    C':'        END OF REPORT CHR                                    
         DROP  R1,R8                                                            
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
*                                                                               
SP48WRKD DSECT                     USED BY ALL CSECTS IN PROGRAM                
VDLFLD   DS    A                                                                
*                                                                               
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
         ORG   Q2USER                                                           
Q2AFLTR  DS    CL4                 AFFILIATE FILTER                             
         ORG   QGRP                                                             
QOPT6    DS    CL1       67        OPTION 6                                     
QOPT7    DS    CL1       68        OPTION 7                                     
         ORG                                                                    
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
       ++INCLUDE SPGENADD                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE DDPSTBLK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048SPREP4802 06/02/16'                                      
         END                                                                    
