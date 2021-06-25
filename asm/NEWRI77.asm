*          DATA SET NEWRI77    AT LEVEL 041 AS OF 02/06/07                      
*PHASE T32077A,*                                                                
*INCLUDE NETACC                                                                 
*INCLUDE NETNET                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE BINSRCH2                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'T32077-KR TAPE'                                                 
T32077   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NEKR**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    RA,4095(RB)       RA=2ND BASE REGISTER                           
         LA    RA,1(RA)                                                         
         USING T32077,RB,RA                                                     
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4          R7-ANETWS4/WORKING STORAGE                   
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         L     R1,ANETWS1                                                       
         ST    R1,ACLISTSV         ANETWS1/CLISTSV                              
         LA    R1,HEADING          ANETWS2/ANETWS3=I/O AREAS                    
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*****    L     R1,BOOKVAL                                                       
*****    A     R1,RELO                                                          
*****    ST    R1,ANTWKTP                                                       
*                                                                               
*        GET A(AREA) PRESERVED BETWEEN REQUESTS                                 
*                                                                               
         L     R6,ATWA                                                          
         USING T320FFD,R6                                                       
*                                                                               
*        GET A(DYNALLOC)                                                        
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFFLINE                          
         BNE   INITTABX                                                         
*                                                                               
         ICM   RF,15,TWADCONS           FROM DDGENTWA                           
         BZ    INIT10                 NO ADDRESS                                
*                                                                               
         MVC   VDYNALLO,TDYNALLO-TWADCOND(RF)   V(DYNALLOC)                     
*                                                                               
*        ALTER FILE NAMES FOR AGENCIES                                          
*                                                                               
         MVC   DSNKRINV+13(2),TWAAGY                                            
         MVC   DSNKREST+13(2),TWAAGY                                            
*                                                                               
         L     R1,TWADCONS                                                      
         USING TWADCOND,R1                                                      
*                                                                               
         L     RF,TSPFUSER                                                      
         ST    RF,ASPFUSER                                                      
*                                                                               
         DROP  R1                                                               
*                                                                               
         L     R1,ASPFUSER                                                      
*                                                                               
*        DETERMINE ADDRESSES OF DCBS STORED IN PRESERVED AREA                   
*                                                                               
         LA    RF,KRINVFL-SPFAREA(R1)                                           
         ST    RF,AKRINVFL          NEW DCB ADDRESS                             
         LA    RF,KRESTFL-SPFAREA(R1)                                           
         ST    RF,AKRESTFL          NEW DCB ADDRESS                             
*                                                                               
INIT10   DS    0H                                                               
*                                                                               
*        INITIALIZE TABLE AREAS                                                 
*                                                                               
         L     R1,=A(PRDTABC)                                                   
*                                                                               
         MVC   0(32,R1),=CL32'** NEWRI77  PRD TABLE    ***'                     
         LA    R1,32(R1)                                                        
         ST    R1,APRDTAB                                                       
*                                                                               
         L     R1,=A(ESTTABC)       POINT TO ESTIMATE TABLE                     
         MVC   0(32,R1),=CL32'** NEWRI77  EST TABLE    ***'                     
         LA    R1,32(R1)                                                        
         ST    R1,AESTTAB                                                       
*                                                                               
INITTABX DS    0H                                                               
*                                                                               
*        ANALYZE MODE                                                           
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
         CLI   MODE,RUNLAST                                                     
         BE    LASTRUN                                                          
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - LASTRUN'                        
***********************************************************************         
*                                                                     *         
*        LAST OF RUN                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LASTRUN  DS    0H                                                               
*                                                                               
         CLI   OFFLINE,C'Y'        SKIP IF NOT OFFLINE                          
         BNE   LASTRUNX                                                         
*                                                                               
         L     RE,ASPFUSER         RESTORE SAVED VALUES                         
         LA    RF,SPFAREAL                                                      
         LA    R0,SPFAREA                                                       
*                                                                               
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*        FIND DESTINATION ID FOR EDICT TRANSMISSION                             
*                                                                               
         LA    R1,AIDTAB           POINT TO DESTINATION TABLE                   
*                                                                               
LSTAIDLP DS    0H                                                               
*                                                                               
         CLI   0(R1),X'FF'         CHECK FOR END OF TABLE                       
         BNE   *+6                                                              
         DC    H'0'                MUST FIND AN ENTRY IN THE TABLE              
*                                                                               
         CLC   TWAAGY,0(R1)        MATCH F * FIRST 1HA                          
         BE    LSTAIDFD                                                         
*                                                                               
LSTAIDCN DS    0H                                                               
         LA    R1,AIDTABLQ(R1)     BUMP TO NEXT ENTRY IN TABLE                  
         B     LSTAIDLP                                                         
*                                                                               
LSTAIDFD DS    0H                                                               
*                                                                               
         MVC   AGYID,2(R1)         SAVE AGENCY ID                               
*                                                                               
*        PUT OUT INVOICE BATCH CONTROL RECORD AND CLOSE TAPE                    
*                                                                               
         BRAS  RE,INVTOT                                                        
*                                                                               
         CLI   SPLTAPE,C'Y'       SKIP IF NOT PRODUCING TAPE                    
         BE    *+8                                                              
         CLI   SPLTAPE,C'T'       SKIP IF NOT PRODUCING TAPE                    
         BNE   LRINVTPX                                                         
*                                                                               
         CLI   SPINVSW,C'O'        SKIP IF INVOICE FILE NEVER OPENED            
         BNE   LRINVTPX                                                         
*                                                                               
         L     R3,AKRINVFL         POINT TO OUTPUT TAPE DCBS                    
         CLOSE ((R3),)             CLOSE TAPE                                   
*                                                                               
         CLI   SPLTAPE,C'Y'       SKIP IF NOT USING EDICT                       
         BNE   LRINVTPX                                                         
*                                                                               
         MVC   P,SPACES            CLOSE REPORT                                 
         GOTO1 VPRINT,DMCB,P-1,=C'CLOSE'  CLOSE REPORT                          
*                                                                               
         BRAS  RE,EDIINV           PUT OUT EDICT NOTICE                         
*                                                                               
LRINVTPX DS    0H                                                               
*                                                                               
*        PUT OUT ESTIMATE BATCH CONTROL RECORD AND CLOSE TAPE                   
*                                                                               
         BRAS  RE,ESTTOT           DO ESTIMATE TOTALS                           
*                                                                               
         CLI   SPLTAPE,C'Y'       SKIP IF NOT PRODUCING TAPE                    
         BE    *+8                                                              
         CLI   SPLTAPE,C'T'       SKIP IF NOT PRODUCING TAPE                    
         BNE   LRESTTPX                                                         
*                                                                               
         CLI   SPESTSW,C'O'        SKIP IF ESTIMATE FILE NEVER OPENED           
         BNE   LRESTTPX                                                         
*                                                                               
         L     R3,AKRESTFL         POINT TO OUTPUT TAPE DCBS                    
         CLOSE ((R3),)             CLOSE TAPE                                   
*                                                                               
         CLI   SPLTAPE,C'Y'       SKIP IF NOT USING EDICT                       
         BNE   LRESTTPX                                                         
*                                                                               
         MVC   P,SPACES            CLOSE REPORT                                 
         GOTO1 VPRINT,DMCB,P-1,=C'CLOSE'  CLOSE REPORT                          
*                                                                               
         BRAS  RE,EDIEST           PUT OUT EDICT NOTICE                         
*                                                                               
LRESTTPX DS    0H                                                               
*                                                                               
LRESTX   DS    0H                                                               
*                                                                               
LRINVX   DS    0H                                                               
*                                                                               
LASTRUNX XIT1                                                                   
*                                                                               
*        AGENCY ID TABLE                                                        
*                                                                               
*        DS    C'2 CH ALPHA',CL2'AGYID'                                         
*                                                                               
AIDTAB   DS    0D                                                               
         DC    C'DU',C'MV'         MEDIAVEST                                    
AIDTABLQ EQU   *-AIDTAB            LENGTH OF TABLE ENTRY                        
         DC    C'YN',C'BR'         BRAVO                                        
         DC    X'FF'               END OF TABLE                                 
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - PR'                             
***********************************************************************         
*                                                                     *         
*        PRINT REPORT                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PR       DS    0H                                                               
*                                                                               
*        ON START OF RUN MOVE DCBS TO SPFUSER AREA                              
*                                                                               
         L     R6,ATWA                                                          
         USING T320FFD,R6                                                       
*                                                                               
         CLI   TWAFIRST,0          IF FIRST REQUEST                             
         BNE   PRINIT1                                                          
*                                                                               
         MVC   SPFSPLID,SPOOLID                                                 
*                                                                               
         MVI   FRSTLAST,C'Y'       REQUEST RUNFRST/RUNLAST                      
         MVI   TWAFIRST,2          GET RUNLAST INDICATOR                        
*                                                                               
         CLI   SPLTAPE,C'Y'       SKIP IF NOT PRODUCING TAPE                    
         BE    *+8                                                              
         CLI   SPLTAPE,C'T'       SKIP IF NOT PRODUCING TAPE                    
         BNE   PRINIT10                                                         
*                                                                               
         GOTO1 VDYNALLO,DMCB,DDKRINV,DSNKRINV  ALLOCATE DATASET                 
*                                                                               
         LINKX MF=(E,PARMLST1),SF=(E,LINKLST)   GET GENERATION NAME             
*                                                                               
         GOTO1 VDYNALLO,DMCB,DDKREST,DSNKREST  ALLOCATE DATASET                 
*                                                                               
         LINKX MF=(E,PARMLST2),SF=(E,LINKLST)   GET GENERATION NAME             
*                                                                               
PRINIT10 DS    0H                                                               
*                                                                               
*        INVOICE FILE INITIALISATION                                            
*                                                                               
         MVI   SPINVSW,C'C'        INVOICE  TAPE NOT OPEN                       
         MVI   SPIHEAD,C'H'        BATCH HEADER RECORD NEEDED                   
         ZAP   SPIBTCTR,=P'0'      YES-INITIALIZE TAPE COUNT                    
*                                                                               
*        CREATE INVOICE BATCH ID OF DATE AND TIME                               
*                                                                               
         TIME  DEC                 R0 - TIME PUS 0HHMMSS.SS                     
*                                  R1 - DATE  P   0CYYDDDF                      
         ST    R1,FULL             DATE                                         
         OI    FULL+3,X'0F'        FORCE SIGN                                   
         UNPK  SPIVBTID(5),FULL+1(3)    DISPLAY DATE                            
         MVI   SPIVBTID+05,C' '       CLEAR EXTRA BYTE                          
*                                    RETURNED IN R0                             
         ZAP   DUB,=P'0'           INIT PACKED FIELD                            
         STCM  R0,15,DUB+3         SAVE P(HHMMSSSS0C)                           
*                                                                               
         AP    DUB,=P'60000000'   ADD SIX HOURS                                 
*                                                                               
         UNPK  SPIVBTID+5(9),DUB+3(5)     PRINT TIME                            
         MVI   SPIVBTID+13,C' '       CLEAR EXTRA BYTE                          
*                                                                               
         OC    SPIVBTID,SPACES        SPACE FILL                                
*                                                                               
         ZAP   SPIVNET,=P'0'       INIT MONEY ACCUMULATORS                      
         ZAP   SPIVCOM,=P'0'                                                    
         ZAP   SPIVADJ,=P'0'                                                    
         ZAP   SPIVGRS,=P'0'                                                    
         ZAP   SPIVBLL,=P'0'                                                    
*                                                                               
*        ESTIMATE FILE INITIALISATION                                           
*                                                                               
         MVI   SPESTSW,C'C'        ESTIMATE FILE NOT OPEN                       
         MVI   SPEHEAD,C'H'        BATCH HEADER TO BE WRITTEN                   
         ZAP   SPEBTCTR,=P'0'      INITIALIZE TAPE COUNT                        
*                                                                               
*        CREATE ESTIMATE BATCH ID OF DATE AND TIME                              
*                                                                               
         TIME  DEC                 R0 - TIME PUS 0HHMMSS.SS                     
*                                  R1 - DATE  P   0CYYDDDF                      
         ST    R1,FULL             DATE                                         
         OI    FULL+3,X'0F'        FORCE SIGN                                   
         UNPK  SPESBTID(5),FULL+1(3)    DISPLAY DATE                            
         MVI   SPESBTID+05,C' '       CLEAR EXTRA BYTE                          
*                                    RETURNED IN R0                             
         ZAP   DUB,=P'0'           INIT PACKED FIELD                            
         STCM  R0,15,DUB+3         SAVE P(HHMMSSSS0C)                           
*                                                                               
         AP    DUB,=P'60000000'   ADD SIX HOURS                                 
         AP    DUB,=P'00000010'   ADD ONE TO FORCE NEW ID                       
*                                                                               
         UNPK  SPESBTID+5(9),DUB+3(5)     PRINT TIME                            
         MVI   SPESBTID+13,C' '       CLEAR EXTRA BYTE                          
*                                                                               
         OC    SPESBTID,SPACES        SPACE FILL                                
*                                                                               
         ZAP   SPESNET,=P'0'       INIT MONEY ACCUMULATORS                      
         ZAP   SPESCOM,=P'0'                                                    
         ZAP   SPESADJ,=P'0'                                                    
         ZAP   SPESGRS,=P'0'                                                    
         ZAP   SPESBLL,=P'0'                                                    
*                                                                               
         L     R0,ASPFUSER         SAVE INTER-REQUEST VALUES                    
         LA    R1,SAVVALSL         AND DCBS                                     
         LA    RE,SPFAREA                                                       
         LR    RF,R1                                                            
*                                                                               
         MVCL  R0,RE                                                            
*                                                                               
         B     PRINIT2                                                          
*                                                                               
PRINIT1  DS    0H                                                               
*                                                                               
         L     RE,ASPFUSER         RESTORE SAVED VALUES                         
         LA    RF,SPFAREAL                                                      
         LA    R0,SPFAREA                                                       
         LR    R1,RF                                                            
*                                                                               
         MVCL  R0,RE                                                            
*                                                                               
PRINIT2  DS    0H                                                               
*                                                                               
         L     RE,APRDTAB          INIT  PRODUCT AND EST TABLES                 
         XC    0(PRTENTL,RE),0(RE)                                              
*                                                                               
         L     RE,AESTTAB                                                       
         XC    0(ETBENTL,RE),0(RE)                                              
*                                                                               
         MVI   IVREC,C' '          INIT INVOICE  RECORD                         
         MVC   IVREC+1(255),IVREC                                               
*                                                                               
         MVI   ESREC,C' '          INIT ESTIMATE RECORD                         
         MVC   ESREC+1(255),ESREC                                               
*                                                                               
*        CLEAR REQUEST TOTALS                                                   
*                                                                               
         ZAP   WESNETP,=P'0'       NET                                          
         ZAP   WESCOMP,=P'0'       COMPENSATION                                 
         ZAP   WESADJP,=P'0'       ADJUSTMENT                                   
         ZAP   WESGRSP,=P'0'       GROSS                                        
         ZAP   WESBLLP,=P'0'       PREVIOUS BILLING                             
*                                                                               
         ZAP   WIVNETP,=P'0'       NET                                          
         ZAP   WIVCOMP,=P'0'       COMPENSATION                                 
         ZAP   WIVADJP,=P'0'       ADJUSTMENT                                   
         ZAP   WIVGRSP,=P'0'       GROSS                                        
         ZAP   WIVBLLP,=P'0'       PREVIOUS BILLING                             
*                                                                               
         XC    DMCB,DMCB                                                        
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD       INIT SORTER                   
*                                                                               
         B     PR5                                                              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,69,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=385'                                   
*                                                                               
PR5      DS    0H                                                               
*                                                                               
         BAS   RE,GTMONLST               GET MONTHLIST                          
*                                                                               
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'P'                 X'94' KEY/ESTIMATE HIGH               
         MVI   NBSELUOP,C'A'              ACTUAL SCHEDULE                       
         MVI   NBSPLOPT,X'C0'             DO SPLIT 30'S                         
*                                                                               
*        CALL NEWRIIO FOR FIRST/NEXT RECORD                                     
*                                                                               
PRIOLOOP DS    0H                                                               
*                                                                               
         NETGO NSNETIO,DMCB,NETBLOCK                                            
*                                                                               
         CLI   NBERROR,0          NO ERRORS TOLERATED                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        ANALYZE NEWRIIO CALLING MODE                                           
*                                                                               
         CLI   NBMODE,NBVALCLI    PROCESS NEW CLIENT                            
         BE    PRIOCLI                                                          
*                                                                               
         CLI   NBMODE,NBPROCUN    PROCESSING UNIT RECORD                        
         BE    PRIOUN                                                           
*                                                                               
         CLI   NBMODE,NBREQLST                                                  
         BE    PRREQLST                                                         
*                                                                               
         B     PRIOCONT                                                         
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - PRIOCLI'                        
***********************************************************************         
*                                                                     *         
*        PROCESS NEW CLIENT                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRIOCLI  DS    0H                 NEW CLIENT                                    
*                                                                               
         L     RF,ACLISTSV           SAVE CLIENT'S PRODUCT LIST                 
         L     RE,NBAIO                                                         
*                                                                               
         USING CKEY,RE               ESTABLISH CLIENT RECORD                    
*                                                                               
         LA    RE,CLIST                                                         
         LA    R1,880                                                           
         MOVE  ((RF),(R1)),(RE)                                                 
         L     RF,ACLISTSV                                                      
         A     RF,=F'880'                                                       
         LA    RE,CLIST2                                                        
         LA    R1,140                                                           
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
         B     PRIOCONT                                                         
*                                                                               
         DROP  RE                                                               
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - PRIOUN'                         
***********************************************************************         
*                                                                     *         
*        PROCESS UNIT RECORD                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRIOUN   DS    0H                                                               
*                                                                               
         CLC   CURCLT,NBACTCLI          ..IF CLIENT HAS CHANGED                 
         BNE   PRIOCLNX                                                         
*                                                                               
         CLC   CUREST,NBACTEST          ..OR EST HAS CHANGED                    
         BNE   PRIOESNX                                                         
*                                                                               
         CLC   CURPRD,NBSPLPRN          ... IF PRD HAS CHANGED                  
         BNE   PRIOPRNX                                                         
*                                                                               
         B     PRIOSAME                                                         
*                                                                               
*        CLIENT OR ESTIMATE HAS CHANGED                                         
*                                                                               
PRIOCLNX DS    0H                                                               
*                                                                               
*        CLEAN UP PREVIOUS CLIENT                                               
*                                                                               
         OC    CURCLT,CURCLT       SKIP IF FIRST TIME                           
         BZ    *+8                                                              
         BAS   RE,DOBILREC              ..ADD BILL RECS TO SORT                 
*                                                                               
*        SET TO START NEW CLIENT                                                
*                                                                               
         MVC   CURCLT,NBACTCLI     SET NEW CLIENT/ESTIMATE                      
*                                                                               
*        NEW ESTIMATE                                                           
*                                                                               
PRIOESNX DS    0H                                                               
*                                                                               
         MVC   CUREST,NBACTEST     SET NEW ESTIMATE                             
*                                                                               
         BRAS  RE,GETEST           GET ENTRY IN KR EST TABLE                    
*                                                                               
******   BAS   RE,CLRCOMTB         CLEAR COMMIS TABLE                           
*                                                                               
*        PRODUCT CHANGED                                                        
*                                                                               
PRIOPRNX DS    0H                                                               
*                                                                               
         MVC   CURPRD,NBSPLPRN          ...  GET NEW PROD                       
*                                                                               
         BAS   RE,GTPROD           GET 3 CHARACTER PROD CODE                    
*                                                                               
         BRAS  RE,GETPRD           GET ENTRY IN KR PRD TABLE                    
*                                                                               
PRIOSAME DS    0H                                                               
*                                                                               
         BRAS  RE,DOUNIT                   PROCESS UNIT REC                     
*                                                                               
PRIOCONT DS    0H                                                               
*                                                                               
         B     PRIOLOOP                    GET NEXT UNIT                        
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - PRREQLST'                       
***********************************************************************         
*                                                                     *         
*        END OF REQUEST                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRREQLST DS    0H                 END OF REQUEST                                
*                                                                               
*        ADD BILL RECORDS TO SORT                                               
*                                                                               
         BAS   RE,DOBILREC              ..ADD BILL RECS TO SORT                 
*                                                                               
*        RETRIEVE RECORDS FROM SORT                                             
*                                                                               
         MVI   RIDSAVE,0           INIT RECORD ID SAVEAREA                      
*                                                                               
         BRAS  RE,INIESTID         INIT ESTID TABLE                             
*                                                                               
         XC    IVHDR,IVHDR         INIT RECORD KEY                              
         XC    ESHDR,ESHDR         INIT RECORD KEY                              
*                                                                               
*        GET RECORDS FROM SORT                                                  
*                                                                               
PRSGETLP DS    0H                                                               
*                                                                               
         XC    SORTREC,SORTREC     INIT SORT RECORD                             
         MVI   SORTRID,0                                                        
*                                                                               
         GOTO1 SORTER,DMCB,=C'GET'       TAKE RECS FROM SORTER                  
*                                                                               
         ICM   R3,15,4(R1)         POINT TO SORTED RECORD                       
         BZ    PRSGETDN               DONE AT END OF SORTED RECORDS             
*                                                                               
         MVC   SORTRID,0(R3)       GET RECORD IDENTIFIER                        
         MVC   SORTREC,1(R3)       SAVE RECORD                                  
         MVC   SORTPACK,257(R3)    PACKED FIELDS                                
*                                                                               
         CLC   RIDSAVE,SORTRID     ON CHANGE IN RECORD ID                       
         BE    PRSGET10                                                         
*                                                                               
         CLI   RIDSAVE,C'1'           IF DOING INVOICES                         
         BNE   *+12                                                             
         BRAS  RE,INVDTL                 DO LAST INVOICE DETAIL                 
         BRAS  RE,INVPTOT                DO INVOICE TOTALS                      
*                                                                               
         CLI   RIDSAVE,C'2'           IF DOING ESTIMATES                        
         BNE   *+12                                                             
         BRAS  RE,ESTDTL                 DO LAST ESTIMATE DETAIL                
         BRAS  RE,ESTPTOT                DO ESTIMATE TOTALS                     
*                                                                               
         MVI   FORCEHED,C'Y'          FORCE NEW PAGE                            
         MVC   RIDSAVE,SORTRID        SAVE RECORD ID                            
*                                                                               
PRSGET10 DS    0H                                                               
*                                                                               
         CLI   SORTRID,C'1'         HANDLE INVOICE  DETAILS                     
         BNE   *+12                                                             
         BRAS  RE,INVDTL                                                        
         B     PRSGETCN                                                         
*                                                                               
         CLI   SORTRID,C'2'        HANDLE ESTIMATE DETAILS                      
         BNE   *+12                                                             
         BRAS  RE,ESTDTL                                                        
         B     PRSGETCN                                                         
*                                                                               
PRSGETCN DS    0H                                                               
*                                                                               
         B     PRSGETLP            GET NEXT SORTED RECORD                       
*                                                                               
*        END OF SORT                                                            
*                                                                               
PRSGETDN DS    0H                                                               
*                                                                               
         CLI   RIDSAVE,C'1'         HANDLE LAST INVOICE  DETAILS                
         BNE   *+12                                                             
         BRAS  RE,INVDTL                                                        
         BRAS  RE,INVPTOT                                                       
*                                                                               
         CLI   RIDSAVE,C'2'        HANDLE LAST ESTIMATE DETAILS                 
         BNE   *+12                                                             
         BRAS  RE,ESTDTL                                                        
         BRAS  RE,ESTPTOT                                                       
*                                                                               
         GOTO1 SORTER,DMCB,=C'END'  SHUT DOWN THE SORT                          
*                                                                               
         L     R0,ASPFUSER         SAVE INTER-REQUEST VALUES                    
         LA    R1,SPFAREAL         AND DCBS                                     
         LA    RE,SPFAREA                                                       
         LR    RF,R1                                                            
*                                                                               
         MVCL  R0,RE                                                            
*                                                                               
PRSGETX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - DOBILREC'                       
***********************************************************************         
*                                                                     *         
*        DO BILL RECORDS                                              *         
*        FILL IN INVOICE CODE AND DOLLAR BUCKETS                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DOBILREC NTR1  LABEL=*                                                          
*                                                                               
*        BUILD SKELETON BILLREC KEY                                             
*              EVERYTHING BUT MONTH OF SERVICE                                  
*                                                                               
         LA    R5,KEY              ESTABLISH BILL RECORD KEY                    
         USING BILLREC,R5                                                       
*                                                                               
         XC    BKEY,BKEY           BUILD START OF BILLREC KEY                   
*                                                                               
         MVC   BKEYAM,NBACTAM      AGENCY/MEDIA                                 
         MVC   BKEYCLT,CURCLT      CLIENT                                       
*                                                                               
* - CONVERT REQUESTED YYMM TO 1 BYTE Y/M BILLING REC FORMAT                     
*                                                                               
         MVC   WKEYMBIL,BILLMON+1  SET X'0N' BILL MONTH                         
         PACK  WORK(2),BDAT(2)     SWITCHES LAST DIGIT OF YY                    
         MVZ   WKEYMBIL,WORK+1                                                  
*                                                                               
*        READ ALL BILL RECS WHOSE MONTH OF SERVICE                              
*              WAS BILLED IN REQUESTED MONTH                                    
*                                                                               
DBBLL    DS    0H                                                               
*                                                                               
         NETGO NVSETSPT,DMCB       SET TO READ SPOT FILE                        
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                READ FIRST BILL RECORD                       
*                                                                               
DBBLLLP  DS    0H                                                               
*                                                                               
         LA    R5,KEY              ESTABLISH BILL RECORD KEY                    
         USING BILLREC,R5                                                       
*                                                                               
*        MATCH ON ID/AM/CLT                                                     
*                                                                               
         CLC   BKEY(BKEYPRD-BKEY),KEYSAVE   BREAK IN AM/CLT                     
         BNE   DBBLLDN                                                          
*                                                                               
         CLC   BKEYMBIL,WKEYMBIL            MATCH ON BILLING MONTH              
         BNE   DBBLLCN                                                          
*                                                                               
         NETGO NVSETSPT,DMCB       SET TO READ SPOT FILE                        
         MVC   FILENAME,=CL8'SPTFIL'  READ RECORD INTO NETWS2                   
         MVC   AIO,ANETWS2                                                      
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R5,AIO              POINT TO FOUND RECORD                        
         USING BILLREC,R5                                                       
*                                                                               
*        UPDATE CURRENT PRODUCT/ESTIMATE                                        
*                                                                               
         CLC   CURPROD,BKEYPRD     ON CHANGE IN PRODUCT                         
         BE    DBBLLPRX                                                         
*                                                                               
         MVC   CURPROD,BKEYPRD     SET PRODUCT CODE                             
*                                                                               
         BRAS  RE,GTPRD            FIND BINARY PRODUCT CODE                     
*                                                                               
         BRAS  RE,GETPRD           GET GFEST DATA                               
*                                                                               
DBBLLPRX DS    0H                                                               
*                                                                               
         CLC   CUREST,BKEYEST      ON CHANGE IN ESTIMATE                        
         BE    DBBLLESX                                                         
*                                                                               
         MVC   CUREST,BKEYEST      SAVE ESTIMATE                                
*                                                                               
         BRAS  RE,GETEST           GET GFEST DATA                               
*                                                                               
DBBLLESX DS    0H                                                               
*                                                                               
         TM    BILSTAT,X'20'       SKIP IF NOT TRUE AOR BILL                    
         BNO   DBAORX              NO                                           
*                                                                               
         CLC   NBSELAGY,=C'YN'     YES/SKIP UNLESS YNR                          
         BNE   DBBLLCN                                                          
*                                                                               
         CLC   =C'KRM',NBSELCLI         AND CLIENT=KRM                          
         BE    *+10                                                             
         CLC   =C'KRX',NBSELCLI         OR  CLIENT=KRX                          
         BNE   DBBLLCN                                                          
*                                                                               
         XC    BNET,BNET           THEN CLEAR NET AND PROCESS                   
*                                                                               
DBAORX   DS    0H                                                               
*                                                                               
*  Y2K CHANGE BDATE = INTERNAL DDS FORMAT  FAF0F0F1 - JAN/2000                  
*             BDAT  =                      F0F0F0F1 - JAN/2000                  
*  BDAT IS IN THAT FORMAT SINCE IT IS INTERESTED IN YEAR WITHIN DECADE          
*  IN ORDER TO CONSTRUCT BILLING KEY ABOVE                                      
*                                                                               
         MVC   WORK(4),BDATE                                                    
         MVC   WORK+4(2),=X'F0F1'                                               
*                                                                               
         GOTO1 DATCON,DMCB,WORK,(X'20',WORK+6)                                  
*                                                                               
         CLC   WORK+6(4),BDAT      MATCH BILLED IN MONTH                        
         BNE   DBBLLCN                                                          
*                                                                               
*        FILL IN INVOICE RECORD FIELDS                                          
*                                                                               
         MVI   IVREC,C' '          INIT INVOICE RECORD                          
         MVC   IVREC+1(L'IVREC-1),IVREC                                         
*                                  INIT MONEY FIELDS                            
         ZAP   IVDNETP,=P'0'       NET                                          
         ZAP   IVDCOMP,=P'0'       COMPENSATION                                 
         ZAP   IVDADJP,=P'0'       ADJUSTMENT                                   
         ZAP   IVDGRSP,=P'0'       GROSS                                        
         ZAP   IVDBLLP,=P'0'       PREVIOUS BILLING                             
*                                                                               
         MVI   SORTRID,C'1'        FLAG AS DOING INVOICE RECORD                 
*                                                                               
         LA    R2,SPIBLAGY                                                      
         BRAS  RE,IBLAGY           FILL IN BILLING AGENCY                       
         BNZ   DBBLLCN             DROP RECORD                                  
         MVC   IVHBLAGY,SPIBLAGY   SAVE BILLING AGENCY                          
*                                                                               
         LA    R2,IVHBTID                                                       
         LA    R3,SPIVBTID                                                      
         BRAS  RE,IBTID            FILL IN BATCH ID                             
*                                                                               
         LA    R2,IVHINVNO                                                      
         BRAS  RE,IINVNO           FILL IN INVOICE NUMBER                       
*                                                                               
         LA    R2,IVHINVDT                                                      
         BRAS  RE,IINVDT           FILL IN INVOICE DATE                         
*                                                                               
         LA    R2,IVHSRCAG                                                      
         BRAS  RE,ISRCAG           FILL IN SOURCE AGENCY                        
*                                                                               
         LA    R2,IVHREVRS                                                      
         BRAS  RE,IREVRS           FILL IN REVERSAL ID                          
*                                                                               
         LA    R2,IVDBLSTA                                                      
         BRAS  RE,IBLSTA           FILL IN BILLING STATUS                       
*                                                                               
         LA    R2,IVDLINE#                                                      
         BRAS  RE,ILINE#           FILL IN LINE NUMBER                          
*                                                                               
         LA    R2,IVDDESC                                                       
         BRAS  RE,IDESC            FILL IN ESTIMATE DESCRIPTION                 
*                                                                               
         LA    R2,IVDEXPTP                                                      
         BRAS  RE,IEXPTP           FILL IN EXPENSE TYPE                         
*                                                                               
         LA    R2,IVDPRDID                                                      
         BRAS  RE,IPRDID           FILL IN PRODUCT ID                           
*                                                                               
         LA    R2,IVDESTID                                                      
         BRAS  RE,IESTID           FILL IN ESTIMATE ID                          
*                                                                               
         LA    R2,IVDCHGDT                                                      
         BRAS  RE,ICHGDT           FILL IN CHARGE DATE                          
*                                                                               
         LA    R2,IVDNETP                                                       
         BRAS  RE,INET             FILL IN NET                                  
*                                                                               
         LA    R2,IVDCOMP                                                       
         BRAS  RE,ICOM             FILL IN COMMISSION                           
*                                                                               
         LA    R2,IVDADJP                                                       
         BRAS  RE,IADJ             FILL IN ADJUSTMENT                           
*                                                                               
         LA    R2,IVDGRSP                                                       
         BRAS  RE,IGRS             FILL IN GROSS                                
*                                                                               
         LA    R2,IVDBLLP                                                       
         BRAS  RE,IBKBLL           FILL IN BILLED SO FAR                        
*                                                                               
         LA    R2,IVDCRAGY                                                      
         BRAS  RE,ICRAGY           FILL IN CREATIVE AGENCY                      
*                                                                               
*        ADD RECORD TO SORT                                                     
*                                                                               
         MVC   SORTREC,IVREC       MOVE RECORD TO SORT AREA                     
         MVC   IVDPACK-IVREC+SORTREC,IVDPACK                                    
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SORTRID                                      
*                                                                               
DBBLLCN  DS    0H                  NEXT BILL RECORD                             
*                                                                               
         NETGO NVSETSPT,DMCB       SET TO READ SPOT FILE                        
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
*                                                                               
         B     DBBLLLP                                                          
*                                                                               
DBBLLDN  DS    0H                                                               
*                                                                               
DBX      NETGO NVSETUNT,DMCB                                                    
*                                                                               
         XC    FILENAME,FILENAME                                                
         MVI   NBFUNCT,NBFRDHI     INDICATE READ SEQUENCE BROKEN                
*                                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - GTPROD'                         
***********************************************************************         
*                                                                     *         
*        GET PRODUCT CODE FROM CLIST                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GTPROD   NTR1  LABEL=*                                                          
*                                                                               
         L     R2,ACLISTSV                                                      
*                                                                               
GTPRODLP DS    0H                                                               
*                                                                               
         CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GTPROD10                                                         
*                                                                               
         MVC   CURPROD,=C'***'    SET TO UNDEFINED                              
*                                                                               
         B     GTPRODX                                                          
*                                                                               
GTPROD10 DS    0H                                                               
*                                                                               
         CLC   NBSPLPRN,3(R2)      MATCH ON 1 BYTE CODE                         
         BE    GTPRODFD                                                         
*                                                                               
GTPRODCN DS    0H                                                               
*                                                                               
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GTPRODLP            RETURN TO LOOP                               
*                                                                               
GTPRODFD DS    0H                  PRODUCT FOUND IN LIST                        
*                                                                               
         MVC   CURPROD,0(R2)      SET 3 CHAR PRINTABLE PRD CODE                 
*                                                                               
GTPRODX  XIT1                                                                   
         TITLE 'T32077-KR TAPE - PRINT REPORT - GTPRD'                          
***********************************************************************         
*                                                                     *         
*        GET PRODUCT INTERNAL CODE FROM CLIST                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GTPRD    NTR1  LABEL=*                                                          
*                                                                               
         L     R2,ACLISTSV                                                      
*                                                                               
GTPRDLP  DS    0H                                                               
*                                                                               
         CLI   3(R2),0             IF E-O-F CLIST                               
         BNE   GTPRD10                                                          
*                                                                               
         MVI   CURPRD,0          SET TO UNDEFINED                               
*                                                                               
         B     GTPRDX                                                           
*                                                                               
GTPRD10  DS    0H                                                               
*                                                                               
         CLC   CURPROD,0(R2)      MATCH ON 3 BYTE CODE                          
         BE    GTPRDFD                                                          
*                                                                               
GTPRDCN  DS    0H                                                               
*                                                                               
         LA    R2,4(R2)            INCREMENT CLIST                              
         B     GTPRDLP             RETURN TO LOOP                               
*                                                                               
GTPRDFD  DS    0H                  PRODUCT FOUND IN LIST                        
*                                                                               
         MVC   CURPRD,3(R2)        SET 1 CHAR PRD CODE                          
*                                                                               
GTPRDX   XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
****************************************                                        
* GET WEEKLIST INTO PERLIST                                                     
*                                                                               
GTMONLST NTR1  LABEL=*                                                          
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'D'                                                       
         MVI   NBRESUME,NBPROCPK                                                
GTWK5    NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBVALDAT                                                  
         BNE   GTWK5                                                            
         MVI   PERTYPE,C'M'        MONTHS ONLY                                  
         LA    R3,24               FOR 24 MONTHS MAX                            
         ST    R3,NUMPER                                                        
         LA    R3,MONLIST                                                       
         ST    R3,AMONLIST                                                      
         NETGO NVWKLST,DMCB,NUMPER,(R3),PERTYPE                                 
*                                                                               
         L     R3,AMONLIST         CREAT MONTHYR LIST                           
         LA    R4,MONYEAR          2 BYTE Y/M + 6 BYTE MMM/YY                   
         L     R5,NUMPER                                                        
GTWK20   GOTO1 DATCON,DMCB,(2,0(R3)),(0,RECWORK)                                
         DS    0H                                                               
         GOTO1 ADDAY,DMCB,RECWORK,RECWORK+10,F'+10'                             
         DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,RECWORK+10),(3,RECWORK)                           
         MVC   0(2,R4),RECWORK                                                  
         GOTO1 DATCON,DMCB,(0,RECWORK+10),(6,RECWORK)                           
         MVC   2(6,R4),RECWORK                                                  
         LA    R3,4(R3)                                                         
         CLI   0(R3),0                                                          
         BE    GTWKX                                                            
         LA    R4,8(R4)                                                         
         BCT   R5,GTWK20                                                        
GTWKX    XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - SPOOLIT'                        
         SPACE 2                                                                
SPOOLIT  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - HDRTN'                          
***********************************************************************         
*                                                                     *         
*        HEADLINE FORMAT ROUTINE                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HDRTN    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ATWA                                                          
         USING T320FFD,R6                                                       
*                                                                               
         MVC   H1(12),=C'NETWORK T.V.'                                          
*                                                                               
         MVC   H3(9),=C'BILL AGY:'                                              
*                                                                               
         MVC   H4(9),=C'BATCH ID:'                                              
*                                                                               
         MVC   H5(9),=C'MEDIA   :'                                              
         MVC   H5+11(1),NBSELMED                                                
*                                                                               
         MVC   H6(9),=C'CLIENT  :'                                              
         MVC   H6+11(3),SPLCLI                                                  
         MVC   H6+16(20),SPLCLIN                                                
*                                                                               
         ICM   R1,15,ABOX          POINT TO BOXES CONTROL BLOCK                 
         BZ    HDX                 YES/ ON-LINE SKIP BOXES                      
         USING BOXD,R1                                                          
*                                                                               
*        BOX INITIALIZATION                                                     
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
*                                                                               
*        FORMAT BOXES FOR ESTIMATE REPORT                                       
*                                                                               
         CLI   RIDSAVE,C'2'        SKIP IF NOT ESTIMATE RECORD                  
         BNE   HDRESTN                                                          
*                                                                               
         MVC   H4+11(L'SPESBTID),SPESBTID                                       
*                                                                               
         MVC   H3+11(L'SPEBLAGY),SPEBLAGY                                       
*                                                                               
         MVC   H4+50(32),=CL32'   ESTIMATE REPORT'                              
*                                                                               
*        SET UP COLUMN LINES                                                    
*                                                                               
         LA    R4,BOXCOLS                                                       
         USING PELINED,R4                                                       
*                                                                               
         MVI   PESTR,C'L'                                                       
         MVI   PEEDESC-1,C'C'                                                   
         MVI   PEBLSTA-1,C'C'                                                   
         MVI   PERQNUM-1,C'C'                                                   
         MVI   PETGTMK-1,C'C'                                                   
         MVI   PEEXPTP-1,C'C'                                                   
         MVI   PEPRDID-1,C'C'                                                   
         MVI   PENET-1,C'C'                                                     
         MVI   PEGRS-1,C'C'                                                     
         MVI   PEERR-1,C'C'                                                     
         MVI   PEEND,C'R'                                                       
*                                                                               
*        SET UP ROW BREAKS                                                      
*                                                                               
         LA    R4,BOXROWS                                                       
*                                                                               
         LA    R4,8(R4)                                                         
         MVI   0(R4),C'T'                                                       
*                                                                               
         LA    R4,3(R4)                                                         
         MVI   0(R4),C'M'                                                       
*                                                                               
         LA    R4,49(R4)                                                        
         MVI   0(R4),C'B'                                                       
*                                                                               
*        SET UP COLUMN HEADLINES                                                
*                                                                               
         LA    R4,H10                                                           
         USING PELINED,R4                                                       
*                                                                               
         MVC   PEESTID,=CL9'ESTIMATE'                                           
         MVC   PEEDESC,=CL20'DESCRIPTION'                                       
         MVC   PEBLSTA,=CL4'BILL'                                               
         MVC   PERQNUM,=CL20'    REQUIREMENT'                                   
         MVC   PETGTMK,=CL3'TGT'                                                
         MVC   PEEXPTP,=CL6' EXP'                                               
         MVC   PEPRDID,=CL10'PRODUCT'                                           
         MVC   PENET,=CL16'      NET'                                           
         MVC   PEGRS,=CL16'     GROSS'                                          
         MVC   PEERR,=CL11'ERRORS'                                              
*                                                                               
         LA    R4,H11              BUMP TO NEXT HEADLINE                        
*                                                                               
         MVC   PEESTID,=CL9'   ID   '                                           
         MVC   PEEDESC,=CL20'           '                                       
         MVC   PEBLSTA,=CL4'STAT'                                               
         MVC   PERQNUM,=CL20'      NUMBER   '                                   
         MVC   PETGTMK,=CL3'CDE'                                                
         MVC   PEEXPTP,=CL6' TYPE'                                              
         MVC   PEPRDID,=CL10'  ID   '                                           
         MVC   PENET,=CL16' '                                                   
         MVC   PEGRS,=CL16' '                                                   
         MVC   PEERR,=CL11'  '                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
HDRESTX  DS    0H                                                               
*                                                                               
         B     HDX                                                              
*                                                                               
HDRESTN  DS    0H                                                               
*                                                                               
         MVC   H4+11(L'SPIVBTID),SPIVBTID                                       
*                                                                               
         MVC   H4+50(32),=CL32'   INVOICE REPORT'                               
*                                                                               
         MVC   H3+11(L'SPIBLAGY),SPIBLAGY                                       
*                                                                               
*        SET UP COLUMN LINES FOR INVOICE REPORT                                 
*                                                                               
         LA    R4,BOXCOLS                                                       
         USING PILINED,R4                                                       
*                                                                               
         MVI   PISTR,C'L'                                                       
         MVI   PIINVDT-1,C'C'                                                   
         MVI   PISRCAG-1,C'C'                                                   
         MVI   PIBLSTA-1,C'C'                                                   
         MVI   PIEXPTP-1,C'C'                                                   
         MVI   PIPRDID-1,C'C'                                                   
         MVI   PIESTID-1,C'C'                                                   
         MVI   PICHGDT-1,C'C'                                                   
         MVI   PINET-1,C'C'                                                     
         MVI   PICOM-1,C'C'                                                     
         MVI   PIGRS-1,C'C'                                                     
         MVI   PICRAGY-1,C'C'                                                   
         MVI   PIERR-1,C'C'                                                     
         MVI   PIEND,C'R'                                                       
*                                                                               
*        SET UP ROW BREAKS                                                      
*                                                                               
         LA    R4,BOXROWS                                                       
*                                                                               
         LA    R4,8(R4)                                                         
         MVI   0(R4),C'T'                                                       
*                                                                               
         LA    R4,3(R4)                                                         
         MVI   0(R4),C'M'                                                       
*                                                                               
         LA    R4,49(R4)                                                        
         MVI   0(R4),C'B'                                                       
*                                                                               
*        SET UP COLUMN HEADLINES                                                
*                                                                               
         LA    R4,H10                                                           
         USING PILINED,R4                                                       
*                                                                               
         MVC   PIINVNM,=CL10' INVOICE'                                          
         MVC   PIINVDT,=CL8'INVOICE'                                            
         MVC   PISRCAG,=CL9'SOURCE '                                            
         MVC   PIBLSTA,=CL4'BILL'                                               
         MVC   PIEXPTP,=CL6' EXP'                                               
         MVC   PIPRDID,=CL10'PRODUCT'                                           
         MVC   PIESTID,=CL9'ESTIMATE'                                           
         MVC   PICHGDT,=CL10'  CHARGE'                                          
         MVC   PINET,=CL16'    NET'                                             
         MVC   PICOM,=CL16'  COMPEN  '                                          
         MVC   PIGRS,=CL16'   GROSS'                                            
         MVC   PICRAGY,=CL10'CREATE'                                            
         MVC   PIERR,=CL11'ERRORS'                                              
*                                                                               
         LA    R4,H11              BUMP TO NEXT HEADLINE                        
*                                                                               
         MVC   PIINVNM,=CL10' NUMBER '                                          
         MVC   PIINVDT,=CL8'  DATE '                                            
         MVC   PISRCAG,=CL9'AGENCY '                                            
         MVC   PIBLSTA,=CL4'STA '                                               
         MVC   PIEXPTP,=CL6' TYP'                                               
         MVC   PIPRDID,=CL10'  ID   '                                           
         MVC   PIESTID,=CL9'   ID   '                                           
         MVC   PICHGDT,=CL10'   DATE '                                          
         MVC   PINET,=CL16'         '                                           
         MVC   PICOM,=CL16'              '                                      
         MVC   PICOM,=CL16'  SATION  '                                          
         MVC   PIGRS,=CL16'          '                                          
         MVC   PICRAGY,=CL10'AGENCY '                                           
         MVC   PIERR,=CL11'  '                                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
         SPACE                                                                  
HDX      XIT1                                                                   
         LTORG                                                                  
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - DOUNIT'                         
***********************************************************************         
*                                                                     *         
*        ADD UNITS DATA TO TABLE                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DOUNIT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        FILL IN ESTIMATE RECORD FIELDS                                         
*        USES ROUTINES COPIED FROM SPOT AND PRINT                               
*                                                                               
         MVI   ESREC,C' '          INIT ESTIMATE RECORD                         
         MVC   ESREC+1(L'ESREC-1),ESREC                                         
*                                                                               
         LA    R2,SPEBLAGY                                                      
         BRAS  RE,IBLAGY           FILL IN BILLING AGENCY                       
         BNE   DOUNITX             DROP RECORD                                  
*                                                                               
         MVC   ESHBLAGY,SPEBLAGY   SAVE BILLING AGENCY                          
*                                                                               
         MVI   SORTRID,C'2'        FLAG AS DOING ESTIMATE RECORD                
*                                                                               
         LA    R2,ESHBTID                                                       
         LA    R3,SPESBTID                                                      
         BRAS  RE,IBTID            FILL IN BATCH ID                             
*                                                                               
         LA    R2,ESHESTID                                                      
         LA    R3,SPESBTID                                                      
         BRAS  RE,IESTID           FILL IN ESTIMATE ID                          
*                                                                               
         LA    R2,ESHDESC                                                       
         BRAS  RE,IDESC            FILL IN ESTIMATE DESCRIPTION                 
*                                                                               
         LA    R2,ESHCHGDT                                                      
         BRAS  RE,ICHGDT           FILL IN CHARGE DATE                          
*                                                                               
         LA    R2,ESHBLSTA                                                      
         BRAS  RE,IBLSTA           FILL IN BILLING STATUS                       
*                                                                               
         LA    R2,ESHREQNM                                                      
         BRAS  RE,IREQNM           FILL IN REQUIREMENT NUMBER                   
*                                                                               
         LA    R2,ESHTGTMK                                                      
         BRAS  RE,ITGTMK           FILL IN TARGET MARKET                        
*                                                                               
         LA    R2,ESDEXPTP                                                      
         BRAS  RE,IEXPTP           FILL IN EXPENSE TYPE                         
*                                                                               
         LA    R2,ESDPRDID                                                      
         BRAS  RE,IPRDID           FILL IN PRODUCT ID                           
*                                                                               
         LA    R2,ESDNETP                                                       
         BRAS  RE,INET             FILL IN NET                                  
*                                                                               
         LA    R2,ESDCOMP                                                       
         BRAS  RE,ICOM             FILL IN COMMISSION                           
*                                                                               
         LA    R2,ESDADJP                                                       
         BRAS  RE,IADJ             FILL IN ADJUSTMENT                           
*                                                                               
         LA    R2,ESDGRSP                                                       
         BRAS  RE,IGRS             FILL IN GROSS                                
*                                                                               
         LA    R2,ESDBLLP                                                       
         BRAS  RE,IBKBLL           FILL IN BILLED SO FAR                        
*                                                                               
         LA    R2,ESDDEAL#                                                      
         BRAS  RE,IDEAL#           FILL IN DEAL NUMBER                          
*                                                                               
*        ADD RECORD TO SORT                                                     
*                                                                               
         MVC   SORTREC,ESREC       MOVE RECORD TO SORT AREA                     
         MVC   ESDPACK-ESREC+SORTREC,ESDPACK                                    
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SORTRID                                      
*                                                                               
DOUNITX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - ESTDTL'                         
***********************************************************************         
*                                                                     *         
*        HANDLE ESTIMATE DETAIL RECORDS                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ESTDTL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SORTRID,C'2'        IF ESTIMATE RECORD COMING IN                 
         BNE   ESDTNEW                                                          
*                                                                               
         LA    R2,ESHESTID-ESREC+SORTREC  POINT TO ESTIMATE ID                  
*                                                                               
         BRAS  RE,GETESTID         FIND IN TABLE                                
         BNE   ESTDTLX             DROP IF NOT IN TABLE                         
*                                                                               
         CLC   ESHDR,SORTREC       IF NO CHANGE IN KEY                          
         BNE   ESDTNEW                                                          
*                                                                               
*        COMBINE RECORDS BY ADDING PACKED COUNTERS TOGETHER                     
*                                                                               
ESTDTCMB DS    0H                                                               
*                                                                               
         LHI   RF,ESDPACK-ESREC    DISPLACMENT TO PACKED BUCKETS                
*                                                                               
         LA    R2,ESREC(RF)        POINT TO 2 SETS OF BUCKETS                   
         LA    R3,SORTREC(RF)                                                   
         LA    R0,5                5 BUCKETS                                    
*                                                                               
ESDTBKLP DS    0H                  ACCUMULATE BUCKETS                           
*                                                                               
         AP    0(8,R2),0(8,R3)                                                  
*                                                                               
ESDTBKCN DS    0H                                                               
         LA    R2,8(R2)            POINT TO NEXT BUCKETS                        
         LA    R3,8(R3)                                                         
         BCT   R0,ESDTBKLP         GO DO NEXT BUCKET                            
*                                                                               
ESDTCMBX DS    0H                                                               
*                                                                               
         B     ESTDTLX                                                          
*                                                                               
*        NEW KEY - FIRST HANDLE OLD RECORD                                      
*                                                                               
ESDTNEW  DS    0H                  CHANGE IN ESTIMATE DETAIL KEY                
*                                                                               
         CLC   ESHDR,SPACES        IF                                           
         BNH   ESDTN10                FIRST TIME                                
*                                     NOT FIRST TIME                            
         LA    R2,ESDNETP          FORMAT NET                                   
         LA    R3,ESDNET                                                        
         BRAS  RE,ONET                                                          
         AP    WESNETP,ESDNETP     ACCUMULATE REQUEST TOTAL                     
         AP    SPESNET,ESDNETP     ACCUMULATE BATCH TOTAL                       
*                                                                               
         LA    R2,ESDCOMP          FORMAT COM                                   
         LA    R3,ESDCOM                                                        
         BRAS  RE,OCOM                                                          
         AP    WESCOMP,ESDCOMP     ACCUMULATE REQUEST TOTAL                     
         AP    SPESCOM,ESDCOMP     ACCUMULATE BATCH TOTAL                       
*                                                                               
         LA    R2,ESDADJP          FORMAT ADJ                                   
         LA    R3,ESDADJ                                                        
         BRAS  RE,OADJ                                                          
         AP    WESADJP,ESDADJP     ACCUMULATE REQUEST TOTAL                     
         AP    SPESADJ,ESDADJP     ACCUMULATE BATCH TOTAL                       
*                                                                               
         LA    R2,ESDGRSP          FORMAT GR0SS                                 
         LA    R3,ESDGRS                                                        
         BRAS  RE,OGRS                                                          
         AP    WESGRSP,ESDGRSP     ACCUMULATE REQUEST TOTAL                     
         AP    SPESGRS,ESDGRSP     ACCUMULATE BATCH TOTAL                       
*                                                                               
         LA    R2,ESDBLLP          FORMAT PREVIOUSLY BILLED                     
         LA    R3,ESDBLL                                                        
         BRAS  RE,OBKBLL                                                        
         AP    WESBLLP,ESDBLLP     ACCUMULATE REQUEST TOTAL                     
         AP    SPESBLL,ESDBLLP     ACCUMULATE BATCH TOTAL                       
*                                                                               
         CLI   SPLTAPE,C'Y'       SKIP IF NOT PRODUCING TAPE                    
         BE    *+8                                                              
         CLI   SPLTAPE,C'T'       SKIP IF NOT PRODUCING TAPE                    
         BNE   ESDTOPX                                                          
*                                                                               
         L     R3,AKRESTFL         POINT TO OUTPUT TAPE DCB                     
*                                                                               
*        OPEN TAPE IF NEEDED                                                    
*                                                                               
         CLI   SPESTSW,C'O'        SKIP IF TAPE ALREADY OPENED                  
         BE    ESDTOPX                                                          
*                                                                               
         OPEN  ((R3),OUTPUT)       OPEN DISK FILE                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SPESTSW,C'O'        INDICATE ESTIMATE FILE OPENED                
*                                                                               
ESDTOPX  DS    0H                                                               
*                                                                               
*        BUILD ESTIMATE BATCH HEADER RECORD                                     
*                                                                               
         CLI   SPEHEAD,C'H'        SKIP IF HEADER ALREADY WRITTEN               
         BNE   ESDTHDRX                                                         
*                                                                               
         MVI   EBHREC,C' '         INIT ESTIMATE BATCH HEADER RECORD            
         MVC   EBHREC+1(255),EBHREC                                             
*                                                                               
         MVC   EBHRECID,=CL3'AE '  SET RECORD ID                                
         MVC   EBHSTART,=CL3'ADE'  SET SECONDARY RECORD ID                      
         MVC   EBHRECV,=CL10'STIMATE' SET REST OF RECORD ID                     
*                                                                               
         CLC   IBHCRDTE,SPACES     IF CREATION DATE NOT SET                     
         BH    *+8                                                              
         BRAS  RE,CRDTE               GO SET IT                                 
*                                                                               
         MVC   EBHCRDTE,IBHCRDTE  SET CREATION DATE                             
         MVC   EBHCRTIM,IBHCRTIM  SET HH:MM:SS                                  
*                                                                               
         CLI   SPLTAPE,C'Y'       SKIP IF NOT PRODUCING TAPE                    
         BE    *+8                                                              
         CLI   SPLTAPE,C'T'       SKIP IF NOT PRODUCING TAPE                    
         BNE   ESDTHDR1                                                         
*                                                                               
         LA    R5,EBHREC             WRITE BATCH HEADER RECORD TO FILE          
         PUT   (R3),(R5)                                                        
*                                                                               
ESDTHDR1 DS    0H                                                               
*                                                                               
         MVI   SPEHEAD,C'N'          TURN OFF SWITCH                            
*                                                                               
ESDTHDRX DS    0H                                                               
*                                                                               
         MVC   SPEBLAGY,ESHBLAGY   SAVE BILLING AGENCY                          
*                                                                               
         CLI   SPLTAPE,C'Y'       SKIP IF NOT PRODUCING TAPE                    
         BE    *+8                                                              
         CLI   SPLTAPE,C'T'       SKIP IF NOT PRODUCING TAPE                    
         BNE   ESDTTAP1                                                         
*                                                                               
         LA    R5,ESREC            POINT TO OUTPUT ESTIMATE RECORD              
*                                                                               
         PUT   (R3),(R5)           PUT TO DISK FILE                             
*                                                                               
ESDTTAP1 DS    0H                                                               
*                                                                               
         AP    SPEBTCTR,=P'1'      BUMP RECORD COUNTER FOR THIS REC             
*                                                                               
ESDTTAPX DS    0H                                                               
*                                                                               
*        PRINT RECORD                                                           
*                                                                               
         LA    R4,P                ESTABLISH ESTIMATE PRINT LINE                
         USING PELINED,R4                                                       
*                                                                               
         MVC   PEESTID,ESHESTID    ESTIMATE ID                                  
         MVC   PEEDESC,ESHDESC     DESCRIPTION                                  
         MVC   PEBLSTA+1(1),ESHBLSTA    BILLING STATUS                          
         MVC   PERQNUM,ESHREQNM    REQUIREMENT NUMBER                           
         MVC   PETGTMK+1(1),ESHTGTMK    TARGET MARKET CODE                      
         MVC   PEEXPTP,ESDEXPTP    EXPENSE TYPE                                 
         MVC   PEPRDID,ESDPRDID    PRODUCT ID                                   
         MVC   PENET,ESDNET        NET                                          
         MVC   PEGRS,ESDGRS        GROSS                                        
         MVC   PEERR,=CL11' '      ERRORS                                       
*                                                                               
*        ERROR ANALYSIS                                                         
*                                                                               
         MVI   ERRCD,0             CLEAR ERROR CODE                             
*                                                                               
         CLC   ESHBLAGY,SPACES     MUST HAVE BILLING AGENCY                     
         BH    *+8                                                              
         OI    ERRCD,ERREST                                                     
*                                                                               
         CLC   PETGTMK,SPACES      MUST HAVE TARGET MARKET CODE                 
         BH    *+8                                                              
         OI    ERRCD,ERREST                                                     
*                                                                               
         CLC   PEEXPTP,SPACES      MUST HAVE EXPENSE TYPE                       
         BH    *+8                                                              
         OI    ERRCD,ERREST                                                     
*                                                                               
         CLC   PEPRDID,SPACES      MUST HAVE PRODUCT ID                         
         BH    *+8                                                              
         OI    ERRCD,ERRPRD                                                     
*                                                                               
         CLI   ERRCD,0             DONE IF NO ERRORS                            
         BE    ESDTERRX                                                         
*                                                                               
         LA    R1,PEERR                                                         
*                                                                               
         TM    ERRCD,ERRAGY        CHECK FOR AGENCY IN ERROR                    
         BZ    *+14                                                             
         MVC   0(4,R1),=C'AGY,'                                                 
         LA    R1,4(R1)                                                         
*                                                                               
         TM    ERRCD,ERRPRD        CHECK FOR PRODUCT ERROR                      
         BZ    *+14                                                             
         MVC   0(4,R1),=C'PRD,'                                                 
         LA    R1,4(R1)                                                         
*                                                                               
         TM    ERRCD,ERREST        CHECK FOR ESTIMATE ERROR                     
         BZ    *+14                                                             
         MVC   0(3,R1),=C'EST'                                                  
         LA    R1,3(R1)                                                         
*                                                                               
         BCTR  R1,0                REMOVE TRAILING COMMA                        
         CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
*                                                                               
         MVI   ERRCD,0             RESET ERROR CODE                             
*                                                                               
ESDTERRX DS    0H                                                               
*                                                                               
         BRAS  RE,SPOOLIT          PRINT LINE                                   
*                                                                               
*        ADD THIS RECORD TO TOTALS                                              
*                                                                               
ESDTN10  DS    0H                                                               
*                                                                               
         CLI   SORTRID,C'2'        SKIP IF NOT AN ESTIMATE RECORD               
         BNE   *+16                                                             
         MVC   ESREC,SORTREC       SAVE NEW RECORD                              
         MVC   ESDPACK,SORTREC+ESDPACK-ESREC                                    
*                                                                               
ESTDTLX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - ESTTOT'                         
***********************************************************************         
*                                                                     *         
*        HANDLE ESTIMATE TOTALS                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ESTTOT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        WRITE CONTROL RECORD TO TAPE                                           
*                                                                               
         MVI   ESCREC,C' '         INIT ESTIMATE CONTROL RECORD                 
         MVC   ESCREC+1(255),ESCREC                                             
*                                                                               
         MVC   ESCBLAGY,SPEBLAGY   COPY BILLING AGENCY                          
         MVC   ESCBTID,SPESBTID    COPY BATCH ID                                
*                                                                               
         LA    R2,SPESNET          FORMAT NET                                   
         LA    R3,ESCNET                                                        
         BRAS  RE,ONET                                                          
*                                                                               
         LA    R2,SPESCOM          FORMAT COM                                   
         LA    R3,ESCCOM                                                        
         BRAS  RE,OCOM                                                          
*                                                                               
         LA    R2,SPESADJ          FORMAT ADJ                                   
         LA    R3,ESCADJ                                                        
         BRAS  RE,OADJ                                                          
*                                                                               
         LA    R2,SPESGRS          FORMAT GR0SS                                 
         LA    R3,ESCGRS                                                        
         BRAS  RE,OGRS                                                          
*                                                                               
         LA    R2,SPESBLL          FORMAT PREVIOUSLY BILLED                     
         LA    R3,ESCBLL                                                        
         BRAS  RE,OBKBLL                                                        
*                                                                               
         OI    SPEBTCTR+7,X'0F'   FORCE SIGN                                    
         UNPK  ESCRECCT,SPEBTCTR  PRINT WITH LEADING ZEROS                      
*                                                                               
         CLI   SPEHEAD,C'H'       SKIP IF HEADER STILL TO BE WRITTEN            
         BE    ETLTPX                NO DETAILS HAVE BEEN WRITTEN               
*                                                                               
         CLI   SPLTAPE,C'Y'       SKIP IF NOT PRODUCING TAPE                    
         BE    *+8                                                              
         CLI   SPLTAPE,C'T'       SKIP IF NOT PRODUCING TAPE                    
         BNE   ETLTPX                                                           
*                                                                               
         L     R3,AKRESTFL         POINT TO OUTPUT TAPE DCB                     
         LA    R5,ESCREC           POINT TO OUTPUT CONTROL RECORD               
*                                                                               
         PUT   (R3),(R5)           PUT TO DISK FILE                             
*                                                                               
ETLTPX   DS    0H                                                               
*                                                                               
         AP    SPEBTCTR,=P'1'      BUMP RECORD COUNTER FOR THIS REC             
*                                                                               
ETLHDX   DS    0H                                                               
*                                                                               
         MVI   ESTREC,C' '         INIT TRAILER RECORD                          
         MVC   ESTREC+1(255),ESTREC                                             
*                                                                               
         MVC   ESTRECID,=C'AE '                                                 
         MVC   ESTSTRT,=CL17'TTRL'                                              
*                                                                               
         OI    SPEBTCTR+7,X'0F'   FORCE SIGN                                    
         UNPK  ESTRECCT,SPEBTCTR  PRINT WITH LEADING ZEROS                      
*                                                                               
         CLI   SPEHEAD,C'H'       SKIP IF HEADER STILL TO BE WRITTEN            
         BE    ETLTP1X               NO DETAILS HAVE BEEN WRITTEN               
*                                                                               
         CLI   SPLTAPE,C'Y'       SKIP IF NOT PRODUCING TAPE                    
         BE    *+8                                                              
         CLI   SPLTAPE,C'T'       SKIP IF NOT PRODUCING TAPE                    
         BNE   ETLTP1X                                                          
*                                                                               
         L     R3,AKRESTFL         POINT TO OUTPUT TAPE DCB                     
         LA    R5,ESTREC           PUT TRAILER RECORD TO TAPE                   
*                                                                               
         PUT   (R3),(R5)                                                        
*                                                                               
ETLTP1X  DS    0H                                                               
*                                                                               
         AP    SPEBTCTR,=P'1'      BUMP RECORD COUNTER FOR THIS REC             
*                                                                               
ESTTOTX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - ESTPTOT'                        
***********************************************************************         
*                                                                     *         
*        PRINT  ESTIMATE TOTALS                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ESTPTOT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        FORMAT ESTIMATE TOTALS                                                 
*                                                                               
         MVC   ESCBLAGY,SPEBLAGY   COPY BILLING AGENCY                          
         MVC   ESCBTID,SPESBTID    COPY BATCH ID                                
*                                                                               
         LA    R2,WESNETP          FORMAT NET                                   
         LA    R3,ESCNET                                                        
         BRAS  RE,ONET                                                          
*                                                                               
         LA    R2,WESCOMP          FORMAT COM                                   
         LA    R3,ESCCOM                                                        
         BRAS  RE,OCOM                                                          
*                                                                               
         LA    R2,WESADJP          FORMAT ADJ                                   
         LA    R3,ESCADJ                                                        
         BRAS  RE,OADJ                                                          
*                                                                               
         LA    R2,WESGRSP          FORMAT GR0SS                                 
         LA    R3,ESCGRS                                                        
         BRAS  RE,OGRS                                                          
*                                                                               
         LA    R2,WESBLLP          FORMAT PREVIOUSLY BILLED                     
         LA    R3,ESCBLL                                                        
         BRAS  RE,OBKBLL                                                        
*                                                                               
         OI    SPEBTCTR+7,X'0F'   FORCE SIGN                                    
         UNPK  ESCRECCT,SPEBTCTR  PRINT WITH LEADING ZEROS                      
*                                                                               
*        PRINT ESTIMATE TOTALS                                                  
*                                                                               
         L     R1,ABOX             ESTABLISH BOXES                              
         USING BOXD,R1                                                          
*                                                                               
         MVI   BOXREQ,C'B'         DRAW SEPARATING LINE                         
*                                                                               
         DROP  R1                                                               
*                                                                               
         LA    R4,P                ESTABLISH ESTIMATE PRINT LINE                
         USING PELINED,R4                                                       
*                                                                               
         BRAS  RE,SPOOLIT          PRINT LINE                                   
*                                                                               
         MVC   PEESTID,=CL9'*ALL*' TOTAL ID                                     
         MVC   PENET,ESCNET        NET                                          
         MVC   PEGRS,ESCGRS        GROSS                                        
*                                                                               
         BRAS  RE,SPOOLIT          PRINT LINE                                   
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
*                                                                               
ESTPTOTX XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - INVTOT'                         
***********************************************************************         
*                                                                     *         
*        HANDLE INVOICE  TOTAL  RECORDS                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INVTOT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        WRITE CONTROL RECORD TO TAPE                                           
*                                                                               
         MVI   IVCREC,C' '         INIT INVOICE BATCH CONTROL REC               
         MVC   IVCREC+1(255),IVCREC                                             
*                                                                               
         MVC   IVCBLAGY,SPIBLAGY   COPY BILLING AGENCY                          
         MVC   IVCBTID,SPIVBTID    COPY BATCH ID                                
*                                                                               
         LA    R2,SPIVNET          FORMAT NET                                   
         LA    R3,IVCNET                                                        
         BRAS  RE,ONET                                                          
*                                                                               
         LA    R2,SPIVCOM          FORMAT COM                                   
         LA    R3,IVCCOM                                                        
         BRAS  RE,OCOM                                                          
*                                                                               
         LA    R2,SPIVADJ          FORMAT ADJ                                   
         LA    R3,IVCADJ                                                        
         BRAS  RE,OADJ                                                          
*                                                                               
         LA    R2,SPIVGRS          FORMAT GR0SS                                 
         LA    R3,IVCGRS                                                        
         BRAS  RE,OGRS                                                          
*                                                                               
         OI    SPIBTCTR+7,X'0F'   FORCE SIGN                                    
         UNPK  IVCRECCT,SPIBTCTR  PRINT WITH LEADING ZEROS                      
*                                                                               
         CLI   SPIHEAD,C'H'       SKIP IF HEADER STILL TO BE WRITTEN            
         BE    ITLTPX                NO DETAILS HAVE BEEN WRITTEN               
*                                                                               
         CLI   SPLTAPE,C'Y'       SKIP IF NOT PRODUCING TAPE                    
         BE    *+8                                                              
         CLI   SPLTAPE,C'T'       SKIP IF NOT PRODUCING TAPE                    
         BNE   ITLTPX                                                           
*                                                                               
         L     R3,AKRINVFL         POINT TO OUTPUT TAPE DCB                     
         LA    R5,IVCREC           POINT TO OUTPUT CONTROL RECORD               
*                                                                               
         PUT   (R3),(R5)           PUT TO DISK FILE                             
*                                                                               
ITLTPX   DS    0H                                                               
*                                                                               
         AP    SPIBTCTR,=P'1'      BUMP RECORD COUNTER FOR THIS REC             
*                                                                               
ITLHDX   DS    0H                                                               
*                                                                               
*        PUT OUT TRAILER RECORD                                                 
*                                                                               
         MVI   IBTREC,C' '         INIT TRAILER RECORD                          
         MVC   IBTREC+1(255),IBTREC                                             
*                                                                               
         MVC   IBTRECID,=C'AI '                                                 
         MVC   IBTSTRT,=CL17'TTRL'                                              
*                                                                               
         OI    SPIBTCTR+7,X'0F'   FORCE SIGN                                    
         UNPK  IBTRECCT,SPIBTCTR  PRINT WITH LEADING ZEROS                      
*                                                                               
         CLI   SPIHEAD,C'H'       SKIP IF HEADER STILL TO BE WRITTEN            
         BE    ITLTP1X               NO DETAILS HAVE BEEN WRITTEN               
*                                                                               
         CLI   SPLTAPE,C'Y'       SKIP IF NOT PRODUCING TAPE                    
         BE    *+8                                                              
         CLI   SPLTAPE,C'T'       SKIP IF NOT PRODUCING TAPE                    
         BNE   ITLTP1X                                                          
*                                                                               
         L     R3,AKRINVFL         POINT TO OUTPUT TAPE DCB                     
         LA    R5,IBTREC           PUT TRAILER RECORD TO TAPE                   
*                                                                               
         PUT   (R3),(R5)                                                        
*                                                                               
ITLTP1X  DS    0H                                                               
*                                                                               
         AP    SPIBTCTR,=P'1'      BUMP RECORD COUNTER FOR THIS REC             
*                                                                               
INVTOTX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32077-KRTAPE - PRINT REPORT - INVPTOT'                         
***********************************************************************         
*                                                                     *         
*        PRINT TOTALS FOR INVOICE REPORT                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INVPTOT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        FORMAT TOTALS                                                          
*                                                                               
         MVC   IVCBLAGY,SPIBLAGY   COPY BILLING AGENCY                          
         MVC   IVCBTID,SPIVBTID    COPY BATCH ID                                
*                                                                               
         LA    R2,WIVNETP          FORMAT NET                                   
         LA    R3,IVCNET                                                        
         BRAS  RE,ONET                                                          
*                                                                               
         LA    R2,WIVCOMP          FORMAT COM                                   
         LA    R3,IVCCOM                                                        
         BRAS  RE,OCOM                                                          
*                                                                               
         LA    R2,WIVADJP          FORMAT ADJ                                   
         LA    R3,IVCADJ                                                        
         BRAS  RE,OADJ                                                          
*                                                                               
         LA    R2,WIVGRSP          FORMAT GR0SS                                 
         LA    R3,IVCGRS                                                        
         BRAS  RE,OGRS                                                          
*                                                                               
         OI    SPIBTCTR+7,X'0F'   FORCE SIGN                                    
         UNPK  IVCRECCT,SPIBTCTR  PRINT WITH LEADING ZEROS                      
*                                                                               
*        PRINT INVOICE TOTALS                                                   
*                                                                               
         L     R1,ABOX             ESTABLISH BOXES                              
         USING BOXD,R1                                                          
*                                                                               
         MVI   BOXREQ,C'B'         DRAW SEPARATING LINE                         
*                                                                               
         DROP  R1                                                               
*                                                                               
         LA    R4,P                ESTABLISH ESTIMATE PRINT LINE                
         USING PILINED,R4                                                       
*                                                                               
         BRAS  RE,SPOOLIT          PRINT LINE                                   
*                                                                               
         MVC   PIINVNM,=CL8'*ALL*' TOTAL ID                                     
         MVC   PINET,IVCNET+4      NET                                          
         MVC   PICOM,IVCCOM+4      COMPENSATION                                 
         MVC   PIGRS,IVCGRS+4      GROSS                                        
*                                                                               
         BRAS  RE,SPOOLIT          PRINT LINE                                   
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
*                                                                               
INVPTOTX XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - INVDTL'                         
***********************************************************************         
*                                                                     *         
*        HANDLE INVOICE  DETAIL RECORDS                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INVDTL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,IVDESTID-IVREC+SORTREC POINT TO ESTIMATE ID                   
*                                                                               
         BRAS  RE,ADDESTID         ADD ESTID TO TABLE                           
*                                                                               
         B     IVDTNEW                DON'T COMBINE RECORDS                     
*                                                                               
*                                                                               
         CLC   IVHDR,SORTREC       IF NO CHANGE IN KEY                          
         BNE   IVDTNEW                                                          
*                                                                               
*        COMBINE RECORDS BY ADDING PACKED COUNTERS TOGETHER                     
*                                                                               
INVDTCMB DS    0H                                                               
*                                                                               
         LHI   RF,IVDPACK-IVREC    DISPLACMENT TO PACKED BUCKETS                
*                                                                               
         LA    R2,IVREC(RF)        POINT TO 2 SETS OF BUCKETS                   
         LA    R3,SORTREC(RF)                                                   
         LA    R0,5                5 BUCKETS                                    
*                                                                               
IVDTBKLP DS    0H                  ACCUMULATE BUCKETS                           
*                                                                               
         AP    0(8,R2),0(8,R3)                                                  
*                                                                               
IVDTBKCN DS    0H                                                               
         LA    R2,8(R2)            POINT TO NEXT BUCKETS                        
         LA    R3,8(R3)                                                         
         BCT   R0,IVDTBKLP         GO DO NEXT BUCKET                            
*                                                                               
IVDTCMBX DS    0H                                                               
*                                                                               
         B     INVDTLX                                                          
*                                                                               
*        NEW KEY - FIRST HANDLE OLD RECORD                                      
*                                                                               
IVDTNEW  DS    0H                  CHANGE IN INVOICE  DETAIL KEY                
*                                                                               
         CLC   IVHDR,SPACES        IF                                           
         BNH   IVDTN10                FIRST TIME                                
*                                     NOT FIRST TIME                            
         LA    R2,IVDNETP          FORMAT NET                                   
         LA    R3,IVDNET                                                        
         BRAS  RE,ONET                                                          
         AP    WIVNETP,IVDNETP     ACCUMULATE REQUEST TOTALS                    
         AP    SPIVNET,IVDNETP     ACCUMULATE BATCH   TOTALS                    
*                                                                               
         LA    R2,IVDCOMP          FORMAT COM                                   
         LA    R3,IVDCOM                                                        
         BRAS  RE,OCOM                                                          
         AP    WIVCOMP,IVDCOMP     ACCUMULATE REQUEST TOTALS                    
         AP    SPIVCOM,IVDCOMP     ACCUMULATE BATCH TOTALS                      
*                                                                               
         LA    R2,IVDADJP          FORMAT ADJ                                   
         LA    R3,IVDADJ                                                        
         BRAS  RE,OADJ                                                          
         AP    WIVADJP,IVDADJP     ACCUMULATE REQUEST TOTALS                    
         AP    SPIVADJ,IVDADJP     ACCUMULATE BATCH TOTALS                      
*                                                                               
         LA    R2,IVDGRSP          FORMAT GR0SS                                 
         LA    R3,IVDGRS                                                        
         BRAS  RE,OGRS                                                          
         AP    WIVGRSP,IVDGRSP     ACCUMULATE REQUEST TOTALS                    
         AP    SPIVGRS,IVDGRSP     ACCUMULATE BATCH TOTALS                      
*                                                                               
         CLI   SPLTAPE,C'Y'       SKIP IF NOT PRODUCING TAPE                    
         BE    *+8                                                              
         CLI   SPLTAPE,C'T'       SKIP IF NOT PRODUCING TAPE                    
         BNE   IVDTOPX                                                          
*                                                                               
         L     R3,AKRINVFL         POINT TO OUTPUT TAPE DCB                     
*                                                                               
*        OPEN TAPE IF NEEDED                                                    
*                                                                               
         CLI   SPINVSW,C'O'        SKIP IF TAPE ALREADY OPENED                  
         BE    IVDTOPX                                                          
*                                                                               
         OPEN  ((R3),OUTPUT)       OPEN DISK FILE                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SPINVSW,C'O'        INDICATE INVOICE  FILE OPENED                
*                                                                               
IVDTOPX  DS    0H                                                               
*                                                                               
*        BUILD INVOICE BATCH HEADER RECORD                                      
*                                                                               
         CLI   SPIHEAD,C'H'        SKIP IF HEADER ALREADY WRITTEN               
         BNE   IVDTHDRX                                                         
*                                                                               
         MVI   IBHREC,C' '         INIT INVOICE BATCH HEADER RECORD             
         MVC   IBHREC+1(255),IBHREC                                             
*                                                                               
         MVC   IBHRECID,=CL3'AI '  SET RECORD ID                                
         MVC   IBHSTART,=CL3'ADI'  SET SECONDARY RECORD ID                      
         MVC   IBHRECV,=CL10'NVOICE' SET REST OF RECORD ID                      
*                                                                               
         CLC   IBHCRDTE,SPACES     IF CREATION DATE NOT SET                     
         BH    *+8                                                              
         BRAS  RE,CRDTE               GO SET IT                                 
*                                                                               
         MVI   SPIHEAD,C'N'       INDICATE HEADER WRITTEN                       
*                                                                               
         CLI   SPLTAPE,C'Y'       SKIP IF NOT PRODUCING TAPE                    
         BE    *+8                                                              
         CLI   SPLTAPE,C'T'       SKIP IF NOT PRODUCING TAPE                    
         BNE   IVDTHDRX                                                         
*                                                                               
         LA    R5,IBHREC             WRITE RECORD TO FILE                       
         PUT   (R3),(R5)                                                        
*                                                                               
IVDTHDRX DS    0H                                                               
*                                                                               
         CLI   SPLTAPE,C'Y'       SKIP IF NOT PRODUCING TAPE                    
         BE    *+8                                                              
         CLI   SPLTAPE,C'T'       SKIP IF NOT PRODUCING TAPE                    
         BNE   IVDTTAP1                                                         
*                                                                               
         LA    R5,IVREC            POINT TO OUTPUT INVOICE  RECORD              
*                                                                               
         PUT   (R3),(R5)           PUT TO DISK FILE                             
*                                                                               
IVDTTAP1 DS    0H                                                               
*                                                                               
         AP    SPIBTCTR,=P'1'      BUMP RECORD COUNTER FOR THIS REC             
*                                                                               
IVDTTAPX DS    0H                                                               
*                                                                               
*        PRINT RECORD                                                           
*                                                                               
         LA    R4,P                ESTABLISH INVOICE  PRINT LINE                
         USING PILINED,R4                                                       
*                                                                               
         MVC   PIINVNM,IVHINVNO    BILL NUMBER                                  
         MVC   PIINVDT,IVHINVDT    BILL DATE                                    
         MVC   PISRCAG,IVHSRCAG    SOURCE AGENCY                                
         MVC   PIBLSTA+1(1),IVDBLSTA    BILLING STATUS                          
         MVC   PIEXPTP,IVDEXPTP    EXPENSE TYPE                                 
         MVC   PIPRDID,IVDPRDID    PRODUCT ID                                   
         MVC   PIESTID,IVDESTID    ESTIMATE ID                                  
         MVC   PICHGDT,IVDCHGDT    CHARGE DATE                                  
         MVC   PINET,IVDNET+4      NET                                          
         MVC   PICOM,IVDCOM+4      COMPENSATION                                 
         MVC   PIGRS,IVDGRS+4      GROSS                                        
         MVC   PICRAGY,IVDCRAGY    CREATIVE AGENCY                              
         MVC   PIERR,=CL11' '      ERRORS                                       
*                                                                               
*        ERROR ANALYSIS                                                         
*                                                                               
         MVI   ERRCD,0             CLEAR ERROR CODE                             
*                                                                               
         CLC   IVHBLAGY,SPACES     MUST HAVE BILLING AGENCY                     
         BH    *+8                                                              
         OI    ERRCD,ERREST                                                     
*                                                                               
         CLC   PIEXPTP,SPACES      MUST HAVE EXPENSE TYPE                       
         BH    *+8                                                              
         OI    ERRCD,ERREST                                                     
*                                                                               
         CLC   PIPRDID,SPACES      MUST HAVE PRODUCT ID                         
         BH    *+8                                                              
         OI    ERRCD,ERRPRD                                                     
*                                                                               
         CLI   ERRCD,0             DONE IF NO ERRORS                            
         BE    IVDTERRX                                                         
*                                                                               
         LA    R1,PIERR                                                         
*                                                                               
         TM    ERRCD,ERRAGY        CHECK FOR AGENCY IN ERROR                    
         BZ    *+14                                                             
         MVC   0(4,R1),=C'AGY,'                                                 
         LA    R1,4(R1)                                                         
*                                                                               
         TM    ERRCD,ERRPRD        CHECK FOR PRODUCT ERROR                      
         BZ    *+14                                                             
         MVC   0(4,R1),=C'PRD,'                                                 
         LA    R1,4(R1)                                                         
*                                                                               
         TM    ERRCD,ERREST        CHECK FOR ESTIMATE ERROR                     
         BZ    *+14                                                             
         MVC   0(3,R1),=C'EST'                                                  
         LA    R1,3(R1)                                                         
*                                                                               
         BCTR  R1,0                REMOVE TRAILING COMMA                        
         CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
*                                                                               
         MVI   ERRCD,0             RESET ERROR CODE                             
*                                                                               
IVDTERRX DS    0H                                                               
*                                                                               
         BRAS  RE,SPOOLIT          PRINT LINE                                   
*                                                                               
IVDTN10  DS    0H                                                               
*                                                                               
         CLI   SORTRID,C'1'        SKIP IF NOT AN INVOICE RECORD                
         BNE   *+16                                                             
         MVC   IVREC,SORTREC       SAVE NEW RECORD                              
         MVC   IVDPACK,SORTREC+IVDPACK-IVREC                                    
*                                                                               
INVDTLX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - CRDTE'                               
***********************************************************************         
*                                                                     *         
*        SET CREATION DATE AND TIME                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CRDTE    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(23,WORK)     SET YYYY-MM-DD                   
*                                  TRANSLATE TO MM/DD/YYYY                      
         MVI   WORK+10,C'/'        FOR TRANSLATE PURPOSES                       
         MVC   WORK+20(10),=X'05060A08090A00010203'                             
         TR    WORK+20(10),WORK    RE-ARRANGE DATE                              
         MVC   IBHCRDTE,WORK+20                                                 
*                                                                               
         TIME  DEC                 GET CURRENT TIME - PUS'HHMMSS.SS'            
*                                    RETURNED IN R0                             
         ZAP   FULL,=P'0'          INIT PACKED FIELD                            
         STCM  R0,14,FULL          SAVE P(HHMMSS0C)                             
*                                                                               
         ZAP   DUB,FULL            DOUBLE WORD FOR EDITING                      
*                                                                               
         AP    DUB,=P'600000'      ADD SIX HOURS                                
*                                                                               
         MVC   WORK(11),=X'40212020207A20207A2020' SET EDIT PATTERN             
         ED    WORK(11),DUB+3      PRINT C'  HH:MM:SS'                          
         MVC   IBHCRTIM,WORK+3     SET HH:MM:SS                                 
*                                                                               
CRDTEX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - WORKTSTG'                            
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKSTG  DS    0D                                                               
*        FIELD NAMES IN GFEST RECORD                                            
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - QNAMETB'                        
***********************************************************************         
*                                                                     *         
*        FIELD NAMES IN GFEST RECORD                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
QNAMETB  DS    0X                  TABLE OF GFEST FIELD NAMES/LENGTHS           
QDIVBD   DC    CL8'DIVBRNCD',AL1(L'PRTDIVBD)   DIVISION BRAND CODE              
QPRDCD   DC    CL8'PROD CD ',AL1(L'PRTPRDCD)   PRODUCT CODE                     
QGFNAT   DC    CL8'GF NTRL ',AL1(L'PRTGFNAT)   GF NATURAL                       
QGFSUB   DC    CL8'GFSBNTRL',AL1(L'PRTGFSUB)   GF SUB NATURAL                   
QBLAGY   DC    CL8'GFBILAGY',AL1(L'PRTBLAGY)                                    
QEXPTP   DC    CL8'GFEXPTYP',AL1(L'PRTEXPTP)                                    
QPRDID   DC    CL8'GFPRODID',AL1(L'PRTPRDID)                                    
QCRTAG   DC    CL8'GFCRTVAG',AL1(L'PRTCRAGY)                                    
QSRCAG   DC    CL8'GFSRCAGY',AL1(L'PRTSRCAG)                                    
QREQNM   DC    CL8'GFREQNUM',AL1(L'PRTREQNM)                                    
QTGTMK   DC    CL8'GFTGRMKT',AL1(L'PRTTGTMK)                                    
QDEAL    DC    CL8'GFDEALNO',AL1(L'PRTDEAL)                                     
QNAMENOQ EQU   (*-QNAMETB)/9       NUMBER OF ENTRIES IN TABLE                   
         DC    X'FF'               EOT                                          
         SPACE 2                                                                
DDKRINV  DC    CL8'KRINVFL '                                                    
DDKREST  DC    CL8'KRESTFL '                                                    
DSNKRINV DC    CL20'NETTAPE.NE0KRXX1'                                           
DSNKREST DC    CL20'NETTAPE.NE0KRXX2'                                           
TMPALLOC DC    XL6'000003000003'                                                
*                                                                               
PARMLST1 CALL  ,(DDKRINV,SPFKRINV),MF=L  GET GENERATION                         
PARMLST2 CALL  ,(DDKREST,SPFKREST),MF=L  GET GENERATION                         
LINKLST  LINKX EP=FRGETDSN,SF=L                                                 
*                                                                               
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,52,C'  KRAFT FOODS TAPE '                                     
         SSPEC H2,52,C'  ---------------- '                                     
         SSPEC H3,52,PERIOD                                                     
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
         GETEL (R5),DATADISP,ELCODE                                             
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - SPFAREA'                             
***********************************************************************         
*                                                                     *         
*        SPFAREA -  VALUES SAVED IN SPFUSER BETWEEN REQUESTS          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPFAREA  DS    0X                                                               
*                                                                               
SPFAGYCD DS    CL1                                                              
SPFTITLE DS    CL63                                                             
SPFTTLSB DS    CL36                                                             
SPFSPLID DS    CL3                                                              
*                                                                               
SPFKRINV DC    CL44' '             GENERATION NAME                              
SPFKREST DC    CL44' '             GENERATION NAME                              
*                                                                               
         DS    0D                  ALIGNMENT                                    
SPIBTCTR DS    PL8                 INVOICE  RECORD COUNTER                      
SPEBTCTR DS    PL8                 ESTIMATE RECORD COUNTER                      
*                                                                               
SPIBLAGY DS    CL8                 INVOICE  BILLING AGENCY                      
SPEBLAGY DS    CL8                 ESTIMATE BILLING AGENCY                      
*                                                                               
SPINVSW  DC    C' '               C'O' - KRINVFL TAPE OPENED                    
SPESTSW  DC    C' '               C'O' - KRESTFL TAPE OPENED                    
*                                                                               
SPIHEAD  DS    XL1                C'H' - HEADER TO BE PRINTED                   
SPEHEAD  DS    XL1                C'H' - HEADER TO BE PRINTED                   
*                                                                               
SPIVBTID DS    CL20                BATCH ID                                     
SPESBTID DS    CL20                BATCH ID                                     
*                                                                               
         DS    0D                  ALIGNMENT                                    
SPIVNET  DS    PL8                 BATCH INVOICE NET                            
SPIVCOM  DS    PL8                 BATCH INVOICE COMMISSION                     
SPIVADJ  DS    PL8                 BATCH INVOICE ADJUSTMENT                     
SPIVGRS  DS    PL8                 BATCH INVOICE GROSS                          
SPIVBLL  DS    PL8                 BATCH INVOICE PREVIOUSLY BILLED              
*                                                                               
SPESNET  DS    PL8                 BATCH ESTIMATE NET                           
SPESCOM  DS    PL8                 BATCH ESTIMATE COMMISSION                    
SPESADJ  DS    PL8                 BATCH ESTIMATE ADJUSTMENT                    
SPESGRS  DS    PL8                 BATCH ESTIMATE GROSS                         
SPESBLL  DS    PL8                 BATCH ESTIMATE PREVIOUSLY BILLED             
*                                                                               
SPFAREAL EQU   *-SPFAREA                                                        
*                                                                               
AGYID    DC    CL2' '              AGENCY ID                                    
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - EDIINV'                         
***********************************************************************         
*                                                                     *         
*         ALERT EDICT ABOUT INVOICE TAPE                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDIINV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        DIRECT OUTPUT TO CLASS G OF PRINT QUEUE                                
*                                                                               
         ICM   R1,15,TWAMASTC      POINT TO MASTC                               
*                                                                               
         ICM   R1,15,MCVREMOT-MASTD(R1) ESTABLISH REMOTE AREA                   
         USING REMOTED,R1                                                       
*                                                                               
         MVC   REMOTKEY(6),=C'KRTAPE' REPORT TITLE                              
         MVC   REMOTDST,=X'0011'   SEND TO SJR                                  
*                                                                               
         CLI   REMOTLPP,68         FORCE SOME CHANGE IN REMOTE AREA             
         BNE   *+12                 THIS SHOULD FORCE THE OPENING OF            
         MVI   REMOTLPP,66          A NEW REPORT                                
         B     *+8                                                              
         MVI   REMOTLPP,68                                                      
*                                                                               
         MVC   REMOTJID,=C'NKR'                                                 
*                                                                               
         MVI   REMOTCLS,C'G'       SET FOR CLASS G                              
*                                                                               
         CLI   PQSW,0              IF PRINT QUEUE NOT OPEN                      
         BNE   EDIVOPNX                                                         
*                                                                               
         GOTO1 OPENPQ                                                           
*                                                                               
EDIVOPNX DS    0H                                                               
*                                                                               
*        GENERATE EDICT PQ ENTRY                                                
*                                                                               
*        BUILD EDICT HEADER LINE                                                
*                                                                               
         MVC   P,SPACES            *HDR* CARD                                   
         LA    R1,P                                                             
*                                                                               
         MVC   4(5,R1),=C'*HDR*'                                                
         MVC   9(6,R1),=C'EDICT='                                               
*                                                                               
         MVC   15(2,R1),AGYID      AGENCY ID                                    
         MVC   17(3,R1),=C'INV'    FILE TYPE                                    
         MVC   20(1,R1),=C'N'      MEDIA                                        
         MVC   21(2,R1),TWAAGY     AGENCY ALPHA                                 
*                                                                               
         MVI   34(R1),C'W'         132 CHARS WIDE                               
         MVI   37(R1),C'D'         THIS IS A DATASET                            
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
*                                                                               
         MVC   P,SPACES            ++DDS TRN CARD                               
         MVC   P(14),=CL14'++DDS NKR  TRN'                                      
         MVC   P+9(2),TWAAGY                                                    
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
         MVC   P,SPACES            ++DDS DSN CARD                               
         MVC   P(5),=CL5'++DDS'                                                 
         MVC   P+11(3),=C'DSN'                                                  
         MVC   P+15(44),SPFKRINV   GENERATION NAME                              
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(5),=C'++DDS'                                                   
         MVC   P+11(8),=C'SUB CLA('                                             
*                                                                               
         MVC   P+19(2),AGYID       AGENCY ID                                    
         MVC   P+21(3),=C'INV'     FILE TYPE                                    
         MVC   P+24(1),=C'N'       MEDIA                                        
*                                                                               
         MVC   P+27(39),=C'),CHA(3),ACC(GXS),USE(U9999632),MOD(1)'              
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P+1(40),=C'*TAPE GENERATED, SENDING DATA VIA EDICT*'             
         MVI   P+43,X'5E'         MOVE IN SEMI COLON FOR EDICT SCAN             
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
EDIINVX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32077-KR TAPE - PRINT REPORT - EDIEST'                         
***********************************************************************         
*                                                                     *         
*         ALERT EDICT ABOUT ESTIMATE TAPE                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EDIEST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*                                                                               
*        DIRECT OUTPUT TO CLASS G OF PRINT QUEUE                                
*                                                                               
         ICM   R1,15,TWAMASTC      POINT TO MASTC                               
*                                                                               
         ICM   R1,15,MCVREMOT-MASTD(R1) ESTABLISH REMOTE AREA                   
         USING REMOTED,R1                                                       
*                                                                               
         MVC   REMOTKEY(6),=C'KRTAPE' REPORT TITLE                              
         MVC   REMOTDST,=X'0011'   SEND TO SJR                                  
*                                                                               
         CLI   REMOTLPP,68         FORCE SOME CHANGE IN REMOTE AREA             
         BNE   *+12                 THIS SHOULD FORCE THE OPENING OF            
         MVI   REMOTLPP,66          A NEW REPORT                                
         B     *+8                                                              
         MVI   REMOTLPP,68                                                      
*                                                                               
         MVC   REMOTJID,=C'NKR'                                                 
*                                                                               
         MVI   REMOTCLS,C'G'       SET FOR CLASS G                              
*                                                                               
         CLI   PQSW,0              IF PRINT QUEUE NOT OPEN                      
         BNE   EIESOPNX                                                         
*                                                                               
         GOTO1 OPENPQ                                                           
*                                                                               
EIESOPNX DS    0H                                                               
*                                                                               
*        GENERATE EDICT PQ ENTRY                                                
*                                                                               
*        BUILD EDICT HEADER LINE                                                
*                                                                               
         MVC   P,SPACES            *HDR* CARD                                   
         LA    R1,P                                                             
*                                                                               
         MVC   4(5,R1),=C'*HDR*'                                                
         MVC   9(6,R1),=C'EDICT='                                               
*                                                                               
         MVC   15(2,R1),AGYID      AGENCY ID                                    
         MVC   17(3,R1),=C'EST'    FILE TYPE                                    
         MVC   20(1,R1),=C'N'      MEDIA                                        
         MVC   21(2,R1),TWAAGY     AGENCY ALPHA                                 
*                                                                               
         MVI   34(R1),C'W'         132 CHARS WIDE                               
         MVI   37(R1),C'D'         THIS IS A DATASET                            
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'                                         
*                                                                               
         MVC   P,SPACES            ++DDS TRN CARD                               
         MVC   P(14),=CL14'++DDS NKR  TRN'                                      
         MVC   P+9(2),TWAAGY                                                    
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
         MVC   P,SPACES            ++DDS DSN CARD                               
         MVC   P(5),=CL5'++DDS'                                                 
         MVC   P+11(3),=C'DSN'                                                  
         MVC   P+15(44),SPFKREST   GENERATION NAME                              
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(5),=C'++DDS'                                                   
         MVC   P+11(8),=C'SUB CLA('                                             
*                                                                               
         MVC   P+19(2),AGYID       AGENCY ID                                    
         MVC   P+21(3),=C'EST'     FILE TYPE                                    
         MVC   P+24(1),=C'N'       MEDIA                                        
*                                                                               
         MVC   P+27(39),=C'),CHA(3),ACC(GXS),USE(U9999632),MOD(1)'              
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P+1(40),=C'*TAPE GENERATED, SENDING DATA VIA EDICT*'             
         MVI   P+43,X'5E'         MOVE IN SEMI COLON FOR EDICT SCAN             
*                                                                               
         GOTO1 VPRINT,DMCB,P-1,=C'BL01'   WRITE LINE                            
*                                                                               
EDIESTX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - DCBS'                                
***********************************************************************         
*                                                                     *         
*        DCBS                                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
KRINVFL  DCB   DDNAME=KRINVFL,DSORG=PS,LRECL=246,BLKSIZE=24600,        X        
               MACRF=(GM,PM),RECFM=FB,EODAD=LSTINVDN                            
*                                                                               
KRESTFL  DCB   DDNAME=KRESTFL,DSORG=PS,LRECL=236,BLKSIZE=23600,        X        
               MACRF=(GM,PM),RECFM=FB,EODAD=LSTESTDN                            
*                                                                               
LSTINVDN DS    0H                                                               
LSTESTDN DS    0H                                                               
*                                                                               
SAVVALSL EQU   *-SPFAREA                                                        
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - GETPRD'                              
***********************************************************************         
*                                                                     *         
* GET GF PRODUCT CODES                                                *         
*                         THIS ROUTINE BUILDS (IN PRDTAB)             *         
*                         A TABLE OF PRODUCT CODES AND THE            *         
*                         INFO FROM THE GF ESTIMATE RECS              *         
*                                                                     *         
*        FIND ENTRY FOR PRODUCT IN TABLE OR BUILD ONE                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETPRD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC                                                          
         USING SPOOLD,R8                                                        
         USING NETSYSD,R9          SYSTEM SPECIFIC WORK                         
*                                                                               
         XC    APRTENT,APRTENT     INIT A(PRODUCT ENTRY)                        
*                                                                               
         CLI   SORTRID,C'1'        IF BILL RECORD                               
         BNE   *+10                                                             
         MVC   GETPRDSV,KEY           SAVE CURRENT KEY                          
*                                                                               
*        SEARCH TABLE FOR CURRENT PRODUCT                                       
*                                                                               
         L     R5,APRDTAB          POINT TO PRODUCT TABLE                       
         USING PRDTABD,R5          ESTABLISH ENTRY IN TABLE                     
*                                                                               
GETPRDLP DS    0H                                                               
*                                                                               
         CLI   PRTPCD,0            DONE AT END OF TABLE                         
         BE    GETPRDDN                                                         
*                                                                               
         CLC   PRTPCD,CURPRD       MATCH ON PRODUCT                             
         BE    GETPRDFD                                                         
*                                                                               
GETPRDCN DS    0H                                                               
*                                                                               
         LA    R5,PRTENTL(R5)       BUMP TO NEXT ENTRY IN TABLE                 
         B     GETPRDLP                                                         
*                                                                               
GETPRDDN DS    0H                                                               
*                                                                               
*        PRODUCT NOT IN TABLE - READ GFEST RECORD FOR PRODUCT                   
*                                                                               
         XC    KEY,KEY             READ GF ESTIMATE RECORD                      
         LA    R3,KEY                 WITH ESTIMATE=0                           
         USING PGESTD,R3                                                        
*                                                                               
         MVI   PGKRID,PGKNDIRQ     SET RECID                                    
         MVI   PGKSID,PGKNDISQ     SET SUB ID                                   
         MVC   PGKAM,NBACTAM       SET AGENCY/MEDIA                             
         MVC   PGKCLT,CURCLT       SET CLIENT                                   
         MVC   PGKPRD,CURPROD      SET PRODUCT                                  
*                                                                               
         NETGO NVSETSPT,DMCB       SET TO READ SPOT FILE                        
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(PGKEST-PGKEY),KEYSAVE   SKIP IF NO RECORD                    
         BNE   GETPRDX                                                          
*                                                                               
         ICM   R0,15,AIO           SAVE A(CURRENT I/OAREA)                      
         MVC   FILENAME,=C'SPTFIL  '                                            
         MVC   AIO,ANETWS3         READ INTO NETWS3                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   PRTPCD,CURPRD       SET PRODUCT CODE IN TABLE                    
*                                                                               
         L     R3,AIO              POINT TO FOUND RECORD                        
*                                                                               
         STCM  R0,15,AIO           RESTORE A(CURRENT I/OAREA)                   
*                                                                               
         LA    R3,PGKEDQ(R3)       SCAN ELEMENTS FOR                            
         SR    R0,R0               DIVISION/BRAND AND PRODUCT CODE              
         SR    RF,RF                                                            
*                                                                               
GTPFLDLP DS    0H                                                               
*                                                                               
         CLI   0(R3),0             DONE AT END OF RECORD                        
         BE    GTPFLDDN                                                         
*                                                                               
         CLI   0(R3),PGSTEIDQ      MUST BE PGEST ELEMENT                        
         BNE   GTPFLDCN                                                         
*                                                                               
         USING PGSTELMD,R3                                                      
*                                                                               
         LA    R1,QNAMETB          POINT TO FIRST FIELD NAME                    
         LA    RE,QNAMENOQ         NUMBER OF ENTRIES IN FIELD NAME TAB          
         LA    R4,PRTFIRST         POINT TO FIRST PRDTAB DATA FIELD             
*                                                                               
GTPNAMLP DS    0H                                                               
*                                                                               
         CLC   PGSTNAME,0(R1)      MATCH ON FIELD NAME                          
         BE    GTPNAMFD                                                         
*                                                                               
GTPNAMCN DS    0H                                                               
*                                                                               
         IC    RF,8(R1)            LENGTH OF DATA IN PRDTAB                     
         LA    R4,0(RF,R4)         NEXT ITEM IN PRDTAB                          
         LA    R1,9(R1)            NEXT NAME                                    
         BCT   RE,GTPNAMLP                                                      
*                                                                               
GTPNAMDN DS    0H                  DATA NOT FOUND                               
         B     GTPFLDCN            IGNORE                                       
*                                                                               
GTPNAMFD DS    0H                                                               
*                                                                               
         IC    RF,8(R1)            LENGTH OF DATA IN FIELD                      
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PGSTDATA    MOVE DATA TO TABLE                           
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    0(0,R4),SPACES      SPACE FILL                                   
*                                                                               
*        RIGHT JUSTIFY PRDID WITH LEADING ZEROS                                 
*                                                                               
         CLC   PGSTNAME,QPRDID    SKIP IF NOT PRODUCT ID                        
         BNE   GTPFLDCN                                                         
*                                                                               
*        FIELD ENDS AT FIRST NON-NUMERIC OR 10 BYTES IF FULL                    
*                                                                               
         IC    RF,8(R1)           NUMBER OF BYTES IN FIELD                      
         LA    RE,0(R4)           START OF DATA IN FIELD                        
*                                                                               
GTPPIDLP DS    0H                                                               
*                                                                               
         CLI   0(RE),C'0'         FIELD ENDS AT 1ST NON-NUMERIC                 
         BL    GTPPIDFD                                                         
*                                                                               
GTPPIDCN DS    0H                                                               
*                                                                               
         AHI   RE,1               NEXT BYTE                                     
         BCT   RF,GTPPIDLP                                                      
*                                                                               
GTPPIDDN DS    0H                                                               
*                                                                               
GTPPIDFD DS    0H                                                               
*                                                                               
         SR    RE,RE                                                            
         IC    RE,8(R1)           MAX FIELD LENGTH                              
         SR    RE,RF              TRUE FIELD LENGTH                             
         BNP   GTPPIDX            ERROR IF NO DATA                              
*                                                                               
         BCTR  RE,0               DECREMENT FOR EXECUTE                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)        PACK NUMBER                                   
*                                                                               
         OI    DUB+7,X'0F'        FORCE SIGN                                    
*                                                                               
         IC    RE,8(R1)           MAX FIELD LENGTH                              
         BCTR  RE,0               DECREMENT FOR EXECUTE                         
         SLL   RE,4               MOVE TO LEFT NYBBLE                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         UNPK  0(0,R4),DUB        ADD IN LEADING ZEROS                          
*                                                                               
GTPPIDX  DS    0H                                                               
*                                                                               
GTPFLDCN DS    0H                                                               
*                                                                               
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GTPFLDLP                                                         
*                                                                               
GTPFLDDN DS    0H                                                               
*                                                                               
         XC    PRTENTL(PRTENTL,R5),PRTENTL(R5) INIT NEXT ENTRY                  
*                                                                               
GETPRDFD DS    0H                  PROD FOUND IN TABLE                          
*                                                                               
         ST    R5,APRTENT          SAVE TABLE ENTRY POINTER                     
*                                                                               
GETPRDX  DS    0H                                                               
*                                                                               
         CLI   SORTRID,C'2'        IF DOING UNIT RECORDS                        
         BNE   GETPRDX1                                                         
*                                                                               
         NETGO NVSETUNT,DMCB          SET TO READ UNIT FILE                     
         XC    FILENAME,FILENAME      RESET FILE NAME                           
         MVI   NBFUNCT,NBFRDHI        SET TO RE-READ FILE POINTERS              
*                                                                               
         B     GETPRDXX                                                         
*                                                                               
GETPRDX1 DS    0H                                                               
*                                                                               
         NETGO NVSETSPT,DMCB       SET TO READ SPTFILE                          
         MVC   KEY,GETPRDSV        RESTORE INCOMING KEY                         
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                RE-READ SPRDIR POINTER                       
*                                                                               
GETPRDXX DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
GETPRDSV DS    XL(L'KEY)           CURRENT KEY SAVEAREA                         
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - GETEST'                              
***********************************************************************         
*                                                                     *         
* GET GF ESTIMATE CODES                                               *         
*                         THIS ROUTINE BUILDS (IN ESTTAB)             *         
*                         A TABLE OF ESTIMATE CODES AND THE           *         
*                         INFO FROM THE GF ESTIMATE RECS              *         
*                                                                     *         
*        FIND ENTRY FOR ESTIMATE IN TABLE OR BUILD ONE                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETEST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING GEND,RC                                                          
         USING SPOOLD,R8                                                        
         USING NETSYSD,R9          SYSTEM SPECIFIC WORK                         
*                                                                               
         XC    AETBENT,AETBENT     INIT A(ESTIMATE ENTRY)                       
*                                                                               
         CLI   SORTRID,C'1'        IF BILL RECORD                               
         BNE   *+10                                                             
         MVC   GETESTSV,KEY           SAVE CURRENT KEY                          
*                                                                               
*        SEARCH TABLE FOR CURRENT ESTIMATE                                      
*                                                                               
         L     R5,AESTTAB          POINT TO ESTIMATE TABLE                      
         USING ESTTABD,R5          ESTABLISH ENTRY IN TABLE                     
*                                                                               
GETESTLP DS    0H                                                               
*                                                                               
         CLI   ETBEST,0            DONE AT END OF TABLE                         
         BE    GETESTDN                                                         
*                                                                               
         CLC   ETBEST,CUREST       MATCH ON ESTIMATE                            
         BE    GETESTFD                                                         
*                                                                               
GETESTCN DS    0H                                                               
*                                                                               
         LA    R5,ETBENTL(R5)       BUMP TO NEXT ENTRY IN TABLE                 
         B     GETESTLP                                                         
*                                                                               
GETESTDN DS    0H                                                               
*                                                                               
*        ESTIMATE NOT IN TABLE - READ GFEST RECORD FOR ESTIMATE                 
*                                                                               
         XC    KEY,KEY             READ GF ESTIMATE RECORD                      
         LA    R3,KEY                 WITH ESTIMATE=0                           
         USING PGESTD,R3                                                        
*                                                                               
         MVI   PGKRID,PGKNDIRQ     SET RECID                                    
         MVI   PGKSID,PGKNDISQ     SET SUB ID                                   
         MVC   PGKAM,NBACTAM       SET AGENCY/MEDIA                             
         MVC   PGKCLT,CURCLT       SET CLIENT                                   
         MVC   PGKPRD,=C'POL'      SET PRODUCT TO POOL                          
         MVC   PGKEST,CUREST       SET ESTIMATE                                 
*                                                                               
         NETGO NVSETSPT,DMCB       SET TO READ SPOT FILE                        
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         MVC   FILENAME,=C'SPTDIR  '                                            
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(PGKEST-PGKEY+L'PGKEST),KEYSAVE SKIP IF NONE                  
         BNE   GETESTX                                                          
*                                                                               
         ICM   R0,15,AIO           SAVE A(CURRENT I/OAREA)                      
         MVC   FILENAME,=C'SPTFIL  '                                            
         MVC   AIO,ANETWS3         READ INTO NETWS3                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   ETBEST,CUREST       SET ESTIMATE CODE IN TABLE ENTRY             
*                                                                               
         L     R3,AIO              POINT TO FOUND RECORD                        
*                                                                               
         STCM  R0,15,AIO           RESTORE A(CURRENT I/OAREA)                   
*                                                                               
         LA    R3,PGKEDQ(R3)       POINT TO FIRST ENTRY IN RECORD               
         SR    R0,R0                                                            
         SR    RF,RF                                                            
*                                                                               
GTEFLDLP DS    0H                                                               
*                                                                               
         CLI   0(R3),0             DONE AT END OF RECORD                        
         BE    GTEFLDDN                                                         
*                                                                               
         CLI   0(R3),PGSTEIDQ      MUST BE PGEST ELEMENT                        
         BNE   GTEFLDCN                                                         
*                                                                               
         USING PGSTELMD,R3                                                      
*                                                                               
         LA    R1,QNAMETB          POINT TO FIRST FIELD NAME                    
         LA    RE,QNAMENOQ         NUMBER OF ENTRIES IN FIELD NAME TAB          
         LA    R4,ETBFIRST         POINT TO FIRST ESTTAB DATA FIELD             
*                                                                               
GTENAMLP DS    0H                                                               
*                                                                               
         CLC   PGSTNAME,0(R1)      MATCH ON FIELD NAME                          
         BE    GTENAMFD                                                         
*                                                                               
GTENAMCN DS    0H                                                               
*                                                                               
         IC    RF,8(R1)            LENGTH OF DATA IN ESTTAB                     
         LA    R4,0(RF,R4)         NEXT ITEM IN ESTTAB                          
         LA    R1,9(R1)            NEXT NAME                                    
         BCT   RE,GTENAMLP                                                      
*                                                                               
GTENAMDN DS    0H                  DATA NOT FOUND                               
         B     GTEFLDCN            IGNORE                                       
*                                                                               
GTENAMFD DS    0H                                                               
*                                                                               
         IC    RF,8(R1)            LENGTH OF DATA IN FIELD                      
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PGSTDATA    MOVE DATA TO TABLE                           
*                                                                               
GTEFLDCN DS    0H                                                               
*                                                                               
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GTEFLDLP                                                         
*                                                                               
GTEFLDDN DS    0H                                                               
*                                                                               
         XC    ETBENTL(ETBENTL,R5),ETBENTL(R5) INIT NEXT ENTRY                  
*                                                                               
GETESTFD DS    0H                  PROD FOUND IN TABLE                          
*                                                                               
         ST    R5,AETBENT          SAVE TABLE ENTRY POINTER                     
*                                                                               
GETESTX  DS    0H                                                               
*                                                                               
         CLI   SORTRID,C'2'        IF DOING UNIT RECORDS                        
         BNE   GETESTX1                                                         
*                                                                               
         NETGO NVSETUNT,DMCB          SET TO READ UNIT FILE                     
         XC    FILENAME,FILENAME      RESET FILE NAME                           
         MVI   NBFUNCT,NBFRDHI        SET TO RE-READ FILE POINTERS              
*                                                                               
         B     GETESTXX                                                         
*                                                                               
GETESTX1 DS    0H                                                               
*                                                                               
         NETGO NVSETSPT,DMCB       SET TO READ SPTFILE                          
         MVC   KEY,GETESTSV        RESTORE INCOMING KEY                         
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                RE-READ SPRDIR POINTER                       
*                                                                               
GETESTXX DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
GETESTSV DS    XL(L'KEY)           CURRENT KEY SAVEAREA                         
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - INIESTID'                            
***********************************************************************         
*                                                                     *         
*        INITIALIZE TABLE OF ESTIDS                                   *         
*                                                                     *         
*        INITIALIZE BXLE REGIOSTERS AND SAVE                          *         
*        CLEAR FIRST ENTRY IN TABLE                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INIESTID NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,=A(ESTIDTB)      POINT TO START OF TABLE                      
         LA    R4,L'IVDESTID       LENGTH OF ENTRY IN TABLE                     
         LR    R5,R3               SET TABLE AS EMPTY                           
         AHI   R5,-1                                                            
*                                                                               
         STM   R3,R5,EITBBXLE      SAVE STARTING BXLE REGISTERS                 
*                                                                               
         XC    0(L'IVDESTID,R3),0(R3) INIT FIRST ENTRY                          
*                                                                               
INESX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - ADDESTID'                            
***********************************************************************         
*                                                                     *         
*        ADD ESTID TO A TABLE                                         *         
*                                                                     *         
*NTRY    R2==> ESTID FOR TABLE                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ADDESTID NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LM    R3,R5,EITBBXLE      LOAD BXLE REGISTERS FOR ESTID TABLE          
*                                                                               
         AHI   R5,1                POINT TO NEXT OPENING IN TABLE               
*                                                                               
         MVC   0(L'IVDESTID,R5),0(R2)  ADD INCOMING TO TABLE                    
*                                                                               
         AR    R5,R4               BUMP TO NEXT AVAILABLE SLOT                  
         XC    0(L'IVDESTID,R5),0(R5) INIT NEXT ENTRY                           
*                                                                               
         AHI   R5,-1                                                            
*                                                                               
         STM   R3,R5,EITBBXLE      RE-SAVE BXLE REGISTERS                       
*                                                                               
ADESX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - GETESTID'                            
***********************************************************************         
*                                                                     *         
*        FIND ESTID IN TABLE                                          *         
*                                                                     *         
*NTRY    R2==> ESTID FOR TABLE                                        *         
*                                                                     *         
*        SINCE ESTID'S ARE SORTED WE CAN STOP SEARCH WHEN WE          *         
*        ARE PAST APPROPRIATE ENTRY IN TABLE                          *         
*                                                                     *         
*EXIT    NE CC MEANS NOT FOUND                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GETESTID NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LM    R3,R5,EITBBXLE      LOAD BXLE REGISTERS FOR ESTID TABLE          
*                                                                               
         CR    R5,R3               CHECK IF TABLE IS EMPTY                      
         BL    GTESX               YES - GET OUT                                
*                                                                               
         CLC   0(L'IVDESTID,R2),0(R3)  MATCH TO TABLE ENTRY                     
         BE    GTESX               FOUND        - GET OUT                       
         BXLE  R3,R4,*-10                                                       
*                                                                               
         LTR   RB,RB               SET NE CC                                    
*                                                                               
GTESX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - BLAGY'                               
***********************************************************************         
*                                                                     *         
*        BILLING AGENCY                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBLAGY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(L'PRTBLAGY,R2),SPACES   INITIALIZE                             
*                                                                               
         ICM   R5,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    IBLAGY10                                                         
*                                                                               
         USING ESTTABD,R5          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBBLAGY,R2),ETBBLAGY RETURN BILLING AGENCY CODE             
*                                                                               
         CLC   ETBBLAGY,SPACES     DONE IF FOUND                                
         BH    IBLAGY20                                                         
*                                                                               
IBLAGY10 DS    0H                                                               
*                                                                               
         ICM   R5,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    IBLAGY20                                                         
*                                                                               
         USING PRDTABD,R5          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTBLAGY,R2),PRTBLAGY RETURN BILLING AGENCY CODE             
*                                                                               
IBLAGY20 DS    0H                                                               
*                                                                               
         OC    0(L'PRTBLAGY,R2),SPACES   MAKE PRINTABLE                         
*                                                                               
         CLC   =C'EXCLUDE',0(R2)   DROP IF BEING EXCLUDED                       
         BNE   *+10                                                             
         LTR   RB,RB               SET NEQ CC                                   
         B     IBLAGYX                                                          
*                                                                               
         CR    RB,RB               SET EQ CC                                    
*                                                                               
IBLAGYX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - BTID'                                
***********************************************************************         
*                                                                     *         
*        BATCH ID  -  CL10'BILLING AGENCY CODE'                       *         
*                     CL10'CREATION DATE'                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBTID    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(L'SPIVBTID,R2),0(R3)       SET BATCH ID                        
*                                                                               
IBTIDX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRITER - BUILD INVOICE NUMBER - INVNO'                        
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD INVOICE NUMBER                              *         
*                                                                     *         
*NTRY     -  INVOICE DATE YYMMDD - BINARY                             *         
*            INVOICE NUMBER  2 BYTES BINARY                           *         
*                                                                     *         
*EXIT    7 BYTE INVOICE NUMBER                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
IINVNO   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,AIO              POINT TO FOUND RECORD                        
         USING BILLREC,R5                                                       
*                                                                               
         MVI   0(R2),C'N'          SET MEDIA TO NETWORK                         
         MVC   1(1,R2),NBSELMED    SET SUB-MEDIA                                
         MVI   2(R2),C'-'          SET DASH                                     
         GOTO1 DATCON,DMCB,BDATE,(23,WORK)   INVOICE DATE                       
         MVC   3(4,R2),WORK        SET YEAR                                     
         MVI   7(R2),C'-'          SET DASH                                     
         MVC   8(2,R2),BINVNO         INV=N/NN/NNNN                             
         MVI   10(R2),C'-'                                                      
         MVC   11(4,R2),BINVNO+2                                                
*                                                                               
IINVNOX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PRWRITER - BUILD INVOICE NUMBER - INVDT'                        
***********************************************************************         
*                                                                     *         
*        INVOICE DATE                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IINVDT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,AIO              POINT TO FOUND RECORD                        
         USING BILLREC,R5                                                       
*                                                                               
         GOTO1 DATCON,DMCB,BDATE,(23,0(R2))   RETURN DATE                       
*                                                                               
IINVDTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - SRCAG'                               
***********************************************************************         
*                                                                     *         
*        SOURCE AGENCY                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ISRCAG   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(L'PRTSRCAG,R2),SPACES   INITIALIZE                             
*                                                                               
         ICM   R5,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    ISRCAG10                                                         
*                                                                               
         USING ESTTABD,R5          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBSRCAG,R2),ETBSRCAG RETURN SOURCE AGENCY CODE              
*                                                                               
         CLC   ETBSRCAG,SPACES     DONE IF FOUND                                
         BH    ISRCAG20                                                         
*                                                                               
ISRCAG10 DS    0H                                                               
*                                                                               
         ICM   R5,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    ISRCAG20                                                         
*                                                                               
         USING PRDTABD,R5          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTSRCAG,R2),PRTSRCAG RETURN SOURCE AGENCY CODE              
*                                                                               
ISRCAG20 DS    0H                                                               
*                                                                               
         OC    0(L'PRTSRCAG,R2),SPACES   MAKE PRINTABLE                         
*                                                                               
ISRCAGX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - REVRS'                               
***********************************************************************         
*                                                                     *         
*        REVERSAL INDICATOR                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IREVRS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   0(R2),C'N'          ALWAYS C'N'                                  
*                                                                               
IREVRSX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - BLSTA'                               
***********************************************************************         
*                                                                     *         
*        BILLING STATUS                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBLSTA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   0(R2),C'M'          ALWAYS C'M'                                  
*                                                                               
IBLSTAX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - REQNM'                               
***********************************************************************         
*                                                                     *         
*        REQUIREMENT NUMBER                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IREQNM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R1,=A(IDESCUSR)     POINT TO ESTIMATE UDEF FLD                   
         MVC   0(L'EUSER1,R2),0(R1) RETURN UDEF FIELD                           
         OC    0(20,R2),SPACES     MAKE PRINTABLE                               
*                                                                               
         B     IREQNMX             DONE                                         
*                                                                               
*        NO LONGER USED                                                         
*                                                                               
         MVC   0(L'PRTREQNM,R2),SPACES   INITIALIZE                             
*                                                                               
         ICM   R5,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    IREQNM10                                                         
*                                                                               
         USING ESTTABD,R5          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBREQNM,R2),ETBREQNM RETURN SOURCE AGENCY CODE              
*                                                                               
         CLC   ETBREQNM,SPACES     DONE IF FOUND                                
         BH    IREQNM20                                                         
*                                                                               
IREQNM10 DS    0H                                                               
*                                                                               
         ICM   R5,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    IREQNM20                                                         
*                                                                               
         USING PRDTABD,R5          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTREQNM,R2),PRTREQNM RETURN SOURCE AGENCY CODE              
*                                                                               
IREQNM20 DS    0H                                                               
*                                                                               
         OC    0(L'PRTREQNM,R2),SPACES   MAKE PRINTABLE                         
*                                                                               
IREQNMX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - TGTMK'                               
***********************************************************************         
*                                                                     *         
*        TARGET MARKET CODE                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ITGTMK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    0(L'PRTTGTMK,R2),SPACES   INITIALIZE                             
*                                                                               
         ICM   R5,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    ITGTMK10                                                         
*                                                                               
         USING ESTTABD,R5          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBTGTMK,R2),ETBTGTMK RETURN TARGET NMKT CODE                
*                                                                               
         CLC   ETBTGTMK,SPACES     DONE IF FOUND                                
         BH    ITGTMK20                                                         
*                                                                               
ITGTMK10 DS    0H                                                               
*                                                                               
         ICM   R5,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    ITGTMK20                                                         
*                                                                               
         USING PRDTABD,R5          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTTGTMK,R2),PRTTGTMK RETURN TARGET MKT CODE                 
*                                                                               
ITGTMK20 DS    0H                                                               
*                                                                               
         OC    0(L'PRTTGTMK,R2),SPACES   MAKE PRINTABLE                         
*                                                                               
ITGTMKX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - LINE#'                               
***********************************************************************         
*                                                                     *         
*        LINE NUMBER - ALWAYS ZERO                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ILINE#   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(10,R2),=10C'0'        ALWAYS ZERO                              
*                                                                               
ILINE#X  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - DESC'                                
***********************************************************************         
*                                                                     *         
*        DESCRIPTION                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IDESC    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(50,R2),=CL50' '   INIT WITH SPACES                             
*                                                                               
*        HAVE TO READ ESTIMATE HEADER                                           
*                                                                               
         CLI   SORTRID,C'1'        IF BILL RECORD                               
         BNE   *+10                                                             
         MVC   IDESCSAV,KEY           SAVE CURRENT KEY                          
*                                                                               
         XC    KEY,KEY             INIT ESTIMATE KEY                            
         LA    R4,KEY                                                           
         USING ESTHDR,R4                                                        
*                                                                               
         MVC   EKEYAM,NBACTAM      AGENCY/MEDIA                                 
         MVC   EKEYCLT,CURCLT      CLIENT                                       
         MVC   EKEYPRD,CURPROD     PRODUCT                                      
         MVC   EKEYEST,CUREST      ESTIMATE                                     
*                                                                               
         CLC   IDESCKEY,EKEY       SKIP IF ALREADY READ                         
         BE    IDESCEKX                                                         
*                                                                               
         MVC   IDESCKEY,EKEY       SAVE KEY                                     
*                                                                               
         ICM   R0,15,AIO           SAVE CURRENT I/OAREA                         
         MVC   AIO,ANETWS3         READ INTO NET WORKAREA 3                     
         NETGO NVSETSPT,DMCB       SET TO READ SPTFILE                          
         MVC   FILENAME,=CL8'SPTDIR'  READING FROM SPTFILE                      
*                                                                               
         GOTO1 HIGH                READ KEY                                     
*                                                                               
         CLC   EKEY,KEYSAVE        MUST FIND RECORD                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FILENAME,=CL8'SPTFIL'  READING FROM SPTFILE                      
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
         MVC   IDESCDSC,EDESC      SAVE ESTIMATE NAME                           
         MVC   IDESCUSR,EUSER1     SAVE UDEF FIELD                              
*                                                                               
         STCM  R0,15,AIO           RESTORE CURRENT I/OAREA                      
*                                                                               
         CLI   SORTRID,C'2'        IF DOING UNIT RECORDS                        
         BNE   IDESCK1                                                          
*                                                                               
         NETGO NVSETUNT,DMCB          SET TO READ UNIT FILE                     
         XC    FILENAME,FILENAME      RESET FILE NAME                           
         MVI   NBFUNCT,NBFRDHI        SET TO RE-READ FILE POINTERS              
*                                                                               
         B     IDESCEKX                                                         
*                                                                               
IDESCK1  DS    0H                                                               
*                                                                               
         NETGO NVSETSPT,DMCB       SET TO READ SPTFILE                          
         MVC   KEY,IDESCSAV        RESTORE INCOMING KEY                         
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                RE-READ SPRDIR POINTER                       
*                                                                               
IDESCEKX DS    0H                                                               
*                                                                               
         MVC   0(L'EDESC,R2),IDESCDSC   RETURN ESTIMATE NAME                    
*                                                                               
IDESCX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
IDESCKEY DS    XL(L'EKEY)          CURRENT  KEY                                 
IDESCSAV DS    XL(L'KEY)           INCOMING KEY                                 
IDESCDSC DS    XL(L'EDESC)         CURRENT  ESTIMATE DESCRIPTION                
IDESCUSR DS    XL(L'EUSER1)        CURRENT ESTIMATE UDEF                        
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - EXPTP'                               
***********************************************************************         
*                                                                     *         
*        EXPENSE TYPE                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IEXPTP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(L'PRTEXPTP,R2),SPACES   INITIALIZE                             
*                                                                               
         ICM   R5,RF,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    IEXPTP10                                                         
         USING ESTTABD,R5          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBEXPTP,R2),ETBEXPTP RETURN EXPENSE TYPE                    
*                                                                               
         CLC   ETBEXPTP,SPACES     DONE IF FOUND                                
         BH    IEXPTP20                                                         
*                                                                               
IEXPTP10 DS    0H                                                               
*                                                                               
         ICM   R5,15,APRTENT          POINT TO PRODUCT ENTRY IN TABLE           
         BZ    IEXPTP20                                                         
         USING PRDTABD,R5          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTEXPTP,R2),PRTEXPTP RETURN SOURCE AGENCY CODE              
*                                                                               
IEXPTP20 DS    0H                                                               
*                                                                               
         OC    0(L'PRTEXPTP,R2),SPACES   MAKE PRINTABLE                         
*                                                                               
IEXPTPX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - PRDID'                               
***********************************************************************         
*                                                                     *         
*        PRODUCT ID                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IPRDID   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(L'PRTPRDID,R2),SPACES   INITIALIZE                             
*                                                                               
         ICM   R5,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    IPRDID10                                                         
         USING ESTTABD,R5          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBPRDID,R2),ETBPRDID RETURN PRODUCT ID                      
*                                                                               
         CLC   ETBPRDID,SPACES     DONE IF FOUND                                
         BH    IPRDID20                                                         
*                                                                               
IPRDID10 DS    0H                                                               
*                                                                               
         ICM   R5,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    IPRDID20                                                         
         USING PRDTABD,R5          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTPRDID,R2),PRTPRDID RETURN SOURCE AGENCY CODE              
*                                                                               
IPRDID20 DS    0H                                                               
*                                                                               
         OC    0(L'PRTPRDID,R2),SPACES   MAKE PRINTABLE                         
*                                                                               
IPRDIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - ESTID'                               
***********************************************************************         
*                                                                     *         
*        ESTIMATE ID - CL3'CLT',CL3'PRD',CL3'EST'                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IESTID   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R3,R2               COPY INPUT POINTER                           
*                                                                               
         MVC   0(3,R3),NBSELCLI            RETURN CLIENT CODE                   
         LA    R3,3(R3)                    BUMP TO NEXT POSITION                
*                                                                               
         MVC   0(L'CURPROD,R3),CURPROD     RETURN PRD                           
         LA    R3,L'CURPROD(R3)            BUMP TO NEXT POSITION                
*                                                                               
         SR    RF,RF                                                            
         IC    RF,CUREST                                                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(3,R3),DUB        ESTIMATE NUMBER                               
*                                                                               
IESTIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - CHGDT'                               
***********************************************************************         
*                                                                     *         
*        CHARGE DATE - FIRST DAY OF BILLABLE MONTH                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ICHGDT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SORTRID,C'1'       IF PROCESSING BILLS                           
         BNE   ICHGDTIN                                                         
*                                                                               
         L     R5,AIO              POINT TO FOUND RECORD                        
         USING BILLREC,R5                                                       
*                                                                               
         MVC   FULL(2),BKEYYSRV    YM OF MONTH OF SERVICE                       
*                                                                               
         MVI   FULL+2,1            FORCE TO FIRST OF THE MONTH                  
*                                                                               
         GOTO1 DATCON,DMCB,(3,FULL),(23,0(R2))   YYYY-MM-DD                     
*                                                                               
         CLC   =C'1900',0(R2)                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
ICHGDTIN DS    0H                                                               
*                                                                               
ICHGDTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - NET'                                 
***********************************************************************         
*                                                                     *         
*        NET                                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INET     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SORTRID,C'2'        IF ESTIMATE PROCESSING                       
         BNE   INETESN                                                          
*                                                                               
         XC    DMCB(12),DMCB                                                    
         GOTO1 =V(NETACC),DMCB,(14,NEACCWRK),NETBLOCK     NETACT+INT            
*                                                                               
*        RESULTS RETURNED IN PL8WRK                                             
*                                                                               
         ZAP   0(8,R2),PL8WRK      NET                                          
*                                                                               
         B     INETX                                                            
*                                                                               
INETESN  DS    0H                                                               
*                                                                               
         L     R5,AIO                                                           
         USING BILLREC,R5          ESTABLISH BILL RECORD                        
*                                                                               
         ICM   RE,15,BNET          NET BILL AMOUNT                              
         CVD   RE,DUB                                                           
         ZAP   0(8,R2),DUB                                                      
*                                                                               
INETX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
ONET     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         EDIT  (P8,0(R2)),(16,0(R3)),2,FLOAT=-   PUT IN RECORD                  
*                                                                               
         CLC   L'IVDNET-4(4,R3),=C' .00'    IF  ZERO                            
         BNE   *+10                                                             
         MVC   L'IVDNET-4(4,R3),=C'0.00'    ADD ZERO                            
*                                                                               
ONETX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - COM'                                 
***********************************************************************         
*                                                                     *         
*        COMMISSION                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ICOM     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   0(8,R2),=P'0'                                                    
*                                                                               
         CLI   SORTRID,C'2'        DONE IF UNIT RECORD                          
         BE    ICOMX                                                            
*                                                                               
         CLC   =C'YN',TWAAGY       SKIP IF YNR                                  
         BE    ICOMX                                                            
*                                                                               
         L     R5,AIO                                                           
         USING BILLREC,R5          ESTABLISH BILL RECORD                        
*                                                                               
         ICM   RE,15,BNET          GET NET                                      
         ICM   RF,15,BACTUAL       COMMISSION                                   
         SR    RF,RE                                                            
         CVD   RF,DUB                                                           
         ZAP   0(8,R2),DUB                                                      
*                                                                               
ICOMX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
OCOM     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'YN',TWAAGY       SKIP IF YNR                                  
         BE    OCOMX                                                            
*                                                                               
         EDIT  (P8,0(R2)),(16,0(R3)),2,FLOAT=-   PUT IN RECORD                  
*                                                                               
         CLC   L'IVDCOM-4(4,R3),=C' .00'    IF  ZERO                            
         BNE   *+10                                                             
         MVC   L'IVDCOM-4(4,R3),=C'0.00'    ADD ZERO                            
*                                                                               
OCOMX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - ADJ'                                 
***********************************************************************         
*                                                                     *         
*        ADJUSTMENT = ZERO                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IADJ     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   0(8,R2),=P'0'      NO CASH DISCOUNT                              
*                                                                               
IADJX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - ADJ'                                 
***********************************************************************         
*                                                                     *         
*        ADJUSTMENT = MINUS CD                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
OADJ     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
OADJX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - GRS'                                 
***********************************************************************         
*                                                                     *         
*        GROSS                                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IGRS     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SORTRID,C'2'        IF UNIT RECORD                               
         BNE   IGRSEN                                                           
*                                                                               
         ZAP   PL8WRK,=P'0'                                                     
         XC    DMCB(12),DMCB                                                    
         GOTO1 =V(NETACC),DMCB,(4,NEACCWRK),NETBLOCK     GROSS + INTEG          
*                                                                               
*        RESULTS RETURNED IN PL8WRK                                             
*                                                                               
         ZAP   0(8,R2),PL8WRK      NET                                          
*                                                                               
         B     IGRSX                                                            
*                                                                               
IGRSEN   DS    0H                  BILL RECORD                                  
*                                                                               
         L     R5,AIO                                                           
         USING BILLREC,R5          ESTABLISH BILL RECORD                        
*                                                                               
         PACK  0(8,R2),BAMT        PACK GROSS                                   
*                                                                               
         NOP   IGRSX               FOR TESTING                                  
*                                                                               
IGRSX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OGRS     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         EDIT  (P8,0(R2)),(16,0(R3)),2,FLOAT=-   PUT IN RECORD                  
*                                                                               
         CLC   L'IVDGRS-4(4,R3),=C' .00'    IF  ZERO                            
         BNE   *+10                                                             
         MVC   L'IVDGRS-4(4,R3),=C'0.00'    ADD ZERO                            
*                                                                               
OGRSX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - CRAGY'                               
***********************************************************************         
*                                                                     *         
*        CREATIVE AGENCY                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ICRAGY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(L'PRTCRAGY,R2),SPACES   INITIALIZE                             
*                                                                               
         ICM   R5,15,AETBENT       POINT TO ESTIMATE ENTRY IN TABLE             
         BZ    ICRAGY10                                                         
         USING ESTTABD,R5          ESTABLISH ESTIMATE TABLE                     
*                                                                               
         MVC   0(L'ETBCRAGY,R2),ETBCRAGY RETURN CREATIVE AGENCY                 
*                                                                               
         CLC   ETBCRAGY,SPACES     DONE IF FOUND                                
         BH    ICRAGY20                                                         
*                                                                               
ICRAGY10 DS    0H                                                               
*                                                                               
         ICM   R5,15,APRTENT       POINT TO PRODUCT ENTRY IN TABLE              
         BZ    ICRAGY20                                                         
         USING PRDTABD,R5          ESTABLISH PRODUCT TABLE                      
*                                                                               
         MVC   0(L'PRTCRAGY,R2),PRTCRAGY RETURN CREATIVE AGENCY CODE            
*                                                                               
ICRAGY20 DS    0H                                                               
*                                                                               
         OC    0(L'PRTCRAGY,R2),SPACES   MAKE PRINTABLE                         
*                                                                               
ICRAGYX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - BKBLL'                               
***********************************************************************         
*                                                                     *         
*        BILLED AMOUNT - NOT PROVIDED                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBKBLL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ZAP   0(8,R2),=P'0'       NOT REPORTING BILLED AMOUNT                  
*                                                                               
IBKBLLX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
OBKBLL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
OBKBLLX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - DEAL#'                               
***********************************************************************         
*                                                                     *         
*        DEAL NUMBER   - NOT PROVIDED                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IDEAL#   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   0(10,R2),SPACES     NOT REPORTING BILLED AMOUNT                  
*                                                                               
IDEAL#X  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
*        KREST RECORD TABLES                                                    
*                                                                               
         DS    0D                 ALIGNMENT                                     
PRDTABC  DS    XL(32+255*PRTENTL) PRODUCT  CODE TABLE                           
         DS    0D                 ALIGNMENT                                     
ESTTABC  DS    XL(32+255*ETBENTL) ESTIMATE CODE TABLE                           
*                                                                               
ESTIDTB  DS    XL(1000*L'IVDESTID) ESTID TABLE                                  
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - WORKD'                               
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE DSECT                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WORKD    DSECT                     MYWORK AREA  ANETWS2                         
*                                                                               
RELO     DS    F                *  FROM EDIT MODULE                             
ACLISTSV DS    F                *                                               
TAPEOPT  DS    CL1              *                                               
BILLMON  DS    XL2              *  REQUESTED BILLING YMD                        
BDAT     DS    CL4              *  REQUESTED BILLING YYMMDD                     
WKEYMBIL DS    XL1                 MONTH OF BILLING                             
*                                                                               
DESTID   DS    CL10                DESTINATION ID                               
*                                                                               
AKRINVFL DS    A                   A(KRINVFL DCB IN SPFUSER)                    
AKRESTFL DS    A                   A(KRESTFL DCB IN SPFUSER)                    
*                                                                               
EITBBXLE DS    3A                  BXLE REGISTERS FOR ESTID TABLE               
*                                                                               
NUMPER   DS    F                                                                
AMONLIST DS    F                                                                
ANTWKTP  DS    F                                                                
ATABLE   DS    F                                                                
ADUPLIC  DS    F                                                                
COUNTER  DS    F                                                                
CDTOT    DS    F                   NUMBER OF COMP TOTALS                        
DOLLARS  DS    CL1                                                              
AGYCODE  DS    CL1                                                              
PREVCOMP DS    CL1                                                              
*                                                                               
         DS    0F                                                               
ASPFUSER DS    A                                                                
VDYNALLO DS    V                   V(DYNALLOC)                                  
APRDTAB  DS    A(APRDTAB)    PRODUCT TABLE                                      
APRTENT  DS    A                   A(ENTRY IN PRODUCT  TABLE)                   
AETBENT  DS    A                   A(ENTRY IN ESTIMATE TABLE)                   
AESTTAB  DS    A(AESTTAB)    ESTIMATE TABLE                                     
AESTZZT  DS    A(AESTZZT)    POL TABLE                                          
DRIVER   DS    A                  A(DRIVER)                                     
AMYIO    DS    A                                                                
AGLOBAL  DS    A                                                                
AMONTHS  DS    A                                                                
*                                                                               
TOTNET   DS    PL8                                                              
TOTCOM   DS    PL8                                                              
*                                  REQUEST TOTALS                               
WIVNETP  DS    PL8                 NET                                          
WIVCOMP  DS    PL8                 COMMISSION                                   
WIVADJP  DS    PL8                 ADJUSTMENT                                   
WIVGRSP  DS    PL8                 GROSS                                        
WIVBLLP  DS    PL8                 PREVIOULY BILLED                             
WESNETP  DS    PL8                 NET                                          
WESCOMP  DS    PL8                 COMMISSION                                   
WESADJP  DS    PL8                 ADJUSTMENT                                   
WESGRSP  DS    PL8                 GROSS                                        
WESBLLP  DS    PL8                 PREVIOULY BILLED                             
*                                                                               
AUTABLE  DS    A                   A(UNITS TABLE)                               
*                                                                               
ERRCD    DS    XL1                 ERROR BYTE                                   
ERRAGY   EQU   X'80'               AGENCY                                       
ERRPRD   EQU   X'40'               PRODUCT                                      
ERREST   EQU   X'08'               ESTIMATE                                     
*                                                                               
RIDSAVE  DS    CL1                 SORT RECORD ID SAVEAREA                      
*                                                                               
CUREST   DS    CL1                                                              
CURPRD   DS    CL1                                                              
CURCLT   DS    CL2                                                              
CURPROD  DS    CL3                                                              
COMPREV  DS    CL1                                                              
DIVPREV  DS    CL1                                                              
PRDPREV  DS    CL4                                                              
CPEPREV  DS    CL9                                                              
COMTYPE  DS    CL1                 COMMISSION BASE=B'NNNN0001'=NET              
COMPCT   DS    PL4                 COMMISSION PCT                               
NEACCWRK DS    CL1       FOR NETACC RETURN                                      
PL8WRK   DS    CL8       FOR NETACC RETURN                                      
PL16WRK  DS    CL16                WORK AREA                                    
TAPEDAT  DS    CL6                 TAPE CREATION DATE YYMMDD                    
HEADRTN  DS    CL1                                                              
TCOMSV   DS    PL10                                                             
TNETSV   DS    PL10                                                             
MYPAK    DS    PL10                                                             
BINDMCB  DS    6F                                                               
*                                                                               
*        TABLE OF TOTALS                                                        
*                                                                               
TOTALS   DS    0D                                                               
TOTAGYID DS    CL2       COMPANY ID                                             
TOLNET   DS    PL8                 NET                                          
TOLCOM   DS    PL8                 COM                                          
TOLNETX  DS    PL8                 NETX                                         
TOLCOMX  DS    PL8                 COMX                                         
TOLNETP  DS    PL8                 NETP                                         
TOLCOMP  DS    PL8                 COMP                                         
TOTLENQ  EQU   *-TOTALS                                                         
         DS    10XL(TOTLENQ)      10 MORE TOTALS                                
TOTMAXQ  EQU   (*-TOTALS)/TOTLENQ   NUMBER OF TOTALS                            
*                                                                               
REPTOT   DS    PL8        ALL REPORT TOTALS                                     
         DS    PL8                                                              
REPPNET  DS    PL8        NET-P                                                 
REPPCOM  DS    PL8        COM-P                                                 
REPXNET  DS    PL8        NET-X                                                 
REPXCOM  DS    PL8        COM-X                                                 
*                                                                               
*                                                                               
MONLIST  DS    CL96                4 BYTE COMPRESSED START-END                  
*                                                                               
MONYEAR  DS   0CL8                 2 BYTE Y/M + 6 BYTE MMM/YY                   
YRMOSTRT DS    CL8                                                              
         ORG   YRMOSTRT                                                         
         DS    CL192               8 X 24(MAX MON)                              
         ORG   *-8                                                              
YRMOEND  DS    CL8                                                              
*                                                                               
PERTYPE  DS    CL1                                                              
BOXSET   DS    CL1                                                              
MYBYTE   DS    CL1                                                              
*                                                                               
RECWORK  DS    CL132               WORK AREA FOR TAPE RECORDS                   
         DS    0D                  ALIGNMENT                                    
         DS    XL7                 FILLER                                       
SORTRID  DS    CL1                 RECORD ID                                    
*                                  C'I' - INVOICE                               
*                                  C'E' - ESTIMATE                              
SORTREC  DS    CL256               SO I CAN SEE SORTREC IN DUMPS                
SORTPACK DS    CL128               PACKED COUNTERS                              
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - INVOICE RECORDS - IBHREC'            
***********************************************************************         
*                                                                     *         
*        INVOICE BATCH HEADER RECORD                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBHREC   DS    0CL256          *** INVOICE BATCH HEADER RECORD                  
*                                                                               
IBHRECID DS    CL3'AI '            RECORD ID                                    
IBHSTART DS    CL3'ADI'            FURTHER RECORD ID                            
IBHRECV  DS    CL10'NVOICE'        MORE RECORD ID                               
IBHCRDTE DS    CL10                CREATION DATE                                
IBHCRTIM DS    CL8                 CREATION TIME                                
         DS    XL(L'IBHREC-(*-IBHREC)) SPARE                                    
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - INVOICE RECORDS - IVREC'             
***********************************************************************         
*                                                                     *         
*        INVOICE RECORD                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IVREC    DS    0CL256          *** INVOICE RECORD                               
*                                                                               
IVHDR    DS    0CL67               INVOICE HEADER                               
IVHBLAGY DS    CL8                 BILLING AGENCY CODE                          
IVHBTID  DS    CL20                BATCH ID                                     
IVHINVNO DS    CL20                INVOICE NUMBER                               
IVHINVDT DS    CL10                INVOICE DATE                                 
IVHSRCAG DS    CL8                 SOURCE AGENCY                                
IVHREVRS DS    CL1                 C'Y' - REVERSAL INVOICE                      
IVDTL    DS    0XL179              INVOICE DETAIL                               
IVDBLSTA DS    CL1                 BILLING STATUS                               
*                                  C'F' - FINAL                                 
*                                  C'P' - IN PROGRESS                           
*                                  C'O' - ORIGINAL                              
*                                  C'M' - MEDIA                                 
IVDLINE# DS    CL10                LINE NUMBER                                  
IVDDESC  DS    CL50                DESCRIPTION                                  
IVDEXPTP DS    CL6                 EXPENSE TYPE                                 
IVDPRDID DS    CL10                PRODUCT  ID                                  
IVDESTID DS    CL20                ESTIMATE ID                                  
IVDCHGDT DS    CL10                CHARGE DATE                                  
IVDNET   DS    CL16                NET                                          
IVDCOM   DS    CL16                COMMISSION                                   
IVDADJ   DS    CL16                OTHER ADJUSTMENT AMOUNT                      
IVDGRS   DS    CL16                GROSS                                        
IVDCRAGY DS    CL8                 CREATIVE AGENCY ID                           
*                                                                               
         DS    XL(L'IVREC-(*-IVREC)) SPARE                                      
*                                                                               
IVDPACK  DS    0XL(5*8)            PACKED BUCKETS                               
IVDNETP  DS    PL8                 NET                                          
IVDCOMP  DS    PL8                 COMMISSION                                   
IVDADJP  DS    PL8                 ADJUSTMENT                                   
IVDGRSP  DS    PL8                 GROSS                                        
IVDBLLP  DS    PL8                 PREVIOULY BILLED                             
IVDPACKX DS    0H                  END OF PACKED BUCKETS                        
*                                                                               
         DS    XL(128-(*-IVDPACK)) SPARE                                        
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - INVOICE RECORDS - IVCREC'            
***********************************************************************         
*                                                                     *         
*        INVOICE CONTROL RECORD                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IVCREC   DS    0CL256          *** INVOICE CONTROL RECORD                       
*                                                                               
IVCBLAGY DS    CL8                 BILLING AGENCY CODE                          
IVCBTID  DS    CL20                BATCH ID                                     
IVCRECCT DS    CL10                RECORD COUNT                                 
IVCNET   DS    CL16                NET                                          
IVCCOM   DS    CL16                COMMISSION                                   
IVCADJ   DS    CL16                OTHER ADJUSTMENT AMOUNT                      
IVCGRS   DS    CL16                GROSS                                        
IVCCRAGY DS    CL8                 CREATIVE AGENCY ID                           
*                                                                               
         DS    XL(L'IVCREC-(*-IVCREC)) SPARE                                    
*                                                                               
*                                                                               
IVCPACK  DS    0XL(5*8)            PACKED BUCKETS                               
IVCNETP  DS    PL8                 NET                                          
IVCCOMP  DS    PL8                 COMMISSION                                   
IVCADJP  DS    PL8                 ADJUSTMENT                                   
IVCGRSP  DS    PL8                 GROSS                                        
IVCBLLP  DS    PL8                 PREVIOULY BILLED                             
IVCPACKX DS    0H                  END OF PACKED BUCKETS                        
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - INVOICE RECORDS - IBTREC'            
***********************************************************************         
*                                                                     *         
*        INVOICE BATCH TRAILER RECORD                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
IBTREC   DS    0CL256          *** INVOICE BATCH TRAILER RECORD                 
*                                                                               
IBTRECID DS    CL3'AI '            RECORD ID                                    
IBTSTRT  DS    CL17'TTRL'          FURTHER RECORD ID                            
IBTRECCT DS    CL5                 RECORD COUNT                                 
*                                                                               
         DS    XL(L'IBTREC-(*-IBTREC)) SPARE                                    
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - ESTIMATE RECORDS - EBHREC'           
***********************************************************************         
*                                                                     *         
*        ESTIMATE BATCH HEADER RECORD                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EBHREC   DS    0CL256          *** ESTIMATE BATCH HEADER RECORD                 
*                                                                               
EBHRECID DS    CL3'AE '            RECORD ID                                    
EBHSTART DS    CL3'ADE'            FURTHER RECORD ID                            
EBHRECV  DS    CL10'STIMATE'       MORE RECORD ID                               
EBHCRDTE DS    CL10                CREATION DATE                                
EBHCRTIM DS    CL8                 CREATION TIME                                
         DS    XL(L'EBHREC-(*-EBHREC)) SPARE                                    
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - ESTIMATE RECORDS - ESREC'            
***********************************************************************         
*                                                                     *         
*        ESTIMATE RECORD                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ESREC    DS    0CL256          *** ESTIMATE RECORD                              
*                                                                               
ESHDR    DS    0CL68               ESTIMATE HEADER                              
ESHBLAGY DS    CL8                 BILLING AGENCY CODE                          
ESHBTID  DS    CL20                BATCH ID                                     
ESHESTID DS    CL20                ESTIMATE NUMBER CLT/PRD/EST                  
ESHDESC  DS    CL50                ESTIMATE DESCRIPTION                         
ESHCHGDT DS    CL10                CHARGE DATE - NOT NEEDED                     
ESHBLSTA DS    CL1                 BILLING STATUS                               
*                                  C'F' - FINAL                                 
*                                  C'P' - IN PROGRESS                           
*                                  C'O' - ORIGINAL                              
*                                  C'M' - MEDIA                                 
ESHREQNM DS    CL20                REQUIREMENT NUMBER                           
ESHTGTMK DS    CL1                 TARGET MARKET CODE                           
*                                                                               
ESDTL    DS    0XL178              ESTIMATE DETAIL                              
ESDEXPTP DS    CL6                 EXPENSE TYPE                                 
ESDPRDID DS    CL10                PRODUCT  ID                                  
ESDNET   DS    CL16                NET                                          
ESDCOM   DS    CL16                COMMISSION                                   
ESDADJ   DS    CL16                OTHER ADJUSTMENT AMOUNT                      
ESDGRS   DS    CL16                GROSS                                        
ESDBLL   DS    CL16                BILLED SO FAR - NOT NEEDED                   
ESDDEAL# DS    CL10                DEAL NUMBER                                  
*                                                                               
         DS    XL(L'ESREC-(*-ESREC)) SPARE                                      
*                                                                               
ESDPACK  DS    0XL(5*8)            PACKED BUCKETS                               
ESDNETP  DS    PL8                 NET                                          
ESDCOMP  DS    PL8                 COMMISSION                                   
ESDADJP  DS    PL8                 ADJUSTMENT                                   
ESDGRSP  DS    PL8                 GROSS                                        
ESDBLLP  DS    PL8                 PREVIOULY BILLED                             
ESDPACKX DS    0H                  END OF PACKED BUCKETS                        
*                                                                               
         DS    XL(128-(*-ESDPACK)) SPARE                                        
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - ESTIMATE RECORDS - IVCREC'           
***********************************************************************         
*                                                                     *         
*        ESTIMATE CONTROL RECORD                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ESCREC   DS    0CL256          *** ESTIMATE CONTROL RECORD                      
*                                                                               
ESCBLAGY DS    CL8                 BILLING AGENCY CODE                          
ESCBTID  DS    CL20                BATCH ID                                     
ESCRECCT DS    CL10                RECORD COUNT                                 
ESCNET   DS    CL16                NET                                          
ESCCOM   DS    CL16                COMMISSION                                   
ESCADJ   DS    CL16                OTHER ADJUSTMENT AMOUNT                      
ESCGRS   DS    CL16                GROSS                                        
ESCBLL   DS    CL16                BILLED                                       
*                                                                               
         DS    XL(L'ESCREC-(*-ESCREC)) SPARE                                    
*                                                                               
ESCPACK  DS    0XL(5*8)            PACKED BUCKETS                               
ESCNETP  DS    PL8                 NET                                          
ESCCOMP  DS    PL8                 COMMISSION                                   
ESCADJP  DS    PL8                 ADJUSTMENT                                   
ESCGRSP  DS    PL8                 GROSS                                        
ESCBLLP  DS    PL8                 PREVIOULY BILLED                             
ESCPACKX DS    0H                  END OF PACKED BUCKETS                        
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - ESTIMATE RECORDS - ISTREC'           
***********************************************************************         
*                                                                     *         
*        ESTIMATE BATCH TRAILER RECORD                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ESTREC   DS    0CL256          *** ESTIMATE BATCH TRAILER RECORD                
*                                                                               
ESTRECID DS    CL3'AE '            RECORD ID                                    
ESTSTRT  DS    CL17'TTRL'          FURTHER RECORD ID                            
ESTRECCT DS    CL5                 RECORD COUNT                                 
*                                                                               
         DS    XL(L'ESTREC-(*-ESTREC)) SPARE                                    
*                                                                               
         SPACE 2                                                                
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - ESTIMATE RECORDS - PELINED'          
***********************************************************************         
*                                                                     *         
*        ESTIMATE PRINT LINE DSECT                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PELINED  DSECT                    DSECT FOR PRINTED REPORT                      
PESTR    DS    CL1                                                              
PEESTID  DS    CL9                 ESTIMATE ID                                  
         DS    CL1                                                              
PEEDESC  DS    CL20                ESTIMATE DESCRIPTION                         
         DS    CL1                                                              
PEBLSTA  DS    CL4                 BILL STATUS                                  
         DS    CL1                                                              
PERQNUM  DS    CL20                REQUIREMENT NUMBER                           
         DS    CL1                                                              
PETGTMK  DS    CL3                 TARGET MARKET                                
         DS    CL1                                                              
PEEXPTP  DS    CL6                 EXPENSE TYPE                                 
         DS    CL1                                                              
PEPRDID  DS    CL10                PRODUCT ID                                   
         DS    CL1                                                              
PENET    DS    CL16                NET                                          
         DS    CL1                                                              
PEGRS    DS    CL16                GROSS                                        
         DS    CL1                                                              
PEERR    DS    CL11                ERROR CODE                                   
PEEND    DS    CL1                                                              
PELENE   EQU   *-PELINED                                                        
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - INVOICE RECORDS - PILINED'           
***********************************************************************         
*                                                                     *         
*        INVOICE  PRINT LINE DSECT                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PILINED  DSECT                    DSECT FOR PRINTED REPORT                      
PISTR    DS    CL1                                                              
PIINVNM  DS    CL15                INVOICE NUMBER                               
         DS    CL1                                                              
PIINVDT  DS    CL10                INVOICE DATE                                 
         DS    CL1                                                              
PISRCAG  DS    CL6                 SOURCE AGENCY                                
         DS    CL1                                                              
PIBLSTA  DS    CL4                 BILL STATUS                                  
         DS    CL1                                                              
PIEXPTP  DS    CL6                 EXPENSE TYPE                                 
         DS    CL1                                                              
PIPRDID  DS    CL10                PRODUCT ID                                   
         DS    CL1                                                              
PIESTID  DS    CL9                 ESTIMATE ID                                  
         DS    CL1                                                              
PICHGDT  DS    CL10                CHARGE DATE                                  
         DS    CL1                                                              
PINET    DS    CL12                NET                                          
         DS    CL1                                                              
PICOM    DS    CL10                COMPENSATION                                 
         DS    CL1                                                              
PIGRS    DS    CL12                GROSS                                        
         DS    CL1                                                              
PICRAGY  DS    CL6                 CREATIVE AGENCY                              
         DS    CL1                                                              
PIERR    DS    CL7                 ERROR CODE                                   
PIEND    DS    CL1                                                              
PILENE   EQU   *-PILINED                                                        
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - PRDTABD'                             
***********************************************************************         
*                                                                     *         
*        PRODUCT TABLE DSECT                                         *          
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRDTABD  DSECT                                                                  
*                                                                               
PRTENT   DS    0X                  ENTRY IN PRODUCT TABLE                       
*                                                                               
PRTPCD   DS    XL1                 PRODUCT CODE                                 
PRTFIRST DS    0X                  FIRST DATA FIELD                             
PRTDIVBD DS    CL2                 DIVISION BRAND CODE                          
PRTPRDCD DS    CL4                 PRODUCT CODE                                 
PRTGFNAT DS    CL3                 GF NATURAL                                   
PRTGFSUB DS    CL3                 GF SUB NATURAL                               
PRTBLAGY DS    CL8                 BILLING AGENCY                               
PRTEXPTP DS    CL6                 EXPENSE TYPE                                 
PRTPRDID DS    CL10                PRODUCT ID                                   
PRTCRAGY DS    CL8                 CREATIVE AGENCY                              
PRTSRCAG DS    CL8                 SOURCE  AGENCY                               
PRTREQNM DS    CL20                REQUIREMENT NUMBER                           
PRTTGTMK DS    CL1                 TARGET MARKETY                               
PRTDEAL  DS    CL10                DEAL NUMBER                                  
*                                                                               
PRTENTL  EQU   *-PRTENT            LENGTH OF ENTRY IN PRODUCT TABLE             
*                                                                               
         TITLE 'T32076 - KRTAPE CREATION - ESTTABD'                             
***********************************************************************         
*                                                                     *         
*        ESTIMATE TABLE DSECT                                        *          
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
ESTTABD  DSECT                                                                  
*                                                                               
ETBENT   DS    0X                  ENTRY IN ESTIMATE TABLE                      
*                                                                               
ETBEST   DS    CL1                 ESTIMATE CODE                                
ETBFIRST DS    0X                  FIRST DATA FIELD                             
ETBDIVBD DS    CL2                 DIVISION BRAND CODE                          
ETBPRDCD DS    CL4                 ESTIMATE CODE                                
ETBGFNAT DS    CL3                 GF NATURAL                                   
ETBGFSUB DS    CL3                 GF SUB NATURAL                               
ETBBLAGY DS    CL8                 BILLING AGENCY                               
ETBEXPTP DS    CL6                 EXPENSE TYPE                                 
ETBPRDID DS    CL10                ESTIMATE ID                                  
ETBCRAGY DS    CL8                 CREATIVE AGENCY                              
ETBSRCAG DS    CL8                 SOURCE  AGENCY                               
ETBREQNM DS    CL20                REQUIREMENT NUMBER                           
ETBTGTMK DS    CL1                 TARGET MARKETY                               
ETBDEAL  DS    CL10                DEAL NUMBER                                  
*                                                                               
ETBENTL  EQU   *-ETBENT            LENGTH OF ENTRY IN ESTIMATE TABLE            
*                                                                               
         EJECT                                                                  
         SPACE                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEWRIFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRID2D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE NEGENUNIT                                                      
         PRINT ON                                                               
BILHD    DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         EJECT                                                                  
       ++INCLUDE SPGENPGEST                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041NEWRI77   02/06/07'                                      
         END                                                                    
