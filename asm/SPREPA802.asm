*          DATA SET SPREPA802  AT LEVEL 165 AS OF 09/27/19                      
*PHASE SPA802A                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE DLFLD                                                                  
*INCLUDE DATVAL                                                                 
         TITLE 'SPA802 - NEW SPOTPAK TRIAL BALANCE'                             
*                                                                               
*    CHANGE LOG                                                                 
*                                                                               
* SMUR SPEC-34560   05/09/19 ADD ACC OFFICE TO DOWNLOAD (19.3)                  
*                                                                               
*                                                                               
*   BPLA 6/15    MORE CLIENTS FOR WI TO IGNORE PREVIOUS                         
*                BILLING IMBALANCES                                             
*                NOT SURE WHY THEY WEREN'T IN ORIGINAL LIST                     
*                ERRORS FLAGGED FOR MANY YEARS AND IGNORED                      
*                ONLY FOR PREVIOUS MONTHS - CURRENT ARE                         
*                ALWAYS FLAGGED                                                 
*                                                                               
*   BPLA 2/15    BIG FIX AND SOME DROPS                                         
*                                                                               
*   BPLA 4/13    SUPPORT FOR NEWTORK SUB-MEDIA V  (VOD)                         
*                                                                               
*   BPLA 1/11    DENOTE TRADE OR CALCULATED NET TRADE BILLS                     
*                "TR" WILL FOLLOW INVOICE # IN INVOICE LIST                     
*                                                                               
*   BPLA 8/09    USE A8 PROFILE READ FOR FIRST CLIENT                           
*                                                                               
*   BPLA 7/09    CHANGE FOR MORE BUYLINES AND NEW WAY TO                        
*                CHECK NETPACK UNIT'S STATION TYPE                              
*                USE NUSTATYP INSTEAD OF CURRENT NURSTAT                        
*                                                                               
*   BPLA 8/08    OPTION TO TRANSMIT DOWNLOAD OUTPUT                             
*                                                                               
*   BPLA 5/08    NEW OPTIONS TO CONTROL THE INCLUDING OF THE                    
*                CLIENT NAME AND FOR INCLUDING ACC OFFICE                       
*                FOR DOWNLOADS                                                  
*                                                                               
*   BPLA 9/06    NEW OPTION FOR DOWNLOADING TO SHOW END DAY                     
*                WITH MOS - D IN PROGPROF+14                                    
*                FOR SPOT AND SOME NET LAST DAY OF BROADCAST MONTH              
*                FOR REGULAR NET - LAST DAY OF CALENDAR MONTH                   
*                NEEDED DATVAL                                                  
*                MOVED STAEND AFTER EST2END DUE TO ADDRESSIBILITY               
*                PROBLEM                                                        
*                                                                               
*   BPLA   05/06     CHANGE TO HANDLE EXPANDED NET PRODUCTS                     
*                    INCLUDES NETBLOCKN (COPY OF PETER Z'S                      
*                    MADE 12/5/05                                               
*                                                                               
*   BPLA   11/05     CHANGES FOR NEW MANUAL STYLE NET 0E01 RECS                 
*                                                                               
*   BPLA   09/05     SUPPORT 2 CHARACTER OFFICES                                
*                                                                               
*   DEIS  04/05      SEND WARNING E-MAILS TO BRUCE AND ELENA (NOT DEIS)         
*                                                                               
*   BPLA  05/04      MORE DOWNLOAD CHGS + ABRE                                  
*                    + EVAN'S CANADIAN/CABLE CHG                                
*                                                                               
*   BPLA  04/04      DOWNLOAD BY SUB-MEDIA                                      
*                                                                               
*   DEIS  03/04      FIX FOR CABLE                                              
*                                                                               
*   DEIS  03/04      GET A(SPFMTINO) FROM SPONSOR (DON'T LINK IT IN)            
*                                                                               
*   BPLA  01/04      UNCLEARANCE LOGIC REVISION                                 
*                    WUNDERMAN CODE REMOVED                                     
*                                                                               
*   BPLA  01/04      SUMMARIES WHEN DOWNLOADING                                 
*                                                                               
*   BPLA  12/03      DOWNLOAD HEADERS FEATURE                                   
*                                                                               
*   BPLA  10/03      PROCESS UNCLEARANCES - USING CLEARANCE                     
*                    STATUS RECORDS                                             
*                                                                               
*   BPLA  09/03      SUPPRESS REQ DETAILS  - "NOREQUEST" CNTRL CARD             
*                                                                               
*   BPLA 03/04       DO EMAILS AT RUNLAST INSTEAD OF FOR EACH REQ               
*                                                                               
*   BPLA 10/02-02/03 DOWNLOAD FEATURES AND E-MAIL NOTIFICATIONS                 
*                    YEAR REPORTING FEATURE                                     
*                    DON'T LOAD NETIO NOR NETVAL                                
*                    USE MCNETPAK TO SEE IF NETPAK RUN                          
*                                                                               
*   BPLA   09/02  SEND OFFICE TOTALS TO EMFILE FOR ALL OFFICE REQS              
*                 ($* IN CLIENT)                                                
*                                                                               
*   DEIS   8/02   USE PL6 VALUES FOR GROSS, NET, AND ACTUAL                     
*                                                                               
*   BPLA   11/00  CHANGES FOR SEPARATE TOTALS REPORT                            
*                 USES EMAIL FILE                                               
*                                                                               
*   BPLA   7/00   CHANGE FOR NETWORK MEDIA BREAKOUT                             
*                 UNIT PROCESSSING MOVED TO ITS OWN CSECT                       
*                                                                               
*   BPLA   6/00   NO-OP CONSOLE MESSAGE AT END OF REQUEST                       
*                 THE ONE FOR THE CLIENT IS ENOUGH                              
*                                                                               
*   BPLA   5/00   CHANGE DOWNLOAD QOPT4 VALUE TO D                              
*                 AND USE QOPT4 = B TO SUPPRESS STA AND MOS                     
*                                                                               
*   BPLA   3/00   DISPLAY CONSOLE MESSAGE WHEN IMBALANCE IS DETECTED            
*                                                                               
*   BPLA  9/99    CHANGE FOR Y2K QAREA+49  (CURRENT MONTH)                      
*                 AND QEND (AS OF DATE)                                         
*                                                                               
*   BPLA  1/99    CHANGES TO SUPPORT NEW DOWNLOADED VERSION                     
*                 QOPT4 = "B"  SUPPRESS STA AND MOS                             
*                 AND PRODUCE SPECIAL DOWNLOAD OUTPUT                           
*                                                                               
*   BPLA 12/98    Y2K FIX TO NETIO BLOCK SETTING                                
*                                                                               
*   BPLA 3/98     GRANT NOW SAYS CANADIAN NETWORK IS LAST 6 BITS                
*                 OF 0E01 STATION INSTEAD OF LAST 5 BITS                        
*                                                                               
*   BPLA 5/97     CALLS TO OFFOUT NO-OPED                                       
*                 WHEN REACTIVATING CHANGE SAVCOFF TO CL2                       
*                 TBHDHK RESTORED TO ONE CHARACTER OFFICES                      
*                                                                               
*   BPLA 5/97      ADD FILENUM "K" TO LIST OF NET FILES                         
*                                                                               
*   SMYE 11/96     USE OFFOUT TO SHOW OFFICE (OFFICE LISTS)                     
*                  CHANGE DTCNV CALLS TO DATCON CALLS                           
*                                                                               
*   BPLA 7/96      EXPAND STABUCKC (USED BY NETIO) TO 3000 BYTES                
*                  FOR BIGGER NETPACK UNIT RECORDS                              
*                                                                               
*   BPLA 8/95      WHEN READING X'0E01' RECORDS SKIP THOSE WITH                 
*                  DATA IN STABKCUR                                             
*                                                                               
*   BPLA 2/95      DON'T USE MSUNPK FOR MANUAL BILLING X'0E01'S                 
*                   - IT WILL RETURN AN ERROR                                   
*                  MANUAL BILLING X'0E01'S HAVE X'D6B5A4' AS THEIR              
*                  PACKED STATION AND X'270E' (9998) AS THEIR MARKET            
*                                                                               
*   BPLA 8/94      CHANGES FOR CANADIAN NETWORK BILLING                         
*                  - USE NETTAB TO REPORT STATIONS                              
*                  VIA THEIR NETWORK                                            
*                                                                               
*   BPLA 6/94      USE SPFMTINO TO DISPLAY INVOICES                             
*                  ALSO DISPLAY CANADIAN 0E01 RECORDS                           
*                  BY THEIR NETWORK                                             
*                  AND SAVING OF NETIO AND NETVALUES                            
*                  MOVED TO RUNFRST                                             
*                                                                               
*   BPLA 3/94      NEW NETPAK FILE-FILENUM J                                    
*                                                                               
*   BPLA 1/94      CHANGES FOR PST REPORTING                                    
*                                                                               
*   BPLA 3/24/93   CHANGES TO HANDLE CABLE HEADENDS                             
*                                                                               
*   BPLA 10/29/92  DISPLAY SEP COMMISSION BILLING AS UFC INSTEAD OF COM         
*                                                                               
*   BPLA 7/8/92     USE SPBVAL ON BILLS AND X'0E' ELEMS                         
*                   ** NOTE - BAMT AND BNET GET ALTERED TO AMOUNTS              
*                             RETURNED BY SPBVAL                                
*                                                                               
*   BPLA 5/12/92    NO LONGER CHECK FOR BILLING DATE IN BUYS                    
*                   THIS FIELD IS NOW USED FOR $PAY SEQ NO.                     
*                                                                               
*   BPLA 1/24/91    ADD GST BILLED AND PAID ACCUMULATORS                        
*                                                                               
         EJECT                                                                  
*        REQUEST OPTIONS                                                        
*                                                                               
*        QOPT1-1    T=TRANSMIT (MUST BE DOWNLOADING USING QOPT5 =S)             
*                                                                               
*        QOPT1      1=SORT ON EST - NO EST TOTALS IN PRD SUMMARY                
*                   2=SAME AS 1 WITH EST TOTALS IN PRD SUMMARY                  
*                   3=BUYLINE DETAIL - DDS ONLY                                 
*                                                                               
*        QOPT2      Y=COMBINE ALL PRDS TOGETHER                                 
*                                                                               
*        QOPT3      Y=SHOW ALL STATIONS - DEFAULT IS ONLY STATIONS              
*                      WITH A BALANCE FORWARD OR CURRENT ACTIVITY               
*                                                                               
*        QOPT4      M=NO MTH OF SERV BREAKOUT                                   
*                   S= NO STATION BREAKOUT                                      
*                   B= SUPPRESS BOTH                                            
*                   D= SUPPRESS BOTH AND PRODUCE SPECIAL DOWNLOAD               
*                                                                               
*        QOPT5      Y=LIST CURRENT CLIENT INVOICES                              
*                   S=PRODUCE STATION DOWNLOAD                                  
*                                                                               
*        QOPT6      N=NETPAK ONLY                                               
*                   NO BUYS READ                                                
*                                                                               
*        QOPT7     Y=TRACE SORT CALLS (FOR TESTING)                             
*                  B=TRACE STATION BUFFALO CALLS                                
*                  N=OVERRIDE PROGPROF+8 TO "N" (OFFICE IN                      
*                    OFFICE LIST TOTALS)                                        
*                  C=OVERRIDE PROGPROF+8 TO "C" (CLIENT IN                      
*                    OFFICE LIST TOTALS)                                        
*                                                                               
*        QEND      OPT - ACTS AS AN 'AS OF DATE'                                
*                                                                               
*        QAREA+49  OPTIONAL - CURRENT MONTH                                     
*                  MAY CONTAIN NO OR NONE                                       
*                  MAY BE NOYY  (YY IS A YEAR)                                  
*                  IF AN 'AS OF' DATE IS ENTERED, ITS                           
*                  MONTH WILL BE USED AS THE END MONTH.                         
*                  THUS THE REPORT BE FOR JAN-END MONTH                         
*                  AS THE "YEAR".                                               
*                                                                               
*     ***NOTE***      QEST MUST BE SPECIFIED - NO FILTERS                       
*                     IF BLANK SET TO 001-255                                   
*                                                                               
************************************************************                    
*        NOTE FOR THE SUMMARY VERSIONS (QOPT4 TO B OR D)                        
*        QOPT2 SHOULD BE "Y"  (COMBINE PRODUCTS)                                
*                                                                               
************************************************************                    
         PRINT NOGEN                                                            
SPA802   CSECT                                                                  
         NMOD1 0,SPA802,RR=R9                                                   
*                                                                               
         ST    R9,TEMPRELO                                                      
         B     *+8                                                              
TEMPRELO DC    F'0'                                                             
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,RC,RA                                                    
         LA    R9,SPACEND                                                       
         USING SPA8WRKD,R9                                                      
         MVC   RELO,TEMPRELO                                                    
*                                                                               
         LA    R8,SPA802+4095                                                   
         LA    R8,1(R8)                                                         
         USING SPA802+4096,R8      ** NOTE USE OF R8 AS SECOND BASE **          
         CLI   MODE,REQFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,OFCFRST         FIRST FOR OFFICE                            
         BE    TBOFFF                                                           
         CLI   MODE,CLTFRST                                                     
         BE    TBCLTF                                                           
         CLI   MODE,PRDFRST                                                     
         BE    TBPRDF                                                           
         CLI   MODE,PROCBUY                                                     
         BE    TBBUY                                                            
         CLI   MODE,STAFRST                                                     
         BE    FBUYSTA                                                          
         CLI   MODE,STALAST                                                     
         BE    LBUYSTA                                                          
         CLI   MODE,REQLAST                                                     
         BE    TBAGY                                                            
         CLI   MODE,OFCLAST       LAST FOR OFFICE                               
         BE    TBOFF                                                            
         CLI   MODE,RUNLAST                                                     
         BE    RLAST                                                            
         CLI   MODE,RUNFRST                                                     
         BE    RFIRST                                                           
         CLI   MODE,CLTLAST                                                     
         BNE   EXIT                                                             
*                                                                               
CKMOD10  GOTO1 VCLTLAST,DMCB,(RC)                                               
         LA    R5,CGSTTOTS       ROLL CLIENT TO OFFICE                          
         LA    R6,OGSTTOTS                                                      
         BAS   RE,ROLLGST                                                       
*                                                                               
         LA    R5,CGSTTOTS       ROLL CLIENT TO REPORT                          
         LA    R6,RGSTTOTS                                                      
         BAS   RE,ROLLGST                                                       
*                                                                               
         LA    R6,CGSTTOTS      CLEAR CLIENT TOTALS                             
         BAS   RE,ZAPGST                                                        
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
RLAST    DS    0H                                                               
         CLI   DOWNIND,X'01'      DOWNLOAD RUN?                                 
         BNE   RLAST5                                                           
         CLI   RCREQREP,C'N'     REQ DETAILS SUPPRESSED?                        
         BNE   RLAST5            IF SO, AT RUNLAST                              
*                         I MUST CLOSE DOWNLOAD REPORT                          
         GOTO1 VDOWNLD,DMCB,(RC)                                                
*                                                                               
RLAST5   CLI   DDSIND,1                                                         
         BNE   EXIT                                                             
         BRAS  RE,SENDMAIL   SEND EMAIL NOTIFICATIONS                           
         B     EXIT                                                             
*                                                                               
RFIRST   DS    0H                                                               
         MVI   DDSIND,0                                                         
         MVI   DOWNIND,0                                                        
         MVI   DPAGEIND,0                                                       
         MVI   DHEADIND,0                                                       
         MVI   DTHDRIND,0                                                       
*                                                                               
         L     R1,VMASTC           ADDRESS OF MASTER CONTROLS                   
         USING MASTD,R1                                                         
         TM    MCPRTIND,MCPRTIRQ   SEE IF SUPPRESS REQ DETAILS                  
         BNO   *+8                                                              
         MVI   RCREQREP,C'N'                                                    
         DROP  R1                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
ZAPGST   LA    R1,8                                                             
ZAPG5    ZAP   0(8,R6),=P'0'                                                    
         LA    R6,8(R6)                                                         
         BCT   R1,ZAPG5                                                         
         BR    RE                                                               
*                                                                               
ROLLGST  LA    R1,8                                                             
ROLLG5   AP    0(8,R6),0(8,R5)                                                  
         LA    R6,8(R6)                                                         
         LA    R5,8(R5)                                                         
         BCT   R1,ROLLG5                                                        
         BR    RE                                                               
         EJECT                                                                  
**                                                                              
INITIAL  DS    0H                                                               
         XC    MYA8PROF,MYA8PROF                                                
         MVI   NETOPT,C'N'         SET FOR NEW NETPAK                           
*                                                                               
         L     R1,VMASTC                                                        
         USING MASTD,R1                                                         
         CLI   MCNETPAK,C'Y'       SEE IF NETPAK                                
         BE    INI0                                                             
*                                                                               
         DROP  R1                                                               
*                                                                               
         MVI   NETOPT,0                                                         
**                                                                              
*                                                                               
INI0     DS    0H                                                               
*                                                                               
         MVI   QETYPE,C'*'         SET TO PROCESS ALL ESTIMATES                 
*                                                                               
         CLI   QOPT5,C'S'          STATION DOWNLOAD?                            
         BNE   INIT0                                                            
*                                                                               
         CLI   QOPT1-1,C'T'        AND TRANSMITTING?                            
         BNE   INI4                                                             
*                                                                               
*  NOTE WHEN TRANSMITTING JCL SHOULD BE SET FOR NO REQUEST DETAILS              
*                                                                               
         CLI   DTHDRIND,1          HAVE I ALREADY SEND A *HDR?                  
         BE    INI5X                                                            
         MVC   P+4(5),=C'*HDR*'                                                 
         MVC   P+9(6),=C'EDICT='                                                
*                                                                               
         LA    R1,AGYTTAB            TABLE OF AGENCY/IDS TO TRANSMIT            
INI2     CLI   0(R1),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                MUST BE IN MY TABLE                          
         CLC   0(2,R1),QAGY                                                     
         BE    INI2B                                                            
         LA    R1,16(R1)                                                        
         B     INI2                                                             
*                                                                               
INI2B    MVC   P+15(6),2(R1)        SET EDICT RECORD CODE TO USER ID            
*                                                                               
         MVI   P+34,C'W'                                                        
         MVC   P2(5),=C'++DDS'                                                  
         MVC   P2+6(2),=C'SP'          SYSTEM                                   
         CLI   NETOPT,C'N'             NET?                                     
         BNE   *+10                                                             
         MVC   P2+6(2),=C'NE'                                                   
*                                                                               
         MVC   P2+8(3),=C'W8D'         REPORT                                   
         MVC   P2+15(3),=C'DA8'                                                 
*                                                                               
*        EDICT REPORT TYPE IS THUS S+W = SW    (FTP'S MUST BE W)                
*                             OR N+W = NW                                       
*                                                                               
         MVC   P2+11(3),=C'TRN'                                                 
         MVI   RCWHATPR,1          DEFAULT TO FIRST                             
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,100        NO HEADLINES - 100 DOESN'T EXIST             
         GOTO1 REPORT              SEND LINE                                    
*                                                                               
         MVC   P(5),=C'++DDS'                                                   
         MVC   P+11(3),=C'FTP'                                                  
         MVC   P+15(4),=C'NONE'                                                 
         CLC   QAREA+49(2),=C'NO'      CURRENT MONT GIVEN?                      
         BE    INI2C                                                            
         MVC   WORK(4),QAREA+49                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(9,WORK+10)                                 
         MVC   P+15(3),WORK+10               MMMYY                              
         MVC   P+18(2),WORK+14                                                  
         MVC   P+20(3),=C'NET'     (FOR NETPAK)                                 
         CLI   NETOPT,C'N'                                                      
         BE    *+10                                                             
         MVC   P+20(3),=C'SPT'    MUST BE SPOT                                  
*                                                                               
INI2C    DS    0H                                                               
         MVI   DTHDRIND,1          SO I WON'T RE-SEND                           
         MVI   RCWHATPR,1          DEFAULT TO FIRST                             
         MVI   RCSUBPRG,100        NO HEADLINES - 100 DOESN'T EXIST             
         GOTO1 REPORT              SEND LINE                                    
         B     INI5X                                                            
*                                                                               
INI4     DS    0H                                                               
         MVI   RCWHATPR,1           DEFAULT TO FIRST PRINTQ                     
*                                                                               
         CLI   QOPT5,C'S'           STATION DOWNLOAD?                           
         BNE   INIT0                                                            
         CLI   DPAGEIND,X'01'       HAVE I ALREADY SENT ONE?                    
         BE    INIT0                                                            
         MVI   FORCEHED,C'Y'        ENSURE NEW PAGE FOR DOWNLOAD                
         MVI   RCSUBPRG,100         EVEN IF NO DATA DOWNLOADED                  
         GOTO1 REPORT                                                           
*                                                                               
         CLI   RCREQREP,C'N'       SUPPRESSING REQ DETAILS?                     
         BNE   INIT00                                                           
*                                                                               
         MVI   FORCEHED,C'Y'        NEED TO SEND 2 PAGES (FOR PIANO)            
         MVI   RCSUBPRG,100         EVEN IF NO DATA DOWNLOADED                  
         GOTO1 REPORT                                                           
INIT00   MVI   DPAGEIND,X'01'      SO I WON'T REDO                              
INI5X    MVI   RCREQREP,C'N'       ALSO SET FOR NO REQ DETAILS                  
*                                  FOR SUBSEQUENT DOWNLOAD REQS                 
*                            NOTE  - THEY SHOULD ALL BE DOWNLOADS               
*                                    FOR THIS TO WORK PROPERLY                  
*                                                                               
INIT0    DS    0H                  SAVE FOR HEADHOOK                            
         L     RF,=A(SAVERC)                                                    
         ST    RC,0(RF)                                                         
         L     RF,=A(SAVERA)                                                    
         ST    RA,0(RF)                                                         
*                                                                               
         L     R0,=A(TBHDHK)                                                    
         A     R0,RELO                                                          
         ST    R0,HEADHOOK                                                      
         ST    R0,SAVHEADK        NEEDED WHEN DOWNLOADING                       
*                                                                               
         LA    R6,RGSTTOTS                                                      
         BAS   RE,ZAPGST                                                        
         LA    R6,OGSTTOTS                                                      
         BAS   RE,ZAPGST                                                        
         LA    R6,CGSTTOTS                                                      
         BAS   RE,ZAPGST                                                        
**                                                                              
         MVI   CANNOPT,0           CANADIAN NETWORK OPTION                      
         CLI   QMED,C'N'                                                        
         BNE   INIT2                                                            
         L     RF,ADAGY                                                         
         CLI   AGYPROF+7-AGYKEY(RF),C'C'                                        
         BNE   INIT2                                                            
         MVI   CANNOPT,C'C'                                                     
*                                                                               
*                                  NETPAK ONLY OPT NOW ALSO ON                  
*                                  PROFILE                                      
INIT2    OC    PROGPROF,PROGPROF   CK FOR PROFILE                               
         BZ    INIT8                                                            
*                                                                               
**NO-OP  MVC   MYA8PROF,PROGPROF   SAVE PROFILE FOR THE WHOLE REPORT            
*                                                                               
         LA    R2,QOPT1                                                         
         LA    R3,PROGPROF                                                      
         LA    R4,5                                                             
INIT5    CLI   0(R2),C' '          SEE IF OPTION REQUESTED                      
         BNE   *+10                YES                                          
         MVC   0(1,R2),0(R3)       GET VALUE FROM PROFILE                       
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,INIT5                                                         
         CLI   QOPT6,C' '          SEE IF QOPT6 SPECIFIED                       
         BNE   *+10                                                             
         MVC   QOPT6(1),PROGPROF+6    NO - GET VALUE FROM PROFILE               
*                                                                               
INIT8    MVI   FCRDBUYS,C'Y'       RESET TO READ BUYS                           
         CLI   NETOPT,C'N'         CAN ONLY BE FOR NEW NETPAK                   
         BNE   INIT10                                                           
         CLI   QOPT6,C'N'          MEANS READ NETPAK UNITS ONLY                 
         BNE   INIT10                                                           
         MVI   FCRDBUYS,C'N'       SET FOR NO BUYS                              
*                                                                               
INIT10   DS    0H                                                               
*                                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   MULTOFF,C'N'                                                     
*                                                                               
         LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         XC    MOSCACT(8),MOSCACT                                               
         L     RF,=A(TBCLTL)                                                    
         A     RF,RELO                                                          
         ST    RF,VCLTLAST                                                      
         L     RF,=A(DOWNLD)                                                    
         A     RF,RELO                                                          
         ST    RF,VDOWNLD                                                       
         L     RF,=A(PRINTIT)                                                   
         A     RF,RELO                                                          
         ST    RF,VPRINTIT                                                      
         L     RF,=V(DLFLD)                                                     
         A     RF,RELO                                                          
         ST    RF,VDLFLD                                                        
         L     RF,=A(PROCNET)                                                   
         A     RF,RELO                                                          
         ST    RF,VPROCNET                                                      
         L     RF,=A(BTOTALS)                                                   
         A     RF,RELO                                                          
         ST    RF,VBTOTS                                                        
         L     RF,=A(BLDNET)                                                    
         A     RF,RELO                                                          
         ST    RF,VBLDNET                                                       
         L     RF,=A(NETTAB)                                                    
         A     RF,RELO                                                          
         ST    RF,VNETTAB                                                       
         L     RF,=A(MAILTAB)                                                   
         A     RF,RELO                                                          
         ST    RF,VMAILTAB                                                      
         L     RF,=V(SORTER)                                                    
         A     RF,RELO                                                          
         ST    RF,SORTER                                                        
         L     RF,=V(DATVAL)                                                    
         A     RF,RELO                                                          
         ST    RF,VDATVAL                                                       
         L     RF,=A(STABUCKC)                                                  
         A     RF,RELO                                                          
         ST    RF,ADSTABUC                                                      
         L     RF,=A(PDTAB)                                                     
         A     RF,RELO                                                          
         ST    RF,APDTAB                                                        
         MVC   0(5,RF),=5X'FF'                                                  
         ST    RF,ANXTPD                                                        
         L     RF,=A(INVTAB)                                                    
         A     RF,RELO                                                          
         ST    RF,AINVTAB                                                       
         MVC   0(5,RF),=5X'FF'                                                  
         ST    RF,ANXTBL                                                        
         L     RF,=A(PRDTAB)                                                    
         A     RF,RELO                                                          
         ST    RF,APRDTAB                                                       
         L     RF,=A(NETBLK)                                                    
         A     RF,RELO                                                          
         ST    RF,ANETBLK                                                       
         L     RF,=A(WWIO)                                                      
         A     RF,RELO                                                          
         ST    RF,AWWIO                                                         
*********L     RF,=V(OFFOUT)                                                    
*********A     RF,RELO                                                          
*********ST    RF,VOFFOUT                                                       
         L     RF,=A(SORTCS)                                                    
         A     RF,RELO                                                          
         ST    RF,ASORTC                                                        
*                                                                               
         L     RE,ADCONLST                                                      
         USING SPADCONS,RE                                                      
         MVC   BRDMON,VBRDMON                                                   
         MVC   AFMTINO,VSPFMINO    A(SPFMTINO)                                  
         DROP  RE                                                               
*                                                                               
         MVI   PBERRSW,0                                                        
         MVI   CBERRSW,0                                                        
         MVI   ESTTBSW,0                                                        
         XC    ERRCLTS,ERRCLTS                                                  
         LA    RF,ERRCLTS                                                       
         ST    RF,ANXTECLT                                                      
         CLI   QOPT1,C'N'                                                       
         BNE   *+8                                                              
         MVI   QOPT1,C' '          SET 'N' TO SPACE                             
         CLI   QOPT1,C'1'          SEE IF SHOWING EST WITHIN MOS                
         BNE   *+8                                                              
         MVI   ESTTBSW,2                                                        
         CLI   QOPT1,C'2'          PRD SUMMARY BY EST WITHIN MOS                
         BNE   *+8                                                              
         MVI   ESTTBSW,X'06'                                                    
         CLI   QOPT1,C'3'          SORT BY LINE NUMBER- DDS ONLY                
         BNE   *+8                                                              
         MVI   ESTTBSW,X'0E'                                                    
         MVC   CMSTART(4),=X'0000FFFF'                                          
         MVC   CMSTARTB(6),=X'000000FFFFFF'                                     
         CLC   QAREA+49(4),SPACES                                               
         BE    INIT20              NO CURRENT MTH                               
         CLC   QAREA+49(2),=C'NO'                                               
         BNE   INIT15                                                           
         CLI   QAREA+51,C'0'     CHECK FOR NOYY  (YY IS YEAR)                   
         BL    INIT14                                                           
         MVC   WORK(2),QAREA+51     YEAR                                        
         MVC   WORK+2(2),=C'01'     JANUARY                                     
         MVC   WORK+8(2),=C'12'     THRU DECEMBER                               
         CLI   QEND,C' '            SEE IF 'AS OF' DATE GIVEN                   
         BE    *+10                                                             
         MVC   WORK+8(2),QEND+2     USE ITS MONTH INSTEAD                       
         MVC   WORK+6(2),QAREA+51   YEAR                                        
         B     INIT15B                                                          
*                                                                               
INIT14   MVC   QAREA+49(4),SPACES                                               
         B     INIT20                                                           
*                                                                               
INIT15   DS    0H                                                               
         MVC   WORK(4),QAREA+49                                                 
         MVC   WORK+6(4),WORK                                                   
INIT15B  MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK+10(2),=C'31'                                                
         GOTO1 DATCON,DMCB,WORK,(2,CMSTART)                                     
         GOTO1 (RF),(R1),WORK+6,(2,CMEND)                                       
         GOTO1 (RF),(R1),WORK,(3,CMSTARTB)                                      
         GOTO1 (RF),(R1),WORK+6,(3,CMENDB)                                      
*                                                                               
**Y2K**                                                                         
*        CONVERT BACK TO 6 BYTE SPECIAL FORMAT DATE                             
         GOTO1 (RF),(R1),(3,CMSTARTB),(0,WORK)                                  
         MVC   QAREA+49(4),WORK                                                 
         CLC   WORK+2(2),WORK+8      WOULD BE DIFFERENT FOR YEAR REQS           
         BE    *+10                                                             
         MVC   QAREA+51(2),SPACES     NOYY NOW CONVERTED TO YY                  
**Y2K**                                                                         
*                                                                               
INIT20   MVC   BQSTART(6),=X'000000FFFFFF'    SET TO PASS ALL BUYS              
         MVC   BQSTARTP(4),=X'0000FFFF'                                         
         CLI   QEND,C' '           SEE IF AS OF DATE SPECIFIED                  
         BE    INIT30              NO                                           
         GOTO1 DATCON,DMCB,(0,QEND),(3,BQEND)                                   
         GOTO1 (RF),(R1),(0,QEND),(2,BQENDP)    SAME DATCON PARAM'S             
**Y2K**                                                                         
*        CONVERT BACK TO 6 BYTE SPECIAL FORMAT DATE                             
         GOTO1 (RF),(R1),(3,BQEND),(0,WORK)                                     
         MVC   QEND(6),WORK                                                     
**Y2K**                                                                         
*                                                                               
INIT30   CLI   NETOPT,C'N'         SEE IF DOING NEW NETWORK                     
         BNE   INIT50                                                           
         BC    0,INIT50                                                         
         OI    *-3,X'F0'           ONLY DO ONCE                                 
         L     R4,ADBUY                                                         
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',NUFLIST,(R4)                      
         B     INIT50                                                           
*                                                                               
NUFLIST  DC    CL8'NUNTFIL'                                                     
         DC    CL8'NUNTDIR'                                                     
         DC    CL10'X'                                                          
*                                                                               
INIT50   CLC   QEST(6),SPACES                                                   
         BNE   INIT55                                                           
         MVC   QEST(6),=C'001255'                                               
         B     INIT60                                                           
INIT55   CLC   QESTEND,SPACES                                                   
         BNE   INIT60                                                           
         MVC   QESTEND,QEST      IF ONLY ONE EST SET QESTEND TO IT              
INIT60   PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STC   R0,MYBEST                                                        
         PACK  DUB,QESTEND                                                      
         CVB   R0,DUB                                                           
         STC   R0,MYBESTE          NEED TO SET NOW FOR TBCLTF                   
*                                  SPONSOR DOES IT LATER FOR                    
*                                  REQS FOR ONE PRD                             
INIT70   DS    0H                                                               
         CLI   QOPT4,C'D'             SPECIAL DOWNLOAD VERSION                  
         BE    INIT71                                                           
         CLI   QOPT5,C'S'             STATION DOWNLOAD                          
         BNE   INIT80                                                           
INIT71   DS    0H                                                               
         XC    HEADHOOK,HEADHOOK      MUST CLEAR HEADHOOK                       
         GOTO1 VDOWNLD,DMCB,(RC)                                                
         MVI   QOPT1,C'N'          NO ESTIMATE DETAIL WHEN DOWNLOADING          
*                                                                               
INIT80   CLI   QCLT,C'$'              IS THIS A REQUESTED OFFICE LIST           
         BNE   INITX                  NO: SKIP                                  
*                                                                               
         MVC   SAVMOL,QCLT+1          SAVE REQUESTED OFFICE LIST                
         CLI   QCLT+2,C' '            IS IT THE TWO CHARACTER VALUE             
         BH    INITX                  YES: NEED TO CONVERT                      
         XC    WORK,WORK              NO: GET PRINTABLE VALUE                   
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'S'       SYSTEM SPOT (NET USES SPOT)               
         MVC   OFFD.OFCAGY,SVAGY      AGENCY CODE                               
         MVC   OFFD.OFCMOL,QCLT+1     SINGLE CHARACTER OFFICE                   
         OI    OFFD.OFCINDS,OFCIMOLC  CALL OFFICER TO CONVERT                   
         L     RE,ADCONLST                                                      
         USING SPADCONS,RE                                                      
         L     RF,VOFFICER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,(C'2',WORK),(0,ACOMFACS)                               
         BNE   INITX                                                            
         MVC   SAVMOL,OFFD.OFCMOL2                                              
         DROP  OFFD                                                             
*                                                                               
INITX    B     EXIT                                                             
*                                      TABLE OF AGENCIES TRANSMITTING           
*                                      THEIR A8 DOWNLOAD FILES                  
*     FORMAT IS QAGY,EDICT RECORD CODE, LUID (SEE THEIR EDICT REC)              
*                                                                               
AGYTTAB  DC    C'WI',CL6'WILA  ',CL8'WIL1500I'                                  
         DC    X'FF'                   END OF TABLE                             
*                                                                               
         EJECT                                                                  
TBOFFF   DS    0H                  OFFICE FIRST                                 
         MVI   MULTOFF,C'Y'                                                     
         LA    R6,OGSTTOTS                                                      
         BAS   RE,ZAPGST                                                        
*                                                                               
*        SAVCOFF NOW SET IN TBCLTF                                              
*                                  CLEAR OFF ACCUMS                             
TBOFFF5  GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'05',BUFFBUFF),(X'80',1)                
         B     EXIT                                                             
**                                                                              
TBCLTF   DS    0H                  CLIENT FIRST                                 
*                                                                               
         OC    MYA8PROF,MYA8PROF  DO I HAVE ONE SAVED?                          
         BNZ   *+10                                                             
         MVC   MYA8PROF,PROGPROF  IF NOT,THEN SAVE THIS ONE FOR REPORT          
*                                                                               
         MVC   WKBCLT,BCLT        SAVE FOR CLEAR STATUS READING                 
*                                                                               
*        SET SAVCOFF HERE FOR ALL REQUESTS                                      
         L     RF,ADCLT                                                         
         MVC   SAVCOFF,SPACES                                                   
         MVC   SAVCOFF(1),COFFICE-CLTHDR(RF)  SAVE OFFICE FOR HEADLINES         
*                                                                               
         MVC   SAVAOFF,SPACES              ALSO SAVE ACC OFFICE                 
         MVC   SAVAOFF,CACCOFC-CLTHDR(RF)                                       
         CLC   SAVAOFF,SPACES                                                   
         BH    *+10                                                             
         MVC   SAVAOFF,SPACES                                                   
*                                                                               
         XC    WORK,WORK                                                        
OFFD     USING OFFICED,WORK                                                     
*                                                                               
         MVI   OFFD.OFCSYS,C'N'                                                 
         CLI   NETOPT,C'N'      SEE IF NETPAK                                   
         BE    *+8                                                              
         MVI   OFFD.OFCSYS,C'S' OTHERWISE SPOT                                  
*                                                                               
         MVC   OFFD.OFCAGY,SVAGY                                                
         MVC   OFFD.OFCOFC,COFFICE-CLTHDR(RF)                                   
*                                                                               
         L     RE,ADCONLST                                                      
         USING SPADCONS,RE                                                      
         L     RF,VOFFICER                                                      
         DROP  RE                                                               
         GOTO1 (RF),DMCB,(C'2',WORK),(0,ACOMFACS)                               
         CLI   0(R1),0                                                          
         BNE   TBCLTF1                                                          
         MVC   SAVCOFF,OFFD.OFCOFC2                                             
*                                                                               
         DROP  OFFD                                                             
*                                                                               
TBCLTF1  DS    0H                                                               
*                                                                               
         CLC   SAVCOFF,=X'0000'     IF STILL ZEROS, MAKE SPACES                 
         BNE   *+10                                                             
         OC    SAVCOFF,SPACES       NEEDED FOR CLIENTS WITH NO OFFICE           
*                                                                               
         CLI   QOPT5,C'S'             DOWNLOAD REQ?                             
         BNE   *+8                                                              
         OI    DOWNIND,X'01'                                                    
*                                                                               
         CLI   QOPT4,C'D'             OTHER KIND OF DOWNLOAD                    
         BNE   *+8                                                              
         OI    DOWNIND,X'01'                                                    
*                                   FIRST READ B1 AND B1X PROFILES              
*                                   NEEDED FOR SPFMTINO - INVOICE               
*                                   DISPLAY                                     
*                                                                               
         CLC   =C'DDS ',QUESTOR    DDS MONTH END REQUEST?                       
         BNE   *+8                                                              
         MVI   DDSIND,1           SET FOR RUNLAST EMAILS                        
*                                                                               
         XC    PROFB1,PROFB1             CLEAR B1 PROFILE                       
         XC    PROFB1X,PROFB1X                                                  
         XC    PROFA8A,PROFA8A                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0B1'          READ B1 PROFILE                        
         MVC   WORK+4(2),AGY                                                    
         MVC   WORK+6(1),MED                                                    
         MVC   WORK+7(3),CLT                                                    
         L     RF,ADCLT                                                         
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE-CLTHDR(RF)                                    
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,PROFB1,DATAMGR                                 
*                                                                               
         MVC   WORK(4),=C'SB1X'          READ SPECIAL PROFILE                   
         NI    WORK,X'BF'                FOR INVOICE FORMATTING                 
         GOTO1 GETPROF,DMCB,WORK,PROFB1X,DATAMGR                                
*                                                                               
*        PROFB1X+4 NOW HAS 'BASE' YEAR FOR MONTH DISPLAYS                       
*                                                                               
         MVC   WORK(4),=C'SA8A'          READ EXTENDED A8A PROFILE              
         NI    WORK,X'BF'                FOR MORE OPTIONS                       
         GOTO1 GETPROF,DMCB,WORK,PROFA8A,DATAMGR                                
*                                                                               
         LA    R6,CGSTTOTS                                                      
         BAS   RE,ZAPGST                                                        
*                                                                               
         MVI   BACTSW,0            INITIALIZE ACTUAL BILLING SW                 
         CLI   QMED,C'N'         SEE IF NETWORK                                 
         BNE   TBCLTF3                                                          
         CLI   NETOPT,C'N'         NETPAK                                       
         BNE   TBCLTF3             NO - DON'T GET BN PROFILE                    
         XC    WORK(50),WORK                                                    
         MVC   WORK+30(4),=C'SOBN'                                              
         MVC   WORK+34(3),QAGY  AGY MEDIA                                       
         MVC   WORK+37(3),CLT                                                   
         GOTO1 GETPROF,DMCB,(0,WORK+30),WORK,DATAMGR                            
         MVC   BACTSW,WORK+3                                                    
TBCLTF3  GOTO1 SORTER,DMCB,SORTC,SORTC+80,(68,ASORTC)                           
         MVI   SORTACT,0            INITIALIZE SORT ACTIVITY                    
         L     RF,APRDTAB                                                       
         MVC   0(3,RF),=3X'FF'                                                  
         ST    RF,ANXTPRD          INITIALIZE TABLE OF PRDS                     
*                                                                               
         CLI   CANNOPT,C'C'        SEE IF CANADIAN                              
         BNE   TBCLTF8                                                          
*                                  BUILD TABLE OF NETWORKS                      
*                                  SET SWITCH TO TRIGGER                        
*                                  NEW FEATURE - MAYBE I                        
*                                  COULD CHECK BILLING PROFILE?                 
*                                                                               
         GOTO1 VBLDNET,DMCB,(RC)                                                
TBCLTF8  DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         ZAP   PREBILLN,=P'0'                                                   
         ZAP   PREBILLG,=P'0'                                                   
         ZAP   CURBILLN,=P'0'                                                   
         ZAP   CURBILLG,=P'0'                                                   
         MVI   CINVSW,0                                                         
         MVI   AINVSW,0                                                         
         MVI   CMINVSW,0                                                        
         ZAP   AINVAMT,=P'0'                                                    
         ZAP   CMINVAMT,=P'0'                                                   
         ZAP   CINVNET,=P'0'                                                    
         ZAP   CINVGRS,=P'0'                                                    
         ZAP   CINVAMT,=P'0'                                                    
         ZAP   CINVGST,=P'0'                                                    
         ZAP   CINVPST,=P'0'                                                    
         MVI   RQPRDAAA,C'Y'                                                    
         CLC   QPRD,=C'POL'                                                     
         BE    TBCLTF9                                                          
         MVI   RQRDPOL,C'N'                                                     
*                                                                               
         CLI   QOPT4,C'B'          SUMMARY FORMAT -                             
         BE    TBCLTFX                                                          
         CLI   QOPT4,C'D'          SUMMARY FORMAT -                             
         BE    TBCLTFX             WITH DOWNLOAD                                
*                                                                               
         CLI   QOPT2,C'Y'          SEE IF COMBINING ALL PRDS                    
         BNE   TBCLTFX                                                          
         CLC   QPRD,=C'ALL'                                                     
         BNE   TBCLTFX                                                          
TBCLTF9  MVI   RQRDPOL,C'Y'        SET TO READ POL KEYS                         
*                                                                               
TBCLTFX  B     EXIT                                                             
         SPACE 2                                                                
TBPRDF   DS    0H                  PRODUCT FIRST                                
*                                                                               
         CLI   QOPT4,C'B'          SUMMARY FORMAT?                              
         BE    TBPRDF5                                                          
         CLI   QOPT4,C'D'          SUMMARY DOWNLOADED?                          
         BE    TBPRDF5                                                          
*                                                                               
         CLI   QOPT2,C'Y'          SEE IF COMBINING ALL PRDS                    
         BE    TBPRD5              YES                                          
TBPRDF5  L     R1,ANXTPRD                                                       
         MVC   0(3,R1),PRD                                                      
         MVC   3(20,R1),PRDNM                                                   
         LA    R1,23(R1)                                                        
         MVC   0(3,R1),=3X'FF'                                                  
         ST    R1,ANXTPRD                                                       
*                                                                               
TBPRD5   DS    0H                                                               
*                                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E01'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         JIF   RQRDPOL,EQ,C'Y',AND,BPRD,EQ,X'FF',TBCF3,JUMP=N                   
         MVC   KEY+5(1),BPRD                                                    
TBCF3    GOTO1 HIGH                                                             
         B     TBCF6                                                            
TBCF5    GOTO1 SEQ                                                              
TBCF6    CLC   KEY(5),KEYSAVE                                                   
         BNE   TBCF40                   END OF PRD                              
         JIF   RQRDPOL,EQ,C'Y',AND,BPRD,EQ,X'FF',TBCF8,JUMP=N                   
         CLC   KEY+5(1),KEYSAVE+5                                               
         BNE   TBCF40                                                           
*                                                                               
TBCF8    DS    0H                                                               
*                                                                               
         CLI   NETOPT,C'N'       SEE IF NETPAK                                  
         BE    TBCF9             THE ONLY OE01 RECORDS THAT EXIST               
*                                FOR NETPAK ARE FOR MANUAL BILLING              
*                                NET MANUAL BILLS MAY HAVE SOMETHING            
*                                HERE - IGNORE IT                               
         CLI   KEY+12,0          SKIP BUCKETS FOR NON-NATIVE CURRENCY           
         BNE   TBCF5                                                            
*                                SKIP ESTS OUT OF RANGE                         
TBCF9    CLC   KEY+6(1),MYBEST                                                  
         BL    TBCF5                                                            
         CLC   KEY+6(1),MYBESTE                                                 
         BH    TBCF5                                                            
TBCF12   L     R6,ADSTABUC                                                      
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         USING STABUCKD,R6                                                      
*                                                                               
         CLI   NETOPT,C'N'         SEE IF NETPAK                                
         BNE   TBCF12B                                                          
         CLI   BPRD,0              PROCESSING EXTENDED PRODUCT?                 
         BNE   TBCF12B                                                          
         CLC   STABKSTA,PRD        PRODUCTS MUST MATCH                          
         BNE   TBCF5                                                            
*                                                                               
TBCF12B  DS    0H                                                               
         XC    TBREC,TBREC                                                      
*                                                                               
         CLI   QOPT4,C'B'          SPECIAL SUMMARY REPORT                       
         BE    TBCF12C                                                          
         CLI   QOPT4,C'D'          SPECIAL SUMMARY REPORT                       
         BE    TBCF12C             WITH DOWNLOAD                                
         CLI   QOPT2,C'Y'          SEE IF DOING ALL PRDS TOGETHER               
         BE    *+10                                                             
TBCF12C  MVC   TBKPRD,PRD                                                       
         TM    ESTTBSW,2           SEE IF SORTING ON EST                        
         BZ    *+10                                                             
         MVC   TBKEST2,STABKEST                                                 
*                                                                               
         MVC   TBKSTA(3),CLT                                                    
         MVC   TBKSTA+3(3),PRD                                                  
         OC    TBKSTA,SPACES                                                    
         CLI   QOPT4,C'B'          SUPRESSING BOTH                              
         BE    TBCF22                                                           
         CLI   QOPT4,C'D'          SPECIAL DOWNLOAD FORMAT                      
         BE    TBCF22                                                           
*                                                                               
         MVC   TBKSTA,=CL8'ALL'                                                 
         CLI   QOPT4,C'S'          NO STATION BREAKOUT                          
         BE    TBCF22                                                           
         OC    KEYSAVE+9(3),KEYSAVE+9   FIRST TIME                              
         BZ    TBCF15                                                           
         CLC   KEY+9(3),KEYSAVE+9       SAME STA - ALPHA STATION                
         BE    TBCF20                   SAVED IN SVSTA                          
*                                                                               
TBCF15   DS    0H                                                               
         CLI   NETOPT,C'N'              IS THIS NETPAK?                         
         BE    TBCF15A                  THEN MUST BE MANUAL BILLING             
*                                       THOSE ARE THE ONLY 0E01'S               
*                                       THAT SHOULD EXIST FOR NET               
*                                       NEW ONES WILL HAVE THE                  
*                                       3 CHAR PRD IN THE STATION               
*                                                                               
         CLC   STABKSTA(3),=X'D6B5A4'   SPECIAL FOR MANUAL BILLING              
         BNE   TBCF15X                                                          
*                                                                               
TBCF15A  DS    0H                                                               
         MVC   SVTBSTA(8),=CL8'ZZZZX'                                           
         JIF   NETOPT,NE,C'N',OR,MYA8PROF+9,NE,C'Y',TBCF20,JUMP=N               
         MVI   SVTBSTA+7,C'N'      DEFAULT TO NETWORK                           
         CLI   STABSTYP,C' '      SEE IF SUBMEDIA PRESENT                       
         BNH   TBCF20                                                           
         MVC   SVTBSTA+7(1),STABSTYP                                            
         B     TBCF20                                                           
*                                                                               
TBCF15X  GOTO1 MSUNPK,DMCB,(X'80',STABKMKT),WORK,SVTBSTA                        
*                                                                               
         CLC   SVTBSTA+5(3),SPACES    SEE IF CABLE                              
         BE    TBCF17                                                           
         MVI   SVTBSTA+4,C'/'                                                   
*                                                                               
TBCF17   CLI   CANNOPT,C'C'        SEE IF CANADIAN NETWORK                      
         BNE   TBCF20                                                           
*                                                                               
         MVI   SVTBSTA+4,C' '                                                   
*                                                                               
         OC    STABKMKT,STABKMKT   SEE IF BILLED BY NETWORK                     
         BZ    TBCF20                                                           
*                                                                               
         CLI   MYA8PROF+7,C'Y'     SEE IF REPORTING                             
         BNE   TBCF20              CANADIAN BILLING BY NETWORK                  
         CLC   SVTBSTA+5(3),SPACES IS IT CABLE?                                 
         BE    TBCF17C             NO - SEARCH FOR NETWORK                      
*                                  NO NETWORK SEARCH FOR CABLE                  
         MVC   SVTBSTA+5(3),SPACES    CLEAR THIS PART                           
         B     TBCF20                                                           
*                                                                               
*                                  NEW NETWORK BILLIN BY STATION                
*                                  NETWORK IS IN LOW 5 BITS                     
*                                  OF THIRD STATION BYTE                        
TBCF17C  MVC   BYTE,STABKSTA+2     USE THIRD BYTE                               
         NI    BYTE,X'3F'          ONLY USE LOW 6 BITS                          
         ZIC   RF,BYTE                                                          
         MH    RF,=Y(NETTABL)                                                   
         A     RF,VNETTAB                                                       
         CLI   0(RF),0        NETWORK NOT FOUND                                 
         BNE   TBCF18                                                           
**                                                                              
*****    MVC   SVTBSTA(7),=C'UNKNOWN'                                           
         B     TBCF20           IF NOT FOUND - SHOW STATION                     
*                               INSTEAD OF "UNKNOWN"                            
*                                                                               
TBCF18   MVC   SVTBSTA,SPACES                                                   
         MVC   SVTBSTA(4),0(RF)                                                 
*                                                                               
TBCF20   MVC   TBKSTA,SVTBSTA                                                   
TBCF22   LA    R2,STABELEM                                                      
         DROP  R6                                                               
         MVI   ELCODE,X'0E'                                                     
         CLI   0(R2),X'0E'                                                      
         BE    TBCF30                                                           
TBCF25   BAS   RE,NEXTEL                                                        
         BNE   TBCF5               DONE - GO DO NEXT RECORD                     
*                                                                               
TBCF30   DS    0H                                                               
         USING STABELEM,R2                                                      
         CLC   STABBDT,BQENDP      SEE IF BILLED AFTER AS OF DATE               
         BH    TBCF25              YES - BYPASS                                 
         CLC   STABBDT,CMEND       SEE IF BILLED AFTER CURRENT MTH              
         BH    TBCF25              YES - BYPASS                                 
         CLI   QOPT4,C'B'          SUPPRESSING BOTH STA AND MOS                 
         BE    TBCF32                                                           
         CLI   QOPT4,C'D'          SUPPRESSING BOTH STA AND MOS                 
         BE    TBCF32              WITH DOWNLOAD                                
         CLI   QOPT4,C'M'          NO MTH OF SERVICE BREAKOUT                   
         BE    TBCF32                                                           
         MVC   TBKMOS,STABPER           MOS                                     
*                                                                               
TBCF32   NI    STABINV,X'FF'-(STABINV_REVERSAL+STABINV_REVERSED)                
         MVC   TBKINV,STABINV      INVOICE                                      
         GOTO1 DATCON,DMCB,(2,STABBDT),(0,WORK)                                 
         ZIC   R0,WORK+1           LAST DIGIT OF YEAR                           
         SLL   R0,4                                                             
         PACK  DUB,WORK+2(2)       MONTH                                        
         CVB   R1,DUB                                                           
         OR    R0,R1                                                            
         STC   R0,TBKINVMO                                                      
         MVC   TBBILDTE,STABBDT           BILLED DATE                           
*                                                                               
         GOTO1 SPBVAL,DMCB,(C'E',STABELEM),SPBVALD,0                            
         DROP  R2                                                               
*                                                                               
         ZAP   TBBILLG,SPBVGRSP                                                 
         ZAP   TBBILLN,SPBVNETP                                                 
         CLI   QOPT7,C'Y'          TRACING SORT CALLS                           
         BNE   *+14                                                             
         MVC   WORK(L'TBREC),TBREC                                              
         BAS   RE,SORTIN                                                        
         GOTO1 SORTER,DMCB,=C'PUT',TBREC                                        
         MVI   SORTACT,C'Y'             SET RECORD INPUT                        
         B     TBCF25                   GO DO NEXT ELEM                         
TBCF40   DS    0H                  PASS OLD UNREVERSED ORG BILLS                
*                                  TO SORT - WITH DUMMY STATION                 
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         JIF   RQRDPOL,EQ,C'Y',AND,BPRD,EQ,X'FF',TBCF42,JUMP=N                  
         MVC   KEY+4(3),PRD                                                     
TBCF42   GOTO1 HIGH                                                             
         B     TBCF46                                                           
TBCF45   GOTO1 SEQ                                                              
TBCF46   CLC   KEY(4),KEYSAVE                                                   
         BNE   TBCF60                                                           
         JIF   RQRDPOL,EQ,C'Y',AND,BPRD,EQ,X'FF',TBCF47,JUMP=N                  
         CLC   KEY+4(3),KEYSAVE+4        MATCH PRDS                             
         BNE   TBCF60                                                           
TBCF47   OC    KEY+8(5),KEY+8                                                   
         BZ    TBCF45                                                           
*                                        SKIP ESTS OUT OF RANGE                 
         CLC   KEY+7(1),MYBEST                                                  
         BL    TBCF45                                                           
         CLC   KEY+7(1),MYBESTE                                                 
         BH    TBCF45                                                           
         MVC   AREC,ADBILL                                                      
         GOTO1 GET                                                              
         L     R6,ADBILL                                                        
         USING BILLREC,R6                                                       
*                              MUST CLEAR BVATAMT FOR SMALL BILLS               
         CLC   BLEN,=H'90'                                                      
         BH    *+10                                                             
         XC    BVATAMT,BVATAMT                                                  
*                                                                               
         XC    BTOTPST,BTOTPST                                                  
*                                                                               
         CLI   QEND,C' '                                                        
         BE    *+14                                                             
         CLC   BDATE,QEND          SEE IF BILLED AFTER AS OF DATE               
         BH    TBCF45              YES - BYPASS                                 
         CLC   QAREA+49(4),SPACES                                               
         BE    TBCF49              NO CURRENT MTH                               
         CLC   QAREA+51(2),SPACES  DO I HAVE ONLY A YEAR?                       
         BNE   TBCF48                                                           
         CLC   BDATE(2),QAREA+49   JUST CHECK YEAR                              
         BH    TBCF45              BILLED AFTER CURRENT YEAR                    
         B     TBCF49                                                           
*                                                                               
TBCF48   CLC   BDATE(4),QAREA+49   SEE IF BILLED AFTER CURRENT MTH              
         BH    TBCF45              YES - BYPASS                                 
*                                                                               
TBCF49   DS    0H                                                               
*                                                                               
         GOTO1 SPBVAL,DMCB,(C'B',BILLREC),SPBVALD,0                             
         ZAP   BNETP,SPBVNETP      SET BNETP TO EFFECTIVE NET                   
         ZAP   BGRSP,SPBVGRSP                                                   
         MVC   BTOTPST,SPBVPST       TOTAL PST                                  
*                                                                               
         CLI   BTYPE,C'B'                                                       
         BE    TBCF50                   NEW BILL - BYPASS                       
         CLI   BTYPE+1,4                                                        
         BE    TBCF50                   DETAIL - BYPASS                         
         CLC   BCANINV(6),=6C'0'   SEE IF CANCELED                              
         BE    TBCF55                                                           
         CLI   QEND,C' '                                                        
         BE    *+14                                                             
         CLC   BCANDT(6),QEND      SEE IF CANCELLED AFTER AS OF DATE            
         BH    TBCF55              YES - CONSIDER UNREV                         
         CLC   QAREA+49(4),SPACES                                               
         BE    TBCF52              NO CURRENT MTH                               
         CLC   QAREA+51(2),SPACES     SEE IF I ONLY HAVE A YEAR                 
         BNE   TBCF49C                                                          
         CLC   BCANDT(2),QAREA+49      TEST REVERSED IN CURRENT YEAR            
         BH    TBCF55              HIGH - CONSIDER UNREV                        
         BL    TBCF50              LOW - BYPASS                                 
         B     TBCF52                                                           
*                                                                               
TBCF49C  CLC   BCANDT(4),QAREA+49      TEST REVERSED IN CURRENT MTH             
         BH    TBCF55              HIGH - CONSIDER UNREV                        
         BL    TBCF50              LOW - BYPASS                                 
         B     TBCF52                                                           
*                                                                               
TBCF50   DS    0H                                                               
         BAS   RE,POSTBILL                                                      
         B     TBCF45                                                           
*                                                                               
TBCF52   XC    TBREC,TBREC                                                      
         MVC   TBKSTA(5),=X'FFFFFFFF00'          CURRENT REVERSALS              
         CLI   QOPT4,C'D'                                                       
         BE    TBCF52A                                                          
         CLI   QOPT4,C'B'                                                       
         BNE   TBCF52C                                                          
TBCF52A  MVC   TBKSTA(3),CLT                                                    
         MVC   TBKSTA+3(3),PRD                                                  
         OC    TBKSTA,SPACES                                                    
*                                                                               
TBCF52C  DS    0H                                                               
         BAS   RE,POSTBILL                                                      
*                                                                               
         CLI   QOPT4,C'B'          SPECIAL SUMMARY REPORT                       
         BE    TBCF52E                                                          
         CLI   QOPT4,C'D'          SPECIAL SUMMARY REPORT                       
         BE    TBCF52E             WITH DOWNLOAD                                
         CLI   QOPT2,C'Y'          SEE IF DOING ALL PRDS TOGETHER               
         BE    *+10                                                             
TBCF52E  MVC   TBKPRD,PRD                                                       
         TM    ESTTBSW,2           SEE IF SORTING ON EST                        
         BZ    *+10                                                             
         MVC   TBKEST2,BKEYEST                                                  
         CLI   QOPT4,C'B'          SUPPRESSING BOTH STA AND MOS                 
         BE    TBCF53                                                           
         CLI   QOPT4,C'D'          SUPPRESSING BOTH STA AND MOS                 
         BE    TBCF53              WITH DOWNLOAD                                
         CLI   QOPT4,C'M'          NO MTH OF SERVICE BREAKOUT                   
         BE    TBCF53                                                           
         MVC   TBKMOS,BKEYYSRV                                                  
TBCF53   PACK  DUB,BINVNO+2(4)                                                  
         CVB   R0,DUB                                                           
         STCM  R0,3,TBKINV                                                      
******   STH   R0,TBKINV                                                        
         MVC   TBKINVMO,BKEYMBIL       BILLING Y/M - ONE BYTE                   
         GOTO1 DATCON,DMCB,(0,BDATE),(2,TBBILDTE)                               
         ZAP   TBBILLN,BNETP                                                    
         ZAP   TBBILLG,BGRSP                                                    
         CLI   QOPT7,C'Y'          TRACING SORT CALLS                           
         BNE   *+14                                                             
         MVC   WORK(L'TBREC),TBREC                                              
         BAS   RE,SORTIN                                                        
         GOTO1 SORTER,DMCB,=C'PUT',TBREC                                        
         MVI   SORTACT,C'Y'                                                     
         GOTO1 DATCON,DMCB,(0,BCANDT),(2,TBBILDTE)                              
         PACK  DUB,BCANINV+2(4)          USE CANCELLATION INV NUMBER            
         CVB   R0,DUB                                                           
         STCM  R0,3,TBKINV                                                      
*******  STH   R0,TBKINV                                                        
         ZIC   R0,BCANDT+1                                                      
         SLL   R0,4                                                             
         PACK  DUB,BCANDT+2(2)                                                  
         CVB   R1,DUB                                                           
         OR    R0,R1                                                            
         STC   R0,TBKINVMO                                                      
         ZAP   TBBILLN,BNETP       TO BE SUBTRACTED FROM CUR MTH BILLS          
         MP    TBBILLN,=P'-1'                                                   
         ZAP   TBBILLG,BGRSP                                                    
         MP    TBBILLG,=P'-1'                                                   
         CLI   QOPT7,C'Y'          TRACING SORT CALLS                           
         BNE   *+14                                                             
         MVC   WORK(L'TBREC),TBREC                                              
         BAS   RE,SORTIN                                                        
         GOTO1 SORTER,DMCB,=C'PUT',TBREC                                        
         MVI   SORTACT,C'Y'                                                     
         B     TBCF45                                                           
         SPACE 2                                                                
TBCF55   XC    TBREC,TBREC                                                      
         BAS   RE,POSTBILL                                                      
         CLI   QOPT4,C'B'          SPECIAL SUMMARY REPORT                       
         BE    TBCF55A                                                          
         CLI   QOPT4,C'D'          SPECIAL SUMMARY REPORT                       
         BE    TBCF55A             WITH DOWNLOAD                                
         CLI   QOPT2,C'Y'          SEE IF DOING ALL PRDS TOGETHER               
         BE    *+10                                                             
TBCF55A  MVC   TBKPRD,PRD                                                       
         TM    ESTTBSW,2           SEE IF SORTING ON EST                        
         BZ    *+10                                                             
         MVC   TBKEST2,BKEYEST                                                  
         MVC   TBKSTA(5),=5X'FF'                                                
         CLI   QOPT4,C'B'          SUPPRESS BOTH STA AND MOS                    
         BE    TBCF55C                                                          
         CLI   QOPT4,C'D'          SUPPRESS BOTH STA AND MOS                    
         BNE   TBCF56              WITH DOWNLOAD                                
TBCF55C  MVC   TBKSTA(3),CLT                                                    
         MVC   TBKSTA+3(3),PRD                                                  
         OC    TBKSTA,SPACES                                                    
         B     TBCF56C                                                          
*                                                                               
TBCF56   CLI   QOPT4,C'M'          NO MTH OF SERVICE BREAKOUT                   
         BE    *+10                                                             
         MVC   TBKMOS,BKEYYSRV                                                  
TBCF56C  PACK  DUB,BINVNO+2(4)                                                  
         CVB   R0,DUB                                                           
         STCM  R0,3,TBKINV                                                      
*******  STH   R0,TBKINV                                                        
         MVC   TBKINVMO,BKEYMBIL       BILLING Y/M - ONE BYTE                   
         GOTO1 DATCON,DMCB,(0,BDATE),(2,TBBILDTE)                               
         ZAP   TBBILLN,BNETP                                                    
         ZAP   TBBILLG,BGRSP                                                    
         CLI   QOPT7,C'Y'          TRACING SORT CALLS                           
         BNE   *+14                                                             
         MVC   WORK(L'TBREC),TBREC                                              
         BAS   RE,SORTIN                                                        
         GOTO1 SORTER,DMCB,=C'PUT',TBREC                                        
         MVI   SORTACT,C'Y'                                                     
         B     TBCF45                                                           
*                                                                               
TBCF60   DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
POSTBILL NTR1                                                                   
         LA    R2,CURBILLN                                                      
         CLC   QAREA+49(4),SPACES  SEE IF I HAVE A CURRENT MTH                  
         BE    POSTB5              NO                                           
         CLC   QAREA+51(2),SPACES   SEE IF I ONLY HAVE A YEAR                   
         BNE   POSTB3                                                           
         CLC   BDATE(2),QAREA+49    ONLY CHECK THE YEAR                         
         BE    POSTB5                                                           
         LA    R2,PREBILLN                                                      
         B     POSTB8                                                           
*                                                                               
POSTB3   CLC   BDATE(4),QAREA+49                                                
         BE    POSTB5                                                           
         LA    R2,PREBILLN                                                      
         B     POSTB8                                                           
*                                                                               
POSTB5   DS    0H                                                               
         CLI   QOPT5,C'Y'          SEE IF LISTING CUR INVOICES                  
         BNE   POSTB8                                                           
         MVI   RCSUBPRG,10                                                      
         L     RF,ADAGY                                                         
         CLI   AGYPROF+7-AGYKEY(RF),C'C'                                        
         BNE   *+8                                                              
         MVI   RCSUBPRG,50                                                      
*                                                                               
         MVC   P+3(3),BKEYPRD                                                   
         CLI   BKEYEST,0                                                        
         BE    INVR10                                                           
         ZIC   R0,BKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+8(3),DUB                                                       
*                                                                               
INVR10   DS    0H                                                               
         CLI   BKEYYSRV+1,12       FUNNY BILLING PERIODS                        
         BNH   INVR15                                                           
         ZIC   R0,BKEYYSRV+1                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+14(2),DUB                                                      
         MVI   P+16,C'/'                                                        
         ZIC   R0,BKEYYSRV                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(2),DUB                                                      
         B     INVR17                                                           
*                                                                               
INVR15   GOTO1 DATCON,DMCB,(3,BKEYYSRV),(9,P+14)                                
INVR17   MVC   P+22(2),BTYPE                                                    
         CLI   BTYPE,C'B'                                                       
         BE    INVR20                                                           
         MVC   P+22(2),=C'S '                                                   
         CLI   BTYPE+1,4                                                        
         BNE   INVR20                                                           
         MVI   P+22,C'D'                                                        
INVR20   DS    0H                                                               
*                                                                               
         GOTO1 AFMTINO,DMCB,BDATE,BINVNO+2,(MED,PROFB1),PROFB1X                 
*                                                                               
         L     RE,DMCB+4                                                        
         MVC   P+28(7),0(RE)                                                    
*                                                                               
         TM    BILSTAT3,BSTTRCNQ   TRADE - CALCULATED NET                       
         BNO   INVR20A                                                          
         MVI   P+24,C'X'                                                        
         B     INVR20X                                                          
*                                                                               
INVR20A  TM    BILSTAT3,X'01'      GRP M MIDAS BARTER                           
         BNO   INVR20C             EQU = BSTMBARQ                               
         MVI   P+24,C'T'                                                        
         B     INVR20X                                                          
*                                                                               
INVR20C  TM    BILSTAT,X'20'       AOR BILL                                     
         BNO   INVR20F                                                          
         MVC   P+24(3),=C'AOR'                                                  
         OI    AINVSW,X'01'                                                     
         B     INVR21                                                           
*                                                                               
INVR20F  TM    BILSTAT,X'01'       COM BILL - UPFRONT                           
         BNO   INVR20H                                                          
         MVC   P+24(3),=C'UFC'                                                  
*******  OI    CMINVSW,X'01'                                                    
         B     INVR21                                                           
*                                                                               
INVR20H  TM    BILSTAT,X'08'       NET BILL                                     
         BNO   INVR20X                                                          
         MVC   P+24(3),=C'NET'                                                  
*******  OI    CMINVSW,X'01'                                                    
         B     INVR21                                                           
*                                                                               
INVR20X  DS    0H                                                               
*                                                                               
INVR21   CLI   BRETAIL,X'81'       RETAIL BILLING                               
         BNE   INVR22                                                           
         MVI   P+35,C'C'                                                        
         B     INVR23                                                           
*                                                                               
INVR22   CLI   BRETAIL,X'41'       RETAIL BILLING - SUMMARY                     
         BNE   INVR23                                                           
         MVI   P+35,C'S'                                                        
INVR23   DS    0H                                                               
         TM    BILSTAT,X'20'          AOR BILL                                  
         BO    INVR23B                ONLY SHOW BILL AMOUNT                     
******   TM    BILSTAT,X'01'          COM BILL                                  
******   BO    INVR23B                ONLY SHOW BILL AMOUNT                     
         EDIT  BNETP,(14,P+37),2,COMMAS=YES,FLOAT=-                             
         EDIT  BGRSP,(14,P+53),2,COMMAS=YES,FLOAT=-                             
*                                                                               
INVR23B  DS    0H                                                               
         ZAP   DOUBLE,BACTP                                                     
         ICM   RF,15,BVATAMT                                                    
         CVD   RF,DUB                                                           
         AP    DOUBLE,DUB                                                       
         ICM   RF,15,BTOTPST      ALSO INCLUDE PST                              
         CVD   RF,DUB                                                           
         AP    DOUBLE,DUB                                                       
*                                                                               
INVR23B3 EDIT  (P8,DOUBLE),(14,P+69),2,COMMAS=YES,FLOAT=-                       
         L     RF,ADAGY                                                         
         CLI   AGYPROF+7-AGYKEY(RF),C'C'                                        
         BNE   INVR23B8                                                         
*                                                                               
         EDIT  (B4,BVATAMT),(14,P+84),2,COMMAS=YES,FLOAT=-                      
*                                                                               
         EDIT  (B4,BTOTPST),(14,P+99),2,COMMAS=YES,FLOAT=-                      
*                                                                               
INVR23B8 TM    BILSTAT,X'20'       AOR BILL                                     
         BO    INVR23C                                                          
*******  TM    BILSTAT,X'01'       COM BILL                                     
*******  BO    INVR23C                                                          
         CLI   BRETAIL,X'41'       RETAIL BILLING - SUMMARY                     
         BNE   INVR24                                                           
         MVI   P+51,C'*'                                                        
         MVI   P+67,C'*'                                                        
INVR23C  MVI   P+83,C'*'           NOT ADDED TO TOTALS                          
INVR24   CLI   BTYPE,C'B'          NEW BILLING                                  
         BE    INVR25              CAN'T BE REVERSED                            
         CLC   BCANINV(6),=6C'0'                                                
         BE    INVR25                                                           
         LA    R3,P                                                             
         OC    BVATAMT,BVATAMT                                                  
         BZ    *+8                                                              
         LA    R3,P2                                                            
         MVC   85(12,R3),=C'(REVERSED BY'                                       
         MVC   98(2,R3),BCANINV                                                 
         MVI   100(R3),C'-'                                                     
         MVC   101(4,R3),BCANINV+2                                              
         MVI   105(R3),C')'                                                     
*                                                                               
INVR25   GOTO1 VPRINTIT,DMCB,(RC)                                               
         CLI   BRETAIL,X'41'       RETAIL BILLING - SUMMARY                     
         BE    POSTBX              DON'T ADD TO TOTALS                          
         TM    BILSTAT,X'20'       AOR BILL                                     
         BO    INVR25C             DON'T ADD TO TOTALS                          
******** TM    BILSTAT,X'01'       COM BILL                                     
******** BO    INVR25D             DON'T ADD TO TOTALS                          
         AP    CINVNET,BNETP       ROLL TO CURRENT INV TOTALS                   
         AP    CINVGRS,BGRSP                                                    
         AP    CINVAMT,BACTP                                                    
*                                   ADD GST                                     
         OC    BVATAMT,BVATAMT                                                  
         BZ    INVR25A                                                          
         MVC   FULL,BVATAMT                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         AP    CINVAMT,DUB                                                      
*                                                                               
         AP    CINVGST,DUB          TOTAL GST                                   
*                                                                               
*****************                   PST FOR BILLS - DO HERE                     
*                                                                               
INVR25A  DS    0H                                                               
         OC    BTOTPST,BTOTPST      CHECK FOR PST                               
         BZ    INVR25AX                                                         
         MVC   FULL,BTOTPST                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         AP    CINVAMT,DUB          ADD TO INVOICE AMOUNT                       
*                                                                               
         AP    CINVPST,DUB          TOTAL PST                                   
*                                                                               
INVR25AX DS    0H                                                               
*                                                                               
*****************                                                               
INVR25B  MVI   CINVSW,1                                                         
         B     POSTB8                                                           
*                                                                               
INVR25C  DS    0H                                                               
         AP    AINVAMT,BACTP                                                    
         MVC   FULL,BVATAMT                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         AP    AINVAMT,DUB         AND TO AOR BILL AMT TOTALS                   
         AP    CINVGST,DUB         AND TO CURRENT GST TOTALS                    
*********************************   PST BILL AMOUNTS HERE                       
*                                                                               
INVR25E  DS    0H                                                               
         OC    BTOTPST,BTOTPST      CHK FOR ANY PST                             
         BZ    INVR25EX                                                         
         MVC   FULL,BTOTPST                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         AP    AINVAMT,DUB         AND TO AOR BILL AMT TOTALS                   
         AP    CINVPST,DUB         AND TO CURRENT PST TOTALS                    
*                                                                               
INVR25EX DS    0H                                                               
*                                                                               
*********************************                                               
         B     POSTB10             ALSO ADD TO CLIENT GST TOTALS                
*                                                                               
***R25D  DS    0H                                                               
***      AP    CMINVAMT,BACTP                                                   
***      MVC   FULL,BVATAMT                                                     
***      L     R0,FULL                                                          
***      CVD   R0,DUB                                                           
***      AP    CMINVAMT,DUB        AND TO COM BILL AMT TOTALS                   
***      AP    CINVGST,DUB         AND TO CURRENT GST TOTALS                    
***      B     POSTB10             ALSO ADD TO CLIENT GST TOTALS                
*                                                                               
POSTB8   CLI   BRETAIL,X'41'       RETAIL SUMMARY BILL                          
         BE    POSTBX              DON'T ADD TO TOTALS                          
         TM    BILSTAT,X'20'       AOR BILL                                     
         BO    POSTB10             DON'T ADD TO TOTALS                          
******** TM    BILSTAT,X'01'       COM BILL                                     
******** BO    POSTB10             DON'T ADD TO TOTALS                          
         AP    0(8,R2),BNETP                                                    
         AP    8(8,R2),BGRSP                                                    
*                                                                               
POSTB10  DS    0H                   SEE IF CANADIAN                             
         L     RF,ADAGY                                                         
         CLI   AGYPROF+7-AGYKEY(RF),C'C'                                        
         BNE   POSTBX                                                           
         LA    R4,CCBGST                                                        
         CLC   QAREA+49(4),SPACES     IF NO CURRENT MTH                         
         BE    POSTB15                ADD TO CURRENT                            
         CLC   QAREA+51(2),SPACES     SEE IF I ONLY HAVE A YEAR                 
         BNE   POSTB13                                                          
         CLC   BDATE(2),QAREA+49       ONLY CHECK THE YEAR                      
         BE    POSTB15                                                          
         LA    R4,CPBGST               POST TO PREVIOUS                         
         B     POSTB15                                                          
*                                                                               
POSTB13  CLC   BDATE(4),QAREA+49                                                
         BE    POSTB15                                                          
         LA    R4,CPBGST               POST TO PREVIOUS                         
*                                                                               
POSTB15  MVC   FULL,BVATAMT                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         AP    0(8,R4),DUB                                                      
*                                                                               
***********************************  ADD BILLED PST HERE                        
POSTB16  DS    0H                                                               
*                                                                               
         OC    BTOTPST,BTOTPST        SEE IF BILL HAD PST                       
         BZ    POSTB18                                                          
         LA    R4,CCBPST                                                        
         CLC   QAREA+49(4),SPACES     IF NO CURRENT MTH                         
         BE    POSTB16E               ADD TO CURRENT                            
         CLC   QAREA+51(2),SPACES     SEE IF I ONLY HAVE A YEAR                 
         BNE   POSTB16C                                                         
         CLC   BDATE(2),QAREA+49       ONLY CHECK THE YEAR                      
         BE    POSTB16E                                                         
         LA    R4,CPBPST               POST TO PREVIOUS                         
         B     POSTB16E                                                         
*                                                                               
POSTB16C CLC   BDATE(4),QAREA+49                                                
         BE    POSTB16E                                                         
         LA    R4,CPBPST               POST TO PREVIOUS                         
POSTB16E MVC   FULL,BTOTPST                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
*                                                                               
         AP    0(8,R4),DUB                                                      
*                                                                               
POSTB18  DS    0H                                                               
*                                                                               
***********************************                                             
POSTBX   XIT1                                                                   
         EJECT                                                                  
TBBUY    DS    0H                                                               
*                                                                               
         CLI   KEY+10,X'FF'                                                     
         BE    *+12                                                             
         TM    KEY+10,X'80'        PASSIVE SPILL POINTER                        
         BO    EXIT                BYPASS                                       
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
*                                                                               
         MVC   SAVMSTA,BUYKMSTA      SAVE MARKET/STATION                        
*                                  FOR CLEARANCE STATUS READING                 
         XC    TBREC,TBREC                                                      
         CLI   QOPT4,C'B'          SPECIAL SUMMARY REPORT                       
         BE    TB0                                                              
         CLI   QOPT4,C'D'          SPECIAL SUMMARY REPORT                       
         BE    TB0                 WITH DOWNLOAD                                
         CLI   QOPT2,C'Y'          SEE IF DOING ALL PRDS TOGETHER               
         BE    *+10                                                             
TB0      MVC   TBKPRD,PRD                                                       
*                                                                               
         CLI   CANNOPT,C'C'         SEE IF CANADIAN NETWORK                     
         BE    TB1                  IF YES - IGNORE THIS TEST                   
*                                                                               
         CLI   BSTA,X'E8'           SEE IF CABLE HEADEND                        
         BL    TB1                                                              
         MVC   TBKSTA,BIGSTA      YES - THEN USE BIGSTA                         
         B     TB1X                                                             
*                                                                               
TB1      MVC   TBKSTA(5),STA                                                    
         OC    TBKSTA,SPACES       SO IT WILL MATCH BILLING                     
*                                                                               
TB1X     DS    0H                                                               
         CLI   QOPT4,C'B'          SUPPRESS BOTH STA AND MOS                    
         BE    TB1X3                                                            
         CLI   QOPT4,C'D'          SUPPRESS BOTH STA AND MOS                    
         BNE   TB1X5               WITH DOWNLOAD                                
TB1X3    MVC   TBKSTA,SPACES                                                    
         MVC   TBKSTA(3),CLT                                                    
         MVC   TBKSTA+3(3),PRD                                                  
         OC    TBKSTA,SPACES       SO IT WILL MATCH BILLING                     
         B     TB4                                                              
*                                                                               
TB1X5    CLI   QOPT4,C'S'          NO STATION BREAKOUT                          
         BNE   TB2                                                              
         MVC   TBKSTA,=CL8'ALL'                                                 
         B     TB4                                                              
*                                                                               
TB2      CLI   CANNOPT,C'C'        SEE IF DOING CANADIAN NETWORK                
         BNE   TB4                                                              
         LA    R2,BDELEM                                                        
TB3      ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    TB4                                                              
         CLI   0(R2),X'68'                                                      
         BNE   TB3                                                              
         CLI   2(R2),0                                                          
         BNH   TB3                 WILL BE 0 IN NETWORK BUY                     
         MVC   TBKSTA(4),2(R2)     USE NETWORK CALL LETTERS                     
*                                  FOR EXPLODED BUYS                            
         MVI   TBKSTA+4,C' '       ONLY USE 4 CALL LETTERS                      
*                                                                               
TB4      LA    R2,BDELEM                                                        
TB5      ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    TBENDBUY                                                         
         CLI   0(R2),6                                                          
         BL    TB5                                                              
         CLI   0(R2),14                                                         
         BH    TB5                                                              
**8/5/88                                                                        
*                                BUYERS WORK SHEET CREATED BAD ELEMS            
         CLI   2(R2),0              MUST IGNORE ELEMS NO DATE                   
         BE    TB5                                                              
         CLI   2(R2),X'FF'          MUST IGNORE ELEMS NO DATE                   
         BE    TB5                                                              
**8/5/88                                                                        
         LA    R5,TBBILLN                                                       
         LA    R4,4                                                             
TB6      ZAP   0(8,R5),=P'0'                                                    
         LA    R5,8(R5)                                                         
         BCT   R4,TB6                                                           
         TM    BDSTAT,X'01'        SEE IF NETPAK BUY                            
         BZ    *+8                                                              
         MVI   DMCB+8,C'T'                                                      
         MVC   FULL(3),BDCOST      SAVE COST                                    
         CLI   BACTSW,C'Y'         SEE IF BILLING ACTUAL                        
         BNE   TB6C                                                             
         XC    BDCOST,BDCOST                                                    
*                                                                               
TB6C     DS    0H                                                               
         XC    XCHDATA,XCHDATA                                                  
         SR    R0,R0                                                            
         TM    BDCIND2,X'20'       TEST CANADIAN                                
         BZ    *+8                                                              
         LA    R0,C'Z'             SET FOR XCHDATA - EXTENDED                   
         CLI   SVOPT31,C'Y'                                                     
         BNE   *+8                                                              
         OI    BDCIND2,X'10'                                                    
*                                                                               
         GOTO1 GETRATE,DMCB,(KEY+3,SPOTS),BUYREC,((R0),(R2)),          X        
               (C'C',XCHDATA)                                                   
*                                                                               
         MVC   BDCOST(3),FULL         RESTORE BDCOST                            
*                                                                               
         XC    STOTPST,STOTPST      BUYREC'S TOTAL PST                          
         LA    R4,10       FOR BCT                                              
         LA    R5,XPSTTAB                                                       
TB7      OC    8(4,R5),8(R5)       CHK FOR PST                                  
         BZ    TB7D                                                             
         L     R0,STOTPST                                                       
         A     R0,8(R5)                                                         
         ST    R0,STOTPST                                                       
TB7D     LA    R5,XPSTLEN(R5)                                                   
         BCT   R4,TB7                                                           
*                                                                               
*                                  NOW STOTPST HAS TOTAL PST                    
*                                                                               
         CLI   QOPT4,C'B'          BOTH STA AND MOS                             
         BE    TB9                                                              
         CLI   QOPT4,C'D'          BOTH STA AND MOS                             
         BE    TB9                 WITH DOWNLOAD                                
         CLI   QOPT4,C'M'          NO MTH OF SERVICE BREAKOUT                   
         BE    TB9                                                              
         GOTO1 BRDMON,DMCB,(X'FF',2(R2)),WORK,RR=RELO                           
         SR    R0,R0                                                            
         ICM   R0,3,WORK                                                        
         SRDL  R0,9                                                             
         SRL   R1,28                                                            
         STC   R0,WORK                                                          
         STC   R1,WORK+1                                                        
*                                                                               
TB8      DS    0H                                                               
         MVC   TBKMOS,WORK                                                      
TB9      TM    ESTTBSW,2           SEE IF SHOWING EST WITHIN MOS                
         BZ    *+10                                                             
         MVC   TBKEST2,BUYKEST                                                  
         TM    ESTTBSW,X'08'       SEE IF SHOWING LINE NUMBER                   
         BZ    TB9A                                                             
*                                                                               
         MVC   TBKLINE+1(1),BUYKEST+1                                           
         TM    BUYRCNTL,BUYRLN2     2 BYTE BUYLINE?                             
         BNO   TB9A                                                             
         MVC   TBKLINE,BUYRLIN                                                  
*                                                                               
TB9A     XC    WORK(20),WORK                                                    
         TM    BDSTAT,X'01'        SEE IF NETPAK BUY                            
         BZ    TB9E                NO                                           
*                                  SAVE INTEGRATION ELEM IN WORK                
         LR    RE,R2                                                            
         SR    R0,R0                                                            
TB9B     IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0             END OF REC                                   
         BE    TB9E                                                             
         CLI   0(RE),X'10'         AFF                                          
         BE    TB9B                SKIP                                         
         CLI   0(RE),X'11'         INTEGRATION                                  
         BNE   TB9E                                                             
         ZIC   R5,1(RE)            ELEM LENGHT                                  
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)                                                    
*                                                                               
*                                                                               
TB9E     DS    0H                                                               
         OC    4(2,R2),4(R2)            TEST PAID                               
         BZ    TB10                                                             
         CLC   4(2,R2),BQENDP      SEE IF PAID AFTER AS OF DATE                 
         BH    TB10                YES - BYPASS                                 
         CLC   4(2,R2),CMEND       SEE IF PAID AFTER CURRENT MTH                
         BH    TB10                YES - BYPASS                                 
         MVC   TBKPDDTE,4(R2)                                                   
*                                                                               
*                                   POST GST TO CLIENT TOTALS                   
         TM    BDCIND2,X'20'        SEE IF CANADIAN                             
         BZ    TB9H                                                             
         LA    R4,CCPGST                                                        
         CLC   4(2,R2),CMSTART                                                  
         BNL   TB9F                                                             
         LA    R4,CPPGST            POST TO PREV                                
*                                                                               
TB9F     L     R0,SGSTAMT         WAS XGSTAMT                                   
         CVD   R0,DUB                                                           
         AP    0(8,R4),DUB                                                      
*                                                                               
******************************* POST PST HERE                                   
         LA    R4,CCPPST                                                        
         CLC   4(2,R2),CMSTART                                                  
         BNL   TB9G                                                             
         LA    R4,CPPPST            POST TO PREV                                
*                                                                               
TB9G     L     R0,STOTPST           TOTAL PST                                   
         CVD   R0,DUB                                                           
         AP    0(8,R4),DUB                                                      
*                                                                               
*******************************                                                 
TB9H     DS    0H                                                               
         L     R0,GROSS                                                         
         CVD   R0,TBPAIDG                                                       
         L     R0,NET                                                           
         CVD   R0,TBPAIDN                                                       
         BAS   RE,PUTBUFF                                                       
         XC    TBKPDDTE,TBKPDDTE                                                
         ZAP   TBPAIDN,=P'0'                                                    
         ZAP   TBPAIDG,=P'0'                                                    
         CLI   WORK,X'11'          SEE IF I HAVE INTEGRATION ELEM               
         BNE   TB10                NO                                           
         OC    WORK+6(2),WORK+6    SEE IF PAID                                  
         BZ    TB10                NO                                           
         CLC   WORK+6(2),BQENDP    SEE IF PAID AFTER AS OF DATE                 
         BH    TB10                YES                                          
         CLC   WORK+6(2),CMEND     SEE IF PAID AFTER CURRENT MONTH              
         BH    TB10                                                             
         GOTO1 GETRATE,DMCB,(KEY+3,SPOTS),BUYREC,(C'I',(R2))                    
         MVC   TBKPDDTE,WORK+6                                                  
         L     R0,GROSS                                                         
         CVD   R0,TBPAIDG                                                       
         L     R0,NET                                                           
         CVD   R0,TBPAIDN                                                       
         BAS   RE,PUTBUFF                                                       
         XC    TBKPDDTE,TBKPDDTE                                                
         ZAP   TBPAIDN,=P'0'                                                    
         ZAP   TBPAIDG,=P'0'                                                    
*                                                                               
TB10     B     TB5            OLD DETAIL BILLING CODE NO-OPED                   
*                                                                               
*                             OLD DETAIL BILLING                                
*B10     CLI   BUYKEY+3,X'FF'                                                   
*        BC    8,TB20                                                           
*        SR    R4,R4                 GET PARTNER NUMBER                         
*        CLI   BDTIME,0                                                         
*        BC    8,TB14                                                           
*        LA    R3,BDELEM                                                        
*        SR    R0,R0                                                            
*B12     IC    R0,1(R3)                                                         
*        AR    R3,R0                                                            
*        CLI   0(R3),4                                                          
*        BC    7,TB12                                                           
*        ST    R3,X                 SAVE PBELEM ADDRESS                         
*        IC    R0,1(R3)            GET PBELEM LNEGTH                            
*        SH    R0,=H'2'                                                         
*        SRDA  R0,32                                                            
*        D     R0,=F'7'                                                         
*        STC   R1,X                R1 HAS NUMBER OF PARTNERS                    
*        LA    R3,2(R3)                                                         
*        LA    R4,1                                                             
*B13     CLC   0(1,R3),KEY+3                                                    
*        BC    8,TB14                                                           
*        LA    R4,1(R4)              BUMP PARTNER NUMBER                        
*        LA    R3,7(R3)                                                         
*        BCT   R1,TB13                                                          
*        SR    R4,R4                 MUST BE ACTIVE PARTNER                     
*B14     SLL   R4,1                 GET BILL DATE ADDRESS                       
*        LA    R4,8(2,R4)                                                       
*        BAS   R7,TB30                                                          
*B18     B     TB5                                                              
*        EJECT                                                                  
*                                  POOL BILLING ITEMS                           
*B20     SR    R0,R0                                                            
*       IC    R0,1(R2)                                                          
*        SH    R0,=H'10'                                                        
*        BC    8,TB5                                                            
*        SRL   R0,2                 SET FOR BCT                                 
*        LA    R3,10(R2)                                                        
*        LA    R5,WORK+8           POINT TO FIRST BILLED DATE                   
*B21     CLI   KEY+3,X'FF'         TEST PROCESSING POL PRD                      
*        BC    8,TB22              YES - DO ALL ALLOCATAONS                     
*        CLC   0(1,R3),KEY+3        TEST RIGHT ALLOCATION                       
*        BC    7,TB23                                                           
*B22     LA    R4,2(R3)              SET BILL DATE ADDRESS                      
*        ST    R2,DMCB+8                                                        
*        TM    BDSTAT,X'01'        SEE IF NETPAK BUY                            
*        BZ    *+8                                                              
*        MVI   DMCB+8,C'T'                                                      
*        MVC   FULL(3),BDCOST                                                   
*        CLI   BACTSW,C'Y'         FROM BN PROFILE                              
*        BNE   *+10                                                             
*        XC    BDCOST,BDCOST                                                    
*        GOTO1 GETRATE,DMCB,(0(R3),SPOTS),BUYREC                                
*        MVC   BDCOST(3),FULL                                                   
*        BAS   R7,TB30                                                          
*        CLI   WORK,X'11'          SEE IF I HAVE INTEGRATION ELEM               
*        BNE   TB23                NO                                           
*        OC    0(2,R5),0(R5)       SEE IF BILLED                                
*        BZ    TB23                NO                                           
*        LR    R4,R5               POINT TO BILLED DATE                         
*        MVC   FULL(3),BDCOST                                                   
*        CLI   BACTSW,C'Y'         FROM BN PROFILE                              
*        BNE   *+10                                                             
*        XC    BDCOST,BDCOST                                                    
*        GOTO1 GETRATE,DMCB,(0(R3),SPOTS),BUYREC,(C'I',(R2))                    
*        MVC   BDCOST(3),FULL                                                   
*        BAS   R7,TB30                                                          
*                                                                               
*B23     LA    R3,4(R3)                                                         
*        LA    R5,2(R5)            BUMP TO NEXT BILLED DATE                     
*        BCT   R0,TB21                                                          
*        B     TB5                 GO DO NEXT ELEM                              
*        SPACE 2                                                                
*B30     DS    0H                  LINKED VIA R7                                
*        OC    0(2,R4),0(R4)       R4 HAS BILL DATE ADDR                        
*        BZ    0(R7)               RETURN                                       
*        CLC   0(2,R4),BQENDP      SEE IF BILLED AFTER AS OF DATE               
*        BH    0(R7)               YES - BYPASS                                 
*        CLC   0(2,R4),CMEND       SEE IF BILLED AFTER CURRENT MTH              
*        BH    0(R7)               YES - BYPASS                                 
*        MVC   TBKINV,=2X'FF'                                                   
*        MVI   TBKINVMO,X'FF'                                                   
*        MVC   TBBILDTE,0(R4)                                                   
*        L     R1,GROSS                                                         
*        CVD   R1,TBBILLG                                                       
*        L     R1,NET                                                           
*        CVD   R1,TBBILLN                                                       
*        GOTO1 PUTBUFF                                                          
*        MVI   TBKINVMO,0                                                       
*        XC    TBKINV,TBKINV                                                    
*        XC    TBBILDTE,TBBILDTE                                                
*        BR    R7                       RETURN                                  
*                                                                               
TBENDBUY B     EXIT                                                             
         EJECT                                                                  
FBUYSTA  DS    0H                                                               
*                                                                               
         MVC   WKBPRD,BPRD       SAVE FOR CLEAR STATUS READING                  
*                                                                               
         XC    SAVMSTA,SAVMSTA                                                  
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'01',BUFFBUFF),(X'80',1)                
         B     EXIT                                                             
         SPACE 2                                                                
LBUYSTA  DS    0H                                                               
LBUYS4X  DS    0H                                                               
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYP,X'01'                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     LBUYS10                                                          
*                                                                               
LBUYS5   GOTO1 BUFFALO,DMCB,=C'SEQ',(X'01',BUFFBUFF),BUFREC,0                   
LBUYS10  CLI   DMCB+8,X'80'        END OF FILE                                  
         BE    LBUYSX                                                           
         CLI   BUFTYP,X'01'        SINCE 'HIGH' DOSEN'T PASS EOF                
         BNE   LBUYSX                                                           
         MVC   TBKEY,BUFREC+1                                                   
         MVC   TBBILLN(32),BUFPBILN                                             
         CLI   QOPT7,C'Y'                                                       
         BNE   *+14                                                             
         MVC   WORK(L'TBREC),TBREC                                              
         BAS   RE,SORTIN                                                        
         GOTO1 SORTER,DMCB,=C'PUT',TBREC                                        
         MVI   SORTACT,C'Y'                                                     
         B     LBUYS5                                                           
*                                                                               
LBUYSX   GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'01',BUFFBUFF),(X'80',1)                
         B     EXIT                                                             
         EJECT                                                                  
PUTBUFF  NTR1                                                                   
         MVI   BUFTYP,X'01'                                                     
         MVC   BUFTYP+1(L'TBKEY),TBREC                                          
         MVC   BUFPBILN(32),TBBILLN                                             
         ZAP   BUFCBILG,=P'0'                                                   
         ZAP   BUFCPAYG,=P'0'                                                   
         ZAP   BUFPBILG,=P'0'                                                   
         ZAP   BUFPPAYG,=P'0'                                                   
*                                                                               
         CLI   QOPT7,C'B'       TRACING STATION BUFFALO PUTS                    
         BNE   PUTBUFX                                                          
         BAS   RE,BUFFIN                                                        
*                                                                               
PUTBUFX  GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
PUTSX    XIT1                                                                   
         EJECT                                                                  
TBAGY    DS    0H                  AGENCY TOTALS                                
*                                                                               
         CLI   RCREQREP,C'N'       SEE IF SUPPRESSING REQ DETAILS               
         BE    TBAG05              YES - DON'T CLOSE DOWNLOAD NOW               
*                                  IT WILL BE DONE AT RUNLAST                   
*                                                                               
         CLI   QOPT4,C'D'           SEE IF SPECIAL DOWNLOAD VERSION             
         BE    TBAG01                                                           
         CLI   QOPT5,C'S'           SEE IF STATION DOWNLOAD                     
         BNE   TBAG05                                                           
*                                                                               
*        IN THESE CASES I  MUST CLOSE THE DOWNLOAD REPORT                       
*        NOW - MODE IS LBUYREQ                                                  
*                                                                               
TBAG01   GOTO1 VDOWNLD,DMCB,(RC)                                                
*                                                                               
TBAG05   CLI   AGYACT,C'Y'                                                      
         BNE   TBAGX                                                            
         MVI   FORCEHED,C'Y'                                                    
         CLC   QCLT,=C'ALL'        ONE CLT SKIP AGY TOTALS                      
         BE    TBAG20                                                           
         CLC   QCLT(2),=C'$*'      ALL OFFICES                                  
         BE    TBAG16                                                           
         CLI   QCLT,C'$'           OFFICE LIST REQUEST                          
         BE    TBAG15                                                           
         CLI   QCLT,C'*'           OFFICE REQUEST                               
         BNE   TBAG50                                                           
         B     TBAG20                                                           
TBAG15   MVI   RCSUBPRG,12        OFFICE LIST TOTALS                            
         CLI   QOPT7,C'N'    SEE IF OVERRIDING MYA8PROF+8 - OFF TOTS            
         BE    TBAG25                                                           
         CLI   QOPT7,C'C'    SEE IF OVERRIDING MYA8PROF+8 - CLT TOTS            
         BE    TBAG15C                                                          
*                                                                               
         CLC   QCLT(2),=C'$*'      ALL OFFICE REQ?                              
         BE    TBAG25              DON'T RECAP BY CLT                           
*                                                                               
         CLI   MYA8PROF+8,C'C'     SEE IF REPORTING BY CLT                      
         BNE   TBAG25                                                           
TBAG15C  MVI   RCSUBPRG,14                                                      
         B     TBAG25                                                           
*                                                                               
TBAG16   MVI   RCSUBPRG,13        MEDIA TOTALS BY OFFICE                        
         B     TBAG25                                                           
*                                                                               
TBAG20   DS    0H                                                               
         MVI   RCSUBPRG,9                                                       
TBAG25   DS    0H                                                               
         MVI   RQEMAIL,C'Y'     FORCE OUTPUT COPY TO EMFILE                     
         XC    BUFREC,BUFREC                                                    
         MVI   TOTTYP,X'06'        REQ TOTALS                                   
         GOTO1 VBTOTS,DMCB,(RC)                                                 
TBAG50   DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'END'                                              
         MVI   RQEMAIL,C'N'     SET OFF                                         
                                                                                
                                                                                
TBAGX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
TBOFF    DS    0H                  OFFICE TOTALS FOR OFFICE LIST REQ            
         CLI   OFFACT,C'Y'                                                      
         BNE   TBOFFX                                                           
         MVI   FORCEHED,C'Y'                                                    
         CLC   QCLT,=C'ALL'        ONE CLT SKIP OFF TOTALS                      
         BE    TBOFF20                                                          
         MVI   RQEMAIL,C'Y'     FORCE OUTPUT COPY TO EMFILE                     
         CLI   QCLT,C'$'           OFFICE LIST REQUEST                          
         BE    TBOFF20                                                          
         MVI   RQEMAIL,C'N'        SET IT OFF                                   
         CLI   QCLT,C'*'           OFFICE REQUEST                               
         BNE   TBOFF50                                                          
*                                                                               
TBOFF20  DS    0H                                                               
         MVI   RCSUBPRG,11                                                      
         XC    BUFREC,BUFREC                                                    
         MVI   TOTTYP,X'05'        OFF TOTALS                                   
         GOTO1 VBTOTS,DMCB,(RC)                                                 
         MVI   RQEMAIL,C'N'        SET IT OFF                                   
*                                                                               
TBOFF50  DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'END'                                              
         MVI   OFFACT,0                                                         
TBOFFX   B     EXIT                                                             
*                                                                               
SORTIN   NTR1                                                                   
         MVC   P+1(8),=C'SORT IN='                                              
         GOTO1 HEXOUT,DMCB,WORK,P+10,56,0                                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         XIT1                                                                   
*                                                                               
BUFFIN   NTR1                                                                   
         MVC   P+1(8),=C'BUFF IN='                                              
         GOTO1 HEXOUT,DMCB,TBREC,P+10,56,0                                      
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         XIT1                                                                   
*                                                                               
         SPACE 2                                                                
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         EJECT                                                                  
         SPACE 2                                                                
         LTORG                                                                  
SORTC    DC    CL80'SORT FIELDS=(1,31,A),FORMAT=BI,WORK=1'                      
SORTT    DC    CL80'RECORD TYPE=F,LENGTH=64'                                    
         DROP  RB,R8                                                            
         EJECT                                                                  
**********************************************************************          
* NOTIFY PEOPLE AUTOMATICALLY THAT REPORT WAS GENERATED              *          
**********************************************************************          
         SPACE 1                                                                
         USING MAILTBD,R2                                                       
SENDMAIL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R2,VMAILTAB                                                      
SENDM10  CLI   MAILAGY,EOF                                                      
         BE    SENDMX                                                           
         CLC   MAILAGY,RCORIGID    ARE WE AT THE RIGHT AGENCY                   
         BE    SENDM20                                                          
         LA    R2,MAILNQ(R2)       BUMP TO NEXT ENTRY                           
         B     SENDM10                                                          
*                                                                               
SENDM20  DS    0H                                                               
         MVC   WARNMSG5,MAILUID           CONNECT ID OF THE AGENCY              
         GOTO1 DATCON,DMCB,(0,TODAY),(5,WARNMSG4)                               
*                                                                               
         MVC   EMSG,SPACES                                                      
         MVC   EMSG(L'WARNMSG1),WARNMSG    START MOVING INTO EMSG               
         LA    R3,EMSG                                                          
         LA    R3,L'WARNMSG1(R3)                                                
*                                                                               
         LA    RF,MAILADD                                                       
         LHI   R1,L'MAILADD                                                     
SENDM30  CLI   0(RF),C' '          IS IT A SPACE                                
         BNE   SENDM40                                                          
         CHI   R1,L'MAILADD                                                     
         BE    SENDMX              NO EMAIL ADDRESS IN TABLE                    
         B     SENDM50                                                          
*                                                                               
SENDM40  MVC   0(1,R3),0(RF)                                                    
         LA    RF,1(RF)            BUMP ONE POSITION IN MAILADD                 
         LA    R3,1(R3)            BUMP ONE IN EMSG                             
         BCT   R1,SENDM30                                                       
SENDM50  MVC   0(WARNLEN1,R3),WARNMSG3                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(L'EMSG,EMSG)                             
*                                                                               
SENDMX   DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  R2                                                               
         DROP  RB                                                               
         SPACE 2                                                                
         DS    CL100           TO SEPARATE ALTERABLE STORAGE                    
*                                                                               
WARNMSG  DS    0CL(L'EMSG)         SOME CONSTANT VALUES FOR E-MAIL              
WARNMSG1 DC    C'AUTONOTE*'                                                     
WARNMSG2 DC    CL45' '             THIS IS VAR LEN, COMMA SEPARATED             
WARNMSG3 DC    C':'                                                             
         DC    C'SA8 REPORT WAS GENERATED ON '                                  
WARNMSG4 DC    CL8' '                      TODAY'S DATE                         
         DC    C' FOR '                                                         
WARNMSG5 DC    CL8' '                      CONNECT ID                           
WARNLEN1 EQU   *-WARNMSG3                                                       
WARNMSG6 DC    CL(L'EMSG-(*-WARNMSG))' '   SPARE SPACES                         
         EJECT                                                                  
*  HEADHOOK                                                                     
*                                                                               
TBHDHK   CSECT                                                                  
         NMOD1 0,TBHDHK                                                         
         L     RC,SAVERC           BASE PROGRAM'S RC                            
         L     RA,SAVERA           BASE PROGRAM'S RA                            
         LA    R9,SPACEND                                                       
         USING SPA8WRKD,R9                                                      
*                                                                               
HDHK2    DS    0H                                                               
         CLI   QCLT,C'*'           CHK FOR OFFICE FILTERS                       
         BNE   HDHK5                                                            
         CLI   QCLT+1,C'-'         ALL BUT                                      
         BE    HDHK4                                                            
         MVC   HEAD1+82(15),=C'** OFFICE 9  **'                                 
         MVC   HEAD1+92(L'SAVCOFF),SAVCOFF                                      
******** GOTO1 VOFFOUT,DMCB,QCLT+1,HEXOUT,HEAD1+92                              
         CLI   QCLT+2,C' '                                                      
         BE    HDHK6               NO RANGE                                     
         MVC   HEAD1+82(15),=C'* OFFICES 1-9 *'                                 
         MVC   HEAD1+92(1),QCLT+1                                               
*******  GOTO1 VOFFOUT,DMCB,QCLT+1,HEXOUT,HEAD1+92                              
         MVC   HEAD1+94(1),QCLT+2                                               
*******  GOTO1 VOFFOUT,DMCB,QCLT+2,HEXOUT,HEAD1+95                              
         B     HDHK6                                                            
*                                                                               
HDHK4    MVC   HEAD1+82(16),=C'* NOT OFFICE 9 *'                                
         MVC   HEAD1+95(1),QCLT+2                                               
*******  GOTO1 VOFFOUT,DMCB,QCLT+2,HEXOUT,HEAD1+95                              
         B     HDHK6                                                            
*                                                                               
HDHK5    CLC   QCLT(2),=C'$*'      ALL OFFICES                                  
         BE    HDHK5C                                                           
         CLI   QCLT,C'$'           OFFICE LIST REQUEST                          
         BNE   HDHK6                                                            
         MVC   HEAD1+82(14),=C'OFFICE LIST 00'                                  
         MVC   HEAD1+94(2),SAVMOL                                               
******   GOTO1 VOFFOUT,DMCB,QCLT+1,HEXOUT,HEAD1+94                              
HDHK5C   CLI   MODE,REQLAST                                                     
         BE    HDHK6                                                            
*                                                                               
         MVC   HEAD2+79(15),=C'** OFFICE 9  **'                                 
         MVC   HEAD2+89(L'SAVCOFF),SAVCOFF   SAVED COFFICE                      
*                                                                               
HDHK6    DS    0H                                                               
         CLI   QAREA+49,C' '       SEE IF I HAVE A CURRENT MTH                  
         BE    HDHK8               NO                                           
         CLC   QAREA+51(2),SPACES   SEE IF I ONLY HAVE A YEAR                   
         BNE   HDHK7                                                            
         MVC   H4+54(2),=C'19'                                                  
         MVC   H4+56(2),QAREA+49                                                
         CLI   QAREA+49,C'9'                                                    
         BNH   HDHK8                                                            
         MVC   H4+54(2),=C'20'                                                  
         ZIC   R0,HEAD4+56                                                      
         SH    R0,=H'10'                                                        
         STC   R0,HEAD4+56                                                      
         B     HDHK8                                                            
*                                                                               
HDHK7    DS    0H                                                               
         MVC   WORK(4),QAREA+49                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(9,H4+71)                                   
         MVC   H4+54(16),=C'CURRENT MONTH OF'                                   
*                                                                               
HDHK8    DS    0H                                                               
         CLI   RCSUBPRG,15         SPECIAL PRD SUMMARY FORMAT                   
         BE    HDHKXX              EVERYTHING NOW DONE IN A801                  
*                                                                               
         CLI   RCSUBPRG,10         INVOICE LIST                                 
         BE    HDHKXX                                                           
         CLI   RCSUBPRG,50         INVOICE LIST                                 
         BE    HDHKXX                                                           
         CLI   RCSUBPRG,6                                                       
         BH    HDHKX               ONLY SPROGS 3,4,5 USE PRODUCT                
         CLI   RCSUBPRG,3                                                       
         BL    HDHKX                                                            
         CLI   QOPT2,C'Y'          SEE IF COMBINING ALL PRDS                    
         BE    HDHK20                                                           
         MVC   H5(7),=C'PRODUCT'                                                
         MVC   H5+9(3),MYKPRD                                                   
         L     R2,APRDTAB                                                       
HDHK9    CLI   0(R2),X'FF'         END OF TABLE                                 
         BNE   HDHK10                                                           
         B     HDHKX               PRD NOT IN TABLE                             
*                                IS OK  - NET UNITS DON'T SET PRDTAB            
HDHK10   CLC   0(3,R2),MYKPRD                                                   
         BE    HDHK12                                                           
         LA    R2,23(R2)                                                        
         B     HDHK9                                                            
*                                                                               
HDHK12   DS    0H                                                               
         MVC   H5+13(20),3(R2)     PRD NAME                                     
         B     HDHKX                                                            
*                                                                               
HDHK15   CLI   QOPT2,C'Y'          SEE IF DOING ALL PRDS TOGETHER               
         BNE   *+10                                                             
HDHK20   MVC   H5(12),=C'ALL PRODUCTS'                                          
*                                                                               
         CLI   QOPT4,C'D'          NO STA OR MOS BREAKOUT                       
         BE    HDHKXX              DOWNLOAD                                     
         CLI   QOPT5,C'S'          STATION DOWNLOAD                             
         BE    HDHKXX                                                           
         B     HDHKX                                                            
*                                                                               
HDHKX    DS    0H                                                               
         CLI   QOPT4,C'B'          SUMMARY VERSION                              
         BE    HDHKXX                                                           
         CLI   QOPT4,C'D'          SUMMARY VERSION                              
         BE    HDHKXX              DOWNLOADED                                   
*                                                                               
         CLI   QOPT4,C'M'          NO MTH OF SERVICE BREAKOUT                   
         BE    HDHKXX                                                           
         CLI   RCSUBPRG,5                                                       
         BNL   HDHKX5                                                           
         MVC   H8+10(3),=C'MOS'                                                 
         MVC   H9+9(6),=6C'-'                                                   
         B     HDHKXX                                                           
*                                                                               
HDHKX5   MVC   H8+5(6),=C'MTH OF'                                               
         MVC   H9+4(7),=C'SERVICE'                                              
         MVC   H10+4(7),=7C'-'                                                  
         B     HDHKXX                                                           
*                                                                               
HDHKXX   XIT1                                                                   
         LTORG                                                                  
*                                                                               
SAVERC   DS    F             FROM BASE PROGRAM                                  
SAVERA   DS    F             FROM BASE PROGRAM                                  
*                                                                               
         EJECT                                                                  
TBCLTL   CSECT                                                                  
         NMOD1 0,TBCLTL                                                         
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,RC,RA                                                    
         LA    R9,SPACEND                                                       
         USING SPA8WRKD,R9                                                      
         LA    R8,TBCLTL+4095                                                   
         LA    R8,1(R8)                                                         
         USING TBCLTL+4096,R8      ** NOTE USE OF SECOND BASE REGISTER          
         CLI   CINVSW,1            SEE IF I HAVE CURRENT INVS                   
         BNE   TBCL                NO                                           
         MVI   RCSUBPRG,10                                                      
         L     RF,ADAGY                                                         
         CLI   AGYPROF+7-AGYKEY(RF),C'C'       SEE IF CANADIAN                  
         BNE   *+8                                                              
         MVI   RCSUBPRG,50                                                      
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+28(6),=C'TOTAL*'                                               
         EDIT  (P8,CINVNET),(14,P+37),2,COMMAS=YES,FLOAT=-                      
         EDIT  (P8,CINVGRS),(14,P+53),2,COMMAS=YES,FLOAT=-                      
         EDIT  (P8,CINVAMT),(14,P+69),2,COMMAS=YES,FLOAT=-                      
         L     RF,ADAGY                                                         
         CLI   AGYPROF+7-AGYKEY(RF),C'C'       SEE IF CANADIAN                  
         BNE   TBCL0                                                            
         EDIT  (P8,CINVGST),(14,P+84),2,COMMAS=YES,FLOAT=-                      
         MVI   P+98,C'*'                                                        
         EDIT  (P8,CINVPST),(14,P+99),2,COMMAS=YES,FLOAT=-                      
         MVI   P+113,C'*'                                                       
*                                                                               
TBCL0    DS    0H                                                               
         MVI   P+51,C'*'                                                        
         MVI   P+67,C'*'                                                        
         MVI   P+83,C'*'                                                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         CLI   AINVSW,X'01'       SEE IF CURRENT AOR BILLS                      
         BNE   TBCL0A                                                           
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+28(37),=C'AOR BILLING - NOT REFLECTED IN TOTALS'               
         EDIT  (P8,AINVAMT),(14,P+69),2,COMMAS=YES,FLOAT=-                      
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
**                                                                              
**       NOTE CMINVSW NO LONGER GETS SET                                        
**                                                                              
TBCL0A   CLI   CMINVSW,X'01'       SEE IF CURRENT COM BILLS                     
         BNE   TBCL                                                             
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+28(37),=C'COM BILLING - NOT REFLECTED IN TOTALS'               
         EDIT  (P8,CMINVAMT),(14,P+69),2,COMMAS=YES,FLOAT=-                     
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
TBCL     EQU   *                                                                
         CLI   NETOPT,C'N'        SEE IF DOING NEW NETWORK                      
         BE    NTU8                NO                                           
*                                                                               
TBWW     DS    0H                  READ FOR WUNDERMAN RECORDS                   
*                                                                               
TBWWXX   B     TBRDCLR             REST IS SAME AS SPOT                         
*                                                                               
         SPACE 2                                                                
TSORTIN  NTR1                                                                   
         MVC   P+1(8),=C'SORT IN='                                              
         GOTO1 HEXOUT,DMCB,WORK,P+10,56,0                                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         XIT1                                                                   
*                                                                               
*              USE NETIO TO READ UNITS                                          
*                                                                               
NTU8     DS    0H                                                               
*                      UNIT PROCESSING NOW IN IT'S OWN CSECT                    
         GOTO1 VPROCNET,DMCB,(RC)                                               
         B     TBCL1               REST SAME AS SPOT                            
         EJECT                                                                  
*                                                                               
*        READ CLEARANCE STATUS RECORDS FOR UNCLEARANCES                         
*        FOR THIS CLIENT AND PASS THEM TO BUFFALO                               
*                                                                               
TBRDCLR  MVC   KEY1,KEY            SAVE CURRENT KEY                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLRSTATD,R6                                                      
         MVC   CLSKTYPE,=X'0DF6'   RECORD TYPE (PASSIVE)                        
         MVC   CLSKAGMD,BAGYMD                                                  
         MVC   CLSKCLT,BCLT      CAN USE BCLT                                   
         GOTO1 HIGH                                                             
         B     TBRCS1X                                                          
TBRCS1   GOTO1 SEQ                                                              
*                                                                               
TBRCS1X  CLC   KEY(5),KEYSAVE    THRU CLIENT                                    
         BNE   TBRCS4                                                           
         L     R6,ADSTABUC        READ INTO STATION BUCKET AREA                 
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         LA    R2,CLSELEMS    POINT TO FIRST ELEMENT                            
*                                                                               
         USING CLSTEL01,R2                                                      
         MVI   ELCODE,X'01'                                                     
         CLI   0(R2),X'01'    FIRST ELEMENT TO PROCESS?                         
         BE    TBRCS2C                                                          
TBRCS2   BAS   RE,CNEXTEL                                                       
         BNE   TBRCS1                                                           
TBRCS2C  DS    0H                                                               
         TM    CLSTSTAT,X'01'   UNCLEARANCE?                                    
         BNO   TBRCS2           NO- THEN SKIP                                   
         OC    CLSTOCLR,CLSTOCLR  MUST HAVE ORIGINAL CLEARED DATE               
         BZ    TBRCS2           TO PROCESS                                      
*                                                                               
         CLC   QPRD,=C'ALL'     IF DOING ALL PRODUCTS                           
         BE    *+14             DON'T CHECK PRODUCT                             
*                                                                               
         CLC   CLSTPRD,BPRD     RIGHT WORK PRODUCT?                             
         BNE   TBRCS2           NO- THEN SKIP                                   
         OC    CMSTART,CMSTART   SEE IF I HAVE A "CURRENT" MONTH                
         BZ    TBRCS2D          NO - THEN PROCESS                               
         CLC   CLSTOCLR,CMSTART   BEFORE CURRENT MONTH?                         
         BL    TBRCS2D            YES - THEN PROCESS                            
*                                ORIGINAL DATE WAS IN OR AFTER                  
*                                CURRENT MONTH                                  
*                                ONLY PROCESS IF CLEARANCE DATE                 
*                                IS AFTER CURRENT MONTH                         
*        I SHOULD SKIP UNCLEARANCES IN THE CURRENT MONTH                        
*        THAT UNCLEARED A CLEARANCE IN THE CURRENT MONTH                        
*                                                                               
         CLC   CLSTCLRD,CMSTART  IS CLEARANCE BEFORE CURRENT MTH?               
         BL    TBRCS2             YES THEN SKIP                                 
         CLC   CLSTCLRD,CMEND    AFTER CURRENT MTH END?                         
         BNH   TBRCS2             NO - THEN CAN SKIP                            
*                                 OTHERWISE MUST PROCESS                        
*                                                                               
TBRCS2D  DS    0H                                                               
*                                                                               
         XC    TBREC,TBREC                                                      
         CLI   QOPT2,C'Y'        SEE IF COMBINING PRODUCTS                      
         BE    TBRCS2E                                                          
         CLC   QPRD,=C'POL'                                                     
         BNE   TBRC13A                                                          
         MVC   TBKPRD,=C'POL'                                                   
         B     TBRC13AX                                                         
*              MUST GET PRD CODE FROM CLIENT HEADER                             
TBRC13A  L     R5,VCLIST       COMBINED CLIST AND CLIST2                        
TBRC13A5 CLC   3(1,R5),CLSTPRD                                                  
         BE    TBRC13A8     FOUND                                               
         LA    R5,4(R5)                                                         
         CLI   0(R5),0      END OF LIST                                         
         BNE   TBRC13A5                                                         
         DC    H'0'               MUST BE ABLE TO FIND THE PRODUCT              
*                                                                               
TBRC13A8 MVC   TBKPRD,0(R5)                                                     
TBRC13AX DS    0H                                                               
*                                                                               
TBRCS2E  DS    0H                                                               
         GOTO1 MSUNPK,DMCB,(X'80',CLSKMKT),WORK,SVTBSTA                         
*                                                                               
         DROP  R6                                                               
*                                                                               
         CLC   SVTBSTA+5(3),SPACES                                              
         BE    *+8                                                              
         MVI   SVTBSTA+4,C'/'                                                   
*                                                                               
         MVC   TBKSTA(3),CLT                                                    
         MVC   TBKSTA+3(3),TBKPRD                                               
         OC    TBKSTA,SPACES                                                    
         CLI   QOPT4,C'B'          SUPRESSING BOTH                              
         BE    TBRCS2E5                                                         
         CLI   QOPT4,C'D'          SPECIAL DOWNLOAD FORMAT                      
         BE    TBRCS2E5                                                         
*                                                                               
         MVC   TBKSTA,=CL8'ALL'                                                 
         CLI   QOPT4,C'S'          NO STATION BREAKOUT                          
         BE    TBRCS2E5                                                         
*                                                                               
         MVC   TBKSTA,SVTBSTA                                                   
*                                                                               
TBRCS2E5 DS    0H                                                               
         ZAP   TBBILLN,=P'0'                                                    
         ZAP   TBBILLG,=P'0'                                                    
         ZAP   TBPAIDN,=P'0'                                                    
         ZAP   TBPAIDG,=P'0'                                                    
         CLI   QOPT4,C'B'          BOTH STA AND MOS                             
         BE    TBRCS2F                                                          
         CLI   QOPT4,C'D'          BOTH STA AND MOS                             
         BE    TBRCS2F             WITH DOWNLOAD                                
         CLI   QOPT4,C'M'          NO MTH OF SERVICE BREAKOUT                   
         BE    TBRCS2F                                                          
         GOTO1 BRDMON,DMCB,(X'FF',CLSTSTDT),WORK,RR=RELO                        
         SR    R0,R0                                                            
         ICM   R0,3,WORK                                                        
         SRDL  R0,9                                                             
         SRL   R1,28                                                            
         STC   R0,WORK                                                          
         STC   R1,WORK+1                                                        
*                                                                               
         MVC   TBKMOS,WORK                                                      
*                                                                               
TBRCS2F  DS    0H                                                               
         CLI   CLSTEST,0      CLEARED BY ESTIMATE?                              
         BE    TBRCS3         CAN'T SHOW                                        
         CLC   QEST(6),=C'001255'    ALL ESTIMATES?                             
         BE    TBRCS2H                                                          
         CLC   CLSTEST,MYBEST      CHECK VERSES REQUESTED EST RANGE             
         BL    TBRCS2              LOW - SKIP                                   
         CLC   CLSTEST,MYBESTE                                                  
         BH    TBRCS2              HIGH - SKIP                                  
*                                                                               
TBRCS2H  TM    ESTTBSW,2           SEE IF SHOWING EST WITHIN MOS                
         BZ    *+10                                                             
         MVC   TBKEST2,CLSTEST                                                  
TBRCS3   DS    0H                                                               
         CLC   CLSTCLRD,CMEND   SEE IF AFTER CURRENT MONTH                      
         BH    TBRCS3C          MUST STILL POST ORIGINAL DATE                   
         CLC   CLSTCLRD,BQENDP  AFTER AS OF DATE?                               
         BH    TBRCS3C     YES- MUST STILL POST ORIGINAL DATE                   
*                                                                               
         MVC   FULL,CLSTGRS   GROSS PAID                                        
         L     R0,FULL                                                          
         LCR   R0,R0          REVERSE SIGN                                      
         CVD   R0,DUB                                                           
         ZAP   TBPAIDG,DUB                                                      
         MVC   FULL,CLSTNET   NET PAID                                          
         L     R0,FULL                                                          
         LCR   R0,R0          REVERSE SIGN                                      
         CVD   R0,DUB                                                           
         LTR   R0,R0          SEE IF ANY RECORDED                               
         BNZ   *+8            IF NOT THEN CALCULATE                             
         BAS   RE,CALCNET  RETURNS CALCULATED NET IN DUB                        
*                                                                               
         ZAP   TBPAIDN,DUB                                                      
         MVC   TBKPDDTE,CLSTCLRD     DATE CLEARED                               
         CLI   QOPT7,C'Y'          SORT TRACING?                                
         BNE   *+14                                                             
         MVC   WORK(L'TBREC),TBREC                                              
         BAS   RE,TSORTIN                                                       
         GOTO1 SORTER,DMCB,=C'PUT',TBREC                                        
*                                                                               
TBRCS3C  DS    0H                                                               
         CLC   CLSTOCLR,CMEND  ORIGINAL DATE AFTER CMEND?                       
         BH    TBRCS3X         YES - DONT POST                                  
         CLC   CLSTOCLR,BQENDP ORIGINAL DATE AFTER AS OF DATE?                  
         BH    TBRCS3X         YES - DON'T POST                                 
*                                                                               
         MVC   FULL,CLSTGRS   GROSS PAID                                        
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         ZAP   TBPAIDG,DUB                                                      
         MVC   FULL,CLSTNET   NET PAID                                          
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         LTR   R0,R0       SEE IF ANY RECORDED                                  
         BNZ   *+8         IF NOT - CALCULATE                                   
         BAS   RE,CALCNET  RETURNS CALCULATED NET IN DUB                        
*                                                                               
         ZAP   TBPAIDN,DUB                                                      
         MVC   TBKPDDTE,CLSTOCLR    ORIG DATE CLEARED                           
         CLI   QOPT7,C'Y'          SORT TRACING?                                
         BNE   *+14                                                             
         MVC   WORK(L'TBREC),TBREC                                              
         BAS   RE,TSORTIN                                                       
         GOTO1 SORTER,DMCB,=C'PUT',TBREC                                        
*                                                                               
TBRCS3X  B     TBRCS2         CHECK FOR MORE ELEMENTS                           
*                                                                               
TBRCS4   XC    KEY,KEY        END OF RECORDS FOR CLIENT                         
         MVC   KEY,KEY1       RESTORE KEY                                       
         GOTO1 HIGH           AND SEQUENTIAL READ                               
         EJECT                                                                  
*                                                                               
TBCL1    MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,3                                                       
         TM    ESTTBSW,2           SEE IF SHOWING EST WITHIN MOS                
         BZ    *+8                                                              
         MVI   RCSUBPRG,4                                                       
         MVI   MLINCNT,0           ZERO MOS LINE COUNT                          
         MVI   DCPAYSW,0                                                        
         MVI   ICBILLSW,0                                                       
         MVI   MTHPSW,0                                                         
         MVI   STAPSW,0                                                         
*                                                                               
         LA    R4,PRDBALI          CLEAR PRD SUMMARY ACCUMS                     
         LA    R5,6                USED FOR QOPT4 =B                            
*                                                                               
TBCL1C   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R5,TBCL1C                                                        
*                                                                               
         LA    R4,DTETOTS                                                       
         LA    R5,26                                                            
*                                                                               
TBCL2    ZAP   0(8,R4),=P'0'       CLEAR DTE,INV,MOS,EST,STA TOTALS             
         LA    R4,8(R4)                                                         
         BCT   R5,TBCL2                                                         
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'01',BUFFBUFF),(X'80',1)                
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'02',BUFFBUFF),(X'80',1)                
*                                  CLEAR CLT ACCUMS                             
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'04',BUFFBUFF),(X'80',1)                
         XC    MYKEY,MYKEY                                                      
         CLI   SORTACT,C'Y'                                                     
         BNE   TBCL5X                                                           
TBCL5    GOTO1 SORTER,DMCB,=C'GET'                                              
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   TBCL10              LAST REC                                     
         BAS   RE,PDDTEEND                                                      
         BAS   RE,INVEND                                                        
         BAS   RE,EST2END                                                       
         BAS   RE,MOSEND                                                        
         BAS   RE,STAEND                                                        
         BAS   RE,PRDEND                                                        
         BAS   RE,CLTEND                                                        
TBCL5X   GOTO1 SORTER,DMCB,=C'END'                                              
         B     TBEXIT                                                           
         SPACE 2                                                                
TBCL10   DS    0H                                                               
         L     R3,DMCB+4           ADDR OF SORTED REC                           
         MVC   TBREC,0(R3)                                                      
         CLI   QOPT7,C'Y'          TRACING SORT CALLS                           
         BNE   *+14                                                             
         MVC   WORK(L'TBREC),TBREC                                              
         BAS   RE,SORTOUT                                                       
         OC    MYKEY,MYKEY                                                      
         BZ    TBCL30              FIRST RECORD                                 
         CLC   TBREC(31),MYKEY                                                  
         BE    TBCL15              SAME PAY DATE                                
         BAS   RE,PDDTEEND                                                      
*                                                                               
TBCL15   CLC   TBREC(20),MYKEY                                                  
         BE    TBCL16                                                           
         BAS   RE,INVEND           INVOICE END                                  
*                                                                               
TBCL16   DS    0H                                                               
         CLC   TBREC(17),MYKEY                                                  
         BE    TBCL18                                                           
         BAS   RE,EST2END                                                       
*                                                                               
TBCL18   CLC   TBREC(14),MYKEY                                                  
         BE    TBCL20                                                           
         BAS   RE,MOSEND                                                        
TBCL20   CLC   TBREC(12),MYKEY                                                  
         BE    TBCL22                                                           
         BAS   RE,EST1END                                                       
*                                                                               
TBCL22   CLC   TBREC(11),MYKEY                                                  
         BE    TBCL24                                                           
         BAS   RE,STAEND                                                        
*                                                                               
TBCL24   CLC   TBREC(3),MYKEY                                                   
         BE    TBCL30                                                           
         BAS   RE,PRDEND                                                        
*                                                                               
TBCL30   DS    0H                                                               
         OC    TBKINVMO(3),TBKINVMO      SEE IF BILLING OR PAYMENT              
         BNZ   PROCB                                                            
*                                                                               
PROCP    DS    0H                  PAYMENT                                      
         CLC   TBKPDDTE,CMSTART    SEE IF IN CURRENT MTH                        
         BL    PD20                                                             
         CLC   TBKPDDTE,CMEND                                                   
         BH    PD20                                                             
*                                  ADD TO DATE TOTALS                           
         AP    DCPAYN,TBPAIDN                                                   
         AP    DCPAYG,TBPAIDG                                                   
         MVI   DCPAYSW,C'Y'                                                     
*                                  ADD TO MTH OF SERV TOTALS                    
         AP    MCPAYN,TBPAIDN                                                   
         AP    MCPAYG,TBPAIDG                                                   
         MVI   MOSCACT,C'Y'        SET CURRENT ACTIVITY                         
         MVI   ESTCACT,C'Y'        SET CURRENT ACTIVITY                         
         TM    ESTTBSW,X'04'           SEE IF DOING PRD SUM BY EST              
         BZ    PDX                                                              
         AP    ECPAYN,TBPAIDN                                                   
         AP    ECPAYG,TBPAIDG                                                   
         B     PDX                                                              
PD20     AP    MPPAYN,TBPAIDN                                                   
         AP    MPPAYG,TBPAIDG                                                   
         TM    ESTTBSW,X'04'           SEE IF DOING PRD SUM BY EST              
         BZ    PDX                                                              
         AP    EPPAYN,TBPAIDN                                                   
         AP    EPPAYG,TBPAIDG                                                   
PDX      DS    0H                                                               
         MVC   MYKEY,TBREC                                                      
         MVC   MOSACT(7),=7C'Y'                                                 
         B     TBCL5                                                            
         SPACE 2                                                                
PROCB    DS    0H                                                               
         CLC   TBBILDTE,CMSTART    SEE IF BILLED IN CURRENT MTH                 
         BL    BL20                                                             
         CLC   TBBILDTE,CMEND                                                   
         BH    BL20                                                             
*                                  ADD TO INVOICE TOTALS                        
         AP    ICBILLN,TBBILLN                                                  
         AP    ICBILLG,TBBILLG                                                  
         MVI   ICBILLSW,C'Y'                                                    
*                                  ADD TO MTH OF SERV TOTALS                    
         AP    MCBILLN,TBBILLN                                                  
         AP    MCBILLG,TBBILLG                                                  
         MVI   MOSCACT,C'Y'                                                     
         MVI   ESTCACT,C'Y'        SET CURRENT ACTIVITY                         
         TM    ESTTBSW,X'04'           SEE IF DOING PRD SUM BY EST              
         BZ    BLX                                                              
         AP    ECBILLN,TBBILLN                                                  
         AP    ECBILLG,TBBILLG                                                  
         B     BLX                                                              
*                                                                               
BL20     AP    MPBILLN,TBBILLN     POST TO PREVIOUS                             
         AP    MPBILLG,TBBILLG                                                  
         TM    ESTTBSW,X'04'           SEE IF DOING PRD SUM BY EST              
         BZ    BLX                                                              
         AP    EPBILLN,TBBILLN                                                  
         AP    EPBILLG,TBBILLG                                                  
*                                                                               
BLX      MVC   MYKEY,TBREC                                                      
         MVC   MOSACT(7),=7C'Y'                                                 
         B     TBCL5                                                            
         EJECT                                                                  
PDDTEEND DS    0H             PAYMENT DATE END                                  
         CLI   DCPAYSW,C'Y'        CHK FOR CURRENT PAYMENTS                     
         BNER  RE                                                               
         L     R4,ANXTPD           ADD TO MOS PAYMENTS TABLE                    
         MVC   0(5,R4),MYKPDDTE    SAVE PAID DATE & PRD                         
         MVC   5(16,R4),DCPAYN                                                  
         MVC   21(1,R4),MYPCRCK    CKCK INDICATOR                               
         MVC   22(3,R4),MYPREP     REP                                          
                                                                                
         LA    R4,25(R4)                                                        
         MVC   0(5,R4),=5X'FF'                                                  
         ST    R4,ANXTPD                                                        
         ZAP   DCPAYN,=P'0'                                                     
         ZAP   DCPAYG,=P'0'                                                     
         MVI   DCPAYSW,0                                                        
         BR    RE                                                               
         SPACE 2                                                                
INVEND   DS    0H                                                               
         CLI   ICBILLSW,C'Y'                                                    
         BNER  RE                                                               
         L     R4,ANXTBL                                                        
         MVC   0(1,R4),MYKINVMO    MONTH                                        
***INVMTH NI   0(R4),X'0F'         SET OFF YEAR BITS                            
         MVC   1(2,R4),MYKINV                                                   
         MVC   3(2,R4),MYBILDT                                                  
***INVMTH MVI   3(R4),0             SO OLD DETAIL BILLING WILL NOT LOOK         
*                                  LIKE END OF TABLE                            
         MVC   5(16,R4),ICBILLN                                                 
         LA    R4,21(R4)                                                        
         MVC   0(5,R4),=5X'FF'                                                  
         ST    R4,ANXTBL                                                        
         MVI   ICBILLSW,0                                                       
         ZAP   ICBILLN,=P'0'                                                    
         ZAP   ICBILLG,=P'0'                                                    
         BR    RE                                                               
         SPACE 2                                                                
EST1END  DS    0H                                                               
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
CALCNET  NTR1                                                                   
*                                                                               
         ZAP   DUB,=P'0'           ZERO RETURNED NET                            
         OC    CLSTGRS,CLSTGRS     DO I HAVE A GROSS?                           
         BZ    CALCNX                                                           
*                                                                               
         CVB   R1,TBPAIDG          USE ADJUSTED HERE                            
         ZAP   DUB,=P'15000'       ASSUME 15 %                                  
         CVB   R0,DUB                                                           
         L     RF,=F'100000'                                                    
         SR    RF,R0                                                            
         MR    RE,R1                                                            
         SLDL  RE,1                                                             
         D     RE,=F'100000'                                                    
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                RF=NET                                       
*                                                                               
         CVD   RF,DUB                                                           
*                                                                               
CALCNX   XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
CNEXTEL  DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    CNEXTEL2                                                         
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     CNEXTEL                                                          
*                                                                               
CNEXTEL2 DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         EJECT                                                                  
*       END OF MONTH OF SERVICE ROUTINE                                         
MOSEND   NTR1                                                                   
         CLI   MOSACT,C'Y'                                                      
         BNE   MOSEX                                                            
         XC    BUFREC,BUFREC                                                    
         MVC   BUFPBILN(64),MPBILLN                                             
*                                                                               
         JIF   QOPT5,EQ,C'S',AND,MYA8PROF+10,EQ,C'M',MOSE,JUMP=N                
         CLI   MOSCACT,C'Y'        SEE IF ACTIVE IN CURRENT MTH                 
         BNE   MOSE45              NO - SKIP TO TOTALS                          
*                                                                               
         CLI   QOPT4,C'B'         SEE IF SUPPRESSING STA + MOS                  
         BE    MOSE45             SKIP TO TOTALS                                
         CLI   QOPT4,C'D'         DOWN LOADING                                  
         BE    MOSE45             SKIP TO TOTALS                                
         CLI   QOPT5,C'S'         STATION DOWNLOAD                              
         BNE   MOSE1              NO PROCESS                                    
*                                                                               
         CLI   MYA8PROF+10,C'M'   MOS ONLY?                                     
         BE    MOSE1                                                            
*                                                                               
         CLI   MYA8PROF+10,C'Y'   SEE IF DOWNLOADING MOS DETAILS                
         BE    MOSE1                                                            
         B     MOSE45             NO - SKIP TO TOTALS                           
*                                                                               
*        MTH OF SERVICE DOWNLOAD - CHECK FOR BALANCE FORWARD                    
*        OR CURRENT ACTIVITY                                                    
*                                                                               
MOSE     LA    R4,MOSTOTS                                                       
         ZAP   DOUBLE,0(8,R4)      PREV BILLED                                  
         SP    DOUBLE,8(8,R4)      PREV CLEARED                                 
         CP    DOUBLE,=P'0'       ANY BALANCE FORWARD?                          
         BNE   MOSE1              YES- CONTINUE                                 
         CLI   MOSCACT,C'Y'       CURRENT ACTIVITY?                             
         BE    MOSE1                                                            
         B     MOSE45             SKIP TO TOTALS                                
*                                                                               
MOSE1    DS    0H                                                               
         TM    ESTTBSW,2           SEE IF SORTING ON EST                        
         BNZ   MOSE40              SKIP TO MTH TOTALS                           
         MVI   MTHPSW,0                                                         
         L     R5,AINVTAB                                                       
         L     R6,APDTAB                                                        
MOSE1A   DS    0H                                                               
         CLI   STAPSW,C'Y'          SEE IF I'VE PRINTED STATION                 
         BNE   MOSE1B                                                           
         ZIC   R3,LINE             REPEAT IF NEW PAGE                           
         LA    R3,1(R3)                                                         
         STC   R3,X                                                             
         CLC   X(1),MAXLINES                                                    
         BNH   MOSE2                                                            
         MVI   MTHPSW,0            SO MOS WILL PRINT ALSO                       
MOSE1B   DS    0H                                                               
         BAS   RE,PRNTSTA                                                       
MOSE1D   MVI   STAPSW,C'Y'                                                      
MOSE2    CLI   MTHPSW,C'Y'         SEE IF MTH PRINTED ALREADY                   
         BE    MOSE3                                                            
         CLI   MYKMOS,0            NO MTH OF SERVICE BREAKOUT                   
         BE    MOSE3                                                            
*                                                                               
         JIF   MYA8PROF+14,EQ,C'Y',AND,QOPT5,EQ,C'S',MOSE2A,JUMP=N              
         JIF   MYA8PROF+14,EQ,C'M',AND,QOPT5,EQ,C'S',MOSE2A,JUMP=N              
         JIF   MYA8PROF+14,EQ,C'D',AND,QOPT5,EQ,C'S',MOSE2A,JUMP=N              
*                                                                               
         CLI   MYKMOS+1,12         13TH PERIOD                                  
         BNH   MOSE2B                                                           
MOSE2A   LA    R2,MYKMOS                                                        
         LA    R3,P+9                                                           
         BAS   RE,MTH13                                                         
         B     MOSE2C                                                           
MOSE2B   GOTO1 DATCON,DMCB,(3,MYKMOS),(9,P+9)                                   
MOSE2C   MVI   MTHPSW,C'Y'                                                      
*                                                                               
MOSE3    DS    0H                                                               
         ST    R5,INVADR     SAVE ADDRESSES - FOR DOWNLOAD                      
         ST    R6,PAYADR                                                        
MOSE5    CLC   0(5,R5),=5X'FF'          END OF INVOICE TABLE                    
         BE    MOSE20                                                           
         EDIT  (P8,5(R5)),(13,P+36),2,COMMAS=YES,FLOAT=-                        
         CLC   0(3,R5),=X'0FFFFF'                                               
         BE    MOSE10              OLD DETAIL BILLING- NO INV #                 
***INVMTH                                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(2,3(R5)),(0,DUB)                                    
         GOTO1 AFMTINO,DMCB,DUB,(2,1(R5)),(MED,PROFB1),PROFB1X                  
*                                                                               
         L     RE,DMCB+4                                                        
         MVC   P+51(7),0(RE)                                                    
MOSE10   EDIT  (P8,13(R5)),(13,P+101),2,COMMAS=YES,FLOAT=-                      
         LA    R5,21(R5)                                                        
MOSE20   DS    0H                                                               
         CLC   0(5,R6),=5X'FF'          END OF PAYMENT TABLE                    
         BE    MOSE30                                                           
         EDIT  (P8,5(R6)),(13,P+59),2,COMMAS=YES,FLOAT=-                        
         GOTO1 DATCON,DMCB,(2,0(R6)),(5,P+76)                                   
         EDIT  (P8,13(R6)),(13,P+117),2,COMMAS=YES,FLOAT=-                      
         CLI   21(R6),0     CR/CK IND?                                          
         BE    MOS2E25                                                          
         TM    21(R6),X'10'    CK?                                              
         BNO   MOS2E22                                                          
         MVC   P+73(2),=C'CK'                                                   
         B     MOS2E25                                                          
*                                                                               
MOS2E22  TM    21(R6),X'20'    CR?                                              
         BNO   MOS2E25                                                          
         MVC   P+73(2),=C'CR'                                                   
*                                                                               
MOS2E25  OC    22(3,R6),22(R6)    REP?                                          
         BZ    MOS2E28                                                          
         MVC   P+85(3),22(R6)     MAY HAVE A REP                                
*                                                                               
MOS2E28  LA    R6,25(R6)                                                        
MOSE30   DS    0H                                                               
*                                                                               
         JIF   QOPT5,EQ,C'S',AND,MYA8PROF+10,EQ,C'M',MOSE30B,JUMP=N             
*        MUST STILL PROCESS IF DOING MTH OF SERV DOWNLOAD                       
*                                                                               
         CLC   P+36(45),SPACES                                                  
         BE    MOSE40              NOTHING LEFT TO PRINT                        
*                                                                               
         CLI   QOPT5,C'S'          SEE IF DOING STATION DOWNLOAD                
         BNE   MOSE35                                                           
         CLI   MYA8PROF+10,C'M'  MOS ONLY?                                      
         BE    MOSE30B                                                          
         CLI   MYA8PROF+10,C'Y'  SEE IF INCLUDING MOS,INVS,AND DATES            
         BNE   MOSE45                                                           
*                               BUILD SPECIAL PRINT LINE FOR DOWNLOAD           
MOSE30B  MVC   P+90(8),P+9      SAVE MOS IN P+90                                
         MVI   P+98,C' '        JUST IN CASE SOMETHING THERE                    
         BAS   RE,PRNTSTA       PUTS STATION IN P                               
         MVC   P+30(8),P        MOVE TO P+20                                    
         MVI   P+38,C' '        JUST IN CASE SOMETHING THERE                    
         MVC   P(14),SPACES                                                     
         MVI   P,C'S'        SYSTEM                                             
         CLI   NETOPT,C'N'                                                      
         BNE   *+8                                                              
         MVI   P,C'N'                                                           
         MVC   P+2(1),QMED                                                      
*                                                                               
         CLC   P+30(8),=CL8'ALL'    STATIONS COMBINED?                          
         BE    MOSE30D                                                          
         JIF   NETOPT,NE,C'N',OR,MYA8PROF+9,NE,C'Y',MOSE30D,JUMP=N              
         MVC   P+2(1),P+30        SUBMEDIA                                      
         CLI   P+30,C' '         MISSING?                                       
         BNE   *+8                                                              
         MVI   P+2,C'N'          DEFAULT TO N                                   
         CLC   P+32(3),=C'ALL'   NETWORK SUBMEDIA +STATION COMBINED?            
         BNE   MOSE30C                                                          
         MVC   P+30(8),=CL8'ALL'                                                
         B     MOSE30D                                                          
*                                                                               
MOSE30C  CLC   P+32(6),=C'MANUAL'  NETWORK SUBMEDIA +MANUAL?                    
         BNE   MOSE30D                                                          
         MVC   P+30(8),=CL8'MANUAL'                                             
*                                                                               
MOSE30D  DS    0H                                                               
         MVC   P+4(L'SAVCOFF),SAVCOFF                                           
*                                                                               
         MVC   P+120(L'SAVAOFF),SPACES                                          
         CLI   PROFA8A+1,C'Y'          DOWNLOAD ACC OFFICE                      
         BNE   *+10                                                             
         MVC   P+120(L'SAVAOFF),SAVAOFF                                         
*                                                                               
         MVC   P+6(3),CLT             CLIENT                                    
         MVC   P+10(3),MYKPRD         PRODUCT                                   
         CLI   QOPT2,C'Y'             SEE IF COMBINING PRDS                     
         BNE   *+10                                                             
         MVC   P+10(3),=C'ALL'                                                  
         GOTO1 VDOWNLD,DMCB,(C'M',(RC))   DOWNLOAD MTH OF SERVICE LINE          
         MVC   P,SPACES                                                         
         MVI   MTHPSW,0            SO MOS WILL REPRINT                          
*                                                                               
         CLI   MYA8PROF+10,C'Y'    INV NO. AND CLEAR DATE?                      
         BNE   MOSE45              SKIP NOW TO TOTALS                           
*                                                                               
         B     MOSE1A              GO PROCESS NEXT                              
*                                                                               
MOSE35   DS    0H                                                               
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         ZIC   R1,MLINCNT          COUNT MOS PRINT LINES                        
         LA    R1,1(R1)                                                         
         STC   R1,MLINCNT                                                       
         B     MOSE1A                                                           
*                                                                               
MOSE40   DS    0H                  PRINT MOS TOTALS                             
         CLI   MLINCNT,1           SEE IF ONLY 1 LINE PRINTED FOR MOS           
         BNH   MOSE45             YES SKIP TOTAL LINE                           
         CLI   STAPSW,C'Y'          SEE IF I'VE PRINTED STATION                 
         BNE   MOSE41                                                           
         ZIC   R3,LINE             REPEAT IF NEW PAGE                           
         LA    R3,1(R3)                                                         
         STC   R3,X                                                             
         CLC   X(1),MAXLINES                                                    
         BNH   MOSE42                                                           
MOSE41   DS    0H                                                               
         BAS   RE,PRNTSTA                                                       
MOSE41B  MVI   STAPSW,C'Y'                                                      
MOSE42   DS    0H                                                               
         CLI   MYKMOS,0            NO MTH OF SERVICE BREAKOUT                   
         BE    MOSE45                                                           
*                                                                               
         JIF   MYA8PROF+14,EQ,C'Y',AND,QOPT5,EQ,C'S',MOSE42B,JUMP=N             
         JIF   MYA8PROF+14,EQ,C'M',AND,QOPT5,EQ,C'S',MOSE42B,JUMP=N             
         JIF   MYA8PROF+14,EQ,C'D',AND,QOPT5,EQ,C'S',MOSE42B,JUMP=N             
*                                                                               
         CLI   MYKMOS+1,12         13TH PERIOD                                  
         BNH   MOSE43                                                           
MOSE42B  LA    R2,MYKMOS                                                        
         LA    R3,P+9                                                           
         BAS   RE,MTH13                                                         
         B     MOSE43A                                                          
MOSE43   GOTO1 DATCON,DMCB,(3,MYKMOS),(9,P+9)                                   
MOSE43A  MVI   P+15,C'*'                                                        
         EDIT  (P8,MCBILLN),(13,P+36),2,COMMAS=YES,FLOAT=-                      
         MVI   P+49,C'*'                                                        
         EDIT  (P8,MCPAYN),(13,P+59),2,COMMAS=YES,FLOAT=-                       
         MVI   P+72,C'*'                                                        
         EDIT  (P8,MCBILLG),(13,P+101),2,COMMAS=YES,FLOAT=-                     
         MVI   P+114,C'*'                                                       
         EDIT  (P8,MCPAYG),(13,P+117),2,COMMAS=YES,FLOAT=-                      
         MVI   P+130,C'*'                                                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                  BUFFALO MOS WITH ANY ACTIVITY                
*                                  TO TOTALS                                    
MOSE45   DS    0H                                                               
         MVI   BUFTYP,X'02'        PRODUCT TOTALS                               
*                                                                               
         CLI   NETOPT,C'N'           SEE IF NETPAK                              
         BNE   MOSE46                                                           
         CLI   MYA8PROF+9,C'Y'       NETPAK MEDIA BREAKOUT?                     
         BNE   MOSE46                                                           
         MVC   BUFNMED(1),MYKSTA+7   NETPAK SUB-MEDIA (MANUALS)                 
         CLI   MYKSTA+1,C' '       SEE IF SPECIAL STATIONS                      
         BNE   MOSE46                                                           
         MVC   BUFNMED,MYKSTA    NETPAK SUB-MEDIA                               
*                                                                               
MOSE46   DS    0H                                                               
         MVC   BUFMOS,MYKMOS                                                    
         TM    ESTTBSW,X'04'       EST WITHIN MOS ON PRD SUMMARY                
         BZ    *+10                NO                                           
         MVC   BUFEST(2),=2X'FF'                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   BUFTYP,X'04'                                                     
         GOTO1 (RF)                                                             
*                                                                               
         CLI   NETOPT,C'N'           SEE IF NETPAK                              
         BNE   MOSE47                                                           
         CLI   MYA8PROF+9,C'Y'       NETPAK MEDIA BREAKOUT?                     
         BNE   MOSE47                                                           
         MVI   BUFTYP,X'02'        PRODUCT                                      
         MVI   BUFNMED,X'FF'       ALL MEDIA COMBINED                           
         GOTO1 (RF)                                                             
         MVI   BUFTYP,X'04'        CLT TOTALS                                   
         GOTO1 (RF)                                                             
*                                                                               
MOSE47   DS    0H                                                               
         XC    BUFEST(2),BUFEST                                                 
         XC    BUFCLT,BUFCLT       ALSO CLEARS BUFNMED                          
         CLI   MULTOFF,C'Y'        SEE IF DOING OFF LIST REQ                    
         BNE   MOSE48                                                           
         MVI   BUFTYP,X'05'        YES -  DO OFFICE TOTALS                      
         MVC   BUFCLT,CLT                                                       
         GOTO1 (RF)                                                             
         MVC   BUFCLT,=3X'FF'                                                   
         GOTO1 (RF)                                                             
*                                                                               
MOSE48   MVI   BUFTYP,X'06'         NOW DO REQUEST TOTALS                       
         MVC   BUFCLT,CLT                                                       
         CLI   MULTOFF,C'Y'        SEE IF DOING OFFICE LIST REQ                 
         BNE   MOSE49                                                           
*                                                                               
         CLI   QOPT7,C'C'        SEE IF OVERRIDING MYA8PROF+8 TO CLT            
         BE    MOSE49                                                           
*                                                                               
         CLI   QOPT7,C'N'        SEE IF OVERRIDING MYA8PROF+8 TO OFF            
         BE    MOSE48C                                                          
*                                                                               
         CLC   QCLT(2),=C'$*'    ALL OFFICE REQ?                                
         BE    MOSE48C           DON'T RECAP BY CLIENT                          
*                                                                               
         CLI   MYA8PROF+8,C'C'      SEE IF REPORTING BY CLT                     
         BE    MOSE49                                                           
*                                                                               
MOSE48C  XC    BUFCLT,BUFCLT                                                    
         MVC   BUFCLT(L'SAVCOFF),SAVCOFF                                        
MOSE49   GOTO1 (RF)                                                             
         MVC   BUFCLT,=3X'FF'                                                   
         GOTO1 (RF)                                                             
         XC    BUFCLT,BUFCLT                                                    
MOSE50   DS    0H                                                               
         MVI   BUFTYP,X'02'                                                     
*                                                                               
         CLI   NETOPT,C'N'           SEE IF NETPAK                              
         BNE   MOSE51                                                           
         CLI   MYA8PROF+9,C'Y'       NETPAK MEDIA BREAKOUT?                     
         BNE   MOSE51                                                           
         MVC   BUFNMED(1),MYKSTA+7   NETPAK SUB-MEDIA (MANUALS)                 
         CLI   MYKSTA+1,C' '       SEE IF SPECIAL STATIONS                      
         BNE   MOSE51                                                           
         MVC   BUFNMED,MYKSTA    NETPAK SUB-MEDIA                               
*                                                                               
MOSE51   DS    0H                                                               
         MVC   BUFMOS,=2X'FF'                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   BUFTYP,X'04'                                                     
         GOTO1 (RF)                                                             
*                                                                               
         CLI   NETOPT,C'N'           SEE IF NETPAK                              
         BNE   MOSE53                                                           
         CLI   MYA8PROF+9,C'Y'       NETPAK MEDIA BREAKOUT?                     
         BNE   MOSE53                                                           
         MVI   BUFTYP,X'02'        PRODUCT                                      
         MVI   BUFNMED,X'FF'       ALL MEDIA COMBINED                           
         GOTO1 (RF)                                                             
         MVI   BUFTYP,X'04'        CLT TOTALS                                   
         GOTO1 (RF)                                                             
*                                                                               
MOSE53   DS    0H                                                               
         XC    BUFCLT,BUFCLT     ALSO CLEAR BUFNMED                             
         CLI   MULTOFF,C'Y'      SEE IF DOING OFFICE LIST REQ                   
         BNE   MOSE55                                                           
         MVI   BUFTYP,X'05'                                                     
         MVC   BUFCLT,CLT                                                       
         GOTO1 (RF)                                                             
         MVC   BUFCLT,=3X'FF'                                                   
         GOTO1 (RF)                                                             
*                                                                               
MOSE55   MVI   BUFTYP,X'06'                                                     
         MVC   BUFCLT,CLT                                                       
         CLI   MULTOFF,C'Y'        SEE IF DOING OFFICE LIST REQ                 
         BNE   MOSE57                                                           
*                                                                               
*                                                                               
         CLI   QOPT7,C'C'        SEE IF OVERRIDING MYA8PROF+8 TO CLT            
         BE    MOSE57                                                           
*                                                                               
         CLI   QOPT7,C'N'        SEE IF OVERRIDING MYA8PROF+8 TO OFF            
         BE    MOSE55C                                                          
*                                                                               
         CLC   QCLT(2),=C'$*'    ALL OFFICE REQ?                                
         BE    MOSE55C           DON'T RECAP BY CLIENT                          
*                                                                               
         CLI   MYA8PROF+8,C'C'      SEE IF REPORTING BY CLT                     
         BE    MOSE57                                                           
*                                                                               
MOSE55C  XC    BUFCLT,BUFCLT                                                    
         MVC   BUFCLT(L'SAVCOFF),SAVCOFF     OFFICE TOTALS                      
MOSE57   GOTO1 (RF)                                                             
         MVC   BUFCLT,=3X'FF'                                                   
         GOTO1 (RF)                                                             
         XC    BUFCLT,BUFCLT                                                    
         L     R2,APDTAB                                                        
         MVC   0(5,R2),=5X'FF'                                                  
         ST    R2,ANXTPD                                                        
         L     R2,AINVTAB                                                       
         MVC   0(5,R2),=5X'FF'                                                  
         ST    R2,ANXTBL                                                        
*                                  ADD TO STATION TOTALS                        
MOSE60   LA    R2,STATOTS                                                       
         LA    R3,MOSTOTS                                                       
         LA    R4,6                                                             
MOSE62   AP    0(8,R2),0(8,R3)                                                  
         LA    R2,8(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R4,MOSE62                                                        
         LA    R2,MOSTOTS                                                       
         LA    R4,8                                                             
MOSE65   ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R4,MOSE65                                                        
MOSEX    MVI   MOSACT,0                                                         
         MVI   MOSCACT,0                                                        
         MVI   MTHPSW,0                                                         
         MVI   MLINCNT,0           ZERO MOS LINE COUNT                          
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         SPACE 2                                                                
TBEXIT   XIT1                                                                   
*                                                                               
SORTOUT  NTR1                                                                   
         MVC   P+1(9),=C'SORT OUT='                                             
*                                                                               
SORT20   GOTO1 HEXOUT,DMCB,WORK,P+10,56,0                                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
SORTX    XIT1                                                                   
*                                                                               
         EJECT                                                                  
MTH13    DS    0H                  PRINTS BILLING PERIODS NOT MTHS              
         ZIC   R0,1(R2)            R2 POINTS TO BINARY YM                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R3),DUB         R3 POINTS TO PRINT LOCATION                  
         JIF   QOPT5,EQ,C'S',AND,MYA8PROF+14,EQ,C'M',MTH13D,JUMP=N              
         JIF   QOPT5,EQ,C'S',AND,MYA8PROF+14,EQ,C'Y',MTH13E,JUMP=N              
         JIF   QOPT5,EQ,C'S',AND,MYA8PROF+14,EQ,C'D',MTH13H,JUMP=N              
*                                                                               
         MVI   2(R3),C'/'                                                       
         ZIC   R0,0(R2)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(2,R3),DUB                                                      
         BR    RE                                                               
*                                                                               
MTH13D   ZIC   R0,0(R2)          DOWNLOAD MOS FORMAT MMYY                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(2,R3),DUB                                                      
         BR    RE                                                               
*                                                                               
MTH13E   ZIC   R0,0(R2)          DOWNLOAD MOS FORMAT YYMM                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R3),DUB                                                      
         ZIC   R0,1(R2)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(2,R3),DUB                                                      
         BR    RE                                                               
*                                                                               
MTH13H   DS    0H               DOWNLOAD MOS FORMAT MMDDYY                      
*                               DAY SET TO LAST OF BROADCAST MONTH              
         ST    RE,SAVERE        SAVE RETURN REGISTER                            
         CLI   NETOPT,C'N'      NETPAK?                                         
         BE    MTH13N           USE CALENDAR MONTH LOGIC                        
MTH13H5  DS    0H                                                               
         MVC   WORK(2),0(R2)    MOVE BINARY YM                                  
         MVI   WORK+2,1         DEFAULT DAY TO 01 SINCE IT'S ALWAYS             
*                               IN THE BROADCAST MONTH                          
         CLI   1(R2),13         SEE IF MONTH 13                                 
         BNE   MTH13H8                                                          
         MVI   WORK+1,12        SET TO 12 - DECEMBER                            
         MVI   WORK+2,31        SET DAY TO 31 FOR MONTH 13                      
*                                                                               
MTH13H8  GOTO1 DATCON,DMCB,(3,WORK),(X'20',WORK+6)                              
         CLI   1(R2),13         SEE IF MONTH 13                                 
         BE    MTH13H9          LEAVE AS THE 13TH - REPORTS AS 1231             
         GOTO1 DATCON,DMCB,(3,WORK),(0,WORK+6)                                  
*                                                                               
*        CALL GETBROAD TO GET BROADCAST MONTH'S START AND END                   
*                                                                               
         GOTO1 GETBROAD,DMCB,(1,WORK+6),WORK+12,GETDAY,ADDAY                    
*                                                                               
*        END DATE RETURNED IN WORK+18(6) YYMMDD                                 
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+18),(X'20',WORK+6)                           
*                                                                               
MTH13H9  DS    0H                                                               
         MVC   WORK(2),WORK+6+2    MM FIRST                                     
         MVC   WORK+2(2),WORK+6+4  DD SECOND                                    
         MVC   WORK+4(2),WORK+6    YY LAST                                      
*                                                                               
         MVC   0(6,R3),WORK        MMDDYY                                       
         MVC   6(2,R3),SPACES                                                   
         L     RE,SAVERE           RESTORE RETURN REGISTER                      
         BR    RE                                                               
*                                                                               
*        NETPAK OR CALENDAR MONTH ROUTINE                                       
*                                                                               
MTH13N   DS    0H                                                               
*                                                                               
*                               BACK TO BROADCAST MTH FOR THESE                 
*                               SUBMEDIA                                        
         CLI   MYKSTA+1,C' '    SEE IF SPECIAL STATIONS                         
         BNE   MTH13N2                                                          
         CLI   MYKSTA,C'S'      NETPAK - SYNDICATION?                           
         BE    MTH13H5                                                          
         CLI   MYKSTA,C'C'      NETPAK - CABLE?                                 
         BE    MTH13H5                                                          
         CLI   MYKSTA,C'V'      NETPAK - VOD                                    
         BE    MTH13H5                                                          
         CLI   MYKSTA,C'O'      NETPAK - OTHER?                                 
         BE    MTH13H5          DO BROADCAST MONTH FOR THESE SUBMEDIA           
*                                                                               
MTH13N2  MVI   2(R3),C'/'         MM/DD/YY                                      
         MVC   3(2,R3),=C'31'     SET TO 31 TO SEE IF DATVAL LIKES IT           
         MVI   5(R3),C'/'                                                       
         ZIC   R0,0(R2)           BINARY YEAR                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  6(2,R3),DUB                                                      
         CLC   0(2,R3),=C'13'     MONTH 13?                                     
         BNE   *+10               LEAVE DAY AT 31                               
         MVC   0(2,R3),=C'12'     CHANGE TO DEC (12)                            
*                                                                               
         GOTO1 VDATVAL,DMCB,(0,0(R3)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BNZ   MTH13NX                                                          
         MVC   3(2,R3),=C'30'     NOW TRY 30                                    
         GOTO1 VDATVAL,DMCB,(0,0(R3)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BNZ   MTH13NX                                                          
         MVC   3(2,R3),=C'28'     NOW TRY 28                                    
         GOTO1 VDATVAL,DMCB,(0,0(R3)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BNZ   MTH13NX                                                          
         MVC   3(2,R3),=C'29'     NOW TRY 29 - FEB IN LEAP YEAR                 
         GOTO1 VDATVAL,DMCB,(0,0(R3)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BNZ   MTH13NX                                                          
         DC    H'0'        BAD DATE                                             
*                                                                               
MTH13NX  DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK+6)                              
         MVC   WORK(2),WORK+6+2    MM FIRST                                     
         MVC   WORK+2(2),WORK+6+4  DD SECOND                                    
         MVC   WORK+4(2),WORK+6    YY LAST                                      
*                                                                               
         MVC   0(6,R3),WORK        MMDDYY                                       
         MVC   6(2,R3),SPACES                                                   
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
         EJECT                                                                  
PRNTSTA  DS    0H                                                               
         MVC   P(8),MYKSTA                                                      
         CLI   NETOPT,C'N'       SEE IF NETPAK                                  
         BNE   PRNTS2                                                           
         CLI   MYA8PROF+9,C'Y'  SEE IF BREAKING OUT NETPAK MEDIA                
         BE    PRNTS3                                                           
*                                                                               
PRNTS2   DS    0H                                                               
         CLI   MYKSTA+4,C'/'      SEE IF CABLE                                  
         BE    PRNTS3                                                           
*                                                                               
         CLI   P+4,C' '                                                         
         BNH   *+14                                                             
         MVC   P+5(1),P+4                                                       
         MVI   P+4,C'-'                                                         
PRNTS3   CLC   MYKSTA(4),=4X'FF'       DUMMY STATIONS                           
         BNE   PRNTS5                                                           
         MVC   P(6),=C'UNREV '         UNREVERSED ORIG BILL                     
         CLI   MYKSTA+4,X'FF'      UNREV ORIG BILL                              
         BE    PRNTSX                                                           
         MVC   P(7),=C'CUR REV'                                                 
         B     PRNTSX                                                           
*                                                                               
PRNTS5   CLC   MYKSTA(5),=C'ZZZZX'    MANUAL BILLING                            
         BE    PRNTS6                                                           
*                                                                               
         CLC   MYKSTA(5),=C'YCKDX'    ***FROM NEW MSPACK/UNPACK***              
         BE    PRNTS6                                                           
*                                                                               
         CLC   MYKSTA(7),=C'UNKNOWN'    COULD OCCUR IF NETWORK WAS              
         BNE   PRNTSX                   DELETED?                                
         MVC   P(7),=C'UNKNOWN'                                                 
         B     PRNTSX                                                           
*                                                                               
PRNTS6   MVC   P(6),=C'MANUAL'                                                  
*                                                                               
PRNTS10  CLI   QOPT5,C'S'          STATION DOWNLOAD?                            
         BNE   PRNTSX                                                           
         CLI   NETOPT,C'N'         ALSO MUST BE NETPAK                          
         BNE   PRNTSX                                                           
         CLI   MYA8PROF+9,C'Y'     REPORTING BY SUBMEDIA?                       
         BNE   PRNTSX                                                           
         MVC   P(2),=C'N '         MUST REPORT UNDER SUBMEDIA N                 
         MVC   P+2(6),=C'MANUAL'                                                
PRNTSX   BR    RE                                                               
*                                                                               
PUTBUFFC NTR1                                                                   
         MVI   BUFTYP,X'01'                                                     
         MVC   BUFTYP+1(L'TBKEY),TBREC                                          
         MVC   BUFPBILN(32),TBBILLN                                             
         ZAP   BUFCBILG,=P'0'                                                   
         ZAP   BUFCPAYG,=P'0'                                                   
         ZAP   BUFPBILG,=P'0'                                                   
         ZAP   BUFPPAYG,=P'0'                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         DROP  R8                                                               
         EJECT                                                                  
PRDEND   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   QOPT4,C'B'          SUMMARY REPORT                               
         BE    PRDEX               JUST EXIT                                    
         CLI   QOPT4,C'D'          SUMMARY REPORT - DOWNLOAD                    
         BE    PRDEX               JUST EXIT                                    
         CLI   QOPT5,C'S'          STATION DOWNLOAD                             
         BE    PRDEX               JUST EXIT                                    
*                                                                               
         CLI   PRDACT,C'Y'                                                      
         BNE   PRDEX                                                            
*                                                                               
PRDE10   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,5                                                       
         TM    ESTTBSW,X'04'                                                    
         BZ    *+8                                                              
         MVI   RCSUBPRG,6                                                       
*                                                                               
         CLI   QOPT2,C'Y'          SEE IF DOING ALL PRDS TOGETHER               
         BNE   PRDE20                                                           
         MVI   RCSUBPRG,7          CALL IT CLT TOTALS                           
         CLI   QOPT1,C' '                                                       
         BE    *+8                                                              
         MVI   RCSUBPRG,8              CLT TOTAL WITH EST BREAKOUT              
*                                                                               
PRDE20   DS    0H                                                               
         XC    BUFREC,BUFREC                                                    
         MVI   TOTTYP,X'02'        PRD TOTALS                                   
         GOTO1 VBTOTS,DMCB,(RC)                                                 
PRDE50   DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'02',BUFFBUFF),(X'80',1)                
         MVI   PRDACT,0                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,3                                                       
         TM    ESTTBSW,2           SEE IF SHOWING EST WITHIN MOS                
         BZ    *+8                                                              
         MVI   RCSUBPRG,4                                                       
         B     PRDEX                                                            
PRDEX    XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
CLTEND   NTR1  BASE=*,LABEL=*                                                   
         CLI   CLTACT,C'Y'                                                      
         BNE   CLTEX                                                            
*                                                                               
         CLI   QOPT4,C'B'    SEE IF DOING PRODUCT SUMMARY                       
         BNE   CLTE20                                                           
*                                                                               
*           PRINT TOTALS FOR ALL PRODUCTS NOW                                   
*                                                                               
         DS    0H                                                               
         MVC   P+4(7),=C'TOTAL**'                                               
         MVI   SPACING,2                                                        
*                                                                               
         EDIT  (P8,PRDBALI),(13,P+21),2,COMMAS=YES,FLOAT=-                      
         EDIT  (P8,PRDBILLN),(13,P+36),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,PRDPAYN),(13,P+59),2,COMMAS=YES,FLOAT=-                      
*                                                                               
         EDIT  (P8,PRDBALO),(13,P+85),2,COMMAS=YES,FLOAT=-                      
*                                                                               
         EDIT  (P8,PRDBILLG),(13,P+101),2,COMMAS=YES,FLOAT=-                    
         EDIT  (P8,PRDPAYG),(13,P+117),2,COMMAS=YES,FLOAT=-                     
         MVC   P+34(2),=2C'*'                                                   
         MVC   P+49(2),=2C'*'                                                   
         MVC   P+72(2),=2C'*'                                                   
         MVC   P+98(2),=2C'*'                                                   
         MVC   P+114(2),=2C'*'                                                  
         MVC   P+130(2),=2C'*'                                                  
*                                                                               
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
CLTE20   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLI   QOPT4,C'B'          SUMMARY REPORT                               
         BE    CLTE25                                                           
         CLI   QOPT4,C'D'          SUMMARY REPORT                               
         BE    CLTE25              DOWNLOADED                                   
         CLI   QOPT5,C'S'          STATION DOWNLOAD                             
         BE    CLTE25                                                           
*                                                                               
         CLI   QOPT2,C'Y'                                                       
         BE    CLTE50              ALL PRDS TOGETHER - SKIP CLT TOTALS          
*                                                                               
         CLI   QPGR,C' '           PRODUCT GROUP REQ?                           
         BH    CLTE25              DO TOTALS                                    
*                                                                               
         CLC   QPRD,=C'ALL'        ONLY ONE PRD - SKIP CLT TOTALS               
         BNE   CLTE50                                                           
*                                                                               
CLTE25   DS    0H                                                               
         MVI   RCSUBPRG,7                                                       
         CLI   QOPT1,C' '                                                       
         BE    *+8                                                              
         MVI   RCSUBPRG,8          WITH EST                                     
         XC    BUFREC,BUFREC                                                    
         MVI   TOTTYP,X'04'                                                     
         GOTO1 VBTOTS,DMCB,(RC)                                                 
CLTE50   DS    0H                                                               
         MVI   CLTACT,0                                                         
CLTEX    XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
         EJECT                                                                  
EST2END  NTR1  BASE=*,LABEL=*                                                   
         TM    ESTTBSW,2           SEE IF SORTING ON EST                        
         BZ    EST2E60                                                          
*                                                                               
         JIF   QOPT5,EQ,C'S',AND,MYA8PROF+10,EQ,C'M',EST2E,JUMP=N               
         JIF   QOPT5,EQ,C'S',AND,MYA8PROF+10,EQ,C'N',EST2EA,JUMP=N              
*                                                                               
         CLI   ESTCACT,C'Y'        CHK FOR CURRENT ACTIVITY                     
         BNE   EST2E50                                                          
*                                                                               
         CLI   QOPT5,C'S'          PUBLICATION DOWNLOAD?                        
         BNE   EST2E0                                                           
*                                                                               
         CLI   MYA8PROF+10,C'M'    MOS ONLY?                                    
         BE    EST2E0                                                           
*                                                                               
         CLI   MYA8PROF+10,C'Y'    MOS,INV,PAY DATE DETAILS?                    
         BE    EST2E0                                                           
         B     EST2E50             SKIP TO TOTALS                               
*                                                                               
*        MTH OF SERVICE DOWNLOAD - CHECK FOR BALANCE FORWARD                    
*        OR CURRENT ACTIVITY                                                    
*                                                                               
EST2E    LA    R4,ESTTOTS                                                       
         ZAP   DOUBLE,0(8,R4)      PREV BILLED                                  
         SP    DOUBLE,8(8,R4)      PREV CLEARED                                 
         CP    DOUBLE,=P'0'       ANY BALANCE FORWARD?                          
         BNE   EST2E0             YES- CONTINUE                                 
EST2EA   CLI   ESTCACT,C'Y'       CURRENT ACTIVITY?                             
         BE    EST2E0                                                           
         B     EST2E50            SKIP TO TOTALS                                
*                                                                               
*                                                                               
EST2E0   DS    0H                                                               
*                                                                               
         L     R5,AINVTAB                                                       
         L     R6,APDTAB                                                        
EST2E2   DS    0H                                                               
         ST    R5,INVADR           SAVE FOR DOWNLOAD                            
         ST    R6,PAYADR           SAVE FOR DOWNLOAD                            
         CLI   STAPSW,C'Y'         SEE IF STATION PRINTED ALREADY               
         BNE   EST2E3                                                           
         ZIC   R3,LINE             REPEAT IF NEW PAGE                           
         LA    R3,1(R3)                                                         
         STC   R3,X                                                             
         CLC   X(1),MAXLINES                                                    
         BNH   EST2E4                                                           
         MVI   MTHPSW,0            SO MTH WILL PRINT ALSO                       
EST2E3   DS    0H                                                               
         BRAS  RE,PRNTSTA                                                       
EST2E3C  MVI   STAPSW,C'Y'                                                      
EST2E4   CLI   MTHPSW,C'Y'         SEE IF MTH OF SERV PRINTED ALREADY           
         BE    EST2E6                                                           
         CLI   MYKMOS,0            NO MTH OF SERVICE BREAKOUT                   
         BE    EST2E6                                                           
*                                                                               
         JIF   MYA8PROF+14,EQ,C'Y',AND,QOPT5,EQ,C'S',EST2E4B,JUMP=N             
         JIF   MYA8PROF+14,EQ,C'M',AND,QOPT5,EQ,C'S',EST2E4B,JUMP=N             
         JIF   MYA8PROF+14,EQ,C'D',AND,QOPT5,EQ,C'S',EST2E4B,JUMP=N             
*                                                                               
         CLI   MYKMOS+1,12                                                      
         BNH   EST2E5              BILLING PERIODS                              
EST2E4B  LA    R2,MYKMOS                                                        
         LA    R3,P+9                                                           
         BRAS  RE,MTH13                                                         
         B     EST2E5B                                                          
EST2E5   GOTO1 DATCON,DMCB,(3,MYKMOS),(9,P+9)                                   
EST2E5B  MVI   MTHPSW,C'Y'                                                      
*                                                                               
EST2E6   DS    0H                                                               
         CLI   MYKEST2,0                                                        
         BE    EST2E8              NO EST                                       
         ZIC   R0,MYKEST2                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(3),DUB                                                      
         OC    MYKLINE,MYKLINE                                                  
         BZ    EST2E8                                                           
         MVI   P+20,C'-'                                                        
         EDIT  (B2,MYKLINE),(3,P+21),0,ALIGN=LEFT                               
*                                                                               
EST2E8   DS    0H                                                               
EST2E10  CLC   0(5,R5),=5X'FF'     END OF INVOICE TABLE                         
         BE    EST2E20                                                          
         EDIT  (P8,5(R5)),(13,P+36),2,COMMAS=YES,FLOAT=-                        
         CLC   0(3,R5),=X'0FFFFF'                                               
         BE    EST2E15                                                          
***INVMTH                                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(2,3(R5)),(0,DUB)                                    
         GOTO1 AFMTINO,DMCB,DUB,(2,1(R5)),(MED,PROFB1),PROFB1X                  
*                                                                               
         L     RE,DMCB+4                                                        
         MVC   P+51(7),0(RE)                                                    
EST2E15  EDIT  (P8,13(R5)),(13,P+101),2,COMMAS=YES,FLOAT=-                      
         LA    R5,21(R5)                                                        
*                                                                               
EST2E20  DS    0H                                                               
         CLC   0(5,R6),=5X'FF'          END OF PAYMENT TABLE                    
         BE    EST2E30                                                          
         EDIT  (P8,5(R6)),(13,P+59),2,COMMAS=YES,FLOAT=-                        
         GOTO1 DATCON,DMCB,(2,0(R6)),(5,P+76)                                   
         EDIT  (P8,13(R6)),(13,P+117),2,COMMAS=YES,FLOAT=-                      
         CLI   21(R6),0     CR/CK IND?                                          
         BE    EST2E25                                                          
         TM    21(R6),X'10'    CK?                                              
         BNO   EST2E22                                                          
         MVC   P+73(2),=C'CK'                                                   
         B     EST2E25                                                          
*                                                                               
EST2E22  TM    21(R6),X'20'    CR?                                              
         BNO   EST2E25                                                          
         MVC   P+73(2),=C'CR'                                                   
*                                                                               
EST2E25  OC    22(3,R6),22(R6)    REP?                                          
         BZ    EST2E28                                                          
         MVC   P+85(3),22(R6)     MAY HAVE A REP                                
*                                                                               
EST2E28  LA    R6,25(R6)                                                        
EST2E30  DS    0H                                                               
*                                                                               
         JIF   QOPT5,EQ,C'S',AND,MYA8PROF+10,EQ,C'M',EST2E30C,JUMP=N            
*        MUST STILL PROCESS IF DOING MTH OF SERV DOWNLOAD                       
         JIF   QOPT5,EQ,C'S',AND,MYA8PROF+10,EQ,C'N',EST2E30C,JUMP=N            
*        OR MYA8PROF+10 IS C'N'                                                 
         CLC   P+36(45),SPACES                                                  
         BE    EST2E40             NOTHING TO PRINT                             
*                                                                               
         CLI   QOPT5,C'S'          SEE IF DOING STA DOWNLOAD                    
         BNE   EST2E21                                                          
*                                                                               
         CLI   MYA8PROF+10,C'M'   MOS ONLY?                                     
         BE    EST2E30C                                                         
*                                                                               
         CLI   MYA8PROF+10,C'Y' SEE IF INCLUDING MOS,INVS,AND DATES             
         BNE   EST2E50                                                          
*                               BUILD SPECIAL PRINT LINE FOR DOWNLOAD           
*                               NOTE - P+17 WILL STILL HAVE EST/LNE             
*                                                                               
EST2E30C MVC   P+90(8),P+9      SAVE MOS                                        
         MVI   P+98,C' '        JUST IN CASE SOMETHING THERE                    
         BRAS  RE,PRNTSTA       PUTS STATION IN P                               
         MVC   P+30(8),P        MOVE TO P+30                                    
         MVI   P+38,C' '        JUST IN CASE SOMETHING THERE                    
         MVC   P(14),SPACES     CLEAR FOR BELOW                                 
         MVI   P,C'S'        SYSTEM                                             
         CLI   NETOPT,C'N'                                                      
         BNE   *+8                                                              
         MVI   P,C'N'                                                           
         MVC   P+2(1),QMED                                                      
*                                                                               
         CLC   P+30(8),=CL8'ALL'    STATIONS COMBINED?                          
         BE    EST2E30D                                                         
         JIF   NETOPT,NE,C'N',OR,MYA8PROF+9,NE,C'Y',EST2E30D,JUMP=N             
         MVC   P+2(1),P+30        SUBMEDIA                                      
         CLI   P+30,C' '         MISSING?                                       
         BNE   *+8                                                              
         MVI   P+2,C'N'          DEFAULT TO N                                   
         CLC   P+32(3),=C'ALL'   NETWORK SUBMEDIA +STATION COMBINED?            
         BNE   EST2E30D                                                         
         MVC   P+30(8),=CL8'ALL'                                                
         B     EST2E30E                                                         
*                                                                               
EST2E30D CLC   P+32(6),=C'MANUAL' NETWORK SUBMEDIA +MANUAL?                     
         BNE   EST2E30E                                                         
         MVC   P+30(8),=CL8'MANUAL'                                             
*                                                                               
EST2E30E DS    0H                                                               
         MVC   P+4(L'SAVCOFF),SAVCOFF                                           
*                                                                               
         MVC   P+120(L'SAVAOFF),SPACES                                          
         CLI   PROFA8A+1,C'Y'          DOWNLOAD ACC OFFICE                      
         BNE   *+10                                                             
         MVC   P+120(L'SAVAOFF),SAVAOFF                                         
*                                                                               
         MVC   P+6(3),CLT             CLIENT                                    
         MVC   P+10(3),MYKPRD         PRODUCT                                   
         CLI   QOPT2,C'Y'             SEE IF COMBINING PRDS                     
         BNE   *+10                                                             
         MVC   P+10(3),=C'ALL'                                                  
         GOTO1 VDOWNLD,DMCB,(C'E',(RC))   DOWNLOAD EST MOS LINE                 
         MVC   P,SPACES                                                         
         MVI   MTHPSW,0            SO MOS WILL PRINT AGAIN                      
*                                                                               
         CLI   MYA8PROF+10,C'Y'    INV NO. AND CLEAR DATE?                      
         BNE   EST2E40                                                          
*                                                                               
         B     EST2E2              JUST RETURN FOR NEXT                         
*                                                                               
EST2E21  DS    0H                                                               
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         ZIC   R1,MLINCNT          COUNT MOS PRINT LINES                        
         LA    R1,1(R1)                                                         
         STC   R1,MLINCNT                                                       
         B     EST2E2                                                           
*                                                                               
EST2E40  DS    0H                                                               
         MVC   P+17(7),SPACES      CLEAR EST AND LINE                           
         L     R2,APDTAB                                                        
         MVC   0(5,R2),=5X'FF'                                                  
         ST    R2,ANXTPD                                                        
         L     R2,AINVTAB                                                       
         MVC   0(5,R2),=5X'FF'                                                  
         ST    R2,ANXTBL                                                        
EST2E50  DS    0H                                                               
         CLI   ESTACT,C'Y'                                                      
         BNE   EST2E60                                                          
         TM    ESTTBSW,X'04'                                                    
         BZ    EST2E60                                                          
*                                                                               
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYP,X'02'                                                     
*                                                                               
         CLI   NETOPT,C'N'           SEE IF NETPAK                              
         BNE   EST2E53                                                          
         CLI   MYA8PROF+9,C'Y'       NETPAK MEDIA BREAKOUT?                     
         BNE   EST2E53                                                          
         MVC   BUFNMED(1),MYKSTA+7   NETPAK SUB-MEDIA (MANUALS)                 
         CLI   MYKSTA+1,C' '       SEE IF SPECIAL STATIONS                      
         BNE   EST2E53                                                          
         MVC   BUFNMED,MYKSTA    NETPAK SUB-MEDIA                               
*                                                                               
EST2E53  DS    0H                                                               
         MVC   BUFMOS,MYKMOS                                                    
         MVC   BUFEST,MYKEST2                                                   
         MVC   BUFPBILN(64),EPBILLN                                             
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   BUFTYP,X'04'        CLT TOTALS                                   
         GOTO1 (RF)                                                             
*                                                                               
         CLI   NETOPT,C'N'           SEE IF NETPAK                              
         BNE   EST2E54                                                          
         CLI   MYA8PROF+9,C'Y'       NETPAK MEDIA BREAKOUT?                     
         BNE   EST2E54                                                          
         MVI   BUFTYP,X'02'        PRODUCT                                      
         MVI   BUFNMED,X'FF'       ALL MEDIA COMBINED                           
         GOTO1 (RF)                                                             
         MVI   BUFTYP,X'04'        CLT TOTALS                                   
         GOTO1 (RF)                                                             
*                                                                               
EST2E54  DS    0H                                                               
         LA    R2,EPBILLN                                                       
         LA    R4,8                FOR BCT                                      
EST2E55  ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R4,EST2E55                                                       
EST2E60  DS    0H                                                               
         MVI   ESTACT,0                                                         
         MVI   ESTCACT,0                                                        
EST2EX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
STAEND   NTR1  BASE=*,LABEL+*      STATION TOTALS                               
         CLI   STAACT,C'Y'                                                      
         BNE   STAEX                                                            
         CLI   QOPT3,C'Y'          SEE IF SHOWING ALL STATIONS                  
         BE    STAE5               YES                                          
         CLI   STAPSW,C'Y'         SEE IF I PRINTED A MOS FOR THIS STA          
         BE    STAE5               YES - MUST DO TOTALS                         
         CP    SCBILLN,=P'0'       CHK FOR CURRENT ACTIVITY                     
         BNE   STAE5                                                            
         CP    SCPAYN,=P'0'                                                     
         BNE   STAE5                                                            
         CP    SPBILLN,SPPAYN      CHK FOR BAL FORWARD                          
         BE    STAE10              NO                                           
*                                                                               
STAE5    DS    0H                                                               
*                                                                               
         CLI   QOPT5,C'S'          STATION DOWNLOAD                             
         BE    STAE9                                                            
*                                                                               
         CLI   STAPSW,C'Y'         SEE IF I'VE PRINTED STATION                  
         BNE   STAE7               NO                                           
         ZIC   R3,LINE             REPEAT IF NEW PAGE                           
         LA    R3,2(R3)                                                         
         STC   R3,X                                                             
         CLC   X(1),MAXLINES                                                    
         BNH   STAE8                                                            
*                                                                               
STAE7    DS    0H                                                               
         CLI   QOPT4,C'B'         SUPPRESSIN STA + MOS                          
         BE    STAE7C                                                           
         CLI   QOPT4,C'D'         SPECIAL DOWNLOAD OUTPUT                       
         BNE   STAE7X                                                           
STAE7A   DS    0H                                                               
         MVI   P,C'S'        SYSTEM                                             
         CLI   NETOPT,C'N'                                                      
         BNE   *+8                                                              
         MVI   P,C'N'                                                           
         MVC   P+2(1),QMED                                                      
         CLC   P+30(8),=CL8'ALL'    STATIONS COMBINED?                          
         BE    STAE7A3                                                          
         JIF   NETOPT,NE,C'N',OR,MYA8PROF+9,NE,C'Y',STAE7A3,JUMP=N              
         MVC   P+2(1),P+30        SUBMEDIA                                      
         CLI   P+30,C' '         MISSING?                                       
         BNE   *+8                                                              
         MVI   P+2,C'N'          DEFAULT TO N                                   
         CLC   P+32(3),=C'ALL'   NETWORK SUBMEDIA +STATION COMBINED?            
         BNE   STAE7A2                                                          
         MVC   P+30(8),=CL8'ALL'                                                
         B     STAE7A3                                                          
*                                                                               
STAE7A2  CLC   P+32(6),=C'MANUAL'  NETWORK SUBMEDIA +MANUAL?                    
         BNE   STAE7A3                                                          
         MVC   P+30(8),=CL8'MANUAL'                                             
*                                                                               
STAE7A3  DS    0H                                                               
         MVC   P+4(L'SAVCOFF),SAVCOFF                                           
*                                                                               
         MVC   P+120(L'SAVAOFF),SPACES                                          
         CLI   PROFA8A+1,C'Y'          DOWNLOAD ACC OFFICE                      
         BNE   *+10                                                             
         MVC   P+120(L'SAVAOFF),SAVAOFF                                         
*                                                                               
         CLI   QOPT5,C'S'              STATION DOWNLOAD?                        
         BE    STAE7A5                                                          
         MVC   P+6(3),MYKSTA           CLIENT                                   
         MVC   P+10(3),MYKSTA+3        PRODUCT                                  
         B     STAE8A                                                           
*                                                                               
STAE7A5  DS    0H                                                               
         MVC   P+6(3),CLT             CLIENT                                    
         MVC   P+10(3),PRD            PRODUCT                                   
         CLI   QOPT2,C'Y'             SEE IF COMBINING PRDS                     
         BNE   *+10                                                             
         MVC   P+10(3),=C'ALL'                                                  
         B     STAE8A                                                           
*                                                                               
STAE7C   DS    0H                                                               
         MVC   P(3),MYKSTA+3         PRODUCT IN PRINT LINE                      
         L     R2,APRDTAB                                                       
STAE7D   CLI   0(R2),X'FF'         END OF TABLE                                 
         BNE   STAE7E                                                           
         B     STAE7H              PRD NOT IN TABLE                             
*                                IS OK  - NET UNITS DON'T SET PRDTAB            
STAE7E   CLC   0(3,R2),MYKSTA+3                                                 
         BE    STAE7F                                                           
         LA    R2,23(R2)                                                        
         B     STAE7D                                                           
*                                                                               
STAE7F   DS    0H                                                               
         MVC   P+4(16),3(R2)     PRD NAME (FIRST 16 CHARACTERS)                 
*                                                                               
STAE7H   DS    0H                                                               
         B     STAE8A                                                           
*                                                                               
STAE7X   DS    0H                                                               
         BRAS  RE,PRNTSTA                                                       
STAE8    MVC   P+9(7),=C'TOTAL**'                                               
STAE8A   MVI   SPACING,2                                                        
         ZAP   DOUBLE,SPBILLN                                                   
         SP    DOUBLE,SPPAYN                                                    
*                                                                               
*        THESE PRD ACCUMULATORS FOR PRD SUMMARY (QOPT4 =B)                      
*                                                                               
         AP    PRDBALI,DOUBLE     PRD BALANCE IN                                
         AP    PRDBILLN,SCBILLN   PRD CURRENT BILL NET                          
         AP    PRDPAYN,SCPAYN     PRD CURRENT PAY NET                           
*                                                                               
         EDIT  (P8,DOUBLE),(13,P+21),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P8,SCBILLN),(13,P+36),2,COMMAS=YES,FLOAT=-                      
         EDIT  (P8,SCPAYN),(13,P+59),2,COMMAS=YES,FLOAT=-                       
         AP    DOUBLE,SCBILLN                                                   
         SP    DOUBLE,SCPAYN                                                    
*                                                                               
*        THESE PRD ACCUMULATORS FOR PRD SUMMARY (QOPT4=B)                       
*                                                                               
         AP    PRDBALO,DOUBLE     PRD BALNACE OUT                               
         AP    PRDBILLG,SCBILLG   PRD CURRENT BILL GROSS                        
         AP    PRDPAYG,SCPAYG     PRD CURRENT PAY GROSS                         
*                                                                               
         EDIT  (P8,DOUBLE),(13,P+85),2,COMMAS=YES,FLOAT=-                       
*                                                                               
*        NEEDED FOR QOPT4 = D  DOWNLOAD VERSION                                 
*                                                                               
         EDIT  (P8,DOUBLE),(12,DOLS),MINUS=YES,ZERO=NOBLANK                     
         CLI   MYA8PROF+11,C'Y'     2 DECIMALS?                                 
         BNE   STAE8AX                                                          
         EDIT  (P8,DOUBLE),(12,DOLS),2,MINUS=YES,ZERO=NOBLANK                   
*                                                                               
STAE8AX  EDIT  (P8,SCBILLG),(13,P+101),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,SCPAYG),(13,P+117),2,COMMAS=YES,FLOAT=-                      
         MVC   P+34(2),=2C'*'                                                   
         MVC   P+49(2),=2C'*'                                                   
         MVC   P+72(2),=2C'*'                                                   
         MVC   P+98(2),=2C'*'                                                   
         MVC   P+114(2),=2C'*'                                                  
         MVC   P+130(2),=2C'*'                                                  
         CLI   QOPT4,C'B'          SUPPRESSIN BOTH STA AND MOS                  
         BE    STAE8D                                                           
         CLI   QOPT4,C'D'          SUPPRESSIN BOTH STA AND MOS                  
         BE    STAE8D              WITH DOWNLOAD                                
         CLI   QOPT4,C'M'          NO MTH OF SERVICE BREAKOUT                   
         BNE   STAE9                                                            
STAE8C   MVI   P+15,C' '           USE ONLY ONE *                               
STAE8D   MVI   P+35,C' '                                                        
         MVI   P+50,C' '                                                        
         MVI   P+73,C' '                                                        
         MVI   P+99,C' '                                                        
         MVI   P+115,C' '                                                       
         MVI   P+131,C' '                                                       
*                                                                               
STAE9    DS    0H                                                               
         CLI   QOPT5,C'S'     STATION DOWNLOAD                                  
         BE    STAE9S                                                           
*                                                                               
         CLI   QOPT4,C'D'                                                       
         BNE   STAE9X                                                           
         GOTO1 VDOWNLD,DMCB,(RC)                                                
         MVC   P,SPACES                                                         
         B     STAE10                                                           
*                                                                               
STAE9S   DS    0H                                                               
         ZAP   DOUBLE,SPBILLN     BALANCE FORMARD                               
         SP    DOUBLE,SPPAYN                                                    
         CLI   MYA8PROF+11,C'Y'    $ WITH DECIMALS?                             
         BE    STAE9S5                                                          
         CLI   MYA8PROF+12,C'Y'    MINUS BEFORE?                                
         BE    STAE9S3                                                          
         EDIT  (P8,DOUBLE),(13,SBALF),MINUS=YES,ZERO=NOBLANK                    
         EDIT  (P8,SCBILLN),(13,SCBIL),MINUS=YES,ZERO=NOBLANK                   
         EDIT  (P8,SCPAYN),(13,SCPAY),MINUS=YES,ZERO=NOBLANK                    
         B     STAE9S10                                                         
*                                                                               
STAE9S3  EDIT  (P8,DOUBLE),(13,SBALF),ZERO=NOBLANK,FLOAT=-                      
         EDIT  (P8,SCBILLN),(13,SCBIL),ZERO=NOBLANK,FLOAT=-                     
         EDIT  (P8,SCPAYN),(13,SCPAY),ZERO=NOBLANK,FLOAT=-                      
         B     STAE9S10                                                         
*                                                                               
STAE9S5  DS    0H                                                               
         CLI   MYA8PROF+12,C'Y'    MINUS BEFORE?                                
         BE    STAE9S7                                                          
         EDIT  (P8,DOUBLE),(13,SBALF),2,MINUS=YES,ZERO=NOBLANK                  
         EDIT  (P8,SCBILLN),(13,SCBIL),2,MINUS=YES,ZERO=NOBLANK                 
         EDIT  (P8,SCPAYN),(13,SCPAY),2,MINUS=YES,ZERO=NOBLANK                  
         B     STAE9S10                                                         
*                                                                               
STAE9S7  EDIT  (P8,DOUBLE),(13,SBALF),2,ZERO=NOBLANK,FLOAT=-                    
         EDIT  (P8,SCBILLN),(13,SCBIL),2,ZERO=NOBLANK,FLOAT=-                   
         EDIT  (P8,SCPAYN),(13,SCPAY),2,ZERO=NOBLANK,FLOAT=-                    
*                                                                               
STAE9S10 AP    DOUBLE,SCBILLN                                                   
         SP    DOUBLE,SCPAYN                                                    
         CLI   MYA8PROF+11,C'Y'    $ WITH DECIMALS?                             
         BE    STAE9S15                                                         
         CLI   MYA8PROF+12,C'Y'    MINUS BEFORE?                                
         BE    STAE9S13                                                         
         EDIT  (P8,DOUBLE),(13,SBALO),MINUS=YES,ZERO=NOBLANK                    
         B     STAE9S20                                                         
*                                                                               
STAE9S13 EDIT  (P8,DOUBLE),(13,SBALO),ZERO=NOBLANK,FLOAT=-                      
         B     STAE9S20                                                         
*                                                                               
STAE9S15 DS    0H                                                               
         CLI   MYA8PROF+12,C'Y'    MINUS BEFORE?                                
         BE    STAE9S17                                                         
         EDIT  (P8,DOUBLE),(13,SBALO),2,MINUS=YES,ZERO=NOBLANK                  
         B     STAE9S20                                                         
*                                                                               
STAE9S17 EDIT  (P8,DOUBLE),(13,SBALO),2,ZERO=NOBLANK,FLOAT=-                    
*                                                                               
STAE9S20 DS    0H                                                               
         BRAS  RE,PRNTSTA       PUTS STATION IN P                               
         MVC   P+30(8),P        MOVE TO P+20                                    
         MVI   P+38,C' '        JUST IN CASE SOMETHING WAS THERE                
         MVC   P(14),SPACES                                                     
         MVI   P,C'S'        SYSTEM                                             
         CLI   NETOPT,C'N'                                                      
         BNE   *+8                                                              
         MVI   P,C'N'                                                           
         MVC   P+2(1),QMED                                                      
         CLC   P+30(8),=CL8'ALL '     STATIONS COMBINED?                        
         BE    STAE9S22                                                         
         JIF   NETOPT,NE,C'N',OR,MYA8PROF+9,NE,C'Y',STAE9S22,JUMP=N             
         MVC   P+2(1),P+30        SUBMEDIA                                      
         CLI   P+30,C' '         MISSING?                                       
         BNE   *+8                                                              
         MVI   P+2,C'N'          DEFAULT TO N                                   
         CLC   P+32(3),=C'ALL'   NETWORK SUBMEDIA +STATION COMBINED?            
         BNE   STAE9S21                                                         
         MVC   P+30(8),=CL8'ALL'                                                
         B     STAE9S22                                                         
*                                                                               
STAE9S21 CLC   P+32(6),=C'MANUAL' NETWORK SUBMEDIA + MANUAL?                    
         BNE   STAE9S22                                                         
         MVC   P+30(8),=CL8'MANUAL'                                             
*                                                                               
STAE9S22 DS    0H                                                               
         MVC   P+4(L'SAVCOFF),SAVCOFF                                           
*                                                                               
         MVC   P+120(L'SAVAOFF),SPACES                                          
         CLI   PROFA8A+1,C'Y'          DOWNLOAD ACC OFFICE                      
         BNE   *+10                                                             
         MVC   P+120(L'SAVAOFF),SAVAOFF                                         
*                                                                               
         MVC   P+6(3),CLT             CLIENT                                    
         MVC   P+10(3),MYKPRD         PRODUCT                                   
         CLI   QOPT2,C'Y'             SEE IF COMBINING PRDS                     
         BNE   *+10                                                             
         MVC   P+10(3),=C'ALL'                                                  
         GOTO1 VDOWNLD,DMCB,(RC)                                                
         MVC   P,SPACES                                                         
         B     STAE10                                                           
*                                                                               
STAE9X   GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
STAE10   LA    R2,STATOTS                                                       
         LA    R3,6                                                             
STAE15   ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,STAE15                                                        
         MVI   STAACT,0                                                         
         MVI   STAPSW,0                                                         
STAEX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                           NETWORK UNIT PROCESSING                             
PROCNET  CSECT                                                                  
         NMOD1 0,PROCNET                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,RC,RA                                                    
         LA    R9,SPACEND                                                       
         USING SPA8WRKD,R9                                                      
         L     R7,ANETBLK                                                       
         USING NETBLOCK,R7                                                      
*                                                                               
*                                                                               
         LA    RE,NETBLOCK                                                      
         LH    RF,=Y(NBBLKEND-NETBLOCK)                                         
         XCEF                                                                   
*                                                                               
*                                  SET SELECT OPTIONS                           
         MVI   NBUSER+13,C'N'     ALWAYS PASS PRE-EMPTED UNITS TO A8            
         MVC   NBSELAGY(3),QAGY    AGY/MED                                      
         MVC   NBSELCLI,CLT        CLT                                          
         MVC   NBSELPRD,=C'POL'    ALWAYS DO ALL PRDS                           
         MVC   NBSELEST(2),BEST    END ST/END                                   
         CLC   =C'NO',QEST                                                      
         BNE   *+10                                                             
         MVC   NBSELEFL,QESTEND    EST FILTER                                   
*NOP     MVC   NBSELSTR(12),=C'750101991231'                                    
         MVC   NBSELSTR,=C'750101' LEAVE END DATE BLANK NETIO DEFAULTS          
*                          TO MAX END DATE OF COMPRESSED FORMAT OF              
*                                  X'FFFF'                                      
*NOP     MVI   NBSELSTR+6,X'FB'                                                 
*                          THESE DATES ARE REALLY 1975 - 2019                   
*                          X'FC'  CAUSED NETIO TO RETURN                        
*                          END BEFORE START ERROR                               
*                          GOOD LUCK TO FUTURE GENERATIONS OF                   
*                          PROGRAMMERS                                          
*                                  SET DATA OPTIONS                             
         MVI   NBDATA,C'U'         UNITS                                        
         MVI   NBSEQ,C'D'          DATE SEQ                                     
         MVC   NBTRCOPT,RCTRACE    TRACE                                        
         MVI   NBFUNCT,NBFNORM     NORMAL FUNCTION                              
         MVI   NBSELPST,C'B'       PASS LOCKED PACKAGES                         
*                                  THEY MIGHT BE BILLED                         
*                                                                               
         MVC   NBAIO,=A(VIRTLREC)  USE VIRTUAL REC AREA                         
         MVC   NBPRINT,PRINT                                                    
         MVC   NBLOADER,LOADER                                                  
         MVC   NBACOM,ACOMFACS                                                  
         OI    NBINDS2,NBBILLRD    RETURN UBILL RECS IF POSSIBLE                
         XC    WK2,WK2             SET UP DSECT FOR BILLREADER                  
         LA    R1,WK2                                                           
         ST    R1,NBABILRD                                                      
K@       USING NBLBILLD,WK2                                                     
         MVC   K@.NBLUNAIO,=A(VIRTLREC)                                         
         OI    K@.NBLFUNC,NBLSEED                                               
         DROP  K@                                                               
*        OI    NBINDS2,X'80'         RETURN ADDIT CHARGES COMPRESSED            
*                                                                               
*                                                                               
NTU10    DS    0H                                                               
         GOTO1 NETIO,DMCB,NETBLOCK                                              
         CLI   NBERROR,NBINVEST                                                 
         BE    NTUXX                                                            
         CLI   NBERROR,NBINVPRD                                                 
         BE    NTUXX                                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN                                                  
         BE    NTU12                                                            
         CLI   NBMODE,NBREQLST                                                  
         BE    NTUX                                                             
         CLI   NBMODE,NBVALCLI     SEE IF I JUST VALIDATED CLIENT               
         BNE   NTU10                                                            
         MVI   NBUSER+13,C'N'      ALWAYS PASS PRE-EMPTED UNITS                 
         B     NTU10                                                            
*                                                                               
         SPACE 3                                                                
* PROCESS UNIT                                                                  
         SPACE 2                                                                
NTU12    DS    0H                                                               
**UNA                   SKIPPING UNALLOCATED UNITS NO-OPED                      
**UNA    CLI   NBPRD,0             SEE IF UNALLOC                               
**UNA    BE    NTU10               YES- -BYPASS                                 
         L     R6,NBAIO                                                         
         USING NURECD,R6                                                        
*                                                                               
*              BUIILD SORT KEY                                                  
*                                                                               
         XC    TBREC,TBREC                                                      
         TM    ESTTBSW,2           SEE IF SHOWING EST WITHIN MOS                
         BZ    *+10                                                             
         MVC   TBKEST2,NUKEST                                                   
         TM    ESTTBSW,X'08'       SEE IF SHOWING LINE NUMBER                   
         BZ    *+10                                                             
         MVC   TBKLINE+1(1),NUKSUB +1 - NO 2 BYTE LINE NOS. IN NET              
         CLI   QOPT4,C'M'          SEE IF SHOWING MTH BREAKOUT                  
         BE    NTU12B              NO                                           
         CLI   QOPT4,C'B'          SEE IF NO STA NOR MTH                        
         BE    NTU12B              NO                                           
         CLI   QOPT4,C'D'          SEE IF NO STA NOR MTH                        
         BE    NTU12B              WITH DOWNLOAD                                
         GOTO1 DATCON,DMCB,(2,NUKDATE),(3,WORK)                                 
         MVC   TBKMOS,WORK         USE CALENDAR MTH                             
NTU12B   MVC   TBKSTA(4),NUKNET                                                 
         OC    TBKSTA,SPACES                                                    
*                                                                               
         CLI   MYA8PROF+9,C'Y'     SEE IF BREAKING OUT NETPAK MEDIA             
         BNE   NTU12BX                                                          
         MVC   TBKSTA,SPACES                                                    
         MVC   TBKSTA+2(4),NUKNET                                               
*                                                                               
         LA    R2,NUDATA                                                        
         MVI   ELCODE,X'02'        NUSDRD ELEMENT - FOR STATION TYPE            
*                                                                               
NTU12B1  DS    0H                                                               
         BAS   RE,NXTEL                                                         
         BNE   NTU12B3                                                          
*                                                                               
         USING NUSDRD,R2                                                        
*                                                                               
         MVC   TBKSTA(1),NUSTATYP                                               
         OC    TBKSTA(1),SPACES                                                 
         CLI   TBKSTA,C'N'         DISPLAY NETWORK AS SPACE                     
         BNE   *+8                                                              
         MVI   TBKSTA,C' '                                                      
*                                                                               
         DROP  R2                                                               
NTU12B3  DS    0H                                                               
         B     NTU12B7                                                          
*                                                                               
**OLD                              CHECKED NURSTAT BEFORE                       
**OLD    TM    NURSTAT,X'03'       SEE IF OTHER                                 
**OLD    BNO   NTU12B3                                                          
**OLD    MVI   TBKSTA,C'O'                                                      
**OLD    B     NTU12B7                                                          
**OLD                                                                           
**OLDB3  DS    0H                                                               
**OLD    TM    NURSTAT,X'02'       SEE IF SYNDICATION                           
**OLD    BNO   NTU12B5                                                          
**OLD    MVI   TBKSTA,C'S'                                                      
**OLD    B     NTU12B7                                                          
**OLD                                                                           
**OLDB5 DS    0H                                                                
**OLD    TM    NURSTAT,X'01'       SEE IF CABLE                                 
**OLD    BNO   NTU12BX                                                          
**OLD    MVI   TBKSTA,C'C'                                                      
**OLD    B     NTU12B7                                                          
*                                                                               
NTU12B7  DS    0H                                                               
         CLI   QOPT4,C'S'          SEE IF NO STATION BREAKOUT                   
         BNE   NTU12C                                                           
         MVC   TBKSTA+2(6),=CL6'ALL'                                            
         B     NTU12E                                                           
*                                                                               
NTU12BX  DS    0H                                                               
         CLI   QOPT4,C'S'          SEE IF NO STATION BREAKOUT                   
         BNE   NTU12C                                                           
         MVC   TBKSTA,=CL8'ALL'                                                 
         B     NTU12E                                                           
*                                                                               
NTU12C   CLI   QOPT4,C'B'         SUPPRESSIN BOTH STA AND MOS                   
         BE    NTU12C5                                                          
         CLI   QOPT4,C'D'         SUPPRESSIN BOTH STA AND MOS                   
         BNE   NTU12E             WITH DOWNLOAD                                 
NTU12C5  MVC   TBKSTA,SPACES                                                    
         MVC   TBKSTA(3),CLT                                                    
         MVC   TBKSTA+3(3),PRD                                                  
         OC    TBKSTA,SPACES                                                    
*                                                                               
NTU12E   DS    0H                                                               
         LA    R2,NUDATA                                                        
         MVI   ELCODE,X'10'        BILL ELEM                                    
*                                                                               
NTU13    DS    0H                                                               
         BAS   RE,NXTEL                                                         
         BNE   NTU14                                                            
*                                                                               
         USING NUBILD,R2                                                        
         TM    NUBILST,X'20'       SEE IF UNBILLED                              
         BO    NTU13               YES SKIP THIS ELEM                           
         OC    NUBILDAT,NUBILDAT   SEE IF BILLED                                
         BZ    NTU13               NO SKIP                                      
         CLC   NUBILDAT,BQENDP     SEE IF BILLED AFTER 'AS OF' DATE             
         BH    NTU13               YES BYPASS                                   
         CLC   NUBILDAT,CMEND      SEE IF BILLED AFTER CURRENT MTH              
         BH    NTU13               YES BYPASS                                   
         CLI   NUBILPRD,0          SEE IF EXPANDED PRODUCT                      
         BNE   NTU13B                                                           
         CLI   NUBILLEN,28                                                      
         BH    *+6                                                              
         DC    H'0'                LENGTH SHOULD BE AT LEAST 29                 
*                                  ELSE BAD ELEMENT                             
         MVC   WPRD3,NUBILGR2+4  (WILL BE NUBILPRC WHEN ELENA'S READY)          
         BAS   RE,CKPRD3           TEST NEED THIS PRODUCT                       
         BNE   NTU13                                                            
         B     NTU13BX                                                          
*                                                                               
NTU13B   DS    0H                                                               
         MVC   WPRD,NUBILPRD                                                    
         BAS   RE,CKPRD            TEST NEED THIS PRODUCT                       
         BNE   NTU13                                                            
*                                                                               
NTU13BX  DS    0H                                                               
*                                                                               
*                                                                               
         GOTO1 SPBVAL,DMCB,(C'U',NUBILEL),SPBVALD,0                             
*                                                                               
*        SET EFFECTIVE VALUES INTO ELEM                                         
*        CAN DO SINCE RECORD IS NOT WRITTEN BACK                                
*                                                                               
         MVC   NUBILGRS,SPBVEGRS                                                
         MVC   NUBILNET,SPBVENET                                                
*                                                                               
*                                                                               
         CLI   QOPT4,C'B'          SPECIAL SUMMARY REPORT                       
         BE    NTU13D                                                           
         CLI   QOPT4,C'D'          SPECIAL SUMMARY REPORT                       
         BE    NTU13D              WITH DOWNLOAD                                
         CLI   QOPT2,C'Y'          SEE IF COMBINING PRDS                        
         BE    NTU13DX                                                          
*                                                                               
         CLC   QPRD,=C'POL'                                                     
         BNE   NTU13D                                                           
         MVC   TBKPRD,=C'POL'                                                   
         B     NTU13DX                                                          
*                                                                               
NTU13D   DS    0H                                                               
*              MUST GET PRD CODE FROM CLIENT HEADER                             
*              IF NUBILPRD IS NOT ZERO                                          
*                                                                               
         CLI   NUBILPRD,0      IF ZERO USE NUBILGR2+4 (3 CHAR PRD)              
         BNE   NTU13D2         (NUBILPRC - IN FUTURE)                           
         MVC   TBKPRD,NUBILGR2+4                                                
         B     NTU13DX                                                          
*                                                                               
NTU13D2  L     R5,VCLIST       COMBINED CLIST AND CLIST2                        
NTU13D5  CLC   3(1,R5),NUBILPRD                                                 
         BE    NTU13D8      FOUND                                               
         LA    R5,4(R5)                                                         
         CLI   0(R5),0      END OF LIST                                         
         BNE   NTU13D5                                                          
         DC    H'0'               MUST BE ABLE TO FIND THE PRODUCT              
*                                                                               
NTU13D8  MVC   TBKPRD,0(R5)                                                     
*              PASS BILLING ELEM TO SORT                                        
NTU13DX  LA    R5,TBBILLN          ZAP ACCUMS                                   
         LA    R4,4                                                             
NTU13E   ZAP   0(8,R5),=P'0'                                                    
         LA    R5,8(R5)                                                         
         BCT   R4,NTU13E                                                        
         L     R0,NUBILGRS                                                      
         CVD   R0,TBBILLG                                                       
         L     R0,NUBILNET                                                      
         CVD   R0,TBBILLN                                                       
         MVC   TBBILDTE,NUBILDAT                                                
         MVC   TBKINV,=2X'FF'                                                   
         MVI   TBKINVMO,X'FF'                                                   
         OC    NUBILNUM,NUBILNUM      CONVERTED UNITS HAVE NO                   
         BZ    NTU13F                 INVOICE NUMBER                            
         GOTO1 DATCON,DMCB,(2,NUBILDAT),(0,WORK)                                
         ZIC   R0,WORK+1           LAST DIGIT OF YEAR                           
         SLL   R0,4                                                             
         PACK  DUB,WORK+2(2)       MONTH                                        
         CVB   R1,DUB                                                           
         OR    R0,R1                                                            
         STC   R0,TBKINVMO                                                      
         GOTO1 AFMTINO,DMCB,0,(C'P',NUBILNUM)                                   
         L     RE,DMCB+4                                                        
         MVC   TBKINV,0(RE)        BINARY INVOICE NUMBER RETURNED               
*                                                                               
**       PACK  DUB,NUBILNUM        INV NUMBER                                   
**       CVB   R0,DUB                                                           
**       STH   R0,HALF                                                          
**       MVC   TBKINV,HALF                                                      
*                                                                               
NTU13F   DS    0H                                                               
         CLI   QOPT4,C'B'             SUPPRESSING STA + MOS                     
         BE    NTU13F5                                                          
         CLI   QOPT4,C'D'             SPECIAL DOWNLOAD VERSION                  
         BNE   NTU13X                                                           
NTU13F5  MVC   TBKSTA+3(3),TBKPRD     MUST USE TBKPRD                           
*                                                                               
NTU13X   GOTO1 PUTBUFFN                                                         
*                                                                               
         B     NTU13                                                            
*                                                                               
NTU14    DS    0H                                                               
         L     R6,NBAIO                                                         
         USING NURECD,R6                                                        
         LA    R2,NUDATA                                                        
         MVI   ELCODE,X'12'        PAY ELEM                                     
*                                                                               
NTU16    DS    0H                                                               
         BAS   RE,NXTEL                                                         
         BNE   NTU20                                                            
*                                                                               
         USING NUPAYD,R2                                                        
         OC    NUPAYDAT,NUPAYDAT   SEE IF PAID                                  
         BZ    NTU16                                                            
         CLC   NUPAYDAT,BQENDP     SEE IF PAID AFTER 'AS OF' DATE               
         BH    NTU16               YES SKIP                                     
         CLC   NUPAYDAT,CMEND      SEE IF PAID AFTER CURRENT MTH                
         BH    NTU16               YES SKIP                                     
         MVI   TBKINVMO,0                                                       
         XC    TBKINV,TBKINV                                                    
         XC    TBBILDTE,TBBILDTE                                                
*                                                                               
         LA    R5,TBBILLN          ZAP ACCUMS                                   
         LA    R4,4                                                             
NTU16C   ZAP   0(8,R5),=P'0'                                                    
         LA    R5,8(R5)                                                         
         BCT   R4,NTU16C                                                        
*                                                                               
         MVC   WPRD3,NBPR1CL3      (ALPHA FOR NBPRD (NUPRD?)                    
         BAS   RE,CKPRD3                                                        
         BNE   NTU18               CHECK NBPR2CL3                               
*                                                                               
         MVC   WK(8),NUPAYGRS                                                   
         CLC   NBPR2CL3,SPACES     IF HAVE 2ND PRD                              
         BNH   NTU16B                                                           
         SR    RF,RF                                                            
         ICM   RF,3,NUP1SHR                                                     
         BAS   RE,SETSHR           GET SHARE FOR THIS PRD                       
*                                                                               
NTU16B   DS    0H                                                               
         XC    TBKPRD,TBKPRD                                                    
         CLI   QOPT4,C'B'          SPECIAL SUMMARY REPORT                       
         BE    NTU16B8                                                          
         CLI   QOPT4,C'D'          SPECIAL SUMMARY REPORT                       
         BE    NTU16B8             WITH DOWNLOAD                                
         CLI   QOPT2,C'Y'          SEE IF COMBINING PRDS                        
         BE    NTU17                                                            
         CLC   QPRD,=C'POL'                                                     
         BNE   NTU16B8                                                          
         MVC   TBKPRD,=C'POL'                                                   
         B     NTU17                                                            
*                                                                               
*        NTU16B2  - OLD ROUTINE TO GET PRD FROM VCLIST                          
*                                                                               
NTU16B8  MVC   TBKPRD,NBPR1CL3     PRODUCT FROM NETBLOCKN                       
NTU17    MVC   TBKPDDTE,NUPAYDAT                                                
         MVC   TBPCRCK,NUPAYCR                                                  
**NO-OP  MVC   TBPREP,NUPAYREP     REP (FUTURE)                                 
         L     R0,WK                                                            
         CVD   R0,TBPAIDG                                                       
         L     R0,WK+4                                                          
         CVD   R0,TBPAIDN                                                       
*                                                                               
         CLI   QOPT4,C'B'          SUPPRESSING STA + MOS                        
         BE    NTU17C                                                           
         CLI   QOPT4,C'D'          SPECIAL DOWNLOAD VERSION                     
         BNE   *+10                                                             
NTU17C   MVC   TBKSTA+3(3),TBKPRD   MUST RESET PRODUCT                          
*                                                                               
         GOTO1 PUTBUFFN                                                         
*                                                                               
NTU18    DS    0H                                                               
         CLC   NBPR2CL3,SPACES     DO I HAVE A SECOND PRODUCT?                  
         BNH   NTU16               IF NOT-DONE WITH PAY ELEMENT                 
         MVC   WPRD3,NBPR2CL3                                                   
         BAS   RE,CKPRD3                                                        
         BNE   NTU16               IF NOT PROCESSING THIS PRD                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,NUP1SHR        1ST PRD SHARE                                
         SH    RF,=H'10000'        COMPLEMENT                                   
         LCR   RF,RF                                                            
         MVC   WK(8),NUPAYGRS                                                   
         BAS   RE,SETSHR           GET SHARE FOR THIS PRD                       
*                                  THEN DONE WITH ELEMENT                       
         XC    TBKPRD,TBKPRD                                                    
         CLI   QOPT4,C'B'          SPECIAL SUMMARY REPORT                       
         BE    NTU18A                                                           
         CLI   QOPT4,C'D'          SPECIAL SUMMARY REPORT                       
         BE    NTU18A              WITH DOWNLOAD                                
         CLI   QOPT2,C'Y'          SEE IF COMBINING PRDS                        
         BE    NTU19                                                            
         CLC   QPRD,=C'POL'                                                     
         BNE   NTU18A                                                           
         MVC   TBKPRD,=C'POL'                                                   
         B     NTU19                                                            
*                                                                               
NTU18A   MVC   TBKPRD,NBPR2CL3                                                  
*                                                                               
NTU19    MVC   TBKPDDTE,NUPAYDAT                                                
         L     R0,WK                                                            
         CVD   R0,TBPAIDG          GROSS PAID                                   
         L     R0,WK+4                                                          
         CVD   R0,TBPAIDN          NET PAID                                     
*                                                                               
         CLI   QOPT4,C'B'             SUPPRESSING STA + MOS                     
         BE    NTU19C                                                           
         CLI   QOPT4,C'D'             SPECIAL DOWNLOAD VERSION                  
         BNE   NTU19X                                                           
NTU19C   MVC   TBKSTA+3(3),TBKPRD     MUST USE TBKPRD                           
NTU19X   DS    0H                                                               
         GOTO1 PUTBUFFN                                                         
         B     NTU16               GO DO NEXT PAY ELEM                          
*                                                                               
NTU20    B     NTU10               DONE WITH UNIT - GET NEXT                    
*                                                                               
*                                                                               
NTUX     DS    0H                                                               
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYP,X'01'                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     NTUX10                                                           
*                                                                               
NTUX5    GOTO1 BUFFALO,DMCB,=C'SEQ',(X'01',BUFFBUFF),BUFREC,0                   
NTUX10   CLI   DMCB+8,X'80'        END OF FILE                                  
         BE    NTUXX                                                            
         CLI   BUFTYP,X'01'        SINCE 'HIGH' DOSEN'T PASS EOF                
         BNE   NTUXX                                                            
         MVC   TBKEY,BUFREC+1                                                   
         MVC   TBBILLN(32),BUFPBILN                                             
         CLI   QOPT7,C'Y'                                                       
         BNE   *+14                                                             
         MVC   WORK(L'TBREC),TBREC                                              
         BAS   RE,NSORTIN                                                       
         GOTO1 SORTER,DMCB,=C'PUT',TBREC                                        
         MVI   SORTACT,C'Y'                                                     
         B     NTUX5                                                            
*                                                                               
NTUXX    XIT1                      REST IS SAME AS SPOT                         
         SPACE 2                                                                
NXTEL    DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NXTEL2                                                           
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NXTEL                                                            
*                                                                               
NXTEL2   DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         SPACE 2                                                                
NSORTIN  NTR1                                                                   
         MVC   P+1(8),=C'SORT IN='                                              
         GOTO1 HEXOUT,DMCB,WORK,P+10,56,0                                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         XIT1                                                                   
         SPACE 2                                                                
*                                                                               
CKPRD    CLC   QPRD,=C'ALL'                                                     
         BE    CKPYES                                                           
         CLC   QPRD,=C'   '                                                     
         BE    CKPYES                                                           
         CLC   QPRD,=C'POL'                                                     
         BE    CKPYES                                                           
         CLC   WPRD,BPRD                                                        
         BE    CKPYES              PRD NOT OK - RETURN WITH CC NE               
         BR    RE                                                               
*                                                                               
CKPRD3   CLC   QPRD,=C'ALL'                                                     
         BE    CKPYES                                                           
         CLC   QPRD,=C'   '                                                     
         BE    CKPYES                                                           
         CLC   QPRD,=C'POL'                                                     
         BE    CKPYES                                                           
         CLC   WPRD3,PRD           MATCH 3 CHARACTER CODES                      
         BE    CKPYES              PRD NOT OK - RETURN WITH CC NE               
         BR    RE                                                               
*                                                                               
CKPYES   CR    RE,RE               PRD OK                                       
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
PUTBUFFN NTR1                                                                   
         MVI   BUFTYP,X'01'                                                     
         MVC   BUFTYP+1(L'TBKEY),TBREC                                          
         MVC   BUFPBILN(32),TBBILLN                                             
         ZAP   BUFCBILG,=P'0'                                                   
         ZAP   BUFCPAYG,=P'0'                                                   
         ZAP   BUFPBILG,=P'0'                                                   
         ZAP   BUFPPAYG,=P'0'                                                   
*                                                                               
         CLI   QOPT7,C'B'                                                       
         BNE   PUTBUFNX                                                         
         BAS   RE,TBUFFIN                                                       
*                                                                               
PUTBUFNX GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
TBUFFIN  NTR1                                                                   
         MVC   P+1(8),=C'BUFF IN='                                              
         GOTO1 HEXOUT,DMCB,TBREC,P+10,56,0                                      
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         XIT1                                                                   
         SPACE 2                                                                
NTUDIV   DS    0H                                                               
         LTR   RF,RF               RF HAS SHARE PCT. 2 DECIMALS                 
         BP    *+8                                                              
         SR    R1,R1                                                            
         BR    RE                                                               
*                                                                               
         SLDA  R0,1                                                             
         D     R0,=F'10000'                                                     
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         BR    RE                                                               
         EJECT                                                                  
*          DATA SET SPREPA802  AT LEVEL 150 AS OF 11/16/05                      
*                                                                               
SETSHR   NTR1                                                                   
         LA    R5,2                2 AMOUNTS                                    
         LA    R6,WK                                                            
*                                                                               
SETS2    DS    0H                                                               
         L     R1,0(R6)                                                         
         M     R0,=F'1'                                                         
         MR    R0,RF                                                            
         BAS   RE,NTUDIV                                                        
         ST    R1,0(R6)                                                         
         LA    R6,4(R6)                                                         
         BCT   R5,SETS2                                                         
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
BTOTALS  CSECT                                                                  
         NMOD1 0,BTOTALS                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,RC,RA                                                    
         LA    R8,BTOTALS+4095                                                  
         LA    R8,1(R8)                                                         
         USING BTOTALS+4096,R8                                                  
*                                                                               
         LA    R9,SPACEND                                                       
         USING SPA8WRKD,R9                                                      
         MVC   BUFTYP,TOTTYP                                                    
         MVI   NETMEDSW,C'N'                                                    
         CLI   NETOPT,C'N'    SEE IF NETPACK                                    
         BNE   BTOT2                                                            
         CLI   MYA8PROF+9,C'Y'                                                  
         BNE   BTOT2                                                            
         MVI   NETMEDSW,C'Y'                                                    
*                                                                               
BTOT2    DS    0H                                                               
         XC    LASTMOS,LASTMOS                                                  
         XC    LASTCLT,LASTCLT                                                  
         CLI   NETMEDSW,C'Y'                                                    
         BNE   *+8                                                              
         MVI   LASTCLT,C' '  NETPAK MEDIA BREAKOUT NEEDS A SPACE                
*                                                                               
         MVI   CLTPSW,0                                                         
         XC    ESTCNT,ESTCNT                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     BTOT10                                                           
BTOT5    GOTO1 BUFFALO,DMCB,=C'SEQ',(TOTTYP,BUFFBUFF),BUFREC,0                  
*                                                                               
BTOT10   CLI   DMCB+8,X'80'        END                                          
         BE    BTOTX                                                            
         CLC   BUFMOS,=2X'FF'          TOTAL LINE                               
         BE    BTOT20                                                           
*                               GO TO BTOT20 SO THAT                            
*                               CLIENT AND/OR MEDIA WILL PRINT                  
*                               IF ONLY A TOTAL RECORD IS FOUND                 
******** BE    BTOT25              OLD CODE                                     
         CLI   BUFMOS,0            NO MTH OF SERV BREAKOUT                      
         BNE   BTOT12                                                           
         TM    ESTTBSW,4           SEE IF DOING EST BREAKOUT                    
         BZ    BTOT5               NO                                           
         CLI   TOTTYP,X'04'        SEE IF DOING PRD OR CLT                      
         BH    BTOT5                                                            
*                                                                               
BTOT12   DS    0H                                                               
         CLC   ESTCNT,=H'1'        MORE THAN 1 EST FOR MOS                      
         BH    BTOT15              PRINT MOS TOTAL EVEN IF 0                    
         CP    BUFPBILN,BUFPPAYN       SEE IF MOS HAS BALANCE FORWARD           
         BNE   BTOT15                                                           
         CP    BUFCBILN,=P'0'      CHK FOR CURRENT ACTIVITY                     
         BNE   BTOT15                                                           
         CP    BUFCPAYN,=P'0'                                                   
         BE    BTOT5                                                            
*                                                                               
BTOT15   DS    0H                                                               
         CLI   TOTTYP,X'04'        SEE IF DOING PRD OR CLT                      
         BH    BTOT20                                                           
         TM    ESTTBSW,4           EST WITHIN MOS RECAP                         
         BZ    BTOT20                                                           
         CLI   BUFEST,0                                                         
         BE    BTOT17                                                           
         CLI   BUFEST+1,X'FF'      MOS TOTAL                                    
         BE    BTOT18                                                           
         ZIC   R0,BUFEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+13(3),DUB                                                      
*                                                                               
BTOT17   LH    R1,ESTCNT                                                        
         LA    R1,1(R1)                                                         
         STH   R1,ESTCNT                                                        
BTOT18   DS    0H                                                               
         CLC   BUFMOS,LASTMOS      SAME MOS                                     
         BNE   BTOT20                                                           
         CLC   BUFEST(2),=2X'FF'                                                
         BNE   BTOT22                                                           
         CLI   QOPT4,C'M'          NO MTH OF SERV BREAKOUT                      
         BE    BTOT5               SKIP MOS TOTAL LINE                          
         CLI   QOPT4,C'B'          NO MTH OF SERV OR STA                        
         BE    BTOT5               SKIP MOS TOTAL LINE                          
         CLI   QOPT4,C'D'          NO MTH OF SERV OR STA                        
         BE    BTOT5               WITH DOWNLOAD                                
         CLC   ESTCNT,=H'1'                                                     
         BH    BTOT19                                                           
         XC    ESTCNT,ESTCNT       SKIP MOS TOTALS IF ONLY 1 EST                
         B     BTOT5                                                            
*                                                                               
BTOT19   DS    0H                                                               
         XC    ESTCNT,ESTCNT                                                    
*                                                                               
BTOT20   DS    0H                                                               
*                                                                               
******   CLI   TOTTYP,X'05'        OFFICE OR AGENCY TOTALS                      
******   BL    BTOT20C                                                          
*                                                                               
*        THE CHECK ABOVE WAS NO-OPED TO ALLOW FOR THE                           
*        NETPAK SUB-MEDIA TOTALS FOR TOTTYP = X'02'                             
*        AND X'04'                                                              
*                                                                               
*        NOTE - BUFCLT WILL BE ZEROS FOR THOSE TYPES                            
*               IF NETMEDSW IS NOT "Y" (AS WILL LASTCLT)                        
*                                                                               
         CLC   LASTCLT,BUFCLT                                                   
         BE    BTOT20C                                                          
         JIF   BUFCLT,NE,=3X'00',AND,BUFCLT,NE,=3X'FF',BTOT20A,JUMP=N           
         CLI   QOPT4,C'B'          NO MTH OF SERV                               
         BE    BT20A                                                            
         CLI   QOPT4,C'D'          NO MTH OF SERV                               
         BE    BT20A               WITH DOWNLOAD                                
         CLI   QOPT4,C'M'          NO MTH OF SERV                               
         BE    BT20A                                                            
         MVI   FORCEHED,C'Y'                                                    
BT20A    MVC   P(3),=C'ALL'                                                     
         B     BTOT20B                                                          
*                                                                               
BTOT20A  MVC   P(3),BUFCLT                                                      
         CLI   TOTTYP,X'05'                                                     
         BNL   BTOT20B                                                          
*                                                                               
         CLI   NETMEDSW,C'Y'                                                    
         BNE   BTOT20C                                                          
*                                                                               
         MVC   SAVEP,P                                                          
         MVC   P,SPACES                                                         
         MVI   FORCEHED,C'Y'                                                    
         CLI   BUFNMED,C' '                                                     
         BE    BTOT20C                                                          
         MVC   P(09),=C'ALL MEDIA'                                              
         CLI   BUFNMED,X'FF'                                                    
         BE    BTOT20A5                                                         
         MVC   P(09),=C'VOD      '       VIDEO ON DEMAND                        
         CLI   BUFNMED,C'V'                                                     
         BE    BTOT20A5                                                         
         MVC   P(09),=C'CABLE    '                                              
         CLI   BUFNMED,C'C'                                                     
         BE    BTOT20A5                                                         
         MVC   P(11),=C'SYNDICATION'                                            
         CLI   BUFNMED,C'S'                                                     
         BE    BTOT20A5                                                         
         MVC   P(11),=C'OTHER      '                                            
         CLI   BUFNMED,C'O'                                                     
         BE    BTOT20A5                                                         
         MVC   P+6(1),BUFNMED        FOR OTHERS SHOW SUBMEDIA CODE              
BTOT20A5 DS    0H                                                               
         MVC   SVNMED,P              SAVE MEDIA FOR NEW PAGE                    
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P,SAVEP                                                          
         MVC   P(3),SPACES        P LINE GETS PRINTED LATER                     
         MVC   LASTCLT,BUFCLT                                                   
         MVI   CLTPSW,1                                                         
         B     BTOT20F                                                          
*                                                                               
BTOT20B  MVC   LASTCLT,BUFCLT                                                   
         MVI   CLTPSW,1                                                         
         MVC   SVPCLT,P            SAVE CLT CODE FOR NEW PAGE                   
*                                                                               
BTOT20C  DS    0H                                                               
         CLI   NETMEDSW,C'Y'                                                    
         BNE   BTOT20C5                                                         
         B     BTOT20C6                                                         
BTOT20C5 DS    0H                                                               
         CLI   TOTTYP,X'05'        OFFICE OR AGY TOTALS                         
         BL    BTOT20F                                                          
*                                                                               
BTOT20C6 DS    0H                                                               
         ZIC   R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,X                                                             
         CLC   X(1),MAXLINES                                                    
         BNH   BTOT20F                                                          
         MVC   P(3),SVPCLT                                                      
*                                                                               
         CLI   NETMEDSW,C'Y'                                                    
         BNE   BTOT20F                                                          
*                                                                               
         CLI   TOTTYP,X'04'                                                     
         BH    BTOT20F                                                          
         MVC   SAVEP,P                                                          
         MVC   P(L'SVNMED),SVNMED                                               
         GOTO1 VPRINTIT,DMCB,(RC)  PRINT THE MEDIA DESCRIPTION                  
         MVC   P,SAVEP                                                          
         MVC   P(3),SPACES                                                      
*                                                                               
BTOT20F  DS    0H                                                               
*                                                                               
         CLC   BUFMOS,=2X'FF'      SEE IF TOTAL LINE                            
         BE    BTOT25                                                           
*                                                                               
         CLI   QOPT4,C'M'          NO MTH OF SERV BREAKOUT                      
         BE    BTOT22                                                           
         CLI   QOPT4,C'B'          NO MTH OF SERV BREAKOUT NO STA               
         BE    BTOT22                                                           
         CLI   QOPT4,C'D'          NO MTH OF SERV BREAKOUT NO STA               
         BE    BTOT22              WITH DOWNLOAD                                
*                                                                               
         CLI   BUFMOS+1,12         13TH MTH                                     
         BNH   BTOT21                                                           
         LA    R2,BUFMOS                                                        
         LA    R3,P+5                                                           
         BAS   RE,BMTH13                                                        
         B     BTOT22                                                           
*                                                                               
BTOT21   GOTO1 DATCON,DMCB,(3,BUFMOS),(9,P+5)                                   
BTOT22   ZAP   DOUBLE,BUFPBILN                                                  
         SP    DOUBLE,BUFPPAYN                                                  
         EDIT  (P8,DOUBLE),(14,P+20),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P8,BUFCBILN),(14,P+37),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,BUFCPAYN),(14,P+55),2,COMMAS=YES,FLOAT=-                     
         AP    DOUBLE,BUFCBILN                                                  
         SP    DOUBLE,BUFCPAYN                                                  
         EDIT  (P8,DOUBLE),(14,P+71),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P8,BUFCBILG),(14,P+89),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,BUFCPAYG),(14,P+106),2,COMMAS=YES,FLOAT=-                    
         MVC   LASTMOS,BUFMOS                                                   
         CLC   BUFEST(2),=2X'FF'                                                
         BNE   BTOT24                                                           
         MVI   P+11,C'*'                                                        
         MVI   P+34,C'*'                                                        
         MVI   P+51,C'*'                                                        
         MVI   P+69,C'*'                                                        
         MVI   P+85,C'*'                                                        
         MVI   P+103,C'*'                                                       
         MVI   P+120,C'*'                                                       
*                                                                               
BTOT24   GOTO1 VPRINTIT,DMCB,(RC)                                               
         B     BTOT5                                                            
*                                                                               
BTOT25   DS    0H                                                               
         CLI   QOPT4,C'B'          NO MOS OR STA                                
         BE    BTOT25C                                                          
         CLI   QOPT4,C'D'          NO MOS OR STA                                
         BE    BTOT25C             WITH DOWNLOAD                                
         CLI   QOPT4,C'M'                                                       
         BNE   BTOT27              MTH BREAKOUT - SKIP A LINE                   
BTOT25C  CLI   TOTTYP,4            SEE IF DOING CLT OR HIGHER                   
         BH    BTOT29              DON'T SKIP                                   
         TM    ESTTBSW,4           EST TOTALS                                   
         BZ    BTOT29                                                           
         B     BTOT27A                                                          
*                                                                               
BTOT27   CLI   TOTTYP,X'05'        OFFICE OR AGENCY TOTALS                      
         BL    BTOT27A                                                          
         CLC   BUFCLT,=3X'FF'                                                   
         BNE   BTOT28                                                           
BTOT27A  GOTO1 VPRINTIT,DMCB,(RC)  SKIP A LINE                                  
BTOT28   MVC   P+5(6),=C'TOTAL*'                                                
BTOT29   DS    0H                                                               
         CLI   NETMEDSW,C'Y'                                                    
         BNE   BTOT29A0                                                         
         B     BTOT29C                                                          
*                                                                               
BTOT29A0 DS    0H                                                               
         CLI   TOTTYP,X'05'        OFFICE OR AGENCY TOTALS                      
         BL    BTOT29F                                                          
*                                                                               
*        THE CHECK ABOVE WAS NO-OPED TO ALLOW FOR THE                           
*        NETPAK SUB-MEDIA TOTALS FOR TOTTYP = X'02'                             
*        AND X'04'                                                              
*                                                                               
         CLI   CLTPSW,1                                                         
         BE    BTOT29C             CLT PRINTED ALREADY                          
         CLC   BUFCLT,=3X'00'                                                   
         BE    BTOT29A                                                          
         CLC   BUFCLT,=3X'FF'                                                   
         BNE   BTOT29B                                                          
BTOT29A  DS    0H                                                               
         MVC   P(3),=C'ALL'                                                     
         B     BTOT29F                                                          
*                                                                               
BTOT29B  MVC   P(3),BUFCLT                                                      
         B     BTOT29F                                                          
*                                                                               
BTOT29C  DS    0H                                                               
         ZIC   R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,X                                                             
         CLC   X(1),MAXLINES                                                    
         BNH   BTOT29F                                                          
         MVC   P(3),SVPCLT                                                      
*                                                                               
         CLI   NETMEDSW,C'Y'                                                    
         BNE   BTOT29F                                                          
         CLI   TOTTYP,X'04'                                                     
         BH    BTOT29F                                                          
         MVC   SAVEP,P                                                          
         MVC   P(L'SVNMED),SVNMED                                               
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P,SAVEP                                                          
         MVC   P(3),SPACES                                                      
*                                                                               
BTOT29F  ZAP   DUB,BUFPBILN                                                     
         MVI   CLTPSW,0                                                         
         SP    DUB,BUFPPAYN                                                     
         ZAP   DOUBLE,DUB                                                       
         EDIT  (P8,DOUBLE),(14,P+20),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P8,BUFCBILN),(14,P+37),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,BUFCPAYN),(14,P+55),2,COMMAS=YES,FLOAT=-                     
         AP    DOUBLE,BUFCBILN                                                  
         SP    DOUBLE,BUFCPAYN                                                  
         EDIT  (P8,DOUBLE),(14,P+71),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P8,BUFCBILG),(14,P+89),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,BUFCPAYG),(14,P+106),2,COMMAS=YES,FLOAT=-                    
         MVI   P+34,C'*'                                                        
         MVI   P+51,C'*'                                                        
         MVI   P+69,C'*'                                                        
         MVI   P+85,C'*'                                                        
         MVI   P+103,C'*'                                                       
         MVI   P+120,C'*'                                                       
         CLI   TOTTYP,X'05'        SEE IF DOING OFFICE OR AGY                   
         BE    BTOT29H                                                          
         CLI   TOTTYP,X'06'        SEE IF DOING OFFICE OR REQ                   
         BE    BTOT29H                                                          
         CLI   QOPT4,C'B'          NO MOS OR STA                                
         BE    BTOT30              NO MOS                                       
         CLI   QOPT4,C'D'          NO MOS OR STA                                
         BE    BTOT30              WITH DOWNLOAD                                
         CLI   QOPT4,C'M'          USE 2* IF SHOWING MOS + EST                  
         BE    BTOT30              NO MOS                                       
         TM    ESTTBSW,X'04'                                                    
         BZ    BTOT30              NO EST                                       
         B     BTOT29J             GO USE 2 *                                   
*                                                                               
BTOT29H  CLC   BUFCLT,=3X'FF'                                                   
         BNE   BTOT30                                                           
         CLI   QOPT4,C'B'          SUMMARY VERSION                              
         BE    BTOT29K                                                          
         CLI   QOPT4,C'D'          SUMMARY VERSION                              
         BE    BTOT29K             DOWNLOADED                                   
*                                                                               
         CLI   QOPT4,C'M'          NO MOS - SKIP THIS *                         
         BE    BTOT29K                                                          
BTOT29J  MVI   P+11,C'*'                                                        
BTOT29K  MVI   P+35,C'*'                                                        
         MVI   P+52,C'*'                                                        
         MVI   P+70,C'*'                                                        
         MVI   P+86,C'*'                                                        
         MVI   P+104,C'*'                                                       
         MVI   P+121,C'*'                                                       
BTOT30   DS    0H                                                               
*                                                                               
         CLI   TOTTYP,X'05'        OFFICE OR AGENCY TOTALS                      
         BL    BTOT32                                                           
         MVI   SPACING,2                                                        
         CLI   MYA8PROF+5,C'Y'                                                  
         BNE   *+8                                                              
         MVI   SPACING,3                                                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         CLC   BUFCLT,=3X'FF'                                                   
         BNE   BTOT5               GO DO NEXT CLT                               
         B     BTOT35                                                           
*                                                                               
BTOT32   DS    0H                                                               
*                                                                               
         CLI   NETMEDSW,C'Y'                                                    
         BNE   BTOT32C                                                          
*                                                                               
         CLI   BUFNMED,X'FF'   SEE IF COMBINED MEDIA TOTALS                     
         BE    BTOT32C          NO - CONTINUE PROCESSING                        
         MVI   P+11,C' '        CLEAR SECOND *                                  
         MVI   P+35,C' '                                                        
         MVI   P+52,C' '                                                        
         MVI   P+70,C' '                                                        
         MVI   P+86,C' '                                                        
         MVI   P+104,C' '                                                       
         MVI   P+121,C' '                                                       
         XC    LASTMOS,LASTMOS     MUST CLEAR                                   
*                                                                               
*                MAY WANT TO CLEAR LASTCLT ALSO                                 
*                DOESN'T SEEM TO HAVE ANY EFFECT                                
*                                                                               
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         B     BTOT5                                                            
*                                                                               
BTOT32C  DS    0H                                                               
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         CLI   TOTTYP,4            SEE IF CLT OR HIGHER                         
         BNL   BTOT35                                                           
         CLI   QOPT2,C'Y'          OR DOING ALL PRDS TOGETHER                   
         BE    BTOT35                                                           
*                                                                               
         CLI   QPGR,C' '         SEE IF PRODUCT GROUP REQ?                      
         BH    BTOTX                                                            
*                                                                               
         CLC   QPRD,=C'ALL'                                                     
         BE    BTOTX                                                            
*                                  ONE PRD DO TOTALS                            
*                                                                               
BTOT35   DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 VPRINTIT,DMCB,(RC)  SKIP 2 LINES                                 
         MVI   ALLOWLIN,5                                                       
         MVC   P+1(12),=C'NET BILLINGS'                                         
         ZAP   DUB,BUFPBILN                                                     
*                                                                               
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
*   EDIT CAN'T PRINT AN AMOUNT OVER 999,999,999.99                              
*                                                                               
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+17(18),WORK+1                                                  
*                                                                               
         ZAP   DUB,BUFPBILN                                                     
         AP    DUB,BUFCBILN                                                     
*                                                                               
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
*   EDIT CAN'T PRINT AN AMOUNT OVER 999,999,999.99                              
*                                                                               
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+68(18),WORK+1                                                  
*                                                                               
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+1(14),=C'NET CLEARANCES'                                       
         ZAP   DUB,BUFPPAYN                                                     
*                                                                               
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
*   EDIT CAN'T PRINT AN AMOUNT OVER 999,999,999.99                              
*                                                                               
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+17(18),WORK+1                                                  
*                                                                               
         ZAP   DUB,BUFPPAYN                                                     
         AP    DUB,BUFCPAYN                                                     
*                                                                               
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
*   EDIT CAN'T PRINT AN AMOUNT OVER 999,999,999.99                              
*                                                                               
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+68(18),WORK+1                                                  
*                                                                               
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+1(14),=C'GROSS BILLINGS'                                       
         ZAP   DUB,BUFPBILG                                                     
*                                                                               
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
*   EDIT CAN'T PRINT AN AMOUNT OVER 999,999,999.99                              
*                                                                               
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+17(18),WORK+1                                                  
*                                                                               
         ZAP   DUB,BUFPBILG                                                     
         AP    DUB,BUFCBILG                                                     
*                                                                               
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
*   EDIT CAN'T PRINT AN AMOUNT OVER 999,999,999.99                              
*                                                                               
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+68(18),WORK+1                                                  
*                                                                               
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+1(16),=C'GROSS CLEARANCES'                                     
         ZAP   DUB,BUFPPAYG                                                     
*                                                                               
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
*   EDIT CAN'T PRINT AN AMOUNT OVER 999,999,999.99                              
*                                                                               
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+17(18),WORK+1                                                  
*                                                                               
         ZAP   DUB,BUFPPAYG                                                     
         AP    DUB,BUFCPAYG                                                     
*                                                                               
*   CAN'T USE EDIT MARCO - $ MAY BE TOO BIG                                     
*   EDIT CAN'T PRINT AN AMOUNT OVER 999,999,999.99                              
*                                                                               
         MVC   WORK(18),=X'4020206B2020206B2020206B2020214B2020'                
         ED    WORK(18),DUB+1                                                   
*                                                                               
         MVI   WORK+18,C' '        START WITH A SPACE                           
*                                                                               
         CP    DUB,=P'0'                                                        
         BNL   *+8                                                              
         MVI   WORK+18,C'-'                                                     
*                                                                               
         MVC   P+68(18),WORK+1                                                  
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
         L     RF,ADAGY                                                         
         CLI   AGYPROF+7-AGYKEY(RF),C'C'                                        
         BNE   BTOT35X                                                          
*                                                                               
         LA    R5,CGSTTOTS                                                      
         CLI   TOTTYP,X'04'        CLIENT TOTALS                                
         BE    BTOT35C                                                          
         CLI   QOPT2,C'Y'          SEE IF COMBINING PRODUCTS                    
         BNE   BTOT35B                                                          
         CLI   TOTTYP,X'02'       WILL BE UNDER PRODUCT TOTALS                  
         BE    BTOT35C                                                          
*                                                                               
BTOT35B  LA    R5,OGSTTOTS                                                      
         CLI   TOTTYP,X'05'        OFFICE TOTALS                                
         BE    BTOT35C                                                          
         LA    R5,RGSTTOTS                                                      
         CLI   TOTTYP,X'06'        REPORT TOTALS                                
         BE    BTOT35C                                                          
         B     BTOT35X                                                          
*                                                                               
BTOT35C  MVC   P+1(16),=C'PREV. BILLED GST'                                     
         EDIT  (P8,0(R5)),(14,P+20),2,COMMAS=YES,FLOAT=-                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+1(14),=C'PREV. PAID GST'                                       
         EDIT  (P8,8(R5)),(14,P+20),2,COMMAS=YES,FLOAT=-                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
BTOT35D  MVC   P+1(16),=C'CURR. BILLED GST'                                     
         EDIT  (P8,16(R5)),(14,P+20),2,COMMAS=YES,FLOAT=-                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+1(14),=C'CURR. PAID GST'                                       
         EDIT  (P8,24(R5)),(14,P+20),2,COMMAS=YES,FLOAT=-                       
         MVI   SPACING,2                                                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
BTOT35E  MVC   P+1(16),=C'PREV. BILLED PST'                                     
         EDIT  (P8,32(R5)),(14,P+20),2,COMMAS=YES,FLOAT=-                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+1(14),=C'PREV. PAID PST'                                       
         EDIT  (P8,40(R5)),(14,P+20),2,COMMAS=YES,FLOAT=-                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
BTOT35F  MVC   P+1(16),=C'CURR. BILLED PST'                                     
         EDIT  (P8,48(R5)),(14,P+20),2,COMMAS=YES,FLOAT=-                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+1(14),=C'CURR. PAID PST'                                       
         EDIT  (P8,56(R5)),(14,P+20),2,COMMAS=YES,FLOAT=-                       
         MVI   SPACING,2                                                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
BTOT35X  DS    0H                                                               
         CLI   TOTTYP,X'05'        OFFICE OR AGENCY                             
         BL    BTOT50                                                           
         CLI   PBERRSW,1                                                        
         BNE   *+8                                                              
         BAS   R7,PBILLERR                                                      
         CLI   CBERRSW,1                                                        
         BNE   *+8                                                              
         BAS   R7,CBILLERR                                                      
         CLI   PBERRSW,1                                                        
         BE    BTOT43                                                           
         CLI   CBERRSW,1                                                        
         BNE   BTOT44                                                           
BTOT43   MVC   P+3(26),=C'*** PLEASE CONTACT DDS ***'                           
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
BTOT44   CLC   ERRCLTS(2),=2X'00'                                               
         BE    BTOTX                                                            
         LA    R7,P+3                                                           
         LA    R5,ERRCLTS                                                       
BTOT45   CLC   0(2,R5),=2X'00'     END OF LIST                                  
         BE    BTOT46                                                           
         GOTO1 CLUNPK,DMCB,0(R5),0(R7)                                          
         MVI   3(R7),C','                                                       
         LA    R7,4(R7)                                                         
         LA    R5,2(R5)                                                         
         B     BTOT45                                                           
*                                                                               
BTOT46   BCTR  R7,0                                                             
         MVI   0(R7),C' '                                                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         B     BTOTX                                                            
*                                                                               
         B     BTOTX                                                            
*                                                                               
*                                                                               
BTOT50   DS    0H                                                               
         MVI   CLTBESW,0                                                        
*                                                                               
* SUPPRESS THE PREVIOUS OUT-OF-BALANCE E-MAIL WARNINGS FOR A HARD-CODED         
* LIST OF WI CLIENTS. THIS IS BECAUSE OF A MESS THAT WAS CREATED IN             
* EARLY 2001 DURING THE PW/COS2 CONVERSION, WHICH DOESN'T REALLY                
* MATTER NOW, BECAUSE THE TOTALS ARE IN BALANCE.                                
*                                                                               
         MVI   WARNFLAG,C'Y'       ASSUME WE'RE NOT SUPPRESSING E-MAIL          
         CLC   QAGY,=C'WI'         FOR INITIATIVE MEDIA ONLY                    
         BNE   BTOT53                                                           
         LA    RE,WICLTLST         LIST OF MEDIA/CLIENTS                        
BTOT51   CLC   QMED,0(RE)          IF IT'S IN THE LIST...                       
         BNE   BTOT52                                                           
         CLC   CLT,1(RE)                                                        
         BNE   BTOT52                                                           
         MVI   WARNFLAG,C'N'       ...WE MIGHT NOT SEND WARNING E-MAIL          
         B     BTOT53                                                           
BTOT52   LA    RE,L'WICLTLST(RE)                                                
         CLI   0(RE),X'FF'                                                      
         BNE   BTOT51                                                           
*                                                                               
BTOT53   DS    0H                                                               
         CP    BUFPBILN,PREBILLN                                                
         BE    BTOT55                                                           
         LA    R5,BUFPBILN                                                      
         LA    R6,PREBILLN                                                      
         BAS   R7,PBILLERR                                                      
BTOT55   DS    0H                                                               
         CP    BUFPBILG,PREBILLG                                                
         BE    BTOT58                                                           
         LA    R5,BUFPBILG                                                      
         LA    R6,PREBILLG                                                      
         BAS   R7,PGBILERR                                                      
*                                                                               
BTOT58   CP    BUFCBILN,CURBILLN                                                
         BE    BTOT60                                                           
         MVI   WARNFLAG,C'Y'       ALWAYS WARN OF CURRENT IMBALANCES            
         LA    R5,BUFCBILN                                                      
         LA    R6,CURBILLN                                                      
         BAS   R7,CBILLERR                                                      
BTOT60   CP    BUFCBILG,CURBILLG                                                
         BE    BTOT70                                                           
         MVI   WARNFLAG,C'Y'       ALWAYS WARN OF CURRENT IMBALANCES            
         LA    R5,BUFCBILG                                                      
         LA    R6,CURBILLG                                                      
         BAS   R7,GBILLERR                                                      
*                                                                               
BTOT70   CLI   CLTBESW,0                                                        
         BE    BTOTX                                                            
         MVC   P+3(26),=C'*** PLEASE CONTACT DDS ***'                           
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
*        SEND CONSOLE MESSAGE WHEN IMBALANCE HAS OCCURED                        
*                                                                               
         CLI   WARNFLAG,C'Y'       SEND WARNING E-MAIL?                         
         BNE   BTOT75              NO                                           
         CLC   =C'SJ',QAGY         (DON'T BOTHER FOR SJR)                       
         BE    BTOT75                                                           
         CLC   =C'TC',QAGY         (OR FOR TCH1)                                
         BE    BTOT75                                                           
         CLC   =C'DEIS',QUESTOR    (OR IF DEIS REQUESTS IT)                     
         BE    BTOT75                                                           
         CLC   =C'YKVA',QUESTOR    (OR IF YKVA REQUESTS IT)                     
         BE    BTOT75                                                           
         CLC   =C'BPLA',QUESTOR    (OR IF BPLA REQUESTS IT)                     
         BE    BTOT75                                                           
*                                                                               
         MVC   OPMSG+57(2),QAGY    DISPLAY AGENCY                               
         MVC   OPMSG+67(1),QMED    AND MEDIA                                    
         MVC   OPMSG+74(3),CLT                                                  
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(L'OPMSG,OPMSG)                           
*                                                                               
BTOT75   L     R5,ANXTECLT                                                      
         MVC   0(2,R5),BCLT                                                     
         LA    R5,2(R5)                                                         
         MVC   0(2,R5),=2X'00'                                                  
         ST    R5,ANXTECLT                                                      
         B     BTOTX                                                            
*                                                                               
*                                                                               
*                                                                               
BTOTX    XIT1                                                                   
*                                                                               
*                                                                               
PGBILERR DS    0H                                                               
         MVC   P+56(6),=C'-GROSS'                                               
PBILLERR MVC   P+3(11),=C'** PREVIOUS'                                          
         MVI   PBERRSW,1                                                        
         OI    CLTBESW,1                                                        
         B     BILLERR                                                          
*                                                                               
GBILLERR MVC   P+56(6),=C'-GROSS'                                               
CBILLERR MVC   P+4(10),=C'** CURRENT'                                           
         OI    CLTBESW,2                                                        
         MVI   CBERRSW,1                                                        
*                                                                               
BILLERR  MVC   P+15(40),=C'INVOICES DO NOT MATCH BILLING DETAILS **'            
         MVI   SPACING,2                                                        
         CLI   TOTTYP,X'05'        OFFICE TOTALS                                
         BE    BILLEX              YES                                          
         CLI   TOTTYP,X'06'        REQ TOTALS                                   
         BE    BILLEX              YES                                          
         EDIT  (P8,0(R6)),(14,P2+9),2,COMMAS=YES,FLOAT=-                        
         EDIT  (P8,0(R5)),(14,P2+38),2,COMMAS=YES,FLOAT=-                       
BILLEX   GOTO1 VPRINTIT,DMCB,(RC)                                               
         BR    R7                  RETURN                                       
*                                                                               
         EJECT                                                                  
BMTH13   DS    0H                  PRINTS BILLING PERIODS NOT MTHS              
         ZIC   R0,1(R2)            R2 POINTS TO BINARY YM                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R3),DUB         R3 POINTS TO PRINT LOCATION                  
*                                                                               
         MVI   2(R3),C'/'                                                       
         ZIC   R0,0(R2)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(2,R3),DUB                                                      
         BR    RE                                                               
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
NETMEDSW DC    C'N'                                                             
*                                                                               
OPMSG    DC    C'AUTONOTE*BPLADDNY,ERATDDNY:** SA8 ERROR DETECTED. AGEN+        
               CY=XX, MEDIA=X, CLT=XXX'                                         
*                                                                               
WARNFLAG DS    C                   'N' = SUPPRESS WARNING E-MAIL                
WICLTLST DS    0CL4                                                             
         DC    C'RBAP'         ADDED 06/15                                      
         DC    C'RBCO'                                                          
         DC    C'RBCS'                                                          
         DC    C'RBSF'                                                          
         DC    C'RBSV'                                                          
         DC    C'RCGW'                                                          
         DC    C'RFDL'                                                          
         DC    C'RG2S'                                                          
         DC    C'RG3S'                                                          
         DC    C'RH1O'         ADDED 06/15                                      
         DC    C'RH1D'         ADDED 06/15                                      
         DC    C'RITN'                                                          
         DC    C'RLZ2'                                                          
         DC    C'RLZ3'                                                          
         DC    C'RMBT'         ADDED 06/15                                      
         DC    C'RMIR'                                                          
         DC    C'RNWB'                                                          
         DC    C'RRZ2'                                                          
         DC    C'RRZ3'                                                          
         DC    C'RSTE'         ADDED 06/15                                      
         DC    C'TCGW'                                                          
         DC    C'TFDL'                                                          
         DC    C'TG2S'                                                          
         DC    C'THD1'                                                          
         DC    C'TLZ2'                                                          
         DC    C'TMSR'                                                          
         DC    C'TRH1'                                                          
         DC    C'TRZ2'                                                          
         DC    C'TRZ3'                                                          
         DC    X'FF'                                                            
         EJECT                                                                  
         SPACE 2                                                                
BLDNET   CSECT                                                                  
         NMOD1 0,BLDNET                                                         
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,RC,RA                                                    
         LA    R9,SPACEND                                                       
         USING SPA8WRKD,R9                                                      
         L     RF,VNETTAB                                                       
         XC    0(256,RF),0(RF)     CLEAR NETWORK TABLE                          
         XC    256(256,RF),256(RF)   (TABLE IS 64*8 LONG)                       
         MVC   KEY1,KEY            SAVE CURRENT KEY                             
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING NDEFRECD,R2                                                      
         MVC   NDEFKTYP,=X'0D11'   RECORD TYPE                                  
         MVC   NDEFKAGY,QAGY       AGENCY                                       
         GOTO1 HIGH                                                             
         B     BLDN04B                                                          
*                                                                               
BLDN04   DS    0H                                                               
         GOTO1 SEQ                                                              
BLDN04B  DS    0H                                                               
         CLC   KEY(4),KEYSAVE                                                   
         BNE   BLDN20                                                           
         OC    NDEFKNET,NDEFKNET   AGENCY-LEVEL NETDEF?                         
         BZ    BLDN04              YES -- SKIP                                  
         CLC   NDEFKCLT,BCLT       THIS CLIENT OK                               
         BE    BLDN06                                                           
         OC    NDEFKCLT,NDEFKCLT   OR AGENCY DEFAULT                            
         BNZ   BLDN04                                                           
BLDN06   DS    0H                                                               
         MVC   AREC,ADSTABUC       READ INTO STABUC                             
         GOTO1 GET                                                              
         L     R3,AREC                                                          
         LA    R3,NDEFEL-NDEFRECD(R3)                                           
*                                                                               
BLDN08   DS    0H                                                               
         CLI   0(R3),0             EOR                                          
         BE    BLDN04              NEXT RECORD                                  
         CLI   0(R3),X'02'         02 ELEM HAS NETWORK NUMBER                   
         BE    BLDN09                                                           
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     BLDN08                                                           
*                                                                               
BLDN09   DS    0H                                                               
         CLI   2(R3),63            DON'T ALLOW IF HIGHER THAN 63                
         BH    BLDN04              (TABLE ALLOWS FOR 0-63)                      
         MVC   DUB(4),NDEFKNET                                                  
         MVI   DUB+4,C'T'                                                       
         GOTO1 MSPACK,DMCB,=C'0000',DUB,X                                       
         ZIC   R4,2(R3)            NETWORK NUMBER                               
         MH    R4,=Y(NETTABL)                                                   
         A     R4,VNETTAB                                                       
         MVC   0(4,R4),NDEFKNET    SET NETWORK CODE                             
         MVC   4(3,R4),X+2         PACKED CODE TOO                              
         B     BLDN04              NEXT RECORD                                  
*                                                                               
BLDN20   DS    0H                  GET NETWORK LEVEL PSTS                       
*                                                                               
         MVC   KEY,KEY1                                                         
         GOTO1 HIGH                RESTORE SEQ                                  
*                                                                               
BLDNETX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
PRINTIT  CSECT                                                                  
         NMOD1 0,PRINTIT                                                        
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,RC,RA                                                    
         LA    R9,SPACEND                                                       
         USING SPA8WRKD,R9                                                      
*                                                                               
         MVI   RCWHATPR,1       DEFAULT TO FIRST PRINTQ                         
*                                                                               
         CLC   P+1(4),=C'SORT'                                                  
         BE    PRNT5            PRINT SORT LINES                                
*                                                                               
         CLI   QOPT4,C'D'       SEE IF DOWNLOADING                              
         BE    PRNT0                                                            
*                                                                               
         CLI   QOPT5,C'S'       OR STATION DOWNLOAD                             
         BNE   PRNT1                                                            
*                                                                               
*    WHEN DOWNLOADING SEND AGENCY/MEDIA SUMMARIES TO 2ND PRINTQ                 
*                                                                               
         CLI   RQEMAIL,C'Y'                                                     
         BNE   PRNT0                                                            
         MVI   RCWHATPR,2                                                       
         MVC   HEADHOOK,SAVHEADK   MUST RESTORE HEADHOOK                        
         B     PRNT1                                                            
*                                                                               
PRNT0    MVC   P,SPACES         JUST CLEAR P AND EXIT                           
         MVC   P2,SPACES        AND P2                                          
         B     PRNTITX                                                          
*                                                                               
PRNT1    DS    0H                                                               
*                                                                               
         CLI   RCSUBPRG,1       SEE IF DOING DETAIL SECTION                     
         BE    PRNT3                                                            
         CLI   RCSUBPRG,3                                                       
         BE    PRNT3                                                            
         CLI   RCSUBPRG,4                                                       
         BE    PRNT3                                                            
         B     PRNT5                                                            
*                                                                               
PRNT3    CLI   QOPT4,C'B'        SEE IF PRODUCT SUMMARY FORMAT                  
         BNE   PRNT5                                                            
         MVI   RCSUBPRG,15       SET SPECIAL SPROG                              
*                                                                               
PRNT5    GOTO1 REPORT                                                           
*                                                                               
PRNTITX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                          FOR DOWNLOAD PRINTING (QOPT4 = D)                    
*                          AND STATION DOWNLOAD (QOPT5 = S)                     
DOWNLD   CSECT                                                                  
         NMOD1 0,DOWNLD                                                         
*                                                                               
         MVI   DMTHSW,0                                                         
         CLI   0(R1),C'A'        MONTH OF SERVICE LINE                          
         BL    DNP1                                                             
         MVC   DMTHSW,0(R1)      SAVE INDICATOR                                 
         MVI   0(R1),0                                                          
DNP1     DS    0H                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,RC,RA                                                    
         LA    R9,SPACEND                                                       
         USING SPA8WRKD,R9                                                      
*                                                                               
         LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         OI    DLCBFLG1,DLCBFXTN                                                
         MVC   DLCXTND(7),MAXLINE                                               
         MVC   DLCBAPR,=A(DNPRINT)                                              
         LA    R0,P                                                             
         ST    R0,DLCBAPL                                                       
*                                                                               
         CLI   MODE,RUNLAST       SEE IF END OF RUN                             
         BE    DNP70                                                            
*                                                                               
         CLI   MODE,REQLAST       SEE IF END OF REPORT                          
         BE    DNP70                                                            
*                                                                               
         CLI   MODE,REQFRST       SEE IF I NEED TO INTIALIZE                    
         BE    DNP80                                                            
*                                                                               
         MVC   DNLINE,P          SAVE CONTENTS OF PRINTLINE                     
         MVC   P,SPACES                                                         
*                                                                               
         CLI   MYA8PROF+15,C'Y'   DOWNLOAD HEADERS?                             
         BNE   DNP1HX                                                           
         CLI   DHEADIND,1        ALREADY DONE?                                  
         BE    DNP1HX                                                           
*                                                                               
DNP1H1   DS    0H                DOWNLOAD HEADERS (FIRST LINE)                  
*                                SYSTEM FIELD - EMPTY                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                MEDIA FIELD                                    
         MVC   DLCBFLD(5),=C'MEDIA'                                             
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                OFFICE FIELD                                   
         MVC   DLCBFLD(6),=C'OFFICE'                                            
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   PROFA8A+1,C'Y'    DOWNLOADING ACC OFFICE?                        
         BNE   DNP1HC                                                           
         MVC   DLCBFLD(10),=C'ACC OFFICE'                                       
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                CLIENT FIELD                                   
DNP1HC   DS    0H                                                               
         MVC   DLCBFLD(6),=C'CLIENT'                                            
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                PRODUCT FIELD                                  
         MVC   DLCBFLD(7),=C'PRODUCT'                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   QOPT4,C'D'         SEE IF DOING PRODUCT DOWNLOAD                 
         BNE   DNP1H0                                                           
*                                  ONLY BALANCE OUT IS SHOWN                    
         MVC   DLCBFLD(7),=C'BALANCE'                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBACT,DLCBEOL    END OF LINE                                   
         GOTO1 VDLFLD                                                           
*                                SECOND LINE - EMPTY EXCEPT "OUT"               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP1H2E           RESET IS THE SAME AS USED FOR                  
*                                QOPT5 = P (PUB DOWNLOAD)                       
*                                                                               
*                                STATION FIELD                                  
DNP1H0   DS    0H                                                               
         MVC   DLCBFLD(7),=C'STATION'                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   MYA8PROF+10,C'N'      NO MOS?                                    
         BE    DNP1H1A                                                          
*                                MONTH OF SERVICE                               
         MVC   DLCBFLD(3),=C'MOS'                                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP1H1A  TM    ESTTBSW,X'02'     ESTIMATE SHOWN?                                
         BZ    DNP1H1B                                                          
*                                ESTIMATE FIELD                                 
         MVC   DLCBFLD(3),=C'EST'                                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                BALANCE FORWARD                                
*                                (FIRST WORD)                                   
DNP1H1B  MVC   DLCBFLD(7),=C'BALANCE'                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                CURRENT BILLING                                
*                                (FIRST WORD)                                   
         MVC   DLCBFLD(7),=C'CURRENT'                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         CLI   MYA8PROF+10,C'Y'  INVOICE/CLEAR DATE DETAILS?                    
         BNE   DNP1H1C                                                          
*                                INVOICE NUMBER                                 
*                                (FIRST WORD)                                   
         MVC   DLCBFLD(7),=C'INVOICE'                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                CURRENT CLEARANCES                             
*                                (FIRST WORD)                                   
DNP1H1C  MVC   DLCBFLD(7),=C'CURRENT'                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         CLI   MYA8PROF+10,C'Y'  INVOICE/CLEAR DATE DETAILS?                    
         BNE   DNP1H1E                                                          
*                                DATE CLEARED                                   
*                                (FIRST WORD)                                   
         MVC   DLCBFLD(4),=C'DATE'                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
DNP1H1E  DS    0H                                                               
*                                BALANCE OUT                                    
*                                (FIRST WORD)                                   
         MVC   DLCBFLD(7),=C'BALANCE'                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBACT,DLCBEOL    END OF LINE                                   
         GOTO1 VDLFLD                                                           
*                                                                               
*                                                                               
DNP1H2   DS    0H                DOWNLOAD HEADERS (SECOND LINE)                 
*                                SYSTEM FIELD - EMPTY                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                MEDIA FIELD - EMPTY                            
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                OFFICE FIELD - EMPTY                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                ACC OFFICE FIELD - EMPTY                       
         CLI   PROFA8A+1,C'Y'     DOWNLOADING ACC OFFICE?                       
         BNE   DNP1H2A0                                                         
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                CLIENT FIELD - EMPTY                           
DNP1H2A0 MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                PRODUCT FIELD - EMPTY                          
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                STATION FIELD - EMPTY                          
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   MYA8PROF+10,C'N'    NO MOS?                                      
         BE    DNP1H2A                                                          
*                                MONTH OF SERVICE - EMPTY                       
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP1H2A  TM    ESTTBSW,X'02'     ESTIMATE SHOWN?                                
         BZ    DNP1H2B                                                          
*                                ESTIMATE FIELD - EMPTY                         
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                BALANCE FORWARD                                
*                                (SECOND WORD)                                  
DNP1H2B  MVC   DLCBFLD(7),=C'FORWARD'                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                CURRENT BILLING                                
*                                (SECOND WORD)                                  
         MVC   DLCBFLD(7),=C'BILLING'                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         CLI   MYA8PROF+10,C'Y'  INVOICE/CLEAR DATE DETAILS?                    
         BNE   DNP1H2C                                                          
*                                INVOICE NUMBER                                 
*                                (SECOND WORD)                                  
         MVC   DLCBFLD(6),=C'NUMBER'                                            
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                CURRENT CLEARANCES                             
*                                (SECOND WORD)                                  
DNP1H2C  MVC   DLCBFLD(9),=C'CLEARANCE'                                         
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         CLI   MYA8PROF+10,C'Y'  INVOICE/CLEAR DATE DETAILS?                    
         BNE   DNP1H2E                                                          
*                                DATE CLEARED                                   
*                                (SECOND WORD)                                  
         MVC   DLCBFLD(7),=C'CLEARED'                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
DNP1H2E  DS    0H                                                               
*                                BALANCE OUT                                    
*                                (SECOND WORD)                                  
         MVC   DLCBFLD(3),=C'OUT'                                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DLCBACT,DLCBEOL    END OF LINE                                   
         GOTO1 VDLFLD                                                           
*                                                                               
         MVI   DHEADIND,1         SET HEADERS SENT                              
*                                                                               
DNP1HX   DS    0H                                                               
*                                                                               
         JIF   MYA8PROF+10,EQ,C'M',AND,DMTHSW,EQ,X'00',DNPX,JUMP=N              
*                                                                               
*        FOR MONTH OF SERVICE ONLY - SKIP TOTAL LINES                           
*                                                                               
         MVC   DLCBFLD(1),DNLINE SYSTEM                                         
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(1),DNLINE+2 MEDIA                                        
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(L'SAVCOFF),DNLINE+4 OFFICE                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   PROFA8A+1,C'Y'     DOWNLOADING ACC OFFICE?                       
         BNE   DNP2                                                             
         MVC   DLCBFLD(L'SAVAOFF),DNLINE+120    ACC OFFICE                      
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP2     DS    0H                                                               
         MVC   DLCBFLD(3),DNLINE+6 CLIENT                                       
*                                                                               
         CLI   PROFA8A+0,C'C'    DOWNLOADING CLT NAME?                          
         BNE   DNP3                                                             
         L     RE,ADCLT                                                         
         USING CLTHDR,RE                                                        
         MVC   DLCBFLD+4(20),CNAME                                              
         DROP  RE                                                               
*                                                                               
DNP3     DS    0H                                                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),DNLINE+10 PRODUCT                                     
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   QOPT5,C'S'        STATION DOWNLOAD                               
         BE    DNP5                                                             
         MVC   DLCBFLD(12),DOLS  BALANCE OUT                                    
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   MYA8PROF+13,C'T'  ALL TEXT FIELDS                                
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP60              GO SEND END OF LINE                           
*                                                                               
DNP5     DS    0H                                                               
*                                                                               
         MVC   DLCBFLD(8),DNLINE+30    STATION                                  
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   MYA8PROF+10,C'Y'   MTH OF SERV,INVS,DATES                        
         BE    DNP10                                                            
         CLI   MYA8PROF+10,C'M'   MTH OF SERV ONLY                              
         BE    DNP10                                                            
*                                                                               
*        MYA8PROF+10 SHOULD NOW BE C'N'                                         
*                                                                               
         CLI   DMTHSW,C'E'          ESTIMATE LINE?                              
         BE    DNP21                                                            
*                                                                               
         TM    ESTTBSW,X'02'        SHOWING ESTIMATE?                           
         BZ    DNP5B                                                            
*                                EMPTY FOR PUB TOTAL                            
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP5B    MVC   DLCBFLD(L'SBALF),SBALF BALANCE FROWARD                           
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   MYA8PROF+13,C'T'  ALL TEXT FIELDS                                
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         TM    ESTTBSW,X'02'     SHOWING ESTIMATE?                              
         BZ    DNP5F                                                            
*                                                                               
*        SHOW ZERO CURRENT BILLING AND CLEARANCE HERE                           
*        THOSE WILL BE IN ESTIMATE LINE                                         
*                                                                               
         MVC   SCBIL,SPACES                                                     
         MVC   SCPAY,SPACES                                                     
         MVI   SCBIL,C'0'                                                       
         CLI   MYA8PROF+11,C'Y'   2 DECIMALS?                                   
         BNE   *+10                                                             
         MVC   SCBIL(3),=C'.00'                                                 
         MVI   SCPAY,C'0'                                                       
         CLI   MYA8PROF+11,C'Y'   2 DECIMALS?                                   
         BNE   *+10                                                             
         MVC   SCPAY(3),=C'.00'                                                 
*                                                                               
DNP5F    MVC   DLCBFLD(L'SCBIL),SCBIL CURRENT BILLING                           
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   MYA8PROF+13,C'T'  ALL TEXT FIELDS                                
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVC   DLCBFLD(L'SCPAY),SCPAY CURRENT CLEARANCES                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   MYA8PROF+13,C'T'  ALL TEXT FIELDS                                
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVC   DLCBFLD(L'SBALO),SBALO BALANCE OUT                               
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   MYA8PROF+13,C'T'  ALL TEXT FIELDS                                
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP60                                                            
*                                                                               
DNP10    DS    0H                  STATION DOWNLOAD WITH                        
*                                  MOS,INVS,CLEARNACE DATES                     
         CLI   DMTHSW,C'M'         SEE IF MOTH OF SERVICE LINE                  
         BE    DNP20                                                            
         CLI   DMTHSW,C'E'         OR ESTIMATE MOS LINE                         
         BE    DNP20                                                            
*                                                                               
         CLI   MYA8PROF+10,C'M'  MOS ONLY - NOT STATION TOTALS                  
         BE    DNPX              JUST EXIT                                      
*                                                                               
*                                NO MOS - SEND EMPTY FIELD                      
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         TM    ESTTBSW,2          SEE IF SORTING ON EST                         
         BZ    DNP12                                                            
*                           YES -NEED TO SEND ANOTHER EMPTY LINE                
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
*                                                                               
DNP12    MVC   DLCBFLD(L'SBALF),SBALF BALANCE FROWARD                           
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   MYA8PROF+13,C'T'  ALL TEXT FIELDS                                
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVI   DLCBFLD,C'0'      DON'T SHOW CURRENT $ HERE                      
         CLI   MYA8PROF+11,C'Y'  $ WITH 2 DECIMALS?                             
         BNE   *+10                                                             
         MVC   DLCBFLD(3),=C'.00'                                               
*                                THEY WILL APPEAR IN MOS LINES                  
******** MVC   DLCBFLD(L'SCBIL),SCBIL CURRENT BILLING                           
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   MYA8PROF+13,C'T'  ALL TEXT FIELDS                                
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                NO INV - SEND EMPTY FIELD                      
         CLI   MYA8PROF+10,C'M'    MOS ONLY?                                    
         BE    DNP12C            SKIP INV NUMBER FIELD                          
*                                                                               
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP12C   MVI   DLCBFLD,C'0'      DON'T SHOW CURRENT $ HERE                      
         CLI   MYA8PROF+11,C'Y'  $ WITH 2 DECIMALS?                             
         BNE   *+10                                                             
         MVC   DLCBFLD(3),=C'.00'                                               
*                                THEY WILL APPEAR IN MOS LINES                  
******** MVC   DLCBFLD(L'SCPAY),SCPAY CURRENT CLEARANCES                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   MYA8PROF+13,C'T'  ALL TEXT FIELDS                                
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                NO INV - SEND EMPTY FIELD                      
         CLI   MYA8PROF+10,C'M'    MOS ONLY?                                    
         BE    DNP12E            SKIP CLEARANCE DATE FIELD                      
*                                NO DATES-SEND EMPTY FIELD                      
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP12E   MVC   DLCBFLD(L'SBALO),SBALO BALANCE OUT                               
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   MYA8PROF+13,C'T'  ALL TEXT FIELDS                                
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP60                                                            
*                                                                               
DNP20    DS    0H                 MONTH OF SERVICE LINE                         
         L     R5,INVADR       ADDRESS OF INV ENTRY                             
         L     R6,PAYADR       ADDRESS OF PAY ENTRY                             
         MVC   DLCBFLD(8),DNLINE+90     MOS SAVE THERE                          
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP21    LA    R4,MOSTOTS                                                       
*                                                                               
         CLI   DMTHSW,C'E'       SEE IF DOING EST MOS LINE                      
         BNE   DNP22                                                            
         MVC   DLCBFLD(7),DNLINE+17     EST/LNE                                 
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         LA    R4,ESTTOTS                                                       
*                                                                               
DNP22    DS    0H                                                               
         CLI   MYA8PROF+10,C'M'  MOS ONLY?                                      
         BNE   DNP22A                                                           
*                                SHOW MONTH BALANCE FORWARD                     
         ST    R1,SAVER1                                                        
         ZAP   DOUBLE,0(8,R4)    PREV BILLED                                    
         SP    DOUBLE,8(8,R4)    PREV CLEARED                                   
         CLI   MYA8PROF+11,C'Y'  $ WITH 2 DECIMALS?                             
         BE    DNP224                                                           
         CLI   MYA8PROF+12,C'Y'   MINUS BEFORE?                                 
         BE    DNP222                                                           
         EDIT  (P8,DOUBLE),(12,MYWORK),MINUS=YES,ZERO=NOBLANK                   
         L     R1,SAVER1       MUST RESTORE R1                                  
         MVC   DLCBFLD(12),MYWORK                                               
         B     DNP222A                                                          
*                                                                               
DNP222   EDIT  (P8,DOUBLE),(13,MYWORK),ZERO=NOBLANK,FLOAT=-                     
         L     R1,SAVER1       MUST RESTORE R1                                  
         MVC   DLCBFLD(13),MYWORK                                               
DNP222A  MVI   DLCBTYP,C'N'                                                     
         CLI   MYA8PROF+13,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP50                                                            
*                                                                               
DNP224   DS    0H                                                               
         CLI   MYA8PROF+12,C'Y'   MINUS BEFORE?                                 
         BE    DNP224A                                                          
         EDIT  (P8,DOUBLE),(12,MYWORK),2,MINUS=YES,ZERO=NOBLANK                 
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(12),MYWORK                                               
         B     DNP224B                                                          
*                                                                               
DNP224A  EDIT  (P8,DOUBLE),(13,MYWORK),2,ZERO=NOBLANK,FLOAT=-                   
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(13),MYWORK                                               
DNP224B  MVI   DLCBTYP,C'N'                                                     
         CLI   MYA8PROF+13,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP50                                                            
*                                                                               
DNP22A   MVI   DLCBFLD,C'0'      EMPTY BALANCE FORWARD FIELD                    
         CLI   MYA8PROF+11,C'Y'  $ WITH 2 DECIMALS?                             
         BNE   *+10                                                             
         MVC   DLCBFLD(3),=C'.00'                                               
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   MYA8PROF+13,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT                                           
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   MYA8PROF+10,C'M'    MOS ONLY?                                    
         BE    DNP50                                                            
*                                                                               
         CLI   MYA8PROF+10,C'N'    OR NO MOS                                    
         BE    DNP50                                                            
*                                                                               
         CLC   0(5,R5),=5X'FF'     DO I HAVE AN INVOICE ENTRY?                  
         BE    DNP25               NO  SEND 2 EMPTY FIELDS                      
         ST    R1,SAVER1     SAVE AND RESTORE R1                                
         CLI   MYA8PROF+11,C'Y'     $ WITH 2 DECIMALS?                          
         BE    DNP22C                                                           
         CLI   MYA8PROF+12,C'Y'     MINUS BEFORE?                               
         BE    DNP22B                                                           
         EDIT  (P8,5(R5)),(11,DLCBFLD),MINUS=YES,ZERO=NOBLANK                   
         B     DNP22D                                                           
*                                                                               
DNP22B   EDIT  (P8,5(R5)),(11,MYWORK),ZERO=NOBLANK,FLOAT=-                      
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(11),MYWORK                                               
         B     DNP22D                                                           
*                                                                               
DNP22C   DS    0H                                                               
         CLI   MYA8PROF+12,C'Y'     MINUS BEFORE?                               
         BE    DNP22C5                                                          
         EDIT  (P8,5(R5)),(12,DLCBFLD),2,MINUS=YES,ZERO=NOBLANK                 
         B     DNP22D                                                           
*                                                                               
DNP22C5  EDIT  (P8,5(R5)),(12,MYWORK),2,ZERO=NOBLANK,FLOAT=-                    
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(12),MYWORK                                               
*                                                                               
DNP22D   DS    0H                                                               
*                            RESTORE R1 AFTER EDITS                             
         L     R1,SAVER1     VDLFLD CALL DEPENDS ON IT                          
         MVI   DLCBTYP,C'N'      NUMBER                                         
         CLI   MYA8PROF+13,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT                                           
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVC   DLCBFLD(7),DNLINE+51   INVOICE NUMBER                            
         MVI   DLCBTYP,C'T'      TEXT                                           
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP30                                                            
*                                                                               
DNP25    DS    0H                                                               
*                                NO INV ENTRY - 2 EMPTY FIELDS                  
         MVI   DLCBFLD,C'0'                                                     
         CLI   MYA8PROF+11,C'Y'  $ WITH 2 DECIMALS?                             
         BNE   *+10                                                             
         MVC   DLCBFLD(3),=C'.00'                                               
         MVI   DLCBTYP,C'N'      NUMBER                                         
         CLI   MYA8PROF+13,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT                                           
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
DNP30    DS    0H                                                               
         CLC   0(5,R6),=5X'FF'    NO PAYMENT ENTRY                              
         BE    DNP35              NO - SEND 2 EMPTY PAYMENT FIELDS              
         ST    R1,SAVER1     SAVE AND RESTORE R1                                
         CLI   MYA8PROF+11,C'Y'   $ WITH 2 DECIMALS                             
         BE    DNP30C                                                           
         CLI   MYA8PROF+12,C'Y'     MINUS BEFORE?                               
         BE    DNP30B                                                           
         EDIT  (P8,5(R6)),(11,DLCBFLD),MINUS=YES,ZERO=NOBLANK                   
         B     DNP30D                                                           
*                                                                               
DNP30B   EDIT  (P8,5(R6)),(11,MYWORK),ZERO=NOBLANK,FLOAT=-                      
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(11),MYWORK                                               
         B     DNP30D                                                           
DNP30C   DS    0H                                                               
         CLI   MYA8PROF+12,C'Y'     MINUS BEFORE?                               
         BE    DNP30C5                                                          
         EDIT  (P8,5(R6)),(12,DLCBFLD),2,MINUS=YES,ZERO=NOBLANK                 
         B     DNP30D                                                           
*                                                                               
DNP30C5  EDIT  (P8,5(R6)),(12,MYWORK),2,ZERO=NOBLANK,FLOAT=-                    
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(12),MYWORK                                               
*                                                                               
DNP30D   DS    0H                                                               
*                           RESTORE R1 AS EDITS CLOBBER IT                      
         L     R1,SAVER1    VDLFLD CALL DEPENDS ON IT                           
         MVI   DLCBTYP,C'N'      NUMBER                                         
         CLI   MYA8PROF+13,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT                                           
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(8),DNLINE+76   CLEARANCE DATE                            
         MVI   DLCBTYP,C'T'      TEXT                                           
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP40             GO SEND EMPTY BALANCE OUT                      
*                                                                               
DNP35    DS    0H                                                               
*                                NO PAY ENTRY - 2 EMPTY FIELDS                  
         MVI   DLCBFLD,C'0'                                                     
         CLI   MYA8PROF+11,C'Y'  $ WITH 2 DECIMALS?                             
         BNE   *+10                                                             
         MVC   DLCBFLD(3),=C'.00'                                               
         MVI   DLCBTYP,C'N'      NUMBER - CLEARANCE AMOUNT                      
         CLI   MYA8PROF+13,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT                                           
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD - DATE                              
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP40    MVI   DLCBFLD,C'0'      EMPTY BALANCE OUT FIELD                        
         CLI   MYA8PROF+11,C'Y'  $ WITH 2 DECIMALS?                             
         BNE   *+10                                                             
         MVC   DLCBFLD(3),=C'.00'                                               
         MVI   DLCBTYP,C'N'      NUMBER                                         
         CLI   MYA8PROF+13,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT                                           
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP60                                                            
*                                                                               
DNP50    DS    0H               MOS ONLY DOWNLOAD                               
         ST    R1,SAVER1     SAVE AND RESTORE R1                                
         CLI   MYA8PROF+11,C'Y'   $ WITH 2 DECIMALS                             
         BE    DNP50C                                                           
         CLI   MYA8PROF+12,C'Y'     MINUS BEFORE?                               
         BE    DNP50B                                                           
         EDIT  (P8,16(R4)),(12,DLCBFLD),MINUS=YES,ZERO=NOBLANK                  
         B     DNP50D                                                           
*                                                                               
DNP50B   EDIT  (P8,16(R4)),(12,MYWORK),ZERO=NOBLANK,FLOAT=-                     
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(12),MYWORK                                               
         B     DNP50D                                                           
DNP50C   DS    0H                                                               
         CLI   MYA8PROF+12,C'Y'     MINUS BEFORE?                               
         BE    DNP50C5                                                          
         EDIT  (P8,16(R4)),(13,DLCBFLD),2,MINUS=YES,ZERO=NOBLANK                
         B     DNP50D                                                           
*                                                                               
DNP50C5  EDIT  (P8,16(R4)),(13,MYWORK),2,ZERO=NOBLANK,FLOAT=-                   
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(13),MYWORK                                               
*                                                                               
DNP50D   DS    0H                                                               
*                           RESTORE R1 AS EDITS CLOBBER IT                      
         L     R1,SAVER1    VDLFLD CALL DEPENDS ON IT                           
         MVI   DLCBTYP,C'N'      NUMBER                                         
         CLI   MYA8PROF+13,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT                                           
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP55    DS    0H               MOS ONLY DOWNLOAD                               
         ST    R1,SAVER1     SAVE AND RESTORE R1                                
         CLI   MYA8PROF+11,C'Y'   $ WITH 2 DECIMALS                             
         BE    DNP55C                                                           
         CLI   MYA8PROF+12,C'Y'     MINUS BEFORE?                               
         BE    DNP55B                                                           
         EDIT  (P8,24(R4)),(12,DLCBFLD),MINUS=YES,ZERO=NOBLANK                  
         B     DNP55D                                                           
*                                                                               
DNP55B   EDIT  (P8,24(R4)),(12,MYWORK),ZERO=NOBLANK,FLOAT=-                     
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(12),MYWORK                                               
         B     DNP55D                                                           
DNP55C   DS    0H                                                               
         CLI   MYA8PROF+12,C'Y'     MINUS BEFORE?                               
         BE    DNP55C5                                                          
         EDIT  (P8,24(R4)),(13,DLCBFLD),2,MINUS=YES,ZERO=NOBLANK                
         B     DNP55D                                                           
*                                                                               
DNP55C5  EDIT  (P8,24(R4)),(13,MYWORK),2,ZERO=NOBLANK,FLOAT=-                   
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(13),MYWORK                                               
*                                                                               
DNP55D   DS    0H                                                               
*                           RESTORE R1 AS EDITS CLOBBER IT                      
         L     R1,SAVER1    MUST RESTORE R1                                     
         MVI   DLCBTYP,C'N'      NUMBER                                         
         CLI   MYA8PROF+13,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT                                           
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP57    DS    0H                                                               
         JIF   MYA8PROF+10,EQ,C'N',AND,DMTHSW,EQ,C'E',DNP40,JUMP=N              
*              ESTIMATE LINE WITH NO MOS                                        
*              SKIP TO SEND EMPTY BALANCE OUT                                   
*              FOR THAT FORMAT BALANCES IN AND OUT WILL                         
*              DISPLAY ONLY AT STATION TOTAL                                    
*                                                                               
         ST    R1,SAVER1     SAVE AND RESTORE R1                                
         ZAP   DOUBLE,0(8,R4)    PREV BILLED                                    
         SP    DOUBLE,8(8,R4)    PREV CLEARED                                   
         AP    DOUBLE,16(8,R4)   CURRENT BILLED                                 
         SP    DOUBLE,24(8,R4)   CURRENT PAID                                   
*                                                                               
         CLI   MYA8PROF+11,C'Y'  $ WITH 2 DECIMALS?                             
         BE    DNP574                                                           
         CLI   MYA8PROF+12,C'Y'   MINUS BEFORE?                                 
         BE    DNP572                                                           
         EDIT  (P8,DOUBLE),(12,MYWORK),MINUS=YES,ZERO=NOBLANK                   
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(12),MYWORK                                               
         B     DNP572A                                                          
*                                                                               
DNP572   EDIT  (P8,DOUBLE),(13,MYWORK),ZERO=NOBLANK,FLOAT=-                     
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(13),MYWORK                                               
DNP572A  MVI   DLCBTYP,C'N'                                                     
         CLI   MYA8PROF+13,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP60                                                            
*                                                                               
DNP574   DS    0H                                                               
         CLI   MYA8PROF+12,C'Y'   MINUS BEFORE?                                 
         BE    DNP574A                                                          
         EDIT  (P8,DOUBLE),(12,MYWORK),2,MINUS=YES,ZERO=NOBLANK                 
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(12),MYWORK                                               
         B     DNP574B                                                          
*                                                                               
DNP574A  EDIT  (P8,DOUBLE),(13,MYWORK),2,ZERO=NOBLANK,FLOAT=-                   
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(13),MYWORK                                               
DNP574B  MVI   DLCBTYP,C'N'                                                     
         CLI   MYA8PROF+13,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP60    DS    0H                                                               
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         GOTO1 VDLFLD                                                           
         B     DNPX                                                             
*                                                                               
DNP70    DS    0H                                                               
         MVC   P,SPACES            JUST IN CASE                                 
         MVI   DLCBACT,C'R'        SET END OF REPORT                            
         GOTO1 VDLFLD                                                           
         B     DNPX                                                             
*                                                                               
DNP80    DS    0H                                                               
         MVC   P,SPACES            JUST IN CASE                                 
         MVI   DLCBACT,C'I'        START AND INTIALIZE REPORT                   
         GOTO1 VDLFLD                                                           
DNPX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
DNPRINT  NTR1                                                                   
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         MVI   SPACING,0                                                        
         MVI   RCWHATPR,1   DOWNLOAD TO FIRST PRINTQ                            
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'N'                                                    
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
DMTHSW   DS    CL1                                                              
SAVER1   DS    F                                                                
MYWORK   DS    CL20                                                             
DNLINE   DS    CL132                                                            
         DS    0H                                                               
MAXLINE  DC    H'132'                                                           
DELIM    DC    C' '        FIELD DELIMITER                                      
EOTCHR   DC    C'"'        END OF TEXT FIELD DELIMITER                          
EOTALT   DC    C''''       END OF TEXT CHR ALTERNATE                            
EOLCHR   DC    X'5E'       END OF LINE CHAR - SEMI-COLON                        
EORCHR   DC    C':'        END OF REPORT CHR                                    
*                                                                               
DLCB     DS    XL256                                                            
         DROP  R1                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
SPA8WRKD DSECT                                                                  
EOF      EQU   X'FF'                                                            
DDSIND   DS    XL1                 SET TO 1 FOR DDS MONTH-END                   
DOWNIND  DS    XL1                 DOWNLOAD RUN INDICATOR                       
DPAGEIND DS    XL1                 FIRST PAGES DONE - DOWNLOADING               
DHEADIND DS    XL1                 HEADERS DOWNLOADED                           
DTHDRIND DS    XL1             SET TO X'01' TRANSMIT *HDR SENT                  
*                                                                               
RUNSAVEL EQU   *-DDSIND                                                         
*                                                                               
WKBCLT   DS    CL2                                                              
WKBPRD   DS    XL1                                                              
*                                                                               
RELO     DS    F                   FROM TEMPRELO                                
NETOPT   DS    CL1                 N IF NETPAK                                  
CANNOPT  DS    CL1                 C IF CANADIAN NETWORK                        
ELCODE   DS    CL1                                                              
MYBEST   DS    CL1                 LOW EST                                      
MYBESTE  DS    CL1                 HIGH EST                                     
MYA8PROF DS    CL16          USED FOR THE WHOLE REPORT                          
PROFA8A  DS    CL16          A8A PROFILE                                        
PROFB1   DS    CL16          B1 PROFILE                                         
PROFB1X  DS    CL16          B1X SPECIAL INVOICE MTH DISPLAY PROFILE            
*                                                                               
EMSG     DS    CL105                                                            
*                                                                               
SAVMSTA  DS    XL5           SAVED MARKET/STATION                               
*                            FOR READING CLEARANCE STATUS RECORDS               
SAVERE   DS    F                                                                
X        DS    F                                                                
WK       DS    4F                                                               
WK2      DS    XL255                                                            
MULTOFF  DS    CL1           SET TO 'Y' IF DOING OFFICE LIST REQ                
DCPAYSW  DS    CL1                                                              
ICBILLSW DS    CL1                                                              
MOSPSW   DS    CL1                                                              
WPRD     DS    CL1      ONE BYTE WORKING PRODUCT                                
WPRD3    DS    CL3      3 CHARACTER WORKING PRODUCT                             
DOLS     DS    CL12     BALANCE OUT DOWNLOAD FOE QOPT4 =D                       
*                                                                               
*        NEXT 4 FIELDS USED FOR THE STATION DOWNLOAD (QOPT5=S)                  
SBALF    DS    CL13     STATION BALANCE FORWARD                                 
SCBIL    DS    CL13     STATION CURRENT BILLING                                 
SCPAY    DS    CL13     STATION CURRENT CLEARANCES                              
SBALO    DS    CL13     STATION BALANCE OUT                                     
*                                                                               
ESTCNT   DS    H                                                                
MLINCNT  DS    CL1                                                              
LASTMOS  DS    CL2                                                              
LASTCLT  DS    CL3                                                              
SVPCLT   DS    CL3                 SAVED CLT CODE FOR NEW PAGE                  
SAVCOFF  DS    CL2                 SAVED OFFICE CODE FOR $N REQS                
SAVMOL   DS    CL2                 SAVED OFFICE LIST CODE FOR PRINTING          
SAVAOFF  DS    CL2                 SAVED ACC OFFICE                             
SVNMED   DS    CL11                NETPAK SUB MEDIA DESCRIPTION                 
SAVEP    DS    CL132               SAVED PRINT LINE                             
****                                                                            
PBERRSW  DS    CL1                                                              
CBERRSW  DS    CL1                                                              
BACTSW   DS    CL1                 SAVED FROM BN PROFILE+3                      
ESTPSW   DS    CL1                                                              
TOTTYP   DS    CL1                                                              
CLTPSW   DS    CL1                                                              
SORTACT  DS    CL1                                                              
MTHPSW   DS    CL1                                                              
STAPSW   DS    CL1                                                              
ESTTBSW  DS    CL1                                                              
MOSCACT  DS    CL1                 MTH OF SERV CURRENT ACTIVITY SW              
ESTCACT  DS    CL1                 EST CURRENT ACTIVITY                         
MOSACT   DS    CL1                 MTH OF SERV ACTIVITY                         
STAACT   DS    CL1                 STATION ACTIVITY                             
ESTACT   DS    CL1                 ESTIMATE ACTIVITY                            
PRDACT   DS    CL1                 PRODUCTACTIVITY                              
CLTACT   DS    CL1                 CLIENT ACTIVITY                              
OFFACT   DS    CL1                 OFFICE ACTIVITY                              
AGYACT   DS    CL1                 MEDIA ACTIVITY                               
*                                                                               
CMSTART  DS    CL2                 CURRENT MTH START                            
CMEND    DS    CL2                 CURRENT MTH END                              
*                                                                               
CMSTARTB DS    CL3                 CURRENT MTH START - BINARY                   
CMENDB   DS    CL3                 CURRENT MTH END   - BINARY                   
*                                                                               
SVTBPRD  DS    CL3                                                              
SVTBSTA  DS    CL8                                                              
*                 FIELDS BELOW USED FOR STATION DOWNLOAD (QOPT5 = S)            
*                      WITH PROGPROF+10 = Y (MOS,INV,DATE DETAILS)              
INVADR   DS    A       INV ENTRY ADDRESS SAVED IN MOSEND                        
PAYADR   DS    A       PAY ENTRY ADDRESS SAVED IN MOSEND                        
*                                                                               
ANETBLK  DS    A                                                                
AWWIO    DS    A                                                                
ASPA8WW  DS    A                                                                
AFMTINO  DS    A                                                                
VOFFOUT  DS    A                                                                
ANXTPD   DS    A                   NEXT ENTRY IN PAYMENT TABLE                  
ANXTBL   DS    A                   NEXT ENTRY IN INVOICE TABLE                  
APDTAB   DS    A                   PAYMENT TABLE                                
AINVTAB  DS    A                   INVOICE TABLE                                
APRDTAB  DS    A                   ADDR OF PRD TABLE                            
ANXTPRD  DS    A                                                                
ANXTECLT DS    A                                                                
SORTER   DS    A                                                                
VDATVAL  DS    A                                                                
ADSTABUC DS    A                                                                
ASORTC   DS    A                                                                
VBTOTS   DS    A                                                                
VPROCNET DS    A                                                                
VCLTLAST DS    A                                                                
VBLDNET  DS    A                                                                
VNETTAB  DS    A                                                                
VDOWNLD  DS    A                                                                
VDLFLD   DS    A                                                                
VPRINTIT DS    A                                                                
VMAILTAB DS    A                                                                
BRDMON   DS    A                                                                
SAVHEADK DS    A         SAVED ADDRESS OF HEADHOOK                              
*                                                                               
MYKEY    DS    0CL31                                                            
MYKPRD   DS    CL3                                                              
MYKSTA   DS    CL8                                                              
MYKEST1  DS    CL1                                                              
MYKMOS   DS    CL2                                                              
MYKEST2  DS    CL1                                                              
MYKLINE  DS    CL2                 2 BYTE LINE NUMBER - DDS ONLY                
MYKINVMO DS    CL1                                                              
MYKINV   DS    CL2                                                              
MYKPDDTE DS    CL2                 PAYMENT DATE                                 
MYKPDPRD DS    CL3                                                              
MYBILDT  DS    CL2                                                              
MYPCRCK  DS    CL1                                                              
MYPREP   DS    CL3                                                              
*                                  SORT RECORD                                  
         DS    0D                                                               
TBKEY    DS    0CL31                                                            
TBREC    DS    0CL64                                                            
TBKPRD   DS    CL3                                                              
TBKSTA   DS    CL8                                                              
TBKEST1  DS    CL1                                                              
TBKMOS   DS    CL2                                                              
TBKEST2  DS    CL1                                                              
TBKLINE  DS    CL2                                                              
TBKINVMO DS    CL1                                                              
TBKINV   DS    CL2                                                              
TBKPDDTE DS    CL2                 PAYMENT DATE                                 
TBKPDPRD DS    CL3                                                              
TBBILDTE DS    CL2                 BILLED DATE                                  
*                                                                               
*     THE NEXT 2 FIELDS ARE ONLY SET FROM A NET PAY ELEMENT                     
*                                                                               
TBPCRCK  DS    CL1                 PAYMENT CR/CK INDICATOR                      
TBPREP   DS    CL3                 PAYMENT REP                                  
         DS    CL1                 FOR ALIGNMENT                                
TBBILLN  DS    D                   BILLED NET                                   
TBBILLG  DS    D                   BILLED GROSS                                 
TBPAIDN  DS    D                   PAID NET                                     
TBPAIDG  DS    D                   PAID GROSS                                   
*                                                                               
*                                                                               
*                                                                               
*                                  BUFFALO RECORD                               
         DS    0D                                                               
BUFREC   DS    0CL96                                                            
BUFKEY   DS    0CL31                                                            
BUFTYP   DS    CL1                 TYPE X'04' = CLT ,X'06' = REQUEST            
*                                  X'05' = OFFICE (IF QCLT=$)                   
*                                  X'02' = PRD                                  
*                                  X'01'= STA - PASSES RECS TO SORT             
BUFNMED  DS    CL1                 NETWORK SUB=MEDIA                            
         DS    CL2                 PRESENT IN X'02' AND X'04'                   
*                                  IF PROGPROF+9 = C'Y'                         
         ORG   BUFNMED                                                          
BUFCLT   DS    CL3                                                              
BUFMOS   DS    CL2                 MTH OF SERV                                  
*                                  X'FFFF' FOR TOTAL LINE                       
BUFEST   DS    CL1                 ESTIMATE                                     
         DS    CL23                                                             
         DS    CL1                 SPARE - NEEDED FOR ALIGNMENT                 
*                                 (COULD BE USED FOR COMMENT)                   
BUFPBILN DS    D                   PREV BILLED NET                              
BUFPPAYN DS    D                   PREV PAID NET                                
BUFCBILN DS    D                   CURRENT BILLED NET                           
BUFCPAYN DS    D                   CURRENT PAID NET                             
BUFCBILG DS    D                   CURRENT BILLED GROSS                         
BUFCPAYG DS    D                   CURRENT PAID GROSS                           
BUFPBILG DS    D                   PREV BILLED GROSS                            
BUFPPAYG DS    D                                                                
*                                                                               
DTETOTS  DS    0D                  APYMENT DATE TOTALS                          
DCPAYN   DS    D                   CURRENT PAID NET                             
DCPAYG   DS    D                   CURRENT APID GROSS                           
*                                                                               
INVTOTS  DS    0D                  INVOICE TOTALS                               
ICBILLN  DS    D                   CURRENT BILLED NET                           
ICBILLG  DS    D                   CURRENT BILLED GROSS                         
*                                                                               
ESTTOTS  DS    0D                  EST TOTALS                                   
EPBILLN  DS    D                   EST PREV BILLED NET                          
EPPAYN   DS    D                   EST PREV PAID NET                            
ECBILLN  DS    D                   EST CURRENT BILLED NET                       
ECPAYN   DS    D                   EST CURRENT PAID NET                         
ECBILLG  DS    D                   EST CURRENT BILLED GROSS                     
ECPAYG   DS    D                   EST CURRENT PAID GROSS                       
EPBILLG  DS    D                   EST PREV BILLED GROSS                        
EPPAYG   DS    D                                                                
*                                                                               
MOSTOTS  DS    0D                  MTH OF SERV TOTALS                           
MPBILLN  DS    D                   MOS PREV BILLED NET                          
MPPAYN   DS    D                   MOS PREV PAID NET                            
MCBILLN  DS    D                   MOS CURRENT BILLED NET                       
MCPAYN   DS    D                   MOS CURRENT PAID NET                         
MCBILLG  DS    D                   MOS CURRENT BILLED GROSS                     
MCPAYG   DS    D                   MOS CURRENT PAID GROSS                       
MPBILLG  DS    D                   MOS PREV BILLED GROSS                        
MPPAYG   DS    D                                                                
*                                                                               
STATOTS  DS    0D                  STATION TOTALS                               
SPBILLN  DS    D                   STA PREV BILLED NET                          
SPPAYN   DS    D                   STA PREV PAID NET                            
SCBILLN  DS    D                   STA CURRENT BILLED NET                       
SCPAYN   DS    D                   STA CURRENT PAID NET                         
SCBILLG  DS    D                   STA CURRENT BILLED GROSS                     
SCPAYG   DS    D                   STA CURRENT PAID GROSS                       
*                                                                               
PREBILLN DS    D                   CLT PREVIOUS BILLING - INVOICE               
PREBILLG DS    D                                                                
CURBILLN DS    D                   CLT CURRENT BILLING - INVOICE                
CURBILLG DS    D                                                                
*                                                                               
CINVNET  DS    D                                                                
CINVGRS  DS    D                                                                
CINVAMT  DS    D                                                                
CINVGST  DS    D                                                                
CINVPST  DS    D                                                                
*                                                                               
AINVAMT  DS    D                                                                
CMINVAMT DS    D                                                                
*                                                                               
BTOTPST  DS    F                   BILL'S TOTAL PST                             
STOTPST  DS    F                   BUY'S TOTAL PST                              
*                                                                               
CGSTTOTS DS    0CL64               CLIENT GST/PST TOTALS                        
CPBGST   DS    PL8                 CLIENT PREV BILLED GST                       
CPPGST   DS    PL8                 CLIENT PREV PAID GST                         
CCBGST   DS    PL8                 CLIENT CURR BILLED GST                       
CCPGST   DS    PL8                 CLIENT CURR PAID GST                         
CPBPST   DS    PL8                 CLIENT PREV BILLED PST                       
CPPPST   DS    PL8                 CLIENT PREV PAID PST                         
CCBPST   DS    PL8                 CLIENT CURR BILLED PST                       
CCPPST   DS    PL8                 CLIENT CURR PAID PST                         
*                                                                               
OGSTTOTS DS    0CL64               OFFICE GST/PST TOTALS                        
OPBGST   DS    PL8                 OFFICE PREV BILLED GST                       
OPPGST   DS    PL8                 OFFICE PREV PAID GST                         
OCBGST   DS    PL8                 OFFICE CURR BILLED GST                       
OCPGST   DS    PL8                 OFFICE CURR PAID GST                         
OPBPST   DS    PL8                 OFFICE PREV BILLED PST                       
OPPPST   DS    PL8                 OFFICE PREV PAID PST                         
OCBPST   DS    PL8                 OFFICE CURR BILLED PST                       
OCPPST   DS    PL8                 OFFICE CURR PAID PST                         
*                                                                               
RGSTTOTS DS    0CL64               REPORT GST/PST TOTALS                        
RPBGST   DS    PL8                 REPORT PREV BILLED GST                       
RPPGST   DS    PL8                 REPORT PREV PAID GST                         
RCBGST   DS    PL8                 REPORT CURR BILLED GST                       
RCPGST   DS    PL8                 REPORT CURR PAID GST                         
RPBPST   DS    PL8                 REPORT PREV BILLED PST                       
RPPPST   DS    PL8                 REPORT PREV PAID PST                         
RCBPST   DS    PL8                 REPORT CURR BILLED PST                       
RCPPST   DS    PL8                 REPORT CURR PAID PST                         
*                                                                               
         DS    0F                                                               
XCHDATA  DS    0XL200              188 USED NAOW                                
*          DATA SET SPXCHAREA  AT LEVEL 008 AS OF 12/22/93                      
* DSECT FOR SPOT/GETRATE CALLS TO RETURN                                        
*                                                                               
*  NOTE TAGS CHANGED TO AVOID DUPLICATES IN SPREPWORKD                          
*                                                                               
SCHAREA  DS    0F                                                               
SGROSS   DS    F                                                                
SNET     DS    F                                                                
STAX     DS    F                                                                
SC58     DS    F                                                                
SGSTCODE DS    C                                                                
SGSTRATE DS    XL3                 GST RATE TO 5 DECS (7 PCT = 07000)           
SGSTAMT  DS    F                   GST AMOUNT IN PENNIES (CAN DOLLARS)          
SGSTAMTX DS    F                   GST AMOUNT IN PENNIES (US DOLLARS)           
XPSTTAB  DS    10CL16                                                           
XPSTTABX EQU   *                                                                
         ORG   XPSTTAB                                                          
XPSTPROV DS    CL2                 PROVINCE CODE                                
XPSTCODE DS    CL1                 PST CODE                                     
         DS    XL2                 N/D                                          
XPSTRATE DS    XL3                 PST RATE TO 5 DEC                            
XPSTAMT  DS    XL4                 PST AMOUNT IN PENNIES (CAN DOLLARS)          
XPSTAMTX DS    XL4                 PST AMOUNT IN PENNIES (US DOLLARS)           
XPSTLEN  EQU   *-XPSTPROV                                                       
         ORG                                                                    
XCHAREAL EQU   *-SCHAREA                                                        
         DS    F                   SPARE                                        
         DS    F                   SPARE                                        
*                                                                               
SVOPT31  DS    CL1                 DEC31/90 OPTION                              
*                                  FROM PROFILE?                                
PRDELSW  DS    CL1                                                              
*                                                                               
SHCUMEPS DS    F                   TOTAL %                                      
SHCUMEAS DS    2PL8                TOTAL $   GROSS/NET                          
*                                                                               
MYPACKED DS    PL8                                                              
*                                                                               
PKBIG    DS    0PL16               FOR DIVIDING PACKED                          
PKQUOT   DS    PL8                 QUOTIENT                                     
PKRMDR   DS    PL8                 REMAINDER                                    
*                                                                               
*                                                                               
WWBLK    DS    CL60                WUNDERMAN INTERFACE BLOCK                    
*                                  COVER WITH SPA8WWBLK                         
*                                  DSECT= SPWWD                                 
*                                                                               
*                                                                               
CLTBESW  DS    CL1                                                              
CINVSW   DS    CL1                                                              
AINVSW   DS    CL1                                                              
CMINVSW  DS    CL1                                                              
*                                                                               
       ++INCLUDE SPBVALD                                                        
*                                                                               
ERRCLTS  DS    CL240                                                            
*                                                                               
*        SPECIAL PRODUCT TOTAL ACCUMULATORS                                     
*        PRINTED AT PRDEND WHEN DOING PRD SUMMARY                               
*                                                                               
PRDBALI  DS    PL8                                                              
PRDBILLN DS    PL8                                                              
PRDPAYN  DS    PL8                                                              
PRDBILLG DS    PL8                                                              
PRDPAYG  DS    PL8                                                              
PRDBALO  DS    PL8                                                              
*                                                                               
         EJECT                                                                  
**********************************************************************          
* MAIL ID TABLE FOR AUTOMATIC NOTIFICATION                           *          
**********************************************************************          
MAILTAB  CSECT                 EMAIL TABLE                                      
*                                                                               
         DS    0CL(MAILNQ)                                                      
         DC    AL2(6487)                 ORIGIN ID                              
         DC    CL8'DTSEC'               CONNECT ID                              
         DC    CL45'KFAU,DAVID CHAMBERS/USA/DDS@DDS'   COMMA SEPARATED          
*                                                                               
         DC    AL2(2462)                                                        
         DC    CL8'YNRR'                                                        
         DC    CL45'AARE,KELLY TAYLOR/USA/DDS@DDS'                              
*                                                                               
         DC    AL2(8151)                                                        
         DC    CL8'YNKLWE'                                                      
         DC    CL45'AARE,KELLY TAYLOR/USA/DDS@DDS'                              
*                                                                               
         DC    AL2(6029)                                                        
         DC    CL8'YNME'                                                        
         DC    CL45'AARE,KELLY TAYLOR/USA/DDS@DDS'                              
*                                                                               
         DC    AL1(EOF)                                                         
         EJECT                                                                  
TABLES   CSECT                                                                  
         DS    0D                                                               
         DC    CL8'**PDTAB*'                                                    
PDTAB    DS    25505C             1000 PAYMENTS X 25 CHARS EACH +5              
*                                                                               
*        FORMAT FOR PDTAB IS PAY DATE/PRD/AMTS/CRCK/REP                         
*                                                                               
         DS    0D                                                               
         DC    CL8'*INVTAB*'                                                    
INVTAB   DS    31505C             1500 INVOICES X 21 CHARS EACH +5              
*                                                                               
         DS    0D                                                               
         DC    CL8'*PRDTAB*'                                                    
PRDTAB   DS    11503C              500 PRDS X 23 CHAR EACH +3                   
*                                                                               
         DS    0D                                                               
         DC    CL8'*NETTAB*'                                                    
NETTAB   DS    CL(MAXNETS*NETTABL)                                              
*                                                                               
         DS    0F                                                               
         SPACE 2                                                                
******** BUFF  LINES=1500,ROWS=1,COLUMNS=8,FLAVOR=PACKED,KEYLIST=(28,A)         
         BUFF  LINES=2000,ROWS=1,COLUMNS=8,COMMENT=1,FLAVOR=PACKED,    X        
               KEYLIST=(31,A)                                                   
STABUCKC CSECT                                                                  
         DS    CL3000                                                           
         DC    X'00'                                                            
*                                                                               
VIRTLREC DS    0D                                                               
         DS    20000C                                                           
*                                                                               
SORTCS   CSECT                                                                  
         DS    71500C                                                           
*                                                                               
NETBLK   CSECT                                                                  
         DS    1200C                                                            
         EJECT                                                                  
**********************************************************************          
* MAIL TABLE DSECT                                                   *          
**********************************************************************          
         SPACE 1                                                                
MAILTBD  DSECT                                                                  
MAILAGY  DS    XL2      AGENCY ORIGIN ID                                        
MAILUID  DS    CL8      CONNECT ID                                              
MAILADD  DS    CL45     E-MAIL ADDRESSES COMMA SEPARATED LIST                   
MAILNQ   EQU   *-MAILTBD           LENGTH                                       
         SPACE 3                                                                
NETTABD  DSECT                     DSECT FOR CANADIAN NETWORK TABLE             
NTBCALL  DS    CL4                                                              
NTBPCKD  DS    XL3                                                              
         DS    CL1                 SPARE                                        
NETTABL  EQU   *-NETTABD                                                        
MAXNETS  EQU   65                 (64 PLUS 1)                                   
         SPACE 3                                                                
*                                                                               
       ++INCLUDE SPGENNDEF                                                      
*                                                                               
NETBLKD  DSECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETBLOCKN                                                      
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDREPMASTD                                                     
*                                                                               
*   WUNDERMAN BLOCK DSECT                                                       
       ++INCLUDE SPA8WWBLK                                                      
*                                                                               
WWIO     CSECT                                                                  
         DS    2000C             WW I/O AREA                                    
         DS    0H                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NEGENUNIT                                                      
         EJECT                                                                  
       ++INCLUDE NETBILLRD                                                      
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
       ++INCLUDE SPGENCLRST                                                     
*                                                                               
       ++INCLUDE SPGENBILL                                                      
** OLD BILLING FIELDS                                                           
BCANINV  EQU   BRETACCT                                                         
BCANDT   EQU   BRETACCT+6                                                       
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
       ++INCLUDE DDOFFICED                                                      
QOPT6    EQU   QGRP                                                             
QOPT7    EQU   QGRP+1                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'165SPREPA802 09/27/19'                                      
         END                                                                    
