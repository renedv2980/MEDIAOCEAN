*          DATA SET PPREPA802  AT LEVEL 066 AS OF 07/18/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE PPA802A                                                                  
*INCLUDE PUBFLOAT                                                               
*INCLUDE PPBVAL                                                                 
*INCLUDE PPFMTINO                                                               
*INCLUDE DLFLD                                                                  
*INCLUDE LOGIO                                                                  
         TITLE 'PPA802 - NEW PRINTPAK BILLING CLEARANCE RPT'                    
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* SMUR SPEC-34559   05/08/19 ADD ACC OFFICE TO DOWNLOAD (19.3)                  
* AWIL SPEC-18      03/01/17 TWO CHARACTER OFFICE LISTS                         
* AKAT CUSTENH-3347 05/20/16 HST INCREASING TO 15% FOR NB 07/01/16    *         
* AKAT SPSUG-20     05/20/16 HST INCREASING TO 15% FOR NF 07/01/16    *         
* AKAT SPSUG-42     05/20/16 HST INCREASING TO 15% FOR PE 10/01/16    *         
*                                                                               
*  BPLA 09/13   FIX DISPLAY AT END OF CLIENT+REPORT                             
*               FOR VALUES OVER $1 BILLION  - USES EDITOR                       
*               SHOULD NOW HANDLE TOTALS UP TO $100 BILLION                     
*                                                                               
*  BPLA 08/12   NEW VALUE FOR PROGPROF+15  B= BASE PUB # WITH VENDOR            
*               NAME. LIKE OPTION V, SHOULD BE USED WITH QOPT6                  
*               SET TO "5", MAY WORK WITH 7.                                    
*                                                                               
*  BPLA 01/12   CANADIAN QST % CHANGE  8.5% TO 9.5%                             
*               NEW PGSTTAB                                                     
*                                                                               
*  BPLA 12/10   CANADIAN QST % CHANGE                                           
*                                                                               
*  BPLA 9/09    SAVE AND USE FIRST A8 PROFILE DATA                              
*                                                                               
*  BPLA 8/08    ADD OPTION TO TRANSMIT A DOWNLOAD A8                            
*               (QOTP1-2 = T) QOPT5 SHOULD BE P                                 
*               CHANGE NOTIFICATIONS (YKVA INSTEAD OF DEIS)                     
*                                                                               
*  BPLA 5/08    NEW VALUE FOR PROGPROF+15 -'C'=DOWNLOAD CLT NAME                
*                                                                               
*  BPLA 1/08    GST/HST CHANGE - NEW PGSTTAB                                    
*                                                                               
*  BPLA 9/06    NEW OPTION FOR DOWNLOADING TO SHOW END DAY                      
*               WITH MOS - D IN PROGPROF+13                                     
*               PRDTAB EXPANDED TO 1500 PRDS                                    
*               LINES= (IN BUFFBUFF) EXPANED TO 2000                            
*                                                                               
*  BPLA 6/06    GST % CHANGE                                                    
*                                                                               
*  BPLA 11/05   CHANGES FOR 2 CHARACTER OFFICES                                 
*                                                                               
*    BPLA   05/04  DOWNLOAD PUB NAMES PROGPROF+15 = V                           
*                  MUST BE USED WITH QOPT6-USUALLY SET TO "5"                   
*                  (MAY HANDLE MARKET IF SET TO "7"?)                           
*                                                                               
*    BPLA   12/03  DOWNLOAD HEADERS CHANGES                                     
*                                                                               
*    BPLA   05/03  SUPPRESS REQ DETAILS- NOREQUEST CONTROL CARD                 
*                                                                               
*    BPLA   11/02  E-MAIL NOTIFICATIONS                                         
*           12/02  DOWNLOAD MODIFICATIONS AND YEAR REPORTING                    
*                  CHANGES                                                      
*                  NOTE - QCONTREQ USED TO CHECK FOR PRODUCT                    
*                         GROUP REQUEST - WORKS FOR NOW                         
*                         AS THAT'S THE ONLY DATA THAT COULD                    
*                         BE THERE                                              
*                                                                               
*    BPLA   10/02  PUBLICATION DOWNLOAD CHANGES                                 
*                  QOPT5 P= PRODUCE PUBLICATION DOWNLOAD                        
*                  NEW REPORT PROFIOLE OPTIONS: (FOR DOWNLOADING)               
*                  PROGPROF+9 - SHOW MOS INV# AND DATES                         
*                  (DOWNLOAD MORE THAN JUST PUB TOTAL LINES)                    
*                  PROGPROF+10  Y=2 DECIMALS IN $ FIELDS                        
*                                                                               
*    BPLA   09/02  OFFICE TOTALS ON THE PQ                                      
*                                                                               
*    BPLA   07/02  NO CONSOLE MESSAGE IF REQUESTED BY DESI OR BPLA              
*                  CLIENT AND MEDIA NAMES OR'ED WITH SPACES                     
*                  (TONY PEREZ REQUESTED)                                       
*                                                                               
*    BPLA   10/01  CHANGE TO RUN FOR A 'CURRENT' YEAR                           
*                                                                               
*    BPLA   10/00  CHANGE FOR SUMMARY PAGES ON SEPARATE OUTPUT                  
*                                                                               
*    KWAN   08/00  NEW PBILLREC AND PPBVAL DSECT AND PNBVAL FOR PPBVAL          
*                                                                               
*    BPLA   07/00  SOME MAXLINES COMPARES FIXED                                 
*                                                                               
*    BPLA   06/00  REMOVE CONSOLE MESSAGE LOGIC FOR END OF REPORT               
*                  (REDUNDANT SINCE CLIENT MESSAGE IS ENOUGH)                   
*                                                                               
*    BPLA   05/00  CHANGES FOR PRD SUMMARY VERSION                              
*                  NEW VALUES FOR QOPT4                                         
*                  B= SUPPRESS BOTH PUB AND MOS                                 
*                     (PRD SUMMARY VERSION)                                     
*                  D= DOWNLOAD PRD SUMMARY                                      
*                                                                               
*   BPLA   04/00  CHANGES FOR CONSOLE MESSAGE                                   
*                                                                               
*   BPLA   9/99   CHANGES FOR Y2K QREGION (AS OF DATE) AND                      
*                 QPROG+52 (CURRENT MONTH)                                      
*                                                                               
*   BPLA   1/99   NEW VALUE FOR QOPT4 - "B" SUPPRESS BOTH PUBS AND              
*                 MONTH OF SERVICE                                              
*                 CLT AND PRD PUT INTO PUB                                      
*             ** NOTE - TO WORK PROPERLY SHOULD BE RUN WITH                     
*                       PRODUCTS SEPARATELY (UNLESS THEY WANT ZZZ)              
*                 PDTAB AND INVTAB EXPANDED TO 600 PAYMENTS AND                 
*                 INVOICES                                                      
*                                                                               
*   BPLA   6/97   FIX PST CALCULATION IN GETGST                                 
*                 PROBLEM WAS WRKNET NOT SET IF GST NOT APPLIED                 
*                 BUT GETPST NEEDED IT.                                         
*                                                                               
*   BPLA   5/97   OFFOUT CALLS NO-OPED                                          
*                 OLD HEADLINE ROUTINE RESTORED                                 
*                                                                               
*  SMYE   10/96   USE OFFOUT TO SHOW OFFICE (PRINTIT AND TBOFFF)                
*                                                                               
*   BPLA   1/96   NEW A8 PROFILE OPTION TO SHOW CLIENTS INSTEAD                 
*                 OF OFFICES ON OFFICE LIST TOTALS                              
*                 (PROGPROF+8)                                                  
*                                                                               
*  SMYE 12/12/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                      
*                                                                               
*  BPLA  7/94     USE PPFMTINO FOR FORMATTING INVOICE NUMBERS                   
*                 NEEDS B1 AND B1X PROFILES - READ FOR EACH CLIENT              
*                                                                               
*  BPLA 5/94      PST CHANGES                                                   
*                                                                               
*  BPLA 10/29/92  DISPLAY SEP COMM BILLS AS UFC INSTEAD OF COM                  
*                                                                               
*  BPLA 9/25/92   USE PPBVAL FOR "EFFECTIVE" VALUES ON                          
*                 UFC BILLS AND BILLING ELEMENTS                                
*                                                                               
*  BPLA 1/23/91   ADD GST BILLED AND PAID ACCUMULATORS                          
*                 AND TO LIST OF PREVIOUS INVOICES                              
*                                                                               
         EJECT                                                                  
*        REQUEST OPTIONS                                                        
*                                                                               
*        QOPT1-2    T= TRANSMITTING (WHEN DOWNLOADING - OPT7=P)                 
*                                                                               
*                   CLASS MUST BE G                                             
*                                                                               
*          **NOTE** THIS SHOULD BE SET IN THE FIRST REQUEST OF THE              
*                   MONTH-END A8'S FOR THE JOB STREAM AND THE JCL               
*                   SHOULD BE SET FOR NO REQUEST DETAILS                        
*                   (OR THEY WILL APPEAR BEFORE THE *HDR* EDICT LINES)          
*                   AND NO CONTROL CARDS - NOCONTROL SHOULD                     
*                   PRECEDE EVEN THE PP0A8 LINE                                 
*                   NOREQUEST CAN BE LATER IN THE JCL                           
*                                                                               
*                   AN EDICT RECORD SHOULD BE SET UP FOR THE AGENCY             
*                   ITS KEY SHOULD BE A8DXX (WHERE XX IS THE AGENCY)            
*                                                                               
**NEW 1/25/89                                                                   
**       QOPT1-1    D= SHOW INSERTION DAY WITH MONTH                            
**                 MTHSW (PROGPROF+7) MUST BE C'I'                              
**NEW 1/25/89                                                                   
*                                                                               
*        QOPT1      1=SORT ON EST - NO EST TOTALS IN PRD SUMMARY                
*                   2=SAME AS 1 WITH EST TOTALS IN PRD SUMMARY                  
*                   3=BUYLINE DETAIL - DDS ONLY                                 
*                                                                               
*        QOPT2      Y=COMBINE ALL PRDS TOGETHER                                 
*                                                                               
*        QOPT3      Y=SHOW ALL PUBS - DEFAULT IS ONLY PUBS                      
*                      WITH A BALANCE FORWARD OR CURRENT ACTIVITY               
*                                                                               
*        QOPT4      M=NO MTH BREAKOUT                                           
*                   S= NO PUB BREAKOUT                                          
*                   B=NO PUB NOR MTH OF SERVICE BREAKOUT                        
*                    -ONE LINE PRD TOTALS                                       
*                   D = DOWNLOAD VERSION LIKE B                                 
*                                                                               
*                   NOTE:  WHEN USING VALUES B AND D                            
*                          QOPT2 SHOULD BE SET TO N                             
*                                                                               
*        QOPT5      Y=LIST CURRENT CLIENT INVOICES                              
*                   P=PUBLICATION DOWNLOAD                                      
*************                                                                   
*              ABOVE OPTIONS, IF LEFT BLANK, WILL                               
*              DEFAULT TO PROFILE  - PROGPROF                                   
*                                                                               
*              PROGPROF+5 Y=TRIPLE SPACE CLIENTS                                
*              PROGPROF+6 Y=SUBTRACT CD                                         
*              PROGPROF+7 MONTH SHOWN I=INSERTION                               
*                                     B=BILLABLE (MOS) DEFAULT                  
*                                                                               
*************                                                                   
*        QOPT6      5=MAGS IN ALPHA ORDER                                       
*                   7=NEWS AND OUTDOOR IN MARKET ORDER                          
*               BLANK=PUB NUMBER ORDER                                          
*                                                                               
*        QOPT7     Y=TRACE PROGRAM SORT CALLS                                   
*                  N=OVERRIDE PROGPROF TO OFFICES IN OFFICE                     
*                    LIST TOTALS                                                
*                  C=OVERRIDE PROGPROF TO CLIENTS IN OFFICE                     
*                    LIST TOTALS                                                
*                                                                               
*        QREGION   OPT - ACTS AS AN 'AS OF DATE'                                
*                                                                               
*        QPROG+52  OPTIONAL - CURRENT MONTH                                     
*                  MAY BE NO OR NONE                                            
*                  MAY CONTAIN NOYY  (YY IS YEAR) FOR RUNNING                   
*                  A YEAR AS THE "CURRENT MONTH".                               
*                  IF AN 'AS OF' DATE IS ENTERED, ITS MONTH                     
*                  WILL BE USED AS THE END MONTH - I.E.                         
*                  REPORT WILL BE FOR JAN-END MONTH OF THE GIVEN YEAR           
*                                                                               
************                                                                    
************  NOTE - FOR THE SUMMARY VERSIONS (QOPT4 B OR D)                    
************         QOPT2 SHOULD BE SET TO Y AND QOPT1 TO N                    
*                                                                               
         PRINT NOGEN                                                            
PPA802   CSECT                                                                  
         NMOD1 0,PPA802,R8                                                      
*                                                                               
         L     RC,0(R1)                                                         
         USING PPWORKD,RC                                                       
         L     RA,PPFILEC                                                       
         USING PPFILED,RA                                                       
         LA    R9,SPACEND                                                       
         USING PPA8WRKD,R9                                                      
         CLI   MODE,FBUYREQ                                                     
         BE    INITIAL                                                          
***OFF                                                                          
         CLI   MODE,OFCFRST        FIRST FOR OFFICE                             
         BE    TBOFFF                                                           
***OFF                                                                          
         CLI   MODE,FBUYCLI                                                     
         BE    TBCLTF                                                           
         CLI   MODE,FBUYPRO                                                     
         BE    TBPRDF                                                           
         CLI   MODE,PROCBUY                                                     
         BE    TBBUY                                                            
         CLI   MODE,PROCBIL                                                     
         BE    DOBILL                                                           
         CLI   MODE,FBUYPUB                                                     
         BE    FBPUB                                                            
         CLI   MODE,LBUYPUB                                                     
         BE    LBPUB                                                            
         CLI   MODE,LBUYREQ                                                     
         BE    TBAGY                                                            
***OFF                                                                          
         CLI   MODE,OFCLAST            LAST FOR OFFICE                          
         BE    TBOFF                                                            
***OFF                                                                          
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RFIRST                                                           
         CLI   MODE,RUNLAST                                                     
         BE    RLAST                                                            
*                                                                               
         CLI   MODE,LBUYCLI                                                     
         BNE   EXIT                                                             
*                                                                               
         LA    R5,CGSTTOTS            ROLL CLIENT GST TOTALS                    
         LA    R6,OGSTTOTS            TO OFFICE                                 
         BAS   RE,ROLLGST                                                       
*                                                                               
         LA    R5,CGSTTOTS            ROLL CLIENT GST TOTALS                    
         LA    R6,RGSTTOTS            TO REPORT                                 
         BAS   RE,ROLLGST                                                       
*                                                                               
         LA    R5,CPSTTOTS            ROLL CLIENT PST TOTALS                    
         LA    R6,OPSTTOTS            TO OFFICE                                 
         BAS   RE,ROLLGST                                                       
*                                                                               
         LA    R5,CPSTTOTS            ROLL CLIENT PST TOTALS                    
         LA    R6,RPSTTOTS            TO REPORT                                 
         BAS   RE,ROLLGST                                                       
*                                                                               
CKMOD10  GOTO1 VCLTLAST,DMCB,(RC)                                               
         CLI   MODE,LBUYREQ                                                     
         BE    TBAGY                                                            
EXIT     XIT1                                                                   
*                                                                               
RFIRST   DS    0H                                                               
         MVI   DDSIND,0                                                         
         MVI   DOWNIND,0                                                        
         MVI   DPAGEIND,0                                                       
         MVI   DHEADIND,0                                                       
         MVI   DTHDRIND,0                                                       
*                                                                               
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         L     R1,VMASTC           ADDRESS OF MASTER CONTROLS                   
         USING MASTD,R1                                                         
         TM    MCPRTIND,MCPRTIRQ   SEE IF SUPPRESS REQ DETAILS                  
         BNO   *+8                                                              
         MVI   RCREQREP,C'N'                                                    
         DROP  R1                                                               
         DROP  RF                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
RLAST    DS    0H                                                               
         CLI   DOWNIND,X'01'       DOWNLOAD RUN?                                
         BNE   RLAST5                                                           
         CLI   RCREQREP,C'N'       REQ DETAILS SUPPRESSED?                      
         BNE   RLAST5                                                           
*                                  AT RUNLAST                                   
*                                  I MUST CLOSE THE DOWNLOADED REPORT           
         GOTO1 VDOWNLD,DMCB,(RC)                                                
*                                                                               
RLAST5   CLI   DDSIND,1                                                         
         BNE   EXIT                                                             
         BAS   RE,SENDMAIL         DO E-MAIL NOTIFICATIONS                      
         B     EXIT                                                             
*                                                                               
ZAPGST   LA    R1,4                                                             
ZAPG5    ZAP   0(8,R6),=P'0'                                                    
         LA    R6,8(R6)                                                         
         BCT   R1,ZAPG5                                                         
         BR    RE                                                               
*                                                                               
ROLLGST  LA    R1,4                                                             
ROLLG5   AP    0(8,R6),0(8,R5)                                                  
         LA    R6,8(R6)                                                         
         LA    R5,8(R5)                                                         
         BCT   R1,ROLLG5                                                        
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
INITIAL  DS    0H                                                               
         XC    MYA8PROF,MYA8PROF                                                
         CLI   QOPT5,C'P'      SEE IF DOWNLOADING                               
         BNE   INIT6                                                            
*                                                                               
         CLI   QOPT1-2,C'T'        AND TRANSMITTING?                            
         BNE   INIT4                                                            
*                                                                               
*  NOTE WHEN TRANSMITTING JCL SHOULD BE SET FOR NO REQUEST DETAILS              
*                                                                               
         CLI   DTHDRIND,1          HAVE I ALREADY SEND A *HDR?                  
         BE    INIT5X                                                           
         MVC   P+4(5),=C'*HDR*'                                                 
         MVC   P+9(6),=C'EDICT='                                                
*                                                                               
         LA    R1,AGYTTAB            TABLE OF AGENCY/IDS TO TRANSMIT            
INIT2    CLI   0(R1),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                MUST BE IN MY TABLE                          
         CLC   0(2,R1),QAGENCY                                                  
         BE    INIT2B                                                           
         LA    R1,16(R1)                                                        
         B     INIT2                                                            
*                                                                               
INIT2B   MVC   P+15(6),2(R1)        SET EDICT RECORD CODE TO USER ID            
*                                                                               
         MVI   P+34,C'W'                                                        
         MVC   PSECOND(5),=C'++DDS'                                             
         MVC   PSECOND+6(2),=C'PR'     SYSTEM                                   
         MVC   PSECOND+8(3),=C'W8D'    REPORT                                   
         MVC   PSECOND+15(3),=C'DA8'                                            
*                                                                               
*        EDICT REPORT TYPE IS THUS P+W = PW    (FTP'S MUST BE W)                
*                                                                               
         MVC   PSECOND+11(3),=C'TRN'                                            
         MVI   RCWHATPR,1          DEFAULT TO FIRST                             
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,100        NO HEADLINES - 100 DOESN'T EXIST             
         GOTO1 REPORT              SEND LINE                                    
*                                                                               
         MVC   P(5),=C'++DDS'                                                   
         MVC   P+11(3),=C'FTP'                                                  
         MVC   P+15(4),=C'NONE'                                                 
         CLC   QPROG+52(2),=C'NO'      CURRENT MONT GIVEN?                      
         BE    INIT2C                                                           
         MVC   WORK(4),QPROG+52                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(9,WORK+10)                                 
         MVC   P+15(3),WORK+10               MMMYY                              
         MVC   P+18(2),WORK+14                                                  
         MVC   P+20(3),=C'PRT'     (FOR PRINT)                                  
*                                                                               
INIT2C   DS    0H                                                               
         MVI   DTHDRIND,1          SO I WON'T RE-SEND                           
         MVI   RCWHATPR,1          DEFAULT TO FIRST                             
         MVI   RCSUBPRG,100        NO HEADLINES - 100 DOESN'T EXIST             
         GOTO1 REPORT              SEND LINE                                    
         B     INIT5X                                                           
*                                                                               
INIT4    DS    0H                                                               
         MVI   RCWHATPR,1          DEFAULT TO FIRST                             
*                                                                               
         CLI   DPAGEIND,X'01'      HAVE I ALREADY SENT ONE?                     
         BE    INIT6               DON'T SEND ANOTHER                           
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,100        NO HEADLINES - 100 DOESN'T EXIST             
         GOTO1 REPORT              SEND EMPTY LINE                              
         CLI   RCREQREP,C'N'     SEE IF SUPPRESSING DETAILS OF REQUEST          
         BNE   INIT5               FOR ALL REQS                                 
*                                                                               
         MVI   FORCEHED,C'Y'       I NEED TO SEND 2 PAGES (FOR PIANO)           
*                                  WHEN DOWNLOADING                             
         MVI   RCSUBPRG,100        NO HEADLINES - 100 DOESN'T EXIST             
         GOTO1 REPORT              SEND ANOTHER EMPTY LINE                      
*                                                                               
INIT5    MVI   DPAGEIND,X'01'      SO I WON'T REDO FOR THE NEXT REQ             
INIT5X   MVI   RCREQREP,C'N'       ALSO - SET FOR NO REQ DETAILS                
*                                  FOR SUBSEQUENT REQS (THEY SHOULD             
*                                  ALSO BE DOWNLOADS)                           
*                                                                               
INIT6    LA    R2,SPACEND+RUNSAVEL                                              
         LA    R3,6                                                             
INIT7    XC    0(250,R2),0(R2)                                                  
         LA    R2,250(R2)                                                       
         BCT   R3,INIT7                                                         
*                                                                               
         MVC   MYTODAY(2),RCDATE+6                                              
         MVC   MYTODAY+2(2),RCDATE+0                                            
         MVC   MYTODAY+4(2),RCDATE+3                                            
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVC   PAGE,=H'1'                                                       
***OFF                                                                          
         MVI   MULTOFF,C'N'                                                     
***OFF                                                                          
         MVI   OPTSETSW,C'N'                                                    
*                                                                               
         LA    R6,RGSTTOTS         CLEAR REPORT GST TOTALS                      
         BAS   RE,ZAPGST                                                        
         LA    R6,OGSTTOTS         CLEAR OFFICE GST TOTALS                      
         BAS   RE,ZAPGST                                                        
*                                                                               
         LA    R6,RPSTTOTS         CLEAR REPORT PST TOTALS                      
         BAS   RE,ZAPGST                                                        
         LA    R6,OPSTTOTS         CLEAR OFFICE PST TOTALS                      
         BAS   RE,ZAPGST                                                        
*                                                                               
*                                                                               
         CLC   QCLIENT,=C'ALL'      ONLY SET NOW FOR CLT=ALL REQS               
         BNE   *+8                                                              
         BAS   RE,SETOPTS                                                       
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
*                                                                               
         LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
         L     R0,=A(BUFFALOC)                                                  
         ST    R0,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         XC    MOSCACT(8),MOSCACT                                               
         L     RF,=A(DOWNLD)                                                    
         ST    RF,VDOWNLD                                                       
         L     RF,=A(PRINTIT)                                                   
         ST    RF,VPRINTIT                                                      
         L     RF,=V(DLFLD)                                                     
         ST    RF,VDLFLD                                                        
         L     RF,=A(TBCLTL)                                                    
         ST    RF,VCLTLAST                                                      
         L     RF,=A(BTOTALS)                                                   
         ST    RF,VBTOTS                                                        
         L     RF,=V(PUBFLOAT)                                                  
         ST    RF,VPUBFLT                                                       
         L     RF,=V(PPBVAL)                                                    
         ST    RF,VPPBVAL                                                       
         L     RF,=V(PPFMTINO)                                                  
         ST    RF,AFMTINO                                                       
         L     RF,=V(LOGIO)                                                     
         ST    RF,VLOGIO                                                        
         L     RF,VCOMFACS                                                      
         MVC   VDATVAL,CDATVAL-COMFACSD(RF)                                     
         LARL  RF,PDTAB                                                         
         MVC   0(8,RF),=8X'FF'                                                  
         ST    RF,ANXTPD                                                        
         LARL  RF,INVTAB                                                        
         MVC   0(5,RF),=5X'FF'                                                  
         ST    RF,ANXTBL                                                        
         L     RF,=A(POSTBILL)                                                  
         ST    RF,APOSTB                                                        
         L     RF,=A(PUBEND)                                                    
         ST    RF,APUBEND                                                       
*                                                                               
         GOTO1 LOADER,DMCB,(0,=C'T00A38  '),0,0                                 
         MVC   VOFFICER,4(R1)      A(OFFICER)                                   
         GOTO1 (RF),DMCB,(0,=C'T00A71  '),0,0                                   
         MVC   VEDITOR,4(R1)       A(EDITOR)                                    
*                                                                               
         MVI   PBERRSW,0                                                        
         MVI   CBERRSW,0                                                        
         MVI   ESTTBSW,0                                                        
         XC    ERRCLTS(250),ERRCLTS                                             
         XC    ERRCLTS+250(255),ERRCLTS+250                                     
         LA    RF,ERRCLTS                                                       
         ST    RF,ANXTECLT                                                      
         MVC   CMSTART(6),=X'000000FFFFFF'                                      
         CLC   QPROG+52(4),SPACES                                               
         BE    INIT20              NO CURRENT MTH                               
         CLC   QPROG+52(2),=C'NO'                                               
         BNE   INIT15                                                           
*                                                                               
         CLI   QPROG+54,C'0'     CHECK FOR NOYY  (YY IS YEAR)                   
         BL    INIT14                                                           
         MVC   WORK(2),QPROG+54     YEAR                                        
         MVC   WORK+2(2),=C'01'     JANUARY                                     
         MVC   WORK+8(2),=C'12'     THRU DECEMBER                               
         CLI   QREGION,C' '         SEE IF 'AS OF' DATE GIVEN                   
         BE    *+10                                                             
         MVC   WORK+8(2),QREGION+2  USE ITS MONTH INSTEAD                       
         MVC   WORK+6(2),QPROG+54   YEAR                                        
         B     INIT15B                                                          
*                                                                               
INIT14   MVC   QPROG+52(4),SPACES                                               
         B     INIT20                                                           
*                                                                               
INIT15   DS    0H                                                               
         MVC   WORK(4),QPROG+52                                                 
         MVC   WORK+6(4),WORK                                                   
INIT15B  MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK+10(2),=C'31'                                                
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(3,CMSTART)                                 
         GOTO1 DATCON,(R1),(0,WORK+6),(3,CMEND)                                 
**Y2K**                                                                         
*        CONVERT BACK TO CHARACTER                                              
         GOTO1 DATCON,DMCB,(3,CMSTART),(0,WORK)                                 
         MVC   QPROG+52(4),WORK                                                 
         CLC   WORK+2(2),WORK+8   WILL BE DIFFERENT FOR YEAR REQS               
         BE    *+10                                                             
         MVC   QPROG+54(2),SPACES     RESET TO SPACES                           
**Y2K**                                                                         
*                                                                               
INIT20   MVC   BQSTART(6),=X'000000FFFFFF'    SET TO PASS ALL BUYS              
         MVC   ASODTP,=3X'FF'                                                   
         CLI   QREGION,C' '        SEE IF AS OF DATE SPECIFIED                  
         BE    INIT30              NO                                           
         GOTO1 DATCON,DMCB,(0,QREGION),(3,ASODTP)                               
**Y2K**                                                                         
*        CONVERT BACK TO CHARACTER                                              
         GOTO1 DATCON,DMCB,(3,ASODTP),(0,WORK)                                  
         MVC   QREGION(6),WORK                                                  
**Y2K**                                                                         
*                                                                               
INIT30   CLI   QOPT4,C'D'             SPECIAL DOWNLOAD VERSION                  
         BE    INIT71                                                           
         CLI   QOPT5,C'P'             PUBLICATION DOWNLOAD                      
         BNE   INIT80                                                           
INIT71   GOTO1 VDOWNLD,DMCB,(RC)                                                
*                                                                               
INIT80   CLI   QCLIENT,C'$'           IS THIS A REQUESTED OFFICE LIST           
         BNE   INITX                  NO: SKIP                                  
*                                                                               
         MVC   SAVMOL,QCLIENT+1       SAVE REQUESTED OFFICE LIST                
         CLI   QCLIENT+2,C' '         IS IT THE TWO CHARACTER VALUE             
         BH    INITX                  YES: NEED TO CONVERT                      
         XC    WORK,WORK              NO: GET PRINTABLE VALUE                   
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'P'       SYSTEM PRINT                              
         MVC   OFFD.OFCAGY,QAGENCY    AGENCY CODE                               
         MVC   OFFD.OFCMOL,QCLIENT+1  SINGLE CHARACTER OFFICE                   
         OI    OFFD.OFCINDS,OFCIMOLC  CALL OFFICER TO CONVERT                   
         GOTO1 VOFFICER,DMCB,(C'2',WORK),(0,VCOMFACS)                           
         BNE   INITX                                                            
         MVC   SAVMOL,OFFD.OFCMOL2                                              
         DROP  OFFD                                                             
*                                                                               
INITX    B     EXIT                                                             
         EJECT                                                                  
*                                      TABLE OF AGENCIES TRANSMITTING           
*                                      THEIR A8 DOWNLOAD FILES                  
*     FORMAT IS QAGENCY,EDICT RECORD CODE, LUID (SEE THEIR EDICT REC)           
*                                                                               
AGYTTAB  DC    C'WI',CL6'WILA  ',CL8'WIL1500I'                                  
         DC    X'FF'                   END OF TABLE                             
*                                                                               
         DS    F                                                                
SETOPTS  DS    0H                                                               
         ST    RE,SETOPTS-4                                                     
         XC    WORK,WORK               READ PROFILE MYSELF                      
         MVC   WORK(4),=C'POA8'                                                 
         MVC   WORK+4(3),QAGENCY                                                
         CLC   QCLIENT,=C'ALL'                                                  
         BE    SETO2                                                            
         MVC   WORK+7(3),PCLTKCLT   FOR WHEN I GET HERE FROM TBCLTF             
         CLI   PCLTOFF,C' '                                                     
         BNH   SETO2                                                            
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
SETO2    GOTO1 GETPROF,DMCB,WORK,PROGPROF,DATAMGR                               
         OC    PROGPROF,PROGPROF   CK FOR PROFILE                               
         BZ    SETO10                                                           
         LA    R2,QOPT1                                                         
         LA    R3,PROGPROF                                                      
         LA    R4,5                                                             
SETO5    CLI   0(R2),C' '          SEE IF OPTION REQUESTED                      
         BNE   *+10                YES                                          
         MVC   0(1,R2),0(R3)       GET VALUE FROM PROFILE                       
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,SETO5                                                         
*                                                                               
SETO10   DS    0H                                                               
         MVC   CDSW,PROGPROF+6        SET CDSW FOR WHOLE REPORT                 
         MVC   MTHSW,PROGPROF+7       SET MTHSW FOR WHOLE REPORT                
         MVI   OPTSETSW,C'Y'       SET OPTIONS SET SWITCH                       
         OC    MYA8PROF,MYA8PROF    DO I ALREDY HAVE ONE?                       
         BNZ   *+10                                                             
         MVC   MYA8PROF,PROGPROF                                                
         L     RE,SETOPTS-4                                                     
         BR    RE                                                               
         EJECT                                                                  
***OFF                                                                          
TBOFFF   DS    0H                                                               
         MVI   MULTOFF,C'Y'        SET PROCESSING OFFICE LIST REQ               
*                                                                               
         LA    R6,OGSTTOTS          CLEAR OFFICE GST TOTALS                     
         BAS   RE,ZAPGST                                                        
*                                                                               
         LA    R6,OPSTTOTS          CLEAR OFFICE PST TOTALS                     
         BAS   RE,ZAPGST                                                        
*                                                                               
**11/05  MVC   SAVCOFF(1),RCSVOFC   SAVE CODE FOR PRINTING                      
**11/05  MVI   SAVCOFF+1,C' '                                                   
******   GOTO1 VOFFOUT,DMCB,RCSVOFC,HEXOUT,SAVCOFF                              
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'05',BUFFBUFF),(X'80',1)                
         B     EXIT                                                             
***OFF                                                                          
         SPACE 2                                                                
TBCLTF   DS    0H                  CLIENT FIRST                                 
*                                                                               
*        SET SAVCOFF HERE FOR ALL REQUESTS                                      
*                                                                               
         MVC   SAVCOFF,SPACES                                                   
         MVC   SAVCOFF(1),PCLTOFF    SAVE OFFICE FOR HEADLINES                  
*                                                                               
         MVC   SAVAOFF,PCLTAOFC            SAVE ACC OFFICE                      
         CLC   SAVAOFF,SPACES                                                   
         BH    *+10                                                             
         MVC   SAVAOFF,SPACES                                                   
*                                                                               
         XC    WORK,WORK                                                        
OFFD     USING OFFICED,WORK                                                     
         MVI   OFFD.OFCSYS,C'P'                                                 
*                                                                               
         MVC   OFFD.OFCAGY,QAGENCY                                              
         MVC   OFFD.OFCPMED,QMEDIA                                              
         MVC   OFFD.OFCOFC,PCLTOFF                                              
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'2',WORK),(0,VCOMFACS)                           
         CLI   0(R1),0                                                          
         BNE   TBCLTF0                                                          
         MVC   SAVCOFF,OFFD.OFCOFC2                                             
*                                                                               
         DROP  OFFD                                                             
*                                                                               
TBCLTF0  DS    0H                                                               
         CLC   SAVCOFF,=X'0000'   IF STILL ZEROS, MAKE SPACES                   
         BNE   *+10                                                             
         OC    SAVCOFF,SPACES    NEEDED FOR CLIENTS WITHOUT AN OFFICE           
*                                                                               
         CLI   QOPT5,C'P'             DOWNLOAD REQ?                             
         BNE   *+8                                                              
         OI    DOWNIND,X'01'                                                    
*                                                                               
         CLI   QOPT4,C'D'             OTHER KIND OF DOWNLOAD                    
         BNE   *+8                                                              
         OI    DOWNIND,X'01'                                                    
*                                                                               
         CLC   =C'DDS ',QUESTOR       DDS MONTH END REQUEST?                    
         BNE   *+8                                                              
         MVI   DDSIND,1               SO EMAIL NOTIFICATIONS                    
*                                     MAY BE SENT                               
*                                                                               
*                                                                               
*                                 MUST READ B1 AND B1X PROFILES FOR             
*                                 PPFMTINO                                      
         XC    PROFB1,PROFB1                                                    
         XC    PROFB1X,PROFB1X                                                  
         XC    WORK,WORK               READ PROFILE MYSELF                      
         MVC   WORK(4),=C'POB1'                                                 
         MVC   WORK+4(3),QAGENCY                                                
         MVC   WORK+7(3),PCLTKCLT   FOR WHEN I GET HERE FROM TBCLTF             
         CLI   PCLTOFF,C' '                                                     
         BNH   TBCLTF1                                                          
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
TBCLTF1  GOTO1 GETPROF,DMCB,WORK,PROFB1,DATAMGR                                 
         MVC   WORK(4),=C'PB1X'                                                 
         NI    WORK,X'BF'          LOWER CASE                                   
         GOTO1 GETPROF,DMCB,WORK,PROFB1X,DATAMGR                                
*                                                                               
         MVC   WORK(4),=C'PA8A'          READ EXTENDED A8A PROFILE              
         NI    WORK,X'BF'                LOWER CASE                             
         GOTO1 GETPROF,DMCB,WORK,PROFA8A,DATAMGR                                
*                                                                               
         LA    R6,CGSTTOTS         CLEAR CLIENT GST TOTALS                      
         BAS   RE,ZAPGST                                                        
*                                                                               
         LA    R6,CPSTTOTS         CLEAR CLIENT PST TOTALS                      
         BAS   RE,ZAPGST                                                        
*                                                                               
         CLC   QCLIENT,=C'ALL'     ALL CLIENT REQUEST                           
         BE    TBCLTF2             OPTIONS SET IN INITIAL                       
         CLI   OPTSETSW,C'Y'       SEE IF OPTIONS SET ALREADY                   
         BE    TBCLTF2                                                          
         BAS   RE,SETOPTS   MUST BE ONE CLIENT OR OFFICE REQ                    
*                                                                               
TBCLTF2  DS    0H                                                               
*                                                                               
         MVC   PROGPROF,MYA8PROF   RESET TO FIRST PROFILE READ                  
*                                                                               
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
*                                                                               
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         MVI   SORTACT,0                                                        
         LARL  RF,PRDTAB                                                        
         MVC   0(3,RF),=3X'FF'                                                  
         ST    RF,ANXTPRD          INITIALIZE TABLE OF PRDS                     
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         ZAP   PREBILLN,=P'0'                                                   
         ZAP   PREBILLG,=P'0'                                                   
         ZAP   CURBILLN,=P'0'                                                   
         ZAP   CURBILLG,=P'0'                                                   
         MVI   CINVSW,0                                                         
         ZAP   CINVNET,=P'0'                                                    
         ZAP   CINVGRS,=P'0'                                                    
         ZAP   CINVCD,=P'0'                                                     
         ZAP   CINVAMT,=P'0'                                                    
         ZAP   CINVGST,=P'0'                                                    
         ZAP   CINVPST,=P'0'                                                    
         MVI   AORSW,0             SET AOR BILLS FOUND INDICATOR OFF            
         ZAP   AINVAMT,=P'0'       CLEAR AOR ACCUMLATOR                         
         MVI   COMSW,0             SET COM BILLS FOUND INDICATOR OFF            
         ZAP   CMINVAMT,=P'0'      CLEAR COM ACCUMLATOR                         
         CLC   QPRODUCT,=C'POL'                                                 
         BE    TBCLTF5                                                          
         CLI   QOPT2,C'Y'          SEE IF COMBINING ALL PRDS                    
         BNE   TBCLTFX                                                          
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   TBCLTFX                                                          
TBCLTF5  DS    0H                                                               
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
*                                                                               
TBPRDF5  DS    0H                                                               
         L     R1,ANXTPRD                                                       
         MVC   0(3,R1),PPRDKPRD                                                 
         MVC   3(20,R1),PPRDNAME                                                
         LA    R1,23(R1)                                                        
         MVC   0(3,R1),=3X'FF'                                                  
         ST    R1,ANXTPRD                                                       
*                                                                               
TBPRD5   DS    0H                                                               
         B     EXIT                NO BUCKETS FOR PRINT                         
         EJECT                                                                  
DOBILL   DS    0H                  PROCESS BILLS                                
         TM    KEY+25,X'C0'        BYPASS CLOSED-OUT RECS                       
         BO    TBBILX                                                           
         TM    PBILLCTL,X'80'      TEST DELETED                                 
         BNZ   TBBILX                                                           
*                                                                               
TBCF40   DS    0H                  PASS OLD UNREVERSED ORG BILLS                
*                                  TO SORT - WITH DUMMY PUB                     
         CLI   QREGION,C' '                                                     
         BE    *+14                                                             
         CLC   PBILLDAT,QREGION    SEE IF BILLED AFTER AS OF DATE               
         BH    TBBILX             YES - BYPASS                                  
         CLC   QPROG+52(4),SPACES                                               
         BE    TBCF49              NO CURRENT MTH                               
*                                                                               
         CLC   QPROG+54(2),SPACES   NO MONTH - CURRENT YEAR                     
         BNE   TBCF45                                                           
         CLC   PBILLDAT(2),QPROG+52 SEE IF BILLED AFTER CURRENT MTH             
         BH    TBBILX              YES - BYPASS                                 
         B     TBCF49                                                           
*                                                                               
TBCF45   CLC   PBILLDAT(4),QPROG+52 SEE IF BILLED AFTER CURRENT MTH             
         BH    TBBILX              YES - BYPASS                                 
*                                                                               
TBCF49   DS    0H                                                               
*                                                                               
*******                                                                         
*******  USE PPBVAL TO EXTRACT "EFFECTIVE" VALUES                               
*******                                                                         
         GOTO1 VPPBVAL,DMCB,(C'B',PBILLREC),PPBVALD                             
*                                                                               
         MVC   SAVPBNET,PBILLNET    MUST SAVE "REAL" NET/NET                    
*                                   - IT'S NEEDED SOMETIMES                     
*                                                                               
         MVC   PBILLGRS,PPBVEBG     SET NEW VALUES INTO PBILLREC                
         MVC   PBILLBIL,PPBVEBB                                                 
         MVC   PBILLNET,PPBVEBN                                                 
         MVC   MYBILLCD,PPBVEBC   "EFFECTIVE" CD                                
*                                                                               
         L     R0,PPBVGST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYGST,DUB                                                        
         L     R0,PPBVPST                                                       
         CVD   R0,DUB                                                           
         ZAP   MYPST,DUB                                                        
***                                                                             
         CLI   CDSW,C'Y'          SEE IF OMITTING CD                            
         BE    TBCF49B             YES                                          
         AP    PBILLNET,MYBILLCD   ADD CD TO NET/NET                            
         ZAP   PBILLBIL,PBILLGRS   PUT GROSS IN PBILLBIL                        
*                                                                               
*        NOTE *** PBILLNET AND PBILLBIL NOW MAY CONTAIN ALTERED VALUES          
*        MYBILLCD CONTAINS CD                                                   
*                                                                               
TBCF49B  CLI   PBILLTYP,C'4'                                                    
         BE    TBCF50                   DETAIL - BYPASS                         
         CLI   PBILLTYP,C'B'       NEW BILL                                     
         BE    TBCF50              TREAT AS OLD DETAIL BILL                     
*                                                                               
*              NOTE - NEW MANUAL BILLS TREATED LIKE OLD SUMMARY                 
*                                                                               
         CLI   PBILLCAN,0                                                       
         BE    TBCF55                                                           
         CLC   PBILLCAN,=6C'0'      SEE IF CANCELED                             
         BE    TBCF55                                                           
         CLI   QREGION,C' '                                                     
         BE    *+14                                                             
         CLC   PBILLCDT,QREGION    SEE IF CANCELLED AFTER AS OF DATE            
         BH    TBCF55              YES - CONSIDER UNREV                         
         CLC   QPROG+52(4),SPACES                                               
         BE    TBCF52              NO CURRENT MTH                               
*                                                                               
         CLC   QPROG+54(2),SPACES      NO MONTH - CURRENT YEAR                  
         BNE   TBCF49F                                                          
         CLC   PBILLCDT(2),QPROG+52    TEST REVERSED IN CURRENT YEAR            
         BH    TBCF55              HIGH - CONSIDER UNREV                        
         BL    TBCF50              LOW - BYPASS                                 
         B     TBCF52                                                           
*                                                                               
TBCF49F  DS    0H                                                               
         CLC   PBILLCDT(4),QPROG+52    TEST REVERSED IN CURRENT MTH             
         BH    TBCF55              HIGH - CONSIDER UNREV                        
         BL    TBCF50              LOW - BYPASS                                 
         B     TBCF52                                                           
*                                                                               
TBCF50   DS    0H                                                               
*                                                                               
*        SEE IF REVERSED MANUAL AOR BILL                                        
*        IF SO MUST STILL PROCESS                                               
*                                                                               
         CLI   PBILLTYP,C'4'                                                    
         BE    TBCF50X                                                          
         CLI   PBILLTYP,C'B'                                                    
         BE    TBCF50X                                                          
         TM    PBILCMSW,X'20'      MANUAL AOR                                   
         BNO   TBCF50X             NO - CAN SKIP                                
         CLI   PBILLCAN,0                                                       
         BNE   TBCF52              MUST PROCESS IF REVERSED                     
         CLC   PBILLCAN,=6C'0'                                                  
         BNE   TBCF52                                                           
TBCF50X  DS    0H                                                               
         GOTO1 APOSTB,DMCB,(RC)                                                 
         B     TBBILX                                                           
*                                                                               
TBCF52   XC    TBREC,TBREC                                                      
         MVC   TBKPUB(5),=X'FFFFFFFF00'          CURRENT REVERSALS              
         CLI   QOPT4,C'B'            PRD SUMMARY VERSION                        
         BE    TBCF52A                                                          
         CLI   QOPT4,C'D'            PRD SUMMARY DOWNLOADED                     
         BNE   TBCF52C                                                          
TBCF52A  MVC   TBKPUB(3),PBILKCLT                                               
         MVC   TBKPUB+3(3),PBILKPRD                                             
         B     TBCF52D           DON'T ENTER THE FF IN TBKPNM                   
*                                                                               
TBCF52C  DS    0H                                                               
         MVI   TBKPNM,X'FF'                                                     
TBCF52D  DS    0H                                                               
         GOTO1 APOSTB,DMCB,(RC)                                                 
*                                                                               
         OC    PBILKEST,PBILKEST   SEE IF EST=000                               
         BZ    TBBILX              YES - DON'T POST TO ACCUMS                   
*                                                                               
         CLI   QOPT2,C'Y'          SEE IF DOING ALL PRDS TOGETHER               
         BE    *+10                                                             
         MVC   TBKPRD,PBILKPRD                                                  
         TM    ESTTBSW,2           SEE IF SORTING ON EST                        
         BZ    *+10                                                             
         MVC   TBKEST2,PBILKEST                                                 
         CLI   QOPT4,C'M'          NO MTH OF SERVICE BREAKOUT                   
         BE    TBCF53                                                           
         CLI   QOPT4,C'B'          PRD SUMMARY VERSION                          
         BE    TBCF53                                                           
         CLI   QOPT4,C'D'          PRD SUMMARY DOWNLOADED                       
         BE    TBCF53                                                           
         MVC   TBKMOS,PBILKMOS                                                  
         CLI   MTHSW,C'I'          SEE IF DOING INSERTION MTH                   
         BNE   TBCF53              NO                                           
         MVC   TBKMOS,=X'FF00'     POST TO DUMMY MONTH                          
*                                                                               
TBCF53   MVC   TBKINV,PBILKBNO                                                  
         MVC   TBKINVMO,PBILKBMN       BILLING Y/M                              
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(3,TBBILDTE)                            
         ZAP   TBBILLN,PBILLNET    NET OR NET/NET                               
         ZAP   TBBILLG,PBILLBIL    GROSS OR GROSS-CD                            
         CLI   QOPT7,C'Y'          TRACING SORT CALLS                           
         BNE   *+14                                                             
         MVC   WORK(56),TBREC                                                   
         BAS   RE,SORTIN                                                        
         GOTO1 SORTER,DMCB,=C'PUT',TBREC                                        
         MVI   SORTACT,C'Y'                                                     
         GOTO1 DATCON,DMCB,(0,PBILLCDT),(3,TBBILDTE)                            
         PACK  DUB,PBILLCAN+2(4)          USE CANCELLATION INV NUMBER           
         CVB   R0,DUB                                                           
         STH   R0,TBKINV                                                        
*                                                                               
******   PACK  DUB,PBILLCDT(2)     YEAR                                         
******   CVB   R1,DUB                                                           
******   STC   R1,TBKINVMO                                                      
******   PACK  DUB,PBILLCDT+2(2)   MONTH                                        
******   CVB   R1,DUB                                                           
******   STC   R1,TBKINVMO+1                                                    
*                                                                               
         MVC   TBKINVMO(2),TBBILDTE    CAN USE BINARY DATE                      
*                                                                               
         TM    PBILCMSW,X'20'       MANUAL AOR BILL                             
         BNO   *+10                                                             
         MVC   PBILLNET,SAVPBNET    RESTORE PBILLNET                            
*                                                                               
         ZAP   DUB,PBILLNET                                                     
         CVB   R0,DUB                                                           
         LCR   R0,R0               MUST BE SUBTRACTED                           
         CVD   R0,DOUBLE                                                        
         ZAP   TBBILLN,DOUBLE                                                   
         ZAP   DUB,PBILLBIL                                                     
         CVB   R0,DUB                                                           
         LCR   R0,R0               MUST BE SUBTRACTED                           
         CVD   R0,DOUBLE                                                        
         ZAP   TBBILLG,DOUBLE                                                   
         CLI   QOPT7,C'Y'          TRACING SORT CALLS                           
         BNE   *+14                                                             
         MVC   WORK(56),TBREC                                                   
         BAS   RE,SORTIN                                                        
         GOTO1 SORTER,DMCB,=C'PUT',TBREC                                        
         MVI   SORTACT,C'Y'                                                     
         B     TBBILX              DONE                                         
         SPACE 2                                                                
TBCF55   XC    TBREC,TBREC                                                      
         GOTO1 APOSTB,DMCB,(RC)                                                 
*                                                                               
         OC    PBILKEST,PBILKEST   SEE IF EST=000                               
         BZ    TBBILX              YES - DON'T POST TO ACCUMS                   
*                                                                               
         CLI   QOPT2,C'Y'          SEE IF DOING ALL PRDS TOGETHER               
         BE    *+10                                                             
         MVC   TBKPRD,PBILKPRD                                                  
         TM    ESTTBSW,2           SEE IF SORTING ON EST                        
         BZ    *+10                                                             
         MVC   TBKEST2,PBILKEST                                                 
         MVC   TBKPUB(5),=5X'FF'                                                
         CLI   QOPT4,C'B'          PRD SUMMARY VERSION                          
         BE    TBCF55B                                                          
         CLI   QOPT4,C'D'        PRD SUMMARY DOWNLOADED                         
         BNE   TBCF56                                                           
TBCF55B  MVC   TBKPUB(3),PBILKCLT                                               
         MVC   TBKPUB+3(3),PBILKPRD                                             
         B     TBCF56C             NO X'FF' IN TBKPNM                           
*                                                                               
TBCF56   MVI   TBKPNM,X'FF'                                                     
TBCF56C  CLI   QOPT4,C'M'          NO MTH OF SERVICE BREAKOUT                   
         BE    TBCF58                                                           
         CLI   QOPT4,C'B'          PRD SUMMARY VERSION                          
         BE    TBCF58                                                           
         CLI   QOPT4,C'D'          PRD SUMMARY DOWNLOADED                       
         BE    TBCF58                                                           
         MVC   TBKMOS,PBILKMOS                                                  
         CLI   MTHSW,C'I'          SEE IF DOING INSERTION MTH                   
         BNE   TBCF58              NO                                           
         MVC   TBKMOS,=X'FF00'     POST TO DUMMY MONTH                          
*                                                                               
TBCF58   MVC   TBKINV,PBILKBNO                                                  
         MVC   TBKINVMO,PBILKBMN       BILLING Y/M                              
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(3,TBBILDTE)                            
         ZAP   TBBILLN,PBILLNET    NET OR NET/NET                               
         ZAP   TBBILLG,PBILLBIL    GROSS OR GROSS-CD                            
         CLI   QOPT7,C'Y'          TRACING SORT CALLS                           
         BNE   *+14                                                             
         MVC   WORK(56),TBREC                                                   
         BAS   RE,SORTIN                                                        
         GOTO1 SORTER,DMCB,=C'PUT',TBREC                                        
         MVI   SORTACT,C'Y'                                                     
         B     TBBILX                                                           
*                                                                               
TBCF60   DS    0H                                                               
TBBILX   DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
         EJECT                                                                  
TBBUY    DS    0H                                                               
         TM    KEY+25,X'C0'        NO CLOSED-OUT BUYS                           
         BO    EXIT                                                             
TB2A     CLC   PPRDKPRD,PBUYKPRD   SEE IF DOING POL BUY                         
         BE    TB2D                NO                                           
         LA    R2,PBDELEM                                                       
         USING PPRELEM,R2                                                       
*              MUST FIND PRD ELEM FOR THIS PRD                                  
         MVI   ECODE,X'21'                                                      
TB2B     BAS   RE,NEXTEL                                                        
         BE    TB2C                                                             
         B     EXIT                                                             
*                                                                               
TB2C     CLC   PPRCODE,PPRDKPRD                                                 
         BNE   TB2B                                                             
*                                                                               
TB2D     DS    0H                  NOT POL OR PRD ELEM FOUND                    
*                                                                               
*        SET UP BUFREC                                                          
*                                                                               
         XC    TBREC,TBREC                                                      
         CLI   QOPT2,C'Y'          SEE IF DOING ALL PRDS TOGETHER               
         BE    *+10                                                             
         MVC   TBKPRD,PPRDKPRD                                                  
*                                                                               
         MVC   TBKPUB(4),PBUYKPUB                                               
         CLI   PROGPROF+15,C'B' SEE IF REPORTING BASE VENDOR WITH NAME          
         BE    *+10                                                             
         MVC   TBKPUB,PBUYKPUB                                                  
*                                                                               
         CLI   QOPT4,C'B'          PRD SUMMARY VERSION                          
         BE    TB2E5                                                            
         CLI   QOPT4,C'D'          PRD SUMMARY DOWNLOADED                       
         BE    TB2E5                                                            
         CLI   QOPT4,C'S'          NO PUB BREAKOUT                              
         BNE   TB2F                                                             
TB2E     MVC   TBKPUB,=CL6'ALL'                                                 
         B     TB2H                                                             
*                                                                               
TB2E5    MVC   TBKPUB(3),PBUYKCLT    JUST PUT CLT AND PRD                       
         MVC   TBKPUB+3(3),PPRDKPRD                                             
*                                                                               
TB2F     MVC   TBPDISK,PUBSRTDA    SAVE PUB DISK ADDR                           
         CLI   QOPT6,C' '          SEE IF SORTING                               
         BE    TB2H                NO                                           
*                                                                               
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
         CLI   QOPT6,C'5'          NAME SORT                                    
         BNE   TB2G                                                             
         MVC   TBKPNM,PUBNAME                                                   
         CLC   PUBNAME(4),=C'THE '                                              
         BNE   TB2H                                                             
         MVC   TBKPNM(16),PUBNAME+4                                             
         MVC   TBKPNM+16(4),SPACES                                              
         B     TB2H                                                             
*                                                                               
TB2G     CLI   QOPT6,C'7'          NEWS AND OUTDOOR IN MKT                      
         BNE   TB2H                IGNORE OTHER CODES                           
         MVC   TBKPNM(2),PUBSTACD                                               
         CLI   PUBSTACD,C' '       SEE IF I HAVE A STATE CODE                   
         BNH   TB2G5               NO                                           
         CLI   PUBSTACD,C'0'       YES - USE IT UNLESS NUMERIC                  
         BL    *+10                                                             
TB2G5    MVC   TBKPNM(3),PUBSTATE                                               
         MVC   TBKPNM+3(17),PUBZNAME                                            
         CLI   QMEDIA,C'O'         SEE IF OUTDOOR                               
         BE    TB2H                YES - DONE                                   
         MVC   TBKPNM+3(16),PUBCITY  NEWS - USE CITY                            
*                                                                               
         DROP  R7                                                               
*                                                                               
TB2H     CLI   QOPT4,C'M'          NO MTH OF SERVICE BREAKOUT                   
         BE    TB3                                                              
         CLI   QOPT4,C'B'          PRD SUMMARY VERSION                          
         BE    TB3                                                              
         CLI   QOPT4,C'D'          PRD SUMMARY DOWNLOADED                       
         BE    TB3                                                              
         MVC   TBKMOS,PBDBDATE     USE BILLABLE DATE                            
         CLI   MTHSW,C'I'          SEE IF DOING INSERTION MTH                   
         BNE   TB3                                                              
         MVC   TBKMOS,PBUYKDAT     YR MTH                                       
**NEW 1/25/89                                                                   
         CLI   QOPT1-1,C'D'        SEE IF SHOWING DAY ALSO                      
         BNE   TB3                                                              
*                           MUST USE 2 BYTE COMPRESSED DATE                     
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(2,TBKMOS)                              
**NEW 1/25/89                                                                   
*                                                                               
TB3      TM    ESTTBSW,2           SEE IF SHOWING EST WITHIN MOS                
         BZ    *+10                                                             
         MVC   TBKEST2,PBUYKEST                                                 
         TM    ESTTBSW,X'08'       SEE IF SHOWING LINE NUMBER                   
         BZ    *+10                                                             
         MVC   TBKLINE,PBUYKLIN                                                 
*                                                                               
*                                                                               
***********    NOW DO PAYMENTS ************                                     
*                                                                               
*        ALL PAYMENTS MADE BEFORE OR DURING MONTH                               
*        ARE POSTED                                                             
GETP     DS    0H                                                               
         ZAP   TEMP1,=P'0'                                                      
         ZAP   TEMP2,=P'0'                                                      
         ZAP   TEMP3,=P'0'                                                      
         ZAP   TEMP4,=P'0'                                                      
         ZAP   TEMP5,=P'0'                                                      
         ZAP   TEMP6,=P'0'                                                      
*                                                                               
TB3D     MVI   ECODE,X'25'                                                      
         LA    R2,PBDELEM                                                       
TB5      BAS   RE,NEXTEL                                                        
         BNE   GETB                END OF PAYMENTS GO DO BILLING                
         USING PPDUMD03,R2                                                      
         OC    PPDDATE,PPDDATE                                                  
         BZ    TB5                                                              
***JV***                                                                        
         B     TB5B               SKIP FOR NOW                                  
*                                                                               
         CLC   QAGENCY,=C'OG'                                                   
         BNE   TB5B                                                             
         CLC   PPDDATE,=X'570C01'   IGNORE PAYMENTS BEFORE DEC01/87             
         BL    TB5                                                              
***JV***                                                                        
TB5B     CLC   PPDDATE,ASODTP      SEE IF PAID AFTER AS OF DATE                 
         BH    TB5                 YES - BYPASS                                 
         CLC   PPDDATE,CMEND       SEE IF PAID AFTER CURRENT MTH                
         BH    TB5                 YES - BYPASS                                 
         CLI   PAGYNAT,C'C'        SEE IF CANADIAN                              
         BNE   TB5D                                                             
         GOTO1 =A(GETGST),DMCB,(RC)  GET PAID GST AND POST TO                   
*                                  CURRENT OR PREVIOUS                          
TB5D     MVC   TBKPDDTE,PPDDATE                                                 
         MVC   TBBILDTE,PPREP      SAVE PAY REP                                 
         LA    R5,TBBILLN                                                       
         LA    R4,4                                                             
TB6      ZAP   0(8,R5),=P'0'                                                    
         LA    R5,8(R5)                                                         
         BCT   R4,TB6                                                           
         MVC   FULL,PPGROSS                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         ZAP   TBPAIDG,DUB                                                      
         ZAP   TBPAIDN,DUB                                                      
         MVC   FULL,PPAGYCOM                                                    
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         SP    TBPAIDN,DUB         GROSS - AC = NET                             
         CLI   CDSW,C'Y'          SEE IF SUBTRACTING CD                         
         BNE   TB7                 NO                                           
         MVC   FULL,PPCSHDSC                                                    
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         SP    TBPAIDG,DUB         GR-CD                                        
         SP    TBPAIDN,DUB         NET-CD                                       
TB7      DS    0H                                                               
         CLC   PPRDKPRD,PBUYKPRD   SEE IF DOING A POL BUY                       
         BE    TB7C                NO                                           
         ZAP   TEMP1,TBPAIDN                                                    
         ZAP   TEMP2,TBPAIDG                                                    
         BAS   RE,PRDSHR           GO GET SHARE FOR THIS PRD                    
         ZAP   TBPAIDN,TEMP1                                                    
         ZAP   TBPAIDG,TEMP2                                                    
*                                                                               
TB7C     CLI   PAGYNAT,C'C'         SEE IF CANADIAN                             
         BNE   TB8                                                              
         AP    CCPGST,TEMP3        CURR PAID GST                                
         AP    CPPGST,TEMP4        PREV PAID GST                                
         AP    CCPPST,TEMP5        CURR PAID PST                                
         AP    CPPPST,TEMP6        PREV PAID PST                                
*                                                                               
TB8      GOTO1 PUTBUFF                                                          
         XC    TBKPDDTE,TBKPDDTE                                                
         ZAP   TBPAIDN,=P'0'                                                    
         ZAP   TBPAIDG,=P'0'                                                    
         ZAP   TEMP3,=P'0'                                                      
         ZAP   TEMP4,=P'0'                                                      
         ZAP   TEMP5,=P'0'                                                      
         ZAP   TEMP6,=P'0'                                                      
         B     TB5                 GO DO NEXT PAY ELEM                          
*                                                                               
*   FIND PRDELEM TO CALCULATE PRD SHARE                                         
*                                                                               
PRDSHR   NTR1                                                                   
*                                NOTE-TEMP3 AND TEMP4 MAY                       
*                                HAVE CURR AND PREV PAID GST                    
*                                                                               
*                                NOTE-TEMP5 AND TEMP6 MAY                       
*                                HAVE CURR AND PREV PAID PST                    
*                                                                               
         ZAP   TEMP1,TBPAIDN                                                    
         ZAP   TEMP2,TBPAIDG                                                    
         XC    WORK(30),WORK                                                    
         IC    R3,PBDWTSUM                                                      
         N     R3,=F'127'                                                       
         BAS   RE,REMCOMP                                                       
         MVI   ELCOD,X'21'                                                      
         LA    R5,PBDELEM                                                       
PRDS5    EQU   *                                                                
         USING PPRELEM,R5                                                       
         BAS   RE,PNEXTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         IC    R4,PPRCOST                                                       
         N     R4,=F'127'                                                       
         BAS   RE,LEFTCOMP                                                      
         CLC   PPRCODE,PPRDKPRD                                                 
         BNE   PRDS5                                                            
*                                                                               
*        SEE IF LAST PRODUCT - IF SO MUST ADD LEFTOVERS                         
*                                                                               
         BAS   RE,PNEXTEL                                                       
         BE    PRDS8                                                            
         MVC   WORK+1(6),WORK+18                                                
PRDS8    BAS   RE,GETSHR                                                        
PRDSX    XIT1                                                                   
*                                                                               
*                                                                               
GETSHR   EQU   *                                                                
         ST    RE,FULL             SAVE RETURN REG                              
         LA    R7,TEMP1                                                         
         LA    R6,6                                                             
GETSHR2  EQU   *                                                                
         ZAP   DUB,0(8,R7)                                                      
         CVB   R0,DUB                                                           
         LR    RF,R0                                                            
         SR    R0,R0                                                            
         IC    R0,WORK(R6)                                                      
         SR    RE,RE                                                            
         LTR   RF,RF                                                            
         BZ    GETSHR6                                                          
         BP    *+10                                                             
         LCR   R0,R0                                                            
         L     RE,=F'-1'                                                        
         DR    RE,R3               / WTSUM (NO ROUND)                           
         MR    RE,R4               X THIS SHARE                                 
         AR    RF,R0               ADD SHARE OF PENNIES                         
         LR    R0,RF                                                            
         CVD   R0,DUB                                                           
         ZAP   0(8,R7),DUB                                                      
GETSHR6  EQU   *                                                                
         LA    R7,8(R7)                                                         
         BCT   R6,GETSHR2                                                       
         L     RE,FULL             RESTORE RETURN REG                           
         BR    RE                                                               
*                                                                               
*                                                                               
*                                  CALCULATE REMAINDER (AMT/WTSUM)              
*                                  STC REMAINDER IN WORK(R6) AND                
*                                  WORK+11(R6)                                  
REMCOMP  EQU   *                                                                
         ST    RE,FULL             SAVE RETURN REG                              
         LA    R7,TEMP1                                                         
         LA    R6,6                                                             
REM2     EQU   *                                                                
         ZAP   DUB,0(8,R7)                                                      
         CVB   R0,DUB                                                           
         LR    RF,R0                                                            
         M     RE,=F'1'                                                         
         DR    RE,R3                                                            
         LPR   RE,RE                                                            
         STC   RE,WORK(R6)                                                      
         STC   RE,WORK+11(R6)                                                   
         LA    R7,8(R7)                                                         
         BCT   R6,REM2                                                          
         L     RE,FULL             RESTORE RETURN REG                           
         BR    RE                                                               
*                                  DETERMINE PART OF REMAINDER FOR              
*                                  THIS SHARE                                   
*                                                                               
LEFTCOMP EQU   *                                                                
         MVC   WORK+18(6),WORK+1   SAVE CURRENT LEFTOVERS                       
*                                                                               
         ST    RE,FULL             SAVE RETURN REG                              
         LA    R6,6                                                             
LEFT2    SR    R1,R1                                                            
         IC    R1,WORK+11(R6)      ORIG. REM                                    
         LTR   R1,R1                                                            
         BZ    LEFT4               NONE                                         
         SR    RF,RF                                                            
         IC    RF,WORK(R6)         LEFT OVER                                    
         LTR   RF,RF                                                            
         BZ    LEFT4               NONE LEFT                                    
         MR    R0,R4               X THIS SHARE                                 
         SLDL  R0,1                                                             
         DR    R0,R3               / WTSUM                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,1                AT LEAST 1                                   
         LR    R0,RF                                                            
         SR    RF,R1                                                            
         BNM   *+8                                                              
         SR    RF,RF                                                            
         LR    R1,R0                                                            
         STC   RF,WORK(R6)         SAVE LEFT                                    
         CLC   PPRCODE,PPRDKPRD    UNLESS THIS IS OUR PRODUCT                   
         BNE   LEFT4                                                            
         STC   R1,WORK(R6)                                                      
LEFT4    EQU   *                                                                
         BCT   R6,LEFT2                                                         
         L     RE,FULL             RESTORE RETURN REG                           
         BR    RE                                                               
*                                                                               
PNEXTEL  CLI   0(R5),0                                                          
         BE    PNEXTELX                                                         
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCOD,0(R5)                                                      
         BER   RE                                                               
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
PNEXTELX LTR   R5,R5                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
GETB     DS    0H                  NOW DO BILLING ELEMS                         
         MVI   ECODE,X'26'                                                      
         LA    R2,PBDELEM                                                       
GETB2    BAS   RE,NEXTEL                                                        
         BNE   BUYEND                                                           
         USING PPDUMD02,R2                                                      
         OC    PBLDATE,PBLDATE                                                  
         BZ    GETB2                                                            
***JV***                                                                        
         B     GETB2B                SKIP FOR NOW                               
*                                                                               
         CLC   QAGENCY,=C'OG'                                                   
         BNE   GETB2B                                                           
         CLC   PBLDATE,=X'570C01'   IGNORE BILLING BEFORE DEC01/87              
         BL    GETB2                                                            
***JV***                                                                        
GETB2B   CLC   PBPRD,PPRDKPRD      SEE IF RIGHT PRODUCT                         
         BNE   GETB2                                                            
         CLC   PBLDATE,ASODTP      SEE IF PAID AFTER AS OF DATE                 
         BH    GETB2               YES - BYPASS                                 
         CLC   PBLDATE,CMEND       SEE IF BILLED AFTER CURRENT MTH              
         BH    GETB2               YES - BYPASS                                 
*                                                                               
*        USE PPBVAL TO EXTRACT "EFFECTIVE" VALUES                               
*                                                                               
         GOTO1 VPPBVAL,DMCB,(C'E',PBILELEM),PPBVALD                             
         MVC   PBGROSS(12),PPBVEEG      USE EFFECTIVE GROSS,AC,CD               
*                                                                               
         MVC   TBBILDTE,PBLDATE                                                 
         LA    R5,TBBILLN                                                       
         LA    R4,4                                                             
GETB6    ZAP   0(8,R5),=P'0'                                                    
         LA    R5,8(R5)                                                         
         BCT   R4,GETB6                                                         
         MVC   FULL,PBGROSS                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         ZAP   TBBILLG,DUB                                                      
         ZAP   TBBILLN,DUB                                                      
         MVC   FULL,PBAGYCOM                                                    
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         SP    TBBILLN,DUB         GROSS - AC = NET                             
         CLI   CDSW,C'Y'          SEE IF SUBTRACTING CD                         
         BNE   GETB7               NO                                           
         MVC   FULL,PBCSHDSC                                                    
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         SP    TBBILLG,DUB         GR-CD                                        
         SP    TBBILLN,DUB         NET-CD                                       
GETB7    DS    0H                                                               
         MVC   TBKINVMO,PBLDATE    ONLY YEAR AND MONTH                          
         MVC   TBKINV,PBINVNO                                                   
         OC    PBINVNO,PBINVNO     SEE IF I HAVE AN INVOICE NUMBER              
         BNZ   GETB8                                                            
         MVI   TBKINVMO,X'FF'                                                   
         MVC   TBKINVMO+1(3),PBLDATE    NO INV SO USE DATE                      
GETB8    GOTO1 PUTBUFF                                                          
         XC    TBKINVMO,TBKINVMO                                                
         XC    TBKINV,TBKINV                                                    
         XC    TBBILDTE,TBBILDTE                                                
         XC    TBKPDDTE,TBKPDDTE                                                
         ZAP   TBBILLN,=P'0'                                                    
         ZAP   TBBILLG,=P'0'                                                    
         B     GETB2               GO DO NEXT BILL ELEM                         
*                             OLD DETAIL BILLING                                
*                                                                               
BUYEND   B     EXIT                                                             
         EJECT                                                                  
FBPUB  DS    0H                                                                 
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'01',BUFFBUFF),(X'80',1)                
         B     EXIT                                                             
         SPACE 2                                                                
LBPUB  DS    0H                                                                 
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYP,X'01'                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     LBUYP10                                                          
*                                                                               
LBUYP5   GOTO1 BUFFALO,DMCB,=C'SEQ',(X'01',BUFFBUFF),BUFREC,0                   
LBUYP10  CLI   DMCB+8,X'80'        END OF FILE                                  
         BE    LBUYPX                                                           
         CLI   BUFTYP,X'01'        SINCE 'HIGH' DOSEN'T PASS EOF                
         BNE   LBUYPX                                                           
         MVC   TBREC(52),BUFREC+1                                               
         MVI   TBREC+47,0                                                       
         MVC   TBBILLN(32),BUFPBILN                                             
         CLI   QOPT7,C'Y'                                                       
         BNE   *+14                                                             
         MVC   WORK(56),TBREC                                                   
         BAS   RE,SORTIN                                                        
         GOTO1 SORTER,DMCB,=C'PUT',TBREC                                        
         MVI   SORTACT,C'Y'                                                     
         B     LBUYP5                                                           
*                                                                               
LBUYPX   GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'01',BUFFBUFF),(X'80',1)                
         B     EXIT                                                             
         EJECT                                                                  
PUTBUFF  NTR1                                                                   
         MVI   BUFTYP,X'01'                                                     
         MVC   BUFTYP+1(52),TBREC                                               
         MVC   BUFPBILN(32),TBBILLN                                             
         ZAP   BUFCBILG,=P'0'                                                   
         ZAP   BUFCPAYG,=P'0'                                                   
         ZAP   BUFPBILG,=P'0'                                                   
         ZAP   BUFPPAYG,=P'0'                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
PUTSX    XIT1                                                                   
         EJECT                                                                  
TBAGY    DS    0H                  AGENCY TOTALS                                
*                                                                               
         CLI   RCREQREP,C'N'       SEE IF SUPPRESSING REQ DETAILS               
         BE    TBAG05              YES - DON'T CLOSE DOWNLOAD NOW               
*                                  WILL BE DONE AT RUNLAST                      
*                                                                               
         CLI   QOPT4,C'D'           SEE IF SPECIAL DOWNLOAD VERSION             
         BE    TBAG01                                                           
         CLI   QOPT5,C'P'          PUBLICATION DOWNLOAD                         
         BNE   TBAG05                                                           
*                                  MODE IS LBUYREQ                              
*        IN THESE CASES I MUST CLOSE THE DOWNLOADED REPORT NOW                  
TBAG01   GOTO1 VDOWNLD,DMCB,(RC)                                                
*                                                                               
TBAG05   CLI   AGYACT,C'Y'                                                      
         BNE   TBAGX                                                            
         MVI   FORCEHED,C'Y'                                                    
         CLC   QCLIENT,=C'ALL'     ONE CLT SKIP AGY TOTALS                      
         BE    TBAG20                                                           
***OFF                                                                          
         CLC   QCLIENT(2),=C'$*'    SEE IF ALL OFFICE REQUEST                   
         BE    TBAG16                                                           
         CLI   QCLIENT,C'$'                                                     
         BE    TBAG15                                                           
***OFF                                                                          
         CLI   QCLIENT,C'&&'       GROUP REQUEST                                
         BE    TBAG20                                                           
         CLI   QCLIENT,C'*'        OFFICE REQUEST                               
         BNE   TBAG50                                                           
***OFF                                                                          
         B     TBAG20                                                           
*                                                                               
TBAG15   MVI   RCSUBPRG,12           OFFICE LIST TOTALS                         
*                                                                               
         CLI   QOPT7,C'N'     OFFICE TOTALS (OVERRIDE OF PROGPROF+8)            
         BE    TBAG25                                                           
*                                                                               
         CLI   QOPT7,C'C'     CLIENT TOTALS (OVERRIDE OF PROGPROF+8)            
         BE    TBAG15C                                                          
*                                                                               
         CLC   QCLIENT(2),=C'$*'     ALL OFFICE REQ?                            
         BE    TBAG25                IGNORE PROFILE                             
*                                                                               
         CLI   PROGPROF+8,C'C'       BY CLT?                                    
         BNE   TBAG25                                                           
TBAG15C  MVI   RCSUBPRG,14                                                      
         B     TBAG25                                                           
*                                                                               
TBAG16   MVI   RCSUBPRG,13           MEDIA TOTALS BY OFFICE                     
         B     TBAG25                                                           
*                                                                               
TBAG20   DS    0H                                                               
         MVI   RCSUBPRG,9                                                       
TBAG25   DS    0H                                                               
         MVI   RQEMAIL,C'Y'        SUMMARY ALSO TO EMLFILE                      
         XC    BUFREC,BUFREC                                                    
         MVI   TOTTYP,X'06'        AGY TOTALS                                   
         MVI   PCLTREC,0           TO NOT USE MEDIA NAME OVERRIDE               
         GOTO1 VBTOTS,DMCB,(RC)                                                 
         MVI   RQEMAIL,C'N'                                                     
*                                                                               
TBAG50   DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'END'                                              
*                                                                               
TBAGX    B     EXIT                                                             
         EJECT                                                                  
***OFF                                                                          
*                                                                               
TBOFF    DS    0H                  OFFICE TOTALS FOR OFFICE LIST REQ            
         CLI   OFFACT,C'Y'                                                      
         BNE   TBOFFX                                                           
         MVI   FORCEHED,C'Y'                                                    
         CLC   QCLIENT,=C'ALL'     ONE CLT SKIP OFF TOTALS                      
         BE    TBOFF20                                                          
         MVI   RQEMAIL,C'Y'        SUMMARY ALSO TO EMLFILE                      
         CLI   QCLIENT,C'$'        OFFICE LIST REQUEST                          
         BE    TBOFF20                                                          
         MVI   RQEMAIL,C'N'        SET OFF EMLFILE OUTPUT                       
         CLI   QCLIENT,C'&&'       GROUP REQUEST                                
         BE    TBOFF20                                                          
         CLI   QCLIENT,C'*'        OFFICE REQUEST                               
         BNE   TBOFF50                                                          
*                                                                               
TBOFF20  DS    0H                                                               
         MVI   RCSUBPRG,11                                                      
         XC    BUFREC,BUFREC                                                    
         MVI   TOTTYP,X'05'        OFF TOTALS                                   
         MVI   PCLTREC,0           TO NOT USE MEDIA NAME OVERRIDE               
         GOTO1 VBTOTS,DMCB,(RC)                                                 
         MVI   RQEMAIL,C'N'        SET OFF EMLFILE OUTPUT                       
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
         SPACE 2                                                                
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ECODE,0(R2)                                                      
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         EJECT                                                                  
**********************************************************************          
* NOTIFY PEOPLE AUTOMATICALLY THAT REPORT WAS GENERATED              *          
**********************************************************************          
         SPACE 1                                                                
         USING MAILTBD,R2                                                       
SENDMAIL NTR1                                                                   
*                                                                               
         LARL  R2,MAILTAB                                                       
SENDM10  CLI   MAILAGY,EOF                                                      
         BE    SENDMX                                                           
         CLC   MAILAGY,RCORIGID    ARE WE AT THE RIGHT AGENCY                   
         BE    SENDM20                                                          
         LA    R2,MAILNQ(R2)       BUMP TO NEXT ENTRY                           
         B     SENDM10                                                          
*                                                                               
SENDM20  DS    0H                                                               
         MVC   WARNMSG5,MAILUID           CONNECT ID OF THE AGENCY              
         GOTO1 DATCON,DMCB,(0,MYTODAY),(5,WARNMSG4)                             
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
SENDMX   B     EXIT                                                             
         DROP  R2                                                               
         SPACE 2                                                                
*                                                                               
WARNMSG  DS    0CL(L'EMSG)         SOME CONSTANT VALUES FOR E-MAIL              
WARNMSG1 DC    C'AUTONOTE*'                                                     
WARNMSG2 DC    CL45' '             THIS IS VAR LEN, COMMA SEPARATED             
WARNMSG3 DC    C':'                                                             
         DC    C'PA8 REPORT WAS GENERATED ON '                                  
WARNMSG4 DC    CL8' '                      TODAY'S DATE                         
         DC    C' FOR '                                                         
WARNMSG5 DC    CL8' '                      CONNECT ID                           
WARNLEN1 EQU   *-WARNMSG3                                                       
WARNMSG6 DC    CL(L'EMSG-(*-WARNMSG))' '   SPARE SPACES                         
         LTORG                                                                  
SORTCARD DC    CL80'SORT FIELDS=(1,47,A),FORMAT=BI'                             
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=84'                                    
*                                                                               
         DROP  RB,R8                                                            
         EJECT                                                                  
POSTBILL NMOD1 0,POSTB                                                          
         L     RC,0(R1)                                                         
         USING PPWORKD,RC                                                       
         L     RA,PPFILEC                                                       
         USING PPFILED,RA                                                       
         LA    R9,SPACEND                                                       
         USING PPA8WRKD,R9                                                      
         LA    R2,CURBILLN                                                      
         CLC   QPROG+52(4),SPACES  SEE IF I HAVE A CURRENT MTH                  
         BE    POSTB5              NO                                           
*                                                                               
         CLC   QPROG+54(2),SPACES    NO MONTH - CURRENT YEAR                    
         BNE   POSTB3                                                           
         CLC   PBILLDAT(2),QPROG+52   ONLY CHECK YEAR                           
         BE    POSTB5                                                           
         LA    R2,PREBILLN                                                      
         B     POSTB8                                                           
*                                                                               
POSTB3   DS    0H                                                               
         CLC   PBILLDAT(4),QPROG+52                                             
         BE    POSTB5                                                           
         LA    R2,PREBILLN                                                      
         B     POSTB8                                                           
*                                                                               
POSTB5   DS    0H                                                               
         CLI   QOPT5,C'Y'          SEE IF LISTING CUR INVOICES                  
         BNE   POSTB8                                                           
         MVI   RCSUBPRG,10                                                      
         MVC   P+3(3),PBILKPRD                                                  
         MVC   P+8(3),=C'000'                                                   
         OC    PBILKEST,PBILKEST   SEE IF EST=000                               
         BZ    INVR10                                                           
         ZIC   R0,PBILKEST                                                      
         SLL   R0,8                                                             
         IC    R0,PBILKEST+1                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+8(3),DUB                                                       
*                                                                               
INVR10   DS    0H                                                               
         CLI   PBILKMOS+1,12       FUNNY BILLING PERIODS                        
         BNH   INVR15                                                           
         ZIC   R0,PBILKMOS+1                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+14(2),DUB                                                      
         MVI   P+16,C'/'                                                        
         ZIC   R0,PBILKMOS                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(2),DUB                                                      
         B     INVR17                                                           
*                                                                               
INVR15   GOTO1 DATCON,DMCB,(3,PBILKMOS),(9,P+14)                                
INVR17   DS    0H                                                               
         CLI   PBILLTYP,C'0'       SEE IF NUMERIC                               
         BNL   INVR19                                                           
         MVC   P+22(2),PBILLTYP    NEW BILLS                                    
         B     INVR20                                                           
*                                                                               
INVR19   MVC   P+22(2),=C'S '                                                   
         CLI   PBILLTYP,C'4'                                                    
         BNE   INVR20                                                           
         MVI   P+22,C'D'                                                        
INVR20   DS    0H                                                               
*                                                                               
         GOTO1 AFMTINO,DMCB,PBILLDAT,(2,PBILKBNO),(PBILKMED,PROFB1),   X        
               PROFB1X                                                          
*                                                                               
         L     RF,DMCB+4           ADDRESS OF "SHORT" FORMAT                    
         MVC   P+28(7),0(RF)                                                    
*                                                                               
         CLI   PBRETAIL,X'81'                                                   
         BNE   INVR22                                                           
         MVI   P+35,C'C'                                                        
         B     INVR23                                                           
*                                                                               
INVR22   CLI   PBRETAIL,X'41'                                                   
         BNE   INVR22A                                                          
         MVI   P+35,C'S'                                                        
         B     INVR23                                                           
*                                                                               
INVR22A  TM    PBILSTAT,X'20'         SEE IF GRP M MIDAS BILL                   
         BZ    INVR22C                                                          
         MVI   P+24,C'T'                                                        
         B     INVR23                                                           
*                                                                               
INVR22C  TM    PBILCMSW,X'20'         AOR BILL                                  
         BNO   INVR22D                                                          
         MVC   P+24(3),=C'AOR'                                                  
         B     INVR23A             ONLY SHOW AMOUNT DUE                         
*                                                                               
INVR22D  TM    PBILCMSW,X'01'         COM BILL                                  
         BNO   INVR22F                                                          
         MVC   P+24(3),=C'UFC'                                                  
         B     INVR23                                                           
*                                                                               
INVR22F  TM    PBILCMSW,X'08'         SEP COM NET BILL                          
         BNO   INVR23                                                           
         MVC   P+24(3),=C'NET'                                                  
         B     INVR23                                                           
*                                                                               
INVR23   EDIT  (P6,PBILLNET),(14,P+37),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P6,PBILLBIL),(14,P+53),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P6,MYBILLCD),(14,P+69),2,COMMAS=YES,FLOAT=-                     
*                                                                               
INVR23A  ZAP   DOUBLE,PBILLRCV                                                  
         CLI   PAGYNAT,C'C'        SEE IF CANADIAN                              
         BNE   INVR23B                                                          
         AP    DOUBLE,MYGST                                                     
         AP    DOUBLE,MYPST                                                     
         EDIT  (P8,MYGST),(14,P+101),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P8,MYPST),(14,P+117),2,COMMAS=YES,FLOAT=-                       
*                                                                               
INVR23B  EDIT  (P8,DOUBLE),(14,P+85),2,COMMAS=YES,FLOAT=-                       
         TM    PBILCMSW,X'20'      FLAG AOR BILLS                               
         BO    INVR23D             THEY AREN'T TOTALLED                         
         CLI   PBRETAIL,X'41'      FLAG RETAIL SUMMARY BILLS                    
         BNE   INVR24              THEY AREN'T TOTALLED                         
*                                                                               
INVR23C  MVI   P+051,C'*'                                                       
         MVI   P+067,C'*'                                                       
         MVI   P+083,C'*'                                                       
INVR23D  MVI   P+099,C'*'                                                       
*                                                                               
INVR24   CLI   PBILLTYP,C'B'       SEE IF REGUALR NEW BILL                      
         BE    INVR25              YES - CAN'T BE CANCELED                      
         CLI   PBILLCAN,0          JUST IN CASE                                 
         BE    INVR25                                                           
         CLC   PBILLCAN,=6C'0'                                                  
         BE    INVR25                                                           
*                                                                               
         LA    R3,PSECOND                                                       
         CP    MYGST,=P'0'                                                      
         BNE   INVR24C                                                          
         CP    MYPST,=P'0'                                                      
         BNE   INVR24C                                                          
         LA    R3,P            IF NO GST OR PST THEN USE P                      
*                                                                               
INVR24C  MVC   106(12,R3),=C'(REVERSED BY'                                      
         MVC   119(2,R3),PBILLCAN                                               
         MVI   121(R3),C'-'                                                     
         MVC   122(4,R3),PBILLCAN+2                                             
         MVI   126(R3),C')'                                                     
*                                                                               
INVR25   GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
         CLI   PBRETAIL,X'41'       RETAIL SUMMARY BILL                         
         BE    POSTBX               DON'T POST TO ACCUMS                        
         TM    PBILCMSW,X'20'       AOR BILL                                    
         BO    INVR25A              DON'T POST TO ACCUMS                        
         B     INVR25X              DON'T POST TO ACCUMS                        
*                                                                               
INVR25A  MVI   AORSW,1                                                          
         AP    AINVAMT,PBILLRCV                                                 
         AP    AINVAMT,MYGST        INCLUDE GST IN AOR BILL AMTS                
         AP    AINVAMT,MYPST        INCLUDE GST IN AOR BILL AMTS                
         AP    CINVGST,MYGST        STILL POST GST                              
         AP    CINVPST,MYPST        STILL POST PST                              
         B     POSTB10              ALSO POST TO CLIENT ACCUMS                  
*                                                                               
************************                                                        
********* THIS CODE NOT USED ANYMORE                                            
********* IT WAS USED WHEN UFC COMM BILLS WERE OMITTED                          
********* FROM TOTALS                                                           
************************                                                        
INVR25C  MVI   COMSW,1                                                          
         AP    CMINVAMT,PBILLRCV                                                
         AP    CMINVAMT,MYGST       INCLUDE GST IN COM BILL AMTS                
         AP    CMINVAMT,MYPST       INCLUDE PST IN COM BILL AMTS                
         AP    CINVGST,MYGST        STILL POST GST                              
         AP    CINVPST,MYPST        STILL POST PST                              
         B     POSTB10              ALSO POST TO CLIENT ACCUMS                  
*                                                                               
INVR25X  OC    PBILKEST,PBILKEST    SEE IF EST=000 BILL                         
         BNZ   INVR30                                                           
         OI    CINVSW,2            SET EST=000 BILL FOUND                       
         B     POSTBX              DON'T POST TO ACCUMS                         
*                                  ROLL TO CURRENT INV TOTALS                   
INVR30   AP    CINVNET,PBILLNET    NET OR NET/NET                               
         AP    CINVGRS,PBILLBIL    GROSS OR GROSS-CD                            
         AP    CINVCD,MYBILLCD                                                  
         AP    CINVAMT,PBILLRCV                                                 
         AP    CINVAMT,MYGST        ADD GST TO BILL AMT TOTAL                   
         AP    CINVAMT,MYPST        ADD PST TO BILL AMT TOTAL                   
         AP    CINVGST,MYGST                                                    
         AP    CINVPST,MYPST                                                    
         OI    CINVSW,1                                                         
*                                                                               
POSTB8   DS    0H                                                               
*                                                                               
         OC    PBILKEST,PBILKEST    SEE IF EST=000 BILL                         
         BZ    POSTBX              YES - DON'T POST                             
         CLI   PBRETAIL,X'41'      RETAIL SUMMARY BILL                          
         BE    POSTBX              DON'T POST                                   
         TM    PBILCMSW,X'20'      AOR BILL                                     
         BO    POSTB10             DON'T POST                                   
*                                                                               
         AP    0(8,R2),PBILLNET    NET OR NET/NET                               
         AP    8(8,R2),PBILLBIL    GROSS OR GROSS - CD                          
*                                                                               
POSTB10  CLI   PAGYNAT,C'C'         SEE IF CANADIAN                             
         BNE   POSTBX                                                           
         LA    R4,CCBGST             CLIENT CURR BILLED GST                     
         LA    RE,CCBPST             CLIENT CURR BILLED PST                     
         CLC   QPROG+52(4),SPACES    NO CURRENT MONTH                           
         BE    POSTB15               POST TO CURRENT                            
*                                                                               
         CLC   QPROG+54(2),SPACES    NO MONTH - CURRENT YEAR                    
         BNE   POSTB13                                                          
         CLC   PBILLDAT(2),QPROG+52   ONLY CHECK YEAR                           
         BE    POSTB15                                                          
         LA    R4,CPBGST              POST TO PREVIOUS                          
         LA    RE,CPBPST              POST TO PREVIOUS                          
         B     POSTB15                                                          
*                                                                               
POSTB13  DS    0H                                                               
         CLC   PBILLDAT(4),QPROG+52                                             
         BE    POSTB15                                                          
         LA    R4,CPBGST              POST TO PREVIOUS                          
         LA    RE,CPBPST              POST TO PREVIOUS                          
*                                                                               
POSTB15  AP    0(8,R4),MYGST                                                    
         AP    0(8,RE),MYPST                                                    
*                                                                               
*                                                                               
POSTBX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
GETGST   NMOD1 0,GETGST            EXTRACT GST AND PST FROM PAY ELEMS           
         L     RC,0(R1)                                                         
         USING PPWORKD,RC                                                       
         L     RA,PPFILEC                                                       
         USING PPFILED,RA                                                       
         LA    R9,SPACEND                                                       
         USING PPA8WRKD,R9                                                      
*                                                                               
         XC    WRKGST,WRKGST                                                    
         XC    WRKNET,WRKNET                                                    
         XC    WGSTPCT,WGSTPCT                                                  
*                                                                               
         USING PPDUMD03,R2                                                      
*                                                                               
         TM    PBDCNDA,X'80'       SEE IF CANADIAN                              
         BZ    GETPSTX         NO GST OR PST IF NOT CANADIAN                    
*                                                                               
GETPG5   DS    0H                                                               
         L     RF,PPGROSS      GROSS                                            
         S     RF,PPAGYCOM     MINUS AGYCOM                                     
*                                                                               
         MVC   BYTE(1),PBDGST      TAKE GST CODE FROM BUY                       
         LA    R5,PGSTTAB                                                       
         CLI   BYTE,0              CODE NOT ENTERED IN BUY                      
         BNE   *+8                                                              
         MVI   BYTE,C'S'             DEFAULT TO STANDARD                        
         SPACE 3                                                                
*                                                                               
*---------------------------> VERIFY CODE IN PGSTTAB                            
*                                                                               
GETPG10B CLC   0(1,R5),BYTE                                                     
         BE    GETPG10D                                                         
GETPG10C LA    R5,8(R5)                                                         
         CLI   0(R5),X'FF'           END OF TABLE                               
         BNE   GETPG10B                                                         
         XC    FULL,FULL                                                        
         B     GETPG30              BAD GST CODE - NO GST                       
*                                                                               
GETPG10D XC    FULL,FULL                                                        
*                                                                               
         CLC   PBUYKDAT,4(R5)   SEE IF INSERTION DATE AFTER EFFECTIVE           
         BNL   GETPG10E         USE IT                                          
         B     GETPG10C       CHECK FOR ANOTHER ENTRY                           
*                                                                               
GETPG10E MVC   WGSTPCT,2(R5)      SAVE GST PCT                                  
*                                                                               
         OC    PBDTAX,PBDTAX       SEE IF I HAVE TAX                            
         BNZ   GETPG20                                                          
*                                                                               
*                                   NOTE - RF HAS NET AT THIS POINT             
GETPG10G DS    0H                                                               
         ST    RF,WRKNET                                                        
*                                                                               
         TM    PBDCNDA,X'01'       SEE IF PAID WITH GST                         
         BZ    GETPGX              NO - DON'T CALCULATE GST PAID                
*                                                                               
         XC    FULL,FULL                                                        
         OC    WGSTPCT,WGSTPCT                                                  
         BZ    GETPG30                                                          
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),WGSTPCT                                                
         L     RE,FULL                                                          
         MR    RE,RE                                                            
         SLDL  RE,1                                                             
         D     RE,=F'100000'        GST TAX HAS 3 DECIMALS                      
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         ST    RF,FULL                                                          
         B     GETPG30        GO ACCUMLATE IN GSTTAXPD                          
*                                                                               
*                          FOR TAX BUYS WORK INCLUDES TAX                       
GETPG20  DS    0H          EXTRACT NET ELEMENT                                  
*                     RETURNS NET IN RF  FOR GETPG10G                           
         L     R0,PPGROSS              PPGROSS                                  
         S     R0,PPAGYCOM            MINUS PPAGYCOM                            
         ST    R0,DUB                                                           
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),PBDTAX                                                 
         L     RF,FULL           RE = SALES TAX PCT                             
         SR    RE,RE                                                            
*                                                                               
         TM    1(R5),X'01'             SEE IF SALES TAX IS ON NET               
         BO    GETPG25                                                          
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),WGSTPCT  GSTTAX PCT                                    
         L     R0,FULL                                                          
         A     R0,=F'100000'                                                    
         ST    R0,FULL                                                          
*                                                                               
         M     RE,FULL                                                          
         D     RE,=F'100000'                                                    
*                                                                               
GETPG25  A     RF,=F'1000000'                                                   
         ST    RF,FULL                                                          
*                                                                               
         L     RF,DUB                                                           
         M     RE,=F'1000000'                                                   
         SLDL  RE,1                                                             
         D     RE,FULL                                                          
         LTR   RF,RF                                                            
         BM    *+8                                                              
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         B     GETPG10G            RF HAS NET                                   
*                                                                               
GETPG30  DS    0H                                                               
         L     R0,FULL             ACCUMLATE GST PAID                           
         ST    R0,WRKGST                                                        
         CVD   R0,DUB                                                           
         CLC   PPDDATE,CMSTART                                                  
         BL    GETPG40                                                          
         AP    TEMP3,DUB         ADD TO CLIENT CURR PAID GST                    
         B     GETPGX                                                           
*                                                                               
GETPG40  AP    TEMP4,DUB         ADD TO CLIENT PREV PAID GST                    
*                                                                               
GETPGX   DS    0H                                                               
*                                                                               
         EJECT                                                                  
*        CALCULATE PST IF NEEDED                                                
*                                                                               
GETPST   DS    0H                                                               
*                                                                               
         TM    PBDCNDA,X'02'       SEE IF PAID WITH PST                         
         BZ    GETPSTX             NO - DON'T CALCULATE PST PAID                
*                                                                               
         LA    R5,PSTAREA          FIRST CLEAR THEN SET-UP PSTAREAS             
         LA    R0,10                                                            
         XC    0(PSTAREAL,R5),0(R5)                                             
         LA    R5,PSTAREAL(R5)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         LA    R5,PSTAREA                                                       
PST      USING PSTAREA,R5                                                       
*                                                                               
         LA    R6,PBUYREC+33       POINT TO FIRST ELEMENT IN RECORD             
         SR    R0,R0                                                            
*                                                                               
GTCPSTLP DS    0H                                                               
*                                                                               
         CLI   0(R6),0             EOR - NO PST ELEMENT                         
         BE    GTCPSTX                SKIP PST CALCULATIONS                     
*                                                                               
         CLI   0(R6),PBYPSTQ       LOOKING FOR BUY PST ELEMENT                  
         BE    GTCPSTFD                                                         
*                                                                               
GTCPSTCN DS    0H                                                               
*                                                                               
         IC    R0,1(R6)            GET ELEMENT LENGTH                           
         AR    R6,R0               POINT TO NEXT ELEMENT                        
         B     GTCPSTLP                                                         
*                                                                               
GTCPSTFD DS    0H                                                               
*                                                                               
         MVC   WRKPSTC,PBYPSTC-PBYPSTEL(R6)   SAVE PST CODES                    
*                                                                               
*        FIND PROVINCE WITH PST CODE                                            
*                                                                               
         LA    R7,1                PROVINCE COUNTER                             
         LA    R6,WRKPSTC          PST CODES BY PROVINCE                        
*                                                                               
GTCPSTPL DS    0H                                                               
*                                                                               
         CLI   0(R6),0             SKIP IF NO PST CODE PRESENT                  
         BE    GTCPSTPC                                                         
*                                                                               
*        FIND PROVINCIAL CODE                                                   
*                                                                               
         LR    R1,R7               COPY PROVINCE COUNTER                        
         BCTR  R1,0                DECREMENT FOR INDEXING                       
         SLL   R1,1                EACH ENTRY IS 2 BYTES                        
         LA    R1,PROVTAB(R1)      POINT TO PROVINCE CODE                       
*                                                                               
*        FIND PST TABLE ENTRY FOR PROVINCE AND PST CODE                         
*                                                                               
GTCPSTT  DS    0H                                                               
*                                                                               
         LA    R8,PPSTTAB          POINT TO PST TABLE                           
         LA    RE,PPSTTABN         NUMBER OF ENTRIES IN TABLE                   
*                                                                               
GTCPSTTL DS    0H                                                               
*                                                                               
         CLC   0(2,R8),0(R1)       MATCH ON PROVINCE CODE                       
         BE    *+10                                                             
         CLC   0(2,R8),=C'ZZ'      OR DEFAULT CODE                              
         BNE   GTCPSTTC                                                         
*                                                                               
         CLC   2(1,R8),0(R6)       MATCH ON PST CODE                            
         BNE   GTCPSTTC                                                         
*                                                                               
         MVC   WPSTDATE,PPDDATE                                                 
*                                                                               
         CLC   WPSTDATE,=AL1(94,6,1)    SEE IF PAID JUN 1/94                    
         BNE   *+10                                                             
         MVC   WPSTDATE,=AL1(94,5,31)   USE MAY 31/94                           
*                                                                               
         CLC   PBUYKDAT,WPSTDATE  SEE IF INSERT DATE PRIOR TO PAY DATE          
         BNL   *+10                                                             
         MVC   WPSTDATE,PBUYKDAT   IF SO USE IT                                 
*                                                                               
         CLC   WPSTDATE,5(R8)      BUY MUST BE AFTER INIT DATE                  
         BL    GTCPSTTC                                                         
*                                                                               
         B     GTCPSTTF            TABLE ENTRY FOUND                            
*                                                                               
GTCPSTTC DS    0H                                                               
*                                                                               
         LA    R8,PPSTTABL(R8)     NEXT TABLE ENTRY                             
         BCT   RE,GTCPSTTL                                                      
*                                                                               
         B     GTCPSTTD            SKIP PST SEARCH                              
*                                                                               
GTCPSTTF DS    0H                  GOT PST CODE ENTRY                           
*                                                                               
         MVC   PST.PSTPROV,0(R1)   SET PROVINCE CODE                            
         MVC   PST.PSTCODE,0(R6)   SET PST CODE                                 
         MVC   PST.PSTPCT,3(R8)    SET PST PER CENT                             
         MVC   PST.PSTBASIS,8(R8)  SET PST BASIS                                
*                                                                               
GTCPSTTD DS    0H                                                               
*                                                                               
GTCPSTPC DS    0H                                                               
*                                                                               
         LA    R6,1(R6)            BUMP CODE POINTER                            
         LA    R7,1(R7)            BUMP PROVINCE COUNTER                        
         CH    R7,=H'10'           MAX 10 PROVINCES                             
         BNH   GTCPSTPL                                                         
*                                                                               
GTCPSTX  DS    0H                                                               
*                                                                               
*                                                                               
         LA    R5,PST.PSTAREA      CALCULATE PST TAXES                          
         LA    R0,10               10 PROVINCES                                 
*                                                                               
GETPSTLP DS    0H                                                               
*                                                                               
         OC    PST.PSTPROV,PST.PSTPROV SKIP IF NO MORE PROVINCES W/ PST         
         BZ    GETPSTDN                                                         
*                                                                               
         L     RF,WRKNET           COPY NET                                     
*                                                                               
         TM    PST.PSTBASIS,X'01'      SKIP PST ON NET ONLY                     
         BO    *+8                                                              
         A     RF,WRKGST           NET + GSTTAX                                 
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,PST.PSTPCT     GET PST PERCENT                              
         MR    RE,RE               CALCULATE PST TAX                            
*                                                                               
         SLDL  RE,1                DOUBLE RESULT                                
         D     RE,=F'100000'       PER CENT HAS 3 DECIMALS                      
         LTR   RF,RF               CHECK FOR NEGATIVE NUMBER                    
         BM    *+8                                                              
         AH    RF,=H'1'            PREPARE FOR ROUNDING                         
         SRA   RF,1                ROUND                                        
*                                                                               
         CVD   RF,DUB                                                           
         CLC   PPDDATE,CMSTART        SEE IF BEFORE CURRENT MONTH               
         BL    GETPST40                                                         
         AP    TEMP5,DUB              POST TO CURRENT                           
         B     GETPST50                                                         
*                                                                               
GETPST40 DS    0H                                                               
         AP    TEMP6,DUB            POST TO PREVIOUS                            
*                                                                               
GETPST50 DS    0H                                                               
*                                                                               
GETPSTCN DS    0H                                                               
*                                                                               
         LA    R5,PSTAREAL(R5)     BUMP TO NEXT PSTAREA                         
         BCT   R0,GETPSTLP                                                      
*                                                                               
GETPSTDN DS    0H                                                               
*                                                                               
         DROP  PST                                                              
         DROP  R2                                                               
GETPSTX  XIT1                      RETURN                                       
*                                                                               
         LTORG                                                                  
*                                                                               
WGSTPCT  DS    XL2                                                              
*                                                                               
         SPACE 2                                                                
       ++INCLUDE PGSTTAB                                                        
         EJECT                                                                  
TBCLTL   NMOD1 0,TBCLTL                                                         
         L     RC,0(R1)                                                         
         USING PPWORKD,RC                                                       
         L     RA,PPFILEC                                                       
         USING PPFILED,RA                                                       
         LA    R9,SPACEND                                                       
         USING PPA8WRKD,R9                                                      
         LA    R8,TBCLTL+4095                                                   
         LA    R8,1(R8)                                                         
         USING TBCLTL+4096,R8      ** NOTE USE OF SECOND BASE REGISTER          
         TM    CINVSW,X'01'        SEE IF I HAVE CURRENT INVS                   
         BNO   TBCL                NO                                           
         MVI   RCSUBPRG,10                                                      
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+28(6),=C'TOTAL*'                                               
         EDIT  (P8,CINVNET),(14,P+37),2,COMMAS=YES,FLOAT=-                      
         EDIT  (P8,CINVGRS),(14,P+53),2,COMMAS=YES,FLOAT=-                      
         EDIT  (P8,CINVCD),(14,P+69),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P8,CINVAMT),(14,P+85),2,COMMAS=YES,FLOAT=-                      
*                                                                               
         MVI   P+051,C'*'                                                       
         MVI   P+067,C'*'                                                       
         MVI   P+083,C'*'                                                       
         MVI   P+099,C'*'                                                       
*                                                                               
         CLI   PAGYNAT,C'C'      SEE IF CANADIAN                                
         BNE   TBCL00                                                           
         EDIT  (P8,CINVGST),(14,P+101),2,COMMAS=YES,FLOAT=-                     
         MVI   P+115,C'*'                                                       
         EDIT  (P8,CINVPST),(14,P+117),2,COMMAS=YES,FLOAT=-                     
         MVI   P+131,C'*'                                                       
*                                                                               
TBCL00   DS    0H                                                               
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
TBCL     TM    CINVSW,X'02'        SEE IF EST=000 BILLS FOUND                   
         BNO   TBCL0               NO                                           
         MVI   RCSUBPRG,10                                                      
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+28(66),=C'NOTE - EST=000 ARE ''PRODUCT TOTAL'' BILLS -X        
                NOT REFLECTED IN TOTALS'                                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
TBCL0    DS    0H                                                               
         CLI   AORSW,1                                                          
         BNE   TBCL0A                                                           
         MVI   RCSUBPRG,10                                                      
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+28(44),=C'AOR BILLING AMOUNT - NOT REFLECTED IN TOTALSX        
               '                                                                
         EDIT  (P8,AINVAMT),(14,P+85),2,COMMAS=YES,FLOAT=-                      
         MVI   P+99,C'*'                                                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         B     TBCL0A                                                           
*                                                                               
TBCL0A   DS    0H                                                               
         CLI   COMSW,1                                                          
         BNE   TBCL1                                                            
         MVI   RCSUBPRG,10                                                      
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+28(44),=C'COM BILLING AMOUNT - NOT REFLECTED IN TOTALSX        
               '                                                                
         EDIT  (P8,CMINVAMT),(14,P+85),2,COMMAS=YES,FLOAT=-                     
         MVI   P+99,C'*'                                                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
*                                                                               
TBCL1    EQU   *                                                                
         MVI   FORCEHED,C'Y'                                                    
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
         CLI   SORTACT,C'Y'       SEE IF SORTING ACTIVITY                       
         BNE   TBCL5X                                                           
TBCL5    GOTO1 SORTER,DMCB,=C'GET'                                              
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   TBCL10              LAST REC                                     
         BAS   RE,PDDTEEND                                                      
         BAS   RE,INVEND                                                        
         BAS   RE,EST2END                                                       
         BAS   RE,MOSEND                                                        
         GOTO1 APUBEND,DMCB,(RC)                                                
         BAS   RE,PRDEND                                                        
         BAS   RE,CLTEND                                                        
TBCL5X   GOTO1 SORTER,DMCB,=C'END'                                              
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
         XC    PUBREC(25),PUBREC      CLEAR PUB REC FOR PPG                     
         B     TBEXIT                                                           
*                                                                               
         DROP  R7                                                               
*                                                                               
         SPACE 2                                                                
TBCL10   DS    0H                                                               
         L     R3,DMCB+4           ADDR OF SORTED REC                           
         MVC   TBREC,0(R3)                                                      
         CLI   QOPT7,C'Y'          TRACING SORT CALLS                           
         BNE   *+14                                                             
         MVC   WORK,TBREC                                                       
         BAS   RE,SORTOUT                                                       
         OC    MYKEY,MYKEY                                                      
         BZ    TBCL30              FIRST RECORD                                 
         CLC   TBREC(47),MYKEY                                                  
         BE    TBCL15              SAME PAY DATE AND REP/TYPE                   
         BAS   RE,PDDTEEND                                                      
*                                                                               
TBCL15   CLC   TBREC(38),MYKEY                                                  
         BE    TBCL16                                                           
         BAS   RE,INVEND           INVOICE END                                  
*                                                                               
TBCL16   DS    0H                                                               
         CLC   TBREC(34),MYKEY                                                  
         BE    TBCL18                                                           
         BAS   RE,EST2END                                                       
*                                                                               
TBCL18   CLC   TBREC(31),MYKEY                                                  
         BE    TBCL20                                                           
         BAS   RE,MOSEND                                                        
TBCL20   DS    0H                  WAS ESTIEND                                  
*                                                                               
TBCL22   CLC   TBREC(29),MYKEY                                                  
         BE    TBCL24                                                           
         GOTO1 APUBEND,DMCB,(RC)                                                
*                                                                               
TBCL24   CLC   TBREC(3),MYKEY                                                   
         BE    TBCL30                                                           
         BAS   RE,PRDEND                                                        
*                                                                               
TBCL30   DS    0H                                                               
         OC    TBKINVMO(4),TBKINVMO      SEE IF BILLING OR PAYMENT              
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
***OFF                                                                          
         MVC   MOSACT(7),=7C'Y'                                                 
***OFF                                                                          
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
***OFF                                                                          
         MVC   MOSACT(7),=7C'Y'                                                 
***OFF                                                                          
         B     TBCL5                                                            
         EJECT                                                                  
PDDTEEND DS    0H             PAYMENT DATE END                                  
         CLI   DCPAYSW,C'Y'        CHK FOR CURRENT PAYMENTS                     
         BNER  RE                                                               
         L     R4,ANXTPD           ADD TO MOS PAYMENTS TABLE                    
         MVC   0(6,R4),MYKPDDTE    SAVE PAID DATE & PRD                         
         MVC   6(2,R4),MYBILDT     CONTAINS PAY REP                             
         MVC   8(16,R4),DCPAYN                                                  
         LA    R4,24(R4)                                                        
         MVC   0(8,R4),=8X'FF'                                                  
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
         MVC   0(2,R4),MYKINVMO    MONTH                                        
         MVC   2(2,R4),MYKINV                                                   
         MVI   4(R4),0             SO OLD DETAIL BILLING WILL NOT LOOK          
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
EST2END  NTR1                                                                   
         TM    ESTTBSW,2           SEE IF SORTING ON EST                        
         BZ    EST2E60                                                          
*                                                                               
         JIF   QOPT5,EQ,C'P',AND,PROGPROF+9,EQ,C'M',EST2E                       
*                                                                               
         JIF   QOPT5,EQ,C'P',AND,PROGPROF+9,EQ,C'N',EST2EA                      
*                                                                               
         CLI   ESTCACT,C'Y'        CHK FOR CURRENT ACTIVITY                     
         BNE   EST2E50                                                          
*                                                                               
         CLI   QOPT5,C'P'          PUBLICATION DOWNLOAD?                        
         BNE   EST2E0                                                           
*                                                                               
         CLI   PROGPROF+9,C'M'     MOS DETAIL ONLY?                             
         BE    EST2E0                                                           
*                                                                               
         CLI   PROGPROF+9,C'Y'     MOS,INV,PAY DATE DETAILS?                    
         BE    EST2E0                                                           
         B     EST2E50                                                          
*                                                                               
*        MTH OF SERVICE DOWNLOAD - CHECK FOR BALANCE FORWARD                    
*        OR CURRENT ACTIVITY                                                    
*                                                                               
*                                                                               
EST2E    LA    R4,ESTTOTS                                                       
         ZAP   DOUBLE,0(8,R4)      PREV BILLED                                  
         SP    DOUBLE,8(8,R4)      PREV CLEARED                                 
         CP    DOUBLE,=P'0'       ANY BALANCE FORWARD?                          
         BNE   EST2E0             YES- CONTINUE                                 
*                                                                               
*        IF PROFPROG+9 IS C'N'  ONLY CHECK FOR CURRENT ACTIVITY                 
*                                                                               
EST2EA   CLI   ESTCACT,C'Y'       CURRENT ACTIVITY?                             
         BE    EST2E0                                                           
         B     EST2E50            SKIP TO TOTALS                                
*                                                                               
EST2E0   DS    0H                                                               
         LARL  R5,INVTAB                                                        
         LARL  R6,PDTAB                                                         
EST2E2   DS    0H                                                               
         ST    R5,INVADR                                                        
         ST    R6,PAYADR                                                        
         CLI   STAPSW,C'Y'         SEE IF PUB PRINTED ALREADY                   
         BNE   EST2E3                                                           
         ZIC   R3,LINE             REPEAT IF NEW PAGE                           
         LA    R3,1(R3)                                                         
         STC   R3,X                                                             
         CLC   X(1),MAXLINES                                                    
         BNH   EST2E4                                                           
         MVI   MTHPSW,0            SO MTH WILL PRINT ALSO                       
EST2E3   DS    0H                                                               
         BAS   RE,PRNTSTA                                                       
EST2E3C  MVI   STAPSW,C'Y'                                                      
EST2E4   CLI   MTHPSW,C'Y'         SEE IF MTH OF SERV PRINTED ALREADY           
         BE    EST2E6                                                           
         CLI   MYKMOS,0            NO MTH OF SERVICE BREAKOUT                   
         BE    EST2E6                                                           
         CLI   MYKMOS,X'FF'        IF DOING INS MTHS                            
         BE    EST2E6              CUR REV AND UNREV ORIG BILLS                 
*                                  WILL HAVE MYKMOS SET TO X'FF00'              
**NEW 1/25/89                                                                   
         CLI   MTHSW,C'I'                                                       
         BNE   EST2E4A                                                          
         CLI   QOPT1-1,C'D'          SEE IF COMPRESSED M/D/Y                    
         BE    EST2E5                                                           
*                                                                               
EST2E4A  JIF   PROGPROF+13,EQ,C'Y',AND,QOPT5,EQ,C'P',EST2E4B                    
         JIF   PROGPROF+13,EQ,C'M',AND,QOPT5,EQ,C'P',EST2E4B                    
         JIF   PROGPROF+13,EQ,C'D',AND,QOPT5,EQ,C'P',EST2E4B                    
*                                                                               
**NEW 1/25/89                                                                   
         CLI   MYKMOS+1,12                                                      
         BNH   EST2E5              BILLING PERIODS                              
EST2E4B  LA    R2,MYKMOS                                                        
         LA    R3,P+9                                                           
         BAS   RE,MTH13                                                         
         B     EST2E5B                                                          
**NEW 1/25/89                                                                   
EST2E5   CLI   MTHSW,C'I'                                                       
         BNE   EST2E5A                                                          
         CLI   QOPT1-1,C'D'          SEE IF COMPRESSED M/D/Y                    
         BNE   EST2E5A                                                          
         GOTO1 DATCON,DMCB,(2,MYKMOS),(5,P+8)                                   
         B      EST2E5B                                                         
**NEW 1/25/89                                                                   
EST2E5A  GOTO1 DATCON,DMCB,(3,MYKMOS),(9,P+9)                                   
EST2E5B  MVI   MTHPSW,C'Y'                                                      
*                                                                               
EST2E6   DS    0H                                                               
         OC    MYKEST2,MYKEST2                                                  
         BZ    EST2E8              NO EST                                       
         MVC   HALF,MYKEST2                                                     
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+17(3),DUB                                                      
         CLI   MYKLINE,1                                                        
         BNH   EST2E8                                                           
         MVI   P+20,C'-'                                                        
         EDIT  (B1,MYKLINE),(3,P+21),0,ALIGN=LEFT                               
*                                                                               
EST2E8   DS    0H                                                               
EST2E10  CLC   0(5,R5),=5X'FF'     END OF INVOICE TABLE                         
         BE    EST2E20                                                          
         EDIT  (P8,5(R5)),(13,P+36),2,COMMAS=YES,FLOAT=-                        
         CLI   0(R5),X'FF'   CHK FOR BILL DATE                                  
         BNE   EST2E13                                                          
         GOTO1 DATCON,DMCB,(3,1(R5)),(5,P+51)                                   
         B     EST2E15                                                          
EST2E13  DS    0H                                                               
         MVC   WORK(2),0(R5)                                                    
         MVI   WORK+3,1       SET DAY TO 1                                      
         GOTO1 DATCON,DMCB,(3,WORK),(0,DUB)                                     
         GOTO1 AFMTINO,DMCB,DUB,(2,2(R5)),(QMEDIA,PROFB1),PROFB1X               
         L     RE,DMCB+4            "SHORT" FORMAT                              
         MVC   P+51(7),0(RE)                                                    
***                                                                             
EST2E15  EDIT  (P8,13(R5)),(13,P+104),2,COMMAS=YES,FLOAT=-                      
         LA    R5,21(R5)                                                        
*                                                                               
EST2E20  DS    0H                                                               
         CLC   0(8,R6),=8X'FF'          END OF PAYMENT TABLE                    
         BE    EST2E30                                                          
         EDIT  (P8,8(R6)),(13,P+59),2,COMMAS=YES,FLOAT=-                        
         GOTO1 DATCON,DMCB,(3,0(R6)),(5,P+76)                                   
         EDIT  (P8,16(R6)),(13,P+117),2,COMMAS=YES,FLOAT=-                      
         MVC   P+86(3),=C'DIR'                                                  
         OC    6(2,R6),6(R6)       CHK FOR CR CK OR REP                         
         BZ    EST2E28                                                          
         TM    6(R6),X'80'         CHK FOR CK                                   
         BNO   EST2E22                                                          
         MVC   P+73(2),=C'CK'                                                   
         B     EST2E25                                                          
EST2E22  TM    6(R6),X'40'         CHK FOR CR                                   
         BNO   EST2E25                                                          
         MVC   P+73(2),=C'CR'                                                   
EST2E25  NI    6(R6),X'3F'         SET OFF 04 OR 08                             
         OC    6(2,R6),6(R6)                                                    
         BZ    EST2E28                                                          
         MVC   HALF,6(R6)                                                       
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+85(4),DUB                                                      
EST2E28  LA    R6,24(R6)                                                        
EST2E30  DS    0H                                                               
*                                                                               
         JIF   QOPT5,EQ,C'P',AND,PROGPROF+9,EQ,C'M',EST2E30C                    
*        MUST STILL PROCESS                                                     
*                                                                               
         JIF   QOPT5,EQ,C'P',AND,PROGPROF+9,EQ,C'N',EST2E30C                    
*        MUST STILL PROCESS                                                     
*                                                                               
         CLC   P+36(45),SPACES                                                  
         BE    EST2E40             NOTHING TO PRINT                             
*                                                                               
         CLI   QOPT5,C'P'          SEE IF DOING PUB DOWNLOAD                    
         BNE   EST2E21                                                          
*                                                                               
         CLI   PROGPROF+9,C'M'  MOS ONLY?                                       
         BE    EST2E30C                                                         
*                               BUILD SPECIAL PRINT LINE FOR DOWNLOAD           
         CLI   PROGPROF+9,C'Y'  SEE IF INCLUDING MOS,INVS,AND DATES             
         BNE   EST2E50                                                          
*                               BUILD SPECIAL PRINT LINE FOR DOWNLOAD           
EST2E30C MVC   P+30(18),SPACES   CLEAR FOR PUB                                  
         MVC   P+110(20),SPACES      MAY BE USED FOR PUB SORT NAME              
*                                    (QOPT6 = 5)                                
         MVC   P+30(7),=C'CUR REV'                                              
         CLC   MYKPUB(4),=4X'FF'                                                
         BNE   EST2E31                                                          
         CLI   MYKPUB+4,X'FF'                                                   
         BNE   EST2E31                                                          
         MVC   P+30(7),=CL7'UNREV'                                              
         B     EST2E34                                                          
EST2E31  DS    0H                                                               
         CLC   MYKPUB(5),=C'ZZZZX'     NEW MANUAL                               
         BNE   EST2E32                                                          
         MVC   P+30(7),=CL7'MANUAL'                                             
         B     EST2E34                                                          
EST2E32  DS    0H                                                               
         CLC   MYKPUB(3),=C'ALL'                                                
         BNE   EST2E33                                                          
         MVC   P+30(7),=CL7'ALL'                                                
         B     EST2E34                                                          
*                                                                               
EST2E33  DS    0H                                                               
         MVC   P+110(20),MYKPNM         PUB NAME (QOPT6 SHOULD BE 5)            
         IC    R2,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R2),MYKPUB),(C'S',P+30)                           
*                          NOTE:  P+17 WILL STILL HAVE EST/LNE                  
EST2E34  DS    0H                                                               
         MVC   P+90(8),P+8    MONTH OR DATE-SAVE IN P+90                        
         CLI   MTHSW,C'I'                                                       
         BNE   EST2E34A                                                         
         CLI   QOPT1-1,C'D'          SEE IF COMPRESSED M/D/Y                    
         BE    EST2E34B                                                         
*                                                                               
EST2E34A DS    0H                                                               
         MVC   P+90(8),P+9    MONTH OR DATE (IF PROGPROF+13=D)                  
EST2E34B MVI   P+98,C' '                                                        
         MVC   P(14),SPACES                                                     
         MVI   P,C'P'        SYSTEM                                             
         MVC   P+2(1),QMEDIA                                                    
         MVC   P+4(L'SAVCOFF),SAVCOFF                                           
*                                                                               
         MVC   P+130(L'SAVAOFF),SPACES                                          
         CLI   PROFA8A,C'Y'            DOWNLOAD ACC OFFICE                      
         BNE   *+10                                                             
         MVC   P+130(L'SAVAOFF),SAVAOFF                                         
*                                                                               
         MVC   P+6(3),PCLTKCLT        CLIENT                                    
         MVC   P+10(3),MYKPRD         PRODUCT                                   
         CLI   QOPT2,C'Y'             SEE IF COMBINING PRDS                     
         BNE   *+10                                                             
         MVC   P+10(3),=C'ALL'                                                  
         GOTO1 VDOWNLD,DMCB,(C'E',(RC))  DOWNLOAD EST MTH OF SERV LINE          
         MVC   P,SPACES            MUST CLEAR PRINT LINE                        
         MVI   MTHPSW,0            SO MOS WILL PRINT AGAIN                      
*                                                                               
         CLI   PROGPROF+9,C'Y'      MOS + INV AND CLEAR DETAILS                 
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
         LARL  R2,PDTAB                                                         
         MVC   0(8,R2),=8X'FF'                                                  
         ST    R2,ANXTPD                                                        
         LARL  R2,INVTAB                                                        
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
         MVC   BUFMOS,MYKMOS                                                    
**NEW 1/25/89                                                                   
         CLI   BUFMOS,X'FF'                                                     
         BE    EST2E53                                                          
         CLI   MTHSW,C'I'                                                       
         BNE   EST2E53                                                          
         CLI   QOPT1-1,C'D'                                                     
         BNE   EST2E53                                                          
         GOTO1 DATCON,DMCB,(2,MYKMOS),(3,WORK)                                  
         MVC   BUFMOS,WORK               ONLY USE Y/M IN TOTALS                 
**NEW 1/25/89                                                                   
EST2E53  MVC   BUFEST,MYKEST2                                                   
         MVC   BUFPBILN(64),EPBILLN                                             
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   BUFTYP,X'04'        CLT TOTALS                                   
         GOTO1 (RF)                                                             
*                                                                               
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
         EJECT                                                                  
MOSEND   NTR1                                                                   
         CLI   MOSACT,C'Y'                                                      
         BNE   MOSEX                                                            
         XC    BUFREC,BUFREC                                                    
         MVC   BUFPBILN(64),MPBILLN                                             
*                                                                               
         JIF   QOPT5,EQ,C'P',AND,PROGPROF+9,EQ,C'M',MOSE                        
*                                                                               
         CLI   MOSCACT,C'Y'        SEE IF ACTIVE IN CURRENT MTH                 
         BNE   MOSE45              NO - SKIP TO TOTALS                          
*                                                                               
         CLI   QOPT4,C'B'          PRD SUMMARY VERSION                          
         BE    MOSE45              SKIP TO TOTALS                               
         CLI   QOPT4,C'D'          PRD SUMMARY DOWNLOADED                       
         BE    MOSE45              SKIP TO TOTALS                               
         CLI   QOPT5,C'P'          PUBLICATION DOWNLOAD                         
         BNE   MOSE1                                                            
*                                                                               
         CLI   PROGPROF+9,C'M'     SEE IF DOWNLOADING MOS ONLY                  
         BE    MOSE1                                                            
*                                                                               
         CLI   PROGPROF+9,C'Y'     SEE IF DOWNLOADING MOS DETAILS               
         BE    MOSE1                                                            
         B     MOSE45              SKIP TO TOTALS                               
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
*                                                                               
MOSE1    DS    0H                                                               
         TM    ESTTBSW,2           SEE IF SORTING ON EST                        
         BNZ   MOSE40              SKIP TO MTH TOTALS                           
         MVI   MTHPSW,0                                                         
         LARL  R5,INVTAB                                                        
         LARL  R6,PDTAB                                                         
MOSE1A   DS    0H                                                               
         CLI   STAPSW,C'Y'          SEE IF I'VE PRINTED PUB                     
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
         CLI   MYKMOS,X'FF'        IF DOING INS MTH CUR REV AND                 
         BE    MOSE3               UNREV ORIG BILLS WILL HAVE                   
*                                  MYKMOS SET TO X'FF00'                        
**NEW 1/25/89                                                                   
         CLI   MTHSW,C'I'                                                       
         BNE   MOSE2A                                                           
         CLI   QOPT1-1,C'D'                                                     
         BE    MOSE2B                                                           
*                                                                               
MOSE2A   JIF   PROGPROF+13,EQ,C'Y',AND,QOPT5,EQ,C'P',MOSE2A5                    
         JIF   PROGPROF+13,EQ,C'M',AND,QOPT5,EQ,C'P',MOSE2A5                    
         JIF   PROGPROF+13,EQ,C'D',AND,QOPT5,EQ,C'P',MOSE2A5                    
*                                                                               
**NEW 1/25/89                                                                   
         CLI   MYKMOS+1,12         13TH PERIOD                                  
         BNH   MOSE2B                                                           
MOSE2A5  LA    R2,MYKMOS                                                        
         LA    R3,P+9                                                           
         BAS   RE,MTH13                                                         
         B     MOSE2C                                                           
**NEW 1/25/89                                                                   
MOSE2B   CLI   MTHSW,C'I'                                                       
         BNE   MOSE2B5                                                          
         CLI   QOPT1-1,C'D'                                                     
         BNE   MOSE2B5                                                          
         GOTO1 DATCON,DMCB,(2,MYKMOS),(5,P+8)                                   
         B     MOSE2C                                                           
**NEW 1/25/89                                                                   
MOSE2B5  GOTO1 DATCON,DMCB,(3,MYKMOS),(9,P+9)                                   
MOSE2C   MVI   MTHPSW,C'Y'                                                      
*                                                                               
MOSE3    DS    0H                                                               
         ST    R5,INVADR        SAVE ADDRESSES FOR PUB DOWNLOAD                 
         ST    R6,PAYADR                                                        
MOSE5    CLC   0(5,R5),=5X'FF'          END OF INVOICE TABLE                    
         BE    MOSE20                                                           
         EDIT  (P8,5(R5)),(13,P+36),2,COMMAS=YES,FLOAT=-                        
         CLI   0(R5),X'FF'   CHK FOR BILL DATE                                  
         BNE   MOSE8                                                            
         GOTO1 DATCON,DMCB,(3,1(R5)),(5,P+51)                                   
         B     MOSE10                                                           
*                                                                               
MOSE8    DS    0H                  MONTH                                        
         MVC   WORK(2),0(R5)                                                    
         MVI   WORK+3,1       SET DAY TO 1                                      
         GOTO1 DATCON,DMCB,(3,WORK),(0,DUB)                                     
         GOTO1 AFMTINO,DMCB,DUB,(2,2(R5)),(QMEDIA,PROFB1),PROFB1X               
         L     RE,DMCB+4            "SHORT" FORMAT                              
         MVC   P+51(7),0(RE)                                                    
***                                                                             
MOSE10   EDIT  (P8,13(R5)),(13,P+104),2,COMMAS=YES,FLOAT=-                      
         LA    R5,21(R5)                                                        
MOSE20   DS    0H                                                               
         CLC   0(8,R6),=8X'FF'          END OF PAYMENT TABLE                    
         BE    MOSE30                                                           
         EDIT  (P8,8(R6)),(13,P+59),2,COMMAS=YES,FLOAT=-                        
         GOTO1 DATCON,DMCB,(3,0(R6)),(5,P+76)                                   
         EDIT  (P8,16(R6)),(13,P+117),2,COMMAS=YES,FLOAT=-                      
         MVC   P+86(3),=C'DIR'                                                  
         OC    6(2,R6),6(R6)       CHK FOR CR CK OR REP                         
         BZ    MOSE28                                                           
         TM    6(R6),X'80'         CHK FOR CK                                   
         BNO   MOSE22                                                           
         MVC   P+73(2),=C'CK'                                                   
         B     MOSE25                                                           
MOSE22   TM    6(R6),X'40'         CHK FOR CR                                   
         BNO   MOSE25                                                           
         MVC   P+73(2),=C'CR'                                                   
MOSE25   NI    6(R6),X'3F'         SET OFF 04 OR 08                             
         OC    6(2,R6),6(R6)                                                    
         BZ    MOSE28                                                           
         MVC   HALF,6(R6)                                                       
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+85(4),DUB                                                      
MOSE28   LA    R6,24(R6)                                                        
MOSE30   DS    0H                                                               
*                                                                               
         JIF   QOPT5,EQ,C'P',AND,PROGPROF+9,EQ,C'M',MOSE30B                     
*        MUST STILL PROCESS IF DOING MTH OF SERV DOWNLOAD                       
*                                                                               
         CLC   P+36(45),SPACES                                                  
         BE    MOSE40              NOTHING LEFT TO PRINT                        
*                                                                               
         CLI   QOPT5,C'P'          SEE IF DOING PUB DOWNLOAD                    
         BNE   MOSE35                                                           
*                                                                               
         CLI   PROGPROF+9,C'M'  MOS ONLY?                                       
         BE    MOSE30B                                                          
*                                                                               
         CLI   PROGPROF+9,C'Y'  SEE IF INCLUDING MOS,INVS,AND DATES             
         BNE   MOSE45                                                           
*                               BUILD SPECIAL PRINT LINE FOR DOWNLOAD           
MOSE30B  MVC   P+30(18),SPACES  MUST CLEAR SOME OF P                            
         MVC   P+110(20),SPACES      MAY BE USED FOR PUB SORT NAME              
*                                    (QOPT6 = 5)                                
         MVC   P+30(7),=C'CUR REV'                                              
         CLC   MYKPUB(4),=4X'FF'                                                
         BNE   MOSE31                                                           
         CLI   MYKPUB+4,X'FF'                                                   
         BNE   MOSE31                                                           
         MVC   P+30(7),=CL7'UNREV'                                              
         B     MOSE34                                                           
MOSE31   DS    0H                                                               
         CLC   MYKPUB(5),=C'ZZZZX'     NEW MANUAL                               
         BNE   MOSE32                                                           
         MVC   P+30(7),=CL7'MANUAL'                                             
         B     MOSE34                                                           
MOSE32   DS    0H                                                               
         CLC   MYKPUB(3),=C'ALL'                                                
         BNE   MOSE33                                                           
         MVC   P+30(7),=CL7'ALL'                                                
         B     MOSE34                                                           
*                                                                               
MOSE33   DS    0H                                                               
         MVC   P+110(20),MYKPNM        PUB SORT NAME                            
         IC    R2,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R2),MYKPUB),(C'S',P+30)                           
*                                                                               
MOSE34   DS    0H                                                               
         MVC   P+90(8),P+8      MOS - SAVE IN P+90                              
         CLI   MTHSW,C'I'       INSERTION MONTH?                                
         BNE   MOSE34A                                                          
         CLI   QOPT1-1,C'D'          SEE IF COMPRESSED M/D/Y                    
         BE    MOSE34B                                                          
*                                                                               
MOSE34A  DS    0H                                                               
         MVC   P+90(8),P+9      MOS - (WITH DAY IF PROGPROF+13=D)               
MOSE34B  MVI   P+98,C' '                                                        
         MVC   P(14),SPACES                                                     
         MVI   P,C'P'        SYSTEM                                             
         MVC   P+2(1),QMEDIA                                                    
         MVC   P+4(L'SAVCOFF),SAVCOFF                                           
*                                                                               
         MVC   P+130(L'SAVAOFF),SPACES                                          
         CLI   PROFA8A,C'Y'            DOWNLOAD ACC OFFICE                      
         BNE   *+10                                                             
         MVC   P+130(L'SAVAOFF),SAVAOFF                                         
*                                                                               
         MVC   P+6(3),PCLTKCLT        CLIENT                                    
         MVC   P+10(3),MYKPRD         PRODUCT                                   
         CLI   QOPT2,C'Y'             SEE IF COMBINING PRDS                     
         BNE   *+10                                                             
         MVC   P+10(3),=C'ALL'                                                  
         GOTO1 VDOWNLD,DMCB,(C'M',(RC))   DOWNLOAD MTH OF SERVICE LINE          
         MVC   P,SPACES            MUST CLEAR PRINT LINE                        
         MVI   MTHPSW,0            SO MOS WILL REPRINT                          
*                                                                               
         CLI   PROGPROF+9,C'Y'     MOS + INV AND CLEAR DETAILS                  
         BNE   MOSE45              SKIP TO TOTALS                               
*                                                                               
         B     MOSE1A              JUST RETURN FOR NEXT                         
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
         CLI   STAPSW,C'Y'          SEE IF I'VE PRINTED PUB                     
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
MOSE42B  DS    0H                                                               
         CLI   MYKMOS,X'FF'        IF DOING INS MTH CUR REV AND                 
         BE    MOSE45              UNREV ORIG BILLS HAVE                        
*                                  MYKMOS SET TO X'FF00'                        
**NEW 1/25/89                                                                   
         CLI   MTHSW,C'I'                                                       
         BNE   MOSE42D                                                          
         CLI   QOPT1-1,C'D'                                                     
         BE    MOSE43                                                           
*                                                                               
MOSE42D  JIF   PROGPROF+13,EQ,C'Y',AND,QOPT5,EQ,C'P',MOSE42E                    
         JIF   PROGPROF+13,EQ,C'M',AND,QOPT5,EQ,C'P',MOSE42E                    
         JIF   PROGPROF+13,EQ,C'D',AND,QOPT5,EQ,C'P',MOSE42E                    
*                                                                               
*                                  TOTALS WILL BE SHOWN AT PUB END              
         CLI   MYKMOS+1,12         13TH PERIOD                                  
         BNH   MOSE43                                                           
MOSE42E  LA    R2,MYKMOS                                                        
         LA    R3,P+9                                                           
         BAS   RE,MTH13                                                         
         B     MOSE43C                                                          
*                                                                               
**NEW 1/25/89                                                                   
MOSE43   CLI   MTHSW,C'I'                                                       
         BNE   MOSE43A                                                          
         CLI   QOPT1-1,C'D'                                                     
         BNE   MOSE43A                                                          
         GOTO1 DATCON,DMCB,(2,MYKMOS),(5,P+8)                                   
         B     MOSE43C                                                          
**NEW 1/25/89                                                                   
MOSE43A  GOTO1 DATCON,DMCB,(3,MYKMOS),(9,P+9)                                   
MOSE43C  DS    0H                                                               
         EDIT  (P8,MCBILLN),(13,P+36),2,COMMAS=YES,FLOAT=-                      
         EDIT  (P8,MCPAYN),(13,P+59),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P8,MCBILLG),(13,P+104),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,MCPAYG),(13,P+117),2,COMMAS=YES,FLOAT=-                      
**NEW 1/25/89                                                                   
         CLI   MTHSW,C'I'                                                       
         BNE   MOSE43D                                                          
         CLI   QOPT1-1,C'D'                                                     
         BNE   MOSE43D                                                          
         MVI   P+16,C'*'                                                        
         B     *+8                                                              
**NEW 1/25/89                                                                   
MOSE43D  MVI   P+15,C'*'                                                        
         MVI   P+49,C'*'                                                        
         MVI   P+72,C'*'                                                        
         MVI   P+130,C'*'                                                       
         CLI   P+117,C' '          SOME THING MAY BE THERE                      
         BH    *+8                                                              
         MVI   P+117,C'*'                                                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                  BUFFALO MOS WITH ANY ACTIVITY                
*                                  TO TOTALS                                    
MOSE45   DS    0H                                                               
         MVI   BUFTYP,X'02'        PRODUCT TOTALS                               
         MVC   BUFMOS,MYKMOS                                                    
**NEW 1/25/89                                                                   
         CLI   BUFMOS,X'FF'                                                     
         BE    MOSE46                                                           
         CLI   MTHSW,C'I'                                                       
         BNE   MOSE46                                                           
         CLI   QOPT1-1,C'D'                                                     
         BNE   MOSE46                                                           
         GOTO1 DATCON,DMCB,(2,MYKMOS),(3,WORK)                                  
         MVC   BUFMOS,WORK               ONLY USE Y/M IN TOTALS                 
**NEW 1/25/89                                                                   
MOSE46   TM    ESTTBSW,X'04'       EST WITHIN MOS ON PRD SUMMARY                
         BZ    *+10                NO                                           
         MVC   BUFEST(3),=3X'FF'                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   BUFTYP,X'04'                                                     
         GOTO1 (RF)                                                             
         XC    BUFEST(3),BUFEST                                                 
***OFF                                                                          
         CLI   MULTOFF,C'Y'        SEE IF DOING OFFICE LIST REQ                 
         BNE   MOSE48                                                           
         MVI   BUFTYP,X'05'        OFFICE TOTALS                                
         MVC   BUFCLT,PCLTKCLT                                                  
         GOTO1 (RF)                                                             
         MVC   BUFCLT,=3X'FF'                                                   
         GOTO1 (RF)                                                             
***OFF                                                                          
MOSE48   MVI   BUFTYP,X'06'                                                     
         MVC   BUFCLT,PCLTKCLT                                                  
***OFF                                                                          
         CLI   MULTOFF,C'Y'        SEE IF DOING OFFICE LIST REQ                 
         BNE   MOSE49                                                           
*                                                                               
         CLI   QOPT7,C'C'     CLIENT TOTALS (OVERRIDE OF PROGPROF+8)            
         BE    MOSE49                                                           
*                                                                               
         CLI   QOPT7,C'N'     OFFICE TOTALS (OVERRIDE OF PROGPROF+8)            
         BE    MOSE48C                                                          
*                                                                               
         CLC   QCLIENT(2),=C'$*'   SEE IF ALL OFFICE IN OFFICE ORDER            
         BE    MOSE48C             ALWAYS DO OFFICE                             
*                                                                               
         CLI   PROGPROF+8,C'C'     CLT TOTALS IN OFFICE LIST SUMMARY            
         BE    MOSE49                                                           
*                                                                               
MOSE48C  XC    BUFCLT,BUFCLT                                                    
         MVC   BUFCLT(2),SAVCOFF                                                
***OFF                                                                          
MOSE49   GOTO1 (RF)                                                             
         MVC   BUFCLT,=3X'FF'                                                   
         GOTO1 (RF)                                                             
         XC    BUFCLT,BUFCLT                                                    
MOSE50   DS    0H                                                               
         MVI   BUFTYP,X'02'                                                     
         MVC   BUFMOS,=2X'FF'                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   BUFTYP,X'04'                                                     
         GOTO1 (RF)                                                             
***OFF                                                                          
         CLI   MULTOFF,C'Y'                                                     
         BNE   MOSE55                                                           
         MVI   BUFTYP,X'05'                                                     
         MVC   BUFCLT,PCLTKCLT                                                  
         GOTO1 (RF)                                                             
         MVC   BUFCLT,=3X'FF'                                                   
         GOTO1 (RF)                                                             
*                                                                               
***OFF                                                                          
MOSE55   MVI   BUFTYP,X'06'                                                     
         MVC   BUFCLT,PCLTKCLT                                                  
***OFF                                                                          
         CLI   MULTOFF,C'Y'    SEE IF OFFICE LIST REQ                           
         BNE   MOSE57                                                           
*                                                                               
*                                                                               
         CLI   QOPT7,C'C'     CLIENT TOTALS (OVERRIDE OF PROGPROF+8)            
         BE    MOSE57                                                           
*                                                                               
         CLI   QOPT7,C'N'     OFFICE TOTALS (OVERRIDE OF PROGPROF+8)            
         BE    MOSE55C                                                          
*                                                                               
         CLC   QCLIENT(2),=C'$*'   SEE IF ALL OFFICE IN OFFICE ORDER            
         BE    MOSE55C             ALWAYS DO OFFICE                             
*                                                                               
         CLI   PROGPROF+8,C'C'     CLT TOTALS IN OFFICE LIST SUMMARY            
         BE    MOSE57                                                           
*                                                                               
MOSE55C  XC    BUFCLT,BUFCLT                                                    
         MVC   BUFCLT(2),SAVCOFF                                                
***OFF                                                                          
MOSE57   GOTO1 (RF)                                                             
         MVC   BUFCLT,=3X'FF'                                                   
         GOTO1 (RF)                                                             
         XC    BUFCLT,BUFCLT                                                    
         LARL  R2,PDTAB                                                         
         MVC   0(8,R2),=8X'FF'                                                  
         ST    R2,ANXTPD                                                        
         LARL  R2,INVTAB                                                        
         MVC   0(5,R2),=5X'FF'                                                  
         ST    R2,ANXTBL                                                        
*                                  ADD TO PUB TOTALS                            
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
         EJECT                                                                  
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
         EJECT                                                                  
PRNTSTA  DS    0H                                                               
         MVC   P(7),=C'CUR REV'                                                 
         CLC   MYKPUB(4),=4X'FF'       DUMMY PUBS                               
         BNE   PRNTS2                                                           
         CLI   MYKPUB+4,X'FF'      UNREV ORIG BILL                              
         BNE   PRNTSX                                                           
         MVC   P(7),=CL7'UNREV '         UNREVERSED ORIG BILL                   
         B     PRNTSX                                                           
*                                                                               
PRNTS2   CLC   MYKPUB(5),=C'ZZZZX'    NEW MANUAL BILLING                        
         BNE   PRNTS4                                                           
         MVC   P(6),=C'MANUAL'                                                  
         B     PRNTSX                                                           
*                                                                               
PRNTS4   CLC   MYKPUB(3),=C'ALL'                                                
         BNE   PRNTS5                                                           
         MVC   P(8),=C'ALL PUBS'                                                
         B     PRNTSX                                                           
*                                                                               
PRNTS5   DS    0H                  NEEDS AT LEAST 3 LINES                       
         ZIC   R1,LINE             SO NAME ALONE WON'T PRINT                    
         LA    R1,3(R1)                                                         
         STC   R1,X                                                             
         CLC   X(1),MAXLINES                                                    
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'       MUST FORCE NEW PAGE                          
*                                                                               
         LR    R0,RE               SAVE RETURN REG                              
         IC    R2,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R2),MYKPUB),(C'S',P)                              
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,MYPDISK,PUBREC,DMWORK                
         TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R3,P                                                             
PRNTS6   CLI   0(R3),C' '                                                       
         BNH   PRNTS8                                                           
         LA    R3,1(R3)                                                         
         B     PRNTS6                                                           
*                                                                               
PRNTS8   LA    R3,1(R3)                                                         
         GOTO1 VPUBFLT,DMCB,PUBREC,(R3)                                         
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                  MUST PRINT PUB ON SEPERATE LINE              
         LR    RE,R0               RESTORE RE                                   
*                                                                               
         DROP  R7                                                               
*                                                                               
PRNTSX   BR    RE                                                               
         LTORG                                                                  
         DROP  RB,R8                                                            
         EJECT                                                                  
MTH13    NTR1  BASE=*,LABEL=*      PRINTS BILLING PERIODS NOT MTHS              
         ZIC   R0,1(R2)            R2 POINTS TO BINARY YM                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R3),DUB         R3 POINTS TO PRINT LOCATION                  
*                                                                               
         JIF   PROGPROF+13,EQ,C'M',AND,QOPT5,EQ,C'P',MTH13D                     
         JIF   PROGPROF+13,EQ,C'Y',AND,QOPT5,EQ,C'P',MTH13E                     
         JIF   PROGPROF+13,EQ,C'D',AND,QOPT5,EQ,C'P',MTH13H                     
*                                                                               
         MVI   2(R3),C'/'                                                       
         ZIC   R0,0(R2)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(2,R3),DUB                                                      
         B     MTH13X                                                           
*                                                                               
MTH13D   ZIC   R0,0(R2)         DOWNLOADING WITH MMYY FORMAT                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(2,R3),DUB                                                      
         B     MTH13X                                                           
*                                                                               
MTH13E   ZIC   R0,0(R2)         DOWNLOADING WITH YYMM FORMAT                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R3),DUB                                                      
         ZIC   R0,1(R2)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(2,R3),DUB                                                      
         B     MTH13X                                                           
*                                                                               
MTH13H   DS    0H                                                               
         MVI   2(R3),C'/'         MM/DD/YY                                      
         MVC   3(2,R3),=C'31'     SET TO 31 TO SEE IF DATVAL LIKES IT           
         MVI   5(R3),C'/'                                                       
         ZIC   R0,0(R2)           BINARY YEAR                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  6(2,R3),DUB                                                      
         CLC   0(2,R3),=C'13'     MONTH 13?                                     
         BNE   *+10               LEAVE DAY AT 31                               
         MVC   0(2,R3),=C'12'     CHANGES TO DEC (12)                           
*                                                                               
         GOTO1 VDATVAL,DMCB,(0,0(R3)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BNZ   MTH13HX                                                          
         MVC   3(2,R3),=C'30'     NOW TRY 30                                    
         GOTO1 VDATVAL,DMCB,(0,0(R3)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BNZ   MTH13HX                                                          
         MVC   3(2,R3),=C'28'     NOW TRY 28                                    
         GOTO1 VDATVAL,DMCB,(0,0(R3)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BNZ   MTH13HX                                                          
         MVC   3(2,R3),=C'29'     NOW TRY 29 - FEB IN LEAP YEAR                 
         GOTO1 VDATVAL,DMCB,(0,0(R3)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BNZ   MTH13HX                                                          
         DC    H'0'        BAD DATE                                             
*                                                                               
MTH13HX  DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(X'20',WORK+6)                              
         MVC   WORK+12(6),WORK+6    CONVERT YYMMDD TO MMDDYY                    
         MVC   WORK(2),WORK+12+2    MM FIRST                                    
         MVC   WORK+2(2),WORK+12+4  DD SECOND                                   
         MVC   WORK+4(2),WORK+12    YY LAST                                     
*                                                                               
         MVC   0(6,R3),WORK        MMDDYY                                       
         MVC   6(2,R3),SPACES                                                   
MTH13X   XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
PRDEND   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   QOPT4,C'B'          SUMMARY REPORT                               
         BE    PRDEX                                                            
         CLI   QOPT4,C'D'          SUMMARY REPORT                               
         BE    PRDEX               DOWNLOADED                                   
         CLI   QOPT5,C'P'          PUBLICATION DOWNLOAD                         
         BE    PRDEX               EXIT                                         
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
PRDEX    XIT1                                                                   
*                                                                               
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
         EDIT  (P8,PRDBALO),(13,P+89),2,COMMAS=YES,FLOAT=-                      
*                                                                               
         EDIT  (P8,PRDBILLG),(13,P+104),2,COMMAS=YES,FLOAT=-                    
         EDIT  (P8,PRDPAYG),(13,P+117),2,COMMAS=YES,FLOAT=-                     
         MVC   P+34(2),=2C'*'                                                   
         MVC   P+49(2),=2C'*'                                                   
         MVC   P+72(2),=2C'*'                                                   
         MVC   P+102(2),=2C'*'                                                  
         MVC   P+130(2),=2C'*'                                                  
         CLI   P+117,C' '          SOMETHING MAY BE THERE                       
         BH    CLTE15                                                           
         MVI   P+117,C'*'                                                       
         CLI   P+118,C' '                                                       
         BH    CLTE15                                                           
         MVI   P+118,C'*'                                                       
CLTE15   DS    0H                                                               
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
         CLI   QOPT5,C'P'          PUBLICATION DOWNLOAD                         
         BE    CLTE25                                                           
*                                                                               
         CLI   QOPT2,C'Y'                                                       
         BE    CLTE50              ALL PRDS TOGETHER - SKIP CLT TOTALS          
*                                                                               
         CLI   QCONTREQ,C'*'       CONTINUATION CARD PRESENT?                   
         BE    CLTE25              FOR NOW COULD BE USED TO                     
*                                  CHECK FOR PRODUCT GROUP REQ                  
*                                  AS THAT'S THE ONLY DATA THAT                 
*                                  COULD BE THERE (FOR NOW)                     
         CLC   QPRODUCT,=C'ALL'    ONLY ONE PRD - SKIP CLT TOTALS               
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
PUBEND   NMOD1 0,PUBEND            PUB TOTALS                                   
         L     RC,0(R1)                                                         
         USING PPWORKD,RC                                                       
         L     RA,PPFILEC                                                       
         USING PPFILED,RA                                                       
         LA    R9,SPACEND                                                       
         USING PPA8WRKD,R9                                                      
         CLI   STAACT,C'Y'                                                      
         BNE   STAEX                                                            
         CLI   QOPT3,C'Y'          SEE IF SHOWING ALL PUBS                      
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
         CLI   QOPT5,C'P'          SEE IF PUBLICATION DOWNLOAD                  
         BE    STAE9                                                            
*                                                                               
         CLI   STAPSW,C'Y'         SEE IF I'VE PRINTED PUB                      
         BNE   STAE7               NO                                           
         ZIC   R3,LINE             REPEAT IF NEW PAGE                           
         LA    R3,3(R3)            NEEDS AT LEAST 3 LINES                       
         STC   R3,X                                                             
         CLC   X(1),MAXLINES                                                    
         BNH   STAE8                                                            
*                                                                               
STAE7    DS    0H                                                               
         CLI   QOPT4,C'B'        PRD SUMMARY VERSION                            
         BE    STAE7C                                                           
         CLI   QOPT4,C'D'        PRD SUMMARY VERSION                            
         BNE   STAE7X            DOWNLOADED                                     
         MVI   P,C'P'       SYSTEM PRINTPAK                                     
         MVC   P+2(1),QMEDIA    MEDIA                                           
         MVC   P+4(L'SAVCOFF),SAVCOFF                                           
*                                                                               
         MVC   P+130(L'SAVAOFF),SPACES                                          
         CLI   PROFA8A,C'Y'            DOWNLOAD ACC OFFICE                      
         BNE   *+10                                                             
         MVC   P+130(L'SAVAOFF),SAVAOFF                                         
*                                                                               
         MVC   P+6(3),MYKPUB     CLIENT                                         
         MVC   P+10(3),MYKPUB+3  PRODUCT                                        
         B     STAE8A                                                           
*                                                                               
STAE7C   DS    0H                                                               
         MVC   P(3),MYKPUB+3         PRODUCT IN PRINT LINE                      
         LARL  R2,PRDTAB                                                        
STAE7D   CLI   0(R2),X'FF'         END OF TABLE                                 
         BNE   STAE7E                                                           
         B     STAE7H              PRD NOT IN TABLE                             
*                                IS OK  - NET UNITS DON'T SET PRDTAB            
STAE7E   CLC   0(3,R2),MYKPUB+3                                                 
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
STAE7X   BAS   RE,SRNTSTA                                                       
STAE8    MVC   P+9(7),=C'TOTAL**'                                               
*                                                                               
STAE8A   DS    0H                                                               
         MVI   SPACING,2                                                        
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
         EDIT  (P8,DOUBLE),(13,P+89),2,COMMAS=YES,FLOAT=-                       
*                                                                               
*        EDIT BELOW NEEDED IF DOWNLOADING (QOPT4 = B)                           
*                                                                               
         EDIT  (P8,DOUBLE),(12,DOLS),MINUS=YES,ZERO=NOBLANK                     
         CLI   PROGPROF+10,C'Y'    2 DECIMALS?                                  
         BNE   STAE8AX                                                          
         EDIT  (P8,DOUBLE),(12,DOLS),2,MINUS=YES,ZERO=NOBLANK                   
*                                                                               
STAE8AX  EDIT  (P8,SCBILLG),(13,P+104),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,SCPAYG),(13,P+117),2,COMMAS=YES,FLOAT=-                      
         MVC   P+34(2),=2C'*'                                                   
         MVC   P+49(2),=2C'*'                                                   
         MVC   P+72(2),=2C'*'                                                   
         MVC   P+102(2),=2C'*'                                                  
         MVC   P+130(2),=2C'*'                                                  
         CLI   P+117,C' '          SOMETHING MAY BE THERE                       
         BH    STAE8B                                                           
         MVI   P+117,C'*'                                                       
         CLI   P+118,C' '                                                       
         BH    STAE8B                                                           
         MVI   P+118,C'*'                                                       
STAE8B   CLI   QOPT4,C'B'          PRD SUMMARY VERSION                          
         BE    STAE8D                                                           
         CLI   QOPT4,C'D'          PRD SUMMARY DOWNLOADED                       
         BE    STAE8C                                                           
         CLI   QOPT4,C'M'          NO MTH OF SERVICE BREAKOUT                   
         BNE   STAE9                                                            
STAE8C   MVI   P+15,C' '           USE ONLY ONE *                               
STAE8D   MVI   P+35,C' '                                                        
         MVI   P+50,C' '                                                        
         MVI   P+73,C' '                                                        
         MVI   P+103,C' '                                                       
         MVI   P+131,C' '                                                       
         CLI   P+118,C'*'                                                       
         BNE   STAE9               IF IT WASN'T A * LEAVE IT ALONE              
         MVI   P+118,C' '                                                       
*                                                                               
STAE9    DS    0H                                                               
         CLI   QOPT5,C'P'     PUBLICATION DOWNLOAD                              
         BE    STAE9S                                                           
*                                                                               
         CLI   QOPT4,C'D'          SPECIAL DOWNLOAD VERSION                     
         BNE   STAE9X                                                           
*                                                                               
         GOTO1 VDOWNLD,DMCB,(RC)                                                
         MVC   P,SPACES          CLEAR PRINT LINE                               
         B     STAE10                                                           
*                                                                               
STAE9S   DS    0H                                                               
         ZAP   DOUBLE,SPBILLN     BALANCE FORMARD                               
         SP    DOUBLE,SPPAYN                                                    
         CLI   PROGPROF+10,C'Y'    2 DECIMALS IN $ FIELDS?                      
         BE    STAE9S5                                                          
         CLI   PROGPROF+11,C'Y'    MINUS SIGN BEFORE $?                         
         BE    STAE9S3                                                          
         EDIT  (P8,DOUBLE),(12,SBALF),MINUS=YES,ZERO=NOBLANK                    
         EDIT  (P8,SCBILLN),(12,SCBIL),MINUS=YES,ZERO=NOBLANK                   
         EDIT  (P8,SCPAYN),(12,SCPAY),MINUS=YES,ZERO=NOBLANK                    
         B     STAE9S10                                                         
*                                                                               
STAE9S3  EDIT  (P8,DOUBLE),(12,SBALF),ZERO=NOBLANK,FLOAT=-                      
         EDIT  (P8,SCBILLN),(12,SCBIL),ZERO=NOBLANK,FLOAT=-                     
         EDIT  (P8,SCPAYN),(12,SCPAY),ZERO=NOBLANK,FLOAT=-                      
         B     STAE9S10                                                         
*                                                                               
STAE9S5  DS    0H                                                               
         CLI   PROGPROF+11,C'Y'    MINUS SIGN BEFORE $?                         
         BE    STAE9S7                                                          
         EDIT  (P8,DOUBLE),(12,SBALF),2,MINUS=YES,ZERO=NOBLANK                  
         EDIT  (P8,SCBILLN),(12,SCBIL),2,MINUS=YES,ZERO=NOBLANK                 
         EDIT  (P8,SCPAYN),(12,SCPAY),2,MINUS=YES,ZERO=NOBLANK                  
         B     STAE9S10                                                         
*                                                                               
STAE9S7  EDIT  (P8,DOUBLE),(12,SBALF),2,ZERO=NOBLANK,FLOAT=-                    
         EDIT  (P8,SCBILLN),(12,SCBIL),2,ZERO=NOBLANK,FLOAT=-                   
         EDIT  (P8,SCPAYN),(12,SCPAY),2,ZERO=NOBLANK,FLOAT=-                    
STAE9S10 DS    0H                                                               
         AP    DOUBLE,SCBILLN                                                   
         SP    DOUBLE,SCPAYN                                                    
         CLI   PROGPROF+10,C'Y'   2 DECIMALS IN $ FIELDS?                       
         BE    STAE9S15                                                         
         CLI   PROGPROF+11,C'Y'    MINUS SIGN BEFORE $?                         
         BE    STAE9S13                                                         
         EDIT  (P8,DOUBLE),(12,SBALO),MINUS=YES,ZERO=NOBLANK                    
         B     STAE9S20                                                         
*                                                                               
STAE9S13 EDIT  (P8,DOUBLE),(12,SBALO),ZERO=NOBLANK,FLOAT=-                      
         B     STAE9S20                                                         
*                                                                               
STAE9S15 DS    0H                                                               
         CLI   PROGPROF+11,C'Y'    MINUS SIGN BEFORE $?                         
         BE    STAE9S17                                                         
         EDIT  (P8,DOUBLE),(12,SBALO),2,MINUS=YES,ZERO=NOBLANK                  
         B     STAE9S20                                                         
*                                                                               
STAE9S17 EDIT  (P8,DOUBLE),(12,SBALO),2,ZERO=NOBLANK,FLOAT=-                    
*                                                                               
STAE9S20 DS    0H                                                               
*                                                                               
         MVC   P,SPACES          CLEAR PRINT LINE                               
         MVC   P+30(7),=C'CUR REV'                                              
         CLC   MYKPUB(4),=4X'FF'                                                
         BNE   STAE9S21                                                         
         CLI   MYKPUB+4,X'FF'                                                   
         BNE   STAE9S24                                                         
         MVC   P+30(7),=CL7'UNREV'                                              
         B     STAE9S24                                                         
*                                                                               
STAE9S21 CLC   MYKPUB(5),=C'ZZZZX'        NEW MANUAL                            
         BNE   STAE9S22                                                         
         MVC   P+30(7),=CL7'MANUAL'                                             
         B     STAE9S24                                                         
*                                                                               
STAE9S22 DS    0H                                                               
         CLC   MYKPUB(3),=C'ALL'                                                
         BNE   STAE9S23                                                         
         MVC   P+30(7),=CL7'ALL'                                                
         B     STAE9S24                                                         
*                                                                               
STAE9S23 DS    0H                                                               
         MVC   P+110(20),MYKPNM     PUB SORT NAME                               
         IC    R2,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R2),MYKPUB),(C'S',P+30)                           
STAE9S24 DS    0H                                                               
         MVC   P(14),SPACES                                                     
         MVI   P,C'P'        SYSTEM - PRINT                                     
         MVC   P+2(1),QMEDIA                                                    
         MVC   P+4(L'SAVCOFF),SAVCOFF                                           
*                                                                               
         MVC   P+130(L'SAVAOFF),SPACES                                          
         CLI   PROFA8A,C'Y'            DOWNLOAD ACC OFFICE                      
         BNE   *+10                                                             
         MVC   P+130(L'SAVAOFF),SAVAOFF                                         
*                                                                               
         MVC   P+6(3),PCLTKCLT        CLIENT                                    
         MVC   P+10(3),MYKPRD         PRODUCT                                   
         CLI   QOPT2,C'Y'             SEE IF COMBINING PRDS                     
         BNE   *+10                                                             
         MVC   P+10(3),=C'ALL'                                                  
         GOTO1 VDOWNLD,DMCB,(RC)                                                
         MVC   P,SPACES                                                         
         B     STAE10                                                           
*                                                                               
STAE9X   GOTO1 VPRINTIT,DMCB,(RC)                                               
STAE10   LA    R2,STATOTS                                                       
         LA    R3,6                                                             
STAE15   ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,STAE15                                                        
         MVI   STAACT,0                                                         
         MVI   STAPSW,0                                                         
STAEX    XIT1                                                                   
*                                                                               
SRNTSTA  DS    0H                                                               
         MVC   P(7),=C'CUR REV'                                                 
         CLC   MYKPUB(4),=4X'FF'       DUMMY PUBS                               
         BNE   SRNTS2                                                           
         CLI   MYKPUB+4,X'FF'      UNREV ORIG BILL                              
         BNE   SRNTSX                                                           
         MVC   P(7),=CL7'UNREV '         UNREVERSED ORIG BILL                   
         B     SRNTSX                                                           
*                                                                               
SRNTS2   CLC   MYKPUB(5),=C'ZZZZX'    NEW MANUAL BILLING                        
         BNE   SRNTS4                                                           
         MVC   P(6),=C'MANUAL'                                                  
         B     SRNTSX                                                           
*                                                                               
SRNTS4   CLC   MYKPUB(3),=C'ALL'                                                
         BNE   SRNTS5                                                           
         MVC   P(8),=C'ALL PUBS'                                                
         B     SRNTSX                                                           
*                                                                               
SRNTS5   DS    0H                  NEEDS AT LEAST 3 LINES                       
         ZIC   R1,LINE             SO NAME ALONE WON'T PRINT                    
         LA    R1,3(R1)                                                         
         STC   R1,X                                                             
         CLC   X(1),MAXLINES                                                    
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'       MUST FORCE NEW PAGE                          
*                                                                               
         LR    R0,RE               SAVE RETURN REG                              
         IC    R2,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R2),MYKPUB),(C'S',P)                              
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,MYPDISK,PUBREC,DMWORK                
         TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R3,P                                                             
SRNTS6   CLI   0(R3),C' '                                                       
         BNH   SRNTS8                                                           
         LA    R3,1(R3)                                                         
         B     SRNTS6                                                           
*                                                                               
SRNTS8   LA    R3,1(R3)                                                         
         GOTO1 VPUBFLT,DMCB,PUBREC,(R3)                                         
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                  MUST PRINT PUB ON SEPERATE LINE              
         LR    RE,R0               RESTORE RE                                   
*                                                                               
         DROP  R7                                                               
*                                                                               
SRNTSX   BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
BTOTALS  NMOD1 0,BTOTALS                                                        
         L     RC,0(R1)                                                         
         USING PPWORKD,RC                                                       
         L     RA,PPFILEC                                                       
         USING PPFILED,RA                                                       
         LA    R9,SPACEND                                                       
         USING PPA8WRKD,R9                                                      
         MVC   BUFTYP,TOTTYP                                                    
         XC    LASTMOS,LASTMOS                                                  
         XC    LASTCLT,LASTCLT                                                  
         MVI   CLTPSW,0                                                         
         XC    ESTCNT,ESTCNT                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     BTOT10                                                           
BTOT5    GOTO1 BUFFALO,DMCB,=C'SEQ',(TOTTYP,BUFFBUFF),BUFREC,0                  
*                                                                               
BTOT10   CLI   DMCB+8,X'80'        END                                          
         BE    BTOTX                                                            
         CLC   BUFMOS,=2X'FF'          TOTAL LINE                               
         BE    BTOT25                                                           
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
         OC    BUFEST(2),BUFEST                                                 
         BE    BTOT17                                                           
         CLI   BUFEST+2,X'FF'      MOS TOTAL                                    
         BE    BTOT18                                                           
         ZIC   R0,BUFEST                                                        
         SLL   R0,8                                                             
         IC    R0,BUFEST+1                                                      
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
         CLC   BUFEST(3),=3X'FF'                                                
         BNE   BTOT22                                                           
         CLI   QOPT4,C'M'          NO MTH OF SERV BREAKOUT                      
         BE    BTOT5               SKIP MOS TOTAL LINE                          
         CLI   QOPT4,C'B'          PRODUCT SUMMARY VERSION                      
         BE    BTOT5               SKIP MOS TOTAL LINE                          
         CLI   QOPT4,C'D'          PRODUCT SUMMARY DOWNLOADED                   
         BE    BTOT5               SKIP MOS TOTAL LINE                          
         CLC   ESTCNT,=H'1'                                                     
         BH    BTOT19                                                           
         XC    ESTCNT,ESTCNT       SKIP MOS TOTALS IF ONLY 1 EST                
         B     BTOT5                                                            
*                                                                               
BTOT19   DS    0H                                                               
         XC    ESTCNT,ESTCNT                                                    
*                                                                               
BTOT20   DS    0H                                                               
***OFF                                                                          
         CLI   TOTTYP,X'05'        AGENCY OR OFFICE TOTALS?                     
         BL    BTOT20C             NO                                           
***OFF                                                                          
         CLC   LASTCLT,BUFCLT      YES                                          
         BE    BTOT20C                                                          
         JIF   BUFCLT,NE,=3X'00',AND,BUFCLT,NE,=3X'FF',BTOT20A                  
         CLI   QOPT4,C'B'          PRD SUMMARY VERSION                          
         BE    BT20A                                                            
         CLI   QOPT4,C'D'          PRD SUMMARY DOWNLOADED                       
         BE    BT20A                                                            
         CLI   QOPT4,C'M'          NO MTH OF SERV                               
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
BT20A    MVC   P(3),=C'ALL'                                                     
         B     BTOT20B                                                          
*                                                                               
BTOT20A  MVC   P(3),BUFCLT                                                      
BTOT20B  MVC   LASTCLT,BUFCLT                                                   
         MVI   CLTPSW,1                                                         
         MVC   SVPCLT,P            SAVE CLT CODE FOR NEW PAGE                   
*                                                                               
BTOT20C  DS    0H                                                               
***OFF                                                                          
         CLI   TOTTYP,X'05'        REQ OR OFF TOTALS                            
         BL    BTOT20F                                                          
***OFF                                                                          
         ZIC   R1,LINE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,X                                                             
         CLC   X(1),MAXLINES                                                    
         BNH   *+10                                                             
         MVC   P(3),SVPCLT                                                      
*                                                                               
BTOT20F  DS    0H                                                               
         CLI   QOPT4,C'M'          NO MTH OF SERV BREAKOUT                      
         BE    BTOT22                                                           
         CLI   QOPT4,C'B'          PRD SUMMARY VERSION                          
         BE    BTOT22                                                           
         CLI   QOPT4,C'D'          PRD SUMMARY DOWNLOADED                       
         BE    BTOT22                                                           
         CLI   BUFMOS,X'FF'       IF DOING INS MTH CUR REV                      
         BNE   BTOT20H             AND UNREV ORIG BILLS                         
*                                  HAVE BUFMOS SET TO X'FF00'                   
         MVC   P+3(8),=C'CUR REV+'                                              
         MVC   PSECOND+3(9),=C'UNREV ORI'                                       
         B     BTOT22                                                           
*                                                                               
BTOT20H  DS    0H                                                               
*                                                                               
         CLI   BUFMOS+1,12         13TH MTH                                     
         BNH   BTOT21                                                           
BTOT20I  LA    R2,BUFMOS                                                        
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
         CLC   BUFEST(3),=3X'FF'                                                
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
         CLI   QOPT4,C'B'          PRD SUMMARY VERSION                          
         BE    BTOT25C             MTH BREAKOUT - SKIP A LINE                   
         CLI   QOPT4,C'D'          PRD SUMMARY DOWNLOADED                       
         BE    BTOT25C             MTH BREAKOUT - SKIP A LINE                   
         CLI   QOPT4,C'M'                                                       
         BNE   BTOT27              MTH BREAKOUT - SKIP A LINE                   
BTOT25C  CLI   TOTTYP,4            SEE IF DOING CLT OR HIGHER                   
         BH    BTOT29              DON'T SKIP                                   
         TM    ESTTBSW,4           EST TOTALS                                   
         BZ    BTOT29                                                           
         B     BTOT27A                                                          
*                                                                               
***OFF                                                                          
BTOT27   CLI   TOTTYP,X'05'        REQ OR OFFICE TOTALS                         
         BL    BTOT27A                                                          
***OFF                                                                          
         CLC   BUFCLT,=3X'FF'                                                   
         BNE   BTOT28                                                           
BTOT27A  GOTO1 VPRINTIT,DMCB,(RC)  SKIP A LINE                                  
BTOT28   MVC   P+5(6),=C'TOTAL*'                                                
BTOT29   DS    0H                                                               
***OFF                                                                          
         CLI   TOTTYP,X'05'        REQ OR OFFICE TOTALS                         
         BL    BTOT29F                                                          
***OFF                                                                          
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
         BNH   *+10                                                             
         MVC   P(3),SVPCLT                                                      
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
***OFF                                                                          
         CLI   TOTTYP,X'05'        SEE IF DOING OFF TOTALS                      
         BE    BTOT29H                                                          
***OFF                                                                          
         CLI   TOTTYP,X'06'        SEE IF DOING REQ TOTALS                      
         BE    BTOT29H                                                          
         CLI   QOPT4,C'B'          PRD SUMMARY VERSION                          
         BE    BTOT30              NO MOS                                       
         CLI   QOPT4,C'D'          PRD SUMMARY DOWNLOADED                       
         BE    BTOT30              NO MOS                                       
         CLI   QOPT4,C'M'          USE 2* IF SHOWING MOS + EST                  
         BE    BTOT30              NO MOS                                       
         TM    ESTTBSW,X'04'                                                    
         BZ    BTOT30              NO EST                                       
         B     BTOT29J             GO USE 2 *                                   
*                                                                               
BTOT29H  CLC   BUFCLT,=3X'FF'                                                   
         BNE   BTOT30                                                           
         CLI   QOPT4,C'B'          PRD SUMMARY VERSION                          
         BE    BTOT29K                                                          
         CLI   QOPT4,C'D'          PRD SUMMARY DOWNLOADED                       
         BE    BTOT29K                                                          
         CLI   QOPT4,C'M'          NO MOS - SKIP THIS *                         
         BE    *+8                                                              
BTOT29J  MVI   P+11,C'*'                                                        
BTOT29K  MVI   P+35,C'*'                                                        
         MVI   P+52,C'*'                                                        
         MVI   P+70,C'*'                                                        
         MVI   P+86,C'*'                                                        
         MVI   P+104,C'*'                                                       
         MVI   P+121,C'*'                                                       
BTOT30   DS    0H                                                               
*                                                                               
***OFF                                                                          
         CLI   TOTTYP,X'05'        REQ OR OFFICE TOTALS                         
         BL    BTOT32                                                           
***OFF                                                                          
         MVI   SPACING,2                                                        
         CLI   PROGPROF+5,C'Y'     SEE IF TRIPLE SPACING CLTS                   
         BNE   *+8                                                              
         MVI   SPACING,3                                                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         CLC   BUFCLT,=3X'FF'                                                   
         BNE   BTOT5               GO DO NEXT CLT                               
         B     BTOT35                                                           
*                                                                               
BTOT32   DS    0H                                                               
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         CLI   TOTTYP,4            SEE IF CLT OR HIGHER                         
         BNL   BTOT35                                                           
         CLI   QOPT2,C'Y'          OR DOING ALL PRDS TOGETHER                   
         BE    BTOT35                                                           
*                                                                               
         CLI   QCONTREQ,C'*'       CONTINUATION CARD PRESENT?                   
         BE    BTOTX               FOR NOW COULD BE USED TO                     
*                                  CHECK FOR PRODUCT GROUP REQ                  
*                                  AS THAT'S THE ONLY DATA THAT                 
*                                  COULD BE THERE (FOR NOW)                     
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    BTOTX                                                            
*                                  ONE PRD DO TOTALS                            
*                                                                               
BTOT35   DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 VPRINTIT,DMCB,(RC)  SKIP 2 LINES                                 
         MVC   P+1(12),=C'NET BILLINGS'                                         
         ZAP   DOUBLE,BUFPBILN                                                  
         BAS   RE,BTOTEDIT                                                      
         MVC   P+17(17),WORK                                                    
         ZAP   DOUBLE,BUFPBILN                                                  
         AP    DOUBLE,BUFCBILN                                                  
         BAS   RE,BTOTEDIT                                                      
         MVC   P+68(17),WORK                                                    
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+1(14),=C'NET CLEARANCES'                                       
         ZAP   DOUBLE,BUFPPAYN                                                  
         BAS   RE,BTOTEDIT                                                      
         MVC   P+17(17),WORK                                                    
         ZAP   DOUBLE,BUFPPAYN                                                  
         AP    DOUBLE,BUFCPAYN                                                  
         BAS   RE,BTOTEDIT                                                      
         MVC   P+68(17),WORK                                                    
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+1(14),=C'GROSS BILLINGS'                                       
         ZAP   DOUBLE,BUFPBILG                                                  
         BAS   RE,BTOTEDIT                                                      
         MVC   P+17(17),WORK                                                    
         ZAP   DOUBLE,BUFPBILG                                                  
         AP    DOUBLE,BUFCBILG                                                  
         BAS   RE,BTOTEDIT                                                      
         MVC   P+68(17),WORK                                                    
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+1(16),=C'GROSS CLEARANCES'                                     
         ZAP   DOUBLE,BUFPPAYG                                                  
         BAS   RE,BTOTEDIT                                                      
         MVC   P+17(17),WORK                                                    
         ZAP   DOUBLE,BUFPPAYG                                                  
         AP    DOUBLE,BUFCPAYG                                                  
         BAS   RE,BTOTEDIT                                                      
         MVC   P+68(17),WORK                                                    
         MVI   SPACING,2                                                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
         CLI   PAGYNAT,C'C'       SEE IF CANADIAN AGY                           
         BNE   BTOT35X                                                          
         LA    R5,CGSTTOTS                                                      
         CLI   TOTTYP,X'04'        CLIENT TOTALS                                
         BE    BTOT35C                                                          
         CLI   QOPT2,C'Y'          SEE IF COMBINING PRDS                        
         BNE   BTOT35B                                                          
         CLI   TOTTYP,X'02'                                                     
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
BTOT35C  MVC   P+3(16),=C'PREV. BILLED GST'                                     
         EDIT  (P8,0(R5)),(14,P+20),2,COMMAS=YES,FLOAT=-                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+3(14),=C'PREV. PAID GST'                                       
         EDIT  (P8,8(R5)),(14,P+20),2,COMMAS=YES,FLOAT=-                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+3(16),=C'CURR. BILLED GST'                                     
         EDIT  (P8,16(R5)),(14,P+20),2,COMMAS=YES,FLOAT=-                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+3(14),=C'CURR. PAID GST'                                       
         EDIT  (P8,24(R5)),(14,P+20),2,COMMAS=YES,FLOAT=-                       
         MVI   SPACING,2                                                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
***OFF                                                                          
BTOT35P  DS    0H                  NOW DO PST TOTLAS                            
         LA    R5,CPSTTOTS                                                      
         CLI   TOTTYP,X'04'        CLIENT TOTALS                                
         BE    BTOT35R                                                          
         CLI   QOPT2,C'Y'          SEE IF COMBINING PRDS                        
         BNE   BTOT35Q                                                          
         CLI   TOTTYP,X'02'                                                     
         BE    BTOT35R                                                          
*                                                                               
BTOT35Q  LA    R5,OPSTTOTS                                                      
         CLI   TOTTYP,X'05'        OFFICE TOTALS                                
         BE    BTOT35R                                                          
         LA    R5,RPSTTOTS                                                      
         CLI   TOTTYP,X'06'        REPORT TOTALS                                
         BE    BTOT35R                                                          
         B     BTOT35X                                                          
*                                                                               
BTOT35R  MVC   P+3(16),=C'PREV. BILLED PST'                                     
         EDIT  (P8,0(R5)),(14,P+20),2,COMMAS=YES,FLOAT=-                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+3(14),=C'PREV. PAID PST'                                       
         EDIT  (P8,8(R5)),(14,P+20),2,COMMAS=YES,FLOAT=-                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+3(16),=C'CURR. BILLED PST'                                     
         EDIT  (P8,16(R5)),(14,P+20),2,COMMAS=YES,FLOAT=-                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+3(14),=C'CURR. PAID PST'                                       
         EDIT  (P8,24(R5)),(14,P+20),2,COMMAS=YES,FLOAT=-                       
         MVI   SPACING,2                                                        
         GOTO1 VPRINTIT,DMCB,(RC)                                               
***OFF                                                                          
BTOT35X  CLI   TOTTYP,X'05'        REQ OR OFFICE TOTALS                         
         BL    BTOT50                                                           
***OFF                                                                          
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
*                                                                               
BTOT43   MVC   P+3(26),=C'*** PLEASE CONTACT DDS ***'                           
         MVC   PSECOND+3(40),=C'*** YOUR FILES MAY BE OUT OF BALANCE **X        
               *'                                                               
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
BTOT44   CLC   ERRCLTS(2),=2X'00'                                               
         BE    BTOTX                                                            
         LA    R5,ERRCLTS                                                       
BTOT44D  LA    R7,P+3                                                           
         LA    R1,25               25 PER LINE                                  
BTOT45   CLC   0(2,R5),=2X'00'     END OF LIST                                  
         BE    BTOT46                                                           
         MVC   0(3,R7),0(R5)                                                    
         MVI   4(R7),C','                                                       
         LA    R7,5(R7)                                                         
         LA    R5,3(R5)                                                         
         BCT   R1,BTOT45                                                        
         BCTR  R7,0                                                             
         MVI   0(R7),C' '                                                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         B     BTOT44D            GO DO NEXT LINE                               
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
***JV                                                                           
         CLC   QAGENCY,=C'OG'         SPECIAL FOR OG                            
         BE    BTOT58                 SKIP PREVIOUS BILLING CHECK               
*                                   SINCE THEY HAVE NO INVOICE RECORDS          
*                                   FOR BILLING DETAILS BEFORE DEC/87           
         CLC   QAGENCY,=C'JW'         SPECIAL FOR JW                            
         BE    BTOT58                 SKIP PREVIOUS BILLING CHECK               
*                                     SINCE THEY ALSO HAVE NO INVOICE           
*                                     RECORDS FROM CONVERSION                   
*                                                                               
***JV                                                                           
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
         LA    R5,BUFCBILN                                                      
         LA    R6,CURBILLN                                                      
         BAS   R7,CBILLERR                                                      
BTOT60   CP    BUFCBILG,CURBILLG                                                
         BE    BTOT70                                                           
         LA    R5,BUFCBILG                                                      
         LA    R6,CURBILLG                                                      
         BAS   R7,GBILLERR                                                      
*                                                                               
BTOT70   CLI   CLTBESW,0                                                        
         BE    BTOTX                                                            
*                                                                               
         MVC   P+3(26),=C'*** PLEASE CONTACT DDS ***'                           
         MVC   PSECOND+3(40),=C'*** YOUR FILES MAY BE OUT OF BALANCE **X        
               *'                                                               
         GOTO1 VPRINTIT,DMCB,(RC)                                               
*                                                                               
*        SEND CONSOLE MESSAGE WHEN IMBALANCE HAS OCCURED                        
*                                                                               
         CLC   =C'YKVA',QUESTOR        (OR IF YKVA REQUESTS IT)                 
         BE    BTOT75                                                           
         CLC   =C'AKAT',QUESTOR        (OR IF AKAT REQUESTS IT)                 
         BE    BTOT75                                                           
*                                                                               
         MVC   OPMSG+49(2),QAGENCY DISPLAY AGENCY                               
         MVC   OPMSG+59(1),QMEDIA  AND MEDIA                                    
         MVC   OPMSG+66(3),PCLTKCLT                                             
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(L'OPMSG,OPMSG)                           
*                                                                               
BTOT75   L     R5,ANXTECLT                                                      
         SH    R5,=H'3'         SEE IF LAST WAS THE SAME                        
         CLC   0(3,R5),PCLTKCLT                                                 
         BE    BTOTX            YES - DON'T STORE TWICE                         
         LA    R5,3(R5)                                                         
         MVC   0(3,R5),PCLTKCLT                                                 
         LA    R5,3(R5)                                                         
         MVC   0(3,R5),=3X'00'                                                  
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
***OFF                                                                          
         CLI   TOTTYP,X'05'        OFFICE TOTALS                                
         BE    BILLEX              YES                                          
***OFF                                                                          
         CLI   TOTTYP,X'06'        REQ TOTALS                                   
         BE    BILLEX              YES                                          
         LA    R4,PSECOND                                                       
         EDIT  (P8,0(R6)),(14,9(R4)),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P8,0(R5)),(14,38(R4)),2,COMMAS=YES,FLOAT=-                      
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
         SPACE 2                                                                
*                                                                               
         DS    F                                                                
BTOTEDIT ST    RE,BTOTEDIT-4                                                    
*                                                                               
         XC    EBLOCK,EBLOCK                                                    
         LA    RE,DOUBLE                                                        
         ST    RE,EBAIN                                                         
         MVI   EBLIN,X'08'                                                      
         MVI   EBTIN,C'P'          INPUT TYPE IS PACKED                         
* OUT                                                                           
         LA    RE,WORK             OUTPUT ADDRESS                               
         ST    RE,EBAOUT                                                        
         MVI   EBLOUT,X'11'        LENGTH  - 17                                 
         MVI   EBDECS,X'02'        2 DECIMALS                                   
         MVI   EBFLOAT,C'-'        FLOAT IN A MINUS SIGN FOR NEG NUM            
         OI    EBOPT,X'A0'         ZERO=NOBLANK,COMMAS                          
         GOTO1 VEDITOR,DMCB,EBLOCK                                              
         L     RE,BTOTEDIT-4                                                    
         BR    RE                  RETURN                                       
*                                                                               
         LTORG                                                                  
*                                                                               
OPMSG    DC    C'AUTONOTE*AKAT,YKVA:** PA8 ERROR DETECTED. AGENCY=XX, M+        
               EDIA=X, CLT=XXX'                                                 
         EJECT                                                                  
PRINTIT  NMOD1 0,PRINTIT                                                        
         L     RC,0(R1)                                                         
         USING PPWORKD,RC                                                       
         L     RA,PPFILEC                                                       
         USING PPFILED,RA                                                       
         LA    R9,SPACEND                                                       
         USING PPA8WRKD,R9                                                      
*                                                                               
         MVI   RCWHATPR,1          DEFAULT TO FIRST PRINTQ                      
         OC    PCLTNAME,SPACES                                                  
         OC    PAGYMED,SPACES                                                   
*                                                                               
         CLI   QOPT4,C'D'          SPECIAL DOWNLOADED OUTPUT                    
         BE    HDHK0                                                            
         CLI   QOPT5,C'P'          PUBLICATION DOWNLOAD                         
         BNE   HDHK1                                                            
*                                                                               
*        WHEN DOWNLOADING SEND EMAILED TOTALS TO SECOND PRINTQ                  
*                                                                               
         CLI   RQEMAIL,C'Y'        EMAILED TOTALS?                              
         BNE   HDHK0                                                            
         MVI   RCWHATPR,2          PRINT ON SECOND PRINTQ                       
         B     HDHK1                                                            
*                                                                               
HDHK0    MVC   P,SPACES            CLEAR LINE AND EXIT                          
         MVC   PSECOND,SPACES      CLEAR LINE AND EXIT                          
         B     HDHKXX                                                           
*                                                                               
HDHK1    CLI   QCLIENT,C'&&'       CHK FOR GROUP REQUEST                        
         BNE   HDHK2                                                            
         MVC   HEAD1+82(13),=C'** GROUP N **'                                   
         MVC   HEAD1+91(1),QCLIENT+1                                            
         B     HDHK6                                                            
*                                                                               
HDHK2    DS    0H                                                               
         CLI   QCLIENT,C'*'        CHK FOR OFFICE FILTERS                       
         BNE   HDHK5                                                            
         CLI   QCLIENT+1,C'-'      ALL BUT                                      
         BE    HDHK4                                                            
         MVC   HEAD1+82(14),=C'** OFFICE 9  **'                                 
         MVC   HEAD1+92(2),SAVCOFF                                              
******   GOTO1 VOFFOUT,DMCB,QCLIENT+1,HEXOUT,HEAD1+89                           
         CLI   QCLIENT+2,C' '                                                   
         BE    HDHK6               NO RANGE                                     
         MVC   HEAD1+82(15),=C'* OFFICES 1-9 *'                                 
         MVC   HEAD1+92(1),QCLIENT+1                                            
******   GOTO1 VOFFOUT,DMCB,QCLIENT+1,HEXOUT,HEAD1+89                           
         MVC   HEAD1+94(1),QCLIENT+2                                            
******   GOTO1 VOFFOUT,DMCB,QCLIENT+2,HEXOUT,HEAD1+93                           
         B     HDHK6                                                            
*                                                                               
HDHK4    MVC   HEAD1+82(16),=C'* NOT OFFICE 9 *'                                
         MVC   HEAD1+95(1),QCLIENT+2                                            
*******  GOTO1 VOFFOUT,DMCB,QCLIENT+2,HEXOUT,HEAD1+92                           
***OFF                                                                          
         B     HDHK6                                                            
*                                                                               
HDHK5    CLI   QCLIENT,C'$'        SEE IF DOING OFFICE LIST REQ                 
         BNE   HDHK6                                                            
         CLI   QCLIENT+1,C'*'          SEE IF DOING ALL OFFICES                 
         BE    HDHK5C                                                           
         MVC   HEAD1+82(14),=C'OFFICE LIST 00'                                  
         MVC   HEAD1+94(2),SAVMOL                                               
*******  GOTO1 VOFFOUT,DMCB,QCLIENT+1,HEXOUT,HEAD1+91                           
*                                                                               
HDHK5C   CLI   MODE,REQLAST                                                     
         BE    HDHK6                                                            
*                                                                               
         MVC   HEAD2+79(15),=C'** OFFICE 9  **'                                 
         MVC   HEAD2+89(2),SAVCOFF                                              
*                                                                               
HDHK6    DS    0H                                                               
         CLI   QPROG+52,C' '       SEE IF I HAVE A CURRENT MTH                  
         BE    HDHK8               NO                                           
         CLC   QPROG+54(2),SPACES    NO MONTH - CURRENT YEAR                    
         BNE   HDHK7                                                            
         MVC   HEAD4+54(2),=C'19'     CENTURY                                   
         MVC   HEAD4+56(2),QPROG+52                                             
         CLI   QPROG+52,C'9'                                                    
         BNH   HDHK8                                                            
         MVC   HEAD4+54(2),=C'20'                                               
         ZIC   R0,HEAD4+56                                                      
         SH    R0,=H'10'                                                        
         STC   R0,HEAD4+56                                                      
         B     HDHK8                                                            
*                                                                               
HDHK7    DS    0H                                                               
         MVC   WORK(4),QPROG+52                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(9,HEAD4+71)                                
         MVC   HEAD4+54(16),=C'CURRENT MONTH OF'                                
*                                                                               
HDHK8    DS    0H                                                               
         CLI   RCSUBPRG,15         SPECIAL PRD SUMMARY                          
         BE    HDHKZ                                                            
*                                                                               
         CLI   RCSUBPRG,10         INVOICE LIST                                 
         BE    HDHKZ                                                            
         CLI   RCSUBPRG,6                                                       
         BH    HDHKX               ONLY SPROGS 3,4,5 USE PRODUCT                
         CLI   RCSUBPRG,3                                                       
         BL    HDHKX                                                            
         CLI   QOPT2,C'Y'          SEE IF COMBINING ALL PRDS                    
         BE    HDHK20                                                           
         MVC   HEAD5(7),=C'PRODUCT'                                             
         MVC   HEAD5+9(3),MYKPRD                                                
         CLI   QOPT7,C'Y'          SEE IF SHOWING SORT TRACE                    
         BE    HDHKX               YES - SKIP PRODUCT NAME                      
*                                  BECAUSE MYKPRD MIGHT BE O                    
*                                                                               
         LARL  R2,PRDTAB                                                        
HDHK9    CLI   0(R2),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                PRD NOT IN TABLE                             
         CLC   0(3,R2),MYKPRD                                                   
         BE    HDHK12                                                           
         LA    R2,23(R2)                                                        
         B     HDHK9                                                            
*                                                                               
HDHK12   DS    0H                                                               
         MVC   HEAD5+13(20),3(R2)  PRD NAME                                     
         OC    HEAD5+13(20),SPACES    NEEDED FOR CD OUTPUT                      
         B     HDHKX                                                            
*                                                                               
HDHK15   CLI   QOPT2,C'Y'          SEE IF DOING ALL PRDS TOGETHER               
         BNE   *+10                                                             
HDHK20   MVC   HEAD5(12),=C'ALL PRODUCTS'                                       
*                                                                               
         CLI   QOPT4,C'B'          PRD SUMMARY VERSION                          
         BE    HDHK23                                                           
         B     HDHKX                                                            
*                                                                               
HDHK23   MVI   RCSUBPRG,15        SET SPECIAL SPROG                             
         B     HDHKZ                                                            
*                                                                               
HDHKX    DS    0H                                                               
         CLI   QOPT4,C'B'          PRD SUMMARY VERSION                          
         BE    HDHKZ                                                            
         CLI   QOPT4,C'M'          NO MTH OF SERVICE BREAKOUT                   
         BE    HDHKZ                                                            
         CLI   RCSUBPRG,5                                                       
         BNL   HDHKX2                                                           
         MVC   HEAD8+11(4),=C'/MOS'                                             
         MVC   HEAD9+11(4),=4C'-'                                               
         CLI   MTHSW,C'I'          SEE IF DOING INS MTHS                        
         BNE   HDHKZ                                                            
         MVC   HEAD8+11(5),=C'/INSM'                                            
         MVC   HEAD9+11(5),=5C'-'                                               
**NEW 1/25/89                                                                   
         CLI   QOPT1-1,C'D'          INSERTION DATE                             
         BNE   HDHKZ                                                            
         MVC   HEAD8+11(5),=C'/INSD'                                            
**NEW 1/25/89                                                                   
         B     HDHKZ                                                            
*                                                                               
HDHKX2   MVC   HEAD8+5(6),=C'MTH OF'                                            
         MVC   HEAD9+4(7),=C'SERVICE'                                           
         MVC   HEAD10+4(7),=7C'-'                                               
         CLI   MTHSW,C'I'          SEE IF DOING INS MONTHS                      
         BNE   HDHKZ                                                            
         MVC   HEAD8+3(9),=C'INSERTION'                                         
         MVC   HEAD9+4(7),=C' MONTH '                                           
         MVC   HEAD10+4(7),=C' ----- '                                          
HDHKZ    MVC   SAVEPRG,RCSUBPRG                                                 
         CLI   CDSW,C'Y'          SEE IF SUBTRACTING CD                         
         BNE   HDHKZ5              NO                                           
         ZIC   R0,RCSUBPRG         YES                                          
         AH    R0,=H'20'                                                        
         STC   R0,RCSUBPRG                                                      
*                                                                               
HDHKZ5   CLI   PAGYNAT,C'C'        SEE IF CANADIAN                              
         BNE   HDHKZ8                                                           
         CLI   RCSUBPRG,30         SEE IF DOING INVOICE LISTING                 
         BE    HDHKZ7                                                           
         CLI   RCSUBPRG,10                                                      
         BNE   HDHKZ8                                                           
*                                                                               
HDHKZ7   ZIC   R0,RCSUBPRG         YES                                          
         AH    R0,=H'50'                                                        
         STC   R0,RCSUBPRG                                                      
*                                                                               
HDHKZ8   GOTO1 REPORT                                                           
         MVC   RCSUBPRG,SAVEPRG    RESTORE RCSUBPRG                             
HDHKXX   XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                          FOR DOWNLOAD PRINTING (QOPT4 = D)                    
*                          OR PUBLICATION DOWNLOAD (QOPT5 = P)                  
DOWNLD   NMOD1 0,DOWNLD                                                         
*                                                                               
         MVI   DMTHSW,0                                                         
         CLI   0(R1),C'A'    MONTH OF SERVICE LINE (M OR E)                     
         BL    DNP1                                                             
         MVC   DMTHSW,0(R1)      SAVE INDICATOR                                 
         MVI   0(R1),0                                                          
DNP1     DS    0H                                                               
         L     RC,0(R1)                                                         
         USING PPWORKD,RC                                                       
         L     RA,PPFILEC                                                       
         USING PPFILED,RA                                                       
         LA    R9,SPACEND                                                       
         USING PPA8WRKD,R9                                                      
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
         CLI   MODE,LBUYREQ       SEE IF END OF REPORT                          
         BE    DNP70                                                            
*                                                                               
         CLI   MODE,FBUYREQ       SEE IF I NEED TO INTIALIZE                    
         BE    DNP80                                                            
*                                                                               
         MVC   DNLINE,P          SAVE CONTENTS OF PRINTLINE                     
         MVC   P,SPACES                                                         
         CLI   PROGPROF+14,C'Y'   DOWNLOAD HEADERS?                             
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
         CLI   PROFA8A,C'Y'      DOWNLOADING ACC OFFICE?                        
         BNE   DNP1HC                                                           
         MVC   DLCBFLD(10),=C'ACC OFFICE'                                       
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                CLIENT FIELD                                   
DNP1HC   MVC   DLCBFLD(6),=C'CLIENT'                                            
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                PRODUCT FIELD                                  
         MVC   DLCBFLD(7),=C'PRODUCT'                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
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
*                                MONTH OF SERVICE                               
DNP1H0   DS    0H                                                               
*                                VENDOR FIELD                                   
         MVC   DLCBFLD(6),=C'VENDOR'                                            
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                MONTH OF SERVICE                               
         CLI   PROGPROF+9,C'N'   NO MOS NOR INV/CLEAR DETAILS                   
         BE    DNP1H1A                                                          
*                                                                               
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
         CLI   PROGPROF+9,C'Y'   INVOICE/CLEAR DATE DETAILS?                    
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
         CLI   PROGPROF+9,C'Y'   INVOICE/CLEAR DATE DETAILS?                    
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
         CLI   PROFA8A,C'Y'      DOWNLOADING ACC OFFICE?                        
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
*                                VENDOR FIELD  - EMPTY                          
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   PROGPROF+9,C'N'   NO MOS INV/CLEAR DETAILS                       
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
         CLI   PROGPROF+9,C'Y'   INVOICE/CLEAR DATE DETAILS?                    
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
         CLI   PROGPROF+9,C'Y'   INVOICE/CLEAR DATE DETAILS?                    
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
         JIF   PROGPROF+9,EQ,C'M',AND,DMTHSW,EQ,X'00',DNPX                      
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
         MVC   DLCBFLD(L'SAVCOFF),DNLINE+4     OFFICE CODE                      
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   PROFA8A,C'Y'       DOWNLOADING ACC OFFICE?                       
         BNE   DNP2                                                             
         MVC   DLCBFLD(L'SAVAOFF),DNLINE+130    ACC OFFICE                      
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP2     DS    0H                                                               
         MVC   DLCBFLD(3),DNLINE+6 CLIENT                                       
*                                                                               
         CLI   PROGPROF+15,C'C'    INCLUDE CLIENT NAME?                         
         BNE   DNP3                                                             
         MVC   DLCBFLD+4(20),PCLTNAME                                           
*                                                                               
DNP3     MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         MVC   DLCBFLD(3),DNLINE+10 PRODUCT                                     
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   QOPT5,C'P'        PUBLICATION DOWNLOAD                           
         BE    DNP5                                                             
*                                                                               
         MVC   DLCBFLD(12),DOLS  BALANCE OUT                                    
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP60                                                            
*                                                                               
DNP5     DS    0H                                                               
*                                                                               
         LA    RE,DLCBFLD-2                                                     
         CLI   PROGPROF+15,C'B'    REPORTING BASE VENDOR WITH NAME              
         BE    DNP5A                                                            
         CLI   PROGPROF+15,C'V'    INCLUDE VENDOR NAME?                         
         BNE   DNP5AX                                                           
DNP5A    CLC   DNLINE+110(20),SPACES    PUB NAME PRESENT?                       
         BE    DNP5AX                                                           
         MVC   DLCBFLD(20),DNLINE+110                                           
         LA    RE,DLCBFLD+19        SCAN BACKWARDS FOR NON-SPACE                
DNP5A2   CLI   0(RE),C' '                                                       
         BH    DNP5AX                                                           
         BCT   RE,DNP5A2         BACK-UP TO FIRST NON-SPACE                     
*                                                                               
DNP5AX   MVC   2(17,RE),DNLINE+30   PUB                                         
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   PROGPROF+9,C'Y'     MOS,INV,PAY DATE DETAILS                     
         BE    DNP10                                                            
*                                                                               
         CLI   PROGPROF+9,C'M'     MOS ONLY?                                    
         BE    DNP10                                                            
*                                                                               
*        PROGPROF+9 SHOULD NOW BE C'N'                                          
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
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
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
         CLI   PROGPROF+10,C'Y'   2 DECIMALS?                                   
         BNE   *+10                                                             
         MVC   SCBIL(3),=C'.00'                                                 
         MVI   SCPAY,C'0'                                                       
         CLI   PROGPROF+10,C'Y'   2 DECIMALS?                                   
         BNE   *+10                                                             
         MVC   SCPAY(3),=C'.00'                                                 
*                                                                               
DNP5F    MVC   DLCBFLD(L'SCBIL),SCBIL CURRENT BILLING                           
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVC   DLCBFLD(L'SCPAY),SCPAY CURRENT CLEARANCES                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVC   DLCBFLD(L'SBALO),SBALO BALANCE OUT                               
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP60              GO SEND END OF LINE                           
*                                                                               
*                                                                               
DNP10    DS    0H                  PUBLICATION DOWNLOAD WITH                    
*                                  MOS,INVS,CLEARNACE DATES                     
         CLI   DMTHSW,C'M'         SEE IF MOTH OF SERVICE LINE                  
         BE    DNP20                                                            
         CLI   DMTHSW,C'E'         OR ESTIMATE MOS LINE                         
         BE    DNP20                                                            
*                                NO MOS - SEND EMPTY FIELD                      
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         TM    ESTTBSW,2          SORTING ON EST?                               
         BZ    DNP12                                                            
*                                SEND ANOTHER EMPTY FIELD                       
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP12    MVC   DLCBFLD(L'SBALF),SBALF BALANCE FROWARD                           
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVI   DLCBFLD,C'0'      DON'T REPORT CURRENT BILLING                   
         CLI   PROGPROF+10,C'Y'  2 DECIMALS IN $ FIELDS                         
         BNE   *+10                                                             
         MVC   DLCBFLD(3),=C'.00'                                               
*                                THEY WILL APPEAR IN MOS LINE                   
*******  MVC   DLCBFLD(L'SCBIL),SCBIL CURRENT BILLING                           
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   PROGPROF+9,C'M'   MOS ONLY?                                      
         BE    DNP12C            DON'T SEND EMPTY INV NUMBER FIELD              
*                                NO INV - SEND EMPTY FIELD                      
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP12C   MVI   DLCBFLD,C'0'      DON'T REPORT CLEARANCES                        
         CLI   PROGPROF+10,C'Y'  2 DECIMALS IN $ FIELDS                         
         BNE   *+10                                                             
         MVC   DLCBFLD(3),=C'.00'                                               
*                                THEY WILL APPEAR IN MOS LINE                   
******** MVC   DLCBFLD(L'SCPAY),SCPAY CURRENT CLEARANCES                        
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   PROGPROF+9,C'M'   MOS ONLY?                                      
         BE    DNP12E            DON'T SEND EMPTY CLEARANCE DATE FIELD          
*                                NO DATES-SEND EMPTY FIELD                      
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP12E   MVC   DLCBFLD(L'SBALO),SBALO BALANCE OUT                               
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP60                                                            
*                                                                               
DNP20    DS    0H                 MONTH OF SERVICE LINE                         
         L     R5,INVADR       ADDRESS OF INV ENTRY                             
         L     R6,PAYADR       ADDRESS OF PAY ENTRY                             
         MVC   DLCBFLD(8),DNLINE+90       MOS                                   
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP21    LA    R4,MOSTOTS                                                       
*                                                                               
         CLI   DMTHSW,C'E'       SEE IF EST MOS LINE                            
         BNE   DNP22                                                            
         MVC   DLCBFLD(7),DNLINE+17     EST/LNE                                 
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         LA    R4,ESTTOTS                                                       
*                                                                               
DNP22    DS    0H                                                               
*                                                                               
         CLI   PROGPROF+9,C'M'  MOS ONLY?                                       
         BNE   DNP224X                                                          
*                                SHOW MONTH BALANCE FORWARD                     
         ST    R1,SAVER1                                                        
         ZAP   DOUBLE,0(8,R4)    PREV BILLED                                    
         SP    DOUBLE,8(8,R4)    PREV CLEARED                                   
         CLI   PROGPROF+10,C'Y'  $ WITH 2 DECIMALS?                             
         BE    DNP224                                                           
         CLI   PROGPROF+11,C'Y'   MINUS BEFORE?                                 
         BE    DNP222                                                           
         EDIT  (P8,DOUBLE),(11,MYWORK),MINUS=YES,ZERO=NOBLANK                   
         L     R1,SAVER1       MUST RESTORE R1                                  
         MVC   DLCBFLD(11),MYWORK                                               
         B     DNP222A                                                          
*                                                                               
DNP222   EDIT  (P8,DOUBLE),(12,MYWORK),ZERO=NOBLANK,FLOAT=-                     
         L     R1,SAVER1       MUST RESTORE R1                                  
         MVC   DLCBFLD(12),MYWORK                                               
DNP222A  MVI   DLCBTYP,C'N'                                                     
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP50                                                            
*                                                                               
DNP224   DS    0H                                                               
         CLI   PROGPROF+11,C'Y'   MINUS BEFORE?                                 
         BE    DNP224A                                                          
         EDIT  (P8,DOUBLE),(11,MYWORK),2,MINUS=YES,ZERO=NOBLANK                 
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(11),MYWORK                                               
         B     DNP224B                                                          
*                                                                               
DNP224A  EDIT  (P8,DOUBLE),(12,MYWORK),2,ZERO=NOBLANK,FLOAT=-                   
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(12),MYWORK                                               
DNP224B  MVI   DLCBTYP,C'N'                                                     
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP50                                                            
*                                                                               
DNP224X  DS    0H                                                               
         MVI   DLCBFLD,C'0'      EMPTY BALANCE FORWARD FIELD                    
         CLI   PROGPROF+10,C'Y'  2 DECIMALS IN $ FIELDS                         
         BNE   *+10                                                             
         MVC   DLCBFLD(3),=C'.00'                                               
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
         CLI   PROGPROF+9,C'M'     MOS ONLY?                                    
         BE    DNP50                                                            
*                                                                               
         CLI   PROGPROF+9,C'N'     OR NO MOS/INV/CLEAR DETAILS                  
         BE    DNP50                                                            
*                                                                               
         CLC   0(5,R5),=5X'FF'     DO I HAVE AN INVOICE ENTRY?                  
         BE    DNP25               NO  SEND 2 EMPTY FIELDS                      
         ST    R1,SAVER1     SAVE AND RESTORE R1                                
         CLI   PROGPROF+10,C'Y'    2 DECIMALS IN $ FIELDS                       
         BE    DNP22C                                                           
         CLI   PROGPROF+11,C'Y'    MINUS SIGN BEFORE $?                         
         BE    DNP22A                                                           
         EDIT  (P8,5(R5)),(11,DLCBFLD),MINUS=YES,ZERO=NOBLANK                   
         B     DNP22D                                                           
*                                                                               
DNP22A   EDIT  (P8,5(R5)),(11,MYWORK),ZERO=NOBLANK,FLOAT=-                      
         L     R1,SAVER1     SINCE THIS EDIT CLOBBERS R1                        
         MVC   DLCBFLD(11),MYWORK                                               
         B     DNP22D                                                           
*                                                                               
DNP22C   DS    0H                                                               
         CLI   PROGPROF+11,C'Y'    MINUS SIGN BEFORE $?                         
         BE    DNP22C5                                                          
         EDIT  (P8,5(R5)),(12,DLCBFLD),2,MINUS=YES,ZERO=NOBLANK                 
         B     DNP22D                                                           
*                                                                               
DNP22C5  EDIT  (P8,5(R5)),(12,MYWORK),2,ZERO=NOBLANK,FLOAT=-                    
         L     R1,SAVER1     SINCE THIS EDIT CLOBBERS R1                        
         MVC   DLCBFLD(12),MYWORK                                               
*                                                                               
DNP22D   DS    0H                                                               
         L     R1,SAVER1     DLFLD CALL DEPENDS ON IT                           
         MVI   DLCBTYP,C'N'      NUMERIC                                        
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
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
         CLI   PROGPROF+10,C'Y'  2 DECIMALS IN $ FIELDS                         
         BNE   *+10                                                             
         MVC   DLCBFLD(3),=C'.00'                                               
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
DNP30    DS    0H                                                               
         CLC   0(5,R6),=5X'FF'    NO PAYMENT ENTRY                              
         BE    DNP35              NO - SEND 2 EMPTY PAYMENT FIELDS              
         ST    R1,SAVER1     SAVE AND RESTORE R1                                
         CLI   PROGPROF+10,C'Y'   2 DECIMALS IN $ FIELDS?                       
         BE    DNP30C                                                           
         CLI   PROGPROF+11,C'Y'    MINUS SIGN BEFORE $?                         
         BE    DNP30B                                                           
         EDIT  (P8,8(R6)),(11,DLCBFLD),MINUS=YES,ZERO=NOBLANK                   
         B     DNP30D                                                           
*                                                                               
DNP30B   EDIT  (P8,8(R6)),(11,MYWORK),ZERO=NOBLANK,FLOAT=-                      
         L     R1,SAVER1     SINCE THIS EDIT CLOBBERS R1                        
         MVC   DLCBFLD(11),MYWORK                                               
         B     DNP30D                                                           
*                                                                               
DNP30C   DS    0H                                                               
         CLI   PROGPROF+11,C'Y'    MINUS SIGN BEFORE $?                         
         BE    DNP30C5                                                          
         EDIT  (P8,8(R6)),(12,DLCBFLD),2,MINUS=YES,ZERO=NOBLANK                 
         B     DNP30D                                                           
*                                                                               
DNP30C5  EDIT  (P8,8(R6)),(12,MYWORK),2,ZERO=NOBLANK,FLOAT=-                    
         L     R1,SAVER1     SINCE THIS EDIT CLOBBERS R1                        
         MVC   DLCBFLD(12),MYWORK                                               
*                                                                               
DNP30D   DS    0H                                                               
         L     R1,SAVER1    DLFLD CALL DEPENDS ON IT                            
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
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
         CLI   PROGPROF+10,C'Y'  2 DECIMALS IN $ FIELDS                         
         BNE   *+10                                                             
         MVC   DLCBFLD(3),=C'.00'                                               
         MVI   DLCBTYP,C'N'      NUMBER - CLEARANCE AMOUNT                      
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         MVI   DLCBTYP,C'T'      TEXT FIELD - DATE                              
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP40    MVI   DLCBFLD,C'0'      EMPTY BALANCE OUT FIELD                        
         CLI   PROGPROF+10,C'Y'  2 DECIMALS IN $ FIELDS                         
         BNE   *+10                                                             
         MVC   DLCBFLD(3),=C'.00'                                               
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP60                                                            
         EJECT                                                                  
*                                                                               
DNP50    DS    0H               MOS ONLY DOWNLOAD                               
         ST    R1,SAVER1     SAVE AND RESTORE R1                                
         CLI   PROGPROF+10,C'Y'   $ WITH 2 DECIMALS                             
         BE    DNP50C                                                           
         CLI   PROGPROF+11,C'Y'     MINUS BEFORE?                               
         BE    DNP50B                                                           
         EDIT  (P8,16(R4)),(11,DLCBFLD),MINUS=YES,ZERO=NOBLANK                  
         B     DNP50D                                                           
*                                                                               
DNP50B   EDIT  (P8,16(R4)),(11,MYWORK),ZERO=NOBLANK,FLOAT=-                     
         L     R1,SAVER1     SINCE THIS EDIT CLOBBERS R1                        
         MVC   DLCBFLD(11),MYWORK                                               
         B     DNP50D                                                           
DNP50C   DS    0H                                                               
         CLI   PROGPROF+11,C'Y'     MINUS BEFORE?                               
         BE    DNP50C5                                                          
         EDIT  (P8,16(R4)),(12,DLCBFLD),2,MINUS=YES,ZERO=NOBLANK                
         B     DNP50D                                                           
*                                                                               
DNP50C5  EDIT  (P8,16(R4)),(12,MYWORK),2,ZERO=NOBLANK,FLOAT=-                   
         L     R1,SAVER1     SINCE THIS EDIT CLOBBERS R1                        
         MVC   DLCBFLD(12),MYWORK                                               
*                                                                               
DNP50D   DS    0H                                                               
         L     R1,SAVER1    VDLFLD CALL DEPENDS ON IT                           
         MVI   DLCBTYP,C'N'      NUMBER                                         
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT                                           
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP55    DS    0H               MOS ONLY DOWNLOAD                               
         ST    R1,SAVER1     SAVE AND RESTORE R1                                
         CLI   PROGPROF+10,C'Y'   $ WITH 2 DECIMALS                             
         BE    DNP55C                                                           
         CLI   PROGPROF+11,C'Y'     MINUS BEFORE?                               
         BE    DNP55B                                                           
         EDIT  (P8,24(R4)),(11,DLCBFLD),MINUS=YES,ZERO=NOBLANK                  
         B     DNP55D                                                           
*                                                                               
DNP55B   EDIT  (P8,24(R4)),(11,MYWORK),ZERO=NOBLANK,FLOAT=-                     
         L     R1,SAVER1     SINCE THIS EDIT CLOBBERS R1                        
         MVC   DLCBFLD(11),MYWORK                                               
         B     DNP55D                                                           
DNP55C   DS    0H                                                               
         CLI   PROGPROF+11,C'Y'     MINUS BEFORE?                               
         BE    DNP55C5                                                          
         EDIT  (P8,24(R4)),(12,DLCBFLD),2,MINUS=YES,ZERO=NOBLANK                
         B     DNP55D                                                           
*                                                                               
DNP55C5  EDIT  (P8,24(R4)),(12,MYWORK),2,ZERO=NOBLANK,FLOAT=-                   
         L     R1,SAVER1     SINCE THIS EDIT CLOBBERS R1                        
         MVC   DLCBFLD(12),MYWORK                                               
*                                                                               
DNP55D   DS    0H                                                               
         L     R1,SAVER1    VDLFLD CALL DEPENDS ON IT                           
*                                                                               
         MVI   DLCBTYP,C'N'      NUMBER                                         
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'      TEXT                                           
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
DNP57    DS    0H                                                               
         JIF   PROGPROF+9,EQ,C'N',AND,DMTHSW,EQ,C'E',DNP40                      
*                                ESTIMATE LINE WITH NO MOS                      
*                                THEN SEND EMPTY BALANCE OUT                    
         ZAP   DOUBLE,0(8,R4)    PREV BILLED                                    
         SP    DOUBLE,8(8,R4)    PREV CLEARED                                   
         AP    DOUBLE,16(8,R4)   CURRENT BILLED                                 
         SP    DOUBLE,24(8,R4)   CURRENT PAID                                   
*                                                                               
         CLI   PROGPROF+10,C'Y'  $ WITH 2 DECIMALS?                             
         BE    DNP574                                                           
         CLI   PROGPROF+11,C'Y'   MINUS BEFORE?                                 
         BE    DNP572                                                           
         EDIT  (P8,DOUBLE),(11,MYWORK),MINUS=YES,ZERO=NOBLANK                   
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(11),MYWORK                                               
         B     DNP572A                                                          
*                                                                               
DNP572   EDIT  (P8,DOUBLE),(12,MYWORK),ZERO=NOBLANK,FLOAT=-                     
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(12),MYWORK                                               
DNP572A  MVI   DLCBTYP,C'N'                                                     
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
         B     DNP50                                                            
*                                                                               
DNP574   DS    0H                                                               
         CLI   PROGPROF+11,C'Y'   MINUS BEFORE?                                 
         BE    DNP574A                                                          
         EDIT  (P8,DOUBLE),(11,MYWORK),2,MINUS=YES,ZERO=NOBLANK                 
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(11),MYWORK                                               
         B     DNP574B                                                          
*                                                                               
DNP574A  EDIT  (P8,DOUBLE),(12,MYWORK),2,ZERO=NOBLANK,FLOAT=-                   
         L     R1,SAVER1                                                        
         MVC   DLCBFLD(12),MYWORK                                               
DNP574B  MVI   DLCBTYP,C'N'                                                     
         CLI   PROGPROF+12,C'T'  ALL FIELDS AS TEXT?                            
         BNE   *+8                                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 VDLFLD                                                           
*                                                                               
*                                                                               
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
         MVI   RCWHATPR,1     DOWNLOAD TO FIRST PRINTQ                          
         MVI   SPACING,0                                                        
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'N'                                                    
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
DMTHSW   DS    CL1         M IF PROCESSING MOS LINE                             
SAVER1   DS    F                                                                
MYWORK   DS    CL12                                                             
DNLINE   DS    CL132                                                            
         DS    0H                                                               
MAXLINE  DC    H'132'                                                           
DELIM    DC    C' '        FIELD DELIMITER                                      
EOTCHR   DC    C'"'        END OF TEXT FIELD DELIMITER                          
EOTALT   DC    C''''       END OF TEXT CHR ALTERNATE                            
EOLCHR   DC    X'5E'       END OF LINE CHAR - SEMI-COLON                        
EORCHR   DC    C':'        END OF REPORT CHR                                    
*                                                                               
DLCB     DS    XL256       DOWNLOAD BLOCK                                       
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
PPA8WRKD DSECT                                                                  
EOF      EQU   X'FF'           END OF FILE EQUATE                               
DDSIND   DS    XL1             DDS MONTH END INDICATOR                          
DOWNIND  DS    XL1             DOWNLOAD RUN INDICATOR                           
DPAGEIND DS    XL1             SET TO X'01' WHEN DOWNLOADING                    
DHEADIND DS    XL1             SET TO X'01' HEADER DOWDLOADED                   
DTHDRIND DS    XL1             SET TO X'01' TRANSMIT *HDR SENT                  
*                                                                               
RUNSAVEL EQU   *-DDSIND        FIELDS ABOVE NOT CLEARED AT FBUYREQ              
*                             -SAVED FROM REQ TO REQ                            
*                                                                               
MYTODAY  DS    CL6           SET FROM RCDATE                                    
*                              AND FIRST BLANK LINE SENT                        
EMSG     DS    CL105                                                            
SAVEPRG  DS    CL1                                                              
ECODE    DS    CL1                                                              
ELCOD    DS    CL1                                                              
OPTSETSW DS    CL1                                                              
WPSTDATE DS    XL3             WORK DATE USED IN PST CALC                       
INVADR   DS    A        ADDR OF INV ENTRY WHEN DOING SOME PUB DOWNLOADS         
PAYADR   DS    A        ADDR OF PAY ENTRY WHEN DOING SOME PUB DOWNLOADS         
X        DS    F                                                                
MYBILLCD DS    PL6            ISA NOW PL6 9(USED TO BE PL5)                     
SAVPBNET DS    PL6            SAVED PBILLNET                                    
DOLS     DS    CL12     BALANCE OUT DOWNLOAD FOR QOPT4 =D                       
*                                                                               
*        NEXT 4 FIELDS USED FOR THE PUB DOWNLOAD (QOPT5=P)                      
SBALF    DS    CL12     PUB BALANCE FORWARD                                     
SCBIL    DS    CL12     PUB CURRENT BILLING                                     
SCPAY    DS    CL12     PUB CURRENT CLEARANCES                                  
SBALO    DS    CL12     PUB BALANCE OUT                                         
         DS    CL6      MAY BE NEEDED                                           
*                                                                               
DCPAYSW  DS    CL1                                                              
ICBILLSW DS    CL1                                                              
MOSPSW   DS    CL1                                                              
CDSW     DS    CL1                 FROM PROGPROF+6 AT INITIAL                   
MTHSW    DS    CL1                 FROM PROGPROF+7 AT INITIAL                   
*                                                                               
MYA8PROF DS    CL16                SAVED FROM FIRST A8 PROFILE READ             
PROFA8A  DS    CL16                A8A PROFILE                                  
*                                                                               
PROFB1   DS    CL16           PB1 PROFILE NEEDED FOR PPFMTINO                   
PROFB1X  DS    CL16           PB1X PROFILE FOR INV MTH FORMATTING               
*                                                                               
***OFF                                                                          
MULTOFF  DS    CL1                                                              
SAVCOFF  DS    CL2                 10/96 - CHANGED TO 2-POSITIONS FOR           
SAVAOFF  DS    CL2                 SAVED ACC OFFICE                             
***OFF                                        HEX OFFICE PRINTING               
SAVMOL   DS    CL2                 03/17 NEEDED FOR OFFICE LISTS                
ESTCNT   DS    H                                                                
MLINCNT  DS    CL1                                                              
LASTMOS  DS    CL2                                                              
LASTCLT  DS    CL3                                                              
SVPCLT   DS    CL3                 SAVED CLT CODE FOR NEW PAGE                  
PBERRSW  DS    CL1                                                              
CBERRSW  DS    CL1                                                              
ESTPSW   DS    CL1                                                              
TOTTYP   DS    CL1                                                              
CLTPSW   DS    CL1                                                              
SORTACT  DS    CL1                SET TO 'Y' IF SORTING FOR CLIENT              
MTHPSW   DS    CL1                                                              
STAPSW   DS    CL1                                                              
ESTTBSW  DS    CL1                                                              
MOSCACT  DS    CL1                 MTH OF SERV CURRENT ACTIVITY SW              
ESTCACT  DS    CL1                 EST CURRENT ACTIVITY                         
MOSACT   DS    CL1                 MTH OF SERV ACTIVITY                         
STAACT   DS    CL1                 PUB ACTIVITY                                 
ESTACT   DS    CL1                 ESTIMATE ACTIVITY                            
PRDACT   DS    CL1                 PRODUCTACTIVITY                              
CLTACT   DS    CL1                 CLIENT ACTIVITY                              
OFFACT   DS    CL1                 OFFICE ACTIVITY                              
AGYACT   DS    CL1                 MEDIA ACTIVITY                               
*                                                                               
CMSTART  DS    CL3                 CURRENT MTH START                            
CMEND    DS    CL3                 CURRENT MTH END                              
*                                                                               
ASODTP   DS    CL3                 BINARY AS OF DATE FROM QREGION               
*                                                                               
SAVRE    DS    F                                                                
WRKNET   DS    F                                                                
WRKGST   DS    F                                                                
WRKPSTC  DS    CL10                                                             
*                                                                               
SVTBPRD  DS    CL3                                                              
SVTBSTA  DS    CL5                                                              
*                                                                               
ANXTPD   DS    A                   NEXT ENTRY IN PAYMENT TABLE                  
ANXTBL   DS    A                   NEXT ENTRY IN INVOICE TABLE                  
ANXTPRD  DS    A                                                                
ANXTECLT DS    A                                                                
VPUBEDIT DS    A                                                                
VPUBFLT  DS    A                                                                
VPPBVAL  DS    A                                                                
VEDITOR  DS    A                                                                
AFMTINO  DS    A                                                                
VOFFICER DS    A                                                                
APOSTB   DS    A                                                                
APUBEND  DS    A                                                                
VBTOTS   DS    A                                                                
VCLTLAST DS    A                                                                
VPRINTIT DS    A                                                                
VDOWNLD  DS    A                                                                
VDLFLD   DS    A                                                                
VLOGIO   DS    A                                                                
VDATVAL  DS    A                                                                
BUFFIO   DS    A                                                                
BUFFBUFF DS    A                                                                
*                                                                               
MYKEY    DS    0CL52                                                            
MYKPRD   DS    CL3                                                              
MYKPNM   DS    CL20                PUB FIELD FOR SORTING                        
MYKPUB   DS    CL6                                                              
MYKMOS   DS    CL2                                                              
**NEW 1/25/89                      MYKMOS WILL BE COMPRESSED Y/M/D              
**NEW 1/25/89                      IF QOPT1-1 = C'D' AND MTHSW = C'I'           
MYKEST2  DS    CL2                                                              
MYKLINE  DS    CL1                 LINE NUMBER - DDS ONLY                       
MYKINVMO DS    CL2                                                              
MYKINV   DS    CL2                                                              
MYKPDDTE DS    CL3                 PAYMENT DATE                                 
MYKPDPRD DS    CL3                                                              
MYBILDT  DS    CL3                 BILLING DATE                                 
         DS    CL1                 SPARE                                        
MYPDISK  DS    CL4                 PUB DISK ADDR                                
*                                  SORT RECORD                                  
         DS    0D                                                               
TBREC    DS    0CL84                                                            
TBKPRD   DS    CL3                                                              
TBKPNM   DS    CL20                                                             
TBKPUB   DS    CL6                                                              
TBKMOS   DS    CL2                                                              
**NEW 1/25/89                      TBKMOS WILL BE COMPRESSED Y/M/D              
**NEW 1/25/89                      IF QOPT1-1 = C'D' AND MTHSW = C'I'           
TBKEST2  DS    CL2                                                              
TBKLINE  DS    CL1                                                              
TBKINVMO DS    CL2                                                              
TBKINV   DS    CL2                                                              
TBKPDDTE DS    CL3                 PAYMENT DATE                                 
TBKPDPRD DS    CL3                                                              
TBBILDTE DS    CL3                 BILLED DATE                                  
*                                  FOR PAYMENTS - CONTAINS PAY REP              
         DS    CL1                 SPARE FOR ALIGNMENT                          
TBPDISK  DS    CL4                 SAVED PUB DISK ADDRESS                       
TBBILLN  DS    PL8                 BILLED NET                                   
TBBILLG  DS    PL8                 BILLED GROSS                                 
TBPAIDN  DS    PL8                 PAID NET                                     
TBPAIDG  DS    PL8                 PAID GROSS                                   
*                                                                               
*                                                                               
*                                                                               
*                                  BUFFALO RECORD                               
         DS    0D                                                               
BUFREC   DS    0CL113                                                           
BUFKEY   DS    0CL53                                                            
BUFTYP   DS    CL1                 TYPE X'04' = CLT ,X'06' = REQ                
***OFF                                                                          
*                                  X'05' = OFFICE (IF QCLIENT = $)              
***OFF                                                                          
*                                  X'02' = PRD                                  
*                                  X'01'= STA - PASSES RECS TO SORT             
*                    **NOTE**      THESE BUF RECS HAVE SAME KEY                 
*                                  AS TBREC                                     
**NEW 1/25/89                                                                   
*                    **NOTE**      IF QOPT1-1=C'D' AND MTHSW = C'I'             
*                                  BUFMOS WILL HAVE MONTH/YEAR                  
*                                  INSTEAD OF COMPRESSED YMD                    
**NEW 1/25/89                                                                   
*                                                                               
*              ** THE FOLLOWING FIELDS ONLY APPLY TO BUFTYPS                    
*              ** 02,04,05,06                                                   
BUFCLT   DS    CL3                                                              
BUFMOS   DS    CL2                 MTH OF SERV                                  
*                                  X'FFFF' FOR TOTAL LINE                       
BUFEST   DS    CL2                 ESTIMATE                                     
         DS    CL1                                                              
         DS    CL44                SPARE                                        
BUFTOTS  DS    0CL64                                                            
BUFPBILN DS    PL8                 PREV BILLED NET                              
BUFPPAYN DS    PL8                 PREV PAID NET                                
BUFCBILN DS    PL8                 CURRENT BILLED NET                           
BUFCPAYN DS    PL8                 CURRENT PAID NET                             
BUFCBILG DS    PL8                 CURRENT BILLED GROSS                         
BUFCPAYG DS    PL8                 CURRENT PAID GROSS                           
BUFPBILG DS    PL8                 PREV BILLED GROSS                            
BUFPPAYG DS    PL8                 PREV PAID GROSS                              
*                                                                               
DTETOTS  DS    0D                  PAYMENT DATE TOTALS                          
DCPAYN   DS    PL8                 CURRENT PAID NET                             
DCPAYG   DS    PL8                 CURRENT PAID GROSS                           
*                                                                               
INVTOTS  DS    0D                  INVOICE TOTALS                               
ICBILLN  DS    PL8                 CURRENT BILLED NET                           
ICBILLG  DS    PL8                 CURRENT BILLED GROSS                         
*                                                                               
ESTTOTS  DS    0D                  EST TOTALS                                   
EPBILLN  DS    PL8                 EST PREV BILLED NET                          
EPPAYN   DS    PL8                 EST PREV PAID NET                            
ECBILLN  DS    PL8                 EST CURRENT BILLED NET                       
ECPAYN   DS    PL8                 EST CURRENT PAID NET                         
ECBILLG  DS    PL8                 EST CURRENT BILLED GROSS                     
ECPAYG   DS    PL8                 EST CURRENT PAID GROSS                       
EPBILLG  DS    PL8                 EST PREV BILLED GROSS                        
EPPAYG   DS    PL8                 EST PREV PAID GROSS                          
*                                                                               
MOSTOTS  DS    0D                  MTH OF SERV TOTALS                           
MPBILLN  DS    PL8                 MOS PREV BILLED NET                          
MPPAYN   DS    PL8                 MOS PREV PAID NET                            
MCBILLN  DS    PL8                 MOS CURRENT BILLED NET                       
MCPAYN   DS    PL8                 MOS CURRENT PAID NET                         
MCBILLG  DS    PL8                 MOS CURRENT BILLED GROSS                     
MCPAYG   DS    PL8                 MOS CURRENT PAID GROSS                       
MPBILLG  DS    PL8                 MOS PREV BILLED GROSS                        
MPPAYG   DS    PL8                 MOS PREV PAID GROSS                          
*                                                                               
STATOTS  DS    0D                  PUB TOTALS                                   
SPBILLN  DS    PL8                 STA PREV BILLED NET                          
SPPAYN   DS    PL8                 STA PREV PAID NET                            
SCBILLN  DS    PL8                 STA CURRENT BILLED NET                       
SCPAYN   DS    PL8                 STA CURRENT PAID NET                         
SCBILLG  DS    PL8                 STA CURRENT BILLED GROSS                     
SCPAYG   DS    PL8                 STA CURRENT PAID GROSS                       
*                                                                               
PREBILLN DS    PL8                 CLT PREVIOUS BILLING - INVOICE               
PREBILLG DS    PL8                                                              
CURBILLN DS    PL8                 CLT CURRENT BILLING - INVOICE                
CURBILLG DS    PL8                                                              
*                                                                               
CINVNET  DS    PL8                                                              
CINVGRS  DS    PL8                                                              
CINVCD   DS    PL8                                                              
CINVAMT  DS    PL8                                                              
CINVGST  DS    PL8                                                              
CINVPST  DS    PL8                                                              
*                                                                               
CLTBESW  DS    CL1                                                              
CINVSW   DS    CL1                                                              
*                                                                               
AINVAMT  DS    PL8                 AOR AMOUNT DUE                               
CMINVAMT DS    PL8                 COM AMOUNT DUE                               
*                                                                               
MYGST    DS    PL8                 BILLED GST FROM PPBVAL                       
MYPST    DS    PL8                 BILLED PST FROM PPBVAL                       
*                                                                               
CGSTTOTS DS    0CL32               CLIENT GST TOTALS                            
CPBGST   DS    PL8                 CLIENT PREV BILLED GST                       
CPPGST   DS    PL8                 CLIENT PREV PAID GST                         
CCBGST   DS    PL8                 CLIENT CURR BILLED GST                       
CCPGST   DS    PL8                 CLIENT CURR PAID GST                         
*                                                                               
OGSTTOTS DS    0CL32               OFFICE GST TOTALS                            
OPBGST   DS    PL8                 OFFICE PREV BILLED GST                       
OPPGST   DS    PL8                 OFFICE PREV PAID GST                         
OCBGST   DS    PL8                 OFFICE CURR BILLED GST                       
OCPGST   DS    PL8                 OFFICE CURR PAID GST                         
*                                                                               
RGSTTOTS DS    0CL32               REPORT GST TOTALS                            
RPBGST   DS    PL8                 REPORT PREV BILLED GST                       
RPPGST   DS    PL8                 REPORT PREV PAID GST                         
RCBGST   DS    PL8                 REPORT CURR BILLED GST                       
RCPGST   DS    PL8                 REPORT CURR PAID GST                         
*                                                                               
CPSTTOTS DS    0CL32               CLIENT PST TOTALS                            
CPBPST   DS    PL8                 CLIENT PREV BILLED PST                       
CPPPST   DS    PL8                 CLIENT PREV PAID PST                         
CCBPST   DS    PL8                 CLIENT CURR BILLED PST                       
CCPPST   DS    PL8                 CLIENT CURR PAID PST                         
*                                                                               
OPSTTOTS DS    0CL32               OFFICE PST TOTALS                            
OPBPST   DS    PL8                 OFFICE PREV BILLED PST                       
OPPPST   DS    PL8                 OFFICE PREV PAID PST                         
OCBPST   DS    PL8                 OFFICE CURR BILLED PST                       
OCPPST   DS    PL8                 OFFICE CURR PAID PST                         
*                                                                               
RPSTTOTS DS    0CL32               REPORT PST TOTALS                            
RPBPST   DS    PL8                 REPORT PREV BILLED PST                       
RPPPST   DS    PL8                 REPORT PREV PAID PST                         
RCBPST   DS    PL8                 REPORT CURR BILLED PST                       
RCPPST   DS    PL8                 REPORT CURR PAID PST                         
*                                                                               
AORSW    DS    CL1                                                              
COMSW    DS    CL1                                                              
*                                                                               
TEMP1    DS    PL8                USED BY PRDSHR                                
TEMP2    DS    PL8                                                              
TEMP3    DS    PL8                                                              
TEMP4    DS    PL8                                                              
TEMP5    DS    PL8                                                              
TEMP6    DS    PL8                                                              
*                                                                               
*        PST DATA - UP TO 10 PROVINCES CAN BE HANDLED                           
*                                                                               
PSTAREA  DS    0F                  PST DATA FOR ONE PROVINCE                    
*                                                                               
PSTPROV  DS    CL2                 PROVINCIAL CODE                              
         DS    CL2                 SPARE                                        
PSTTAX   DS    F                   PAYABYLE PST TAX                             
PSTTAXPD DS    F                   PAID PST TAX                                 
PSTCODE  DS    H                   PST CODE                                     
PSTPCT   DS    H                   PST %                                        
PSTBASIS DS    CL1                 X'-01' SALES TAX ON NET                      
         DS    CL3                 SPARE                                        
PSTTAXBL DS    F                   TAX BILLED                                   
PSTAREAL EQU   *-PSTAREA           LENGTH OF PSTAREA FOR 1 PROV                 
         ORG   PSTAREA                                                          
         DS    10XL(PSTAREAL)      PROVIDE ROOM FOR 10 PROVINCES                
*                                                                               
*PPBVALD WORK AREA                                                              
       ++INCLUDE PPBVALD                                                        
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
       ++INCLUDE DDEBLOCK                                                       
*                                                                               
ERRCLTS  DS    CL555               ROOM FOR 185 CLIENTS                         
*                                                                               
PPA8WEND EQU   *                                                                
*                                                                               
PPA802   CSECT                                                                  
*                                                                               
**********************************************************************          
* MAIL ID TABLE FOR AUTOMATIC NOTIFICATION                           *          
**********************************************************************          
MAILTAB  DS    0D              EMAIL TABLE                                      
*                                                                               
*&&DO                                                                           
         DS    0CL(MAILNQ)                                                      
         DC    AL2(6487)                 ORIGIN ID                              
         DC    CL8'DTSEC'               CONNECT ID                              
         DC    CL45'KFAU,DAVID CHAMBERS/USA/DDS@DDS'                            
*                                                                               
         DC    AL2(2462)                                                        
         DC    CL8'YNRR'                                                        
         DC    CL45'JCLE,AARE,CWHE'                                             
*                                                                               
         DC    AL2(8151)                                                        
         DC    CL8'YNKLWE'                                                      
         DC    CL45'JCLE,AARE,CWHE'                                             
*                                                                               
         DC    AL2(6029)                                                        
         DC    CL8'YNME'                                                        
         DC    CL45'JCLE,AARE,CWHE'                                             
*&&                                                                             
*                                                                               
         DC    AL1(EOF)                                                         
         EJECT                                                                  
*                                                                               
         DS    0D                  PAYMENT TABLE                                
         DC    C'**PDTAB*'                                                      
PDTAB    DS    16808C              700 PAYMENTS X 24 CHARS EACH +8              
*                                                                               
         DS    0D                  INVOICE TABLE                                
         DC    C'*INVTAB*'                                                      
INVTAB   DS    14705C              700 INVOICES X 21 CHARS EACH +5              
*                                                                               
         DS    0D                  PRODUCT TABLE                                
         DC    C'*PRDTAB*'                                                      
PRDTAB   DS    34503C             1500 PRDS X 23 CHAR EACH +3                   
*                                                                               
         DS    0F                                                               
         BUFF  LINES=2000,ROWS=1,COLUMNS=8,FLAVOR=PACKED,KEYLIST=(48,A)X        
               ,COMMENT=5                                                       
*                                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
* MAIL TABLE DSECT                                                   *          
**********************************************************************          
         SPACE 1                                                                
MAILTBD  DSECT                                                                  
MAILAGY  DS    XL2      AGENCY ORIGIN ID                                        
MAILUID  DS    CL8      CONNECT ID                                              
MAILADD  DS    CL45     E-MAIL ADDRESSES COMMA SEPARATED LIST                   
MAILNQ   EQU   *-MAILTBD           LENGTH                                       
         EJECT                                                                  
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDOFFICED                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
       ++INCLUDE PBYPSTEL                                                       
PBYPSTQ  EQU   X'84'                                                            
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066PPREPA802 07/18/19'                                      
         END                                                                    
