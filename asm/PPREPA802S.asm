*          DATA SET PPREPA802S AT LEVEL 013 AS OF 05/01/02                      
*PHASE PPA802A,+0                                                               
*INCLUDE PUBFLOAT                                                               
*INCLUDE PPBVAL                                                                 
*INCLUDE PPFMTINO                                                               
*INCLUDE OFFOUT                                                                 
*INCLUDE DLFLD                                                                  
*INCLUDE LOGIOA                                                                 
         TITLE 'PPA802 - NEW PRINTPAK BILLING CLEARANCE RPT'                    
*                 THIS VERSION WAS PPREPA802O                                   
*                 RENAMED 6/67/97 TO PPREPA802                                  
*                                                                               
*  CHANGE LOG                                                                   
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
*        REQUEST OPTIONS                                                        
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
*                     ONE LINE PRD TOTALS DOWNLOADED.                           
*                                                                               
*        QOPT5      Y=LIST CURRENT CLIENT INVOICES                              
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
*                                                                               
         PRINT NOGEN                                                            
PPA802   CSECT                                                                  
         NMOD1 0,PPA802,RR=R9                                                   
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RC,0(R1)                                                         
         USING PPWORKD,RC                                                       
         L     RA,PPFILEC                                                       
         USING PPFILED,RA                                                       
         LA    R9,SPACEND                                                       
         USING PPA8WRKD,R9                                                      
         LA    R8,PPA802+4095                                                   
         LA    R8,1(R8)                                                         
         USING PPA802+4096,R8      ** NOTE USE OF R8 AS SECOND BASE **          
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
         LA    R2,SPACEND                                                       
         LA    R3,6                                                             
INIT3    XC    0(250,R2),0(R2)                                                  
         LA    R2,250(R2)                                                       
         BCT   R3,INIT3                                                         
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
*                                                                               
         LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         XC    MOSCACT(8),MOSCACT                                               
         L     RF,=A(DOWNLD)                                                    
         A     RF,RELO                                                          
         ST    RF,VDOWNLD                                                       
         L     RF,=A(PRINTIT)                                                   
         A     RF,RELO                                                          
         ST    RF,VPRINTIT                                                      
         L     RF,=A(TBCLTL)                                                    
         A     RF,RELO                                                          
         ST    RF,VCLTLAST                                                      
         L     RF,=A(BTOTALS)                                                   
         A     RF,RELO                                                          
         ST    RF,VBTOTS                                                        
         L     RF,=V(PUBFLOAT)                                                  
         A     RF,RELO                                                          
         ST    RF,VPUBFLT                                                       
         L     RF,=V(PPBVAL)                                                    
         A     RF,RELO                                                          
         ST    RF,VPPBVAL                                                       
         L     RF,=V(PPFMTINO)                                                  
         A     RF,RELO                                                          
         ST    RF,AFMTINO                                                       
         L     RF,=V(OFFOUT)                                                    
         A     RF,RELO                                                          
         ST    RF,VOFFOUT                                                       
         L     RF,=V(LOGIO)                                                     
         A     RF,RELO                                                          
         ST    RF,VLOGIO                                                        
         L     RF,=A(STABUCKC)                                                  
         A     RF,RELO                                                          
         ST    RF,ADSTABUC                                                      
         L     RF,=A(PDTAB)                                                     
         A     RF,RELO                                                          
         ST    RF,APDTAB                                                        
         MVC   0(8,RF),=8X'FF'                                                  
         ST    RF,ANXTPD                                                        
         L     RF,=A(INVTAB)                                                    
         A     RF,RELO                                                          
         ST    RF,AINVTAB                                                       
         MVC   0(5,RF),=5X'FF'                                                  
         ST    RF,ANXTBL                                                        
         L     RF,=A(PRDTAB)                                                    
         A     RF,RELO                                                          
         ST    RF,APRDTAB                                                       
         L     RF,=A(SORTCS)                                                    
         A     RF,RELO                                                          
         ST    RF,ASORTC                                                        
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
         MVC   QPROG+52(4),SPACES                                               
         B     INIT20                                                           
*                                                                               
INIT15   DS    0H                                                               
         MVC   WORK(4),QPROG+52                                                 
         MVC   WORK+6(4),WORK                                                   
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK+10(2),=C'31'                                                
*        GOTO1 DTCNV,DMCB,(0,WORK),(1,CMSTART)                                  
         GOTO1 DATCON,DMCB,(0,WORK),(3,CMSTART)                                 
*        GOTO1 (RF),(R1),(0,WORK+6),(1,CMEND)                                   
         GOTO1 DATCON,(R1),(0,WORK+6),(3,CMEND)                                 
**Y2K**                                                                         
*        CONVERT BACK TO CHARACTER                                              
         GOTO1 DATCON,DMCB,(3,CMSTART),(0,WORK)                                 
         MVC   QPROG+52(4),WORK                                                 
**Y2K**                                                                         
*                                                                               
INIT20   MVC   BQSTART(6),=X'000000FFFFFF'    SET TO PASS ALL BUYS              
         MVC   ASODTP,=3X'FF'                                                   
         CLI   QREGION,C' '        SEE IF AS OF DATE SPECIFIED                  
         BE    INIT30              NO                                           
*        GOTO1 DTCNV,DMCB,(0,QREGION),(1,ASODTP)                                
         GOTO1 DATCON,DMCB,(0,QREGION),(3,ASODTP)                               
**Y2K**                                                                         
*        CONVERT BACK TO CHARACTER                                              
         GOTO1 DATCON,DMCB,(3,ASODTP),(0,WORK)                                  
         MVC   QREGION(6),WORK                                                  
**Y2K**                                                                         
*                                                                               
INIT30   CLI   QOPT4,C'B'             SPECIAL DOWNLOAD VERSION                  
         BNE   INITX                                                            
         GOTO1 VDOWNLD,DMCB,(RC)                                                
INITX    B     EXIT                                                             
         EJECT                                                                  
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
         MVI   OPTSETSW,C'Y'       SET OPTIONS SET SWITCH                       
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
         MVC   SAVCOFF(1),RCSVOFC   SAVE CODE FOR PRINTING                      
         MVI   SAVCOFF+1,C' '                                                   
******   GOTO1 VOFFOUT,DMCB,RCSVOFC,HEXOUT,SAVCOFF                              
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'05',BUFFBUFF),(X'80',1)                
         B     EXIT                                                             
***OFF                                                                          
         SPACE 2                                                                
TBCLTF   DS    0H                  CLIENT FIRST                                 
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
TBCLTF2  GOTO1 SORTER,DMCB,SORTC,SORTC+80,(48,ASORTC)                           
         MVI   SORTACT,0                                                        
         L     RF,APRDTAB                                                       
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
         CLI   QOPT2,C'Y'          SEE IF COMBINING ALL PRDS                    
         BE    TBPRD5              YES                                          
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
         CLC   PBILLDAT(4),QPROG+52 SEE IF BILLED AFTER CURRENT MTH             
         BH    TBBILX              YES - BYPASS                                 
*                                                                               
TBCF49   DS    0H                                                               
*                                                                               
*******                                                                         
*******  USE PPBVAL TO EXTRACT "EFFECTIVE" VALUES                               
*******                                                                         
         GOTO1 VPPBVAL,DMCB,(C'B',PBILLREC),PPBVALD                             
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
         CLC   PBILLCDT(4),QPROG+52    TEST REVERSED IN CURRENT MTH             
         BH    TBCF55              HIGH - CONSIDER UNREV                        
         BL    TBCF50              LOW - BYPASS                                 
         B     TBCF52                                                           
*                                                                               
TBCF50   DS    0H                                                               
         BAS   RE,POSTBILL                                                      
         B     TBBILX                                                           
*                                                                               
TBCF52   XC    TBREC,TBREC                                                      
         MVC   TBKPUB(5),=X'FFFFFFFF00'          CURRENT REVERSALS              
         CLI   QOPT4,C'B'                                                       
         BNE   TBCF52C                                                          
         MVC   TBKPUB(3),PBILKCLT                                               
         MVC   TBKPUB+3(3),PBILKPRD                                             
         B     TBCF52D           DON'T ENTER THE FF IN TBKPNM                   
*                                                                               
TBCF52C  DS    0H                                                               
         MVI   TBKPNM,X'FF'                                                     
TBCF52D  BAS   RE,POSTBILL                                                      
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
         CLI   QOPT4,C'B'          NO MTH OF SERVICE BREAKOUT                   
         BE    TBCF53                                                           
         MVC   TBKMOS,PBILKMOS                                                  
         CLI   MTHSW,C'I'          SEE IF DOING INSERTION MTH                   
         BNE   TBCF53              NO                                           
         MVC   TBKMOS,=X'FF00'     POST TO DUMMY MONTH                          
*                                                                               
TBCF53   MVC   TBKINV,PBILKBNO                                                  
         MVC   TBKINVMO,PBILKBMN       BILLING Y/M                              
*        GOTO1 DTCNV,DMCB,(0,PBILLDAT),(1,TBBILDTE)                             
         GOTO1 DATCON,DMCB,(0,PBILLDAT),(3,TBBILDTE)                            
         ZAP   TBBILLN,PBILLNET    NET OR NET/NET                               
         ZAP   TBBILLG,PBILLBIL    GROSS OR GROSS-CD                            
         CLI   QOPT7,C'Y'          TRACING SORT CALLS                           
         BNE   *+14                                                             
         MVC   WORK(56),TBREC                                                   
         BAS   RE,SORTIN                                                        
         GOTO1 SORTER,DMCB,=C'PUT',TBREC                                        
         MVI   SORTACT,C'Y'                                                     
*        GOTO1 DTCNV,DMCB,(0,PBILLCDT),(1,TBBILDTE)                             
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
         BAS   RE,POSTBILL                                                      
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
         CLI   QOPT4,C'B'                                                       
         BNE   TBCF56                                                           
         MVC   TBKPUB(3),PBILKCLT                                               
         MVC   TBKPUB+3(3),PBILKPRD                                             
         B     TBCF56C             NO X'FF' IN TBKPNM                           
*                                                                               
TBCF56   MVI   TBKPNM,X'FF'                                                     
TBCF56C  CLI   QOPT4,C'M'          NO MTH OF SERVICE BREAKOUT                   
         BE    TBCF58                                                           
         CLI   QOPT4,C'B'          NO MTH OF SERVICE BREAKOUT                   
         BE    TBCF58                                                           
         MVC   TBKMOS,PBILKMOS                                                  
         CLI   MTHSW,C'I'          SEE IF DOING INSERTION MTH                   
         BNE   TBCF58              NO                                           
         MVC   TBKMOS,=X'FF00'     POST TO DUMMY MONTH                          
*                                                                               
TBCF58   MVC   TBKINV,PBILKBNO                                                  
         MVC   TBKINVMO,PBILKBMN       BILLING Y/M                              
*        GOTO1 DTCNV,DMCB,(0,PBILLDAT),(1,TBBILDTE)                             
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
POSTBILL NTR1                                                                   
         LA    R2,CURBILLN                                                      
         CLC   QPROG+52(4),SPACES  SEE IF I HAVE A CURRENT MTH                  
         BE    POSTB5              NO                                           
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
*INVR15   GOTO1 DTCNV,DMCB,(1,PBILKMOS),(5,P+14)                                
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
         BNE   INVR22C                                                          
         MVI   P+35,C'S'                                                        
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
INVR23   EDIT  (P5,PBILLNET),(14,P+37),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P5,PBILLBIL),(14,P+53),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P5,MYBILLCD),(14,P+69),2,COMMAS=YES,FLOAT=-                     
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
INVR23C  MVI   P+51,C'*'                                                        
         MVI   P+67,C'*'                                                        
         MVI   P+83,C'*'                                                        
INVR23D  MVI   P+99,C'*'                                                        
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
INVR24C  MVC   101(12,R3),=C'(REVERSED BY'                                      
         MVC   114(2,R3),PBILLCAN                                               
         MVI   116(R3),C'-'                                                     
         MVC   117(4,R3),PBILLCAN+2                                             
         MVI   121(R3),C')'                                                     
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
         EJECT                                                                  
TBBUY    DS    0H                                                               
         TM    KEY+25,X'C0'        NO CLOSED-OUT BUYS                           
         BO    EXIT                                                             
TB2A     CLC   PPRDKPRD,PBUYKPRD   SEE IF DOING POL BUY                         
         BE    TB2D                NO                                           
         LA    R2,PBDELEM                                                       
         USING PPRELEM,R2                                                       
*              MUST FIND PRD ELEM FOR THIS PRD                                  
         MVI   ELCODE,X'21'                                                     
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
         MVC   TBKPUB,PBUYKPUB                                                  
         CLI   QOPT4,C'B'          NO PUB BREAKOUT (NOR MTH)                    
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
         XC    TBKPNM+16(4),TBKPNM+16                                           
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
         CLI   QOPT4,C'B'          NO MTH OF SERVICE BREAKOUT                   
         BE    TB3                                                              
         MVC   TBKMOS,PBDBDATE     USE BILLABLE DATE                            
         CLI   MTHSW,C'I'          SEE IF DOING INSERTION MTH                   
         BNE   TB3                                                              
         MVC   TBKMOS,PBUYKDAT     YR MTH                                       
**NEW 1/25/89                                                                   
         CLI   QOPT1-1,C'D'        SEE IF SHOWING DAY ALSO                      
         BNE   TB3                                                              
*                           MUST USE 2 BYTE COMPRESSED DATE                     
*        GOTO1 DTCNV,DMCB,(1,PBUYKDAT),(2,TBKMOS)                               
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
TB3D     MVI   ELCODE,X'25'                                                     
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
         XC    WORK(24),WORK                                                    
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
         BAS   RE,GETSHR                                                        
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
         BCR   8,RE                                                             
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
PNEXTELX LTR   R5,R5                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
GETB     DS    0H                  NOW DO BILLING ELEMS                         
         MVI   ELCODE,X'26'                                                     
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
         CLI   QOPT4,C'B'           SEE IF SPECIAL DOWNLOAD VERSION             
         BNE   TBAG05                                                           
*                                  MODE IS LBUYREQ                              
*                                  I MUST CLOSE THE DOWNLOADED REPORT           
         GOTO1 VDOWNLD,DMCB,(RC)                                                
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
TBAG25   XC    BUFREC,BUFREC                                                    
         MVI   TOTTYP,X'06'        AGY TOTALS                                   
         MVI   PCLTREC,0           TO NOT USE MEDIA NAME OVERRIDE               
         GOTO1 VBTOTS,DMCB,(RC)                                                 
TBAG50   DS    0H                                                               
         GOTO1 SORTER,DMCB,=C'END'                                              
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
         CLI   QCLIENT,C'$'        OFFICE LIST REQUEST                          
         BE    TBOFF20                                                          
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
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         EJECT                                                                  
         LTORG                                                                  
SORTC    DC    CL80'SORT FIELDS=(1,47,A),FORMAT=BI,WORK=1'                      
SORTT    DC    CL80'RECORD TYPE=F,LENGTH=84'                                    
         EJECT                                                                  
GETGST   CSECT            ROUTINE TO EXTRACT GST AND PST                        
         NMOD1 0,GETGST      FROM PAY ELEMS                                     
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
         MVC   FULL(1),PBDGST      TAKE GST CODE FROM BUY                       
         LA    R5,PGSTTAB                                                       
         CLI   FULL,0              CODE NOT ENTERED IN BUY                      
         BNE   *+8                                                              
         MVI   FULL,C'S'             DEFAULT TO STANDARD                        
         SPACE 3                                                                
*                                                                               
*---------------------------> VERIFY CODE IN PGSTTAB                            
*                                                                               
GETPG10B CLC   0(1,R5),FULL                                                     
         BE    GETPG10D                                                         
         LA    R5,8(R5)                                                         
         CLI   0(R5),X'FF'           END OF TABLE                               
         BNE   GETPG10B                                                         
         XC    FULL,FULL                                                        
         B     GETPG30              BAD GST CODE - NO GST                       
*                                                                               
GETPG10D XC    FULL,FULL                                                        
*                                                                               
         OC    2(2,R5),2(R5)      GST TAX PCT                                   
         BZ    GETPG10E      NO GST TAX  - BUT STILL FIND WRKNET                
         CLC   PBUYKDAT,4(R5)   SEE IF INSERTION DATE BEFORE START-UP           
         BL    GETPG10E      STILL FIND WRKNET                                  
         CLC   PBDPDATE,4(R5)   SEE IF PAYABLE DATE BEFORE START-UP             
         BL    GETPG10E      STILL FIND WRKNET                                  
*                                                                               
         MVC   WGSTPCT,2(R5)      SAVE GST PCT                                  
*                                                                               
GETPG10E OC    PBDTAX,PBDTAX       SEE IF I HAVE TAX                            
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
         USING PSTAREA,R5                                                       
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
         MVC   PSTPROV,0(R1)       SET PROVINCE CODE                            
         MVC   PSTCODE,0(R6)       SET PST CODE                                 
         MVC   PSTPCT,3(R8)        SET PST PER CENT                             
         MVC   PSTBASIS,8(R8)      SET PST BASIS                                
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
         LA    R5,PSTAREA          CALCULATE PST TAXES                          
         LA    R0,10               10 PROVINCES                                 
*                                                                               
GETPSTLP DS    0H                                                               
*                                                                               
         OC    PSTPROV,PSTPROV     SKIP IF NO MORE PROVINCES WITH PST           
         BZ    GETPSTDN                                                         
*                                                                               
         L     RF,WRKNET           COPY NET                                     
*                                                                               
         TM    PSTBASIS,X'01'      SKIP PST ON NET ONLY                         
         BO    *+8                                                              
         A     RF,WRKGST           NET + GSTTAX                                 
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,PSTPCT         GET PST PERCENT                              
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
         DROP  R5                                                               
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
TBCLTL   CSECT                                                                  
         NMOD1 0,TBCLTL                                                         
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
         MVI   P+51,C'*'                                                        
         MVI   P+67,C'*'                                                        
         MVI   P+83,C'*'                                                        
         MVI   P+99,C'*'                                                        
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
         BAS   RE,PUBEND                                                        
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
         BAS   RE,PUBEND                                                        
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
         CLI   ESTCACT,C'Y'        CHK FOR CURRENT ACTIVITY                     
         BNE   EST2E50                                                          
*                                                                               
         L     R5,AINVTAB                                                       
         L     R6,APDTAB                                                        
EST2E2   DS    0H                                                               
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
**NEW 1/25/89                                                                   
EST2E4A  CLI   MYKMOS+1,12                                                      
         BNH   EST2E5              BILLING PERIODS                              
         LA    R2,MYKMOS                                                        
         LA    R3,P+9                                                           
         BAS   RE,MTH13                                                         
         B     EST2E5B                                                          
**NEW 1/25/89                                                                   
EST2E5   CLI   MTHSW,C'I'                                                       
         BNE   EST2E5A                                                          
         CLI   QOPT1-1,C'D'          SEE IF COMPRESSED M/D/Y                    
         BNE   EST2E5A                                                          
*        GOTO1 DTCNV,DMCB,(2,MYKMOS),(3,P+8)                                    
         GOTO1 DATCON,DMCB,(2,MYKMOS),(5,P+8)                                   
         B      EST2E5B                                                         
**NEW 1/25/89                                                                   
*EST2E5A  GOTO1 DTCNV,DMCB,(1,MYKMOS),(5,P+9)                                   
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
*        GOTO1 DTCNV,DMCB,(1,1(R5)),(3,P+51)                                    
         GOTO1 DATCON,DMCB,(3,1(R5)),(5,P+51)                                   
         B     EST2E15                                                          
EST2E13  DS    0H                                                               
         MVC   WORK(2),0(R5)                                                    
         MVI   WORK+3,1       SET DAY TO 1                                      
*        GOTO1 DTCNV,DMCB,(1,WORK),(0,DUB)                                      
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
*        GOTO1 DTCNV,DMCB,(1,0(R6)),(3,P+76)                                    
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
         CLC   P+36(45),SPACES                                                  
         BE    EST2E40             NOTHING TO PRINT                             
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         ZIC   R1,MLINCNT          COUNT MOS PRINT LINES                        
         LA    R1,1(R1)                                                         
         STC   R1,MLINCNT                                                       
         B     EST2E2                                                           
*                                                                               
EST2E40  DS    0H                                                               
         MVC   P+17(7),SPACES      CLEAR EST AND LINE                           
         L     R2,APDTAB                                                        
         MVC   0(8,R2),=8X'FF'                                                  
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
         MVC   BUFMOS,MYKMOS                                                    
**NEW 1/25/89                                                                   
         CLI   BUFMOS,X'FF'                                                     
         BE    EST2E53                                                          
         CLI   MTHSW,C'I'                                                       
         BNE   EST2E53                                                          
         CLI   QOPT1-1,C'D'                                                     
         BNE   EST2E53                                                          
*        GOTO1 DTCNV,DMCB,(2,MYKMOS),(1,WORK)                                   
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
MOSE1    XC    BUFREC,BUFREC                                                    
         MVC   BUFPBILN(64),MPBILLN                                             
         CLI   MOSCACT,C'Y'        SEE IF ACTIVE IN CURRENT MTH                 
         BNE   MOSE45              NO - SKIP TO TOTALS                          
*                                                                               
         CLI   QOPT4,C'B'          SKIP TO TOTALS WHEN SUPPRESSING              
         BE    MOSE45              PUB AND MTH                                  
*                                                                               
         TM    ESTTBSW,2           SEE IF SORTING ON EST                        
         BNZ   MOSE40              SKIP TO MTH TOTALS                           
         MVI   MTHPSW,0                                                         
         L     R5,AINVTAB                                                       
         L     R6,APDTAB                                                        
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
**NEW 1/25/89                                                                   
MOSE2A   CLI   MYKMOS+1,12         13TH PERIOD                                  
         BNH   MOSE2B                                                           
         LA    R2,MYKMOS                                                        
         LA    R3,P+9                                                           
         BAS   RE,MTH13                                                         
         B     MOSE2C                                                           
**NEW 1/25/89                                                                   
MOSE2B   CLI   MTHSW,C'I'                                                       
         BNE   MOSE2B5                                                          
         CLI   QOPT1-1,C'D'                                                     
         BNE   MOSE2B5                                                          
*        GOTO1 DTCNV,DMCB,(2,MYKMOS),(3,P+8)                                    
         GOTO1 DATCON,DMCB,(2,MYKMOS),(5,P+8)                                   
         B     MOSE2C                                                           
**NEW 1/25/89                                                                   
*MOSE2B5  GOTO1 DTCNV,DMCB,(1,MYKMOS),(5,P+9)                                   
MOSE2B5  GOTO1 DATCON,DMCB,(3,MYKMOS),(9,P+9)                                   
MOSE2C   MVI   MTHPSW,C'Y'                                                      
*                                                                               
MOSE3    DS    0H                                                               
MOSE5    CLC   0(5,R5),=5X'FF'          END OF INVOICE TABLE                    
         BE    MOSE20                                                           
         EDIT  (P8,5(R5)),(13,P+36),2,COMMAS=YES,FLOAT=-                        
         CLI   0(R5),X'FF'   CHK FOR BILL DATE                                  
         BNE   MOSE8                                                            
*        GOTO1 DTCNV,DMCB,(1,1(R5)),(3,P+51)                                    
         GOTO1 DATCON,DMCB,(3,1(R5)),(5,P+51)                                   
         B     MOSE10                                                           
*                                                                               
MOSE8    DS    0H                  MONTH                                        
         MVC   WORK(2),0(R5)                                                    
         MVI   WORK+3,1       SET DAY TO 1                                      
*        GOTO1 DTCNV,DMCB,(1,WORK),(0,DUB)                                      
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
*        GOTO1 DTCNV,DMCB,(1,0(R6)),(3,P+76)                                    
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
         CLC   P+36(45),SPACES                                                  
         BE    MOSE40              NOTHING LEFT TO PRINT                        
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
*                                  TOTALS WILL BE SHOWN AT PUB END              
MOSE42D  CLI   MYKMOS+1,12         13TH PERIOD                                  
         BNH   MOSE43                                                           
         LA    R2,MYKMOS                                                        
         LA    R3,P+9                                                           
         BAS   RE,MTH13                                                         
         B     MOSE43C                                                          
*                                                                               
**NEW 1/25/89                                                                   
MOSE43   CLI   MTHSW,C'I'                                                       
         BNE   MOSE43A                                                          
         CLI   QOPT1-1,C'D'                                                     
         BNE   MOSE43A                                                          
*        GOTO1 DTCNV,DMCB,(2,MYKMOS),(3,P+8)                                    
         GOTO1 DATCON,DMCB,(2,MYKMOS),(5,P+8)                                   
         B     MOSE43C                                                          
**NEW 1/25/89                                                                   
*MOSE43A  GOTO1 DTCNV,DMCB,(1,MYKMOS),(5,P+9)                                   
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
*        GOTO1 DTCNV,DMCB,(2,MYKMOS),(1,WORK)                                   
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
         L     R2,APDTAB                                                        
         MVC   0(8,R2),=8X'FF'                                                  
         ST    R2,ANXTPD                                                        
         L     R2,AINVTAB                                                       
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
PUBEND   NTR1                 PUB TOTALS                                        
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
         CLI   STAPSW,C'Y'         SEE IF I'VE PRINTED PUB                      
         BNE   STAE7               NO                                           
         ZIC   R3,LINE             REPEAT IF NEW PAGE                           
         LA    R3,2(R3)                                                         
         STC   R3,X                                                             
         CLC   X(1),MAXLINES                                                    
         BNH   STAE8                                                            
*                                                                               
STAE7    DS    0H                                                               
         CLI   QOPT4,C'B'        SEE IF SUPPRESSING PUB AND MOS                 
         BNE   STAE7X                                                           
         MVI   P,C'P'       SYSTEM PRINTPAK                                     
         MVC   P+2(1),QMEDIA    MEDIA                                           
         MVC   P+4(1),PCLTOFF                                                   
         MVC   P+6(3),MYKPUB     CLIENT                                         
         MVC   P+10(3),MYKPUB+3  PRODUCT                                        
         B     STAE8A                                                           
*                                                                               
STAE7X   BAS   RE,PRNTSTA                                                       
STAE8    MVC   P+9(7),=C'TOTAL**'                                               
*                                                                               
STAE8A   DS    0H                                                               
         MVI   SPACING,2                                                        
         ZAP   DOUBLE,SPBILLN                                                   
         SP    DOUBLE,SPPAYN                                                    
         EDIT  (P8,DOUBLE),(13,P+21),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P8,SCBILLN),(13,P+36),2,COMMAS=YES,FLOAT=-                      
         EDIT  (P8,SCPAYN),(13,P+59),2,COMMAS=YES,FLOAT=-                       
         AP    DOUBLE,SCBILLN                                                   
         SP    DOUBLE,SCPAYN                                                    
         EDIT  (P8,DOUBLE),(13,P+89),2,COMMAS=YES,FLOAT=-                       
*                                                                               
*        EDIT BELOW NEEDED IF DOWNLOADING (QOPT4 = B)                           
*                                                                               
         EDIT  (P8,DOUBLE),(11,DOLS),MINUS=YES,ZERO=NOBLANK                     
*                                                                               
         EDIT  (P8,SCBILLG),(13,P+104),2,COMMAS=YES,FLOAT=-                     
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
STAE8B   CLI   QOPT4,C'B'          NO MTH OF SERVICE BREAKOUT                   
         BE    STAE8C                                                           
         CLI   QOPT4,C'M'          NO MTH OF SERVICE BREAKOUT                   
         BNE   STAE9                                                            
STAE8C   MVI   P+15,C' '           USE ONLY ONE *                               
         MVI   P+35,C' '                                                        
         MVI   P+50,C' '                                                        
         MVI   P+73,C' '                                                        
         MVI   P+103,C' '                                                       
         MVI   P+131,C' '                                                       
         CLI   P+118,C'*'                                                       
         BNE   STAE9               IF IT WASN'T A * LEAVE IT ALONE              
         MVI   P+118,C' '                                                       
*                                                                               
STAE9    DS    0H                                                               
         CLI   QOPT4,C'B'          SPECIAL DOWNLOAD VERSION                     
         BNE   STAE9X                                                           
*                                                                               
         GOTO1 VDOWNLD,DMCB,(RC)                                                
         MVC   P,SPACES          CLEAR PRINT LINE                               
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
         EJECT                                                                  
PRDEND   NTR1                                                                   
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
         EJECT                                                                  
*                                                                               
CLTEND   NTR1                                                                   
         CLI   CLTACT,C'Y'                                                      
         BNE   CLTEX                                                            
CLTE20   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT2,C'Y'                                                       
         BE    CLTE50              ALL PRDS TOGETHER - SKIP CLT TOTALS          
         CLC   QPRODUCT,=C'ALL'    ONLY ONE PRD - SKIP CLT TOTALS               
         BNE   CLTE50                                                           
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
         MVI   2(R3),C'/'                                                       
         ZIC   R0,0(R2)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(2,R3),DUB                                                      
         BR    RE                                                               
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
PRNTS5   DS    0H                  NEEDS AT LEAST 2 LINES                       
         ZIC   R1,LINE                                                          
         LA    R1,2(R1)                                                         
         STC   R1,X                                                             
         CLC   X,MAXLINES                                                       
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
         EJECT                                                                  
BTOTALS  CSECT                                                                  
         NMOD1 0,BTOTALS                                                        
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
         CLI   QOPT4,C'B'          NO MTH OF SERV BREAKOUT                      
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
         IF    BUFCLT,NE,=3X'00',AND,BUFCLT,NE,=3X'FF',BTOT20A                  
         CLI   QOPT4,C'B'          NO MTH OF SERV (NOR PUB)                     
         BE    *+16                                                             
         CLI   QOPT4,C'M'          NO MTH OF SERV                               
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   P(3),=C'ALL'                                                     
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
         CLC   X,MAXLINES                                                       
         BNH   *+10                                                             
         MVC   P(3),SVPCLT                                                      
*                                                                               
BTOT20F  DS    0H                                                               
         CLI   QOPT4,C'M'          NO MTH OF SERV BREAKOUT                      
         BE    BTOT22                                                           
         CLI   QOPT4,C'B'          NO MTH OF SERV BREAKOUT                      
         BE    BTOT22                                                           
         CLI   BUFMOS,X'FF'       IF DOING INS MTH CUR REV                      
         BNE   BTOT20H             AND UNREV ORIG BILLS                         
*                                  HAVE BUFMOS SET TO X'FF00'                   
         MVC   P+3(8),=C'CUR REV+'                                              
         MVC   PSECOND+3(9),=C'UNREV ORI'                                       
         B     BTOT22                                                           
*                                                                               
BTOT20H  CLI   BUFMOS+1,12         13TH MTH                                     
         BNH   BTOT21                                                           
         LA    R2,BUFMOS                                                        
         LA    R3,P+5                                                           
         BAS   RE,BMTH13                                                        
         B     BTOT22                                                           
*                                                                               
*BTOT21   GOTO1 DTCNV,DMCB,(1,BUFMOS),(5,P+5)                                   
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
         CLI   QOPT4,C'B'          NO MTH OF SERVIVE BREAKOUT                   
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
         CLC   X,MAXLINES                                                       
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
         CLI   QOPT4,C'B'          USE 2* IF SHOWING MOS + EST                  
         BE    BTOT30              NO MOS                                       
         CLI   QOPT4,C'M'          USE 2* IF SHOWING MOS + EST                  
         BE    BTOT30              NO MOS                                       
         TM    ESTTBSW,X'04'                                                    
         BZ    BTOT30              NO EST                                       
         B     BTOT29J             GO USE 2 *                                   
*                                                                               
BTOT29H  CLC   BUFCLT,=3X'FF'                                                   
         BNE   BTOT30                                                           
         CLI   QOPT4,C'B'          NO MOS - SKIP THIS *                         
         BE    *+16                                                             
         CLI   QOPT4,C'M'          NO MOS - SKIP THIS *                         
         BE    *+8                                                              
BTOT29J  MVI   P+11,C'*'                                                        
         MVI   P+35,C'*'                                                        
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
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    BTOTX                                                            
*                                  ONE PRD DO TOTALS                            
*                                                                               
BTOT35   DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 VPRINTIT,DMCB,(RC)  SKIP 2 LINES                                 
         MVC   P+3(12),=C'NET BILLINGS'                                         
         EDIT  (P8,BUFPBILN),(14,P+20),2,COMMAS=YES,FLOAT=-                     
         ZAP   DOUBLE,BUFPBILN                                                  
         AP    DOUBLE,BUFCBILN                                                  
         EDIT  (P8,DOUBLE),(14,P+71),2,COMMAS=YES,FLOAT=-                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+3(14),=C'NET CLEARANCES'                                       
         EDIT  (P8,BUFPPAYN),(14,P+20),2,COMMAS=YES,FLOAT=-                     
         ZAP   DOUBLE,BUFPPAYN                                                  
         AP    DOUBLE,BUFCPAYN                                                  
         EDIT  (P8,DOUBLE),(14,P+71),2,COMMAS=YES,FLOAT=-                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+3(14),=C'GROSS BILLINGS'                                       
         EDIT  (P8,BUFPBILG),(14,P+20),2,COMMAS=YES,FLOAT=-                     
         ZAP   DOUBLE,BUFPBILG                                                  
         AP    DOUBLE,BUFCBILG                                                  
         EDIT  (P8,DOUBLE),(14,P+71),2,COMMAS=YES,FLOAT=-                       
         GOTO1 VPRINTIT,DMCB,(RC)                                               
         MVC   P+3(16),=C'GROSS CLEARANCES'                                     
         EDIT  (P8,BUFPPAYG),(14,P+20),2,COMMAS=YES,FLOAT=-                     
         ZAP   DOUBLE,BUFPPAYG                                                  
         AP    DOUBLE,BUFCPAYG                                                  
         EDIT  (P8,DOUBLE),(14,P+71),2,COMMAS=YES,FLOAT=-                       
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
*        SEND CONSOLE MESSAGE WHEN IMBALANCE HAS OCCURED                        
*                                                                               
         MVC   OPMSG+33(2),QAGENCY      DISPLAY AGENCY                          
         MVC   OPMSG+46(1),QMEDIA       AND MEDIA                               
         GOTO1 VLOGIO,DMCB,X'FF400001',(L'OPMSG,OPMSG),(7,A8LABLE)              
*                                                                               
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
         MVC   OPMSG2+33(2),QAGENCY     DISPLAY AGENCY                          
         MVC   OPMSG2+46(1),QMEDIA      AND MEDIA                               
         MVC   OPMSG2+52(3),PCLTKCLT                                            
         GOTO1 VLOGIO,DMCB,X'FF400001',(L'OPMSG2,OPMSG2),(7,A8LABLE)            
*                                                                               
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
         MVI   2(R3),C'/'                                                       
         ZIC   R0,0(R2)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  3(2,R3),DUB                                                      
         BR    RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
*                                                                               
A8LABLE  DC    CL7'A8ERROR'                                                     
*                                                                               
OPMSG    DC    CL47'** PA8 ERROR DETECTED FOR AGENCY XX FOR MEDIA X'            
OPMSG2   DC    CL55'** PA8 ERROR DETECTED FOR AGENCY XX FOR MEDIA X CLTX        
               =XXX'                                                            
         EJECT                                                                  
PRINTIT  CSECT                                                                  
         NMOD1 0,PRINTIT                                                        
         L     RC,0(R1)                                                         
         USING PPWORKD,RC                                                       
         L     RA,PPFILEC                                                       
         USING PPFILED,RA                                                       
         LA    R9,SPACEND                                                       
         USING PPA8WRKD,R9                                                      
*                                                                               
         CLI   QOPT4,C'B'          SPECIAL DOWNLOADED OUTPUT                    
         BE    HDHKXX                                                           
*                                                                               
         CLI   QCLIENT,C'&&'       CHK FOR GROUP REQUEST                        
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
         MVC   HEAD1+82(14),=C'** OFFICE 9 **'                                  
         MVC   HEAD1+92(1),QCLIENT+1                                            
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
         MVC   HEAD1+82(13),=C'OFFICE LIST 9'                                   
         MVC   HEAD1+94(1),QCLIENT+1                                            
*******  GOTO1 VOFFOUT,DMCB,QCLIENT+1,HEXOUT,HEAD1+91                           
*                                                                               
HDHK5C   CLI   MODE,REQLAST                                                     
         BE    HDHK6                                                            
*                                                                               
         MVC   HEAD2+79(14),=C'** OFFICE 9 **'                                  
         MVC   HEAD2+89(1),SAVCOFF                                              
*                                                                               
HDHK6    DS    0H                                                               
         CLI   QPROG+52,C' '       SEE IF I HAVE A CURRENT MTH                  
         BE    HDHK8               NO                                           
         MVC   WORK(4),QPROG+52                                                 
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,HEAD4+71)                                 
         GOTO1 DATCON,DMCB,(0,WORK),(9,HEAD4+71)                                
         MVC   HEAD4+54(16),=C'CURRENT MONTH OF'                                
*                                                                               
HDHK8    DS    0H                                                               
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
         L     R2,APRDTAB                                                       
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
         B     HDHKX                                                            
*                                                                               
HDHK15   CLI   QOPT2,C'Y'          SEE IF DOING ALL PRDS TOGETHER               
         BNE   *+10                                                             
HDHK20   MVC   HEAD5(12),=C'ALL PRODUCTS'                                       
*                                                                               
HDHKX    DS    0H                                                               
         CLI   QOPT4,C'B'          NO MTH OF SERVICE BREAKOUT                   
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
*                          FOR DOWNLOAD PRINTING (QOPT4 = B)                    
DOWNLD   CSECT                                                                  
         NMOD1 0,DOWNLD                                                         
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
         CLI   MODE,LBUYREQ       SEE IF END OF REPORT                          
         BE    DNP10                                                            
*                                                                               
         CLI   MODE,FBUYREQ       SEE IF I NEED TO INTIALIZE                    
         BE    DNP20                                                            
*                                                                               
         MVC   DNLINE,P          SAVE CONTENTS OF PRINTLINE                     
         MVC   P,SPACES                                                         
*                                                                               
         MVC   DLCBFLD(1),DNLINE SYSTEM                                         
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
*                                                                               
         MVC   DLCBFLD(1),DNLINE+2 MEDIA                                        
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
*                                                                               
         MVC   DLCBFLD(1),DNLINE+4 OFFICE                                       
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
*                                                                               
         MVC   DLCBFLD(3),DNLINE+6 CLIENT                                       
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
*                                                                               
         MVC   DLCBFLD(3),DNLINE+10 PRODUCT                                     
         MVI   DLCBTYP,C'T'      TEXT FIELD                                     
         MVI   DLCBACT,DLCBPUT                                                  
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
*                                                                               
         MVC   DLCBFLD(11),DOLS  BALANCE OUT                                    
         MVI   DLCBTYP,C'N'      NUMBER FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
*                                                                               
         MVI   DLCBACT,DLCBEOL    SEND END OF LINE                              
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
         B     DNPX                                                             
*                                                                               
DNP10    DS    0H                                                               
         MVC   P,SPACES            JUST IN CASE                                 
         MVI   DLCBACT,C'R'        SET END OF REPORT                            
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
         B     DNPX                                                             
*                                                                               
DNP20    DS    0H                                                               
         MVC   P,SPACES            JUST IN CASE                                 
         MVI   DLCBACT,C'I'        START AND INTIALIZE REPORT                   
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),(R1)                                                        
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
         DS    0H                                                               
MAXLINE  DC    H'132'                                                           
DELIM    DC    C' '        FIELD DELIMITER                                      
EOTCHR   DC    C'"'        END OF TEXT FIELD DELIMITER                          
EOTALT   DC    C''''       END OF TEXT CHR ALTERNATE                            
EOLCHR   DC    X'5E'       END OF LINE CHAR - SEMI-COLON                        
EORCHR   DC    C':'        END OF REPORT CHR                                    
         EJECT                                                                  
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
PPA8WRKD DSECT                                                                  
SAVEPRG  DS    CL1                                                              
ELCODE   DS    CL1                                                              
ELCOD    DS    CL1                                                              
OPTSETSW DS    CL1                                                              
WPSTDATE DS    XL3             WORK DATE USED IN PST CALC                       
X        DS    F                                                                
MYBILLCD DS    PL5                                                              
DOLS     DS    CL11           USED IN DOWNLOAD VERSION                          
DCPAYSW  DS    CL1                                                              
ICBILLSW DS    CL1                                                              
MOSPSW   DS    CL1                                                              
CDSW     DS    CL1                 FROM PROGPROF+6 AT INITIAL                   
MTHSW    DS    CL1                 FROM PROGPROF+7 AT INITIAL                   
*                                                                               
PROFB1   DS    CL16           PB1 PROFILE NEEDED FOR PPFMTINO                   
PROFB1X  DS    CL16           PB1X PROFILE FOR INV MTH FORMATTING               
*                                                                               
***OFF                                                                          
MULTOFF  DS    CL1                                                              
SAVCOFF  DS    CL2                 10/96 - CHANGED TO 2-POSITIONS FOR           
***OFF                                        HEX OFFICE PRINTING               
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
WRKNET   DS    F                                                                
WRKGST   DS    F                                                                
WRKPSTC  DS    CL10                                                             
*                                                                               
SVTBPRD  DS    CL3                                                              
SVTBSTA  DS    CL5                                                              
*                                                                               
ANXTPD   DS    A                   NEXT ENTRY IN PAYMENT TABLE                  
ANXTBL   DS    A                   NEXT ENTRY IN INVOICE TABLE                  
APDTAB   DS    A                   PAYMENT TABLE                                
AINVTAB  DS    A                   INVOICE TABLE                                
APRDTAB  DS    A                   ADDR OF PRD TABLE                            
ANXTPRD  DS    A                                                                
ANXTECLT DS    A                                                                
VPUBEDIT DS    A                                                                
VPUBFLT  DS    A                                                                
VPPBVAL  DS    A                                                                
AFMTINO  DS    A                                                                
VOFFOUT DS     A                                                                
ADSTABUC DS    A                                                                
ASORTC   DS    A                                                                
VBTOTS   DS    A                                                                
VCLTLAST DS    A                                                                
VPRINTIT DS    A                                                                
VDOWNLD  DS    A                                                                
VLOGIO   DS    A                                                                
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
ERRCLTS  DS    CL555               ROOM FOR 185 CLIENTS                         
*                                                                               
*                                                                               
*                                                                               
PDTAB    CSECT                                                                  
         DS    14408C              600 PAYMENTS X 24 CHARS EACH +8              
INVTAB   CSECT                                                                  
         DS    12605C              600 INVOICES X 21 CHARS EACH +5              
PRDTAB   CSECT                                                                  
         DS    13803C              600 PRDS X 23 CHAR EACH +3                   
         DS    0F                                                               
         SPACE 2                                                                
         BUFF  LINES=1500,ROWS=1,COLUMNS=8,FLAVOR=PACKED,KEYLIST=(48,A)X        
               ,COMMENT=5                                                       
STABUCKC CSECT                                                                  
         DS    CL2000                                                           
         DC    X'00'                                                            
SORTCS   CSECT                                                                  
         DS    73600C                                                           
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
       ++INCLUDE PBYPSTEL                                                       
PBYPSTQ  EQU   X'84'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PPREPA802S05/01/02'                                      
         END                                                                    
