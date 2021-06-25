*          DATA SET SPREPDB02  AT LEVEL 143 AS OF 10/21/20                      
*PHASE SPDB02A                                                                  
*INCLUDE BUFFERIN                                                               
*INCLUDE DLFLD                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE BINSR31                                                                
                                                                                
*===================================================================            
* NOTE ABOUT 'I' DATA EXTRACTS - THEY ARE NO LONGER USED                        
*                                                                               
* NOTE ABOUT COUNTING SPOT ELEMENT NUMBERS IN THIS PROGRAM ---                  
* IN PROCJ, BOTH MINUS AND MINUSSED ELEMENTS ARE SKIPPED.                       
* BUT IN BLDAFD, ONLY MINUS SPOTS ARE SKIPPED. THIS IS BECAUSE                  
* SPOTS CAN BE MISSED AT THE LOCAL LEVEL, SO BETWEEN                            
* THE NETWORK LEVEL AND LOCAL LEVEL WE HAVE TO HAVE THE SAME                    
* ELEMENT NUMBERS IN ORDER TO FIND THE RIGHT AFFID!                             
* ==================================================================            
                                                                                
*===================================================================            
*   APR17 MHER REMOVE CODE THAT NOPS COMSCORE/RENTRAK DEMOS                     
*   OCT16 MHER CHANGES FOR RENTRAK DEMOS                                        
* 21OCT14 MHER BUILD TABLE OF CANNET AFFIDS WHEN STATION CHANGES                
*              AND LOOK THEM UP                                                 
* 16JAN09 EJOR ADD DPT CODE AND MKT/GOAL LOCKIN DATA                            
*              AD-ID SUPPORT                                                    
*              CLEAR AFFID FIELDS                                               
*              ALWAYS CALL GETRATE FOR COST2                                    
* 03JUN08 EJOR ADD RATE TYPE FIELDS                                             
* 20JUN06 MHER ADD FIELDS TO 'B' AND 'S' RECORDS                                
*              AND SUPPORT MORE SLNS/EQTAB FACTORS                              
* 12APR06 MHER SUPPORT FINANCIAL RECS (QCODE=DB/F OR UF)                        
* 02NOV05 MHER SEND 2 DECIMAL VALUES FOR AFFID VERSION (PROCJ)                  
* 17OCT05 MHER UB REQUESTS AUTO QUARTERS BACK/FORWARD                           
* 12AUG05 MHER SUPPORT FOR AFFID EXTRACTS (QOPT2=J)                             
* 31MAR05 MHER SUPPORT FOR 2-DECIMAL RATINGS                                    
* 30SEP03 MHER FOR UB, ONLY BUILD CMLTAB WHEN CLIENT CHANGES                    
*              AND IGNORE BUYLINES CHANGED BY SPOT/PAY                          
* 05SEP03 MHER SUPPORT FOR ISCI DATA BY BUY RECORD                              
*              READS RECOVERY FILE, CREATES BUYFILE                             
* 09MAY03 MHER RESTRUCTURE AND ADD SUPPORT FOR ISCI DATA                        
* 31MAY02 MHER PUT C'A'/C'D' TO SUPPORT DELETE PROCESSING                       
*===================================================================            
                                                                                
*===================================================================            
* QOPT1 OUTPUTS FIXED LENGTH FIELDS                                             
*       BINARY FIELDS WITH 1 DEC GO OUT AT 8 BYTES                              
*                          2 DEC GO OUT AT 13 BYTES                             
*                                                                               
* QOPT2 IS EXTRACT TYPE - F=FINANCIAL (BILLING AND CLEARANCES)                  
*                         R=RECEIVABLE (BILLED) ONLY (UF REQUESTS)              
*                         P=PAYABLES ONLY             " " " " " "               
*                         I=ISCI                                                
*                         J=BUYDEMOS - PUR/PUR RERATE/AFF RERATE                
*                   <BLANK>=PURCHASED DEMO MODE                                 
*                                                                               
* QOPT3 =Y EXCLUDE CLIENT IF COPT2 IS SET TO EXCLUDE FROM J1 REPORT             
*                                                                               
* QOPT5 =Y FOR BUFFALO TRACE                                                    
*       =X TO SUPPRESS OUTPUT (TO TEST WITHOUT MILLIONS OF LINES)               
*                                                                               
* UB REQUESTS PROCESS I AND J MODES                                             
* UF REQUESTS PROCESS FINANCIAL (PYBL/RCVBL)                                    
*===================================================================            
         TITLE 'SPDB02 - CREATE DB2 DATA FILES'                                 
SPDB02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPDB02                                                         
         L     RC,=A(SPDBWORK)                                                  
         USING SPDBWORK,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BNE   *+12                                                             
         BRAS  RE,PROCB                                                         
         J     EXIT                                                             
*                                                                               
         CLI   MODE,PROCGOAL                                                    
         BNE   *+12                                                             
         BRAS  RE,PROCG                                                         
         J     EXIT                                                             
*                                                                               
         CLI   MODE,STAFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,STAF                                                          
         J     EXIT                                                             
*                                                                               
         CLI   MODE,STALAST                                                     
         BNE   *+12                                                             
         BRAS  RE,STAL                                                          
         J     EXIT                                                             
*                                                                               
         CLI   MODE,MKTFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,MKTF                                                          
         J     EXIT                                                             
*                                                                               
         CLI   MODE,ESTFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,ESTF                                                          
         J     EXIT                                                             
*                                                                               
         CLI   MODE,ESTLAST                                                     
         BNE   *+12                                                             
         BRAS  RE,ESTL                                                          
         J     EXIT                                                             
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,CLTF                                                          
         J     EXIT                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,REQF                                                          
         J     EXIT                                                             
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*                                                                               
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
         CLI   MODE,REQLAST                                                     
         BNE   EQXIT                                                            
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
*============================================================*                  
* OPEN A REPORT ON THE PRTQUE AND INITALIZE FOR DOWNLOADING  *                  
*============================================================*                  
*                                                                               
RUNF     DS    0H                                                               
         BRAS  RE,SETATAB                                                       
*                                                                               
         XC    DLCB,DLCB                                                        
D        USING DLCBD,DLCB                                                       
*                                                                               
         MVI   D.DLCBACT,C'I'      START AND INITIALIZE REPORT                  
         MVC   D.DLCBAPR,=A(BLPRINT) PRINT ROUTINE ADDRESS                      
         LA    R0,P                                                             
         ST    R0,D.DLCBAPL        PRINT LINE ADDRESS                           
         OI    D.DLCBFLG1,DLCBFXTN                                              
         MVC   D.DLCXTND(7),MAXLINE                                             
*                                                                               
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         L     RE,=A(PRBLPER)                                                   
         ST    RE,APRBLPER                                                      
*                                                                               
         MVC   DUB(8),=CL8'T00A'   LOAD EDITOR                                  
*                                                                               
         MVI   BYTE,QEDITOR                                                     
         GOTO1 HEXOUT,DMCB,BYTE,DUB+4,1,0                                       
*                                                                               
         GOTO1 LOADER,DMCB,DUB,0                                                
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   D.DLCBAED,4(R1)     DLFLD REQUIRES A(EDITOR)                     
*                                                                               
         MVI   BYTE,QTSAROFF                                                    
         GOTO1 HEXOUT,DMCB,BYTE,DUB+4,1,0                                       
*                                                                               
         GOTO1 LOADER,DMCB,DUB,0                                                
         OC    4(4,R1),4(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   VTSAROFF,4(R1)                                                   
*                                                                               
         LY    R0,CMLTABL                                                       
         STORAGE OBTAIN,LENGTH=(R0),LOC=31,COND=YES                             
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,CMLPAR2                                                       
*                                                                               
         LY    R0,AFDTABL                                                       
         STORAGE OBTAIN,LENGTH=(R0),LOC=31,COND=YES                             
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,AFDPAR2                                                       
*                                                                               
         LY    R0,D29TABL                                                       
         STORAGE OBTAIN,LENGTH=(R0),LOC=31,COND=YES                             
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,D29ATAB          SET TABLE ADDRESS                            
*                                                                               
         XC    GETBLK,GETBLK                                                    
         LA    RF,GETBLK                                                        
         USING GETBUYD,RF                                                       
         MVI   GBYACT,GBYINIT                                                   
         MVC   GBYAGY,AGY                                                       
         MVC   GBYCOMF,ACOMFACS                                                 
         DROP  RF                                                               
*                                                                               
         GOTO1 VGETBUY,GETBLK                                                   
         MVC   SV1OR2,GETBLK+GBY1OR2-GETBUYD                                    
*                                                                               
         CLC   QAGY,=C'T1'                                                      
         BE    *+14                                                             
         CLC   QAGY,=C'SJ'                                                      
         BNE   RUNF10                                                           
         CLC   QCLT,=C'TBL'                                                     
         BNE   RUNF10                                                           
         MVI   SV1OR2,2                                                         
*                                                                               
RUNF10   DS    0H                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CDEMTABS-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,SPBOOKTB                                               
         ICM   RE,15,0(R1)         A(BOOKTYPE TABLE) RETURNED IN P1             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,VBKTYPTB         SAVE A(BOOKTYPE TABLE)                       
         MVC   BKTYPTBL,6(R1)      SAVE L'BOOKTYPE TABLE ENTRY                  
*                                                                               
RUNFX    J     EXIT                                                             
*                                                                               
*=================================================================              
* AT RUNLAST, PUT OUT EOR                                                       
*=================================================================              
*                                                                               
RUNL     LA    R1,DLCB                                                          
         USING DLCBD,R1                                                         
         MVI   D.DLCBACT,C'R'      SET E-O-R                                    
         GOTO1 =V(DLFLD),(R1)                                                   
         DROP  R1                                                               
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*=====================================================================*         
* REQUEST FIRST PROCESSING                                            *         
*=====================================================================*         
*                                                                               
REQF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,BLDD29           BUILD TABLE OF 0D29 RECS                     
*                                                                               
         OI    RQOPT2,RQOPT2_NONTDEM   SET NONT DEMOS NORMALIZED FLAG           
*                                                                               
         BRAS  RE,ISCANADA                                                      
         BE    *+8                                                              
         OI    RQOPTS,RQOPTS_POST  SET SPOT POSTING FOR US ONLY                 
*                                                                               
         OI    RCOPTS,RCOPT_NOCENT   SUPPRESS C'' OVERTYPE IN MEDEQUIV         
*                                                                               
         CLC   QCODE,=C'UB'        OR FINANCIAL TURNAROUND                      
         BNE   *+8                                                              
         MVI   RQDEL,C'Y'                                                       
*                                                                               
         MVC   REQCBNET,QTODAY                                                  
         XC    FILTSTA,FILTSTA                                                  
*                                                                               
         BRAS  RE,ISCANADA                                                      
         BNE   REQF1                                                            
         CLC   QCODE,=C'DB'        LOAD MODE?                                   
         BE    REQF1               YES: NO LOCAL REQUESTS                       
*                                                                               
         CLC   REQCBNET,SPACES     CABLE NETWORK REQUEST?                       
         BNH   REQF1                                                            
*                                                                               
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         MVC   STAPMED,QMED                                                     
         MVC   STAPQMKT,QMKT                                                    
         MVC   STAPQSTA,QSTA                                                    
         MVC   STAPQNET(2),REQCBNET                                             
         MVC   STAPAGY,AGY                                                      
         MVI   STAPCTRY,C'C'                                                    
         MVC   STAPACOM,ACOMFACS                                                
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         JNE   *+2                                                              
         MVC   FILTSTA,STAPMKST                                                 
*                                                                               
         DROP  R1                                                               
*                                                                               
REQF1    CLI   AUTOSET,C'Y'        TEST AUTO DATES SET YET                      
         BE    REQF2                                                            
         BRAS  RE,SETDATES                                                      
         MVI   AUTOSET,C'Y'                                                     
*                                                                               
REQF2    MVI   FCRDGOAL,C'N'       SET TO SUPPRESS GOALS                        
         CLI   QOPT2,C'F'          TEST FINANCIAL REQUEST                       
         BE    REQF4                                                            
         CLC   QCODE,=C'UF'        OR FINANCIAL TURNAROUND                      
         BE    REQF4                                                            
         MVI   FCRDGOAL,C'Y'       ALL MEDIA REQUESTS READ GOALS                
*                                                                               
REQF4    MVI   RQDAYPT,C'Y'        READ DAYPT HEADERS                           
         MVI   RQEQUIV,C'Y'        READ EQUIV HEADERS                           
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         CLC   =C'ALL',QPRD                                                     
         BNE   *+8                                                              
         MVI   RQRDPOL,C'Y'        SET READ POL ONLY IF POSSIBLE                
*                                                                               
         L     RE,=A(MYHDHK)                                                    
         ST    RE,HEADHOOK                                                      
         L     RE,=A(HDHKR9)                                                    
         STM   R9,RC,0(RE)                                                      
*                                                                               
* PUT KILL RECORDS TO OUTPUT                                                    
*                                                                               
         MVC   THISAGY,QAGY                                                     
         MVC   THISMED,QMED                                                     
         MVC   THISCLT,QCLT                                                     
         MVC   THISPRD,QPRD                                                     
         MVC   THISEST,QEST                                                     
         MVC   THISMKT,QMKT                                                     
         MVC   THISSTA,SPACES                                                   
         MVC   THISSTA(5),QSTA                                                  
         CLI   THISSTA+4,C'T'                                                   
         BNE   *+8                                                              
         MVI   THISSTA+4,C' '      MAKE ' ' FOR CONSISTENCY                     
*                                                                               
         CLC   QCODE,=C'UF'        TEST FINANCIAL TURNAROUND                    
         BE    REQF8                                                            
         CLC   QCODE,=C'UB'        TEST MEDIA TURNAROUND                        
         BNE   REQFX                                                            
*                                  KILL GOALS IF UB MODE                        
         L     R4,AKLLGTAB          POINT TO DELETE GOALS TABLE                 
         BRAS  RE,OUTPUT                                                        
*                                                                               
         CLI   QOPT2,C'I'          TEST ISCI MODE                               
         BE    REQF10                                                           
         CLI   QOPT2,C'J'          OR RERATE MODE                               
         BE    REQF10                                                           
*                                                                               
         L     R4,AKLLBTAB          POINT TO DELETE BUYS TABLE                  
*                                                                               
         BRAS  RE,ISCANADA                                                      
         BNE   REQF7                                                            
         CLI   QMED,C'C'                                                        
         BNE   REQF7                                                            
*                                                                               
         MVC   BYTE,THISMED                                                     
         MVI   THISMED,C'T'                                                     
         BRAS  RE,OUTPUT                                                        
         MVI   THISMED,C'N'                                                     
         BRAS  RE,OUTPUT                                                        
         MVC   THISMED,BYTE                                                     
         B     REQFX                                                            
*                                                                               
REQF7    DS    0H                                                               
         BRAS  RE,OUTPUT                                                        
*                                                                               
         B     REQFX                                                            
                                                                                
* FINANCIAL REQUEST DELETES BILLING FOR EST                                     
*                           CLEARANCES FOR MKT FOR CABLE                        
*                                      OR  STA IF NOT CABLE                     
                                                                                
REQF8    CLI   QSTA,C' '           TEST STATION PRESENT                         
         BH    *+10                YES                                          
         MVC   QSTA,=CL5'ALL  '    NO - THEN MAKE IT 'ALL'                      
*                                                                               
         BRAS  RE,ISCANADA                                                      
         BNE   REQF8A                                                           
*                                                                               
         CLC   REQCBNET,SPACES    CABLE NETWORK?                                
         BNH   REQF8A                                                           
         MVI   THISSTA+4,C'/'                                                   
         MVC   THISSTA+5(L'REQCBNET),REQCBNET                                   
*                                                                               
REQF8A   DS    0H                                                               
         L     R4,AKLLRTAB                                                      
         SR    R5,R5               NO STATION ENTRY IN TABLE                    
         CLI   QOPT2,C'R'          TEST RECEIVABLE (BILLING) T/A                
         BE    REQF8X                                                           
*                                                                               
         L     R4,AKLLPTAB                                                      
         L     R5,=A(KLLPSTA)      POINT TO STATION ENTRY                       
         BRAS  RE,ISCANADA                                                      
         BNE   *+8                                                              
         L     R5,=A(KLLPSTAC)     POINT TO STATION ENTRY - CANADA              
         CLI   QOPT2,C'P'          TEST PAYABLE T/A                             
         BNE   REQFX                                                            
                                                                                
*=================================================================              
* COPY THE NEXT ENTRY (TO SEND 1 BLANK) OVER THE STATION ENTRY                  
* IF IT'S A REQUEST FOR ALL STATIONS                                            
*=================================================================              
                                                                                
*                                                                               
REQF8X   CLC   QSTA(3),=C'ALL'                                                  
         BE    *+6                                                              
         SR    R5,R5                                                            
*                                                                               
         LTR   R5,R5               TEST FOR A KILL STATION ENTRY                
         BZ    REQF9                                                            
         MVC   SVTABENT,0(R5)      SAVE IT FOR 'ALL' STATION REQ                
         MVC   0(8,R5),8(R5)       MOVE NEXT ENTRY OVER IT                      
*                                                                               
REQF9    BRAS  RE,OUTPUT                                                        
*                                                                               
         LTR   R5,R5                                                            
         BZ    *+10                                                             
         MVC   0(8,R5),SVTABENT    RESTORE THE ENTRY NOW                        
         B     REQFX                                                            
*                                                                               
*================================================================               
* FOR ISCI PROCESSING IN TURNAROUND MODE, READ THE RECOVERY FILE                
* AND CREATE A 'BUYFILE' OF ALL CHANGED BUY RECORDS                             
* THIS IS NOT DONE FOR FINANCIAL MODE                                           
*================================================================               
                                                                                
QSPTFIL  EQU   X'21'                                                            
QCOPY    EQU   X'01'                                                            
QCHG     EQU   X'02'                                                            
QADD     EQU   X'03'                                                            
                                                                                
REQF10   DS     0H                                                              
         CLI   SORTED,C'Y'         TEST SORTED FILE YET                         
         BE    REQFX                                                            
*                                                                               
         OPEN  (RECVIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
*                                                                               
REQF12   L     R0,ADBUY                                                         
         AHI   R0,-4                                                            
         GET   RECVIN,(0)                                                       
*                                                                               
         L     R8,ADBUY                                                         
         USING RECVHDRD,R8                                                      
*                                                                               
         CLI   0(R8),QSPTFIL       TEST SPTFILE                                 
         BNE   REQF12                                                           
*                                                                               
         CLI   DM$RPRG,X'13'       TEST SPOT/PAY PROGRAM                        
         BE    REQF12              YES - IGNORE                                 
*                                                                               
REQF14   CLI   1(R8),QCHG          TEST CHANGE                                  
         BE    *+12                                                             
         CLI   1(R8),QADD          TEST ADD                                     
         BNE   REQF12                                                           
         DROP  R8                                                               
*                                                                               
         L     RE,ADBUY                                                         
         AHI   RE,-4                                                            
         AH    RE,0(RE)                                                         
         XC    0(2,RE),0(RE)       CLEAR EOR                                    
*                                                                               
         AHI   R8,24                                                            
         USING BUYRECD,R8                                                       
*                                                                               
REQF16   CLI   0(R8),X'10'         TEST BUY RECORD                              
         BL    REQF12                                                           
         SR    R0,R0                                                            
         IC    R0,0(R8)            GET AGYMD BYTE                               
         SRL   R0,4                DROP MEDIA                                   
         CLM   R0,1,BAGY                                                        
         BNE   REQF12                                                           
         CLI   3(R8),X'FF'         TEST POL BUY                                 
         BNE   REQF12                                                           
*                                                                               
S        USING SORTRECD,SORTREC                                                 
         XC    SORTREC,SORTREC                                                  
         MVC   S.SORTAGMD,0(R8)    A-M                                          
         MVC   S.SORTCLT,1(R8)     CLT                                          
         MVC   S.SORTPRD,3(R8)     PRD                                          
         MVC   S.SORTMKT,4(R8)     MKT                                          
         MVC   S.SORTSTA,6(R8)     STA                                          
         MVC   S.SORTEST,9(R8)     EST                                          
*                                                                               
         MVC   S.SORTLIN+1(1),10(R8)    LIN (1-BYTE)                            
         TM    15(R8),BUYRLN2      2-BYTE LINE NUMBERS?                         
         BZ    *+10                                                             
         MVC   S.SORTLIN,10(R8)    LIN (2-BYTE)                                 
*                                                                               
         MVC   S.SORTSTAT,13(R8)   STATUS                                       
         AP    SORTCNT,=P'1'                                                    
         ZAP   S.SORTSEQ,SORTCNT                                                
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
*                                                                               
*=============================================================                  
* ADD TSAR RECORDS FOR SPILL MARKETS TOO IF QOPT2=I OR J                        
*=============================================================                  
                                                                                
         CLI   QOPT2,C'I'          TEST ISCI MODE                               
         BE    *+12                NO - SPILL MKTS NOT NEEDED                   
         CLI   QOPT2,C'J'          OR AFFID RERATE MODE                         
         BNE   REQF12              NO - SPILL MKTS NOT NEEDED                   
*                                                                               
         LA    R6,BDELEM                                                        
*                                                                               
REQF20   SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0                                                          
         BE    REQF12                                                           
*                                                                               
         CLI   0(R6),3                                                          
         BNE   REQF20                                                           
*                                                                               
         MVC   S.SORTMKT,4(R6)     MKT                                          
         MVI   S.SORTSPL,X'80'     SET SPILL FLAG                               
         AP    SORTCNT,=P'1'                                                    
         ZAP   S.SORTSEQ,SORTCNT                                                
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         B     REQF20                                                           
         DROP  R8                                                               
         DROP  S                                                                
                                                                                
*===============================================================                
* EOF ON RECOVERY FILE                                                          
*===============================================================                
                                                                                
REQF30   CLOSE RECVIN                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L    R0,TSARBUFL            GET LENGTH OF BUFFER                       
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)  ANY,ANY MEANS 31-BIT STORAGE          
         LTR   RF,RF                 TEST ALL WENT WELL                         
         BZ    *+6                                                              
         DC    H'0'                  THIS SHOULDN'T HAPPEN                      
         ST    R1,TSARBUFF           SAVE BUFFER ADDRESS                        
*                                                                               
         XC    TSAREA,TSAREA         CLEAR PARAMETER BLOCK                      
T        USING TSARD,TSAREA                                                     
*                                                                               
         MVI   T.TSOFFACT,TSAINI     SET ACTION = INITIALIZE                    
         MVC   T.TSABUF,TSARBUFF     SET BUFFER ADDRESS                         
         MVC   T.TSAREC,TSARBUFL     SET BUFFLEN HERE                           
         OI    T.TSIND2,TSI2MANY     4 BYTE REC COUNT/NUMBER                    
         LHI   R0,L'SORTREC-4        GET KEY LENGTH                             
         STC   R0,T.TSKEYL                                                      
         LHI   R0,L'SORTREC          GET RECORD LENGTH                          
         STH   R0,T.TSRECL                                                      
*                                                                               
         GOTO1 VTSAROFF,TSAREA                                                  
         CLI   T.TSERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CP    SORTCNT,=P'0'       TEST ANY SORT RECORDS                        
         BNE   REQF40                                                           
         GOTO1 =V(SORTER),DMCB,=C'END'  NO - END IT ALL                         
         B     REQFX                                                            
*                                                                               
REQF40   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
*                                                                               
         ICM   RE,15,4(R1)                                                      
         BZ    REQFX                                                            
         MVC   SORTREC,0(RE)                                                    
*                                                                               
         CLC   SVSORT(12),SORTREC  SAME A-M/CLT/PRD/MKT/STA/EST/LIN             
         BE    REQF40              YES - ONLY NEED IT ONCE!                     
         MVC   SVSORT,SORTREC                                                   
         MVI   T.TSOFFACT,TSAADD                                                
         LA    RE,SORTREC                                                       
         ST    RE,T.TSAREC                                                      
         GOTO1 VTSAROFF,TSAREA                                                  
         CLI   T.TSERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         B     REQF40                                                           
*                                                                               
REQFX    MVI   SORTED,C'Y'                                                      
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* CLTFRST PROCESSING                                                  *         
*=====================================================================*         
                                                                                
CLTF     NTR1  BASE=*,LABEL=*                                                   
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
*                                                                               
         CLI   QCONTREQ,C'*'       DO WE HAVE 2ND REQ CARD?                     
         BNE   CLTF1               NO, NO CLIENT RANGE FILTERING                
* MUST HAVE BOTH START AND END LETTERS FOR CC RANGE FILTERING                   
         CLC   Q2CLTST,=3C' '      CLIENT CODE RANGE START?                     
         BNH   CLTF1               NO - NO FILTERING                            
         CLC   Q2CLTEND,=3C' '     CLIENT CODE RANGE END?                       
         BNH   CLTF1               NO - NO FILTERING                            
*                                                                               
         CLC   Q2CLTST,CLT                                                      
         BH    *+14                                                             
         CLC   Q2CLTEND,CLT                                                     
         BNL   CLTF1                                                            
         MVI   MODE,CLTLAST        SKIP THIS CLIENT                             
         J     EXIT                                                             
*                                                                               
CLTF1    DS    0H                                                               
         CLI   QOPT3,C'Y'          TEST FOLLOW J1 EXCLUSION OPT                 
         BNE   CLTF2               NO                                           
         TM    COPT2,COP2EXDB      TEST EXCLUDE FROM J1 REQUEST                 
         BZ    CLTF2                                                            
         MVI   MODE,CLTLAST                                                     
         J     EXIT                                                             
*                                                                               
CLTF2    L     R1,VSLNTAB                                                       
         LH    RE,0(R1)            ENTRY LENGTH                                 
         L     RF,2(R1)            TABLE LENGTH                                 
         AR    RF,R1               SET EOT ADDRESS                              
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         MVI   BYTE,C'T'                                                        
         CLI   QMED,C'T'                                                        
         BE    CLTF4                                                            
         CLI   QMED,C'N'                                                        
         BE    CLTF4                                                            
         CLI   QMED,C'C'                                                        
         BE    CLTF4                                                            
*                                                                               
         MVI   BYTE,C'R'                                                        
         CLI   QMED,C'R'                                                        
         BE    CLTF4                                                            
         CLI   QMED,C'X'                                                        
         BE    CLTF4                                                            
         DC    H'0'                                                             
                                                                                
*                                                                               
CLTF4    CLC   =C'00',0(R1)        TEST DEFAULT TABLE                           
         BE    CLTF6                                                            
         CLC   0(2,R1),QAGY        ELSE MATCH AGY                               
         BNE   *+14                                                             
CLTF6    CLC   BYTE,2(R1)          AND MEDIA                                    
         BE    CLTF8                                                            
*                                                                               
         BXLE  R1,RE,CLTF4                                                      
         DC    H'0'                                                             
*                                                                               
CLTF8    AHI   R1,4                POINT BEYOND HEADER                          
         ST    R1,MYSLNTAB         AND SET AS SLNTAB ADDRESS                    
*                                                                               
         XC    SVB0PROF,SVB0PROF                                                
         MVC   WORK(12),=CL12'S0B0'                                             
         MVC   WORK+4(3),SVAGY                                                  
         MVC   WORK+7(3),CLIENT                                                 
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         MVC   WORK+24(12),WORK    SAVE PROFILE KEY                             
         DROP  R6                                                               
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,SVB0PROF,DATAMGR                               
*                                                                               
         CLI   QOPT2,C'I'          TEST ISCI EXTRACT                            
         BE    CLTF10                                                           
         CLI   QOPT2,C'J'          SRECS NOW HAVE FILMS TOO                     
         BE    CLTF10                                                           
         B     CLTF20                                                           
*                                                                               
CLTF10   BAS   RE,BLDCML                                                        
*                                                                               
CLTF20   DS    0H                                                               
         BRAS  RE,ISCANADA                                                      
         JNE   CLTFX                                                            
         CLI   QMED,C'C'                                                        
         JE    *+12                                                             
         CLI   QMED,C'N'                                                        
         JNE   CLTFX                                                            
         BRAS  RE,BLDNET           BUILD NETWORK TABLE                          
*                                                                               
         BRAS  RE,BLDSTNET         BUILD TABLE OF NET STATIONS BOUGHT           
*                                                                               
         CLI   PROGPROF+12,C'Y'    TEST TO REPORT NETWORK AFFIDS                
         BNE   CLTFX                                                            
*                                                                               
CLTFX    J     EXIT                                                             
         EJECT                                                                  
*==============================================================                 
* BUILD COMMERCIAL TABLE                                                        
* NOTE THIS CODE SUPPRESSES TRACING - IT PRINTS TOO MUCH                        
*==============================================================                 
*                                                                               
BLDCML   NTR1                                                                   
*                                                                               
         L     RE,ADCLT                                                         
         CLC   CMLTBID,0(RE)       SAME A-M/CLT                                 
         JE    BLDCMLX                                                          
         MVC   CMLTBID,0(RE)       SAVE A-M/CLT                                 
*                                                                               
         MVC   CMLSVTRC,RCTRACE    SAVE TRACE FLAG                              
         MVI   RCTRACE,C'N'        AND TURN TRACE OFF                           
*                                                                               
         L     R0,CMLPAR2          GET A(MYCMLTAB)                              
         L     R1,CMLTABL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         SAM31                                                                  
         MVCL  R0,RE                                                            
         SAM24                                                                  
         XC    CMLPAR3,CMLPAR3     CLEAR RECORD COUNT                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         L     RE,ADCLT                                                         
         MVC   KEY+2(3),1(RE)      A/M, CLIENT                                  
*                                                                               
         BRAS  RE,ISCANADA                                                      
         BNE   BLDCML50                                                         
*                                                                               
         L     RE,ADCLT                                                         
         LLC   RE,CKEYAM-CLTHDR(RE) CLIENT'S A/M BYTE                           
         NILL  GRE,X'000F'         ZERO OUT AGENCY NIBBLE                       
         CHI   RE,8                MEDIA 'C'?                                   
         BNE   BLDCML50            NO - REGULAR PROCESSING                      
*                                                                               
* SPECIAL PROCESSING FOR CANADA, MEDIA "C" BUYS HERE                            
* MUST READ BOTH MEDIA N AND MEDIA T COMMERCIAL                                 
*                                                                               
         NI    KEY+2,X'F0'         TURN OFF MEDIA NIBBLE                        
         OI    KEY+2,X'01'         SET MEDIA TO 'T'                             
         BAS   RE,BLDCM2                                                        
*                                                                               
* NOW REPEAT FOR MEDIA 'N'                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         L     RE,ADCLT                                                         
         MVC   KEY+2(3),1(RE)      A/M, CLIENT                                  
         NI    KEY+2,X'F0'         TURN OFF MEDIA NIBBLE                        
         OI    KEY+2,X'03'         SET MEDIA TO 'N'                             
*                                                                               
BLDCML50 DS    0H                                                               
         BAS   RE,BLDCM2                                                        
*                                                                               
BLDCMLX  MVC   RCTRACE,CMLSVTRC                                                 
         J     EXIT                                                             
CMLSVTRC DS    X                                                                
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
* BUILD TABLE OF COMMERCIALS FOR GIVEN A/M, CLT                                 
* KEY IS EXPECTED TO HAVE X'0A21',A/M,CLT FILLED IN                             
BLDCM2   NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'TRFDIR',KEYSAVE,KEY                       
*                                                                               
* SKIP CMMLSEQ RECORD                                                           
*                                                                               
BLDCM22  GOTO1 DATAMGR,DMCB,DMRSEQ,=C'TRFDIR',KEYSAVE,KEY                       
*                                                                               
BLDCM24  CLC   KEY(5),KEYSAVE      0A21/A-M/CLT                                 
         JNE   EQXIT                                                            
*                                                                               
         L     R6,ADBUY                                                         
         USING CMLRECD,R6                                                       
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,=C'TRFFILE',KEY+14,(R6),DMWORK               
*                                                                               
         XC    MYCMLREC,MYCMLREC                                                
*                                                                               
         MVC   MYCBAGYMD,CMLKAM     A/M                                         
         MVC   MYCSEQ,CMLSEQ+1      SAVE 2 BYTES OF CMML SEQ                    
         MVC   MYCADID,SPACES       DEFAULT AD-ID SPACES                        
         MVC   MYCEBC,SPACES        MAKE SURE EBC IS SPACES IF PACKED           
         TM    CMLRSTAT,CMLKSTA_PCKD                                            
         BNZ   *+10                                                             
         MVC   MYCEBC,CMLKCML        AND COMMERCIAL CODE                        
         MVC   MYCDESC1,CMLTITLE     USE 15 BYTE TITLE                          
         MVC   MYCCLS,CMLCLASS       AND 4-BYTE CLASS                           
*                                                                               
         MVI   ELCDLO,X'30'          FIND DESC ELEMENTS                         
         MVI   ELCDHI,X'30'                                                     
         LA    R4,MYCDESC2                                                      
         AHI   R6,24                                                            
*                                                                               
BLDCM26  BRAS  RE,NEXTEL                                                        
         BNE   BLDCM210                                                         
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-4               SET FOR EX                                   
         CHI   RE,19               MAX LEN IS 20                                
         BNH   *+8                                                              
         LHI   RE,19                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),3(R6)                                                    
         OC    0(20,R4),SPACES                                                  
         AHI   R4,L'MYCDESC2                                                    
         B     BLDCM26                                                          
*                                                                               
BLDCM210 LA    R1,MYCDESC1          REMOVE SEMICOLONS FROM TEXT                 
         LHI   R0,MYCMLX-MYCDESC1                                               
*                                                                               
BLDCM212 CLC   0(1,R1),EOLCHR      TEST SEMICOLON                               
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLC   0(1,R1),EORCHR      OR COLON                                     
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         AHI   R1,1                                                             
         BCT   R0,BLDCM212                                                      
*                                                                               
         L     R6,ADBUY                                                         
         AHI   R6,24                                                            
         MVI   ELCDLO,X'A0'         FIND ADID ELEMENTS                          
         MVI   ELCDHI,X'A0'                                                     
*                                                                               
BLDCM220 BRAS  RE,NEXTEL                                                        
         BNE   BLDCM230                                                         
         USING CMLADIEL,R6                                                      
         MVC   MYCADID,CMLADID                                                  
         DROP  R6                                                               
*                                                                               
BLDCM230 DS    0H                                                               
         MVI   CMLPAR4,X'01'       SET 'INSERT IF NOT FOUND'                    
         SAM31                                                                  
         GOTO1 VBINSR31,CMLPAR1,MYCMLREC                                        
         SAM24                                                                  
         OC    CMLPAR1+1(3),CMLPAR1+1                                           
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         B     BLDCM22                                                          
         LTORG                                                                  
*                                                                               
*=====================================================================*         
* ESTIMATE FIRST PROCESSING                                           *         
*=====================================================================*         
                                                                                
ESTF     NTR1  BASE=*,LABEL=*                                                   
                                                                                
         L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
*                                                                               
         CLC   EEND,MAXEND         TEST EST ENDED 5 OR MORE YEARS AGO           
         BNH   ESTF2                YES                                         
*                                                                               
         OC    AUTOSTRT,AUTOSTRT   TEST AUTO DATES ACTIVE                       
         BZ    ESTF6                                                            
         CLC   EEND,AUTOSTRT       EST END BEFORE REQ START                     
         BL    ESTF2                                                            
         CLC   ESTART,AUTOEND      EST START AFTER REQ END                      
         BH    ESTF2                                                            
         B     ESTF6                                                            
*                                                                               
ESTF2    MVI   MODE,ESTLAST                                                     
         J     EXIT                                                             
*                                                                               
ESTF6    XC    SVBOOK,SVBOOK                                                    
         XC    SVRERATE,SVRERATE                                                
*                                                                               
         CLI   QOPT2,C'J'          TEST BUYLINE RERATE                          
         BNE   *+10                                                             
         MVC   QBOOK1(6),=C'ACT NO'                                             
*                                                                               
         CLI   QBOOK1,C' '                                                      
         BE    ESTF10                                                           
*                                                                               
         CLC   =C'ACT',QBOOK1                                                   
         BNE   ESTF8                                                            
         MVC   SVBOOK,=X'FFFF'                                                  
         B     ESTF10                                                           
*                                                                               
ESTF8    PACK  DUB,QBOOK1(2)                                                    
         CVB   R0,DUB                                                           
         STC   R0,SVBOOK                                                        
*                                                                               
         PACK  DUB,QBOOK1+2(2)                                                  
         CVB   R0,DUB                                                           
         STC   R0,SVBOOK+1                                                      
*                                                                               
ESTF10   LHI   R0,2                SET EST ADJ                                  
         CLI   QRERATE,C' '                                                     
         BE    ESTF12                                                           
*                                                                               
         LHI   R0,3                SET FOR PURCHASED RERATED                    
         CLC   =C'NO',QHUT1                                                     
         BE    *+8                                                              
         AHI   R0,1                SET FOR ADJUSTMENT                           
*                                                                               
         CLI   QRERATE,C'I'        RERATE BASED ON INVOICE                      
         BNE   *+8                                                              
         AHI   R0,3                                                             
                                                                                
ESTF12   ST    R0,SVRERATE                                                      
*                                                                               
         MVC   PRDBUFLN,=Y(PTBUFFL) INITIALIZE PROPERLY                         
         L     R0,PRDBUFF                                                       
         LHI   R1,220              MAX NUMBER OF PRDS                           
         MH    R1,PRDBUFLN                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BRAS  RE,SETEST           SET ESTHDR DEMOS IN PTBUFF                   
*                                                                               
         CLI   EPRDCD+1,255        TEST THIS IS POL EST                         
         BE    ESTF14                                                           
                                                                                
* COPY BRAND DATA TO POL EST SO CAN FIND IT LATER                               
                                                                                
         MVI   EPRDCD+1,X'FF'      SET TO DO POL ENTRY                          
         BRAS  RE,SETEST                                                        
                                                                                
* COPY POL (220) TO UNALL (219)                                                 
                                                                                
ESTF14   MVI   EPRDCD+1,219        SET PRD CODE IN EST FOR UNALL                
         BRAS  RE,SETEST           AND SET VALUES FOR IT                        
                                                                                
* FILL PRODUCT CODES INTO BUFFER                                                
                                                                                
ESTF20   CLC   QCODE,=C'UF'        TEST FINANCIAL REQUEST                       
         BE    *+12                YES                                          
         CLI   QOPT2,C'F'          OR FINANCIAL TURNAROUND                      
         BNE   ESTF30              IF NOT, READ ESTS FOR DEMOS                  
*                                                                               
         L     R4,ADCLT                                                         
         AHI   R4,CLIST-CLTHDRD                                                 
*                                                                               
ESTF22   CLI   0(R4),C'A'                                                       
         BL    ESTF40                                                           
         SR    RE,RE                                                            
         IC    RE,3(R4)                                                         
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF                                                       
         USING PTBUFFD,RE                                                       
         MVC   PTPRDN,3(R4)                                                     
         MVC   PTPRDA,0(R4)                                                     
         LA    R4,4(R4)                                                         
         B     ESTF22                                                           
         EJECT                                                                  
*================================================================               
* READ BRAND ESTIMATES TO GET DEMO LISTS                                        
*================================================================               
                                                                                
ESTF30   L     R4,ADCLT                                                         
         AHI   R4,CLIST-CLTHDRD                                                 
*                                                                               
ESTF32   XC    KEY,KEY                                                          
         MVC   KEY(13),EKEY                                                     
         CLC   =C'AAA',0(R4)                                                    
         BE    ESTF34                                                           
         CLC   =C'POL',0(R4)                                                    
         BE    ESTF34                                                           
         MVC   KEY+4(3),0(R4)      SET PRD CODE                                 
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   ESTF34                                                           
*                                                                               
         GOTO1 GETEST                                                           
*                                                                               
         BRAS  RE,SETEST           SET PTBUFF/NONT DEMO ENTRIES                 
*                                                                               
ESTF34   LA    R4,4(R4)            NEXT PRODUCT IN CLTLIST                      
         CLI   0(R4),C'A'                                                       
         BNL   ESTF32                                                           
* RESTORE POL ESTIMATE VALUES                                                   
         MVC   KEY(13),EKEY                                                     
         MVC   KEY+4(3),=C'POL'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETEST                                                           
*                                                                               
ESTF40   L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
*                                                                               
         LR    R0,R3                                                            
         LHI   R1,MEDDATA-MEDBLOCK                                              
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         MVCL  R0,R4                                                            
*                                                                               
         LHI   R0,256                                                           
         STCM  R0,15,MEDLCHNK                                                   
         LHI   R0,1                                                             
         STCM  R0,15,MEDNUMPE                                                   
*                                                                               
         MVI   MEDEXTAX,C'Y'       SET TO EXTRACT TAX                           
*                                                                               
         CLC   QCODE,=C'UF'        TEST FINANCIAL T/A                           
         BE    ESTF42                                                           
         CLI   QOPT2,C'F'          OR FINANCIAL REQ                             
         BE    ESTF42                                                           
* MEDIA ONLY VALUES                                                             
         LHI   R0,56               DO 56 WEEKS BECAUSE OF QUARTERS              
         ST    R0,MEDNUMWK                                                      
         MVI   MEDEXTDM,14         ALWAYS 14 DEMOS                              
         B     ESTF44                                                           
*                                                                               
ESTF42   LHI   R0,13               FINANCIAL REQ DOES 13 MONTHS                 
         ST    R0,MEDNUMMO                                                      
         MVI   MEDEXTDM,0          NO DEMOS                                     
         MVI   MEDEXTAC,C'Y'                                                    
         DROP  R3                                                               
*                                                                               
ESTF44   DS    0H                                                               
         L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
*                                                                               
         MVC   SVQSTART(12),QSTART SAVE QSTART/QEND                             
*                                                                               
         OC    AUTOSTRT,AUTOSTRT                                                
         BZ    *+10                                                             
         MVC   QSTART(12),AUTOSTRT                                              
*                                                                               
         CLC   QSTART,ESTART       USE LATER OF REQ OR EST STARTS               
         BH    *+10                                                             
         MVC   QSTART,ESTART                                                    
*                                                                               
         CLC   QEND,EEND           USE EARLIER OF EST OR REQ END                
         BL    *+10                                                             
         MVC   QEND,EEND                                                        
*                                                                               
         GOTO1 DATCON,DMCB,QSTART,(2,BQSTARTP)                                  
         GOTO1 (RF),(R1),QEND,(2,BQENDP)                                        
*                                                                               
         MVI   SPOTPROF+2,X'00'    NEVER EVER DO NON-BDCST WEEKS                
*                                                                               
         MVC   SV00DAY,SPOTPROF+8                                               
         CLI   EOWSDAY,0           OOW ROTATOR?                                 
         BE    *+10                NO                                           
         MVC   SPOTPROF+8(1),EOWSDAY YES - NEW FISCAL BASE DAY NUMBER           
*                                                                               
         GOTO1 MEDDATE,DMCB,SPWORKD                                             
*                                                                               
         MVC   QSTART(12),SVQSTART RESTORE REQUEST DATES                        
*                                                                               
         CLI   QOPT2,C'F'          TEST FINANCIAL REQUEST                       
         BE    EST46               YES - PROCESS BILLS                          
         CLC   QCODE,=C'UF'        TEST T/A REQUEST                             
         JNE   EXIT                                                             
         CLI   QOPT2,C'R'          TEST RECEIVABLE T/A                          
         JNE   EXIT                                                             
*                                                                               
EST46    SR    R0,R0                                                            
         IC    R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISEST,DUB                                                      
*                                                                               
         GOTOR BUFFERIN,DMCB,('BUFFAINI',BUFFET),0,ACOMFACS                     
*                                                                               
         MVC   THISAGY,QAGY        SET THESE FOR OUTPUT                         
         MVC   THISMED,QMED                                                     
         MVC   THISCLT,CLT                                                      
*                                                                               
         BRAS  RE,PRBILL           PROCESS BILL RECORDS                         
*                                                                               
         BRAS  RE,OUTBILL          OUTPUT BILL RECORDS                          
*                                                                               
         CLI   QOPT2,C'R'          TEST RECEIVABLE T/A                          
         JNE   EXIT                                                             
         GOTO1 AENDREQ             END OF REQUEST (NO RETURN HERE)              
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
ESTL     NTR1  BASE=*,LABEL=*                                                   
         MVC   SPOTPROF+8(1),SV00DAY  SAVED FISCAL BASE DAY NUMBER              
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*=================================================================              
* PROCESS STATION BILLING RECORDS FOR FINANCIAL REQUEST                         
*=================================================================              
                                                                                
PRBILL   NTR1  BASE=*,LABEL=*                                                   
         GOTOR BUFFERIN,DMCB,('BUFFAINI',BUFFET),0,ACOMFACS                     
         XC    BUFFCNT,BUFFCNT     CLEAR BUFFALO COUNTER                        
*                                                                               
         MVI   SVCNMED,0                                                        
         BRAS  RE,ISCANADA                                                      
         BNE   PRB0                                                             
         CLI   QMED,C'C'           COMBINED?                                    
         BNE   PRB0                                                             
         MVI   SVCNMED,X'01'       TV                                           
*                                                                               
PRB0     DS    0H                                                               
         XC    KEY,KEY                                                          
K        USING STABUCKD,KEY                                                     
         MVC   K.STABKCOD,=X'0E01'                                              
*                                                                               
         L     RE,ADEST                                                         
         MVC   K.STABKAM,1(RE)     A-M                                          
*                                                                               
         CLI   SVCNMED,X'00'                                                    
         BE    PRB1                                                             
         NI    K.STABKAM,X'F0'                                                  
         OC    K.STABKAM,SVCNMED                                                
*                                                                               
PRB1     DS    0H                                                               
         MVC   K.STABKCLT,2(RE)     MOVE A-M/CLT                                
         MVC   K.STABKEST,BEST     SET ESTNUM                                   
*                                                                               
         MVI   K.STABKPRD,1        SET FIRST POSSIBLE PRODUCT                   
         CLC   QCODE,=C'UF'                                                     
         BNE   PRB2                                                             
         MVC   K.STABKPRD,BPRD     SET PRODUCT CODE                             
*                                                                               
PRB2     GOTO1 HIGH                                                             
         B     PRB6                                                             
*                                                                               
PRB4     GOTO1 SEQ                                                              
*                                                                               
PRB6     CLC   KEY(5),KEYSAVE      TEST SAME 0E01/A-M/CLT                       
         BNE   PRBX                NO - DONE                                    
*                                                                               
         CLC   QCODE,=C'UF'        TEST FINANCIAL T/A                           
         BNE   *+14                                                             
         CLC   KEY(6),KEYSAVE      TEST SAME 0E01/A-M/CLT/PRD                   
         BNE   PRBX                                                             
*                                                                               
         CLC   K.STABKEST,BEST     COMPARE ESTIMATE                             
         BE    PRB12                                                            
         BH    PRB8                IF HIGH - NEXT PRD                           
         B     PRB10               ELSE READ HIGH FOR EST                       
*                                                                               
PRB8     MVC   K.STABKEST(4),=X'FFFFFFFF'  FORCE NEXT PRD                       
         B     PRB2                                                             
*                                                                               
PRB10    MVC   K.STABKEST,BEST                                                  
         XC    K.STABKMKT(6),K.STABKMKT CLEAR MKT/STA/TYPE                      
         B     PRB2                                                             
         DROP  K                                                                
*                                                                               
PRB12    L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         USING STABUCKD,R6                                                      
         GOTO1 GET                                                              
*                                                                               
*======================================================*                        
* SEARCH FOR ELEMENTS IN REQUEST PERIOD                *                        
*======================================================*                        
                                                                                
         AHI   R6,24               POINT TO FIRST ELEMENT                       
PRB20    CLI   0(R6),0                                                          
         BE    PRB4                                                             
         CLI   0(R6),X'0E'                                                      
         BE    PRB24                                                            
*                                                                               
PRB22    SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         B     PRB20                                                            
*                                                                               
K        USING STABUCKD,KEY                                                     
PRB24    XC    BFKEY,BFKEY                                                      
         MVI   BFFKTYP,C'F'                                                     
*                                                                               
         CLI   SVCNMED,X'00'                                                    
         BE    PRB24A                                                           
*                                                                               
         CLI   SVCNMED,X'01'                                                    
         BNE   *+8                                                              
         MVI   BFFKQMED,C'T'                                                    
         CLI   SVCNMED,X'03'                                                    
         BNE   *+8                                                              
         MVI   BFFKQMED,C'N'                                                    
*                                                                               
PRB24A   DS    0H                                                               
         MVC   BFFKMKT,K.STABKMKT                                               
         MVC   BFFKSTA,K.STABKSTA                                               
*                                                                               
         BRAS  RE,ISCANADA                                                      
         BNE   PRB25                                                            
         MVC   BYTE,K.STABKAM                                                   
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'03'                                                       
         BNE   PRB25                                                            
         OI    BFFKFLG,BFFKFCNQ                                                 
*                                                                               
PRB25    DS    0H                                                               
         SR    RE,RE                                                            
         IC    RE,K.STABKPRD         PRODUCT NUMBER                             
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF          POINT TO PRDBUFF ENTRY                       
         USING PTBUFFD,RE                                                       
*                                                                               
PRB32    MVC   BFFKQPRD,PTPRDA                                                  
         DROP  K,RE                                                             
*                                                                               
         USING STABELEM,R6                                                      
*                                                                               
PRB34    MVC   BFFKYM,STABPER                                                   
*                                                                               
         CLI   BFFKYM+1,X'0D'                                                   
         BNE   *+8                                                              
         MVI   BFFKYM+1,X'0C'                                                   
*                                                                               
         XC    BFFDATA,BFFDATA                                                  
         SR    R0,R0                                                            
         ICM   R0,12,STABSPTS      LEFT ALIGN                                   
         SRA   R0,16                                                            
         ST    R0,BFFSPOTS                                                      
         MVC   BFFGRS,STABGRS                                                   
         MVC   BFFNET,STABNET                                                   
*                                                                               
         CLI   STABELEM+1,21       TEST TAX PRESENT                             
         BL    PRB36               NO                                           
         MVC   FULL(3),STABTAX                                                  
         CLI   STABELEM+1,22       IF LEN=22 TAX AT STABTAX+1                   
         BNE   *+10                NO                                           
         MVC   FULL(3),STABTAX+1                                                
         L     R0,FULL                                                          
         SRA   R0,8                                                             
         ST    R0,BFFTAX                                                        
*                                                                               
PRB36    GOTOR PUTBUF                                                           
         B     PRB22                                                            
*                                                                               
PRBX     DS    0H                                                               
         CLI   SVCNMED,X'01'       LAST MEDIA DONE = TV?                        
         JNE   EQXIT               NO - EXIT                                    
         MVI   SVCNMED,X'03'       DO NETWORK NOW                               
         J     PRB0                                                             
*                                                                               
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
*=================================================================              
* OUTBILL - FOR FINANCIAL REQUESTS                                              
* READ BUFFALO BILL RECORDS AND OUTPUT                                          
*=================================================================              
                                                                                
         USING BUFFPARM,R1                                                      
*                                                                               
OUTBILL  NTR1  BASE=*,LABEL=*                                                   
         OC    BUFFCNT,BUFFCNT                                                  
         JZ    EXIT                                                             
         XC    BFKEY,BFKEY                                                      
         GOTOR BUFFERIN,DMCB,('BUFFARDH',BUFFET),BFREC,ACOMFACS                 
         B     OUTBL12                                                          
                                                                                
OUTBL10  GOTOR BUFFERIN,DMCB,('BUFFASEQ',BUFFET),BFREC,ACOMFACS                 
*                                                                               
OUTBL12  CLI   BUFFERRS,BUFFEEOF                                                
         JE    EXIT                                                             
         DROP  R1                                                               
*                                                                               
OUTBL14  CLI   BFKEY,C'F'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   BFFKQMED,X'00'                                                   
         BE    *+10                                                             
         MVC   THISMED,BFFKQMED                                                 
*                                                                               
         GOTOR MSUNPK,DMCB,(X'80',BFFKMKT),THISMKT,THISSTA                      
         CLI   THISSTA+4,C'T'                                                   
         BNE   *+8                                                              
         MVI   THISSTA+4,C' '                                                   
*                                                                               
         XC    THISNET,THISNET                                                  
         TM    BFFKFLG,BFFKFCNQ                                                 
         BZ    OUTBL15                                                          
         LA    R1,BFFKSTA                                                       
         BRAS  RE,GETCNET                                                       
         BNE   *+10                                                             
         MVC   THISNET,WORK                                                     
*                                                                               
OUTBL15  DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,BFFKYM),(23,THISYYMM) NO FUNNY DATES              
         MVC   THISYYMM+7(3),=C'-01'                                            
*                                                                               
         L     R4,ARRECTAB                                                      
         BRAS  RE,OUTPUT                                                        
         B     OUTBL10                                                          
*                                                                               
*=====================================================================*         
* MKTFRST PROCESSING                                                            
* NOTE THAT FOR TURNAROUND ISCI MODE THIS ROUTINE WILL INVOKE                   
* STAFRST AND PROCBUY CALLS                                                     
* WHEN DONE, CLEARING SVBUYKEY FORCES SP0003 TO SKIP ALL                        
* BUY PROCESSING AND GO ON TO GOALS !                                           
*=====================================================================*         
                                                                                
MKTF     NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTOR BUFFERIN,DMCB,('BUFFAINI',BUFFET),0,ACOMFACS                     
*                                                                               
         XC    BUFFCNT,BUFFCNT     CLEAR BUFFALO COUNTER                        
         MVC   THISAGY,QAGY                                                     
         MVC   THISMED,QMED                                                     
         MVC   THISCLT,CLT                                                      
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,BMKT                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISMKT,DUB                                                      
* BUILD BASE OF THISID                                                          
         XC    THISID,THISID                                                    
         MVC   TIDAGYA,QAGY                                                     
         MVC   TIDQMED,QMED                                                     
         MVC   TIDBCLT,BCLT                                                     
*                                                                               
         CLC   QCODE,=C'UB'                                                     
         BNE   MKTFX                                                            
         CLI   QOPT2,C'I'          TEST ISCI MODE                               
         BE    MKTF2                                                            
         CLI   QOPT2,C'J'          TEST RERATE MODE                             
         BE    MKTF2                                                            
         B     MKTFX                                                            
*                                                                               
*==============================================================                 
* ISCI TURNAROUND PROCESSING                                                    
* FIND TSAR RECORDS FOR THIS MARKET                                             
*==============================================================                 
                                                                                
MKTF2    XC    SVBUYKEY,SVBUYKEY                                                
*                                                                               
S        USING SORTRECD,SORTREC                                                 
         XC    SORTREC,SORTREC                                                  
*                                                                               
         MVC   S.SORTAGMD,BAGYMD   A-M                                          
         MVC   S.SORTCLT,BCLT      CLT                                          
         MVC   S.SORTPRD,BPRD      PRD                                          
         MVC   S.SORTEST,BEST      EST                                          
         MVC   S.SORTMKT,BMKT      MKT                                          
         MVC   SVSORT,SORTREC                                                   
*                                                                               
         LA    RE,SORTREC                                                       
         ST    RE,T.TSAREC                                                      
*                                                                               
         MVI   T.TSOFFACT,TSARDH                                                
         GOTO1 VTSAROFF,TSAREA                                                  
         TM    T.TSERRS,TSEEOF                                                  
         BO    MKFT24                                                           
*                                                                               
MKTF10   CLC   SVSORT(7),SORTREC  A-M/CLT/PRD/EST/MKT                           
         BNE   MKFT24                                                           
*                                                                               
         XC    KEY,KEY                                                          
B        USING BUYKEY,KEY                                                       
         MVC   B.BUYKAM,S.SORTAGMD                                              
         MVC   B.BUYKCLT,S.SORTCLT                                              
         MVC   B.BUYKPRD,S.SORTPRD                                              
         MVC   B.BUYKMSTA,S.SORTMKT   MKT-STA                                   
         MVC   B.BUYKEST,S.SORTEST                                              
         MVC   B.BUYKBUY(1),S.SORTSPL  SET SPILL FLAG                           
*                                                                               
         MVC   B.BUYKBUY+1(1),S.SORTLIN+1                                       
         CLI   SV1OR2,2                                                         
         BNE   *+10                                                             
         MVC   B.BUYKLIN,S.SORTLIN                                              
*                                                                               
MKTF12   MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'      DO NOT TEST REC DELETED                      
         GOTO1 HIGH                                                             
*                                                                               
         LHI   RF,12                                                            
         CLI   SV1OR2,2                                                         
         BNE   *+8                                                              
         LHI   RF,13                                                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE     SAME A-M/CL/PR/MK/ST/ES/SPL/LIN               
         BNE   MKFT22                                                           
*                                                                               
MKTF14   GOTO1 GETBUY              IF SPILL, MKT IN BUY WILL DIFFER             
*                                                                               
         CLC   KEY(9),SVBUYKEY     SAME A-M/CLT/PRD/MKT/STA                     
         BE    MKTF16                                                           
*                                                                               
         MVC   BMKTSTA,B.BUYKMSTA                                               
         MVC   SVBUYKEY,KEY          SAVE BUY DETAILS                           
         BRAS  RE,STAF                                                          
*                                                                               
MKTF16   LLC   R0,B.BUYKBUY+1      1-BYTE BUYLINE IN DIRECTORY POINTER          
         TM    B.BUYKCNTL,BUYRLN2                                               
         BZ    *+8                                                              
         ICM   R0,3,B.BUYKLIN                                                   
         EDIT  (R0),THISBUY,FILL=0                                              
*                                                                               
         CLI   QOPT2,C'I'                                                       
         BNE   MKTF18                                                           
         MVC   TIDBMKT,KEY+4       MOVE MARKET FROM DIRECTORY KEY               
         L     R4,AKLLITAB         KILL THIS BUY RECORD                         
         BRAS  RE,OUTPUT                                                        
         B     MKTF20                                                           
*                                                                               
MKTF18   CLI   QOPT2,C'J'                                                       
         BNE   MKTF20                                                           
         L     R6,ADBUY                                                         
         MVC   WORK(13),0(R6)      BUYREC KEY                                   
         MVC   WORK+4(2),KEY+4     USE MARKET FROM DIRECTORY                    
         GOTO1 HEXOUT,DMCB,WORK,THISIDX,13                                      
         L     R4,AKLLJTAB                                                      
         BRAS  RE,OUTPUT                                                        
*                                                                               
MKTF20   L     RE,ADBUY                                                         
         TM    15(RE),X'80'        TEST BUY DELETED                             
         BO    MKFT22              YES- NEXT RECORD                             
*                                                                               
         BRAS  RE,PROCB                                                         
*                                                                               
MKFT22   MVI   T.TSOFFACT,TSANXT                                                
         GOTO1 VTSAROFF,TSAREA                                                  
         CLI   T.TSERRS,0                                                       
         BE    MKTF10                                                           
*                                                                               
MKFT24   XC    SVBUYKEY,SVBUYKEY  TELL CONTROLLER NO BUYS THIS MKT              
*                                                                               
MKTFX    J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*=====================================================================*         
* STAFRST PROCESSING                                                            
*=====================================================================*         
                                                                                
STAF     NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTOR BUFFERIN,DMCB,('BUFFAINI',BUFFET),0,ACOMFACS                     
*                                                                               
         XC    BUFFCNT,BUFFCNT     CLEAR BUFFALO COUNTER                        
         MVC   TIDBSTA,BMKTSTA+2                                                
*                                                                               
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'U'                                                     
         MVC   STAPMED,QMED                                                     
         MVC   STAPAGY,AGY                                                      
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMKST,BMKTSTA                                                 
*                                                                               
         MVI   STAPCTRY,C'C'                                                    
         BRAS  RE,ISCANADA                                                      
         JE    *+8                                                              
         MVI   STAPCTRY,C'U'                                                    
*                                                                               
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         JNE   *+2                                                              
*                                                                               
         MVC   THISMKT,STAPQMKT                                                 
         MVC   THISSTA,STAPQSTA                                                 
* FOR IDF PATCH                                                                 
         CLC   =C'CBLT',THISSTA                                                 
         JNE   *+8                                                              
         J     *+4                                                              
*                                                                               
         CLI   THISSTA+4,C'T'                                                   
         BNE   *+8                                                              
         MVI   THISSTA+4,C' '                                                   
*                                                                               
STAF2    CLI   QMED,C'N'                                                        
         JE    *+12                                                             
         CLI   QMED,C'C'                                                        
         JNE   STAF10                                                           
*                                                                               
         CLI   PROGPROF+12,C'Y'    TEST TO SEND NETWORK AFFIDS                  
         JNE   STAF10                                                           
         CLI   QOPT2,C'I'                                                       
         JE    *+12                                                             
         CLI   QOPT2,C'J'                                                       
         JNE   STAF10                                                           
*                                                                               
STAF10   DS    0H                                                               
         MVC   THISNET4,SPACES                                                  
         BRAS  RE,ISCANADA                                                      
         BE    STAF20                                                           
         CLI   THISSTA,C'0'                                                     
         BL    STAF20                                                           
*                                                                               
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'X'                                                     
         MVC   STAPQNET,THISSTA+5                                               
         MVC   STAPACOM,ACOMFACS                                                
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         JNE   STAF20                                                           
         MVC   THISNET4,STAPQSTA                                                
         DROP  R1                                                               
*                                                                               
STAF20   DS    0H                                                               
*                                                                               
STAFX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* BUY RECORD PROCESSING                                                         
*=====================================================================*         
                                                                                
PROCB    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ADBUY                                                         
         CLC   LASTKEY(9),0(R6)    TEST SAME A-M/CL/PR/MK/ST(3)                 
         JE    PROCB1                                                           
         MVC   LASTKEY(9),0(R6)                                                 
*                                                                               
         BRAS  RE,ISCANADA                                                      
         BNE   PROCB1                                                           
         MVC   BYTE,0(R6)                                                       
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'03'                                                       
         BNE   PROCB1                                                           
*                                                                               
         BRAS  RE,BLDAFD           GET NET AFFIDS FOR THIS STATION              
*                                                                               
PROCB1   L     R6,ADBUY                                                         
         MVC   TIDPURP,SPACES                                                   
         CLI   SVB0PROF+9,C'Y'     TEST IDR=PURPOSE CODE                        
         BNE   PROCB2                                                           
*                                                                               
         TM    BUYRCNTL-BUYREC(R6),BUYRDEL                                      
         JO    EQXIT                                                            
*                                                                               
         L     R6,ADBUY                                                         
         LA    R6,24(R6)           POINT TO FIRST ELEMENT                       
         MVI   ELCDLO,X'70'        FIND PURPOSE CODE IF ANY                     
         MVI   ELCDHI,X'70'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   PROCB2                                                           
         MVC   TIDPURP,3(R6)                                                    
         OC    TIDPURP,SPACES                                                   
*                                                                               
PROCB2   DS    0H                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         OC    FILTSTA,FILTSTA                                                  
         BZ    *+14                                                             
         CLC   FILTSTA,BUYKMSTA                                                 
         JNE   EQXIT                                                            
*                                                                               
         BRAS  RE,SETRATE                                                       
*                                                                               
         XC    THISNET,THISNET                                                  
*                                                                               
         BRAS  RE,ISCANADA                                                      
         BNE   PROCB3                                                           
*                                                                               
         CLI   QMED,C'C'                                                        
         BNE   PROCB2A                                                          
         LA    R1,THISMED                                                       
         ICM   R1,8,BUYKAM                                                      
         BRAS  RE,HEX2MED                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PROCB2A  CLI   THISMED,C'N'        NET?                                         
         BNE   PROCB3                                                           
*                                                                               
         LA    RF,BDELEM                                                        
*                                                                               
PROCB2B  CLI   0(RF),0                                                          
         BE    PROCB3                                                           
         CLI   0(RF),NTWKCODQ                                                   
         BE    *+16                                                             
         LLC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     PROCB2B                                                          
         MVC   THISNET,2(RF)                                                    
*                                                                               
PROCB3   CLI   PROGPROF+6,C'*'     TEST TO EXCLUDE DPT                          
         BNH   PROCB4                                                           
         CLC   BDDAYPT,PROGPROF+6                                               
         JE    EXIT                                                             
*                                                                               
PROCB4   CLI   PROGPROF+7,C'*'     TEST TO EXCLUDE ANOTHER DPT                  
         BNH   PROCB6                                                           
         CLC   BDDAYPT,PROGPROF+7                                               
         JE    EXIT                                                             
*                                                                               
PROCB6   MVC   THISREP,SPACES                                                   
         OC    BDREP,BDREP                                                      
         BZ    PROCB7                                                           
         GOTO1 VRCPACK,DMCB,(C'U',BDREP),THISREP                                
*                                                                               
PROCB7  MVC   THISADJ,SPACES                                                    
*                                                                               
        L     RE,ADCLT                                                          
        USING CLTHDR,RE                                                         
*                                                                               
        CLI   CPROF+9,C'0'         TEST ADJ CODES                               
        BE    PROCB8               NO                                           
*                                                                               
        MVC   THISADJ(1),BDPROGT                                                
        CLI   CPROF+9,C'1'        TEST ALPHA ADJ                                
        BE    PROCB8              YES                                           
        DROP  RE                                                                
*                                                                               
        SR    R0,R0                                                             
        IC    R0,BDPROGT                                                        
        SRDL  R0,4                                                              
        STC   R0,THISADJ                                                        
        OI    THISADJ,X'F0'                                                     
        SRL   R1,28                                                             
        STC   R1,THISADJ+1                                                      
        OI    THISADJ+1,X'F0'                                                   
        DROP  R6                                                                
*                                                                               
PROCB8   DS    0H                                                               
*        XC    PSLIST,PSLIST                                                    
         LA    RE,PSLIST                                                        
         LHI   RF,L'PSLIST                                                      
         XCEFL                                                                  
*                                                                               
         GOTOR MEDPSL,DMCB,SPWORKD,PSLIST                                       
*                                                                               
         BRAS  RE,SETNONT          SET DEMO NUMBERS FOR NONT DEMOMS             
*                                                                               
         CLI   QOPT2,C'I'          TEST ISCI OUTPUT                             
         BE    PROCI                                                            
*                                                                               
         CLI   QOPT2,C'J'          TEST BUY RERATE                              
         BNE   PROCB10                                                          
*                                                                               
         BRAS  RE,ISCANADA                                                      
         BNE   PROCB9                                                           
*                                                                               
         L     RE,ADBUY                                                         
         LLC   R0,0(RE)            GET AG-MD                                    
         N     R0,=X'0000000F'                                                  
         CHI   R0,3                                                             
         JNE   PROCB9                                                           
         OC    BDCOST-BUYREC(L'BDCOST,RE),BDCOST-BUYREC(RE)                     
         BNZ   PROCB9                                                           
*                                                                               
         BRAS  RE,ZERODEMO                                                      
         JNE   PROCB9                                                           
*                                                                               
* HAVE ZERO COST, ZERO DEMO SITUATION HERE                                      
* CHECK INDIVIDUAL SPOTS, MAKE SURE THERE ARE NO COST OVERRIDES                 
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
PROCB8A  BRAS  RE,NEXTEL                                                        
         JNE   EXIT WENT THROUGH ALL ELEMENTS, NO COST OVERRIDE FOUND           
*                                                                               
         TM    RSTATUS-REGELEM(R6),RSRATOVQ X'02', COST OVERRIDE?               
         JZ    PROCB8A                   NO, CHECK NEXT ELEMENT                 
*                                                                               
PROCB9   DS    0H                                                               
         BRAS  RE,PROCJ                                                         
         J     EXIT                                                             
*                                                                               
PROCB10  LA    R2,PSLIST-2         USE R2 AS PSLIST POINTER                     
*                                                                               
PROCB12  LA    R2,2(R2)                                                         
         CLI   0(R2),0                                                          
         JE    EXIT                                                             
*                                                                               
         L     R3,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,R3                                                      
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
*                                                                               
         GOTOR MEDGETBY,DMCB,SPWORKD,SVRERATE                                   
*                                                                               
         MVC   TIDBMKT,KEY+4       MOVE MARKET FROM DIRECTORY KEY               
         MVC   THISSPL,MEDSPILL    SET SPILL FLAG                               
         CLI   THISSPL,C'N'                                                     
         BE    *+8                                                              
         OI    TIDBMKT,X'80'       SET SPILL MARKET FLAG                        
*                                                                               
         XC    BFREC(BFRECL),BFREC                                              
         SR    RE,RE                                                            
         IC    RE,MEDBRAND         PRODUCT NUMBER                               
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF          POINT TO PRDBUFF ENTRY                       
         USING PTBUFFD,RE                                                       
         MVC   BFKBPRD,MEDBRAND                                                 
         MVC   BFKQPRD,PTPRDA      SET 3 CHAR PRD CODE                          
*                                                                               
         CLC   BFKQPRD,SPACES                                                   
         BH    PROCB13                                                          
*                                                                               
         EDIT  (B1,BFKBPRD),(3,BFKQPRD),FILL=0                                  
*                                                                               
         DROP  RE                                                               
*                                                                               
PROCB13  DS    0H                                                               
         CLI   QOPT2,C'F'          TEST BILLING/CLEARANCE REQ                   
         BE    PROCB50                                                          
         CLC   QCODE,=C'UF'        OR BILLING/CLEARANCE T/A                     
         BE    PROCB50                                                          
*                                                                               
         MVI   BFKTYPE,C'D'        SET DEMO RECORD                              
         MVC   BFKSLN,MEDSPTLN                                                  
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         MVC   BFKMED,THISMED                                                   
         MVC   BFKNET,THISNET                                                   
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BUYKEST                                                       
         STC   R0,TIDBEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISEST,DUB                                                      
*                                                                               
         MVC   BFKDPT,BDDAYPT                                                   
         MVC   BFKDPTNM,MEDDPART   3 CHAR DPT NAME                              
*                                                                               
         MVC   BFKTIME,BDTIMST                                                  
         MVC   BFKPROG,BDPROGRM    PROGRAM NAME                                 
*                                                                               
         LA    R1,BFKPROG                                                       
         LHI   R0,L'BFKPROG                                                     
*                                                                               
PROCB14  CLC   0(1,R1),EOLCHR      CHECK FOR SEMICOL                            
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLC   0(1,R1),EORCHR      CHECK FOR COLON                              
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
*                                                                               
         AHI   R1,1                                                             
         BCT   R0,PROCB14                                                       
*                                                                               
         MVC   BFKDAY,BDDAY                                                     
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    R5,MEDWEEKS                                                      
*                                                                               
PROCB20  ST    R5,AMEDWEEK                                                      
*                                                                               
         ICM   R4,15,4(R5)         ANYTHING THERE                               
         BZ    PROCB40             NO                                           
         USING MEDDATA,R4                                                       
*                                                                               
         OC    MEDBYD(12),MEDBYD   TEST SPOTS OR DOLLARS                        
         BZ    PROCB40                                                          
         MVC   BFKWEEK,0(R5)       WEEK START DATE                              
*                                                                               
         MVC   BFSPOTS,MEDBYSPT                                                 
         MVC   BFDOL,MEDBYD                                                     
         MVC   BFDOLEQ,MEDBYDEQ                                                 
         MVC   BFTAX,MEDMSTAX                                                   
* MOVE RAW DEMO VALUES TO BUFFALO REC                                           
         LA    R4,MEDBY1           FIRST DEMO VALUE                             
         LA    R5,BFDEM01                                                       
         LHI   R6,14                                                            
*                                                                               
PROCB30  MVC   0(4,R5),0(R4)                                                    
         LA    R4,8(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R6,PROCB30                                                       
*                                                                               
         GOTOR PUTBUF                                                           
*                                                                               
PROCB40  L     R5,AMEDWEEK         GET ACCUM ADDRESS                            
         AHI   R5,L'MEDDATES       NEXT WEEK                                    
         C     R5,MEDALAST                                                      
         BH    PROCB12                                                          
*                                                                               
         LA    R0,MEDMON01                                                      
         CR    R5,R0                                                            
         BNL   PROCB12                                                          
*                                                                               
         B     PROCB20                                                          
*                                                                               
*==================================================================             
* EXTRACT FINANCIAL DATA                                                        
*==================================================================             
*                                                                               
PROCB50  CLI   MEDSPILL,C'Y'       IGNORE FINANCIAL SPILL                       
         JE    EXIT                                                             
*                                                                               
         MVI   BFKTYPE,C'C'        SET CLEARANCE RECORD                         
*                                                                               
         MVC   BFKNET,THISNET                                                   
*                                                                               
         LA    R5,MEDMON01                                                      
*                                                                               
PROCB60  ST    R5,AMEDMON                                                       
         ICM   R4,15,4(R5)          POINT TO DATA                               
         BZ    PROCB70                                                          
         USING MEDDATA,R4                                                       
*                                                                               
         OC    MEDBYD(12),MEDBYD   TEST SPOTS OR DOLLARS                        
         BZ    PROCB70                                                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,MEDBRAND         PRODUCT NUMBER                               
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF          POINT TO PRDBUFF ENTRY                       
         USING PTBUFFD,RE                                                       
*                                                                               
PROCB64  MVC   BFFKQPRD,PTPRDA                                                  
         DROP  RE                                                               
* (NOTE THAT OUTPUT OCCURS ON STALAST)                                          
         GOTO1 DATCON,DMCB,(2,2(R5)),(3,DUB)  CONVERT MONTH END DATE            
         MVC   BFFKYM,DUB                     BROADCAST YM                      
*                                                                               
*        MVC   BFFSPOTS,MEDBYPSP                                                
         MVC   BFSPOTS,MEDBYPSP                                                 
*        MVC   BFFGRS,MEDBYGRS                                                  
         MVC   BFFGRS,MEDBYPAY                                                  
*        MVC   BFFNET,MEDBYNET                                                  
         MVC   BFFNET,MEDBYNPY                                                  
*        MVC   BFFTAX,MEDMSTAX                                                  
         MVC   BFFTAX,MEDBYTXP                                                  
*                                                                               
         GOTOR PUTBUF                                                           
*                                                                               
PROCB70  L     R5,AMEDMON                                                       
         AHI   R5,L'MEDDATES       NEXT MONTH                                   
         C     R5,MEDALAST                                                      
         BH    PROCB12                                                          
*                                                                               
         LA    R0,MEDQRT01                                                      
         CR    R5,R0                                                            
         BNL   PROCB12                                                          
*                                                                               
         B     PROCB60                                                          
         DROP  R3                                                               
         LTORG                                                                  
*                                                                               
*=============================================================                  
* PROCESS ONE SPOT AT A TIME FOR ISCI DATA                                      
* USE SPOTHOOK TO SELECT THE SPOT                                               
* IF ACURSPOT=0 ON RETURN, THIS BUY IS FINISHED                                 
*=============================================================                  
                                                                                
PROCI    DS    0H                                                               
         LARL  RE,BUYHOOK                                                       
         ST    RE,SPOTHOOK                                                      
         L     RE,=A(BUYHKR9)                                                   
         STM   R9,RC,0(RE)                                                      
*                                                                               
         LA    R2,PSLIST-2         USE R2 AS PSLIST POINTER                     
*                                                                               
PROCI2   LA    R2,2(R2)                                                         
         CLI   0(R2),0                                                          
         JE    EXIT                                                             
                                                                                
         L     R3,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,R3                                                      
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
* GET DEMO NAMES FOR THIS PRD                                                   
         L     R4,PRDBUFF                                                       
         USING PTBUFFD,R4                                                       
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         BCTR  R0,0                                                             
         MH    R0,PRDBUFLN                                                      
         AR    R4,R0               POINT TO SLOT FOR THIS PRD                   
         LHI   R0,20                                                            
         GOTO1 DEMOCON,DMCB,((R0),PTDEMO),(6,THISDM01),                X        
               (C'S',ADBLOCK)                                                   
         DROP  R4                                                               
*                                                                               
         XC    MGBYSPOT,MGBYSPOT                                                
*                                                                               
PROCI4   LH    R0,MGBYSPOT                                                      
         AHI   R0,1                                                             
         STH   R0,MGBYSPOT         BUMP SEARCH SPOT NUMBER                      
*                                                                               
         XC    CURSPOT,CURSPOT     CLEAR HOOK COUNT                             
         XC    ACURSPOT,ACURSPOT   AND ADDRESS OF SPOT                          
         GOTOR MEDGETBY,DMCB,SPWORKD,SVRERATE                                   
*                                                                               
         OC    ACURSPOT,ACURSPOT   TEST SPOT FOUND                              
         BZ    PROCI2              IF NOT FOUND, NEXT LIST ENTRY                
*                                                                               
         L     R6,ACURSPOT         GET SPOT ADDRESS                             
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'10'         TEST AFFID FOLLOWS                           
         BNE   PROCI10             NO                                           
*                                                                               
         USING AFFELEM,R6                                                       
         CLC   ADATE,BQSTARTP      TEST AFFID PRIOR TO REQ START                
         BL    PROCI4                                                           
         CLC   ADATE,BQENDP        TEST AFTER REQ END                           
         BH    PROCI4                                                           
         DROP  R6                                                               
*                                                                               
         B     PROCI10     <<< PROCESS ALL SPOTS >>>                            
*                                                                               
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'12'         TEST FILM FOLLOWS                            
         BNE   PROCI4              NO                                           
         OC    3(2,R6),3(R6)       TEST NULLS                                   
         BZ    PROCI4              YES - IGNORE                                 
                                                                                
*===========================================================                    
* GET NETWORK AFFID DATA IF NECESSARY                                           
*===========================================================                    
                                                                                
PROCI10  CLI   PROGPROF+12,C'Y'    TEST REPORT NETWORK AFDS                     
         JNE   PROCI12             NO                                           
*                                                                               
         L     RE,ADBUY                                                         
         LLC   R0,0(RE)            GET AGYMD                                    
         N     R0,=X'0000000F'     DROP AGY                                     
         CHI   R0,3                TEST NETWORK                                 
         JNE   PROCI12                                                          
*                                                                               
         BRAS  RE,GETSPNUM         GET SPOT NUMBER IN RECORD                    
*                                                                               
         L     RE,ADBUY                                                         
         XC    AFDREC,AFDREC                                                    
         MVC   AFDEST,BUYKEST-BUYKEY(RE)                                        
         MVC   AFDBUY,BUYRLIN-BUYKEY(RE)                                        
         MVC   AFDSPNUM,ELEMNUM                                                 
         MVI   AFDPAR4,0                                                        
         SAM31                                                                  
         GOTO1 VBINSR31,AFDPAR1,AFDREC  GET AFFID DATA                          
*                                                                               
         TM    0(R1),X'80'            TEST  FOUND                               
         JNZ   PROCI10X                                                         
* MOVE AFFID DATA                                                               
         L     RE,0(R1)            GET 31-BIT ADDRESS OF DATA                   
         MVC   AFDREC,0(RE)                                                     
PROCI10X DS    0H                                                               
         SAM24                                                                  
*                                                                               
PROCI12  BAS   RE,IBLD                                                          
         JNE   PROCI20                                                          
*                                                                               
         L     R4,AIRECTAB                                                      
         BRAS  RE,OUTPUT                                                        
*                                                                               
PROCI20  J     PROCI4                                                           
*                                                                               
*=====================================================================*         
* CREATE OUTPUT RECORD                                                *         
*=====================================================================*         
                                                                                
IBLD     NTR1                                                                   
*                                                                               
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
*                                                                               
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)            POINT TO PERIOD TOTALS                       
         USING MEDDATA,R4                                                       
*                                                                               
         OC    MEDBYD(12),MEDBYD   TEST SPOTS OR DOLLARS                        
         JZ    NEQXIT              NONE - EXIT WITH CC ENQ                      
*                                                                               
         MVC   THISSPL,MEDSPILL    SET SPILL FLAG                               
         CLI   THISSPL,C'Y'                                                     
         BE    *+8                                                              
         MVI   THISSPL,C'N'                                                     
*                                                                               
         XC    BFREC(BFRECL),BFREC                                              
*                                                                               
         SR    RE,RE                                                            
         IC    RE,MEDBRAND         PRODUCT NUMBER                               
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF          POINT TO PRDBUFF ENTRY                       
         USING PTBUFFD,RE                                                       
*                                                                               
         MVC   BFKBPRD,MEDBRAND                                                 
         MVC   BFKQPRD,PTPRDA      SET 3 CHAR PRD CODE                          
         DROP  RE                                                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,MEDSPTLN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISSLN,DUB                                                      
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BUYKEST                                                       
         STC   R0,TIDBEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISEST,DUB                                                      
*                                                                               
         LLC   R0,BUYKBUY                                                       
         TM    BUYRCNTL,BUYRLN2    2-BYTE LINE NUMBERS?                         
         BZ    *+8                                                              
         ICM   R0,3,BUYRLIN                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISBUY,DUB                                                      
*                                                                               
IBLD1X   LLC   R0,ELEMNUM          SPOT NUMBER IN RECORD                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISSPT,DUB                                                      
*                                                                               
         L     R6,ADBUY                                                         
         MVC   BFKDPT,BDDAYPT                                                   
         MVC   BFKDPTNM,MEDDPART   3 CHAR DPT NAME                              
*                                                                               
         MVC   BFKTIME,BDTIMST                                                  
         MVC   BFKPROG,BDPROGRM    PROGRAM NAME                                 
*                                                                               
         LA    R1,BFKPROG                                                       
         LHI   R0,L'BFKPROG                                                     
*                                                                               
IBLD2    CLC   0(1,R1),EOLCHR      CHECK FOR SEMICOL                            
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLC   0(1,R1),EORCHR      CHECK OR COLON                               
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         AHI   R1,1                                                             
         BCT   R0,IBLD2                                                         
*                                                                               
         MVC   THISTIME,SPACES     BUYREC TIME                                  
         GOTO1 UNTIME,DMCB,BDTIMST,THISTIME                                     
*                                                                               
         LA    R0,7                                                             
         LA    R1,THISDAY                                                       
         MVC   0(7,R1),=C'MTWTFSS'                                              
         IC    RE,BDDAY                                                         
         SLL   RE,25               GET MONDAY BIT LEFT ALIGNED                  
*                                                                               
IBLD2A   LTR   RE,RE               REG IS NEG IF DAY BIT ON                     
         BM    *+8                                                              
         MVI   0(R1),C'.'                                                       
*                                                                               
         LA    R1,1(R1)                                                         
         SLL   RE,1                                                             
         BCT   R0,IBLD2A                                                        
*                                                                               
         DROP  R6                                                               
* DIVIDE DOLS BY SPOTS TO GET RATE PER SPOT                                     
         L     R1,MEDBYD                                                        
         M     R0,=F'2'                                                         
         CLC   MEDBYSPT,=F'1'      0 SPOTS IF -RATE                             
         BNH   *+8                                                              
         D     R0,MEDBYSPT                                                      
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,BFDOL                                                         
*                                                                               
         L     R1,MEDMSTAX                                                      
         M     R0,=F'2'                                                         
         CLC   MEDBYSPT,=F'1'                                                   
         BNH   *+8                                                              
         D     R0,MEDBYSPT                                                      
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         ST    R1,BFTAX                                                         
*                                                                               
         LA    R6,MEDBY1                                                        
         LA    R7,BFDEM01                                                       
         LHI   RF,14                                                            
* GET DEMO LIST FOR THIS PRD                                                    
         SR    RE,RE                                                            
         IC    RE,BFKBPRD          PRODUCT NUMBER                               
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF          POINT TO PRDBUFF ENTRY                       
         USING PTBUFFD,RE                                                       
         LA    RE,PTDEMO           POINT TO DEMO LIST FOR PRD                   
         DROP  RE                                                               
*                                                                               
IBLD4    L     R1,0(R6)            DEMO VALUE                                   
*                                                                               
         CLC   MEDBYSPT,=F'1'                                                   
         BNH   IBLD6                                                            
* NEED TO DIVIDE DEMO VALUE BY NUMBER OF SPOTS                                  
         M     R0,=F'2'            X 2                                          
         D     R0,MEDBYSPT                                                      
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
IBLD6    ST    R1,0(R7)                                                         
*                                                                               
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DECIMALS ACTIVE                       
         BZ    IBLD8                                                            
         CLI   1(RE),C'R'                                                       
         BE    *+12                                                             
         CLI   1(RE),C'E'                                                       
         BNE   IBLD8                                                            
         M     R0,=F'2'            ROUND DOWN TO 1 DECIMAL                      
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,0(R7)            AND OVERWRITE ORIG VALUE                     
*                                                                               
IBLD8    LA    R6,8(R6)                                                         
         LA    R7,4(R7)                                                         
         AHI   RE,3                                                             
         BCT   RF,IBLD4                                                         
*                                                                               
IBLD10   MVC   THISAFTM,SPACES                                                  
         MVC   THISAFCM,SPACES                                                  
         MVC   THISTRCM,SPACES                                                  
         MVC   THISAFID,SPACES                                                  
         MVC   THISTRID,SPACES                                                  
         MVC   THISTRD1,SPACES                                                  
         MVC   THISTRD2,SPACES                                                  
         MVC   THISTRD3,SPACES                                                  
         MVC   THISTRCL,SPACES                                                  
*                                                                               
         L     R6,ACURSPOT         CHECK FOR AFFID                              
         LLC   R0,1(R6)                                                         
         AR    R6,R0               POINT TO POTENTIAL AFFID                     
         CLI   0(R6),X'10'                                                      
         JE    IBLD12                                                           
*                                                                               
         OC    AFDACTDT,AFDACTDT   HAVE NETWORK AFFID DATE/TIME?                
         JNZ   IBLD14              YES                                          
* NO AFFID                                                                      
         L     R6,ACURSPOT         IF NO AFFID, USE SPOT DATE                   
         GOTO1 DATCON,DMCB,(2,2(R6)),(10,THISDATE)                              
         J     IBLD22              GO ADVANCE TO NEXT ELEM                      
*                                                                               
IBLD12   MVC   AFDACTDT,2(R6)      MOVE AFFID DATE                              
         MVC   AFDACTTM,4(R6)      MOVE AFFID TIME                              
*                                                                               
IBLD14   GOTO1 DATCON,DMCB,(2,AFDACTDT),(10,THISDATE)                           
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),AFDACTTM                                                 
         NI    FULL,X'0F'                                                       
         MVC   THISAFTM,SPACES                                                  
         GOTO1 UNTIME,DMCB,FULL,THISAFTM                                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),FLMCODEQ      TEST AFFID FILM ELEM, X'12'                  
         JE    IBLD16                                                           
         OC    AFDFILM,AFDFILM     TEST HAVE FILE ALREADY                       
         JZ    IBLD20                                                           
         MVC   MYCSEQ,AFDFILM                                                   
         J     IBLD18                                                           
*                                                                               
IBLD16   OC    3(2,R6),3(R6)       TEST NO FILM                                 
         JZ    IBLD20                                                           
*                                                                               
         MVC   MYCSEQ,3(R6)        FILM CODE FROM FLMELEM ELEMENT               
*                                                                               
IBLD18   DS    0H                                                               
         L     RF,ADBUY                                                         
         MVC   MYCBAGYMD,0(RF)     A/M FROM BUY                                 
         MVI   CMLPAR4,0                                                        
         SAM31                                                                  
         GOTO1 VBINSR31,CMLPAR1,MYCMLREC   GET ISCI CODE                        
         SAM24                                                                  
*                                                                               
         TM    0(R1),X'80'            TEST  FOUND                               
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,GETCML                                                        
         MVC   THISAFCM,MYCEBC                                                  
         MVC   THISAFID,MYCADID                                                 
*                                                                               
IBLD20   CLI   0(R6),X'18'         TEST TRAFFIC FILM ELEM                       
         BE    IBLD24                                                           
         CLI   0(R6),X'10'                                                      
         BL    IBLDX                                                            
         CLI   0(R6),X'1F'                                                      
         BH    IBLDX                                                            
*                                                                               
IBLD22   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     IBLD20                                                           
*                                                                               
IBLD24   CLI   1(R6),9                MAKE SURE NOT A DEALER TAG                
         BE    IBLDX                                                            
         OC    2(2,R6),2(R6)          IGNORE IF NO FILM                         
         BZ    IBLDX                                                            
*                                                                               
         L     RF,ADBUY                                                         
         MVC   MYCBAGYMD,0(RF)     A/M FROM BUY                                 
         MVC   MYCSEQ,2(R6)                                                     
         MVI   CMLPAR4,X'00'                                                    
         SAM31                                                                  
         GOTO1 VBINSR31,CMLPAR1,MYCMLREC                                        
         SAM24                                                                  
*                                                                               
         TM    0(R1),X'80'            TEST  FOUND                               
         BO    IBLDX                                                            
*                                                                               
         BRAS  RE,GETCML                                                        
*                                                                               
         MVC   THISTRCM,MYCEBC                                                  
         MVC   THISTRID,MYCADID                                                 
         MVC   THISTRD1,MYCDESC1                                                
         MVC   THISTRD2,MYCDESC2                                                
         MVC   THISTRD3,MYCDESC3                                                
         MVC   THISTRCL,MYCCLS                                                  
*                                                                               
IBLDX    J     EQXIT                                                            
         DROP  R3                                                               
         LTORG                                                                  
*                                                                               
*==============================================================                 
* FOR BUYLINE DEMOS, FIRST EXTRACT DEMOS, THEN REPORT SPOT DATA                 
*==============================================================                 
                                                                                
PROCJ    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYREC,R6                                                        
*                                                                               
         OC    BDCOST,BDCOST                                                    
         BNZ   PROCJ05                                                          
         TM    BDCIND,BDCMINSQ                                                  
         BZ    PROCJ05                                                          
         LA    R6,BDELEM                                                        
         DROP  R6                                                               
*                                                                               
         MVI   ELCDLO,X'71'                                                     
         MVI   ELCDHI,X'71'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   PROCJ05                                                          
         MVI   ZEROCST2,C'Y'                                                    
*                                                                               
PROCJ05  DS    0H                                                               
         XC    WORK,WORK                                                        
         L     R6,ADBUY                                                         
         MVC   WORK(13),0(R6)      MOVE BUY KEY                                 
         MVC   WORK+4(2),KEY+4     USE MARKET FROM DIRECTORY                    
         MVC   THISBMKT,KEY+4      SAVE BINARY MARKET FOR LATER                 
         GOTO1 HEXOUT,DMCB,WORK,THISIDX,13                                      
*                                                                               
         XC    WORK+BUYKBUY-BUYREC(L'BUYKBUY),WORK+BUYKBUY-BUYREC               
         CLI   WORK+BUYKSTAC-BUYREC,X'E8'                                       
         BL    *+8                                                              
         NI    WORK+BUYKSTAC+2-BUYREC,X'80'                                     
*                                                                               
         GOTO1 HEXOUT,DMCB,WORK,THISORDR,13                                     
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,4(R6)          GET ORIGINATING MKT NUMBER                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISORIG,DUB                                                     
*                                                                               
         LA    RE,220              GET DEMO LIST FOR POL                        
         STC   RE,BYTE             SET PRDCODE FOR SETDEM                       
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF          POINT TO PRDBUFF ENTRY                       
         USING PTBUFFD,RE                                                       
         LA    R4,PTDEMO           POINT TO DEMO LIST FOR PRD                   
         DROP  RE                                                               
*                                                                               
         BRAS  RE,COUNTDEM         COUNT NUMBER OF DEMOS                        
         BRAS  RE,SETDEM           AND SET MODIFIERS/CODES FOR OUTPUT           
*                                                                               
PROCJ10  L     R3,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,R3                                                      
         MVI   MEDBRAND,X'FF'                                                   
         L     RE,ADBUY                                                         
         MVC   MEDSPTLN,BDSEC-BUYREC(RE)                                        
*                                                                               
         OC    NUMDEMS,NUMDEMS                                                  
         BZ    PROCJ12                                                          
*                                                                               
         XC    SPOTHOOK,SPOTHOOK   NO HOOK EXCEPT AFFID PROCESSING              
* GET PURCHASED DEMOS                                                           
         GOTOR MEDGETBY,DMCB,SPWORKD,1                                          
         LA    R6,BFDEM01          POINT TO OUTPUT AREA                         
         BAS   RE,EXTDEM           EXTRACT DEMOS                                
* GET PURCH RERATED DEMOS                                                       
         GOTOR MEDGETBY,DMCB,SPWORKD,3                                          
         LA    R6,BFDEM01R         POINT TO OUTPUT AREA                         
         BAS   RE,EXTDEM           EXTRACT DEMOS                                
*                                                                               
*=============================================================                  
* NOW CREATE BUY DESCRIPTION (C'B') RECORD                                      
*=============================================================                  
PROCJ12  DS    0H                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'99'                                                     
         MVI   ELCDHI,X'99'                                                     
         BRAS  RE,NEXTEL                                                        
         JNE   PROCJ15                                                          
         DROP  R6                                                               
*                                                                               
         XC    THISCHPD,THISCHPD                                                
*                                                                               
         LA    R0,ACTVADD+2-ACTVELEM(R6)                                        
         GOTO1 DATCON,DMCB,(3,(R0)),(23,THISCRDT)                               
         LA    R0,ACTVCHG+2-ACTVELEM(R6)                                        
         GOTO1 DATCON,DMCB,(3,(R0)),(23,THISCHDT)                               
         LA    R1,ACTVCHG-ACTVELEM(R6)                                          
         BRAS  RE,TRNPID                                                        
         BNE   PROCJ15                                                          
         MVC   THISCHPD,WORK                                                    
*                                                                               
PROCJ15  DS    0H                                                               
         MVC   THISBYID,SPACES                                                  
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,IDELCODQ                                                  
         MVI   ELCDHI,IDELCODQ                                                  
         BRAS  RE,NEXTEL                                                        
         JNE   PROCJ17                                                          
         DROP  R6                                                               
*                                                                               
         MVC   THISBYID,IDCONNO-IDELEM(R6)                                      
*                                                                               
PROCJ17  L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BUYKEST                                                       
         STC   R0,TIDBEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISEST,DUB                                                      
*                                                                               
         LLC   R0,BUYKBUY                                                       
         TM    BUYRCNTL,BUYRLN2    2-BYTE LINE NUMBERS?                         
         BZ    *+8                                                              
         ICM   R0,3,BUYRLIN                                                     
         EDIT  (R0),THISBUY,FILL=0                                              
*                                                                               
         L     R6,ADBUY            RESTORE R6                                   
         MVC   BFKDPT,BDDAYPT                                                   
         MVC   BFKDPTNM,MEDDPART   3 CHAR DPT NAME                              
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BDSEC                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISSLN,DUB                                                      
*                                                                               
         MVC   THISTIME,SPACES     BUYREC TIME                                  
         GOTO1 UNTIME,DMCB,BDTIMST,THISTIME                                     
*                                                                               
         MVC   BFKPROG,BDPROGRM    PROGRAM NAME                                 
*                                                                               
         LA    R1,BFKPROG                                                       
         LHI   R0,L'BFKPROG                                                     
*                                                                               
PROCJ20  CLC   0(1,R1),EOLCHR      CHECK FOR SEMICOL                            
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLC   0(1,R1),EORCHR      CHECK FOR COLON                              
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
*                                                                               
         AHI   R1,1                                                             
         BCT   R0,PROCJ20                                                       
*                                                                               
         LA    R0,7                                                             
         LA    R1,THISDAY                                                       
         MVC   0(7,R1),=C'MTWTFSS'                                              
         IC    RE,BDDAY                                                         
         SLL   RE,25               GET MONDAY BIT LEFT ALIGNED                  
*                                                                               
PROCJ22  LTR   RE,RE               REG IS NEG IF DAY BIT ON                     
         BM    *+8                                                              
         MVI   0(R1),C'.'                                                       
*                                                                               
         LA    R1,1(R1)                                                         
         SLL   RE,1                                                             
         BCT   R0,PROCJ22                                                       
         DROP  R6                                                               
*                                                                               
         MVC   THISBKTP,SPACES                                                  
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,DLUCODEQ     X'24' DEMO LOOK-UP OVERRIDE ELEMENT          
         MVI   ELCDHI,DLUCODEQ                                                  
*                                                                               
PROCJ22A DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   PROCJ24                                                          
         DROP  R6                                                               
         USING DLUELEM,R6                                                       
*                                                                               
         CLI   DLUBKTYP,X'00'                                                   
         BE    PROCJ24                                                          
*                                                                               
         L     RF,VBKTYPTB         A(BOOKTYPE TABLE)                            
         USING SPBKTYPD,RF                                                      
*                                                                               
PROCJ23  CLI   0(RF),X'FF'         EOT?                                         
         BE    PROCJ24                                                          
*                                                                               
         CLC   DLUBKTYP,SPBKTYPN  IS BOOKTYPE IN TABLE?                         
         BE    *+12                                                             
         AH    RF,BKTYPTBL         NO: TRY NEXT                                 
         B     PROCJ23                                                          
*                                                                               
         MVC   THISBKTP,SPBKTYPA                                                
         DROP  RF,R6                                                            
*                                                                               
PROCJ24  DS    0H                                                               
         L     R4,ABRECTAB         FIRST THE BUY DETAILS                        
         BRAS  RE,OUTPUT                                                        
*                                                                               
         L     R4,AMRECTAB         THEN THE DEMOS                               
         BRAS  RE,OUTPUT                                                        
*                                                                               
*==================================================================             
* NOW CREATE OUTPUT FOR EACH SPOT                                               
*==================================================================             
*                                                                               
         MVI   HIGHOVER,C'N'                                                    
         BRAS  RE,HIGHDEMO                                                      
         BE    *+8                                                              
         MVI   HIGHOVER,C'Y'                                                    
*                                                                               
         CLI   ZEROCST2,C'Y'       ZERO-COST, NEG COS2 SITUATION?               
         BNE   PROCJ25                                                          
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         MVC   BDCOST,=X'000001'   YES, PUT IN FAKE $.01 COST                   
         DROP  R6                                                               
*                                                                               
PROCJ25  DS    0H                                                               
         LA    RE,BUYHOOK                                                       
         ST    RE,SPOTHOOK                                                      
         L     RE,=A(BUYHKR9)                                                   
         STM   R9,RC,0(RE)                                                      
*                                                                               
         LA    R2,PSLIST-2         USE R2 AS PSLIST POINTER                     
*                                                                               
PROCJ30  LA    R2,2(R2)                                                         
         CLI   0(R2),0                                                          
*        JE    EXIT                                                             
         BE    PROCJ90             RE-SET COST BACK TO ZERO                     
         XC    ELEMDATE,ELEMDATE                                                
*                                                                               
         ST    R2,ACURPSL                                                       
*                                                                               
         L     RE,ADCLT                                                         
         AHI   RE,CLIST-CLTHDRD                                                 
*                                                                               
PROCJ32  MVC   THISQPRD,=C'POL'    USE POL FOR UNALLOCATED                      
         CLI   0(R2),219           X'DB' = UNALLOCATED                          
         BE    PROCJ35                                                          
         CLC   3(1,RE),0(R2)       MATCH PRD                                    
         BE    PROCJ34                                                          
         AHI   RE,4                                                             
         CLI   0(RE),C' '                                                       
         BH    PROCJ32                                                          
         DC    H'0'                                                             
*                                                                               
PROCJ34  MVC   THISQPRD,0(RE)      SET EBCDIC PRODUCT                           
PROCJ35  L     R3,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,R3                                                      
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
         XC    MGBYSPOT,MGBYSPOT                                                
*                                                                               
PROCJ40  LH    R0,MGBYSPOT                                                      
         AHI   R0,1                                                             
         STH   R0,MGBYSPOT         BUMP SEARCH SPOT NUMBER                      
*                                                                               
         XC    CURSPOT,CURSPOT     CLEAR HOOK COUNT                             
         XC    ACURSPOT,ACURSPOT   AND ADDRESS OF SPOT                          
         XC    AFDREC,AFDREC                                                    
                                                                                
* GET THE DOLLARS AND AFFID DEMOS FOR THIS SPOT                                 
         MVI   MEDD0REP,C'Y'       ALWAYS REPORT SPILL SPOTS                    
         LHI   RF,7                                                             
         OC    NUMDEMS,NUMDEMS                                                  
         BNZ   *+6                                                              
         XR    RF,RF               CALL WITH 0 PARAMETER IF NO DEMOS            
*                                                                               
         CLI   HIGHOVER,C'Y'                                                    
         BNE   *+6                                                              
         XR    RF,RF               CALL WITH 0 PARAMETER IF NO DEMOS            
*                                                                               
         GOTOR MEDGETBY,DMCB,SPWORKD,(RF)                                       
         MVI   MEDD0REP,C'N'       ALWAYS REPORT SPILL SPOTS                    
*                                                                               
         MVI   THISDMER,C' '                                                    
         CLI   MEDNOBK,C'Y'                                                     
         BNE   *+8                                                              
         MVI   THISDMER,C'O'                                                    
*                                                                               
         ICM   R6,15,ACURSPOT      TEST SPOT FOUND                              
         BZ    PROCJ30             IF NOT FOUND, NEXT LIST ENTRY                
*                                                                               
         ZAP   ELEMNUM,=P'0'                                                    
         LR    R4,R6               SAVE A(CURRENT SPOT)                         
*                                                                               
* CALCULATE ELEMENT SEQUENCE NUMBER                                             
*                                                                               
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
PROCJ41  BRAS  RE,NEXTEL                                                        
         JNE   *+2                                                              
*                                                                               
         TM    RSTATUS-REGELEM(R6),X'C0'  TEST MINUS OR MINUSSED                
         BNZ   PROCJ41                    IGNORE                                
*                                                                               
         CLC   2(2,R4),2(R6)       SAME DATE?                                   
         BNE   PROCJ41             NO - DO NOT INCREMENT THE SEQ NUMBER         
         AP    ELEMNUM,=P'1'                                                    
         CR    R6,R4                                                            
         BL    PROCJ41                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    R6,R4               RESTORE A(CURRENT SPOT)                      
*                                                                               
         BRAS  RE,GETEQV                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(2,2(R6)),(23,THISDATE)                              
*                                                                               
         EDIT  ELEMNUM,THISSEQ                                                  
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)            GET CURRENT SLN                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISSLN,DUB                                                      
                                                                                
* FIND THE MEDBLOCK ENTRY FOR THIS SPOT TO GET WEEK DATE                        
                                                                                
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
*                                                                               
         L     R5,MEDAFRST                                                      
PROCJ50  CLC   2(2,R6),2(R5)       PRIOR TO WEEK END DATE                       
         BNH   PROCJ52                                                          
         AHI   R5,12               NEXT WEEK                                    
         C     R5,MEDALAST                                                      
         BNH   PROCJ50                                                          
         DC    H'0' ???                                                         
*                                                                               
PROCJ52  L     R4,8(R5)            POINT TO TOTAL ACCUMS                        
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD   TEST SPOTS OR DOLLARS                        
         BZ    PROCJ80             NONE - EXIT WITH CC ENQ                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,0(R5)),(23,THISWEEK)                              
*                                                                               
         GOTO1 (RF),(R1),(2,2(R5)),(23,THISYYMM) NO FUNNY DATES                 
         MVC   THISYYMM+7(3),=C'-01'                                            
         MVC   BFDOL,MEDBYD                                                     
         MVC   BFTAX,MEDMSTAX                                                   
         CLI   ZEROCST2,C'Y'       ZERO-COST, NEG COS2 SITUATION?               
         JNE   *+16                                                             
         XC    BFDOL,BFDOL                                                      
         XC    BFTAX,BFTAX                                                      
*                                                                               
* TEST TO EXTRACT COST 2                                                        
*&&DO                                                                           
         L     RE,ADCLT                                                         
         USING CLTHDRD,RE                                                       
         OC    CCOST2,CCOST2       TEST COST2 ACTIVE                            
         BNZ   PROCJ53B                                                         
         TM    COPT1,COP1COSQ      OR ACTIVE THIS WAY                           
         BO    PROCJ53B                                                         
         TM    COPT4,COP4TRD       OR ACTIVE THIS WAY                           
         BO    PROCJ53B                                                         
         DROP  RE                                                               
*                                                                               
* NOTE: THIS SECTION OF CODE WAS NEVER IN USE, BUT I'LL LEAVE IT                
*       HERE SINCE I THINK THIS BELONGS WITH THE ABOVE CHECKS                   
*       TO TEST WHETHER WE SHOULD EXTRACT COST2 OR NOT!                         
* ON 17NOV09 IT WAS DECIDED THAT WE WOULD ALWAYS EXTRACT COST2                  
*                                                                               
         XR    R0,R0               SEE IF THERE IS A COS2 ELEM ON BUY           
         L     RE,ADBUY                                                         
         LA    RE,24(RE)           POINT TO FIRST ELEMENT                       
*                                                                               
PROCJ53A IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BE    PROCJ54                                                          
         CLI   0(RE),X'71'                                                      
         BE    PROCJ53B                                                         
         CLI   0(RE),X'73'                                                      
         BNE   PROCJ53A                                                         
*&&                                                                             
PROCJ53B XC    BFDOL2,BFDOL2                                                    
         L     RE,MEDBUFF                                                       
         CLI   MEDSPILL-MEDBLOCK(RE),C'Y'                                       
         JE    PROCJ54                                                          
         MVC   SPOTS,=C'COS2'                                                   
         GOTO1 GETRATE,DMCB,(X'FF',SPOTS),ADBUY,(R6)  GET COST2 VALUE           
         MVC   BFDOL2,GROSS                                                     
*                                                                               
PROCJ54  MVC   THISAFDT,SPACES                                                  
         MVC   THISAFTM,SPACES                                                  
         MVC   THISAFCM,SPACES                                                  
         MVC   THISTRCM,SPACES                                                  
         MVC   THISAFID,SPACES                                                  
         MVC   THISTRID,SPACES                                                  
         MVC   THISTRD1,SPACES                                                  
         MVC   THISTRD2,SPACES                                                  
         MVC   THISTRD3,SPACES                                                  
         MVC   THISTRCL,SPACES                                                  
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'10'         TEST AFFID FOLLOWS                           
         JE    PROCJ56                                                          
                                                                                
*===========================================================                    
* NO AFFID - GET NETWORK AFFID DATA IF NECESSARY                                
*===========================================================                    
                                                                                
         CLI   PROGPROF+12,C'Y'    TEST REPORT NETWORK AFDS                     
         JNE   PROCJ66             NO                                           
*                                                                               
         L     RE,ADBUY                                                         
         LLC   R0,0(RE)            GET AGYMD                                    
         N     R0,=X'0000000F'     DROP AGY                                     
         CHI   R0,3                TEST NETWORK                                 
         JNE   PROCJ66                                                          
                                                                                
* NOTE THIS IS THE ABSOLUTE SPOT NUMBER IN THE RECORD                           
* **NOT** INCLUDING MINUS SPOTS                                                 
* **NOT** THE SPOT SEQUENCE NUMBER FOR THIS DATE                                
                                                                                
         BAS   RE,GETSPNUM         GET SPOT NUMBER IN RECORD                    
*                                                                               
         L     RE,ADBUY                                                         
         XC    AFDREC,AFDREC                                                    
         MVC   AFDEST,BUYKEST-BUYKEY(RE)                                        
         MVC   AFDBUY,BUYRLIN-BUYKEY(RE)                                        
         MVC   AFDSPNUM,ELEMNUM                                                 
         MVI   AFDPAR4,0                                                        
         SAM31                                                                  
         GOTO1 VBINSR31,AFDPAR1,AFDREC  GET AFFID DATA                          
*                                                                               
         TM    0(R1),X'80'            TEST  FOUND                               
         JNZ   PROCJ55                                                          
* MOVE AFFID DATA                                                               
         L     RE,0(R1)            GET 31-BIT ADDRESS OF DATA                   
         MVC   AFDREC,0(RE)        MOVE AFFID DATE/TIME/FILM                    
*                                                                               
PROCJ55  SAM24                                                                  
*                                                                               
         OC    AFDACTDT,AFDACTDT   HAVE NETWORK AFFID DATE/TIME?                
         JNZ   PROCJ58             YES                                          
         J     PROCJ66             SKIP IF NO AFFID                             
*                                                                               
PROCJ56  MVC   AFDACTDT,2(R6)      MOVE AFFID DATE                              
         MVC   AFDACTTM,4(R6)      MOVE AFFID TIME                              
*                                                                               
PROCJ58  GOTO1 DATCON,DMCB,(2,AFDACTDT),(23,THISAFDT)                           
         MVI   THISAFSP,C' '                                                    
*                                                                               
         ICM   R0,3,AFDACTTM                                                    
         N     R0,=X'00000FFF'                                                  
         CHI   R0,2400             SEND 00:00, NOT 24:00                        
         BL    *+8                                                              
         AHI   R0,-2400                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB                                                      
         MVC   THISAFTM(2),WORK                                                 
         MVI   THISAFTM+2,C':'                                                  
         MVC   THISAFTM+3(2),WORK+2                                             
         MVI   THISAFTM+5,C':'                                                  
         MVC   THISAFTM+6(2),=C'00'                                             
*                                                                               
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),FLMCODEQ      TEST AFFID FILM ELEM, X'12'                  
         JE    PROCJ60                                                          
         OC    AFDFILM,AFDFILM     TEST HAVE FILM ALREADY                       
         JZ    PROCJ66                                                          
         MVC   MYCSEQ,AFDFILM                                                   
         J     PROCJ62                                                          
*                                                                               
PROCJ60  OC    3(2,R6),3(R6)       TEST NO FILM                                 
         JZ    PROCJ66                                                          
*                                                                               
         MVC   MYCSEQ,3(R6)        MOVE FILM CODE                               
*                                                                               
PROCJ62  DS    0H                                                               
         L     RF,ADBUY                                                         
         MVC   MYCBAGYMD,0(RF)     A/M FROM BUY                                 
         MVI   CMLPAR4,0                                                        
         SAM31                                                                  
         GOTO1 VBINSR31,CMLPAR1,MYCMLREC   GET ISCI CODE                        
         SAM24                                                                  
                                                                                
* IF FILM NOT FOUND, JUST IGNORE IT. BLOWING UP IS TOO SEVERE MHER 7/07         
                                                                                
         TM    0(R1),X'80'         TEST  FOUND                                  
         JO    PROCJ66             IGNORE IF NOT FOUND!                         
*                                                                               
         BRAS  RE,GETCML           MOVE CMML FROM 31-BIT STORAGE                
         MVC   THISAFCM,MYCEBC                                                  
         MVC   THISAFID,MYCADID                                                 
*                                                                               
PROCJ66  CLI   0(R6),X'18'         TEST TRAFFIC FILM ELEM                       
         BE    PROCJ68                                                          
         CLI   0(R6),X'10'                                                      
         BL    PROCJ70                                                          
         CLI   0(R6),X'1F'                                                      
         BH    PROCJ70                                                          
*                                                                               
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PROCJ66                                                          
*                                                                               
PROCJ68  CLI   1(R6),9                MAKE SURE NOT A DEALER TAG                
         BE    PROCJ70                                                          
         OC    2(2,R6),2(R6)          IGNORE IF NO FILM                         
         BZ    PROCJ70                                                          
*                                                                               
         L     RF,ADBUY                                                         
         MVC   MYCBAGYMD,0(RF)     A/M FROM BUY                                 
         MVC   MYCSEQ,2(R6)                                                     
         MVI   CMLPAR4,X'00'                                                    
*        MVI   CMLPAR4,X'01'                                                    
         SAM31                                                                  
         GOTO1 VBINSR31,CMLPAR1,MYCMLREC                                        
         SAM24                                                                  
*                                                                               
         TM    0(R1),X'80'            TEST  FOUND                               
         BO    PROCJ70                                                          
*                                                                               
         BRAS  RE,GETCML                                                        
*                                                                               
         MVC   THISTRCM,MYCEBC                                                  
         MVC   THISTRID,MYCADID                                                 
         MVC   THISTRD1,MYCDESC1                                                
         MVC   THISTRD2,MYCDESC2                                                
         MVC   THISTRD3,MYCDESC3                                                
         MVC   THISTRCL,MYCCLS                                                  
*                                                                               
PROCJ70  DS    0H                                                               
         BRAS  RE,ISCANADA                                                      
         BNE   PROCJ72                                                          
*                                                                               
         ICM   R6,15,ACURSPOT      TEST SPOT FOUND                              
         BZ    PROCJ72                                                          
*                                                                               
         L     RE,MEDBUFF                                                       
         CLI   MEDSPILL-MEDBLOCK(RE),C'Y' IF SPILL, NO TAX                      
         JE    PROCJ72                                                          
*                                                                               
         GOTO1 GETRATE,DMCB,(X'FF',SPOTS),ADBUY,(C'Z',(R6)),           X        
               (C'C',XA$XCHAREA)                                                
*                                                                               
         L     RF,XA$XGSTAMT       GST                                          
         LA    RE,XA$XPSTTAB       PROVINCE TABLE                               
         LHI   R0,10               NUMBER OF PROVINCES                          
         A     RF,XA$XPSTAMT-XA$XPSTTAB(RE)  ADD PST TO TOTAL TAX               
         LA    RE,XA$XPSTLEN(RE)                                                
         BCT   R0,*-8                                                           
         ST    RF,BFTAX                                                         
*                                                                               
PROCJ72  DS    0H                                                               
         L     R4,ASRECTAB                                                      
         BRAS  RE,OUTPUT                                                        
*                                                                               
         XC    AFDEMS,AFDEMS                                                    
*                                                                               
         LR    R0,R6               SAVE R6 - EXTDEM NEEDS IT                    
         LA    R6,AFDEM01          POINT TO OUTPUT AREA                         
         BAS   RE,EXTDEM           EXTRACT DEMOS                                
         LR    R6,R0               RESTORE                                      
*                                                                               
         L     R4,ATRECTAB                                                      
         BRAS  RE,OUTPUT                                                        
*                                                                               
PROCJ80  B     PROCJ40                                                          
*                                                                               
PROCJ90  DS    0H                                                               
         CLI   ZEROCST2,C'Y'       ZERO-COST, NEG COS2 SITUATION?               
         JNE   EXIT                                                             
         MVI   ZEROCST2,C'N'                                                    
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         XC    BDCOST,BDCOST       YES, RESTORE $0.00 COST                      
         DROP  R6                                                               
         J     EXIT                                                             
         EJECT                                                                  
*==================================================================             
* GET THE ABSOLUTE SPOT NUMBER IN THE RECORD                                    
* ACURSPOT POINTS TO THE SPOT BEING PROCESSED                                   
* SPOT NUMBER IS RETURNED IN ELEMNUM                                            
*==================================================================             
                                                                                
GETSPNUM DS    0H                                                               
         L     R6,ADBUY                                                         
         LA    R6,BDELEM-BUYREC(R6)                                             
         LA    R7,1                SET CURRENT SPOT NUMBER                      
         SR    R0,R0                                                            
*                                                                               
GETSP2   ICM   R0,1,1(R6)                                                       
         JZ    *+2                                                              
         AR    R6,R0                                                            
*                                                                               
         CLI   0(R6),0                                                          
         JE    *+2                                                              
         CLI   0(R6),X'0B'                                                      
         JE    *+12                                                             
         CLI   0(R6),X'0C'                                                      
         JNE   GETSP2                                                           
*                                                                               
GETSP4   TM    6(R6),X'80'         TEST MINUS                                   
         JNZ   GETSP2                                                           
         C     R6,ACURSPOT                                                      
         JE    GETSPX                                                           
         JH    *+2                 IF R6 IS HIGH, BAD NEWS!                     
         LA    R7,1(R7)                                                         
         J     GETSP2                                                           
*                                                                               
GETSPX   STC   R7,ELEMNUM          SAVE SPOT NUMBER                             
         BR    RE                                                               
*                                                                               
*==========================================================                     
* ON ENTRY R6 POINTS TO 4-BYTE ACCUMS                                           
*==========================================================                     
                                                                                
EXTDEM   NTR1                                                                   
         XC    0(80,R6),0(R6)      CLEAR ACCUMS                                 
*                                                                               
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
*                                                                               
         LA    R5,MEDPERD          GET PERIOD TOTALS                            
         L     R4,4(R5)                                                         
         USING MEDDATA,R4                                                       
*                                                                               
         OC    MEDBYD(12),MEDBYD   TEST SPOTS OR DOLLARS                        
         JZ    EXIT                                                             
         DROP  R4                                                               
*                                                                               
         LLC   RE,MEDBRAND                                                      
         CLI   MEDBRAND,X'FF'                                                   
         BNE   *+8                                                              
         LHI   RE,220                                                           
         STC   RE,BYTE             SAVE PRD CODE FOR SETDEM                     
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF          POINT TO PRDBUFF ENTRY                       
         DROP  R3                                                               
         USING PTBUFFD,RE                                                       
         LA    R4,PTDEMO           POINT TO DEMO LIST                           
*                                                                               
         BRAS  RE,COUNTDEM                                                      
         BRAS  RE,SETDEM                                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,15,NUMDEMS                                                    
         JZ    EXIT                                                             
         DROP  RE                                                               
*                                                                               
         LR    R3,R4               USE R3 FOR DEMO LIST                         
         L     R4,4(R5)            R4 FOR MEDDATA                               
         USING MEDDATA,R4                                                       
         SR    R5,R5               CLEAR INDEX REG                              
*                                                                               
EXTDEM10 L     RF,MEDBY1(R5)       GET AVERAGE DEMOS PER SPOT                   
*                                                                               
         MVI   ISRATING,C'Y'                                                    
         CLI   1(R3),C'R'                                                       
         BE    EXTDEM20                                                         
         CLI   0(R3),C'R'          NONT RATING                                  
         BE    EXTDEM20                                                         
         CLI   1(R3),C'E'                                                       
         BE    EXTDEM20                                                         
         CLI   0(R3),C'E'          NONT RATING                                  
         BE    EXTDEM20                                                         
         MVI   ISRATING,C'N'                                                    
         B     EXTDEM24                                                         
*                                                                               
* RATING/EXT RATING HERE                                                        
EXTDEM20 TM    RQOPTS,RQOPTS_2DEC  TEST 2-DECIMALS ACTIVE                       
         BO    EXTDEM26            YES - THEN RATING VALUE IS OK                
         MHI   RF,10                                                            
         B     EXTDEM26                                                         
*                                                                               
* IMPRESSION HERE                                                               
EXTDEM24 DS    0H                                                               
         TM    RQOPTS,RQOPTS_2DECIMP TEST 2-DECIMAL IMPRESSIONS                 
         BO    EXTDEM26            YES - THEN RATING VALUE IS OK                
         MHI   RF,10               ELSE SCALE RATING AND ALL IMPS               
*                                                                               
EXTDEM26 M     RE,=F'2'                                                         
         CLC   MEDBYSPT,=F'1'                                                   
         BNH   *+8                                                              
         D     RE,MEDBYSPT                                                      
         AHI   RF,1                                                             
         SRL   RF,1                                                             
*                                                                               
         CLI   ISRATING,C'Y'                                                    
         BNE   EXTDEM28                                                         
         C     RF,=F'7000'         IF RATING, MAX VAL IS 70.00                  
         BL    EXTDEM28                                                         
         SR    RF,RF                                                            
*                                                                               
EXTDEM28 ST    RF,0(R6)                                                         
*                                                                               
         AHI   R3,3                NEXT DEMO DESCRIPTION                        
         AHI   R6,4                NEXT OUTPUT DEMO                             
         AHI   R5,8                NEXT INPUT DEMO                              
         BCT   R0,EXTDEM10                                                      
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*==============================================================                 
* SET DEMO TYPES/NUMBERS FOR OUTPUT                                             
* ON ENTRY R4 POINTS TO DEMO LIST FOR BRAND                                     
*==============================================================                 
                                                                                
SETDEM   NTR1                                                                   
*                                                                               
         LA    R0,DEMDATA          CLEAR THE AREA                               
         LHI   R1,DEMDATAX-DEMDATA                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R6,DEMMOD01                                                      
         ICM   R7,15,NUMDEMS                                                    
         BNZ   SETDEM2                                                          
         MVI   0(R6),C'Z'          SET NULL MODIFIER                            
         MVC   1(5,R6),=C'00000'                                                
         B     SETDEMX                                                          
*                                                                               
SETDEM2  CLI   0(R4),0             TEST NONT DEMO                               
         JE    SETDEM4             NO                                           
         MVC   0(1,R6),0(R4)       MOVE DEMO MODIFIER (R OR X)                  
         SR    R0,R0                                                            
         ICM   R0,3,1(R4)                                                       
         J     SETDEM6                                                          
*                                                                               
SETDEM4  MVC   0(1,R6),1(R4)       MOVE DEMO MODIFIER                           
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         MVI   0(R6),C' '                                                       
*                                                                               
         SR    R0,R0                                                            
         IC    R0,2(R4)            DEMO CATEGORY NUMBER                         
*                                                                               
SETDEM6  CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(5,R6),DUB                                                      
*                                                                               
         LA    R4,3(R4)            NEXT DEMO DESCRIPTOR                         
         LA    R6,L'DEMDATA(R6)    NEXT OUTPUT AREA                             
         JCT   R7,SETDEM2                                                       
*                                                                               
SETDEMX  J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*================================================================               
* FOR FINANCIAL REPORT, ONLY PROCESS PAID SPOTS                                 
* FOR OTHERS -                                                                  
* BUYHOOK WILL MATCH MGBYSPOT TO CURSPOT AND SET SPOTYORN TO Y                  
* FOR ONE AND ONLY ONE SPOT ON EACH MEDGETBY CALL                               
* (CODE COPIED FROM SPREPPF02)                                                  
*=============================================================                  
                                                                                
         DS    0D                                                               
BUYHOOK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LM    R9,RA,BUYHKR9                                                    
         L     RC,BUYHKRC                                                       
*                                                                               
         MVI   SPOTYORN,C'N'                                                    
*                                                                               
         L     R6,SPOTADDR                                                      
         USING REGELEM,R6                                                       
*                                                                               
         BRAS  RE,ISCANADA                                                      
         BNE   BH20                                                             
         L     RF,ACURPSL                                                       
         CLI   1(RF),X'00'                                                      
         BNE   BH20                                                             
         CLC   1(1,RF),RPALLOC+(RPTIME-RPALLOC)                                 
         BE    BH20                                                             
         CLI   RLEN,RLPOL2LQ                                                    
         BNE   BH20                                                             
         CLC   1(1,RF),RPALLOC2+(RPTIME-RPALLOC)                                
         BNE   BUYHKX                                                           
*                                                                               
BH20     DS    0H                                                               
         TM    RSTATUS,X'C0'       TEST MINUS OR MINUSSED                       
         BNZ   BUYHKX              YES- IGNORE                                  
*                                                                               
         LH    R0,CURSPOT          BUMP SPOT COUNT                              
         AHI   R0,1                                                             
         STH   R0,CURSPOT                                                       
*                                                                               
         CLC   CURSPOT,MGBYSPOT                                                 
         BNE   BUYHKX                                                           
         MVC   ACURSPOT,SPOTADDR   SAVE SPOT ADDRESS                            
         MVI   SPOTYORN,C'Y'       AND SET TO PROCESS IT                        
*                                                                               
BUYHKX   J     EXIT                                                             
         LTORG                                                                  
*                                                                               
BUYHKR9  DS    A                                                                
BUYHKRA  DS    A                                                                
BUYHKRB  DS    A                                                                
BUYHKRC  DS    A                                                                
         EJECT                                                                  
*==============================================================                 
* BUILD NETWORK AFFIDAVIT TABLE                                                 
* NOTE THIS CODE SUPPRESSES TRACING - IT PRINTS TOO MUCH                        
*==============================================================                 
*                                                                               
BLDAFD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AFDSVTRC,RCTRACE    SAVE TRACE FLAG                              
         MVI   RCTRACE,C'N'        AND TURN TRACE OFF                           
         MVC   BLDSVKEY,KEY        SAVE BUY RECORD KEY                          
*                                                                               
         L     R4,AFDPAR2                                                       
         SAM31                                                                  
         XC    0(256,R4),0(R4)     CLEAR TOP OF TABLE                           
         SAM24                                                                  
         XC    AFDPAR3,AFDPAR3     CLEAR RECORD COUNT                           
         L     R5,=A(AFDTABMAX)                                                 
*                                                                               
                                                                                
* FIND NETWORK ELEMENT IN BUY RECORD                                            
                                                                                
         L     R8,ADBUY                                                         
         OC    4(2,R8),4(R8)       TEST MARKET 0 BUY                            
         JZ    BLDAFD20                                                         
*                                                                               
         LA    R6,24(R8)                                                        
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BRAS  RE,NEXTEL                                                        
         JNE   BLDAFDX                                                          
*                                                                               
BLDAFD4  XC    KEY,KEY                                                          
         L     RE,ADCLT                                                         
         MVC   KEY(3),1(RE)        MOVE A-M/CLT                                 
         NI    KEY,X'F0'           DROP MEDIA                                   
         OI    KEY,X'03'           SET TO NETWORK                               
         MVI   KEY+3,X'FF'                                                      
* GET PACKED NETWORK                                                            
         XC    STAWORK,STAWORK                                                  
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         MVC   STAPMED,=C'N'                                                    
         MVC   STAPQMKT,=C'0000'                                                
         MVC   STAPQSTA(4),2(R6)                                                
         MVI   STAPQSTA+4,C'N'                                                  
         MVC   STAPAGY,AGY                                                      
         MVI   STAPCTRY,C'C'                                                    
         MVC   STAPACOM,ACOMFACS                                                
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         JNE   *+2                                                              
         MVC   KEY+6(2),STAPSTA    MOVE PACKED NET ONLY                         
*                                                                               
         GOTO1 HIGH                                                             
         J     BLDAFD12                                                         
*                                                                               
BLDAFD10 GOTO1 SEQ                                                              
*                                                                               
BLDAFD12 CLC   KEY(8),KEYSAVE      A-M/CLT/FF/0000/STA(2)                       
         JNE   BLDAFDX                                                          
*                                                                               
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
BLDAFD20 LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         SR    R7,R7               CLEAR ELEMNUM                                
*                                                                               
BLDAFD22 BRAS  RE,NEXTEL                                                        
         JNE   BLDAFD10                                                         
*                                                                               
         TM    6(R6),X'80'         TEST MINUS                                   
         JNZ   BLDAFD22                                                         
         LA    R7,1(R7)            BUMP SPOT NUMBER                             
*                                                                               
         LLC   RE,1(R6)                                                         
         AR    RE,R6                                                            
         CLI   0(RE),X'10'         TEST AFFID PRESENT                           
         JNE   BLDAFD22                                                         
*                                                                               
         XC    AFDREC,AFDREC                                                    
         MVC   AFDEST,BUYKEST                                                   
         MVC   AFDBUY,BUYRLIN                                                   
         STC   R7,AFDSPNUM                                                      
         MVC   AFDACTDT,2(RE)                                                   
         MVC   AFDACTTM,4(RE)                                                   
*                                                                               
         LLC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),X'12'         TEST FILM ELEM PRESENT                       
         JNE   BLDAFD30                                                         
         MVC   AFDFILM,3(RE)       SAVE FILM CODE                               
*                                                                               
BLDAFD30 DS    0H                                                               
         MVI   AFDPAR4,X'01'                                                    
         SAM31                                                                  
         MVC   0(L'AFDREC,R4),AFDREC   ADD REC TO TABLD                         
         LA    R4,L'AFDREC(R4)                                                  
         SAM24                                                                  
         BCT   R5,BLDAFD22         DECREMENT COUNT                              
         DC    H'0'                AFDTAB FULL                                  
*                                                                               
BLDAFDX  MVI   AFDREC,X'FF'             ADD A HIGH KEY                          
         MVC   AFDREC+1(L'AFDREC-1),AFDREC                                      
         SAM31                                                                  
         MVC   0(L'AFDREC,R4),AFDREC   ADD REC TO TABLD                         
         SAM24                                                                  
*                                                                               
         LA    R5,1(R5)            BUMP RECORD OCUNT                            
         L     R0,=A(AFDTABMAX)                                                 
         SR    R0,R5               GIVES NUMBER OF RECS IN TABLE                
         ST    R0,AFDPAR3                                                       
*                                                                               
         MVC   RCTRACE,AFDSVTRC                                                 
*                                                                               
         MVC   SVDMINB,DMINBTS                                                  
         MVC   SVDMOUTB,DMOUTBTS                                                
         MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,X'FD'      DO NOT TEST REC DELETED                      
*                                                                               
         MVC   KEY,BLDSVKEY                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         GOTO1 GETBUY                                                           
*                                                                               
         MVC   DMINBTS,SVDMINB                                                  
         MVC   DMOUTBTS,SVDMOUTB                                                
*                                                                               
         J     EXIT                                                             
*                                                                               
         DROP  R8                                                               
AFDSVTRC DC    X'00'                                                            
BLDSVKEY DS    XL13                                                             
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* STALAST - OUTPUT BUFFALO DATA                                                 
*==================================================================             
*                                                                               
         USING BUFFPARM,R1                                                      
*                                                                               
STAL     NTR1  BASE=*,LABEL=*                                                   
         OC    BUFFCNT,BUFFCNT                                                  
         JZ    EXIT                                                             
         XC    BFKEY,BFKEY                                                      
         GOTOR BUFFERIN,DMCB,('BUFFARDH',BUFFET),BFREC,ACOMFACS                 
         B     STAL12                                                           
                                                                                
STAL10   GOTOR BUFFERIN,DMCB,('BUFFASEQ',BUFFET),BFREC,ACOMFACS                 
*                                                                               
STAL12   CLI   BUFFERRS,BUFFEEOF                                                
         JE    EXIT                                                             
         DROP  R1                                                               
*                                                                               
STAL14   DS    0H                                                               
         MVC   THISNET,BFKNET                                                   
         CLI   BFKEY,C'C'          TEST CLEARANCE                               
         BE    STAL20                                                           
*                                                                               
         CLI   BFKEY,C'D'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,BLDDREC                                                       
*                                                                               
         MVC   BYTE,TIDQMED                                                     
         BRAS  RE,ISCANADA                                                      
         BNE   STAL15                                                           
         CLI   QMED,C'C'                                                        
         BNE   STAL15                                                           
         MVC   TIDQMED,BFKMED                                                   
*                                                                               
* PASS DXUDB A HEX STRING TO AVOID PARSING ERRORS AND                           
* LOAD TO AVOID SPECIAL CHARACTER PROBLEMS (QUOTATION MARK)                     
*                                                                               
STAL15   DS    0H                                                               
         GOTO1 HEXOUT,DMCB,THISID,THISIDX,L'THISID,0                            
         MVC   TIDQMED,BYTE                                                     
*                                                                               
         L     R4,ADRECTAB         POINT TO DEMO REC TABLE                      
         BRAS  RE,OUTPUT                                                        
*                                                                               
         B     STAL10                                                           
                                                                                
*==============================================================                 
* OUTPUT CLEARED DATA FOR FINANCIAL REQUEST                                     
*==============================================================                 
                                                                                
STAL20   GOTO1 DATCON,DMCB,(3,BFFKYM),(23,THISYYMM) NO FUNNY DATES              
         MVC   THISYYMM+7(3),=C'-01'                                            
*                                                                               
         L     R4,APRECTAB                                                      
         BRAS  RE,OUTPUT                                                        
         B     STAL10                                                           
         LTORG                                                                  
*                                                                               
*===============================================================                
* GOAL RECORD PROCESSING                                                        
*===============================================================                
                                                                                
PROCG    NTR1  BASE=*,LABEL=*                                                   
                                                                                
         GOTOR BUFFERIN,DMCB,('BUFFAINI',BUFFET),0,ACOMFACS                     
         XC    BUFFCNT,BUFFCNT     CLEAR BUFFALO COUNTER                        
*                                                                               
         L     R3,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,R3                                                      
                                                                                
*================================================================               
* NOTE MUST USE KEY TO GET ACTIVE/PASSIVE PIGGYBACKS RIGHT                      
*================================================================               
                                                                                
         LA    R6,KEY                                                           
         USING GOALRECD,R6                                                      
*                                                                               
         BRAS  RE,ISCANADA                                                      
         BNE   PROCG1                                                           
         CLI   QMED,C'C'                                                        
         BNE   PROCG1                                                           
         LA    R1,THISMED                                                       
         ICM   R1,8,GKEYAM                                                      
         BRAS  RE,HEX2MED                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PROCG1   DS    0H                                                               
         MVC   MEDBRAND,GKEYPRD    SET PRODUCT CODE                             
         MVC   MEDSPTLN,GKEYSLN    SET LEN FOR THIS PRD                         
                                                                                
* GET DEMO NAMES FOR THIS PRD                                                   
                                                                                
         L     R4,PRDBUFF                                                       
         USING PTBUFFD,R4                                                       
         SR    R0,R0                                                            
         IC    R0,MEDBRAND                                                      
         BCTR  R0,0                                                             
         MH    R0,PRDBUFLN                                                      
         AR    R4,R0               POINT TO SLOT FOR THIS PRD                   
         LA    R0,1                                                             
         CLC   =X'0021',PTDEMO     TEST USER DEMO                               
         JE    EXIT                YES - EXIT                                   
*                                                                               
         GOTO1 DEMOCON,DMCB,((R0),PTDEMO),(6,THISDM01),                X        
               (C'S',ADBLOCK),0,VNONTNMS                                        
* INITIALIZE GREC DEMO VALUES TOO                                               
         LA    RE,DEMMOD01                                                      
         LLC   R0,PTDEMO+2         DEMO CATEGORY NUMBER                         
         MVC   0(1,RE),PTDEMO+1    MOVE DEMO MODIFIER                           
*                                                                               
         CLI   PTDEMO,C' '         TEST NONT DEMO                               
         BNH   *+14                                                             
         MVC   0(1,RE),PTDEMO                                                   
         ICM   R0,3,PTDEMO+1                                                    
*                                                                               
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         MVI   0(RE),C' '                                                       
*                                                                               
         LTR   R0,R0                                                            
         BZ    PROCG40                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(5,RE),DUB                                                      
         DROP  R4                                                               
*                                                                               
         MVI   TIDGPRD2,0                                                       
         CLI   GKEYAGY,GKEYNHRQ    TEST PRD2 NOT A PRODUCT                      
         BE    *+10                                                             
         MVC   TIDGPRD2,GKEYPRD2   SET TO MAKE GOAL KEY UNIQUE                  
         DROP  R6                                                               
*                                                                               
         L     R6,ADGOAL                                                        
         USING GOALRECD,R6                                                      
         MVC   TIDGPURP,GDIDR      SET PURPOSE CODE VALUE                       
         OC    TIDGPURP,SPACES     INSURE SPACES IF NULL ????                   
*                                                                               
         SR    R0,R0                                                            
         IC    R0,GKEYEST                                                       
         STC   R0,TIDGEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISEST,DUB                                                      
*                                                                               
         GOTO1 MEDGETGL,DMCB,(RA)                                               
*                                                                               
PROCG2   XC    BFREC(BFRECL),BFREC                                              
         MVI   BFKTYPE,C'G'        SET GOAL DATA                                
         SR    RE,RE                                                            
         IC    RE,MEDBRAND         PRODUCT NUMBER                               
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF          POINT TO PRDBUFF ENTRY                       
         USING PTBUFFD,RE                                                       
*                                                                               
         CLI   PTPRDA,C' '         IF NO ESTIMATE IN PRDBUFF                    
         JNH   EXIT                SKIP IT !                                    
         MVC   BFKQPRD,PTPRDA      SET 3 CHAR PRD CODE                          
         MVC   BFKBPRD,MEDBRAND                                                 
         SR    R0,R0                                                            
         OC    PTDEMO(3),PTDEMO    TEST ANY DEMOS                               
         JZ    EXIT                                                             
         LA    R0,1                                                             
         ST    R0,NUMDEMS          SET NUMBER OF DEMOS                          
*                                                                               
         MVC   BFKSLN,MEDSPTLN                                                  
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,TIDGPRD2       GET SECOND PRD IF ANY                        
         BZ    PROCG8                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF          POINT TO PRDBUFF ENTRY                       
         MVC   BFKQPRD2,PTPRDA     SET 3 CHAR PRD CODE                          
         MVC   BFKBPRD2,TIDGPRD2                                                
         DROP  RE                                                               
*                                                                               
PROCG8   L     R1,ADDPTTAB                                                      
         LHI   R0,36                                                            
*                                                                               
PROCG10  CLC   0(1,R1),GKEYDPT                                                  
         BE    PROCG12                                                          
         LA    R1,5(R1)                                                         
         BCT   R0,PROCG10                                                       
         LA    R1,=C'XXZZZ'                                                     
*                                                                               
PROCG12  MVC   BFKDPTNM,2(R1)                                                   
         MVC   BFKDPT,GKEYDPT                                                   
         DROP  R6                                                               
*                                                                               
         LA    R5,MEDWEEKS                                                      
*                                                                               
PROCG20  ICM   R4,15,4(R5)         POINT TO DOLLARS                             
         BZ    PROCG30                                                          
         USING MEDDATA,R4                                                       
*                                                                               
         MVC   BFKWEEK,0(R5)       WEEK START DATE                              
*                                                                               
         CLI   RQLKGLS,C'Y'        PROCESSING GOAL LOCKIN DATA?                 
         BNE   PROCG21              NO                                          
         OC    MEDGLD(12),MEDGLD   TEST SPOTS OR DOLLARS                        
         BZ    PROCG30                                                          
         MVC   BFGKDOL,MEDGLD                                                   
         MVC   BFGKDOLQ,MEDGLDEQ                                                
         OC    NUMDEMS,NUMDEMS     TEST ANY DEMOS IN EST                        
         BZ    *+10                                                             
         MVC   BFGKDEM1,MEDGL1     DEMO 1                                       
         B     PROCG23                                                          
*                                                                               
PROCG21  CLI   RQLKGLS,C'M'        PROCESSING MKT LOCKIN DATA?                  
         BE    PROCG22                                                          
         OC    MEDGLD(12),MEDGLD   TEST SPOTS OR DOLLARS                        
         BZ    PROCG30                                                          
         MVC   BFDOL,MEDGLD                                                     
         MVC   BFDOLEQ,MEDGLDEQ                                                 
         OC    NUMDEMS,NUMDEMS     TEST ANY DEMOS IN EST                        
         BZ    *+10                                                             
         MVC   BFDEM01,MEDGL1      DEMO 1                                       
         B     PROCG23                                                          
*                                                                               
PROCG22  OC    MEDLKD(16),MEDLKD   TEST LOCKIN SPOTS/DOLLARS                    
         BZ    PROCG30                                                          
         MVC   BFLKSPT,MEDLKSPT    LOCKIN SPOTS                                 
         MVC   BFLKDOL,MEDLKD      LOCKIN DOLLARS                               
         MVC   BFLKDOLQ,MEDLKDEQ   LOCKIN EQUIV DOLLARS                         
         OC    NUMDEMS,NUMDEMS     TEST ANY DEMOS IN EST                        
         BZ    *+10                                                             
         MVC   BFLKDEM1,MEDLK1     LOCKIN DEMO 1                                
*                                                                               
PROCG23  TM    RQOPTS,RQOPTS_2DEC  TEST 2-DECIMALS ACTIVE                       
         BZ    PROCG28                                                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,MEDBRAND         PRODUCT NUMBER                               
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF          POINT TO PRDBUFF ENTRY                       
         USING PTBUFFD,RE                                                       
         CLI   PTDEMO+1,C'R'                                                    
         BE    *+12                                                             
         CLI   PTDEMO+1,C'E'                                                    
         BNE   PROCG28                                                          
*                                                                               
         LA    RF,BFGKDEM1                                                      
         CLI   RQLKGLS,C'Y'                                                     
         BE    PROCG24                                                          
         LA    RF,BFLKDEM1                                                      
         CLI   RQLKGLS,C'M'                                                     
         BE    PROCG24                                                          
         LA    RF,BFDEM01                                                       
*                                                                               
PROCG24  L     R1,0(RF)                                                         
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,0(RF)                                                         
*                                                                               
PROCG28  GOTOR PUTBUF                                                           
*                                                                               
PROCG30  AHI   R5,L'MEDDATES       NEXT WEEK                                    
*                                                                               
         C     R5,MEDALAST                                                      
         BH    PROCG32                                                          
*                                                                               
         LA    R0,MEDMON01                                                      
         CR    R5,R0                                                            
         BL    PROCG20                                                          
*                                                                               
PROCG32  DS    0H                                                               
         CLI   RQLKGLS,C'Y'        PROCESSED LOCKED GOALS ALREADY?              
         BE    PROCG40              YES                                         
*                                                                               
         CLI   RQLKGLS,C'M'        PROCESSED MKT LOCKIN $ YET?                  
         BE    PROCG34              YES                                         
         MVI   RQLKGLS,C'M'                                                     
         GOTO1 MEDGETLK,DMCB,(RA)                                               
         B     PROCG2                                                           
*                                                                               
PROCG34  MVI   RQLKGLS,C'Y'         NO - GO GET THEM                            
         GOTO1 MEDGETGL,DMCB,(RA)                                               
         B     PROCG2                                                           
*                                                                               
*============================================================                   
* CREATE OUTPUT RECORD FOR GOAL DATA                                            
*============================================================                   
*                                                                               
PROCG40  MVI   RQLKGLS,C'N'        RE-SET PROCESS LOCKED GOALS                  
         OC    BUFFCNT,BUFFCNT                                                  
         JZ    EXIT                                                             
*                                                                               
         USING BUFFPARM,R1                                                      
         XC    BFKEY,BFKEY                                                      
         GOTOR BUFFERIN,DMCB,('BUFFARDH',BUFFET),BFREC,ACOMFACS                 
         B     PROCG52                                                          
                                                                                
PROCG50  GOTOR BUFFERIN,DMCB,('BUFFASEQ',BUFFET),BFREC,ACOMFACS                 
*                                                                               
PROCG52  CLI   BUFFERRS,BUFFEEOF                                                
         JE    EXIT                                                             
         DROP  R1                                                               
*                                                                               
PROCG54  SR    R0,R0                                                            
         IC    R0,BFKSLN                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISSLN,DUB                                                      
*                                                                               
         MVC   TIDGPRD,BFKBPRD     COMPLETE HEX KEY                             
         MVC   TIDGEST,BEST                                                     
         MVC   TIDGMKT,BMKT                                                     
         MVC   TIDGDPT,BFKDPT                                                   
         MVC   TIDGSLN,BFKSLN                                                   
*                                                                               
         BRAS  RE,GETEQV           GET EQUIV FACTOR IN THISEQV                  
*                                                                               
         GOTO1 DATCON,DMCB,(2,BFKWEEK),(X'20',DUB)                              
         GOTO1 (RF),(R1),,(23,THISWEEK)                                         
*                                                                               
         GOTO1 (RF),(R1),,DUB          GET X'FA' DATE FOR BRDMON                
         GOTO1 GETBROAD,DMCB,DUB,WORK           GET BRDMON DATES                
         GOTO1 DATCON,DMCB,WORK+6,(2,WORK+12)   GET 2-BYTE END DATE             
         GOTO1 (RF),(R1),(2,WORK+12),(23,THISYYMM) NO FUNNY DATES               
         MVC   THISYYMM+7(3),=C'-01'                                            
*                                                                               
* PASS DXUDB A HEX STRING TO AVOID PARSING ERRORS AND LOAD                      
* TO AVOID SPECIAL CHARACTER PROBLEMS (QUOTATION MARK)                          
*                                                                               
         GOTO1 HEXOUT,DMCB,THISIDG,THISIDGX,L'THISIDG,0                         
*                                                                               
PROCG55  L     R4,AGRECTAB                                                      
         CLI   QOPT2,C'I'          TEST ISCI OPTION                             
         BNE   *+8                                                              
         L     R4,ALRECTAB                                                      
*                                                                               
         BRAS  RE,OUTPUT                                                        
         B     PROCG50                                                          
         DROP  R3                                                               
         LTORG                                                                  
*                                                                               
*=====================================================================*         
* CREATE ACT FILE RECORD                                                        
* ON ENTRY R4 POINTS TO RECORD BUILD TABLE                                      
*=====================================================================*         
*                                                                               
BLDDREC  NTR1  BASE=*,LABEL=*                                                   
         SR    R0,R0                                                            
         IC    R0,BFKSLN                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISSLN,DUB                                                      
*                                                                               
         MVC   TIDBPRD,BFKBPRD                                                  
         MVC   TIDBSLN,BFKSLN      SET BINARY SLN                               
         MVC   TIDBDPT,BFKDPT      1 CHAR DPT                                   
*                                                                               
         BRAS  RE,GETEQV           GET EQUIV FACTOR IN THISEQV                  
*                                                                               
         GOTO1 DATCON,DMCB,(2,BFKWEEK),(X'20',DUB)                              
         GOTO1 (RF),(R1),,(23,THISWEEK)                                         
*                                                                               
         GOTO1 (RF),(R1),,DUB          GET X'FA' DATE FOR BRDMON                
         GOTO1 GETBROAD,DMCB,DUB,WORK           GET BRDMON DATES                
*                                                                               
         GOTO1 DATCON,DMCB,WORK+6,(2,WORK+12)   GET 2-BYTE END DATE             
         GOTO1 (RF),(R1),(2,WORK+12),(23,THISYYMM)                              
         MVC   THISYYMM+7(3),=C'-01'                                            
*                                                                               
         MVC   THISTIME,SPACES                                                  
         GOTO1 UNTIME,DMCB,BFKTIME,THISTIME                                     
*                                                                               
         LA    R0,7                                                             
         LA    R1,THISDAY                                                       
         MVC   0(7,R1),=C'MTWTFSS'                                              
         IC    RE,BFKDAY                                                        
         SLL   RE,25               GET MONDAY BIT LEFT ALIGNED                  
*                                                                               
BLDDR2   LTR   RE,RE               REG IS NEG IF DAY BIT ON                     
         BM    *+8                                                              
         MVI   0(R1),C'.'                                                       
*                                                                               
         LA    R1,1(R1)                                                         
         SLL   RE,1                                                             
         BCT   R0,BLDDR2                                                        
* GET DEMO LIST FOR THIS PRD                                                    
         SR    RE,RE                                                            
         IC    RE,BFKBPRD          PRODUCT NUMBER                               
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         A     RE,PRDBUFF          POINT TO PRDBUFF ENTRY                       
         USING PTBUFFD,RE                                                       
         LA    R4,PTDEMO           POINT TO DEMO LIST FOR PRD                   
         DROP  RE                                                               
*                                                                               
         BRAS  RE,COUNTDEM         COUNT NUMBER OF DEMOS                        
*                                                                               
         LA    R0,DEMDATA          CLEAR THE AREA, STUPIDO !                    
         LHI   R1,DEMDATAX-DEMDATA                                              
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R5,BFDEM01                                                       
         LA    R6,DEMMOD01                                                      
         ICM   R7,15,NUMDEMS                                                    
         BNZ   BLDDR10                                                          
         MVI   0(R6),C'Z'          SET NULL MODIFIER                            
         MVC   1(5,R6),=C'00000'                                                
         XC    BFDEM01,BFDEM01     SET ZERO VALUES                              
         B     BLDDRECX                                                         
*                                                                               
BLDDR10  MVC   0(1,R6),1(R4)       MOVE DEMO MODIFIER                           
         LLC   R0,2(R4)            DEMO CATEGORY NUMBER                         
*                                                                               
         CLI   0(R4),C' '          TEST NONT DEMO                               
         JNH   *+14                                                             
         MVC   0(1,R6),0(R4)                                                    
         ICM   R0,3,1(R4)                                                       
*                                                                               
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         MVI   0(R6),C'Z'                                                       
*                                                                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(5,R6),DUB                                                      
*                                                                               
         L     R1,0(R5)            GET DEMO VALUE                               
*                                                                               
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DECIMALS ACTIVE                       
         BZ    BLDDR20                                                          
*                                                                               
BLDDR12  CLI   1(R4),C'R'                                                       
         BE    BLDDR14                                                          
         CLI   1(R4),C'E'                                                       
         BE    BLDDR14                                                          
         CLI   0(R4),C'R'          NONT RATING                                  
         BE    BLDDR14                                                          
         CLI   0(R4),C'E'                                                       
         BNE   BLDDR20                                                          
*                                                                               
BLDDR14  M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,0(R5)            AND OVERWRITE ORIG VALUE                     
*                                                                               
BLDDR20  CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  6(8,R6),DUB                                                      
*                                                                               
         LA    R4,3(R4)            NEXT DEMO DESCRIPTOR                         
         LA    R5,4(R5)            NEXT RAW DEMO                                
         LA    R6,L'DEMDATA(R6)    NEXT OUTPUT AREA                             
         BCT   R7,BLDDR10                                                       
*                                                                               
BLDDRECX J     EXIT                                                             
*                                                                               
*=============================================================*                 
* COUNT NUMBER OF DEMO ENTRIES FOR THIS PRODUCT                                 
*=============================================================*                 
*                                                                               
COUNTDEM NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LHI   R0,20                                                            
         LR    R1,R4                                                            
*                                                                               
COUNTDM2 OC    0(3,R1),0(R1)                                                    
         BZ    COUNTDM4                                                         
         AHI   R1,3                                                             
         BCT   R0,COUNTDM2                                                      
*                                                                               
COUNTDM4 LHI   R1,20                                                            
         SR    R1,R0                                                            
         ST    R1,NUMDEMS                                                       
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=============================================================*                 
* SET DEMO VALUES FROM ESTHDR IN PTBUFF                                         
* SET X'80' IN PTDEMO IF NON-T DEMO IS A RATING                                 
*=============================================================*                 
*                                                                               
SETEST   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,PRDBUFF                                                       
         USING PTBUFFD,RE                                                       
*                                                                               
         L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,EPRDCD                                                      
         CHI   R0,255                                                           
         BNE   *+8                                                              
         LHI   R0,220              POL = 220                                    
         BCTR  R0,0                                                             
         MH    R0,PRDBUFLN                                                      
         AR    RE,R0               POINT TO PTBUFF SLOT FOR THIS PRD            
         ST    RE,DUB              SAVE ADDRESS                                 
         MVC   PTPRDN,EPRDCD+1                                                  
         MVC   PTPRDA,EKEY+4       ALPHA PRD CODE                               
         MVC   PTDEMO,EDEMLST                                                   
         MVC   PTWGHT,EWGTLST                                                   
                                                                                
*============================================================                   
* MODIFY THE NONT DEMO CODES IN PTBUFF                                          
*============================================================                   
                                                                                
         CLI   QMED,C'T'           NONT DEMOS ONLY FOR TV                       
         JNE   SETESTX                                                          
         CLC   ELEN,=Y(ESTHDRLN)   ANY NONT DEMOS IN RECORD                     
         JNH   SETESTX                                                          
*                                                                               
         LA    R4,PTDEMO                                                        
         LA    R0,20                                                            
*                                                                               
SETEST2  OC    0(3,R4),0(R4)       TEST MORE DEMOS                              
         JZ    SETEST10                                                         
*                                                                               
         CLI   2(R4),0             TEST NONT DEMO                               
         JNE   SETEST6             NO - SKIP                                    
*                                                                               
         LLC   R5,1(R4)            GET SEQNUM                                   
         BCTR  R5,0                                                             
         MHI   R5,L'ENONTDMS                                                    
         LA    R5,ENONTDMS(R5)     POINT TO NONT DEMO NAME                      
         MVC   0(1,R4),0(R5)       MOVE DEMO MODIFIER (R OR X FOR NOW)          
*                                                                               
         BAS   RE,GETNONT          AND FILL IN SEQNUM                           
         JE    SETEST4                                                          
* REMOVE THIS DEMO FROM LIST                                                    
         BAS   RE,SETKILL                                                       
         J     SETEST8                                                          
*                                                                               
SETEST4  MVC   1(2,R4),HALF                                                     
*                                                                               
SETEST6  LA    R4,3(R4)                                                         
*                                                                               
SETEST8  JCT   R0,SETEST2                                                       
                                                                                
* IF ALL NOT DEMOS INVALID, AND NO DEMOS LEFT, SET RATING HOMES                 
                                                                                
         OC    PTDEMO(3),PTDEMO    TEST NO DEMOS                                
         JNZ   SETEST10                                                         
         MVC   PTDEMO(3),=X'00D901'                                             
         J     SETESTX                                                          
                                                                                
*==================================================================             
* SAVE THE POL NONT DEMO NAMES AND SEQNUMS TO XLATE BUY DEMO CODES              
*==================================================================             
                                                                                
SETEST10 CLI   EPRDCD+1,X'FF'      TEST POL                                     
         JNE   SETESTX                                                          
*                                                                               
         LARL  R4,POLNTDEM         THIS CODE FOR POL DEMOS                      
         LA    R5,ENONTDMS                                                      
         LA    R0,20                                                            
*                                                                               
* BEWARE!!! THERE CAN BE MISSING DEMO NAMES IN THE LIST !!!!                    
*                                                                               
SETEST12 CLI   0(R5),C' '          TEST THERE IS A NAME                         
         JNH   SETEST14                                                         
         MVC   0(7,R4),0(R5)       MOVE DEMO TEXT                               
*                                                                               
         BAS   RE,GETNONT          AND FILL IN SEQNUM                           
         JNE   SETEST14            IF NOT FOUND, JUST SKIP IT                   
*                                                                               
         MVC   7(2,R4),HALF                                                     
         LA    R4,L'POLNTDEM(R4)                                                
*                                                                               
SETEST14 LA    R5,L'ENONTDMS(R5)                                                
         JCT   R0,SETEST12                                                      
*                                                                               
SETESTX  XIT1                                                                   
         DROP  R6,RE                                                            
*                                                                               
* MOVE DEMOS FROM 3(R4) LEFT 3 BYTES TO OVERWRITE INVALID DEMO                  
*                                                                               
SETKILL  NTR1                                                                   
         CHI   R0,1                TEST THIS IS DEMO 20                         
         JNE   SETKILL2                                                         
         XC    0(3,R4),0(R4)                                                    
         J     EXIT                                                             
*                                                                               
SETKILL2 MVC   0(3,R4),3(R4)       MOVE LEFT 1 SLOT                             
         LA    R4,3(R4)                                                         
         JCT   R0,SETKILL2                                                      
         J     EXIT                                                             
                                                                                
*==========================================================                     
* LOOK UP THE DEMO SEQNUM FOR NONT DEMO AT 0(R5)                                
* DEMOS ARE SUPPOSED TO LOOK LIKE RX.... OR X ....                              
* SO JUST MATCH ON CHARACTERS FOLLOWING THE X                                   
* AND RETURN IT IN HALF                                                         
*==========================================================                     
                                                                                
GETNONT  NTR1                                                                   
         SAM31                                                                  
*                                                                               
         L     RF,D29COUNT                                                      
         L     R1,D29ATAB                                                       
         USING D29RECD,R1                                                       
*                                                                               
         CLI   0(R5),C'R'                                                       
         JNE   *+8                                                              
         LA    R5,1(R5)                                                         
         CLI   0(R5),C'X'                                                       
         JNE   GETNONT4                                                         
         LA    R5,1(R5)                                                         
*                                                                               
GETNONT2 CLC   D29DEMNM,0(R5)                                                   
         JE    GETNONT6                                                         
         AHI   R1,D29RECL                                                       
         JCT   RF,GETNONT2                                                      
GETNONT4 SAM24                                                                  
         J     NEQXIT                                                           
*                                                                               
GETNONT6 MVC   HALF,D29DEMSQ                                                    
         SAM24                                                                  
         J     EQXIT                                                            
         DROP  R1                                                               
                                                                                
*=============================================================*                 
* CHANGE BUY RECORD DEMO ELEMENT TO SET SEQNUMS FOR NONT DEMOS                  
* DEMO WILL BE MOD(1)/SEQ(2)                                                    
*                                                                               
* FIND THE NAME OF THE RENTRAK DEMO IN THE BUYREC NTDEMEL                       
* THEN MATCH TO THE NAMES IN THE POL DEMO LIST                                  
* IF NOT THERE, SET MODIFIER TO Z AND USE RENTRAK NUMBER                        
*=============================================================*                 
*                                                                               
SETNONT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
*                                                                               
         MVI   ELCDLO,X'50'                                                     
         MVI   ELCDHI,X'50'                                                     
         BRAS  RE,NEXTEL                                                        
         JNE   EXIT                                                             
         LA    R6,2(R6)            POINT TO FIRST NAME                          
         ST    R6,ANAMEL           AND SAVE THE ADDRESS                         
*                                                                               
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCDLO,X'02'                                                     
         MVI   ELCDHI,X'03'                                                     
         J     *+8                                                              
*                                                                               
SETNONT2 L     R6,ADEMEL           GET A DEMO ELEMENT                           
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   EXIT                                                             
         ST    R6,ADEMEL                                                        
*                                                                               
         LLC   R0,1(R6)                                                         
         AHI   R0,-24                                                           
         JNP   EXIT                                                             
*                                                                               
         SRL   R0,3                SET FOR NUMBER OF DEMOS                      
         LA    R6,24(R6)           POINT TO FIRST DEMO                          
*                                                                               
SETNONT4 CLI   2(R6),0             TEST NONT DEMO                               
         JNE   SETNONT10           NO                                           
*                                                                               
         LLC   R1,1(R6)            GET NONT INDEX                               
         BCTR  R1,0                                                             
         MHI   R1,9                                                             
         A     R1,ANAMEL           POINT TO NAME                                
*                                                                               
         LARL  RE,POLNTDEM         POINT TO POL DEMO LIST                       
         LA    RF,20                                                            
*                                                                               
SETNONT6 CLC   0(7,R1),0(RE)       MATCH NAME                                   
         JE    SETNONT9                                                         
         LA    RE,9(RE)                                                         
         JCT   RF,SETNONT6                                                      
*                                                                               
SETNONT8 MVI   0(R6),C'Z'          SET NONT NOT IN EST                          
         MVC   2(1,R6),1(R6)       MOVE SEQNUM                                  
         MVI   1(R6),0                                                          
         J     SETNONT10                                                        
*                                                                               
SETNONT9 MVC   0(1,R6),0(RE)       SET DEMO MODIFIER                            
         MVC   1(2,R6),7(RE)       AND SEQNUM                                   
*                                                                               
SETNONT10 LA    R6,8(R6)            POINT TO NEXT DEMO                          
          JCT   R0,SETNONT4                                                     
          J     SETNONT2                                                        
*                                                                               
ANAMEL   DS    A                                                                
ADEMEL   DS    A                                                                
*=============================================================*                 
* EXTRACT EQUIVALENCE FACTOR FROM EQUREC                                        
*=============================================================*                 
*                                                                               
GETEQV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    RE,RE                                                            
         IC    RE,BFKSLN                                                        
         AR    RE,RE               X 2                                          
         A     RE,MYSLNTAB         POINT TO TABLE ENTRY                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,0(RE)            DSPL TO EQUIV TABLE ENTRY                    
         LA    RF,EQTAB(RF)                                                     
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,0(RF)          GET FACTOR                                   
         BNZ   *+8                                                              
         LHI   R0,1000                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  THISEQV,DUB                                                      
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*=============================================================*                 
* OUTPUT  DATA TO PRTQUE REPORT                                                 
*=============================================================*                 
*                                                                               
OUTPUT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   THISNET,SPACES                                                   
         BH    *+10                                                             
         MVC   THISNET,=C'N/A '                                                 
*                                                                               
         AP    RUNRECS,=P'1'                                                    
*                                                                               
OUTPUT4  L     RE,0(R4)            GET DATA ADDR                                
         SR    RF,RF                                                            
         IC    RF,4(R4)            GET DATA LEN                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   D.DLCBFLD(0),0(RE)                                               
*                                                                               
         CLI   5(R4),C'T'          TEST TEXT                                    
         BNE   OUTPUT6                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    D.DLCBFLD(0),SPACES                                              
         CLI   QOPT1,C'Y'          TEST FIXED LENGTH OUTPUT                     
         BNE   *+10                                                             
         MVC   D.DLCBLEN,4(R4)     FIX OUTPUT LENGTH                            
         B     OUTPUT18                                                         
*                                                                               
OUTPUT6  CLI   5(R4),C'B'          TEST BINARY                                  
         BNE   OUTPUT14                                                         
*                                                                               
         CLI   QOPT1,C'Y'          TEST FIXED LENGTH OUTPUT                     
         BE    OUTPUT8                                                          
         MVC   D.DLCBNDP,7(R4)     SET NUMBER OF DECIMAL PLACES                 
         MVC   D.DLCBLEN,4(R4)     SET DATA LENGTH                              
         B     OUTPUT20                                                         
*                                                                               
* FIXED LEN NUMERIC OUTPUT                                                      
*                                                                               
OUTPUT8  ICM   R0,15,0(RE)         GET VALUE IN R0                              
         MVI   D.DLCBTYP,C'N'      TYPE=NUMERIC                                 
*                                                                               
OUTPUT8A CLI   7(R4),1             TEST 1 DECIMAL                               
         BNE   OUTPUT10                                                         
         MVC   WORK(11),=X'4021202020202020204B20'                              
         CVD   R0,DUB                                                           
         ED    WORK(11),DUB+3                                                   
         MVC   D.DLCBFLD(8),WORK+3                                              
         MVI   D.DLCBLEN,8         FIX OUTPUT LEN                               
         B     OUTPUT30                                                         
*                                                                               
OUTPUT10 CLI   7(R4),2             TEST 2 DECIMAL                               
         BNE   OUTPUT12                                                         
*                                                                               
         MVC   WORK(17),=X'40212020202020202020202020204B2020'                  
         LA    R1,DUB                                                           
         LTR   R0,R0                                                            
         BNM   OUTPUT11                                                         
         MVC   WORK(17),=X'404021202020202020202020204B202060'                  
         LA    R1,DUB+1                                                         
*                                                                               
OUTPUT11 CVD   R0,DUB                                                           
         ED    WORK(17),0(R1)                                                   
         MVC   D.DLCBFLD(13),WORK+4                                             
         MVI   D.DLCBLEN,13                                                     
         B     OUTPUT30                                                         
*                                                                               
OUTPUT12 CVD   R0,DUB                                                           
         UNPK  D.DLCBFLD(13),DUB                                                
         OI    D.DLCBFLD+12,X'F0'                                               
         MVI   D.DLCBLEN,13                                                     
         LTR   R0,R0                                                            
         BNM   *+8                                                              
         MVI   D.DLCBFLD,C'-'                                                   
         B     OUTPUT30                                                         
*                                                                               
OUTPUT14 TM    7(R4),X'01'         TEST CVD REQUIRED                            
         BZ    OUTPUT16                                                         
         ICM   R0,15,0(RE)         GET DATA VALUE                               
         CVD   R0,DUB                                                           
         LTR   R0,R0                                                            
         BM    *+8                                                              
         OI    DUB+7,X'0F'                                                      
         SLL   RF,4                SET LEN TO UNPK TO                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  D.DLCBFLD(0),DUB                                                 
         B     OUTPUT20                                                         
*                                                                               
OUTPUT16 CLI   5(R4),C'X'          TEST HEX                                     
         BNE   OUTPUT18                                                         
         CLI   QOPT1,C'Y'          TEST FIXED LENGTH OUTPUT                     
         BNE   *+10                                                             
         MVC   D.DLCBLEN,4(R4)     FIX OUTPUT LENGTH                            
*                                                                               
OUTPUT18 CLI   6(R4),0             TEST FIELD CAN END RECORD                    
         BE    OUTPUT20            NO                                           
         CLC   D.DLCBFLD(1),6(R4)  ELSE COMPARE                                 
         BNH   OUTPUT32            AND POSSIBLY END                             
*                                                                               
OUTPUT20 MVC   D.DLCBTYP(1),5(R4)                                               
         CLI   5(R4),C'X'          TEST HEX OUTPUT                              
         BNE   *+8                                                              
         MVI   D.DLCBTYP,C'T'      TELL DLFLD IT'S TEXT                         
*                                                                               
OUTPUT30 MVI   D.DLCBACT,DLCBPUT                                                
*                                                                               
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),DLCB                                                        
         MVI   D.DLCXDELC,C' '     ALWAYS RESTORE TERMINATOR                    
*                                                                               
         LA    R4,L'RECTAB(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   OUTPUT4                                                          
*                                                                               
OUTPUT32 MVI   D.DLCBACT,DLCBEOL                                                
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),DLCB                                                        
*                                                                               
         MVI   D.DLCBACT,DLCBEOL                                                
         L     RF,=V(DLFLD)                                                     
         GOTO1 (RF),DLCB                                                        
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*==============================================================*                
* USER PRINT ROUTINE EXIT CALLED BY DLFLD                      *                
* ALL DATA PRINTED HERE GOES ON PAGE 2                         *                
*==============================================================*                
BLPRINT  NTR1  BASE=*,LABEL=*                                                   
         CLI   QOPT5,C'X'          TEST SUPPRESS OUTPUT                         
         BNE   BLPRINT2                                                         
         LHI   R0,14                                                            
         LA    R1,P                                                             
         MVC   0(132,R1),SPACES                                                 
         AHI   R1,132                                                           
         BCT   R0,*-10                                                          
         J     EXIT                                                             
*                                                                               
BLPRINT2 MVI   LINE,0              FORCE NO PAGE BREAK                          
         GOTO1 REPORT                                                           
         MVI   FORCEHED,C'N'                                                    
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*=====================================================================*         
* PUT RECORD TO BUFFERIN AND DO TRACE IF NECESSARY                    *         
*=====================================================================*         
                                                                                
PUTBUF   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         OC    BFDATA(BFDATAL),BFDATA                                           
         JZ    EXIT                                                             
*                                                                               
         L     R0,BUFFCNT                                                       
         AHI   R0,1                                                             
         ST    R0,BUFFCNT                                                       
*                                                                               
         CLI   QOPT5,C'Y'         TEST TRACE OPTION                             
         BNE   PUTBUF02                                                         
         GOTOR PRNTBL,DMCB,=C'PUTBUF',BFREC,C'DUMP',BFRECL,=C'1D00'             
                                                                                
PUTBUF02 GOTOR BUFFERIN,DMCB,('BUFFAPUT',BUFFET),BFREC,ACOMFACS                 
                                                                                
PUTBUFX  J     EXIT                                                             
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
MYHDHK   NTR1  BASE=*,LABEL=*                                                   
         LM    R9,RA,HDHKR9                                                     
         USING SPWORKD,RA,R9                                                    
         L     RC,HDHKRC                                                        
         XIT1                                                                   
*                                                                               
HDHKR9   DS    A                                                                
HDHKRA   DS    A                                                                
HDHKRB   DS    A                                                                
HDHKRC   DS    A                                                                
         LTORG                                                                  
*                                                                               
*=================================================================              
* SET AUTO REQUEST DATES IF REQUIRED                                            
*=================================================================              
                                                                                
SETDATES NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    AUTOSTRT,AUTOSTRT                                                
         XC    AUTOEND,AUTOEND                                                  
*                                                                               
* SET A MAX EST END DATE OF 5 YEARS AGO                                         
         GOTO1 ADDAY,DMCB,(C'Y',TODAY),MAXEND,-5                                
*                                                                               
         MVC   WORK(12),=CL12'S0DB'                                             
         MVC   WORK+4(3),SVAGY                                                  
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,PROGPROF,DATAMGR                               
*                                                                               
         CLI   PROGPROF+8,0        TEST FEATURE ENABLED                         
         JE    EXIT                NO                                           
         CLC   =C'NOAUTO',QSTAUTO  TEST USE REQUEST DATES                       
         BNE   SETDAT2                                                          
         MVC   QSTAUTO(6),SPACES                                                
         J     EXIT                                                             
*                                                                               
SETDAT2  SR    R0,R0                                                            
         IC    R0,PROGPROF+8                                                    
         MHI   R0,3                                                             
         AHI   R0,2                GO BACK 2 EXTRA MONTHS                       
         LNR   R0,R0                                                            
*                                                                               
         MVC   WORK(6),TODAY                                                    
         CLC   QCODE,=C'DB'        LOAD MODE?                                   
         BNE   SETDAT3                                                          
*                                                                               
         GOTO1 GETDAY,DMCB,(0,WORK),(0,WORK+6)                                  
         CLI   DMCB,6              IS THIS A SATURDAY?                          
         BNE   SETDAT3              -NO                                         
*                                   -YES, ADD A DAY TO MAKE IT SUNDAY           
         GOTO1 ADDAY,DMCB,(C'D',WORK),WORK+6,1                                  
         MVC   WORK(6),WORK+6                                                   
*                                                                               
SETDAT3  DS    0H                                                               
         MVC   WORK+4(2),=C'15'                                                 
*                                                                               
SETDAT4  GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,(R0)                               
*                                                                               
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK+2(2),=C'01'   TEST FOR A MONTH THAT STARTS A QTR            
         BE    SETDAT6                                                          
         CLC   WORK+2(2),=C'04'                                                 
         BE    SETDAT6                                                          
         CLC   WORK+2(2),=C'07'                                                 
         BE    SETDAT6                                                          
         CLC   WORK+2(2),=C'10'                                                 
         BE    SETDAT6                                                          
         LHI   R0,1                SET TO ADVANCE A MONTH                       
         B     SETDAT4                                                          
                                                                                
* GET BROADCAST MONTH DATES FROM MOBILE                                         
                                                                                
SETDAT6  MVC   WORK+6(6),WORK      SET END DATE=TO DATE                         
         GOTO1 MOBILE,DMCB,(2,WORK),(1,WORK+16)                                 
*                                                                               
         GOTO1 DATCON,DMCB,(2,WORK+16),AUTOSTRT                                 
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,PROGPROF+9     GET QUARTERS FORWARD                         
         BP    *+8                                                              
         LHI   R0,2                DEFAULT TO 2 QUARTERS FORWARD                
         BCTR  R0,0                ARITHMETIC                                   
         MHI   R0,3                X 3                                          
         AHI   R0,2                AND 2 MORE MONTH                             
         MVC   WORK(6),TODAY                                                    
         MVC   WORK+4(2),=C'15'                                                 
*                                                                               
SETDAT8  GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,(R0)                               
*                                                                               
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK+2(2),=C'03'   TEST FOR A MONTH THAT ENDS A QTR              
         BE    SETDAT10                                                         
         CLC   WORK+2(2),=C'06'                                                 
         BE    SETDAT10                                                         
         CLC   WORK+2(2),=C'09'                                                 
         BE    SETDAT10                                                         
         CLC   WORK+2(2),=C'12'                                                 
         BE    SETDAT10                                                         
         LHI   R0,1                SET TO ADVANCE A MONTH                       
         B     SETDAT8                                                          
*                                                                               
SETDAT10 MVC   WORK+6(6),WORK      SET END DATE=TO DATE                         
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK+10(2),=C'31'                                                
         GOTO1 MOBILE,DMCB,(2,WORK),(1,WORK+16)                                 
*                                                                               
         GOTO1 DATCON,DMCB,(2,WORK+18),AUTOEND  GET BRDMON END DATE             
*                                                                               
* IF YOU DON'T DO THIS, YOU WILL GET ESTS BASED ON WHAT IS IN QSTART!           
         MVC   QSTART(12),AUTOSTRT                                              
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*=====================================================================*         
* READ AND PROCESS BILL RECORDS                                                 
*=====================================================================*         
*                                                                               
*&&DO                                                                           
PROCBL   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E01'                                                  
         MVC   KEY+2(1),SVAGYMD                                                 
         MVC   KEY+3(2),SVCLT                                                   
         MVC   KEY+5(1),SVPRDCD                                                 
         CLI   SVPRDCD,X'FF'                                                    
         BNE   *+8                                                              
         MVI   KEY+5,0                                                          
         MVC   KEY+6(1),BEST                                                    
         GOTO1 HIGH                                                             
         B     PRB22                                                            
*                                                                               
PRB20    GOTO1 SEQ                                                              
*                                                                               
PRB22    CLC   KEY(5),KEYSAVE      0E01/A-M/CLT                                 
         BNE   PRBX                                                             
         CLC   KEY(6),KEYSAVE      0E01/A-M/CLT/PRD                             
         BE    PRB30                                                            
*                                                                               
* CHANGE OF PRODUCT *                                                           
*                                                                               
         CLI   BPRD,X'FF'          TEST POL REQUEST                             
         BNE   PRBX                                                             
*                                                                               
         CLI   QCOST2,C'Y'         TEST COS2 REQUEST                            
         BNE   PRB30                                                            
         CLI   KEY+12,1            TEST 2ND CURRENCY BILL                       
         BNE   PRB20                                                            
*                                                                               
PRB30    L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         USING STABUCKD,R6                                                      
         GOTO1 GET                                                              
*                                                                               
*======================================================*                        
* SEARCH FOR ELEMENTS IN REQUEST PERIOD                *                        
*======================================================*                        
*                                                                               
         LA    R6,24(R6)                                                        
PRB30    CLI   0(R6),0                                                          
         BE    PRBX                                                             
         CLI   0(R6),X'0E'                                                      
         BE    PRB34                                                            
*                                                                               
PRB32    SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         B     PRB30                                                            
*                                                                               
PRB34    L     R4,APRBLPER                                                      
*                                                                               
PRB34    CLC   0(2,R4),2(R6)       MATCH PERIOD NUMBERS                         
         BE    PRB30                                                            
         LA    R4,6(R4)                                                         
         CLI   0(R4),0                                                          
         BE    PRB32                                                            
         B     PRB34                                                            
*                                                                               
         XC    BFREC,BFREC                                                      
         MVI   BFTYPE,C'B'         SET TYPE=BILL                                
         L     RE,ADBUY                                                         
         MVC   PRKMKT(5),7(RE)     MKT/STA (NOTE-MKT IS MGR IF                  
*                                           READING ID POINTERS)                
         GOTO1 MSUNPK,DMCB,PRKMKT,WORK,STA                                      
         MVC   PRNAME(4),STA                                                    
         MVC   PRNAME+4(3),=C'- M'                                              
         MVC   PRNAME+5(1),STA+4                                                
         CLI   STA+4,C'A'                                                       
         BE    PRB26                                                            
         CLI   STA+4,C'F'                                                       
         BE    PRB26                                                            
         MVC   PRNAME+5(2),=C'TV'                                               
         CLI   QMED,C'T'                                                        
         BE    PRB26                                                            
         MVC   PRNAME+4(5),SPACES                                               
*                                                                               
         USING STABELEM,R6                                                      
PRB26    DS    0H                                                               
         XC    GUPGROSS(12),GUPGROSS                                            
         XC    SVBFORM,SVBFORM                                                  
         CLI   RQGETBF,C'Y'        TEST BILL FORMULA ADJUST                     
         BNE   PRB27               YES, USE GETBF, BUT ONLY TO GET              
         MVI   MODE,PROCBUY        FORMULA, NOT AMOUNT                          
         GOTO1 GETBF,DMCB,(BPRD,GUPGROSS),SVBFORM                               
         MVI   MODE,ESTFRST                                                     
*                                                                               
PRB27    DS    0H                                                               
         GOTO1 SPBVAL,DMCB,(C'E',STABELEM),SPBVALD,SVBFORM                      
*                                                                               
         MVC   GUPGROSS,SPBVACT    ACTUAL                                       
         CLI   RQGETBF,C'Y'                                                     
         BE    *+10                                                             
         MVC   GUPGROSS,SPBVEGRS   OR EFFECTIVE GROSS                           
         MVC   GUPNET,SPBVENET     AND NET                                      
         MVC   GUPTAX,SPBVETAX     AND TAX                                      
*                                                                               
         OC    PRGROSS+1(3),PRGROSS+1   TEST GROSS-UP REQUEST                   
         BZ    *+8                                                              
         BRAS  RE,PRBGUP                                                        
*                                                                               
PRB28    L     R1,GUPGROSS                                                      
         LH    R0,STABSPTS                                                      
*                                                                               
         CLI   RCSUBPRG,4          TEST BILLED SUMMARY                          
         BE    *+8                                                              
         LCR   R0,R0                                                            
         LCR   R1,R1                                                            
*                                                                               
         CVD   R0,DUB                                                           
         AP    PRDATA(8),DUB       ADD TO TOTAL SPOTS                           
*                                                                               
PRB30    CVD   R1,DUB                                                           
         ZAP   SVDOLS,DUB          SAVE DOLLARS                                 
         L     R0,GUPTAX           GET TAX DOLLARS                              
         CLI   RCSUBPRG,4          TEST BILLED SUMMARY                          
         BE    *+6                                                              
         LCR   R0,R0                                                            
         CVD   R0,DUB                                                           
         ZAP   SVTAX,DUB                                                        
         AP    0(8,R3),SVDOLS      ADD TO MONTHLY DOLLARS                       
         TM    DATASW,DSTAX        TEST REPORTING TAX SEPARATELY                
         BZ    PRB32               NO                                           
         AP    104(8,R3),SVTAX     ADD TO TAX DOLLARS                           
         CLI   PROGPROF+4,C'X'     TEST EXCLUDE TAX FROM GROSS                  
         BNE   *+10                NO                                           
         SP    0(8,R3),SVTAX       YES - GET IT OUT OF THERE                    
PRB32    B     PRB32                                                            
*                                                                               
PRBX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
*&&                                                                             
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
SETRATE  NTR1  BASE=*,LABEL=*                                                   
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         TM    BDCIND2,BDCCONVQ    IS BDCIND ALREADY CHAR (CONVERTED)?          
         BZ    *+14                 NO - NEED TO INTERPRET                      
         MVC   THISRTYP,BDCIND      YES - JUST PASS IT AS IS :)                 
         B     SETRATEX                                                         
*                                                                               
         MVI   THISRTYP,BDCC       COMISSION ONLY                               
         TM    BDCIND2,X'80'                                                    
         BNZ   SETRATEX                                                         
*                                                                               
         MVI   THISRTYP,BDCNTP     P RATE                                       
         CLI   BDCIND,X'00'                                                     
         BE    SETRATEX                                                         
*                                                                               
         MVI   THISRTYP,BDCF       FEE RATE                                     
         TM    BDCIND,X'80'                                                     
         BNZ   SETRATEX                                                         
         MVI   THISRTYP,BDCQ       Q RATE                                       
         TM    BDCIND,X'40'                                                     
         BNZ   SETRATEX                                                         
         MVI   THISRTYP,BDCGRS     GROSS                                        
         TM    BDCIND,X'20'                                                     
         BNZ   SETRATEX                                                         
         MVI   THISRTYP,BDCN       NET                                          
         TM    BDCIND,X'10'                                                     
         BNZ   SETRATEX                                                         
         MVI   THISRTYP,BDCV                                                    
         TM    BDCIND,X'08'                                                     
         BNZ   SETRATEX                                                         
         MVI   THISRTYP,BDCS       SPECIAL                                      
         TM    BDCIND,X'04'                                                     
         BNZ   SETRATEX                                                         
         MVI   THISRTYP,BDCX       X RATE                                       
         TM    BDCIND,X'02'                                                     
         BNZ   SETRATEX                                                         
         CLC   QAGY,=C'DF'         NTP ONLY FOR PROGRAM EXCHANGE                
         BNE   *+16                                                             
         MVI   THISRTYP,BDCNTP     NTP RATE                                     
         TM    BDCIND,X'FE'        X'FE' IS SAME IN GETRATE                     
         BZ    SETRATEX            CAREFUL HERE - NTP HAS NO BITS!              
*                                                                               
         MVI   THISRTYP,C'?'       I HAVE NO IDEA WHAT IT IS                    
*                                                                               
SETRATEX J     EQXIT                                                            
         LTORG                                                                  
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
GETCML   L     RF,0(R1)                                                         
         SAM31                                                                  
         MVC   MYCMLREC,0(RF)                                                   
         SAM24                                                                  
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
SPDBWORK DS    0D                                                               
         DC    CL8'SPDBWORK'                                                    
PSLIST   DS    XL600                                                            
         DC    CL8'**TSARD*'                                                    
TSAREA   DS    CL48                                                             
*                                                                               
VTSAROFF DS    A                                                                
TSARBUFF DS    A                                                                
TSARBUFL DC    A(30000*1024)                                                    
*                                                                               
ARRECTAB DS    A                                                                
AIRECTAB DS    A                                                                
ABRECTAB DS    A                                                                
AMRECTAB DS    A                                                                
ASRECTAB DS    A                                                                
ATRECTAB DS    A                                                                
ADRECTAB DS    A                                                                
APRECTAB DS    A                                                                
AGRECTAB DS    A                                                                
ALRECTAB DS    A                                                                
*                                                                               
AKLLGTAB DS    A                                                                
AKLLBTAB DS    A                                                                
AKLLRTAB DS    A                                                                
AKLLPTAB DS    A                                                                
AKLLITAB DS    A                                                                
AKLLJTAB DS    A                                                                
*                                                                               
*                                                                               
SVRERATE DS    A                                                                
SAVERE   DS    A                                                                
AMEDWEEK DS    A                                                                
AMEDMON  DS    A                                                                
APRBLPER DS    A                                                                
MYSLNTAB DS    A                                                                
BUFFCNT  DS    F                                                                
NUMDEMS  DS    F                                                                
CMLTBID  DS    XL4                 00/A-M/CLT OF CURRENT TABLE                  
BUFFERIN DC    V(BUFFERIN)                                                      
CURSPOT  DS    H                                                                
MGBYSPOT DS    H                                                                
ACURSPOT DS    A                                                                
ACURPSL  DS    A                   ADDR OF CURRENT MEDPSL LINE                  
SVB0PROF DS    CL16                                                             
SVQSTART DS    CL12                                                             
AUTOSTRT DS    CL6                                                              
AUTOEND  DS    CL6                                                              
MAXEND   DS    CL6                 MAX END DATE (8 QUARTERS BACK)               
SVBOOK   DS    XL2                                                              
AUTOSET  DC    C'N'                                                             
LASTKEY  DS    XL9                 LAST BUY A-M/CL/PR/MK/ST                     
*                                                                               
ELEMDATE DS    XL2                                                              
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
ELEMNUM  DS    PL2                                                              
ISRATING DS    C                                                                
SORTED   DC    C'N'                                                             
SAVEQSTA DS    CL5                                                              
SVTABENT DS    D                                                                
ZEROCST2 DS    C                   ZERO COST, NEGATIVE COST2                    
SVDMINB  DS    X                                                                
SVDMOUTB DS    X                                                                
HIGHOVER DS    C                   HIGH DEMO OVERRIDE VALUES ON BUY             
SV00DAY  DS    X                   FISCAL START DAY                             
*                                                                               
         DS    0D                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=6100,             X        
               MACRF=GM,EODAD=REQF30                                            
*                                                                               
         DS    0D                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,25,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=25'                                    
*                                                                               
SORTREC  DS    XL25                                                             
SVSORT   DS    XL25                                                             
SORTCNT  DC    PL4'0'                                                           
SVCNMED  DS    XL2                                                              
*                                                                               
SV1OR2   DS    XL1                 2 FOR 2-BYTE LINE NUMBERS                    
*                                                                               
         DS    0F                  ALIGN GETBLK ON A FULLWORD BOUNDARY          
GETBLK   DS    XL64                FOR SPGETBUYD                                
*                                                                               
STAWORK  DS    XL(STAPACKL)                                                     
FILTSTA  DS    XL5                 PACKED CBL STATION TO FILTER ON              
REQCBNET DS    CL3                 CABLE NETWORK                                
*                                                                               
*PREFIX=XA$                                                                     
       ++INCLUDE SPXCHAREA                                                      
*PREFIX=                                                                        
VBKTYPTB DS    A                   A(BOOKTYPE TABLE) IN DEMTABS                 
BKTYPTBL DS    H                   L'BOOKTYPE TABLE ENTRY                       
                                                                                
*=========================================================                      
* NOTE THAT SORT SEQUENCE IS A-M/CLT/PRD/EST/MKT-STA/LINE                       
*=========================================================                      
                                                                                
SORTRECD DSECT                                                                  
SORTAGMD DS    XL1                                                              
SORTCLT  DS    XL2                                                              
SORTPRD  DS    XL1                                                              
SORTEST  DS    XL1                                                              
SORTMKT  DS    XL2                                                              
SORTSTA  DS    XL3                                                              
SORTLIN  DS    XL2                                                              
SORTSPL  DS    XL1                 X'80' FOR SPILL MARKET                       
         DS    XL2                                                              
SORTSTAT DS    XL1                                                              
SORTSEQ  DS    XL4                                                              
         DS    XL5                                                              
*===>>>>                                                                        
SPDB02   CSECT                                                                  
*===>>>>                                                                        
CMLTABMAX EQU  100000                                                           
CMLTABL  DC    A(CMLTABMAX*MYCMLRECL)                                           
*                                                                               
CMLPAR1  DC    A(0)                                                             
CMLPAR2  DC    A(0)                A(TABLE)                                     
CMLPAR3  DC    F'0'                RECORD COUNT                                 
CMLPAR4  DC    A(MYCMLRECL)       RECORD LENGTH                                 
CMLPAR5  DC    A(MYCMLKEYL)        KEYDSPL/KEYLEN                               
CMLPAR6  DC    A(CMLTABMAX)                                                     
*                                                                               
         DS    0D                                                               
MYCMLREC DS    0XL(MYCMLRECL)                                                   
MYCSTART DS    0X                  REC START, USED FOR EQUATES                  
*                                                                               
MYCBAGYMD DS   X                   BINARY AGENCY/MEDIA                          
MYCSEQ   DS    XL2                 BINARY SEQUENCE NUMBER                       
*                                                                               
MYCMLKEYL EQU   *-MYCSTART                                                      
*                                                                               
MYCEBC   DS    CL8                 EBCDIC COMMERCIAL                            
MYCDESC1 DS    CL15                                                             
MYCDESC2 DS    CL20                                                             
MYCDESC3 DS    CL20                                                             
MYCCLS   DS    CL4                                                              
MYCADID  DS    CL12                                                             
MYCMLX   EQU   *                                                                
MYCMLRECL EQU   *-MYCSTART                                                      
*                                                                               
AFDTABMAX EQU  200000                                                           
AFDTABL  DC    A(AFDTABMAX*AFDRECL)                                             
*                                                                               
AFDPAR1  DC    A(0)                                                             
AFDPAR2  DC    A(0)                A(TABLE)                                     
AFDPAR3  DC    F'0'                RECORD COUNT                                 
AFDPAR4  DC    A(AFDRECL)          RECORD LENGTH                                
AFDPAR5  DC    A(4)                KEYDSPL/KEYL=EST/LINE/SPOTNUM                
AFDPAR6  DC    A(AFDTABMAX)                                                     
*                                                                               
         DS    0D                                                               
AFDREC   DS    0XL(AFDRECL)                                                     
AFDRECST DS    0X                  RECORD START, USED FOR EQUATES               
AFDEST   DS    XL1                                                              
AFDBUY   DS    XL2                 BUYLINE                                      
AFDSPNUM DS    XL1                 SPOT NUMBER (0B/0C ELEM NUMBER)              
AFDACTDT DS    XL2                 AFFID DATE                                   
AFDACTTM DS    XL2                 AFFID TIME                                   
AFDFILM  DS    XL2                                                              
AFDRECL  EQU   *-AFDRECST                                                       
*                                                                               
D29TABMAX EQU  32000                                                            
D29TABL  DC    A(D29TABMAX*D29RECL)                                             
*                                                                               
D29PAR1  DC    A(0)                                                             
D29PAR2  DC    A(0)                A(TABLE)                                     
D29ATAB  EQU   D29PAR2                                                          
D29PAR3  DC    F'0'                RECORD COUNT                                 
D29COUNT EQU   D29PAR3                                                          
D29PAR4  DC    A(D29RECL)          RECORD LENGTH                                
D29PAR5  DC    AL1(0),AL3(7)       KEYDSPL/KEYL=7                               
D29PAR6  DC    A(D29TABMAX)                                                     
*                                                                               
*                                                                               
VBINSR31 DC    V(BINSRCH)          *INCLUDED BINSRCH31                          
*                                                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'RUNTOTS'                                                     
RUNTOTS  DS    0D                                                               
RUNRECS  DC    PL8'0'                                                           
RUNSPOTS DC    PL8'0'                                                           
RUNDOLS  DC    PL8'0'                                                           
*                                                                               
         DS    0D                                                               
DLCB     DS    XL256                                                            
*                                                                               
*                                                                               
* THESE FIELDS USED TO GET WIDE PRINT LINE OVERRIDES                            
* THEY GET MOVED INTO THE DLCB TO OVERRIDE MAXLINE                              
*                                                                               
MAXLINE  DC    H'132'              MAX LINE WIDTH                               
DELIM    DC    C' '                FIELD DELIMITER CHR                          
EOTCHR   DC    C'"'                END OF TEXT FIELD DELIMITER                  
EOTALT   DC    C''''               END OF TEXT CHR ALTERNATE                    
EOLCHR   DC    X'5E'               END OF LINE CHAR - SEMICOLON                 
EORCHR   DC    C':'                END OF REPORT CONTROL CHR                    
*                                                                               
*                                                                               
*=============================================================                  
* BUFFERIN RECORD                                                               
*=============================================================                  
         DS    0D                                                               
         DC    CL8'**BFREC*'                                                    
BFREC    DS    0D                                                               
*                                                                               
BFKEY    DS    0XL40                                                            
BFKTYPE  DS    CL1'D'              DEMO RECORD                                  
BFKQPRD  DS    CL3                                                              
BFKBPRD  DS    XL1                                                              
BFKQPRD2 DS    CL3                                                              
BFKBPRD2 DS    XL1                                                              
BFKDPT   DS    CL1                                                              
BFKDPTNM DS    CL3                                                              
BFKSLN   DS    XL1                                                              
BFKWEEK  DS    XL2                 2 BYTE WEEK START DATE                       
BFKMED   DS    C                   MEDIA (CANADA ONLY)                          
BFKNET   DS    CL4                 CANADIAN NETWK CALL LETTERS                  
*                                                                               
         ORG   BFKEY                                                            
BFFKTYP  DS    CL1'F'              FINANCIAL RECORD                             
BFFKQMED DS    C                                                                
BFFKYM   DS    XL2                 YEAR/MONTH                                   
BFFKQPRD DS    CL3                                                              
BFFKMKT  DS    XL2                 MARKET                                       
BFFKSTA  DS    XL3                 STATION                                      
BFFKFLG  DS    X                   FLAG                                         
BFFKFCNQ EQU   X'01'               CANADIAN NETWORK                             
*                                                                               
         ORG   BFKEY+40            FIELDS NOT IN KEY ANYMORE                    
BFKDAY   DS    XL1                                                              
BFKTIME  DS    XL4                 START/END TIMES                              
BFKPROG  DS    CL17                                                             
         DS    XL10                SPARE                                        
*                                                                               
* DEMOGRAPHIC DATA                                                              
*                                                                               
BFDATA   DS    0F                                                               
BFSPOTS  DS    F                                                                
BFDOL    DS    F                                                                
BFDOLEQ  DS    F                                                                
BFTAX    DS    F                                                                
BFDOL2   DS    F                   COST2 DOLLARS                                
BFDEM01  DS    F                                                                
BFDEM02  DS    F                                                                
BFDEM03  DS    F                                                                
BFDEM04  DS    F                                                                
BFDEM05  DS    F                                                                
BFDEM06  DS    F                                                                
BFDEM07  DS    F                                                                
BFDEM08  DS    F                                                                
BFDEM09  DS    F                                                                
BFDEM10  DS    F                                                                
BFDEM11  DS    F                                                                
BFDEM12  DS    F                                                                
BFDEM13  DS    F                                                                
BFDEM14  DS    F                                                                
BFDEM15  DS    F                                                                
BFDEM16  DS    F                                                                
BFDEM17  DS    F                                                                
BFDEM18  DS    F                                                                
BFDEM19  DS    F                                                                
BFDEM20  DS    F                                                                
         ORG   BFDEM02                                                          
BFGKDOL  DS    F                   GOAL LOCKIN DOLLARS                          
BFGKDOLQ DS    F                   GOAL LOCKIN EQUIV DOLLARS                    
BFGKDEM1 DS    F                   GOAL LOCKIN DEMO                             
*                                                                               
BFLKSPT  DS    F                   MKT LOCKIN SPOTS                             
BFLKDOL  DS    F                   MKT LOCKIN DOLLARS                           
BFLKDOLQ DS    F                   MKT LOCKIN EQUIV DOLLARS                     
BFLKDEM1 DS    F                   MKT LOCKIN DEMO                              
         ORG                                                                    
*                                                                               
         ORG                                                                    
BFDATAN  EQU   (*-BFDATA)/4        NUMBER OF COLUMNS                            
BFRECL   EQU   *-BFREC                                                          
BFDATAL  EQU   *-BFDATA                                                         
*                                                                               
* FOR FINANCIAL RECORD (TYPE=F)                                                 
*                                                                               
         ORG   BFDATA                                                           
BFFDATA  DS    0XL16                                                            
BFFSPOTS DS    F                                                                
BFFGRS   DS    F                                                                
BFFNET   DS    F                                                                
BFFTAX   DS    F                                                                
                                                                                
*====================================================================           
* THESE VALUES USED FOR RERATED DEMOS - NOT WRITTEN TO BUFFERIN FILE            
*====================================================================           
         ORG                                                                    
BFDEM01R DS    F                                                                
BFDEM02R DS    F                                                                
BFDEM03R DS    F                                                                
BFDEM04R DS    F                                                                
BFDEM05R DS    F                                                                
BFDEM06R DS    F                                                                
BFDEM07R DS    F                                                                
BFDEM08R DS    F                                                                
BFDEM09R DS    F                                                                
BFDEM10R DS    F                                                                
BFDEM11R DS    F                                                                
BFDEM12R DS    F                                                                
BFDEM13R DS    F                                                                
BFDEM14R DS    F                                                                
BFDEM15R DS    F                                                                
BFDEM16R DS    F                                                                
BFDEM17R DS    F                                                                
BFDEM18R DS    F                                                                
BFDEM19R DS    F                                                                
BFDEM20R DS    F                                                                
*                                  THESE VALUES FOR AFFIDS                      
         DS    0D                                                               
AFDEMS   DS    0XL80                                                            
AFDEM01  DS    F                                                                
AFDEM02  DS    F                                                                
AFDEM03  DS    F                                                                
AFDEM04  DS    F                                                                
AFDEM05  DS    F                                                                
AFDEM06  DS    F                                                                
AFDEM07  DS    F                                                                
AFDEM08  DS    F                                                                
AFDEM09  DS    F                                                                
AFDEM10  DS    F                                                                
AFDEM11  DS    F                                                                
AFDEM12  DS    F                                                                
AFDEM13  DS    F                                                                
AFDEM14  DS    F                                                                
AFDEM15  DS    F                                                                
AFDEM16  DS    F                                                                
AFDEM17  DS    F                                                                
AFDEM18  DS    F                                                                
AFDEM19  DS    F                                                                
AFDEM20  DS    F                                                                
*                                                                               
THISBEAA DC   C'A'                                                              
THISBEAB DC   C'B'                                                              
THISBEAD DC   C'D'                                                              
THISBEAE DC   C'E'                                                              
THISBEAF DC   C'F'                                                              
THISBEAG DC   C'G'                                                              
THISBEAH DC   C'H'                                                              
THISBEAI DC   C'I'                                                              
THISBEAJ DC   C'J'                                                              
THISBEAK DC   C'K'                                                              
THISBEAL DC   C'L'                                                              
THISBEAM DC   C'M'                                                              
THISBEAN DC   C'N'                 MREC FOR RENTRAK                             
THISBEAP DC   C'P'                                                              
THISBEAR DC   C'R'                                                              
THISBEAS DC   C'S'                                                              
THISBEAT DC   C'T'                                                              
THISBEAU DC   C'U'                 TREC FOR RENTRAK                             
THISBEAX DC   C'X'                                                              
THISBEAY DC   C'Y'                                                              
THISBEAZ DC   C'Z'                                                              
POLPRD   DC   C'POL'                                                            
*                                                                               
THISBLNK DC   C' '                                                              
*                                                                               
THISID   DS    XL20                                                             
         ORG   THISID                                                           
TIDAGYA  DS    CL2                                                              
TIDQMED  DS    CL1                                                              
TIDBCLT  DS    XL2                                                              
TIDBPRD  DS    XL1                                                              
TIDBEST  DS    XL1                                                              
TIDBMKT  DS    XL2                 X'80' IS ON FOR SPILL MARKET                 
TIDBSTA  DS    XL3                                                              
TIDBDPT  DS    XL1                                                              
TIDBSLN  DS    XL1                                                              
TIDPURP  DS    CL6                 PURPOSE CODE                                 
         ORG                                                                    
*                                  UNIQUE ID FOR GOALS                          
         ORG   THISID                                                           
THISIDG  DS    0XL18                                                            
TIDGAGYA DS    CL2                                                              
TIDGQMED DS    CL1                                                              
TIDGCLT  DS    XL2                                                              
TIDGPRD  DS    XL1                                                              
TIDGEST  DS    XL1                                                              
TIDGMKT  DS    XL2                                                              
TIDGPRD2 DS    XL1                                                              
TIDGDPT  DS    XL1                                                              
TIDGSLN  DS    XL1                                                              
TIDGPURP DS    CL6                 PURPOSE CODE                                 
         ORG                                                                    
*                                                                               
THISAGY  DS    CL2                                                              
THISMED  DS    CL1                                                              
THISCLT  DS    CL3                                                              
THISPRD  DS    CL3                                                              
THISEST  DS    CL3                                                              
THISBMKT DS    XL2                                                              
THISMKT  DS    CL4                                                              
THISORIG DS    CL4                                                              
THISSTA  DS    CL8                                                              
THISNET  DS    CL4                 CANADIAN NETWORK                             
THISSPL  DS    CL1                 THIS IS SPILL (Y/N)                          
THISBUY  DS    CL3                 BUYLINE NUMBER                               
THISSPT  DS    CL3                 SPOT NUMBER                                  
THISWEEK DS    CL10                YYYY-MM-DD                                   
THISYYMM DS    CL10                YYYY-MM-DD                                   
THISDATE DS    CL10                YYYY-MM-DD                                   
THISCRDT DS    CL10                YYYY-MM-DD  BUY CREATION DATE                
THISCHDT DS    CL10                YYYY-MM-DD  BUY CHANGE DATE                  
THISCHPD DS    CL8                 PID                                          
THISSEQ  DS    CL3                                                              
THISSLN  DS    CL3                                                              
THISDAY  DS    CL7                                                              
THISEQV  DS    CL5                                                              
THISTIME DS    CL11                                                             
THISREP  DS    CL3                                                              
THISADJ  DS    CL2                                                              
THISRTYP DS    CL1                 RATE TYPE                                    
THISTRCM DS    CL8                 TRAFFIC COMMERCIAL CODE                      
THISAFCM DS    CL8                 AFFID COMMERCIAL CODE                        
THISTRID DS    CL12                TRAFFIC AD ID                                
THISAFID DS    CL12                AFFID AD ID                                  
THISBYID DS    CL12                BUY ID                                       
THISBKTP DS    CL2                 BOOK TYPE                                    
*                                                                               
THISAF19 DS    0CL19               DATE/TIME STRING                             
THISAFDT DS    CL10                                                             
THISAFSP DS    CL1                                                              
THISAFTM DS    CL8                 AFFID TIME (HH:MM:SS)                        
*                                                                               
THISTRD1 DS    CL15                DESC 1                                       
THISTRD2 DS    CL20                                                             
THISTRD3 DS    CL20                                                             
THISTRCL DS    CL4                                                              
THISDMTY DS    CL1                                                              
THISBOOK DS    CL2                                                              
THISQPRD DS    CL3                                                              
THISIDX  DS    XL(L'THISID*2)                                                   
THISIDGX DS    XL(L'THISIDG*2)                                                  
THISORDR DS    XL(L'THISID*2)                                                   
THISSPIL DS    CL4                                                              
*                                                                               
THISDMER DS    C                                                                
THISNET4 DS    CL4                 4-CHARACTER NETWORK                          
*                                                                               
         DS    0D                                                               
DEMDATA  DS    0XL14                                                            
*                                                                               
DEMMOD01 DS    CL1                                                              
DEMCAT01 DS    CL5                                                              
DEMVAL01 DS    CL8                                                              
*                                                                               
DEMMOD02 DS    CL1                                                              
DEMCAT02 DS    CL5                                                              
DEMVAL02 DS    CL8                                                              
*                                                                               
DEMMOD03 DS    CL1                                                              
DEMCAT03 DS    CL5                                                              
DEMVAL03 DS    CL8                                                              
*                                                                               
DEMMOD04 DS    CL1                                                              
DEMCAT04 DS    CL5                                                              
DEMVAL04 DS    CL8                                                              
*                                                                               
DEMMOD05 DS    CL1                                                              
DEMCAT05 DS    CL5                                                              
DEMVAL05 DS    CL8                                                              
*                                                                               
DEMMOD06 DS    CL1                                                              
DEMCAT06 DS    CL5                                                              
DEMVAL06 DS    CL8                                                              
*                                                                               
DEMMOD07 DS    CL1                                                              
DEMCAT07 DS    CL5                                                              
DEMVAL07 DS    CL8                                                              
*                                                                               
DEMMOD08 DS    CL1                                                              
DEMCAT08 DS    CL5                                                              
DEMVAL08 DS    CL8                                                              
*                                                                               
DEMMOD09 DS    CL1                                                              
DEMCAT09 DS    CL5                                                              
DEMVAL09 DS    CL8                                                              
*                                                                               
DEMMOD10 DS    CL1                                                              
DEMCAT10 DS    CL5                                                              
DEMVAL10 DS    CL8                                                              
*                                                                               
DEMMOD11 DS    CL1                                                              
DEMCAT11 DS    CL5                                                              
DEMVAL11 DS    CL8                                                              
*                                                                               
DEMMOD12 DS    CL1                                                              
DEMCAT12 DS    CL5                                                              
DEMVAL12 DS    CL8                                                              
*                                                                               
DEMMOD13 DS    CL1                                                              
DEMCAT13 DS    CL5                                                              
DEMVAL13 DS    CL8                                                              
*                                                                               
DEMMOD14 DS    CL1                                                              
DEMCAT14 DS    CL5                                                              
DEMVAL14 DS    CL8                                                              
*                                                                               
DEMMOD15 DS    CL1                                                              
DEMCAT15 DS    CL5                                                              
DEMVAL15 DS    CL8                                                              
*                                                                               
DEMMOD16 DS    CL1                                                              
DEMCAT16 DS    CL5                                                              
DEMVAL16 DS    CL8                                                              
*                                                                               
DEMMOD17 DS    CL1                                                              
DEMCAT17 DS    CL5                                                              
DEMVAL17 DS    CL8                                                              
*                                                                               
DEMMOD18 DS    CL1                                                              
DEMCAT18 DS    CL5                                                              
DEMVAL18 DS    CL8                                                              
*                                                                               
DEMMOD19 DS    CL1                                                              
DEMCAT19 DS    CL5                                                              
DEMVAL19 DS    CL8                                                              
*                                                                               
DEMMOD20 DS    CL1                                                              
DEMCAT20 DS    CL5                                                              
DEMVAL20 DS    CL8                                                              
*                                                                               
DEMDATAX EQU   *                                                                
         ORG   DEMDATA                                                          
THISDM01 DS    CL6                                                              
THISDM02 DS    CL6                                                              
THISDM03 DS    CL6                                                              
THISDM04 DS    CL6                                                              
THISDM05 DS    CL6                                                              
THISDM06 DS    CL6                                                              
THISDM07 DS    CL6                                                              
THISDM08 DS    CL6                                                              
THISDM09 DS    CL6                                                              
THISDM10 DS    CL6                                                              
THISDM11 DS    CL6                                                              
THISDM12 DS    CL6                                                              
THISDM13 DS    CL6                                                              
THISDM14 DS    CL6                                                              
THISDM15 DS    CL6                                                              
THISDM16 DS    CL6                                                              
THISDM17 DS    CL6                                                              
THISDM18 DS    CL6                                                              
THISDM19 DS    CL6                                                              
THISDM20 DS    CL6                                                              
         ORG                                                                    
*                                                                               
BUFFET   BUFFD TYPE=B,KEYLEN=L'BFKEY,COLUMNS=BFDATAN,FILE=BUFFWK,      *        
               BUFFERS=10                                                       
*                                                                               
IOAREA   DS    XL1024                                                           
*                                                                               
*                                                                               
* ENTRIES ARE                                                                   
* AL4(DATA)                                                                     
* AL1(L'DATA)                                                                   
* CL1'TYPE'                                                                     
* C'  '            IF NOT X'00' EOR IF FIELD NOT > THIS VALUE                   
* X'01'            CONVERT THE FIELD TO DECIMAL BEFORE WRITE                    
* OR IF TYPE=B,    LAST BYTE IS NUMBER OF DECIMAL PLACES                        
*                                                                               
*===============================================================                
* IN FIXED LENGTH MODE, BINARY FIELDS ARE OUTPUT AT FIXED LENGTHS               
* 1 DECIMAL VALUES GO OUT AS 8 BYTES                                            
* 2 DECIMAL VALUES GO OUT AS 13 BYTES                                           
*===============================================================                
*                                                                               
         DS    0D                                                               
RECTAB   DS    0XL8                                                             
         DC    CL8'DRECTAB*'                                                    
DRECTAB  DC    AL4(THISBEAD),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISIDX),AL1(L'THISIDX),C'X',2X'00' (HEX FIELD)              
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(BFKQPRD),AL1(L'BFKQPRD),C'T',2X'00'                          
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
         DC    AL4(THISSTA),AL1(L'THISSTA),C'T',2X'00'                          
         DC    AL4(THISSPL),AL1(L'THISSPL),C'T',2X'00'                          
         DC    AL4(BFKDPTNM),AL1(L'BFKDPTNM),C'T',2X'00'                        
         DC    AL4(BFKDPT),AL1(L'BFKDPT),C'T',2X'00'                            
         DC    AL4(THISSLN),AL1(L'THISSLN),C'N',2X'00'                          
         DC    AL4(TIDPURP),AL1(L'TIDPURP),C'T',2X'00'                          
         DC    AL4(THISWEEK),AL1(L'THISWEEK),C'T',2X'00'                        
         DC    AL4(THISYYMM),AL1(L'THISYYMM),C'T',2X'00'                        
         DC    AL4(THISEQV),AL1(L'THISEQV),C'N',2X'00'                          
         DC    AL4(THISDMTY),AL1(L'THISDMTY),C'T',2X'00' RERATE TYPE            
*                                                                               
         DC    AL4(BFSPOTS),AL1(4),C'B',X'00',X'00'                             
         DC    AL4(BFDOL),AL1(4),C'B',X'00',X'02'   <== 2 DEC                   
         DC    AL4(BFDOLEQ),AL1(4),C'B',X'00',X'02' <== 2 DEC                   
         DC    AL4(BFTAX),AL1(4),C'B',X'00',X'02'   <== 2 DEC                   
*                                                                               
         DC    AL4(DEMMOD01),AL1(L'DEMMOD01),C'T',C' ',X'00' RTG/IMP            
         DC    AL4(DEMCAT01),AL1(L'DEMCAT01),C'N',2X'00' CATEGORY NUM           
         DC    AL4(BFDEM01),AL1(4),C'B',X'00',X'01'      VALUE/ 1 DEC           
*                                                                               
         DC    AL4(DEMMOD02),AL1(L'DEMMOD02),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT02),AL1(L'DEMCAT02),C'N',2X'00'                        
         DC    AL4(BFDEM02),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD03),AL1(L'DEMMOD03),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT03),AL1(L'DEMCAT03),C'N',2X'00'                        
         DC    AL4(BFDEM03),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD04),AL1(L'DEMMOD04),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT04),AL1(L'DEMCAT04),C'N',2X'00'                        
         DC    AL4(BFDEM04),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD05),AL1(L'DEMMOD05),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT05),AL1(L'DEMCAT05),C'N',2X'00'                        
         DC    AL4(BFDEM05),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD06),AL1(L'DEMMOD06),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT06),AL1(L'DEMCAT06),C'N',2X'00'                        
         DC    AL4(BFDEM06),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD07),AL1(L'DEMMOD07),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT07),AL1(L'DEMCAT07),C'N',2X'00'                        
         DC    AL4(BFDEM07),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD08),AL1(L'DEMMOD08),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT08),AL1(L'DEMCAT08),C'N',2X'00'                        
         DC    AL4(BFDEM08),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD09),AL1(L'DEMMOD09),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT09),AL1(L'DEMCAT09),C'N',2X'00'                        
         DC    AL4(BFDEM09),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD10),AL1(L'DEMMOD10),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT10),AL1(L'DEMCAT10),C'N',2X'00'                        
         DC    AL4(BFDEM10),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD11),AL1(L'DEMMOD11),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT11),AL1(L'DEMCAT11),C'N',2X'00'                        
         DC    AL4(BFDEM11),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD12),AL1(L'DEMMOD12),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT12),AL1(L'DEMCAT12),C'N',2X'00'                        
         DC    AL4(BFDEM12),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD13),AL1(L'DEMMOD13),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT13),AL1(L'DEMCAT13),C'N',2X'00'                        
         DC    AL4(BFDEM13),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD14),AL1(L'DEMMOD14),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT14),AL1(L'DEMCAT14),C'N',2X'00'                        
         DC    AL4(BFDEM14),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD15),AL1(L'DEMMOD15),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT15),AL1(L'DEMCAT15),C'N',2X'00'                        
         DC    AL4(BFDEM15),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD16),AL1(L'DEMMOD16),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT16),AL1(L'DEMCAT16),C'N',2X'00'                        
         DC    AL4(BFDEM16),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD17),AL1(L'DEMMOD17),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT17),AL1(L'DEMCAT17),C'N',2X'00'                        
         DC    AL4(BFDEM17),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD18),AL1(L'DEMMOD18),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT18),AL1(L'DEMCAT18),C'N',2X'00'                        
         DC    AL4(BFDEM18),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD19),AL1(L'DEMMOD19),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT19),AL1(L'DEMCAT19),C'N',2X'00'                        
         DC    AL4(BFDEM19),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD20),AL1(L'DEMMOD20),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT20),AL1(L'DEMCAT20),C'N',2X'00'                        
         DC    AL4(BFDEM20),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC   X'FF'                                                             
         EJECT                                                                  
*=================================================================              
* THIS RECORD SENT FOR BUYLINE RERATE VERSION OF DOWNLOAD                       
*=================================================================              
         DS    0D                                                               
         DC    CL8'BRECTAB*'                                                    
BRECTAB  DC    AL4(THISBEAB),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISIDX),AL1(26),C'X',2X'00'         HEX BUYKEY              
*                                                                               
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(POLPRD),AL1(L'POLPRD),C'T',2X'00'                            
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
         DC    AL4(THISSTA),AL1(L'THISSTA),C'T',2X'00'                          
         DC    AL4(THISBUY),AL1(L'THISBUY),C'N',2X'00'                          
         DC    AL4(THISORIG),AL1(L'THISORIG),C'N',2X'00' ORIGIN MKT             
*                                                                               
         DC    AL4(BFKDPTNM),AL1(L'BFKDPTNM),C'T',2X'00'                        
         DC    AL4(THISSLN),AL1(L'THISSLN),C'N',2X'00'                          
         DC    AL4(THISDAY),AL1(L'THISDAY),C'T',2X'00'                          
         DC    AL4(THISTIME),AL1(L'THISTIME),C'T',2X'00'                        
         DC    AL4(TIDPURP),AL1(L'TIDPURP),C'T',2X'00'                          
         DC    AL4(BFKPROG),AL1(L'BFKPROG),C'T',2X'00'                          
         DC    AL4(THISREP),AL1(L'THISREP),C'T',2X'00'                          
         DC    AL4(THISADJ),AL1(L'THISADJ),C'T',2X'00'                          
         DC    AL4(THISRTYP),AL1(L'THISRTYP),C'T',2X'00'                        
*                                                                               
         DC    AL4(BFKDPT),AL1(L'BFKDPT),C'T',2X'00'                            
         DC    AL4(THISCRDT),AL1(L'THISCRDT),C'T',2X'00' CREATE DATE            
         DC    AL4(THISCHDT),AL1(L'THISCHDT),C'T',2X'00' CHANGE DATE            
         DC    AL4(THISCHPD),AL1(L'THISCHPD),C'T',2X'00' CHANGE PID             
*                                                                               
         DC    AL4(THISBYID),AL1(L'THISBYID),C'T',2X'00' BUY ID                 
*                                                                               
         DC    AL4(THISNET4),AL1(L'THISNET4),C'T',2X'00' 4CHAR NETWORK          
         DC    AL4(THISBKTP),AL1(L'THISBKTP),C'T',2X'00' BOOK TYPE              
         DC    AL4(THISORDR),AL1(26),C'X',2X'00'         HEX ORDER ID           
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         DS    0D                                                               
SRECTAB  DC    AL4(THISBEAS),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISIDX),AL1(26),C'X',2X'00'          HEX BUYKEY             
         DC    AL4(THISDATE),AL1(L'THISDATE),C'T',2X'00' ROT DAY DATE           
         DC    AL4(THISSEQ),AL1(L'THISSEQ),C'N',2X'00'   SEQNUM                 
         DC    AL4(THISQPRD),AL1(L'THISQPRD),C'T',2X'00'                        
         DC    AL4(THISSLN),AL1(L'THISSLN),C'N',2X'00'                          
         DC    AL4(THISWEEK),AL1(L'THISWEEK),C'T',2X'00'                        
         DC    AL4(THISYYMM),AL1(L'THISYYMM),C'T',2X'00'                        
         DC    AL4(BFDOL),AL1(4),C'B',X'00',X'02'   <== 2 DEC                   
         DC    AL4(BFTAX),AL1(4),C'B',X'00',X'02'   <== 2 DEC                   
         DC    AL4(BFDOL2),AL1(4),C'B',X'00',X'02'                              
         DC    AL4(THISAF19),AL1(L'THISAF19),C'T',2X'00' DATE/TIME              
         DC    AL4(THISAFCM),AL1(L'THISAFCM),C'T',2X'00'                        
         DC    AL4(THISTRCM),AL1(L'THISTRCM),C'T',2X'00'                        
         DC    AL4(THISAFID),AL1(L'THISAFID),C'T',2X'00'                        
         DC    AL4(THISTRID),AL1(L'THISTRID),C'T',2X'00'                        
         DC    X'FF'                                                            
*                                                                               
         DS    0D                                                               
TRECTAB  DC    AL4(THISBEAT),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISIDX),AL1(26),C'X',2X'00'          HEX BUYKEY             
         DC    AL4(THISDATE),AL1(L'THISDATE),C'T',2X'00' ROT DAY DATE           
         DC    AL4(THISSEQ),AL1(L'THISSEQ),C'N',2X'00'   SEQNUM                 
         DC    AL4(THISQPRD),AL1(L'THISQPRD),C'T',2X'00'                        
*                                                                               
         DC    AL4(DEMMOD01),AL1(L'DEMMOD01),C'T',C' ',X'00' RTG/IMP            
         DC    AL4(DEMCAT01),AL1(L'DEMCAT01),C'N',2X'00' CATEGORY NUM           
         DC    AL4(AFDEM01),AL1(4),C'B',X'00',X'02'       VALUE/ 2 DEC          
         DC    AL4(THISDMER),AL1(L'THISDMER),C'T',2X'00' DEM LOOKUP ERR         
*                                                                               
         DC    AL4(DEMMOD02),AL1(L'DEMMOD02),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT02),AL1(L'DEMCAT02),C'N',2X'00'                        
         DC    AL4(AFDEM02),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(THISDMER),AL1(L'THISDMER),C'T',2X'00' DEM LOOKUP ERR         
*                                                                               
         DC    AL4(DEMMOD03),AL1(L'DEMMOD03),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT03),AL1(L'DEMCAT03),C'N',2X'00'                        
         DC    AL4(AFDEM03),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(THISDMER),AL1(L'THISDMER),C'T',2X'00' DEM LOOKUP ERR         
*                                                                               
         DC    AL4(DEMMOD04),AL1(L'DEMMOD04),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT04),AL1(L'DEMCAT04),C'N',2X'00'                        
         DC    AL4(AFDEM04),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(THISDMER),AL1(L'THISDMER),C'T',2X'00' DEM LOOKUP ERR         
*                                                                               
         DC    X'FF'                                                            
*                                                                               
MRECTAB  DC    AL4(THISBEAM),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISIDX),AL1(26),C'X',2X'00'          HEX BUYKEY             
*                                                                               
         DC    AL4(DEMMOD01),AL1(L'DEMMOD01),C'T',C' ',X'00' RTG/IMP            
         DC    AL4(DEMCAT01),AL1(L'DEMCAT01),C'N',2X'00' CATEGORY NUM           
         DC    AL4(BFDEM01),AL1(4),C'B',X'00',X'02'      PURCH/ 2 DEC           
         DC    AL4(BFDEM01R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD02),AL1(L'DEMMOD02),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT02),AL1(L'DEMCAT02),C'N',2X'00'                        
         DC    AL4(BFDEM02),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM02R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD03),AL1(L'DEMMOD03),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT03),AL1(L'DEMCAT03),C'N',2X'00'                        
         DC    AL4(BFDEM03),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM03R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD04),AL1(L'DEMMOD04),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT04),AL1(L'DEMCAT04),C'N',2X'00'                        
         DC    AL4(BFDEM04),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM04R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD05),AL1(L'DEMMOD05),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT05),AL1(L'DEMCAT05),C'N',2X'00'                        
         DC    AL4(BFDEM05),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM05R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD06),AL1(L'DEMMOD06),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT06),AL1(L'DEMCAT06),C'N',2X'00'                        
         DC    AL4(BFDEM06),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM06R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD07),AL1(L'DEMMOD07),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT07),AL1(L'DEMCAT07),C'N',2X'00'                        
         DC    AL4(BFDEM07),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM07R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD08),AL1(L'DEMMOD08),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT08),AL1(L'DEMCAT08),C'N',2X'00'                        
         DC    AL4(BFDEM08),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM08R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD09),AL1(L'DEMMOD09),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT09),AL1(L'DEMCAT09),C'N',2X'00'                        
         DC    AL4(BFDEM09),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM09R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD10),AL1(L'DEMMOD10),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT10),AL1(L'DEMCAT10),C'N',2X'00'                        
         DC    AL4(BFDEM10),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM10R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD11),AL1(L'DEMMOD11),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT11),AL1(L'DEMCAT11),C'N',2X'00'                        
         DC    AL4(BFDEM11),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM11R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD12),AL1(L'DEMMOD12),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT12),AL1(L'DEMCAT12),C'N',2X'00'                        
         DC    AL4(BFDEM12),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM12R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD13),AL1(L'DEMMOD13),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT13),AL1(L'DEMCAT13),C'N',2X'00'                        
         DC    AL4(BFDEM13),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM13R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD14),AL1(L'DEMMOD14),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT14),AL1(L'DEMCAT14),C'N',2X'00'                        
         DC    AL4(BFDEM14),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM14R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD15),AL1(L'DEMMOD15),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT15),AL1(L'DEMCAT15),C'N',2X'00'                        
         DC    AL4(BFDEM15),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM15R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD16),AL1(L'DEMMOD16),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT16),AL1(L'DEMCAT16),C'N',2X'00'                        
         DC    AL4(BFDEM16),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM16R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD17),AL1(L'DEMMOD17),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT17),AL1(L'DEMCAT17),C'N',2X'00'                        
         DC    AL4(BFDEM17),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM17R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD18),AL1(L'DEMMOD18),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT18),AL1(L'DEMCAT18),C'N',2X'00'                        
         DC    AL4(BFDEM18),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM18R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD19),AL1(L'DEMMOD19),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT19),AL1(L'DEMCAT19),C'N',2X'00'                        
         DC    AL4(BFDEM19),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM19R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC    AL4(DEMMOD20),AL1(L'DEMMOD20),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT20),AL1(L'DEMCAT20),C'N',2X'00'                        
         DC    AL4(BFDEM20),AL1(4),C'B',X'00',X'02'                             
         DC    AL4(BFDEM20R),AL1(4),C'B',X'00',X'02'     RERATED                
*                                                                               
         DC   X'FF'                                                             
         EJECT                                                                  
         DC    CL8'GRECTAB*'                                                    
GRECTAB  DC    AL4(THISBEAG),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISIDGX),AL1(L'THISIDGX),C'X',2X'00'  (HEX FIELD)           
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(BFKQPRD),AL1(L'BFKQPRD),C'T',2X'00'                          
         DC    AL4(BFKQPRD2),AL1(L'BFKQPRD2),C'T',2X'00'                        
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
         DC    AL4(BFKDPTNM),AL1(L'BFKDPTNM),C'T',2X'00'                        
         DC    AL4(THISSLN),AL1(L'THISSLN),C'N',2X'00'                          
         DC    AL4(TIDPURP),AL1(L'TIDPURP),C'T',2X'00'                          
         DC    AL4(THISWEEK),AL1(L'THISWEEK),C'T',2X'00'                        
         DC    AL4(THISYYMM),AL1(L'THISYYMM),C'T',2X'00'                        
         DC    AL4(THISEQV),AL1(L'THISEQV),C'N',2X'00'                          
*                                                                               
         DC    AL4(BFDOL),AL1(4),C'B',X'00',X'02'   2 DEC                       
         DC    AL4(BFDOLEQ),AL1(4),C'B',X'00',X'02' 2 DEC                       
*                                                                               
         DC    AL4(DEMMOD01),AL1(L'DEMMOD01),C'T',C' ',X'00'  RTG/IMP           
         DC    AL4(DEMCAT01),AL1(L'DEMCAT01),C'N',X'00',X'00' CATG              
         DC    AL4(BFDEM01),AL1(4),C'B',X'00',X'01' 1 DEC                       
* GOAL LOCKIN VALUES                                                            
         DC    AL4(BFGKDOL),AL1(L'BFGKDOL),C'B',X'00',X'02'                     
         DC    AL4(BFGKDOLQ),AL1(L'BFGKDOLQ),C'B',X'00',X'02'                   
         DC    AL4(BFGKDEM1),AL1(L'BFGKDEM1),C'B',X'00',X'01' 1 DEC             
* MARKET LOCKIN VALUES                                                          
         DC    AL4(BFLKSPT),AL1(L'BFLKSPT),C'B',2X'00'                          
         DC    AL4(BFLKDOL),AL1(L'BFLKDOL),C'B',X'00',X'02'                     
         DC    AL4(BFLKDOLQ),AL1(L'BFLKDOLQ),C'B',X'00',X'02'                   
         DC    AL4(BFLKDEM1),AL1(L'BFLKDEM1),C'B',X'00',X'01' 1 DEC             
*                                                                               
         DC    AL4(BFKDPT),AL1(L'BFKDPT),C'T',2X'00'                            
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
         DC    CL8'IRECTAB*'       ISCI DATA                                    
IRECTAB  DC    AL4(THISBEAI),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(BFKQPRD),AL1(L'BFKQPRD),C'T',2X'00'                          
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISBUY),AL1(L'THISBUY),C'N',2X'00'                          
         DC    AL4(THISSPT),AL1(L'THISSPT),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
         DC    AL4(THISSTA),AL1(L'THISSTA),C'T',2X'00'                          
         DC    AL4(THISSPL),AL1(L'THISSPL),C'T',2X'00'                          
         DC    AL4(BFKDPTNM),AL1(L'BFKDPTNM),C'T',2X'00'                        
         DC    AL4(THISSLN),AL1(L'THISSLN),C'N',2X'00'                          
         DC    AL4(THISDAY),AL1(L'THISDAY),C'T',2X'00'                          
         DC    AL4(THISTIME),AL1(L'THISTIME),C'T',2X'00'                        
         DC    AL4(TIDPURP),AL1(L'TIDPURP),C'T',2X'00'                          
         DC    AL4(THISDATE),AL1(L'THISDATE),C'T',2X'00'                        
         DC    AL4(THISAFTM),AL1(L'THISAFTM),C'T',2X'00'                        
         DC    AL4(BFKPROG),AL1(L'BFKPROG),C'T',2X'00'                          
         DC    AL4(THISAFCM),AL1(L'THISAFCM),C'T',2X'00'                        
         DC    AL4(THISTRCM),AL1(L'THISTRCM),C'T',2X'00'                        
         DC    AL4(THISTRD1),AL1(L'THISTRD1),C'T',2X'00'                        
         DC    AL4(THISTRD2),AL1(L'THISTRD2),C'T',2X'00'                        
         DC    AL4(THISTRD3),AL1(L'THISTRD3),C'T',2X'00'                        
         DC    AL4(THISTRCL),AL1(L'THISTRCL),C'T',2X'00'                        
*                                                                               
         DC    AL4(BFDOL),AL1(4),C'B',X'00',X'02'   <== 2 DEC                   
         DC    AL4(BFTAX),AL1(4),C'B',X'00',X'02'   <== 2 DEC                   
*                                                                               
         DC    AL4(THISDM01),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM01),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM02),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM02),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM03),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM03),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM04),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM04),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM05),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM05),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM06),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM06),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM07),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM07),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM08),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM08),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM09),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM09),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM10),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM10),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
* I'M NOT REALLY SURE WHY THIS IS HERE!                                         
         DC    AL4(THISRTYP),AL1(L'THISRTYP),C'T',2X'00'                        
*                                                                               
         DC    AL4(BFKDPT),AL1(L'BFKDPT),C'T',2X'00'                            
*                                                                               
         DC    AL4(THISAFID),AL1(L'THISAFID),C'T',2X'00'                        
         DC    AL4(THISTRID),AL1(L'THISTRID),C'T',2X'00'                        
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*===============================================================                
* THIS IS USED FOR GOAL DATA WHEN DOING ISCI EXTRACTS.                          
* THE BINARY KEY IS DROPPED                                                     
* THE EQUIVALENCE FACTOR IS DROPPED                                             
* THE BROADCAST MONTH IS DROPPED                                                
* THE DEMO NAME IS SENT IN EACH RECORD                                          
*===============================================================                
                                                                                
         DC    CL8'LRECTAB'                                                     
LRECTAB  DC    AL4(THISBEAL),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(BFKQPRD),AL1(L'BFKQPRD),C'T',2X'00'                          
         DC    AL4(BFKQPRD2),AL1(L'BFKQPRD2),C'T',2X'00'                        
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
         DC    AL4(BFKDPTNM),AL1(L'BFKDPTNM),C'T',2X'00'                        
         DC    AL4(THISSLN),AL1(L'THISSLN),C'N',2X'00'                          
         DC    AL4(TIDGPURP),AL1(L'TIDGPURP),C'T',2X'00'                        
         DC    AL4(THISWEEK),AL1(L'THISWEEK),C'T',2X'00'                        
*                                                                               
         DC    AL4(BFDOL),AL1(4),C'B',X'00',X'02'   2 DEC                       
*                                                                               
         DC    AL4(THISDM01),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM01),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(BFKDPT),AL1(L'BFKDPT),C'T',2X'00'                            
* GOAL LOCKIN VALUES                                                            
         DC    AL4(BFGKDOL),AL1(L'BFGKDOL),C'B',X'00',X'02'                     
         DC    AL4(BFGKDOLQ),AL1(L'BFGKDOLQ),C'B',X'00',X'02'                   
         DC    AL4(BFGKDEM1),AL1(L'BFGKDEM1),C'B',X'00',X'01' 1 DEC             
* MARKET LOCKIN VALUES                                                          
         DC    AL4(BFLKSPT),AL1(L'BFLKSPT),C'B',2X'00'                          
         DC    AL4(BFLKDOL),AL1(L'BFLKDOL),C'B',X'00',X'02'                     
         DC    AL4(BFLKDOLQ),AL1(L'BFLKDOLQ),C'B',X'00',X'02'                   
         DC    AL4(BFLKDEM1),AL1(L'BFLKDEM1),C'B',X'00',X'01' 1 DEC             
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
         DC    CL8'RRECTAB*'  <=== RECEIVABLE (BILLED) RECORD                   
RRECTAB  DC    AL4(THISBEAR),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(BFFKQPRD),AL1(L'BFFKQPRD),C'T',2X'00'                        
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
         DC    AL4(THISSTA),AL1(L'THISSTA),C'T',2X'00'                          
         DC    AL4(THISYYMM),AL1(L'THISYYMM),C'T',2X'00'                        
*                                                                               
         DC    AL4(BFFSPOTS),AL1(4),C'B',X'00',X'00'                            
         DC    AL4(BFFGRS),AL1(4),C'B',X'00',X'02'                              
         DC    AL4(BFFNET),AL1(4),C'B',X'00',X'02'                              
         DC    AL4(BFFTAX),AL1(4),C'B',X'00',X'02'                              
*                                                                               
         DC   X'FF'                                                             
*                                                                               
         DC    CL8'PRECTAB*'  <=== PAYABLE RECORD                               
PRECTAB  DC    AL4(THISBEAP),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(BFFKQPRD),AL1(L'BFFKQPRD),C'T',2X'00'                        
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
         DC    AL4(THISSTA),AL1(L'THISSTA),C'T',2X'00'                          
         DC    AL4(THISYYMM),AL1(L'THISYYMM),C'T',2X'00'                        
*                                                                               
         DC    AL4(BFFSPOTS),AL1(4),C'B',X'00',X'00'                            
         DC    AL4(BFFGRS),AL1(4),C'B',X'00',X'02'                              
         DC    AL4(BFFNET),AL1(4),C'B',X'00',X'02'                              
         DC    AL4(BFFTAX),AL1(4),C'B',X'00',X'02'                              
*                                                                               
         DC   X'FF'                                                             
         EJECT                                                                  
*========================================================                       
* THESE TABLES TO KILL RECORDS FOR TURNAROUND MODE                              
* TABLES START WITH RECORD TYPE BYTE, THEN ACTION BYTE K=KILL,                  
* THEN 1 ENTRY FOR EACH COLUMN IN THE KEY TABLE                                 
*========================================================                       
         SPACE 1                                                                
KLLBTAB  DC    AL4(THISBEAE),AL1(1),C'T',2X'00'      RECORD TYPE                
         DC    AL4(THISBEAK),AL1(1),C'T',2X'00'      KILL RECORD                
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'  PRD=ALL               
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
* FOLLOWING SPACE FIELDS FILL OUT THE REQUIRED COLUMNS                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    X'FF'                                                            
         SPACE 1                                                                
KLLGTAB  DC    AL4(THISBEAH),AL1(1),C'T',2X'00'      RECORD TYPE                
         DC    AL4(THISBEAK),AL1(1),C'T',2X'00'      KILL RECORD                
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00' PRD=ALL                
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00' PRD2=ALL               
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
* FOLLOWING SPACE FIELDS FILL OUT THE REQUIRED COLUMNS                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    X'FF'                                                            
         SPACE 1                                                                
KLLITAB  DC    AL4(THISBEAJ),AL1(1),C'T',2X'00'      RECORD TYPE                
         DC    AL4(THISBEAK),AL1(1),C'T',2X'00'      KILL RECORD                
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'  PRD=ALL               
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISBUY),AL1(L'THISBUY),C'N',2X'00'                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'  SPOT=ALL              
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
         DC    AL4(THISSTA),AL1(L'THISSTA),C'T',2X'00'                          
* FOLLOWING SPACE FIELDS FILL OUT THE REQUIRED COLUMNS                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    X'FF'                                                            
         SPACE 1                                                                
KLLJTAB  DC    AL4(THISBEAX),AL1(1),C'T',2X'00'             THISBEAB            
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'      THISAGY             
         DC    AL4(THISIDX),AL1(26),C'X',2X'00'             THISIDX             
* FOLLOWING SPACE FIELDS FILL OUT THE REQUIRED COLUMNS                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISMED             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISCLT             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    POLPRD              
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISEST             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISMKT             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISSTA             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISBUY             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISORIG            
*                                                                               
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    BFKDPTNM            
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISSLN             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISDAY             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISTIME            
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    TIDPURP             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    BFKPROG             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISREP             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISADJ             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISRTYP            
*                                                                               
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    BFKDPT              
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISCRDT            
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISCHDT            
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISCHPD            
*                                                                               
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISBYID            
*                                                                               
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISNET4            
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISBKTP            
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'    THISORDR            
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
         DC    CL8'KLLFTAB*'  <=== KILL RECEIVABLE (BILLING) R RECS             
KLLRTAB  DC    AL4(THISBEAY),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISBEAK),AL1(1),C'T',2X'00'      KILL RECORD                
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(THISPRD),AL1(L'THISPRD),C'T',2X'00' ALL PRDS                 
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC   X'FF'                                                             
*                                                                               
         DC    CL8'KLLPTAB*'  <=== KILL PAYABLE (P) RECORDS                     
KLLPTAB  DC    AL4(THISBEAZ),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISBEAK),AL1(1),C'T',2X'00'      KILL RECORD                
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
KLLPSTA  DC    AL4(THISSTA),AL1(L'THISSTA),C'T',2X'00'                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC   X'FF'                                                             
         EJECT                                                                  
****************************************************************                
****************************************************************                
****************************************************************                
* CANADA-SPECIFIC TABLES                                                        
* THESE HAVE EXTRA FIELDS THAT US AGENCIES DON'T USE                            
****************************************************************                
****************************************************************                
****************************************************************                
*                                                                               
*===============================================================                
* IN FIXED LENGTH MODE, BINARY FIELDS ARE OUTPUT AT FIXED LENGTHS               
* 1 DECIMAL VALUES GO OUT AS 8 BYTES                                            
* 2 DECIMAL VALUES GO OUT AS 13 BYTES                                           
*===============================================================                
*                                                                               
         DS    0D                                                               
         DC    CL8'DRECTABC'                                                    
DRECTABC DC    AL4(THISBEAD),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISIDX),AL1(L'THISIDX),C'X',2X'00' (HEX FIELD)              
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(BFKQPRD),AL1(L'BFKQPRD),C'T',2X'00'                          
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
         DC    AL4(THISSTA),AL1(L'THISSTA),C'T',2X'00'                          
         DC    AL4(THISSPL),AL1(L'THISSPL),C'T',2X'00'                          
         DC    AL4(BFKDPTNM),AL1(L'BFKDPTNM),C'T',2X'00'                        
         DC    AL4(BFKDPT),AL1(L'BFKDPT),C'T',2X'00'                            
         DC    AL4(THISSLN),AL1(L'THISSLN),C'N',2X'00'                          
         DC    AL4(TIDPURP),AL1(L'TIDPURP),C'T',2X'00'                          
         DC    AL4(THISWEEK),AL1(L'THISWEEK),C'T',2X'00'                        
         DC    AL4(THISYYMM),AL1(L'THISYYMM),C'T',2X'00'                        
         DC    AL4(THISEQV),AL1(L'THISEQV),C'N',2X'00'                          
         DC    AL4(THISDMTY),AL1(L'THISDMTY),C'T',2X'00' RERATE TYPE            
*                                                                               
         DC    AL4(BFSPOTS),AL1(4),C'B',X'00',X'00'                             
         DC    AL4(BFDOL),AL1(4),C'B',X'00',X'02'   <== 2 DEC                   
         DC    AL4(BFDOLEQ),AL1(4),C'B',X'00',X'02' <== 2 DEC                   
         DC    AL4(BFTAX),AL1(4),C'B',X'00',X'02'   <== 2 DEC                   
*                                                                               
         DC    AL4(THISNET),AL1(L'THISNET),C'T',2X'00'                          
*                                                                               
         DC    AL4(DEMMOD01),AL1(L'DEMMOD01),C'T',C' ',X'00' RTG/IMP            
         DC    AL4(DEMCAT01),AL1(L'DEMCAT01),C'N',2X'00' CATEGORY NUM           
         DC    AL4(BFDEM01),AL1(4),C'B',X'00',X'01'      VALUE/ 1 DEC           
*                                                                               
         DC    AL4(DEMMOD02),AL1(L'DEMMOD02),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT02),AL1(L'DEMCAT02),C'N',2X'00'                        
         DC    AL4(BFDEM02),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD03),AL1(L'DEMMOD03),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT03),AL1(L'DEMCAT03),C'N',2X'00'                        
         DC    AL4(BFDEM03),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD04),AL1(L'DEMMOD04),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT04),AL1(L'DEMCAT04),C'N',2X'00'                        
         DC    AL4(BFDEM04),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD05),AL1(L'DEMMOD05),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT05),AL1(L'DEMCAT05),C'N',2X'00'                        
         DC    AL4(BFDEM05),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD06),AL1(L'DEMMOD06),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT06),AL1(L'DEMCAT06),C'N',2X'00'                        
         DC    AL4(BFDEM06),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD07),AL1(L'DEMMOD07),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT07),AL1(L'DEMCAT07),C'N',2X'00'                        
         DC    AL4(BFDEM07),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD08),AL1(L'DEMMOD08),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT08),AL1(L'DEMCAT08),C'N',2X'00'                        
         DC    AL4(BFDEM08),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD09),AL1(L'DEMMOD09),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT09),AL1(L'DEMCAT09),C'N',2X'00'                        
         DC    AL4(BFDEM09),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD10),AL1(L'DEMMOD10),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT10),AL1(L'DEMCAT10),C'N',2X'00'                        
         DC    AL4(BFDEM10),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD11),AL1(L'DEMMOD11),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT11),AL1(L'DEMCAT11),C'N',2X'00'                        
         DC    AL4(BFDEM11),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD12),AL1(L'DEMMOD12),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT12),AL1(L'DEMCAT12),C'N',2X'00'                        
         DC    AL4(BFDEM12),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD13),AL1(L'DEMMOD13),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT13),AL1(L'DEMCAT13),C'N',2X'00'                        
         DC    AL4(BFDEM13),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD14),AL1(L'DEMMOD14),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT14),AL1(L'DEMCAT14),C'N',2X'00'                        
         DC    AL4(BFDEM14),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD15),AL1(L'DEMMOD15),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT15),AL1(L'DEMCAT15),C'N',2X'00'                        
         DC    AL4(BFDEM15),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD16),AL1(L'DEMMOD16),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT16),AL1(L'DEMCAT16),C'N',2X'00'                        
         DC    AL4(BFDEM16),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD17),AL1(L'DEMMOD17),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT17),AL1(L'DEMCAT17),C'N',2X'00'                        
         DC    AL4(BFDEM17),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD18),AL1(L'DEMMOD18),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT18),AL1(L'DEMCAT18),C'N',2X'00'                        
         DC    AL4(BFDEM18),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD19),AL1(L'DEMMOD19),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT19),AL1(L'DEMCAT19),C'N',2X'00'                        
         DC    AL4(BFDEM19),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC    AL4(DEMMOD20),AL1(L'DEMMOD20),C'T',C' ',X'00'                    
         DC    AL4(DEMCAT20),AL1(L'DEMCAT20),C'N',2X'00'                        
         DC    AL4(BFDEM20),AL1(4),C'B',X'00',X'01'                             
*                                                                               
         DC   X'FF'                                                             
*                                                                               
*=================================================================              
* THIS RECORD SENT FOR BUYLINE RERATE VERSION OF DOWNLOAD                       
*=================================================================              
         DS    0D                                                               
         DC    CL8'BRECTABC'                                                    
BRECTABC DC    AL4(THISBEAB),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISIDX),AL1(26),C'X',2X'00'         HEX BUYKEY              
*                                                                               
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(POLPRD),AL1(L'POLPRD),C'T',2X'00'                            
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
         DC    AL4(THISSTA),AL1(L'THISSTA),C'T',2X'00'                          
         DC    AL4(THISBUY),AL1(L'THISBUY),C'N',2X'00'                          
         DC    AL4(THISORIG),AL1(L'THISORIG),C'N',2X'00' ORIGIN MKT             
*                                                                               
         DC    AL4(BFKDPTNM),AL1(L'BFKDPTNM),C'T',2X'00'                        
         DC    AL4(THISSLN),AL1(L'THISSLN),C'N',2X'00'                          
         DC    AL4(THISDAY),AL1(L'THISDAY),C'T',2X'00'                          
         DC    AL4(THISTIME),AL1(L'THISTIME),C'T',2X'00'                        
         DC    AL4(TIDPURP),AL1(L'TIDPURP),C'T',2X'00'                          
         DC    AL4(BFKPROG),AL1(L'BFKPROG),C'T',2X'00'                          
         DC    AL4(THISREP),AL1(L'THISREP),C'T',2X'00'                          
         DC    AL4(THISADJ),AL1(L'THISADJ),C'T',2X'00'                          
         DC    AL4(THISRTYP),AL1(L'THISRTYP),C'T',2X'00'                        
*                                                                               
         DC    AL4(BFKDPT),AL1(L'BFKDPT),C'T',2X'00'                            
*                                                                               
         DC    AL4(THISNET),AL1(L'THISNET),C'T',2X'00'                          
         DC    AL4(THISCRDT),AL1(L'THISCRDT),C'T',2X'00' CREATE DATE            
         DC    AL4(THISCHDT),AL1(L'THISCHDT),C'T',2X'00' CHANGE DATE            
         DC    AL4(THISCHPD),AL1(L'THISCHPD),C'T',2X'00' CHANGE PID             
*                                                                               
         DC    AL4(THISBYID),AL1(L'THISBYID),C'T',2X'00' BUY ID                 
*                                                                               
         DC    AL4(THISNET4),AL1(L'THISNET4),C'T',2X'00' 4CHAR NETWORK          
         DC    AL4(THISBKTP),AL1(L'THISBKTP),C'T',2X'00' BOOK TYPE              
         DC    AL4(THISORDR),AL1(26),C'X',2X'00'         HEX ORDER ID           
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         DC    CL8'IRECTABC'       ISCI DATA                                    
IRECTABC DC    AL4(THISBEAI),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(BFKQPRD),AL1(L'BFKQPRD),C'T',2X'00'                          
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISBUY),AL1(L'THISBUY),C'N',2X'00'                          
         DC    AL4(THISSPT),AL1(L'THISSPT),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
         DC    AL4(THISSTA),AL1(L'THISSTA),C'T',2X'00'                          
         DC    AL4(THISSPL),AL1(L'THISSPL),C'T',2X'00'                          
         DC    AL4(BFKDPTNM),AL1(L'BFKDPTNM),C'T',2X'00'                        
         DC    AL4(THISSLN),AL1(L'THISSLN),C'N',2X'00'                          
         DC    AL4(THISDAY),AL1(L'THISDAY),C'T',2X'00'                          
         DC    AL4(THISTIME),AL1(L'THISTIME),C'T',2X'00'                        
         DC    AL4(TIDPURP),AL1(L'TIDPURP),C'T',2X'00'                          
         DC    AL4(THISDATE),AL1(L'THISDATE),C'T',2X'00'                        
         DC    AL4(THISAFTM),AL1(L'THISAFTM),C'T',2X'00'                        
         DC    AL4(BFKPROG),AL1(L'BFKPROG),C'T',2X'00'                          
         DC    AL4(THISAFCM),AL1(L'THISAFCM),C'T',2X'00'                        
         DC    AL4(THISTRCM),AL1(L'THISTRCM),C'T',2X'00'                        
         DC    AL4(THISTRD1),AL1(L'THISTRD1),C'T',2X'00'                        
         DC    AL4(THISTRD2),AL1(L'THISTRD2),C'T',2X'00'                        
         DC    AL4(THISTRD3),AL1(L'THISTRD3),C'T',2X'00'                        
         DC    AL4(THISTRCL),AL1(L'THISTRCL),C'T',2X'00'                        
*                                                                               
         DC    AL4(BFDOL),AL1(4),C'B',X'00',X'02'   <== 2 DEC                   
         DC    AL4(BFTAX),AL1(4),C'B',X'00',X'02'   <== 2 DEC                   
*                                                                               
         DC    AL4(THISDM01),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM01),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM02),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM02),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM03),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM03),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM04),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM04),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM05),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM05),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM06),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM06),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM07),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM07),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM08),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM08),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM09),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM09),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
*                                                                               
         DC    AL4(THISDM10),AL1(6),C'T',X'00',X'00' DEMONAME                   
         DC    AL4(BFDEM10),AL1(4),C'B',X'00',X'01'  VALUE/ 1 DEC               
* I'M NOT REALLY SURE WHY THIS IS HERE!                                         
         DC    AL4(THISRTYP),AL1(L'THISRTYP),C'T',2X'00'                        
*                                                                               
         DC    AL4(BFKDPT),AL1(L'BFKDPT),C'T',2X'00'                            
*                                                                               
         DC    AL4(THISAFID),AL1(L'THISAFID),C'T',2X'00'                        
         DC    AL4(THISTRID),AL1(L'THISTRID),C'T',2X'00'                        
*                                                                               
         DC    AL4(THISNET),AL1(L'THISNET),C'T',2X'00'                          
*                                                                               
         DC    X'FF'                                                            
*                                                                               
*                                                                               
         DC    CL8'RRECTABC'  <=== RECEIVABLE (BILLED) RECORD                   
RRECTABC DC    AL4(THISBEAR),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(BFFKQPRD),AL1(L'BFFKQPRD),C'T',2X'00'                        
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
         DC    AL4(THISSTA),AL1(L'THISSTA),C'T',2X'00'                          
         DC    AL4(THISYYMM),AL1(L'THISYYMM),C'T',2X'00'                        
*                                                                               
         DC    AL4(BFFSPOTS),AL1(4),C'B',X'00',X'00'                            
         DC    AL4(BFFGRS),AL1(4),C'B',X'00',X'02'                              
         DC    AL4(BFFNET),AL1(4),C'B',X'00',X'02'                              
         DC    AL4(BFFTAX),AL1(4),C'B',X'00',X'02'                              
*                                                                               
         DC    AL4(THISNET),AL1(L'THISNET),C'T',2X'00'                          
*                                                                               
         DC   X'FF'                                                             
*                                                                               
         DC    CL8'PRECTABC'  <=== PAYABLE RECORD                               
PRECTABC DC    AL4(THISBEAP),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(BFFKQPRD),AL1(L'BFFKQPRD),C'T',2X'00'                        
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
         DC    AL4(THISSTA),AL1(L'THISSTA),C'T',2X'00'                          
         DC    AL4(THISYYMM),AL1(L'THISYYMM),C'T',2X'00'                        
*                                                                               
         DC    AL4(BFFSPOTS),AL1(4),C'B',X'00',X'00'                            
         DC    AL4(BFFGRS),AL1(4),C'B',X'00',X'02'                              
         DC    AL4(BFFNET),AL1(4),C'B',X'00',X'02'                              
         DC    AL4(BFFTAX),AL1(4),C'B',X'00',X'02'                              
*                                                                               
         DC    AL4(THISNET),AL1(L'THISNET),C'T',2X'00'                          
*                                                                               
         DC   X'FF'                                                             
*                                                                               
*                                                                               
*========================================================                       
* THESE TABLES TO KILL RECORDS FOR TURNAROUND MODE                              
* TABLES START WITH RECORD TYPE BYTE, THEN ACTION BYTE K=KILL,                  
* THEN 1 ENTRY FOR EACH COLUMN IN THE KEY TABLE                                 
*========================================================                       
*                                                                               
KLLBTABC DC    AL4(THISBEAE),AL1(1),C'T',2X'00'      RECORD TYPE                
         DC    AL4(THISBEAK),AL1(1),C'T',2X'00'      KILL RECORD                
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'  PRD=ALL               
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
* FOLLOWING SPACE FIELDS FILL OUT THE REQUIRED COLUMNS                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    X'FF'                                                            
*                                                                               
KLLJTABC DC    AL4(THISBEAX),AL1(1),C'T',2X'00'            THISBEAB             
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'     THISAGY              
         DC    AL4(THISIDX),AL1(26),C'X',2X'00'            THISIDX              
* FOLLOWING SPACE FIELDS FILL OUT THE REQUIRED COLUMNS                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISMED              
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISCLT              
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   POLPRD               
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISEST              
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISMKT              
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISSTA              
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISBUY              
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISORIG             
*                                                                               
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   BFKDPTNM             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISSLN              
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISDAY              
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISTIME             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   TIDPURP              
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   BFKPROG              
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISREP              
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISADJ              
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISRTYP             
*                                                                               
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   BFKDPT               
*                                                                               
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISNET              
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISCRDT             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISCHDT             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISCHPD             
*                                                                               
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISBYID             
*                                                                               
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISNET4             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISBKTP             
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'   THISORDR             
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         DC    CL8'KLLFTABC'  <=== KILL RECEIVABLE (BILLING) R RECS             
KLLRTABC DC    AL4(THISBEAY),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISBEAK),AL1(1),C'T',2X'00'      KILL RECORD                
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(THISPRD),AL1(L'THISPRD),C'T',2X'00' ALL PRDS                 
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC   X'FF'                                                             
*                                                                               
         DC    CL8'KLLPTABC'  <=== KILL PAYABLE (P) RECORDS                     
KLLPTABC DC    AL4(THISBEAZ),AL1(1),C'T',2X'00'                                 
         DC    AL4(THISBEAK),AL1(1),C'T',2X'00'      KILL RECORD                
         DC    AL4(THISAGY),AL1(L'THISAGY),C'T',2X'00'                          
         DC    AL4(THISMED),AL1(L'THISMED),C'T',2X'00'                          
         DC    AL4(THISCLT),AL1(L'THISCLT),C'T',2X'00'                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISEST),AL1(L'THISEST),C'N',2X'00'                          
         DC    AL4(THISMKT),AL1(L'THISMKT),C'N',2X'00'                          
KLLPSTAC DC    AL4(THISSTA),AL1(L'THISSTA),C'T',2X'00'                          
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
         DC    AL4(THISBLNK),AL1(L'THISBLNK),C'T',2X'00'                        
*                                                                               
         DC   X'FF'                                                             
*                                                                               
*                                                                               
BLDNET   NTR1  BASE=*,LABEL=*                                                   
         LARL  RE,NETTAB                                                        
         ICM   RF,15,=AL4(NETTABLQ)                                             
         XCEFL                                                                  
*                                                                               
         MVC   KEY1,KEY            SAVE CURRENT KEY                             
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING NDEFRECD,R2                                                      
         MVC   NDEFKTYP,=X'0D11'   RECORD TYPE                                  
         MVC   NDEFKAGY,QAGY       AGENCY                                       
         GOTO1 HIGH                                                             
         B     BLDN04B                                                          
*                                                                               
BLDN04   GOTO1 SEQ                                                              
*                                                                               
BLDN04B  DS    0H                                                               
         CLC   KEY(4),KEYSAVE      REC TYPE, QAGY                               
         BNE   BLDN20                                                           
         OC    NDEFKNET,NDEFKNET   AGENCY-LEVEL NETDEF?                         
         BZ    BLDN04              YES -- SKIP                                  
         CLC   NDEFKCLT,BCLT       THIS CLIENT OK                               
         BE    *+14                                                             
         OC    NDEFKCLT,NDEFKCLT   OR AGENCY DEFAULT                            
         BNZ   BLDN04                                                           
*                                                                               
         MVC   AREC,ADBUY          READ INTO ADBUY                              
         GOTO1 GET                                                              
         L     R3,AREC                                                          
         LA    R3,NDEFEL-NDEFRECD(R3)                                           
*                                                                               
BLDN08   DS    0H                                                               
         CLI   0(R3),0             EOR                                          
         BE    BLDN04              NEXT RECORD                                  
         CLI   0(R3),NDEFNELQ      02 ELEM HAS NETWORK NUMBER                   
         BE    BLDN09                                                           
*                                                                               
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     BLDN08                                                           
*                                                                               
BLDN09   DS    0H                                                               
         USING NDEFEL02,R3                                                      
         CLI   NDEFNET,MAXNETS     TOO MANY NETWORKS?                           
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LLC   R4,NDEFNET          NETWORK NUMBER                               
         MHI   R4,NETTABL                                                       
         A     R4,=A(NETTAB)                                                    
         DROP  R3                                                               
*                                                                               
         OC    0(NETTABL,R4),0(R4)  IF WE ALREADY HAVE AN ENTRY FOR             
         BNZ   BLDN04               THIS NET #, USE IT.                         
*                                                                               
         USING NETTABD,R4                                                       
         MVC   NTBCALL,NDEFKNET    SET NETWORK CODE                             
         B     BLDN04              NEXT RECORD                                  
         DROP  R4                                                               
*                                                                               
BLDN20   DS    0H                                                               
         MVC   KEY,KEY1                                                         
         GOTO1 HIGH                RESTORE SEQ                                  
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
*================================================================               
* GET CANADIAN NETWORK FROM PACKED STA                                          
* R1 EXPECTED TO ADDRESS PACKED STATON                                          
* ON EXIT WORK(4) HAS THE NETWORK CALL LETTERS                                  
*================================================================               
                                                                                
GETCNET  NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(4),=C'    '    INIT WORK(4) TO SPACES                       
         LLC   R0,2(R1)            CANADIAN NETWORK BYTE                        
*                                                                               
         LTR   R0,R0                                                            
         JM    NEQXIT              ERROR IF NEGATIVE                            
         CHI   R0,MAXNETS                                                       
         JH    NEQXIT              ERROR IF HIGHER THAN MAXNETS                 
*                                                                               
         MHI   R0,NETTABL                                                       
         LARL  RF,NETTAB                                                        
         AR    RF,R0                                                            
         CLC   0(4,RF),SPACES      ANYTHING AT THAT POSITION?                   
         JNH   NEQXIT                                                           
         MVC   WORK(4),0(RF)                                                    
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* HEXMED - CONVERTS BETWEEN A/M BYTE AND MEDIA CHARACTER                        
*                                                                               
* ON ENTRY: R1 HOB = A/M BYTE OR MEDIA CHARACTER                                
*           R1 = A(OUTPUT BYTE)                                                 
*           UNEQUAL CONDITION CODE SET IF INVALID INPUT                         
***********************************************************************         
                                                                                
HEX2MED  LR    RF,R1                                                            
         SRL   RF,24               RIGHT ALIGN A/M                              
         LA    R0,15               X'0000000F'                                  
         NR    RF,R0               DROP AGY                                     
         LARL  R0,MDTAB                                                         
         AR    RF,R0                                                            
         MVC   0(1,R1),0(RF)                                                    
         CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
MDTAB    DC    C' TRNX   C'                                                     
*                                                                               
*                                                                               
ISCANADA L     RF,ADAGY                                                         
         CLI   AGYPROF+7-AGYHDR(RF),C'C'  ONLY FOR CANADIAN AGY                 
         BR    RE                                                               
*                                                                               
*                                                                               
SETATAB  NTR1  BASE=*,LABEL=*                                                   
* DEFAULT (US) TABLES                                                           
         LAY   RF,RRECTAB                                                       
         ST    RF,ARRECTAB                                                      
         LAY   RF,IRECTAB                                                       
         ST    RF,AIRECTAB                                                      
         LAY   RF,BRECTAB                                                       
         ST    RF,ABRECTAB                                                      
         LAY   RF,MRECTAB                                                       
         ST    RF,AMRECTAB                                                      
         LAY   RF,SRECTAB                                                       
         ST    RF,ASRECTAB                                                      
         LAY   RF,TRECTAB                                                       
         ST    RF,ATRECTAB                                                      
         LAY   RF,DRECTAB                                                       
         ST    RF,ADRECTAB                                                      
         LAY   RF,PRECTAB                                                       
         ST    RF,APRECTAB                                                      
         LAY   RF,GRECTAB                                                       
         ST    RF,AGRECTAB                                                      
         LAY   RF,LRECTAB                                                       
         ST    RF,ALRECTAB                                                      
*                                                                               
         LAY   RF,KLLGTAB                                                       
         ST    RF,AKLLGTAB                                                      
         LAY   RF,KLLBTAB                                                       
         ST    RF,AKLLBTAB                                                      
         LAY   RF,KLLRTAB                                                       
         ST    RF,AKLLRTAB                                                      
         LAY   RF,KLLPTAB                                                       
         ST    RF,AKLLPTAB                                                      
         LAY   RF,KLLITAB                                                       
         ST    RF,AKLLITAB                                                      
         LAY   RF,KLLJTAB                                                       
         ST    RF,AKLLJTAB                                                      
*                                                                               
         BRAS  RE,ISCANADA                                                      
         BNE   SETAX                                                            
*                                                                               
* CANADA-SPECIFIC TABLES                                                        
         LAY   RF,RRECTABC                                                      
         ST    RF,ARRECTAB                                                      
         LAY   RF,IRECTABC                                                      
         ST    RF,AIRECTAB                                                      
         LAY   RF,BRECTABC                                                      
         ST    RF,ABRECTAB                                                      
         LAY   RF,DRECTABC                                                      
         ST    RF,ADRECTAB                                                      
         LAY   RF,PRECTABC                                                      
         ST    RF,APRECTAB                                                      
*                                                                               
         LAY   RF,KLLBTABC                                                      
         ST    RF,AKLLBTAB                                                      
         LAY   RF,KLLRTABC                                                      
         ST    RF,AKLLRTAB                                                      
         LAY   RF,KLLPTABC                                                      
         ST    RF,AKLLPTAB                                                      
         LAY   RF,KLLJTABC                                                      
         ST    RF,AKLLJTAB                                                      
*                                                                               
SETAX    J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         DS    0D                                                               
         DC    CL8'*NETTAB*'                                                    
NETTAB   DS    0D                                                               
         DS    CL(MAXNETS*NETTABL)                                              
         DC    X'00'                                                            
NETTABLQ EQU   *-NETTAB                                                         
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*        TRANSLATE PID TO A NAME                                      *         
*        ON ENTRY:                                                    *         
*        R1 A(PID)                                                    *         
*        8-CHARACTER PID RETURNED IN WORK                             *         
***********************************************************************         
                                                                                
TRNPID   NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK,SPACES        INIT OUTPUT                                   
*                                                                               
         LA    R6,IOAREA                                                        
         USING SA0REC,R6           ESTABLISH KEY AS PERSON AUTH REC             
         XC    SA0KEY,SA0KEY       INIT KEY                                     
*                                                                               
         MVI   SA0KTYP,SA0KTYPQ    SET RECORD TYPE                              
         L     RF,VMASTC                                                        
         MVC   SA0KAGY,MCAGYSEC-MASTD(RF)                                       
         CLC   SA0KAGY,SPACES                                                   
         BH    *+10                                                             
         MVC   SA0KAGY,QAGY                                                     
         MVC   SA0KNUM,0(R1)       SET PID                                      
*                                                                               
         MVC   WORK(L'SA0KEY),0(R6)                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,=CL7'CTFILE',SA0KEY,SA0REC,0                 
*                                                                               
         CLC   SA0KEY,WORK         SKIP IF RECORD NOT FOUND                     
         JNE   NEQXIT                                                           
*                                                                               
*        FIND PERSON'S ID ELEMENT                                               
*                                                                               
         LA    R6,SA0DATA          POINT TO FIRST ELEMENT                       
*                                                                               
TRNPID10 CLI   0(R6),0             CHECK FOR END OF RECORD                      
         JE    NEQXIT                                                           
*                                                                               
         CLI   0(R6),SAPALELQ      X'C3' PERSONAL ID ELEMENT                    
         JNE   *+14                                                             
         MVC   WORK(L'SAPALPID),SAPALPID-SAPALD(R6)                             
         J     EQXIT                                                            
*                                                                               
         LLC   R0,1(R6)            GET ELEMENT LENGTH                           
         LTR   R0,R0                                                            
         JE    NEQXIT                                                           
         AR    R6,R0               BUMP TO NEXT ELEMENT                         
         J     TRNPID10            GO FIND NEXT ELEMENT                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* BUILD TABLE OF 0D29 RECORDS IN 31-BIT STORAGE                                 
* AND SET BINSRCH PARMS                                                         
* TO TRANSLATE NON-TRADITIONAL DEMO NAMES TO THEIR SEQNUMS                      
*============================================================                   
*                                                                               
BLDD29   NTR1  BASE=*,LABEL=*                                                   
         L     R4,D29PAR2           GET ADDRESS OF TABLE                        
         LHI   R5,D29TABMAX                                                     
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING NTDRECD,R6                                                       
         MVI   NTDKTYP,NTDKTYPQ                                                 
         MVI   NTDKSUB,NTDKSUBQ                                                 
         MVI   NTDKRTGSV,C'C'      SET FOR COMSCORE                             
         MVC   NTDKAGMD,BAGYMD                                                  
         GOTO1 HIGH                                                             
         J     BLDD29B                                                          
*                                                                               
BLDD29A  GOTO1 SEQ                                                              
*                                                                               
BLDD29B  CLC   KEY(4),KEYSAVE                                                   
         JNE   BLDD29X                                                          
*                                                                               
         SAM31                                                                  
         MVC   0(5,R4),NTDKALPH    MOVE ALPHA                                   
         MVC   5(2,R4),NTDKSEQ                                                  
         LA    R4,7(R4)                                                         
         SAM24                                                                  
         JCT   R5,BLDD29A                                                       
         DC    H'0'                TABLE FULL!                                  
*                                                                               
BLDD29X  LHI   R0,D29TABMAX                                                     
         SR    R0,R5                                                            
         ST    R0,D29COUNT                                                      
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
ZERODEMO NTR1  BASE=*,LABEL=*                                                   
         L     R6,ADBUY                                                         
         OC    4(2,R6),4(R6)       IS IT A MARKET 0 BUY                         
         JZ    NEQXIT              ALWAYS LOOKUP                                
*                                                                               
         AHI   R6,24                                                            
         MVI   ELCDLO,X'02'         FIND DEMO ELEMENT                           
         MVI   ELCDHI,X'02'                                                     
         BRAS  RE,NEXTEL                                                        
         JNE   EQXIT               NO DEMO ELEMENT                              
*                                                                               
         LLC   RF,1(R6)            ELEMENT LENGTH                               
         SHI   RF,24                                                            
*                                                                               
         LTR   RF,RF                                                            
         JZ    EQXIT                                                            
*                                                                               
         XR    RE,RE                                                            
         D     RE,=F'8'                                                         
         LTR   RE,RE               RE=REMAINDER, RF=NUMBER OF DEMOS             
         JNZ   NEQXIT              REMAINDER BETTER BE ZERO                     
*                                                                               
         AHI   R6,24               ADVANCE TO DEMOS                             
*                                                                               
ZEROD10  DS    0H                                                               
         MVC   FULL,4(R6)          DEMO VALUE                                   
         NI    FULL,X'3F'          TURN OFF 2 HI ORDER BITS                     
         OC    FULL,FULL                                                        
         JNZ   NEQXIT                                                           
         AHI   R6,8                                                             
         BCT   RF,ZEROD10                                                       
                                                                                
* NOW CHECK FOR ANY POST-BUY DEMOS - ANY PRESENCE --> LOOKUP                    
                                                                                
         L     R6,ADBUY                                                         
         AHI   R6,24                                                            
         MVI   ELCDLO,X'22'                                                     
         MVI   ELCDHI,X'23'                                                     
         BRAS  RE,NEXTEL                                                        
         JE    NEQXIT                                                           
                                                                                
* NOW CHECK NETSTTAB - TABLE OF STATIONS BOUGHT                                 
                                                                                
         L     R6,ADBUY                                                         
         AHI   R6,24                                                            
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BRAS  RE,NEXTEL                                                        
         JNE   *+2                                                              
*                                                                               
         LA    R4,DUB                                                           
         USING NETSTAD,R4                                                       
                                                                                
* FIRST SEE IF THERE ARE ANY ESTIMATE LEVEL RECS FOR THIS NETWORK               
                                                                                
         XC    DUB,DUB                                                          
         MVC   NETSTNET,2(R6)   MOVE NETWORK CALL LETTERS                       
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         MVC   NETSTEST,BUYKEST                                                 
*                                                                               
         LARL  R1,NETSTAP1              POINT TO BINSRCH PARMS                  
         MVI   NETSTAP4-NETSTAP0(R1),2  SET TO READ HIGH                        
         GOTO1 VBINSR31,(R1),DUB         FOR NET/EST                            
         TM    0(R1),X'80'              TEST NOT FOUND                          
         JO    ZEROD20                                                          
         L     RE,0(R1)                                                         
         CLC   0(5,RE),DUB              DID WE FIND NET/EST                     
         JE    *+8                      YES - GO READ FOR STATION               
ZEROD20  MVI   NETSTEST,0               NOT FOUND - CLEAR EST                   
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         MVC   NETSTSTA,BUYKSTA                                                 
*                                                                               
         CLI   BUYKSTA+2,X'B0'  TEST CABLE STATION                              
         JL    *+14                                                             
         MVI   NETSTSTA,0                                                       
         MVC   NETSTSTA+1(1),BUYKSTA+2                                          
         DROP  R6                                                               
*                                                                               
         LARL  R1,NETSTAP1              POINT TO BINSRCH PARMS                  
         MVI   NETSTAP4-NETSTAP0(R1),2  SET TO READ HIGH                        
         GOTO1 VBINSR31,(R1),DUB                                                
         TM    0(R1),X'80'         TEST NOT FOUND                               
         JO    EQXIT               NOT IN TABLE - NO LOOKUP                     
         L     RE,0(R1)                                                         
         CLC   0(8,RE),DUB         DID WE FIND RECORD                           
         JNE   EQXIT                                                            
         J     NEQXIT                                                           
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* BUILD TABLE OF NETWORK STATIONS BOUGHT FOR THIS CLIENT                        
*                                                                               
* NET CALL LETTERS (4)                                                          
* ESTIMATE         (1)                                                          
* STATION          (2)                                                          
* SPARE            (1)                                                          
*===============================================================                
                                                                                
BLDSTNET NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   KEY1,KEY            SAVE CURRENT KEY                             
* SET UP FOR STAPACK CALLS                                                      
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'P'                                                     
         MVC   STAPMED,QMED                                                     
         MVC   STAPQMKT,=C'0000'                                                
         MVC   STAPAGY,AGY                                                      
         MVI   STAPCTRY,C'C'                                                    
         MVC   STAPACOM,ACOMFACS                                                
         DROP  R1                                                               
*                                                                               
         XC    NETSTAP3,NETSTAP3   CLEAR COUNT OF RECORDS IN TABLE              
         SR    R0,R0                                                            
         BCTR  R0,0                                                             
         ST    R0,DUB                                                           
         ST    R0,DUB+4                                                         
         LARL  R1,NETSTAP1              POINT TO BINSRCH PARMS                  
         MVI   NETSTAP4-NETSTAP0(R1),1  SET TO ADD                              
         GOTO1 VBINSR31,(R1),DUB         REC WITH FFFFF...                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING NDEFRECD,R2                                                      
         MVC   NDEFKTYP,=X'0D11'   RECORD TYPE                                  
         MVC   NDEFKAGY,QAGY       AGENCY                                       
         GOTO1 HIGH                                                             
         J     BLDST04                                                          
*                                                                               
BLDST02  GOTO1 SEQ                                                              
*                                                                               
BLDST04  DS    0H                                                               
         CLC   KEY(4),KEYSAVE      0D11/QAGY                                    
         JNE   BLDSTX                                                           
         OC    NDEFKNET,NDEFKNET   TEST NO NETWORK                              
         JZ    BLDST02                                                          
         CLC   NDEFKCLT,BCLT       FILTER ON CLIENT                             
         JE    *+14                                                             
         OC    NDEFKCLT,NDEFKCLT   OR AGENCY DEFAULT                            
         JNZ   BLDST02                                                          
*                                                                               
         MVC   AREC,ADBUY          READ INTO ADBUY                              
         GOTO1 GET                                                              
         L     R3,AREC                                                          
         LA    R3,NDEFEL-NDEFRECD(R3)                                           
*                                                                               
BLDST10  CLI   0(R3),0             EOR                                          
         JE    BLDST02             NEXT RECORD                                  
         CLI   0(R3),NDEFNELQ      02 ELEM HAS NETWORK NUMBER                   
         JE    BLDST12                                                          
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         J     BLDST10                                                          
*                                                                               
         USING NDEFEL02,R3                                                      
BLDST12  MVC   BYTE,NDEFNET        SAVE NETWORK NUMBER (01=CABLE)               
*                                                                               
         L     R3,AREC                                                          
         LA    R3,NDEFEL-NDEFRECD(R3)                                           
*                                                                               
         USING NDEFEL01,R3                                                      
BLDST20  CLI   0(R3),1                                                          
         JNE   BLDST30                                                          
         CLC   NDEFPCT,=F'-1'      TEST NOT BOUGHT                              
         JE    BLDST30                                                          
         OC    NDEFPCT,NDEFPCT     OR 0%                                        
         JZ    BLDST30                                                          
                                                                                
* ELSE ADD TO TABLE OF STATIONS BOUGHT                                          
                                                                                
         XC    DUB,DUB                                                          
         LA    R4,DUB                                                           
         USING NETSTAD,R4                                                       
         MVC   NETSTNET,NDEFKNET                                                
         MVC   NETSTEST,NDEFKEST                                                
*                                                                               
         LA    R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
*                                                                               
         MVC   STAPQSTA,NDEFSTA                                                 
         MVC   STAPQNET,SPACES                                                  
*                                                                               
         CLI   BYTE,1              TEST CABLE                                   
         JNE   BLDST22             NO                                           
         MVC   STAPQSTA,NDEFKNET   CABLE USES CALL LETTERS/PROV CODE            
         MVI   STAPQSTA+4,C' '                                                  
         MVC   STAPQNET(2),NDEFMSUF                                             
*                                                                               
BLDST22  GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         JE    BLDST24                                                          
         J     BLDST24             TO ALLOW PATCH TO DIE ON ERROR               
*                                                                               
BLDST24  MVC   NETSTSTA,STAPSTA    SAVE 2-BYTE STATION                          
         CLI   BYTE,1              TEST CABLE                                   
         JNE   *+14                                                             
         MVI   NETSTSTA,0                                                       
         MVC   NETSTSTA+1(1),STAPSTA+2 SAVE BINARY PROVINCE CODE                
         DROP  R1                                                               
*                                                                               
         LARL  R1,NETSTAP1              POINT TO BINSRCH PARMS                  
         MVI   NETSTAP4-NETSTAP0(R1),1  SET TO ADD TO TABLE                     
         GOTO1 VBINSR31,(R1),DUB         IF NOT FOUND                           
*                                                                               
BLDST30  LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         JE    BLDST02                                                          
         J     BLDST20                                                          
         DROP  R4                                                               
*                                                                               
BLDSTX   DS    0H                                                               
         MVC   KEY,KEY1                                                         
         GOTO1 HIGH                RESTORE SEQ                                  
*                                                                               
         XIT1                                                                   
*                                                                               
NETSTAP0 DS    0A                                                               
NETSTAP1 DS    A                                                                
NETSTAP2 DC    A(NETSTTAB)         A(TABLE)                                     
NETSTAP3 DC    F'0'                NUMBER OF ENTRIES                            
NETSTAP4 DC    A(8)                RECORD LENGTH                                
NETSTAP5 DC    AL1(0),AL3(8)       DSPL OF KEY/L'KEY                            
NETSTAP6 DC    A((NETSTTBX-NETSTTAB)/8)  MAX ENTRIES                            
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
* LOOK AT THE BUY'S DEMO VALUES                                                 
* FOR MANUAL OVERRIDES, MAKE SURE THE VALUE IS NO LARGER THAN                   
* THE HARD-CODED MAX VALUE                                                      
* VERY HIGH IMPRESSION VALUES AND LOW SPOT COUNTS CAUSE PROBLEMS                
* IN SPGETDEME - DIVIDE INSTRUCTION FAILS WITH S0C9                             
HIGHDEMO NTR1  BASE=*,LABEL=*                                                   
         L     R6,ADBUY                                                         
         AHI   R6,24                                                            
         MVI   ELCDLO,NDCORGQ       X'02' DEMOGRAPHIC ELEMENT                   
         MVI   ELCDHI,NDCORGQ                                                   
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   EQXIT               NO X'02' FOUND = NO OVERRIDES, GOOD          
*                                                                               
         LLC   RF,NDLEN-NDELEM(R6)                                              
         LA    R7,0(R6,RF)         R7 - END OF ELEMENT                          
         LHI   RF,NDEMNO-NDELEM                                                 
         LA    R6,0(R6,RF)         R6 - FIRST DEMO                              
*                                                                               
HIGHDM10 DS    0H                                                               
         CR    R6,R7                                                            
         JNL   EQXIT               EOL, NO OVERRIDES FOUND, GOOD                
*                                                                               
         USING NDEMNO,R6                                                        
         TM    NDEMRAW,NDEMMANQ    MANUAL OVERRIDE?                             
         BZ    HIGHDM20                                                         
*                                                                               
         MVC   FULL,NDEMRAW        DEMO VALUE                                   
         NI    FULL,X'FF'-NDEMMANQ-NDEM2DEC TURN OFF FLAGS                      
*                                                                               
MAXOVER  EQU   100000              MAX VALUE FOR MANUAL OVERRIDE                
*                                                                               
         LA    RF,=AL4(MAXOVER*10) 100K WITH 1-DEC PRECISION                    
         TM    NDEMRAW,NDEM2DEC                                                 
         BZ    *+8                                                              
         LA    RF,=AL4(MAXOVER*100) 100K WITH 2-DEC PRECISION                   
         CLC   FULL,0(RF)                                                       
*                                                                               
         JH    NEQXIT              HIGH OVERRIDE FOUND - BAD                    
*                                                                               
HIGHDM20 DS    0H                                                               
         LA    R6,8(R6)                                                         
         B     HIGHDM10                                                         
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
NETSTAD  DSECT                     NETWORK STATION TABLE DSECT                  
NETSTNET DS    XL4                 NETWORK CALL LETTERS                         
NETSTEST DS    XL1                 ESTIMATE NUMBER                              
NETSTSTA DS    XL2                 2-BYTE PACKED STA OR X'00/CBLSEQNUM          
         DS    XL1                 ALIGN FOR 8-BYTE ENTRIES                     
                                                                                
NETTABD  DSECT                     DSECT FOR CANADIAN NETWORK TABLE             
NTBCALL  DS    CL4                                                              
NETTABL  EQU   *-NETTABD                                                        
MAXNETS  EQU   255                                                              
*                                                                               
D29RECD  DSECT                                                                  
D29DEMNM DS    CL5                                                              
D29DEMSQ DS    XL2                                                              
D29RECL  EQU   *-D29RECD                                                        
*                                                                               
SPDB02   CSECT                                                                  
         DS    0D                                                               
         DC    CL8'*PRBLPER'                                                    
PRBLPER  DS    304X                                                             
PRBLPERX EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'POLNTDEM'                                                    
POLNTDEM DS    20XL9               20 X DEMONAME(7)/DEMO SEQNUM(2)              
*                                                                               
         DS    0D                                                               
         DC    CL8'NETSTTAB'                                                    
NETSTTAB DS    100000XL8                                                        
NETSTTBX EQU   *                                                                
*                                                                               
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
*                                                                               
       ++INCLUDE SPGENSTAB                                                      
*                                                                               
       ++INCLUDE SPGENNTDEM                                                     
*                                                                               
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE DDDLCB                                                         
*                                                                               
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
*                                                                               
PMED     DS    CL1                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PMKT     DS    CL4                                                              
         DS    CL1                                                              
PSTA     DS    CL5                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL1                                                              
PCOST    DS    CL8                                                              
         DS    CL1                                                              
PDATE    DS    CL6                                                              
         DS    CL1                                                              
PTIME    DS    CL6                                                              
         DS    CL1                                                              
         ORG   PDATE                                                            
PSPOTS   DS    CL3                                                              
         ORG                                                                    
                                                                                
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPREPPTBUF                                                     
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPGENNDEF                                                      
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGETBUYD                                                      
*                                                                               
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
*                                                                               
*                                                                               
SPWORKD  DSECT                                                                  
         ORG   Q2USER                                                           
Q2CLTST  DS    CL3                 CLIENT CODE RANGE START LETTER               
Q2CLTEND DS    CL3                 CLIENT CODE RANGE END LETTER                 
         ORG                                                                    
*                                                                               
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
RECVHDRD DSECT                                                                  
*PREFIX=DM$                                                                     
       ++INCLUDE DMRCVRHDR                                                      
*PREFIX=                                                                        
*                                                                               
       ++INCLUDE SEACSFILE                                                      
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DEDEMTABD                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'143SPREPDB02 10/21/20'                                      
         END                                                                    
