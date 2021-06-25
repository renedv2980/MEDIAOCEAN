*          DATA SET SPREPSC02  AT LEVEL 110 AS OF 02/18/21                      
*PHASE SPSC02A                                                                  
*INCLUDE SPLDCPTR                                                               
*INCLUDE NSIWEEK                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         SPACE                                                                  
* QOPT1 = 'Y' MEANS FIX INVOICE RECORDS                                         
* QOPT3 = 'Y' MEANS SUPPORT BAD SSC REVERSAL VIA RECVIN-RECOVERY TAPE           
* QOPT4 = 'Y' MEANS THE WUNDERMAN TRACE IS ON                                   
* QOPT5 = 'Y' MEANS THE INTERNAL TRACE IS ON                                    
         TITLE 'STATION CALL LETTER CHANGE PROGRAM - APPLICATION'               
***********************************************************************         
* USER LVL DATE     JIRA       CHANGE LOG                             *         
* ---- --- -----    ---------- -------------------------------------- *         
* AKAT 109 05/16/19 SPEC-35842 MAX BUY REC LEN IS 5972 - NOT 5975     *         
* HWON 110 ???????? SPEC-25164 SUPPORT US-SBTK WORK/REVISION RECORDS  *         
* HWON 110 ???????? SPEC-17719 SUPPORT SSC REVERSAL                   *         
*                                                                     *         
***********************************************************************         
* LEV  2-4  APR02/90   FIX MEDIA N AND X - RETURNED 00000 STA                   
*                       FROM MSPACK                                             
* LEV 26-29 OCT17/90   BYPASS DEL BUYS, MED C FIX, NETNIBBLE FIX                
* LEV 30    JUL21/93   ADD CABLE HEAD                                           
* LEV 31    NOV17/93   ADD TRFUPD UPDATE RECORDS FOR TRAFFIC FILE     *         
* LEV 32-33 JAN25/94   CHANGE TRFUPD AND FIX CANADIAN NEWSTA BUG      *         
* LEV 34    FEB15/94   ADD STRAFFIC                                   *         
* LEV 35    FEB24/94   FIX STRAFFIC                                   *         
* LEV 36    FEB25/94   ADD DOUBLE BOOKING RECORD (0D7B)               *         
* LEV 37    FEB27/94   FIX TRAFFIC STATION                            *         
* LEV 38    JAN31/95   FIX MSPACK FOR CANADA                          *         
* LEV 39    MAR14/95   SHOW UP TO 99999 FOR RECORD COUNTS             *         
* LEV 40-41 SEP29/95   ADD 9D PREV STATION ELEM                       *         
* LEV 42    OCT12/95   ADD NEW INVOICING                              *         
* LEV 43    OCT26/95   FIX DOUBLE BOOKING RDHI ERROR                  *         
* LEV 44    JAN17/96   NEW INVOICES, CK FOR DELETED RECS              *         
* LEV 45    SEP17/96   FIX FOR CALL LETTERS XXXX-L                    *         
* LEV 46    JAN17/97   ADD RECORDS CHANGED DESC                       *         
* LEV 47    FEB04/97   ADD SPGENWIPW 0D7A RECORD, PULL WUNDERMAN      *         
* LEV 49    AUG13/97   FIX WILA PROFIT WITHIN                         *         
* LEV 50    NOV12/97   ADD SPOT DARE ORDER RECS                       *         
* LEV 51    FEB05/98   CHANGE CANADIAN NET FROM 5 TO 8 BITS           *         
* LEV 52    FEB09/98   MORE SPACE FOR SPBUFF - 4000 TO 8000           *         
* LEV 53    FEB20/98   FIX CLT SPECIFIC DAR ORDER BUG                 *         
* LEV 54    FEB23/98   FIX DAR ORDERS RUNNING AWAY                    *         
* LEV 55    JUL14/98   FIX DAR ORDERS DELETED KEY                     *         
* LEV 56    AUG07/98   ADD NWS CODE                                   *         
* LEV 57    DEC10/98   SHOW MEL'S NEW MEDIA COPIED BUYS               *         
* LEV 58    MAY08/00   COPY WI STATION LOCKIN RECORDS                 *         
* LEV 59    SEP28/00   COPY WI STATION LOCKIN RECORDS (XSPFIL)        *         
* LEV 60    OCT25/00   READ DELETED DAR RECORDS                       *         
* LEV 61    NOV03/00   FIX NEW LOCKIN RECS LOOP                       *         
* LEV 62    FEB02/01   FIX BAD LOCKIN RECS KILL LENGTH                *         
* LEV 72    MAY05/04   COMMENT OUT UID CODE (AKAT)                    *         
* LEV 77    OCTO5/04   FIX CLT SPECIFIC DAR ORDER CMMT BUG            *         
* LEV 78    OCT12/04   ADD NEW 0D6E (STAFIX IN SFM) RECS (AKAT)       *         
* LEV 79    MAY26/05   ADD SPGENORHIS 0D1B RECORDS (AKAT)             *         
* LEV 82    JAN17/06   3975 IS MAX RECORD LENGTH FOR RECUP ADD        *         
* LEV 88    DEC27/06   SEND EMAIL WHEN STAFIX MESSAGE IS GENERATED    *         
* LEV 91    FEB01/07   UPDATE ALL DARE PASSIVE KEYS AS D/A CHANGED    *         
*                      DON'T MOVE DELETED MAKEGOODS                   *         
* LEV 96    JUL05/07   TURN OFF ADDED BY DESKTOP FLAG WHEN ADDING BUYS*         
* LEV 98    SEP21/09   ERROR MESSAGE ENHANCEMENTS                     *         
* LEV 101   SEP15/10   1) ADD NEW DTM RECS                            *         
*                      2) FIX SID CABLE BUG                           *         
*                      3) FIX INSTRUCTION RECAP CABLE BUG             *         
*                      4) FIX NETWORK BUG IN ORDER HISTORY RECORDS    *         
* LEV 102   JUL14/11   2-BYTE BUYLINE SUPPORT                         *         
* LEV 103   SEP17/12   SUPPORT NEW BANDS D AND S                      *         
* LEV 104   NOV20/12   FIX DTM DEMO REC MOVED TO WRONG LINE#          *         
* LEV 105   JAN03/13   ONLY TEST STATIONS THAT PACK LOW FOR MEDIA C   *         
* LEV 107   SEP18/13 1-CLEAR PURGE BIT ON DELETE AND RESTORE IT ON ADD*         
*                    2-REPLACE @DDS.NET W @MEDIAOCEAN.COM             *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
* R E C O R D S  C H A N G E D                                        *         
***********************************************************************         
* TYPE OF RECORD          FILE KEY   ROUTINE                          *         
* **************          **** ***   *******                          *         
* SPOT BUYS                          SP200                            *         
* OLD INVOICE RECORDS     SPOT 0B    SP400                            *         
* NEW INVOICE RECORDS     XSP  0E03  SNINV                            *         
* STATION BUCKET RECORDS  SPOT 0E01  YOU MIGHT HAVE THOUGHT, BUT NO   *         
* NSID RECORDS            SPOT 0C    SP500   (DEFUNCT)                *         
* DARE ORDER RECORDS      SPOT 0D34  DARORD                           *         
* DARE BATCH RECORDS      SPOT 0D35  DARBATCH                         *         
* DESTINATION RECORDS     SPOT 0D3D  SP6A00                           *         
* LAST METHOD RECORDS     SPOT 0D3E  SP6B00                           *         
* SID RECORDS             SPOT 0D59  SP600   (DEFUNCT)                *         
* WILA STATION LOCKIN REC SPOT 0D72  PW200   (DEFUNCT)                *         
* WILA STATION LOCKIN REC XSP  0D73  PW300   (DEFUNCT)                *         
* WILA PROFIT WITHIN      SPOT 0D7A  PW100   (DEFUNCT)                *         
* DOUBLE BOOKING RECORDS  SPOT 0D7B  DB100   (DEFUNCT)                *         
* TRAFFIC INST RECAP      STR  0A24  SP700                            *         
* TRAFFIC SHIP RECAP      STR  0A25  SP800                            *         
* TRAFFIC BUY ACT         STR  0A2E  SP900                            *         
* TRAFFIC BUY             STR  0A32  SPA00                            *         
* TRAFFIC LABEL LIST      STR  0A2F  SPB00                            *         
* TRAFFIC STATION LIST    STR  0A31  SPC00                            *         
* TRAFFIC PATTERN REC     STR  0A22  SPD00                            *         
* TRAFFIC STATION REC     STR  0A28  SPE00                            *         
* ORDER HISTORY RECORDS   XSP  0D1B  SPG00                            *         
* CN-DTM CAMPAIGN STALIST XSP  0D03  SPDTCS                           *         
* CN-DTM PROGRAM RECORDS  XSP  0D05  SPDTP                            *         
* CN-DTM DEMO RECORDS     XSP  0D06  SPDTD                            *         
* CN-DTM ORDER RECORDS    XSP  0D07  SPDTO                            *         
* US-SBTK REVISION RECORD XSP  0E10  SPBTRV  (NOOPED)                 *         
* US-SBTK WORK RECORD     XSP  0E11  SPBTWK  (NOOPED)                 *         
*                                                                     *         
***********************************************************************         
         SPACE                                                                  
SPSC02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPSC02,R7,R3                                                   
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         SPACE 2                                                                
* CONTROL SECTION *                                                             
         SPACE                                                                  
         CLI   MODE,PROCBUY                                                     
         BE    SP200               UPDATE THE BUY SPTDIR RECORDS                
         CLI   MODE,CLTFRST                                                     
         BE    SP050               SKIP A LINE                                  
         CLI   MODE,REQFRST                                                     
         BE    SP000               GET THE NEW STATION CODE                     
         CLI   MODE,REQLAST                                                     
         BE    SP400               UPDATE THE INVOICE RECORDS                   
         CLI   MODE,RUNFRST                                                     
         BNE   EXIT                                                             
*                                                                               
         L     RE,SSB                                                           
         USING SSBD,RE                                                          
         OI    SSOSTAT2,SSOSROLC   RECOVER OFFLINE COPIES/CHANGES               
         MVI   SSORPRG,C'S'        SET PROGRAM ID FOR RCVRHDR                   
         DROP  RE              *** ALL LETTERS SET IN FASSBOFF ***              
*                                                                               
         LA    R0,SPSCHDHK                                                      
         ST    R0,HEADHOOK                                                      
         STM   R9,RB,SPSCR9                                                     
         ST    R7,SPSCR7                                                        
         ST    R3,SPSCR3                                                        
*                                                                               
         XC    BKTPARMS(24),BKTPARMS  SETUP BINSRCH PARAMETERS                  
         GOTOR ,BKTPARMS,,A(BUYTABLE),0,BUYTBLQ,L'BTBUYKEY,A(MAXBUYS)           
*                                                                               
         BRAS  RE,BRSTRTAB         BUILD TABLE OF ADD KEYS TO RESTORE           
*                                   RECS DUE TO BAD CALL LETTER SWITCH          
YES      CR    RB,RB                                                            
         J     EXIT                                                             
NO       LTR   RB,RB                                                            
EXIT     XIT1                                                                   
*                                                                               
         SPACE                                                                  
         DS    0D                                                               
PATCH1   DS    CL16                                                             
PATCH2   DS    CL16                                                             
*                                                                               
         EJECT                                                                  
* THIS SECTION GETS THE NEW CALL LETTERS *                                      
*  FROM QBOOK1 AND SAVES IT IN NEWCALL   *                                      
         SPACE                                                                  
SP000    DS    0H                                                               
*                                                                               
* CHECK WHETHER TO DISALLOW CALL LETTER CHANGE                                  
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,CHKCAN                                                        
         CLI   QMED,C'T'                                                        
         BNE   SP010                                                            
         CLI   QBOOK1+4,C'L'       LOW POWER STATION?                           
         BE    SP010                                                            
         CLI   QBOOK1+4,C'D'       BAND D?                                      
         BE    SP010               YES                                          
         MVI   QBOOK1+4,C'T'                                                    
SP010    DS    0H                                                               
         CLI   QMED,C'N'                                                        
         BNE   SP014                                                            
         MVI   QBOOK1+4,C'N'                                                    
SP014    DS    0H                                                               
         MVC   NEWCALL,QBOOK1      GET NEW CALL LETTERS                         
         SPACE                                                                  
         MVI   BUYSW,C'N'                                                       
         MVI   CANAGY,C'N'                                                      
         MVI   RQDEL,C'Y'          PROCESS DELETED BUY RECORDS                  
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   RCSUBPRG,1          SET BUY HEADER                               
         XC    SAVDA,SAVDA         CLEAR SAVED DISK ADDR                        
         XC    COUNT,COUNT                                                      
         CLI   QMED,C'N'                                                        
         BNE   *+8                                                              
         MVI   QSTA+4,C'N'                                                      
         CLI   QMED,C'X'                                                        
         BNE   *+8                                                              
         MVI   QSTA+4,C'X'                                                      
         CLI   QMED,C'T'                                                        
         BNE   SP020                                                            
         CLI   QSTA+4,C'L'         LOW POWER STATION?                           
         BE    SP020                                                            
         CLI   QSTA+4,C'D'         BAND D?                                      
         BE    SP020               YES                                          
         MVI   QSTA+4,C'T'                                                      
         SPACE                                                                  
SP020    DS    0H                                                               
         LLC   R0,QMED                                                          
         CLI   QMED,C'C'                                                        
         BNE   *+12                                                             
         MVI   QMED,C'T'                                                        
         MVI   QSTA+4,C'T'                                                      
*                                                                               
         GOTO1 MSPACK,DMCB,QMKT,QSTA,OLDMSTA                                    
*                                                                               
         STC   R0,QMED                                                          
         CLI   QMED,C'C'                                                        
         BNE   SP024                                                            
         STC   R0,QSTA+4                                                        
         OC    OLDMSTA+2(2),OLDMSTA+2                                           
         BNZ   SP024                                                            
         TM    OLDMSTA+4,X'E0'                                                  
         JZ    *+2                                                              
SP024    DS    0H                                                               
         CLI   QMED,C'N'                                                        
         BNE   *+8                                                              
         MVI   NEWCALL+4,C'N'                                                   
         SPACE                                                                  
         CLI   QMED,C'X'                                                        
         BNE   *+8                                                              
         MVI   NEWCALL+4,C'X'                                                   
         SPACE                                                                  
         CLI   QMED,C'T'                                                        
         BNE   SP025                                                            
         CLI   NEWCALL+4,C'L'      LOW POWER STATION?                           
         BE    SP025                                                            
         CLI   NEWCALL+4,C'D'      BAND D?                                      
         BE    SP025               YES                                          
         MVI   NEWCALL+4,C'T'                                                   
         SPACE                                                                  
SP025    CLI   QMED,C'C'           COMBINED                                     
         BNE   *+8                                                              
         MVI   NEWCALL+4,C'T'                                                   
         SPACE                                                                  
         GOTO1 (RF),(R1),QMKT,NEWCALL,NEWMSTA                                   
         CLI   QMED,C'C'           CANADIAN AGENCY MEDIA C REQUEST?             
         BNE   SP026               NO                                           
         OC    NEWMSTA+2(2),NEWMSTA+2                                           
         BNZ   SP026                                                            
         TM    NEWMSTA+4,X'E0'                                                  
         JZ    *+2                                                              
         SPACE                                                                  
SP026    CLC   OLDMSTA,NEWMSTA                                                  
         BE    STAEQERR                                                         
         XC    SAVCLT,SAVCLT                                                    
         CLC   QCLT,=C'ALL'        IS THIS CLIENT SPECIFIC REQ?                 
         BE    SP030                NO.                                         
         GOTO1 CLPACK,DMCB,QCLT,SAVCLT                                          
SP030    DS    0H                                                               
***                                                                             
*  WE'RE CHECKING IF WE HAVE A STATION FIX FROM THIS WEEK THAT HAS THE          
*  THE NEW STATION FROM THIS REQUEST AS THE OLD STATION THE STAFIX REC          
***                                                                             
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STAFXRCD,R4                                                      
         MVI   STFKTYP,STFKTYQ     (X'0D') RECORD TYPE                          
         MVI   STFKSBTY,STFKSBTQ   (X'6E') SUBRECORD TYPE                       
         MVC   STFKAGMD,BAGYMD     AGENCY/MEDIA                                 
         GOTO1 HIGH                                                             
         B     SP030B                                                           
*                                                                               
SP030A   GOTO1 SEQ                                                              
*                                                                               
SP030B   CLC   KEY(3),KEYSAVE      STAFIX FOR THIS A/M?                         
         BNE   SP030C              NO, DONE                                     
*                                                                               
         CLC   QCLT,=CL3'ALL'      IS THIS AN ALL CLIENT REQUEST?               
         BE    *+10                YES                                          
         CLC   STFKCLI,=X'FFFF'    IS THIS STAFIX REC FOR ALL CLIENTS?          
         BE    *+14                YES                                          
         CLC   STFKCLI,SAVCLT      CLIENT CODES MATCH?                          
         BNE   SP030A              NO, READ SEQ                                 
*                                                                               
         CLC   STFKOSTA,NEWSTA     OLD STA (STAFIX)==NEW STA (THIS REQ)         
         BNE   SP030A              NO, READ SEQ                                 
*                                                                               
         CLC   STFKMKT,NEWMKT      OLD MKT (STAFIX)==NEW MKT (THIS REQ)         
         BNE   SP030A              NO, READ SEQ                                 
         DROP  R4                                                               
*                                                                               
         MVC   AREC,=A(SPBUFF)                                                  
         GOTO1 GET                                                              
*                                                                               
         L     R6,AREC                                                          
         AHI   R6,STFFRST-STFKEY   POINT TO FIRST ELEMENT                       
*                                                                               
STAF01   LLC   R1,1(R6)                                                         
         AR    R1,R6               R1 POINTS TO NEXT ELEM (IF EXISTS)           
         CLI   0(R1),STFIDELQ      HAVE MORE DETAILS ELEMENTS (X'10')?          
         BNE   STAF02              NO                                           
         LR    R6,R1                                                            
         B     STAF01                                                           
*                                                                               
         USING STFIDELD,R6                                                      
STAF02   MVI   GETDAY,1            MAKE SUNDAY FIRST DAY OF WEEK                
*                                                                               
         GOTO1 DATCON,DMCB,(2,STFIDDAT),(0,EDATE)   SET YYMMDD DATE             
         GOTO1 =V(NSIWEEK),DMCB,EDATE,GETDAY,ADDAY,DATCON                       
         MVC   WEEK1(1),0(R1)      WEEK NUMBER IN BINARY FOR PREV REQ           
         MVC   WEEK1+1(1),4(R1)    YEAR NUMBER IN BINARY FOR PREV REQ           
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,EDATE)   SET THE EBCDIC DATE           
         GOTO1 =V(NSIWEEK),DMCB,EDATE,GETDAY,ADDAY,DATCON                       
         MVC   WEEK2(1),0(R1)      WEEK NUMBER IN BINARY FOR TODAY              
         MVC   WEEK2+1(1),4(R1)    YEAR NUMBER IN BINARY FOR TODAY              
*                                                                               
         CLC   WEEK1,WEEK2         ARE THEY BOTH IN THE SAME WEEK?              
         BNE   SP030A              NO, READ SEQ                                 
         DROP  R6                                                               
*                                                                               
         BRAS  RE,EMAIL            SEND AN E-MAIL!                              
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P+10(16),=C'STATION FIX FOR '                                    
         MVC   P+26(5),NEWCALL                                                  
         MVC   P+31(43),=C' (AS THE OLD STATION) ALREADY RAN THIS WEEK'         
         MVC   P+74(31),=C'. PLEASE REQUEST THIS NEXT WEEK'                     
         J     ERRORX                                                           
*                                                                               
SP030C   DS    0H                                                               
****************************** ATTENTION ******************************         
*                      UID CODE IS COMMENTED OUT                      *         
* * * * * * * * * * * * * * *  ATTENTION  * * * * * * * * * * * * * * *         
*                      UID CODE IS COMMENTED OUT                      *         
* * * * * * * * * * * * * * *  ATTENTION  * * * * * * * * * * * * * * *         
*                      UID CODE IS COMMENTED OUT                      *         
****************************** ATTENTION ******************************         
*&&DO                                                                           
*                                                                               
* CHECK MASTER RECORD UIDS                                                      
*                                                                               
         CLI   QMED,C'R'                                                        
         BNE   SP031                                                            
         L     R6,ADAGY                                                         
         MVI   ELCODE,X'01'                                                     
         MVC   DATADISP,=AL2(24)                                                
         BRAS  RE,GETEL                                                         
         JNE   *+2                 NO "01" ELEMENT FOUND                        
         USING AGYEL,R6                                                         
         TM    AGYFLAG2,AGYFLAG2_UID                                            
         BNO   SP031                                                            
         DROP  R6                                                               
*                                                                               
         XC    SKEY,SKEY                                                        
         LA    R2,SKEY                                                          
         USING STAREC,R2                                                        
         MVI   STAKTYPE,STAKTYPQ                                                
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,NEWCALL    CALL LETTERS                                 
         MVC   STAKAGY,QAGY        AGY                                          
         MVC   STAKCLT,=CL3'000'                                                
         MVC   STAKFILL,=CL3'000'                                               
         MVC   SVSTKEY,SKEY                                                     
         L     R2,ADSTAT                                                        
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',SKEY,(R2),0                  
         CLC   SVSTKEY,0(R2)                                                    
         JNE   *+2                                                              
         MVC   NEWUID,STUNIQID                                                  
*                                                                               
         XC    SKEY,SKEY                                                        
         LA    R2,SVSTKEY                                                       
         MVC   STAKCALL,QSTA                                                    
         MVC   SKEY(L'SVSTKEY),SVSTKEY                                          
         L     R2,ADSTAT                                                        
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',SKEY,(R2),0                  
         CLC   SVSTKEY,0(R2)                                                    
         JNE   *+2                                                              
         MVC   OLDUID,STUNIQID                                                  
         DROP  R2                                                               
*                                  UPDATE OLD MASTER RECORD'S UNIQUE ID         
         XC    CURUID,CURUID                                                    
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CT9ARECD,R2                                                      
         MVI   CT9AKTYP,CT9AKTYQ                                                
         MVI   CT9AKMED,C'R'                                                    
         MVC   CT9AKCLL,QSTA            STA CALL LETTERS                        
         L     R2,ADBUY                                                         
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,(R2)                      
         CLC   0(CT9AKDTE-CT9AKEY,R2),KEY     SAME KEY?                         
         BNE   SP030A05            NO CURUID TO COMPARE, CHECK OLDUID           
         MVC   CURUID,CT9AUID                                                   
         DROP  R2                                                               
*                                                                               
*****  WE NEED TO CHECK THE CURUID VS OLDUID FIRST!!                            
         CLC   CURUID,OLDUID                                                    
         BE    SP030A10             - YES, SAME, CONTINUE NORMALLY              
*                                                                               
*****  CURUID != OLDUID, SO WE NEED TO SEE IF OLD = NEW !!                      
SP030A05 OC    OLDUID(L'OLDUID*2),OLDUID                                        
         BZ    SP031                                                            
         CLC   NEWUID,OLDUID                                                    
         BNE   SP030D               - NOT THE SAME, BRANCH TO ERROR             
         OC    CURUID,CURUID       DID WE GET A X'9A' RECORD?                   
         BNZ   *+10                 - YES WE DID                                
         MVC   CURUID,OLDUID                                                    
*                                                                               
SP030A10 L     R2,ADSTAT           WE CONTINUE NORMALLY HERE                    
         USING STAREC,R2                                                        
         MVC   STUNIQID,CURUID                                                  
         DROP  R2                                                               
*                                                                               
         CLI   RCWRITE,C'N'        SKIP DATAMGR WRITE IF WRITE=NO               
         BE    SP031                                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'STATION',(R2),(R2)                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     SP031                                                            
*                                                                               
SP030D   DS    0H                                                               
         MVC   P,SPACES                                                         
         MVC   P(50),=CL50'ERROR: STATIONS MUST HAVE SAME UNIQUE IDS'           
         GOTO1 REPORT                                                           
         MVC   P+10(20),=CL20'STATION   UID'                                    
         GOTO1 REPORT                                                           
         MVC   P+10(25),=CL25'-------   ------'                                 
         GOTO1 REPORT                                                           
         MVC   P(3),=C'OLD'                                                     
         MVC   P+10(5),QSTA                                                     
         MVC   P+25(6),OLDUID                                                   
         GOTO1 REPORT                                                           
         MVC   P(3),=C'NEW'                                                     
         MVC   P+10(5),NEWCALL                                                  
         MVC   P+25(6),NEWUID                                                   
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*&&                                                                             
SP031    DS    0H                                                               
         XC    SAVEKEY(40),SAVEKEY                                              
         LA    R5,NCOUNTS          CLEAR COUNTERS                               
         LA    R4,COUNTS                                                        
*                                                                               
SP032    ZAP   0(4,R4),=P'0'                                                    
         LA    R4,L'COUNTS(R4)                                                  
         BCT   R5,SP032                                                         
         SPACE                                                                  
         L     R1,ADAGY                                                         
         USING AGYHDRD,R1                                                       
         CLI   AGYPROF+7,C'C'                                                   
         BNE   SP040                                                            
         MVI   CANAGY,C'Y'         FLAG AS CANADIAN AGENCY REQUEST              
         SPACE                                                                  
SP040    MVC   KEY1,KEY            SAVE CURRENT KEY                             
         B     EXIT                                                             
         DROP  R1                                                               
         SPACE                                                                  
* CONTROL BREAK FOR NEW CLIENT - SKIP LINE *                                    
         SPACE                                                                  
SP050    CLI   CLTBUYSW,C'Y'                                                    
         BNE   SP060                                                            
         XC    P,P                                                              
         GOTO1 REPORT                                                           
         MVI   CLTBUYSW,C'N'       SET CLT PROCESSED SWITCH OFF                 
SP060    B     EXIT                                                             
         EJECT                                                                  
* SP200 - THIS SECTION REPLACES THE CALL LETTERS *                              
*  IN AMERICAN BUYS WITH THE NEW CALL LETTERS    *                              
         SPACE                                                                  
SP200    DS    0H                                                               
         SPACE                                                                  
         MVI   DMOUTBTS,X'FD'                                                   
         MVI   BUYSW,C'Y'          SET THE 'BUY PROCESSED' SWITCH               
         MVI   CLTBUYSW,C'Y'       SET THE CLT 'PROCESSED' SWITCH               
         CLI   CANAGY,C'Y'         ARE WE DEALING WITH A CANADIAN AGY?          
         BE    SP220                YES                                         
SP210    DS    0H                                                               
         CLI   KEY+10,0                                                         
         BE    SP230               ACTIVE POINTER                               
         MVI   TRACECDE,C'1'                                                    
         BAS   RE,SPDEL            DELETE THE PASSIVE POINTER                   
         B     EXIT                                                             
***                                                                             
* SP220 - THIS SECTION REPLACES THE CALL LETTERS *                              
*  IN COMBINED TV BUYS WITH THE NEW CALL LETTERS *                              
***                                                                             
SP220    MVC   DUB(1),KEY          A/M                                          
         NI    DUB,X'0F'           STRIP THE AGENCY                             
         CLI   DUB,8               IS THIS A COMBINED BUY?                      
         BNE   SP210               NO                                           
***      CLI   KEY+11,0            TEST IS WRONG FOR 2 BYTE BUYLINES!           
***      BNE   SP230               ACTIVE POINTER                               
         CLI   KEY+3,X'FF'         POL POINTER?                                 
         BNE   *+12                NO - DELETE                                  
         TM    KEY+10,X'80'        YES - SPILL POINTER?                         
         BZ    SP230               NO - PROCESS                                 
         MVC   SAVEKEY2(17),KEY                                                 
*                                                                               
         MVI   TRACECDE,C'2'                                                    
         BAS   RE,SPDEL            DELETE THE PASSIVE POINTERS                  
*                                                                               
         MVC   DUB(1),KEY          RESTORE ORIGINAL MEDIA IN KEY                
         NI    DUB,X'F0'                                                        
         OC    DUB(1),KEY+10                                                    
         MVC   KEY(1),DUB                                                       
         MVI   KEY+10,X'FF'                                                     
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         JNE   *+2                                                              
*                                                                               
         MVI   TRACECDE,C'3'                                                    
         BAS   RE,SPDEL            DELETE THE PASSIVE POINTERS                  
*                                                                               
         MVC   KEY(13),SAVEKEY2                                                 
         OI    DMINBTS,X'08'       PASS BACK THE DELETED RECORDS                
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEYSAVE(13),KEY                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
* THIS SECTION REPLACES THE ACTIVE POINTERS *                                   
         SPACE                                                                  
SP230    DS    0H                                                               
         CLC   SAVEKEY(10),KEY     1ST TIME FOR THIS CLT/PRD/EST?               
         BE    SP235                NO.                                         
         GOTO1 =A(HIGHBUY)         CALCULATE THE HIGHEST BUY LINE               
*                                                                               
SP235    DS    0H                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   *+12                                                             
         MVI   TRACECDE,C'6'                                                    
         BAS   RE,SPTRACE          *** TRACE ***                                
*                                                                               
         CLI   USRSW1,C'Y'        MORE THAN MAX BUYS FOR THIS C/P/E/M/S         
         BE    EXIT                 YES.                                        
         MVC   SAVEKEY(13),KEY                                                  
*                                                                               
SP240    DS    0H                                                               
         GOTO1 GETBUY                                                           
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
         SPACE                                                                  
         TM    BUYRCNTL,X'80'      THIS A DELETED BUY                           
         BO    EXIT                 YES, BYPASS                                 
*                                                                               
         GOTOR CRSTRTAB,DMCB,(C'S',ADBUY)  KEY IN RESTORE TABLE?                
         BNE   EXIT                  NO, BYPASS                                 
*                                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   *+12                                                             
         MVI   TRACECDE,C'7'                                                    
         BAS   RE,SPTRACE          *** TRACE ***                                
*                                                                               
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   SP250                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,ADBUY,DMWORK                  
         TM    DM3,X'FD'                                                        
         JNZ   *+2                 DATAMGR ERROR                                
*                                                                               
SP250    CLI   CANAGY,C'Y'                                                      
         BNE   SP260                                                            
         GOTO1 =A(SPCNETWK)        FIX THE CANADIAN NETWORK ELEMENT             
*                                                                               
SP260    OC    NSHL,NSHL           HAVE BUYLINE FOR NEW STA?                    
         BZ    SP280               NO - NO NEED TO CORRECT BUY LINE             
         LLC   R5,10(R8)           GET THE BUY LINE NUMBER                      
         LLC   R6,NSHL+1           HIGHEST BUYLINE FOR NEW STATION              
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BNE   SP265               NO                                           
         ICM   R5,3,10(R8)         YES - 2 BYTE BUYLINE                         
         ICM   R6,3,NSHL           YES - HIGHEST LINE FOR NEW STA               
*                                                                               
SP265    AR    R5,R6               NEW BUYLINE NUMBER                           
*                                                                               
         XC    WORK,WORK                                                        
BT       USING BUYTBLD,WORK                                                     
         MVC   BT.BTBUYKEY,0(R8)                                                
         STC   R5,10(R8)           REPLACE WITH NEW BUY LINE NUMBER             
         MVC   BT.BTNEWBLN+1(1),10(R8)                                          
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BNE   *+14                NO                                           
         STCM  R5,3,10(R8)         YES - REPLACE WITH NEW BUY LINE NUM          
         MVC   BT.BTNEWBLN,10(R8)                                               
         DROP  BT                                                               
*                                                                               
         GOTOR BINSRCH,BKTPARMS,(X'01',WORK) ADD TO BINSRCH                     
         OC    0(4,R1),0(R1)       TABLE FULL?                                  
         JZ    *+2                  YES, DO NOT CONTINUE                        
*                                                                               
         BAS   RE,SPPKG            FIX THE PACKAGE ELEMENT, IF ANY              
***                                                                             
* THIS SECTION IS COMMON TO BOTH AMERICAN AND CANADIAN STATION FIX *            
***                                                                             
SP280    XC    WORK,WORK           ADD STA CALL LETTER CHANGE ELEM              
         LA    R6,WORK                                                          
         USING SFXELEM,R6                                                       
         MVI   SFXCODE,SFXCODEQ                                                 
         MVI   SFXLEN,SFXLENQ                                                   
         MVC   SFXSTA,6(R8)                                                     
         MVC   SFXDATE,TODAYP                                                   
         DROP  R6                                                               
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,13(R8)                                                      
         LR    RF,RE               SEE IF ROOM IN REC FOR ELEM                  
         AHI   RF,SFXLENQ                                                       
***      CH    RF,=H'5975'         MAX REC LENGTH                               
         CH    RF,=H'5972'         MAX REC LENGTH                               
         BNL   SP300                                                            
         LA    R5,0(R8,RE)                                                      
         GOTO1 RECUP,DMCB,(R8),(R6),(R5)                                        
*                                                                               
SP300    MVC   NETNIBLE,8(R8)      SAVE MEDIA OR CANADIAN NETWORK               
         MVC   6(3,R8),NEWSTA      SET THE NEW STATION CODE                     
         CLI   QSTA,C'0'           THIS A CABLE STATION                         
         BL    SP301               NO                                           
         NI    8(R8),X'80'         SET OFF ANY UNWANTED                         
         NI    NETNIBLE,X'7F'      DROP CABLE HEAD                              
         OC    8(1,R8),NETNIBLE    RESTORE CABLE HEAD NETWORK                   
         B     SP302                                                            
*                                                                               
SP301    CLI   CANAGY,C'Y'         ARE WE DEALING WITH A CANADIAN AGY?          
         BNE   SP302               NO                                           
         MVC   DUB(1),0(R8)                                                     
         NI    DUB,X'0F'                                                        
         CLI   DUB,3               NETWORK                                      
         BNE   SP302                                                            
***                                                                             
* CANADIAN NETWORK IS NOW 8, NOT 5 BITS                                         
***                                                                             
         NI    8(R8),X'00'         SET OFF ANY BUMMER BITS                      
         OC    8(1,R8),NETNIBLE    RESTORE CANADIAN NETWORK                     
*                                                                               
SP302    NI    15(R8),X'7F'        'UNSET' THE 'DELETED' FLAG                   
         CLI   QOPT5,C'Y'                                                       
         BNE   SP305                                                            
         MVC   KEY+6(3),6(R8)      MOVE IN MODIFIED NEWSTA                      
         MVI   TRACECDE,C'8'                                                    
         BAS   RE,SPTRACE          *** TRACE ***                                
*                                                                               
SP305    NI    BDSTAT3-BUYREC(R8),X'FF'-BDST3_DSKADD                            
         LH    R1,COUNT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,COUNT            # RECORDS CHANGED                            
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   SP310                NO                                          
         GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,ADBUY,DMWORK                  
         CLI   DM3,0                                                            
         JNE   *+2                 DATAMGR ERROR                                
*                                                                               
SP310    GOTO1 =V(LDCPTR),DMCB,ADBUY,A(SPBUFF),0,0                              
         L     R5,=A(SPBUFF)                                                    
         BAS   RE,SPPRINT                                                       
         MVI   USRSW2,C'Y'                                                      
         LA    R5,18(R5)                                                        
         CLI   0(R5),0                                                          
         BE    SP380               DONE                                         
         CLI   BUYKPRD,X'FF'       POL BUY?                                     
         BE    SP320                                                            
         CLI   BDTIME,0            TEST FOR PIGGYBACK                           
         BE    SP380                NO.                                         
         LA    R5,18(R5)           A PASSIVE POINTER WAS ALREADY ADDED          
         B     SP330                                                            
*                                                                               
SP320    CLI   BDMASPRD,0          IS THERE A POL MASTER PRODUCT CODE           
         BE    SP330                NO.                                         
         CLC   3(1,R5),BDMASPRD    IS THIS THE POOL MASTER PRD CODE?            
         BNE   SP330                NO.                                         
         LA    R5,18(R5)                                                        
         CLI   0(R5),0                                                          
         BE    SP380                                                            
         CLI   BDMASPRD+1,0        ARE THERE 2 POOL MASTER PRD CODES?           
         BE    SP330                NO.                                         
         CLC   3(1,R5),BDMASPRD+1  IS THIS THE 2ND POOL MASTER PRD CODE         
         BNE   SP330                NO.                                         
         LA    R5,18(R5)                                                        
*                                                                               
SP330    CLI   0(R5),0                                                          
         BE    SP380                                                            
*                                                                               
SP340    MVC   KEY(13),0(R5)                                                    
         CLI   QOPT5,C'Y'                                                       
         BNE   SP350                                                            
         MVI   TRACECDE,C'9'                                                    
         BAS   RE,SPTRACE          *** TRACE ***                                
*                                                                               
SP350    CLI   RCWRITE,C'Y'                                                     
         BNE   SP370                                                            
         CLC   =X'FF00',KEY+10                                                  
         BNE   SP360                                                            
         CLC   KEY+3(1),BDMASPRD                                                
         BE    SP370                                                            
         CLC   KEY+3(1),BDMASPRD+1                                              
         BE    SP370                                                            
*                                                                               
SP360    GOTO1 DATAMGR,DMCB,DMADD,SPTDIR,KEY,KEY,DMWORK                         
         TM    DM3,X'DD'          ALLOW DUP KEY ON ADD OR DELETED REC           
         JNZ   *+2                 ERROR                                        
*                                                                               
SP370    LA    R5,18(R5)                                                        
         CLI   0(R5),0             IS THIS THE END OF THE SPBUFF?               
         BNE   SP340                NO.                                         
*                                                                               
SP380    MVC   KEY(13),SAVEKEY                                                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),SPTDIR,KEY,KEY                       
         CLC   KEY(13),SAVEKEY                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* SP400 - THIS SECTION UPDATES THE INVOICE RECORDS                              
***********************************************************************         
SP400    CLI   BUYSW,C'Y'          WERE ANY BUYS FOUND?                         
         BE    SP406                YES.                                        
         CLI   BUYSW,0             IS THIS REQLAST PROC FROM PROG CK            
         BE    EXIT                 YES.                                        
         MVC   P+10(19),=C'NO BUYS WERE FOUND!'                                 
         GOTO1 REPORT                                                           
         B     SP410                                                            
         SPACE                                                                  
SP406    LLC   R0,KEY                                                           
         MVI   KEYSAVE,X'FF'       FORCE TO BUY                                 
         BAS   RE,REPRT            PRINT TOTALS                                 
         STC   R0,KEY              RESTORE KEY                                  
         SPACE                                                                  
SP410    MVI   BUYSW,0             SET BUYSW FOR PROG CK                        
         MVC   AREC,ADBUY                                                       
         CLI   QOPT1,C'Y'          SHOULD I FIX INVOICES?                       
         BNE   SP400X               NO. SKIP INVOICES                           
         XC    COUNT,COUNT                                                      
         MVI   FORCEHED,C'Y'        YES.                                        
         MVI   RCSUBPRG,2          SET INVOICE HEADER                           
         L     R8,AREC                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,X'0B'           INVOICE RECORD                               
         MVC   KEY+1(1),SVAGYMD    A-M                                          
         MVC   KEY+2(3),OLDSTA     STATION CALL LETTERS                         
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    SP415                NO.                                         
         MVC   KEY+5(2),SAVCLT     CLIENT                                       
         SPACE                                                                  
SP415    GOTO1 HIGH                                                             
         B     SP422                                                            
         SPACE                                                                  
SP420    GOTO1 SEQ                                                              
         SPACE                                                                  
SP422    CLC   KEY(5),KEYSAVE      SAME 0B/A-M/STA                              
         BE    SP424                YES                                         
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP499                 NO, TRY NINV RECORD                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,7,KEY+2                                                       
         ICM   RF,7,KEYSAVE+2                                                   
         SRL   RE,7                DROP NET FROM CABLE HEAD                     
         SRL   RF,7                                                             
         CR    RE,RF               IS THIS THE SAME CABLE HEAD                  
         BNE   SP499                NO, TRY NINV RECORDS                        
         SPACE                                                                  
SP424    OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    SP426                NO.                                         
         CLC   KEY+5(2),SAVCLT                                                  
         BNE   SP499                CHECK NINV RECORDS                          
         SPACE                                                                  
SP426    MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   SP430                                                            
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
         SPACE                                                                  
SP430    MVC   KEY+2(3),NEWSTA     DOES NEW INVOICE ALREADY EXIST?              
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP434                                                            
         MVC   BYTE,SAVEKEY+4                                                   
         NI    BYTE,X'7F'                                                       
         OC    KEY+4(1),BYTE       SET IN NETWORK                               
         SPACE                                                                  
SP434    GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     SAME 0B/A-M/STA/CLT/DATE/SEQ                 
         BE    SP490                YES. DON'T MOVE                             
         MVC   KEY(13),SAVEKEY      NO. RESTORE KEY                             
         GOTO1 HIGH                                                             
         SPACE                                                                  
         GOTO1 GET                                                              
         OC    SAVDA,SAVDA         IS THIS 1ST INVOICE?                         
         BZ    SP435                YES.                                        
         ICM   RF,15,KEY+14        GET THE DISK ADDR                            
         C     RF,SAVDA            SHOULD I CHANGE THIS RECORD?                 
         BNL   SP420                NO. THIS IS A NEW ONE                       
         SPACE                                                                  
SP435    CLI   RCWRITE,C'Y'                                                     
         BNE   SP440                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC,DMWORK                   
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
SP440    MVC   DUB(1),4(R8)        SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+4                                                   
         MVC   2(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+2(3),NEWSTA                                                  
         NI    15(R8),X'7F'        UNDELETE THE RECORD                          
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP444                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    4(1,R8),DUB        SET IN NETWORK                                
         OC    KEY+4(1),DUB+1                                                   
         SPACE                                                                  
SP444    CLI   QOPT5,C'Y'                                                       
         BNE   SP450                                                            
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
         SPACE                                                                  
SP450    CLI   RCWRITE,C'Y'                                                     
         BNE   SP460                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,AREC,DMWORK                   
         CLI   DM3,0                                                            
         JNE   *+2                                                              
SP460    OC    SAVDA,SAVDA         IS THIS 1ST ADD?                             
         BZ    SP470                NO.                                         
         MVC   SAVDA,KEY+14        SAVE DISK ADDR OF 1ST ADD                    
         SPACE                                                                  
* PRINT THE OLD INVOICE RECORD *                                                
         SPACE                                                                  
SP470    DS    0H                                                               
         LH    R1,COUNT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,COUNT            # RECORDS CHANGED                            
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,KEY+5,P+10                                           
         GOTO1 DATCON,(R1),(2,KEY+7),(5,P+20)                                   
         GOTO1 HEXOUT,(R1),KEY+9,P+34,1,=C'MIX'                                 
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         SPACE                                                                  
SP480    MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),SPTDIR,KEY,KEY                       
         CLC   KEY(13),SAVEKEY                                                  
         BE    SP420                                                            
         DC    H'0'                                                             
         SPACE                                                                  
* PRINT ERROR FOR INVOICE ALREADY ON NEW STATION FOR THIS DATE *                
         SPACE                                                                  
SP490    GOTO1 CLUNPK,DMCB,KEY+5,P+10                                           
         GOTO1 DATCON,(R1),(2,KEY+7),(5,P+20)                                   
         GOTO1 HEXOUT,(R1),KEY+9,P+34,1,=C'MIX'                                 
         MVC   P+45(40),=CL40'* ERROR * INVOICE ALREADY ON NEW STATION'         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SP480                                                            
         SPACE 3                                                                
         SPACE                                                                  
SP499    BAS   RE,REPRT            PRINT TOTALS                                 
*                                                                               
* THIS SECTION UPDATE NEW INVOICE RECORD                                        
*                                                                               
         GOTO1 =A(SNINV)           GO MOVE NEW INVOICES                         
         EJECT                                                                  
*                                                                               
SP400X   DS    0H                                                               
*                                                                               
* THIS SECTION UPDATES AUTHORIZATION RECORDS                                    
*                                                                               
         MVI   FORCEHED,C'Y'        YES.                                        
         MVI   RCSUBPRG,3          SET AUTHORIZATION HEADER                     
         GOTO1 =A(SNAUTH)          GO MOVE NEW INVOICES                         
         EJECT                                                                  
*                                                                               
* THIS SECTION UPDATES DARE ORDER RECORDS *                                     
*                                                                               
         DS    0H                                                               
         MVI   FORCEHED,C'Y'        YES.                                        
         MVI   RCSUBPRG,4          SET DARE HEADER                              
         GOTO1 =A(DARORD)          GO MOVE DARE ORDER RECORDS                   
         EJECT                                                                  
*                                                                               
* THIS SECTION UPDATES NWS (NEW BUYERS WORKSHEET RECORRDS *                     
*                                                                               
*        GOTO1 =A(NWS)             GO MOVE NWS RECORDS                          
*                                                                               
***********************************************************************         
* SP500 - THIS SECTION UPDATES NSID RECORDS                                     
***********************************************************************         
SP500    MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,5          SET SID/NSID HEADER                          
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         XC    SCHEME,SCHEME       DO 'ALL' SCHEME FIRST                        
*                                                                               
SP510    MVI   KEY,SIRKTYPQ        NEW SID RECORD                               
         MVC   KEY+1(1),SVAGYMD    A-M                                          
         MVC   KEY+2(2),SCHEME     SCHEME CODE                                  
         GOTO1 HIGH                                                             
         CLC   KEY(2),KEYSAVE      TEST SAME A/M                                
         BNE   SP500X              NO -- FINISHED                               
*                                                                               
         MVC   SCHEME,KEY+2        SAVE SCHEME CODE                             
         MVC   KEY+4(5),OLDMSTA    OLD MARKET/STATION                           
         GOTO1 HIGH                                                             
         B     SP530                                                            
*                                                                               
SP520    GOTO1 SEQ                                                              
*                                                                               
SP530    CLC   KEY(2),KEYSAVE      TEST 0C/A-M                                  
         BNE   SP500X               NO. CHECK OLD SID RECORDS                   
*                                                                               
         CLC   KEY(9),KEYSAVE      TEST 0C/A-M/SCHEME/MKT/STA                   
         BE    SP534                YES                                         
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP585                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,7,KEY+6                                                       
         ICM   RF,7,KEYSAVE+6                                                   
         SRL   RE,7                DROP NET FROM CABLE HEAD                     
         SRL   RF,7                                                             
         CR    RE,RF               IS THIS THE SAME CABLE HEAD                  
         BNE   SP585                NO. TRY NEXT SCHEME                         
*                                                                               
SP534    MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   *+12                                                             
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
         MVC   KEY+4(5),NEWMSTA    DOES NEW RECORD ALREADY EXIST?               
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP536                                                            
         MVC   BYTE,SAVEKEY+8                                                   
         NI    BYTE,X'7F'                                                       
         OC    KEY+8(1),BYTE       SET IN NETWORK                               
         SPACE                                                                  
SP536    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     SAME 0C/A-M/SCHEME/MKTSTA/DPT/YR/PER         
         BE    SP590               YES, DUPLICATE KEYS (TSK,TSK)                
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE DATAMGR SEQ                          
*                                                                               
         GOTO1 GET                                                              
         USING SIRRECD,R8                                                       
*                                                                               
         GOTOR CRSTRTAB,DMCB,(C'S',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   SP520                       NO, READ NEXT                        
*                                                                               
SP540    CLI   RCWRITE,C'Y'                                                     
         BNE   SP550                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC,DMWORK                   
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
SP550    MVC   DUB(1),SIRKSTA+2    SAVE POSSIBLE CABLE NETWORK                  
         MVC   DUB+1(1),KEY+8                                                   
         MVC   SIRKSTA,NEWSTA      UPDATE CALL LETTERS IN RECORD                
         MVC   KEY+6(3),NEWSTA     IN KEY AS WELL                               
         NI    SIRRCNTL,X'7F'      UNDELETE THE RECORD                          
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP556                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    SIRKSTA+2(1),DUB   SET IN NETWORK                                
         OC    KEY+8(1),DUB+1     SET IN NETWORK                                
         SPACE                                                                  
         DROP  R8                                                               
*                                                                               
SP556    CLI   QOPT5,C'Y'                                                       
         BNE   SP560                                                            
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP560    CLI   RCWRITE,C'Y'                                                     
         BNE   SP570                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,AREC,DMWORK                   
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
SP570    LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
*                                                                               
SP580    MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),SPTDIR,KEY,KEY                       
         CLC   KEY(13),SAVEKEY                                                  
         BE    SP520                                                            
         DC    H'0'                                                             
*                                                                               
SP585    SR    RF,RF               TRY THE NEXT POSSIBLE SCHEME CODE            
         ICM   RF,3,SCHEME                                                      
         LA    RF,1(RF)                                                         
         STCM  RF,3,SCHEME                                                      
         B     SP510                                                            
*                                                                               
* PRINT ERROR FOR NSID REC ALREADY ON NEW STATION FOR THIS DATE *               
*                                                                               
SP590    GOTO1 MSUNPK,DMCB,KEY+4,P+10,P+20                                      
         GOTO1 HEXOUT,(R1),KEY+8,P+38,1,=C'MIX'                                 
         MVC   P+45(40),=CL40'* ERROR NSID REC EXISTS FOR NEW STATION'          
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SP580                                                            
*                                                                               
SP500X   DS    0H                                                               
         BAS   RE,REPRT                                                         
         EJECT                                                                  
*                                                                               
* SP600 - THIS SECTION UPDATES SID RECORDS *                                    
*                                                                               
SP600    DS    0H                                                               
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D59'     SID RECORD                                   
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         MVC   KEY+3(5),OLDMSTA    OLD MARKET/STATION                           
         GOTO1 HIGH                                                             
         B     SP620                                                            
*                                                                               
SP610    GOTO1 SEQ                                                              
*                                                                               
SP620    CLC   KEY(8),KEYSAVE      SAME 0B/A-M/MSTA                             
         BE    SP624                YES, GO ON                                  
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP600X                                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,7,KEY+5                                                       
         ICM   RF,7,KEYSAVE+5                                                   
         SRL   RE,7                DROP NET FROM CABLE HEAD                     
         SRL   RF,7                                                             
         CR    RE,RF               IS THIS THE SAME CABLE HEAD                  
         BNE   SP600X               NO. CHECK STATION REC                       
*                                                                               
SP624    MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   SP630                                                            
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP630    MVC   KEY+3(5),NEWMSTA    DOES NEW RECORD ALREADY EXIST?               
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP634                                                            
         MVC   BYTE,SAVEKEY+7      SAVE CABLE HEAD NETWORK                      
         NI    BYTE,X'7F'                                                       
         OC    KEY+7(1),BYTE       SET IN NETWORK                               
*                                                                               
SP634    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     SAME 0B/A-M/MSTA/PER/SEQ/DPT...              
         BE    DUPERR              YES, DUPLICATE KEYS (TSK,TSK)                
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE DATAMGR SEQ                          
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         GOTOR CRSTRTAB,DMCB,(C'S',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   SP610                       NO, READ NEXT                        
*                                                                               
SP640    CLI   RCWRITE,C'Y'                                                     
         BNE   SP650                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC,DMWORK                   
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
SP650    MVC   DUB(1),7(R8)        SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+7                                                   
         MVC   5(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+5(3),NEWSTA                                                  
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP656                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    7(1,R8),DUB   SET IN NETWORK                                     
         OC    KEY+7(1),DUB+1     SET IN NETWORK                                
*                                                                               
SP656    NI    15(R8),X'7F'        UNDELETE THE RECORD                          
         CLI   QOPT5,C'Y'                                                       
         BNE   SP660                                                            
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
         SPACE                                                                  
SP660    CLI   RCWRITE,C'Y'                                                     
         BNE   SP670                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,AREC,DMWORK                   
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
SP670    LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
*                                                                               
SP680    MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),SPTDIR,KEY,KEY                       
         CLC   KEY(13),SAVEKEY                                                  
         BE    SP610                                                            
         DC    H'0'                                                             
*                                                                               
* PRINT ERROR FOR SID REC ALREADY ON NEW STATION FOR THIS DATE *                
*                                                                               
DUPERR   DS    0H                                                               
         GOTO1 MSUNPK,DMCB,KEY+3,P+10,P+20                                      
         GOTO1 HEXOUT,(R1),KEY+8,P+38,1,=C'MIX'                                 
         MVC   P+45(40),=CL40'* ERROR * SID REC EXISTS FOR NEW STATION'         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SP680                                                            
*                                                                               
SP600X   DS    0H                                                               
         BAS   RE,REPRT                                                         
*                                                                               
* SP6A00 - THIS SECTION UPDATES CALL LETTERS FOR DESTINATION RECORDS            
*                                                                               
SP6A00   DS    0H                                                               
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   RCSUBPRG,6          SET DESTINATION HEADER                       
*                                                                               
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         XC    SAVDA,SAVDA         CLEAN OUT SAVED DISK ADDRESS                 
*                                                                               
         MVI   KEY,X'0D'           DESTINE RECORD TYPE                          
         MVI   KEY+1,X'3D'         DESTINE RECORD SUBTYPE                       
         MVC   KEY+2(1),SVAGYMD                                                 
         MVC   KEY+3(3),OLDSTA                                                  
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    SP6A15               - NO.                                       
         MVC   KEY+6(2),SAVCLT     CLIENT                                       
         SPACE                                                                  
SP6A15   GOTO1 HIGH                                                             
         B     SP6A22                                                           
         SPACE                                                                  
SP6A20   GOTO1 SEQ                                                              
         SPACE                                                                  
SP6A22   CLC   KEY(6),KEYSAVE      SAME 0D3D/AGYMD/STA?                         
         BNE   SP6A00X                                                          
*                                                                               
SP6A24   OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    SP6A26               - NO.                                       
         CLC   KEY+6(2),SAVCLT                                                  
         BNE   SP6A20              GO TO THE NEXT RECORD                        
*                                                                               
SP6A26   MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   SP6A30                                                           
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP6A30   MVC   KEY+3(3),NEWSTA                                                  
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP6A34                                                           
         MVC   BYTE,SAVEKEY+5                                                   
         NI    BYTE,X'7F'                                                       
         OC    KEY+5(1),BYTE       SET IN NETWORK                               
*                                                                               
SP6A34   GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      SAME 0D3D/AM/STA/CLT                         
         BE    SP6A90               YES. DON'T MOVE                             
         MVC   KEY(13),SAVEKEY      NO. RESTORE KEY                             
         GOTO1 HIGH                                                             
*                                                                               
         GOTO1 GET                                                              
         OC    SAVDA,SAVDA         IS THIS 1ST DESTINE?                         
         BZ    SP6A35               YES.                                        
         ICM   RF,15,KEY+14        GET THE DISK ADDR                            
         C     RF,SAVDA            SHOULD I CHANGE THIS RECORD?                 
         BNL   SP6A20               NO. THIS IS A NEW ONE                       
         B     SP6A35A                                                          
*                                                                               
SP6A35   GOTOR CRSTRTAB,DMCB,(C'S',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   SP6A20                      NO, READ NEXT                        
*                                                                               
SP6A35A  CLI   RCWRITE,C'Y'                                                     
         BNE   SP6A40                                                           
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC,DMWORK                   
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
SP6A40   MVC   DUB(1),5(R8)        SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+5                                                   
         MVC   3(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+3(3),NEWSTA                                                  
         NI    15(R8),X'7F'        UNDELETE THE RECORD                          
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP6A44                                                           
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    5(1,R8),DUB         SET IN NETWORK                               
         OC    KEY+5(1),DUB+1                                                   
*                                                                               
SP6A44   CLI   QOPT5,C'Y'                                                       
         BNE   SP6A50                                                           
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP6A50   CLI   RCWRITE,C'Y'                                                     
         BNE   SP6A60                                                           
         GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,AREC,DMWORK                   
         CLI   DM3,0                                                            
         JNE   *+2                                                              
SP6A60   OC    SAVDA,SAVDA         IS THIS 1ST ADD?                             
         BZ    SP6A70               NO.                                         
         MVC   SAVDA,KEY+14        SAVE DISK ADDR OF 1ST ADD                    
*                                                                               
* PRINT THE NEW DESTINATION RECORD *                                            
         SPACE                                                                  
SP6A70   DS    0H                                                               
         LH    R1,COUNT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,COUNT            # RECORDS CHANGED                            
*                                                                               
         BRAS  RE,SP6APRT                                                       
*                                                                               
SP6A80   MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),SPTDIR,KEY,KEY                       
         CLC   KEY(13),SAVEKEY                                                  
         BE    SP6A20                                                           
         DC    H'0'                                                             
*                                                                               
SP6APRT  LR    R6,RE                                                            
         GOTO1 CLUNPK,DMCB,KEY+6,P+10                                           
         GOTO1 MSUNPK,DMCB,KEY+1,WORK,P+20                                      
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         BR    R6                                                               
*                                                                               
* PRINT ERROR FOR DESTINE ALREADY ON NEW STATION (CLIENT) *                     
*                                                                               
SP6A90   MVC   P+45(40),=CL40'* ERROR * DESTINE RECORD ALREADY EXISTS'          
         BRAS  RE,SP6APRT                                                       
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SP6A80                                                           
*                                                                               
SP6A00X  DS    0H                                                               
         BAS   RE,REPRT                                                         
*                                                                               
* SP6B00 - THIS SECTION UPDATES CALL LETTERS FOR LAST METHOD RECORDS            
*                                                                               
SP6B00   DS    0H                                                               
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   RCSUBPRG,7          SET LAST METHOD HEADER                       
*                                                                               
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         XC    SAVDA,SAVDA         CLEAN OUT SAVED DISK ADDRESS                 
*                                                                               
         MVI   KEY,X'0D'           LAST METHOD RECORD TYPE                      
         MVI   KEY+1,X'3E'         LAST METHOD RECORD SUBTYPE                   
***  2 BYTES OF NULLS HERE    KEY+2 AND KEY+3                                   
         MVC   KEY+4(1),SVAGYMD                                                 
***  WE'RE STARTING AT NULLS FOR THE BUYER                                      
         MVC   KEY+8(3),OLDSTA                                                  
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    SP6B15               - NO.                                       
         MVC   KEY+11(2),SAVCLT     CLIENT                                      
*                                                                               
SP6B15   GOTO1 HIGH                                                             
         B     SP6B22                                                           
*                                                                               
SP6B20   GOTO1 SEQ                                                              
*                                                                               
SP6B22   CLC   KEY(5),KEYSAVE      SAME 0D3E/X'0000'/AGYMD?                     
         BNE   SP6B00X                                                          
         CLC   KEY+8(3),OLDSTA     SAME 0D3E/0000/AGYMD/___/STA?                
         BNE   SP6B20              GO TO THE NEXT RECORD                        
*                                                                               
SP6B24   OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    SP6B26               - NO.                                       
         CLC   KEY+11(2),SAVCLT                                                 
         BNE   SP6B20              GO TO THE NEXT RECORD                        
*                                                                               
SP6B26   MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   SP6B30                                                           
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP6B30   MVC   KEY+8(3),NEWSTA                                                  
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP6B34                                                           
         MVC   BYTE,SAVEKEY+5                                                   
         NI    BYTE,X'7F'                                                       
         OC    KEY+10(1),BYTE      SET IN NETWORK                               
*                                                                               
SP6B34   GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     SAME 0D3E/0000/AM/___/STA/CLT                
         BE    SP6B90               YES. DON'T MOVE                             
         MVC   KEY(13),SAVEKEY      NO. RESTORE KEY                             
         GOTO1 HIGH                                                             
*                                                                               
         GOTO1 GET                                                              
         OC    SAVDA,SAVDA         IS THIS 1ST LAST METHOD?                     
         BZ    SP6B35               YES.                                        
         ICM   RF,15,KEY+14        GET THE DISK ADDR                            
         C     RF,SAVDA            SHOULD I CHANGE THIS RECORD?                 
         BNL   SP6B20               NO. THIS IS A NEW ONE                       
         B     SP6B35A                                                          
*                                                                               
SP6B35   GOTOR CRSTRTAB,DMCB,(C'S',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   SP6B20                      NO, READ NEXT                        
*                                                                               
SP6B35A  CLI   RCWRITE,C'Y'                                                     
         BNE   SP6B40                                                           
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC,DMWORK                   
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
SP6B40   MVC   DUB(1),10(R8)       SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+10                                                  
         MVC   8(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+8(3),NEWSTA                                                  
         NI    15(R8),X'7F'        UNDELETE THE RECORD                          
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP6B44                                                           
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    10(1,R8),DUB         SET IN NETWORK                              
         OC    KEY+10(1),DUB+1                                                  
*                                                                               
SP6B44   CLI   QOPT5,C'Y'                                                       
         BNE   SP6B50                                                           
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP6B50   CLI   RCWRITE,C'Y'                                                     
         BNE   SP6B60                                                           
         GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,AREC,DMWORK                   
         CLI   DM3,0                                                            
         JNE   *+2                                                              
SP6B60   OC    SAVDA,SAVDA         IS THIS 1ST ADD?                             
         BZ    SP6B70               NO.                                         
         MVC   SAVDA,KEY+14        SAVE DISK ADDR OF 1ST ADD                    
*                                                                               
* PRINT THE NEW LAST METHOD RECORD *                                            
         SPACE                                                                  
SP6B70   DS    0H                                                               
         LH    R1,COUNT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,COUNT            # RECORDS CHANGED                            
*                                                                               
         GOTO1 CLUNPK,DMCB,KEY+11,P+10                                          
         MVC   P+17(3),KEY+5                                                    
***      GOTO1 MSUNPK,DMCB,KEY+8,P+20,P+25                                      
*        GOTO1 DATCON,(R1),(2,KEY+7),(5,P+20)                                   
*        GOTO1 HEXOUT,(R1),KEY+9,P+38,1,=C'MIX'                                 
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
SP6B80   MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),SPTDIR,KEY,KEY                       
         CLC   KEY(13),SAVEKEY                                                  
         BE    SP6B20                                                           
         DC    H'0'                                                             
*                                                                               
* PRINT ERROR FOR LASTMTHD ALREADY ON NEW STATION (CLIENT) *                    
*                                                                               
SP6B90   GOTO1 CLUNPK,DMCB,KEY+11,P+10                                          
         MVC   P+17(3),KEY+5                                                    
*        GOTO1 DATCON,(R1),(2,KEY+7),(5,P+20)                                   
*        GOTO1 HEXOUT,(R1),KEY+9,P+38,1,=C'MIX'                                 
         MVC   P+45(40),=CL40'* ERROR * LASTMTHD RECORD ALREADY EXISTS'         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SP6B80                                                           
*                                                                               
SP6B00X  BAS   RE,REPRT                                                         
*                                                                               
* DARBATCH - THIS SECTION UPDATES DARE BATCH RECORDS *                          
*                                                                               
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   RCSUBPRG,8          SET DAR BATCH HEADER                         
         BRAS  RE,DARBATCH      GO UPDATE DARE BATCH RECORDS                    
         BAS   RE,REPRT                                                         
*                                                                               
* PW100 - THIS SECTION UPDATES PROFIT WITHIN RECORDS *                          
*                                                                               
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   RCSUBPRG,9          SET PROFIT WITHIN HEADER                     
         GOTO1 =A(PW100)        GO UPDATE PROFIT WITHIN RECORDS                 
         BAS   RE,REPRT                                                         
*&&DO                                                                           
*                                                                               
* PW200 - THIS SECTION UPDATES STATION LOCKIN RECORDS  ***DEFUNCT***            
*                                                                               
         GOTO1 =A(PW200)        GO UPDATE STATION LOCKIN RECORDS                
         BAS   RE,REPRT                                                         
*&&                                                                             
* PW200 - THIS SECTION UPDATES STATION LOCKIN XSP RECORDS *                     
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   RCSUBPRG,10         SET STATION LOCK-IN HEADER                   
         GOTO1 =A(PW300)        GO UPDATE STATION LOCK-IN RECORDS               
         BAS   RE,REPRT                                                         
*                                                                               
* DB100 - THIS SECTION UPDATES DOUBLE BOOKING RECORDS *                         
*                                                                               
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   RCSUBPRG,11         SET DOUBLE BOOKING HEADER                    
         GOTO1 =A(DB100)        GO UPDATE DOUBLE BOOKING RECORDS                
         BAS   RE,REPRT                                                         
         EJECT                                                                  
***********************************************************************         
* SP700 - THIS SECTION UPDATES TRAFFIC INSTRUCTION RECAP RECORDS *              
***********************************************************************         
SP700    MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   RCSUBPRG,12         SET INST RECAP HEADER                        
*                                                                               
         L     RE,UTL                                                           
         CLI   FCUPTRF,C'Y'        TEST TRAFFIC SYSTEM OP                       
         BNE   *+10                                                             
         MVC   4(1,RE),RCUTLTRF    SET TRAFFIC SYSTEM NUMBER                    
*                                                                               
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A24'     INST RECAP RECORD                            
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+10                NO, THEN ALL CLIENTS                         
         MVC   KEY+3(2),SAVCLT                                                  
         GOTO1 HIGH                                                             
         B     SP720                                                            
*                                                                               
SP710    GOTO1 SEQ                                                              
*                                                                               
SP720    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SP700X              NO,THEN TRY NEXT                             
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+14                NO, THEN ALL CLIENTS                         
         CLC   KEY+3(2),SAVCLT                                                  
         BNE   SP700X                                                           
         CLC   KEY+6(5),OLDMSTA    OLD MARKET/STATION                           
         BE    SP726                                                            
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP710               NO                                           
         MVC   DUB(5),KEY+6        MKT/STA                                      
         NI    DUB+4,X'80'         DROP NETWORK BITS                            
         CLC   DUB(5),OLDMSTA      MATCH ON MKT/STA?                            
         BNE   SP710               NO                                           
*                                                                               
SP726    MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   SP730                                                            
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP730    MVC   KEY+6(5),NEWMSTA    DOES NEW RECORD ALREADY EXIST?               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP734                                                            
         MVC   BYTE,SAVEKEY+10     SAVE CABLE HEAD NETWORK                      
         NI    BYTE,X'7F'                                                       
         OC    KEY+10(1),BYTE      SET IN NETWORK                               
*                                                                               
SP734    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DUP10               YES, DUPLICATE KEYS (TSK,TSK)                
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE DATAMGR SEQ                          
*                                                                               
         GOTO1 GET                                                              
*                                                                               
SP740    GOTOR CRSTRTAB,DMCB,(C'T',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   SP710                       NO, READ NEXT                        
*                                                                               
SP740A   CLI   RCWRITE,C'Y'                                                     
         BNE   SP750                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',=C'TRFFILE',KEY,AREC,DMWORK               
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
SP750    MVC   DUB(1),10(R8)       SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+10                                                  
         MVC   8(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+8(3),NEWSTA                                                  
         NI    15(R8),X'7F'        UNDELETE THE RECORD                          
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP756                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    10(1,R8),DUB        SET IN NETWORK                               
         OC    KEY+10(1),DUB+1                                                  
*                                                                               
SP756    CLI   QOPT5,C'Y'                                                       
         BNE   SP760                                                            
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP760    CLI   RCWRITE,C'Y'                                                     
         BNE   SP770                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
SP770    LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
*                                                                               
         BRAS  RE,SP7PRT                                                        
*                                                                               
SP780    MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         OI    DMINBTS,X'08'       CHECK DELETED RECS TOO                       
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),SAVEKEY                                                  
         BE    SP710                                                            
         DC    H'0'                                                             
*                                                                               
* PRINT TRAFFIC INSTR RECAP                                                     
*                                                                               
SP7PRT   LR    R6,RE                                                            
         GOTO1 CLUNPK,DMCB,KEY+3,P+10                                           
         GOTO1 MSUNPK,(R1),KEY+6,P+17,P+22                                      
         GOTOR HEXOUT,DMCB,KEY+11,P+29,1,=C'TOG',0                              
         GOTOR HEXOUT,DMCB,KEY+12,P+34,1,=C'TOG',0                              
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         BR    R6                                                               
*                                                                               
* PRINT ERROR FOR TRAFFIC INSTR RECAP ALREADY ON NEW STATION *                  
*                                                                               
DUP10    MVC   P+45(40),NEWEXIST                                                
         BRAS  RE,SP7PRT                                                        
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SP780                                                            
*                                                                               
SP700X   BAS   RE,REPRT                                                         
         EJECT                                                                  
***********************************************************************         
* SP800 - THIS SECTION UPDATES TRAFFIC SHIPPING RECAP RECORDS *                 
***********************************************************************         
SP800    MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   RCSUBPRG,13         SET SHIP RECAP HEADER                        
*                                                                               
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A25'     SHIP RECAP RECORD                            
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+10                NO, THEN ALL CLIENTS                         
         MVC   KEY+3(2),SAVCLT                                                  
         GOTO1 HIGH                                                             
         B     SP820                                                            
*                                                                               
SP810    GOTO1 SEQ                                                              
*                                                                               
SP820    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SP800X              NO,THEN TRY NEXT                             
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+14                NO, THEN ALL CLIENTS                         
         CLC   KEY+3(2),SAVCLT                                                  
         BNE   SP800X                                                           
         CLC   KEY+5(5),OLDMSTA    OLD MARKET/STATION                           
         BE    SP826                                                            
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP810                                                            
         MVC   DUB(5),KEY+5                                                     
         NI    DUB+4,X'80'                                                      
         CLC   DUB(5),OLDMSTA                                                   
         BNE   SP810                                                            
*                                                                               
SP826    MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   SP830                                                            
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP830    MVC   KEY+5(5),NEWMSTA    DOES NEW RECORD ALREADY EXIST?               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP834                                                            
         MVC   BYTE,SAVEKEY+9      SAVE CABLE HEAD NETWORK                      
         NI    BYTE,X'7F'                                                       
         OC    KEY+9(1),BYTE       SET IN NETWORK                               
*                                                                               
SP834    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DUP20               YES, DUPLICATE KEYS (TSK,TSK)                
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE DATAMGR SEQ                          
*                                                                               
         GOTO1 GET                                                              
*                                                                               
SP840    GOTOR CRSTRTAB,DMCB,(C'T',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   SP810                       NO, READ NEXT                        
*                                                                               
SP840A   CLI   RCWRITE,C'Y'                                                     
         BNE   SP850                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',=C'TRFFILE',KEY,AREC,DMWORK               
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
SP850    MVC   DUB(1),9(R8)        SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+9                                                   
         MVC   7(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+7(3),NEWSTA                                                  
         NI    15(R8),X'7F'        UNDELETE THE RECORD                          
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP856                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    9(1,R8),DUB         SET IN NETWORK                               
         OC    KEY+9(1),DUB+1                                                   
*                                                                               
SP856    CLI   QOPT5,C'Y'                                                       
         BNE   SP860                                                            
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP860    CLI   RCWRITE,C'Y'                                                     
         BNE   SP870                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
SP870    LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
*                                                                               
         BRAS  RE,SP8PRT                                                        
*                                                                               
SP880    MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         OI    DMINBTS,X'08'       CHECK DELETED RECS TOO                       
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),SAVEKEY                                                  
         BE    SP810                                                            
         DC    H'0'                                                             
*                                                                               
* PRINT TRAFFIC SHIPPING RECAP                                                  
*                                                                               
SP8PRT   LR    R6,RE                                                            
         GOTO1 CLUNPK,DMCB,KEY+3,P+10                                           
         GOTO1 MSUNPK,(R1),KEY+5,P+17,P+22                                      
         GOTOR HEXOUT,DMCB,KEY+10,P+32,3,=C'TOG',0                              
****     EDIT  (B3,KEY+11),(8,P+30),ZERO=NOBLANK                                
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         BR    R6                                                               
*                                                                               
* PRINT ERROR FOR TRAFFIC SHIPPING RECAP ALREADY ON NEW STATION *               
*                                                                               
DUP20    MVC   P+45(40),NEWEXIST                                                
         MVI   P+70,C'5'                                                        
         BRAS  RE,SP8PRT                                                        
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SP880                                                            
*                                                                               
SP800X   BAS   RE,REPRT                                                         
         EJECT                                                                  
***********************************************************************         
* SP900 - THIS SECTION UPDATES TRAFFIC BUY ACTIVITY RECORDS *                   
***********************************************************************         
SP900    MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   RCSUBPRG,14         SET BUY ACTIVITY HEADER                      
*                                                                               
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2E'     BUY ACT/ESTM RECORD                          
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+10                NO, THEN ALL CLIENTS                         
         MVC   KEY+3(2),SAVCLT                                                  
         GOTO1 HIGH                                                             
         B     SP920                                                            
*                                                                               
SP910    GOTO1 SEQ                                                              
*                                                                               
SP920    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SP900X              NO,THEN TRY NEXT                             
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+14                NO, THEN ALL CLIENTS                         
         CLC   KEY+3(2),SAVCLT                                                  
         BNE   SP900X                                                           
         CLC   KEY+5(5),OLDMSTA    OLD MARKET/STATION                           
         BE    SP926                                                            
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP910                                                            
         MVC   DUB(5),KEY+5                                                     
         NI    DUB+4,X'80'                                                      
         CLC   DUB(5),OLDMSTA                                                   
         BNE   SP910                                                            
*                                                                               
SP926    MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   SP930                                                            
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP930    MVC   KEY+5(5),NEWMSTA    DOES NEW RECORD ALREADY EXIST?               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP934                                                            
         MVC   BYTE,SAVEKEY+9      SAVE CABLE HEAD NETWORK                      
         NI    BYTE,X'7F'                                                       
         OC    KEY+9(1),BYTE       SET IN NETWORK                               
*                                                                               
SP934    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DUP30               YES, DUPLICATE KEYS (TSK,TSK)                
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE DATAMGR SEQ                          
*                                                                               
         GOTO1 GET                                                              
*                                                                               
SP940    GOTOR CRSTRTAB,DMCB,(C'T',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   SP910                       NO, READ NEXT                        
*                                                                               
SP940A   CLI   RCWRITE,C'Y'                                                     
         BNE   SP950                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',=C'TRFFILE',KEY,AREC,DMWORK               
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
SP950    MVC   DUB(1),9(R8)        SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+9                                                   
         MVC   7(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+7(3),NEWSTA                                                  
         NI    15(R8),X'7F'        UNDELETE THE RECORD                          
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP956                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    9(1,R8),DUB         SET IN NETWORK                               
         OC    KEY+9(1),DUB+1                                                   
*                                                                               
SP956    CLI   QOPT5,C'Y'                                                       
         BNE   SP960                                                            
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP960    CLI   RCWRITE,C'Y'                                                     
         BNE   SP970                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
SP970    LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
*                                                                               
         BRAS  RE,SP9PRT                                                        
*                                                                               
SP980    MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         OI    DMINBTS,X'08'       CHECK DELETED RECS TOO                       
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),SAVEKEY                                                  
         BE    SP910                                                            
         DC    H'0'                                                             
*                                                                               
* PRINT TRAFFIC BUY ACTIVITY                                                    
*                                                                               
SP9PRT   LR    R6,RE                                                            
         GOTO1 CLUNPK,DMCB,KEY+3,P+10             PRINT CLT                     
         GOTO1 MSUNPK,(R1),KEY+5,P+17,P+22        PRINT MKT/STA                 
         EDIT  (B1,KEY+11),(3,P+28),ZERO=NOBLANK  PRINT EST                     
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         BR    R6                                                               
*                                                                               
* PRINT ERROR FOR TRAFFIC BUY ACTIVITY ALREADY ON NEW STATION *                 
*                                                                               
DUP30    MVC   P+45(40),NEWEXIST                                                
         MVI   P+70,C'E'                                                        
         BRAS  RE,SP9PRT                                                        
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SP980                                                            
*                                                                               
SP900X   BAS   RE,REPRT                                                         
*                                                                               
* SPA00 - THIS SECTION UPDATES TRAFFIC BUY RECORDS *                            
*                                                                               
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   RCSUBPRG,15         SET TRAFFIC RECORDS                          
         BRAS  RE,SPA00                                                         
*                                                                               
* SPB00 - THIS SECTION UPDATES TRAFFIC STATION LABEL LIST RECORDS *             
*                                                                               
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   RCSUBPRG,16         SET TRAFFIC RECORDS                          
         BRAS  RE,SPB00                                                         
*                                                                               
* SPC00 - THIS SECTION UPDATES TRAFFIC MARKET STATION LIST RECORDS *            
*                                                                               
         BRAS  RE,SPC00                                                         
*                                                                               
* SPD00 - THIS SECTION UPDATES TRAFFIC PATTERN RECORDS *                        
*                                                                               
         BRAS  RE,SPD00                                                         
*                                                                               
* SPE00 - THIS SECTION UPDATES TRAFFIC STATION RECORDS *                        
*                                                                               
         BRAS  RE,SPE00                                                         
*                                                                               
* SPG00 - THIS SECTION UPDATES ORDER HISTORY RECORDS *                          
*                                                                               
         L     RE,UTL                                                           
         MVC   4(1,RE),SPOTSE      RESTORE SPOT SYSTEM NUMBER                   
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   RCSUBPRG,20         SET ORDER HISTORY HEADER                     
         BRAS  RE,SPG00            UPDATE ORDER HISTORY RECORDS                 
*                                                                               
* THIS SECTION UPDATES CANADIAN DESKTOP RECORDS                                 
*                                                                               
SPCDT    MVI   AGYMDN,0            CLEAR AGENCY/MEDIA N                         
         CLI   QMED,C'C'           MEDIA C REQUEST?                             
         BNE   SPCDT10             NO                                           
         MVC   AGYMDN,BAGYMD       SET AGENCY/MEDIA T                           
         NI    AGYMDN,X'F0'        STRIP MEDIA C                                
         OI    AGYMDN,X'03'        MEDIA N BITS                                 
*                                                                               
         XC    DUB,DUB             CLEAR DUB                                    
         MVC   DUB(4),QSTA         MOVE OLD STATION INTO DUB                    
         MVI   DUB+4,C'N'          PACK IT FOR MEDIA N                          
         GOTO1 MSPACK,DMCB,QMKT,DUB,OLDMSTAN                                    
         XC    DUB,DUB             CLEAR DUB                                    
         MVC   DUB(4),NEWCALL      MOVE NEW STATION INTO DUB                    
         MVI   DUB+4,C'N'          PACK IT FOR MEDIA N                          
         GOTO1 (RF),(R1),QMKT,DUB,NEWMSTAN                                      
*                                                                               
SPCDT10  MVC   DATADISP,=AL2(42)   DISPLACEMENT INTO FIRST ELEM                 
*                                                                               
* SPDTCS - THIS SECTION UPDATES CAN-DTM CAMPAIGN STALIST X'0D03' RECS           
*                                                                               
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         MVI   RCSUBPRG,21         SET DTM HEADER                               
         BRAS  RE,SPDTCS           DTM CAMPAIGN STALIST X'0D03' RECS            
*                                                                               
* SPDTP - THIS SECTION UPDATES CAN-DTM PROGRAM X'0D05' RECS                     
*                                                                               
         BRAS  RE,SPDTP            DTM PROGRAM X'0D05' RECS                     
*                                                                               
* SPDTD - THIS SECTION UPDATES CAN-DTM DEMO X'0D06' RECS                        
*                                                                               
         BRAS  RE,SPDTD            DTM DEMO X'0D06' RECS                        
*                                                                               
* SPDTO - THIS SECTION UPDATES CAN-DTM ORDER X'0D07' RECS                       
*                                                                               
         BRAS  RE,SPDTO            DTM ORDER X'0D07' RECS                       
**                                                                              
** SPBTRV - THIS SECTION UPDATES US-SBTK REVISION X'0E10' RECS                  
**                                                                              
SPBTRV   DS    0H                                                               
         MVI   RCSUBPRG,22         SET US SBTK HEADER                           
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         GOTOR SPBTKRC,DMCB,DRVKSUBQ SBTK REVISION X'0E10' RECS                 
*                                                                               
* SPBTWK - THIS SECTION UPDATES US-SBTK WORK X'0E11' RECS                       
*                                                                               
SPBTWK   DS    0H                                                               
         MVI   FORCEHED,C'Y'       START NEW PAGE                               
         GOTOR SPBTKRC,DMCB,DWKKSUBQ SBTK WORK X'0E11' RECS                     
***                                                                             
* GO AND ADD/CHANGE STATION FIX RECORD FOR SFM                                  
***                                                                             
         MVC   DATADISP,=AL2(24)   DISPLACEMENT INTO FIRST ELEM                 
         BRAS  RE,STAFIXIT         ADD/UPDATE STAFIX RECORD                     
*                                                                               
* SPSTA - THIS SECTION CKS IF STATION REC EXISTS FOR NEW CALL LETTERS *         
*                                                                               
SPSTA    MVC   KEY(17),=17C'0'                                                  
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED       MEDIA                                        
         MVC   KEY+2(5),QBOOK1     NEW CALL LETTERS                             
         CLI   QBOOK1+4,C' '                                                    
         BNE   *+10                                                             
         MVC   KEY+6(1),QMED                                                    
         MVC   KEY+7(2),QAGY       AGENCY                                       
         CLC   QCLT,=C'ALL'        ALL CLIENTS?                                 
         BE    SPSTA10                                                          
         MVC   KEY+9(3),QCLT       CLIENT                                       
SPSTA10  DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,STATION,KEY,ADSTAT                           
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
         SPACE                                                                  
         L     R6,ADSTAT                                                        
         CLC   0(9,R6),KEY         SAME MEDIA/STA/AGY?                          
         BNE   SPSTA20              NO.  PRINT WARNING                          
         CLC   QCLT,=C'ALL'                                                     
         BE    CKEOJ                                                            
         CLC   9(3,R6),QCLT        SAME CLT?                                    
         BE    CKEOJ                                                            
SPSTA20  DS    0H                                                               
         MVC   SAVEPRT(132),P                                                   
         XC    P,P                                                              
         MVI   SPACING,2                                                        
         MVC   P+10(27),=CL27'WARNING - STATION NOT FOUND'                      
         GOTO1 REPORT                                                           
         MVC   P,SAVEPRT                                                        
         SPACE                                                                  
CKEOJ    DS    0H                                                               
         CLI   QMED,C'C'           CANADIAN COMBINED?                           
         BNE   EXIT                NO - EXIT                                    
         MVC   BYTE,SVAGYMD        YES - SAVE A/M IN BYTE                       
         NI    BYTE,X'0F'          TURN OFF MEDIA BITS                          
         CLI   BYTE,X'08'          JUST PROCESSED MEDIA C?                      
         BNE   CKEOJ10             NO                                           
         NI    SVAGYMD,X'F7'       YES - TURN OFF MEDIA C BIT                   
         OI    SVAGYMD,X'01'       SET TO MEDIA T (PROCESS SECOND)              
         B     SP410               GO PROCESS ALL THE MEDIA T RECS              
*                                                                               
CKEOJ10  CLI   BYTE,X'01'          JUST PROCESSED MEDIA T?                      
         BNE   CKEOJ20             NO - MEDIA N WAS LAST - WERE DONE            
         NI    SVAGYMD,X'FE'       TURN OFF MEDIA T BIT                         
         OI    SVAGYMD,X'03'       SET TO MEDIA T (PROCESS LAST)                
         B     SP410               GO PROCESS ALL THE MEDIA N RECS              
***                                                                             
* SET SVAGYMD BACK TO A "C" IN CASE THERE IS ANOTHER MEDIA C REQUEST            
* AFTER THIS ONE. SPFILCON WON'T RESET SVAGYMD FOR YOU IF THE AGY/MED           
* IS THE SAME AS THE PREVIOUS ONE (SEE NREQ13A). OTHERWISE, WERE                
* STUCK WITH MEDIA N AND IT WILL SKIP THE THE MEDIA C/T RECS!                   
***                                                                             
CKEOJ20  NI    SVAGYMD,X'FC'       TURN OFF MEDIA N BIT                         
         OI    SVAGYMD,X'08'       RESTORE MEDIA C                              
         B     EXIT                FINALLY DONE                                 
                                                                                
***********************************************************************         
* SPDEL - THIS SECTION DELETES THE BUY PASSIVE POINTERS *                       
***********************************************************************         
         SPACE                                                                  
SPDEL    NTR1                                                                   
*                                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   SPDEL10                                                          
         BAS   RE,SPTRACE          *** TRACE ***                                
*                                                                               
SPDEL10  DS    0H                                                               
         MVI   KEY+13,C'S'         PASSIVE POINTER - DELETE RECORD              
*                                                                               
         GOTOR CRSTRTAB,DMCB,(C'S',KEY)    KEY IN RESTORE TABLE?                
         BNZ   SPDEL20               NO, BYPASS                                 
*                                                                               
SPDEL15  CLI   QOPT5,C'Y'                                                       
         BNE   *+12                                                             
         MVI   TRACECDE,C'5'                                                    
         BAS   RE,SPTRACE          *** TRACE ***                                
*                                                                               
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   SPDEL20                                                          
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY,KEY                                
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
SPDEL20  DS    0H                                                               
         B     EXIT                                                             
***                                                                             
* THIS ROUTINE PRINTS THE BUY LINE NUMBERS FOR EACH PRODUCT                     
***                                                                             
SPPRINT  NTR1                                                                   
         L     R4,ADCLT                                                         
         USING CLTHDRD,R4                                                       
         MVC   P+5(3),CLT          GET THE CLIENT CODE                          
         MVC   P+10(20),CLTNM      GET THE NAME OF THE CLIENT                   
         LA    R6,CLIST                                                         
*                                                                               
SPPRT10  CLC   3(1,R6),3(R5)       IS THIS THE SAME PRODUCT CODE?               
         BE    SPPRT20                                                          
         LA    R6,4(R6)                                                         
         CLI   0(R6),C' '                                                       
         BNL   SPPRT10                                                          
         MVC   P+37(22),=CL22'PRODUCT NAME NOT FOUND'                           
         B     SPPRT30                                                          
         DROP  R4                                                               
*                                                                               
SPPRT20  MVC   P+47(3),0(R6)       GET THE PRODUCT MNEMONIC                     
*                                                                               
SPPRT30  EDIT  (B1,9(R8)),(3,P+64),ZERO=NOBLANK                                 
         TM    0(R8),X'08'         THIS MEL'S COPIED BUYS                       
         BZ    *+8                                                              
         MVI   P+69,C'*'                                                        
*                                                                               
         LLC   R0,10(R8)           GET THE NEW BUY NUMBER                       
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BNE   *+8                 NO                                           
         ICM   R0,3,10(R8)         YES                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         EDIT  (P8,DUB),(3,P+111),ZERO=NOBLANK                                  
         LLC   RF,NSHL+1           1 BYTE BUYLINE                               
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BNE   *+8                 NO                                           
         ICM   RF,3,NSHL           YES                                          
         SR    R0,RF               GET THE OLD BUY NUMBER                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         EDIT  (P8,DUB),(3,P+86),ZERO=NOBLANK                                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
***                                                                             
* THIS ROUTINE PRINTS OUT #RECORDS CHANGED                                      
***                                                                             
REPRT    NTR1                                                                   
         LH    R0,COUNT                                                         
         EDIT  (R0),(5,P+4),ZERO=NOBLANK                                        
*                                                                               
         CLI   KEYSAVE,X'10'       TEST BUY RECORDS                             
         JNH   *+12                                                             
         LA    R1,=CL32'  BUY RECORDS CHANGED'                                  
         J     REP20                                                            
*                                                                               
         CLI   KEYSAVE,X'0B'       TEST INVOICE RECORDS                         
         JNE   *+12                                                             
         LA    R1,=CL32'  OLD INV RECORDS CHANGED'                              
         J     REP20                                                            
*                                                                               
         CLI   KEYSAVE,SIRKTYPQ    TEST NSID RECORDS                            
         JNE   *+12                                                             
         LA    R1,=CL32'  NSID RECORDS CHANGED'                                 
         J     REP20                                                            
*                                                                               
         LAY   R1,RECTAB           POINT TO RECORD TABLE                        
REP10    CLI   0(R1),X'FF'                                                      
         JE    REPERR                                                           
         CLC   0(2,R1),KEYSAVE     TEST FOR REC TYPE                            
         JE    REP20                                                            
         LA    R1,L'RECTAB(R1)     BUMP TO NEXT ENTRY                           
         J     REP10                                                            
*                                                                               
REP20    MVC   P+10(L'RECTAB-2),2(R1)                                           
         GOTOR REPORT                                                           
         J     REPX                                                             
*                                                                               
REPERR   DS    0H                                                               
         MVC   P+10(32),=C'CHANGED RECORD TYPE NOT IN TABLE'                    
         GOTOR HEXOUT,DMCB,KEY,P+45,13,=C'TOG',0                                
         GOTOR REPORT                                                           
*                                                                               
*                                                                               
REPX     J     EXIT                                                             
         EJECT                                                                  
* ERROR ROUTINES *                                                              
         SPACE                                                                  
STAEQERR DS    0H                                                               
         XC    P,P                                                              
         MVC   P+10(37),=C'OLD AND NEW STATION CALL LETTERS SAME'               
         SPACE                                                                  
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         SPACE 2                                                                
* THIS SECTION PRINTS A TRACE RECORD OF THE KEY *                               
         SPACE                                                                  
SPTRACE  NTR1                                                                   
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVC   P(1),TRACECDE                                                    
         GOTOR HEXOUT,DMCB,KEY,P+10,18,=C'MIX',0                                
SPTRACE2 DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTOR REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         MVI   TRACECDE,0                                                       
         J     EXIT                                                             
***                                                                             
* SPPKG - THIS ROUTINE FIXES THE BUY LINE NUMBER IN PKG ELEMENT                 
***                                                                             
SPPKG    NTR1                                                                   
         XR    R5,R5               CLEAR R5                                     
         LA    RE,24(R8)           POINT TO 1ST ELEMENT                         
*                                                                               
SPPKG10  LLC   R0,1(RE)            ELEMENT LENGTH                               
         AR    RE,R0               BUMP TO NEXT ELEMENT                         
         CLI   0(RE),0             END OF RECORD?                               
         BE    EXIT                YES                                          
         CLI   0(RE),5             PACKAGE ELEMENT?                             
         BNE   SPPKG10             NO                                           
         LLC   R0,1(RE)            ELEMENT LENGTH                               
         SHI   R0,3                R0 HAS THE NUMBER OF LINE NUMBERS            
         BP    *+6                 AT LEAST ONE LINE NUMBER?                    
         DC    H'0'                NO - DEATH                                   
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BNE   *+8                 NO                                           
         SRL   R0,1                YES - DIVIDE BY 2                            
         LA    RE,3(RE)            POINT TO 1ST LINE NUMBER                     
*                                                                               
SPPKG20  CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BE    SPPKG30             YES                                          
         IC    R5,0(RE)            BUYLINE NUMBER                               
         AR    R5,R6               NEW BUYLINE                                  
         STC   R5,0(RE)            ADJUST BUY LINE NUMBER                       
         LA    RE,1(RE)            BUMP TO NEXT BUYLINE NUMBER                  
         B     SPPKG40             DONE PROCESSING THIS BUYLINE                 
*                                                                               
SPPKG30  ICM   R5,3,0(RE)          2-BYTE BUYLINE NUMBER                        
         AR    R5,R6               NEW BUYLINE                                  
         STCM  R5,3,0(RE)          ADJUST BUY LINE NUMBER                       
         LA    RE,2(RE)            BUMP TO NEXT BUYLINE NUMBER                  
*                                                                               
SPPKG40  BCT   R0,SPPKG20          PROCESS NEXT BUYLINE                         
         B     EXIT                EXIT                                         
         DROP  RB,R7,R3                                                         
***                                                                             
* HEADHOOK ROUTINE                                                              
***                                                                             
         DS    0F                                                               
         USING *,RF                                                             
SPSCHDHK NTR1                                                                   
         LM    R9,RB,SPSCR9                                                     
         L     R7,SPSCR7                                                        
         L     R3,SPSCR3                                                        
         DROP  RF                                                               
         USING SPSC02,RB,R7,R3                                                  
         MVC   H3+59(4),QMKT                                                    
         MVC   H4+64(4),QSTA                                                    
         LA    R1,H4+67                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         LA    R1,1(,R1)                                                        
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),QSTA+4                                                   
         CLI   1(R1),C'T'                                                       
         BNE   *+8                                                              
         MVI   2(R1),C'V'                                                       
         CLI   1(R1),C'R'                                                       
         BNE   *+8                                                              
         MVI   2(R1),C'M'                                                       
         SPACE                                                                  
         MVC   H5+64(4),NEWCALL                                                 
         LA    R1,H5+67                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         LA    R1,1(,R1)                                                        
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),NEWCALL+4                                                
         CLI   1(R1),C'T'                                                       
         BNE   *+8                                                              
         MVI   2(R1),C'V'                                                       
         CLI   1(R1),C'R'                                                       
         BNE   *+8                                                              
         MVI   2(R1),C'M'                                                       
         CLI   CANAGY,C'Y'         THIS A CANADIAN AGENCY REQUEST               
         BNE   EXIT                                                             
         OC    CANNET,CANNET      THIS A NETWORK                                
         BZ    EXIT                                                             
         MVC   H4+72(9),=C'NETWORK ='                                           
         MVC   H4+81(4),CANNET                                                  
         LA    R1,H4+84                                                         
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         LA    R1,1(,R1)                                                        
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),CANNET+4                                                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
EXIT1    XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         LTORG                                                                  
*                                                                               
* LOCAL WORKING STORAGE                                                         
*                                                                               
         DS    0D                                                               
COUNTS   DS    0CL20                                                            
REGCHAN  DC    PL4'0',CL16'REG CONTACTS CHG'                                    
GASCHAN  DC    PL4'0',CL16'GRADE ASSIGN CHG'                                    
LOGCHAN  DC    PL4'0',CL16'LOG ENTRIES CHG'                                     
INVCHAN  DC    PL4'0',CL16'INVOICES CHG'                                        
ORDCHAN  DC    PL4'0',CL16'ORD ENTRIES CHG'                                     
DUPLREC  DC    PL4'0',CL16'DUP RECORDS'                                         
NCOUNTS  EQU   ((*-COUNTS)/L'COUNTS)                                            
         EJECT                                                                  
* DATA *                                                                        
         SPACE                                                                  
*                                                                               
RTBNPRMS DS    6F                  RESTORE TABLE BINSRCH PARAMETER              
BKTPARMS DS    6F                  BUY KEY TABLE BINSRCH PARAMETER              
*                                                                               
SAVDA    DS    F                   SAVE DISK ADDR OF 1ST NEW INVOICE            
SPSCR9   DC    3F'0'               SAVE AREA FOR R9, RA AND RB                  
SPSCR7   DC     F'0'               SAVE AREA FOR R7                             
SPSCR3   DC     F'0'               SAVE AREA FOR R3                             
SAVCLT   DS    H                                                                
SAVEKEY  DC    XL20'00'                                                         
SAVEKEY2 DC    XL20'00'                                                         
         DC    C'**BKEY**'                                                      
BKEY2    DC    XL40'00'                                                         
BKEYSAV2 DC    XL40'00'                                                         
SAVEKEYO DC    XL40'00'                                                         
*                                                                               
**********************SVKEY    DC    XL13'00'                                   
NEWEXIST DC    CL40'* ERROR * NEW STATION 0A24 RECORD EXISTS'                   
BUYSW    DS    C                                                                
CLTBUYSW DC    C'N'                                                             
CANAGY   DS    C                                                                
OSHL     DS    XL2                 OLD STATION HIGH LINE #                      
*N        DS    XL2                                                             
NSHL     DS    XL2                 NEW STATION HIGH LINE #                      
*M        DS    XL2                                                             
TRACECDE DS    C                   TRACE CODE                                   
SAVEPRT  DS    CL132                                                            
*                                                                               
NEWCALL  DS    CL5                 NEW STATION CALL LETTERS                     
*                                                                               
OLDMSTA  DS    0CL5                  D BINARY MARKET/STATION                    
OLDMKT   DS    XL2                   D BINARY MARKET                            
OLDSTA   DS    CL3                   D BINARY STATION                           
*                                                                               
NEWMSTA  DS    0CL5                                                             
NEWMKT   DS    XL2                                                              
NEWSTA   DS    CL3                                                              
*                                                                               
* FOR CANADIAN D'TOP KEYS                                                       
*                                                                               
OLDMSTAN DS    0CL5                                                             
OLDMKTN  DS    XL2                                                              
OLDSTAN  DS    CL3                                                              
*                                                                               
NEWMSTAN DS    0CL5                                                             
NEWMKTN  DS    XL2                                                              
NEWSTAN  DS    CL3                                                              
*                                                                               
CANNMKT  DS    CL4                                                              
CANNET   DS    CL5                                                              
CNETMSTA DS    CL5                 CANADIAN NETWORK CALL LETTERS                
NETNIBLE DS    XL1                                                              
COUNT    DS    H                   RECORD COUNTER                               
ELCODE   DS    CL1                                                              
CHGEL    DS    CL1                                                              
SCHEME   DS    XL2                 NSID SCHEME CODE                             
***                                                                             
* STORAGE NEEDED TO PROCESS NEW STAFIX RECORDS                                  
***                                                                             
WEEK1    DS    CL2                 WEEK/YEAR IN BINARY FOR OLD REQUEST          
WEEK2    DS    CL2                 WEEK/YEAR IN BINARY FOR TODAY                
EDATE    DS    CL6                 TEMPORARY EBCDIC DATE STORAGE                
AGYMDN   DS    XL1                 AGENCY/MEDIA N (FOR MEDIA C REQUEST)         
*                                                                               
SKEY     DS    XL24 ************** UID IS COMMENTED OUT                         
SVSTKEY  DS    XL15 ************** UID IS COMMENTED OUT                         
*                                                                               
*CURUID   DS    XL6  ************** CURRENT UID                                 
*OLDUID   DS    XL6  ************** OLD ONE                                     
*NEWUID   DS    XL6  ************** NEW ONE, DUH                                
*STAIO    DS    XL(SCBLSQNQ)                                                    
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        +        
               MACRF=GM,EODAD=BRTX                                              
*                                                                               
         DROP  RB,R7,R3                                                         
*                                                                               
* TABLE USED TO PRINT OUT RECORD TOTALS                                         
*                                                                               
RECTAB   DS    0CL32                                                            
         DC    XL2'0D34',CL30'DARE ORDER RECORDS CHANGED    '                   
         DC    XL2'0D35',CL30'DARE BATCH RECORDS CHANGED    '                   
         DC    XL2'0D3D',CL30'DESTINATION RECORDS CHANGED   '                   
         DC    XL2'0D3E',CL30'LAST METHOD RECORDS CHANGED   '                   
         DC    XL2'0D59',CL30'SID RECORDS CHANGED           '                   
         DC    XL2'0D72',CL30'STATION LOCK-IN RECS CHANGED  '                   
         DC    XL2'0D73',CL30'STATION LOCK-IN XRECS CHANGED '                   
         DC    XL2'0D7A',CL30'WILA PROFIT WITHIN            '                   
         DC    XL2'0D7B',CL30'DOUBLE BOOKING RECS CHANGED   '                   
         DC    XL2'0A22',CL30'PATTERN RECORDS CHANGED       '                   
         DC    XL2'0A24',CL30'INST RECAP RECORDS CHANGED    '                   
         DC    XL2'0A25',CL30'SHIP RECAP RECORDS CHANGED    '                   
         DC    XL2'0A28',CL30'STAT ADDR RECORDS CHANGED     '                   
         DC    XL2'0A2E',CL30'BUY ACT/ESTM RECORDS CHANGED  '                   
         DC    XL2'0A2F',CL30'LABEL LIST RECORDS CHANGED    '                   
         DC    XL2'0A31',CL30'STATION LIST RECORDS CHANGED  '                   
         DC    XL2'0A32',CL30'TRAFFIC BUY RECORDS CHANGED   '                   
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE REPLACES THE CALL LETTERS   *                                    
*  IN THE CANADIAN NETWORK STATION ELEMENT *                                    
***********************************************************************         
SPCNETWK NMOD1 0,SPCNETWK                                                       
         USING SPSC02+8192,R3                                                   
         LA    R6,24(R8)           POINT TO THE FIRST ELEMENT                   
SPCNT10  DS    0H                                                               
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             IS THIS THE END OF THE RECORD?               
         BE    SPCNTX                                                           
         CLC   =X'6806',0(R6)      IS THIS A CANADIAN NTWK ELT PTR?             
         BNE   SPCNT10                                                          
         MVC   CANNET(4),2(R6)     GET THE NETWORK CALL LETTERS                 
         MVI   CANNET+4,C'N'                                                    
         MVC   CANNMKT,=17C'0'     NETWORK'S MARKET NUMBER IS ZERO              
         GOTO1 MSPACK,DMCB,CANNMKT,CANNET,CNETMSTA                              
         OC    CNETMSTA+2(2),CNETMSTA+2                                         
         BNZ   SPCNT14                                                          
         TM    CNETMSTA+4,X'E0'                                                 
         JZ    *+2                                                              
SPCNT14  XC    KEY,KEY                                                          
         MVC   KEY(10),0(R8)                                                    
         MVC   KEY+11(2),10(R8)                                                 
         MVC   KEY+4(5),CNETMSTA                                                
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   ERROR10                                                          
         CLC   KEY+11(1),KEYSAVE+11                                             
         BNE   ERROR10                                                          
         GOTO1 GETBUY                                                           
         LA    R6,24(R8)           POINT TO THE FIRST ELEMENT                   
SPCNT20  DS    0H                                                               
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             IS THIS THE END OF THE RECORD?               
         BE    ERROR20                                                          
         CLI   0(R6),X'68'         IS THIS A CANADIAN NETWORK ELEMENT?          
         BNE   SPCNT20              NO. TRY NEXT ELEMENT                        
         MVC   DOUBLE(5),OLDMSTA                                                
         NI    DOUBLE+4,X'00'      NETWORK NOW 8 BITS                           
         MVC   DUB(5),2(R6)                                                     
         NI    DUB+4,X'00'                                                      
         CLC   DOUBLE(5),DUB       IS THIS THE RIGHT 'OLD' MKT/STA?             
         BNE   SPCNT20                                                          
         MVC   NETNIBLE,6(R6)     SAVE NIBBLE                                   
*        NI    NETNIBLE,X'1F'     ONLY SAVE 5 BITS                              
         MVC   4(3,R6),NEWSTA      REPLACE CALL LETTERS                         
         SPACE                                                                  
         NI    6(R6),X'00'         GET RID OF ANY BUMMER BITS                   
         SPACE                                                                  
         OC    6(1,R6),NETNIBLE   RESTORE NETWORK NIBBLE                        
         SPACE                                                                  
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   SPCNT25              NO.                                         
         SPACE                                                                  
         GOTO1 PUTBUY                                                           
         SPACE                                                                  
SPCNT25  DS    0H                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   SPCNT30                                                          
         MVC   SAVEPRT(132),P      *** TRACE ***                                
         XC    P,P                                                              
         MVI   P,C'9'                                                           
         GOTO1 HEXOUT,DMCB,0(R6),P+10,11,=C'MIX',0                              
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT                                                   
SPCNT30  DS    0H                                                               
         MVC   KEY(13),SAVEKEY     RESTORE THE BUY RECORD                       
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         GOTO1 GETBUY                                                           
SPCNTX   XIT1                                                                   
         SPACE                                                                  
ERROR10  DS    0H                                                               
         XC    P,P                                                              
         MVC   P+10(33),=C'CANADIAN NETWORK RECORD NOT FOUND'                   
         B     ERRORX                                                           
         SPACE                                                                  
ERROR20  DS    0H                                                               
         XC    P,P                                                              
         MVC   P+10(27),=C'BAD CANADIAN NETWORK RECORD'                         
*                                                                               
ERRORX   DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
***********************************************************************         
* FIND THE HIGHEST LINE NUMBER (OSHL) FOR THE OLD STATION AND                   
*  THEN FINDS THE HIGHEST LINE NUMBER (NSHL) FOR NEW STATION                    
***********************************************************************         
HIGHBUY  NMOD1 0,HIGHBUY                                                        
         USING SPSC02+8192,R3                                                   
         MVI   USRSW1,C'N'         SET USRSW1 TO LESS THAN 255 BUYS             
         MVC   SAVEKEY(13),KEY                                                  
         GOTO1 HIGH                                                             
HBUY00   CLC   KEY(10),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST?                
         BNE   HBUY10                                                           
         CLI   KEY+10,X'80'        DON'T COUNT SPILL POINTERS!!                 
         BE    HBUY10              DONE WHEN YOU GET ONE.                       
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 SEQ                                                              
         B     HBUY00                                                           
*                                                                               
HBUY10   MVI   OSHL,0              1 BYTE BUYLINE                               
         MVC   OSHL+1(1),KEYSAVE+11 SET HIGHEST LINE# IN OLD STA                
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BNE   *+10                NO                                           
         MVC   OSHL,KEYSAVE+11     YES - 2-BYTE BUYLINE                         
         MVC   KEY(13),SAVEKEY                                                  
         MVC   KEY+6(3),NEWSTA                                                  
         CLI   CANAGY,C'Y'                                                      
         BE    HBUY12                                                           
         CLI   KEY+6,X'E8'         TEST SF CABLE                                
         BL    HBUY12                                                           
         MVC   DUB(3),SAVEKEY+6    MOVE SF STATION                              
         NI    DUB+2,X'7F'         DROP BIT THAT IS PART OF STATION             
         OC    KEY+8(1),DUB+2      'OR' IN CABLE NTWK                           
*                                                                               
HBUY12   XC    KEY+11(2),KEY+11    CLEAR FOR 2-BYTES                            
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
*                                                                               
HBUY20   CLC   KEY(10),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST?                
         BNE   HBUY30                                                           
         CLI   KEY+10,X'80'        DON'T COUNT SPILL POINTERS!!                 
         BE    HBUY30              DONE WHEN YOU GET ONE.                       
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 SEQ                                                              
         B     HBUY20                                                           
*                                                                               
HBUY30   MVI   NSHL,0              1 BYTE BUYLINE                               
         MVC   NSHL+1(1),KEYSAVE+11      SET HIGHEST LINE# IN NEW STA           
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BNE   *+10                NO                                           
         MVC   NSHL,KEYSAVE+11 YES - 2-BYTE BUYLINE                             
         XR    RF,RF               CLEAR RF                                     
         ICM   RF,3,NSHL           HIGHEST BUY IN NEW STA                       
         XR    RE,RE               CLEAR RE                                     
         ICM   RE,3,OSHL           HIGHEST BUY IN OLD STA                       
         AR    RF,RE               NEXT BUYLINE NUMBER                          
         LA    RE,255              MAX 255 FOR 1 BYTE BUYLINES                  
         CLI   VGETBUY,2           2-BYTE BUYLINE?                              
         BNE   *+8                 NO                                           
         LA    RE,999              MAX 999 FOR 2-BYTE BUYLINES                  
         CR    RF,RE               ARE THERE TOO MANY BUYS?                     
         BH    HBUYERR             ERROR - TOO MANY BUYS                        
*                                                                               
HBUY40   MVC   KEY(13),SAVEKEY     RESTORE LAST KEY READ BY CONTROLLER          
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
HBUYX    XIT1                                                                   
*                                                                               
HBUYERR  XC    P,P                                                              
         MVC   P+5(3),CLT          GET THE CLIENT CODE                          
         MVC   P+10(33),=CL33'ERROR-TOO MANY BUYS BOTH STATIONS'                
         L     R4,ADCLT                                                         
         USING CLTHDRD,R4                                                       
         LA    R4,CLIST                                                         
         DROP  R4                                                               
*                                                                               
HBUYER10 CLC   3(1,R4),SAVEKEY+3   IS THIS THE SAME PRODUCT CODE?               
         BE    HBUYER20                                                         
         LA    R4,4(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNL   HBUYER10                                                         
         MVC   P+47(15),=CL15'*** UNKNOWN PRD'                                  
         B     HBUYER30                                                         
*                                                                               
HBUYER20 MVC   P+47(3),0(R4)       GET THE PRODUCT MNEMONIC                     
*                                                                               
HBUYER30 MVC   DUB(1),SAVEKEY+9                                                 
         EDIT  (B1,DUB),(3,P+64)                                                
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVI   USRSW1,C'Y'         MORE THAN MAX BUYS                           
         B     HBUY40                                                           
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
***********************************************************************         
* PW100 - THIS SECTION UPDATES DOUBLE BOOKING RECORDS *                         
***********************************************************************         
PW100    NMOD1 0,**PW100*                                                       
         USING SPSC02+8192,R3                                                   
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D7A'     PROFIT WITHIN RECORD                         
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    PW105               NO                                           
         MVC   KEY+3(2),SAVCLT     MOVE THE CLIENT IN                           
         SPACE                                                                  
PW105    GOTO1 HIGH                                                             
         B     PW120                                                            
*                                                                               
PW110    GOTO1 SEQ                                                              
*                                                                               
PW120    LA    R1,2                COMPARE FOR A LENGTH OF 3                    
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+8                 NO                                           
         LA    R1,4                YES - COMPARE THE CLIENT AS WELL             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE      SAME 0D7A/A-M/CLT?                           
         BNE   PW190                NO. END OF THIS REC TYPE/AM                 
         SPACE                                                                  
         CLC   OLDSTA,KEY+9                                                     
         BE    PW124                                                            
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    PW110                                                            
         MVC   FULL(3),KEY+9                                                    
         NI    FULL+2,X'80'                                                     
         CLC   OLDSTA,FULL                                                      
         BNE   PW110                NO, NEXT                                    
*                                                                               
PW124    MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   PW130                                                            
*                                                                               
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVI   P+1,C'O'                                                         
         GOTO1 HEXOUT,DMCB,KEY,P+10,18,=C'MIX',0                                
         DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
*                                                                               
PW130    MVC   KEY+9(3),NEWSTA     DOES NEW RECORD ALREADY EXIST?               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    PW134                                                            
         MVC   BYTE,SAVEKEY+11     SAVE CABLE HEAD NETWORK                      
         NI    BYTE,X'7F'                                                       
         OC    KEY+11(1),BYTE      SET IN NETWORK                               
*                                                                               
PW134    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     SAME 0D7A/A-M/CLT/PRD/EST/MKT/STA            
         BE    PWDUPERR             YES, DUPLICATE KEYS (TSK,TSK)               
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE DATAMGR SEQ                          
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
*                                                                               
PW140    GOTO1 GET                                                              
*                                                                               
         GOTOR CRSTRTAB,DMCB,(C'S',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   PW110                       NO, READ NEXT                        
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   PW150                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC,DMWORK                   
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
PW150    MVC   DUB(1),11(R8)       SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+11                                                  
         MVC   9(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+9(3),NEWSTA                                                  
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    PW156                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    11(1,R8),DUB        SET IN NETWORK                               
         OC    KEY+11(1),DUB       SET IN NETWORK                               
*                                                                               
PW156    NI    15(R8),X'7F'        UNDELETE THE RECORD                          
         CLI   QOPT5,C'Y'                                                       
         BNE   PW160                                                            
*                                                                               
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVI   P+1,C'I'                                                         
         GOTO1 HEXOUT,DMCB,KEY,P+10,18,=C'MIX',0                                
         DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         SPACE                                                                  
PW160    CLI   RCWRITE,C'Y'                                                     
         BNE   PW170                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,AREC,DMWORK                   
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
PW170    LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
*                                                                               
PW180    MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         OI    DMINBTS,X'08'       CHECK DELETED RECS TOO                       
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),SAVEKEY                                                  
         BE    PW110                                                            
         DC    H'0'                                                             
*                                                                               
* PRINT ERROR FOR P/W REC ALREADY ON NEW STATION *                              
*                                                                               
PWDUPERR DS    0H                                                               
         MVC   P+10(4),=C'CLT='                                                 
         GOTO1 CLUNPK,DMCB,KEY+3,P+14                                           
         MVC   P+19(4),=C'PRD='                                                 
         GOTO1 HEXOUT,(R1),KEY+5,P+23,1,=C'MIX'                                 
*                                  EDIT ESTIMATE NUMBER                         
         MVC   P+27(4),=C'EST='                                                 
         EDIT  (B1,KEY+6),(3,P+31),ZERO=NOBLANK                                 
         GOTO1 MSUNPK,DMCB,KEY+7,P+40,P+50                                      
         MVC   P+60(40),=CL40'* ERROR * P/W REC EXISTS FOR NEW STATION'         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     PW180                                                            
*                                                                               
PW190    XIT1                                                                   
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* THIS SECTION IS COMMENTED OUT BY AKAT AS PER ABEA AS OF 8/3/04      *         
***********************************************************************         
***********************************************************************         
* PW200 - THIS SECTION UPDATES STATION LOCKIN RECORDS *                         
***********************************************************************         
PW200    NMOD1 0,**PW200*                                                       
         USING SPSC02+8192,R3                                                   
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING SLHRECD,R6                                                       
         MVI   SLHKTYP,SLHKTYPQ                                                 
         MVI   SLHKSUB,SLHKSUBQ                                                 
         MVC   SLHKAGMD,BAGYMD                                                  
         SPACE                                                                  
         GOTO1 HIGH                                                             
         B     PW220                                                            
*                                                                               
PW210    GOTO1 SEQ                                                              
*                                                                               
PW220    DS    0H                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   PW224                                                            
         SPACE                                                                  
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVI   P+1,C'I'                                                         
         GOTO1 HEXOUT,DMCB,KEY,P+10,18,=C'MIX',0                                
         DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         SPACE                                                                  
PW224    CLC   KEY(3),KEYSAVE      SAME 0D73/A-M                                
         BNE   PW290                NO. END OF THIS REC TYPE/AM                 
         SPACE                                                                  
         CLC   OLDMSTA,KEY+5       IS THIS REQUESTED MARKET/STATION             
         BE    PW226                GO MOVE IT                                  
         SPACE                                                                  
         CLC   OLDSTA,KEY+7        IS THIS THE REQUESTED STATION                
         BE    MKTERR               WHY IS IT IN WRONG MARKET?                  
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    PW210                                                            
         MVC   FULL(3),KEY+7                                                    
         NI    FULL+2,X'80'                                                     
         CLC   OLDSTA,FULL                                                      
         BNE   PW210                NO, NEXT                                    
*                                                                               
PW226    MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   PW230                                                            
*                                                                               
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVI   P+1,C'O'                                                         
         GOTO1 HEXOUT,DMCB,KEY,P+10,18,=C'MIX',0                                
         DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
*                                                                               
PW230    MVC   KEY+7(3),NEWSTA     DOES NEW RECORD ALREADY EXIST?               
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    PW234                                                            
         MVC   BYTE,SAVEKEY+9      SAVE CABLE HEAD NETWORK                      
         NI    BYTE,X'7F'                                                       
         OC    KEY+09(1),BYTE      SET IN NETWORK                               
         SPACE                                                                  
PW234    GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE     SAME 0D73/A-M/CLT/MKT/STA/SEQ                
         BE    PWDUPERL             YES, DUPLICATE KEYS (TSK,TSK)               
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE DATAMGR SEQ                          
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
*                                                                               
PW240    GOTO1 GET                                                              
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   PW250                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC,DMWORK                   
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
PW250    MVC   DUB(1),9(R8)        SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+9                                                   
         MVC   7(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+7(3),NEWSTA                                                  
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    PW256                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    91(1,R8),DUB        SET IN NETWORK                               
         OC    KEY+9(1),DUB        SET IN NETWORK                               
         SPACE                                                                  
PW256    NI    15(R8),X'7F'        UNDELETE THE RECORD                          
         CLI   QOPT5,C'Y'                                                       
         BNE   PW260                                                            
*                                                                               
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVI   P+1,C'I'                                                         
         GOTO1 HEXOUT,DMCB,KEY,P+10,18,=C'MIX',0                                
         DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
*                                                                               
         SPACE                                                                  
PW260    CLI   RCWRITE,C'Y'                                                     
         BNE   PW270                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,AREC,DMWORK                   
         CLI   DM3,0                                                            
         JNE   *+2                                                              
         SPACE                                                                  
PW270    DS    0H                                                               
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
*                                                                               
PW280    MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         OI    DMINBTS,X'08'       CHECK DELETED RECS TOO                       
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),SAVEKEY                                                  
         BE    PW210                                                            
         DC    H'0'                                                             
*                                                                               
* PRINT ERROR FOR P/W REC ALREADY ON NEW STATION *                              
*                                                                               
PWDUPERL DS    0H                                                               
         MVC   P+10(4),=C'CLT='                                                 
         GOTO1 CLUNPK,DMCB,KEY+3,P+14                                           
         SPACE                                                                  
         GOTO1 MSUNPK,DMCB,KEY+5,P+40,P+50                                      
         MVC   P+60(40),=CL40'* ERROR * P/W REC EXISTS FOR NEW STATION'         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     PW280                                                            
*                                                                               
MKTERR   DS    0H                                                               
         MVC   P+10(4),=C'CLT='                                                 
         GOTO1 CLUNPK,DMCB,KEY+3,P+14                                           
         SPACE                                                                  
         GOTO1 MSUNPK,DMCB,KEY+5,P+40,P+50                                      
         MVC   P+60(37),=CL37'* ERROR * STATION EXISTS IN WRONG MKT'            
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     PW210                                                            
*                                                                               
* UPDATE NEW XSPFILE LOCKIN RECORDS HERE                                        
*                                                                               
PW290    DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* PW300 - THIS SECTION UPDATES XSPT STATION LOCK-IN RECORDS                     
***********************************************************************         
PW300    NMOD1 0,**PW300*                                                       
         USING SPSC02+8192,R3                                                   
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    XKEY,XKEY                                                        
         LA    R6,XKEY                                                          
K        USING XSLKRECD,R6                                                      
         MVI   K.XSLKKTYP,XSLKKTYPQ                                             
         MVI   K.XSLKKSUB,XSLKKSUBQ                                             
         MVC   K.XSLKKAGMD,BAGYMD                                               
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    PW305               NO                                           
         MVC   K.XSLKKCLT,SAVCLT   YES - READ FOR THIS CLIENT                   
         SPACE                                                                  
PW305    MVC   XKEYSAVE,XKEY                                                    
         MVC   SAVEKEYX,XKEY                                                    
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',XKEYSAVE,XKEY                     
         CLI   DM3,0                                                            
         BE    PW320                                                            
         DC    H'0'                                                             
*                                                                               
PW310    DS   0H                                                                
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'XSPDIR',XKEYSAVE,XKEY                     
*                                                                               
PW320    DS   0H                                                                
         CLI   QOPT5,C'Y'                                                       
         BNE   PW324                                                            
         SPACE                                                                  
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVI   P+1,C'I'                                                         
         GOTO1 HEXOUT,DMCB,XKEY,P+10,40,=C'MIX',0                               
         DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         SPACE                                                                  
PW324    DS   0H                                                                
         CLC   XKEY(2),XKEYSAVE    SAME 0D73                                    
         BNE   PW390                NO. END OF THIS REC TYPE                    
         SPACE                                                                  
         CLC   XKEY+17(1),XKEYSAVE+17  SAME AGMD                                
         BNE   PW390                    NO, END                                 
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    PW325                NO                                          
         CLC   XKEY+18(2),XKEYSAVE+18  SAME CLT?                                
         BNE   PW390                    NO, END                                 
         SPACE                                                                  
PW325    CLC   OLDMSTA,K.XSLKKMKT  IS THIS REQUESTED MARKET/STATION             
         BE    PW326                GO MOVE IT                                  
         CLC   OLDSTA,K.XSLKKSTA   IS THIS THE REQUESTED STATION                
         BE    MKTERRX              WHY IS IT IN WRONG MARKET?                  
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    PW310                                                            
         MVC   FULL(3),K.XSLKKSTA+2                                             
         NI    FULL+2,X'80'                                                     
         CLC   OLDSTA,FULL                                                      
         BNE   PW310                NO, NEXT                                    
*                                                                               
PW326    MVC   SAVEKEYX,XKEY       SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   PW330                                                            
*                                                                               
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVI   P+1,C'O'                                                         
         GOTO1 HEXOUT,DMCB,XKEY,P+10,4,=C'MIX',0                                
         GOTO1 HEXOUT,DMCB,K.XSLKKAGMD,P+15,15,=C'MIX',0                        
         DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
*                                                                               
PW330    MVC   K.XSLKKSTA(3),NEWSTA   DOES NEW RECORD ALREADY EXIST?            
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    PW334                                                            
         MVC   BYTE,SAVEKEYX+XSLKKSTA+2-XSLKRECD                                
         NI    BYTE,X'7F'                                                       
         OC    K.XSLKKSTA+2(1),BYTE    SET IN NETWORK                           
*                                                                               
PW334    MVC   XKEYSAVE,XKEY                                                    
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',XKEYSAVE,XKEY                     
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
         CLC   XKEY(32),XKEYSAVE   SAME 0D73/A-M/CLT/MKT/STA/ETC                
         BE    PXDUPERL             YES, DUPLICATE KEYS (TSK,TSK)               
*                                                                               
         MVC   XKEY,SAVEKEYX                                                    
         MVC   XKEYSAVE,SAVEKEYX                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',XKEYSAVE,XKEY                     
         CLI   DM3,0                                                            
         JNZ   *+2                                                              
*                                                                               
         CLC   XKEY(32),XKEYSAVE                                                
         JNE   *+2                                                              
*                                                                               
PW340    GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFILE',XKEY+36,AREC,DMWORK              
*                                                                               
         GOTOR CRSTRTAB,DMCB,(C'X',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   PW310                       NO, READ NEXT                        
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   PW350                                                            
         OI    XKEY+32,X'80'        MARK FOR DELETION                           
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'XSPDIR ',XKEY,XKEY                     
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
         L     R1,AREC                                                          
         OI    34(R1),X'80'        MARK RECORD FOR DELETION                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFILE',XKEY+36,AREC,DMWORK          
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                  SAVE POSSIBLE CABLE HEAD NETWORK             
PW350    DS   0H                                                                
         MVC   DUB(1),XSLKKSTA+2-XSLKRECD(R8)                                   
         MVC   DUB+1(1),K.XSLKKSTA+2                                            
         MVC   XSLKKSTA-XSLKRECD(,R8),NEWSTA    UPDATE CALL LETTERS             
         MVC   K.XSLKKSTA,NEWSTA                                                
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    PW356                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    XSLKKSTA+2-XSLKRECD(1,R8),DUB      SET IN NETWORK                
         OC    K.XSLKKSTA+2(1),DUB                 SET IN NETWORK               
*                                                                               
PW356    NI    XKEY+32,X'7F'       UNDELETE THE KEY                             
         L     R1,AREC                                                          
         NI    34(R1),X'7F'        UNDELETE THE RECORD                          
         CLI   QOPT5,C'Y'                                                       
         BNE   PW360                                                            
*                                                                               
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVI   P+1,C'I'                                                         
         GOTO1 HEXOUT,DMCB,XKEY,P+10,4,=C'MIX',0                                
         GOTO1 HEXOUT,DMCB,K.XSLKKAGMD,P+15,30,=C'MIX',0                        
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
*                                                                               
PW360    CLI   RCWRITE,C'Y'                                                     
         BNE   PW370                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFILE',XKEY+36,AREC,DMWORK              
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
PW370    LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
*                                                                               
         GOTO1 CLUNPK,DMCB,K.XSLKKCLT,P+10           PRINT CLT                  
         GOTO1 MSUNPK,DMCB,K.XSLKKMKT,P+17,P+22      PRINT MKT/STA              
         EDIT  (B1,K.XSLKKEST),(3,P+28),ZERO=NOBLANK PRINT EST                  
         MVC   P+33(1),K.XSLKKDPT                    PRINT DPT                  
         EDIT  (B1,K.XSLKKLEN),(3,P+36),ZERO=NOBLANK PRINT LEN                  
         EDIT  (B1,K.XSLKKLEN2),(3,P+40),ZERO=BLANK  PRINT LEN2                 
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
PW380    MVC   XKEY,SAVEKEYX       RESTORE KEY                                  
         OI    DMINBTS,X'08'       CHECK DELETED RECS TOO                       
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),=C'XSPDIR',XKEYSAVE,XKEY           
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
         NI    DMINBTS,X'F7'                                                    
         CLC   XKEY(32),SAVEKEYX                                                
         BE    PW310                                                            
         DC    H'0'                                                             
*                                                                               
* PRINT ERROR FOR P/W REC ALREADY ON NEW STATION *                              
*                                                                               
PXDUPERL DS    0H                                                               
         GOTO1 CLUNPK,DMCB,K.XSLKKCLT,P+10           PRINT CLT                  
         GOTO1 MSUNPK,DMCB,K.XSLKKMKT,P+17,P+22      PRINT MKT/STA              
         EDIT  (B1,K.XSLKKEST),(3,P+28),ZERO=NOBLANK PRINT EST                  
         MVC   P+33(1),K.XSLKKDPT                    PRINT DPT                  
         EDIT  (B1,K.XSLKKLEN),(3,P+36),ZERO=NOBLANK PRINT LEN                  
         EDIT  (B1,K.XSLKKLEN2),(3,P+40),ZERO=BLANK  PRINT LEN2                 
         MVC   P+45(48),=CL48'* ERROR * STATION LOCK-IN EXISTS FOR NEW +        
               STATION'                                                         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   XKEYSAVE,SAVEKEYX                                                
         B     PW380                                                            
*                                                                               
MKTERRX  DS    0H                                                               
         MVC   P+10(4),=C'CLT='                                                 
         GOTO1 CLUNPK,DMCB,K.XSLKKCLT,P+14                                      
         GOTO1 MSUNPK,DMCB,K.XSLKKMKT,P+20,P+30                                 
         MVC   P+40(39),=CL39'* ERROR * STATION IN WRONG MKT-X LOCKIN'          
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   XKEYSAVE,SAVEKEYX                                                
         B     PW310                                                            
*                                                                               
PW390    DS    0H                                                               
         MVC   KEYSAVE(2),=X'0D73' FORCE RIGHT TOTALS TO PRINT                  
         XIT1                                                                   
         LTORG                                                                  
         DS    0D                                                               
         DC    C'**XKEY**'                                                      
XKEY     DC    XL48'00'                                                         
XKEYSAVE DC    XL48'00'                                                         
SAVEKEYX DC    XL48'00'                                                         
         DROP  RB,R3,K                                                          
         EJECT                                                                  
***********************************************************************         
* DB100 - THIS SECTION UPDATES DOUBLE BOOKING RECORDS *                         
***********************************************************************         
DB100    NMOD1 0,DB100                                                          
         USING SPSC02+8192,R3                                                   
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DBLKEY,R4                                                        
         MVI   DBLKTYPE,X'0D'      DOUBLE BOOKING RECORD                        
         MVI   DBLKSTYP,X'7B'                                                   
         MVC   DBLKAGMD,SVAGYMD    A-M                                          
         GOTO1 HIGH                                                             
         B     DB120                                                            
*                                                                               
DB110    GOTO1 SEQ                                                              
*                                                                               
DB120    CLC   KEY(3),KEYSAVE      SAME 0D7B/A-M                                
         BNE   DB190                NO. END OF THIS REC TYPE/AM                 
         CLC   OLDSTA,DBLKSTA                                                   
         BE    DB124                                                            
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    DB110                                                            
         MVC   FULL(3),DBLKSTA                                                  
         NI    FULL+2,X'80'                                                     
         CLC   OLDSTA,FULL                                                      
         BNE   DB110                NO, NEXT                                    
*                                                                               
DB124    MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   DB130                                                            
*                                                                               
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVI   P+1,C'O'                                                         
         GOTO1 HEXOUT,DMCB,KEY,P+10,18,=C'MIX',0                                
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
*                                                                               
DB130    MVC   DBLKSTA,NEWMSTA     DOES NEW RECORD ALREADY EXIST?               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    DB134                                                            
         MVC   BYTE,SAVEKEY+7      SAVE CABLE HEAD NETWORK                      
         NI    BYTE,X'7F'                                                       
         OC    DBLKSTA+2(1),BYTE   SET IN NETWORK                               
*                                                                               
DB134    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     SAME 0D7B/A-M/YR/MO/STA                      
         BE    DBDUPERR             YES, DUPLICATE KEYS (TSK,TSK)               
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE DATAMGR SEQ                          
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
*                                                                               
DB140    GOTO1 GET                                                              
*                                                                               
         GOTOR CRSTRTAB,DMCB,(C'S',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   DB110                       NO, READ NEXT                        
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   DB150                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC,DMWORK                   
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
DB150    MVC   DUB(1),7(R8)        SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+7                                                   
         MVC   5(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+5(3),NEWSTA                                                  
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    DB156                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    7(1,R8),DUB         SET IN NETWORK                               
         OC    KEY+7(1),DUB        SET IN NETWORK                               
*                                                                               
DB156    NI    15(R8),X'7F'        UNDELETE THE RECORD                          
         CLI   QOPT5,C'Y'                                                       
         BNE   DB160                                                            
*                                                                               
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVI   P+1,C'I'                                                         
         GOTO1 HEXOUT,DMCB,KEY,P+10,18,=C'MIX',0                                
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
*                                                                               
DB160    CLI   RCWRITE,C'Y'                                                     
         BNE   DB170                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,AREC,DMWORK                   
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
DB170    LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
*                                                                               
DB180    MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         OI    DMINBTS,X'08'       CHECK DELETED RECS TOO                       
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),SAVEKEY                                                  
         BE    DB110                                                            
         DC    H'0'                                                             
*                                                                               
* PRINT ERROR FOR D/B REC ALREADY ON NEW STATION FOR THIS YR MON *              
*                                                                               
DBDUPERR DS    0H                                                               
         GOTO1 MSUNPK,DMCB,KEY+3,P+10,P+20                                      
         GOTO1 HEXOUT,(R1),KEY+8,P+38,1,=C'MIX'                                 
         MVC   P+45(40),=CL40'* ERROR * D/B REC EXISTS FOR NEW STATION'         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     DB180                                                            
*                                                                               
DB190    XIT1                                                                   
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* DARORD - THIS SECTION UPDATES DARE ORDER RECORDS *                            
***********************************************************************         
DARORD   NMOD1 0,**DARO**                                                       
         USING SPSC02+8192,R3                                                   
         XC    COUNT,COUNT                                                      
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         MVI   DOKTYPE,DOKTYPQ     DARE ORDER RECORD                            
         MVI   DOKSUBTY,DOKSTYPQ                                                
         MVC   DOKAGMD,SVAGYMD     A-M                                          
         MVC   KEYSAVE,KEY                                                      
DO200    GOTO1 HIGH                                                             
         B     DO220                                                            
*                                                                               
DO210    GOTO1 SEQ                                                              
*                                                                               
DO220    CLC   KEY(5),KEYSAVE    SAME 0D/34/A-M                                 
         BNE   DO600                NO, DONE                                    
         CLC   DOKSTA,OLDSTA     STATION CALL LETTERS                           
         BE    DO230                YES                                         
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    DO210                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,7,DOKSTA                                                      
         ICM   RF,7,OLDSTA                                                      
         SRL   RE,7                DROP NET FROM CABLE HEAD                     
         SRL   RF,7                                                             
         CR    RE,RF               IS THIS THE SAME CABLE HEAD                  
         BNE   DO210                NO                                          
*                                                                               
DO230    MVC   SAVEKEY,KEY         SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   DO240                                                            
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,DOTRACE          ***TRACE***                                  
*                                                                               
DO240    MVC   DOKSTA,NEWSTA       DOES NEW DARE ORD ALREADY EXIST?             
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    DO250                                                            
         MVC   BYTE,SAVEKEY+11                                                  
         NI    BYTE,X'7F'                                                       
         OC    DOKSTA+2(1),BYTE    SET IN NETWORK                               
*                                                                               
DO250    MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     SAME 0D34/A-M/ORD/STA/TYPE                   
         BE    DO580                YES. DON'T MOVE                             
         MVC   KEY,SAVEKEY         NO. RESTORE KEY                              
         GOTO1 HIGH                                                             
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         CLI   DOKCMT,0            DARE SUPPORT REC?                            
         BNE   DO265                YES, MOVE IT                                
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    DO260                NO.                                         
*                                                                               
         L     R6,AREC                                                          
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         JNE   *+2                                                              
         USING DOIDELD,R6                                                       
         CLC   DOIDCLT,SAVCLT       MATCH ON CLIENT?                            
         BE    DO260                YES                                         
DO255    MVI   DOKCMT,X'FF'         NO, SKIP ALL COMMENT TYPES AND              
         B     DO200                GET NEXT ORDER                              
         DROP  R6                                                               
*                                                                               
DO260    GOTOR CRSTRTAB,DMCB,(C'S',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   DO255                                                            
*                                                                               
DO265    CLI   RCWRITE,C'Y'                                                     
         BNE   DO280                                                            
*                                                                               
         MVI   KEY+13,X'80'        CLEAR ALL AND MARK FOR DELETION              
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR ',KEY,KEY                       
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
         MVI   15(R8),X'80'        CLEAR ALL AND MARK FOR DELETION              
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFILE',KEY+14,AREC,DMWORK           
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
DO270    MVC   KEY+13(1),SAVEKEY+13   RESTORE THE STATUS BITS                   
         MVC   15(1,R8),SAVEKEY+13    RESTORE THE STATUS BITS                   
*                                                                               
DO280    MVC   DARCNET1,DOKSTA+2-DOKEY(R8)                                      
         MVC   DARCNET2,DOKSTA+2                                                
         MVC   DOKSTA-DOKEY(3,R8),NEWSTA      UPDATE CALL LETTERS               
         MVC   DOKSTA,NEWSTA                                                    
         CLI   SAVEKEY+DOKCMT-DOKEY,0  BYPASS AGY/REP COMMENTS                  
         BNE   DO290                                                            
*                                                                               
         L     R6,AREC                                                          
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         JNE   *+2                                                              
         USING DOIDELD,R6                                                       
         MVC   DAROLDST,DOISTA     SAVE OLD STATION                             
         MVC   DOISTA,NEWSTA                                                    
*                                                                               
DO290    CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    DO300                                                            
         NI    DARCNET1,X'7F'                                                   
         NI    DARCNET2,X'7F'                                                   
         CLC   DARCNET1,DARCNET2   BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    DOKSTA+2(1),DARCNET1 SET IN NETWORK                              
         CLI   SAVEKEY+DOKCMT-DOKEY,0  BYPASS AGY/REP COMMENTS                  
         BNE   DO300                                                            
         OC    DOISTA+2(1),DARCNET1 SET IN NETWORK                              
         DROP  R6                                                               
*                                                                               
DO300    CLI   QOPT5,C'Y'                                                       
         BNE   DO310                                                            
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,DOTRACE          ***TRACE***                                  
*                                                                               
DO310    CLI   RCWRITE,C'Y'                                                     
         BNE   DO320                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,=C'SPTFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         JNE   *+2                                                              
DO320    MVC   SAVDA,KEY+14        SAVE DISK ADDR OF 1ST ADD                    
*                                                                               
* DELETE/ADD PASSIVE KEYS *                                                     
*                                                                               
         CLI   SAVEKEY+DOKCMT-DOKEY,0  BYPASS AGY/REP COMMENTS                  
         BNE   DO490                                                            
*                                                                               
* UPDATE 0DB4                                                                   
*                                                                               
DO400    XC    KEY,KEY                                                          
         MVI   DBKTYPE,DBKTYPQ                                                  
         MVI   DBKSUBTY,DBKSTYPQ                                                
         MVC   DBKAGMD,SVAGYMD                                                  
         L     R6,AREC                                                          
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         JNE   *+2                                                              
         USING DOIDELD,R6                                                       
         MVC   DBKBYR,DOIDBYR                                                   
         MVC   DBKORD,SAVEKEY+DOKORDER-DOKEY                                    
         MVC   DBKSTA,SAVEKEY+DOKSTA-DOKEY                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         MVI   KEY+13,C'S'         PASSIVE POINTER - DELETE RECORD              
         CLI   QOPT5,C'Y'                                                       
         BNE   DO405                                                            
         BAS   RE,DOTRACE          *** TRACE ***                                
*                                                                               
DO405    CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   DO410                                                            
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY,KEY                                
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
         MVC   DBKSTA,NEWSTA       UPDATE CALL LETTERS                          
         MVI   KEY+13,0            PASSIVE POINTER - UNDELETE RECORD            
         MVC   KEY+14(4),SAVDA                                                  
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    *+10                                                             
         OC    DBKSTA+2(1),DARCNET1 SET IN NETWORK                              
*                                                                               
         GOTO1 DATAMGR,DMCB,DMADD,SPTDIR,KEY,KEY,DMWORK                         
         TM    DM3,X'DD'          ALLOW DUP KEY ON ADD OR DELETED REC           
         JNZ   *+2                 ERROR                                        
*                                                                               
* UPDATE 0DB5                                                                   
*                                                                               
DO410    XC    KEY,KEY                                                          
         MVI   DCKTYPE,DCKTYPQ                                                  
         MVI   DCKSUBTY,DCKSTYPQ                                                
         MVC   DCKAGMD,SVAGYMD                                                  
         MVC   DCKCLT,DOIDCLT                                                   
         MVC   DCKPRD,DOIDPRD                                                   
         MVC   DCKEST,DOIDEST                                                   
         MVC   DCKSTA,DAROLDST                                                  
         MVC   DCKPRD2,DOIDPRD2                                                 
         MVC   DCKFLTNM,DOIDFLTN                                                
*                                                                               
         ST    R6,FULL             SAVE ADDRESS OF DOIDELD                      
         L     R6,AREC                                                          
         MVI   ELCODE,X'03'        READ THE SUPP ELEM                           
         BAS   RE,GETEL                                                         
         JNE   *+2                                                              
         USING DOSPELD,R6                                                       
         MVC   SVMKT,DOSPMKT                                                    
*                                                                               
         TM    DOSPFLG1,DOSPTRDE   IS THIS TRADE ORDER?                         
         BZ    *+8                 NO                                           
         OI    DCKFLAG,DCKFTRDE    YES, READ TRADE PASSIVE KEY                  
*                                                                               
         L     R6,FULL             RESTORE ADDRESS OF DOIDELD                   
         USING DOIDELD,R6                                                       
*                                                                               
         OI    DMINBTS,X'08'       PASS BACK THE DELETED RECORDS                
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         MVC   SAVEKEY2,KEY                                                     
         NI    KEY+12,X'FF'-DCKFSCSW  CLEAR SSC BIT                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         MVC   KEY,SAVEKEY2                                                     
         MVI   KEY+13,C'S'         PASSIVE POINTER - DELETE RECORD              
         CLI   QOPT5,C'Y'                                                       
         BNE   DO415                                                            
         BAS   RE,DOTRACE          *** TRACE ***                                
*                                                                               
DO415    CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   DO420                                                            
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY,KEY                                
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
         MVC   DCKSTA,NEWSTA       UPDATE CALL LETTERS                          
         MVI   KEY+13,0            PASSIVE POINTER - UNDELETE RECORD            
         MVC   KEY+14(4),SAVDA                                                  
         OI    KEY+12,DCKFSCSW                                                  
*                                                                               
         L     R6,FULL             RESTORE ADDRESS OF DOIDELD                   
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    *+10                                                             
         OC    DCKSTA+2(1),DARCNET1 SET IN NETWORK                              
*                                                                               
         GOTO1 DATAMGR,DMCB,DMADD,SPTDIR,KEY,KEY,DMWORK                         
         TM    DM3,X'DD'          ALLOW DUP KEY ON ADD OR DELETED REC           
         JNZ   *+2                 ERROR                                        
*                                                                               
* UPDATE 0DB7                                                                   
*                                                                               
DO420    XC    KEY,KEY                                                          
         MVI   DSKTYPE,DSKTYPQ                                                  
         MVI   DSKSUBTY,DSKSTYPQ                                                
         MVC   DSKAGMD,SVAGYMD                                                  
         MVC   DSKBYR,DOIDBYR                                                   
         MVC   DSKSTA,SAVEKEY+DOKSTA-DOKEY                                      
         MVC   DSKORD,SAVEKEY+DOKORDER-DOKEY                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         MVI   KEY+13,C'S'         PASSIVE POINTER - DELETE RECORD              
         CLI   QOPT5,C'Y'                                                       
         BNE   DO425                                                            
         BAS   RE,DOTRACE          *** TRACE ***                                
*                                                                               
DO425    CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   DO430                                                            
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY,KEY                                
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
         MVC   DSKSTA,NEWSTA       UPDATE CALL LETTERS                          
         MVI   KEY+13,0            PASSIVE POINTER - UNDELETE RECORD            
         MVC   KEY+14(4),SAVDA                                                  
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    *+10                                                             
         OC    DSKSTA+2(1),DARCNET1 SET IN NETWORK                              
*                                                                               
         GOTO1 DATAMGR,DMCB,DMADD,SPTDIR,KEY,KEY,DMWORK                         
         TM    DM3,X'DD'          ALLOW DUP KEY ON ADD OR DELETED REC           
         JNZ   *+2                 ERROR                                        
*                                                                               
* UPDATE 0DBD                                                                   
*                                                                               
DO430    OC    SVMKT,SVMKT         HAVE A MARKET?                               
         BZ    DO490               NO, DON'T BOTHER WITH THE PASSIVES           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   DCMKTYPE,DCMKTYPQ                                                
         MVI   DCMKSTYP,DCMKSTYQ                                                
         MVC   DCMKAGMD,SVAGYMD                                                 
         L     R6,FULL             RESTORE ADDRESS OF DOIDELD                   
         USING DOIDELD,R6                                                       
         MVC   DCMKCLT,DOIDCLT                                                  
         MVC   DCMKPRD,DOIDPRD                                                  
         MVC   DCMKEST,DOIDEST                                                  
         MVC   DCMKMKT,SVMKT                                                    
         MVC   DCMKORDR,SAVEKEY+DOKORDER-DOKEY                                  
         DROP  R6                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         MVI   KEY+13,C'S'         PASSIVE POINTER - DELETE RECORD              
         CLI   QOPT5,C'Y'                                                       
         BNE   DO435                                                            
         BAS   RE,DOTRACE          *** TRACE ***                                
*                                                                               
DO435    CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   DO440                                                            
*                                                                               
         MVC   KEY+14(4),SAVDA                                                  
         MVI   KEY+13,0            PASSIVE POINTER - UNDELETE RECORD            
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY,KEY                                
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
* UPDATE 0DBE                                                                   
*                                                                               
DO440    XC    KEY,KEY                                                          
         MVI   DBCKTYPE,DBCKTYPQ                                                
         MVI   DBCKSTYP,DBCKSTYQ                                                
         MVC   DBCKAGMD,SVAGYMD                                                 
         L     R6,FULL             RESTORE ADDRESS OF DOIDELD                   
         USING DOIDELD,R6                                                       
         MVC   DBCKCLT,DOIDCLT                                                  
         MVC   DBCKMKT,SVMKT                                                    
         MVC   DBCKORDR,SAVEKEY+DOKORDER-DOKEY                                  
         GOTO1 VRCPACK,DMCB,(C'P',DOIDBYR),DBCKBYR                              
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         MVI   KEY+13,C'S'         PASSIVE POINTER - DELETE RECORD              
         CLI   QOPT5,C'Y'                                                       
         BNE   DO445                                                            
         BAS   RE,DOTRACE          *** TRACE ***                                
*                                                                               
DO445    CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   DO450                                                            
*                                                                               
         MVC   KEY+14(4),SAVDA                                                  
         MVI   KEY+13,0            PASSIVE POINTER - UNDELETE RECORD            
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY,KEY                                
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
* UPDATE 0DBF                                                                   
*                                                                               
DO450    XC    KEY,KEY                                                          
         MVI   DBMKTYPE,DBMKTYPQ                                                
         MVI   DBMKSTYP,DBMKSTYQ                                                
         MVC   DBMKAGMD,SVAGYMD                                                 
         L     R6,FULL             RESTORE ADDRESS OF DOIDELD                   
         USING DOIDELD,R6                                                       
         MVC   DBMKMKT,SVMKT                                                    
         MVC   DBMKCLT,DOIDCLT                                                  
         MVC   DBMKORDR,SAVEKEY+DOKORDER-DOKEY                                  
         GOTO1 VRCPACK,DMCB,(C'P',DOIDBYR),DBCKBYR                              
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         MVI   KEY+13,C'S'         PASSIVE POINTER - DELETE RECORD              
         CLI   QOPT5,C'Y'                                                       
         BNE   DO455                                                            
         BAS   RE,DOTRACE          *** TRACE ***                                
*                                                                               
DO455    CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   DO490                                                            
*                                                                               
         MVC   KEY+14(4),SAVDA                                                  
         MVI   KEY+13,0            PASSIVE POINTER - UNDELETE RECORD            
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY,KEY                                
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
* PRINT THE DARE ORDER RECORD *                                                 
*                                                                               
DO490    LH    R1,COUNT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,COUNT            # RECORDS CHANGED                            
*                                                                               
         CLI   SAVEKEY+DOKCMT-DOKEY,0  BYPASS AGY/REP COMMENTS                  
         BNE   DO550                                                            
*                                                                               
         L     R6,FULL                                                          
         USING DOIDELD,R6                                                       
         GOTO1 CLUNPK,DMCB,DOIDCLT,P+10            PRINT CLIENT                 
         MVC   WORK(2),SVMKT                                                    
         MVC   WORK+2(3),NEWSTA                                                 
         GOTO1 MSUNPK,DMCB,WORK,P+17,P+22          PRINT MKT/STA                
         EDIT  (B1,DOIDEST),(3,P+28),ZERO=NOBLANK  PRINT EST                    
         DROP  R6                                                               
*                                                                               
DO550    DS    0H                                                               
         GOTOR PRTORD,DMCB,SAVEKEY+DOKORDER-DOKEY,P+35                          
*                                                                               
         EDIT  (B1,SAVEKEY+DOKCMT-DOKEY),(1,P+45),ZERO=NOBLANK                  
         CLI   SAVEKEY+DOKCMT-DOKEY,0                                           
         BE    DO560                                                            
         MVC   P+46(8),=C'=AGY CMT'                                             
         CLI   SAVEKEY+DOKCMT-DOKEY,1                                           
         BE    DO560                                                            
         MVC   P+46(8),=C'=REP CMT'                                             
         CLI   SAVEKEY+DOKCMT-DOKEY,2                                           
         BE    DO560                                                            
         MVC   P+46(8),=C'=HISTORY'                                             
         CLI   SAVEKEY+DOKCMT-DOKEY,5                                           
         BE    DO560                                                            
         MVC   P+47(7),=C'???????'                                              
DO560    MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
DO570    MVC   KEY,SAVEKEY         RESTORE KEY                                  
         OI    DMINBTS,X'08'       PASS BACK THE DELETED RECORDS                
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),SAVEKEY                                                  
         BE    DO210                                                            
         DC    H'0'                                                             
*                                                                               
* PRINT ERROR FOR DAR ORD ALREADY ON NEW STATION FOR THIS DATE *                
*                                                                               
DO580    GOTOR PRTORD,DMCB,SAVEKEY+DOKORDER-DOKEY,P+35                          
*                                                                               
         EDIT  (B1,SAVEKEY+DOKCMT-DOKEY),(1,P+45),ZERO=NOBLANK                  
         CLI   SAVEKEY+DOKCMT-DOKEY,0                                           
         BE    DO590                                                            
         MVC   P+46(8),=C'=AGY CMT'                                             
         CLI   SAVEKEY+DOKCMT-DOKEY,1                                           
         BE    DO590                                                            
         MVC   P+46(8),=C'=REP CMT'                                             
         CLI   SAVEKEY+DOKCMT-DOKEY,2                                           
         BE    DO590                                                            
         MVC   P+46(8),=C'=HISTORY'                                             
         CLI   SAVEKEY+DOKCMT-DOKEY,5                                           
         BE    DO590                                                            
         MVC   P+47(7),=C'???????'                                              
DO590    MVC   P+55(40),=CL40'* ERROR * DAR ORD ALREADY ON NEW STATION'         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     DO570                                                            
         DROP  R4                                                               
*                                                                               
DO600    LH    R0,COUNT                                                         
         EDIT  (R0),(5,P+4),ZERO=NOBLANK                                        
         MVC   P+10(26),=CL26'DARE ORDER RECORDS CHANGED'                       
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
* THIS SECTION PRINTS A TRACE RECORD OF THE KEY *                               
*                                                                               
DOTRACE  NTR1                                                                   
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVC   P(1),TRACECDE                                                    
         GOTO1 HEXOUT,DMCB,KEY,P+10,13,=C'MIX',0                                
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         MVI   TRACECDE,0                                                       
         XIT1                                                                   
         LTORG                                                                  
DAROLDST DS    CL3                                                              
DARCNET1 DS    CL1                                                              
DARCNET2 DS    CL1                                                              
         DROP  RB,R3                                                            
*&&DO                                                                           
*                                                                               
* NWS - THIS SECTION UPDATES NWS RECORDS *                                      
*                                                                               
NWS      NMOD1 0,**+NWS**                                                       
         USING SPSC02+8192,R3                                                   
         XC    SAVECAM,SAVECAM                                                  
         XC    SAVEHDR,SAVEHDR                                                  
         XC    NWSTOTS,NWSTOTS                                                  
*                                                                               
* -- READ THROUGH CAMP RECORDS - AND IF CLT REQUESTED MATCH ON IT               
*                                                                               
         LA    R6,KEY                                                           
         USING CAMRECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   CAMKTYP,CAMKTYPQ   X'0D'                                         
         MVI   CAMKSUB,CAMKSUBQ   X'66'                                         
         MVC   CAMKAGMD,BAGYMD    A/M                                           
         GOTO1 HIGH                                                             
*                                                                               
NW100    CLC   KEY(3),KEYSAVE     ANY RECORDS FOR AGMD?                         
         BNE   NW300                                                            
*                                                                               
         CLC   =C'ALL',QCLT       SPECIFIC CLIENT REQUESTED?                    
         BE    NW110              NO - USE THIS RECORD                          
*                                                                               
         GOTO1 GET                                                              
         L     R6,AREC                                                          
         CLC   CAMCLT,BCLT        MUST MATCH ON CLIENT                          
         BE    NW110                                                            
         GOTO1 SEQ                TRY NEXT CAMPAIGN RECORD                      
         B     NW100                                                            
*                                                                               
NW110    MVC   SAVECAM(13),KEY    SAVE CAMPAIGN KEY                             
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,13                                                      
         BAS   RE,NWTRACE         ** TRACE **                                   
         DROP  R6                                                               
*                                                                               
* -- BUILD NWS HEADER KEY - LOOKING FOR OLD MKT/ STATION                        
*                                                                               
NW120    LA    R6,KEY                                                           
         USING BWHRECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   BWHKTYP,BWHKTYPQ   X'0D'                                         
         MVI   BWHKSUB,BWHKSUBQ   X'67'                                         
         MVC   BWHKAGMD,BAGYMD    A/M                                           
         MVC   BWHKBYR,SAVECAM+3  BUYER                                         
         MVC   BWHKCAM,SAVECAM+4  CAMPAIGN                                      
         MVC   BWHKMKT,OLDMKT     OLD MARKET                                    
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   NW190              NO HEADER FOR THIS CAMPAIGN/MKT               
*                                                                               
         CLI   QOPT5,C'Y'            TRACE REQ                                  
         BNE   *+12                                                             
         MVI   TRACECDE,14                                                      
         BAS   RE,NWTRACE          *** TRACE ***                                
*                                                                               
         GOTO1 GET                                                              
         L     R6,AREC                                                          
         LA    R2,BWHFSTEL                                                      
NW124    CLI   0(R2),X'02'                                                      
         BE    NW126                                                            
         LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   NW124                                                            
         DC    H'0'               MUST BE STATION LIST                          
*                                                                               
NW126    CLC   QSTA,BWHSTA-BWHEL(R2)         OLD STATION IN RECORD              
         BE    NW130                                                            
         LLC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),X'02'                                                      
         BE    NW126                                                            
         B     NW190              STATION NOT IN CAMPAIGN - TRY NEXT            
*                                                                               
NW130    MVC   NOLDSTCD,BWHSEQ-BWHEL(R2)  SAVE OLD STATION CODE                 
         MVC   BWHSTA-BWHEL(5,R2),NEWCALL NEW NWS STATION CALL LETTERS          
         LH    R1,NWSHDR                                                        
         LA    R1,1(,R1)                                                        
         STH   R1,NWSHDR                                                        
         CLI   RCWRITE,C'Y'                                                     
         BNE   NW136                                                            
         GOTO1 PUT                                                              
*                                                                               
NW136    DS    0H                                                               
         MVC   SAVEHDR,KEY       SAVE HEADER KEY                                
         DROP  R6                                                               
*                                                                               
* -- LOOK FOR NWS DETAIL RECORDS WITH OLD SEQ NUMBER                            
* -- AND OLD STATION CODE                                                       
*                                                                               
NW170    DS    0H                 FIX DTL RECORDS                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING BWDRECD,R1         SET MASTER KEY                                
         MVI   BWDKTYP,BWDKTYPQ   X'0D'                                         
         MVI   BWDKSUB,BWDKSUBQ   X'68'                                         
         MVC   BWDKAGMD,BAGYMD    A/M                                           
         MVC   BWDKBYR,SAVEHDR+3 BUYER FROM HEADER RECORD                       
         MVC   BWDKSEQ,SAVEHDR+8 SEQUENCE NUMBER FROM HEADER RECORD             
         GOTO1 HIGH                                                             
         DROP  R1                                                               
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,13                                                      
         BAS   RE,NWTRACE         ** TRACE **                                   
*                                                                               
NW172    DS    0H                 FIX DTL RECORDS                               
         CLC   KEY(06),KEYSAVE                                                  
         BNE   NW190                                                            
* NOTE - PUT CHANGES TO STA ADD/DEL HERE                                        
         GOTO1 GET                                                              
         L     R6,AREC                                                          
         MVI   BYTE,0                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    NW174                                                            
*                                                                               
         LA    R4,=CL30'NWS BUY DTL REC HAS NO 01 ELEM'                         
         L     R6,AREC                                                          
         SR    R5,R5                                                            
         ICM   R5,3,13(R6)                                                      
         GOTO1 =V(PRNTBL),DMCB,(30,(R4)),(R6),C'DUMP',(R5),=C'0D'               
*                                                                               
         B     NW190                                                            
         USING BWDEL,R6                                                         
NW174    CLC   QSTA,BWDSTA                                                      
         BNE   NW176                                                            
         MVI   BYTE,1              INDICATE RECORD CHANGED                      
         MVC   BWDSTA(5),NEWCALL                                                
NW176    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    NW174                                                            
         CLI   BYTE,1              WAS RECORD CHANGED                           
         BNE   NW178                                                            
         LH    R1,NWSDTL                                                        
         LA    R1,1(,R1)                                                        
         STH   R1,NWSDTL                                                        
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   NW178                                                            
         L     R6,AREC                                                          
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFILE',KEY+14,(R6),DMWORK           
*                                                                               
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
NW178    DS    0H                                                               
         GOTO1 SEQ                                                              
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,13                                                      
         BAS   RE,NWTRACE         ** TRACE **                                   
         B     NW172                                                            
*                                                                               
* NOW LOOK FOR BUY REVISION SAVE REC *                                          
*                                                                               
NW190    DS    0H                                                               
         LA    R6,KEY                                                           
         USING NBRRECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   NBRKTYP,NBRKTYPQ   X'0D'                                         
         MVI   NBRKSTY,NBRKSTYQ   X'6B'                                         
         MVC   NBRKAGMD,BAGYMD    A/M                                           
         MVC   NBRKBYR,SAVECAM+3  BUYER                                         
         MVC   NBRKSEQ,SAVECAM+4  CAMPAIGN                                      
         MVC   NBRKSTA,OLDSTA     OLD STATION                                   
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BNE   NW290              NO BUY REV FOR THIS CAMPAIGN/MKT/STA          
*                                                                               
         CLI   QOPT5,C'Y'            TRACE REQ                                  
         BNE   *+12                                                             
         MVI   TRACECDE,14                                                      
         BAS   RE,NWTRACE          *** TRACE ***                                
*                                                                               
         GOTO1 GET                                                              
         L     R6,AREC                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   NW196                                                            
         OI    KEY+13,X'80'        MARK FOR DELETION                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR ',KEY,KEY                       
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
         OI    15(R6),X'80'        MARK RECORD FOR DELETION                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFILE',KEY+14,(R6),DMWORK           
*                                                                               
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
NW194    NI    KEY+13,X'7F'        TURN OFF DELETE BIT                          
         NI    15(R8),X'7F'        TURN OFF DELETE BIT                          
*                                                                               
NW196    DS    0H                                                               
         MVC   NBRKSTA-NBRKEY+KEY,OLDSTA                                        
         MVC   NBRKSTA,OLDSTA                                                   
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   NW202                                                            
         USING NBRSELD,R6                                                       
         CLC   QSTA,NBRSSTA                                                     
         BE    NW204                                                            
         CLI   QSTA+4,C'T'                                                      
         JNE   *+2                                                              
         CLI   NBRSSTA+4,C' '                                                   
         JNE   *+2                                                              
         CLC   QSTA(4),NBRSSTA                                                  
         JNE   *+2                                                              
*                                                                               
         MVC   NBRSSTA(4),NEWCALL                                               
         B     NW206                                                            
*                                                                               
NW202    DS   0H                                                                
         LA    R4,=CL30'NWS BUY DTL REC HAS NO 10 ELEM'                         
         L     R6,AREC                                                          
         SR    R5,R5                                                            
         ICM   R5,3,13(R6)                                                      
         GOTO1 =V(PRNTBL),DMCB,(30,(R4)),(R6),C'DUMP',(R5),=C'0D'               
         B     NW290                                                            
*                                                                               
NW204    DS   0H                                                                
         MVC   NBRSSTA(5),NEWCALL                                               
*                                                                               
NW206    DS   0H                                                                
         LH    R1,NWSBRV                                                        
         LA    R1,1(,R1)                                                        
         STH   R1,NWSBRV                                                        
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   NW290                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,=C'SPTFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
NW290    DS    0H                 NEXT CAMPAIGN RECORD                          
         MVC   KEY(13),SAVECAM    RE-ESTABLISH CAMPAIGN KEY                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         GOTO1 SEQ                                                              
         B     NW100              TRY NEXT CAMPAIGN                             
*                                                                               
NW300    LH    R0,NWSHDR                                                        
         EDIT  (R0),(5,P+4),ZERO=NOBLANK                                        
         MVC   P+10(20),=CL20'NWS HDR RECS CHANGED'                             
         GOTO1 REPORT                                                           
         SPACE                                                                  
         LH    R0,NWSDTL                                                        
         EDIT  (R0),(5,P+4),ZERO=NOBLANK                                        
         MVC   P+10(20),=CL20'NWS DTL RECS CHANGED'                             
         GOTO1 REPORT                                                           
         SPACE                                                                  
         LH    R0,NWSBRV                                                        
         EDIT  (R0),(5,P+4),ZERO=NOBLANK                                        
         MVC   P+10(20),=CL20'NWS BRV RECS CHANGED'                             
         GOTO1 REPORT                                                           
         SPACE                                                                  
NWX      XIT1                                                                   
         SPACE 2                                                                
* THIS SECTION PRINTS A TRACE RECORD OF THE KEY *                               
         SPACE                                                                  
NWTRACE  NTR1                                                                   
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVC   P(1),TRACECDE                                                    
         GOTO1 HEXOUT,DMCB,KEY,P+10,18,=C'MIX',0                                
         SPACE                                                                  
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         MVI   TRACECDE,0                                                       
         XIT1                                                                   
         DS    0D                                                               
         DC    C'SAVE-CAM'                                                      
SAVECAM  DS    XL18                                                             
         DS    0D                                                               
         DC    C'SAVE-HDR'                                                      
SAVEHDR  DS    XL18                                                             
NOLDSTCD DS    XL1                                                              
         DS    0F                                                               
NWSTOTS  DS    0XL6                                                             
NWSHDR   DS    H                                                                
NWSDTL   DS    H                                                                
NWSBRV   DS    H                                                                
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* SNINV - THIS SECTION UPDATES NEW INVOICE RECORDS                              
***********************************************************************         
SNINV    NMOD1 0,*SNINV*                                                        
         USING SPSC02+8192,R3                                                   
         XC    COUNT,COUNT                                                      
         L     R8,AREC                                                          
         XC    BKEY,BKEY                                                        
         MVI   BKEY,SNVKTYPQ       INVOICE RECORD                               
         MVI   BKEY+1,SNVKSUBQ     INVOICE RECORD                               
         MVC   BKEY+2(1),SVAGYMD   A-M                                          
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    SN415                NO.                                         
         MVC   BKEY+3(2),SAVCLT    CLIENT                                       
         SPACE                                                                  
SN415    MVC   BKEYSAVE,BKEY                                                    
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',BKEYSAVE,BKEY                     
         B     SN422                                                            
         SPACE                                                                  
SN420    GOTO1 DATAMGR,DMCB,DMRSEQ,=C'XSPDIR',BKEY,BKEY                         
         SPACE                                                                  
SN422    CLC   BKEY(3),BKEYSAVE    SAME 0E/03/A-M                               
         BNE   SN500                NO, TRY NSID                                
         SPACE                                                                  
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    SN424                NO.                                         
         CLC   BKEY+3(2),SAVCLT                                                 
         BNE   SN500                CHECK NSID RECORDS                          
         SPACE                                                                  
SN424    CLC   BKEY+5(3),OLDSTA    STATION CALL LETTERS                         
         BE    SN426                YES                                         
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SN420                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,7,BKEY+5                                                      
         ICM   RF,7,OLDSTA                                                      
         SRL   RE,7                DROP NET FROM CABLE HEAD                     
         SRL   RF,7                                                             
         CR    RE,RF               IS THIS THE SAME CABLE HEAD                  
         BNE   SN420                NO                                          
*                                                                               
SN426    MVC   SAVEKEYI(40),BKEY    SAVE THE KEY                                
         CLI   QOPT5,C'Y'                                                       
         BNE   SN430                                                            
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SITRACE          ***TRACE***                                  
*                                                                               
SN430    MVC   BKEY+5(3),NEWSTA    DOES NEW INVOICE ALREADY EXIST?              
         MVC   BKEY+SNVKMINK-SNVKEY(6),=6X'FF'                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SN434                                                            
         MVC   BYTE,SAVEKEYI+7                                                  
         NI    BYTE,X'7F'                                                       
         OC    BKEY+7(1),BYTE      SET IN NETWORK                               
*                                                                               
SN434    MVC   BKEYSAVE,BKEY                                                    
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',BKEYSAVE,BKEY             
         CLC   BKEY(32),BKEYSAVE   SAME 0E03/A-M/CLT/STA/DATE/INV/SEQ           
         BE    SN490                YES. DON'T MOVE                             
*                                                                               
         MVC   BKEYSAVE(40),SAVEKEYI    NO. RESTORE KEY                         
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',BKEYSAVE,BKEY                     
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFILE',BKEY+36,AREC,DMWORK              
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         OC    SAVDA,SAVDA         IS THIS 1ST INVOICE?                         
         BZ    SN435                YES.                                        
         ICM   RF,15,BKEY+36       GET THE DISK ADDR                            
         C     RF,SAVDA            SHOULD I CHANGE THIS RECORD?                 
         BNL   SN420                NO. THIS IS A NEW ONE                       
         B     SN436                                                            
*                                                                               
SN435    GOTOR CRSTRTAB,DMCB,(C'X',AREC)   KEY IN RESTORE TABLE?                
         BNZ   SN420                 NO, DON'T MOVE                             
*                                                                               
SN436    CLI   RCWRITE,C'Y'                                                     
         BNE   SN440                                                            
         OI    BKEY+32,X'80'        MARK FOR DELETION                           
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'XSPDIR ',BKEY,BKEY                     
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
         OI    34(R8),X'80'        MARK RECORD FOR DELETION                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFILE',BKEY+36,AREC,DMWORK          
*                                                                               
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
SN437    NI    BKEY+32,X'7F'        TURN OFF DELETE BIT                         
         NI    34(R8),X'7F'        TURN OFF DELETE BIT                          
*                                                                               
SN440    MVC   DUB(1),7(R8)        SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),BKEY+7                                                  
         MVC   5(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   BKEY+5(3),NEWSTA                                                 
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SN444                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    7(1,R8),DUB        SET IN NETWORK                                
         OC    BKEY+7(1),DUB+1                                                  
*                                                                               
SN444    CLI   QOPT5,C'Y'                                                       
         BNE   SN450                                                            
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SITRACE          ***TRACE***                                  
*                                                                               
SN450    CLI   RCWRITE,C'Y'                                                     
         BNE   SN460                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFILE',BKEY+36,AREC,DMWORK              
         CLI   DM3,0                                                            
         JNE   *+2                                                              
SN460    OC    SAVDA,SAVDA         IS THIS 1ST ADD?                             
         BZ    SN470                NO.                                         
         MVC   SAVDA,BKEY+36       SAVE DISK ADDR OF 1ST ADD                    
*                                                                               
* PRINT THE NEW INVOICE RECORD *                                                
*                                                                               
SN470    CLC   =X'FFFFFFFFFFFF',BKEY+24                                         
         BNE   SN480                                                            
*                                                                               
         LH    R1,COUNT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,COUNT            # RECORDS CHANGED                            
*                                                                               
         GOTO1 CLUNPK,DMCB,BKEY+3,P+10                                          
         MVC   HALF,BKEY+8                                                      
         XC    HALF,=X'FFFF'                                                    
         GOTO1 DATCON,(R1),(2,HALF),(5,P+20)                                    
         MVC   P+34(10),BKEY+12      INVOICE NUMBER                             
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
SN480    MVC   BKEYSAVE(40),SAVEKEYI   RESTORE KEY                              
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',BKEYSAVE,BKEY             
         CLC   BKEY(32),SAVEKEYI                                                
         BE    SN420                                                            
         DC    H'0'                                                             
*                                                                               
* PRINT ERROR FOR INVOICE ALREADY ON NEW STATION FOR THIS DATE *                
*                                                                               
SN490    GOTO1 CLUNPK,DMCB,BKEY+3,P+10                                          
         MVC   HALF,BKEY+8                                                      
         XC    HALF,=X'FFFF'                                                    
         GOTO1 DATCON,(R1),(2,HALF),(5,P+20)                                    
         MVC   P+34(10),BKEY+12      INVOICE NUMBER                             
         MVC   P+45(40),=CL40'* ERROR * INVOICE ALREADY ON NEW STATION'         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     SN480                                                            
*                                                                               
SN500    LH    R0,COUNT                                                         
         EDIT  (R0),(5,P+4),ZERO=NOBLANK                                        
         MVC   P+10(23),=CL23'NEW INV RECORDS CHANGED'                          
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
* THIS SECTION PRINTS A TRACE RECORD OF THE KEY *                               
*                                                                               
SITRACE  NTR1                                                                   
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVC   P(1),TRACECDE                                                    
         GOTO1 HEXOUT,DMCB,BKEY,P+10,40,=C'MIX',0                               
SITRACE2 DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         MVI   TRACECDE,0                                                       
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* SNAUTH - THIS SECTION UPDATES AUTHORIZATION RECORD                            
***********************************************************************         
SNAUTH   NMOD1 0,*SNAUTH*                                                       
         USING SPSC02+8192,R3                                                   
         XC    COUNT,COUNT                                                      
*                                                                               
K        USING AUTRECD,BKEY                                                     
         L     R8,AREC                                                          
         USING AUTRECD,R8                                                       
         XC    K.AUTKEY,K.AUTKEY                                                
*                                                                               
         MVI   K.AUTKTYP,AUTKTYQQ                                               
         MVI   K.AUTKSUB,AUTKSUBQ                                               
         MVC   K.AUTKAM,SVAGYMD                                                 
         OC    SAVCLT,SAVCLT                                                    
         BZ    SNA415                                                           
         MVC   K.AUTKCLT,SAVCLT                                                 
*                                                                               
SNA415   MVC   BKEYSAVE,BKEY                                                    
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',BKEYSAVE,BKEY                     
         B     SNA422                                                           
         SPACE                                                                  
SNA420   GOTO1 DATAMGR,DMCB,DMRSEQ,=C'XSPDIR',BKEY,BKEY                         
*                                                                               
SNA422   DS    0H                                                               
         CLC   BKEY(20),BKEYSAVE     SAME AGY/MEDIA - FIX LEN                   
         BNE   SNA500               NO, EXIT                                    
         OC    SAVCLT,SAVCLT        CLIENT FILTER ENTERED?                      
         BZ    SNA424                                                           
         CLC   K.AUTKCLT,SAVCLT     SAME CLIENT AS FILTER?                      
         BNE   SNA500                                                           
*                                                                               
SNA424   DS    0H                                                               
         CLC   K.AUTKMKT,OLDMKT      MARKET                                     
         BNE   SNA420               WRONG MARKET - READ NEXT                    
         CLC   K.AUTKSTA,OLDSTA      STATION CALL LETTERS                       
         BNE   SNA420               WRONG STATION - READ NEXT                   
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFILE',BKEY+36,AREC,DMWORK              
*****                                                                           
*        LA    R4,=CL30'ORIGINAL AUTHORIZATION RECORD'                          
*        SR    R5,R5                                                            
*        ICM   R5,3,AUTRLEN                                                     
*        GOTO1 =V(PRNTBL),DMCB,(30,(R4)),(R8),C'DUMP',(R5),=C'0D'               
*****                                                                           
*                                                                               
         GOTOR CRSTRTAB,DMCB,(C'X',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   SNA420                      NO, READ NEXT                        
*                                                                               
         LH    R1,COUNT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,COUNT            # RECORDS CHANGED                            
*        CHI   R1,100                                                           
*        BNL   SNA500                                                           
*                                                                               
SNA426   MVC   SAVEKEYI(32),BKEY    SAVE THE KEY                                
         CLI   QOPT5,C'Y'                                                       
         BNE   SNA430                                                           
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SITRACE3          ***TRACE***                                 
         SPACE                                                                  
SNA430   DS    0H                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   SNA440                                                           
         OI    K.AUTKSTAT,X'80'      MARK DIR FOR DELETION                      
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'XSPDIR ',BKEY,BKEY                     
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
         OI    AUTRSTAT,X'80'        MARK RECORD FOR DELETION                   
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFILE',BKEY+36,AREC,DMWORK          
                                                                                
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
SNA437   DS    0H                                                               
         NI    K.AUTKSTAT,X'7F'      TURN OFF DELETE BIT IN KEY                 
         NI    AUTRSTAT,X'7F'        TURN OFF DELETE BIT IN RECORD              
         SPACE                                                                  
SNA440   DS    0H                                                               
         MVC   K.AUTKSTA,NEWSTA      UPDATE CALL LETTERS IN KEY                 
         MVC   AUTKSTA,NEWSTA      UPDATE CALL LETTERS IN RECORD                
*                                                                               
SNA444   CLI   QOPT5,C'Y'                                                       
         BNE   SNA450                                                           
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SITRACE3         ***TRACE***                                  
         SPACE                                                                  
SNA450   CLI   RCWRITE,C'Y'                                                     
         BNE   SNA460                                                           
         GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFILE',BKEY+36,AREC,DMWORK              
         CLI   DM3,0                                                            
         BE    SNA460                                                           
         CLI   DM3,X'20'                                                        
         JNE   *+2                                                              
* ISSUE DUPLIATE KEY ERROR MESSAGE                                              
         MVC   P(35),=CL40'* ERROR * DUPLICATE KEY ON ADD: '                    
         GOTO1 HEXOUT,DMCB,BKEY,P+35,32,=C'MIX'                                 
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
SNA460   DS    0H                                                               
         MVC   BKEYSAVE(32),SAVEKEYI   RESTORE KEY                              
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',BKEYSAVE,BKEY             
         CLC   BKEY(32),SAVEKEYI                                                
         BE    SNA420                                                           
*                                                                               
SNA500   LH    R0,COUNT                                                         
         EDIT  (R0),(5,P+4),ZERO=NOBLANK                                        
         MVC   P+10(30),=CL30'AUTHORIZATION RECORDS CHANGED'                    
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
* THIS SECTION PRINTS A TRACE RECORD OF THE KEY *                               
*                                                                               
SITRACE3 NTR1                                                                   
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVC   P(1),TRACECDE                                                    
         GOTO1 HEXOUT,DMCB,BKEY,P+10,40,=C'MIX',0                               
         DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         MVI   TRACECDE,0                                                       
         XIT1                                                                   
         LTORG                                                                  
         DC    C'**BKEY**'                                                      
BKEY     DC    XL40'00'                                                         
BKEYSAVE DC    XL40'00'                                                         
SAVEKEYI DC    XL40'00'                                                         
         EJECT                                                                  
         DROP  RB,R8,R3                                                         
         SPACE                                                                  
***********************************************************************         
* DARBATCH - THIS SECTION UPDATES CALL LETTERS FOR DARE BATCH RECORDS           
***********************************************************************         
         USING SPSC02+8192,R3                                                   
DARBATCH NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         XC    SAVDA,SAVDA         CLEAN OUT SAVED DISK ADDRESS                 
*                                                                               
         MVI   KEY,X'0D'           DARE BATCH RECORD TYPE                       
         MVI   KEY+1,X'35'         DARE BATCH RECORD SUBTYPE                    
         MVC   KEY+2(1),SVAGYMD                                                 
         MVC   KEY+3(5),OLDMSTA    MOVING IN MARKET AND STATION                 
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    DRBT010              - NO.                                       
         MVC   KEY+8(2),SAVCLT     SAME 0D35/AM/MKT/STA/CLT?                    
*                                                                               
DRBT010  GOTO1 HIGH                                                             
         B     DRBT030                                                          
*                                                                               
DRBT020  GOTO1 SEQ                                                              
*                                                                               
DRBT030  CLC   KEY(8),KEYSAVE      SAME 0D35/AM/MKT/STA?                        
         JNE   EXIT                                                             
*                                                                               
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    DRBT040              - NO.                                       
         CLC   KEY+8(2),SAVCLT                                                  
         BNE   DRBT020             GO TO THE NEXT RECORD                        
*                                                                               
DRBT040  MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   DRBT050                                                          
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
DRBT050  MVC   KEY+3(5),NEWMSTA                                                 
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    DRBT060                                                          
         MVC   BYTE,SAVEKEY+7                                                   
         NI    BYTE,X'7F'                                                       
         OC    KEY+7(1),BYTE       SET IN NETWORK                               
*                                                                               
DRBT060  GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     SAME 0D35/AM/MKT/STA/CLT?                    
         BE    DRBT200              YES. DON'T MOVE                             
         MVC   KEY(13),SAVEKEY      NO. RESTORE KEY                             
         GOTO1 HIGH                                                             
*                                                                               
         GOTO1 GET                                                              
         OC    SAVDA,SAVDA         IS THIS 1ST DARE BATCH?                      
         BZ    DRBT070              YES.                                        
         ICM   RF,15,KEY+14        GET THE DISK ADDR                            
         C     RF,SAVDA            SHOULD I CHANGE THIS RECORD?                 
         BNL   DRBT020              NO. THIS IS A NEW ONE                       
         B     DRBT080                                                          
*                                                                               
DRBT070  GOTOR CRSTRTAB,DMCB,(C'S',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   DRBT020                     NO, READ NEXT                        
*                                                                               
DRBT080  CLI   RCWRITE,C'Y'                                                     
         BNE   DRBT090                                                          
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC,DMWORK                   
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
DRBT090  MVC   DUB(1),7(R8)        SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+7                                                   
         MVC   3(5,R8),NEWMSTA     UPDATE CALL LETTERS                          
         MVC   KEY+3(5),NEWMSTA                                                 
         NI    15(R8),X'7F'        UNDELETE THE RECORD                          
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    DRBT100                                                          
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    7(1,R8),DUB          SET IN NETWORK                              
         OC    KEY+7(1),DUB+1                                                   
*                                                                               
DRBT100  CLI   QOPT5,C'Y'                                                       
         BNE   DRBT120                                                          
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
DRBT120  CLI   RCWRITE,C'Y'                                                     
         BNE   DRBT140                                                          
         GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,AREC,DMWORK                   
         CLI   DM3,0                                                            
         JNE   *+2                                                              
DRBT140  OC    SAVDA,SAVDA         IS THIS 1ST ADD?                             
         BZ    DRBT160              NO.                                         
         MVC   SAVDA,KEY+14        SAVE DISK ADDR OF 1ST ADD                    
*                                                                               
* PRINT THE NEW DARE BATCH RECORD *                                             
*                                                                               
DRBT160  DS    0H                                                               
         LH    R1,COUNT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,COUNT            # RECORDS CHANGED                            
*                                                                               
         GOTO1 CLUNPK,DMCB,KEY+8,P+10              PRINT CLIENT                 
         GOTO1 MSUNPK,DMCB,KEY+3,P+17,P+22         PRINT MKT                    
         EDIT  (B1,KEY+11),(3,P+28),ZERO=NOBLANK   PRINT EST                    
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
DRBT180  MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),SPTDIR,KEY,KEY                       
         CLC   KEY(13),SAVEKEY                                                  
         BE    DRBT020                                                          
         DC    H'0'                                                             
*                                                                               
* PRINT ERROR FOR DARE BATCH ALREADY ON NEW STATION (CLIENT) *                  
*                                                                               
DRBT200  GOTO1 CLUNPK,DMCB,KEY+8,P+10              PRINT CLIENT                 
         GOTO1 MSUNPK,DMCB,KEY+3,P+17,P+22         PRINT MKT                    
         EDIT  (B1,KEY+11),(3,P+28),ZERO=NOBLANK   PRINT EST                    
         MVC   P+45(42),=CL42'* ERROR * DARE BATCH RECORD ALREADY EXIST+        
               S'                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     DRBT180                                                          
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
***********************************************************************         
* SUBROUTINE TO BLOCK CALL LETTER CHANGE FOR CANADIAN NETWORKS, MEDIA=N         
***********************************************************************         
CHKCAN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   QMED,C'N'                                                        
         JNE   EXIT                                                             
*                                                                               
         LA    R2,KEY                                                           
         USING NDEFKEY,R2                                                       
         XC    NDEFKEY,NDEFKEY                                                  
         MVC   NDEFKTYP,=X'0D11'                                                
         MVC   NDEFKAGY,QAGY                                                    
         MVC   NDEFKNET,QSTA                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         JNE   EXIT                                                             
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P+10(30),=CL30'CANNOT CHANGE CANADIAN NETWORK'                   
         MVC   P+40(20),=CL20' CALL LETTERS'                                    
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R2                                                            
***********************************************************************         
* ADD STATION FIX RECORDS FOR SFM                                     *         
***********************************************************************         
STAFIXIT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AREC,=A(SPBUFF)                                                  
*                                                                               
         L     RE,AREC             CLEAR THE IO AREA                            
         L     RF,=F'4000'                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,AREC                                                          
         USING STAFXRCD,R6                                                      
         MVI   STFKTYP,STFKTYQ     (X'0D') RECORD TYPE                          
         MVI   STFKSBTY,STFKSBTQ   (X'6E') SUBRECORD TYPE                       
         MVC   STFKAGMD,BAGYMD     AGENCY/MEDIA                                 
         MVC   STFKCLI,BCLT        CLIENT (PACKED)                              
         CLC   QCLT,=CL3'ALL'      ALL CLIENTS?                                 
         BNE   *+10                NO                                           
         MVC   STFKCLI,=XL2'FFFF'  YES, MOVE X'FFFF' IN                         
         MVC   STFKOSTA,OLDSTA     OLD STATION                                  
         MVC   STFKMKT,NEWMKT      MARKET                                       
         MVC   STFKNSTA,NEWSTA     NEW STATION                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                DOES RECORD PREVIOUSLY EXIST?                
         CLC   KEY(13),KEYSAVE                                                  
         BNE   STAF05               - NO, CONTINUE LIKE NORMAL                  
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,SPTFILE,KEY+14,AREC,DMWORK                   
         LA    R7,STFFRST          POINT TO FIRST ELEMENT                       
         XR    R0,R0                                                            
*                                                                               
STAF00   CLI   0(R7),0             ANY ELEMENT HERE?                            
         BE    STAF10               - NOPE, GOOD TO GO                          
         IC    R0,1(R7)             - YEAH, BUMP UNTIL WE GET PAST LAST         
         AR    R7,R0                                                            
         B     STAF00                                                           
***                                                                             
* BUILD DETAILS ELEMENT                                                         
***                                                                             
STAF05   LA    R7,STFFRST          R7 WILL NOW POINT RIGHT AFTER KEY            
*                                                                               
STAF10   XC    WORK,WORK                                                        
         LA    R5,WORK                                                          
         USING STFIDELD,R5                                                      
         MVI   STFIDEL,STFIDELQ    X'10' ELEMENT CODE                           
         MVI   STFIDLEN,STFIDLNQ                                                
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,STFIDDAT)   SAVE CREATED DATE          
         MVC   STFIDRQR,QUESTOR    SAVE REQUESTOR'S NAME                        
*                                                                               
         L     R1,VMASTC                                                        
         USING MASTD,R1                                                         
         LA    R2,MCRFHDR                                                       
         USING RQHITRM,R2                                                       
         MVC   STFIDATH,RQHPSWD    SAVE OFF PASSWORD                            
         DROP  R1,R2,R5                                                         
*                                                                               
         GOTO1 RECUP,DMCB,AREC,WORK,(R7)                                        
*                                                                               
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         JNE   STAFX               NO, DONE                                     
*                                                                               
         CLC   KEY(13),KEYSAVE     RECORD EXISTS PREVIOUSLY?                    
         BNE   STAF15               - NOPE, DO ADDREC INSTEAD OF PUTREC         
*                                                                               
         GOTO1 DATAMGR,DMCB,PUTREC,SPTFILE,KEY+14,AREC,DMWORK                   
         B     STAFX                                                            
*                                                                               
STAF15   GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,AREC,DMWORK                   
*                                                                               
STAFX    J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R6                                                            
         EJECT                                                                  
***********************************************************************         
* SPA00 - THIS SECTION UPDATES TRAFFIC BUY RECORDS *                            
***********************************************************************         
SPA00    NTR1  BASE=*,LABEL=*                                                   
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A32'     TRAFFIC BUY RECORD                           
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+10                NO, THEN ALL CLIENTS                         
         MVC   KEY+3(2),SAVCLT                                                  
         GOTOR HIGH                                                             
         B     SPA20                                                            
*                                                                               
SPA10    GOTOR SEQ                                                              
*                                                                               
SPA20    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SPA00X              NO,THEN TRY NEXT                             
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+14                NO, THEN ALL CLIENTS                         
         CLC   KEY+3(2),SAVCLT                                                  
         BNE   SPA00X                                                           
         CLC   KEY+6(5),OLDMSTA    OLD MARKET/STATION                           
         BE    SPA24                                                            
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SPA10                                                            
         MVC   DUB(5),KEY+6                                                     
         NI    DUB+4,X'80'                                                      
         CLC   DUB(5),OLDMSTA                                                   
         BNE   SPA10                                                            
*                                                                               
SPA24    MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   SPA30                                                            
         MVI   TRACECDE,C'O'                                                    
         BRAS  RE,SPTRACE          ***TRACE***                                  
*                                                                               
SPA30    MVC   KEY+6(5),NEWMSTA    DOES NEW RECORD ALREADY EXIST?               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SPA34                                                            
         MVC   BYTE,SAVEKEY+10     SAVE CABLE HEAD NETWORK                      
         NI    BYTE,X'7F'                                                       
         OC    KEY+10(1),BYTE      SET IN NETWORK                               
*                                                                               
SPA34    GOTOR HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DUPA0               YES, DUPLICATE KEYS (TSK,TSK)                
         MVC   KEY(13),SAVEKEY                                                  
         GOTOR HIGH                RESTORE DATAMGR SEQ                          
*                                                                               
         GOTOR GET                                                              
*                                                                               
SPA40    GOTOR CRSTRTAB,DMCB,(C'T',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   SPA10                       NO, READ NEXT                        
*                                                                               
SPA40A   CLI   RCWRITE,C'Y'                                                     
         BNE   SPA50                                                            
         GOTOR DATAMGR,DMCB,=C'DMDEL',=C'TRFFILE',KEY,AREC,DMWORK               
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
SPA50    MVC   DUB(1),10(R8)       SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+10                                                  
         MVC   8(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+8(3),NEWSTA                                                  
         NI    15(R8),X'7F'        UNDELETE THE RECORD                          
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SPA56                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    10(1,R8),DUB        SET IN NETWORK                               
         OC    KEY+10(1),DUB+1                                                  
*                                                                               
SPA56    CLI   QOPT5,C'Y'                                                       
         BNE   SPA60                                                            
         MVI   TRACECDE,C'I'                                                    
         BRAS  RE,SPTRACE          ***TRACE***                                  
*                                                                               
SPA60    CLI   RCWRITE,C'Y'                                                     
         BNE   SPA70                                                            
         GOTOR DATAMGR,DMCB,ADDREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
SPA70    LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
*                                                                               
         BRAS  RE,SPAPRT                                                        
*                                                                               
SPA80    MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         OI    DMINBTS,X'08'       CHECK DELETED RECS TOO                       
         MVI   DMOUTBTS,X'FD'                                                   
         GOTOR HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),SAVEKEY                                                  
         BE    SPA10                                                            
         DC    H'0'                                                             
*                                                                               
* PRINT TRAFFIC BUY RECORD                                                      
*                                                                               
SPAPRT   LR    R6,RE                                                            
         GOTOR CLUNPK,DMCB,KEY+3,P+10                                           
         GOTOR MSUNPK,(R1),KEY+6,P+17,P+22                                      
         MVI   SPACING,2                                                        
         GOTOR REPORT                                                           
         BR    R6                                                               
*                                                                               
* PRINT ERROR FOR TRAFFIC BUY RECORD ALREADY ON NEW STATION *                   
*                                                                               
DUPA0    MVC   P+45(40),NEWEXIST                                                
         MVC   P+69(2),=C'32'                                                   
         BRAS  RE,SPAPRT                                                        
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SPA80                                                            
*                                                                               
SPA00X   BRAS  RE,REPRT                                                         
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* SPB00 - THIS SECTION UPDATES TRAFFIC STATION LABEL LIST RECORDS *             
***********************************************************************         
SPB00    NTR1  BASE=*,LABEL=*                                                   
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2F'     LABEL LIST RECORD                            
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+10                NO, THEN ALL CLIENTS                         
         MVC   KEY+3(2),SAVCLT                                                  
         GOTOR HIGH                                                             
         B     SPB20                                                            
*                                                                               
SPB10    GOTOR SEQ                                                              
*                                                                               
SPB20    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SPB00X              NO,THEN TRY NEXT                             
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+14                NO, THEN ALL CLIENTS                         
         CLC   KEY+3(2),SAVCLT                                                  
         BNE   SPB00X                                                           
*                                                                               
         GOTOR GET                                                              
*                                                                               
         GOTOR CRSTRTAB,DMCB,(C'T',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   SPB10                 NO, BYPASS                                 
*                                                                               
         L     R6,ADBUY            POINT TO RECORD                              
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   SPB10                                                            
         MVI   CHGEL,C'N'          SET OFF ELEM CHANGED                         
         B     *+12                                                             
*                                                                               
SPB30    BRAS  RE,NEXTEL                                                        
         BNE   SPB40                                                            
*                                                                               
         CLC   2(3,R6),OLDSTA      TEST IF OLD STATION                          
         BE    SPB34                                                            
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SPB30                                                            
         MVC   DUB(3),2(R6)                                                     
         NI    DUB+2,X'80'                                                      
         CLC   DUB(3),OLDSTA                                                    
         BNE   SPB30                                                            
         MVC   BYTE,4(R6)                                                       
         NI    BYTE,X'7F'                                                       
         MVC   2(3,R6),NEWSTA      INSERT NEW STATION                           
         OC    4(1,R6),BYTE                                                     
         MVI   CHGEL,C'Y'          CHANGED ELEMENT                              
         B     SPB30                                                            
*                                                                               
SPB34    MVC   2(3,R6),NEWSTA      INSERT NEW STATION                           
         MVI   CHGEL,C'Y'          CHANGED ELEMENT                              
         B     SPB30                                                            
*                                                                               
SPB40    DS    0H                                                               
         CLI   CHGEL,C'Y'          TEST ELEMENTS CHANGED                        
         BNE   SPB10               NO, THEN NEXT RECORD                         
         CLI   RCWRITE,C'Y'                                                     
         BNE   SPB70                                                            
         CLI   QOPT5,C'Y'                                                       
         BNE   SPB60                                                            
         MVI   TRACECDE,C'I'                                                    
         BRAS  RE,SPTRACE          ***TRACE***                                  
*                                                                               
SPB60    GOTOR DATAMGR,DMCB,PUTREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
SPB70    LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
         B     SPB10                                                            
*                                                                               
SPB00X   BRAS  RE,REPRT                                                         
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* SPC00 - THIS SECTION UPDATES TRAFFIC MARKET STATION LIST RECORDS *            
***********************************************************************         
SPC00    NTR1  BASE=*,LABEL=*                                                   
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         MVI   CHGEL,0                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A31'     STATION LIST RECORD                          
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+10                NO, THEN ALL CLIENTS                         
         MVC   KEY+3(2),SAVCLT                                                  
         GOTOR HIGH                                                             
         B     SPC20                                                            
*                                                                               
SPC10    GOTOR SEQ                                                              
*                                                                               
SPC20    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SPC00X              NO,THEN TRY NEXT                             
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+14                NO, THEN ALL CLIENTS                         
         CLC   KEY+3(2),SAVCLT                                                  
         BNE   SPC00X                                                           
         CLC   KEY+8(2),OLDMKT     TEST OLD MARKET THE SAME                     
         BNE   SPC10                                                            
*                                                                               
         GOTOR GET                                                              
*                                                                               
         GOTOR CRSTRTAB,DMCB,(C'T',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   SPC10                 NO, BYPASS                                 
*                                                                               
         L     R6,ADBUY            POINT TO RECORD                              
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   SPC10                                                            
         MVI   CHGEL,C'N'                                                       
         B     *+12                                                             
*                                                                               
SPC30    BRAS  RE,NEXTEL                                                        
         BNE   SPC40                                                            
*                                                                               
         CLC   2(3,R6),OLDSTA      TEST IF OLD STATION                          
         BE    SPC34                                                            
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SPC30                                                            
         MVC   DUB(3),2(R6)                                                     
         NI    DUB+2,X'80'                                                      
         CLC   2(3,R6),OLDSTA                                                   
         BNE   SPC30                                                            
         MVC   BYTE,4(R6)                                                       
         NI    BYTE,X'7F'                                                       
         MVC   2(3,R6),NEWSTA      INSERT NEW STATION                           
         OC    4(1,R6),BYTE                                                     
         MVI   CHGEL,C'Y'          CHANGED ELEMENT                              
         B     SPC30                                                            
*                                                                               
SPC34    MVC   2(3,R6),NEWSTA      INSERT NEW STATION                           
         MVI   CHGEL,C'Y'                                                       
         B     SPC30                                                            
*                                                                               
SPC40    DS    0H                                                               
         CLI   CHGEL,C'Y'                                                       
         BNE   SPC10                                                            
*                                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   SPC60                                                            
         MVI   TRACECDE,C'I'                                                    
         BRAS  RE,SPTRACE          ***TRACE***                                  
*                                                                               
SPC60    CLI   RCWRITE,C'Y'                                                     
         BNE   SPC70                                                            
         GOTOR DATAMGR,DMCB,PUTREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
SPC70    LH    R0,COUNT                                                         
         AHI   R0,1                                                             
         STH   R0,COUNT            # RECORDS CHANGED                            
         B     SPC10                                                            
*                                                                               
SPC00X   DS    0H                                                               
         BRAS  RE,REPRT                                                         
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* SPD00 - THIS SECTION UPDATES TRAFFIC PATTERN RECORDS *                        
***********************************************************************         
SPD00    NTR1  BASE=*,LABEL=*                                                   
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         MVI   CHGEL,0                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A22'     PATTERN RECORD                               
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+10                NO, THEN ALL CLIENTS                         
         MVC   KEY+3(2),SAVCLT                                                  
         GOTOR HIGH                                                             
         B     SPD20                                                            
*                                                                               
SPD10    GOTOR SEQ                                                              
*                                                                               
SPD20    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SPD00X              NO,THEN TRY NEXT                             
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+14                NO, THEN ALL CLIENTS                         
         CLC   KEY+3(2),SAVCLT                                                  
         BNE   SPD00X                                                           
*                                                                               
         GOTOR GET                                                              
*                                                                               
         GOTOR CRSTRTAB,DMCB,(C'T',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   SPD10                 NO, BYPASS                                 
*                                                                               
         L     R6,ADBUY                                                         
*                                                                               
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         JNE   *+2                                                              
         MVI   CHGEL,C'N'                                                       
*                                                                               
         CLI   2(R6),C'S'          TEST STATION LIST                            
         BNE   SPD10                                                            
         LLC   R0,1(R6)            #STATIONS IN LIST                            
         SHI   R0,3                                                             
         SRDL  R0,32                                                            
         D     R0,=F'5'                                                         
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'00'               DIE IF ISN'T EVEN                            
*                                                                               
         LR    R0,R1                                                            
         LA    R1,3(R6)                                                         
SPD40    CLC   0(5,R1),QSTA        TEST OLD STATION                             
         BNE   SPD44                                                            
         MVC   0(5,R1),NEWCALL     MOVE IN NEW STATION                          
         MVI   CHGEL,C'Y'                                                       
         B     SPD46                                                            
*                                                                               
SPD44    OC    0(2,R1),0(R1)                                                    
         BNZ   SPD46                                                            
         MVC   DUB(3),2(R1)                                                     
         NI    DUB+2,X'80'                                                      
         CLC   DUB(3),OLDSTA                                                    
         BNE   SPD46                                                            
         MVC   BYTE,4(R1)                                                       
         NI    BYTE,X'7F'                                                       
         MVC   2(3,R1),NEWSTA      INSERT NEW STATION                           
         OC    4(1,R1),BYTE        SET NETWORK                                  
         MVI   CHGEL,C'Y'                                                       
*                                                                               
SPD46    LA    R1,5(R1)                                                         
         BCT   R0,SPD40                                                         
*                                                                               
         CLI   CHGEL,C'Y'          TEST CHANGED ELEMENT                         
         BNE   SPD10                                                            
         CLI   QOPT5,C'Y'                                                       
         BNE   SPD60                                                            
         MVI   TRACECDE,C'I'                                                    
         BRAS  RE,SPTRACE          ***TRACE***                                  
*                                                                               
SPD60    CLI   RCWRITE,C'Y'                                                     
         BNE   SPD70                                                            
         GOTOR DATAMGR,DMCB,PUTREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
SPD70    LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
         B     SPD10                                                            
*                                                                               
SPD00X   DS    0H                                                               
         BRAS  RE,REPRT                                                         
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* SPE00 - THIS SECTION UPDATES TRAFFIC STATION RECORDS *                        
***********************************************************************         
SPE00    NTR1  BASE=*,LABEL=*                                                   
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A28'     STATION ADDRESS RECS                         
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         GOTOR HIGH                                                             
         B     SPE20                                                            
*                                                                               
SPE10    GOTOR SEQ                                                              
*                                                                               
SPE20    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SPE00X              NO,THEN TRY NEXT                             
         CLC   KEY+3(5),QSTA       OLD STATION                                  
         BNE   SPE10                                                            
*                                                                               
         MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   SPE30                                                            
         MVI   TRACECDE,C'O'                                                    
         BRAS  RE,SPTRACE          ***TRACE***                                  
*                                                                               
SPE30    MVC   KEY+3(5),NEWCALL    DOES NEW RECORD ALREADY EXIST?               
         GOTOR HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DUPE0               YES, DUPLICATE KEYS (TSK,TSK)                
         MVC   KEY(13),SAVEKEY                                                  
         GOTOR HIGH                RESTORE DATAMGR SEQ                          
*                                                                               
         GOTOR GET                                                              
*                                                                               
         GOTOR CRSTRTAB,DMCB,(C'T',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   SPE10                 NO, BYPASS                                 
*                                                                               
SPE40    CLI   RCWRITE,C'Y'                                                     
         BNE   SPE50                                                            
         GOTOR DATAMGR,DMCB,=C'DMDEL',=C'TRFFILE',KEY,AREC,DMWORK               
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
SPE50    MVC   3(5,R8),NEWCALL     UPDATE CALL LETTERS                          
         MVC   KEY+3(5),NEWCALL                                                 
         NI    15(R8),X'7F'        UNDELETE THE RECORD                          
         CLI   QOPT5,C'Y'                                                       
         BNE   SPE60                                                            
         MVI   TRACECDE,C'I'                                                    
         BRAS  RE,SPTRACE          ***TRACE***                                  
*                                                                               
SPE60    CLI   RCWRITE,C'Y'                                                     
         BNE   SPE70                                                            
         GOTOR DATAMGR,DMCB,ADDREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
SPE70    LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
*                                                                               
SPE80    MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         OI    DMINBTS,X'08'       CHECK DELETED RECS TOO                       
         MVI   DMOUTBTS,X'FD'                                                   
         GOTOR HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),SAVEKEY                                                  
         BE    SPE10                                                            
         DC    H'0'                                                             
*                                                                               
* PRINT ERROR FOR TRAFFIC STATION ADDR RECORD ALREADY ON NEW STATION *          
*                                                                               
DUPE0    MVC   P+10(5),KEY+3                                                    
         MVC   P+45(40),NEWEXIST                                                
         MVI   P+70,C'8'                                                        
         MVI   SPACING,2                                                        
         GOTOR REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SPE80                                                            
*                                                                               
SPE00X   DS    0H                                                               
         BRAS  RE,REPRT                                                         
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* SPG00 - THIS SECTION UPDATES ORDER HISTORY RECORDS *                          
***********************************************************************         
SPG00    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AREC,ADBUY                                                       
*                                                                               
         L     RE,AREC             CLEAR THE IO AREA                            
         L     RF,=F'4000'                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    COUNT,COUNT                                                      
         LA    R8,BKEY2                                                         
         XC    BKEY2,BKEY2                                                      
         USING OHISKEY,R8                                                       
         MVI   OHISTYP,OHISTYQ      X'0D'                                       
         MVI   OHISSTYP,OHISSTYQ    X'1B'                                       
         MVC   BKEYSAV2,BKEY2                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',BKEYSAV2,BKEY2                    
         B     SPG020                                                           
*                                                                               
SPG010   GOTO1 DATAMGR,DMCB,DMRSEQ,=C'XSPDIR',BKEY2,BKEY2                       
*                                                                               
SPG020   CLC   OHISKEY(2),BKEYSAV2  X'0D1B' RECORD?                             
         BNE   SPGX                 NO, DONE                                    
*                                                                               
         CLC   OHISBKAM,SVAGYMD     SAME A/M?                                   
         BNE   SPG010               NO, READ SEQ                                
*                                                                               
         OC    SAVCLT,SAVCLT        CLIENT SPECIFIC REQ?                        
         BZ    SPG030               NO                                          
         CLC   OHISBKCL,SAVCLT      YES - HAVE THE SAME CLIENT?                 
         BNE   SPG010               NO, READ SEQ                                
*                                                                               
SPG030   CLC   OHISBKMK,OLDMKT      OLD MKT?                                    
         BNE   SPG010               NO, READ SEQ                                
*                                                                               
         CLC   OHISBKST,OLDSTA      OLD STA?                                    
         BE    SPG040               YES                                         
*                                                                               
         CLI   QSTA,C'0'           IS THIS A CABLE HEAD?                        
         BL    SPG010              NO, READ SEQ                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,7,OHISBKST                                                    
         ICM   RF,7,OLDSTA                                                      
         SRL   RE,7                DROP NET FROM CABLE HEAD                     
         SRL   RF,7                                                             
         CR    RE,RF               IS THIS THE SAME CABLE HEAD                  
         BNE   SPG010              NO, READ SEQ                                 
*                                                                               
SPG040   MVC   SAVEKEYO(40),BKEY2  SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   SPG050                                                           
         MVI   P,C'I'               INPUT                                       
         GOTO1 HEXOUT,DMCB,BKEY2,P+10,40,=C'MIX',0                              
         GOTO1 REPORT                                                           
*                                                                               
SPG050   BRAS  RE,SPGBLN           GET NEW BUYLINE                              
*                                                                               
SPG060   MVC   OHISBKST,NEWSTA     SEE IF NEW ORDER HISTORY REC EXISTS          
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SPG070                                                           
         MVC   BYTE,SAVEKEYO+(OHISBKST+2-OHISKEY)                               
         NI    BYTE,X'7F'                                                       
         OC    OHISBKST+2(1),BYTE     SET IN NETWORK                            
*                                                                               
SPG070   MVC   BKEYSAV2,BKEY2                                                   
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',BKEYSAV2,BKEY2            
         CLC   BKEY2(32),BKEYSAV2   ALREADY HAVE NEW ORDER HISTORY REC?         
         JNE   SPG080               NO                                          
***                                                                             
* PRINT ERROR FOR NEW STATION ALREADY EXISTS FOR ORDER HISTORY RECORD           
***                                                                             
         MVC   P+47(35),=CL35'* ERROR * ORDER HISTORY HAS NEW STA'              
         MVI   SPACING,2                                                        
         B     SPG130                                                           
*                                                                               
SPG080   MVC   BKEYSAV2(40),SAVEKEYO    NO. RESTORE KEY                         
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',BKEYSAV2,BKEY2                    
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFILE',BKEY2+36,AREC,DMWORK             
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         GOTOR CRSTRTAB,DMCB,(C'X',AREC)  KEY IN RESTORE TABLE?                 
         BNZ   SPG010                      NO, READ NEXT                        
*                                                                               
         L     R8,AREC                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   SPG090                                                           
         OI    BKEY2+32,X'80'      MARK FOR DELETION                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'XSPDIR ',BKEY2,BKEY2                   
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
*                                                                               
         OI    34(R8),X'80'        MARK RECORD FOR DELETION                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFILE',BKEY2+36,AREC,DMWORK         
*                                                                               
         TM    DM3,X'FD'                                                        
         JNZ   *+2                                                              
         NI    BKEY2+32,X'7F'      TURN OFF DELETE BIT                          
         NI    34(R8),X'7F'        TURN OFF DELETE BIT                          
*                                                                               
SPG090   BRAS  RE,SPGBLN           GET NEW BUYLINE                              
         MVC   BKEY2+OHISBKLN-OHISKEY(3),OHISBKLN                               
*                                                                               
         MVC   DUB(1),OHISBKST+2   SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),BKEY2+(OHISBKST+2-OHISKEY)                              
*                                                                               
         MVC   OHISBKST,NEWSTA     UPDATE CALL LETTERS                          
         MVC   BKEY2+OHISBKST-OHISKEY(3),NEWSTA                                 
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SPG100                                                           
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         JNE   *+2                                                              
         OC    OHISBKST+2(1),DUB     SET IN NETWORK                             
         OC    BKEY2+(OHISBKST+2-OHISKEY)(1),DUB+1                              
*                                                                               
SPG100   CLI   QOPT5,C'Y'                                                       
         BNE   SPG110                                                           
         MVI   P,C'O'               OUTPUT                                      
         GOTO1 HEXOUT,DMCB,BKEY2,P+10,40,=C'MIX',0                              
         GOTO1 REPORT                                                           
*                                                                               
SPG110   CLI   RCWRITE,C'Y'                                                     
         BNE   SPG120                                                           
         GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFILE',BKEY2+36,AREC,DMWORK             
         CLI   DM3,0                                                            
         JNE   *+2                                                              
*                                                                               
SPG120   LA    R8,BKEY2                                                         
         LH    R1,COUNT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,COUNT            # RECORDS CHANGED                            
*                                                                               
SPG130   BRAS  RE,SPGPORDH                                                      
*                                                                               
         MVC   BKEYSAV2(40),SAVEKEYO   RESTORE KEY                              
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',BKEYSAV2,BKEY2            
         CLC   BKEY2(32),SAVEKEYO                                               
         BE    SPG010                                                           
         DC    H'0'                                                             
*                                                                               
SPGX     LH    R0,COUNT                                                         
         EDIT  (R0),(5,P+4),ZERO=NOBLANK                                        
         MVC   P+10(29),=CL29'ORDER HISTORY RECORDS CHANGED'                    
         GOTO1 REPORT                                                           
         J     EXIT                                                             
*                                                                               
SPGBLN   NTR1                                                                   
         XC    HALF,HALF                                                        
         OC    HALF+1(1),OHISBKLN                                               
         JNZ   *+10                                                             
         OC    HALF,OHISBKL2                                                    
         GOTOR GTNWBYLN,DMCB,OHISBKAM,OHISBKCL,OHISBKMK,OHISBKST,      +        
               OHISBKES,(L'HALF,HALF)                                           
         JNE   EXIT                                                             
         CLI   HALF,0              1-BYTE BUYLINE?                              
         JNE   *+14                                                             
         MVC   OHISBKLN,HALF+1                                                  
         J     *+10                                                             
         MVC   OHISBKL2,HALF                                                    
         J     EXIT                                                             
*                                                                               
* PRINT ORDER HISTORY                                                           
*                                                                               
SPGPORDH NTR1                                                                   
         GOTO1 CLUNPK,DMCB,OHISBKCL,P+10           PRINT CLIENT                 
         GOTO1 MSUNPK,DMCB,OHISBKMK,P+17,P+22      PRINT MKT/STA                
         EDIT  (B1,OHISBKES),(3,P+28),ZERO=NOBLANK PRINT EST                    
*                                                                               
         GOTOR PRTORD,DMCB,OHISORD,P+32                                         
*                                                                               
         XC    HALF,HALF                                                        
         OC    HALF+1(1),SAVEKEYO+OHISBKLN-OHISKEY                              
         JNZ   *+10                                                             
         OC    HALF,SAVEKEYO+OHISBKL2-OHISKEY                                   
         EDIT  (B2,HALF),(3,P+46),ZERO=NOBLANK PRINT LINE                       
*                                                                               
         XC    HALF,HALF                                                        
         OC    HALF+1(1),OHISBKLN                                               
         JNZ   *+10                                                             
         OC    HALF,OHISBKL2                                                    
         EDIT  (B2,HALF),(3,P+55),ZERO=NOBLANK PRINT LINE                       
         GOTO1 REPORT                                                           
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R8                                                            
         EJECT                                                                  
*                                                                               
* PRINT DARE ORDER NUMBER                                                       
*                                                                               
PRTORD   NTR1  BASE=*,LABEL=*                                                   
         L     R2,4(R1)                                                         
         L     R1,0(R1)                                                         
         MVC   FULL,0(R1)                                                       
         XC    FULL,=4X'FF'                                                     
         TM    FULL,X'80' '        NEW STYLE?                                   
         BZ    PRTORD10                                                         
         NI    FULL,X'FF'-X'80'                                                 
         ICM   R1,15,FULL                                                       
         CVD   R1,DUB                                                           
         AP    DUB,=P'04000000'                                                 
         OI    DUB+7,X'0F'                                                      
         UNPK  0(8,R2),DUB                                                      
         B     PRTORDX                                                          
*                                                                               
PRTORD10 L     R1,FULL                                                          
         AHI   R1,1                                                             
         ZICM  R1,FULL,2                                                        
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R2),DUB                                                      
         ZICM  R1,FULL+2,2                                                      
         EDIT  (R1),(4,4(R2)),0,FILL=0    SEQUENCE NUMBER                       
PRTORDX  J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* SPDTCS - THIS SECTION UPDATES CAN-DTM CAMPAIGN STALIST X'0D03' RECS           
***********************************************************************         
SPDTCS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AREC,ADBUY           USE ADBUY AS THE I/O AREA                   
*                                                                               
         L     RE,AREC              CLEAR THE IO AREA                           
         L     RF,=F'6000'          FOR 6K                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    COUNT,COUNT          CLEAR THE COUNT                             
         LA    R8,BKEY2             KEY = BKEY2                                 
         XC    BKEY2,BKEY2          CLEAR THE KEY                               
         USING CSLRECD,R8           CAMPAIGN STALIST DSECT                      
         MVI   CSLKTYPE,CSLKTYPQ    X'0D'                                       
         MVI   CSLKSTYP,CSLKSTYQ    X'03'                                       
         MVC   CSLKAGMD,SVAGYMD     A/M                                         
         OC    SAVCLT,SAVCLT        CLIENT SPECIFIC REQ?                        
         BZ    *+10                 NO                                          
         MVC   CSLKCLT,SAVCLT       YES - MOVE IN THE CLIENT                    
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',BKEY2,BKEY2                       
         B     SPDTC20                                                          
*                                                                               
SPDTC10  GOTO1 DATAMGR,DMCB,DMRSEQ,=C'XSPDIR',BKEY2,BKEY2                       
*                                                                               
SPDTC20  CLC   CSLKEY(2),=X'0D03' X'0D03' RECORD?                               
         BNE   SPDTCX               NO - DONE                                   
*                                                                               
         CLC   CSLKAGMD,SVAGYMD     REQUESTED A/M?                              
         BNE   SPDTCX               NO - EXIT                                   
*                                                                               
SPDTC24  OC    SAVCLT,SAVCLT        CLIENT SPECIFIC REQ?                        
         BZ    *+14                 NO                                          
         CLC   CSLKCLT,SAVCLT       YES - HAVE THE SAME CLIENT?                 
         BNE   SPDTCX               NO - DONE                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFILE',BKEY2+36,AREC,DMWORK             
*                                                                               
         L     R6,AREC              R6 = STALIST REC                            
         MVI   ELCODE,X'03'         STATION LIST ELEMENT                        
         BRAS  RE,GETEL             GET THE ELEMENT                             
         B     *+8                  BRANCH OVER NEXTEL                          
*                                                                               
SPDTC20A BRAS  RE,NEXTEL            GET THE NEXT X'03' ELEMENT                  
         BNE   SPDTC10              DONE/NO X'03' ELEMENTS                      
*                                                                               
         USING CSLSTELD,R6          STATION LIST ELEMENT DSECT                  
         XR    RE,RE                CLEAR RE                                    
         LLC   RF,CSLSTLEN          ELEMENT LENGTH                              
         SHI   RF,CSLSTLNQ          MINUS ELEMENT CODE AND LENGTH               
         BNP   SPDTC20A             IGNORE IF ELEMENT IS BAD!                   
         D     RE,=F'5'             DIVIDE BY 5 TO GET NUMBER OF ELEMS          
         LA    R1,CSLSTSTA          START OF STATION LIST                       
         DROP  R6                                                               
*                                                                               
SPDTC20B CLC   QSTA(4),0(R1)        MATCH ON OLD STATION FOR 4 BYTES?           
         BE    SPDTC20C             YES                                         
         LA    R1,5(R1)             BUMP TO NEXT STATION                        
         BCT   RF,SPDTC20B          LOOP BACK AND TEST NEXT STATION             
         B     SPDTC20A             LOOK FOR ANOTHER X'03' ELEMENT              
*                                                                               
SPDTC20C MVC   0(4,R1),NEWCALL      YES - MOVE THE NEW CALL LETTERS IN          
         CLI   QOPT5,C'Y'           TRACE?                                      
         BNE   SPDTC25              NO                                          
         MVI   P,C'O'               OUTPUT                                      
         GOTO1 HEXOUT,DMCB,BKEY2,P+10,40,=C'MIX',0                              
         GOTO1 REPORT                                                           
*                                                                               
SPDTC25  CLI   RCWRITE,C'Y'         WRITE RECORD BACK?                          
         BNE   SPDTC30              NO                                          
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFILE',BKEY2+36,AREC,DMWORK         
         TM    DM3,X'FD'            ANY ERRORS?                                 
         JNZ   *+2                  YES - DEATH!                                
*                                                                               
SPDTC30  LH    R1,COUNT             NUM OF CAMPAIGN STAEST RECS CHANGED         
         LA    R1,1(R1)             INCIMENT BY 1                               
         STH   R1,COUNT             NEW RECORD CHANGED TOTAL                    
         B     SPDTC10              YES                                         
*                                                                               
SPDTCX   LH    R0,COUNT             RECORD COUNT                                
         EDIT  (R0),(5,P+4),ZERO=NOBLANK                                        
         MVC   P+10(36),=C'DTM CAMPAIGN STALIST RECORDS CHANGED'                
         GOTO1 REPORT               PRINT                                       
         J     EXIT                 EXIT                                        
*                                                                               
         LTORG                                                                  
         DROP  RB,R8                                                            
         EJECT                                                                  
***********************************************************************         
* SPDTP - THIS SECTION UPDATES CAN-DTM PROGRAM X'0D05' RECS                     
***********************************************************************         
SPDTP    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AREC,ADBUY           USE ADBUY AS THE I/O AREA                   
*                                                                               
         L     RE,AREC              CLEAR THE IO AREA                           
         L     RF,=F'6000'          FOR 6K                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    COUNT,COUNT          CLEAR THE COUNT                             
         XC    SAVEKEY,SAVEKEY      CLEAR SAVEKEY                               
         LA    R8,BKEY2             KEY = BKEY2                                 
         XC    BKEY2,BKEY2          CLEAR THE KEY                               
         USING DPRRECD,R8           PROGRAM RECORD DSECT                        
         MVI   DPRKTYPE,DPRKTYPQ          X'0D'                                 
         MVI   DPRKSTYP,DPRKSTYQ          X'05'                                 
         MVC   DPRKAGMD,SVAGYMD     A/M                                         
         OC    SAVCLT,SAVCLT        CLIENT SPECIFIC REQ?                        
         BZ    *+10                 NO                                          
         MVC   DPRKCLT,SAVCLT       YES - MOVE IN THE CLIENT                    
*                                                                               
SPDTP05  GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',BKEY2,BKEY2                       
         B     SPDTP20                                                          
*                                                                               
SPDTP10  GOTO1 DATAMGR,DMCB,DMRSEQ,=C'XSPDIR',BKEY2,BKEY2                       
*                                                                               
SPDTP20  CLC   DPRKEY(2),=X'0D05'      X'0D05' RECORD?                          
         BNE   SPDTPX               NO - DONE                                   
*                                                                               
         CLC   DPRKAGMD,SVAGYMD     REQUESTED A/M?                              
         BNE   SPDTPX               NO - EXIT                                   
*                                                                               
         OC    SAVCLT,SAVCLT        CLIENT SPECIFIC REQ?                        
         BZ    *+14                 NO                                          
         CLC   DPRKCLT,SAVCLT       YES - HAVE THE SAME CLIENT?                 
         BNE   SPDTPX               NO - DONE                                   
*                                                                               
         CLC   DPRKMKT(5),OLDMKT OLD MKT/STA?                                   
         BE    SPDTP24A             YES                                         
         CLC   DPRKAGMD,AGYMDN      PROCESSING MEDIA N?                         
         BE    SPDTP31              YES - CHECK X'68' ELEM                      
         B     SPDTP10              NO - READ SEQ                               
*                                                                               
SPDTP24A CLI   QOPT5,C'Y'           TRACE?                                      
         BNE   SPDTP25              NO                                          
         MVI   P,C'I'               INPUT                                       
         GOTO1 HEXOUT,DMCB,BKEY2,P+10,40,=C'MIX',0                              
         GOTO1 REPORT               PRINT KEY                                   
*                                                                               
SPDTP25  MVC   SAVEKEYO,BKEY2       SAVE THE KEY                                
         MVC   DPRKSTA,NEWSTA       NEW STATION                                 
         XC    HALF,HALF            NO LINE NUMBER YET                          
*                                                                               
         MVC   BKEYSAV2,BKEY2       SAVE THE KEY                                
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',BKEYSAV2,BKEY2            
*                                                                               
         CLC   BKEY2(32),BKEYSAV2   RECORD ALREADY EXISTS?                      
         BE    SPDTP27              YES, FIND NEXT AVAIL LINE#                  
         B     SPDTP30              OTHERWISE, GO ADD IT                        
*                                                                               
SPDTP26  GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),=C'XSPDIR',BKEY2,BKEY2               
*                                                                               
SPDTP27  CLC   BKEY2(30),BKEYSAV2   HAVE NEW STATION?                           
         BNE   SPDTP30              NO                                          
         MVC   HALF,DPRKLIN         YES - SAVE LAST LINE NUMBER                 
         B     SPDTP26              GO READ SEQ                                 
*                                                                               
SPDTP30  MVC   BKEY2,SAVEKEYO       RESTORE THE ORIG KEY W/OLD STA              
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',BKEY2,BKEY2                       
*                                                                               
SPDTP31  GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFILE',BKEY2+36,AREC,DMWORK             
*                                                                               
         CLC   DPRKAGMD,AGYMDN      MEDIA N?                                    
         BNE   SPDTP33              NO - DON'T CHECK X'68' ELEM                 
         L     R6,AREC              R6 = PROGRAM REC                            
         USING DPRNPELD,R6          NETWORK PRORATION ELEMENT DSECT             
         MVI   ELCODE,DPRNPELQ      X'68' ELEMENT                               
         BRAS  RE,GETEL             HAVE A X'68' ELEMENT?                       
         B     *+8                  BRANCH OVER NEXTEL                          
SPDTP32  BRAS  RE,NEXTEL            HAVE ANY MORE X'68' ELEMENTS?               
         BNE   SPDTP10              NO - GO READ SEQ                            
         CLC   OLDMSTAN,DPRNPMKT    MATCH ON OLD MKT/STA?                       
         BNE   SPDTP32              NO - GO CHECK FOR MORE X'68' ELEMS          
         MVC   DPRNPSTA,NEWSTAN     YES - MOVE NEW STATION IN                   
         DROP  R6                                                               
*                                                                               
         LH    R1,COUNT             NUM OF PROGRAM RECS CHANGED                 
         LA    R1,1(R1)             INCIMENT BY 1                               
         STH   R1,COUNT             NEW RECORD CHANGED TOTAL                    
*                                                                               
         CLI   RCWRITE,C'Y'         WRITE RECORD BACK?                          
         BNE   SPDTP10              NO                                          
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFILE',BKEY2+36,AREC,DMWORK         
         TM    DM3,X'FD'            ANY ERRORS?                                 
         BZ    SPDTP10              NO                                          
         DC    H'0'                 YES - DEATH!                                
*                                                                               
SPDTP33  L     R8,AREC              R8 = PROGRAM REC                            
         CLI   RCWRITE,C'Y'         WRITE RECORD BACK?                          
         BNE   SPDTP35              NO                                          
         OI    BKEY2+32,X'80'       MARK DIRECTORY KEY FOR DELETION             
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'XSPDIR ',BKEY2,BKEY2                   
         TM    DM3,X'FD'            ANY ERRORS?                                 
         JNZ   *+2                  YES - DEATH!                                
*                                                                               
         OI    34(R8),X'80'         MARK RECORD FOR DELETION                    
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFILE',BKEY2+36,AREC,DMWORK         
         TM    DM3,X'FD'            ANY ERRORS?                                 
         JNZ   *+2                  YES - DEATH!                                
*                                                                               
         NI    BKEY2+32,X'7F'       TURN OFF DELETE BIT IN KEY                  
         NI    34(R8),X'7F'         TURN OFF DELETE BIT IN REC                  
*                                                                               
SPDTP35  MVC   DPRKSTA,NEWSTA       NEW CALL LETTERS IN RECORD                  
         XR    RE,RE                CLEAR RE                                    
         ICM   RE,3,HALF            NEW STA ALREADY HAS A LINE NUMBER?          
         BZ    SPDTP36              NO                                          
         STCM  RE,3,DPRKLIN         SAVE NEW LINE NUMBER IN RECORD              
*                                                                               
SPDTP36  LA    R8,BKEY2             RESTORE R8                                  
         MVC   DPRKSTA,NEWSTA       NEW CALL LETTERS KEY                        
         LTR   RE,RE                HAVE A NEW LINE NUMBER                      
         BZ    *+8                  NO                                          
         STCM  RE,3,DPRKLIN         YES - SAVE NEW LINE NUMBER IN KEY           
*                                                                               
         CLI   QOPT5,C'Y'           TRACE?                                      
         BNE   SPDTP40              NO                                          
         MVI   P,C'O'               OUTPUT                                      
         GOTO1 HEXOUT,DMCB,BKEY2,P+10,40,=C'MIX',0                              
         GOTO1 REPORT                                                           
*                                                                               
SPDTP40  CLI   RCWRITE,C'Y'         WRITE RECORD BACK?                          
         BNE   SPDTP45              NO                                          
         GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFILE',BKEY2+36,AREC,DMWORK             
         CLI   DM3,0                ANY ERRORS?                                 
         JNE   *+2                  YES - DEATH                                 
*                                                                               
SPDTP45  LH    R1,COUNT             NUM OF PROGRAM RECS CHANGED                 
         LA    R1,1(R1)             INCIMENT BY 1                               
         STH   R1,COUNT             NEW RECORD CHANGED TOTAL                    
*                                                                               
SPDTP50  MVC   BKEY2,SAVEKEYO       RESTORE KEY                                 
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',BKEY2,BKEY2               
         CLC   BKEY2(32),SAVEKEYO   HAVE THE KEY WE JUST PROCESSED?             
         BE    SPDTP10              YES                                         
         DC    H'0'                 NO - DEATH                                  
*                                                                               
SPDTPX   LH    R0,COUNT             RECORD COUNT                                
         EDIT  (R0),(5,P+4),ZERO=NOBLANK                                        
         MVC   P+10(27),=C'DTM PROGRAM RECORDS CHANGED'                         
         GOTO1 REPORT               PRINT                                       
         J     EXIT                 EXIT                                        
         LTORG                                                                  
         DROP  RB,R8                                                            
         EJECT                                                                  
***********************************************************************         
* SPDTD - THIS SECTION UPDATES CAN-DTM DEMO X'0D06' RECS                        
***********************************************************************         
SPDTD    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AREC,ADBUY           USE ADBUY AS THE I/O AREA                   
*                                                                               
         L     RE,AREC              CLEAR THE IO AREA                           
         L     RF,=F'6000'          FOR 6K                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    COUNT,COUNT          CLEAR THE COUNT                             
         LA    R7,BKEY2             KEY = BKEY2                                 
         XC    BKEY2,BKEY2          CLEAR THE KEY                               
         USING DDMRECD,R7           R7 = DTM DEMO KEY DSECT                     
         L     R8,AREC              R8 = DTM DEMO REC                           
R        USING DDMRECD,R8           DTM DEMO RECORD DSECT                       
         MVI   DDMKTYPE,DDMKTYPQ          X'0D'                                 
         MVI   DDMKSTYP,DDMKSTYQ          X'06'                                 
         MVC   DDMKAGMD,SVAGYMD     A/M                                         
         OC    SAVCLT,SAVCLT        CLIENT SPECIFIC REQ?                        
         BZ    *+10                 NO                                          
         MVC   DDMKCLT,SAVCLT       YES - MOVE IN THE CLIENT                    
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',BKEY2,BKEY2                       
         B     SPDTD20                                                          
*                                                                               
SPDTD10  GOTO1 DATAMGR,DMCB,DMRSEQ,=C'XSPDIR',BKEY2,BKEY2                       
*                                                                               
SPDTD20  CLC   DDMKEY(2),=X'0D06' X'0D06' RECORD?                               
         BNE   SPDTDX               NO - DONE                                   
*                                                                               
         CLC   DDMKAGMD,SVAGYMD     REQUESTED A/M?                              
         BNE   SPDTDX               NO - EXIT                                   
*                                                                               
         OC    SAVCLT,SAVCLT        CLIENT SPECIFIC REQ?                        
         BZ    *+14                 NO                                          
         CLC   DDMKCLT,SAVCLT       YES - HAVE THE SAME CLIENT?                 
         BNE   SPDTDX               NO - DONE                                   
*                                                                               
         CLC   DDMKAGMD,AGYMDN      MEDIA N?                                    
         BNE   SPDTD21              NO                                          
         OC    OLDMKT,OLDMKT        MARKET ZERO (NETWORK REQUEST)?              
         BNZ   *+14                 NO - THIS IS A PASSIVE POINTER              
         CLC   DDMKSTA,OLDSTAN      MATCH ON STATION?                           
         B     *+10                 GO TEST CC                                  
         CLC   DDMKLCL,OLDSTAN      MATCH ON LOCAL STATION?                     
         B     *+10                 GO TEST CC                                  
SPDTD21  CLC   OLDMSTA,DDMKMKT      OLD MKT/STA?                                
         BNE   SPDTD10              NO - READ SEQ                               
*                                                                               
         CLI   DDMKSPL,C'S'         SPILL POINTER FLAG?                         
         BE    SPDTD10              YES - LET THE DUMP & LOAD ADD THIS          
*                                                                               
         CLI   QOPT5,C'Y'           TRACE?                                      
         BNE   SPDTD25              NO                                          
         MVI   P,C'I'               OUTPUT                                      
         GOTO1 HEXOUT,DMCB,BKEY2,P+10,40,=C'MIX',0                              
         GOTO1 REPORT               PRINT INPUT KEY                             
*                                                                               
SPDTD25  MVC   SAVEKEYO,BKEY2       SAVE THE KEY                                
         XC    HALF,HALF            NO LINE NUMBER YET                          
         CLC   DDMKAGMD,AGYMDN      MEDIA N?                                    
         BNE   SPDTD26              NO                                          
         OC    OLDMKT,OLDMKT        MARKET ZERO (NETWORK REQUEST)?              
         BNZ   *+14                 NO                                          
         MVC   DDMKSTA,NEWSTAN      NEW STATION                                 
         B     SPDTD27              STA SET                                     
         MVC   DDMKLCL,NEWSTAN      NEW LOCAL STATION                           
         B     SPDTD27              STA SET                                     
*                                                                               
SPDTD26  MVC   DDMKSTA,NEWSTA       NEW STATION                                 
*                                                                               
SPDTD27  MVC   BKEYSAV2,BKEY2       SAVE THE KEY                                
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',BKEYSAV2,BKEY2            
*                                                                               
         CLC   BKEY2(32),BKEYSAV2   RECORD ALREADY EXISTS?                      
         BE    SPDTD29              YES, FIND NEXT AVAIL LINE#                  
         B     SPDTD30              OTHERWISE, GO ADD IT                        
*                                                                               
SPDTD28  GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),=C'XSPDIR',BKEY2,BKEY2               
*                                                                               
SPDTD29  CLC   BKEY2(26),BKEYSAV2   HAVE NEW STATION?                           
         BNE   SPDTD30              NO                                          
         CLC   DDMKLCL,BKEYSAV2+28     LOCAL STA MATCH?                         
         BNE   SPDTD30              NO                                          
         CLI   DDMKSPL,C'S'         DID WE FIND SPILL?                          
         BE    SPDTD28              YES - IGNORE IT                             
         MVC   HALF,DDMKLIN         YES - SAVE LAST LINE NUMBER                 
         B     SPDTD28              GO READ SEQ                                 
*                                                                               
SPDTD30  MVC   BKEY2,SAVEKEYO       RESTORE THE ORIG KEY W/OLD STA              
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',BKEY2,BKEY2                       
*                                                                               
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFILE',BKEY2+36,AREC,DMWORK             
*                                                                               
         CLI   RCWRITE,C'Y'         WRITE RECORD BACK?                          
         BNE   SPDTD35              NO                                          
         OI    BKEY2+32,X'80'       MARK DIRECTORY KEY FOR DELETION             
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'XSPDIR ',BKEY2,BKEY2                   
         TM    DM3,X'FD'            ANY ERRORS?                                 
         JNZ   *+2                  YES - DEATH!                                
*                                                                               
         NI    BKEY2+32,X'7F'       TURN OFF DELETE BIT IN KEY                  
         CLC   DDMKAGMD,AGYMDN      MEDIA N?                                    
         BNE   SPDTD31              NO                                          
         OC    DDMKMKT,DDMKMKT         KEY HAS MARKET ZERO?                     
         BNZ   SPDTD35              NO - REC ONLY DELETED ON MKT ZERO           
*                                                                               
SPDTD31  OI    R.DDMSTAT,X'80'      MARK RECORD FOR DELETION                    
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFILE',BKEY2+36,AREC,DMWORK         
         TM    DM3,X'FD'            ANY ERRORS?                                 
         JNZ   *+2                  YES - DEATH!                                
*                                                                               
         NI    R.DDMSTAT,X'7F'      TURN OFF DELETE BIT IN REC                  
*                                                                               
SPDTD35  XR    RE,RE                CLEAR RE                                    
         ICM   RE,3,HALF            HAVE HIGHEST LINE NUMBER?                   
         BZ    SPDTD37              NO - DON'T CHANGE                           
         CLC   DDMKAGMD,AGYMDN      MEDIA N?                                    
         BNE   SPDTD36              NO                                          
         OC    DDMKMKT,DDMKMKT        MARKET ZERO?                              
         BZ    SPDTD36              YES - OK TO CHANGE LINE NUMBER              
         ICM   RE,3,R.DDMKLIN       GET LINE NUMBER OF CURRENT REC              
         B     *+12                 PUT NEW LINE NUM IN PASSIVE KEY             
SPDTD36  AHI   RE,1                 YES - BUMP TO NEXT AVAILABLE LINE           
         STCM  RE,3,R.DDMKLIN       NEW LINE NUMBER IN REC                      
         STCM  RE,3,DDMKLIN         NEW LINE NUMBER IN KEY                      
*                                                                               
SPDTD37  CLC   DDMKAGMD,AGYMDN      MEDIA N?                                    
         BNE   SPDTD39              NO                                          
         OC    OLDMKT,OLDMKT        MARKET ZERO (NETWORK REQUEST)?              
         BZ    SPDTD38              YES                                         
         MVC   DDMKLCL,NEWSTAN      NEW LOCAL STATION IN KEY                    
         MVC   R.DDMKLCL,NEWSTAN NEW LOCAL STATION IN RECORD                    
         B     SPDTD40              STA SET                                     
*                                                                               
SPDTD38  MVC   DDMKSTA,NEWSTAN      NEW STATION IN KEY                          
         MVC   R.DDMKSTA,NEWSTAN NEW STATION IN RECORD                          
         B     SPDTD40              STA SET                                     
*                                                                               
SPDTD39  MVC   DDMKSTA,NEWSTA       NEW STATION IN KEY                          
         MVC   R.DDMKSTA,NEWSTA     NEW STATION IN RECORD                       
*                                                                               
SPDTD40  CLI   QOPT5,C'Y'           TRACE?                                      
         BNE   SPDTD41              NO                                          
         MVI   P,C'O'               OUTPUT                                      
         GOTO1 HEXOUT,DMCB,BKEY2,P+10,40,=C'MIX',0                              
         GOTO1 REPORT               PRINT OUTPUT KEY                            
*                                                                               
SPDTD41  CLI   RCWRITE,C'Y'         WRITE RECORD BACK?                          
         BNE   SPDTD45              NO                                          
         CLC   DDMKAGMD,AGYMDN      MEDIA N?                                    
         BNE   SPDTD42              NO                                          
         OC    DDMKMKT,DDMKMKT        KEY HAS MARKET ZERO?                      
         BZ    SPDTD42              YES - ADD THE DIR AND FIL                   
         GOTO1 DATAMGR,DMCB,DMADD,=C'XSPDIR',BKEY2,BKEY2,DMWORK                 
         CLI   DM3,0                ANY ERRORS?                                 
         BE    SPDTD45              NO                                          
         DC    H'0'                 YES - DEATH                                 
*                                                                               
SPDTD42  GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFILE',BKEY2+36,AREC,DMWORK             
         CLI   DM3,0                ANY ERRORS?                                 
         JNE   *+2                  YES - DEATH                                 
*                                                                               
SPDTD45  LH    R1,COUNT             NUM OF DTM DEMO RECS CHANGED                
         LA    R1,1(R1)             INCIMENT BY 1                               
         STH   R1,COUNT             NEW RECORD CHANGED TOTAL                    
*                                                                               
SPDTD50  MVC   BKEY2,SAVEKEYO       RESTORE KEY                                 
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',BKEY2,BKEY2               
         CLC   BKEY2(32),SAVEKEYO   HAVE THE KEY WE JUST PROCESSED?             
         BE    SPDTD10              YES                                         
         DC    H'0'                 NO - DEATH                                  
*                                                                               
SPDTDX   LH    R0,COUNT             RECORD COUNT                                
         EDIT  (R0),(5,P+4),ZERO=NOBLANK                                        
         MVC   P+10(24),=C'DTM DEMO RECORDS CHANGED'                            
         GOTO1 REPORT               PRINT                                       
         J     EXIT                 EXIT                                        
         LTORG                                                                  
         DROP  RB,R7,R                                                          
         EJECT                                                                  
***********************************************************************         
* SPDTO - THIS SECTION UPDATES CAN-DTM ORDER X'0D07' RECS                       
***********************************************************************         
SPDTO    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AREC,ADBUY           USE ADBUY AS THE I/O AREA                   
*                                                                               
         L     RE,AREC              CLEAR THE IO AREA                           
         L     RF,=F'6000'          FOR 6K                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    COUNT,COUNT          CLEAR THE COUNT                             
         LA    R8,BKEY2             KEY = BKEY2                                 
         XC    BKEY2,BKEY2          CLEAR THE KEY                               
         USING CORRECD,R8           PROGRAM RECORD DSECT                        
         MVI   CORKTYPE,CORKTYPQ          X'0D'                                 
         MVI   CORKSTYP,CORKSTYQ          X'07'                                 
         MVC   CORKAGMD,SVAGYMD        A/M                                      
         OC    SAVCLT,SAVCLT        CLIENT SPECIFIC REQ?                        
         BZ    *+10                 NO                                          
         MVC   CORKCLT,SAVCLT       YES - MOVE IN THE CLIENT                    
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',BKEY2,BKEY2                       
         B     SPDTO20                                                          
*                                                                               
SPDTO10  GOTO1 DATAMGR,DMCB,DMRSEQ,=C'XSPDIR',BKEY2,BKEY2                       
*                                                                               
SPDTO20  CLC   CORKEY(2),=X'0D07' X'0D07' RECORD?                               
         BNE   SPDTOX               NO - DONE                                   
*                                                                               
         CLC   CORKAGMD,SVAGYMD     REQUESTED A/M?                              
         BNE   SPDTOX               NO - EXIT                                   
*                                                                               
         OC    SAVCLT,SAVCLT        CLIENT SPECIFIC REQ?                        
         BZ    *+14                 NO                                          
         CLC   CORKCLT,SAVCLT       YES - HAVE THE SAME CLIENT?                 
         BNE   SPDTOX               NO - DONE                                   
*                                                                               
SPDTO25  GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFILE',BKEY2+36,AREC,DMWORK             
*                                                                               
         L     R6,AREC              R6 = ORDER REC                              
         USING CORIDELD,R6          ORDER ID ELEMENT DSECT                      
         MVI   ELCODE,CORIDELQ      X'02' ELEMENT                               
         BRAS  RE,GETEL             HAVE A X'02' ELEMENT?                       
         B     *+8                  BRANCH OVER NEXTEL                          
SPDTO26  BRAS  RE,NEXTEL            HAVE ANY MORE X'02' ELEMENTS?               
         BNE   SPDTO10              NO - GO READ SEQ                            
         CLC   CORIDMKT(5),OLDMKT      MATCH ON OLD MKT/STA?                    
         BNE   SPDTO26              NO - GO CHECK FOR MORE X'02' ELEMS          
         CLI   QOPT5,C'Y'           TRACE?                                      
         BNE   SPDTO27              NO                                          
         MVC   P(11),=C'02 ELM CHG' ONLY THE X'02' ELEM CHANGED                 
         GOTO1 HEXOUT,DMCB,BKEY2,P+10,40,=C'MIX',0                              
         GOTO1 REPORT               PRINT KEY                                   
*                                                                               
SPDTO27  MVC   CORIDSTA,NEWSTA      YES - MOVE NEW STATION IN                   
         DROP  R6                   DROP R6                                     
*                                                                               
         LH    R1,COUNT             NUM OF PROGRAM RECS CHANGED                 
         LA    R1,1(R1)             INCIMENT BY 1                               
         STH   R1,COUNT             NEW RECORD CHANGED TOTAL                    
*                                                                               
         CLI   RCWRITE,C'Y'         WRITE RECORD BACK?                          
         BNE   SPDTO10              NO                                          
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFILE',BKEY2+36,AREC,DMWORK         
         TM    DM3,X'FD'            ANY ERRORS?                                 
         BZ    SPDTO10              NO                                          
         DC    H'0'                 YES - DEATH!                                
*                                                                               
SPDTOX   LH    R0,COUNT             RECORD COUNT                                
         EDIT  (R0),(5,P+4),ZERO=NOBLANK                                        
         MVC   P+10(25),=C'DTM ORDER RECORDS CHANGED'                           
         GOTO1 REPORT               PRINT                                       
         J     EXIT                 EXIT                                        
         LTORG                                                                  
         DROP  RB,R8                                                            
         EJECT                                                                  
***********************************************************************         
* SPBTKRC - THIS SECTION UPDATES US-SBTK REVISION/WORK RECORDS                  
***********************************************************************         
         USING SPBTKRCD,R5                                                      
SPBTKRC  NTR1  BASE=*,LABEL=*,WORK=(R5,SPBTKRCL)                                
*                                                                               
         LR    R0,R5               CLEAR SAVED STORAGE                          
         LHI   R1,SPBTKRCL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   SBRSUBT,DM1+3       SAVE RECORD SUBTYPE                          
*                                                                               
         CLI   CANAGY,C'Y'         ARE WE DEALING WITH A CANADIAN AGY?          
         BNE   *+12                                                             
         CLI   QMED,C'C'           COMBINED?                                    
         BE    SPBTKRCX                                                         
*                                                                               
         GOTO1 GTSTAEDT,DMCB,QSTA,SPOLDSTA                                      
         OC    SPOLDSTA,SPACES                                                  
         GOTO1 GTSTAEDT,DMCB,NEWCALL,SPNEWSTA                                   
         OC    SPNEWSTA,SPACES                                                  
*                                                                               
         MVC   AREC,ADBUY           USE ADBUY AS THE I/O AREA                   
*                                                                               
         L     RE,AREC              CLEAR THE IO AREA                           
         L     RF,=F'6000'          FOR 6K                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    COUNT,COUNT          CLEAR THE COUNT                             
         XC    BKEY2,BKEY2          CLEAR THE KEY                               
         LA    R8,BKEY2             KEY = BKEY2                                 
         USING DRVRECD,R8           SBTK REVISION/WORK DSECT                    
         MVI   DRVKTYP,DRVKTYPQ      X'0E'                                      
         MVC   DRVKSUB,SBRSUBT       SET PASSED SUBTYPE                         
         MVC   DRVKAM,SVAGYMD        A/M                                        
         OC    SAVCLT,SAVCLT        CLIENT SPECIFIC REQ?                        
         BZ    *+10                 NO                                          
         MVC   DRVKCLT,SAVCLT       YES - MOVE IN THE CLIENT                    
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',BKEY2,BKEY2                       
         B     SPBR010                                                          
*                                                                               
SPBRSEQ  GOTO1 DATAMGR,DMCB,DMRSEQ,=C'XSPDIR',BKEY2,BKEY2                       
*                                                                               
SPBR010  CLI   DRVKTYP,DRVKTYPQ    MATCH RECORD TYPE?                           
         BNE   SPBTKRCX            NO - DONE                                    
         CLC   DRVKSUB,SBRSUBT     MATCH RECORD TYPE?                           
         BNE   SPBTKRCX            NO - DONE                                    
*                                                                               
         CLC   DRVKAM,SVAGYMD      REQUESTED A/M?                               
         BNE   SPBTKRCX            NO - EXIT                                    
*                                                                               
         OC    SAVCLT,SAVCLT       CLIENT SPECIFIC REQ?                         
         BZ    SPBR020             NO                                           
         CLC   DRVKCLT,SAVCLT      YES - HAVE THE SAME CLIENT?                  
         BNE   SPBTKRCX            NO - DONE                                    
*                                                                               
SPBR020  GOTOR CRSTRTAB,DMCB,(C'X',BKEY2) KEY IN RESTORE TABLE?                 
         BNZ   SPBRSEQ                     NO, READ NEXT                        
*                                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   SPBRGET                                                          
         MVI   TRACECDE,C'I'                                                    
         BRAS  RE,SPBRTRAC         ***TRACE***                                  
*                                                                               
SPBRGET  GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFILE',BKEY2+36,AREC,DMWORK             
*                                                                               
         MVI   CHGEL,0                                                          
*                                                                               
         L     R6,AREC             R6 = SBTK REVSHEET/REVLINE RECORD            
         OC    DRVKREVL-DRVKEY(L'DRVKREVL,R6),DRVKREVL-DRVKEY(R6)               
         BNZ   SPBR050             NOT A SHEET RECORD                           
*                                                                               
* PROCESSING A SBTK SHEET RECORD                                                
*                                                                               
         MVC   WORK,SPACES                                                      
         USING RSNELD,R6           SHEET NAME DSECT                             
         MVI   ELCODE,RSNELQ       X'10' ELEMENT                                
         BRAS  RE,GETEL            HAVE A X'10' ELEMENT?                        
         BNE   SPBR030              NO - CHECK CHGEL                            
         LLC   RF,RSNLEN                                                        
         BCTR  RF,0                                                             
         CHI   RF,30                                                            
         BL    *+8                                                              
         LHI   RF,29                                                            
         MVC   WORK+20(0),RSNAME   SAVE THE NAME OF SHEET                       
         EX    RF,*-6                                                           
         DROP  R6                                                               
*                                                                               
         L     R6,AREC             R6 = SBTK REVSHEET/REVLINE RECORD            
         USING RSPHELD,R6          DAY/TIME PERIOD OVERRIDE DSECT               
SPBR030  MVI   ELCODE,RSPHELQ      X'45' ELEMENT                                
         BRAS  RE,GETEL            HAVE A X'45' ELEMENT?                        
         B     *+8                 BRANCH OVER NEXTEL                           
SPBR040  BRAS  RE,NEXTEL           HAVE ANY MORE X'45' ELEMENTS?                
         BNE   SPBR070              NO - CHECK CHGEL                            
         CLI   RSPHSTA,C'0'        CABLE?                                       
         BNL   SPBR040                YES, SKIP THEM                            
*                                                                               
         CLC   SPOLDSTA,RSPHSTA    MATCH ON STATION?                            
         BNE   SPBR040             NO - GO CHECK FOR MORE X'02' ELEMS           
         MVC   RSPHSTA,SPNEWSTA                                                 
         MVI   CHGEL,C'Y'                                                       
         B     SPBR040                                                          
         DROP  R6                                                               
*                                                                               
* PROCESSING A SBTK LINE RECORD                                                 
*                                                                               
         USING RLDELD,R6           LINE DESCRIPTION DSECT                       
SPBR050  MVI   ELCODE,RLDELQ       X'11' ELEMENT                                
         BRAS  RE,GETEL            HAVE A X'11' ELEMENT?                        
         B     *+8                 BRANCH OVER NEXTEL                           
SPBR060  BRAS  RE,NEXTEL           HAVE ANY MORE X'02' ELEMENTS?                
         BNE   SPBR070             NO - GO READ SEQ                             
         CLI   RLDSTA,C'0'         CABLE?                                       
         BNL   SPBR060              YES, SKIP THEM                              
*                                                                               
         CLC   RLDSTA,SPOLDSTA     MATCH ON STATION?                            
         BNE   SPBR070              NO -                                        
         MVC   RLDSTA,SPNEWSTA                                                  
         MVI   CHGEL,C'Y'                                                       
*                                                                               
SPBR070  CLI   CHGEL,C'Y'           ELEM UPDATED? PUT REC BACK?                 
         BNE   SPBRSEQ               NO                                         
*                                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   SPBR080                                                          
         MVI   TRACECDE,C'O'                                                    
         BRAS  RE,SPBRTRAC         ***TRACE***                                  
*                                                                               
SPBR080  LH    R1,COUNT             NUM OF PROGRAM RECS CHANGED                 
         LA    R1,1(R1)             INCIMENT BY 1                               
         STH   R1,COUNT             NEW RECORD CHANGED TOTAL                    
*                                                                               
SPBR090  GOTO1 CLUNPK,DMCB,DRVKCLT,P+10            PRINT CLIENT                 
         MVC   P+17(3),DRVKPRD                     PRINT PRODUCT                
         EDIT  (B1,DRVKEST),(3,P+23),ZERO=NOBLANK  PRINT EST                    
         EDIT  (B2,DRVKMKT),(4,P+30),ZERO=NOBLANK  PRINT MKT                    
*                                                                               
         CLI   WORK+20,C' '        HAVE SHEET NAME TO PRINT?                    
         BNH   SPBR100                                                          
         OC    DRVKREVL-DRVKEY(L'DRVKREVL,R8),DRVKREVL-DRVKEY(R8)               
         BNZ   *+8                                                              
         MVI   P+55,C'*'           MARKER TO SHOW SHEET WAS CHANGED             
         MVC   P+56(30),WORK+20    PRINT SHEET NAME                             
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   WORK,SPACES         DON'T PRINT SHEET NAME AGAIN                 
         B     SPBR090                                                          
*                                                                               
SPBR100  OC    DRVKREVL-DRVKEY(L'DRVKREVL,R8),DRVKREVL-DRVKEY(R8)               
         BZ    SPBRSEQ                                                          
*                                                                               
* HAVE A LINE RECORD                                                            
*                                                                               
         MVC   P+56(2),=C'--'                                                   
         MVC   P+58(L'RLDPROG),RLDPROG   PRINT LINE PROGRAM NAME                
*                                                                               
         OC    RLDBLINE,RLDBLINE                                                
         JZ    SPBR115                                                          
*                                                                               
         EDIT  (B2,RLDBLINE),(3,P+42)               PRINT LINE                  
*                                                                               
         GOTOR GTNWBYLN,DMCB,DRVKAM,DRVKCLT,DRVKMKT,OLDSTA,DRVKEST,    +        
               (L'RLDBLINE,RLDBLINE)                                            
*                                                                               
         MVI   P+52,C'*'                                                        
         EDIT  (B2,RLDBLINE),(3,P+51)               PRINT LINE                  
         DROP  R6                                                               
*                                                                               
         LR    R4,R6                                                            
         USING RLXELD,R6           LINE DESCRIPTION DSECT                       
         MVI   ELCODE,RLXELQ       X'11' ELEMENT                                
         BRAS  RE,NEXTEL           HAVE ANY MORE X'02' ELEMENTS?                
         BNE   SPBR115                                                          
         MVC   RLXBUYLN,RLDBLINE-RLDELD(R4)                                     
         DROP  R6                                                               
*                                                                               
SPBR115  MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
         CLI   RCWRITE,C'Y'         WRITE RECORD BACK?                          
         BNE   SPBRSEQ              NO                                          
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFILE',BKEY2+36,AREC,DMWORK         
         TM    DM3,X'FD'            ANY ERRORS?                                 
         BZ    SPBRSEQ              NO                                          
         DC    H'0'                 YES - DEATH!                                
*                                                                               
SPBTKRCX LH    R0,COUNT             RECORD COUNT                                
         EDIT  (R0),(5,P+4),ZERO=NOBLANK                                        
*                                                                               
         MVC   P+10(28),=C'US SBTK WORK RECORDS CHANGED'                        
         CLI   SBRSUBT,DWKKSUBQ    PROCESSING WORK RECORDS?                     
         BE    *+10                                                             
         MVC   P+10(32),=C'US SBTK REVISION RECORDS CHANGED'                    
*                                                                               
         GOTO1 REPORT               PRINT                                       
         J     EXIT                 EXIT                                        
         DROP  R8                                                               
*                                                                               
SPBRTRAC NTR1                                                                   
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVC   P(1),TRACECDE                                                    
         GOTO1 HEXOUT,DMCB,BKEY2,P+10,32,=C'MIX',0                              
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         MVI   TRACECDE,0                                                       
         XIT1                                                                   
***********************************************************************         
* MUST MODIFY STATION TO FIT REVISION/WORKSHEET FORMAT                          
*   ON ENTRY : P1    SSSS OR SSSSB                                              
*   ON EXIT  : P2    SSSS (BAND T) O/W SSSS-B OR SSS-B                          
***********************************************************************         
GTSTAEDT NTR1                                                                   
         L     R1,DMCB             R1 = A(INPUT STATION)                        
         L     R2,DMCB+4           R2 = A(OUTPUT STATION)                       
*                                                                               
         LA    RE,3                SET EX TO COPY 4 CHARS                       
         CLI   3(R1),C' '          3 CHAR STATION?                              
         BH    *+6                 NO                                           
         BCTR  RE,0                YES, SET EX TO COPY 3 CHARS                  
*                                                                               
         MVC   0(0,R2),0(R1)                                                    
         EX    RE,*-6              COPY THE STATION                             
*                                                                               
         LA    RF,1(RE,R2)         POINT TO END STATION                         
         CLI   4(R1),C' '          HAVE BAND?                                   
         BE    GTSTAEDTX           NO, DONE                                     
*                                                                               
         CLI   QMED,C'R'           MEDIA R?                                     
         BNE   GTSTA010                                                         
         MVI   0(RF),C'-'          MOVE IN -                                    
         MVC   1(1,RF),4(R1)       AND BAND(IE. WABC-F, DXNY-S, YYHW-C)         
         B     GTSTAEDTX                                                        
*                                                                               
GTSTA010 CLI   QMED,C'X'           MEDIA X?                                     
         BE    GTSTA020             MOVE IN BAND (IE WABCX)                     
*                                                                               
         CLI   QMED,C'T'           MEDIA T?                                     
         JNE   *+2                  NEW MEDIA? CHECK STATION FORMAT!!           
*                                                                               
         CLI   4(R1),C'T'          MEDIA T WITH 'T' BAND?                       
         BE    GTSTAEDTX           YES, LEAVE OFF THE T (IE. WABC)              
*                                                                               
GTSTA020 MVC   0(1,RF),4(R1)       MOVE IN BAND (IE CMNYD, YYSLL)               
*                                                                               
GTSTAEDTX J    EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
SPBTKRCD DSECT                                                                  
SBRSUBT  DS    X                   RECORD SUBTYPE                               
SPOLDSTA DS    CL(L'RLDSTA)                                                     
SPNEWSTA DS    CL(L'RLDSTA)                                                     
SPBTKRCL EQU   *-SPBTKRCD                                                       
SPSC02   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK IF BUY RECORD LINE NUMBER CHANGED, AND RETURN NEW LINE NUMBER           
*   ON ENTRY : P1      A(1-BYTE BIN AGYMD)                                      
*            : P2      A(2-BYTE BIN CLIENT)                                     
*            : P3      A(2-BYTE BIN MARKET)                                     
*            : P4      A(3-BYTE BIN STATION)                                    
*            : P5      A(1-BYTE BIN ESTIMATE)                                   
*            : P6      A(2-BYTE BIN LINE NUMBER)                                
*                                                                               
*   ON EXIT  : CC    EQ NEW LINE NUMBER                                         
*              P6    UPDATED WITH NEW LINE NUMBER)                              
*              CC    NEQ - NO NEW LINE NUMBER                                   
*                                                                               
***********************************************************************         
GTNWBYLN NTR1  BASE=*,LABEL=*                                                   
                                                                                
BK       USING BUYRECD,WORK                                                     
         XC    WORK,WORK                                                        
         L     R2,0(R1)                                                         
         MVC   BK.BUYKAM,0(R2)                                                  
         L     R2,4(R1)                                                         
         MVC   BK.BUYKCLT,0(R2)                                                 
         MVI   BK.BUYKPRD,X'FF'                                                 
         L     R2,8(R1)                                                         
         MVC   BK.BUYKMKTN,0(R2)                                                
         L     R2,12(R1)                                                        
         MVC   BK.BUYKSTAC,0(R2)                                                
         L     R2,16(R1)                                                        
         MVC   BK.BUYKEST,0(R2)                                                 
         L     R2,20(R1)                                                        
         MVC   BK.BUYRLIN,0(R2)                                                 
         DROP  BK                                                               
*                                                                               
GTNB020  GOTOR BINSRCH,BKTPARMS,(X'00',WORK)    FIND EXACT MATCH                
         TM    0(R1),X'01'         RECORD FOUND?                                
         JO    NO                                                               
         L     RE,0(R1)                                                         
         MVC   0(2,R2),BTNEWBLN-BUYTBLD(RE)                                     
         J     YES                                                              
*                                                                               
***********************************************************************         
* EMAIL                                                                         
***********************************************************************         
EMAIL    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,ADCONLST                                                      
         USING SPADCONS,R6                                                      
         GOTOR VSMTP,DMCB,('SMTPAINI',JESMAIL)                                  
*                                                                               
         LA    R5,FULL                                                          
         EXTRACT (R5),FIELDS=TIOT                                               
*                                                                               
         L     R5,FULL                                                          
         USING TIOTD,R5                                                         
         MVC   SUBJECT+39(8),TIOCNJOB                                           
         DROP  R5                                                               
*                                                                               
         GOTOR VSMTP,DMCB,('SMTPATCS',SMTPTO),(L'SUBJECT,SUBJECT),     +        
               (0,SMTPCC),0                                                     
*                                                                               
***      GOTOR VSMTP,DMCB,('SMTPAPRS',TOWHO),(L'SUBJECT,SUBJECT)                
         MVC   P,SPACES                                                         
         MVC   P(13),=C'CANNOT RE-USE'                                          
         MVC   P+14(5),QBOOK1                                                   
         MVC   P+20(16),=C'UNTIL NEXT WEEK.'                                    
*                                                                               
         MVC   P+37(13),=C'PLEASE INFORM'                                       
*                                                                               
         L     R5,VMASTC                                                        
         USING MASTD,R5                                                         
         LA    R2,MCRFHDR                                                       
         USING RQHITRM,R2                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'            PERSONAL AUTH RECS IN CTFILE                 
         MVC   KEY+1(2),QAGY                                                    
         MVC   KEY+23(2),RQHPSWD   PERSONAL AUTH NUMBER                         
         DROP  R2,R5                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,ADBUY                     
         L     R5,ADBUY                                                         
         LA    R2,P+51                                                          
         CLC   KEY(25),0(R5)       FOUND IT?                                    
         BNE   GN99                NO                                           
         LA    R1,28(R5)           POINT TO FIRST ELEM                          
*                                                                               
GN10     CLI   0(R1),0             END OF RECORD?                               
         BE    GN99                YES                                          
         CLI   0(R1),X'C3'         C3 ELEM?                                     
         BE    GN15                YUP                                          
         LLC   R4,1(R1)            BUMP TO NEXT ELEM                            
         AR    R1,R4                                                            
         B     GN10                                                             
*                                                                               
GN15     XC    KEY,KEY                                                          
         MVI   KEY,C'F'                                                         
         MVI   KEY+1,X'04'                                                      
         MVC   KEY+13(2),QAGY                                                   
         MVC   KEY+15(8),2(R1)     PERSONAL ID                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,ADBUY                     
         L     R5,ADBUY                                                         
         CLC   KEY(23),0(R5)       FOUND IT?                                    
         BNE   GN99                NO                                           
         LA    R1,28(R5)           POINT TO FIRST ELEM                          
*                                                                               
GN20     CLI   0(R1),0             END OF RECORD?                               
         BE    GN99                YES                                          
         CLI   0(R1),X'C5'         C5 ELEM?                                     
         BE    GN25                YUP                                          
         LLC   R4,1(R1)            BUMP TO NEXT ELEM                            
         AR    R1,R4                                                            
         B     GN20                NOPE                                         
*                                                                               
GN25     LA    R5,SANAMES-SANAMEL(R1)          FIRST NAME IN LIST               
         USING SANAMES,R5                                                       
*                                                                               
         LLC   R4,SANAMELN                     LENGTH OF FIRST NAME             
         BCTR  R4,0                            FOR EX                           
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SANAME                  LAST NAME TO SCREEN              
         LA    R5,2(R4,R5)                     BUMP TO NEXT NAME IN REC         
         LA    R2,1(R4,R2)                                                      
*                                                                               
         TM    SANAMIND-SANAMEL(R1),SANAMIMN   MIDDLE NAME PRESENT?             
         BZ    *+12                            NO                               
         IC    R4,SANAMELN                     LENGTH OF MIDDLE NAME            
         LA    R5,1(R4,R5)                     BUMP TO NEXT NAME IN REC         
*                                                                               
         IC    R4,SANAMELN                     LENGTH OF LAST NAME              
         BCTR  R4,0                            FOR EX                           
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R2),SANAME                  LAST NAME TO SCREEN              
         B     GNX                                                              
         DROP  R5                                                               
*                                                                               
GN99     MVC   0(13,R2),=C'** UNKNOWN **'                                       
         AHI   R2,14                                                            
*                                                                               
GNX      GOTOR VSMTP,DMCB,('SMTPAPTL',P)                                        
         MVC   P,SPACES                                                         
         MVC   P(9),=C'OF AGENCY'                                               
         MVC   P+10(2),QAGY                                                     
         MVC   P+13(43),=C'THAT THE CALL LETTER CHANGE DID NOT PROCESS'         
         GOTOR VSMTP,DMCB,('SMTPAPTL',P)                                        
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(L'QAREA),QAREA                                                 
         GOTOR VSMTP,DMCB,('SMTPAPTL',P)                                        
*                                                                               
         GOTOR VSMTP,DMCB,('SMTPASND',0)                                        
         GOTOR VSMTP,DMCB,('SMTPAEND',0) DETACH SMTP                            
         DROP  R3,R6                                                            
*                                                                               
         J     EXIT                                                             
*                                                                               
JESMAIL  DC    CL8'JESMAIL '                                                    
SMTPTO   DC    CL24'INT_SPOT@MEDIAOCEAN.COM:'                                   
SMTPCC   DC    0CL82                                                            
         DC    CL40'WHO@MEDIAOCEAN.COM,HWONG@MEDIAOCEAN.COM,'                   
         DC    CL21'AKATZ@MEDIAOCEAN.COM,'                                      
         DC    CL34'US-SPOTPRODUCTTEAM@MEDIAOCEAN.COM:'                         
SMTPBCC  DC    CL1' '                                                           
SUBJECT  DC    C'BAD STATION CALL LETTER CHANGE REQUEST JOBNAMEX'               
         LTORG                                                                  
         DROP  RB                                                               
         SPACE                                                                  
                                                                                
***********************************************************************         
* THE FOLLOWING ROUTINES ARE INTENDED TO SUPPORT SSC REVERSALS                  
* - SSC REVERSALS WILL PRIMARILY BE USED WHEN A CLIENT INCORRECTLY              
*   REQUESTS 2 SEPARATE SSC, MOVING BOTH STATIONS A AND B RECORDS               
*   TO STATION C. THIS MAKES IT IMPOSSIBLE FOR A NORMAL SSC TO                  
*   SELECTIVELY REVERSE A SINGLE SSC                                            
*                                                                               
* BRSTRTAB - BUILD BINSRCH TABLE OF 32 BYTE KEYS OF ALL RECORDS THAT            
*  WERE ADDED BY THE SSC THAT IS BEING REVERSED                                 
* CRSTRTAB - CHECK BINSRCH TABLE TO DETERMINE IF RECORD SHOULD BE               
*  REVERSED                                                                     
*                                                 -HWON 3/20/2017               
* REQUIREMENTS:  QOPT3 IS SET TO Y                                              
*                RECVIN IS PASSED IN                                            
*                                                                               
***********************************************************************         
         USING SPSC02+8192,R3                                                   
BRSTRTAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    RTBNPRMS(24),RTBNPRMS  SETUP BINSRCH PARAMETERS                  
*                                                                               
         CLI   QOPT3,C'Y'          SUPPORT RECVIN/RECOVERY?                     
         JNE   BRTXX                NO                                          
*                                                                               
         OPEN  (RECVIN,INPUT)      OPEN RECOVERY DCB                            
         LTR   RF,RF                                                            
         JNZ   *+2                 MUST PASS IN RECOVERY TAPE                   
*                                                                               
         GOTOR ,RTBNPRMS,,A(RSTRTBL),0,L'KEY,L'KEY,A(MAXRSTR)                   
*                                                                               
         LAY   R6,MYRECLEN                                                      
BRTGETRC GET   RECVIN,(R6)                                                      
         USING MYRECLEN,R6                                                      
*                                                                               
         XC    KEY,KEY                                                          
BRTSPOT  CLC   =X'2103',MYRECVH    CHECK SPTFILE (ADDS ONLY)                    
         BNE   BRTXSPT                                                          
         MVC   KEY(13),MYREC                                                    
         B     BRTPTBNS                                                         
*                                                                               
BRTXSPT  CLC   =X'3703',MYRECVH    CHECK XSPTFIL (ADDS ONLY)                    
         BNE   BRTTRFF                                                          
         MVC   KEY(32),MYREC                                                    
*                                                                               
         CLC   KEY(2),=X'0E03'     NEW INVOICE?                                 
         BNE   BRTPTBNS                                                         
*                                   YES, CLEAR MINIO ELEMENT KEY                
         XC    KEY+SNVKMINK-SNVKEY(L'SNVKMINK),KEY+SNVKMINK-SNVKEY              
         B     BRTPTBNS                                                         
*                                                                               
BRTTRFF  CLC   =X'3203',MYRECVH    CHECK TRAFFIC (ADDS ONLY)                    
         BNE   BRTGETRC                                                         
         MVC   KEY(13),MYREC                                                    
*                                  PUT KEY TO BINSRCH                           
BRTPTBNS GOTOR BINSRCH,RTBNPRMS,(X'01',KEY)  ADD TO BINSRCH                     
         OC    0(4,R1),0(R1)       TABLE FULL?                                  
         BNZ   BRTGETRC                                                         
         DC    H'0'                  YES, DIE                                   
*                                                                               
BRTX     DS    0H                                                               
*&&DO                                                                           
         XC    KEY,KEY                                                          
BRTX10   GOTOR BINSRCH,RTBNPRMS,(X'02',KEY)  FIND IN BINSRCH                    
         TM    0(R1),X'01'         REC NOT FOUND?                               
         BO    BRTXX                                                            
         L     RF,0(R1)                                                         
         MVC   KEY,0(RF)                                                        
         GOTOR HEXOUT,DMCB,KEY,P,L'KEY,=C'TOG',0                                
         GOTOR REPORT                                                           
         B     BRTX10                                                           
*&&                                                                             
BRTXX    J     EXIT                                                             
         LTORG                                                                  
         DROP  RB,R6,R3                                                         
***********************************************************************         
* CHECK BINSRCH TABLE TO SEE IF KEY IS PRESENT                                  
*                                                                               
*  ON ENTRY:  P1   BYTE0           (S)POT/(X)SPOT/(T)TRAFFIC                    
*                  BYTE1-3         A(KEY OR RECORD)                             
*                                                                               
*  ON EXIT :  CC   Z = KEY WAS FOUND -OR- NO BINSRCH/RECVIN                     
*                  NZ = KEY WAS NOT FOUND                                       
***********************************************************************         
         USING SPSC02+8192,R3                                                   
CRSTRTAB NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    RTBNPRMS(24),RTBNPRMS  BINSRCH PARAMETERS SETUP??                
         JZ    EXIT                                                             
*                                                                               
         L     R2,0(R1)                                                         
*                                                                               
         XC    KEY2,KEY2                                                        
CRTSPOT  CLI   0(R1),C'S'          SPTFILE?                                     
         BNE   CRTXSPT                                                          
         MVC   KEY2(13),0(R2)                                                   
*                                                                               
* THE BELOW CODE IS TO NOT MARK PASSIVE'S IF THE ACTIVE WAS NOT MOVED           
*                                                                               
         CLI   CANAGY,C'Y'         CANADIAN AGENCY?                             
         BE    CRTBNS               YES, SKIP BELOW LOGIC                       
         CLI   KEY2,X'10'          HAVE BUY REC?                                
         BL    CRTBNS               NO, SKIP BELOW LOGIC                        
         CLI   KEY2+10,X'FF'       BUY BRAND-POL PASSIVE?                       
         BNE   CRTBNS               NO, SKIP BELOW LOGIC                        
         MVI   KEY2+3,X'FF'         YES, CONVERT TO ACTIVE KEY AND              
         MVI   KEY2+10,0                                                        
         MVC   KEY2+11(1),KEY2+12                                               
         MVI   KEY2+12,X'01'                                                    
         B     CRTBNS              SEE IF ACTIVE IS IN BINSRCH TABLE            
*                                                                               
CRTXSPT  CLI   0(R1),C'X'          XSPTFILE?                                    
         BNE   CRTTRAF                                                          
         MVC   KEY2,0(R2)                                                       
*                                                                               
         CLC   KEY2(2),=X'0E03'    NEW INVOICE?                                 
         BNE   CRTBNS                                                           
         XC    KEY2+SNVKMINK-SNVKEY(L'SNVKMINK),KEY2+SNVKMINK-SNVKEY            
         B     CRTBNS                                                           
*                                                                               
CRTTRAF  CLI   0(R1),C'T'          TRAFFIC?                                     
         JNE   *+2                                                              
         MVC   KEY2(13),0(R2)                                                   
*                                                                               
CRTBNS   GOTOR BINSRCH,RTBNPRMS,(X'00',KEY2)    FIND EXACT MATCH                
         TM    0(R1),X'01'         RECORD FOUND?                                
         J     EXIT                 RETURN CC                                   
         LTORG                                                                  
         DROP  RB,R3                                                            
*                                                                               
         SPACE                                                                  
         DS    0D                                                               
SPBUFF   DS    8000C                                                            
         ORG   SPBUFF                                                           
MYRECLEN DS    F                                                                
MYRECVH  DS    XL24                                                             
MYREC    DS    6000C                                                            
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    CL8'RSTRTBL'                                                     
RSTRTBL  DS    (MAXRSTR)XL(L'KEY)                                               
MAXRSTR  EQU   20000               MAXIMUM KEYS                                 
*                                                                               
         DS    0D                                                               
         DC    CL8'BUYTABLE'                                                    
BUYTABLE DS    (MAXBUYS)XL(BUYTBLQ)                                             
MAXBUYS  EQU   10000               MAXIMUM BUY ENTRIES                          
*                                                                               
BUYTBLD  DSECT                                                                  
BTBUYKEY DS    XL13                ORIGINAL BUY KEY                             
BTNEWBLN DS    XL2                 NEW BUYLINE                                  
BUYTBLQ  EQU  *-BUYTBLD                                                         
*                                                                               
         PRINT OFF                                                              
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
STAHDRD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSNV                                                       
         EJECT                                                                  
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
       ++INCLUDE SPGENSLH                                                       
         EJECT                                                                  
*PREFIX=X                                                                       
       ++INCLUDE SPGENXLK                                                       
*PREFIX=                                                                        
         EJECT                                                                  
       ++INCLUDE SPGENNDEF                                                      
         EJECT                                                                  
* NEW BUYERS WORKSHEET RECORDS                                                  
       ++INCLUDE SPNWSCAM                                                       
         EJECT                                                                  
       ++INCLUDE SPNWSHDR                                                       
         EJECT                                                                  
       ++INCLUDE SPNWSDTL                                                       
         EJECT                                                                  
       ++INCLUDE SPNWSBRV                                                       
         EJECT                                                                  
* MINIO FOR NEW BUYERS WORKSHEET                                                
       ++INCLUDE DDMINBLK                                                       
         EJECT                                                                  
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
       ++INCLUDE SPGENDRORD                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPGENAUTH                                                      
       ++INCLUDE SPGENSTAFX                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DMREQHDRA                                                      
       ++INCLUDE SPGENORHIS                                                     
       ++INCLUDE SPGENCDORD        CANADIAN DTM RECORDS                         
       ++INCLUDE SPGENDREV         US SBTK RECORDS                              
       ++INCLUDE DDSMTPD                                                        
       ++INCLUDE SEACSFILE         SECURITY SYSTEM RECORD DSECTS                
       ++INCLUDE SPGENDBLBK                                                     
SSBD     DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
TIOTD    DSECT                                                                  
         IEFTIOT1                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'110SPREPSC02 02/18/21'                                      
         END                                                                    
