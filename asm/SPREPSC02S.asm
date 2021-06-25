*          DATA SET SPREPSC02S AT LEVEL 073 AS OF 05/13/04                      
*PHASE SPSC02A                                                                  
*INCLUDE SPLDCPTR                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         SPACE                                                                  
* QOPT1 = 'Y' MEANS FIX INVOICE RECORDS                                         
* QOPT4 = 'Y' MEANS THE WUNDERMAN TRACE IS ON                                   
* QOPT5 = 'Y' MEANS THE INTERNAL TRACE IS ON                                    
         TITLE 'STATION CALL LETTER CHANGE PROGRAM - APPLICATION'               
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
*                                                                     *         
***********************************************************************         
* R E C O R D S  C H A N G E D                                        *         
* SPOT BUYS                                                           *         
* OLD INVOICE RECORDS     SPOT 0B                                     *         
* NEW INVOICE RECORDS     XSP  0E03                                   *         
* STATION BUCKET RECORDS  SPOT 0E01 (YOU MIGHT HAVE THOUGHT, BUT NO ..*         
* NSID RECORDS            SPOT 0C                                     *         
* DARE ORDER RECORDS      SPOT 0D34                                   *         
* DARE BATCH RECORDS      SPOT 0D35                                   *         
* DESTINATION RECORDS     SPOT 0D3D                                   *         
* LAST METHOD RECORDS     SPOT 0D3E                                   *         
* SID RECORDS             SPOT 0D59                                   *         
* WILA STATION LOCKIN REC SPOT 0D72                                   *         
* WILA STATION LOCKIN REC XSP  0D73                                   *         
* WILA PROFIT WITHIN      SPOT 0D7A                                   *         
* DOUBLE BOOKING RECORDS  SPOT 0D7B                                   *         
* TRAFFIC INST RECAP      STR  0A24                                   *         
* TRAFFIC SHIP RECAP      STR  0A25                                   *         
* TRAFFIC BUY ACT         STR  0A2E                                   *         
* TRAFFIC BUY             STR  0A32                                   *         
* TRAFFIC LABEL LIST      STR  0A2F                                   *         
* TRAFFIC STATION LIST    STR  0A31                                   *         
* TRAFFIC PATTERN REC     STR  0A22                                   *         
* TRAFFIC STATION REC     STR  0A28                                   *         
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
         OI    3(RE),X'08'         NEED TO RECOVER COPIES AND CHANGES           
         LA    R0,SPSCHDHK                                                      
         ST    R0,HEADHOOK                                                      
         STM   R9,RB,SPSCR9                                                     
         ST    R7,SPSCR7                                                        
         ST    R3,SPSCR3                                                        
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         SPACE                                                                  
         DS    0D                                                               
PATCH1   DS    CL16                                                             
PATCH2   DS    CL16                                                             
         EJECT                                                                  
* THIS SECTION GETS THE NEW CALL LETTERS *                                      
*  FROM QBOOK1 AND SAVES IT IN NEWCALL   *                                      
         SPACE                                                                  
SP000    DS    0H                                                               
*                                                                               
* CHECK WHETHER TO DISALLOW CALL LETTER CHANGE                                  
         BRAS  RE,CHKCAN                                                        
*                                                                               
         CLI   QMED,C'T'                                                        
         BNE   SP010                                                            
         CLI   QBOOK1+4,C'L'       LOW POWER STATION?                           
         BE    SP010                                                            
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
         MVI   RCSUBPRG,1                                                       
         XC    SAVDA,SAVDA         CLEAR SAVED DISK ADDR                        
         XC    COUNT,COUNT                                                      
         CLI   QMED,C'N'                                                        
         BNE   *+8                                                              
         MVI   QSTA+4,C'N'                                                      
         SPACE                                                                  
         CLI   QMED,C'X'                                                        
         BNE   *+8                                                              
         MVI   QSTA+4,C'X'                                                      
         SPACE                                                                  
         CLI   QMED,C'T'                                                        
         BNE   SP020                                                            
         CLI   QSTA+4,C'L'         LOW POWER STATION?                           
         BE    SP020                                                            
         MVI   QSTA+4,C'T'                                                      
         SPACE                                                                  
SP020    DS    0H                                                               
         ZIC   R0,QMED                                                          
         CLI   QMED,C'C'                                                        
         BNE   *+12                                                             
         MVI   QMED,C'T'                                                        
         MVI   QSTA+4,C'T'                                                      
         SPACE                                                                  
         GOTO1 MSPACK,DMCB,QMKT,QSTA,OLDMSTA                                    
         SPACE                                                                  
         STC   R0,QMED                                                          
         CLI   QMED,C'C'                                                        
         BNE   *+8                                                              
         STC   R0,QSTA+4                                                        
         SPACE                                                                  
         OC    OLDMSTA+2(2),OLDMSTA+2                                           
         BNZ   SP024                                                            
         TM    OLDMSTA+4,X'E0'                                                  
         BNZ   SP024                                                            
         DC    H'0'                                                             
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
         MVI   NEWCALL+4,C'T'                                                   
         SPACE                                                                  
SP025    CLI   QMED,C'C'           COMBINED                                     
         BNE   *+8                                                              
         MVI   NEWCALL+4,C'T'                                                   
         SPACE                                                                  
         GOTO1 (RF),(R1),QMKT,NEWCALL,NEWMSTA                                   
         OC    NEWMSTA+2(2),NEWMSTA+2                                           
         BNZ   SP026                                                            
         TM    NEWMSTA+4,X'E0'                                                  
         BNZ   SP026                                                            
         DC    H'0'                                                             
         SPACE                                                                  
SP026    CLC   OLDMSTA,NEWMSTA                                                  
         BE    STAEQERR                                                         
         XC    SAVCLT,SAVCLT                                                    
         CLC   QCLT,=C'ALL'        IS THIS CLIENT SPECIFIC REQ?                 
         BE    SP030                NO.                                         
         GOTO1 CLPACK,DMCB,QCLT,SAVCLT                                          
SP030    DS    0H                                                               
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
         BE    *+6                                                              
         DC    H'0'                NO "01" ELEMENT FOUND                        
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
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NEWUID,STUNIQID                                                  
*                                                                               
         XC    SKEY,SKEY                                                        
         LA    R2,SVSTKEY                                                       
         MVC   STAKCALL,QSTA                                                    
         MVC   SKEY(L'SVSTKEY),SVSTKEY                                          
         L     R2,ADSTAT                                                        
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',SKEY,(R2),0                  
         CLC   SVSTKEY,0(R2)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
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
         SPACE                                                                  
* SP220 - THIS SECTION REPLACES THE CALL LETTERS *                              
*  IN COMBINED TV BUYS WITH THE NEW CALL LETTERS *                              
         SPACE                                                                  
SP220    DS    0H                                                               
         MVC   DUB(1),KEY                                                       
         NI    DUB,X'0F'                                                        
         CLI   DUB,8               IS THIS A COMBINED BUY?                      
         BNE   SP210                NO.                                         
         CLI   KEY+11,0                                                         
         BNE   SP230               ACTIVE POINTER                               
         MVC   SAVEKEY2(17),KEY                                                 
         MVI   TRACECDE,C'2'                                                    
         BAS   RE,SPDEL            DELETE THE PASSIVE POINTERS                  
         SPACE                                                                  
         MVC   DUB(1),KEY          RESTORE ORIGINAL MEDIA IN KEY                
         NI    DUB,X'F0'                                                        
         OC    DUB(1),KEY+10                                                    
         MVC   KEY(1),DUB                                                       
         MVI   KEY+10,X'FF'                                                     
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRACECDE,C'3'                                                    
         BAS   RE,SPDEL            DELETE THE PASSIVE POINTERS                  
         SPACE                                                                  
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
         CLI   USRSW1,C'Y'         ARE THERE MORE THAN 255 BUYS?                
         BE    EXIT                 YES.                                        
SP235    DS    0H                                                               
         MVC   SAVEKEY(13),KEY                                                  
         CLI   QOPT5,C'Y'                                                       
         BNE   SP240                                                            
         MVI   TRACECDE,C'4'                                                    
         BAS   RE,SPTRACE          *** TRACE ***                                
SP240    DS    0H                                                               
         GOTO1 GETBUY                                                           
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
         SPACE                                                                  
         TM    BUYRCNTL,X'80'      THIS A DELETED BUY                           
         BO    EXIT                 YES, BYPASS                                 
         SPACE                                                                  
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   SP250                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,ADBUY                         
         TM    DM3,X'FD'                                                        
         BZ    SP250                                                            
         DC    H'0'                DATAMGR ERROR                                
SP250    DS    0H                                                               
         SPACE                                                                  
         CLI   CANAGY,C'Y'                                                      
         BNE   SP260                                                            
         GOTO1 =A(SPCNETWK)        FIX THE CANADIAN NETWORK ELEMENT             
SP260    DS    0H                                                               
         CLI   M,0                                                              
         BE    SP280               NO NEED TO CORRECT BUY LINE NUMBERS          
         ZIC   R5,10(R8)           GET THE BUY LINE NUMBER                      
         ZIC   R6,M                                                             
         AR    R5,R6                                                            
         STC   R5,10(R8)           REPLACE WITH NEW BUY LINE NUMBER             
         BAS   RE,SPPKG            FIX THE PACKAGE ELEMENT, IF ANY              
         EJECT                                                                  
* THIS SECTION IS COMMON TO BOTH AMERICAN AND CANADIAN STATION FIX *            
         SPACE                                                                  
SP280    DS    0H                                                               
         SPACE                                                                  
* ADD MARKET FIX ELEMENT FOR AUDIT TRAIL *                                      
         SPACE                                                                  
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING SFXELEM,R6                                                       
         MVI   SFXCODE,SFXCODEQ                                                 
         MVI   SFXLEN,SFXLENQ                                                   
         MVC   SFXSTA,6(R8)                                                     
         MVC   SFXDATE,TODAYP                                                   
         DROP  R6                                                               
         SPACE                                                                  
         SR    RE,RE                                                            
         ICM   RE,3,13(R8)                                                      
         SPACE                                                                  
         LR    RF,RE               SEE IF ROOM IN REC FOR ELEM                  
         AH    RF,=AL2(SFXLENQ)                                                 
         CH    RF,=H'3975'         MAX REC LENGTH                               
         BH    SP300                                                            
         LA    R5,0(R8,RE)                                                      
         SPACE                                                                  
         GOTO1 RECUP,DMCB,(R8),(R6),(R5)                                        
         SPACE                                                                  
SP300    DS    0H                                                               
         MVC   NETNIBLE,8(R8)     SAVE MEDIA OR CANADIAN NETWORK                
         MVC   6(3,R8),NEWSTA      SET THE NEW STATION CODE                     
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE STATION                         
         BL    SP301                NO                                          
         SPACE                                                                  
         NI    8(R8),X'80'         SET OFF ANY UNWANTED                         
         NI    NETNIBLE,X'7F'     DROP CABLE HEAD                               
         OC    8(1,R8),NETNIBLE   RESTORE CABLE HEAD NETWORK                    
         B     SP302                                                            
         SPACE                                                                  
SP301    CLI   CANAGY,C'Y'         ARE WE DEALING WITH A CANADIAN AGY?          
         BNE   SP302                NO                                          
         MVC   DUB(1),0(R8)                                                     
         NI    DUB,X'0F'                                                        
         CLI   DUB,3              NETWORK                                       
         BNE   SP302                                                            
         SPACE                                                                  
* CANADIAN NETWORK IS NOW 8, NOT 5 BITS                                         
         SPACE                                                                  
         NI    8(R8),X'00'         SET OFF ANY BUMMER BITS                      
         SPACE                                                                  
*        NI    NETNIBLE,X'1F'     DROP STATION BITS                             
         SPACE                                                                  
         OC    8(1,R8),NETNIBLE   RESTORE CANADIAN NETWORK                      
         SPACE                                                                  
SP302    NI    15(R8),X'7F'        'UNSET' THE 'DELETED' FLAG                   
         CLI   QOPT5,C'Y'                                                       
         BNE   SP305                                                            
         MVC   KEY+6(3),6(R8)      MOVE IN MODIFIED NEWSTA                      
         MVI   TRACECDE,C'5'                                                    
         BAS   RE,SPTRACE          *** TRACE ***                                
SP305    DS    0H                                                               
         SPACE                                                                  
         LH    R1,COUNT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,COUNT            # RECORDS CHANGED                            
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   SP310                NO                                          
         GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,ADBUY,DMWORK                  
         CLI   DM3,0                                                            
         BE    SP310                                                            
         DC    H'0'                DATAMGR ERROR                                
SP310    DS    0H                                                               
         GOTO1 =V(LDCPTR),DMCB,ADBUY,A(SPBUFF)                                  
         L     R5,=A(SPBUFF)                                                    
         BAS   RE,SPPRINT                                                       
         MVI   USRSW2,C'Y'                                                      
         LA    R5,18(R5)                                                        
         CLI   0(R5),0                                                          
         BE    SP380               DONE                                         
         CLI   BUYKEY+3,X'FF'                                                   
         BE    SP320                                                            
         CLI   BDTIME,0            TEST FOR PIGGYBACK                           
         BE    SP380                NO.                                         
         LA    R5,18(R5)           A PASSIVE POINTER WAS ALREADY ADDED          
         B     SP330                                                            
         SPACE                                                                  
SP320    DS    0H                                                               
         CLI   BDMASPRD,0          IS THERE A POL MASTER PRODUCT CODE           
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
SP330    DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    SP380                                                            
SP340    DS    0H                                                               
         MVC   KEY(13),0(R5)                                                    
         CLI   QOPT5,C'Y'                                                       
         BNE   SP350                                                            
         MVI   TRACECDE,C'6'                                                    
         BAS   RE,SPTRACE          *** TRACE ***                                
SP350    DS    0H                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   SP370                                                            
         CLC   =X'FF00',KEY+10                                                  
         BNE   SP360                                                            
         CLC   KEY+3(1),BDMASPRD                                                
         BE    SP370                                                            
         CLC   KEY+3(1),BDMASPRD+1                                              
         BE    SP370                                                            
SP360    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMADD,SPTDIR,KEY,KEY                                
         SPACE                                                                  
         TM    DM3,X'DD'          ALLOW DUP KEY ON ADD OR DELETED REC           
         BZ    SP370                                                            
         DC    H'0'                ERROR                                        
SP370    DS    0H                                                               
         LA    R5,18(R5)                                                        
         CLI   0(R5),0             IS THIS THE END OF THE SPBUFF?               
         BNE   SP340                NO.                                         
         SPACE                                                                  
SP380    DS    0H                                                               
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),SPTDIR,KEY,KEY                       
         CLC   KEY(13),SAVEKEY                                                  
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
* SP400 - THIS SECTION UPDATES THE INVOICE RECORDS *                            
         SPACE                                                                  
SP400    CLI   BUYSW,C'Y'          WERE ANY BUYS FOUND?                         
         BE    SP406                YES.                                        
         CLI   BUYSW,0             IS THIS REQLAST PROC FROM PROG CK            
         BE    EXIT                 YES.                                        
         MVC   P+10(19),=C'NO BUYS WERE FOUND!'                                 
         GOTO1 REPORT                                                           
         B     SP410                                                            
         SPACE                                                                  
SP406    ZIC   R0,KEY                                                           
         MVI   KEYSAVE,X'FF'       FORCE TO BUY                                 
         BAS   RE,REPRT            PRINT TOTALS                                 
         STC   R0,KEY              RESTORE KEY                                  
         SPACE                                                                  
SP410    MVI   BUYSW,0             SET BUYSW FOR PROG CK                        
         CLI   QOPT1,C'Y'          SHOULD I FIX INVOICES?                       
         BNE   SP500                NO. CHECK NSID REC                          
         XC    COUNT,COUNT                                                      
         MVI   FORCEHED,C'Y'        YES.                                        
         MVI   RCSUBPRG,2                                                       
         MVC   AREC,ADBUY                                                       
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
         BL    SN400                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,7,KEY+2                                                       
         ICM   RF,7,KEYSAVE+2                                                   
         SRL   RE,7                DROP NET FROM CABLE HEAD                     
         SRL   RF,7                                                             
         CR    RE,RF               IS THIS THE SAME CABLE HEAD                  
         BNE   SN400                NO, TRY NINV RECORDS                        
         SPACE                                                                  
SP424    OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    SP426                NO.                                         
         CLC   KEY+5(2),SAVCLT                                                  
         BNE   SN400                CHECK NINV RECORDS                          
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
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC                          
         TM    DM3,X'FD'                                                        
         BZ    SP440                                                            
         DC    H'0'                                                             
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
         BE    *+6                                                              
         DC    H'0'                                                             
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
         BE    SP460                                                            
         DC    H'0'                                                             
SP460    OC    SAVDA,SAVDA         IS THIS 1ST ADD?                             
         BZ    SP470                NO.                                         
         MVC   SAVDA,KEY+14        SAVE DISK ADDR OF 1ST ADD                    
         SPACE                                                                  
* PRINT THE NEW INVOICE RECORD *                                                
         SPACE                                                                  
SP470    DS    0H                                                               
         LH    R1,COUNT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,COUNT            # RECORDS CHANGED                            
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,KEY+5,P+10                                           
         GOTO1 DATCON,(R1),(2,KEY+7),(5,P+20)                                   
         GOTO1 HEXOUT,(R1),KEY+9,P+38,1,=C'MIX'                                 
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
         GOTO1 HEXOUT,(R1),KEY+9,P+38,1,=C'MIX'                                 
         MVC   P+45(40),=CL40'* ERROR * INVOICE ALREADY ON NEW STATION'         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SP480                                                            
         SPACE 3                                                                
* NOW DO NEW INVOICING RECORDS                                                  
         SPACE                                                                  
SN400    BAS   RE,REPRT            PRINT TOTALS                                 
         GOTO1 =A(SNINV)           GO MOVE NEW INVOICES                         
*                                                                               
* AUTHORIZATION RECORDS                                                         
*                                                                               
SP500    GOTO1 =A(SNAUTH)          GO MOVE NEW INVOICES                         
         EJECT                                                                  
* THIS SECTION UPDATES DARE ORDER RECORDS *                                     
         DS    0H                                                               
         GOTO1 =A(DARORD)          GO MOVE DARE ORDER RECORDS                   
         SPACE                                                                  
* THIS SECTION UPDATES NWS (NEW BUYERS WORKSHEET RECORRDS *                     
         SPACE                                                                  
*        GOTO1 =A(NWS)             GO MOVE NWS RECORDS                          
         SPACE                                                                  
* THIS SECTION UPDATES NSID RECORDS *                                           
         SPACE                                                                  
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
         BNE   SP600               NO -- FINISHED                               
*                                                                               
         MVC   SCHEME,KEY+2        SAVE SCHEME CODE                             
         MVC   KEY+4(5),OLDMSTA    OLD MARKET/STATION                           
         GOTO1 HIGH                                                             
         B     SP530                                                            
*                                                                               
SP520    GOTO1 SEQ                                                              
*                                                                               
SP530    CLC   KEY(2),KEYSAVE      TEST 0C/A-M                                  
         BNE   SP600                NO. CHECK OLD SID RECORDS                   
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
SP540    CLI   RCWRITE,C'Y'                                                     
         BNE   SP550                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC                          
         TM    DM3,X'FD'                                                        
         BZ    SP550                                                            
         DC    H'0'                                                             
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
         BE    *+6                                                              
         DC    H'0'                                                             
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
         BE    SP570                                                            
         DC    H'0'                                                             
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
SP600    DS    0H                                                               
         BAS   RE,REPRT                                                         
         EJECT                                                                  
* SP600 - THIS SECTION UPDATES SID RECORDS *                                    
         SPACE                                                                  
         DS    0H                                                               
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
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP6A00                                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,7,KEY+7                                                       
         ICM   RF,7,KEYSAVE+7                                                   
         SRL   RE,7                DROP NET FROM CABLE HEAD                     
         SRL   RF,7                                                             
         CR    RE,RF               IS THIS THE SAME CABLE HEAD                  
         BNE   SP6A00               NO. CHECK STATION REC                       
*                                                                               
SP624    MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   SP630                                                            
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP630    MVC   KEY+3(5),NEWMSTA    DOES NEW RECORD ALREADY EXIST?               
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP634                                                            
         MVC   BYTE,SAVEKEY+7      SAVE CABLE HEAD NETWORK                      
         NI    BYTE,X'7F'                                                       
         OC    KEY+7(1),BYTE       SET IN NETWORK                               
         SPACE                                                                  
SP634    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     SAME 0B/A-M/MSTA/PER/SEQ/DPT...              
         BE    DUPERR              YES, DUPLICATE KEYS (TSK,TSK)                
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE DATAMGR SEQ                          
*                                                                               
         GOTO1 GET                                                              
*                                                                               
SP640    CLI   RCWRITE,C'Y'                                                     
         BNE   SP650                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC                          
         TM    DM3,X'FD'                                                        
         BZ    SP650                                                            
         DC    H'0'                                                             
*                                                                               
SP650    MVC   DUB(1),7(R8)        SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+7                                                   
         MVC   5(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+5(3),NEWSTA                                                  
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP656                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    7(1,R8),DUB   SET IN NETWORK                                     
         OC    KEY+7(1),DUB+1     SET IN NETWORK                                
         SPACE                                                                  
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
         BE    SP670                                                            
         DC    H'0'                                                             
         SPACE                                                                  
SP670    DS    0H                                                               
         LH    R0,COUNT                                                         
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
* SP6A00 - THIS SECTION UPDATES CALL LETTERS FOR DESTINATION RECORDS            
SP6A00   DS    0H                                                               
         BAS   RE,REPRT                                                         
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
         BNE   SP6B00                                                           
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
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP6A34                                                           
         MVC   BYTE,SAVEKEY+5                                                   
         NI    BYTE,X'7F'                                                       
         OC    KEY+5(1),BYTE       SET IN NETWORK                               
*                                                                               
*PF34    MVC   KEYSAVE,KEY                                                      
*PF34    GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),SPTDIR,KEY,AREC                      
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
         SPACE                                                                  
*                                                                               
SP6A35   CLI   RCWRITE,C'Y'                                                     
         BNE   SP6A40                                                           
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC                          
         TM    DM3,X'FD'                                                        
         BZ    SP6A40                                                           
         DC    H'0'                                                             
*                                                                               
SP6A40   MVC   DUB(1),5(R8)        SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+5                                                   
         MVC   3(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+3(3),NEWSTA                                                  
         NI    15(R8),X'7F'        UNDELETE THE RECORD                          
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP6A44                                                           
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         BE    *+6                                                              
         DC    H'0'                                                             
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
         BE    SP6A60                                                           
         DC    H'0'                                                             
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
         GOTO1 CLUNPK,DMCB,KEY+6,P+10                                           
*        GOTO1 DATCON,(R1),(2,KEY+7),(5,P+20)                                   
*        GOTO1 HEXOUT,(R1),KEY+9,P+38,1,=C'MIX'                                 
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
SP6A80   MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),SPTDIR,KEY,KEY                       
         CLC   KEY(13),SAVEKEY                                                  
         BE    SP6A20                                                           
         DC    H'0'                                                             
*                                                                               
* PRINT ERROR FOR DESTINE ALREADY ON NEW STATION (CLIENT) *                     
*                                                                               
SP6A90   GOTO1 MSUNPK,DMCB,KEY+3,P+10,P+20                                      
         GOTO1 CLUNPK,DMCB,KEY+6,P+38                                           
*        GOTO1 DATCON,(R1),(2,KEY+7),(5,P+20)                                   
*        GOTO1 HEXOUT,(R1),KEY+9,P+38,1,=C'MIX'                                 
         MVC   P+45(40),=CL39'* ERROR * DESTINE RECORD ALREADY EXISTS'          
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SP6A80                                                           
*                                                                               
* SP6B00 - THIS SECTION UPDATES CALL LETTERS FOR LAST METHOD RECORDS            
SP6B00   DS    0H                                                               
         BAS   RE,REPRT                                                         
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
         BNE   SP6C00                                                           
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
*                                                                               
SP6B35   CLI   RCWRITE,C'Y'                                                     
         BNE   SP6B40                                                           
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC                          
         TM    DM3,X'FD'                                                        
         BZ    SP6B40                                                           
         DC    H'0'                                                             
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
         BE    *+6                                                              
         DC    H'0'                                                             
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
         BE    SP6B60                                                           
         DC    H'0'                                                             
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
SP6B90   GOTO1 MSUNPK,DMCB,KEY+8,P+10,P+20                                      
         GOTO1 CLUNPK,DMCB,KEY+11,P+38                                          
*        GOTO1 DATCON,(R1),(2,KEY+7),(5,P+20)                                   
*        GOTO1 HEXOUT,(R1),KEY+9,P+38,1,=C'MIX'                                 
         MVC   P+45(40),=CL40'* ERROR * LASTMTHD RECORD ALREADY EXISTS'         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SP6B80                                                           
*                                                                               
SP6C00   DS    0H                                                               
         BAS   RE,REPRT                                                         
         BRAS  RE,SP6C0000         MOVED TO END TO FREE ADDRESSIBILITY          
*                                                                               
SP690    DS    0H                                                               
         BAS   RE,REPRT                                                         
         EJECT                                                                  
* PW100 - THIS SECTION UPDATES PROFIT WITHIN RECORDS *                          
         SPACE                                                                  
         GOTO1 =A(PW100)        GO UPDATE PROFIT WITHIN RECORDS                 
         BAS   RE,REPRT                                                         
         SPACE 3                                                                
* PW200 - THIS SECTION UPDATES STATION LOCKIN RECORDS *                         
         SPACE                                                                  
         GOTO1 =A(PW200)        GO UPDATE STATION LOCKIN RECORDS                
         BAS   RE,REPRT                                                         
         SPACE 3                                                                
* PW200 - THIS SECTION UPDATES STATION LOCKIN XSP RECORDS *                     
         SPACE                                                                  
         GOTO1 =A(PW300)        GO UPDATE STATION LOCKIN RECORDS                
         BAS   RE,REPRT                                                         
         SPACE 3                                                                
* DB100 - THIS SECTION UPDATES DOUBLE BOOKING RECORDS *                         
         SPACE                                                                  
         GOTO1 =A(DB100)        GO UPDATE DOUBLE BOOKING RECORDS                
         BAS   RE,REPRT                                                         
         EJECT                                                                  
* SP700 - THIS SECTION UPDATES TRAFFIC INSTRUCTION RECAP RECORDS *              
         SPACE                                                                  
*                                                                               
         L     RE,UTL                                                           
         CLI   FCUPTRF,C'Y'        TEST TRAFFIC SYSTEM OP                       
         BNE   *+10                                                             
         MVC   4(1,RE),RCUTLTRF    SET TRAFFIC SYSTEM NUMBER                    
*                                                                               
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A24'     INST RECAP RECORD                            
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         SPACE                                                                  
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+10                NO, THEN ALL CLIENTS                         
         MVC   KEY+3(2),SAVCLT                                                  
         GOTO1 HIGH                                                             
         B     SP720                                                            
*                                                                               
SP710    GOTO1 SEQ                                                              
*                                                                               
SP720    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SP800               NO,THEN TRY NEXT                             
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+14                NO, THEN ALL CLIENTS                         
         CLC   KEY+3(2),SAVCLT                                                  
         BNE   SP800                                                            
         CLC   KEY+6(5),OLDMSTA    OLD MARKET/STATION                           
         BE    SP726                                                            
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP710                                                            
         MVC   DUB(5),KEY+3                                                     
         NI    DUB+4,X'80'                                                      
         CLC   DUB(5),OLDMSTA                                                   
         BNE   SP710                                                            
*                                                                               
SP726    MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   SP730                                                            
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP730    MVC   KEY+6(5),NEWMSTA    DOES NEW RECORD ALREADY EXIST?               
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP734                                                            
         MVC   BYTE,SAVEKEY+10     SAVE CABLE HEAD NETWORK                      
         NI    BYTE,X'7F'                                                       
         OC    KEY+10(1),BYTE      SET IN NETWORK                               
         SPACE                                                                  
SP734    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DUP10               YES, DUPLICATE KEYS (TSK,TSK)                
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE DATAMGR SEQ                          
*                                                                               
         GOTO1 GET                                                              
*                                                                               
SP740    CLI   RCWRITE,C'Y'                                                     
         BNE   SP750                                                            
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMDEL',=C'TRFFILE',KEY,AREC                      
         TM    DM3,X'FD'                                                        
         BZ    SP750                                                            
         DC    H'0'                                                             
*                                                                               
SP750    MVC   DUB(1),10(R8)       SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+10                                                  
         MVC   8(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+8(3),NEWSTA                                                  
         NI    15(R8),X'7F'        UNDELETE THE RECORD                          
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP756                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    10(1,R8),DUB        SET IN NETWORK                               
         OC    KEY+10(1),DUB+1                                                  
         SPACE                                                                  
SP756    CLI   QOPT5,C'Y'                                                       
         BNE   SP760                                                            
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP760    CLI   RCWRITE,C'Y'                                                     
         BNE   SP770                                                            
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,ADDREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         BE    SP770                                                            
         DC    H'0'                                                             
*                                                                               
SP770    DS    0H                                                               
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
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
* PRINT ERROR FOR TRAFFIC INSTR RECAP ALREADY ON NEW STATION *                  
*                                                                               
DUP10    GOTO1 CLUNPK,DMCB,KEY+3,P+10                                           
         GOTO1 MSUNPK,(R1),KEY+6,P+20,P+30                                      
         MVC   P+45(40),NEWEXIST                                                
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SP780                                                            
*                                                                               
SP800    DS    0H                                                               
         BAS   RE,REPRT                                                         
         EJECT                                                                  
* SP800 - THIS SECTION UPDATES TRAFFIC SHIPPING RECAP RECORDS *                 
         SPACE                                                                  
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A25'     SHIP RECAP RECORD                            
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         SPACE                                                                  
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+10                NO, THEN ALL CLIENTS                         
         MVC   KEY+3(2),SAVCLT                                                  
         GOTO1 HIGH                                                             
         B     SP820                                                            
*                                                                               
SP810    GOTO1 SEQ                                                              
*                                                                               
SP820    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SP900               NO,THEN TRY NEXT                             
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+14                NO, THEN ALL CLIENTS                         
         CLC   KEY+3(2),SAVCLT                                                  
         BNE   SP900                                                            
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
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP834                                                            
         MVC   BYTE,SAVEKEY+9      SAVE CABLE HEAD NETWORK                      
         NI    BYTE,X'7F'                                                       
         OC    KEY+9(1),BYTE       SET IN NETWORK                               
         SPACE                                                                  
SP834    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DUP20               YES, DUPLICATE KEYS (TSK,TSK)                
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE DATAMGR SEQ                          
*                                                                               
         GOTO1 GET                                                              
*                                                                               
SP840    CLI   RCWRITE,C'Y'                                                     
         BNE   SP850                                                            
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMDEL',=C'TRFFILE',KEY,AREC                      
         TM    DM3,X'FD'                                                        
         BZ    SP850                                                            
         DC    H'0'                                                             
*                                                                               
SP850    MVC   DUB(1),9(R8)        SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+9                                                   
         MVC   7(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+7(3),NEWSTA                                                  
         NI    15(R8),X'7F'        UNDELETE THE RECORD                          
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP856                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    9(1,R8),DUB         SET IN NETWORK                               
         OC    KEY+9(1),DUB+1                                                   
         SPACE                                                                  
SP856    CLI   QOPT5,C'Y'                                                       
         BNE   SP860                                                            
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP860    CLI   RCWRITE,C'Y'                                                     
         BNE   SP870                                                            
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,ADDREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         BE    SP870                                                            
         DC    H'0'                                                             
*                                                                               
SP870    DS    0H                                                               
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
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
* PRINT ERROR FOR TRAFFIC SHIPPING RECAP ALREADY ON NEW STATION *               
*                                                                               
DUP20    GOTO1 CLUNPK,DMCB,KEY+3,P+10                                           
         GOTO1 MSUNPK,(R1),KEY+5,P+20,P+30                                      
         MVC   P+45(40),NEWEXIST                                                
         MVI   P+70,C'5'                                                        
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SP880                                                            
*                                                                               
SP900    DS    0H                                                               
         BAS  RE,REPRT                                                          
         EJECT                                                                  
* SP900 - THIS SECTION UPDATES TRAFFIC BUY ACTIVITY RECORDS *                   
         SPACE                                                                  
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2E'     BUY ACT/ESTM RECORD                          
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         SPACE                                                                  
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+10                NO, THEN ALL CLIENTS                         
         MVC   KEY+3(2),SAVCLT                                                  
         GOTO1 HIGH                                                             
         B     SP920                                                            
*                                                                               
SP910    GOTO1 SEQ                                                              
*                                                                               
SP920    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SPA00               NO,THEN TRY NEXT                             
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+14                NO, THEN ALL CLIENTS                         
         CLC   KEY+3(2),SAVCLT                                                  
         BNE   SPA00                                                            
         CLC   KEY+5(5),OLDMSTA    OLD MARKET/STATION                           
         BE    SP926                                                            
         SPACE                                                                  
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
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP934                                                            
         MVC   BYTE,SAVEKEY+9      SAVE CABLE HEAD NETWORK                      
         NI    BYTE,X'7F'                                                       
         OC    KEY+9(1),BYTE       SET IN NETWORK                               
         SPACE                                                                  
SP934    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DUP30               YES, DUPLICATE KEYS (TSK,TSK)                
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE DATAMGR SEQ                          
*                                                                               
         GOTO1 GET                                                              
*                                                                               
SP940    CLI   RCWRITE,C'Y'                                                     
         BNE   SP950                                                            
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMDEL',=C'TRFFILE',KEY,AREC                      
         TM    DM3,X'FD'                                                        
         BZ    SP950                                                            
         DC    H'0'                                                             
*                                                                               
SP950    MVC   DUB(1),9(R8)        SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+9                                                   
         MVC   7(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+7(3),NEWSTA                                                  
         NI    15(R8),X'7F'        UNDELETE THE RECORD                          
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP956                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    9(1,R8),DUB         SET IN NETWORK                               
         OC    KEY+9(1),DUB+1                                                   
         SPACE                                                                  
SP956    CLI   QOPT5,C'Y'                                                       
         BNE   SP960                                                            
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP960    CLI   RCWRITE,C'Y'                                                     
         BNE   SP970                                                            
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,ADDREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         BE    SP970                                                            
         DC    H'0'                                                             
*                                                                               
SP970    DS    0H                                                               
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
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
* PRINT ERROR FOR TRAFFIC BUY ACTIVITY ALREADY ON NEW STATION *                 
*                                                                               
DUP30    GOTO1 CLUNPK,DMCB,KEY+3,P+10                                           
         GOTO1 MSUNPK,(R1),KEY+5,P+20,P+30                                      
         MVC   P+45(40),NEWEXIST                                                
         MVI   P+70,C'E'                                                        
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SP980                                                            
*                                                                               
SPA00    DS    0H                                                               
         BAS  RE,REPRT                                                          
         EJECT                                                                  
* SPA00 - THIS SECTION UPDATES TRAFFIC BUY RECORDS *                            
         SPACE                                                                  
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A32'     TRAFFIC BUY RECORD                           
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         SPACE                                                                  
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+10                NO, THEN ALL CLIENTS                         
         MVC   KEY+3(2),SAVCLT                                                  
         GOTO1 HIGH                                                             
         B     SPA20                                                            
*                                                                               
SPA10    GOTO1 SEQ                                                              
*                                                                               
SPA20    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SPB00               NO,THEN TRY NEXT                             
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+14                NO, THEN ALL CLIENTS                         
         CLC   KEY+3(2),SAVCLT                                                  
         BNE   SPB00                                                            
         SPACE                                                                  
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
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SPA30    MVC   KEY+6(5),NEWMSTA    DOES NEW RECORD ALREADY EXIST?               
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SPA34                                                            
         MVC   BYTE,SAVEKEY+10     SAVE CABLE HEAD NETWORK                      
         NI    BYTE,X'7F'                                                       
         OC    KEY+10(1),BYTE      SET IN NETWORK                               
         SPACE                                                                  
SPA34    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DUP40               YES, DUPLICATE KEYS (TSK,TSK)                
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE DATAMGR SEQ                          
*                                                                               
         GOTO1 GET                                                              
*                                                                               
SPA40    CLI   RCWRITE,C'Y'                                                     
         BNE   SPA50                                                            
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMDEL',=C'TRFFILE',KEY,AREC                      
         TM    DM3,X'FD'                                                        
         BZ    SPA50                                                            
         DC    H'0'                                                             
*                                                                               
SPA50    MVC   DUB(1),10(R8)       SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+10                                                  
         MVC   8(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+8(3),NEWSTA                                                  
         NI    15(R8),X'7F'        UNDELETE THE RECORD                          
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SPA56                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    10(1,R8),DUB        SET IN NETWORK                               
         OC    KEY+10(1),DUB+1                                                  
         SPACE                                                                  
SPA56    CLI   QOPT5,C'Y'                                                       
         BNE   SPA60                                                            
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SPA60    CLI   RCWRITE,C'Y'                                                     
         BNE   SPA70                                                            
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,ADDREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         BE    SPA70                                                            
         DC    H'0'                                                             
*                                                                               
SPA70    DS    0H                                                               
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
*                                                                               
SPA80    MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         OI    DMINBTS,X'08'       CHECK DELETED RECS TOO                       
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),SAVEKEY                                                  
         BE    SPA10                                                            
         DC    H'0'                                                             
*                                                                               
* PRINT ERROR FOR TRAFFIC BUY RECORD ALREADY ON NEW STATION *                   
*                                                                               
DUP40    GOTO1 CLUNPK,DMCB,KEY+3,P+10                                           
         GOTO1 MSUNPK,(R1),KEY+6,P+20,P+30                                      
         MVC   P+45(40),NEWEXIST                                                
         MVC   P+69(2),=C'32'                                                   
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SPA80                                                            
*                                                                               
SPB00    DS    0H                                                               
         BAS  RE,REPRT                                                          
         EJECT                                                                  
* SPB00 - THIS SECTION UPDATES TRAFFIC STATION LABEL LIST RECORDS *             
         SPACE                                                                  
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A2F'     LABEL LIST RECORD                            
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         SPACE                                                                  
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+10                NO, THEN ALL CLIENTS                         
         MVC   KEY+3(2),SAVCLT                                                  
         GOTO1 HIGH                                                             
         B     SPB20                                                            
*                                                                               
SPB10    GOTO1 SEQ                                                              
*                                                                               
SPB20    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SPC00               NO,THEN TRY NEXT                             
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+14                NO, THEN ALL CLIENTS                         
         CLC   KEY+3(2),SAVCLT                                                  
         BNE   SPC00                                                            
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         L     R6,ADBUY            POINT TO RECORD                              
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   SPB10                                                            
         MVI   CHGEL,C'N'          SET OFF ELEM CHANGED                         
         B     *+12                                                             
*                                                                               
SPB30    BAS   RE,NEXTEL                                                        
         BNE   SPB40                                                            
*                                                                               
         CLC   2(3,R6),OLDSTA      TEST IF OLD STATION                          
         BE    SPB34                                                            
         SPACE                                                                  
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
         SPACE                                                                  
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
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SPB60    DS    0H                                                               
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,PUTREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         BE    SPB70                                                            
         DC    H'0'                                                             
*                                                                               
SPB70    DS    0H                                                               
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
         B     SPB10                                                            
*                                                                               
SPC00    DS    0H                                                               
         BAS   RE,REPRT                                                         
         EJECT                                                                  
* SPC00 - THIS SECTION UPDATES TRAFFIC MARKET STATION LIST RECORDS *            
         SPACE                                                                  
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         MVI   CHGEL,0                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A31'     STATION LIST RECORD                          
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         SPACE                                                                  
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+10                NO, THEN ALL CLIENTS                         
         MVC   KEY+3(2),SAVCLT                                                  
         GOTO1 HIGH                                                             
         B     SPC20                                                            
*                                                                               
SPC10    GOTO1 SEQ                                                              
*                                                                               
SPC20    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SPD00               NO,THEN TRY NEXT                             
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+14                NO, THEN ALL CLIENTS                         
         CLC   KEY+3(2),SAVCLT                                                  
         BNE   SPD00                                                            
         CLC   KEY+8(2),OLDMKT     TEST OLD MARKET THE SAME                     
         BNE   SPC10                                                            
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         L     R6,ADBUY            POINT TO RECORD                              
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   SPC10                                                            
         MVI   CHGEL,C'N'                                                       
         B     *+12                                                             
*                                                                               
SPC30    BAS   RE,NEXTEL                                                        
         BNE   SPC40                                                            
*                                                                               
         CLC   2(3,R6),OLDSTA      TEST IF OLD STATION                          
         BE    SPC34                                                            
         SPACE                                                                  
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
         SPACE                                                                  
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
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SPC60    CLI   RCWRITE,C'Y'                                                     
         BNE   SPC70                                                            
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,PUTREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         BE    SPC70                                                            
         DC    H'0'                                                             
*                                                                               
SPC70    DS    0H                                                               
         LH    R0,COUNT                                                         
         AHI   R0,1                                                             
         STH   R0,COUNT            # RECORDS CHANGED                            
         B     SPC10                                                            
*                                                                               
SPD00    DS    0H                                                               
         BAS   RE,REPRT                                                         
         EJECT                                                                  
* SPD00 - THIS SECTION UPDATES TRAFFIC PATTERN RECORDS *                        
         SPACE                                                                  
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         MVI   CHGEL,0                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A22'     PATTERN RECORD                               
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         SPACE                                                                  
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+10                NO, THEN ALL CLIENTS                         
         MVC   KEY+3(2),SAVCLT                                                  
         GOTO1 HIGH                                                             
         B     SPD20                                                            
*                                                                               
SPD10    GOTO1 SEQ                                                              
*                                                                               
SPD20    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SPE00               NO,THEN TRY NEXT                             
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    *+14                NO, THEN ALL CLIENTS                         
         CLC   KEY+3(2),SAVCLT                                                  
         BNE   SPE00                                                            
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         L     R6,ADBUY                                                         
*                                                                               
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   CHGEL,C'N'                                                       
*                                                                               
         CLI   2(R6),C'S'          TEST STATION LIST                            
         BNE   SPD10                                                            
         ZIC   R0,1(R6)            #STATIONS IN LIST                            
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
         SPACE                                                                  
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
         SPACE                                                                  
SPD46    LA    R1,5(R1)                                                         
         BCT   R0,SPD40                                                         
*                                                                               
         CLI   CHGEL,C'Y'          TEST CHANGED ELEMENT                         
         BNE   SPD10                                                            
         CLI   QOPT5,C'Y'                                                       
         BNE   SPD60                                                            
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SPD60    CLI   RCWRITE,C'Y'                                                     
         BNE   SPD70                                                            
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,PUTREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         BE    SPD70                                                            
         DC    H'0'                                                             
*                                                                               
SPD70    DS    0H                                                               
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
         B     SPD10                                                            
*                                                                               
SPE00    DS    0H                                                               
         BAS   RE,REPRT                                                         
         EJECT                                                                  
* SPE00 - THIS SECTION UPDATES TRAFFIC STATION RECORDS *                        
         SPACE                                                                  
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A28'     STATION ADDRESS RECS                         
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         SPACE                                                                  
         GOTO1 HIGH                                                             
         B     SPE20                                                            
*                                                                               
SPE10    GOTO1 SEQ                                                              
*                                                                               
SPE20    CLC   KEY(3),KEYSAVE      SAME TYPE/AM                                 
         BNE   SPF00               NO,THEN TRY NEXT                             
         CLC   KEY+3(5),QSTA       OLD STATION                                  
         BNE   SPE10                                                            
*                                                                               
         MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   SPE30                                                            
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SPE30    MVC   KEY+3(5),NEWCALL    DOES NEW RECORD ALREADY EXIST?               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    DUPE0               YES, DUPLICATE KEYS (TSK,TSK)                
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE DATAMGR SEQ                          
*                                                                               
         GOTO1 GET                                                              
*                                                                               
SPE40    CLI   RCWRITE,C'Y'                                                     
         BNE   SPE50                                                            
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMDEL',=C'TRFFILE',KEY,AREC                      
         TM    DM3,X'FD'                                                        
         BZ    SPE50                                                            
         DC    H'0'                                                             
*                                                                               
SPE50    MVC   3(5,R8),NEWCALL     UPDATE CALL LETTERS                          
         MVC   KEY+3(5),NEWCALL                                                 
         NI    15(R8),X'7F'        UNDELETE THE RECORD                          
         CLI   QOPT5,C'Y'                                                       
         BNE   SPE60                                                            
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SPE60    CLI   RCWRITE,C'Y'                                                     
         BNE   SPE70                                                            
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,ADDREC,=C'TRFFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         BE    SPE70                                                            
         DC    H'0'                                                             
*                                                                               
SPE70    DS    0H                                                               
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
*                                                                               
SPE80    MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         OI    DMINBTS,X'08'       CHECK DELETED RECS TOO                       
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
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
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SPE80                                                            
*                                                                               
SPF00    DS    0H                                                               
         BAS   RE,REPRT                                                         
*                                                                               
         L     RE,UTL                                                           
         MVC   4(1,RE),SPOTSE      RESTORE SPOT SYSTEM NUMBER                   
*                                                                               
         EJECT                                                                  
* SPSTA - THIS SECTION CKS IF STATION REC EXISTS FOR NEW CALL LETTERS *         
         SPACE                                                                  
SPSTA    DS    0H                                                               
         MVC   KEY(17),=17C'0'                                                  
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
         BZ    *+6                                                              
         DC    H'0'                                                             
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
         CLI   QMED,C'C'           THIS CANADIAN COMBINED                       
         BNE   EXIT                                                             
         MVC   BYTE,SVAGYMD                                                     
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'08'                                                       
         BNE   CKEOJ10                                                          
         NI    SVAGYMD,X'F7'                                                    
         OI    SVAGYMD,X'01'         SET TO TV                                  
         B     SP410                                                            
         SPACE                                                                  
CKEOJ10  CLI   BYTE,X'01'          THIS TV                                      
         BNE   EXIT                                                             
         NI    SVAGYMD,X'FE'                                                    
         OI    SVAGYMD,X'03'         SET TO NET                                 
         B     SP410                                                            
         EJECT                                                                  
* SPDEL - THIS SECTION DELETES THE PASSIVE POINTERS *                           
         SPACE                                                                  
SPDEL    NTR1                                                                   
         MVI   KEY+13,C'S'         PASSIVE POINTER - DELETE RECORD              
         CLI   QOPT5,C'Y'                                                       
         BNE   SPDEL10                                                          
         BAS   RE,SPTRACE          *** TRACE ***                                
SPDEL10  DS    0H                                                               
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   SPDEL20                                                          
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY,KEY                                
         TM    DM3,X'FD'                                                        
         BZ    SPDEL20                                                          
         DC    H'0'                                                             
SPDEL20  DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* THIS ROUTINE PRINTS THE BUY LINE NUMBERS FOR EACH PRODUCT *                   
         SPACE                                                                  
SPPRINT  NTR1                                                                   
         L     R4,ADCLT                                                         
         USING CLTHDRD,R4                                                       
         MVC   P+5(3),CLT          GET THE CLIENT CODE                          
         MVC   P+10(20),CLTNM      GET THE NAME OF THE CLIENT                   
         LA    R6,CLIST                                                         
SPPRT10  DS    0H                                                               
         CLC   3(1,R6),3(R5)       IS THIS THE SAME PRODUCT CODE?               
         BE    SPPRT20                                                          
         LA    R6,4(R6)                                                         
         CLI   0(R6),C' '                                                       
         BNL   SPPRT10                                                          
         MVC   P+37(22),=CL22'PRODUCT NAME NOT FOUND'                           
         B     SPPRT30                                                          
         DROP  R4                                                               
         SPACE                                                                  
SPPRT20  DS    0H                                                               
         MVC   P+47(3),0(R6)       GET THE PRODUCT MNEMONIC                     
SPPRT30  DS    0H                                                               
*                                  EDIT ESTIMATE NUMBER                         
         SPACE                                                                  
         EDIT  (B1,9(R8)),(3,P+64),ZERO=NOBLANK                                 
         SPACE                                                                  
         TM    0(R8),X'08'         THIS MEL'S COPIED BUYS                       
         BZ    *+8                                                              
         MVI   P+69,C'*'                                                        
         SPACE                                                                  
         ZIC   R0,10(R8)           GET THE NEW BUY NUMBER                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         EDIT  (P8,DUB),(3,P+111),ZERO=NOBLANK                                  
         ZIC   RF,M                                                             
         SR    R0,RF               GET THE OLD BUY NUMBER                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         EDIT  (P8,DUB),(3,P+86),ZERO=NOBLANK                                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              THIS ROUTINE PRINTS OUT #RECORDS CHANGED                         
*                                                                               
REPRT    NTR1                                                                   
         LH    R0,COUNT                                                         
         EDIT  (R0),(5,P+4),ZERO=NOBLANK                                        
*                                                                               
         CLI   KEYSAVE,X'10'       TEST BUY RECORDS                             
         BNH   *+12                                                             
         LA    R1,=CL32'  BUY RECORDS CHANGED'                                  
         B     REP20                                                            
*                                                                               
         CLI   KEYSAVE,X'0B'       TEST INVOICE RECORDS                         
         BNE   *+12                                                             
         LA    R1,=CL32'  OLD INV RECORDS CHANGED'                              
         B     REP20                                                            
*                                                                               
         CLI   KEYSAVE,SIRKTYPQ    TEST NSID RECORDS                            
         BNE   *+12                                                             
         LA    R1,=CL32'  NSID RECORDS CHANGED'                                 
         B     REP20                                                            
*                                                                               
         L     R1,=A(RECTAB)       POINT TO RECORD TABLE                        
REP10    CLI   0(R1),X'FF'                                                      
         BE    REPERR                                                           
         CLC   0(2,R1),KEYSAVE     TEST FOR REC TYPE                            
         BE    REP20                                                            
         LA    R1,L'RECTAB(R1)     BUMP TO NEXT ENTRY                           
         B     REP10                                                            
*                                                                               
REP20    MVC   P+10(L'RECTAB-2),2(R1)                                           
         GOTO1 REPORT                                                           
         B     REPX                                                             
*                                                                               
REPERR   DS    0H                                                               
         MVC   P+10(32),=C'CHANGED RECORD TYPE NOT IN TABLE'                    
         GOTO1 HEXOUT,DMCB,KEY,P+45,13,=C'TOG',0                                
         GOTO1 REPORT                                                           
*                                                                               
*                                                                               
REPX     B     EXIT                                                             
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
         GOTO1 HEXOUT,DMCB,KEY,P+10,18,=C'MIX',0                                
SPTRACE2 DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
         MVI   TRACECDE,0                                                       
         B     EXIT                                                             
         EJECT                                                                  
* SPPKG - THIS ROUTINE FIXES THE BUY LINE NUMBER IN PKG ELEMENT *               
         SPACE                                                                  
SPPKG    NTR1                                                                   
         LA    RE,24(R8)           POINT TO 1ST ELEMENT                         
SPPKG10  DS    0H                                                               
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0             IS THIS THE END OF THE RECORD?               
         BE    EXIT                 YES.                                        
         CLI   0(RE),5             IS THIS A PACKAGE ELEMENT?                   
         BNE   SPPKG10              NO.                                         
         ZIC   R0,1(RE)            GET THE LENGTH OF THE ELEMENT                
         SH    R0,=H'3'            R0 HAS THE NUMBER OF LINE NUMBERS            
         BP    *+6                                                              
         DC    H'0'                                                             
         LR    RF,RE                                                            
         LA    RE,3(RE)            POINT TO 1ST LINE NUMBER                     
SPPKG20  DS    0H                                                               
         IC    R5,0(RE)                                                         
         AR    R5,R6                                                            
         STC   R5,0(RE)            ADJUST BUY LINE NUMBER                       
         LA    RE,1(RE)                                                         
         BCT   R0,SPPKG20                                                       
         B     EXIT                                                             
         SPACE                                                                  
         MVC   SAVEPRT(132),P                                                   
         XC    P,P                                                              
         MVI   P,C'P'                                                           
         LR    RE,RF                                                            
         GOTO1 HEXOUT,DMCB,0(RE),P+10,30,=C'MIX',0                              
         B     SPTRACE2                                                         
         EJECT                                                                  
* HEADHOOK ROUTINE *                                                            
         SPACE                                                                  
         DROP  RB,R7,R3                                                         
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
         GETEL R6,24,ELCODE                                                     
*                                                                               
         LTORG                                                                  
         SPACE 5                                                                
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
SAVDA    DS    F                   SAVE DISK ADDR OF 1ST NEW INVOICE            
SPSCR9   DC    3F'0'               SAVE AREA FOR R9, RA AND RB                  
SPSCR7   DC     F'0'               SAVE AREA FOR R7                             
SPSCR3   DC     F'0'               SAVE AREA FOR R3                             
SAVCLT   DS    H                                                                
SAVEKEY  DC    XL20'00'                                                         
SAVEKEY2 DC    XL20'00'                                                         
**********************SVKEY    DC    XL13'00'                                   
NEWEXIST DC    CL40'* ERROR * NEW STATION 0A24 RECORD EXISTS'                   
BUYSW    DS    C                                                                
CLTBUYSW DC    C'N'                                                             
CANAGY   DS    C                                                                
M        DS    C                                                                
N        DS    C                                                                
TRACECDE DS    C                   TRACE CODE                                   
SAVEPRT  DS    CL132                                                            
         DS    0H                                                               
NEWCALL  DS    CL5                 NEW STATION CALL LETTERS                     
         DS    0H                                                               
OLDMSTA  DS    0CL5                                                             
OLDMKT   DS    H                                                                
OLDSTA   DS    CL3                                                              
         DS    0H                                                               
NEWMSTA  DS    0CL5                                                             
NEWMKT   DS    H                                                                
NEWSTA   DS    CL3                                                              
CANNMKT  DS    CL4                                                              
CANNET   DS    CL5                                                              
CNETMSTA DS    CL5                 CANADIAN NETWORK CALL LETTERS                
NETNIBLE DS    XL1                                                              
COUNT    DS    H                   RECORD COUNTER                               
ELCODE   DS    CL1                                                              
CHGEL    DS    CL1                                                              
SCHEME   DS    XL2                 NSID SCHEME CODE                             
*SKEY     DS    XL24 ************** UID IS COMMENTED OUT                        
*SVSTKEY  DS    XL15 ************** UID IS COMMENTED OUT                        
*                                                                               
*CURUID   DS    XL6  ************** CURRENT UID                                 
*OLDUID   DS    XL6  ************** OLD ONE                                     
*NEWUID   DS    XL6  ************** NEW ONE, DUH                                
*STAIO    DS    XL(SCBLSQNQ)                                                    
*                                                                               
         DROP  RB,R7,R3                                                         
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
* THIS ROUTINE REPLACES THE CALL LETTERS   *                                    
*  IN THE CANADIAN NETWORK STATION ELEMENT *                                    
         SPACE                                                                  
SPCNETWK NMOD1 0,SPCNETWK                                                       
         USING SPSC02+8192,R3                                                   
         LA    R6,24(R8)           POINT TO THE FIRST ELEMENT                   
SPCNT10  DS    0H                                                               
         ZIC   R0,1(R6)                                                         
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
         BNZ   SPCNT14                                                          
         DC    H'0'                                                             
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
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             IS THIS THE END OF THE RECORD?               
         BE    ERROR20                                                          
         CLC   =X'680B',0(R6)      IS THIS A CANADIAN NETWORK ELEMENT?          
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
         BE    *+6                                                              
         DC    H'0'                                                             
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
         DROP  RB,R3                                                            
         EJECT                                                                  
* THIS SECTION FINDS THE HIGHEST LINE NUMBER (N) FOR THIS *                     
*  STATION IN ITS OLD MARKET AND THEN FINDS THE HIGHEST   *                     
*  LINE NUMBER (M) FOR THIS STATION IN ITS NEW MARKET     *                     
         SPACE                                                                  
HIGHBUY  NMOD1 0,HIGHBUY                                                        
         USING SPSC02+8192,R3                                                   
         MVI   USRSW1,C'N'         SET USRSW1 TO LESS THAN 255 BUYS             
         MVC   SAVEKEY(13),KEY                                                  
         GOTO1 HIGH                                                             
HBUY00   CLC   KEY(10),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST?                
         BNE   HBUY10                                                           
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 SEQ                                                              
         B     HBUY00                                                           
*                                                                               
HBUY10   MVC   N,KEYSAVE+11        N HAS HIGHEST LINE NUMBER IN OLD MKT         
         MVC   KEY(13),SAVEKEY                                                  
         MVC   KEY+6(3),NEWSTA                                                  
         CLI   KEY+6,X'E8'         TEST SF CABLE                                
         BL    HBUY12                                                           
         MVC   DUB(3),SAVEKEY+6    MOVE SF STATION                              
         NI    DUB+2,X'7F'         DROP BIT THAT IS PART OF STATION             
         OC    KEY+8(1),DUB+2      'OR' IN CABLE NTWK                           
*                                                                               
HBUY12   MVI   KEY+11,0                                                         
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
*                                                                               
HBUY20   CLC   KEY(10),KEYSAVE     SAME A-M/CLT/PRD/MKT/STA/EST?                
         BNE   HBUY30                                                           
         MVC   KEYSAVE(13),KEY                                                  
         GOTO1 SEQ                                                              
         B     HBUY20                                                           
         SPACE                                                                  
HBUY30   DS    0H                                                               
         MVC   M,KEYSAVE+11        M HAS HIGHEST LINE NUMBER IN NEW MKT         
         ZIC   RF,M                                                             
         ZIC   RE,N                                                             
         AR    RF,RE                                                            
         CH    RF,=H'255'          ARE THERE TOO MANY BUYS?                     
         BH    HBUYER              ERROR - TOO MANY BUYS                        
HBUY40   MVC   KEY(13),SAVEKEY     RESTORE LAST KEY READ BY CONTROLLER          
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),KEYSAVE                                                  
         BE    HBUYX                                                            
         DC    H'0'                                                             
HBUYX    XIT1                                                                   
         SPACE                                                                  
HBUYER   XC    P,P                                                              
         MVC   P+5(3),CLT          GET THE CLIENT CODE                          
         MVC   P+10(33),=CL33'ERROR-TOO MANY BUYS BOTH STATIONS'                
         L     R4,ADCLT                                                         
         USING CLTHDRD,R4                                                       
         LA    R4,CLIST                                                         
         DROP  R4                                                               
         SPACE                                                                  
HBUYER10 CLC   3(1,R4),SAVEKEY+3   IS THIS THE SAME PRODUCT CODE?               
         BE    HBUYER20                                                         
         LA    R4,4(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNL   HBUYER10                                                         
         MVC   P+47(15),=CL15'*** UNKNOWN PRD'                                  
         B     HBUYER30                                                         
         SPACE                                                                  
HBUYER20 MVC   P+47(3),0(R4)       GET THE PRODUCT MNEMONIC                     
         SPACE                                                                  
HBUYER30 MVC   DUB(1),SAVEKEY+9                                                 
         EDIT  (B1,DUB),(3,P+64)                                                
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVI   USRSW1,C'Y'                                                      
         B     HBUY40                                                           
         DROP  RB                                                               
         EJECT                                                                  
* PW100 - THIS SECTION UPDATES DOUBLE BOOKING RECORDS *                         
         SPACE                                                                  
PW100    NMOD1 0,**PW100*                                                       
         USING SPSC02+8192,R3                                                   
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D7A'     PROFIT WITHIN RECORD                         
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         SPACE                                                                  
         GOTO1 HIGH                                                             
         B     PW120                                                            
*                                                                               
PW110    GOTO1 SEQ                                                              
*                                                                               
PW120    CLC   KEY(3),KEYSAVE      SAME 0D7A/A-M                                
         BNE   PW190                NO. END OF THIS REC TYPE/AM                 
         SPACE                                                                  
         CLC   OLDSTA,KEY+9                                                     
         BE    PW124                                                            
         SPACE                                                                  
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
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    PW134                                                            
         MVC   BYTE,SAVEKEY+11     SAVE CABLE HEAD NETWORK                      
         NI    BYTE,X'7F'                                                       
         OC    KEY+11(1),BYTE      SET IN NETWORK                               
         SPACE                                                                  
PW134    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     SAME 0D7A/A-M/CLT/PRD/EST/MKT/STA            
         BE    PWDUPERR             YES, DUPLICATE KEYS (TSK,TSK)               
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE DATAMGR SEQ                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    PW140                                                            
         DC    H'0'                                                             
*                                                                               
PW140    GOTO1 GET                                                              
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   PW150                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC                          
         TM    DM3,X'FD'                                                        
         BZ    PW150                                                            
         DC    H'0'                                                             
*                                                                               
PW150    MVC   DUB(1),11(R8)       SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+11                                                  
         MVC   9(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+9(3),NEWSTA                                                  
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    PW156                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    11(1,R8),DUB        SET IN NETWORK                               
         OC    KEY+11(1),DUB       SET IN NETWORK                               
         SPACE                                                                  
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
         BE    PW170                                                            
         DC    H'0'                                                             
         SPACE                                                                  
PW170    DS    0H                                                               
         LH    R0,COUNT                                                         
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
         SPACE                                                                  
         GOTO1 MSUNPK,DMCB,KEY+7,P+40,P+50                                      
         MVC   P+60(40),=CL40'* ERROR * P/W REC EXISTS FOR NEW STATION'         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     PW180                                                            
*                                                                               
PW190    DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
* PW200 - THIS SECTION UPDATES STATION LOCKIN RECORDS *                         
         SPACE                                                                  
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
         BE    PW240                                                            
         DC    H'0'                                                             
*                                                                               
PW240    GOTO1 GET                                                              
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   PW250                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC                          
         TM    DM3,X'FD'                                                        
         BZ    PW250                                                            
         DC    H'0'                                                             
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
         BE    *+6                                                              
         DC    H'0'                                                             
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
         BE    PW270                                                            
         DC    H'0'                                                             
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
* PW300 - THIS SECTION UPDATES XSPT STATION LOCKIN RECORDS *                    
         SPACE                                                                  
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
K        USING SLKRECD,R6                                                       
         MVI   K.SLKKTYP,SLKKTYPQ                                               
         MVI   K.SLKKSUB,SLKKSUBQ                                               
         MVC   K.SLKKAGMD,BAGYMD                                                
         SPACE                                                                  
         MVC   XKEYSAVE,XKEY                                                    
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
         SPACE                                                                  
         CLC   OLDMSTA,K.SLKKMKT   IS THIS REQUESTED MARKET/STATION             
         BE    PW326                GO MOVE IT                                  
         SPACE                                                                  
         CLC   OLDSTA,K.SLKKSTA    IS THIS THE REQUESTED STATION                
         BE    MKTERRX              WHY IS IT IN WRONG MARKET?                  
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    PW310                                                            
         MVC   FULL(3),K.SLKKSTA+2                                              
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
         GOTO1 HEXOUT,DMCB,K.SLKKAGMD,P+15,15,=C'MIX',0                         
         DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
*                                                                               
PW330    MVC   K.SLKKSTA(3),NEWSTA    DOES NEW RECORD ALREADY EXIST?            
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    PW334                                                            
         SPACE                                                                  
*                                            SAVE CABLE HEAD NETWORK            
         SPACE                                                                  
         MVC   BYTE,SAVEKEYX+SLKKSTA+2-SLKRECD                                  
         NI    BYTE,X'7F'                                                       
         OC    K.SLKKSTA+2(1),BYTE     SET IN NETWORK                           
         SPACE                                                                  
PW334    DS   0H                                                                
         MVC   XKEYSAVE,XKEY                                                    
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',XKEYSAVE,XKEY                     
         CLI   DM3,0                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   XKEY(32),XKEYSAVE   SAME 0D73/A-M/CLT/MKT/STA/ETC                
         BE    PXDUPERL             YES, DUPLICATE KEYS (TSK,TSK)               
         MVC   XKEY,SAVEKEYX                                                    
         MVC   XKEYSAVE,SAVEKEYX                                                
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',XKEYSAVE,XKEY                     
         CLI   DM3,0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         CLC   XKEY(32),XKEYSAVE                                                
         BE    PW340                                                            
         DC    H'0'                                                             
*                                                                               
PW340    DS   0H                                                                
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFILE',XKEY+36,AREC,DMWORK              
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   PW350                                                            
         SPACE                                                                  
         OI    XKEY+32,X'80'        MARK FOR DELETION                           
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'XSPDIR ',XKEY,XKEY                     
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AREC                                                          
         OI    34(R1),X'80'        MARK RECORD FOR DELETION                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFILE',XKEY+36,AREC,DMWORK          
*                                                                               
         TM    DM3,X'FD'                                                        
         BZ    PW350                                                            
         DC    H'0'                                                             
*                                  SAVE POSSIBLE CABLE HEAD NETWORK             
PW350    DS   0H                                                                
         MVC   DUB(1),SLKKSTA+2-SLKRECD(R8)                                     
         MVC   DUB+1(1),K.SLKKSTA+2                                             
         MVC   SLKKSTA-SLKRECD(,R8),NEWSTA      UPDATE CALL LETTERS             
         MVC   K.SLKKSTA,NEWSTA                                                 
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    PW356                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    SLKKSTA+2-SLKRECD(1,R8),DUB        SET IN NETWORK                
         OC    K.SLKKSTA+2(1),DUB                  SET IN NETWORK               
         SPACE                                                                  
PW356    DS   0H                                                                
         NI    XKEY+32,X'7F'       UNDELETE THE KEY                             
         L     R1,AREC                                                          
         NI    34(R1),X'7F'        UNDELETE THE RECORD                          
         CLI   QOPT5,C'Y'                                                       
         BNE   PW360                                                            
*                                                                               
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVI   P+1,C'I'                                                         
         GOTO1 HEXOUT,DMCB,XKEY,P+10,4,=C'MIX',0                                
         GOTO1 HEXOUT,DMCB,K.SLKKAGMD,P+15,30,=C'MIX',0                         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
*                                                                               
         SPACE                                                                  
PW360    CLI   RCWRITE,C'Y'                                                     
         BNE   PW370                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFILE',XKEY+36,AREC,DMWORK              
         CLI   DM3,0                                                            
         BE    PW370                                                            
         DC    H'0'                                                             
         SPACE                                                                  
PW370    DS    0H                                                               
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT            # RECORDS CHANGED                            
*                                                                               
PW380    MVC   XKEY,SAVEKEYX       RESTORE KEY                                  
         OI    DMINBTS,X'08'       CHECK DELETED RECS TOO                       
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),=C'XSPDIR',XKEYSAVE,XKEY           
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         NI    DMINBTS,X'F7'                                                    
         CLC   XKEY(32),SAVEKEYX                                                
         BE    PW310                                                            
         DC    H'0'                                                             
*                                                                               
* PRINT ERROR FOR P/W REC ALREADY ON NEW STATION *                              
*                                                                               
PXDUPERL DS    0H                                                               
         MVC   P+10(4),=C'CLT='                                                 
         GOTO1 CLUNPK,DMCB,K.SLKKCLT,P+14                                       
         SPACE                                                                  
         GOTO1 MSUNPK,DMCB,K.SLKKMKT,P+40,P+50                                  
         MVC   P+60(40),=CL40'* ERROR * P/W REC EXISTS FOR NEW STATION'         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   XKEYSAVE,SAVEKEYX                                                
         B     PW380                                                            
*                                                                               
MKTERRX  DS    0H                                                               
         MVC   P+10(4),=C'CLT='                                                 
         GOTO1 CLUNPK,DMCB,K.SLKKCLT,P+14                                       
         SPACE                                                                  
         GOTO1 MSUNPK,DMCB,K.SLKKMKT,P+20,P+30                                  
         MVC   P+40(37),=CL39'* ERROR * STATION IN WRONG MKT-X LOCKIN'          
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
         DROP  RB,R3                                                            
         EJECT                                                                  
* DB100 - THIS SECTION UPDATES DOUBLE BOOKING RECORDS *                         
         SPACE                                                                  
DB100    NMOD1 0,DB100                                                          
         USING SPSC02+8192,R3                                                   
         MVC   AREC,ADBUY                                                       
         L     R8,AREC                                                          
         XC    COUNT,COUNT                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D7B'     DOUBLE BOOKING RECORD                        
         MVC   KEY+2(1),SVAGYMD    A-M                                          
         SPACE                                                                  
         GOTO1 HIGH                                                             
         B     DB120                                                            
*                                                                               
DB110    GOTO1 SEQ                                                              
*                                                                               
DB120    CLC   KEY(3),KEYSAVE      SAME 0D7B/A-M                                
         BNE   DB190                NO. END OF THIS REC TYPE/AM                 
         SPACE                                                                  
         CLC   OLDSTA,KEY+5                                                     
         BE    DB124                                                            
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    DB110                                                            
         MVC   FULL(3),KEY+5                                                    
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
         DS    0H                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   P(132),SAVEPRT      RESTORE PRINT LINE                           
*                                                                               
DB130    MVC   KEY+5(3),NEWMSTA    DOES NEW RECORD ALREADY EXIST?               
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    DB134                                                            
         MVC   BYTE,SAVEKEY+7      SAVE CABLE HEAD NETWORK                      
         NI    BYTE,X'7F'                                                       
         OC    KEY+7(1),BYTE       SET IN NETWORK                               
         SPACE                                                                  
DB134    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     SAME 0D7B/A-M/YR/MO/STA                      
         BE    DBDUPERR             YES, DUPLICATE KEYS (TSK,TSK)               
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE DATAMGR SEQ                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    DB140                                                            
         DC    H'0'                                                             
*                                                                               
DB140    GOTO1 GET                                                              
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   DB150                                                            
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC                          
         TM    DM3,X'FD'                                                        
         BZ    DB150                                                            
         DC    H'0'                                                             
*                                                                               
DB150    MVC   DUB(1),7(R8)        SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+7                                                   
         MVC   5(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   KEY+5(3),NEWSTA                                                  
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    DB156                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    7(1,R8),DUB   SET IN NETWORK                                     
         OC    KEY+7(1),DUB       SET IN NETWORK                                
         SPACE                                                                  
DB156    NI    15(R8),X'7F'        UNDELETE THE RECORD                          
         CLI   QOPT5,C'Y'                                                       
         BNE   DB160                                                            
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
DB160    CLI   RCWRITE,C'Y'                                                     
         BNE   DB170                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,AREC,DMWORK                   
         CLI   DM3,0                                                            
         BE    DB170                                                            
         DC    H'0'                                                             
         SPACE                                                                  
DB170    DS    0H                                                               
         LH    R0,COUNT                                                         
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
DB190    DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB,R3                                                            
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
         DC    XL2'0A28',CL30'STAT ADDRESS RECORDS CHANGED  '                   
         DC    X'FF'                                                            
         EJECT                                                                  
* DARORD - THIS SECTION UPDATES DARE ORDER RECORDS *                            
         SPACE                                                                  
DARORD   NMOD1 0,**DARO**                                                       
         USING SPSC02+8192,R3                                                   
         XC    COUNT,COUNT                                                      
         L     R8,AREC                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING DOKEY,R4                                                         
         MVI   DOKTYPE,DOKTYPQ     DARE ORDER RECORD                            
         MVI   DOKSUBTY,DOKSTYPQ                                                
         MVC   DOKAGMD,SVAGYMD     A-M                                          
         SPACE                                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         B     DO220                                                            
         SPACE                                                                  
DO200    GOTO1 SEQ                                                              
         SPACE                                                                  
DO220    CLC   KEY(5),KEYSAVE    SAME 0D/34/A-M                                 
         BNE   DO300                NO, DONE                                    
         SPACE                                                                  
         CLC   DOKSTA,OLDSTA     STATION CALL LETTERS                           
         BE    DO226                YES                                         
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    DO200                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RE,7,DOKSTA                                                      
         ICM   RF,7,OLDSTA                                                      
         SRL   RE,7                DROP NET FROM CABLE HEAD                     
         SRL   RF,7                                                             
         CR    RE,RF               IS THIS THE SAME CABLE HEAD                  
         BNE   DO200                NO                                          
         SPACE                                                                  
DO226    MVC   SAVEKEY,KEY         SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   DO230                                                            
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,DOTRACE          ***TRACE***                                  
         SPACE                                                                  
DO230    MVC   DOKSTA,NEWSTA       DOES NEW DARE ORD ALREADY EXIST?             
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    DO234                                                            
         MVC   BYTE,SAVEKEY+11                                                  
         NI    BYTE,X'7F'                                                       
         OC    DOKSTA+2(1),BYTE    SET IN NETWORK                               
         SPACE                                                                  
DO234    MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(13),KEYSAVE     SAME 0D34/A-M/ORD/STA/TYPE                   
         BE    DO290                YES. DON'T MOVE                             
         SPACE                                                                  
         MVC   KEY,SAVEKEY         NO. RESTORE KEY                              
         GOTO1 HIGH                                                             
         SPACE                                                                  
         GOTO1 GET                                                              
         SPACE                                                                  
         CLI   DOKCMT,0            IF COMMENT REC, MOVE IT                      
         BNE   DO236                                                            
         SPACE                                                                  
         OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    DO236                NO.                                         
         SPACE                                                                  
         L     R6,AREC                                                          
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DOIDELD,R6                                                       
         CLC   DOIDCLT,SAVCLT                                                   
         BNE   DO200                GET NEXT                                    
         DROP  R6                                                               
         SPACE                                                                  
DO236    CLI   RCWRITE,C'Y'                                                     
         BNE   DO240                                                            
         OI    KEY+13,X'80'        MARK FOR DELETION                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR ',KEY,KEY                       
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    15(R8),X'80'        MARK RECORD FOR DELETION                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFILE',KEY+14,AREC,DMWORK           
*                                                                               
         TM    DM3,X'FD'                                                        
         BZ    DO237                                                            
         DC    H'0'                                                             
DO237    NI    KEY+13,X'7F'        TURN OFF DELETE BIT                          
         NI    15(R8),X'7F'        TURN OFF DELETE BIT                          
         SPACE                                                                  
*        SAVE POSSIBLE CABLE HEAD NETWORK                                       
         SPACE                                                                  
DO240    MVC   DARCNET1,DOKSTA+2-DOKEY(R8)                                      
         MVC   DARCNET2,DOKSTA+2                                                
         MVC   DOKSTA-DOKEY(3,R8),NEWSTA      UPDATE CALL LETTERS               
         MVC   DOKSTA,NEWSTA                                                    
         SPACE                                                                  
         CLI   SAVEKEY+DOKCMT-DOKEY,0  BYPASS AGY/REP COMMENTS                  
         BNE   DO242                                                            
         SPACE                                                                  
         L     R6,AREC                                                          
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DOIDELD,R6                                                       
         MVC   DAROLDST,DOISTA     SAVE OLD STATION                             
         SPACE                                                                  
         MVC   DOISTA,NEWSTA                                                    
         SPACE                                                                  
DO242    CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    DO244                                                            
         NI    DARCNET1,X'7F'                                                   
         NI    DARCNET2,X'7F'                                                   
         CLC   DARCNET1,DARCNET2   BETTER BE SAME NETWORK                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    DOKSTA+2(1),DARCNET1 SET IN NETWORK                              
         SPACE                                                                  
         CLI   SAVEKEY+DOKCMT-DOKEY,0  BYPASS AGY/REP COMMENTS                  
         BNE   DO244                                                            
         SPACE                                                                  
         OC    DOISTA+2(1),DARCNET1 SET IN NETWORK                              
         DROP  R6                                                               
         SPACE                                                                  
DO244    CLI   QOPT5,C'Y'                                                       
         BNE   DO250                                                            
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,DOTRACE          ***TRACE***                                  
         SPACE                                                                  
DO250    CLI   RCWRITE,C'Y'                                                     
         BNE   DO260                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,=C'SPTFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         BE    DO260                                                            
         DC    H'0'                                                             
DO260    MVC   SAVDA,KEY+14        SAVE DISK ADDR OF 1ST ADD                    
         SPACE                                                                  
* DELETE/ADD PASSIVE KEYS *                                                     
         SPACE                                                                  
         CLI   SAVEKEY+DOKCMT-DOKEY,0  BYPASS AGY/REP COMMENTS                  
         BNE   DO276                                                            
         SPACE                                                                  
* UPDATE 0DB4                                                                   
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVI   DBKTYPE,DBKTYPQ                                                  
         MVI   DBKSUBTY,DBKSTYPQ                                                
         MVC   DBKAGMD,SVAGYMD                                                  
         L     R6,AREC                                                          
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DOIDELD,R6                                                       
         MVC   DBKBYR,DOIDBYR                                                   
         MVC   DBKORD,SAVEKEY+DOKORDER-DOKEY                                    
         MVC   DBKSTA,SAVEKEY+DOKSTA-DOKEY                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   KEY+13,C'S'         PASSIVE POINTER - DELETE RECORD              
         CLI   QOPT5,C'Y'                                                       
         BNE   DO262                                                            
         BAS   RE,DOTRACE          *** TRACE ***                                
DO262    DS    0H                                                               
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   DO264                                                            
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY,KEY                                
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   DBKSTA,NEWSTA       UPDATE CALL LETTERS                          
         MVI   KEY+13,0            PASSIVE POINTER - UNDELETE RECORD            
         MVC   KEY+14(4),SAVDA                                                  
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    *+10                                                             
         OC    DBKSTA+2(1),DARCNET1 SET IN NETWORK                              
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,DMADD,SPTDIR,KEY,KEY                                
         SPACE                                                                  
         TM    DM3,X'DD'          ALLOW DUP KEY ON ADD OR DELETED REC           
         BZ    *+6                                                              
         DC    H'0'                ERROR                                        
         SPACE                                                                  
* UPDATE 0DB5                                                                   
         SPACE                                                                  
DO264    XC    KEY,KEY                                                          
         MVI   DCKTYPE,DCKTYPQ                                                  
         MVI   DCKSUBTY,DCKSTYPQ                                                
         MVC   DCKAGMD,SVAGYMD                                                  
         MVC   DCKCLT,DOIDCLT                                                   
         MVC   DCKPRD,DOIDPRD                                                   
         MVC   DCKEST,DOIDEST                                                   
         MVC   DCKSTA,DAROLDST                                                  
         MVC   DCKPRD2,DOIDPRD2                                                 
         MVC   DCKFLTNM,DOIDFLTN                                                
         OI    DMINBTS,X'08'       PASS BACK THE DELETED RECORDS                
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   KEY+13,C'S'         PASSIVE POINTER - DELETE RECORD              
         CLI   QOPT5,C'Y'                                                       
         BNE   DO266                                                            
         BAS   RE,DOTRACE          *** TRACE ***                                
DO266    DS    0H                                                               
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   DO270                                                            
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY,KEY                                
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   DCKSTA,NEWSTA       UPDATE CALL LETTERS                          
         MVI   KEY+13,0            PASSIVE POINTER - UNDELETE RECORD            
         MVC   KEY+14(4),SAVDA                                                  
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    *+10                                                             
         OC    DCKSTA+2(1),DARCNET1 SET IN NETWORK                              
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,DMADD,SPTDIR,KEY,KEY                                
         SPACE                                                                  
         TM    DM3,X'DD'          ALLOW DUP KEY ON ADD OR DELETED REC           
         BZ    *+6                                                              
         DC    H'0'                ERROR                                        
         SPACE                                                                  
* UPDATE 0DB7                                                                   
         SPACE                                                                  
DO270    XC    KEY,KEY                                                          
         MVI   DSKTYPE,DSKTYPQ                                                  
         MVI   DSKSUBTY,DSKSTYPQ                                                
         MVC   DSKAGMD,SVAGYMD                                                  
         MVC   DSKBYR,DOIDBYR                                                   
         MVC   DSKSTA,SAVEKEY+DOKSTA-DOKEY                                      
         MVC   DSKORD,SAVEKEY+DOKORDER-DOKEY                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   KEY+13,C'S'         PASSIVE POINTER - DELETE RECORD              
         CLI   QOPT5,C'Y'                                                       
         BNE   DO274                                                            
         BAS   RE,DOTRACE          *** TRACE ***                                
DO274    DS    0H                                                               
         CLI   RCWRITE,C'Y'        SHOULD I WRITE THIS RECORD?                  
         BNE   DO276                                                            
         GOTO1 DATAMGR,DMCB,DMWRT,SPTDIR,KEY,KEY                                
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   DSKSTA,NEWSTA       UPDATE CALL LETTERS                          
         MVI   KEY+13,0            PASSIVE POINTER - UNDELETE RECORD            
         MVC   KEY+14(4),SAVDA                                                  
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    *+10                                                             
         OC    DSKSTA+2(1),DARCNET1 SET IN NETWORK                              
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,DMADD,SPTDIR,KEY,KEY                                
         SPACE                                                                  
         TM    DM3,X'DD'          ALLOW DUP KEY ON ADD OR DELETED REC           
         BZ    *+6                                                              
         DC    H'0'                ERROR                                        
         SPACE                                                                  
* PRINT THE DARE ORDER RECORD *                                                 
         SPACE                                                                  
DO276    DS    0H                                                               
         SPACE                                                                  
         LH    R1,COUNT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,COUNT            # RECORDS CHANGED                            
         SPACE                                                                  
         CLI   SAVEKEY+DOKCMT-DOKEY,0  BYPASS AGY/REP COMMENTS                  
         BNE   DO280                                                            
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,DOIDCLT,P+10                                         
         SPACE                                                                  
DO280    MVC   FULL,SAVEKEY+DOKORDER-DOKEY                                      
         XC    FULL,=X'FFFFFFFF'                                                
         SR    R0,R0                                                            
         ICM   R0,3,FULL                                                        
         A     R0,=F'90000'                                                     
         C     R0,=F'100000'                                                    
         BNH   *+8                                                              
         S     R0,=F'100000'                                                    
         EDIT  (R0),(5,P+18)                                                    
         SR    R0,R0                                                            
         ICM   R0,3,FULL+2                                                      
         EDIT  (R0),(5,P+35),ZERO=NOBLANK                                       
         SPACE                                                                  
         MVI   P+41,C'0'                                                        
         CLI   SAVEKEY+DOKCMT-DOKEY,0                                           
         BE    DO284                                                            
         MVC   P+41(9),=C'1=AGY CMT'                                            
         CLI   SAVEKEY+DOKCMT-DOKEY,1                                           
         BE    DO284                                                            
         MVC   P+41(9),=C'2=REP CMT'                                            
         CLI   SAVEKEY+DOKCMT-DOKEY,2                                           
         BE    DO284                                                            
         MVC   P+43(7),=C'??? REC'                                              
DO284    MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         SPACE                                                                  
DO286    MVC   KEY,SAVEKEY         RESTORE KEY                                  
         OI    DMINBTS,X'08'       PASS BACK THE DELETED RECORDS                
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         SPACE                                                                  
         CLC   KEY(13),SAVEKEY                                                  
         BE    DO200                                                            
         DC    H'0'                                                             
         SPACE                                                                  
* PRINT ERROR FOR DAR ORD ALREADY ON NEW STATION FOR THIS DATE *                
         SPACE                                                                  
         USING DOIDELD,R6                                                       
DO290    GOTO1 CLUNPK,DMCB,DOIDCLT,P+10                                         
         MVC   FULL,DOKORDER                                                    
         XC    FULL,=X'FFFFFFFF'                                                
         SR    R0,R0                                                            
         ICM   R0,3,FULL                                                        
         A     R0,=F'90000'                                                     
         C     R0,=F'100000'                                                    
         BNH   *+8                                                              
         S     R0,=F'100000'                                                    
         EDIT  (R0),(5,P+18)                                                    
         SR    R0,R0                                                            
         ICM   R0,3,FULL+2                                                      
         EDIT  (R0),(5,P+35),ZERO=NOBLANK                                       
         SPACE                                                                  
         CLI   DOKCMT,0                                                         
         BE    DO294                                                            
         MVC   P+30(7),=C'AGY CMT'                                              
         CLI   DOKCMT,1                                                         
         BE    DO294                                                            
         MVC   P+30(7),=C'REP CMT'                                              
         CLI   DOKCMT,2                                                         
         BE    DO294                                                            
         MVC   P+30(7),=C'??? REC'                                              
DO294    MVC   P+45(50),=CL40'* ERROR * DAR ORD ALREADY ON NEW STATION'         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     DO286                                                            
         DROP  R4,R6                                                            
         SPACE                                                                  
DO300    LH    R0,COUNT                                                         
         EDIT  (R0),(5,P+4),ZERO=NOBLANK                                        
         MVC   P+10(26),=CL26'DARE ORDER RECORDS CHANGED'                       
         GOTO1 REPORT                                                           
         XIT1                                                                   
         SPACE 2                                                                
* THIS SECTION PRINTS A TRACE RECORD OF THE KEY *                               
         SPACE                                                                  
DOTRACE  NTR1                                                                   
         MVC   SAVEPRT(132),P      SAVE PRINT LINE                              
         MVC   P,SPACES                                                         
         MVC   P(1),TRACECDE                                                    
         GOTO1 HEXOUT,DMCB,KEY,P+10,13,=C'MIX',0                                
         SPACE                                                                  
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
         EJECT                                                                  
* NWS - THIS SECTION UPDATES NWS RECORDS *                                      
         SPACE                                                                  
NWS      NMOD1 0,**+NWS**                                                       
         USING SPSC02+8192,R3                                                   
         SPACE                                                                  
         XC    SAVECAM,SAVECAM                                                  
         XC    SAVEHDR,SAVEHDR                                                  
         XC    NWSTOTS,NWSTOTS                                                  
         SPACE                                                                  
* -- READ THROUGH CAMP RECORDS - AND IF CLT REQUESTED MATCH ON IT               
         SPACE                                                                  
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
         SPACE                                                                  
         CLI   QOPT5,C'Y'            TRACE REQ                                  
         BNE   *+12                                                             
         MVI   TRACECDE,14                                                      
         BAS   RE,NWTRACE          *** TRACE ***                                
         SPACE                                                                  
         GOTO1 GET                                                              
         L     R6,AREC                                                          
         SPACE                                                                  
         LA    R2,BWHFSTEL                                                      
NW124    CLI   0(R2),X'02'                                                      
         BE    NW126                                                            
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   NW124                                                            
         DC    H'0'               MUST BE STATION LIST                          
         SPACE                                                                  
NW126    CLC   QSTA,BWHSTA-BWHEL(R2)         OLD STATION IN RECORD              
         BE    NW130                                                            
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),X'02'                                                      
         BE    NW126                                                            
         B     NW190              STATION NOT IN CAMPAIGN - TRY NEXT            
*                                                                               
NW130    MVC   NOLDSTCD,BWHSEQ-BWHEL(R2)  SAVE OLD STATION CODE                 
         MVC   BWHSTA-BWHEL(5,R2),NEWCALL NEW NWS STATION CALL LETTERS          
         SPACE                                                                  
         LH    R1,NWSHDR                                                        
         LA    R1,1(,R1)                                                        
         STH   R1,NWSHDR                                                        
         SPACE                                                                  
         CLI   RCWRITE,C'Y'                                                     
         BNE   NW136                                                            
         GOTO1 PUT                                                              
         SPACE                                                                  
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
         SPACE                                                                  
NW172    DS    0H                 FIX DTL RECORDS                               
         CLC   KEY(06),KEYSAVE                                                  
         BNE   NW190                                                            
* NOTE - PUT CHANGES TO STA ADD/DEL HERE                                        
         SPACE                                                                  
         GOTO1 GET                                                              
         L     R6,AREC                                                          
         SPACE                                                                  
         MVI   BYTE,0                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    NW174                                                            
         SPACE                                                                  
         LA    R4,=CL30'NWS BUY DTL REC HAS NO 01 ELEM'                         
         L     R6,AREC                                                          
         SR    R5,R5                                                            
         ICM   R5,3,13(R6)                                                      
         GOTO1 =V(PRNTBL),DMCB,(30,(R4)),(R6),C'DUMP',(R5),=C'0D'               
         SPACE                                                                  
         B     NW190                                                            
         USING BWDEL,R6                                                         
NW174    CLC   QSTA,BWDSTA                                                      
         BNE   NW176                                                            
         MVI   BYTE,1              INDICATE RECORD CHANGED                      
         MVC   BWDSTA(5),NEWCALL                                                
NW176    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    NW174                                                            
         SPACE                                                                  
         CLI   BYTE,1              WAS RECORD CHANGED                           
         BNE   NW178                                                            
         SPACE                                                                  
         LH    R1,NWSDTL                                                        
         LA    R1,1(,R1)                                                        
         STH   R1,NWSDTL                                                        
         SPACE                                                                  
         CLI   RCWRITE,C'Y'                                                     
         BNE   NW178                                                            
         L     R6,AREC                                                          
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFILE',KEY+14,(R6),DMWORK           
*                                                                               
         CLI   DM3,0                                                            
         BE    NW178                                                            
         DC    H'0'                                                             
         SPACE                                                                  
NW178    DS    0H                                                               
         GOTO1 SEQ                                                              
         CLI   QOPT5,C'Y'          TRACE REQ                                    
         BNE   *+12                                                             
         MVI   TRACECDE,13                                                      
         BAS   RE,NWTRACE         ** TRACE **                                   
         B     NW172                                                            
         SPACE                                                                  
* NOW LOOK FOR BUY REVISION SAVE REC *                                          
         SPACE                                                                  
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
         SPACE                                                                  
         CLI   QOPT5,C'Y'            TRACE REQ                                  
         BNE   *+12                                                             
         MVI   TRACECDE,14                                                      
         BAS   RE,NWTRACE          *** TRACE ***                                
         SPACE                                                                  
         GOTO1 GET                                                              
         L     R6,AREC                                                          
         SPACE                                                                  
         CLI   RCWRITE,C'Y'                                                     
         BNE   NW196                                                            
         OI    KEY+13,X'80'        MARK FOR DELETION                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR ',KEY,KEY                       
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    15(R6),X'80'        MARK RECORD FOR DELETION                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFILE',KEY+14,(R6),DMWORK           
*                                                                               
         TM    DM3,X'FD'                                                        
         BZ    NW194                                                            
         DC    H'0'                                                             
NW194    NI    KEY+13,X'7F'        TURN OFF DELETE BIT                          
         NI    15(R8),X'7F'        TURN OFF DELETE BIT                          
         SPACE                                                                  
NW196    DS    0H                                                               
         MVC   NBRKSTA-NBRKEY+KEY,OLDSTA                                        
         MVC   NBRKSTA,OLDSTA                                                   
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   NW202                                                            
         USING NBRSELD,R6                                                       
         CLC   QSTA,NBRSSTA                                                     
         BE    NW204                                                            
         SPACE                                                                  
         CLI   QSTA+4,C'T'                                                      
         BNE   NW200                                                            
         CLI   NBRSSTA+4,C' '                                                   
         BNE   NW200                                                            
         CLC   QSTA(4),NBRSSTA                                                  
         BE    *+6                                                              
NW200    DC    H'0'                                                             
         SPACE                                                                  
         MVC   NBRSSTA(4),NEWCALL                                               
         B     NW206                                                            
         SPACE                                                                  
NW202    DS   0H                                                                
         LA    R4,=CL30'NWS BUY DTL REC HAS NO 10 ELEM'                         
         L     R6,AREC                                                          
         SR    R5,R5                                                            
         ICM   R5,3,13(R6)                                                      
         GOTO1 =V(PRNTBL),DMCB,(30,(R4)),(R6),C'DUMP',(R5),=C'0D'               
         B     NW290                                                            
         SPACE                                                                  
NW204    DS   0H                                                                
         MVC   NBRSSTA(5),NEWCALL                                               
         SPACE                                                                  
NW206    DS   0H                                                                
         LH    R1,NWSBRV                                                        
         LA    R1,1(,R1)                                                        
         STH   R1,NWSBRV                                                        
         SPACE                                                                  
         CLI   RCWRITE,C'Y'                                                     
         BNE   NW208                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,=C'SPTFILE',KEY+14,AREC,DMWORK               
         CLI   DM3,0                                                            
         BE    NW208                                                            
         DC    H'0'                                                             
NW208    DS   0H                                                                
         SPACE                                                                  
NW290    DS    0H                 NEXT CAMPAIGN RECORD                          
         MVC   KEY(13),SAVECAM    RE-ESTABLISH CAMPAIGN KEY                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 SEQ                                                              
         B     NW100              TRY NEXT CAMPAIGN                             
         SPACE                                                                  
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
NWSTOTS  DS   0XL6                                                              
NWSHDR   DS    H                                                                
NWSDTL   DS    H                                                                
NWSBRV   DS    H                                                                
         LTORG                                                                  
         DROP  RB,R3                                                            
         EJECT                                                                  
* SN400 - THIS SECTION UPDATES NEW INVOICE RECORDS *                            
         SPACE                                                                  
         DS    0H                                                               
         SPACE                                                                  
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
         SPACE                                                                  
SN426    MVC   SAVEKEYI(40),BKEY    SAVE THE KEY                                
         CLI   QOPT5,C'Y'                                                       
         BNE   SN430                                                            
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SITRACE          ***TRACE***                                  
         SPACE                                                                  
SN430    MVC   BKEY+5(3),NEWSTA    DOES NEW INVOICE ALREADY EXIST?              
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SN434                                                            
         MVC   BYTE,SAVEKEYI+7                                                  
         NI    BYTE,X'7F'                                                       
         OC    BKEY+7(1),BYTE      SET IN NETWORK                               
         SPACE                                                                  
SN434    MVC   BKEYSAVE,BKEY                                                    
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',BKEYSAVE,BKEY             
         SPACE                                                                  
         CLC   BKEY(32),BKEYSAVE   SAME 0E03/A-M/CLT/STA/DATE/INV/SEQ           
         BE    SN490                YES. DON'T MOVE                             
         SPACE                                                                  
         MVC   BKEYSAVE(40),SAVEKEYI    NO. RESTORE KEY                         
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'XSPDIR',BKEYSAVE,BKEY                     
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,GETREC,=C'XSPFILE',BKEY+36,AREC,DMWORK              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         OC    SAVDA,SAVDA         IS THIS 1ST INVOICE?                         
         BZ    SN435                YES.                                        
         ICM   RF,15,BKEY+36       GET THE DISK ADDR                            
         C     RF,SAVDA            SHOULD I CHANGE THIS RECORD?                 
         BNL   SN420                NO. THIS IS A NEW ONE                       
         SPACE                                                                  
SN435    CLI   RCWRITE,C'Y'                                                     
         BNE   SN440                                                            
         OI    BKEY+32,X'80'        MARK FOR DELETION                           
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'XSPDIR ',BKEY,BKEY                     
         TM    DM3,X'FD'                                                        
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    34(R8),X'80'        MARK RECORD FOR DELETION                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFILE',BKEY+36,AREC,DMWORK          
*                                                                               
         TM    DM3,X'FD'                                                        
         BZ    SN437                                                            
         DC    H'0'                                                             
SN437    NI    BKEY+32,X'7F'        TURN OFF DELETE BIT                         
         NI    34(R8),X'7F'        TURN OFF DELETE BIT                          
         SPACE                                                                  
SN440    MVC   DUB(1),7(R8)        SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),BKEY+7                                                  
         MVC   5(3,R8),NEWSTA      UPDATE CALL LETTERS                          
         MVC   BKEY+5(3),NEWSTA                                                 
         SPACE                                                                  
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SN444                                                            
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    7(1,R8),DUB        SET IN NETWORK                                
         OC    BKEY+7(1),DUB+1                                                  
         SPACE                                                                  
SN444    CLI   QOPT5,C'Y'                                                       
         BNE   SN450                                                            
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SITRACE          ***TRACE***                                  
         SPACE                                                                  
SN450    CLI   RCWRITE,C'Y'                                                     
         BNE   SN460                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,=C'XSPFILE',BKEY+36,AREC,DMWORK              
         CLI   DM3,0                                                            
         BE    SN460                                                            
         DC    H'0'                                                             
SN460    OC    SAVDA,SAVDA         IS THIS 1ST ADD?                             
         BZ    SN470                NO.                                         
         MVC   SAVDA,BKEY+36       SAVE DISK ADDR OF 1ST ADD                    
         SPACE                                                                  
* PRINT THE NEW INVOICE RECORD *                                                
         SPACE                                                                  
SN470    DS    0H                                                               
         CLC   =X'FFFFFFFFFFFF',BKEY+24                                         
         BNE   SN480                                                            
         SPACE                                                                  
         LH    R1,COUNT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,COUNT            # RECORDS CHANGED                            
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,BKEY+3,P+10                                          
         MVC   HALF,BKEY+8                                                      
         XC    HALF,=X'FFFF'                                                    
         GOTO1 DATCON,(R1),(2,HALF),(5,P+20)                                    
         MVC   P+38(10),BKEY+12      INVOICE NUMBER                             
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         SPACE                                                                  
SN480    MVC   BKEYSAVE(40),SAVEKEYI   RESTORE KEY                              
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'XSPDIR',BKEYSAVE,BKEY             
         CLC   BKEY(32),SAVEKEYI                                                
         BE    SN420                                                            
         DC    H'0'                                                             
         SPACE                                                                  
* PRINT ERROR FOR INVOICE ALREADY ON NEW STATION FOR THIS DATE *                
         SPACE                                                                  
SN490    GOTO1 CLUNPK,DMCB,BKEY+3,P+10                                          
         MVC   HALF,BKEY+8                                                      
         XC    HALF,=X'FFFF'                                                    
         GOTO1 DATCON,(R1),(2,HALF),(5,P+20)                                    
         MVC   P+38(10),BKEY+12      INVOICE NUMBER                             
         MVC   P+45(50),=CL40'* ERROR * INVOICE ALREADY ON NEW STATION'         
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         B     SN480                                                            
         SPACE                                                                  
SN500    LH    R0,COUNT                                                         
         EDIT  (R0),(5,P+4),ZERO=NOBLANK                                        
         MVC   P+10(23),=CL23'NEW INV RECORDS CHANGED'                          
         GOTO1 REPORT                                                           
         XIT1                                                                   
         SPACE 2                                                                
* THIS SECTION PRINTS A TRACE RECORD OF THE KEY *                               
         SPACE                                                                  
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
         DS   0D                                                                
*                                                                               
*                                                                               
**********************************                                              
* UPDATING AUTHORIZATION RECORDS *                                              
**********************************                                              
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
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    AUTRSTAT,X'80'        MARK RECORD FOR DELETION                   
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFILE',BKEY+36,AREC,DMWORK          
                                                                                
         TM    DM3,X'FD'                                                        
         BZ    SNA437                                                           
         DC    H'0'                                                             
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
         BE    *+6                                                              
         DC    H'0'                                                             
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
*                                                                               
***********************************************************************         
* SP6C00 - THIS SECTION UPDATES CALL LETTERS FOR DARE BATCH RECORDS             
SP6C0000 NTR1  BASE=*,LABEL=*                                                   
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
         BZ    SP6C15               - NO.                                       
         MVC   KEY+8(2),SAVCLT     SAME 0D35/AM/MKT/STA/CLT?                    
*                                                                               
SP6C15   GOTO1 HIGH                                                             
         B     SP6C22                                                           
*                                                                               
SP6C20   GOTO1 SEQ                                                              
*                                                                               
SP6C22   CLC   KEY(8),KEYSAVE      SAME 0D35/AM/MKT/STA?                        
         JNE   EXIT                                                             
*                                                                               
SP6C24   OC    SAVCLT,SAVCLT       IS THIS CLIENT SPECIFIC REQ?                 
         BZ    SP6C26               - NO.                                       
         CLC   KEY+8(2),SAVCLT                                                  
         BNE   SP6C20              GO TO THE NEXT RECORD                        
*                                                                               
SP6C26   MVC   SAVEKEY(13),KEY     SAVE THE KEY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   SP6C30                                                           
         MVI   TRACECDE,C'O'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP6C30   MVC   KEY+3(5),NEWMSTA                                                 
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP6C34                                                           
         MVC   BYTE,SAVEKEY+7                                                   
         NI    BYTE,X'7F'                                                       
         OC    KEY+7(1),BYTE       SET IN NETWORK                               
*                                                                               
SP6C34   GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     SAME 0D35/AM/MKT/STA/CLT?                    
         BE    SP6C90               YES. DON'T MOVE                             
         MVC   KEY(13),SAVEKEY      NO. RESTORE KEY                             
         GOTO1 HIGH                                                             
*                                                                               
         GOTO1 GET                                                              
         OC    SAVDA,SAVDA         IS THIS 1ST DARE BATCH?                      
         BZ    SP6C35               YES.                                        
         ICM   RF,15,KEY+14        GET THE DISK ADDR                            
         C     RF,SAVDA            SHOULD I CHANGE THIS RECORD?                 
         BNL   SP6C20               NO. THIS IS A NEW ONE                       
*                                                                               
SP6C35   CLI   RCWRITE,C'Y'                                                     
         BNE   SP6C40                                                           
         GOTO1 DATAMGR,DMCB,=C'DMDEL',SPTFILE,KEY,AREC                          
         TM    DM3,X'FD'                                                        
         BZ    SP6C40                                                           
         DC    H'0'                                                             
*                                                                               
SP6C40   MVC   DUB(1),7(R8)        SAVE POSSIBLE CABLE HEAD NETWORK             
         MVC   DUB+1(1),KEY+7                                                   
         MVC   3(5,R8),NEWMSTA     UPDATE CALL LETTERS                          
         MVC   KEY+3(5),NEWMSTA                                                 
         NI    15(R8),X'7F'        UNDELETE THE RECORD                          
*                                                                               
         CLI   QSTA,C'0'           THIS A CABLE HEAD                            
         BL    SP6C44                                                           
         NI    DUB,X'7F'                                                        
         NI    DUB+1,X'7F'                                                      
         CLC   DUB(1),DUB+1        BETTER BE SAME NETWORK                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    7(1,R8),DUB          SET IN NETWORK                              
         OC    KEY+7(1),DUB+1                                                   
*                                                                               
SP6C44   CLI   QOPT5,C'Y'                                                       
         BNE   SP6C50                                                           
         MVI   TRACECDE,C'I'                                                    
         BAS   RE,SPTRACE          ***TRACE***                                  
*                                                                               
SP6C50   CLI   RCWRITE,C'Y'                                                     
         BNE   SP6C60                                                           
         GOTO1 DATAMGR,DMCB,ADDREC,SPTFILE,KEY+14,AREC,DMWORK                   
         CLI   DM3,0                                                            
         BE    SP6C60                                                           
         DC    H'0'                                                             
SP6C60   OC    SAVDA,SAVDA         IS THIS 1ST ADD?                             
         BZ    SP6C70               NO.                                         
         MVC   SAVDA,KEY+14        SAVE DISK ADDR OF 1ST ADD                    
*                                                                               
* PRINT THE NEW DARE BATCH RECORD *                                             
         SPACE                                                                  
SP6C70   DS    0H                                                               
         LH    R1,COUNT                                                         
         LA    R1,1(,R1)                                                        
         STH   R1,COUNT            # RECORDS CHANGED                            
*                                                                               
         GOTO1 CLUNPK,DMCB,KEY+8,P+10                                           
*        GOTO1 DATCON,(R1),(2,KEY+7),(5,P+20)                                   
*        GOTO1 HEXOUT,(R1),KEY+9,P+38,1,=C'MIX'                                 
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
SP6C80   MVC   KEY(13),SAVEKEY     RESTORE KEY                                  
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),SPTDIR,KEY,KEY                       
         CLC   KEY(13),SAVEKEY                                                  
         BE    SP6C20                                                           
         DC    H'0'                                                             
*                                                                               
* PRINT ERROR FOR DARE BATCH ALREADY ON NEW STATION (CLIENT) *                  
*                                                                               
SP6C90   GOTO1 MSUNPK,DMCB,KEY+8,P+10,P+20                                      
         GOTO1 CLUNPK,DMCB,KEY+8,P+38                                           
*        GOTO1 DATCON,(R1),(2,KEY+7),(5,P+20)                                   
*        GOTO1 HEXOUT,(R1),KEY+9,P+38,1,=C'MIX'                                 
         MVC   P+45(42),=CL40'* ERROR * DARE BATCH RECORD ALREADY EXIST+        
               S'                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVC   KEYSAVE,SAVEKEY                                                  
         B     SP6C80                                                           
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
*                                                                               
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
         DROP  R2                                                               
*                                                                               
*                                                                               
         DC    C'**BKEY**'                                                      
BKEY     DC    XL40'00'                                                         
BKEYSAVE DC    XL40'00'                                                         
SAVEKEYI DC    XL40'00'                                                         
         DROP  RB,R3                                                            
         SPACE                                                                  
         DS    0D                                                               
SPBUFF   DS    8000C                                                            
*        PRINT OFF                                                              
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
       ++INCLUDE SPGENXLK                                                       
         EJECT                                                                  
       ++INCLUDE SPGENNDEF                                                      
*        EJECT                                                                  
*      ++INCLUDE CTGENRAD                                                       
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
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073SPREPSC02S05/13/04'                                      
         END                                                                    
