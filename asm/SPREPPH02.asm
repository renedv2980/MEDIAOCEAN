*          DATA SET SPREPPH02  AT LEVEL 014 AS OF 02/09/15                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 045021.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE SPPH02A                                                                  
*INCLUDE GETUSER                                                                
*INCLUDE DDUCOM                                                                 
*INCLUDE NETACC                                                                 
*INCLUDE NETNET                                                                 
         TITLE 'SPPH02 - PHILIP MORRIS INTERFACE'                               
*                                                                               
***************************************************************                 
***************************************************************                 
*        DO NOT RUN A MIXTURE OF ESTIMATE AND INVOICE FILE                      
*        TAPE REQUESTS IN THE SAME JOB STREAM - IT WON'T WORK                   
***************************************************************                 
***************************************************************                 
*        CHANGE LOG                                                             
*                                                                               
*    BPLA   06/06   RELINK WITH NEW NETNET                                      
*                                                                               
*    BPLA   04/06   SET GLOBAL ERROR MESSAGE AND CHECK IT                       
*                   AT RUNLAST                                                  
*                                                                               
*    BPLA   04/06   VCLIST CHECKING CHANGED                                     
*                                                                               
*    BPLA  6/14/05  SKIP ESTIMATE UCOMM CHECK AT FEST                           
*                   IT WILL BE CAUGHT LATER                                     
*                                                                               
*                                                                               
*                                                                               
*        QOPT1 I=INVOICE FILE ONLY, E=ESTIMATE FILE ONLY,                       
*              B=BOTH (FOR TESTING?)                                            
*        QOPT5 Y=LIST 'CURRENT' INVOICES                                        
*        QOPT6 Y= TEST RUN - NO TAPE, AND CONTINUE IF ERRORS                    
*                            ARE FOUND                                          
*        QOPT7 P=PDUMP RECORDS                                                  
*                                                                               
*        QSTART(6) = PERIOR START                                               
*        QEND(6) = PERIOD END - MAY BE BLANK                                    
*                                                                               
*                                                                               
SPPH02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPPH02,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    R8,SPACEND                                                       
         USING SPPHWRKD,R8                                                      
         LA    R7,SPPH02+4095                                                   
         LA    R7,1(R7)                                                         
         USING SPPH02+4096,R7     **NOTE USE OF R7 AS BASE REGS*                
                                                                                
         CLI   MODE,RUNFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,REQFRST                                                     
         BE    FIRSTB                                                           
         CLI   MODE,CLTFRST                                                     
         BE    FCLI                                                             
         CLI   MODE,CLTLAST                                                     
         BE    LCLI                                                             
         CLI   MODE,PRDFRST                                                     
         BE    FPRD                                                             
         CLI   MODE,PROCBUY                                                     
         BE    PROCESS                                                          
         CLI   MODE,ESTFRST                                                     
         BE    FEST                                                             
******   CLI   MODE,REQLAST                                                     
******   BE    PUTBUFF       PUTBUFF NOW DONE AT LAST FOR CLIENT                
         CLI   MODE,RUNLAST                                                     
         BE    TOTALS                                                           
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                         RUN FIRST                                             
INITIAL  DS    0H                                                               
         L     RF,=A(PROCNET)                                                   
         A     RF,RELO                                                          
         ST    RF,VPROCNET                                                      
*                                                                               
         L     RF,=A(NETBLK)                                                    
         A     RF,RELO                                                          
         ST    RF,ANETBLK                                                       
*                                                                               
         L     RF,=V(NETNET)                                                    
         A     RF,RELO                                                          
         ST    RF,ANETNET                                                       
*                                                                               
         L     RF,=V(NETACC)                                                    
         A     RF,RELO                                                          
         ST    RF,ANETACC                                                       
*                                                                               
         L     R0,=V(GETUSER)                                                   
         A     R0,RELO                                                          
         ST    R0,VGETUSER                                                      
*                                                                               
         L     R0,=V(DDUCOM)                                                    
         A     R0,RELO                                                          
         ST    R0,VDDUCOM                                                       
*                                                                               
         L     RF,ADCONLST                                                      
         USING SPADCONS,RF                                                      
         MVC   AFMTINO,VSPFMINO   A(SPFMTINO)                                   
         DROP  RF                                                               
*                                                                               
         L     R0,=A(DISPTAB)                                                   
         A     R0,RELO                                                          
         ST    R0,ADISPTAB                                                      
*                                                                               
         XC    MYDUMP,MYDUMP                                                    
         MVI   RERRSW,C'N'                                                      
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY1)       YYMMDD                        
         GOTO1 DATCON,DMCB,(5,0),(X'14',TODAYY)   YYYYMMDD                      
         ZAP   TOTCNT,=P'0'                                                     
         MVI   EHDRSW,C'N'                                                      
         MVI   IHDRSW,C'N'                                                      
         MVI   FRSTBILL,C'N'                                                    
         MVI   LASTBILL,C'N'                                                    
         ZAP   INVTOTD,=P'0'                                                    
         ZAP   INVRCNT,=P'0'                                                    
         ZAP   ESTRCNT,=P'0'                                                    
*                                                                               
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
*                                                                               
         MVI   DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
         MVI   TAPESW,0                                                         
         MVI   ALLOWSW,0           DYNAMIC ALLOCATION NOT DONE                  
*                                                                               
INIT5    MVI   NETOPT,C'N'         SET FOR NEW NETPAK                           
*                                                                               
         L     R1,VMASTC                                                        
         USING MASTD,R1                                                         
         CLI   MCNETPAK,C'Y'       SEE IF NETPAK                                
         BE    INIT7                                                            
*                                                                               
         DROP  R1                                                               
*                                                                               
         MVI   NETOPT,0                                                         
INIT7    DS    0H                                                               
         L     R0,=A(TITLES)                                                    
         CLI   NETOPT,C'N'                                                      
         BNE   *+8                                                              
         L     R0,=A(NTITLES)                                                   
*                                                                               
         A     R0,RELO                                                          
         ST    R0,ATITLES                                                       
*                                                                               
         L     R0,=A(LENTAB)                                                    
         A     R0,RELO                                                          
         ST    R0,ALENTAB                                                       
*                                                                               
         L     R2,ALENTAB          ZAP ACCUMS                                   
         LA    R3,9                                                             
INIT7L   ZAP   4(4,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R3,INIT7L                                                        
*                                                                               
*                                                                               
INIT8    MVI   FCRDBUYS,C'Y'       RESET TO READ BUYS                           
         CLI   NETOPT,C'N'         CAN ONLY BE FOR NEW NETPAK                   
         BNE   INIT30                                                           
         MVI   FCRDBUYS,C'N'       SET FOR NO BUYS                              
*                                 (I'LL BE READING NETPAK UNITS)                
INIT30   CLI   NETOPT,C'N'         SEE IF DOING NEW NETWORK                     
         BNE   INIT70                                                           
         BC    0,INIT70                                                         
         OI    *-3,X'F0'           ONLY DO ONCE                                 
         L     R4,ADBUY                                                         
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',NUFLIST,(R4)                      
         B     INIT70                                                           
*                                                                               
NUFLIST  DC    CL8'NUNTFIL'                                                     
         DC    CL8'NUNTDIR'                                                     
         DC    CL10'X'                                                          
*                                  REQS FOR ONE PRD                             
INIT70   DS    0H                                                               
**                                                                              
INITX    B     EXIT                                                             
         EJECT                                                                  
*                       REQUEST FIRST                                           
FIRSTB   DS    0H                                                               
*                                                                               
         MVC   DYNDDN,=CL8'SPHTAPE'                                             
         MVC   DYNDSN,=CL20'NETTAPE.NE0PHXX1'                                   
         MVC   DYNDSN+13(2),QAGY                                                
*                                                                               
         CLI   NETOPT,C'N'      NETPAK?                                         
         BE    REQF10                                                           
         MVC   DYNDSN(3),=C'SPT'    ALTER FOR SPOT                              
         MVC   DYNDSN+8(2),=C'SP'                                               
*                                                                               
REQF10   DS    0H                                                               
         MVC   SVQOPT1,QOPT1      SAVE FILE TYPE                                
         MVC   SVQOPT6,QOPT6      SAVE DO TAPE OPTION                           
         MVC   SVQOPT7,QOPT7      SAVE PDUMPING OPTION                          
         MVI   FCRDBUYS,C'Y'                                                    
         CLI   QOPT1,C'I'     INVOICE FILE ONLY                                 
         BNE   *+8                                                              
         MVI   FCRDBUYS,C'N'   DON'T READ BUYS                                  
*                                                                               
         TIME  DEC                                                              
*                                                                               
*   DDS COMPUTER 'CLOCK' STARTS AT 6:00 AM = HOUR 00                            
*   WE MAY NEED TO ADD 6 TO THE HOURS TO GET CORRECT MILITARY TIME.             
*                                                                               
         LR    R1,R0       TIME IN HUNDREDTHS IN R1                             
         SRL   R1,8        TO REMOVE LAST 2 DIGITS.                             
         SLL   R1,4                                                             
         AH    R1,=H'12'    SET LAST HALF BYTE TO C                             
         ST    R1,FULL                                                          
         ZAP   DUB,=P'0'                                                        
         MVC   DUB+4(4),FULL                                                    
         OI    DUB+7,X'0F'                                                      
         UNPK  NOW(6),DUB+4(4)    NOW SHOULD BE HHMMSS                          
*                                                                               
         LA    RE,VENTAB           VENDOR TABLE                                 
REQF1C   CLI   0(RE),X'FF'         AGENCY NOT IN MY TABLE                       
         BNE   *+6                                                              
         DC    H'0'            DIE FOR NOW                                      
         CLC   QAGY,0(RE)                                                       
         BE    REQF1D                                                           
         LA    RE,15(RE)                                                        
         B     REQF1C                                                           
*                                                                               
REQF1D   MVC   VENDOR,2(RE)      VENDOR CODE                                    
         MVC   PAYEE,8(RE)       PAYEE CODE                                     
*                                                                               
         CLC   QEST(2),=C'NO'                                                   
         BE    REQF65                                                           
REQF50   CLC   QEST(6),SPACES                                                   
         BNE   REQF55                                                           
         MVC   QEST(6),=C'001255'                                               
         B     REQF60                                                           
REQF55   CLC   QESTEND,SPACES                                                   
         BNE   REQF60                                                           
         MVC   QESTEND,QEST      IF ONLY ONE EST SET QESTEND TO IT              
REQF60   PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STC   R0,MYBEST                                                        
         PACK  DUB,QESTEND                                                      
         CVB   R0,DUB                                                           
         STC   R0,MYBESTE          NEED TO SET NOW FOR TBCLTF                   
         B     REQF70                                                           
*                                                                               
REQF65   MVI   MYBEST,1                                                         
         MVI   MYBESTE,255                                                      
REQF70   DS    0H                                                               
*                                  SPONSOR DOES IT LATER FOR                    
         MVI   RQPRDAAA,C'Y'                                                    
         MVI   RQRDPOL,C'N'                                                     
*                                                                               
         CLI   QOPT6,C'Y'        SEE IF TEST REQUEST                            
         BE    FIRSTB0                                                          
         CLI   TAPESW,C'N'       SEE IF A PRIOR REQUEST WAS TEST                
         BE    MIXERR                                                           
         MVI   TAPESW,C'Y'       SET TAPE BEING PRODUCED                        
*                                                                               
         TM    ALLOWSW,X'01'     DYNAMIC ALLOCATION DONE ALREADY?               
         BO    REQF75                                                           
*                                                                               
         GOTO1 DYNALLOC,DMCB,(0,DYNDDN),(0,DYNDSN)                              
         OI    ALLOWSW,X'01'    SO I WON'T DO AGAIN                             
         OPEN  (SPHTAPE,OUTPUT)                                                 
*                                                                               
REQF75   B     FIRSTB0X                                                         
*                                                                               
FIRSTB0  CLI   TAPESW,C'Y'      SEE IF A PRIOR REQUEST WAS LIVE                 
         BE    MIXERR                                                           
         MVI   TAPESW,C'N'                                                      
         B     FIRSTB0X                                                         
*                                                                               
MIXERR   MVC   P1(37),=C'*** MIX OF TEST AND LIVE REQUESTS ***'                 
         MVC   P2(37),=C'*** THIS REQUEST HAS BEEN SKIPPED ***'                 
         GOTO1 REPORT                                                           
         MVI   MODE,REQLAST    SKIP TO NEXT REQUEST                             
         B     EXIT                                                             
*                                                                               
FIRSTB0X DS    0H                                                               
*                             SET MYUSER FROM AGYTAB                            
         LA    R1,AGYTAB                                                        
FIRSTB1  CLI   0(R1),X'FF'            END OF TABLE                              
         BNE   *+6                                                              
         DC    H'0'                   INVALID AGENCY                            
*                                                                               
         CLC   0(2,R1),QAGY                                                     
         BE    FIRSTB1D                                                         
         LA    R1,AGYTABL(R1)                                                   
         B     FIRSTB1                                                          
*                                                                               
FIRSTB1D MVC   MYUSER,2(R1)                                                     
*                                                                               
FIRSTB2  DS    0H                                                               
         XC    ESTU1,ESTU1          CLEAR USER FIELDS                           
         XC    ESTU2,ESTU2                                                      
         XC    PRDU1,PRDU1                                                      
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         ZAP   CINVGRS,=P'0'                                                    
         ZAP   CINVBIL,=P'0'                                                    
         ZAP   CINVCD,=P'0'                                                     
         ZAP   CINVRCV,=P'0'                                                    
         MVI   CINVSW,0                                                         
         XC    ESTLLST(250),ESTLLST    CLEAR LIST OF UNLOCKED ESTS              
         XC    ESTLLST+250(250),ESTLLST+250                                     
*                                                                               
*                                  SAVE DATES FOR ACTIVITY CHECKING             
         GOTO1 DATCON,DMCB,(0,QSTART),(3,SVQSTART)                              
         MVC   SVQEND,=3X'FF'                                                   
*                                                                               
         CLI   NETOPT,C'N'             IS THIS NETPAK?                          
         BE    FIRSTB2X                YES - SKIP MEDBLOCK STUFF                
*                                                                               
*        ESTABLISH MEDBLOCK                                                     
*                                                                               
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         XC    MEDNUMWK(8),MEDNUMWK                                             
         MVI   MEDNUMMO+3,13       13 MONTHS?                                   
         XC    MEDNUMQT,MEDNUMQT                                                
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDLCHNK,=F'320'    **ENOUGH FOR 2ND CURRENCY???                 
         MVI   MEDEXTDM,0                                                       
         MVI   MEDEXTAC,C'Y'                                                    
*                                                                               
         GOTO1 MEDPRDRD,DMCB,SPWORKD                                            
*                                                                               
         DROP  RF                                                               
*                                                                               
FIRSTB2X CLC   QEND,SPACES                                                      
         BE    FIRSTB3                                                          
         GOTO1 DATCON,DMCB,(0,QEND),(3,SVQEND)                                  
FIRSTB3  MVC   SVSTART(12),QSTART    SAVE EBCDIC DATES FOR BILL CHK             
         CLC   QEND,SPACES                                                      
         BNE   FIRSTB3C                                                         
*NOP*    MVC   SVEND(6),=C'999999'                                              
         MVC   SVEND(6),=6X'FF'    FOR 21ST CENTURY                             
*                                                                               
FIRSTB3C MVC   QSTART(12),SPACES      PAST ALL DATA                             
         MVC   BQSTART,=X'000000'                                               
         MVC   BQEND,=X'FFFFFF'                                                 
         MVC   BQSTARTP,=X'0000'                                                
         MVC   BQENDP,=X'FFFF'                                                  
*                                                                               
         LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         B     EXIT                                                             
*                                                                               
FIRSTB4  DS    0H                                                               
FIRSTBX  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
FCLI     DS    0H                  FIRST BUY FOR CLIENT                         
*                                                                               
         XC    BADESTS(240),BADESTS   CLEAR BAD ESTIMATE TABLE                  
*                                                                               
         MVI   BADESTS,X'FF'        SET NEW END                                 
*                                                                               
         XC    B1PROF,B1PROF        FIRST READ B1 AND B1X PROFILES              
         XC    B1XPROF,B1XPROF      B1X PROFILE                                 
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SOB1'                                                 
         MVC   WORK+4(2),AGY                                                    
         MVC   WORK+6(1),MED                                                    
         MVC   WORK+7(3),CLT                                                    
         L     RF,ADCLT                                                         
         USING CLTHDR,RF                                                        
*     NOW ALWAYS PASS OFFICE DATA                                               
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  RF                                                               
*                                                                               
FBC1     DS    0H                                                               
         GOTO1 GETPROF,DMCB,WORK,B1PROF,DATAMGR                                 
         MVC   WORK(4),=C'SB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
         EJECT                                                                  
         CLI   QOPT1,C'E'         ESTIMATE FILE ONLY?                           
         BE    EXIT               SKIP BILL READING                             
*                                                                               
*        NOW READ BILLS                                                         
*                                                                               
*                                                                               
         XC    LINVFULL,LINVFULL      CLEAR FOR NEW REQ                         
         XC    LBQDATE,LBQDATE                                                  
         MVI   FRSTBILL,C'Y'                                                    
         MVI   LASTBILL,C'N'                                                    
         ZAP   INVTOTD,=P'0'     CLEAR TOTAL AMOUNT DUE                         
         XC    INVTAB,INVTAB     CLEAR INVOICE DETAIL TABLE                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         CLC   QPRD,=C'ALL'                                                     
         BE    TBCF42                                                           
         MVC   KEY+4(3),QPRD                                                    
TBCF42   GOTO1 HIGH                                                             
         B     TBCF46                                                           
TBCF45   GOTO1 SEQ                                                              
TBCF46   CLC   KEY(4),KEYSAVE                                                   
         BNE   TBCF60                                                           
         CLC   QPRD,=C'ALL'                                                     
         BE    TBCF47                                                           
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
         MVC   PRD,BKEYPRD      SET PRD FROM BILL HEADER                        
*                                                                               
         CLC   QAGY,=C'H9'      ONLY FOR STARCOM                                
         BNE   TBCF47B                                                          
         CP    BACTP,=P'0'      ZERO BILL?                                      
         BE    TBCF45           THEN SKIP                                       
*                                                                               
TBCF47B  TM    BILSTAT,X'20'    SKIP AOR BILLS                                  
         BO    TBCF45                                                           
         CLI   BRETAIL,X'41'    SKIP RETAIL CORP SUMMARY BILLS                  
         BE    TBCF45                                                           
*                              MUST CLEAR BVATAMT FOR SMALL BILLS               
         CLC   BLEN,=H'90'                                                      
         BH    *+10                                                             
         XC    BVATAMT,BVATAMT                                                  
*                                                                               
         CLC   BDATE,SVSTART                                                    
         BL    TBCF45              SKIP IF BEFORE                               
         CLC   BDATE,SVEND                                                      
         BH    TBCF45              SKIP IF AFTER                                
         GOTO1 AFMTINO,DMCB,BDATE,(4,BINVNO+2),(MED,B1PROF),B1XPROF             
         L     RF,DMCB                                                          
         MVC   DINVFULL,0(RF)      FULL INVOICE NUMBER                          
*                                                                               
         MVI   CKESTREC,C'L'       SET FROM BILL                                
         GOTO1 =A(CKEST)           READ EST AND COMMENT                         
*                                                                               
         MVC   WORK(1),BKEYYSRV                                                 
         MVC   WORK+1(1),BKEYMSRV                                               
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(3,WORK),(X'14',WORK+6)                              
*                                                                               
         CLC   UCOMDATA(4),WORK+6        YEARS MUST MATCH                       
         BE    BILLA10                                                          
*                                                                               
         MVC   P1+2(72),=C'** ERROR - MONTH MM/YYYY NOT IN VALIDITY YEAX        
               R YYYY  - PRD=XXX EST=XXX **'                                    
*                                                                               
         MVC   P1+19(2),WORK+10                                                 
         MVC   P1+22(4),WORK+6                                                  
         MVC   P1+48(4),UCOMDATA                                                
         MVC   P1+60(3),BKEYPRD                                                 
         ZIC   R0,BKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+68(3),DUB                                                     
*                                                                               
         MVI   RCSUBPRG,10                                                      
         BAS   RE,MYRPT                                                         
         MVI   RERRSW,C'Y'                                                      
*                                                                               
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    BILLA10               CONTINUE - ELSE DIE                        
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
BILLA10  DS    0H                                                               
*                                                                               
         CLI   MYUSER,C'E'         SEE IF USING ESTIMATE USER FIELDS            
         BNE   TBCF50                                                           
*                                                                               
         CLI   ESTU1+25,C'X'          MEANS EXCLUDE                             
         BE    TBCF45                 SKIP THIS BILL                            
*                                                                               
TBCF50   GOTO1 =A(INVOICE)                                                      
         B     TBCF45               READ ANOTHER                                
*                                                                               
*                                                                               
TBCF60   DS    0H                                                               
         MVI   LASTBILL,C'Y'                                                    
         GOTO1 =A(INVOICE)                                                      
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                  LAST FOR CLIENT                              
LCLI     DS    0H                                                               
         CLI   NETOPT,C'N'        NETPAK?                                       
         BNE   LCLI10             NO - DO NOTHING                               
         CLI   QOPT1,C'I'         INVOICES ONLY?                                
         BE    LCLI10                                                           
*                      UNIT PROCESSING NOW IN IT'S OWN CSECT                    
         GOTO1 VPROCNET,DMCB                                                    
LCLI10   B     PUTBUFF                                                          
         EJECT                                                                  
*                                                                               
FPRD     DS    0H                  FIRST BUY FOR PRODUCT                        
         XC    PRDU1,PRDU1                                                      
         CLI   MYUSER,C'Y'        SEE IF USING USER FIELDS                      
         BNE   FPRDX                                                            
         CLC   PKEYPRD,=C'AAA'    NOT FOR PRD=AAA                               
         BE    FPRDX                                                            
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'S',ADCLT),(C'P',ADPRD),PRDU1,0                  
         CLI   DMCB,X'FF'                                                       
         BE    FPRDERR                                                          
         CLI   PRDU1+21,C' '    MUST FIND DATA                                  
         BNH   FPRDERR                                                          
         B     FPRDX                                                            
*                                                                               
FPRDERR  DS    0H                                                               
         MVC   P1,SPACES                                                        
         MVC   P1(36),=C'*** MISSING PRODUCT USER FIELD 1 ***'                  
         MVC   P1+40(3),PKEYPRD                                                 
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVI   RERRSW,C'Y'                                                      
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    FPRDX                 CONTINUE - ELSE DIE                        
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
FPRDX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
FEST     DS    0H                  ESTIMATE FIRST                               
*                                                                               
*        FIRST TRY TO FIND UNDER BRAND EST                                      
*                                                                               
*        GET ESTIMATE UCOMS FOR PRODUCT POL                                     
*        POL ESTIMATE READ INTO ADCOMREC (BIG ENOUGH FOR EST?)                  
*                                                                               
         XC    UCOMDATA,UCOMDATA   CLEAR UCOMM DATA                             
         L     RF,ADEST                                                         
         MVC   KEY,0(RF)                                                        
         L     RE,ADCOMREC                                                      
         ST    RE,AREC                                                          
         MVC   KEY+EKEYPRD-ESTHDR(3),=C'POL'                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   FESTMISS                                                         
         GOTO1 GET                                                              
         CLI   DMCB+8,0                                                         
         BNE   FESTMISS                                                         
*                                                                               
*        CALL DDUCOM TO GET ESTIMATE'S FIRST 4 UCOMS                            
*                                                                               
         MVC   USAVKEY,KEY   SAVE MY KEY                                        
         LA    R5,UCOMBLK     SET-UP UCOM CONTROL BLOCK                         
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R5                                                       
         MVC   UCPRD,PRD          FIRST TRY FOR BRAND                           
*                                                                               
FEST1    MVC   UCACOMF,ACOMFACS     COMFACS                                     
         MVI   UCSYS,C'N'        SYSTEM TO NETPAK                               
         CLI   NETOPT,C'N'       NETPAK?                                        
         BE    *+8                                                              
         MVI   UCSYS,C'S'        SYSTEM TO PRINT (SPOT)                         
         MVC   UCSAM,BAGYMD      AGENCY/MEDIA                                   
         MVC   UCSCLT,BCLT       PACKED CLIENT                                  
*                                DO UCOMMS FOR PRD POL                          
         OI    UCOPT,UCOEST     RETURN ESTIMATE UCOMMS                          
         L     R2,ADEST          BRAND ESTIMATE                                 
         USING ESTHDR,R2                                                        
         MVC   UCSEST,EKEYEST                                                   
*                                                                               
         GOTO1 VDDUCOM,UCOMBLK    NEW UCOM CALL SINCE GOTO MACRO                
         CLI   UCERROR,0         TRASHED WRKING STORAGE USED BY DDUCOM          
         BNE   FEST3X       ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCDATA,UCDNOEST                                                  
         BNO   FEST2                                                            
         CLC   UCPRD,=C'POL'     DID I JUST TRY FOR POL?                        
         BE    FEST2X                                                           
         MVC   UCPRD,=C'POL'     TRY FOR POL UCOMMS                             
         B     FEST1                                                            
*                                                                               
FEST2    XC    UCTTLS(UCALL),UCTTLS                                             
         L     R4,UCETTLS     EST TITLES                                        
         MVC   UCTTLS,0(R4)   SAVE INFO IN MY STORAGE                           
         LA    R4,UCTTLS      AS OPPOSED TO RD CHANE                            
         L     RE,UCEDATA     EST DATA                                          
         MVC   UCOMDATA,0(RE)                                                   
*                                                                               
FEST2X   CLI   QOPT1,C'E'     ESTIMATE FILE ONLY                                
         BE    FEST3X         ANY ERRORS WILL BE CAUGHT LATER                   
*                                                                               
         B     FEST3X         ALSO DO FOR INVOICE FILE RUNS                     
*                             AS ERRORS WILL ALSO BE CAUGHT LATER               
*                            -THIS SKIPPING PREVENTS THESE ERRORS               
*                             FROM STOPPING RUNS WHEN THE ERRORS                
*                             OCCURR ON UNBILLED ESTIMATES                      
*                                                                               
         OC    UCOMDATA(4),UCOMDATA   1ST IS VALIDITY YEAR                      
         BNZ   FEST3X                                                           
         MVC   P1+2(29),=C'** ERROR - PRD XXX EST NNN - '                       
         MVC   P1+17(3),PRD                                                     
         ZIC   R0,EKEYEST      (FROM BRAND ESTIMATE)                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+25(3),DUB+6(2)                                                
         MVC   P1+31(24),=C'MISSING VALIDITY YEAR **'                           
         GOTO1 REPORT                                                           
         MVI   RERRSW,C'Y'                                                      
         CLI   QOPT6,C'Y'      TEST RUN?                                        
         BE    FEST3X          KEEP GOING                                       
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
FEST3X   DS    0H                                                               
         MVC   KEY,USAVKEY      RESTORE SEQ READ                                
         GOTO1 HIGH                                                             
         B     EXIT                                                             
         DROP  R5                                                               
*                                                                               
FESTX    DS    0H                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
FESTMISS DS    0H                  POL ESTIMATE MISSING                         
         MVC   P1+2(29),=C'** ERROR - POL EST NNN - MISSING **'                 
         L     R2,ADEST      TO BRAND ESTIMATE                                  
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+21(3),DUB+6(2)                                                
         GOTO1 REPORT                                                           
         MVI   RERRSW,C'Y'                                                      
         CLI   QOPT6,C'Y'      TEST RUN?                                        
         BE    FEST3X          KEEP GOING                                       
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
*                                                                               
PROCESS  DS    0H                  PROCESS BUYRECS                              
*****                                                                           
         MVI   CKESTREC,C'B'       SET FROM BUY                                 
         GOTO1 =A(CKEST)           GO READ EST AND USER FIELDS                  
*                                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
*                                   SAVE REQUEST VALUES                         
         MVC   SVQST,QSTART                                                     
         MVC   SVQED,QEND                                                       
         MVC   SVQSTP,BQSTARTP                                                  
         MVC   SVQEDP,BQENDP                                                    
         MVC   SVQSTB,BQSTART                                                   
         MVC   SVQEDB,BQEND                                                     
*                                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
*                                   SAVE REQUEST VALUES                         
         MVC   QSTART,ESTART       USE ESTIMATE'S START AND END                 
         MVC   QEND,EEND                                                        
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 DATCON,DMCB,QSTART,(2,BQSTARTP)                                  
         GOTO1 DATCON,DMCB,QEND,(2,BQENDP)                                      
*                                                                               
         GOTO1 MEDDATE,DMCB,SPWORKD                                             
*                                                                               
         DS    0H                                                               
         MVC   QSTART,SVQST       RESTORE REQUEST'S DATE VALUES                 
         MVC   QEND,SVQED                                                       
         MVC   BQSTARTP,SVQSTP                                                  
         MVC   BQENDP,SVQEDP                                                    
         MVC   BQSTART,SVQSTB                                                   
         MVC   BQEND,SVQEDB                                                     
         CLI   MYUSER,C'E'         SEE IF USING ESTIMATE USER FIELDS            
         BNE   PB205                                                            
*                                                                               
         CLI   ESTU1+25,C'X'          MEANS EXCLUDE                             
         BE    EXIT                                                             
*                                                                               
PB205    XC    BUFREC,BUFREC                                                    
         MVI   BUFTYPE,C'E'                                                     
         MVC   BUFMED,QMED                                                      
*                                                                               
****                                                                            
****     DESRIPTION FROM USER FIELD  - NO-OPED                                  
****                                                                            
         B     PB205U                                                           
*                                                                               
**       CLI   MYUSER,C'E'           SEE IF USING USER FIELDS                   
**       BNE   PB205U                                                           
**       MVC   BUFYR(4),ESTU1+21     1ST FOR CHARS OF UDEF1                     
**       MVC   BUFCOM,SPACES                                                    
**       MVC   BUFCOM(3),ESTU1+22    2ND-4TH CHRAS OF EST UDEF1                 
**                                                                              
**       L     RF,ADEST                                                         
**       USING ESTHDR,RF                                                        
**       MVC   BUFCOM+20(12),ESTART      ALSO SAVE DATES                        
**                                                                              
**       B     PB205E                                                           
**                                                                              
*                                                                               
PB205U   DS    0H                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         MVC   BUFCOM(20),EDESC                                                 
*                                                                               
******************************************************************              
*        THE 3 FIELDS BELOW WILL BE SET FROM UCOM                               
******************************************************************              
         MVC   BUFCOME1,UCOMDATA    VALIDITY YEAR                               
         MVC   BUFCOME2,UCOMDATA+32  GL ACCOUNT                                 
         MVC   BUFCOME3,UCOMDATA+64  INTERNAL ORDER                             
         MVC   BUFCOME4,UCOMDATA+96  COST CENTER                                
*                                                                               
         MVC   BUFYR,UCOMDATA+3    LAST DIGIT OF VALIDITY YEAR                  
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUFEST(3),DUB                                                    
         MVC   BUFCOMEN,EDESC     ESTIMATE NAME                                 
*                                                                               
*                                                                               
PB205E   DS    0H                                                               
         MVC   BUFPRD,EKEYPRD    USE THE PRODUCT I'M PROCESSING                 
*                                                                               
******   CLI   MYUSER,C'Y'                                                      
******   BNE   *+10                                                             
******   MVC   BUFPRD,PRDU1+21                                                  
*                                                                               
         DROP  RF                                                               
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         MVC   MEDBRAND,BPRD                                                    
         MVI   MEDSPTLN,0                                                       
         GOTO1 MEDGETBY,DMCB,SPWORKD,0                                          
         L     RF,MEDBUFF                                                       
         L     R4,MEDAFRST                                                      
*                                                                               
         DROP  RF                                                               
*                                                                               
PB206    DS    0H           PROCESS CHUNK                                       
*                                                                               
*        PROCESS BY END DATE AS IT WILL ALWAYS BE                               
*        IN THE 'RIGHT' BROADCAST MONTH                                         
*                                                                               
         GOTO1 DATCON,DMCB,(2,2(R4)),(X'14',WRKDATE)                            
*                                                                               
         L     RF,4(R4)                                                         
         USING MEDDATA,RF                                                       
         L     R0,MEDBYGRS                                                      
         CVD   R0,DUB                                                           
         L     R0,MEDBYNET                                                      
         CVD   R0,NETDUB                                                        
*                                                                               
         DROP  RF                                                               
*                                                                               
PB207    DS    0H                                                               
         XC    BUFSTA,BUFSTA     MUST RECLEAR FOR ESTIMATE $                    
         MVC   BUFMTH,WRKDATE    YYYYMM                                         
*                                                                               
         CP    DUB,=P'0'                                                        
         BNE   PB208                                                            
         CP    NETDUB,=P'0'                                                     
         BE    PB300                SKIP IF NO $                                
*                                                                               
PB208    ZAP   BUFCD,=P'0'           NONE FOR SPOT/NET                          
         ZAP   BUFAMTD,DUB           AMOUNT DUE (USE GROSS FOR NOW)             
         ZAP   BUFCOMM,DUB           COMMISSION= AMT. DUE - NET                 
         SP    BUFCOMM,NETDUB                                                   
*                                                                               
*                                                                               
PB210    GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
         MVC   BUFSTA(L'STA),STA    FOR STATION/NETWORK SPEND LINES             
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
         B     PB300                                                            
         EJECT                                                                  
*                                                                               
PB300    DS    0H                                                               
         L     RF,MEDBUFF                                                       
         USING MEDBLOCK,RF                                                      
         LA    R4,L'MEDDATES(R4)                                                
         C     R4,MEDALAST                                                      
         BNH   PB206        PROCESS NEXT CHUNK                                  
         B     EXIT         DONE WITH BUY'S DATA                                
*                                                                               
         DROP  RF                                                               
         EJECT                                                                  
PUTBUFF  DS    0H      FIRST PRINT TOTAL LINE FOR CURRENT INVS                  
         TM    CINVSW,1                                                         
         BZ    PUTB2                                                            
         MVI   RCSUBPRG,10                                                      
         BAS   RE,MYRPT                                                         
         MVC   P1+28(7),=C'*TOTAL*'                                             
         EDIT  (P8,CINVGRS),(14,P1+37),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,CINVBIL),(14,P1+53),2,COMMAS=YES,FLOAT=-                     
         EDIT  (P8,CINVCD),(14,P1+69),2,COMMAS=YES,FLOAT=-                      
         EDIT  (P8,CINVRCV),(14,P1+85),2,COMMAS=YES,FLOAT=-                     
         MVI   P1+51,C'*'                                                       
         MVI   P1+67,C'*'                                                       
         MVI   P1+83,C'*'                                                       
         MVI   P1+99,C'*'                                                       
         BAS   RE,MYRPT                                                         
PUTB2    MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,20                                                      
*                                  PUT BUFFALO RECS TO TAPE                     
*                                  AT LBUYREQ                                   
*                                  FIRST DO B12 AND B13 RECORDS                 
         ZAP   GTTOTCD,=P'0'                                                    
         ZAP   GTTOTAMT,=P'0'                                                   
         ZAP   GTTOTCOM,=P'0'                                                   
*                                                                               
         CLI   QOPT1,C'I'        INVOICE FILE?                                  
         BE    EXIT              THEN DONE                                      
*                                                                               
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
*                                                                               
         XC    BUFREC,BUFREC                                                    
         B     PUTB100                                                          
         EJECT                                                                  
PUTB100  DS    0H                                                               
*                                                                               
*        NEXT OUTPUT ESTIMATE FILE HEADER RECORD                                
*                                                                               
         CLI   EHDRSW,C'Y'      DID I ALREADY DO IT?                            
         BE    PUTB1X                                                           
         MVI   EHDRSW,C'Y'                                                      
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTEFHD,R6                                                        
         MVI   PTEFHTYP,C'1'                                                    
         MVC   PTEFHAPP,=CL10'ADVERT-EST'                                       
         MVC   PTEFHVER,=C'0001'      VERSION                                   
         MVC   PTEFHDAT(8),TODAYY                                               
         MVC   PTEFHTIM,NOW                                                     
*                                                                               
PUTB1D   MVC   PTEFHVEN,VENDOR     VENDOR CODE                                  
         MVC   RECTYPE,=C'E1'                                                   
         BAS   RE,MWRITE                                                        
         AP    ESTRCNT,=P'1'       DUMP FILE COUNT                              
*                                                                               
         DROP  R6                                                               
*                                                                               
*        READ BUFFALO RECORDS AND OUTPUT ESTIMATE FILE                          
*                                                                               
PUTB1X   XC    BUFREC,BUFREC                                                    
         XC    LASTEST,LASTEST                                                  
         XC    LASTEVY,LASTEVY                                                  
         XC    LASTEGL,LASTEGL                                                  
         XC    LASTEIO,LASTEIO                                                  
         XC    LASTECC,LASTECC                                                  
*                                                                               
         ZAP   ESTCD,=P'0'       CLEAR ESTIMATE TOTALS                          
         ZAP   ESTAMTD,=P'0'                                                    
         ZAP   ESTCOMM,=P'0'                                                    
*                                                                               
         MVI   BUFTYPE,C'E'                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     PUTB110                                                          
PUTB105  GOTO1 BUFFALO,DMCB,=C'SEQ',(C'E',BUFFBUFF),BUFREC,0                    
*                                                                               
PUTB110  CLI   DMCB+8,X'80'        END OF FILE                                  
         BE    PUTB200X                                                         
         CLI   BUFTYPE,C'E'                                                     
         BNE   PUTB200X            END OF E'S                                   
         CLI   LASTEST,0           FIRST REC?                                   
         BNE   PUTB112                                                          
         MVC   LASTEST(9),BUFREC                                                
         MVC   LASTEVY,BUFCOME1      VALIDITY YEAR                              
         MVC   LASTEGL,BUFCOME2      GL ACCOUNT                                 
         MVC   LASTEIO,BUFCOME3      IO NUMBER                                  
         MVC   LASTECC,BUFCOME4      COST CENTER                                
         B     PUTB113             GO TO NEW ESTIMATE ROUTINE                   
*                                                                               
PUTB112  CLC   BUFREC(9),LASTEST                                                
         BE    PUTB120             SAME ESTIMATE                                
         MVI   ETOTSW,C'Y'                                                      
         BAS   RE,DISPLAY                                                       
*                                                                               
PUTB113  DS    0H                  NEW ESTIMATE                                 
*                                  MUST OUTPUT ESTIMATE HEADER                  
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTEHD,R6                                                         
         MVI   PTEHTYP,C'4'                                                     
         MVC   PTEHEST(1),QMED                                                  
         CLI   QMED,C'T'     IF TV ALTER TO B                                   
         BNE   *+8                                                              
         MVI   PTEHEST,C'B'                                                     
         CLI   QMED,C'N'     IF NETWORK ALTER TO W                              
         BNE   *+8                                                              
         MVI   PTEHEST,C'W'                                                     
         MVC   PTEHEST+1(3),CLT                                                 
         MVC   PTEHEST+4(3),BUFPRD                                              
         MVC   PTEHEST+7(3),BUFEST                                              
         MVC   PTEHESTD(20),BUFCOMEN     ESTIMATE NAME                          
         MVC   PTEHVYR,BUFMTH    YYYY                                           
         MVI   PTEHCAT,C'S'      SYSTEM - SPOT                                  
         CLI   NETOPT,C'N'       IS THIS NET?                                   
         BNE   *+8                                                              
         MVI   PTEHCAT,C'N'      SYSTEM - NET                                   
         MVC   PTEHCAT+1(1),BUFMED  MEDIA                                       
         MVC   PTEHREV,=C'0000'  REVISON NUMBER - ALWAYS 0000 FOR MEDIA         
         MVC   PTEHREVD,TODAYY   USE TODAY                                      
         MVC   PTEHGL,BUFCOME2   GL ACCOUNT                                     
         OC    PTEHGL,SPACES                                                    
         MVC   PTEHIO,BUFCOME3   INTERNAL ORDER #                               
         OC    PTEHIO,SPACES                                                    
*                                                                               
         CLC   PTEHIO,SPACES                                                    
         BNE   *+10              IF FOUND - DON'T SEND COST CENTER              
*                                                                               
         MVC   PTEHCC,BUFCOME4   COST CENTER                                    
         OC    PTEHCC,SPACES                                                    
         MVC   RECTYPE,=C'E4'                                                   
         BAS   RE,MWRITE                                                        
         AP    ESTRCNT,=P'1'       DUMP FILE COUNT                              
*                                                                               
         MVI   ERRSW,C'N'                                                       
*                                                                               
         OC    BUFCOME1,BUFCOME1   VALIDITY YEAR PRESENT?                       
         BNZ   PUTB113E                                                         
         MVI   ERRSW,C'Y'                                                       
         MVI   RERRSW,C'Y'                                                      
         MVC   P1+2(29),=C'** ERROR - PRD XXX EST NNN - '                       
         MVC   P1+17(3),BUFPRD                                                  
         MVC   P1+25(3),BUFEST                                                  
         MVC   P1+31(24),=C'MISSING VALIDITY YEAR **'                           
         MVI   RCSUBPRG,20                                                      
         GOTO1 REPORT                                                           
*                                                                               
PUTB113E OC    BUFCOME2,BUFCOME2   GL ACCOUNT PRESENT?                          
         BNZ   PUTB113F                                                         
         MVI   ERRSW,C'Y'                                                       
         MVI   RERRSW,C'Y'                                                      
         MVC   P1+2(29),=C'** ERROR - PRD XXX EST NNN - '                       
         MVC   P1+17(3),BUFPRD                                                  
         MVC   P1+25(3),BUFEST                                                  
         MVC   P1+31(24),=C'** MISSING GL ACCOUNT **'                           
         MVI   RCSUBPRG,20                                                      
         GOTO1 REPORT                                                           
*                                                                               
PUTB113F OC    BUFCOME3(L'BUFCOME3+L'BUFCOME4),BUFCOME3 I/O+COST C              
         BNZ   PUTB114                                                          
         MVI   ERRSW,C'Y'                                                       
         MVI   RERRSW,C'Y'                                                      
         MVC   P1+2(29),=C'** ERROR - PRD XXX EST NNN - '                       
         MVC   P1+17(3),BUFPRD                                                  
         MVC   P1+25(3),BUFEST                                                  
         MVC   P1+31(38),=C'** MISSING BOTH I/O AND COST CENTER **'             
         MVI   RCSUBPRG,20                                                      
         GOTO1 REPORT                                                           
*                                                                               
PUTB114  OC    BUFCOME3,BUFCOME3   I/O #                                        
         BZ    PUTB115                                                          
         OC    BUFCOME4,BUFCOME4   COST CENTER                                  
         BZ    PUTB115                                                          
******   MVI   ERRSW,C'Y'                                                       
         MVC   P1+2(29),=C'** WARNING - PRD XXX EST NNN - '                     
         MVC   P1+19(3),BUFPRD                                                  
         MVC   P1+27(3),BUFEST                                                  
         MVC   P1+31(38),=C'** BOTH I/O AND COST CENTER PRESENT **'             
         MVI   RCSUBPRG,20                                                      
         GOTO1 REPORT                                                           
*                                                                               
PUTB115  CLI   ERRSW,C'N'            ANY ERROR FOUND?                           
         BE    PUTB120                                                          
*                                                                               
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    PUTB120               CONTINUE - ELSE DIE                        
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
         DROP  R6                                                               
PUTB120  DS    0H                                                               
         MVC   LASTEST,BUFREC                                                   
         MVC   LASTEVY,BUFCOME1      VALIDITY YEAR                              
         MVC   LASTEGL,BUFCOME2      GL ACCOUNT                                 
         MVC   LASTEIO,BUFCOME3      IO NUMBER                                  
         MVC   LASTECC,BUFCOME4      COST CENTER                                
*                                                                               
         CLC   BUFMTH(4),BUFCOME1   SEE IF WITHIN VALIDITY YEAR                 
         BE    PUTB125                                                          
*                                                                               
         MVC   P1+2(72),=C'** ERROR - MONTH MM/YYYY NOT IN VALIDITY YEAX        
               R YYYY  - PRD=XXX EST=XXX **'                                    
         MVC   P1+19(2),BUFMTH+4                                                
         MVC   P1+22(4),BUFMTH                                                  
         MVC   P1+48(4),BUFCOME1                                                
         MVC   P1+60(3),BUFPRD                                                  
         MVC   P1+68(3),BUFEST                                                  
         MVI   RCSUBPRG,20                                                      
         GOTO1 REPORT                                                           
         MVI   RERRSW,C'Y'                                                      
*                                                                               
         CLI   QOPT6,C'Y'            SEE IF TEST RUN                            
         BE    PUTB125               CONTINUE - ELSE DIE                        
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
*                                                                               
PUTB125  DS    0H                                                               
         BAS   RE,DISPLAY          PRINT ESTIMATE/STA $                         
*                                                                               
         OC    BUFSTA,BUFSTA                                                    
         BZ    PUTBEST           ESTIMATE DETAIL LINE                           
         B     PUTBSTA           MUST BE SPEND LINE                             
*                                                                               
         EJECT                                                                  
PUTBEST  DS    0H               ESTIMATE DETAIL LINE                            
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTELD,R6                                                         
         MVI   PTELTYP,C'5'                                                     
         MVC   PTELMMWC(5),=C'MEDIA'                                            
         MVC   PTELMOS,BUFMTH    YYYYMM                                         
*                                                                               
*        NOTE: IT'S POSSIBLE THAT THESE AMOUNTS MIGHT BE NEGATIVE               
*              THEY DON'T HAVE AN INDICATOR ON THIS RECORD                      
*              MAYBE ONE IS NEEDED FOR EACH FIELD?                              
*                                                                               
         EDIT  BUFCD,(12,PTELCD),2,FILL=0,ZERO=NOBLANK                          
         EDIT  BUFAMTD,(12,PTELNET),2,FILL=0,ZERO=NOBLANK                       
         EDIT  BUFCOMM,(12,PTELCOM),2,FILL=0,ZERO=NOBLANK                       
         MVC   RECTYPE,=C'E5'                                                   
         BAS   RE,MWRITE                                                        
         AP    ESTRCNT,=P'1'       DUMP FILE COUNT                              
         B     PUTB105             GO DO NEXT BUFFALO REC                       
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
PUTBSTA  DS    0H               STATION SPEND LINE                              
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTESLD,R6                                                        
         MVI   PTESLTYP,C'6'                                                    
         MVC   PTESLCAR(L'BUFSTA),BUFSTA                                        
         MVC   PTESLMOS,BUFMTH   YYYYMM                                         
         EDIT  BUFAMTD,(12,PTESLNET),2,FILL=0,ZERO=NOBLANK                      
         MVC   RECTYPE,=C'E6'                                                   
         BAS   RE,MWRITE                                                        
         AP    ESTRCNT,=P'1'       DUMP FILE COUNT                              
         B     PUTB105             GO DO NEXT BUFFALO REC                       
*                                                                               
         DROP  R6                                                               
*                                                                               
PUTB200X MVI   ETOTSW,C'Y'         TOTALS FOR LAST ESTIMATE                     
         BAS   RE,DISPLAY                                                       
*                                                                               
         MVC   P1+2(16),=C'**REPORT TOTAL**'                                    
         EDIT  GTTOTCD,(14,P1+37),2,COMMAS=YES,FLOAT=-                          
         EDIT  GTTOTAMT,(14,P1+54),2,COMMAS=YES,FLOAT=-                         
         EDIT  GTTOTCOM,(14,P1+71),2,COMMAS=YES,FLOAT=-                         
         MVI   P1+51,C'*'                                                       
         MVI   P1+52,C'*'                                                       
         MVI   P1+68,C'*'                                                       
         MVI   P1+69,C'*'                                                       
         MVI   P1+85,C'*'                                                       
         MVI   P1+86,C'*'                                                       
         MVI   SPACING,2                                                        
         MVI   RCSUBPRG,20                                                      
         BAS   RE,MYRPT                                                         
         MVC   P1+2(14),=C'UNLOCKED ESTS='                                      
         OC    ESTLLST(2),ESTLLST                                               
         BNZ   PUTB102                                                          
         MVC   P1+17(4),=C'NONE'                                                
         B     PUTB102X                                                         
*                                                                               
PUTB102  LA    R1,P1+17                                                         
         LA    R2,ESTLLST                                                       
PUTB102B OC    0(1,R2),0(R2)                                                    
         BZ    PUTB102X                                                         
         ZIC   R0,0(R2)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R1),DUB                                                      
         OC    1(1,R2),1(R2)                                                    
         BZ    PUTB102X                                                         
         MVI   3(R1),C','                                                       
         LA    R1,4(R1)                                                         
         LA    R2,1(R2)                                                         
         B     PUTB102B                                                         
*                                                                               
PUTB102X MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
*                                 ESTIMATE FILE CONTROL RECORD                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
TOTALS   DS    0H                                                               
*                                                                               
         CLI   SVQOPT1,C'I'       INVOICE FILE RUN?                             
         BNE   TOTE                                                             
*                                 INVOICE FILE CONTROL RECORD                   
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTIFCD,R6                                                        
         MVI   PTIFCTYP,C'9'                                                    
         AP    INVRCNT,=P'1'     FOR THIS RECORD                                
         EDIT  INVRCNT,(5,PTIFCCNT),0,FILL=0,ZERO=NOBLANKS                      
         MVC   RECTYPE,=C'I9'                                                   
         BAS   RE,MWRITE                                                        
         B     TOT0                                                             
*                                                                               
         DROP  R6                                                               
TOTE     CLI   SVQOPT1,C'E'       ESTIMATE FILE RUN?                            
         BNE   TOT0                                                             
*                                                                               
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTEFCD,R6                                                        
         MVI   PTEFCTYP,C'9'                                                    
         AP    ESTRCNT,=P'1'     FOR THIS RECORD                                
         EDIT  ESTRCNT,(5,PTEFCCNT),0,FILL=0,ZERO=NOBLANKS                      
         MVC   RECTYPE,=C'E9'                                                   
         BAS   RE,MWRITE                                                        
*                                                                               
         DROP  R6                                                               
TOT0     DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         L     R4,ATITLES                                                       
         L     R3,ALENTAB                                                       
         LA    R6,9                FOR BCT                                      
TOT2     MVC   P1+7(30),0(R4)                                                   
         EDIT  (P4,4(R3)),(9,P1+40),0,COMMAS=YES                                
         BAS   RE,MYRPT                                                         
         LA    R4,30(R4)                                                        
         LA    R3,8(R3)                                                         
         BCT   R6,TOT2                                                          
         BAS   RE,MYRPT            SKIP A LINE                                  
         MVC   P1+17(13),=C'TOTAL RECORDS'                                      
         EDIT  TOTCNT,(9,P1+40),0,COMMAS=YES                                    
         MVI   P1+49,C'*'                                                       
         BAS   RE,MYRPT                                                         
*                                                                               
         CLI   RERRSW,C'Y'          WERE ANY ERRORS ENCOUNTERED?                
         BNE   TOT20                                                            
         MVC   P1+1(32),=C'* ERRORS HAVE BEEN ENCOUNTERED *'                    
         MVC   P2+1(40),=C'* PLEASE CHECK ALL INDIVIDUAL REQUESTS *'            
         MVC   P3+1(43),=C'* A LIVE RUN WILL CAUSE A PROGRAM FAILURE *'         
         MVI   SPACING,2                                                        
         BAS   RE,MYRPT                                                         
*                                                                               
TOT20    DS    0H                                                               
*                                                                               
         CLI   TAPESW,C'Y'          SEE IF PRODUCING A TAPE                     
         BNE   EXIT                                                             
*                                                                               
         CLOSE (SPHTAPE,)                                                       
         B     EXIT                                                             
         EJECT                                                                  
         DS    XL500    FOR NOW - JUST SO MWRITE AND MYPRT CAN ARE              
*                       ADDRESSED BY OTHER MODULES                              
*                                                                               
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE1,0(R2)                                                    
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         SPACE 2                                                                
MWRITE   NTR1                      FIND RECORD LENGHT IN LENTAB                 
*                                                                               
         L     R1,ALENTAB                                                       
MWRITE4  CLC   0(2,R1),RECTYPE                                                  
         BE    MWRITE5                                                          
         LA    R1,8(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   MWRITE4                                                          
         DC    H'0'                UNKNOWN TYPE                                 
*                                                                               
MWRITE5  MVC   HALF,2(R1)                                                       
         AP    4(4,R1),=P'1'                                                    
         LH    R3,HALF                                                          
         LA    R3,4(R3)                                                         
         STH   R3,OUTREC-4                                                      
         CLI   SVQOPT7,C'P'                                                     
         BE    WRIT1                                                            
         CLI   QOPT7,C'P'                                                       
         BNE   WRIT2                                                            
WRIT1    MVC   P1(125),OUTREC                                                   
         MVC   P2(125),OUTREC+125                                               
         OI    P2,X'01'                                                         
         OI    P3,X'01'                                                         
         GOTO1 HEXOUT,DMCB,OUTREC-4,P4+10,54,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+50,P5+18,50,=C'N'                               
         GOTO1 (RF),(R1),OUTREC+100,P6+18,50,=C'N'                              
         MVC   P4+1(7),=C'001-050'                                              
         MVC   P5+1(7),=C'051-100'                                              
         MVC   P6+1(7),=C'101-150'                                              
         BAS   RE,MYRPT                                                         
WRIT2    DS    0H                                                               
         CLI   SVQOPT6,C'Y'     SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         CLI   QOPT6,C'Y'       SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         LA    R1,SPHTAPE                                                       
         LA    R0,OUTREC-4                                                      
         PUT   (1),(0)                                                          
WRIT3    AP    TOTCNT,=P'1'                                                     
         XIT1                                                                   
*                                                                               
MYRPT    NTR1                                                                   
         CLI   QOPT6,C'Y'              SEE IF TEST RUN                          
         BE    MYRPT3                                                           
         CLI   SVQOPT6,C'Y'            SEE IF TEST RUN                          
         BNE   *+10                                                             
*                                                                               
MYRPT3   MVC   HEAD4+54(12),=C'**TEST RUN**'                                    
*                                                                               
         MVC   QSTART(12),SVSTART      RESTORE FOR HEADLINES                    
*NOP*    CLC   SVEND,=C'999999'                                                 
         CLC   SVEND,=6X'FF'       FOR 21ST CENTURY                             
         BNE   MYRPT5                                                           
         MVC   QEND,SPACES                                                      
MYRPT5   GOTO1 REPORT                                                           
         MVC   QSTART(12),SPACES                                                
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
DISPLAY  NTR1                                                                   
         CLI   BUFREC,X'FF'                                                     
         BE    DPLAYX                                                           
         MVI   RCSUBPRG,20         FOR PRD/EST RECAP                            
*                                                                               
         CLI   ETOTSW,C'Y'          DOING ESTIMATE TOTAL?                       
         BNE   DPLAY10                                                          
         MVC   P1+2(3),LASTEST+2   PRODUCT                                      
         MVC   P1+7(4),LASTEST+5   (YEAR DIGIT AND DDS EST)                     
         MVC   P1+14(6),=C'TOTAL*'                                              
         EDIT  ESTCD,(14,P1+37),2,COMMAS=YES,FLOAT=-                            
         EDIT  ESTAMTD,(14,P1+54),2,COMMAS=YES,FLOAT=-                          
         EDIT  ESTCOMM,(14,P1+71),2,COMMAS=YES,FLOAT=-                          
         MVI   P1+51,C'*'                                                       
         MVI   P1+68,C'*'                                                       
         MVI   P1+85,C'*'                                                       
         MVC   P2+14(14),=C'VALIDITY YEAR='                                     
         MVC   P2+28(4),LASTEVY                                                 
         MVC   P2+33(11),=C'GL ACCOUNT='                                        
         MVC   P2+44(10),LASTEGL                                                
         MVC   P2+55(19),=C'INTERNAL ORDER NO.='                                
         MVC   P2+74(12),LASTEIO                                                
         MVC   P2+87(12),=C'COST CENTER='                                       
         MVC   P2+99(10),LASTECC                                                
         ZAP   ESTCD,=P'0'       CLEAR AFTER PRINTING                           
         ZAP   ESTAMTD,=P'0'                                                    
         ZAP   ESTCOMM,=P'0'                                                    
         MVI   SPACING,2                                                        
         MVI   ETOTSW,C'N'                                                      
         B     DPLAY20                                                          
*                                                                               
DPLAY10  MVC   P1+2(3),BUFPRD                                                   
         MVC   P1+7(4),BUFYR       (YEAR DIGIT AND DDS EST)                     
         MVC   P1+14(8),BUFSTA                                                  
         MVC   P1+25(2),BUFMTH+4    MONTH                                       
         MVI   P1+27,C'/'                                                       
         MVC   P1+28(4),BUFMTH      YEAR                                        
*                                                                               
         EDIT  BUFCD,(14,P1+37),2,COMMAS=YES,FLOAT=-                            
         EDIT  BUFAMTD,(14,P1+54),2,COMMAS=YES,FLOAT=-                          
         EDIT  BUFCOMM,(14,P1+71),2,COMMAS=YES,FLOAT=-                          
         OC    BUFSTA,BUFSTA     DON'T TOTAL STATION RECORDS                    
         BNZ   DPLAY20                                                          
         AP    ESTCD,BUFCD       ADD TO ESTIMATE TOTALS                         
         AP    ESTAMTD,BUFAMTD                                                  
         AP    ESTCOMM,BUFCOMM                                                  
*                                                                               
         AP    GTTOTCD,BUFCD                                                    
         AP    GTTOTAMT,BUFAMTD                                                 
         AP    GTTOTCOM,BUFCOMM                                                 
DPLAY20  BAS   RE,MYRPT                                                         
DPLAYX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
VENTAB   DS    0C                                                               
******   DC    C'H9',CL6'VEN-H9',CL7'PAY-H9'    STARCOM                         
         DC    C'H9',CL6'417899',CL7'2103391'   STARCOM - LEO B.                
         DC    C'DU',CL6'VEN-DU',CL7'PAY-DU'    MEDIAVEST                       
         DC    C'SJ',CL6'VEN-SJ',CL7'PAY-SJ'    SJR TESTING                     
         DC    C'TC',CL6'VEN-TC',CL7'PAY-TC'    TRC TESTING                     
         DC    X'FFFF'         END OF TABLE                                     
*                                                                               
CSHDSC   DC    F'0'       DUMMY SINCE SPOT/NET DON'T HAVE                       
         EJECT                                                                  
*        AGENCY TABLE                                                           
*                                                                               
*        AGENCY CODE/VENDOR/COMPANY/MYUSER VALUE                                
*                                   E=ESTIMATE USER FIELDS                      
*                                                                               
AGYTAB   DC    C'H9',C'E'          STARCOM/LEO                                  
         DC    C'YN',C' '          YOUNG AND RUBICAM                            
         DC    C'DU',C' '          MEDIAVEST?                                   
         DC    C'TC',C' '          TRC -TESTING                                 
         DC    C'SJ',C' '          SJR  -TESTING                                
         DC    X'FFFF'                                                          
*                                                                               
AGYTABL  EQU   3       TABLE ENTRY LENGTH                                       
         EJECT                                                                  
*                                                                               
SPHTAPE  DCB   DDNAME=SPHTAPE,DSORG=PS,RECFM=VB,LRECL=408,             X        
               BLKSIZE=4084,MACRF=PM                                            
*                           NETWORK UNIT PROCESSING                             
PROCNET  CSECT                                                                  
         NMOD1 0,PROCNET                                                        
*                                                                               
*        RA AND R9 FOR SPWORKD                                                  
*        R8 FOR SPPHWRKD                                                        
*                                                                               
*        NOTE THAT R7 SHOULD NOT BE USED IN THIS MODULE                         
*        AS IT'S NEEDED TO REFERENCE ROUTINES IN THE MAIN CODE                  
*                                                                               
         L     R4,ANETBLK                                                       
         USING NETBLOCK,R4                                                      
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
         MVC   NBSELSTR(12),=C'750101991231'                                    
         MVI   NBSELSTR+6,X'FB'                                                 
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
*                                                                               
         MVC   NBAIO,=A(VIRTLREC)  USE VIRTUAL REC AREA                         
         MVC   NBPRINT,PRINT                                                    
         MVC   NBLOADER,LOADER                                                  
         MVC   NBACOM,ACOMFACS                                                  
*                                                                               
         XC    BUFREC,BUFREC                                                    
         MVI   BUFTYPE,C'E'                                                     
         MVC   BUFMED,QMED                                                      
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
         BE    NTUXX                                                            
         CLI   NBMODE,NBVALCLI     SEE IF I JUST VALIDATED CLIENT               
         BNE   NTU10                                                            
         MVI   NBUSER+13,C'N'      ALWAYS PASS PRE-EMPTED UNITS                 
         B     NTU10                                                            
*                                                                               
         SPACE 3                                                                
* PROCESS UNIT                                                                  
         SPACE 2                                                                
NTU12    DS    0H                                                               
         L     R6,NBAIO                                                         
         USING NURECD,R6                                                        
*                                                                               
         CLI   NUPRD,0              SEE IF UNALLOCATED                          
         BE    NTU10                IF SO SKIP                                  
         MVC   SVNUPRD2,NUPRD2       SAVE SECOND PRODUCT                        
*                                                                               
         MVI   CKESTREC,C'U'         SET FROM UNIT                              
         MVC   CKUPRD,NUPRD                                                     
         GOTO1 =A(CKEST)                                                        
*                                                                               
         CLI   MYUSER,C'E'                                                      
         BNE   PROCN1                                                           
*                                                                               
         CLI   ESTU1+25,C'X'         MEANS SKIP THIS ESTIMATE                   
         BE    NTU10                                                            
*                                                                               
PROCN1   DS    0H                                                               
****                                                                            
****     USER FIELDS FOR FOR BUFYR - NO-OPED                                    
****                                                                            
         B     PROCN3                                                           
*                                                                               
***      CLI   MYUSER,C'E'           SEE IF USING USER FIELDS                   
***      BNE   PROCN3                                                           
***      MVC   BUFYR(4),ESTU1+21     1ST FOR CHARS OF UDEF1                     
***      MVC   BUFCOM,SPACES                                                    
***      MVC   BUFCOM(3),ESTU1+22    2ND-4TH CHRAS OF EST UDEF1                 
***                                                                             
***      L     RF,ADEST                                                         
***      USING ESTHDR,RF                                                        
***      MVC   BUFCOM+20(12),ESTART      ALSO SAVE DATES                        
***                                                                             
***      B     PROCN5                                                           
***      DROP  RF                                                               
*                                                                               
*                                                                               
PROCN3   DS    0H                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         MVC   BUFCOM(20),EDESC                                                 
*                                                                               
******************************************************************              
*        THE 3 FIELDS BELOW WILL BE SET FROM UCOM                               
******************************************************************              
         MVC   BUFCOME1,UCOMDATA    VALIDITY YEAR                               
         MVC   BUFCOME2,UCOMDATA+32  GL ACCOUNT                                 
         MVC   BUFCOME3,UCOMDATA+64  INTERNAL ORDER                             
         MVC   BUFCOME4,UCOMDATA+96  COST CENTER                                
*                                                                               
         MVC   BUFYR,UCOMDATA+3    LAST DIGIT OF VALIDITY YEAR                  
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUFEST(3),DUB                                                    
*                                                                               
         DROP  RF                                                               
*                                                                               
PROCN5   DS    0H                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,NUKDATE),(X'14',WRKDATE)                          
*                                                                               
*        GET NET ORDERED TIME+INTEG+SPECIAL                                     
*        GET 15 % OF GROSS ORDERED TIME+INTEG+SPECIAL                           
*        THEIR PRD AAA FORMULA IS NET 15% OF GROSS                              
****                                                                            
****     DIFFERENT FOR Y&R??                                                    
****                                                                            
*                                                                               
         PRINT GEN                                                              
         GOTO1 ANETACC,DMCB,(38,NEACCNET),NETBLOCK,0                            
         GOTO1 ANETACC,DMCB,(28,NEACCGRS),NETBLOCK,(2,1500)                     
         PRINT NOGEN                                                            
*                                                                               
         ZAP   NETDUB,NEACCNET+1(8)                                             
         ZAP   MYDUB,NEACCNET+1(8)                                              
         AP    MYDUB,NEACCGRS+1(8)                                              
*                                                                               
*        DO FIRST PRODUCT                                                       
*                                                                               
         L     R5,VCLIST       EXPANDED PRODUCT LIST                            
*                                                                               
NTU12A   CLC   3(1,R5),NUPRD                                                    
         BE    NTU12B                                                           
         LA    R5,4(R5)                                                         
         CLI   0(R5),0      END OF LIST                                         
         BNE   NTU12A                                                           
         DC    H'0'         CAN'T FIND PRODUCT                                  
*                                                                               
NTU12B   MVC   BUFPRD,0(R5)                                                     
         L     R4,ANETBLK        RESET R4                                       
*                                                                               
         ZAP   SDUB,MYDUB        SET SHARE TO FULL VALUES                       
         ZAP   SNETDUB,NETDUB                                                   
*                                                                               
         CLI   NUPRD2,0          DO I HAVE A SECOND PRD?                        
         BE    NTU12BX                                                          
*                                                                               
         CVB   R0,MYDUB                                                         
         ST    R0,WK                                                            
         CVB   R0,NETDUB                                                        
         ST    R0,WK+4                                                          
         SR    RF,RF                                                            
         ICM   RF,3,NUP1SHR                                                     
         BAS   RE,SETSHR           GET SHARE FOR THIS PRD                       
*                                  DOES 2 AMOUNTS                               
         L     R0,WK                                                            
         CVD   R0,SDUB                                                          
         L     R0,WK+4                                                          
         CVD   R0,SNETDUB                                                       
*                                                                               
*                                                                               
NTU12BX  DS    0H                                                               
*                                                                               
NTU12C   MVC   BUFMTH,WRKDATE      YYYYMM                                       
*                                                                               
         ZAP   BUFAMTD,SDUB                                                     
         ZAP   BUFCOMM,SDUB                                                     
         SP    BUFCOMM,SNETDUB       COMMISSION IS AMT DUE - NET                
         ZAP   BUFCD,=P'0'          ZERO CASH DISCOUNT                          
*                                                                               
NTU12F   GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVC   BUFSTA(L'NUKNET),NUKNET   NETWORK                                
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         XC    BUFSTA,BUFSTA       RECLEAR                                      
*                                                                               
         CLI   SVNUPRD2,0          DO I HAVE A SECOND PRD?                      
         BE    NTU10               NO - THEN FINISHED                           
         CLI   NUPRD2,0            MEANS I DID IT ALREADY                       
         BE    NTU10                                                            
*                                                                               
         MVI   CKESTREC,C'U'         SET FROM UNIT                              
         MVC   CKUPRD,NUPRD2                                                    
         GOTO1 =A(CKEST)                                                        
*                                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         MVC   BUFCOM(20),EDESC                                                 
*                                                                               
******************************************************************              
*        THE 3 FIELDS BELOW WILL BE SET FROM UCOM                               
******************************************************************              
         MVC   BUFCOME1,UCOMDATA    VALIDITY YEAR                               
         MVC   BUFCOME2,UCOMDATA+32  GL ACCOUNT                                 
         MVC   BUFCOME3,UCOMDATA+64  INTERNAL ORDER                             
         MVC   BUFCOME4,UCOMDATA+96  COST CENTER                                
*                                                                               
         MVC   BUFYR,UCOMDATA+3    LAST DIGIT OF VALIDITY YEAR                  
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUFEST(3),DUB                                                    
*                                                                               
         DROP  RF                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,NUP1SHR        1ST PRD SHARE                                
         SH    RF,=H'10000'        COMPLEMENT                                   
         LCR   RF,RF                                                            
         CVB   R0,MYDUB                                                         
         ST    R0,WK                                                            
         CVB   R0,NETDUB                                                        
         ST    R0,WK+4                                                          
         BAS   RE,SETSHR           GET SHARE FOR THIS PRD                       
*                                  DOES 2 AMOUNTS                               
         L     R0,WK                                                            
         CVD   R0,SDUB                                                          
         L     R0,WK+4                                                          
         CVD   R0,SNETDUB                                                       
*                                                                               
*              MUST GET PRD CODE FROM CLIENT HEADER                             
NTU18A   L     R5,VCLIST         EXPANDED PRODUCT LIST                          
NTU18B   CLC   3(1,R5),NUPRD2                                                   
         BE    NTU18C                                                           
         LA    R5,4(R5)                                                         
         CLI   0(R5),0            END OF LIST                                   
         BNE   NTU18B                                                           
         DC    H'0'                MUST FIND PRD                                
*                                                                               
NTU18C   MVC   BUFPRD,0(R5)                                                     
         L     R4,ANETBLK        RESET R4                                       
         MVI   NUPRD2,0         SO I WON'T REDO                                 
         B     NTU12BX          PROCESS 2ND PRODUCT                             
*                                                                               
         DROP  R4                COVERED NETBLOCK                               
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
CKPYES   CR    RE,RE               PRD OK                                       
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
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
         SPACE 3                                                                
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
*                                                                               
         EJECT                                                                  
INVOICE  CSECT                                                                  
         NMOD1 0,INVOICE                                                        
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS R7, R8 AND R9                              
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
*                                                                               
         DS    0H                                                               
         L     RF,ADBILL                                                        
         USING BILLREC,RF                                                       
*                                                                               
         CLI   FRSTBILL,C'Y'      FIRST BILL RECORD                             
         BNE   BILL5                                                            
*                                                                               
         MVI   FRSTBILL,C'N'      SO I'LL ONLY DO ONCE                          
*                                 INVOICE FILE HEADER                           
         CLI   IHDRSW,C'Y'        HAVE I DONE ONE?                              
         BE    BILL5                                                            
         MVI   IHDRSW,C'Y'                                                      
*                                                                               
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTIFHD,R6                                                        
         MVI   PTIFHTYP,C'1'                                                    
         MVC   PTIFHAPP,=CL10'ADVERT-INV'                                       
         MVC   PTIFHVER,=C'0001'      VERSION                                   
         MVC   PTIFHDAT(8),TODAYY                                               
         MVC   PTIFHTIM,NOW         HHMMSS (MILITARY)                           
*                                                                               
         MVC   PTIFHVEN,VENDOR     VENDOR CODE                                  
         MVC   PTIFHPAY,PAYEE      PAYEE CODE                                   
         MVC   RECTYPE,=C'I1'                                                   
         BAS   RE,MWRITE                                                        
         AP    INVRCNT,=P'1'       DUMP FILE COUNT                              
*                                                                               
         DROP  R6                                                               
*                                                                               
BILL5    CLI   LASTBILL,C'Y'                                                    
         BE    BILL10                                                           
*                                                                               
         CLC   LINVFULL,DINVFULL   SAME INVOICE NUMBER?                         
         BNE   BILL8                                                            
         CLC   LBQDATE,BQDATE      AND RUN DATE?                                
         BE    BILL50                                                           
*                                                                               
*        NEW INVOICE                                                            
*                                                                               
BILL8    MVC   LINVFULL,DINVFULL                                                
         MVC   LBQDATE,BQDATE                                                   
*                                                                               
*                                                                               
*   SEE IF I NEED TO WRITE RECORDS FOR LAST INVOICE                             
*                                                                               
BILL10   OC    INVTAB(INVLEN),INVTAB    ANY ENTRIES?                            
         BZ    BILL20                                                           
*                                                                               
*        FIRST OUTPUT INVOICE HEADER                                            
*                                                                               
         LA    R6,OUTREC                                                        
         USING PTIHD,R6                                                         
         MVI   PTIHCMI,C' '      CLEAR CREDIT MEMO INDICATOR                    
         CP    INVTOTD,=P'0'                                                    
         BNL   *+8                                                              
         MVI   PTIHCMI,C'X'      CREDIT                                         
         EDIT  INVTOTD,(12,PTIHNET),2,FILL=0,ZERO=NOBLANK                       
         MVC   RECTYPE,=C'I4'                                                   
         BAS   RE,MWRITE                                                        
         AP    INVRCNT,=P'1'      BUMP FILE REC COUNT                           
*                                                                               
         DROP  R6                                                               
*                                                                               
*        NOW OUTPUT INVOICE DETAIL RECORDS                                      
*                                                                               
         LA    R5,INVTAB                                                        
BILL15   OC    0(INVLEN,R5),0(R5)    ANY ENTRY                                  
         BZ    BILL20                                                           
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTILD,R6                                                         
         MVI   PTILTYP,C'5'                                                     
         MVC   PTILMMWC(5),=C'MEDIA'                                            
         MVC   WORK(2),0(R5)                                                    
         MVI   WORK+2,X'01'    SET DAY TO 1                                     
         GOTO1 DATCON,DMCB,(3,WORK),(X'14',PTILMOS)                             
*                                                                               
         MVI   PTILCMI,C' '       MUST CLEAR                                    
         CP    2(6,R5),=P'0'                                                    
         BNL   *+8                                                              
         MVI   PTILCMI,C'X'       NEGATIVE AMOUNT DUE                           
*                                                                               
         MVC   PTILQUAN(5),=C'1.000'      HARD CODED                            
*                                                                               
*        NOTE THAT IT'S POSSIBLE THAT THE COMMISSION                            
*        JUST MIGHT HAVE THE OPPOSITE SIGN OF THAT OF                           
*        THE AMOUNT DUE.                                                        
*        MAYBE ONE IS NEEDED FOR EACH FIELD?                                    
*                                                                               
         MVC   PTILCD,=C'000000000.00'                                          
         EDIT  (P6,2(R5)),(12,PTILNET),2,FILL=0,ZERO=NOBLANK                    
         EDIT  (P6,8(R5)),(12,PTILCOM),2,FILL=0,ZERO=NOBLANK                    
         MVC   RECTYPE,=C'I5'                                                   
         BAS   RE,MWRITE                                                        
         AP    INVRCNT,=P'1'      BUMP FILE REC COUNT                           
         LA    R5,INVLEN(R5)                                                    
         B     BILL15                                                           
*                                                                               
         DROP  R6                                                               
*                                                                               
BILL20   CLI   LASTBILL,C'Y'                                                    
         BE    BILL90            DONE - DO FILE CONTROL RECORD                  
         ZAP   INVTOTD,=P'0'     CLEAR TOTAL AMOUNT DUE                         
         XC    INVTAB,INVTAB     CLEAR INVOICE DETAIL TABLE                     
*                                                                               
         LA    R5,INVTAB                                                        
         ST    R5,ANXTINV                                                       
*                                                                               
*        CREATE INVOICE HEADER - BUT DON'T WRITE TO FILE YET                    
*                                                                               
         L     RF,ADBILL        RESET RF TO BILLREC                             
*                                                                               
*        FIELDS BELOW ONLY USED IF AN ERROR IS FOUND                            
*                                                                               
         MVC   LBPRD,BKEYPRD     PRODUCT                                        
         MVC   LBEST,BKEYEST     AND ESTIMATE                                   
         MVC   LUCOMDAT,UCOMDATA     SAVE VALIDITY YEAR                         
*                                                                               
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         USING PTIHD,R6                                                         
         MVI   PTIHTYP,C'4'                                                     
         MVC   PTIHINV(L'DINVFULL),DINVFULL                                     
         GOTO1 DATCON,DMCB,(0,BQDATE),(X'14',PTIHDAT)                           
         L     RF,ADBILL        RESET RF                                        
         MVC   PTIHEST(1),QMED                                                  
         CLI   QMED,C'T'     IF TV ALTER TO B                                   
         BNE   *+8                                                              
         MVI   PTIHEST,C'B'                                                     
         CLI   QMED,C'N'     IF NETWORK ALTER TO W                              
         BNE   *+8                                                              
         MVI   PTIHEST,C'W'                                                     
         MVC   PTIHEST+1(3),CLT                                                 
         MVC   PTIHEST+4(3),BKEYPRD                                             
*                                                                               
*******  MVC   PTIHEST+6(1),UCOMDATA+3 YEAR DIGIT FROM VALIDITY YEAR            
         ZIC   R0,BKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PTIHEST+7(3),DUB                                                 
*                                                                               
         CLI   MYUSER,C'E'          SEE IF USING USER FIELDS                    
         BNE   BILL20C                                                          
         MVC   PTIHEST,SPACES        CLEAR                                      
         MVC   PTIHEST,ESTU1+21     PM ESTIMATE NUMBER                          
         OC    PTIHEST,SPACES                                                   
         B     BILL20C                                                          
*                                                                               
BILL20C  DS    0H                                                               
         EJECT                                                                  
*                                                                               
*        INVOICE DETAIL TABLE                                                   
*                                                                               
BILL50   DS    0H                                                               
         L     RF,ADBILL                                                        
         L     R5,ANXTINV    POINT TO NEXT ENTRY                                
         MVC   0(1,R5),BKEYYSRV      MOS-YEAR                                   
         MVC   1(1,R5),BKEYMSRV      MOS-MONTH                                  
         MVC   2(6,R5),BACTP         ACTUAL BILL AMOUNT                         
         ZAP   DUB,BACTP                                                        
*                                                                               
         AP    INVTOTD,BACTP    INVOICE TOTAL                                   
*                                                                               
         SP    DUB,BNETP        ACTUAL-NET=COMMISSION?                          
         MVC   8(6,R5),DUB+2                                                    
         LA    R5,INVLEN(R5)                                                    
         ST    R5,ANXTINV                                                       
*                                                                               
         L     RF,ADBILL                                                        
*                                                                               
         CLI   QOPT5,C'Y'          SEE IF LISTING CUR INVOICES                  
         BNE   POSTB8                                                           
         MVC   P1+3(3),BKEYPRD                                                  
         MVC   P1+8(1),UCOMDATA+3  YEAR DIGIT FROM VALIDITY YEAR                
         MVC   P1+9(3),=C'000'                                                  
         ZIC   R0,BKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+9(3),DUB                                                      
*                                                                               
         CLI   MYUSER,C'E'         USING ESTIMATE USER FIELDS?                  
         BNE   INVR10                                                           
         MVC   P2+4(3),=C'PM#'                                                  
         MVC   P2+8(16),ESTU1+21    PM ESTIMATE NUMBER                          
*                                                                               
INVR10   DS    0H                                                               
         CLI   BKEYMSRV,12         FUNNY BILLING PERIODS                        
         BNH   INVR15                                                           
         ZIC   R0,BKEYMSRV                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+14(2),DUB                                                     
         MVI   P1+16,C'/'                                                       
         ZIC   R0,BKEYYSRV                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+17(2),DUB                                                     
         B     INVR17                                                           
*                                                                               
INVR15   GOTO1 DATCON,DMCB,(3,BKEYYSRV),(6,P1+14)                               
         L     RF,ADBILL                                                        
INVR17   DS    0H                                                               
         MVC   P1+23(2),BTYPE                                                   
         B     INVR20                                                           
INVR20   DS    0H                                                               
****     CLI   MYUSER,C'E'                                                      
****     BNE   *+20                                                             
****     MVC   P1+28(1),DINVFULL                                                
****     MVC   P1+29(7),DINVFULL+2      SKIPS FIRST C'-'                        
****     B     *+10                                                             
         MVC   P1+28(10),DINVFULL                                               
*                                                                               
         ZAP   MYBILLCD,=P'0'     NO CD                                         
*                   SO RECIEVALBE + CD IS SAME                                  
         EDIT  (P6,BGRSP),(14,P1+37),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P6,BGRSP),(14,P1+53),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P8,MYBILLCD),(14,P1+69),2,COMMAS=YES,FLOAT=-                    
         EDIT  (P6,BACTP),(14,P1+85),2,COMMAS=YES,FLOAT=-                       
         L     RF,ADBILL     RESET RF                                           
*                                                                               
*        PRINT HAS SOME REVERSED MANUL BILL STUFF HERE                          
*                                                                               
INVR25   GOTO1 DATCON,DMCB,(0,BQDATE),(5,P1+101)                                
         L     RF,ADBILL     RESET RF                                           
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(5,P1+111)                              
         L     RF,ADBILL     RESET RF                                           
*                                                                               
INVR28   MVI   RCSUBPRG,10                                                      
         BAS   RE,MYRPT                                                         
*                                                                               
*                                  ROLL TO CURRENT INV TOTALS                   
INVR30   AP    CINVGRS,BGRSP       GROSS                                        
         AP    CINVBIL,BGRSP       SAME SINCE NO CD                             
         AP    CINVCD,MYBILLCD                                                  
         AP    CINVRCV,BACTP       RECEIVABLE                                   
         OI    CINVSW,1                                                         
*                                                                               
POSTB8   DS    0H                                                               
         B     BILLX                                                            
*                                                                               
         DROP  RF                                                               
         EJECT                                                                  
*                                                                               
BILL90  DS     0H                                                               
*                                 INVOICE FILE CONTROL RECORD                   
*                                 NOW DONE AT RUNLAST                           
BILLX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
CKEST    CSECT                                                                  
         NMOD1 0,CKEST                                                          
*****                                                                           
*****    NOTE - DO NOT USE REGISTERS R7, R8, R9                                 
*****           IN THIS CSECT                                                   
*****           THEY ARE USED FOR THE WHOLE PROGRAM                             
*****                                                                           
*                                                                               
         MVI   BILLONLY,C'N'                                                    
*                                                                               
         DS    0H                                                               
         MVC   PPGKEY,KEY                                                       
         MVC   PPGAREC,AREC                                                     
         MVI   CKESTSW,0    WILL BE SET TO X'01' IF I READ SOMETHING            
         XC    KEY,KEY                                                          
         L     RF,ADBUY                                                         
         USING BUYREC,RF                                                        
         MVC   KEY+1(1),BUYKAM    A/M                                           
         MVC   KEY+2(2),BUYKCLT   CLT                                           
         MVC   KEY+4(3),PRD       PRD                                           
         MVC   KEY+7(1),BUYKEST   EST                                           
         MVC   CKEPRD,PRD                                                       
         CLI   CKESTREC,C'B'        FROM SPOT BUY                               
         BE    CKEST3                                                           
*                                                                               
         CLI   QOPT5,C'Y'      LISTING CURRENT INVOICES?                        
         BNE   *+8                                                              
         MVI   RCSUBPRG,10         WHEN DOING BILLS                             
         DROP  RF                                                               
         L     RF,ADBILL                                                        
         USING BILLREC,RF                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(8),BILLREC      00/A/M/CLT/PRD/EST                           
         MVC   CKEPRD,BKEYPRD                                                   
         CLI   CKESTREC,C'L'        FROM BILL                                   
         BE    CKEST3                                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
*                                                                               
*              MUST GET PRD CODE FROM CLIENT HEADER                             
CKEST2   L     R5,VCLIST         EXPANDED PRODUCT LIST                          
CKEST2B  CLC   3(1,R5),CKUPRD    PRODUCT I'M PROCESSING                         
         BE    CKEST2C                                                          
         LA    R5,4(R5)                                                         
         CLI   0(R5),0             END OF LIST                                  
         BNE   CKEST2B                                                          
         DC    H'0'                MUST FIND PRD                                
*                                                                               
CKEST2C  DS    0H                                                               
         MVC   CKEPRD,0(R5)                                                     
         L     R4,ANETBLK                                                       
         USING NETBLOCK,R4                                                      
         L     RF,NBAIO                                                         
         USING NURECD,RF                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NUKAM                                                   
         MVC   KEY+2(2),NUKCLT                                                  
         MVC   KEY+4(3),CKEPRD                                                  
         MVC   KEY+7(1),NUKEST                                                  
         CLI   CKESTREC,C'U'        FROM A UNIT                                 
         BE    CKEST3                                                           
*                                                                               
         DC    H'0'                 ERROR - UNKNOWN RECORD TYPE                 
*                                                                               
         DROP  R4                                                               
         DROP  RF                                                               
*                                                                               
CKEST3   L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         CLC   ESTHDR(8),KEY        SEE IF I ALREADY HAVE EST                   
         BE    CKEST5                                                           
         MVI   CKESTSW,1                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                EST MUST BE ON FILE                          
         GOTO1 GETEST                                                           
*                                                                               
         BAS   RE,ESTF            ADD TO ESTTABLE                               
*                                                                               
CKEST5   DS    0H                                                               
*                                                                               
CKEST5C  DS    0H                                                               
         XC    ESTU1,ESTU1       CLEAR EST USER FIELDS                          
         XC    ESTU2,ESTU2                                                      
*                                                                               
         CLI   MYUSER,C'E'         SEE IF USER FIELDS IN USE                    
         BNE   CKEST50                                                          
*                                                                               
         CLC   EKEYPRD,=C'AAA'     NOT FOR PRD AAA                              
         BE    CKEST50                                                          
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'S',ADCLT),(C'E',ADEST),ESTU1,0                  
         L     RF,ADEST       RESET RF                                          
         CLI   DMCB,X'FF'                                                       
         BE    UESTERR                                                          
         CLI   ESTU1+21,C' '    MUST FIND DATA                                  
         BNH   UESTERR                                                          
         B     CKEST50                                                          
*                                                                               
UESTERR  DS    0H                                                               
         LA    R1,BADESTS                                                       
         MVC   WORK(3),EKEYPRD                                                  
         MVC   WORK+3(1),EKEYEST                                                
UESTERR2 CLI   0(R1),X'FF'         END OF TABLE                                 
         BE    UESTERR3                                                         
         CLC   WORK(4),0(R1)                                                    
         BE    CKEST50                                                          
         LA    R1,5(R1)                                                         
         B     UESTERR2                                                         
*                                                                               
UESTERR3 MVC   0(4,R1),WORK                                                     
         MVI   4(R1),0                                                          
         MVI   5(R1),X'FF'        SET NEW END OF TABLE                          
*                                                                               
         MVC   P1(35),=C'*** MISSING ESTIMATE USER FIELD ***'                   
         MVC   P1+40(3),EKEYPRD                                                 
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+45(3),DUB                                                     
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         MVI   RERRSW,C'Y'                                                      
         CLI   QOPT6,C'Y'           SEE IF TEST RUN                             
         BE    CKEST50                                                          
         MVC   P2(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
         DROP  RF                                                               
*                                                                               
*                                                                               
CKEST50  DS    0H                                                               
*                                                                               
*        FIRST TRY TO FIND UNDER BRAND EST                                      
*                                                                               
*        GET ESTIMATE UCOMS FOR PRODUCT POL                                     
*        POL ESTIMATE READ INTO ADCOMREC (BIG ENOUGH FOR EST?)                  
*                                                                               
         XC    UCOMDATA,UCOMDATA   CLEAR UCOMM DATA                             
         L     RF,ADEST                                                         
         MVC   KEY,0(RF)                                                        
         L     RE,ADCOMREC                                                      
         ST    RE,AREC                                                          
         MVC   KEY+EKEYPRD-ESTHDR(3),=C'POL'                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CKESMISS                                                         
         GOTO1 GET                                                              
         CLI   DMCB+8,0                                                         
         BNE   CKESMISS                                                         
*                                                                               
*        CALL DDUCOM TO GET ESTIMATE'S FIRST 4 UCOMS                            
*                                                                               
         MVC   USAVKEY,KEY   SAVE MY KEY                                        
         LA    R5,UCOMBLK     SET-UP UCOM CONTROL BLOCK                         
         XC    UCOMBLK(L'UCOMBLK),UCOMBLK                                       
         USING DDUCOMD,R5                                                       
         MVC   UCPRD,CKEPRD       FIRST TRY FOR BRAND                           
*                                                                               
CKES1    MVC   UCACOMF,ACOMFACS     COMFACS                                     
         MVI   UCSYS,C'N'        SYSTEM TO NETPAK                               
         CLI   NETOPT,C'N'       NETPAK?                                        
         BE    *+8                                                              
         MVI   UCSYS,C'S'        SYSTEM TO PRINT (SPOT)                         
         MVC   UCSAM,BAGYMD      AGENCY/MEDIA                                   
         MVC   UCSCLT,BCLT       PACKED CLIENT                                  
*                                DO UCOMMS FOR PRD POL                          
         OI    UCOPT,UCOEST     RETURN ESTIMATE UCOMMS                          
         L     R2,ADEST          BRAND ESTIMATE                                 
         USING ESTHDR,R2                                                        
         MVC   UCSEST,EKEYEST                                                   
*                                                                               
         GOTO1 VDDUCOM,UCOMBLK    NEW UCOM CALL SINCE GOTO MACRO                
         CLI   UCERROR,0         TRASHED WRKING STORAGE USED BY DDUCOM          
         BNE   CKES3X       ERROR RETURN - JUST EXIT DON'T DIE                  
         TM    UCDATA,UCDNOEST                                                  
         BNO   CKES2                                                            
         CLC   UCPRD,=C'POL'     DID I JUST TRY FOR POL?                        
         BE    CKES2X                                                           
         MVC   UCPRD,=C'POL'     TRY FOR POL UCOMMS                             
         B     CKES1                                                            
*                                                                               
CKES2    XC    UCTTLS(UCALL),UCTTLS                                             
         L     R4,UCETTLS     EST TITLES                                        
         MVC   UCTTLS,0(R4)   SAVE INFO IN MY STORAGE                           
         LA    R4,UCTTLS      AS OPPOSED TO RD CHANE                            
         L     RE,UCEDATA     EST DATA                                          
         MVC   UCOMDATA,0(RE)                                                   
*                                                                               
CKES2X   CLI   QOPT1,C'E'     ESTIMATE FILE ONLY                                
         BE    CKES3X         ANY ERRORS WILL BE CAUGHT LATER                   
*                                                                               
         OC    UCOMDATA(4),UCOMDATA   1ST IS VALIDITY YEAR                      
         BNZ   CKES3X                                                           
         MVC   P1+2(29),=C'** ERROR - PRD XXX EST NNN - '                       
         MVC   P1+17(3),PRD                                                     
         ZIC   R0,EKEYEST      (FROM BRAND ESTIMATE)                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+25(3),DUB+6(2)                                                
         MVC   P1+31(24),=C'MISSING VALIDITY YEAR **'                           
         GOTO1 REPORT                                                           
         MVI   RERRSW,C'Y'                                                      
         CLI   QOPT6,C'Y'      TEST RUN?                                        
         BE    CKES3X          KEEP GOING                                       
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
CKES3X   DS    0H                                                               
         MVI   CKESTSW,1        READ SOMETHING                                  
         DROP  R5                                                               
*                                                                               
CKEST80  MVC   KEY,PPGKEY                                                       
         MVC   AREC,PPGAREC                                                     
         CLI   CKESTSW,1           SEE IF I READ SOMETHING                      
         BNE   CKESTX              NO - SKIP READ HIGH                          
         GOTO1 HIGH                                                             
CKESTX   XIT1                                                                   
*                                                                               
CKESMISS DS    0H                  POL ESTIMATE MISSING                         
         MVC   P1+2(29),=C'** ERROR - POL EST NNN - MISSING **'                 
         L     R2,ADEST      TO BRAND ESTIMATE                                  
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+21(3),DUB+6(2)                                                
         GOTO1 REPORT                                                           
         MVI   RERRSW,C'Y'                                                      
         CLI   QOPT6,C'Y'      TEST RUN?                                        
         BE    CKES3X          KEEP GOING                                       
         MVC   P1(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
ESTF     NTR1                                                                   
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         OC    ELOCKYM,ELOCKYM    SEE IF LOCK YEAR/MONTH PRESENT                
         BNZ   ESTFX                                                            
         LA    R1,500                                                           
         LA    R2,ESTLLST                                                       
ESTF5    CLC   0(1,R2),EKEYEST                                                  
         BE    ESTFX                                                            
         OC    0(1,R2),0(R2)                                                    
         BNZ   ESTF10                                                           
         MVC   0(1,R2),EKEYEST                                                  
         B     ESTFX                                                            
*                                                                               
ESTF10   LA    R2,1(R2)                                                         
         BCT   R1,ESTF5                                                         
         DC    H'0'                TOO MANY UNLOCKED ESTS                       
*                                                                               
ESTFX    XIT                                                                    
*                                                                               
         DROP  RF                                                               
*                                                                               
         LTORG                                                                  
CKESTSW  DC    X'00'                                                            
*****                                                                           
NTITLES  CSECT                                                                  
         DS    0C                                                               
         DC    CL30'NET  - INV. FILE HEADER'                                    
         DC    CL30'            INV. HEADER'                                    
         DC    CL30'              INV. LINE'                                    
         DC    CL30'           INV. CONTROL'                                    
         DC    CL30'        EST FILE HEADER'                                    
         DC    CL30'             EST HEADER'                                    
         DC    CL30'               EST LINE'                                    
         DC    CL30'             SPEND LINE'                                    
         DC    CL30'            EST CONTROL'                                    
*                                                                               
*                                                                               
TITLES   CSECT                                                                  
         DS    0C                                                               
         DC    CL30'SPOT - INV. FILE HEADER'                                    
         DC    CL30'            INV. HEADER'                                    
         DC    CL30'              INV. LINE'                                    
         DC    CL30'           INV. CONTROL'                                    
         DC    CL30'        EST FILE HEADER'                                    
         DC    CL30'             EST HEADER'                                    
         DC    CL30'               EST LINE'                                    
         DC    CL30'             SPEND LINE'                                    
         DC    CL30'            EST CONTROL'                                    
*                                                                               
         EJECT                                                                  
*                                                                               
DISPTAB  CSECT                                                                  
         DC    X'0101',H'0'        JAN                                          
         DC    X'0102',H'18'                                                    
         DC    X'0103',H'36'                                                    
         DC    X'01FF',H'54'       1ST QUARTER                                  
         DC    X'0204',H'72'                                                    
         DC    X'0205',H'90'                                                    
         DC    X'0206',H'108'                                                   
         DC    X'02FF',H'126'      2ND QUARTER                                  
         DC    X'0307',H'144'                                                   
         DC    X'0308',H'162'                                                   
         DC    X'0309',H'180'                                                   
         DC    X'03FF',H'198'      3RD QUARTER TOTAL                            
         DC    X'040A',H'216'                                                   
         DC    X'040B',H'234'                                                   
         DC    X'040C',H'252'                                                   
         DC    X'04FF',H'270'      4TH QUARTER                                  
         DC    X'FFFF',H'288'      GRAND TOTAL                                  
*                                                                               
TOTDISP  EQU   288    DISPLACEMENT TO EST TOTALS IN A RECS                      
*                                                                               
         EJECT                                                                  
*             TABLE OF RECORD TYPES,LENGTHS AND COUNTS                          
* SPOT/NET                                                                      
LENTAB   CSECT                                                                  
         DC    CL2'I1',AL2(PTIFHX),PL4'0'                                       
         DC    CL2'I4',AL2(PTIHX),PL4'0'                                        
         DC    CL2'I5',AL2(PTILX),PL4'0'                                        
         DC    CL2'I9',AL2(PTIFCX),PL4'0'                                       
         DC    CL2'E1',AL2(PTEFHX),PL4'0'                                       
         DC    CL2'E4',AL2(PTEHX),PL4'0'                                        
         DC    CL2'E5',AL2(PTELX),PL4'0'                                        
         DC    CL2'E6',AL2(PTESLX),PL4'0'                                       
         DC    CL2'E9',AL2(PTEFCX),PL4'0'                                       
         DC    XL4'00',PL4'0'      EXTRA LINE                                   
         DC    X'FFFF'                                                          
         EJECT                                                                  
SPPHWRKD DSECT                                                                  
NETOPT   DS    CL1                                                              
SVQOPT1  DS    CL1          SO I'LL KNOW AT RUNLAST WHICH FILE TYPE             
SVQOPT6  DS    CL1          SO I'LL KNOW AT RUNLAST IF TEST RUN                 
SVQOPT7  DS    CL1          SO I'LL KNOW AT RUNLAST IF PDUMPING                 
ERRSW    DS    CL1          FOR EACH REQUEST                                    
RERRSW   DS    CL1          ONE TO CHECK AT RUNLAST                             
EHDRSW   DS    CL1                                                              
IHDRSW   DS    CL1                                                              
RECTYPE  DS    CL2          USED BY MWRITE                                      
VENDOR   DS    CL6          SET FROM VENTAB                                     
PAYEE    DS    CL7          SET FROM VENTAB                                     
TOTCNT   DS    PL4'0'                                                           
MYDUB    DS    PL8                                                              
NETDUB   DS    PL8                                                              
SDUB     DS    PL8          USED FOR SHARES IN PROCNET                          
SNETDUB  DS    PL8          USED FOR SHARES IN PROCNET                          
SVNUPRD2 DS    XL1          SAVED NUPRD2                                        
*                                                                               
CKUPRD   DS    XL1          USED IN CKEST - FROM UNIT                           
CKEPRD   DS    CL3          SET IN CKEST                                        
*                                                                               
MYDUMP   DS    XL2                                                              
*                                                                               
ALLOWSW  DS    XL1                                                              
DYNDDN   DS    CL8                                                              
DYNDSN   DS    CL20                                                             
         DS    0F          ALIGNMENT FOR WK                                     
*                                                                               
WK       DS    CL20                                                             
WPRD     DS    XL1                                                              
ELCODE   DS    XL1                                                              
*                                                                               
WRKDATE  DS    XL8                                                              
*                                                                               
*        NETACC RETURN AREAS                                                    
*                                                                               
NEACCNET DS    CL9         1 + 8 BYTE PACKED NET ORDERED                        
NEACCGRS DS    CL9         1 + 8 BYTE PACKED GROSS ORDERED                      
*                                                                               
MYBEST   DS    XL1         FROM QEST                                            
MYBESTE  DS    XL1         FROM QESTEND                                         
*                                                                               
B1PROF   DS    CL16                                                             
B1XPROF  DS    CL16                                                             
*                                                                               
DINVFULL DS    CL10                                                             
*                                                                               
LINVFULL DS    CL10     SAVED                                                   
LBQDATE  DS    CL6      SAVED                                                   
LBPRD    DS    CL3                                                              
LBEST    DS    XL1                                                              
LUCOMDAT DS    CL4      SAVED VALIDITY YEAR                                     
*                                                                               
INVTOTD  DS    PL5      TOTAL $ FOR INV NUMBER                                  
LASTBILL DS    CL1      Y= LAST BILL                                            
FRSTBILL DS    CL1      Y= FIRST BILL                                           
INVRCNT  DS    PL5      COUNT OF INVOICE FILE RECORDS                           
ESTRCNT  DS    PL5      COUNT OF ESTIMATE FILE RECORDS                          
*                                                                               
WORK2    DS    CL64                                                             
ADISPTAB DS    A                                                                
ALENTAB  DS    A                                                                
ATITLES  DS    A                                                                
VGETUSER DS    A                                                                
VDDUCOM  DS    A                                                                
VPROCNET DS    A                                                                
ANETBLK  DS    A                                                                
ANETNET  DS    A                                                                
ANETACC  DS    A                                                                
AFMTINO  DS    A                                                                
ACONIO1  DS    A                                                                
ANXTINV  DS    A                                                                
*                                                                               
MYUSER   DS    CL1       SET FROM AGYTAB AT FBUYREQ                             
*                                                                               
ESTU1    DS    CL54      USER FIELDS                                            
ESTU2    DS    CL38                                                             
PRDU1    DS    CL54                                                             
*                                                                               
*        UCOM FIELDS AND CONTROL BLOCK                                          
UCOMBLK  DS    CL(UCOMDLNQ)     DDUCOM CONTROL BLOCK                            
UCTTLS   DS    CL80             LEN=20*4                                        
UCOMDATA DS    CL128            LEN=32*4                                        
UCALL    EQU   *-UCTTLS                                                         
USAVKEY  DS    XL13             TO SAVE CURRENT READ SEQUENCE                   
UCOMQ    EQU   *-UCOMBLK                                                        
         SPACE 3                                                                
MYBILLCD DS    PL8                                                              
*                                                                               
CINVGRS  DS    PL8        CURRENT INVOICE TOTALS                                
CINVBIL  DS    PL8                                                              
CINVCD   DS    PL8                                                              
CINVRCV  DS    PL8                                                              
CINVSW   DS    CL1                                                              
*                                                                               
ADDDEL   DS    CL1                 X'01' IF ADDED + DELETED IN PERIOD           
BILLONLY DS    CL1                 SET IN CKEST Y= BILL ONLY ESTIMATE           
*                                  (NO CHANGES)                                 
SAVMED   DS    CL2               USED IN MWRITE TO SAVE 'REAL' MEDIA            
CKESTREC DS    CL1                                                              
*                                                                               
ETOTSW   DS    CL1                                                              
ESTCD    DS    PL8                 ESTIMATE TOTALS FOR PRINTING                 
ESTAMTD  DS    PL8                                                              
ESTCOMM  DS    PL8                                                              
*                                                                               
GTTOTCD  DS    PL8                 REPORT TOTALS                                
GTTOTAMT DS    PL8                                                              
GTTOTCOM DS    PL8                                                              
*                                                                               
TODAY1   DS    CL6                                                              
TODAYY   DS    CL8         YYYYMMDD                                             
NOW      DS    CL6         TIME HHMMSS                                          
ELCODE1  DS    CL1                                                              
SAVCGR   DS    PL8                                                              
*                                                                               
TAPESW   DS    CL1         STARTS AS 0 CHANGED TO N OR Y AT FBUYCLI             
*                          MIX NOT ALLOWED                                      
*                                                                               
SVQSTART DS    CL3                                                              
SVQEND   DS    CL3                                                              
BQS      DS    XL3                                                              
BQE      DS    XL3                                                              
*                                                                               
SVQST    DS    CL6               REQUEST'S VALUES                               
SVQED    DS    CL6                   "                                          
SVQSTP   DS    XL2                   "                                          
SVQEDP   DS    XL2                   "                                          
SVQSTB   DS    XL3                   "                                          
SVQEDB   DS    XL3                   "                                          
*                                                                               
CIRCDAT  DS    CL3                                                              
TRCODE   DS    CL1                                                              
PPGKEY   DS    CL32                                                             
PPGAREC  DS    CL4                                                              
ZEROS    DS    CL30                                                             
LASTEST  DS    CL9          USED TO CHECK CHANGE OF EST                         
LASTEVY  DS    CL4          VALIDITY YEAR                                       
LASTEGL  DS    CL10         GL ACCOUNT                                          
LASTEIO  DS    CL12         IO NUMBER                                           
LASTECC  DS    CL10         COST CENTER                                         
*                           WHEN READING BUFFALO RECS                           
APPBYOWK DS    A                                                                
         DS    F                                                                
INVTAB   DS    CL252        ROOM FOR 18 MOS X14                                 
*                           ENTRIES ARE MOS (MY)                                
*                                       BACTP  (ACTUAL AMT DUE)                 
*                                       COMMISSION (BACTP-BNETP)                
INVLEN   EQU   14                                                               
*                                                                               
BUFREC   DS    0CL118                                                           
BUFKEY   DS    0CL24                                                            
BUFTYPE  DS    CL1                 E=ESTIMATE                                   
*                                                                               
BUFMED   DS    CL1                 MEDIA                                        
BUFPRD   DS    CL3                 PRODUCT                                      
BUFYR    DS    CL1                 LAST DIGIT OF YEAR                           
BUFEST   DS    CL3                 EST                                          
BUFMTH   DS    CL6                 MONTH  YYYYMM                                
BUFSTA   DS    CL8                 STATION  (EMPTY FOR EST DETAIL)              
         DS    CL1                 SPARE                                        
*                                                                               
BUFCOM   DS    0CL70                COMMENT                                     
BUFCOMEN DS    CL20                ESTIMATE NAME                                
BUFCOME1 DS    CL4                 UCOM1 - VALIDITY YEAR                        
BUFCOME2 DS    CL10                UCOM2 - GL ACCOUNT                           
BUFCOME3 DS    CL12                UCOM3 - INTERNAL ORDER NUMBER                
BUFCOME4 DS    CL10                UCOM4 - COST CENTER                          
         DS    CL14                SPARE                                        
*                                                                               
BUFCD    DS    PL8                 CASH DISCOUNT                                
BUFAMTD  DS    PL8                 AMOUNT DUE (CALC. WITH BILL. FORM.)          
BUFCOMM  DS    PL8                 COMMISSION (DUE-NET)                         
         ORG                                                                    
         DS    F                                                                
OUTREC   DS    CL500                                                            
*                                                                               
*                                                                               
ESTLLST  DS    CL500               LIST OF UNLOCKED ESTS (1 BYTES PER)          
*                                                                               
BADESTS  DS    CL240            ROOM FOR 40 BAD PRD/ESTS                        
*                                PRD(3)/EST(2)/+ ONE BYTE                       
*                                TO BE USED FOR ERRORS                          
*                                                                               
         DS    CL50             SPARE                                           
*                                                                               
         BUFF  LINES=4000,ROWS=1,COLUMNS=3,FLAVOR=PACKED,COMMENT=70,KEYX        
               LIST=(24,A)                                                      
*                                                                               
VIRTLREC DS    0D                                                               
         DS    20000C                                                           
*                                                                               
NETBLK   CSECT                                                                  
         DS    1200C                                                            
*                                                                               
NETBLKD  DSECT                                                                  
*                                                                               
       ++INCLUDE NENETRATED                                                     
         PRINT OFF                                                              
       ++INCLUDE NETBLOCKD                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DDREPMASTD                                                     
*                                                                               
         PRINT ON                                                               
       ++INCLUDE PTIFILED                                                       
       ++INCLUDE DDUCOMD                                                        
QOPT6    EQU   QGRP                                                             
QOPT7    EQU   QGRP+1                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPREPPH02 02/09/15'                                      
         END                                                                    
