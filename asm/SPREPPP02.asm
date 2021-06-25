*          DATA SET SPREPPP02  AT LEVEL 003 AS OF 02/09/15                      
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
*PHASE SPPT02A                                                                  
*INCLUDE GETUSER                                                                
*INCLUDE SPFMTINO                                                               
*INCLUDE NETNET                                                                 
*INCLUDE NETACC                                                                 
         TITLE 'SPPT02 - PHILIP MORRIS INTERFACE'                               
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
*                                                                               
*        QOPT5 Y=LIST 'CURRENT' INVOICES                                        
*        QOPT6 Y= TEST RUN - NO TAPE, AND CONTINUE IF ERRORS                    
*                            ARE FOUND                                          
*        QOPT7 P=PDUMP RECORDS                                                  
*                                                                               
*        QSTART(6) = PERIOR START                                               
*        QEND(6) = PERIOD END - MAY BE BLANK                                    
*                                                                               
*                                                                               
SPPT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPPT02,RR=R9                                                   
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
         USING SPPTWRKD,R8                                                      
         LA    R7,SPPT02+4095                                                   
         LA    R7,1(R7)                                                         
         USING SPPT02+4096,R7     **NOTE USE OF R7 AS BASE REGS*                
                                                                                
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
         CLI   MODE,REQLAST                                                     
         BE    PUTBUFF                                                          
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
         L     R0,=V(SPFMTINO)                                                  
         A     R0,RELO                                                          
         ST    R0,AFMTINO                                                       
*                                                                               
         L     R0,=A(DISPTAB)                                                   
         A     R0,RELO                                                          
         ST    R0,ADISPTAB                                                      
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY1)                                     
         ZAP   TOTCNT,=P'0'                                                     
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
         CLI   NETOPT,C'N'                                                      
         BNE   *+8                                                              
         L     R0,=A(NLENTAB)                                                   
         A     R0,RELO                                                          
         ST    R0,ALENTAB                                                       
*                                                                               
         L     R2,ALENTAB          ZAP ACCUMS                                   
         LA    R3,3                                                             
INIT7L   ZAP   5(4,R2),=P'0'                                                    
         LA    R2,9(R2)                                                         
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
         MVC   DYNDDN,=CL8'SPTTAPE'                                             
         MVC   DYNDSN,=CL20'NETTAPE.NE0PTXX1'                                   
         MVC   DYNDSN+13(2),QAGY                                                
*                                                                               
         CLI   NETOPT,C'N'      NETPAK?                                         
         BE    REQF10                                                           
         MVC   DYNDSN(3),=C'SPT'    ALTER FOR SPOT                              
         MVC   DYNDSN+8(2),=C'SP'                                               
*                                                                               
REQF10   DS    0H                                                               
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
         OPEN  (OUTFILE,OUTPUT)                                                 
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
*                             SET MYAGY AND MYCLT FROM AGYTAB                   
         LA    R1,AGYTAB                                                        
FIRSTB1  CLI   0(R1),X'FF'            END OF TABLE                              
         BNE   *+6                                                              
         DC    H'0'                   INVALID AGENCY                            
*                                                                               
         CLC   0(2,R1),QAGY                                                     
         BE    FIRSTB1D                                                         
         LA    R1,13(R1)                                                        
         B     FIRSTB1                                                          
*                                                                               
FIRSTB1D MVC   MYAGY,2(R1)                                                      
         MVC   MYCLT,8(R1)                                                      
         MVC   MYUSER,12(R1)         USER FIELDS IN USE                         
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
         CLI   COFFICE,C' '                                                     
         BNH   FBC1                                                             
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
*                                                                               
*        NOW READ BILLS                                                         
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
*                                                                               
         TM    BILSTAT,X'20'    SKIP AOR BILLS                                  
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
         B     EXIT                                                             
         EJECT                                                                  
*                                  LAST FOR CLIENT                              
LCLI     DS    0H                                                               
         CLI   NETOPT,C'N'        NETPAK?                                       
         BNE   EXIT               NO - DO NOTHING                               
*                      UNIT PROCESSING NOW IN IT'S OWN CSECT                    
         GOTO1 VPROCNET,DMCB                                                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
FPRD     DS    0H                  FIRST BUY FOR PRODUCT                        
         XC    PRDU1,PRDU1                                                      
         CLI   MYUSER,C'Y'        SEE IF USING USER FIELDS                      
         BNE   FPRDX                                                            
         CLC   PKEYPRD,=C'AAA'    NOT FOR PRD=AAA                               
         BE    FPRDX                                                            
*                                                                               
         GOTO1 VGETUSER,DMCB,(C'S',CLTHDR),(C'P',PRDHDR),PRDU1,0                
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
         B     EXIT                                                             
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
         MVC   QSTART,ESTART       USE ESTIMATE'S START AND END                 
         MVC   QEND,EEND                                                        
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
*                                                                               
         DROP  RF                                                               
*                                                                               
         CLI   MYUSER,C'E'         SEE IF USING ESTIMATE USER FIELDS            
         BNE   PB205                                                            
*                                                                               
         CLI   ESTU1+25,C'X'          MEANS EXCLUDE                             
         BE    EXIT                                                             
*                                                                               
PB205    XC    BUFREC,BUFREC                                                    
         MVI   BUFTYPE,C'A'                                                     
         MVC   BUFMED,QMED                                                      
*                                                                               
         CLI   MYUSER,C'E'           SEE IF USING USER FIELDS                   
         BNE   PB205U                                                           
         MVC   BUFYR(4),ESTU1+21     1ST FOR CHARS OF UDEF1                     
         MVC   BUFCOM,SPACES                                                    
         MVC   BUFCOM(3),ESTU1+22    2ND-4TH CHRAS OF EST UDEF1                 
*                                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         MVC   BUFCOM+20(12),ESTART      ALSO SAVE DATES                        
*                                                                               
         B     PB205E                                                           
*                                                                               
*        CODE BELOW MIGHT BE USED BY FUTURE AGENCIES                            
*        THAT FOLLOW PRINT'S LOGIC MORE CLOSELY                                 
*                                                                               
PB205U   DS    0H                                                               
         MVC   BUFCOM(20),EDESC                                                 
*                                                                               
         MVC   BUFCOM+20(12),ESTART      ALSO SAVE DATES                        
*                                                                               
         MVC   BUFYR,ESTART+1      LAST DIGIT OF YEAR                           
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUFEST(3),DUB                                                    
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
         GOTO1 DATCON,DMCB,(2,2(R4)),(3,WRKDATE)                                
*                                                                               
         L     RF,4(R4)                                                         
         USING MEDDATA,RF                                                       
         L     R0,MEDBYGRS                                                      
         CVD   R0,DUB                                                           
*                                                                               
         DROP  RF                                                               
*        APPLY BILLING FORMULA TO MEDDATA'S VALUES HERE                         
*        FOR TESTING JUST USE GROSS NOW                                         
*                                                                               
PB207    DS    0H                                                               
         MVI   BUFQTR,X'01'                                                     
         CLI   WRKDATE+1,X'03'                                                  
         BNH   PB208                                                            
         MVI   BUFQTR,X'02'                                                     
         CLI   WRKDATE+1,X'06'                                                  
         BNH   PB208                                                            
         MVI   BUFQTR,X'03'                                                     
         CLI   WRKDATE+1,X'09'                                                  
         BNH   PB208                                                            
         MVI   BUFQTR,X'04'                                                     
PB208    MVC   BUFMTH,WRKDATE+1     MONTH                                       
*                                                                               
         MVC   HALF,BUFQTR         SAVE QUARTER                                 
         ZAP   BUFTOTG,DUB                                                      
         L     R1,CSHDSC                                                        
         SR    R0,R1                 R0 SHOULD STILL HAVE GROSS                 
         CVD   R0,DUB                                                           
         ZAP   BUFTOTCD,DUB                                                     
*                                                                               
PB209    ZAP   BUFCHGG,=P'0'         NO CHANGES RECORDED                        
         ZAP   BUFCHGCD,=P'0'                                                   
         ZAP   BUFACTS,=P'0'                                                    
*                                                                               
PB210    GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   BUFMTH,X'FF'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   BUFQTR,X'FF'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
         B     PB300                                                            
*                                                                               
*                ****************************************************           
*                *  SET HIGH-ORDER OF "TEST" YEAR FOR "USER" DATES  *           
*                ****************************************************           
PB2DATE  NTR1                                                                   
         ZIC   RE,TODAY1           HIGH-ORDER OF CURRENT YEAR                   
         CLI   TODAY1+1,C'0'       CURRENT YEAR ENDING IN 0 ?                   
         BNE   PB2D1               NO                                           
         CLI   BUFCOM+21,C'9'      USER YEAR 9 ?                                
         BE    PB2DSUB             YES - SET HIGH-ORDER OF YR "BACK" 1          
         CLI   BUFCOM+21,C'8'      USER YEAR 8 ?                                
         BE    PB2DSUB             YES                                          
         CLI   BUFCOM+21,C'7'      USER YEAR 7 ?                                
         BE    PB2DSUB             YES                                          
         B     PB2DXIT             NO "ADJUSTMENT" NEEDED                       
PB2D1    CLI   TODAY1+1,C'1'       CURRENT YEAR ENDING IN 1 ?                   
         BNE   PB2D2               NO                                           
         CLI   BUFCOM+21,C'9'      USER YEAR 9 ?                                
         BE    PB2DSUB             YES                                          
         CLI   BUFCOM+21,C'8'      USER YEAR 8 ?                                
         BE    PB2DSUB             YES                                          
         B     PB2DXIT             NO "ADJUSTMENT" NEEDED                       
PB2D2    CLI   TODAY1+1,C'2'       CURRENT YEAR ENDING IN 2 ?                   
         BNE   PB2D7               NO                                           
         CLI   BUFCOM+21,C'9'      USER YEAR 9 ?                                
         BE    PB2DSUB             YES                                          
         B     PB2DXIT             NO "ADJUSTMENT" NEEDED                       
*                                                                               
PB2D7    CLI   TODAY1+1,C'7'       CURRENT YEAR ENDING IN 7 ?                   
         BNE   PB2D8               NO                                           
         CLI   BUFCOM+21,C'0'      USER YEAR 0 ?                                
         BE    PB2DADD             YES - SET HIGH-ORDER OF YR "AHEAD" 1         
         B     PB2DXIT             NO "ADJUSTMENT" NEEDED                       
PB2D8    CLI   TODAY1+1,C'8'       CURRENT YEAR ENDING IN 8 ?                   
         BNE   PB2D9               NO                                           
         CLI   BUFCOM+21,C'0'      USER YEAR 0 ?                                
         BE    PB2DADD             YES                                          
         CLI   BUFCOM+21,C'1'      USER YEAR 1 ?                                
         BE    PB2DADD             YES                                          
         B     PB2DXIT             NO "ADJUSTMENT" NEEDED                       
PB2D9    CLI   TODAY1+1,C'9'       CURRENT YEAR ENDING IN 9 ?                   
         BNE   PB2DXIT             NO "ADJUSTMENT" NEEDED                       
         CLI   BUFCOM+21,C'0'      USER YEAR 0 ?                                
         BE    PB2DADD             YES                                          
         CLI   BUFCOM+21,C'1'      USER YEAR 1 ?                                
         BE    PB2DADD             YES                                          
         CLI   BUFCOM+21,C'2'      USER YEAR 2 ?                                
         BE    PB2DADD             YES                                          
         B     PB2DXIT             NO "ADJUSTMENT" NEEDED                       
PB2DADD  LA    RE,1(RE)            SET HI-ORDER OF TEST YEAR "UP" 1             
         B     PB2DXIT             DONE                                         
PB2DSUB  BCTR  RE,0                SET HI-ORDER OF TEST YEAR "BACK" 1           
*****    B     PB2DXIT             DONE                                         
PB2DXIT  DS    0H                                                               
         STC   RE,BUFCOM+20        HI-ORDER OF FROM AND TO TEST DATES           
         STC   RE,BUFCOM+26                                                     
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
PB300    DS    0H                  WAS BILLING ELEMENTS                         
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
*                                  PUT BUFFALO RECS TO TAPE                     
*                                  AT LBUYREQ                                   
*                                  FIRST DO B12 AND B13 RECORDS                 
         ZAP   GTTOTG,=P'0'        FOR GRAND TOTALS                             
         ZAP   GTTOTCD,=P'0'                                                    
         ZAP   GTCHGG,=P'0'                                                     
         ZAP   GTCHGCD,=P'0'                                                    
*                                                                               
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         MVC   OUTREC+200(100),SPACES                                           
         MVC   OUTREC+300(100),SPACES                                           
         MVC   OUTREC+400(100),SPACES                                           
         USING PMRECD,R6                                                        
         MVC   PMAGENCY,MYAGY AGENCY CODE                                       
         MVC   PMMEDIA,=C'NT'                                                   
         CLI   NETOPT,C'N'       NETPAK?                                        
         BE    PUTB4                                                            
         MVC   PMMEDIA,=C'ST'   FOR ALL SPOT MEDIA                              
*                                                                               
PUTB4    MVI   PMRTYPE,C'B'                                                     
         MVC   PMCLIENT,MYCLT                                                   
*                                                                               
         XC    BUFREC,BUFREC                                                    
         B     PUTB100             NO B RECS - SKIP TO A'S                      
         EJECT                                                                  
PUTB100  DS    0H                  FOR A10 AND A13 RECORDS                      
*                                                                               
         USING PMRECD,R6                                                        
         MVC   PMAGENCY,MYAGY AGENCY CODE                                       
         MVC   PMMEDIA,=C'NT'                                                   
         CLI   NETOPT,C'N'       NETPAK?                                        
         BE    PUTB103                                                          
         MVC   PMMEDIA,=C'ST'   FOR ALL SPOT MEDIA                              
*                                                                               
PUTB103  MVI   PMRTYPE,C'A'                                                     
         MVC   PMCLIENT,MYCLT                                                   
*                                                                               
         XC    BUFREC,BUFREC                                                    
         XC    LASTREC,LASTREC                                                  
         MVI   BUFTYPE,C'A'                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     PUTB110                                                          
PUTB105  GOTO1 BUFFALO,DMCB,=C'SEQ',(C'A',BUFFBUFF),BUFREC,0                    
*                                                                               
PUTB110  CLI   DMCB+8,X'80'        END OF FILE                                  
         BE    PUTB200             GO DO A10 AND A13                            
         CLI   BUFTYPE,C'A'                                                     
         BNE   PUTB200             END OF A'S                                   
         CLI   LASTREC,0                                                        
         BNE   PUTB112                                                          
         MVC   LASTREC(8),BUFREC                                                
         B     PUTB113                                                          
PUTB112  CLC   BUFREC(8),LASTREC                                                
         BE    PUTB120                                                          
         CLI   LASTREC,X'FF'       END OF A'S                                   
         BE    *+10                                                             
         MVC   LASTREC,BUFREC                                                   
*                                                                               
         BAS   RE,DISPLAY          PRINT ESTIMATE TOTALS                        
*                                                                               
         BAS   RE,MWRITE                                                        
         CLI   LASTREC,X'FF'                                                    
         BE    PUTB200X                                                         
*                                                                               
PUTB113  MVC   PMEST,BUFYR         LAST DIGIT OF YEAR + EST                     
         MVC   PMPRD,BUFPRD                                                     
         MVC   PMRCODE,=C'60'                                                   
         CLI   NETOPT,C'N'         NETPAK?                                      
         BE    *+8                                                              
         MVI   PMRCODE,C'7'        ALL SPOT MEDIA                               
*                                                                               
PUTB114  MVC   PMACCT(4),BUFCOM       ACCOUNT NUMBER                            
*                                                                               
PUTB114D MVC   PMESTD(20),BUFCOM       EST NAME (INCLUDES ACCT NO.)             
*NOP*    MVC   PMSTART(12),BUFCOM+20   EST START AND END                        
         GOTO1 DATCON,DMCB,(0,BUFCOM+20),(X'20',PMSTART)    EST START           
         GOTO1 DATCON,DMCB,(0,BUFCOM+26),(X'20',PMEND)      EST END             
*NOP*PUTB114X MVC   PMTDATE,TODAY1                                              
PUTB114X GOTO1 DATCON,DMCB,(0,TODAY1),(X'20',PMTDATE)                           
*NOP*    MVC   PMRDATE,SVSTART          REVISION DATE - PERIOD START            
         GOTO1 DATCON,DMCB,(0,SVSTART),(X'20',PMRDATE)                          
         LA    R2,PMMONTHS                                                      
         LA    R1,51                                                            
PUTB115  ZAP   0(6,R2),=P'0'                                                    
         LA    R2,6(R2)                                                         
         BCT   R1,PUTB115                                                       
*                                                                               
PUTB120  L     R2,ADISPTAB         FIND DISPLACEMENT INTO PMMONTHS              
         LA    R1,17               FOR BCT                                      
PUTB125  CLC   BUFQTR(2),0(R2)                                                  
         BE    PUTB130                                                          
         LA    R2,4(R2)                                                         
         BCT   R1,PUTB125                                                       
*                                                                               
         CLI   QOPT6,C'Y'          SEE IF TESTING                               
         BE    PUTB130             THEN DON'T DIE                               
         DC    H'0'                FATAL ERROR                                  
*                                                                               
PUTB130  LA    R1,PMMONTHS                                                      
         LH    R0,2(R2)                                                         
         AR    R1,R0               ADD DISPLACEMENT                             
         CP    0(6,R1),=P'0'       NOTHING SHOULD BE HERE                       
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
         CP    6(6,R1),=P'0'                                                    
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
         CP    12(6,R1),=P'0'                                                   
         BE    *+6                                                              
         DC    H'0'                FATAL ERROR                                  
         ZAP   0(6,R1),BUFTOTG                                                  
         ZAP   12(6,R1),BUFTOTCD                                                
         ZAP   6(6,R1),BUFCHGG                                                  
*                                                                               
         B     PUTB105                                                          
*                                                                               
PUTB200  CLI   LASTREC,0           SEE IF I DID ANY RECORDSS                    
         BE    PUTB200X            NO - THEN DONE                               
         MVI   LASTREC,X'FF'       GO FINISH LAST REC                           
         B     PUTB112                                                          
*                                                                               
PUTB200X MVC   P1+2(16),=C'**REPORT TOTAL**'                                    
         EDIT  GTTOTG,(14,P1+20),2,COMMAS=YES,FLOAT=-                           
         EDIT  GTTOTCD,(14,P1+37),2,COMMAS=YES,FLOAT=-                          
*****    EDIT  GTCHGG,(14,P1+91),2,COMMAS=YES,FLOAT=-                           
*****    EDIT  GTCHGCD,(14,P1+108),2,COMMAS=YES,FLOAT=-                         
         MVI   P1+34,C'*'                                                       
         MVI   P1+51,C'*'                                                       
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
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
TOTALS   DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         L     R4,ATITLES                                                       
         L     R3,ALENTAB                                                       
         LA    R6,3                FOR BCT                                      
TOT2     MVC   P1+7(17),0(R4)                                                   
         EDIT  (P4,5(R3)),(9,P1+26),0,COMMAS=YES                                
         BAS   RE,MYRPT                                                         
         LA    R4,17(R4)                                                        
         LA    R3,9(R3)                                                         
         BCT   R6,TOT2                                                          
         BAS   RE,MYRPT            SKIP A LINE                                  
         MVC   P1+7(13),=C'TOTAL RECORDS'                                       
         EDIT  TOTCNT,(9,P1+26),0,COMMAS=YES                                    
         MVI   P1+35,C'*'                                                       
         BAS   RE,MYRPT                                                         
*                                                                               
         CLI   TAPESW,C'Y'          SEE IF PRODUCING A TAPE                     
         BNE   EXIT                                                             
*                                                                               
         CLOSE (OUTFILE,)                                                       
         B     EXIT                                                             
         EJECT                                                                  
         DS    XL700    FOR NOW - JUST SO MWRITE AND MYPRT CAN ARE              
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
MWRITE2  MVC   WORK(1),PMRTYPE                                                  
         MVC   WORK+1(2),PMRCODE                                                
         L     R1,ALENTAB                                                       
MWRITE4  CLC   0(3,R1),WORK                                                     
         BE    MWRITE5                                                          
         LA    R1,9(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   MWRITE4                                                          
         DC    H'0'                UNKNOWN TYPE                                 
*                                                                               
MWRITE5  MVC   HALF,3(R1)                                                       
         AP    5(4,R1),=P'1'                                                    
         LH    R3,HALF                                                          
         LA    R3,4(R3)                                                         
         STH   R3,OUTREC-4                                                      
         CLI   QOPT7,C'P'                                                       
         BNE   WRIT2                                                            
         MVC   P1(125),OUTREC                                                   
         MVC   P2(125),OUTREC+125                                               
         MVC   P3(125),OUTREC+250                                               
         OI    P2,X'01'                                                         
         OI    P3,X'01'                                                         
         GOTO1 HEXOUT,DMCB,OUTREC-4,P4+10,54,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+50,P5+18,50,=C'N'                               
         GOTO1 (RF),(R1),OUTREC+100,P6+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+150,P7+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+200,P8+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+250,P9+18,50,=C'N'                              
         GOTO1 (RF),(R1),OUTREC+300,P10+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+350,P11+18,50,=C'N'                             
         GOTO1 (RF),(R1),OUTREC+400,P12+18,50,=C'N'                             
         MVC   P4+1(7),=C'001-050'                                              
         BAS   RE,MYRPT                                                         
WRIT2    DS    0H                                                               
         CLI   QOPT6,C'Y'       SEE IF TEST RUN                                 
         BE    WRIT3            THEN NO TAPE                                    
         LA    R1,OUTFILE                                                       
         LA    R0,OUTREC-4                                                      
         PUT   (1),(0)                                                          
WRIT3    AP    TOTCNT,=P'1'                                                     
         XIT1                                                                   
*                                                                               
MYRPT    NTR1                                                                   
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
         MVI   RCSUBPRG,20         FOR PRD/EST RECAP                            
         MVC   P1+2(3),PMPRD                                                    
         MVC   P1+7(4),PMEST                                                    
         LA    R6,PMMONTHS+TOTDISP                                              
DPLAY5   EDIT  (P6,0(R6)),(14,P1+20),2,COMMAS=YES,FLOAT=-                       
         EDIT  (P6,12(R6)),(14,P1+37),2,COMMAS=YES,FLOAT=-                      
****     EDIT  BUFCHGG,(14,P1+91),2,COMMAS=YES,FLOAT=-                          
****     EDIT  BUFCHGCD,(14,P1+108),2,COMMAS=YES,FLOAT=-                        
****     CLI   BUFPUB,X'FF'                                                     
****     BNE   DPLAY8                                                           
         AP    GTTOTG,0(6,R6)                                                   
         AP    GTTOTCD,12(6,R6)                                                 
****     MVI   P1+29,C'*'                                                       
****     MVI   P1+48,C'*'                                                       
****     MVI   P1+105,C'*'                                                      
****     MVI   P1+122,C'*'                                                      
         MVI   SPACING,2                                                        
DPLAY8   BAS   RE,MYRPT                                                         
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
CSHDSC   DC    F'0'       DUMMY SINCE SPOT/NET DON'T HAVE                       
         EJECT                                                                  
*        AGENCY TABLE                                                           
*                                                                               
*        AGENCY CODE/VENDOR/COMPANY/MYUSER VALUE                                
*                                   E=ESTIMATE USER FIELDS                      
*                                                                               
AGYTAB   DC    C'H9',C'310771',C'10PM',C'E'                                     
         DC    X'FFFF'                                                          
         EJECT                                                                  
*                                                                               
OUTFILE  DCB   DDNAME=SPTTAPE,DSORG=PS,RECFM=VB,LRECL=408,             X        
               BLKSIZE=4084,MACRF=PM                                            
*                           NETWORK UNIT PROCESSING                             
PROCNET  CSECT                                                                  
         NMOD1 0,PROCNET                                                        
*                                                                               
*        RA AND R9 FOR SPWORKD                                                  
*        R8 FOR SPPTWRKD                                                        
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
         MVI   BUFTYPE,C'A'                                                     
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
*                                                                               
         MVI   CKESTREC,C'U'         SET FROM UNIT                              
         GOTO1 =A(CKEST)                                                        
*                                                                               
         CLI   MYUSER,C'E'                                                      
         BNE   PROCN1                                                           
*                                                                               
         CLI   ESTU1+25,C'X'         MEANS SKIP THIS ESTIMATE                   
         BE    NTU10                                                            
*                                                                               
PROCN1   DS    0H                                                               
         CLI   MYUSER,C'E'           SEE IF USING USER FIELDS                   
         BNE   PROCN3                                                           
         MVC   BUFYR(4),ESTU1+21     1ST FOR CHARS OF UDEF1                     
         MVC   BUFCOM,SPACES                                                    
         MVC   BUFCOM(3),ESTU1+22    2ND-4TH CHRAS OF EST UDEF1                 
*                                                                               
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         MVC   BUFCOM+20(12),ESTART      ALSO SAVE DATES                        
*                                                                               
         B     PROCN5                                                           
*                                                                               
*        CODE BELOW MIGHT BE USED BY FUTURE AGENCIES                            
*        THAT FOLLOW PRINT'S LOGIC MORE CLOSELY                                 
*                                                                               
PROCN3   DS    0H                                                               
         MVC   BUFCOM(20),EDESC                                                 
*                                                                               
         MVC   BUFCOM+20(12),ESTART      ALSO SAVE DATES                        
*                                                                               
         MVC   BUFYR,ESTART+1      LAST DIGIT OF YEAR                           
         ZIC   R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BUFEST(3),DUB                                                    
*                                                                               
         DROP  RF                                                               
*                                                                               
PROCN5   DS    0H                                                               
         MVC   SVNUPRD2,NUPRD2       SAVE SECOND PRODUCT                        
*                                                                               
         GOTO1 DATCON,DMCB,(2,NUKDATE),(3,WRKDATE)                              
*                                                                               
*        GET NET ORDERED TIME+INTEG+SPECIAL                                     
*        GET 15 % OF GROSS ORDERED TIME+INTEG+SPECIAL                           
*        THEIR PRD AAA FORMULA IS NET 15% OF GROSS                              
*                                                                               
*                                                                               
         PRINT GEN                                                              
         GOTO1 ANETACC,DMCB,(38,NEACCNET),NETBLOCK,0                            
         GOTO1 ANETACC,DMCB,(28,NEACCGRS),NETBLOCK,(2,1500)                     
         PRINT NOGEN                                                            
*                                                                               
         ZAP   MYDUB,NEACCNET+1(8)                                              
         AP    MYDUB,NEACCGRS+1(8)                                              
*                                                                               
*        DO FIRST PRODUCT                                                       
*                                                                               
         L     R5,VCLIST       EXPANDED PRODUCT LIST                            
         LA    R4,252           WAS 220                                         
*                                                                               
NTU12A   CLC   3(1,R5),NUPRD                                                    
         BE    NTU12B                                                           
         LA    R5,4(R5)                                                         
         BCT   R4,NTU12A                                                        
         DC    H'0'         CAN'T FIND PRODUCT                                  
*                                                                               
NTU12B   MVC   BUFPRD,0(R5)                                                     
         L     R4,ANETBLK        RESET R4                                       
*                                                                               
         CVB   R0,MYDUB                                                         
         CLI   NUPRD2,0          DO I HAVE A SECOND PRD?                        
         BE    NTU12BX                                                          
*                                                                               
         ST    R0,WK                                                            
         SR    RF,RF                                                            
         ICM   RF,3,NUP1SHR                                                     
         BAS   RE,SETSHR           GET SHARE FOR THIS PRD                       
         L     R0,WK                                                            
*                                                                               
NTU12BX  DS    0H                                                               
*                                                                               
         DS    0H                                                               
         MVI   BUFQTR,X'01'                                                     
         CLI   WRKDATE+1,X'03'                                                  
         BNH   NTU12C                                                           
         MVI   BUFQTR,X'02'                                                     
         CLI   WRKDATE+1,X'06'                                                  
         BNH   NTU12C                                                           
         MVI   BUFQTR,X'03'                                                     
         CLI   WRKDATE+1,X'09'                                                  
         BNH   NTU12C                                                           
         MVI   BUFQTR,X'04'                                                     
NTU12C   MVC   BUFMTH,WRKDATE+1     MONTH                                       
*                                                                               
         MVC   HALF,BUFQTR         SAVE QUARTER                                 
         CVD   R0,DUB                                                           
         ZAP   BUFTOTG,DUB                                                      
         L     R1,=F'0'             ZERO CASH DISCOUNT                          
         SR    R0,R1                                                            
         CVD   R0,DUB                                                           
         ZAP   BUFTOTCD,DUB                                                     
*                                                                               
NTU12E   ZAP   BUFCHGG,=P'0'         NO CHANGES RECORDED                        
         ZAP   BUFCHGCD,=P'0'                                                   
         ZAP   BUFACTS,=P'0'                                                    
*                                                                               
NTU12F   GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   BUFMTH,X'FF'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVI   BUFQTR,X'FF'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
*                                                                               
         CLI   SVNUPRD2,0          DO I HAVE A SECOND PRD?                      
         BE    NTU10               NO - THEN FINISHED                           
         CLI   NUPRD2,0            MEAN I DID IT ALREADY                        
         BE    NTU10                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,NUP1SHR        1ST PRD SHARE                                
         SH    RF,=H'10000'        COMPLEMENT                                   
         LCR   RF,RF                                                            
         CVB   R0,MYDUB                                                         
         ST    R0,WK                                                            
         BAS   RE,SETSHR           GET SHARE FOR THIS PRD                       
         L     R0,WK                                                            
*                                                                               
*              MUST GET PRD CODE FROM CLIENT HEADER                             
NTU18A   L     R5,VCLIST         EXPANDED PRODUCT LIST                          
         LA    R4,252            NEW MAXIMUM                                    
NTU18B   CLC   3(1,R5),NUPRD2                                                   
         BE    NTU18C                                                           
         LA    R5,4(R5)                                                         
         BCT   R4,NTU18B                                                        
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
BILL2    DS    0H                                                               
         LA    R6,OUTREC                                                        
         MVC   OUTREC(100),SPACES                                               
         MVC   OUTREC+100(100),SPACES                                           
         MVC   OUTREC+200(100),SPACES                                           
         MVC   OUTREC+300(100),SPACES                                           
         MVC   OUTREC+400(100),SPACES                                           
         USING PMRECD,R6                                                        
         MVC   PMAGENCY,MYAGY AGENCY CODE                                       
         MVC   PMMEDIA,=C'NT'                                                   
         CLI   NETOPT,C'N'       NETPAK?                                        
         BE    BILL2B                                                           
         MVC   PMMEDIA,=C'ST'   FOR ALL SPOT MEDIA                              
*                                                                               
BILL2B   MVI   PMRTYPE,C'C'                                                     
         MVC   PMCLIENT,MYCLT                                                   
         L     RF,ADEST                                                         
         USING ESTHDR,RF                                                        
         MVC   PMEST(1),ESTART+1   LAST DIGIT OF YEAR                           
         DROP  RF                                                               
         L     RF,ADBILL                                                        
         USING BILLREC,RF                                                       
         ZIC   R0,BKEYEST                                                       
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PMEST+1(3),DUB                                                   
*                                                                               
         CLI   MYUSER,C'E'          SEE IF USING USER FIELDS                    
         BNE   BILL2C                                                           
         MVC   PMEST,ESTU1+21       1ST 4 CHARS OF EST UDEF1                    
         B     BILL2D                                                           
*                                                                               
BILL2C   DS    0H                                                               
*                                                                               
BILL2D   MVC   PMPRD+1(3),BKEYPRD     ONLY FOR C62 OR C72 REC                   
*                                                                               
******   CLI   MYUSER,C'Y'         IF USING USER FIELDS                         
******   BNE   *+10                                                             
******   MVC   PMPRD+1(3),PRDU1+21                                              
*                                     PUT PRD IN COL 27 NOT 26                  
         MVC   PMRCODE,=C'62'      FIRST DO PRD BILL                            
         CLI   NETOPT,C'N'      NETPAK?                                         
         BE    *+8              YES                                             
         MVI   PMRCODE,C'7'       FOR ALL SPOT MEDIA                            
*                                                                               
BILL2D5  ZAP   PMIGRS,BACTP        RECEIVABLE (WAS GROSS - BGRSP)               
         ZAP   PMIGLCD,BACTP       SAME SINCE NO CD                             
*                                                                               
         CLI   MYUSER,C'E'                                                      
         BNE   *+20                                                             
         MVC   PMIINVNO(1),DINVFULL                                             
         MVC   PMIINVNO+1(7),DINVFULL+2   SKIPS FIRST C'-'                      
         B     *+10                                                             
*                                                                               
         MVC   PMIINVNO(10),DINVFULL    FULL INV NUMBER FROM PPFMTINO           
*                                                                               
         GOTO1 DATCON,DMCB,(0,BQDATE),(X'20',PMIDATE)                           
         L     RF,ADBILL                                                        
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(X'20',PMIDDATE)                        
         L     RF,ADBILL                                                        
*                                                                               
         CLI   QOPT5,C'Y'          SEE IF LISTING CUR INVOICES                  
         BNE   POSTB8                                                           
         MVC   P1+3(3),BKEYPRD                                                  
         MVC   P1+8(3),=C'000'                                                  
         ZIC   R0,BKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+8(3),DUB                                                      
*                                                                               
         CLI   MYUSER,C'E'         USING ESTIMATE USER FIELDS?                  
         BNE   INVR10                                                           
         MVC   P1+8(4),ESTU1+21    FIRST 4 CHARS OF EST UDEF1                   
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
         CLI   MYUSER,C'E'                                                      
         BNE   *+20                                                             
         MVC   P1+28(1),DINVFULL                                                
         MVC   P1+29(7),DINVFULL+2      SKIPS FIRST C'-'                        
         B     *+10                                                             
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
*                                                                               
         DROP  RF                                                               
         EJECT                                                                  
BILL4    DS    0H                                                               
         BAS   RE,MWRITE                                                        
*                                                                               
         B     BILLX    JIM SAYS THEY DON'T NEED C63 NOR C73                    
*****                                                                           
*****          NOW DO C13 RECORD                                                
*****                                                                           
*****    MVC   PMPRD+1(3),SPACES       SAME AS C12 OR C22 BUT NO PRD            
*****                                  PRODUCT WAS IN COL 27 NOT 26             
*****    MVC   PMRCODE,=C'13'                                                   
*****    CLI   PBILKMED,C'N'                                                    
*****    BE    BILL7                                                            
*****    MVI   PMRCODE,C'2'         MAGS,SUPP,TRADE                             
*****    CLI   PBILKMED,C'O'        SEE IF OUTDOOR                              
*****    BNE   BILL7                                                            
*****    MVI   PMRCODE,C'3'                                                     
*                                                                               
BILLX    XIT1                                                                   
*                                                                               
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
         CLI   CKESTREC,C'L'        FROM BILL                                   
         BE    CKEST3                                                           
*                                                                               
         DROP  RF                                                               
*                                                                               
         L     R4,ANETBLK                                                       
         USING NETBLOCK,R4                                                      
         L     RF,NBAIO                                                         
         USING NURECD,RF                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),NUKAM                                                   
         MVC   KEY+2(2),NUKCLT                                                  
         MVC   KEY+4(3),=C'POL'     PRODUCT POL                                 
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
         CLI   QOPT6,C'Y'           SEE IF TEST RUN                             
         BE    CKEST50                                                          
         MVC   P2(23),=C'*** REQUEST STOPPED ***'                               
         GOTO1 REPORT                                                           
         DC    H'0'              MUST DIE                                       
*                                                                               
*                                                                               
CKEST50  DS    0H                                                               
*                                                                               
CKEST80  MVC   KEY,PPGKEY                                                       
         MVC   AREC,PPGAREC                                                     
         CLI   CKESTSW,1           SEE IF I READ SOMETHING                      
         BNE   CKESTX              NO - SKIP READ HIGH                          
         GOTO1 HIGH                                                             
CKESTX   XIT1                                                                   
*                                                                               
         DROP  RF                                                               
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
         DC    CL17'NETWORK    A60'                                             
         DC    CL17'           C62'                                             
         DC    CL17'              '    TO SKIP A LINE                           
*                                                                               
TITLES   CSECT                                                                  
         DS    0C                                                               
         DC    CL17'SPOT       A70'                                             
         DC    CL17'           C72'                                             
         DC    CL17'              '    TO SKIP A LINE                           
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
*                                  TABLE OF RECORD LENGHTS                      
NLENTAB  CSECT                                                                  
* NETWORK                                                                       
         DC    CL3'A60',AL2(PMA60X-PMKEY),PL4'0'                                
         DC    CL3'C62',AL2(PMC62X-PMKEY),PL4'0'                                
         DC    XL5'00',PL4'0'      EXTRA LINE                                   
* SPOT                                                                          
LENTAB   CSECT                                                                  
         DC    CL3'A70',AL2(PMA70X-PMKEY),PL4'0'                                
         DC    CL3'C72',AL2(PMC72X-PMKEY),PL4'0'                                
         DC    XL5'00',PL4'0'      EXTRA LINE                                   
         DC    X'FFFF'                                                          
         EJECT                                                                  
SPPTWRKD DSECT                                                                  
NETOPT   DS    CL1                                                              
TOTCNT   DS    PL4'0'                                                           
MYDUB    DS    PL8                                                              
SVNUPRD2 DS    XL1          SAVED NUPRD2                                        
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
WRKDATE  DS    XL3                                                              
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
WORK2    DS    CL64                                                             
ADISPTAB DS    A                                                                
ALENTAB  DS    A                                                                
ATITLES  DS    A                                                                
VGETUSER DS    A                                                                
VPROCNET DS    A                                                                
ANETBLK  DS    A                                                                
ANETNET  DS    A                                                                
ANETACC  DS    A                                                                
AFMTINO  DS    A                                                                
ACONIO1  DS    A                                                                
*                                                                               
MYAGY    DS    CL6       SET FROM AGYTAB AT FBUYREQ                             
MYCLT    DS    CL4       SET FROM AGYTAB AT FBUYREQ                             
MYUSER   DS    CL1       SET FROM AGYTAB AT FBUYREQ                             
*                                                                               
ESTU1    DS    CL54      USER FIELDS                                            
ESTU2    DS    CL38                                                             
PRDU1    DS    CL54                                                             
*                                                                               
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
GTTOTG   DS    PL8                 REPORT TOTALS                                
GTTOTCD  DS    PL8                                                              
GTCHGG   DS    PL8                                                              
GTCHGCD  DS    PL8                                                              
*                                                                               
TODAY1   DS    CL6                                                              
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
LASTREC  DS    CL8                                                              
APPBYOWK DS    A                                                                
         DS    F                                                                
*                                                                               
BUFREC   DS    0CL103                                                           
BUFKEY   DS    0CL23                                                            
BUFTYPE  DS    CL1                 A OR B                                       
BUFMED   DS    CL1                 MEDIA                                        
BUFYR    DS    CL1                 LAST DIGIT OF YEAR                           
BUFEST   DS    CL3                 EST                                          
BUFPRD   DS    CL3                 PRODUCT                                      
BUFPUB   DS    CL8                 PUB (FOR B TYPE ONLY)                        
BUFPSUF  DS    CL1                 SUFFIX (FOR NEWS - NOT USED YET)             
BUFPZONE DS    CL2                 OUTDOOR ZONE                                 
BUFQTR   DS    CL1                 QUARTER NO.   (FOR A TYPE ONLY)              
*              BUFQTR=X'FF' FOR GRAND TOTAL                                     
*              ELSE BINARY 01-04                                                
BUFMTH   DS    CL1                 MONTH (FOR A TYPE ONLY)                      
*              BUFYM=X'FF'  FOR QUARTER TOTAL                                   
         DS    CL1                 SPARE                                        
*                                                                               
BUFCOM   DS    CL40                COMMENT                                      
*                                                                               
*                                                                               
*              THESE FIELDS FOR  TYPE B BUFFALO RECS                            
*                       AND FOR  TYPE A BUFFALO RECS                            
*                                                                               
BUFCHGG  DS    PL8                 TOTAL OF B10'S OR B20'S  GROSS               
BUFCHGCD DS    PL8                 TOTAL OF B10'S OR B20'S  GROSS-CD            
BUFTOTG  DS    PL8                 FILE TOTAL - GROSS                           
BUFTOTCD DS    PL8                 FILE TOTAL - GROSS - CD                      
BUFACTS  DS    PL8                                                              
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
         BUFF  LINES=4000,ROWS=1,COLUMNS=5,FLAVOR=PACKED,COMMENT=40,KEYX        
               LIST=(23,A)                                                      
*                                                                               
PPBYOWRK CSECT                                                                  
         DS    CL600                                                            
         DC    X'0000'                                                          
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
       ++INCLUDE PMINTFCON                                                      
QOPT6    EQU   QGRP                                                             
QOPT7    EQU   QGRP+1                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPPP02 02/09/15'                                      
         END                                                                    
