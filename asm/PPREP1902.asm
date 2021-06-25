*          DATA SET PPREP1902  AT LEVEL 061 AS OF 04/19/16                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 046711.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE PP1902A                                                                  
*INCLUDE PPRTLOOK                                                               
*INCLUDE DATVAL                                                                 
*INCLUDE GETADVC                                                                
*INCLUDE PPGETCU                                                                
*INCLUDE PPGETADR                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PP1902 - PRINT CONTRACT UTILIZATION'                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SMYE 04/06    DISPLAY "S" BEFORE DATE FOR STEWARDSHIP BUYS                    
*                                                                               
* KWAN 06/11/02 CU LOOK UP, FIX TOTAL PREMIUM                                   
*                                                                               
*  SMYE 05/00     CHANGES FOR LARGER PPNEWFILE                                  
*                                                                               
*  SMYE 03/00     ADD THE DISPLAY OF ADDRESS, EMAIL, FAX AND TELEPHONE          
*                 (NEW CSECT CREATED - COMPRT)                                  
*                                                                               
*  BPLA 11/16/99  FIX BUG - IN PBUY - DON'T TOTAL                               
*                 PREMIUMS IF TESTPASS =1                                       
*                                                                               
*  BPLA 9/2/99    MODIFY PREMIUM CHARGE TOTAL ANNOTATION                        
*                                                                               
*  KWAN 06/09/99  EXCLUDE PREMIUM CHARGES FROM ANALYSIS                         
*                                                                               
*  KWAN 03/25/99  EXPAND PL12 FILEDS TO PL14 TO HANDLE LARGE PACK NUMBS         
*                                                                               
*  SMYE 10/30/98  NO-OP MEDIA N EXCEPTION TO RATES-BY-PRODUCT DISPLAY           
*                 IN PROC CON58Z                                                
*                                                                               
*  BPLA  9/98     DO SAME NEW ANALYSIS MESSAGE FOR TIMES                        
*                 (LEVEL = X), PAGE (LEVEL=P) AND ISSUE (LEVEL=U)               
*                 CONTRACTS                                                     
*                 ALSO FIX MESSAGES FOR 1 LINE/INCH/TIME                        
*                 CHG "LINES" TO "LINE" AND "INCHES" TO "INCH", ETC.            
*                                                                               
*  SMYE 06/98     ROUND UP PERCENT LEVEL AT ANALMSG....                         
*                                                                               
*  SMYE 05/98     ADD PUBLISHER TO HEADLINES AND ADD P=ALL OR P=NNNN            
*                   IN QPUB+1 FOR "PUBLISHER" CONTRACTS ONLY                    
*                                                                               
*  SMYE 04/98     SHOW LEVEL "DIFFERENCES" IN DOLLARS, LINES, OR INCHES         
*                   AFTER UTILIZATION CALCULATION AT ANALMSG...                 
*                                                                               
*  BPLA  3/98     ADVCTAB EXPANDED FROM 4000 TO 6000 BYTES                      
*                                                                               
*  SMYE 12/97     ADDED QOPT9=H TO BYPASS "HOLD" SFH BUYS                       
*                 CHANGED QOPT1-1 TO QOPT8 TO ALLOW FOR CONTINUATION            
*                    REQUEST CARD                                               
*                 DISPLAY 'H' BEFORE BUY DATE FOR HOLD BUYS                     
*                   (OVERRIDDEN BY 'T' IF BUY BOTH TEST AND HOLD)               
*                                                                               
*  BPLA 8/96      EXPAND ADVCTAB TO 4000 BYTES FROM 2000                        
*                                                                               
*  BPLA 1/96      IF PRBLIND = "N" (NET $) DISPLAY "N$" BEFORE LEVEL            
*                                                                               
*  SMYE 12/06/95  CHANGED DTCNV TO DATCON WITH "NEW" PARAM'S                    
*                                                                               
*  BPLA 5/95      ALWAYS DISPLAY EITHER /I OR /L FOR UNIT RATES                 
*                 IF NO NEWSPAPER SPACE DISPLAY PBYOUNTS IN BDESC               
*                 AS WELL AS IN BLINES                                          
*                                                                               
*  BPLA 3/95      HANDLE INCH RATES FOR NON-INCH LEVEL INDS                     
*                                                                               
*  SMUR 07/07/94  CALCULATE CONTRACT UTILIZATION USING NET $                    
*                 FOR NET CONTRACTS WHOSE LVL IND ='$'                          
*                                                                               
*  BPLA 11/22/93  EXPAND SORTTAB FOR 2000 BUYS AND USE MAXSORT                  
*                 FOR MAXIMUM CHECKING                                          
*                                                                               
*  BPLA 9/28/93   FIX TRUNCATION OF BIG TOTALS                                  
*                                                                               
*  BPLA 9/22/93   SKIP TO NEXT AOR AGY IF PUB LINK NOT FOUND                    
*                                                                               
*  BPLA 8/11/93   CHANGES TO USE PPGETCU FOR CU DATA                            
*                                                                               
*                                                                               
*  BPLA 6/7/93    AOR AGENCY FILTER IN HEADLINES FOR AU                         
*                                                                               
*  BPLA 6/3/93    FOR PAU ADD AGENCY FILTER (IN QESTEND)                        
*                                                                               
*  BPLA 3/15/93   FIX HEADLINES FOR AU REPORT                                   
*                                                                               
*  BPLA 12/22/92  FIXES TO ACROSS EDITION SUMMARY                               
*                 DON'T DISPLAY PERCENT AND RATES                               
*                 AND CHANGE LEVEL MESSAGES                                     
*         NOTE -  MODE WILL BE LBUYREQ WHEN DOING ACROSS EDT SUMMARY            
*                                                                               
*        19= INSERTION/CONTRACT LISTING (CONTRACT UTILIZATION REPORT)           
*        AU= ADVERTISER CONTRACT UTILIZATION REPORT                             
*                                                                               
* REQUEST RULES                                                                 
*                                                                               
*  PUB   DATE  CON NUM                                                          
*  ---   ----  -------                                                          
*                                                                               
*  ALL    YES   NO                                                              
*                                                                               
*  YES    YES   NO                                                              
*                                                                               
*  YES    NO    YES                                                             
         SPACE 2                                                                
*                                                                               
*                                                                               
*        QPUB+1 MAY HAVE P=ALL OR P=NNNN FOR SELECTING PUBLISHER                
*                 PUBS ONLY - RULES ARE LIKE THOSE FOR QPUB=ALL                 
*                                                                               
*        QESTEND MAY HAVE AGENCY FILTER FOR PAU                                 
*                                                                               
*        QPAY(3) (COL 53) CHANGE CONTROL DATE                                   
*                                                                               
*        QOPT1-3 (COL 59) P=PRD,D=INS DATE SORT (OVERRIDES PROFILE)             
*        QOPT1-2 (COL 60) Y=SUPPRESS NET                                        
* *NOTE* QOPT1-1 (COL 61) Y=SUPPRESS DETAILS                                    
* *NOTE*       (CHANGED TO QOPT8 - SEE BELOW)                                   
*                                                                               
*        QOPT1 Y=CONTRACT AND REQ END MTHS MUST MATCH                           
*        QOPT2 Y=PRINT FREE FORM COMMENTS                                       
*                                                                               
*        QOPT3 L=SHOW LOWER LEVEL WITH CURRENT                                  
*              H=SHOW HIGHER LEVEL WITH CURRENT                                 
*              B=SHOW BOTH WITH CURRENT                                         
*              BLANK = SHOW NEITHER WITH CURRENT                                
*                                                                               
*        QOPT4 Y=SHOW ONLY CHANGED CONTRACTS                                    
*                                                                               
*        QOPT5 Y=SKIP CONTRACTS IF CLT HAS NO BUYS ON PUB                       
*                                                                               
*        QOPT6 Y=INCLUDE TEST BUYS (19T REQUESTS)                               
*                                                                               
*        QOPT7  SET BUY PPG FOR 'SORTED' REQUESTS                               
*              F=FIRST REQ, C=CONTINUATION                                      
*                                                                               
*        QOPT8 Y=SUPPRESS DETAILS                                               
*                                                                               
*        QOPT9 H=EXCLUDE HELD BUYS (SFH)                                        
*************                                                                   
*                                                                               
*        PROFILE (PROGPROF)                                                     
*                                                                               
*        +0    Y=ONE CONTRACT/PAGE (DEFAULT)                                    
*              N=DON'T SKIP TO NEW PAGE FOR EACH CONTRACT/PUB                   
*                                                                               
*        +1    Y=EQUIVALENCE LINES TO INCHES AND VICE VERSA (DEFAULT)           
*              N=DON'T                                                          
*                                                                               
*        +2    Y=PRINT ANALYSIS MESSAGE (DEFAULT)                               
*              N=DON'T                                                          
*                                                                               
*        +3    Y=INCLUDE TEST BUYS IN TOTALS FOR 19T REQS (DEFAULT)             
*              N=DON'T                                                          
*                                                                               
*        +4    P=PRODUCT SORT (DEFAULT)                                         
*              D=INSERTION DATE SORT                                            
*        +5    FIRST $ COLUMN   G=GROSS,N=NET,C=CD,1=G-CD,2=N-CD                
*                               BLANK=NO DATA                                   
*                                                                               
*        +6    2ND   $ COLUMN   G=GROSS,N=NET,C=CD,1=G-CD,2=N-CD                
*                               BLANK=NO DATA                                   
*                                                                               
*        +7    3RD   $ COLUMN   G=GROSS,N=NET,C=CD,1=G-CD,2=N-CD                
*                               BLANK=NO DATA                                   
*                                                                               
         SPACE 2                                                                
         EJECT                                                                  
PP1902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP1902,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         MVI   RC2DSECT,C'Y'          SECOND DSECT                              
         L     R9,PPWORK2C                                                      
         USING PPWORK2D,R9                                                      
         LA    R8,SPACEND                                                       
         USING PP19WRKD,R8                                                      
*                                                                               
         MVC   ACONIO1,ACONIO      GET & STORE A(PCONREC)IN ACONIO1             
*                                                                               
         LA    R7,PP1902+4095                                                   
         LA    R7,1(R7)                                                         
         USING PP1902+4096,R7                                                   
*                                                                               
         L     RC,PPFILEC                                                       
         LR    R3,RC                                                            
         AHI   R3,4096                                                          
         USING PPFILED,RC,R3             R3 NEEDED FOR PUBREC ADDR              
*                                                                               
         EJECT                                                                  
RUNF     CLI   MODE,RUNFRST                                                     
         BNE   CON1                                                             
         XC    PUBREC(35),PUBREC                                                
         MVI   LNEED,0                                                          
         MVI   TCONVDTE,0                                                       
         XC    TCONVFAC,TCONVFAC                                                
         MVI   TCONVIND,0                                                       
         MVI   WKDASHES,C'-'                                                    
         MVC   WKDASHES+1(L'WKDASHES-1),WKDASHES                                
*                                                                               
* SET RELOCATABLE ADDRESSES                                                     
*                                                                               
         L     R0,=V(RATELOOK)                                                  
         A     R0,RELO                                                          
         ST    R0,ARTLOOK                                                       
         L     R0,=V(PPGETCU)                                                   
         A     R0,RELO                                                          
         ST    R0,APPGETCU                                                      
         L     R0,=V(DATVAL)                                                    
         A     R0,RELO                                                          
         ST    R0,ADATVAL                                                       
         L     R0,=V(PPGETADR)                                                  
         A     R0,RELO                                                          
         ST    R0,APGETADR                                                      
         L     R0,=A(CLPRT)                                                     
         A     R0,RELO                                                          
         ST    R0,ACLPRT                                                        
         L     R0,=A(COMPRT)                                                    
         A     R0,RELO                                                          
         ST    R0,ACOMPRT                                                       
         L     R0,=A(CONSCHD)                                                   
         A     R0,RELO                                                          
         ST    R0,ACONSCHD                                                      
         L     R0,=A(SORTSCH)                                                   
         A     R0,RELO                                                          
         ST    R0,ASORTSCH                                                      
         L     R0,=A(SORTTAB)                                                   
         A     R0,RELO                                                          
         ST    R0,ASORTTAB                                                      
         L     R0,=A(PRTTOTS)                                                   
         A     R0,RELO                                                          
         ST    R0,APRTTOTS                                                      
         ST    R0,ANEXTSRT                                                      
*                                                                               
         LA    R0,MAXEQU          SET MAX BUYS FOR SORT                         
         ST    R0,MAXSORT                                                       
*                                                                               
         L     R0,=V(GETADVC)                                                   
         A     R0,RELO                                                          
         ST    R0,AGETADVC                                                      
*                                                                               
         L     R0,=A(ADVCTAB)                                                   
         A     R0,RELO                                                          
         ST    R0,AADVCTAB                                                      
         ST    R0,ANEXTAC                                                       
*                                                                               
         L     R0,=A(RTLKELS)                                                   
         A     R0,RELO                                                          
         ST    R0,ARTLKELS                                                      
*                                                                               
         LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
*                                                                               
*                                                                               
         L     RF,UTL                                                           
         MVC   SAVSYS,4(RF)         SAVE MY REAL SE NUMBER                      
*                                                                               
*        OPEN THE CONTROL FILE                                                  
*                                                                               
         MVI   4(RF),X'0A'                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMOPEN',=CL8'CONTROL',GENFILES,DMWORK          
         L     RF,UTL                                                           
         MVC   4(1,RF),SAVSYS                                                   
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
CON1     DS    0H                                                               
         CLI   MODE,LBUYREQ        WILL GET FOR LAST SORTED CONTRACT            
         BNE   CON2                                                             
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
CON2     DS    0H                                                               
         CLI   MODE,FBUYREQ                                                     
         BNE   CON2A                                                            
*                                                                               
         XC    ADRDATA,ADRDATA     CLEAR ALL ADDRESS, TEL, & FAX INFO           
         XC    PADRDATA,PADRDATA                                                
*                                                                               
         CLI   QOPT7,C'C'                                                       
         BE    CON2C                                                            
*                                                                               
         MVI   E18SW,0                                              L02         
         CLI   QOPT3,C' '                                                       
         BE    NOT18R                                                           
         MVC   E18SW,QOPT3                                                      
NOT18R   DS    0H                                                   L02         
*******                                                             L02         
         GOTO1 BUFFALO,DMCB,=C'RESET',BUFFBUFF                                  
*                                                                               
         LA    R2,REQTOTS                                                       
         LA    R1,ACCNUM                                                        
*                                                                               
CONCLR   ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R1,CONCLR                                                        
*                                                                               
         LA    R2,SCONTOTS            SAVED CONTRACT TOTALS                     
         LA    R1,ACCNUM                                                        
*                                                                               
CONCLR5  ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R1,CONCLR5                                                       
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',BUFFBUFF                                  
CON2C    CLI   QPUB,C'0'           IF ONE PUB                                   
         BNL   CON01                                                            
         CLI   QSORT,C' '          OR NO SORT                                   
         BE    CON01               PROC REQ NOW                                 
*                                                                               
*                                                                               
         ZAP   TOTCNT,=P'0'                                                     
         ZAP   TOTPCNT,=P'0'                                                    
         ZAP   LOCKCNT,=P'0'                                                    
         ZAP   CHACNT,=P'0'                                                     
         B     EXIT                ELSE WAIT                                    
*                                                                               
CON01    DS    0H                                                               
         CLI   QOPT7,C'C'          SEE IF CONTINUATION REQ                      
         BE    CON02                                                            
         ZAP   TOTCNT,=P'0'                                                     
         ZAP   TOTPCNT,=P'0'                                                    
         ZAP   LOCKCNT,=P'0'                                                    
         ZAP   CHACNT,=P'0'                                                     
         XC    PUBKMED(7),PUBKMED  TO INSURE PROPER TOTPCNT                     
CON02    DS    0H                                                               
         CLI   QOPT7,C'C'          SEE IF CONTINUATION REQ                      
         BE    *+10                YES - DON'T RESET PAGE                       
CON02C   MVC   PAGE,=H'1'                                                       
CON02E   MVI   FORCEHED,C'Y'                                                    
CON03    DS    0H                                                               
         MVC   SVPROG,QPROG                                                     
         XC    SVRCON,SVRCON                                                    
         CLC   QEST,SPACES                                                      
         BE    CON03D                                                           
         PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
*        STH   R0,SVRCON          SAVE REQUESTED CONTRACT NUMBER    L01         
         STH   R0,DUB                                               L01         
         MVC   SVRCON,DUB                                           L01         
CON03D   XC    PCLTKEY,PCLTKEY                                                  
         MVI   HDSW,0                                                           
         MVI   FCRDBUY,X'21'                                                    
         B     CONCLT                                                           
*                                                                               
CON2A    CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
CON2AX   DS    0H                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
CONCLT   DS    0H                                                               
         CLI   QOPT7,C' '          SEE IF SORTED REQ                            
         BH    *+8                                                              
         MVI   PUBSW,0                                                          
         XC    KEY,KEY                                                          
         CLI   PCLTKEY,0           TEST FIRST TIME                              
         BZ    CONCLT2             YES                                          
*                                                                               
         CLC   QPROG(2),=C'AU'     SEE IF DOING ADVERTISER                      
         BE    CON120              CAN ONLY BE ONE CLIENT                       
*                                                                               
         CLC   =C'ALL',QCLIENT                                                  
         BE    CONCLT1                                                          
         CLI   QCLIENT,C'&&'        GROUP REQUEST                               
         BE    CONCLT1                                                          
         CLI   QCLIENT,C'*'                                                     
         BNE   CON120                                                           
CONCLT1  DS    0H                                                               
         MVC   KEY,PCLTKEY                                                      
         GOTO1 HIGH                                                             
         GOTO1 SEQ                 READ NEXT CLIENT                             
         MVI   FORCEHED,C'Y'                                                    
         B     CONCLT4                                                          
*                                                                               
CONCLT2  MVC   KEY(3),PAGYREC      A/M                                          
         MVI   KEY+3,2                                                          
         CLC   =C'ALL',QCLIENT                                                  
         BE    CONCLT3                                                          
         CLI   QCLIENT,C'&&'        GROUP REQUEST                               
         BE    CONCLT3                                                          
         CLI   QCLIENT,C'*'                                                     
         BNE   CONCLT6                                                          
*                                                                               
CONCLT3  DS    0H                                                               
* ALL CLIENTS                                                                   
         GOTO1 HIGH                                                             
CONCLT4  CLC   KEY(4),KEYSAVE      A/M/X                                        
         BE    CONCLTX                                                          
         B     CON120                                                           
* ONE CLIENT                                                                    
CONCLT6  MVC   KEY+4(3),QCLIENT                                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(7),KEY                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CONCLTX  LA    R0,PCLTREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,GET              GET CLIENT HEADER                            
*                                                                               
         CLI   QCLIENT,C'*'                                                     
         BNE   CONCLTX1                                                         
         CLC   QCLIENT+1(1),PCLTOFF     TEST RIGHT OFFICE                       
         BNE   CONCLT1                                                          
         B     CONCLTX2                                                         
*                                                                               
CONCLTX1 CLI   QCLIENT,C'&&'            GROUP REQUEST                           
         BNE   CONCLTX2                                                         
         CLC   QCLIENT+1(1),PCLTBLGP    TEST RIGHT GROUP                        
         BNE   CONCLT1                                                          
CONCLTX2 DS    0H                                                               
*                                       MUST READ CLIENT PROFILE HERE           
         MVC   PROFKEY,=CL12'P000'                                              
         MVC   PROFKEY+4(3),QAGENCY                                             
         MVC   PROFKEY+7(3),PCLTKCLT                                            
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   PROFKEY+10,C'*'                                                  
         MVC   PROFKEY+11(1),PCLTOFF                                            
         GOTO1 GETPROF,DMCB,PROFKEY,SYSPROF,DATAMGR                             
         MVC   PROFKEY+2(2),QPROG                                               
         GOTO1 (RF),(R1),,PROGPROF                                              
         OC    PROGPROF,PROGPROF                                                
         BNZ   *+10                                                             
         MVC   PROGPROF(12),=C'YYYYPGC2NNNN'     SET DEFAULT VALUES             
*                                           GROSS,CD,NET-CD                     
         MVC   ADRSW(3),PROGPROF+9   ADDRESS, TEL, & FAX OPTIONS                
*                                                                               
         MVC   SORTOPT,PROGPROF+4                                               
         CLI   QOPT1-3,C' '                                                     
         BE    *+10                                                             
         MVC   SORTOPT,QOPT1-3                                                  
         MVC   DOLTYPS,PROGPROF+5                                               
         MVI   DOLTYPS+3,X'FF'      SET END OF LIST                             
         CLI   QOPT1-2,C'Y'         SEE IF SUPPRESSING NET                      
         BNE   CON10                                                            
         LA    R1,DOLTYPS                                                       
         LA    RF,3                                                             
CON09    CLI   0(R1),C'N'            NET                                        
         BE    CON09C                                                           
         CLI   0(R1),C'2'            NET-CD                                     
         BE    CON09C                                                           
         B     CON09X                                                           
*                                                                               
CON09C   MVI   0(R1),C'X'          SUPPRESS NET AND NET-CD                      
CON09X   LA    R1,1(R1)                                                         
         BCT   RF,CON09                                                         
*                                                                               
CON10    XC    BSTART(6),BSTART                                                 
         MVC   BEND,=3X'FF'                                                     
         CLC   QSTART,SPACES       TEST DATE PARAM SPECIFIED                    
         BE    CON10A              NO                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
         GOTO1 DATCON,(R1),(0,QEND),(3,BEND)                                    
*                                                                               
CON10A   CLC   QPUB,SPACES                                                      
         BNE   *+10                                                             
         MVC   QPUB(3),=C'ALL'                                                  
*                                                                               
         B     CON10A4         FOR 19 ALLOW ZZZ AND                             
*                              ONE CONTRACT NUMBER                              
*                                                                               
CON10A3  CLC   QPUB+8(3),=C'ZZZ'   SEE IF DOING ALL ZONES + EDTS                
         BE    CON10A5                                                          
CON10A4  CLC   QPUB(3),=C'ALL'                                                  
         BE    CON10A5                                                          
         CLC   =C'P=',QPUB+1       PUBLISHER OPTION ?                           
         BNE   CON10B              NO                                           
CON10A5  CLC   QEST,SPACES         MUST NOT SPECIFY CONTRACT                    
         BNE   CONERR                                                           
         CLC   QSTART,SPACES       AND MUST NAME DATE                           
         BE    CONERR                                                           
         B     CON12                                                            
CON10B   CLC   QSTART,SPACES       MUST NAME DATE OR CONTRACT                   
         BNE   CON12                                                            
         CLC   QEST,SPACES                                                      
         BNE   CON12                                                            
CONERR   MVC   P1(L'ERRMSG),ERRMSG                                              
         MVC   P2(80),QRECORD                                                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
ERRMSG   DC    C'INVALID PUB/CONTRACT/DATE SPECIFICATION'                       
         SPACE 2                                                                
CON12    XC    KEY,KEY             BUILD CONTRACT KEY                           
         MVC   KEY(7),PCLTKEY      A/M/X/CLT                                    
         MVI   KEY+3,X'10'                                                      
         CLC   =C'ALL',QPUB                                                     
         BE    CON16                                                            
         CLC   =C'P=',QPUB+1       PUBLISHER OPTION ?                           
         BE    CON16               YES - TREAT LIKE ALL                         
         EJECT                                                                  
* ONE PUB                                                                       
         MVC   WORK(11),QPUB                                                    
         CLC   QPUB+8(3),=C'ZZZ'   SEE IF DOING ALL ZONES +EDTS                 
         BNE   CON12B                                                           
         MVC   WORK+8(3),SPACES                                                 
CON12B   GOTO1 PUBVAL,DMCB,WORK,KEY+7                                           
         CLC   QPUB+8(3),=C'ZZZ'                                                
         BE    CON12F                                                           
         CLC   QEST,SPACES         TEST CON SPECIFIED                           
         BNE   CON14               NO                                           
* ONE PUB/ALL CONTRACTS                                                         
* OR NNN,ZZZ AND ONE CONTRACT                                                   
*                                                                               
CON12F   GOTO1 HIGH                                                             
         B     CON13                                                            
*                                                                               
CON12G   GOTO1 SEQ                                                              
*                                                                               
CON13    CLC   KEY(11),KEYSAVE     SAME A/M/X/CLT/BASE PUB                      
         BNE   CONCLT                                                           
         CLC   QPUB+8(3),=C'ZZZ'   SEE IF DOING ALL ZONES + EDTS                
         BE    CON13D              YES                                          
         CLC   KEY(13),KEYSAVE     NO CHK FULL PUB NUMBER                       
         BNE   CONCLT                                                           
CON13D   OC    SVRCON,SVRCON       SEE IF CONTRACT NUMBER SPECIFIED             
         BZ    CON20                                                            
         CLC   KEY+13(2),SVRCON                                                 
         BNE   CON12G              WRONG NUMBER-KEEP LOOKING                    
         B     CON20               GO FILTER ON DATES                           
*                                                                               
* ONE PUB/ONE CONTRACT                                                          
*                                                                               
CON14    PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   KEY+13(2),HALF                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE     SAMEA/M/X/CLT/PUB/CON                        
         BNE   CONCLT                                                           
         B     CON20                                                            
*                                                                               
*ALL PUBS/ALL CONTRACTS                                                         
*                                                                               
CON16    GOTO1 HIGH                                                             
*                                                                               
CON17    CLC   KEY(7),KEYSAVE      TEST SAME A/M/X/CLT                          
         BNE   CONCLT                                                           
         EJECT                                                                  
* GET CONTRACT REC AND FILTER ON DATES                                          
*                                                                               
CON20    DS    0H                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         LA    R0,PCONREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,GET                                                           
*                                                                               
         CLC   PCONEDT,BSTART      CON END BEFORE REQ START                     
         BL    CON92                                                            
         CLC   BEND,PCONSDT        REQ END BEFORE CON START                     
         BL    CON92                                                            
         CLI   QOPT1,C'Y'                                                       
         BNE   CON25                                                            
         CLC   BEND(2),PCONEDT        MTHS MUST MATCH                           
         BNE   CON92                                                            
*                                                                               
CON25    JIF   QPRODUCT,EQ,=C'ALL',OR,QPRODUCT,EQ,=C'   ',CON28,JUMP=N          
         CLI   PCONPRD,C'A'        DOING ONE PRD - SO PASS CONS FOR             
         BL    CON28               ALL PRDS OR MATCHING PRDS                    
         CLC   PCONPRD,QPRODUCT                                                 
         BNE   CON92                                                            
         B     CON28                                                            
*                                                                               
         SPACE 2                                                                
CON28    CLI   QOPT4,C'Y'          SEE IF SHOWING ONLY CHGED CONTRACTS          
         BNE   CON29               NO                                           
         CLC   PCONMOD,QPAY            CHK CHG CONTROL DATE                     
         BL    CON92               SKIP THIS CON                                
*                                                                               
CON29    CLI   QOPT5,C'Y'        SEE IF SKIPPING CON IF NO BUYS                 
         BNE   CON30                                                            
         MVI   TESTPASS,1                                                       
         MVC   BQSTART(6),PCONSTRT                                              
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 ACONSCHD                                                         
         CLI   TESTPASS,1                                                       
         BE    CON92               MEANS NO BUY WAS FOUND                       
*                                  TEST PASS SET TO 0 IF CONSCHD                
*                                  FINDS A BUY                                  
*                                  THEN SKIP THIS CONTRACT                      
         EJECT                                                                  
* GET PUB RECORDS                                                               
CON30    DS    0H                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         CLC   PCONKPUB(6),PUBKPUB                                              
         BNE   CON31                                                            
         CLC   PCONKMED(1),PUBKMED                                              
         BE    CON34                    ALREADY HAVE PUBRECORD                  
CON31    DS    0H                                                               
         XC    SVPPUBL,SVPPUBL                                                  
         MVI   PUBSW,0                                                          
         AP    TOTPCNT,=P'1'       BUMP PUB COUNTER                             
         XC    KEY,KEY                                                          
         MVC   KEY(1),PCONKEY+2    MEDIA                                        
         MVC   KEY+1(6),PCONKPUB                                                
         MVC   KEY+7(2),PCONKAGY                                                
         MVI   KEY+9,X'81'                                                      
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE     CHECK FOUND X'81' REC                        
         BE    CON32               YES                                          
         CLC   KEY+7(2),=C'ZZ'     CHECK FOUND ZZ X'81' REC                     
         BE    CON32               YES                                          
*                                                                               
         MVC   KEYSAVE+7(2),=C'ZZ' TRY FOR DEFAULT                              
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         GOTO1 HIGHPUB                                                          
         CLC   KEY(10),KEYSAVE     MUST FIND DEFAULT                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CON32    LA    R0,PUBREC                                                        
         ST    R0,IOAREA                                                        
         BAS   RE,GETPUB                                                        
         MVC   PUBKED,KEY+6  SET ED CODE IN PUBREC                              
* NOW TRY FOR LTLREC                                                            
* NO-OP LTLREC READ AND REP READ ***                                            
         B     CON34                                                            
CON34    DS    0H                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         LA    R4,P1               ***** POINT R4 TO P1 ******                  
         CLI   PUBPLSH,C' '        ANYTHING IN PUBLISHER ?                      
         BNH   CON34C              NO                                           
         CLC   PUBPLSH,PREPKREP    REP CODE                                     
         BNE   CON34A                                                           
         CLC   PCONKEY(3),PREPKEY  AGY/MED                                      
         BE    CON34A6            ALREADY HAVE REP RECORD FOR PUBLISHER         
CON34A   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PCONKEY      AGY/MED                                      
         MVI   KEY+3,X'11'         REP RECORD                                   
         MVC   KEY+4(4),PUBPLSH                                                 
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BE    CON34A2                                                          
*                                                                               
         XC    PREPNAME,PREPNAME                                                
         MVC   PREPNAME(27),=C'** PUBLISHER NOT ON FILE **'                     
         B     CON34A6             REP RECORD NOT FOUND                         
*                                                                               
CON34A2  LA    R0,PREPREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,GET                                                           
*                                                                               
CON34A6  MVC   0(9,R4),=C'PUBLISHER'                                            
         MVC   10(4,R4),PUBPLSH                                                 
         MVC   16(30,R4),PREPNAME                                               
         LA    R4,132(R4)                                                       
*                                                                               
CON34C   CLC   =C'P=',QPUB+1       PUBLISHER OPTION ?                           
         BNE   CON34T              NO                                           
         CLC   =C'ALL',QPUB+3      ALL PUBLISHERS ?                             
         BNE   CON34D              NO                                           
         CLI   PUBPLSH,C' '        ANY PUBLISHER IN PUBREC ?                    
         BH    CON34T              YES                                          
         B     CON92               SKIP THIS CONTRACT                           
*                                                                               
CON34D   CLC   PUBPLSH,QPUB+3      PUBREC HAS DESIRED PUBLISHER ?               
         BNE   CON92               NO - SKIP THIS CONTRACT                      
*                                                                               
CON34T   CLI   PUBSW,C'P'          TEST PUB PRINTED YET                         
         BE    CON40                                                            
         MVI   PUBSW,C'P'                                                       
         MVI   LNEED,8                                                          
         OC    PUBZNAME,SPACES                                                  
*NOP*    LA    R4,P1                                                            
         CLI   QMEDIA,C'N'                                                      
         BE    CON35                                                            
*                                  MAG FORMAT                                   
         MVC   0(20,R4),PUBNAME                                                 
         MVC   132(20,R4),PUBZNAME                                              
         B     CON37                                                            
*                                                                               
CON35    DS    0H                                                               
         LA    R5,132(R4)                                                       
         CLI   PUBCITY,C' '                                                     
         BNH   CON36                                                            
         CLI   PUBSTATE,C' '                                                    
         BNH   CON36                                                            
         LA    R5,132+4(R4)                                                     
         MVC   0(2,R4),PUBSTATE                                                 
         MVI   2(R4),C','                                                       
         MVC   4(16,R4),PUBCITY                                                 
         LA    R4,20(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,3(R4)                                                         
*                                                                               
CON36    DS    0H                                                               
         MVC   0(20,R4),PUBNAME                                                 
         MVC   0(20,R5),PUBZNAME                                                
*                                                                               
CON37    DS    0H                                                               
         LA    R4,20(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),PUBKPUB),4(R4)                                
*                                                                               
*                                                                               
         MVI   3(R4),C'('                                                       
         LA    R4,21(R4)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C')'                                                       
*                                                                               
*NOP*    MVC   SVPNAME,P2         SAVE PUB NAME AND NUMBER                      
*NOP*    MVC   SVPZNAME,P3        AND ZONE                                      
*                                  FOR PRINTING INSERTION CHGS FOR P16          
         LA    R4,P1                                                            
         CLC   0(9,R4),=C'PUBLISHER'                                            
         BNE   CON39                                                            
         MVC   SVPPUBL,0(R4)                                                    
         LA    R4,132(R4)                                                       
CON39    MVC   SVPNAME,0(R4)      SAVE PUB NAME AND NUMBER                      
         MVC   SVPZNAME,132(R4)   AND ZONE   (SEE *NOP* ABOVE)                  
         MVI   SVPZNAME+40,X'00'  TO FORCE PRINT LINE                           
*                                                                               
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
*                                                                               
         EJECT                                                                  
CON40    DS    0H                                                               
*                                  PREPARE TO READ BUYS                         
*                                                                               
CON71    DS    0H                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         LA    R2,PROTOTS                                                       
         LA    R1,ACCNUM*4                                                      
*                                                                               
CON72    ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R1,CON72                                                         
*                                                                               
         CLI   PROGPROF+0,C'N'       ONE CONTRACT PER PAGE                      
         BE    CON73                                                            
         MVI   FORCEHED,C'Y'                                                    
CON73    MVI   HDSW,2                                                           
         CLI   QMEDIA,C'N'                                                      
         BE    *+8                                                              
         MVI   HDSW,3                                                           
CON74    MVC   BQSTART(6),PCONSTRT                                              
         MVC   HALF,PCONNUM                                                     
*                                                                               
         DROP  RF                                                               
*                                                                               
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVPCON,DUB                                                       
         MVI   TESTPASS,0                                                       
         CLI   PROGPROF+0,C'N'                                                  
         BNE   CON74C                                                           
         MVI   PRTSW,C'P'     SET DOING PUB                                     
*                             SP CLPRT WON'T PUT IN HEADLINES                   
*NOP*    MVI   LNEED,5                                                          
*NOP*    MVC   P1(L'SVPNAME),SVPNAME                                            
*NOP*    MVC   P2(L'SVPZNAME),SVPZNAME                                          
*NOP*    MVI   P2+L'SVPZNAME,X'00'    SO P3 WILL ALWAYS PRINT                   
*NOP*    MVC   P3(08),=C'CONTRACT'                                              
*NOP*    MVC   P3+9(3),SVPCON                                                   
         MVI   LNEED,5                                                          
         LA    R4,P1                                                            
         CLC   SVPPUBL(9),=C'PUBLISHER'                                         
         BNE   CON74B                                                           
         MVI   LNEED,6                                                          
         MVC   0(L'SVPPUBL,R4),SVPPUBL                                          
         LA    R4,132(R4)                                                       
CON74B   MVC   0(L'SVPNAME,R4),SVPNAME                                          
         LA    R4,132(R4)                                                       
         MVC   0(L'SVPZNAME,R4),SVPZNAME                                        
         MVI   L'SVPZNAME(R4),X'00'   SEE *NOP* ABOVE                           
         LA    R4,132(R4)                                                       
         MVC   0(8,R4),=C'CONTRACT'                                             
         MVC   9(3,R4),SVPCON                                                   
         GOTO1 ACLPRT                                                           
*                                                                               
CON74C   MVI   PRTSW,C'B'     SET DOING BUYS FOR CLPRT                          
*                                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         LA    R2,SCONTOTS          CLEAR SAVED CONTRACT TOTALS                 
         LA    R1,ACCNUM                                                        
CON74C5  ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R1,CON74C5                                                       
*                                                                               
         LA    R2,PCONREC+33        ALSO SET PLIND                              
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVI   PLIND,0                                                          
         MVI   ELCOD,X'20'                                                      
CON74C7  BAS   RE,NEXTEL                                                        
         BNE   CON74CX                                                          
         CLI   PLIND,0                                                          
         BNE   CON74CX                                                          
         MVC   PLIND,PRBLIND-PRBELEM(R2)                                        
         B     CON74C7                                                          
*                                                                               
CON74CX  DS    0H                                                               
         CLI   SORTOPT,C'D'      INS DATE SORT                                  
         BNE   CON74CX5                                                         
         GOTO1 ASORTSCH                                                         
         B     CON74CX6                                                         
*                                                                               
CON74CX5 GOTO1 ACONSCHD       GO PROCESS BUYS                                   
CON74CX6 GOTO1 ACLPRT         SKIP A LINE                                       
*                             NOW PRINT CONTRACT INFO AFTER BUYS                
         MVC   MID1+32(90),=C'NUMBER     CONTRACT DATES     LEVEL      X        
               PERCENT      RATE    DESCRIPTION        EFF. DATE'               
         MVC   MID2+32(90),=C'------     --------------     -----      X        
               -------      ----    -----------        ---------'               
         CLI   MODE,LBUYREQ       MEANS I'M DOING ACROSS EDITION                
         BNE   *+16               SUMMARY                                       
         MVC   MID1+73(17),SPACES  BLANK OUT PERCENT AND RATE                   
         MVC   MID2+73(17),SPACES                                               
*                                                                               
         MVI   FORCEMID,C'Y'                                                    
         MVI   LNEED,6                                                          
*                                                                               
         MVI   PRTSW,C'C'         SET DOING CONTRACTS FOR CLPRT                 
         MVC   P1+64(L'CURLEVEL),CURLEVEL                                       
         CLI   MODE,LBUYREQ       MEANS I'M DOING ACROSS EDITION                
         BE    *+10               SUMMARY                                       
         MVC   P1+64(L'CURRATE),CURRATE                                         
*                                                                   L02         
***                                                                 L02         
         GOTO1 ACLPRT                                                           
*                                                                   L02         
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         CLI   PCONPRD,C'A'        SEE IF PRD CONTRACT                          
         BL    *+10                NO                                           
         MVC   P1+26(3),PCONPRD                                                 
*                                                                               
         MVC   HALF,PCONNUM                                                     
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P1+33(3),DUB                                                     
*                                                                               
         AP    TOTCNT,=P'1'                                                     
         CLC   QPAY(3),SPACES                                                   
         BE    CON40B                                                           
         CLC   PCONMOD,QPAY                                                     
*                                                                               
         DROP  RF                                                               
*                                                                               
         BL    CON40B                                                           
         MVI   P1+36,C'*'                                                       
         AP    CHACNT,=P'1'                                                     
CON40B   DS    0H                                                               
         L     R6,ACONIO1          A(PCONREC)                                   
         USING PCONREC,R6                                                       
*                                                                               
         MVI   PLIND,0             USED TO SAVE PRIMARY LEVEL IND               
         XC    PLEVEL,PLEVEL       USED TO SAVE PRIMARY LEVEL                   
         TM    PCONLIND,X'80'      SEE IF LOCKED                                
         BZ    CON40B5                                                          
         MVI   P1+37,C'L'                                                       
         AP    LOCKCNT,=P'1'                                                    
CON40B5  GOTO1 DATCON,DMCB,(3,PCONSDT),(5,P1+40)                                
*                                                                               
         GOTO1 DATCON,(R1),(3,PCONEDT),(5,P1+49)                                
*                                                                               
         MVI   P1+48,C'-'                                                       
         LA    R2,PCONREC+33                                                    
*                                                                               
         DROP  R6                                                               
*                                                                               
E18LCODE MVI   ELCOD,X'20'                                                      
         BAS   RE,NEXTEL                                                        
         BE    CON40C                                                           
         MVI   SPACING,2        RATES NOT FOUND SKIP BEFORE NEXT                
         GOTO1 ACLPRT                                                           
         B     CON60                                                            
*                                                                               
CON40C   DS    0H                                                               
         OC    PLIND(6),PLIND     SEE IF I ALREADY SAVE PRIMARY DATA            
         BNZ   CON40C5                                                          
         MVC   PLIND,PRBLIND-PRBELEM(R2)                                        
         MVC   PLEVEL,PRBLEVEL-PRBELEM(R2)                                      
CON40C5  DS    0H                                                               
         MVC   P1+58(50),SPACES                                                 
         CLI   QMEDIA,C'N'                                                      
         BNE   CONRTX                                                           
         LA    R1,PRBLIND-PRBELEM(R2)                                           
         CLI   0(R1),C'L'                                                       
         BNE   CONRTX                                                           
         LA    R1,PRBIND-PRBELEM(R2)                                            
         TM    0(R1),X'80'                                                      
         BO    CONRTX                                                           
* AWAY WE GO                                                                    
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         GOTO1 ARTLOOK,DMCB,0,PUBREC,PCONREC,ARTLKELS                           
*                                                                               
         DROP  RF                                                               
*                                                                               
         CLI   0(R1),0             CHECK FOR ERRORS                             
         BNE   CONRTX                                                           
*                                                                               
         L     R2,ARTLKELS         POINT TO NEW ELEMENTS                        
*                                                                               
CONRTX   B     *+12                                                             
CON51    BAS   RE,NEXTEL                                                        
         BNE   CON60                                                            
         MVC   W,SPACES                                                         
*                                                                               
         LA    R4,PRBLEVEL-PRBELEM(R2)                                          
         CP    0(L'PRBLEVEL,R4),=P'0'                                           
         BNZ   CON51A                                                           
         SR    R0,R0                                                            
         CLI   PRBLIND-PRBELEM(R2),C'S'        IF NO LEVEL JUST                 
         BE    CON51BC                         DISPLAY 'SPECIAL'                
         MVC   W+06(4),=C'OPEN'                                                 
         LA    R4,PRBIND-PRBELEM(R2)                                            
         TM    0(R4),X'01'                                                      
         BZ    CON54                                                            
         MVC   W+06(4),=C'FLAT'                                                 
         B     CON54                                                            
*                                                                               
CON51A   DS    0H                                                               
         MVI   PNETSW,C'N'                                                      
         MVI   W+05,C'N'                                                        
         LA    R6,PRBLEVEL-PRBELEM(R2)                                          
         CLI   PRBLIND-PRBELEM(R2),C'N'      NET $ VOLUME                       
         BE    CON51A2                       TREAT AS NET CONTRACT              
         MVI   W+05,C' '                                                        
*                                                                               
         CLI   PRBLIND-PRBELEM(R2),C'$'                                         
         BNE   CON51B                                                           
         TM    PRBIND-PRBELEM(R2),X'10'      OR NET RATE                        
         BNO   *+8                                                              
*                         SHOULD I ALSO PRECEED WITH "N" IN W+05 ?              
CON51A2  MVI   PNETSW,C'Y'                                                      
         EDIT  (P5,(R6)),(11,W+06),ALIGN=LEFT,COMMAS=YES,FLOAT=$                
*                                                                               
         B     CON54                                                            
*                                                                               
CON51B   DS    0H                                                               
         EDIT  (P5,(R6)),(9,W+06),ALIGN=LEFT,COMMAS=YES                         
*                                                                               
CON51BC  LA    R6,W+06                                                          
         AR    R6,R0                                                            
         LA    R1,PRBLIND-PRBELEM(R2)                                           
CON52    LA    RF,5(R6)      SAVE ADDRESS OF 'S'                                
         CLI   0(R1),C'X'                                                       
         BNE   *+14                                                             
         MVC   1(05,R6),=C'TIMES'                                               
         B     CON53                                                            
         CLI   0(R1),C'P'                                                       
         BNE   *+14                                                             
         MVC   1(05,R6),=C'PAGES'                                               
         B     CON53                                                            
         CLI   0(R1),C'L'                                                       
         BNE   *+14                                                             
         MVC   1(05,R6),=C'LINES'                                               
         B     CON53                                                            
***      LA    RF,6(R6)                                                         
         CLI   0(R1),C'I'                                                       
         BNE   *+14                                                             
         MVC   1(06,R6),=C'INCHES'                                              
         B     CON53                                                            
         LA    RF,6(R6)                                                         
         CLI   0(R1),C'U'                                                       
         BNE   *+14                                                             
         MVC   1(06,R6),=C'ISSUES'                                              
         B     CON53                                                            
         CLI   0(R1),C'S'                                                       
         BNE   CON53                                                            
         MVC   1(7,R6),=C'SPECIAL'                                              
         B     CON54                                                            
*                                                                               
CON53    LA    R4,PRBLEVEL-PRBELEM(R2)                                          
         CP    0(L'PRBLEVEL,R4),=P'1'                                           
         BNE   CON54                                                            
         MVC   0(2,RF),SPACES                                                   
***      MVI   0(RF),C' '                BLANK OUT 'S'                          
*                                        IF LEVEL IS ONE                        
CON54    DS    0H                                                               
         CLI   MODE,LBUYREQ      MEANS I'M DOING ACROSS EDT SUMMARY             
         BE    CON58A           SKIP TO DESCRIPTION                             
*                                                                               
         LA    R4,PRBPCT-PRBELEM(R2)                                            
         CP    0(L'PRBPCT,R4),=P'0'                                             
         BZ    CON55                                                            
         EDIT  (P3,(R4)),(5,W+20),2                                             
         CLC   W+22(3),=C'.00'                                                  
         BNE   *+10                                                             
         MVC   W+22(3),SPACES                                                   
*                                                                               
CON55    LA    R6,PRBRATE-PRBELEM(R2)                                           
         CP    0(5,R6),=P'0'                                                    
         BE    CON58                                                            
         TM    PRBIND-PRBELEM(R2),X'40'                                         
         BNZ   CON55B                                                           
         TM    PRBIND-PRBELEM(R2),X'20'      UNIT RATE                          
         BNZ   CON56                                                            
         CLI   QMEDIA,C'N'                                                      
         BE    CON56                                                            
         CLI   PRBLIND-PRBELEM(R2),C'L'                                         
         BE    CON56                                                            
         CLI   PRBLIND-PRBELEM(R2),C'I'                                         
         BE    CON56                                                            
CON55B   DS    0H                                                               
         TM    PRBIND-PRBELEM(R2),X'10'       NET INDICATOR       L01           
         BNO   *+8                                                L01           
         MVI   W+26,C'N'                                          L01           
         TM    PRBIND-PRBELEM(R2),X'02'         S INDICATOR       L04           
         BNO   *+8                                                L04           
         MVI   W+26,C'S'                                          L04           
         TM    PRBIND-PRBELEM(R2),X'04'         C INDICATOR       L05           
         BNO   *+8                                                L05           
         MVI   W+26,C'C'                                          L05           
         EDIT  (P5,(R6)),(10,W+27),2,COMMAS=YES                                 
         B     CON58                                                            
CON56    TM    PRBIND-PRBELEM(R2),X'10'       NET INDICATOR       L01           
         BNO   *+8                                                L01           
         MVI   W+26,C'N'                                          L01           
         TM    PRBIND-PRBELEM(R2),X'02'         S INDICATOR       L04           
         BNO   *+8                                                L04           
         MVI   W+26,C'S'                                          L04           
         TM    PRBIND-PRBELEM(R2),X'04'         C INDICATOR       L05           
         BNO   *+8                                                L05           
         MVI   W+26,C'C'                                          L05           
         EDIT  (P5,(R6)),(10,WORK),5                                            
*                                                                               
         LA    R1,W+27                                                          
         CLC   WORK+7(3),=C'000'                                                
         BNE   *+10                                                             
         MVC   WORK+7(3),SPACES                                                 
         MVC   0(10,R1),WORK                                                    
*                                                                               
         LA    RF,=C'/I'                                                        
         CLI   PRBLIND-PRBELEM(R2),C'I'                                         
         BE    CON57                                                            
         TM    PRBIND-PRBELEM(R2),X'08' INCH RATE=NON-INCH LEVEL IND            
         BO    CON57                                                            
         LA    RF,=C'/L'            MUST BE LINE RATE                           
******   NOW ALWAYS SHOW /I OR /L                                               
******   CLI   PRBLIND-PRBELEM(R2),C'L'                                         
******   BNE   CON58                                                            
*                                                                               
CON57    DS    0H                                                               
         LA    R1,10(R1)                                                        
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   1(2,R1),0(RF)                                                    
*                                                                               
CON58    CLI   W+26,C'N'      NET INDICATOR                        L01          
         BE    BING00                                              L01          
         CLI   W+26,C'C'      COMMISSION ONLY                      L05          
         BE    BING00                                              L05          
         CLI   W+26,C'S'      NO COMMISSION                        L04          
         BNE   CON58A                                              L04          
BING00   LA    R1,W+26                                             L01          
         LA    R5,7                                                L01          
CON58AA  CLI   1(R1),C' '                                          L01          
         BH    MOVEIT                                              L01          
         LA    R1,1(R1)                                            L01          
         BCT   R5,CON58AA                                          L01          
MOVEIT   MVC   0(1,R1),W+26                                        L04          
         CHI   R5,7                NO SPACES                       L04          
         BE    *+8                                                 L41          
*                                                                  L01          
         MVI   W+26,C' '                                           L04          
*                                                                  L01          
CON58A   LA    R1,PRBDESC-PRBELEM(R2)                                           
*                                                                               
*                                                                               
         LA    R5,PPBYOWRK                                                      
         USING PPBYOUTD,R5                                                      
*                                                                               
         ST    R1,PBYOINPT                                                      
         MVC   PBYOINPT(1),QMEDIA                                               
         MVI   PBYOCTL,X'80'       ONLY SPACE INPUT                             
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
*****                                                                           
         CLC   PBYOSPC(2),=C'R='                                                
         BNE   CON58C                                                           
         MVC   W+40(5),PBYOSPC                                                  
         MVI   W+45,C','                                                        
         MVC   W+46(12),PBYOSPC+5                                               
         B     *+10                                                             
*****                                                                           
CON58C   MVC   W+40(17),PBYOSPC                                                 
         CLI   PBYOSPC2,C' '                                                    
         BNH   CON5804                                                          
*****                                                                           
         CLC   PBYOSPC(2),=C'R='                                                
         BNE   CON58D                                                           
         MVC   P2+95(5),PBYOSPC2                                                
         MVI   P2+100,C','                                                      
         MVC   P2+101(12),PBYOSPC2+5                                            
         B     CON5804                                                          
*****                                                                           
CON58D   MVC   P2+95(17),PBYOSPC2                                               
         DROP  R5                                                               
CON5804  DS    0H                                                               
*                                                                               
*                                                                               
         LA    R6,PRBDATE-PRBELEM(R2)                                           
         OC    0(3,R6),0(R6)                                                    
         BZ    CON58Z                                                           
         GOTO1 DATCON,DMCB,(3,(R6)),(5,W+58)                                    
*                                                                               
*NOP*CON58Z   CLI   QMEDIA,C'N'                                                 
*NOP*    BE    CON59                                                            
CON58Z   LA    R6,PRBOPEN-PRBELEM(R2)   MAY HAVE PRODUCT HERE                   
         CLI   0(R6),C'A'                                                       
         BL    CON59                                                            
         MVI   W+66,C'-'                                                        
         MVC   W+67(3),0(R6)           DISPLAY PRODUCT                          
CON59    DS    0H                                                               
         MVC   P1+60(66),W+05         (SINCE W+05 MIGHT HAVE A "N")             
         GOTO1 ACLPRT                                                           
*                                                                               
         B     CON51                                                            
         EJECT                                                                  
* COMMENTS                                                                      
*                                                                               
*                                                                  L02          
*                                                                               
CON60    DS    0H                                                               
         OC    PLIND(6),PLIND     SEE IF I HAVE PRIMARY IND/LEVEL               
         BZ    CON60B            NO - THEN NO ANALYSIS MESSAGE                  
         CP    PLEVEL,=P'0'       SEE IF I HAVE LEVEL                           
         BE    CON60B            NO - NO ANALYSIS MESSAGE                       
         BAS   RE,ANALMSG                                                       
*                                                                               
CON60B   CLI   E18SW,0       RATE COMPARISION FOR 19               L02          
         BE    CON61                                               L02          
CON60H   CLI   E18LCODE+1,X'21'                                    L02          
         BE    E18HIGH                                             L02          
         BH    E18INIT       INITIALIZE E18LCODE - DONE            L02          
         CLI   E18SW,C'H'   SEE IF ONLY DOING HIGHER                            
         BE    E18HIGH                                                          
*   FIRST TIME THRU                                                L02          
         MVC   P1+64(L'LOWLEVEL),LOWLEVEL                                       
         CLI   MODE,LBUYREQ      MEANS I'M DOING ACROSS EDTS SUMMARY            
         BE    *+10                                                             
         MVC   P1+64(L'LOWRATE),LOWRATE                                         
*                                                                               
         MVI   E18LCODE+1,X'21'  FORCE TO LOOK FOR LOWER RATES     L02          
         MVI   LNEED,3                                                          
         GOTO1 ACLPRT   PRINT MESSAGE                              L02          
         MVC   P1+64(31),=C'   NO LOWER LEVELS FOUND       '                    
         CLI   MODE,LBUYREQ    MEANS I'M DOING ACROSS EDT SUMMARY               
         BE    *+10                                                             
         MVC   P1+64(31),=C'   NO LOWER LEVEL RATES FOUND  '      L02           
         B     CON40B                                              L02          
*                                                                  L02          
E18HIGH  CLI   E18SW,C'L'       SEE IF DOING ONLY LOWER                         
         BE    E18INIT                                                          
*                                                                  L02          
         MVC   P1+64(L'HIGLEVEL),HIGLEVEL                                       
         CLI   MODE,LBUYREQ     MEANS I'M DOING ACROSS EDT SUMMARY              
         BE    *+10                                                             
         MVC   P1+64(L'HIGRATE),HIGRATE                                         
*                                                                               
         MVI   E18LCODE+1,X'22'  FORCE TO LOOK FOR HIGHER RATES    L02          
         MVI   LNEED,3                                                          
         GOTO1 ACLPRT   PRINT MESSAGE                              L02          
         MVC   P1+64(32),=C'   NO HIGHER LEVELS FOUND        '      L02         
         CLI   MODE,LBUYREQ    MEANS I'M DOING ACROSS EDT SUMMARY               
         BE    *+10                                                             
         MVC   P1+64(32),=C'   NO HIGHER LEVEL RATES FOUND   '      L02         
         B     CON40B                                              L02          
*                                                                  L02          
E18INIT  MVI   E18LCODE+1,X'20'  NORMALIZE                         L02          
*                                                                               
         LA    R2,SCONTOTS    CLEAR SAVED CONTRACT TOTALS                       
         LA    R1,ACCNUM                                                        
*                                                                               
E18INIT5 ZAP   0(8,R2),=P'0'                                                    
         LA    R2,8(R2)                                                         
         BCT   R1,E18INIT5                                                      
*                                                                               
*                                                                               
* COMMENTS "PLUS"                                                               
*                                                                               
CON61    DS    0H                                                               
*                                                                               
         GOTO1 ACOMPRT                                                          
*                                                                               
CON70    DS    0H                                                               
         CLI   MODE,LBUYREQ        SINCE I MAY GO THRU CON74CX6                 
         BE    EXIT            FROM RUNL AT END OF ALL ZONE/EDT REQ             
*                                                                               
         CLI   PROGPROF+0,C'N'         NOT ONE CONTRACT PER PAGE                
         BNE   CON75                                                            
         GOTO1 ACLPRT               SO SKIP A LINE BETWEEN CONTRACTS            
CON75    DS    0H                                                               
*                                                                               
CON80    MVC   QPROG(80),SVPROG        RESTORE REQUEST                          
         CLC   QPUB,SPACES                                                      
         BNE   *+10                                                             
         MVC   QPUB(3),=C'ALL'     SET TO ALL                                   
         MVC   ESTACT(5),=5C'N'                                                 
         MVI   PUBSW,0             SO PUB NAME WILL PRINT                       
         MVI   HDSW,1              RESET FOR NEXT CONTRACT                      
         CLI   PROGPROF+0,C'N'      ONE CONTRACT PER PAGE                       
         BE    CON92                                                            
         MVI   FORCEHED,C'Y'                                                    
         B     CON92               GO PROCESS NEXT CONTRACT                     
*                                                                               
CON92    DS    0H                                                               
******** CLC   QEST,SPACES         IF ONE CON SPECIFIED, DONE                   
******** BNE   CONCLT                                                           
* ELSE READ NEXT CON KEY                                                        
*                                                                   L02         
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         MVC   KEY,PCONKEY                                                      
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
*                                                                               
         DROP  RF                                                               
*                                                                               
         CLC   =C'ALL',QPUB                                                     
         BE    CON17                                                            
         CLC   =C'P=',QPUB+1       PUBLISHER OPTION ?                           
         BE    CON17               YES - TREAT LIKE ALL                         
         B     CON13                                                            
*                                                                               
CON120   DS    0H                                                               
         CLI   QOPT7,C' '          IF REQUEST IS FROM SORT                      
         BNE   *+8                 LEAVE CONTROLLER IN CHARGE                   
         MVI   MODE,LBUYREQ        ELSE - NEXT REQ (SKIP ANY BUY RD)            
         B     RUNL                                                             
         EJECT                                                                  
*                                                                               
*                                  AFTER CONTRACT RATES AND COMMENTS            
*                                  MODE WILL BE SET BACK TO FBUYCLI             
*                                  SO BUYS WILL BE READ FOR NEW CON             
*                                                                               
         SPACE 2                                                                
RUNL     EQU   *                                                                
         CLC   QPUB+8(3),=C'ZZZ'   SEE IF DOING ALL ZONES/EDTS                  
         BNE   EXIT                                                             
RUNL1B   DS    0H                  ACROSS EDITION SUMMARY                       
         MVI   HDSW,0                                                           
         MVI   FORCEHED,C'Y'       SKIP TO NEW PAGE                             
         MVC   P1+24(27),=C'** ACROSS EDITION TOTALS **'                        
         LA    R5,REQTOTS                                                       
         MVI   STARS,2                                                          
         GOTO1 APRTTOTS                                                         
         GOTO1 ACLPRT                                                           
         MVC   SCONTOTS(ACCNUM*8),REQTOTS    SAVE VENDOR TOTALS FOR             
*                                                                               
*        REDO ANALYSIS FOR ACROSS EDITION TOTALS                                
*        USING LAST CONTRACT READ                                               
*        ALL CONTRACTS FOR THE BASE PUB SHOULD BE THE SAME PLIND                
*        AND PLEVEL FOR ANALYSIS TO BE CORRECT                                  
*                                                                               
         B     CON74CX6                                                         
*                                                                               
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTELX             RETURN WITH CC =                             
         CLC   ELCOD,0(R2)                                                      
         BCR   8,RE                                                             
         B     NEXTEL+2                                                         
NEXTELX  LTR   R2,R2               RETURN WITH CC NOT =                         
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
ANALMSG  NTR1                                                                   
         CLI   PROGPROF+2,C'N'         SEE IF PRINT ANALYSIS MESSAGE            
         BE    ANALX                                                            
*                                                                               
         CLI   PNETSW,C'Y'                                                      
         BNE   *+20                                                             
         ZAP   PACKED14,SCONTOTS+16(8) NET-CD                                   
         AP    PACKED14,SCONTOTS+8(8)  NET DOLLARS                              
         B     *+10                                                             
         ZAP   PACKED14,SCONTOTS(8)   GROSS DOLLARS                             
         CLI   PLIND,C'N'          NET $ VOLUME (PNETSW WILL BE "Y")            
         BE    ANA5                                                             
         CLI   PLIND,C'$'          $ VOLUME                                     
         BE    ANA5                                                             
         ZAP   PACKED14,SCONTOTS+40(8) CONTRACT INCHES                          
         CLI   PLIND,C'I'          INCHES                                       
         BE    ANA5                                                             
         ZAP   PACKED14,SCONTOTS+32(8)  TO CONTRACT LINES                       
         CLI   PLIND,C'L'                                                       
         BE    ANA6                                                             
         ZAP   PACKED14,SCONTOTS+24(8)  TO CONTRACT INSERTIONS/TIMES            
         CLI   PLIND,C'X'                                                       
         BE    ANA9                NO NEED TO MULTIPLY SINCE CARRIED            
*                                  WITH 4 DECIMALS                              
         CLI   PLIND,C'P'          SAME FOR PAGES                               
         BE    ANA9                                                             
         CLI   PLIND,C'S'          SAME FOR SPECIAL                             
         BE    ANA9                                                             
         CLI   PLIND,C'U'          SAME FOR ISSUES                              
         BE    ANA9                                                             
         B     ANALX               NOTHING TO ANALIZE                           
*                                                                               
ANA5     DS    0H                                                               
         MVC   SVPK14,PACKED14    SAVE UNCHANGED TOTALS                         
*                                                                               
*                                 SINCE CON $ HAS 2 DECIMALS                    
*                                 BUT PLEVEL IS WHOLE $                         
*                                 ONLY MULTIPLY BY 100                          
         MP    PACKED14,=P'100'   SINCE CON INCHES HAS 2 DECIMALS               
*                                 BUT PLEVEL IS WHOLE INCHES                    
*                                 ONLY MULTIPLY BY 100                          
         B     ANA9                                                             
*                                                                               
ANA6     DS    0H                                                               
ANA7     DS    0H                                                               
         MVC   SVPK14,PACKED14     SAVE UNCHANGED TOTALS                        
         MP    PACKED14,=P'10000'  FOR PCT 2 DECIMALS                           
ANA9     DS    0H                                                               
         MVC   WORKP14,PACKED14    SET FOR DIVISION BELOW                       
*                                                                               
         CLI   PLIND,C'N'          CK NET                                       
         BE    ANA9B                                                            
         CLI   PLIND,C'$'          CK DOLLAR VOLUME                             
         BNE   ANA9C                                                            
ANA9B    CLI   PROGPROF+8,C'Y'     CK PROFILE                                   
         BNE   ANA9C                                                            
         MVC   DUB,SCONPREM                                                     
         CLI   PNETSW,C'Y'         SEE IF DOING NET                             
         BNE   *+10                                                             
         MVC   DUB,SCONNETP                                                     
         MP    DUB,=P'100'                                                      
         SP    WORKP14,DUB                                                      
*                                                                               
ANA9C    MP    WORKP14,=P'10'      FOR PCT 3 DECIMALS                           
         DP    WORKP14,PLEVEL                                                   
*                                                                               
* ROUND UP PERCENT IF NECESSARY                                                 
*                                                                               
         CLI   WORKP14+8,X'50'                                                  
         BNH   ANA9D                                                            
         AP    WORKP14(9),=P'10'   ROUND UP                                     
*                                                                               
ANA9D    DS    0H                                                               
         DP    WORKP14(9),=P'10'   FOR PCT 2 DECIMALS                           
         GOTO1 ACLPRT                                                           
         EDIT  (P7,WORKP14),(17,P1+25),2,COMMAS=YES                             
         MVC   P1+44(38),=C'PERCENT OF THIS CONTRACT LEVEL REACHED'             
         GOTO1 ACLPRT                                                           
*                                                                               
         CLI   PLIND,C'N'          NET $ VOLUME                                 
         BE    ANA10                                                            
         CLI   PLIND,C'$'          $ VOLUME                                     
         BE    ANA10                                                            
         CLI   PLIND,C'I'          INCHES                                       
         BE    ANA10                                                            
         CLI   PLIND,C'X'          TIMES                                        
         BE    ANA10                                                            
         CLI   PLIND,C'P'          PAGES                                        
         BE    ANA10                                                            
         CLI   PLIND,C'U'          ISSUES                                       
         BE    ANA10                                                            
         CLI   PLIND,C'L'          LINES                                        
         BNE   ANALX               (MAYBE TO UNMULTIPLIED ANALYSIS)             
ANA10    DS    0H                                                               
*                                                                               
         ZAP   WORKP14,PLEVEL      MAKE PLEVEL IN WORKP14                       
         MP    WORKP14,=P'10000'   EQUIVALENT TO PACKED14 - 4 DEC               
*                                                                               
         CLI   PLIND,C'N'          CK NET                                       
         BE    ANA10AF                                                          
         CLI   PLIND,C'$'          CK DOLLAR VOLUME                             
         BNE   ANA10A                                                           
ANA10AF  CLI   PROGPROF+8,C'Y'     CK PROFILE                                   
         BNE   ANA10A                                                           
         MVC   DUB,SCONPREM                                                     
         CLI   PNETSW,C'Y'         SEE IF DOING NET                             
         BNE   *+10                                                             
         MVC   DUB,SCONNETP                                                     
         MP    DUB,=P'100'                                                      
         AP    WORKP14,DUB         INCLUDE PREMIUM CHARGES                      
*                                                                               
ANA10A   SP    WORKP14,PACKED14                                                 
         DP    WORKP14,=P'100'     TO 2 DEC FOR DISPLAY                         
*                                                                               
         CP    WORKP14(12),=P'0' SEE IF CONT LEVEL EXACTLY ACHIEVED             
         BE    ANA10T                                                           
*                                                                               
         LA    RE,P1+44                                                         
         CLI   PLIND,C'N'          NET $ VOLUME                                 
         BE    ANA10B                                                           
         CLI   PLIND,C'$'          $ VOLUME                                     
         BE    ANA10B                                                           
         MVC   P1+44(06),=C'INCHES'                                             
         LA    RE,P1+51                                                         
         CLI   PLIND,C'I'          INCHES                                       
         BE    ANA10B                                                           
         MVC   P1+44(06),=C'LINES '                                             
         LA    RE,P1+50                                                         
         CLI   PLIND,C'L'          LINES                                        
         BE    ANA10B                                                           
         MVC   P1+44(06),=C'TIMES '                                             
         LA    RE,P1+50                                                         
         CLI   PLIND,C'X'          TIMES                                        
         BE    ANA10B                                                           
         MVC   P1+44(06),=C'PAGES '                                             
         LA    RE,P1+50                                                         
         CLI   PLIND,C'P'          PAGES                                        
         BE    ANA10B                                                           
         MVC   P1+44(06),=C'ISSUES'                                             
         LA    RE,P1+51                                                         
         CLI   PLIND,C'U'          ISSUES                                       
         BE    ANA10B                                                           
         XC    P1+44(7),P1+44                                                   
         B     ANALX                                                            
*                                                                               
ANA10B   CP    WORKP14(12),=P'0'   POSITIVE ?                                   
         BNL   ANA10H              YES                                          
*                                                                               
         LA    RE,P1+44                                                         
         LA    RE,L'EXCLVL(RE)                                                  
         CLI   PLIND,C'X'          TIMES OR LINES - NO DECIMALS                 
         BE    ANA10C                                                           
         CLI   PLIND,C'P'          PAGES - NO DECIMALS                          
         BE    ANA10C                                                           
         CLI   PLIND,C'U'          ISSUES- NO DECIMALS                          
         BE    ANA10C                                                           
         CLI   PLIND,C'L'          LINES HAVE NO DEC PLACES                     
         BNE   ANA10D                                                           
ANA10C   DP    WORKP14(12),=P'100'   SO "REMOVE" THEM                           
         EDIT  (P10,WORKP14),(17,0(RE)),COMMAS=YES,ALIGN=LEFT                   
         AR    RE,R0          R0 HAS NO. OF SIGNIFICANT CHAR'S OUTPUT           
         MVC   1(6,RE),=C'LINES '                                               
         CLI   PLIND,C'L'                                                       
         BE    ANA10CX                                                          
         MVC   1(6,RE),=C'TIMES '                                               
         CLI   PLIND,C'X'                                                       
         BE    ANA10CX                                                          
         MVC   1(6,RE),=C'PAGES '                                               
         CLI   PLIND,C'P'                                                       
         BE    ANA10CX                                                          
         MVC   1(6,RE),=C'ISSUES'  MUST BE ISSUES                               
*                                                                               
ANA10CX  CP    WORKP14(10),=P'-1'  CHECK FOR 1                                  
         BNE   ANA10F                                                           
         MVI   6(RE),C' '                                                       
         CLI   5(RE),C'S'                                                       
         BNE   *+8                                                              
         MVI   5(RE),C' '          REMOVE "S"                                   
         B     ANA10F                                                           
*                                                                               
ANA10D   DS    0H                                                               
         EDIT  (P12,WORKP14),(17,0(RE)),2,COMMAS=YES,ALIGN=LEFT                 
         CLI   PLIND,C'I'          INCHES                                       
         BNE   ANA10E              MUST BE DOLLARS                              
         AR    RE,R0          R0 HAS NO. OF SIGNIFICANT CHAR'S OUTPUT           
         MVC   1(6,RE),=C'INCHES'                                               
*                                                                               
         CP    WORKP14(12),=P'-100' CHECK FOR 1.00                              
         BNE   ANA10F                                                           
         MVC   5(2,RE),SPACES  REMOVE "ES"                                      
         B     ANA10F                                                           
*                                                                               
ANA10E   EDIT  (P12,WORKP14),(17,0(RE)),2,COMMAS=YES,ALIGN=LEFT,FLOAT=$         
*                                                                               
ANA10F   MVC   P1+44(L'EXCLVL),EXCLVL                                           
         MVI   SPACING,2                                                        
         GOTO1 ACLPRT              SKIP AFTER                                   
*                                                                               
*                                                                               
*                                                                               
         CLI   PLIND,C'N'          NET?                                         
         BE    ANA10F20                                                         
         CLI   PLIND,C'$'          $ VOLUME?                                    
         BNE   ANA10F50                                                         
ANA10F20 CLI   PROGPROF+8,C'Y'     CK PROFILE                                   
         BNE   ANA10F50                                                         
*                                                                               
         CP    SCONPREM,=P'0'      IT'S ZERO, NO NEED TO PRINT                  
         BE    ANA10F50            SCONNETP WILL BE ZERO TOO                    
*                                                                               
         MVC   P1,SPACES                                                        
         MVC   P1+44(6),=C'NOTE: '                                              
         LA    R2,P1+50                                                         
*                                                                               
         CLI   PNETSW,C'Y'         SEE IF DOING NET                             
         BNE   ANA10F30                                                         
         EDIT  (P8,SCONNETP),(17,0(R2)),2,COMMAS=YES,ALIGN=LEFT                 
         B     ANA10F35                                                         
*                                                                               
ANA10F30 EDIT  (P8,SCONPREM),(17,0(R2)),2,COMMAS=YES,ALIGN=LEFT                 
ANA10F35 CLI   0(R2),C' '                                                       
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
         LA    R2,1(R2)                                                         
*                                                                               
         CLI   PNETSW,C'Y'         SEE IF DOING NET                             
         BNE   ANA10F40                                                         
         MVC   0(4,R2),=C'NET '                                                 
         LA    R2,4(R2)                                                         
*                                                                               
ANA10F40 MVC   0(L'PREMCHG,R2),PREMCHG                                          
         GOTO1 ACLPRT                                                           
*                                                                               
*                                                                               
*                                                                               
ANA10F50 B     ANALX                                                            
*                                                                               
ANA10H   DS    0H                                                               
         CLI   PLIND,C'X'          TIMES OR LINES - NO DECIMALS                 
         BE    ANA10H5                                                          
         CLI   PLIND,C'P'          OR PAGES                                     
         BE    ANA10H5                                                          
         CLI   PLIND,C'U'          OR ISSUES                                    
         BE    ANA10H5                                                          
         CLI   PLIND,C'L'          LINES HAVE NO DEC PLACES                     
         BNE   ANA10K                                                           
ANA10H5  DP    WORKP14(12),=P'100'   SO "REMOVE" THEM                           
         EDIT  (P10,WORKP14),(17,P1+25),COMMAS=YES                              
         CP    WORKP14(10),=P'1' CHECK FOR 1                                    
         BNE   ANA10P                                                           
         MVI   P1+49,C' '    (LAST S IN ISSUES)                                 
         CLI   P1+48,C'S'                                                       
         BNE   *+8                                                              
         MVI   P1+48,C' '    BLANK OUT "S"                                      
         SH    RE,=H'1'      AND BACK-UP RE                                     
         B     ANA10P                                                           
*                                                                               
ANA10K   EDIT  (P12,WORKP14),(17,P1+25),2,COMMAS=YES                            
         CLI   PLIND,C'I'          INCHES                                       
         BE    ANA10K5                                                          
         EDIT  (P12,WORKP14),(17,P1+25),2,COMMAS=YES,FLOAT=$                    
         B     ANA10P                                                           
ANA10K5  DS    0H                                                               
         CP    WORKP14(12),=P'100' CHECK FOR 1.00                               
         BNE   ANA10P                                                           
         MVC   P1+48(2),SPACES                                                  
         SH    RE,=H'2'       AND BACK=UP RE                                    
*                                                                               
ANA10P   MVC   0(L'RCHLVL,RE),RCHLVL                                            
         MVI   SPACING,2                                                        
         GOTO1 ACLPRT              SKIP AFTER                                   
         B     ANA10T15                                                         
*                                                                               
ANA10T   MVC   P1+44(31),=C'CONTRACT LEVEL EXACTLY ACHIEVED'                    
         MVI   SPACING,2                                                        
         GOTO1 ACLPRT              SKIP AFTER                                   
*                                                                               
*                                                                               
*                                                                               
ANA10T15 CLI   PLIND,C'N'          NET?                                         
         BE    ANA10T20                                                         
         CLI   PLIND,C'$'          $ VOLUME?                                    
         BNE   ANALX                                                            
ANA10T20 CLI   PROGPROF+8,C'Y'     CK PROFILE                                   
         BNE   ANALX                                                            
*                                                                               
         CP    SCONPREM,=P'0'      IT'S ZERO, NO NEED TO PRINT                  
         BE    ANALX               SCONNETP WILL BE ZERO TOO                    
*                                                                               
         MVC   P1,SPACES                                                        
         MVC   P1+44(6),=C'NOTE: '                                              
         LA    R2,P1+50                                                         
*                                                                               
         CLI   PNETSW,C'Y'         SEE IF DOING NET                             
         BNE   ANA10T30                                                         
         EDIT  (P8,SCONNETP),(17,0(R2)),2,COMMAS=YES,ALIGN=LEFT                 
         B     ANA10T35                                                         
*                                                                               
ANA10T30 EDIT  (P8,SCONPREM),(17,0(R2)),2,COMMAS=YES,ALIGN=LEFT                 
ANA10T35 CLI   0(R2),C' '                                                       
         BE    *+12                                                             
         LA    R2,1(R2)                                                         
         B     *-12                                                             
         LA    R2,1(R2)                                                         
*                                                                               
         CLI   PNETSW,C'Y'         SEE IF DOING NET                             
         BNE   ANA10T40                                                         
         MVC   0(4,R2),=C'NET '                                                 
         LA    R2,4(R2)                                                         
*                                                                               
ANA10T40 MVC   0(L'PREMCHG,R2),PREMCHG                                          
         GOTO1 ACLPRT                                                           
*                                                                               
ANALX    XIT1                                                                   
         EJECT                                                                  
*                                                                               
GET      NTR1                                                                   
         GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,IOAREA,DMWORK                 
*                                                                               
         B     DMX                                                              
*                                                                               
*                                                                               
GETPUB   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,GETREC,PUBFILE,KEY+27,IOAREA,DMWORK                 
*                                                                               
DMX      TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
GENFILES DS    0D                                                               
         DC    CL8' GENDIR'                                                     
         DC    CL8' GENFIL'                                                     
         DC    C'X'                                                             
*                                                                               
CURRATE  DC    C'*** CURRENT LEVEL RATES ***'                      L02          
LOWRATE  DC    C'*** LOWER LEVEL RATES ***'                        L02          
HIGRATE  DC    C'*** HIGHER LEVEL RATES ***'                       L02          
*                                                                  L02          
*                                                                               
CURLEVEL DC    C'*** CURRENT LEVELS ***'                           L02          
LOWLEVEL DC    C'*** LOWER LEVELS ***'                             L02          
HIGLEVEL DC    C'*** HIGHER LEVELS ***'                            L02          
*                                                                  L02          
*                                                                  L02          
EXCLVL   DC    C'THIS CONTRACT LEVEL EXCEEDED BY '                              
RCHLVL   DC    C'NEEDED TO ACHIEVE THIS LEVEL'                                  
*                                                                               
*                                                                               
PREMCHG  DC    C'PREMIUM CHARGES EXCLUDED FROM ANALYSIS'                        
*                                                                               
*                                                                               
PATCH    DC    30X'00'                                                          
         EJECT                                                                  
COMPRT   CSECT                                                                  
         NMOD1 0,COMPRT                                                         
         L     RC,PPFILEC                                                       
         LR    R3,RC                                                            
         AHI   R3,4096                                                          
         USING PPFILED,RC,R3                                                    
*                                                                               
         L     R6,ACONIO1          A(PCONREC)                                   
         USING PCONREC,R6                                                       
*                                                                               
         MVI   BYTE,0                                                           
         OC    PCONREV,PCONREV                                                  
         BZ    COMP2                                                            
         GOTO1 ACLPRT                 SKIP A LINE                               
         MVC   P1+49(25),=C'SUPERSEDES CONTRACT DATED'                          
         GOTO1 DATCON,DMCB,(3,PCONREV),(5,P1+75)                                
         GOTO1 ACLPRT                                                           
*                                                                               
COMP2    DS    0H                  CONSTRUCT AND PRINT ADDRESS                  
         LA    R4,ADRDATA                                                       
         USING ADRDATAD,R4                                                      
         MVI   OVRSRCE,0           CLEAR OVERRIDE INDICATOR                     
         MVI   ADRDUP,C' '         CLEAR DUP ADDRESS INDICATOR                  
*                                                                               
COMP2D   LA    RE,ADRREC           POINT TO ADDRESS REC                         
         USING PGETADRD,RE                                                      
         CLC   CLTDATA(3),PCONKAGY       SAME CLIENT AGY/MED ?                  
         BNE   COMP4                       NO - READ ADDRESS                    
         CLC   PCONKPUB(6),PGADKPUB      SAME PUB ?                             
         BNE   COMP4                       NO - READ ADDRESS                    
         DROP  RE                                                               
         CLC   PCONKCLT,CLTCODE    SAME CLIENT ?                                
         BE    COMP6               YES - DO NOT REREAD ADDRESS                  
*                                                                               
COMP4    DS    0H                  READ FOR CONTRACT ADDRESS                    
         XC    PADRDATA,PADRDATA   CLEAR "PRIOR ADDRESS"                        
         MVI   ADRTYP,C'C'         CONTRACT                                     
*                                    FILL IN 7 BYTES OF CLTDATA                 
         MVC   CLTDATA(3),PCONKAGY   CLIENT AGY/MED                             
         MVC   CLTCODE,PCONKCLT      CLIENT CODE                                
         MVC   CLTOFF,PCLTOFF        CLIENT OFFICE                              
*                                                                               
         DROP  R6                                                               
*                                                                               
         XCEFL ADRREC,300          CLEAR ADDRESS RECORD AREA                    
         XC    ADRWORK,ADRWORK                                                  
*                                                                               
         GOTO1 APGETADR,DMCB,(ADRTYP,CLTDATA),PUBREC,DATAMGR,0                  
*                                                                               
         CLI   0(R1),X'FF'         ERROR IN CALL ?                              
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),0             ADDRESS RECORD FOUND ?                       
         BE    COMP6               NO - TRY FOR A REP                           
*                                                                               
         MVC   ADRWORK(4),0(R1)    2-4 = ADDRESS LEVEL                          
*                                                                               
         L     RE,4(R1)            A(ADDRESS INFO FROM CALL)                    
         LA    R1,PGADELQ+PGADLEN  LENGTH OF ADDRESS RECORD (287)               
         LA    RF,ADRREC           DESTINATION OF ADDRESS RECORD                
         MOVE  ((RF),(R1)),(RE)    MOVE ADDRESS RECORD                          
*                                                                               
         CLI   ADRWORK+1,X'FF'     IF HAVE CLIENT ADDR NO NEED                  
         BL    COMP20              TO LOOK FOR REP                              
*                                                                               
*                                  NOW TRY FOR A REP                            
COMP6    DS    0H                                                               
         LA    R2,PUBREC+33                                                     
         XC    DUB,DUB                                                          
         MVI   ELCOD,X'14'                                                      
COMP8    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   COMP11              NO REP ELEM FOUND                            
*                                                                               
         USING PUBREPEL,R2                                                      
         CLC   PUBRPOFF,=3X'FF'                                                 
         BE    COMP10                                                           
         CLC   PUBRPOFF,PCLTKCLT                                                
         BE    COMP10                                                           
         CLI   PUBRPOFF,X'FF'                                                   
         BNE   COMP8                                                            
         CLC   PUBRPOFF+1(1),PCLTOFF                                            
         BNE   COMP8                                                            
COMP10   DS    0H                                                               
         OC    PUBCNREP,PUBCNREP                                                
         BZ    COMP8               NO OVERIDES THIS ELEM                        
COMP10B  DS    0H                                                               
         MVC   DUB(4),PUBCNREP                                                  
         MVC   DUB+4(3),PUBRPOFF                                                
COMP11   DS    0H                                                               
         OC    ADRWORK+1(3),ADRWORK+1                                           
         BZ    COMP12                                                           
         CLC   ADRWORK+1(3),DUB+4     TEST 'LEVEL'                              
         BNH   COMP20              ADDR MORE SPECIFIC                           
*                                                                               
         DROP  R2                                                               
*                                                                               
COMP12   DS    0H                                                               
         OC    DUB(4),DUB                                                       
         BZ    COMP20              NO REP                                       
*                                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         MVC   SVBKEY,KEY          SAVE KEY                                     
         XC    KEY,KEY                                                          
         MVC   KEY(3),PCONKAGY     AGENCY/MEDIA                                 
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),DUB        REP CODE                                     
*                                                                               
         DROP  RF                                                               
*                                                                               
         CLC   KEY(10),PREPKEY     DONT READ IF ALREADY THERE                   
         BE    COMP15                                                           
         XC    PREPKEY(199),PREPKEY                                             
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(10),KEY                                                  
         BNE   COMP17                                                           
         LA    R0,PREPREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,GET              READ REP RECORD                              
         B     COMP17                                                           
*                                                                               
COMP15   DS    0H                                                               
*                             DIDN'T READ ANYTHING SO JUST RESTORE KEY          
         MVC   KEY,SVBKEY                                                       
         B     COMP19              REP ALREADY THERE                            
*                                                                               
COMP17   DS    0H                                                               
*                                  RESTORE KEY AND SEQUENCE                     
         MVC   KEY,SVBKEY                                                       
         GOTO1 HIGH                                                             
*                                                                               
COMP19   DS    0H                                                               
         OC    PREPNAME,PREPNAME                                                
         BZ    COMP20              NO REP FOUND                                 
         XC    ADRDATA,ADRDATA                                                  
         MVC   ADRNAM(L'PREPNAME),PREPNAME                                      
         MVC   ADRLIN1(L'PREPLIN1),PREPLIN1                                     
         MVC   ADRLIN2(L'PREPLIN2),PREPLIN2                                     
         MVC   ADRATTN(L'PREPATTN),PREPATTN                                     
         MVC   ADRTEL(L'PREPTEL),PREPTEL                                        
         MVC   ADRFAX(L'PREPFAX),PREPFAX                                        
         B     COMP30                                                           
*                                                                               
*                                                                               
COMP20   DS    0H                  ADDRESS FROM PUB ADDRESS OVERRIDE            
         CLI   ADRWORK+1,0         ADDRESS RECORD FOUND ?                       
         BE    COMP40              NO - USE "DIRECT TO PUB"                     
         XC    ADRDATA,ADRDATA                                                  
         LA    RE,ADRREC                                                        
         USING PGETADRD,RE                                                      
         MVC   ADRNAM(L'PGADNAME),PGADNAME                                      
         MVC   ADRLIN1(L'PGADLIN1),PGADLIN1                                     
         MVC   ADRLIN2(L'PGADLIN2),PGADLIN2                                     
         MVC   ADRATTN(L'PGADATTN),PGADATTN                                     
         MVC   ADRTEL(L'PGADTEL),PGADTEL                                        
         MVC   ADRFAX(L'PGADFAX),PGADFAX                                        
         MVC   ADREML(L'PGADEADD),PGADEADD                                      
*                                                                               
         DROP  RE                                                               
*                                                                               
COMP30   DS    0H                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         LA    R2,PCONREC+33       ATTN OVERRIDE                                
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVI   ELCOD,X'50'                                                      
         BAS   RE,NEXTEL                                                        
         BNE   COMP32                                                           
         CLI   ADRSW,C'Y'          SHOW ADDRESS ?                               
         BNE   COMP32              NO                                           
         USING PCATELD,R2                                                       
         CLI   PCATNAM,0                                                        
         BNH   COMP32                                                           
         MVC   ADRATTN(L'PCATNAM),PCATNAM     REPLACE ATTENTION                 
         OI    OVRSRCE,X'01'       INDICATE ATTN FROM CONTRACT                  
*                                                                               
         DROP  R2                                                               
*                                                                               
COMP32   DS    0H                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         LA    R2,PCONREC+33                                                    
         MVI   ELCOD,X'55'         TEL AND FAX ELEMENT                          
         BAS   RE,NEXTEL                                                        
         BNE   COMP35                                                           
         USING PCTFELD,R2                                                       
         CLI   TFXSW,C'T'          SHOW TELEPHONE ?                             
         BE    COMP32B             YES                                          
         CLI   TFXSW,C'B'          SHOW TELEPHONE AND FAX ?                     
         BNE   COMP32F             NO                                           
COMP32B  CLI   PCONTELE,C' '       CONTRACT TELEPHONE ?                         
         BNH   COMP32F             NO - TEST FAX                                
         CLC   =C'DEF',PCONTELE    DEFAULT ?                                    
         BE    COMP32F             YES - LEAVE ADRTEL AS IS                     
         MVC   ADRTEL(L'PCONTELE),PCONTELE     REPLACE ADRTEL                   
         OI    OVRSRCE,X'02'       INDICATE TEL FROM CONTRACT                   
*                                                                               
COMP32F  DS    0H                                                               
         CLI   PCONFAX,C' '        CONTRACT FAX ?                               
         BNH   COMP35              NO - DONE WITH TEL AND FAX                   
         CLI   TFXSW,C'F'          SHOW FAX ?                                   
         BE    COMP32J             YES                                          
         CLI   TFXSW,C'B'          SHOW TELEPHONE AND FAX ?                     
         BNE   COMP35              NO                                           
COMP32J  CLC   =C'DEF',PCONFAX     DEFAULT ?                                    
         BE    COMP35              YES - LEAVE ADRFAX AS IS                     
         MVC   ADRFAX(L'PCONFAX),PCONFAX      REPLACE ADRFAX                    
         OI    OVRSRCE,X'04'       INDICATE FAX FROM CONTRACT                   
*                                  NOW DO SOME ACTUAL PRINTING                  
         DROP  R2,RF                                                            
*                                  NOW DO SOME ACTUAL PRINTING                  
COMP35   DS    0H                                                               
         CLI   ADRNAM,C' '         ANYTHING FROM ADDRESS OR REP REC ?           
         BNH   COMP40              NO - USE PUB NAME, ETC.                      
         CLI   PROGPROF+0,C'N'     ONE CONTRACT PER PAGE ?                      
         BNE   COMP48              YES - NO "ADDRESS SAME" MSG.                 
         CLC   ADRNAM(ADRTEL-ADRNAM),PADRDATA    SAME ADDRESS ?                 
         BNE   COMP48                            NO                             
         MVI   ADRDUP,C'X'         INDICATE SAME ADDRESS                        
         NI    OVRSRCE,X'FF'-X'01'    TURN OFF ATTN OVERRIDE INDICATOR          
         B     COMP48              PRINT THE ADDRESS                            
*                                                                               
COMP40   DS    0H                  USE PUB NAME, ETC.                           
         XC    ADRDATA,ADRDATA                                                  
         MVC   ADRNAM(L'PUBNAME),PUBNAME                                        
         MVC   ADRZNAM(L'PUBZNAME),PUBZNAME                                     
         MVC   ADRLIN1,PUBLINE1                                                 
         MVC   ADRLIN2,PUBLINE2                                                 
*                                                                               
         LA    R2,PUBREC+33                                                     
         MVI   ELCOD,X'11'                                                      
         BAS   RE,NEXTEL                                                        
         BNE   COMP30              NO SUPPLEMENTAL ADDRESS ELEM                 
*                                  GO CHECK CONTRACT OVERRIDES                  
         USING PPDUMD11,R2         CHECK SUPPLEMENTAL ADDRESS                   
         CLI   PUBATTN,0           ATTENTION NAME ?                             
         BNH   COMP42              NO                                           
         MVC   ADRATTN(L'PUBATTN),PUBATTN                                       
COMP42   CLI   PUBTEL,0            TELEPHONE ?                                  
         BNH   COMP44              NO                                           
         MVC   ADRTEL(L'PUBTEL),PUBTEL                                          
COMP44   CLI   PUBSFAXN,0          FAX ?                                        
         BNH   COMP30              NO                                           
         MVC   ADRFAX(L'PUBSFAXN),PUBSFAXN                                      
         B     COMP30              CHECK OVERRIDES                              
*                                                                               
         DROP  R2                                                               
*                                                                               
COMP48   DS    0H        SET LNEED FOR A COMPLETE ADDRESS ON SAME PAGE          
         LA    RE,1                                                             
         CLI   ADRSW,C'Y'          SHOW ADDRESS ?                               
         BNE   COMP48J             NO - CHECK TELEPHONE                         
         CLI   ADRDUP,C'X'         SAME ADDRESS ?                               
         BNE   COMP48B             NO                                           
         LA    RE,3(RE)            "ADDRESS SAME AS PRIOR"                      
         B     COMP48J             GO CHECK FOR TELEPHONE, ETC.                 
COMP48B  LA    RE,1(RE)            FOR NAME                                     
         CLI   ADRZNAM,C' '                                                     
         BNH   COMP48D                                                          
         LA    RE,1(RE)            ZONE NAME                                    
COMP48D  CLI   ADRATTN,C' '                                                     
         BNH   COMP48F                                                          
         LA    RE,1(RE)            ATTENTION                                    
COMP48F  CLI   ADRLIN1,C' '                                                     
         BNH   COMP48H                                                          
         LA    RE,1(RE)            ADDRESS LINE 1                               
COMP48H  CLI   ADRLIN2,C' '                                                     
         BNH   COMP48J                                                          
         LA    RE,1(RE)            ADDRESS LINE 2                               
COMP48J  CLI   ADRTEL,C' '                                                      
         BNH   COMP48L                                                          
         CLI   TFXSW,C'T'          SHOW TELEPHONE ?                             
         BE    COMP48K             YES                                          
         CLI   TFXSW,C'B'          SHOW TELEPHONE AND FAX ?                     
         BNE   COMP48L             NO                                           
COMP48K  LA    RE,1(RE)            TELEPHONE                                    
COMP48L  CLI   ADRFAX,C' '                                                      
         BNH   COMP48N                                                          
         CLI   TFXSW,C'F'          SHOW FAX ?                                   
         BE    COMP48M             YES                                          
         CLI   TFXSW,C'B'          SHOW TELEPHONE AND FAX ?                     
         BNE   COMP48N             NO                                           
COMP48M  LA    RE,1(RE)            FAX                                          
COMP48N  CLI   ADREML,C' '                                                      
         BNH   COMP48P                                                          
         CLI   EMLSW,C'Y'          SHOW E-MAIL ?                                
         BNE   COMP48P             NO                                           
         LA    RE,1(RE)            E-MAIL                                       
COMP48P  CLI   OVRSRCE,0           ANY "OVERRIDES" FROM CONTRACT ?              
         BNH   COMP48X             NO - FINISHED WITH LNEED                     
         LA    RE,2(RE)            BLANK LINE - INFO LINE                       
COMP48X  LTR   RE,RE               ANY LINE NEED ?                              
         BZ    COMP50              NO                                           
         STC   RE,LNEED                                                         
*                                                                               
COMP50   DS    0H                                                               
         CLI   ADRSW,C'Y'          SHOW ADDRESS ?                               
         BNE   COMP55              NO - CHECK TELEPHONE                         
         CLI   ADRDUP,C'X'         SAME ADDRESS ?                               
         BNE   COMP50C             NO                                           
         CLI   BYTE,1              ONLY SKIP IF I HAVEN'T ALREADY               
         BE    COMP50A                                                          
         GOTO1 ACLPRT              SKIP A LINE                                  
*                                                                               
COMP50A  DS    0H                                                               
         MVC   P1+49(30),=CL30'ADDRESS SAME AS PRIOR'                           
         GOTO1 ACLPRT                                                           
         MVI   BYTE,0              FORCE A LINE SKIP AFTER                      
         B     COMP55              GO DO TELEPHONE                              
*                                                                               
COMP50C  CLI   BYTE,1              ONLY SKIP IF I HAVEN'T ALREADY               
         BE    COMP50E                                                          
         GOTO1 ACLPRT              SKIP A LINE                                  
*                                                                               
COMP50E  MVC   P1+49(L'ADRNAM),ADRNAM                                           
         GOTO1 ACLPRT                                                           
         MVI   BYTE,0                                                           
         CLI   ADRZNAM,C' '                                                     
         BNH   COMP50F                                                          
         MVC   P1+49(L'ADRZNAM),ADRZNAM                                         
         GOTO1 ACLPRT                                                           
         MVI   BYTE,0                                                           
*                                                                               
COMP50F  CLI   ADRATTN,C' '                                                     
         BNH   COMP50G                                                          
         MVC   P1+49(5),=C'ATTN='                                               
         MVC   P1+55(L'ADRATTN),ADRATTN                                         
         GOTO1 ACLPRT                                                           
         MVI   BYTE,0                                                           
*                                                                               
COMP50G  CLI   ADRLIN1,C' '                                                     
         BNH   COMP50J                                                          
         MVC   P1+49(L'ADRLIN1),ADRLIN1                                         
         GOTO1 ACLPRT                                                           
         MVI   BYTE,0                                                           
*                                                                               
COMP50J  CLI   ADRLIN2,C' '                                                     
         BNH   COMP55                                                           
         MVC   P1+49(L'ADRLIN2),ADRLIN2                                         
         GOTO1 ACLPRT                                                           
         MVI   BYTE,0                                                           
*                                  TELEPHONE                                    
COMP55   CLI   ADRTEL,C' '                                                      
         BNH   COMP60              GO DO FAX                                    
         CLI   TFXSW,C'T'          SHOW TELEPHONE ?                             
         BE    COMP55D             YES                                          
         CLI   TFXSW,C'B'          SHOW TELEPHONE AND FAX ?                     
         BNE   COMP60              NO                                           
COMP55D  CLI   BYTE,1              ONLY SKIP IF I HAVEN'T ALREADY               
         BE    COMP55P                                                          
         GOTO1 ACLPRT              SKIP A LINE                                  
*                                                                               
COMP55P  MVC   P1+49(4),=C'TEL='                                                
         MVC   P1+54(L'ADRTEL),ADRTEL                                           
         GOTO1 ACLPRT                                                           
         MVI   BYTE,1                                                           
*                                  FAX                                          
COMP60   CLI   ADRFAX,C' '                                                      
         BNH   COMP70              GO DO E-MAIL                                 
         CLI   TFXSW,C'F'          SHOW FAX ?                                   
         BE    COMP60D             YES                                          
         CLI   TFXSW,C'B'          SHOW TELEPHONE AND FAX ?                     
         BNE   COMP70              NO                                           
COMP60D  CLI   BYTE,1              ONLY SKIP IF I HAVEN'T ALREADY               
         BE    COMP60J                                                          
         GOTO1 ACLPRT              SKIP A LINE                                  
*                                                                               
COMP60J  DS    0H                                                               
*                                                                               
         CLC   =C'FX=',ADRFAX      CONTROL FILE FAX ?                           
         BNE   COMP60P             NO                                           
*                                                                               
         OC    ADRFAX,SPACES                                                    
         MVC   MYFAX,ADRFAX        NEEDED FOR FAXING                            
*                                                                               
         GOTO1 =A(GETFAX),DMCB,RR=Y          PUT REAL FAX IN TOFAX              
*                                                                               
         CLC   TOFAX,SPACES        CONTROL FILE FAX RECORD NOT FOUND            
         BE    COMP60P             LEAVE ADRFAX AS IS                           
         MVC   ADRFAX,TOFAX        REPLACE WITH CONTROL FILE FAX                
*                                                                               
COMP60P  MVC   P1+49(4),=C'FAX='                                                
         MVC   P1+54(L'ADRFAX),ADRFAX                                           
         GOTO1 ACLPRT                                                           
         MVI   BYTE,1                                                           
*                                  E-MAIL                                       
COMP70   CLI   ADREML,C' '                                                      
         BNH   COMP75              FINISH "ADDRESS" HANDLING                    
         CLI   EMLSW,C'Y'          SHOW E-MAIL ?                                
         BNE   COMP75              NO                                           
         CLI   BYTE,1              ONLY SKIP IF I HAVEN'T ALREADY               
         BE    COMP70P                                                          
         GOTO1 ACLPRT              SKIP A LINE                                  
*                                                                               
COMP70P  DS    0H                                                               
*                                                                               
         MVC   P1+49(7),=C'E-MAIL='                                             
         MVC   P1+57(L'ADREML),ADREML                                           
         GOTO1 ACLPRT                                                           
         MVI   BYTE,1                                                           
*                                                                               
COMP75   DS    0H                                                               
         CLI   OVRSRCE,0           ANY "OVERRIDES" FROM CONTRACT ?              
         BNH   COMP80              NO - FINISHED WITH "ADDRESSES"               
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         MVC   WORK(5),=C'NOTE:'                                                
         LA    RE,5(RE)            BUMP OVER                                    
         TM    OVRSRCE,X'01'       ATTN OVERRIDE ?                              
         BNO   COMP75B             NO                                           
         MVC   1(5,RE),=C'ATTN,'                                                
         LA    RE,6(RE)            BUMP OVER                                    
COMP75B  TM    OVRSRCE,X'02'       TEL OVERRIDE ?                               
         BNO   COMP75D             NO                                           
         MVC   1(4,RE),=C'TEL,'                                                 
         LA    RE,5(RE)            BUMP OVER                                    
COMP75D  TM    OVRSRCE,X'04'       FAX OVERRIDE ?                               
         BNO   COMP75F             NO                                           
         MVC   1(3,RE),=C'FAX'                                                  
         LA    RE,5(RE)            BUMP OVER                                    
COMP75F  BCTR  RE,0                MOVE LEFT 1 SPACE                            
         CLI   0(RE),C','                                                       
         BNE   COMP75J                                                          
         MVI   0(RE),C' '          CLEAR COMMA                                  
COMP75J  LA    RE,1(RE)            BUMP OVER                                    
         MVC   0(29,RE),=C'FROM CONTRACT SCREEN OVERRIDE'                       
         GOTO1 ACLPRT              SKIP A LINE                                  
         MVC   P1+49(L'WORK),WORK                                               
         GOTO1 ACLPRT                                                           
*                                                                               
COMP80   DS    0H                                                               
         MVC   PADRDATA,ADRDATA    SAVE THE ADDRESS INFO                        
         MVI   BYTE,0              FOR LINE SKIP AFTER ADDRESSES                
*                                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         LA    R2,PCONREC+33       ATTN OVERRIDE                                
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVI   ELCOD,X'50'                                                      
         BAS   RE,NEXTEL                                                        
         BNE   COMP91                                                           
         USING PCATELD,R2                                                       
*                                                                               
COMP90   CLC   PCATPCT,=2X'00'      SEE IF CD OVERRIDEN                         
         BE    COMP90M                                                          
         CLI   BYTE,1                                                           
         BE    COMP90C                                                          
         GOTO1 ACLPRT                                                           
COMP90C  MVC   P1+49(11),=C'CASH DISC.='                                        
         CLC   PCATPCT,=2X'FF'                                                  
         BNE   COMP90F                                                          
         MVC   P1+61(3),=C'0.0'                                                 
         B     COMP90J                                                          
*                                                                               
COMP90F  EDIT  PCATPCT,(4,P1+61),1                                              
COMP90J  GOTO1 ACLPRT                                                           
         MVI   BYTE,1                                                           
*                                                                               
COMP90M  OC    PCATMAX,PCATMAX                                                  
         BZ    COMP90P                                                          
         CLI   BYTE,1                                                           
         BE    COMP90N                                                          
         GOTO1 ACLPRT              SKIP A LINE                                  
COMP90N  MVC   P1+49(23),=C'MAXIMUM BUYS PER ISSUE='                            
         EDIT  (B2,PCATMAX),(4,P1+73),0,ALIGN=LEFT                              
         GOTO1 ACLPRT                                                           
         MVI   BYTE,1                                                           
*                                                                               
COMP90P  OC    PCATMAXZ,PCATMAXZ                                                
         BZ    COMP91                                                           
         CLI   BYTE,1                                                           
         BE    COMP90S                                                          
         GOTO1 ACLPRT              SKIP A LINE                                  
COMP90S  MVC   P1+49(49),=C'MAXIMUM BUYS PER ISSUE ACROSS ZONES AND EDIX        
               TIONS='                                                          
*                                                                               
         EDIT  (B2,PCATMAXZ),(4,P1+99),0,ALIGN=LEFT                             
         GOTO1 ACLPRT                                                           
         MVI   BYTE,1                                                           
*                                                                               
*                                                                               
COMP91   DS    0H                                                               
         CLI   QOPT2,C'Y'          SEE IF PRINTING FREE FORM COMMENTS           
         BNE   COMPX                                                            
*                                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         MVI   BYTE,0                                                           
         MVI   ELCOD,X'40'                                                      
         LA    R2,PCONREC+33                                                    
*                                                                               
         DROP  RF                                                               
*                                                                               
COMP92   BAS   RE,NEXTEL                                                        
         BNE   COMPX                                                            
         CLI   BYTE,0                                                           
         BNE   COMP94                                                           
         GOTO1 ACLPRT              SKIP A LINE BEFORE 1ST COMMENT               
         MVI   BYTE,1                                                           
COMP94   BAS   RE,PRTCOM                                                        
         B     COMP92                                                           
*                                                                               
         DROP  R2,R4                                                            
*                                                                               
COMPX    XMOD1 1                                                                
         EJECT                                                                  
PRTCOM   NTR1                                                                   
         LA    R4,2(R2)                                                         
         SR    R5,R5                                                            
*                                                                               
         CLC   0(2,R4),=C'E-'      E- COMMENTS DONT PRINT ON CONTRACTS          
         BE    PRTCOMX                                                          
         CLC   0(3,R4),=C'RC='                                                  
         BE    PRTCOMX                                                          
*                                                                               
PRTCOM2  IC    R5,1(R2)                                                         
         SH    R5,=H'2'                                                         
         CLI   0(R4),C'+'                                                       
         BNE   PRTCOM3                                                          
         MVC   SPACING,1(R4)                                                    
         NI    SPACING,X'0F'                                                    
         CLI   SPACING,3                                                        
         BNH   *+8                                                              
         MVI   SPACING,3                                                        
         CLI   SPACING,0                                                        
         BNE   *+8                                                              
         MVI   SPACING,1                                                        
         LA    R4,2(R4)                                                         
         SH    R5,=H'2'                                                         
         GOTO1 ACLPRT                                                           
         B     PRTCOM6                                                          
*                                                                               
PRTCOM3  CLC   0(2,R4),=C'E+'                                                   
         BNE   PRTCOM6                                                          
         LA    R4,2(R4)            DONT PRINT E+                                
         SH    R5,=H'2'                                                         
*                                                                               
PRTCOM6  LTR   R5,R5                                                            
         BNP   PRTCOMX                                                          
         BCTR  R5,0                                                             
         EX    R5,MVCOM                                                         
         GOTO1 ACLPRT                                                           
PRTCOMX  XIT1                                                                   
*                                                                               
MVCOM    MVC   P1+49(0),0(R4)                                                   
         EJECT                                                                  
*                                                                               
* GET FAX NUMBER FROM CONTROL FILE FAX RECORD                                   
*                                                                               
GETFAX   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   TOFAX,SPACES                                                     
         MVC   SIOKEY,KEY       SAVE KEY                                        
GETFAXB  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTFXKEY,R4                                                       
         MVI   CTFXKTYP,CTFXEQU                                                 
         MVC   CTFXAGY,QAGENCY                                                  
         MVC   CTFXCODE,MYFAX+3                                                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',KEY,IO                     
         CLC   IO(18),KEYSAVE      COMPARE 7-BYTE FAX CODE                      
         BNE   GETFAXX                                                          
         LA    R4,IO                                                            
         LA    RE,CTFXEL1                                                       
         B     GETFX4                                                           
         SPACE 1                                                                
GETFX2   ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         SPACE 1                                                                
GETFX4   CLI   0(RE),0                                                          
         BE    GETFAXX                                                          
         CLI   0(RE),CTFX1ELQ                                                   
         BE    GETFXNO                                                          
         B     GETFX2                                                           
*                                                                               
         USING CTFX1EL,RE                                                       
GETFXNO  ZIC   R1,CTFX1LEN         FAX NUMBER                                   
         AHI   R1,-3                                                            
         CHI   R1,24                                                            
         BL    *+8                                                              
         LA    R1,24                                                            
         EX    R1,*+8                                                           
         B     GETFX2                                                           
         MVC   TOFAX(0),CTFX1NUM                                                
*                                                                               
GETFAXX  MVC   KEY,SIOKEY      RESTORE MY KEY                                   
         B     GETFAXXX                                                         
*                                                                               
SIOKEY   DS    CL32                                                             
IO       DS    CL500                                                            
*                                                                               
GETFAXXX XIT1                                                                   
*                                                                               
         DROP  R4,RE                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
CONSCHD  CSECT                                                                  
         NMOD1 0,CONSCHD                                                        
*                                                                               
         LA    R7,CONSCHD+4095                                                  
         LA    R7,1(R7)                                                         
         USING CONSCHD+4096,R7                                                  
*                                                                               
         L     RC,PPFILEC                                                       
         LR    R3,RC                                                            
         AHI   R3,4096                                                          
         USING PPFILED,RC,R3             R3 NEEDED FOR PUBREC ADDR              
*                                                                               
         ZAP   OAGYCNT,=P'0'       INITIALIZE OTHER AGY COUNTER                 
         XC    LASTOAGY,LASTOAGY                                                
*                                                                               
         CLC   QPROG(2),=C'AU'     SEE IF ADVERTISER                            
         BNE   CSCLT1X                                                          
*                                                                               
         LA    R4,PCLTREC+33                                                    
CSCLT0   CLI   0(R4),X'15'                                                      
         BE    CSCLT0C                                                          
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   CSCLT0                                                           
         DC    H'0'                    INVALID CLIENT                           
*                                                                               
CSCLT0C  MVC   SADVDATA,2(R4)          SAVE ADV DATA                            
*                                                                               
*                                                                               
*        GOTO GETADVC TO GET LIST OF AGYS/CLTS                                  
*                                                                               
*        MUST SWITCH TO THE CONTROL SYSTEM                                      
*                                                                               
         USING PCLTADVE,R4                                                      
*                                                                               
CSCLT1   L     RF,UTL                                                           
         MVI   4(RF),X'0A'                                                      
         XC    WORK(20),WORK                                                    
         MVI   WORK,C'P'                                                        
         MVC   WORK+1(1),PCLTKMED                                               
         MVC   WORK+2(2),PCLTAOR                                                
         MVC   WORK+4(3),PCLTADV                                                
*                                                                               
         CLC   QESTEND,SPACES     SEE IF AGENCY FILTER GIVEN                    
         BE    CSCLT1A0                                                         
         MVI   WORK+7,C'*'                                                      
         MVC   WORK+8(3),QESTEND                                                
         CLI   QESTEND+2,C' '     WILL BE NON BLANK IF FILTER                   
         BNE   CSCLT1A0                                                         
         MVC   WORK+7(3),QESTEND     ONE AGENCY                                 
*                                                                               
         DROP  R4                                                               
*                                                                               
CSCLT1A0 DS    0H                                                               
         GOTO1 AGETADVC,DMCB,WORK,AADVCTAB,DATAMGR                              
*                                                                               
*                                                                               
         MVC   ANEXTAC,AADVCTAB     ADDRESS OF NEXT AGY/CLT                     
*                                                                               
         XC    OPENSES,OPENSES      LIST OF OPENED FILES                        
         L     RF,UTL                                                           
         MVC   OPENSES(1),4(RF)     SINCE MY FILE IS ALREADY OPENED             
         B     CSCLT1B                                                          
*                                                                               
CSCLT1A  DS    0H                   GET NEXT AGY/CLT                            
         L     R6,ANEXTAC                                                       
         LA    R6,GETVLEN(R6)                                                   
         ST    R6,ANEXTAC                                                       
         B     CSCLT1C                                                          
*                                                                               
CSCLT1B  DS    0H                    PROCESS NEXT/FIRST AGY/CLT                 
         L     R6,ANEXTAC                                                       
CSCLT1C  CLC   0(2,R6),=X'FFFF'      END                                        
         BNE   CSCLT1C5              DONE                                       
         BAS   RE,CSENDAGY           FINISH LAST AGY                            
         B     CONL                                                             
*                                                                               
         USING GETADVCD,R6                                                      
*                                                                               
CSCLT1C5 L     RF,UTL                                                           
         MVC   4(1,RF),GETVSE       SWITCH TO AGENCY FILE                       
         LA    R2,OPENSES                                                       
CSCLT1D  CLC   0(1,R2),GETVSE                                                   
         BE    CSCLT1G                                                          
         CLI   0(R2),0                                                          
         BNE   CSCLT1E                                                          
         MVC   0(1,R2),GETVSE      SET OPENED                                   
*                                                                               
*        OPEN THE PRINTFILE                                                     
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),GETVSE                                                   
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'PRINT',PRTFILES,PPBYOWRK                
         B     CSCLT1G                                                          
*                                                                               
CSCLT1E  LA    R2,1(R2)                                                         
         B     CSCLT1D                                                          
*                                                                               
CSCLT1G  L     RF,UTL                                                           
         MVC   4(1,RF),GETVSE       SWITCHES ME TO AGENCY FILE                  
         CLC   WORKCLT(2),GETVAGY                                               
         BE    CSCLT1K                                                          
         CLI   WORKCLT,0            SEE IF FIRST TIME                           
         BE    CSCLT1K                                                          
*                                                                               
         BAS   RE,CSENDAGY                                                      
*                                                                               
CSCLT1K  DS    0H                                                               
*                                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         MVC   WORKCLT(2),GETVAGY                                               
         MVC   WORKCLT+2(1),QMEDIA                                              
         MVC   WORKCLT+4(3),GETVACLT                                            
         MVC   WORKPUB,PCONKPUB                                                 
*                                                                               
         TM    GETVCNTL,X'01'          SEE IF PUB LINK                          
         BZ    CS4                                                              
*          FIND PUB LINK                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'FD'                                                        
         MVC   KEY+1(1),QMEDIA                                                  
         MVC   KEY+2(3),QCLIENT                                                 
         MVC   KEY+5(2),QAGENCY                                                 
         MVC   KEY+7(2),WORKCLT         AGENCY                                  
         MVC   KEY+9(6),PCONKPUB                                                
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 HIGHPUB                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   CSCLT1A               LINK NOT FOUND - SKIP TO NEXT AGY          
         MVC   WORKPUB,KEY+15                                                   
         B     CS4                                                              
*                                                                               
***      THIS CODE FOR QPROG=19                                                 
*                                                                               
CSCLT1X  DS    0H                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         MVC   WORKCLT,PCLTKEY                                                  
         MVC   WORKPUB,PCONKPUB                                                 
*                                                                               
         DROP  RF                                                               
*                                                                               
         CLI   PCLTPROF+5,C'1'     TEST THIS A MASTER CLIENT                    
         BNE   CS4                                                              
* READ THRU CLTHDRS FOR SLAVE CLIENTS                                           
         XC    WORKCLT+4(4),WORKCLT+4        FOR FIRST TIME                     
*                                                                               
CSCLT2   XC    KEY,KEY                                                          
         MVC   KEY(7),WORKCLT                                                   
         MVI   KEY+7,X'FF'                                                      
         GOTO1 HIGH                                                             
         B     CSCLT4A                                                          
CSCLT4   GOTO1 SEQ                                                              
CSCLT4A  CLC   KEY(4),KEYSAVE                                                   
         BE    CSCLT6                                                           
         CLI   WORKCLT+4,X'FF'     SEE IF I'VE SEARCHED FOR                     
         BE    CONL                OTHER AGY DATA UNDER THE MASTER              
*                                  IF YES THEN I'M DONE                         
         XC    KEY,KEY             ELSE DO MASTER                               
         MVC   KEY(7),PCLTKEY                                                   
         MVI   WORKCLT+4,X'FF'                                                  
         B     CS4A                                                             
CSCLT6   LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
*                                                                               
CSCLT8   BAS   RE,SGET                                                          
*                                                                               
         CLC   PCLTKCLT,PCLTPROF+6-PCLTKEY+PBUYREC                              
         BNE   CSCLT4                                                           
*                                                                               
         MVC   WORKCLT,PBUYREC     SAVE THIS CLIENT KEY                         
*                                                                               
*                                                                               
CS4      DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(7),WORKCLT      A/M/X/CLT                                    
CS4A     DS    0H                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         MVI   KEY+3,X'21'                                                      
         MVC   KEY+7(6),WORKPUB                                                 
*                                  NOTE - IF DOING ALL ZONES,EDTS               
*                                  THEN PCONPRD IS FROM FIRST CON READ          
         CLI   PCONPRD,C'A'        SEE IF DOING A PRD-CONTRACT                  
         BL    *+10                                                             
         MVC   KEY+13(3),PCONPRD                                                
         JIF   QPRODUCT,EQ,=C'ALL',OR,QPRODUCT,EQ,=C'   ',CS4B,JUMP=N           
         MVC   KEY+13(3),QPRODUCT                                               
*                                                                               
         DROP  RF                                                               
*                                                                               
CS4B     DS    0H                                                               
**NEW 2/17/88                                                                   
         OC    KEY+13(3),KEY+13      SEE IF DOING ONE PRD                       
         BZ    CS4C                                                             
         MVC   KEY+16(3),BQSTART     SET DATE IN KEY                            
**NEW 2/17/88                                                                   
CS4C     GOTO1 HIGH                                                             
         B     CS6A                                                             
CS6      MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
CS6A     DS    0H                                                               
         CLC   KEY(16),KEYSAVE     SAME A/M/X/C/PUB/PRD                         
         BNE   CSENDPRD                                                         
*                                                                               
CS7      CLC   KEY+16(3),BQSTART                                                
**NEW 2/17/88                                                                   
         BL    CS7LOW                                                           
**NEW 2/17/88                                                                   
         CLC   KEY+16(3),BQEND                                                  
**NEW 2/17/88                                                                   
         BH    CS7HIGH                                                          
**NEW 2/17/88                                                                   
*                                                                               
         B     CS7H                 SKIP                                        
**NEW 2/17/88                                                                   
**NEW 2/17/88       LOGIC TO CHK PRD OMITTED SINCE IF ONE PRODUCT               
**NEW 2/17/88       IT IS SET IN KEY IN CS4A                                    
**NEW 2/17/88       ALSO CHG OF PRODUCT IS CHECKED IN CS6A                      
**NEW 2/17/88                                                                   
*                                                                               
CS7LOW   MVC   KEY+16(3),BQSTART                                                
         XC    KEY+19(6),KEY+19       MUST CLEAR EST                            
         B     CS4C                                                             
*                                                                               
CS7HIGH  MVC   KEY+16(3),=3X'FF'     TO GET NEXT PRD                            
         XC    KEY+19(6),KEY+19       CLEAR EST                                 
         B     CS4C                                                             
**NEW 2/17/88                                                                   
*                                                                               
CS7H     DS    0H                                                               
**NEW 2/17/88                                                                   
**NEW 2/17/88          DON'T READ EST FOR TEST STATUS ANYMORE                   
**NEW 2/17/88                                                                   
*                                                                               
CS7M     LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,SGET                                                          
***TESTBUY***                                                                   
         CLI   QOPT6,C'Y'          SEE IF INCLUDING TEST BUYS                   
*NOP*    BE    CS7M1                                                            
         BE    CS7M0                                                            
*                                                                               
         CLI   PBDBFD,C'T'         SKIP TEST BUYS FOR ANALYSIS                  
         BE    CS6                                                              
***TESTBUY***                      NEED TO SET FOR OTHER AGY DATA               
*                                                                               
CS7M0    CLI   QOPT9,C'H'          SEE IF EXCLUDING HELD SFH BUYS               
         BNE   CS7M1               NO                                           
*                                                                               
         TM    PBDSTAT,X'08'       HELD SFH BUY ?                               
         BO    CS6                 YES - SKIP                                   
*                                                                               
CS7M1    GOTO1 GETINS,DMCB,PBUYREC,GROSS,(C'Y',PBUYKPRD)                        
         B     PBUY                                                             
*                                                                               
CS7M2    GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKPRD                               
         EJECT                                                                  
PBUY     DS    0H                  PROCESS BUY                                  
*                                                                               
         CLI   PROGPROF+3,C'N' SEE IF INCLUDING TEST BUYS IN TOTALS             
         BNE   PBUY0            FOR 19T REQS                                    
         CLI   PBDBFD,C'T'         SEE IF TEST BUY                              
         BE    PBUY1               DON'T POST TO PREMIUM TOTALS                 
         DS    0H                                                               
*                                                                               
PBUY0    DS    0H                                                               
*                                                                               
*                                                                               
PBUY1    CLI   TESTPASS,1                                                       
         BNE   PBUY2                                                            
         MVI   TESTPASS,0           SET BUY FOUND                               
         B     CONLX                EXIT                                        
*                                                                               
PBUY2    L     R0,PREMIUM                                                       
         CVD   R0,DUB                                                           
         AP    PROPREM,DUB                                                      
*                                                                               
         ZAP   PRONETP,PROPREM                                                  
         LA    R2,PBUYREC+33                                                    
         CLI   0(R2),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                ELEMENT MUST EXIST                           
*                                                                               
         USING PBDELEM,R2                                                       
         ZAP   DUB,PRONETP                                                      
         MP    DUB,PBDACP                                                       
         DP    DUB,=P'100000'                                                   
         SP    PRONETP,DUB(4)                                                   
         DROP  R2                                                               
*                                                                               
*                                                                               
PBUY3    MVC   ESTACT(5),=5C'Y'                                                 
*                                  PBUY4 TO PBUY70 FOR 19                       
*                                                                               
PBUY4    DS    0H                                                               
         XC    MYCU,MYCU                                                        
         MVI   CULOOKUP,0                                                       
*                                                                               
         CLC   QPROG,=C'AU'       AOR CONTRACT ULTILIZATION RPT                 
         BNE   PBUY4A                                                           
         L     RF,UTL                                                           
         MVC   4(1,RF),SAVSYS    MUST RETURN TO AOR                             
*                                TO READ PCURECS                                
*                                                                               
PBUY4A   DS    0H                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         GOTO1 APPGETCU,DMCB,PBUYREC,PCONREC,DATAMGR                            
         CLI   DMCB,X'FF'                                                       
         BE    PBUY4AH                                                          
         MVC   MYCU,DMCB+5      LAST 3 BYTES                                    
         OC    MYCU,MYCU        LOOKED UP CU VALUE PRESENT?                     
         BZ    PBUY4AH                                                          
         MVI   CULOOKUP,C'Y'                                                    
         DROP  RF                                                               
*                                                                               
PBUY4AH  CLC   QPROG,=C'AU'       AOR CONTRACT ULTILIZATION RPT                 
         BNE   PBUY4B                                                           
*                                                                               
         L     RE,ANEXTAC                                                       
G1       USING GETADVCD,RE                                                      
         L     RF,UTL                                                           
         MVC   4(1,RF),G1.GETVSE      MUST RETURN TO AGENCY FILE                
*                                                                               
PBUY4B   GOTO1 HIGH              NEEDED SINCE PPGETCU MAY DESTROY               
*                                SEQUENCIAL READING                             
*                                                                               
PBUY4D   DS    0H                                                               
*                                                                               
*****    CLI   QOPT1-1,C'Y'        SEE IF NOT PRINT DETAILS                     
         CLI   QOPT8,C'Y'          SEE IF NOT PRINT DETAILS                     
         BE    PBUY85B             SKIP TO TOTALING                             
*                                                                               
         LA    R4,P1+2                                                          
         USING BUYLND,R4                                                        
         CLC   QPROG(2),=C'AU'     SEE IF ADVERTISER REPORT                     
         BNE   PBUY4D5                                                          
         MVC   ABAGY,PBUYKAGY                                                   
         MVC   ABCLT,PBUYKCLT                                                   
         MVC   ABPRD,PBUYKPRD                                                   
*                                   DISPLAY CREATION DATE                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBDBUYDT),(5,ABNDATE)                             
*                                                                               
         B     PBUY4D6                                                          
*                                                                               
PBUY4D5  CLI   PCLTPROF+5,C'1'     SEE IF MASTER CLT                            
         BNE   *+10                                                             
         MVC   BCLT,PBUYKCLT                                                    
         MVC   BPRD,PBUYKPRD                                                    
PBUY4D6  DS    0H                                                               
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BEST,DUB                                                         
         LA    R5,PPBYOWRK                                                      
         USING PPBYOUTD,R5                                                      
         XC    PBYOINPT(24),PBYOINPT                                            
         LA    R0,PBUYREC                                                       
         L     R1,DATCON                                                        
         LA    R2,GROSS                                                         
         STM   R0,R2,0(R5)                                                      
         MVI   PBYOCTL,X'20'                                                    
         GOTO1 PPBYOUT,DMCB,PPBYOWRK                                            
         LA    R1,ABDATE-1                                                      
         CLC   QPROG(2),=C'AU'                                                  
         BE    *+8                                                              
         LA    R1,BDATE-1                                                       
         MVC   1(L'BDATE,R1),PBYOMDY                                            
***HELDBUY***                                                                   
         TM    PBDSTAT,X'08'                                                    
         BNO   *+8                                                              
         MVI   0(R1),C'H'                                                       
***HELDBUY***                                                                   
***TESTBUY***               (OVERRIDES 'H')                                     
         CLI   PBDBFD,C'T'                                                      
         BNE   PBUY4D8                                                          
         MVI   0(R1),C'T'                                                       
         TM    PBDSTAT2,X'40'      STEWARD BUY ?                                
         BNO   PBUY4D8             NO                                           
         MVI   0(R1),C'S'                                                       
***TESTBUY***                                                                   
PBUY4D8  DS    0H                                                               
         CLI   PBYOMDY2,C' '       CHECK FOR SECOND DATE                        
         BE    *+18                                                             
         MVI   133(R1),C'+'                                                     
         MVC   134(8,R1),PBYOMDY2                                               
         B     BUY4E                                                            
*                                                                               
         CLC   PBYOISNM,SPACES                                                  
         BE    BUY4E                                                            
         MVC   134(11,R1),PBYOISNM                                              
*                                                                               
BUY4E    MVC   BORATE,PBYOUR                                                    
         CLI   QMEDIA,C'N'                                                      
         BNE   PBUY30                                                           
         CLC   PBYOPRM,SPACES                                                   
         BNH   PBUY4G                                                           
         LA    R1,BORATE                                                        
         CLC   BORATE,SPACES       SEE IF P USED                                
         BNH   *+8                                                              
         LA    R1,BORATE+132       YES - USE P2                                 
         MVC   0(11,R1),PBYOPRM                                                 
PBUY4G   DS    0H                                                               
         CLI   PBYOSPC,C' '        SEE IF I HAVE A SPACE DESCRIPTION            
         BE    PBUY5                                                            
         MVC   BDESC(10),PBYOSPC                                                
         B     PBUY10                                                           
*                                                                               
PBUY5    DS    0H                  NO SPACE SO SHOW UNITS THERE                 
         MVC   BDESC(L'PBYOUNTS),PBYOUNTS                                       
*                                                                               
PBUY10   MVC   BLINES,PBYOUNTS                                                  
         LA    R2,5                                                             
         LA    R1,BLINES                                                        
PBUY15   CLI   BLINES+6,C' '                                                    
         BNE   PBUY35                                                           
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R1),PBYOUNTS                                                 
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         BCT   R2,PBUY15                                                        
         B     PBUY35                                                           
*                                                                               
PBUY30   MVC   BDESC(20),PBYOSPC     NON-NEWSPAPERS                             
         MVC   BDESC+132(20),PBYOSPC2                                           
*                                                                               
PBUY35   EQU   *                                                                
         LA    R2,DOLTYPS                                                       
         LA    R6,BOGROSS                                                       
PBUY38   CLI   0(R2),X'FF'                                                      
         BE    PBUY70                                                           
         CLI   0(R2),C'X'                                                       
         BE    PBUY65                                                           
         CLI   0(R2),C'G'                                                       
         BNE   PBUY40                                                           
         EDIT  (B4,GROSS),(14,0(R6)),2,COMMAS=YES,FLOAT=-                       
         B     PBUY65                                                           
PBUY40   CLI   0(R2),C'C'                                                       
         BNE   PBUY45                                                           
         EDIT  (B4,CSHDSC),(14,0(R6)),2,COMMAS=YES,FLOAT=-                      
         B     PBUY65                                                           
PBUY45   CLI   0(R2),C'2'       NET-CD                                          
         BNE   PBUY50                                                           
         EDIT  (B4,PYABLE),(14,0(R6)),2,COMMAS=YES,FLOAT=-                      
         B     PBUY65                                                           
PBUY50   CLI   0(R2),C'N'        NET                                            
         BNE   PBUY55                                                           
         L     R0,PYABLE                                                        
         A     R0,CSHDSC                                                        
         ST    R0,FULL2                                                         
         EDIT  (B4,FULL2),(14,0(R6)),2,COMMAS=YES,FLOAT=-                       
         B     PBUY65                                                           
PBUY55   CLI   0(R2),C'1'        GROSS-CD                                       
         BNE   PBUY65                                                           
         L     R0,GROSS                                                         
         S     R0,CSHDSC                                                        
         ST    R0,FULL2                                                         
         EDIT  (B4,FULL2),(14,0(R6)),2,COMMAS=YES,FLOAT=-                       
         B     PBUY65                                                           
*                                                                               
PBUY65   LA    R2,1(R2)                                                         
         LA    R6,15(R6)                                                        
         B     PBUY38                                                           
*                                                                               
PBUY70   EQU   *                   FIND AND PRINT COMMENTS                      
         CLI   BDATE+132,C' '      SEE IF P2 USED                               
         BE    PBUY73                                                           
         GOTO1 ACLPRT                                                           
         LA    R6,BDATE                                                         
         B     PBUY75                                                           
PBUY73   LA    R6,BDATE+132        USE P2 FOR FIRST COMMENT                     
PBUY75   LA    R2,PBUYREC+33                                                    
         MVI   ELCOD,X'66'                                                      
PBUY76   BAS   RE,SNEXTEL                                                       
         BNE   PBUY80                                                           
         ZIC   R5,1(R2)            ELEM LENGHT                                  
         SH    R5,=H'3'            ADJUST FOR CODE + LENGHT                     
         BM    PBUY76              AND EXEC                                     
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),2(R2)       EXECUTED                                     
         GOTO1 ACLPRT                                                           
         LA    R6,BDATE            RESET TO P1                                  
         B     PBUY76                                                           
*                                                                               
PBUY80   CLC   P1,SPACES           SEE IF ANYTHING LEFT TO PRINT                
         BE    PBUY85              NO                                           
         GOTO1 ACLPRT                                                           
PBUY85   DS    0H                                                               
         OC    MYCU,MYCU           SEE IF CU= OVERRIDEN                         
         BZ    PBUY85B                                                          
         CLC   MYCU,=X'000001'      MEANS ZERO                                  
         BNE   PBUY85A0                                                         
         MVC   BDATE(6),=C'(CU=0)'                                              
         B     PBUY85A8                                                         
*                                                                               
PBUY85A0 MVC   BDATE(4),=C'(CU='                                                
         EDIT  (B3,MYCU),(12,W),4,COMMAS=YES,ALIGN=LEFT,DROP=4                  
         LA    R1,11                                                            
         LA    RF,W+11                                                          
PBUY85A  CLI   0(RF),C'.'                                                       
         BE    PBUY85A5                                                         
         CLI   0(RF),C' '                                                       
         BNE   PBUY85A7                                                         
PBUY85A5 SH    RF,=H'1'                                                         
         BCT   R1,PBUY85A                                                       
*                                                                               
PBUY85A7 DS    0H                                                               
         CLI   CULOOKUP,C'Y'       CU VALUE IS LOOKED UP?                       
         BNE   *+16                                                             
         MVI   1(RF),C'*'          INDICATES THAT CU IS LOOKED UP               
         AHI   RF,1                                                             
         AHI   R1,1                                                             
*                                                                               
         MVI   1(RF),C')'                                                       
         AHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BDATE+4(0),W                                                     
*                                                                               
PBUY85A8 GOTO1 ACLPRT                                                           
*                                                                               
PBUY85B  CLI   PROGPROF+3,C'N' SEE IF INCLUDING TEST BUYS IN TOTALS             
         BNE   PBUY85C          FOR 19T REQS                                    
         CLI   PBDBFD,C'T'         SEE IF TEST BUY                              
         BE    CS6                 DON'T POST TO TOTALS                         
*                                                                               
PBUY85C  DS    0H                                                               
         CLI   QOPT9,C'H'          SEE IF EXCLUDING HELD SFH BUYS               
         BNE   PBUY85D             NO                                           
*                                                                               
         TM    PBDSTAT,X'08'       HELD SFH BUY ?                               
         BO    CS6                 YES - DON'T POST TO TOTALS                   
*                                                                               
PBUY85D  DS    0H                                                               
         L     R0,GROSS                                                         
         CVD   R0,DUB                                                           
         AP    PROGRS,DUB                                                       
         L     R0,CSHDSC                                                        
         CVD   R0,DUB                                                           
         AP    PROCD,DUB                                                        
         L     R0,PYABLE           NET                                          
         CVD   R0,DUB                                                           
         AP    PRONET,DUB                                                       
         OC    MYCU,MYCU           CHK FOR CONTRACT UNITS OVERRIDE              
         BZ    PBUY85M                                                          
         CLC   MYCU,=X'000001'      SPECIAL FOR ZERO                            
         BE    PBUY86               BYPASS CONTRACT UNITS COUNTER               
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),MYCU                                                   
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         AP    PROINS,DUB                                                       
         B     PBUY86                                                           
*                                                                               
PBUY85M  CLI   PBDSPACE,C'*'       SEE IF REAL INS                              
         BE    PBUY86                                                           
         CLI   PBUYKPRD,C'*'        SEE IF OTHER AGENCY BUY                     
         BNE   PBUY85X                                                          
         LA    R2,PBUYREC+33       FIND NUMBER OF INS IN COMMENT                
         MVI   ELCOD,X'66'                                                      
PB85B    BAS   RE,SNEXTEL                                                       
         BNE   PB85R               NOT FOUND                                    
         LA    R4,13               11+2                                         
         CLC   2(11,R2),=C'INSERTIONS='                                         
         BE    PB85F                                                            
         LA    R4,6                4+2                                          
         CLC   2(4,R2),=C'INS='    ALSO ACCEPT INS=                             
         BE    PB85F                                                            
         LA    R4,10               8+2                                          
         CLC   2(8,R2),=C'INSERTS='   OR INSERTS=                               
         BE    PB85F                                                            
         LA    R4,11               9+2                                          
         CLC   2(9,R2),=C'POSTINGS='    OR POSTINGS=                            
         BE    PB85F                                                            
         B     PB85B                                                            
*                                                                               
PB85F    ZIC   R1,1(R2)            CALC NUMBER OF DIGITS                        
         SR    R1,R4                                                            
         BNP   PB85B               MUST HAVE AT LEAST ONE                       
         AR    R2,R4               POINT TO FIRST DIGIT                         
         SR    R5,R5               USED TO COUNT DIGITS                         
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
PB85G    CLI   0(R2),C'0'                                                       
         BL    PB85H               STOP ON NON-DIGIT                            
         CLI   0(R2),C'9'                                                       
         BH    PB85H                                                            
         LA    R5,1(R5)                                                         
         MVC   0(1,R6),0(R2)                                                    
         LA    R6,1(R6)                                                         
         LA    R2,1(R2)                                                         
         BCT   R1,PB85G                                                         
*                                                                               
PB85H    LTR   R5,R5               SEE IF I HAVE ANY DIGITS                     
         BZ    PB85B               NO TRY FOR ANOTHER ELEM                      
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         MP    DUB,=P'10000'                                                    
         AP    PROINS,DUB                                                       
         B     PBUY86                                                           
*                                                                               
PB85R    EQU   *                                                                
PBUY85X  AP    PROINS,=P'10000'       NOW 4 DECIMALS                            
*                                                                               
PBUY86   EQU   *                                                                
         ZAP   DUB,PBDUNITS                                                     
         CLI   PBDUIND,C'I'                                                     
         BE    PBUY86B                                                          
         CLI   PBDUIND,X'89'       LOWER CASE I                                 
         BNE   PBUY86F             HAS UNITS TO 2 DECIMALS                      
PBUY86B  DS    0H                                                               
         CLI   PBDUIND,X'89'       LOWER CASE I                                 
         BE    PBUY86D             HAS UNITS TO 2 DECIMALS                      
         MP    DUB,=P'100'         TO GET 2 DECIMALS                            
PBUY86D  AP    PROINCH,DUB                                                      
*                                                                               
         CLI   PROGPROF+1,C'N'     SEE IF EQUIVALENCING TO LINES                
         BE    PBUY86X             NO                                           
         MP    DUB,=P'14'                                                       
         AP    DUB,=P'50'                                                       
         DP    DUB,=P'100'        CONVERT TO LINES                              
         AP    PROLINES,DUB(6)                                                  
         B     PBUY86X                                                          
*                                                                               
PBUY86F  DS    0H                  MUST BE LINES                                
         AP    PROLINES,PBDUNITS                                                
         CLI   PROGPROF+1,C'N'      SEE IF EQUIVALENCING TO INCHES              
         BE    PBUY86X                                                          
         ZAP   DUB,PBDUNITS                                                     
         MP    DUB,=P'100'                                                      
         DP    DUB,=P'14'                                                       
         AP    PROINCH,DUB(6)                                                   
PBUY86X  B     CS6           GO DO NEXT BUY                                     
         EJECT                                                                  
CSENDPRD DS    0H                                                               
         CLI   PRDACT,C'Y'                                                      
         BNE   CSENDP5                                                          
         MVC   P1+30(20),=C'** PRODUCT TOTALS **'                               
*****    CLI   QOPT1-1,C'Y'        SEE IF SUPPRESSING DETAILS                   
         CLI   QOPT8,C'Y'          SEE IF SUPPRESSING DETAILS                   
         BNE   *+16                                                             
         MVC   P1+25(7),=C'**     '                                             
         MVC   P1+28(3),PBUYKPRD                                                
*                                                                               
         LA    R5,PROTOTS                                                       
         MVI   STARS,1                                                          
         GOTO1 APRTTOTS                                                         
         MVI   SPACING,2                                                        
         GOTO1 ACLPRT                                                           
         LA    R4,CLTTOTS                                                       
         BAS   RE,ADDCLR                                                        
CSENDP5  MVC   ESTACT(2),=2C'N'                                                 
**NEW 2/17/88                                                                   
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         CLI   PCONPRD,C'A'        SEE IF DOING A PRD CONTRACT                  
         BNL   CSENDCLT            YES - DONE                                   
         JIF   QPRODUCT,EQ,=C'ALL',OR,QPRODUCT,EQ,=C'   ',CSENDP8,     +        
               JUMP=N                                                           
         B     CSENDCLT                                                         
*                                                                               
         DROP  RF                                                               
*                                                                               
**NEW 2/17/88                                                                   
CSENDP8  CLC   KEY(13),KEYSAVE     TEST SAME A/M/X/CLT/PUB                      
         BE    CS7                 YES - PROCESS THIS KEY                       
*                                                                               
CSENDCLT DS    0H                  CLIENT END                                   
         CLI   CLTACT,C'Y'                                                      
         BNE   CSENDC5                                                          
*                                                                               
         CLC   QPROG(2),=C'AU'     SEE IF DOING ADVERTISER                      
         BE    CSENDC1             DO CLIENT TOTALS                             
         CLI   PCLTPROF+5,C'1'     SEE IF DOING MASTER CLIENT                   
         BNE   CSENDC3             NO - NO CLT TOTALS                           
CSENDC1  MVC   P1+30(19),=C'** CLIENT TOTALS **'                                
*****    CLI   QOPT1-1,C'Y'        SEE IF SUPPRESSING DETAILS                   
         CLI   QOPT8,C'Y'          SEE IF SUPPRESSING DETAILS                   
         BNE   *+16                                                             
         MVC   P1+25(7),=C'**     '                                             
         MVC   P1+28(3),PBUYKCLT                                                
         LA    R5,CLTTOTS                                                       
         MVI   STARS,1                                                          
         GOTO1 APRTTOTS                                                         
         MVI   SPACING,2                                                        
         GOTO1 ACLPRT                                                           
CSENDC3  LA    R4,CONTOTS          ROLL TO CONTRACT TOTALS                      
*                                                                               
         CLC   QPROG(2),=C'AU'     SEE IF ADVERTISER                            
         BNE   *+8                                                              
         LA    R4,AGYTOTS          ROLL TO AGYTOTS                              
*                                                                               
         LA    R5,CLTTOTS                                                       
         BAS   RE,ADDCLR                                                        
CSENDC5  MVC   ESTACT(3),=3C'N'                                                 
*                                                                               
CSENDCX  DS    0H                                                               
         CLC   QPROG(2),=C'AU'         SEE IF ADVERTISER                        
         BE    CSCLT1A                 GO DO NEXT AGY/CLT                       
*                                                                               
         CLI   PCLTPROF+5,C'1'         TEST MASTER CLIENT                       
         BNE   CONL                                                             
         B     CSCLT2              READ NEXT CLIENT                             
*                                                                               
         EJECT                                                                  
         SPACE 2                                                                
         EJECT                                                                  
CONL     DS    0H                  END OF CONTRACT                              
*                                                                               
         CLC   QPROG(2),=C'AU'     SEE IF ADVERTISER                            
         BNE   CONL5                                                            
         L     RF,UTL                                                           
         MVC   4(1,RF),SAVSYS      RETURN TO MY SYSTEM                          
*                                                                               
CONL5    CLI   TESTPASS,1                                                       
         BE    CONLX               DO NOTHING FOR TEST PASS                     
*                                                                               
         CLI   CONACT,C'Y'                                                      
         BNE   CONLX                                                            
         MVC   P1+30(21),=C'** CONTRACT TOTALS **'                              
         LA    R5,CONTOTS                                                       
         MVI   STARS,2                                                          
         GOTO1 APRTTOTS                                                         
         GOTO1 ACLPRT                                                           
         MVC   SCONTOTS(ACCNUM*8),CONTOTS    SAVE CONTRACT TOTALS FOR           
*                                      19 ANALYSIS MESSAGE                      
         LA    R4,REQTOTS                                                       
         BAS   RE,ADDCLR                                                        
         B     CONLX                                                            
*                                                                               
CONLX    XMOD1 1                                                                
         EJECT                                                                  
ADDCLR   EQU   *                                                                
         LA    R6,ACCNUM                                                        
ADDCL5   AP    0(8,R4),0(8,R5)                                                  
         LA    R4,8(R4)                                                         
         ZAP   0(8,R5),=P'0'                                                    
         LA    R5,8(R5)                                                         
         BCT   R6,ADDCL5                                                        
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
         SPACE 2                                                                
SNEXTEL  SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    SNEXTELX            RETURN WITH CC =                             
         CLC   ELCOD,0(R2)                                                      
         BCR   8,RE                                                             
         B     SNEXTEL+2                                                        
SNEXTELX LTR   R2,R2               RETURN WITH CC NOT =                         
         BR    RE                                                               
         EJECT                                                                  
CSENDAGY NTR1                                                                   
* AGENCY TOTALS                                                                 
         CLI   AGYACT,C'Y'                                                      
         BNE   CSENDA2                                                          
*                                                                               
CSENDA1  MVC   P1+30(19),=C'** AGENCY TOTALS **'                                
*****    CLI   QOPT1-1,C'Y'        SEE IF SUPPRESSING DETAILS                   
         CLI   QOPT8,C'Y'          SEE IF SUPPRESSING DETAILS                   
         BNE   *+16                                                             
         MVC   P1+26(6),=C'**     '                                             
         MVC   P1+29(2),WORKCLT       AGENCY                                    
         LA    R5,AGYTOTS                                                       
         MVI   STARS,1                                                          
         GOTO1 APRTTOTS                                                         
         MVI   SPACING,2                                                        
         GOTO1 ACLPRT                                                           
*                                                                               
CSENDA2  DS    0H                                                               
         LA    R4,CONTOTS          ROLL TO CONTOTS                              
         LA    R5,AGYTOTS                                                       
         BAS   RE,ADDCLR                                                        
*                                                                               
CSENDAX  DS    0H                                                               
         MVI   AGYACT,C'N'                                                      
         XIT1                                                                   
*                                                                               
SGET     NTR1                                                                   
         GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,IOAREA,DMWORK                 
*                                                                               
SDMX     TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
PRTFILES DS    0D                                                               
         DC    CL8' PRTDIR'                                                     
         DC    CL8' PRTFILE'                                                    
         DC    CL8' PUBDIR'                                                     
         DC    CL8' PUBFILE'                                                    
         DC    C'X'                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
PRTTOTS  CSECT                                                                  
         NMOD1 0,PRTTOTS                                                        
         LA    R2,DOLTYPS                                                       
         LA    R6,P1+77                                                         
PRTT5    CLI   0(R2),X'FF'          END OF TABLE                                
         BE    PRTTOTS2                                                         
         CLI   0(R2),C'X'                                                       
         BE    PRTT50                                                           
         CLI   0(R2),C'G'           GROSS                                       
         BNE   PRTT10                                                           
         EDIT  (P8,0(R5)),(13,0(R6)),2,COMMAS=YES,FLOAT=-                       
         B     PRTT45               GO DO *'S                                   
PRTT10   CLI   0(R2),C'C'           CD                                          
         BNE   PRTT15                                                           
         EDIT  (P8,8(R5)),(13,0(R6)),2,COMMAS=YES,FLOAT=-                       
         B     PRTT45                                                           
PRTT15   CLI   0(R2),C'2'           NET-CD                                      
         BNE   PRTT20                                                           
         EDIT  (P8,16(R5)),(13,0(R6)),2,COMMAS=YES,FLOAT=-                      
         B     PRTT45                                                           
PRTT20   CLI   0(R2),C'N'           NET                                         
         BNE   PRTT25                                                           
         ZAP   MYDUB,16(8,R5)                                                   
         AP    MYDUB,8(8,R5)                                                    
         EDIT  (P8,MYDUB),(13,0(R6)),2,COMMAS=YES,FLOAT=-                       
         B     PRTT45                                                           
*                                                                               
PRTT25   CLI   0(R2),C'1'           GROSS-CD                                    
         BNE   PRTT50                                                           
         ZAP   MYDUB,0(8,R5)                                                    
         SP    MYDUB,8(8,R5)                                                    
         EDIT  (P8,MYDUB),(13,0(R6)),2,COMMAS=YES,FLOAT=-                       
         B     PRTT45                                                           
*                                                                               
PRTT45   DS    0H                                                               
         MVI   13(R6),C'*'                                                      
         CLI   STARS,2                                                          
         BNE   PRTT50                                                           
         MVI   14(R6),C'*'                                                      
*                                                                               
PRTT50   LA    R2,1(R2)                                                         
         LA    R6,15(R6)                                                        
         B     PRTT5                                                            
*                                                                               
*                                                                               
*                                                                               
PRTTOTS2 EDIT  (P8,24(R5)),(12,W),4,COMMAS=YES,DROP=4                           
         LA    R1,11                                                            
         LA    R6,P1+48                                                         
         LA    RF,W+11                                                          
PRTTOTS3 CLI   0(RF),C'.'                                                       
         BE    PRTTOTS4                                                         
         CLI   0(RF),C' '                                                       
         BNE   PRTTOTS5                                                         
PRTTOTS4 SH    RF,=H'1'                                                         
         LA    R6,1(R6)                                                         
         BCT   R1,PRTTOTS3                                                      
*                                                                               
PRTTOTS5 EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),W                                                        
         MVC   P1+61(5),=C'PAGES'                                               
         LA    RF,P1+65            SAVE ADDR OF 'S'                             
         CLI   PLIND,C'P'                                                       
         BE    PRTTOTS6                                                         
         MVC   P1+61(5),=C'TIMES'                                               
         CLI   PLIND,C'X'                                                       
         BE    PRTTOTS6                                                         
         MVC   P1+61(10),=C'INSERTIONS'                                         
         LA    RF,P1+70                                                         
         CLI   QMEDIA,C'O'         OUTDOOR                                      
         BNE   *+14                                                             
         MVC   P1+61(10),=C'POSTINGS  '                                         
         LA    RF,P1+68                                                         
*                                                                               
PRTTOTS6 CLC   P1+58(3),=C' 1 '                                                 
         BNE   *+8                                                              
         MVI   0(RF),C' '          NO S                                         
*                                                                               
         CLI   QMEDIA,C'N'                                                      
         BNE   PRTTX10                                                          
         EDIT  (P8,32(R5)),(9,P2+51),0,COMMAS=YES                               
         MVC   P2+61(5),=C'LINES'                                               
         EDIT  (P8,40(R5)),(12,P3+48),2,COMMAS=YES                              
         MVC   P3+61(6),=C'INCHES'                                              
*                                                                               
         CP    48(8,R5),=P'0'      CHECKING PREMIUM                             
         BE    PRTTOTX                                                          
         EDIT  (P8,48(R5)),(12,P4+48),2,COMMAS=YES,FLOAT=-                      
         MVC   P4+61(5),=C'GROSS'                                               
         MVC   P4+67(15),=C'PREMIUM CHARGES'                                    
         EDIT  (P8,56(R5)),(12,P5+48),2,COMMAS=YES,FLOAT=-                      
         MVC   P5+61(04),=C'NET '                                               
         MVC   P5+65(15),=C'PREMIUM CHARGES'                                    
         B     PRTTOTX                                                          
*                                                                               
PRTTX10  DS    0H                                                               
         CP    48(8,R5),=P'0'      CHECKING PREMIUM                             
         BE    PRTTOTX                                                          
         EDIT  (P8,48(R5)),(12,P2+48),2,COMMAS=YES,FLOAT=-                      
         MVC   P2+61(5),=C'GROSS'                                               
         MVC   P2+67(15),=C'PREMIUM CHARGES'                                    
         EDIT  (P8,56(R5)),(12,P3+48),2,COMMAS=YES,FLOAT=-                      
         MVC   P3+61(04),=C'NET '                                               
         MVC   P3+65(15),=C'PREMIUM CHARGES'                                    
         B     PRTTOTX                                                          
*                                                                               
PRTTOTX  XMOD1 1                                                                
         LTORG                                                                  
*                                                                               
***********************************************************************         
*                                                                               
         EJECT                                                                  
SORTSCH  CSECT                                                                  
         NMOD1 0,SORTSCH                                                        
*                                                                               
         LA    R7,SORTSCH+4095                                                  
         LA    R7,1(R7)                                                         
         USING SORTSCH+4096,R7                                                  
*                                                                               
         L     RC,PPFILEC                                                       
         LR    R3,RC                                                            
         AHI   R3,4096                                                          
         USING PPFILED,RC,R3             R3 NEEDED FOR PUBREC ADDR              
*                                                                               
         ZAP   OAGYCNT,=P'0'       INITIALIZE OTHER AGY COUNTER                 
         XC    LASTOAGY,LASTOAGY                                                
*                                                                               
         CLC   QPROG(2),=C'AU'     SEE IF ADVERTISER                            
         BNE   SRCLT1X                                                          
*                                                                               
         LA    R4,PCLTREC+33                                                    
SRCLT0   CLI   0(R4),X'15'                                                      
         BE    SRCLT0C                                                          
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   SRCLT0                                                           
         DC    H'0'                    INVALID CLIENT                           
*                                                                               
SRCLT0C  MVC   SADVDATA,2(R4)          SAVE ADV DATA                            
*                                                                               
*                                                                               
*        GOTO GETADVC TO GET LIST OF AGYS/CLTS                                  
*                                                                               
*        MUST SWITCH TO THE CONTROL SYSTEM                                      
*                                                                               
         USING PCLTADVE,R4                                                      
*                                                                               
SRCLT1   L     RF,UTL                                                           
         MVI   4(RF),X'0A'                                                      
         XC    WORK(20),WORK                                                    
         MVI   WORK,C'P'                                                        
         MVC   WORK+1(1),PCLTKMED                                               
         MVC   WORK+2(2),PCLTAOR                                                
         MVC   WORK+4(3),PCLTADV                                                
*                                                                               
         CLC   QESTEND,SPACES     SEE IF AGENCY FILTER GIVEN                    
         BE    SRCLT1A0                                                         
         MVI   WORK+7,C'*'                                                      
         MVC   WORK+8(3),QESTEND                                                
         CLI   QESTEND+2,C' '     WILL BE NON BLANK IF FILTER                   
         BNE   SRCLT1A0                                                         
         MVC   WORK+7(3),QESTEND   ONE AGENCY                                   
*                                                                               
*                                                                               
         DROP  R4                                                               
*                                                                               
SRCLT1A0 DS    0H                                                               
         GOTO1 AGETADVC,DMCB,WORK,AADVCTAB,DATAMGR                              
*                                                                               
*                                                                               
         MVC   ANEXTAC,AADVCTAB     ADDRESS OF NEXT AGY/CLT                     
*                                                                               
         XC    OPENSES,OPENSES      LIST OF OPENED FILES                        
         L     RF,UTL                                                           
         MVC   OPENSES(1),4(RF)     SINCE MY FILE IS ALREADY OPENED             
         B     SRCLT1B                                                          
*                                                                               
SRCLT1A  DS    0H                   GET NEXT AGY/CLT                            
         L     R6,ANEXTAC                                                       
         LA    R6,GETVLEN(R6)                                                   
         ST    R6,ANEXTAC                                                       
         B     SRCLT1C                                                          
*                                                                               
SRCLT1B  DS    0H                    PROCESS NEXT/FIRST AGY/CLT                 
         L     R6,ANEXTAC                                                       
SRCLT1C  CLC   0(2,R6),=X'FFFF'      END                                        
         BNE   SRCLT1C5              DONE                                       
         BAS   RE,SRENDAGY           FINISH LAST AGY                            
         B     SCONL                                                            
*                                                                               
         USING GETADVCD,R6                                                      
*                                                                               
SRCLT1C5 L     RF,UTL                                                           
         MVC   4(1,RF),GETVSE       SWITCH TO AGENCY FILE                       
         LA    R2,OPENSES                                                       
SRCLT1D  CLC   0(1,R2),GETVSE                                                   
         BE    SRCLT1G                                                          
         CLI   0(R2),0                                                          
         BNE   SRCLT1E                                                          
         MVC   0(1,R2),GETVSE      SET OPENED                                   
*                                                                               
*        OPEN THE PRINTFILE                                                     
*                                                                               
         L     RF,UTL                                                           
         MVC   4(1,RF),GETVSE                                                   
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'PRINT',SRTFILES,PPBYOWRK                
         B     SRCLT1G                                                          
*                                                                               
SRCLT1E  LA    R2,1(R2)                                                         
         B     SRCLT1D                                                          
*                                                                               
SRCLT1G  L     RF,UTL                                                           
         MVC   4(1,RF),GETVSE       SWITCHES ME TO AGENCY FILE                  
         CLC   WORKCLT(2),GETVAGY                                               
         BE    SRCLT1K                                                          
         CLI   WORKCLT,0            SEE IF FIRST TIME                           
         BE    SRCLT1K                                                          
*                                                                               
         BAS   RE,SRENDAGY                                                      
*                                                                               
SRCLT1K  MVC   WORKCLT(2),GETVAGY                                               
         MVC   WORKCLT+2(1),QMEDIA                                              
         MVC   WORKCLT+4(3),GETVACLT                                            
*                                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         MVC   WORKPUB,PCONKPUB                                                 
*                                                                               
         TM    GETVCNTL,X'01'          SEE IF PUB LINK                          
         BZ    SR4                                                              
*          FIND PUB LINK                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,X'FD'                                                        
         MVC   KEY+1(1),QMEDIA                                                  
         MVC   KEY+2(3),QCLIENT                                                 
         MVC   KEY+5(2),QAGENCY                                                 
         MVC   KEY+7(2),WORKCLT         AGENCY                                  
         MVC   KEY+9(6),PCONKPUB                                                
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 HIGHPUB                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BNE   SRCLT1A               IF NOT FOUND - SKIP TO NEXT AGY            
         MVC   WORKPUB,KEY+15                                                   
         B     SR4                                                              
*                                                                               
*        THIS CODE IS FOR 19                                                    
*                                                                               
SRCLT1X  DS    0H                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         MVC   WORKPUB,PCONKPUB                                                 
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVC   WORKCLT,PCLTKEY                                                  
         CLI   PCLTPROF+5,C'1'     TEST THIS A MASTER CLIENT                    
         BNE   SR4                                                              
* READ THRU CLTHDRS FOR SLAVE CLIENTS                                           
         XC    WORKCLT+4(4),WORKCLT+4        FOR FIRST TIME                     
*                                                                               
SRCLT2   XC    KEY,KEY                                                          
         MVC   KEY(7),WORKCLT                                                   
         MVI   KEY+7,X'FF'                                                      
         GOTO1 HIGH                                                             
         B     SRCLT4A                                                          
SRCLT4   GOTO1 SEQ                                                              
SRCLT4A  CLC   KEY(4),KEYSAVE                                                   
         BE    SRCLT6                                                           
         CLI   WORKCLT+4,X'FF'     SEE IF I'VE SEARCHED FOR                     
         BE    SCONL               OTHER AGY DATA UNDER THE MASTER              
*                                  IF YES THEN I'M DONE                         
         XC    KEY,KEY             ELSE DO MASTER                               
         MVC   KEY(7),PCLTKEY                                                   
         MVI   WORKCLT+4,X'FF'                                                  
         L     R1,ASORTTAB                                                      
         ST    R1,ANEXTSRT                                                      
         XC    SORTCNT,SORTCNT                                                  
         B     SR4A                                                             
*                                                                               
SRCLT6   LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
*                                                                               
SRCLT8   BAS   RE,SRGET                                                         
*                                                                               
         CLC   PCLTKCLT,PCLTPROF+6-PCLTKEY+PBUYREC                              
         BNE   SRCLT4                                                           
*                                                                               
         MVC   WORKCLT,PBUYREC     SAVE THIS CLIENT KEY                         
*                                                                               
*                                                                               
SR4      L     R1,ASORTTAB                                                      
         ST    R1,ANEXTSRT                                                      
         XC    SORTCNT,SORTCNT                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(7),WORKCLT      A/M/X/CLT                                    
SR4A     MVI   KEY+3,X'21'                                                      
         MVC   KEY+7(6),WORKPUB                                                 
*                                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
*                                  NOTE - IF DOING ALL ZONES,EDTS               
*                                  THEN PCONPRD IS FROM FIRST CON READ          
         CLI   PCONPRD,C'A'        SEE IF DOING A PRD-CONTRACT                  
         BL    *+10                                                             
         MVC   KEY+13(3),PCONPRD                                                
*                                                                               
         DROP  RF                                                               
*                                                                               
         JIF   QPRODUCT,EQ,=C'ALL',OR,QPRODUCT,EQ,=C'   ',SR4B,JUMP=N           
         MVC   KEY+13(3),QPRODUCT                                               
*                                                                               
SR4B     DS    0H                                                               
**NEW 2/17/88                                                                   
         OC    KEY+13(3),KEY+13      SEE IF DOING ONE PRD                       
         BZ    SR4C                                                             
         MVC   KEY+16(3),BQSTART     SET DATE IN KEY                            
**NEW 2/17/88                                                                   
SR4C     GOTO1 HIGH                                                             
         B     SR6A                                                             
SR6      MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
SR6A     DS    0H                                                               
         CLC   KEY(13),KEYSAVE     SAME A/M/X/C/PUB                             
         BNE   SRENDPUB                                                         
*                                                                               
SR7      CLC   KEY+16(3),BQSTART                                                
**NEW 2/17/88                                                                   
         BL    SR7LOW                                                           
**NEW 2/17/88                                                                   
         CLC   KEY+16(3),BQEND                                                  
**NEW 2/17/88                                                                   
         BH    SR7HIGH                                                          
**NEW 2/17/88                                                                   
*                                                                               
         B     SR7H                 SKIP                                        
**NEW 2/17/88                                                                   
**NEW 2/17/88       LOGIC TO CHK PRD OMITTED SINCE IF ONE PRODUCT               
**NEW 2/17/88       IT IS SET IN KEY IN SR4A                                    
**NEW 2/17/88       ALSO CHG OF PRODUCT IS CHECKED IN SR6A                      
**NEW 2/17/88                                                                   
*                                                                               
SR7LOW   MVC   KEY+16(3),BQSTART                                                
         XC    KEY+19(6),KEY+19       MUST CLEAR EST                            
         B     SR4C                                                             
*                                                                               
SR7HIGH  MVC   KEY+16(3),=3X'FF'     TO GET NEXT PRD                            
         XC    KEY+19(6),KEY+19       CLEAR EST                                 
         B     SR4C                                                             
**NEW 2/17/88                                                                   
*                                                                               
SR7H     DS    0H                                                               
**NEW 2/17/88                                                                   
**NEW 2/17/88          DON'T READ EST FOR TEST STATUS ANYMORE                   
**NEW 2/17/88                                                                   
*                                                                               
SR7M     LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,SRGET                                                         
***TESTBUY***                                                                   
         CLI   QOPT6,C'Y'          SEE IF INCLUDING TEST BUYS                   
*NOP*    BE    SR7M1                                                            
         BE    SR7M0                                                            
*                                                                               
         CLI   PBDBFD,C'T'         SKIP TEST BUYS FOR ANALYSIS                  
         BE    SR6                                                              
***TESTBUY***                      NEED TO SET FOR OTHER AGY DATA               
*                                                                               
SR7M0    CLI   QOPT9,C'H'          SEE IF EXCLUDING HELD SFH BUYS               
         BNE   SR7M1               NO                                           
*                                                                               
         TM    PBDSTAT,X'08'       HELD SFH BUY ?                               
         BO    SR6                 YES - SKIP HELD BUYS FOR ANALYSIS            
*                                                                               
SR7M1    DS    0H                                                               
         L     R1,ANEXTSRT                                                      
         MVC   0(3,R1),KEY+16        DATE HIGH                                  
         CLC   QPROG(2),=C'AU'                                                  
         BNE   *+10                                                             
         MVC   3(3,R1),PBDBUYDT      CREATED DATE                               
         MVC   6(3,R1),KEY+13        THEN PRODUCT                               
         MVC   9(2,R1),KEY+19        THEN EST                                   
         MVC   11(4,R1),KEY+27       DISK ADDR                                  
         LA    R1,15(R1)                                                        
         MVI   0(R1),X'FF'                                                      
         ST    R1,ANEXTSRT                                                      
         L     R1,SORTCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,SORTCNT                                                       
         C     R1,MAXSORT                                                       
         BNH   *+6                                                              
         DC    H'0'                 TOO MANY BUYS                               
         B     SR6                                                              
*                                                                               
SRENDPUB DS    0H                                                               
*                                  XSORT BUYS IN SORTTAB                        
         L     R2,SORTCNT                                                       
         LTR   R2,R2               SEE IF ANY TO SORT                           
         BZ    SRENDPBX                                                         
         L     R4,ASORTTAB                                                      
         ST    R4,ANEXTSRT                                                      
         GOTO1 XSORT,DMCB,(0,(R4)),(R2),15,11,0                                 
SRENDPB2 CLI   0(R4),X'FF'         END OF BUYS                                  
         BE    SRENDPBX                                                         
         MVC   KEY+27(4),11(R4)                                                 
         LA    R4,15(R4)           SET R4 TO NEXT BUY IN TABLE                  
         ST    R4,ANEXTSRT                                                      
         LA    R0,PBUYREC                                                       
         ST    R0,IOAREA                                                        
         BAS   RE,SRGET            READ BUYREC                                  
SRENDPB4 DS    0H                                                               
         GOTO1 GETINS,DMCB,PBUYREC,GROSS,(C'Y',PBUYKPRD)                        
         B     SBUY                                                             
         EJECT                                                                  
SBUY     DS    0H                  PROCESS BUY                                  
*                                                                               
*                                                                               
*                                                                               
         CLI   QOPT6,C'Y'          SEE IF EXCLUDING TEST BUYS                   
         BNE   SBUY1                                                            
*                                                                               
         L     R0,PREMIUM                                                       
         CVD   R0,DUB                                                           
         AP    CLTPREM,DUB                                                      
*                                                                               
         ZAP   CLTNETP,CLTPREM                                                  
         LA    R2,PBUYREC+33                                                    
         CLI   0(R2),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                ELEMENT MUST EXIST                           
*                                                                               
         USING PBDELEM,R2                                                       
         ZAP   DUB,CLTNETP                                                      
         MP    DUB,PBDACP                                                       
         DP    DUB,=P'100000'                                                   
         SP    CLTNETP,DUB(4)                                                   
         DROP  R2                                                               
*                                                                               
*                                                                               
*                                                                               
SBUY1    MVC   ESTACT(5),=5C'Y'                                                 
*                                  SBUY4 TO SBUY70 FOR 19                       
*                                  CHK FOR RATE CHG ELEM FOR TODAY              
*                                  MEANS THIS SHOULD BE A RE-RUN                
SBUY4    DS    0H                                                               
         XC    MYCU,MYCU                                                        
*                                                                               
         CLC   QPROG,=C'AU'      SEE IF AOR CONTRACT ULTIIZATION                
         BNE   SBUY4A                                                           
         L     RF,UTL                                                           
         MVC   4(1,RF),SAVSYS    MUST RETURN TO AOR                             
*                                TO READ PCURECS                                
SBUY4A   DS    0H                                                               
         L     RF,ACONIO1          A(PCONREC)                                   
         USING PCONREC,RF                                                       
*                                                                               
         GOTO1 APPGETCU,DMCB,PBUYREC,PCONREC,DATAMGR                            
         CLI   DMCB,X'FF'                                                       
         BE    *+10                                                             
         MVC   MYCU,DMCB+5      LAST 3 BYTES                                    
*                                                                               
         DROP  RF                                                               
*                                                                               
         CLC   QPROG,=C'AU'       AOR CONTRACT ULTILIZATION RPT                 
         BNE   SBUY4B                                                           
*                                                                               
         L     RE,ANEXTAC                                                       
G1       USING GETADVCD,RE                                                      
         L     RF,UTL                                                           
         MVC   4(1,RF),G1.GETVSE      MUST RETURN TO AGENCY FILE                
*                                                                               
SBUY4B   DS    0H                                                               
*                                                                               
*                                                                               
SBUY4D   DS    0H                                                               
*                                                                               
*****    CLI   QOPT1-1,C'Y'         SEE IF NOT PRINTING DETAILS                 
         CLI   QOPT8,C'Y'           SEE IF NOT PRINTING DETAILS                 
         BE    SBUY85B              SKIP TO TOTALING                            
*                                                                               
         LA    R4,P1+2                                                          
         USING BUYLND,R4                                                        
         CLC   QPROG(2),=C'AU'     SEE IF ADVERTISER REPORT                     
         BNE   SBUY4D5                                                          
         MVC   ABAGY,PBUYKAGY                                                   
         MVC   ABCLT,PBUYKCLT                                                   
         MVC   ABPRD,PBUYKPRD                                                   
*                                   DISPLAY CREATION DATE                       
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBDBUYDT),(5,ABNDATE)                             
*                                                                               
         B     SBUY4D6                                                          
*                                                                               
SBUY4D5  CLI   PCLTPROF+5,C'1'     SEE IF MASTER CLT                            
         BNE   *+10                                                             
         MVC   BCLT,PBUYKCLT                                                    
         MVC   BPRD,PBUYKPRD                                                    
SBUY4D6  DS    0H                                                               
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BEST,DUB                                                         
         LA    R5,PPBYOWRK                                                      
         USING PPBYOUTD,R5                                                      
         XC    PBYOINPT(24),PBYOINPT                                            
         LA    R0,PBUYREC                                                       
         L     R1,DATCON                                                        
         LA    R2,GROSS                                                         
         STM   R0,R2,0(R5)                                                      
         MVI   PBYOCTL,X'20'                                                    
         GOTO1 PPBYOUT,DMCB,PPBYOWRK                                            
*                                                                               
         LA    R1,ABDATE-1                                                      
         CLC   QPROG(2),=C'AU'                                                  
         BE    *+8                                                              
         LA    R1,BDATE-1                                                       
         MVC   1(L'BDATE,R1),PBYOMDY                                            
***HELDBUY***                                                                   
         TM    PBDSTAT,X'08'                                                    
         BNO   *+8                                                              
         MVI   0(R1),C'H'                                                       
***HELDBUY***                                                                   
***TESTBUY***               (OVERRIDES 'H')                                     
         CLI   PBDBFD,C'T'                                                      
         BNE   SBUY4D8                                                          
         MVI   0(R1),C'T'                                                       
         TM    PBDSTAT2,X'40'      STEWARD BUY ?                                
         BNO   SBUY4D8             NO                                           
         MVI   0(R1),C'S'                                                       
***TESTBUY***                                                                   
SBUY4D8  DS    0H                                                               
         CLI   PBYOMDY2,C' '       CHECK FOR SECOND DATE                        
         BE    *+14                                                             
         MVI   133(R1),C'+'                                                     
         MVC   134(8,R1),PBYOMDY2                                               
*                                                                               
         MVC   BORATE,PBYOUR                                                    
         CLI   QMEDIA,C'N'                                                      
         BNE   SBUY30                                                           
         CLC   PBYOPRM,SPACES                                                   
         BNH   SBUY4G                                                           
         LA    R1,BORATE                                                        
         CLC   BORATE,SPACES       SEE IF P USED                                
         BNH   *+8                                                              
         LA    R1,BORATE+132       YES - USE P2                                 
         MVC   0(11,R1),PBYOPRM                                                 
SBUY4G   DS    0H                                                               
         CLI   PBYOSPC,C' '                                                     
         BE    SBUY5                                                            
         MVC   BDESC(10),PBYOSPC                                                
         B     SBUY10                                                           
*                                                                               
SBUY5    DS    0H                                                               
         MVC   BDESC(L'PBYOUNTS),PBYOUNTS                                       
*                                                                               
SBUY10   MVC   BLINES,PBYOUNTS                                                  
         LA    R2,5                                                             
         LA    R1,BLINES                                                        
SBUY15   CLI   BLINES+6,C' '                                                    
         BNE   SBUY35                                                           
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R1),PBYOUNTS                                                 
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         BCT   R2,SBUY15                                                        
         B     SBUY35                                                           
*                                                                               
SBUY30   MVC   BDESC(20),PBYOSPC     NON-NEWSPAPERS                             
         MVC   BDESC+132(20),PBYOSPC2                                           
*                                                                               
SBUY35   EQU   *                                                                
         LA    R2,DOLTYPS                                                       
         LA    R6,BOGROSS                                                       
SBUY38   CLI   0(R2),X'FF'                                                      
         BE    SBUY70                                                           
         CLI   0(R2),C'X'                                                       
         BE    SBUY65                                                           
         CLI   0(R2),C'G'                                                       
         BNE   SBUY40                                                           
         EDIT  (B4,GROSS),(14,0(R6)),2,COMMAS=YES,FLOAT=-                       
         B     SBUY65                                                           
SBUY40   CLI   0(R2),C'C'                                                       
         BNE   SBUY45                                                           
         EDIT  (B4,CSHDSC),(14,0(R6)),2,COMMAS=YES,FLOAT=-                      
         B     SBUY65                                                           
SBUY45   CLI   0(R2),C'2'       NET-CD                                          
         BNE   SBUY50                                                           
         EDIT  (B4,PYABLE),(14,0(R6)),2,COMMAS=YES,FLOAT=-                      
         B     SBUY65                                                           
SBUY50   CLI   0(R2),C'N'        NET                                            
         BNE   SBUY55                                                           
         L     R0,PYABLE                                                        
         A     R0,CSHDSC                                                        
         ST    R0,FULL2                                                         
         EDIT  (B4,FULL2),(14,0(R6)),2,COMMAS=YES,FLOAT=-                       
         B     SBUY65                                                           
SBUY55   CLI   0(R2),C'1'        GROSS-CD                                       
         BNE   SBUY65                                                           
         L     R0,GROSS                                                         
         S     R0,CSHDSC                                                        
         ST    R0,FULL2                                                         
         EDIT  (B4,FULL2),(14,0(R6)),2,COMMAS=YES,FLOAT=-                       
         B     SBUY65                                                           
*                                                                               
SBUY65   LA    R2,1(R2)                                                         
         LA    R6,15(R6)                                                        
         B     SBUY38                                                           
*                                                                               
SBUY70   EQU   *                   FIND AND PRINT COMMENTS                      
         CLI   BDATE+132,C' '      SEE IF P2 USED                               
         BE    SBUY73                                                           
         GOTO1 ACLPRT                                                           
         LA    R6,BDATE                                                         
         B     SBUY75                                                           
SBUY73   LA    R6,BDATE+132        USE P2 FOR FIRST COMMENT                     
SBUY75   LA    R2,PBUYREC+33                                                    
         MVI   ELCOD,X'66'                                                      
SBUY76   BAS   RE,SRNEXTEL                                                      
         BNE   SBUY80                                                           
         ZIC   R5,1(R2)            ELEM LENGHT                                  
         SH    R5,=H'3'            ADJUST FOR CODE + LENGHT                     
         BM    SBUY76              AND EXEC                                     
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),2(R2)       EXECUTED                                     
         GOTO1 ACLPRT                                                           
         LA    R6,BDATE            RESET TO P1                                  
         B     SBUY76                                                           
*                                                                               
SBUY80   CLC   P1,SPACES           SEE IF ANYTHING LEFT TO PRINT                
         BE    SBUY85              NO                                           
         GOTO1 ACLPRT                                                           
SBUY85   DS    0H                                                               
         OC    MYCU,MYCU           SEE IF CU= OVERRIDEN                         
         BZ    SBUY85B                                                          
         CLC   MYCU,=X'000001'      MEANS ZERO                                  
         BNE   SBUY85A0                                                         
         MVC   BDATE(6),=C'(CU=0)'                                              
         B     SBUY85A8                                                         
*                                                                               
SBUY85A0 MVC   BDATE(4),=C'(CU='                                                
         EDIT  (B3,MYCU),(12,W),4,COMMAS=YES,ALIGN=LEFT,DROP=4                  
         LA    R1,11                                                            
         LA    RF,W+11                                                          
SBUY85A  CLI   0(RF),C'.'                                                       
         BE    SBUY85A5                                                         
         CLI   0(RF),C' '                                                       
         BNE   SBUY85A7                                                         
SBUY85A5 SH    RF,=H'1'                                                         
         BCT   R1,SBUY85A                                                       
*                                                                               
SBUY85A7 DS    0H                                                               
         MVI   1(RF),C')'                                                       
         LA    R1,1(R1)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BDATE+4(0),W                                                     
SBUY85A8 GOTO1 ACLPRT                                                           
**                                                                              
***TESTBUY***                                                                   
SBUY85B  CLI   PROGPROF+3,C'N' SEE IF INCLUDING TEST BUYS IN TOTALS             
         BNE   SBUY85C          FOR 19T REQS                                    
         CLI   PBDBFD,C'T'         SEE IF TEST BUY                              
         BE    SRENDPNX            DON'T POST TO TOTALS                         
SBUY85C  DS    0H                                                               
***TESTBUY***                                                                   
*                                                                               
         CLI   QOPT9,C'H'          SEE IF EXCLUDING HELD SFH BUYS               
         BNE   SBUY85D             NO                                           
*                                                                               
         TM    PBDSTAT,X'08'       HELD SFH BUY ?                               
         BO    SRENDPNX            YES - DON'T POST TO TOTALS                   
*                                                                               
SBUY85D  DS    0H                                                               
         L     R0,GROSS                                                         
         CVD   R0,DUB                                                           
         AP    CLTGRS,DUB                                                       
         L     R0,CSHDSC                                                        
         CVD   R0,DUB                                                           
         AP    CLTCD,DUB                                                        
         L     R0,PYABLE           NET                                          
         CVD   R0,DUB                                                           
         AP    CLTNET,DUB                                                       
         OC    MYCU,MYCU           CHK FOR CONTRACT UNITS OVERRIDE              
         BZ    SBUY85M                                                          
         CLC   MYCU,=X'000001'      SPECIAL FOR ZERO                            
         BE    SBUY86               BYPASS CONTRACT UNITS COUNTER               
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),MYCU                                                   
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         AP    CLTINS,DUB                                                       
         B     SBUY86                                                           
*                                                                               
SBUY85M  CLI   PBDSPACE,C'*'       SEE IF REAL INS                              
         BE    SBUY86                                                           
         CLI   PBUYKPRD,C'*'                                                    
         BNE   SBUY85X                                                          
*                                                                               
         LA    R2,PBUYREC+33       FIND NUMBER OF INS IN COMMENT                
         MVI   ELCOD,X'66'                                                      
SPB85B   BAS   RE,SRNEXTEL                                                      
         BNE   SPB85R              NOT FOUND                                    
         LA    R4,13               11+2                                         
         CLC   2(11,R2),=C'INSERTIONS='                                         
         BE    SPB85F                                                           
         LA    R4,6                4+2                                          
         CLC   2(4,R2),=C'INS='    ALSO ACCEPT INS=                             
         BE    SPB85F                                                           
         LA    R4,10               8+2                                          
         CLC   2(8,R2),=C'INSERTS='   OR INSERTS=                               
         BE    SPB85F                                                           
         LA    R4,11               9+2                                          
         CLC   2(9,R2),=C'POSTINGS='    OR POSTINGS=                            
         BE    SPB85F                                                           
         B     SPB85B                                                           
*                                                                               
SPB85F   ZIC   R1,1(R2)            CALC NUMBER OF DIGITS                        
         SR    R1,R4                                                            
         BNP   SPB85B              MUST HAVE AT LEAST ONE                       
         AR    R2,R4               POINT TO FIRST DIGIT                         
         SR    R5,R5               USED TO COUNT DIGITS                         
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
SPB85G   CLI   0(R2),C'0'                                                       
         BL    SPB85H              STOP ON NON-DIGIT                            
         CLI   0(R2),C'9'                                                       
         BH    SPB85H                                                           
         LA    R5,1(R5)                                                         
         MVC   0(1,R6),0(R2)                                                    
         LA    R6,1(R6)                                                         
         LA    R2,1(R2)                                                         
         BCT   R1,SPB85G                                                        
*                                                                               
SPB85H   LTR   R5,R5               SEE IF I HAVE ANY DIGITS                     
         BZ    SPB85B              NO TRY FOR ANOTHER ELEM                      
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         MP    DUB,=P'10000'                                                    
         AP    CLTINS,DUB                                                       
         B     SBUY86                                                           
*                                                                               
SPB85R   EQU   *                                                                
SBUY85X  EQU   *                                                                
         AP    CLTINS,=P'10000'       NOW 4 DECIMALS                            
*                                                                               
SBUY86   EQU   *                                                                
         ZAP   DUB,PBDUNITS                                                     
         CLI   PBDUIND,C'I'                                                     
         BE    SBUY86B                                                          
         CLI   PBDUIND,X'89'       LOWER CASE I                                 
         BNE   SBUY86F             HAS UNITS TO 2 DECIMALS                      
SBUY86B  DS    0H                                                               
         CLI   PBDUIND,X'89'       LOWER CASE I                                 
         BE    SBUY86D             HAS UNITS TO 2 DECIMALS                      
         MP    DUB,=P'100'         TO GET 2 DECIMALS                            
SBUY86D  AP    CLTINCH,DUB                                                      
*                                                                               
         CLI   PROGPROF+1,C'N'     SEE IF EQUIVALENCING TO LINES                
         BE    SBUY86X             NO                                           
         MP    DUB,=P'14'                                                       
         AP    DUB,=P'50'                                                       
         DP    DUB,=P'100'        CONVERT TO LINES                              
         AP    CLTLINES,DUB(6)                                                  
         B     SBUY86X                                                          
*                                                                               
SBUY86F  DS    0H                  MUST BE LINES                                
         AP    CLTLINES,PBDUNITS                                                
         CLI   PROGPROF+1,C'N'      SEE IF EQUIVALENCING TO INCHES              
         BE    SBUY86X                                                          
         ZAP   DUB,PBDUNITS                                                     
         MP    DUB,=P'100'                                                      
         DP    DUB,=P'14'                                                       
         AP    CLTINCH,DUB(6)                                                   
SBUY86X  B     SRENDPNX      GO DO NEXT BUY                                     
*                                                                               
SRENDPNX L     R4,ANEXTSRT                                                      
         B     SRENDPB2            GO DO NEXT BUY                               
*                                                                               
         EJECT                                                                  
*                                                                               
SRENDPBX DS    0H                                                               
*                                                                               
SRENDCLT DS    0H                  CLIENT END                                   
         CLI   CLTACT,C'Y'                                                      
         BNE   SRENDC5                                                          
         CLC   QPROG(2),=C'AU'     SEE IF ADVERTISER                            
         BE    SRENDC2                                                          
*                                                                               
         CLI   PCLTPROF+5,C'1'     SEE IF DOING MASTER CLIENT                   
         BNE   SRENDC3             NO - NO CLT TOTALS                           
SRENDC2  MVC   P1+30(19),=C'** CLIENT TOTALS **'                                
*****    CLI   QOPT1-1,C'Y'        SEE IF SUPPRESSING DETAILS                   
         CLI   QOPT8,C'Y'          SEE IF SUPPRESSING DETAILS                   
         BNE   *+16                                                             
         MVC   P1+25(7),=C'**     '                                             
         MVC   P1+28(3),PBUYKCLT                                                
*                                                                               
         LA    R5,CLTTOTS                                                       
         MVI   STARS,1                                                          
         GOTO1 APRTTOTS                                                         
         MVI   SPACING,2                                                        
         GOTO1 ACLPRT                                                           
SRENDC3  LA    R4,CONTOTS          ROLL TO CONTRACT TOTALS                      
         CLC   QPROG(2),=C'AU'     SEE IF ADVERTISER                            
         BNE   *+8                                                              
         LA    R4,AGYTOTS          ROLL TO AGYTOTS                              
         LA    R5,CLTTOTS                                                       
         BAS   RE,SADDCLR                                                       
SRENDC5  MVC   ESTACT(3),=3C'N'                                                 
*                                                                               
SRENDCX  DS    0H                                                               
         CLC   QPROG(2),=C'AU'          SEE IF ADVERTISER                       
         BE    SRCLT1A                 GO DO NEXT AGY/CLT                       
*                                                                               
         CLI   PCLTPROF+5,C'1'         TEST MASTER CLIENT                       
         BNE   SCONL                                                            
         B     SRCLT2              READ NEXT CLIENT                             
*                                                                               
         SPACE 2                                                                
SCONL    DS    0H                  END OF CONTRACT                              
*                                                                               
         CLC   QPROG(2),=C'AU'     SEE IF ADV                                   
         BNE   SCONL4                                                           
         L     RF,UTL                                                           
         MVC   4(1,RF),SAVSYS     RETURN TO MY SYSTEM                           
*                                                                               
SCONL4   CLI   TESTPASS,1                                                       
         BE    SCONL               DO NOTHING FOR TEST PASS                     
*                                                                               
         CLI   CONACT,C'Y'                                                      
         BNE   SCONLX                                                           
         MVC   P1+30(21),=C'** CONTRACT TOTALS **'                              
         LA    R5,CONTOTS                                                       
         MVI   STARS,2                                                          
         GOTO1 APRTTOTS                                                         
         GOTO1 ACLPRT                                                           
         MVC   SCONTOTS(ACCNUM*8),CONTOTS    SAVE CONTRACT TOTALS FOR           
*                                      19 ANALYSIS MESSAGE                      
         LA    R4,REQTOTS                                                       
         BAS   RE,SADDCLR                                                       
         B     SCONLX                                                           
SCONLX   XMOD1 1                                                                
*                                                                               
SRENDAGY NTR1                                                                   
* AGENCY TOTALS                                                                 
         CLI   AGYACT,C'Y'                                                      
         BNE   SRENDA2                                                          
*                                                                               
SRENDA1  MVC   P1+30(19),=C'** AGENCY TOTALS **'                                
*****    CLI   QOPT1-1,C'Y'        SEE IF SUPPRESSING DETAILS                   
         CLI   QOPT8,C'Y'          SEE IF SUPPRESSING DETAILS                   
         BNE   *+16                                                             
         MVC   P1+26(6),=C'**     '                                             
         MVC   P1+29(2),WORKCLT       AGENCY                                    
         LA    R5,AGYTOTS                                                       
         MVI   STARS,1                                                          
         GOTO1 APRTTOTS                                                         
         MVI   SPACING,2                                                        
         GOTO1 ACLPRT                                                           
*                                                                               
SRENDA2  DS    0H                                                               
         LA    R4,CONTOTS          ROLL TO CONTOTS                              
         LA    R5,AGYTOTS                                                       
         BAS   RE,SADDCLR                                                       
*                                                                               
SRENDAX  DS    0H                                                               
         MVI   AGYACT,C'N'                                                      
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
SADDCLR  EQU   *                                                                
         LA    R6,ACCNUM                                                        
SADDCL5  AP    0(8,R4),0(8,R5)                                                  
         LA    R4,8(R4)                                                         
         ZAP   0(8,R5),=P'0'                                                    
         LA    R5,8(R5)                                                         
         BCT   R6,SADDCL5                                                       
         BR    RE                                                               
         SPACE 2                                                                
SRNEXTEL SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    SRNEXTLX            RETURN WITH CC =                             
         CLC   ELCOD,0(R2)                                                      
         BCR   8,RE                                                             
         B     SRNEXTEL+2                                                       
SRNEXTLX LTR   R2,R2               RETURN WITH CC NOT =                         
         BR    RE                                                               
*                                                                               
SRGET    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,GETREC,PRTFILE,KEY+27,IOAREA,DMWORK                 
*                                                                               
         B     SRDMX                                                            
*                                                                               
SRDMX    TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
SRTFILES DS    0D                                                               
         DC    CL8' PRTDIR'                                                     
         DC    CL8' PRTFILE'                                                    
         DC    CL8' PUBDIR'                                                     
         DC    CL8' PUBFILE'                                                    
         DC    C'X'                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
CLPRT    CSECT                                                                  
         NMOD1 0,CLPRT                                                          
         USING PPWORKD,RA                                                       
         L     R9,PPWORK2C                                                      
         USING PPWORK2D,R9                                                      
         LA    R8,SPACEND                                                       
         USING PP19WRKD,R8                                                      
         L     RC,PPFILEC                                                       
         LR    R3,RC                                                            
         AHI   R3,4096                                                          
         USING PPFILED,RC,R3                                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,LINE                                                          
         IC    RF,LNEED                                                         
         MVI   LNEED,0                                                          
         AR    RE,RF                                                            
         STC   RE,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BL    CLPRT4                                                           
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         CLI   PRTSW,C'C'      SEE IF DOING CONTRACT LINE                       
         BNE   CLPRT4                                                           
         MVC   MID1+32(90),=C'NUMBER     CONTRACT DATES     LEVEL      X        
               PERCENT      RATE    DESCRIPTION        EFF. DATE'               
         MVC   MID2+32(90),=C'------     --------------     -----      X        
               -------      ----    -----------        ---------'               
         CLI   MODE,LBUYREQ       MEANS I'M DOING ACROSS EDITION                
         BNE   *+16               SUMMARY                                       
         MVC   MID1+73(17),SPACES  BLANK OUT PERCENT AND RATE                   
         MVC   MID2+73(17),SPACES                                               
*                                                                               
         MVI   FORCEMID,C'Y'                                                    
*                                                                               
CLPRT4   DS    0H                                                               
         MVC   RCSUBPRG,HDSW                                                    
         CLI   HDSW,2                                                           
         BL    CLPRT8                                                           
         CLI   HDSW,3                                                           
         BH    CLPRT8                                                           
         CLI   MODE,LBUYREQ                                                     
         BNE   CLPRT5                                                           
         MVC   HEAD7(13),=C'REPORT TOTALS'                                      
         B     CLPRT6                                                           
*                                                                               
CLPRT5   CLI   PRTSW,C'P'               SEE IF PRINT PUB LINE                   
         BNE   CLPRT5C                                                          
         CLI   FORCEHED,C'Y'                                                    
         BNE   CLPRT6                                                           
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         B     CLPRTXX            PRINT NOTHING                                 
*                                                                               
CLPRT5C  DS    0H                                                               
         CLC   QESTEND,SPACES                                                   
         BE    CLPRT5D                                                          
         MVC   HEAD4(14),=C'AGENCY FILTER='                                     
         LA    RE,HEAD4+15                                                      
         LA    RF,QESTEND                                                       
         LA    R1,3     FOR BCT                                                 
CLPRT5C2 TM    0(RF),X'40'         CHECK FOR NEGATIVE FILTER                    
         BZ    CLPRT5C5                                                         
CLPRT5C3 MVC   0(1,RE),0(RF)                                                    
         OI    0(RE),X'40'        SET ON FOR PRINTING                           
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,CLPRT5C2                                                      
         B     CLPRT5C7                                                         
*                                                                               
CLPRT5C5 MVI   0(RE),C'-'                                                       
         LA    RE,1(RE)                                                         
         B     CLPRT5C3                                                         
*                                                                               
CLPRT5C7 DS    0H                                                               
CLPRT5D  LA    RE,HEAD5                    *** WAS STARTING  ***                
*NOP*    MVC   HEAD6(L'SVPPUBL),SVPPUBL    *** AT HEAD6 HERE ***                
*NOP*    MVC   HEAD7(L'SVPNAME),SVPNAME                                         
*NOP*    MVC   HEAD8(L'SVPZNAME),SVPZNAME                                       
*NOP*    MVC   HEAD9(8),=C'CONTRACT'                                            
*NOP*    MVC   HEAD9+9(3),SVPCON                                                
*                                                                               
         MVC   0(L'SVPPUBL,RE),SVPPUBL                                          
         LA    RE,132(RE)                                                       
         MVC   0(L'SVPNAME,RE),SVPNAME                                          
         LA    RE,132(RE)                                                       
         MVC   0(L'SVPZNAME,RE),SVPZNAME                                        
         LA    RE,132(RE)                                                       
         MVC   0(8,RE),=C'CONTRACT'                                             
         MVC   9(3,RE),SVPCON                                                   
*****    CLI   QOPT1-1,C'Y'        SEE IF SUPPRESSING DETAILS                   
         CLI   QOPT8,C'Y'          SEE IF SUPPRESSING DETAILS                   
         BE    CLPRT7                                                           
         CLC   QPROG(2),=C'AU'     SEE IF ADVERTISER                            
         BNE   CLPRT5F                                                          
         MVC   HEAD11+2(35),=C'AGY  CLT  PRD    DATE       CREATED'             
         MVC   HEAD12+2(35),=C'---  ---  ---  --------     -------'             
         B     CLPRT8B                                                          
*                                                                               
CLPRT5F  CLI   PCLTPROF+5,C'1'     SEE IF MASTER CLT                            
         BNE   CLPRT6                                                           
CLPRT5H  MVC   HEAD11+17(16),=C'CLT  PRD    DATE'                               
         MVC   HEAD12+17(18),=C'---  ---  --------'                             
         B     CLPRT8B                                                          
*                                                                               
CLPRT6   DS    0H                                                               
         MVC   HEAD11+22(11),=C'PRD    DATE'                                    
         MVC   HEAD12+22(13),=C'---  --------'                                  
         B     CLPRT8B                                                          
*                                                                               
CLPRT7   DS    0H                                                               
         MVC   HEAD11+28(3),=C'PRD'                                             
         MVC   HEAD12+28(3),=C'---'                                             
         B     CLPRT8B                                                          
*                                                                               
CLPRT8   DS    0H                                                               
CLPRT8B  CLI   QOPT6,C'Y'             FOR PP18 ONLY                             
         BNE   CLPRT9                                                           
         MVI   HEAD4+115,C'T'                                                   
         MVC   HEAD6+97(34),=C'*INCLUDES ANY PROPOSED INSERTIONS*'              
         CLI   QMEDIA,C'O'                                                      
         BNE   CLPRT8D                                                          
         MVC   HEAD6+120(11),=C'POSTINGS* '                                     
*                                                                               
CLPRT8D  CLI   PROGPROF+3,C'N'    SEE IF TEST BUYS NOT IN TOTALS                
         BNE   CLPRT9                                                           
         MVC   HEAD7+97(24),=C'*NOT INCLUDED IN TOTALS*'                        
*                                                                               
CLPRT9   CLC   QPAY(3),SPACES                                                   
         BE    CLPRT12             NO CONTROL DATE                              
         LA    R4,HEAD4+68                                                      
         MVC   HEAD4+54(13),=C'CHANGED SINCE'                                   
*                                  SEE IF DOING CHANGES ONLY                    
         CLI   QOPT4,C'Y'                                                       
         BE    CLPRT10                                                          
         LA    R4,HEAD4+72                                                      
         MVC   HEAD4+52(19),=C'CHANGE CONTROL DATE'                             
CLPRT10  GOTO1 DATCON,DMCB,(3,QPAY),(5,0(R4))                                   
*                                                                               
CLPRT12  DS    0H                                                               
         LA    R2,DOLTYPS                                                       
         LA    R4,HEAD10+76                                                     
CLPRT14  CLI   0(R2),X'FF'                                                      
         BE    CLPRTX                                                           
         CLI   0(R2),C'X'                                                       
         BE    CLPRT50                                                          
         CLI   0(R2),C'G'        GROSS                                          
         BNE   CLPRT15                                                          
         MVC   132(14,R4),=C'         GROSS'                                    
         MVC   264(14,R4),=C'         -----'                                    
         B     CLPRT50                                                          
CLPRT15  CLI   0(R2),C'N'        GROSS                                          
         BNE   CLPRT20                                                          
         MVC   132(14,R4),=C'           NET'                                    
         MVC   264(14,R4),=C'           ---'                                    
         B     CLPRT50                                                          
CLPRT20  CLI   0(R2),C'C'        CASH DISC                                      
         BNE   CLPRT25                                                          
         MVC   0(14,R4),=C'        CASH  '                                      
         MVC   132(14,R4),=C'      DISCOUNT'                                    
         MVC   264(14,R4),=C'      --------'                                    
         B     CLPRT50                                                          
CLPRT25  CLI   0(R2),C'1'        GROSS-CD                                       
         BNE   CLPRT30                                                          
         MVC   0(14,R4),=C'        GROSS '                                      
         MVC   132(14,R4),=C'       LESS CD'                                    
         MVC   264(14,R4),=C'       -------'                                    
         B     CLPRT50                                                          
CLPRT30  CLI   0(R2),C'2'        NET-CD                                         
         BNE   CLPRT50                                                          
         MVC   0(14,R4),=C'         NET  '                                      
         MVC   132(14,R4),=C'       LESS CD'                                    
         MVC   264(14,R4),=C'       -------'                                    
         B     CLPRT50                                                          
*                                                                               
CLPRT50  LA    R4,15(R4)                                                        
         LA    R2,1(R2)                                                         
         B     CLPRT14                                                          
*                                                                               
CLPRTX   EQU   *                                                                
         CLI   RCSUBPRG,2                                                       
         BL    CLPRTX5                                                          
         CLI   RCSUBPRG,3                                                       
         BH    CLPRTX5                                                          
*****    CLI   QOPT1-1,C'Y'         NO DETAILS                                  
         CLI   QOPT8,C'Y'           NO DETAILS                                  
         BNE   CLPRTX5                                                          
         MVI   RCSUBPRG,0                                                       
*                                                                               
CLPRTX5  GOTO1 REPORT                                                           
*                                                                               
CLPRTXX  XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
PP19WRKD DSECT                                                                  
WKTODAY  DS    CL6                                                              
IOAREA   DS    A                                                                
APRDTABT DS    A                                                                
ARTLOOK  DS    A                                                                
APPGETCU DS    A                                                                
ADATVAL  DS    A                                                                
APGETADR DS    A                                                                
BUFFBUFF DS    A                                                                
BUFFIO   DS    A                                                                
ACLPRT   DS    A                                                                
ACOMPRT  DS    A                                                                
ACONSCHD DS    A                                                                
ASORTSCH DS    A                                                                
ASORTTAB DS    A                                                                
ANEXTSRT DS    A                                                                
APRTTOTS DS    A                                                                
AGETADVC DS    A                                                                
AADVCTAB DS    A                                                                
ANEXTAC  DS    A                                                                
ARTLKELS DS    A                                                                
*                                                                               
WID      DS    CL16                                                             
MAPFILE  DS    A                                                                
*                                                                               
ACONIO1  DS    A                   A(PCONREC) FROM ACONIO IN PPG                
*                                                                               
*                                                                               
LASTCKEY DS    CL8                                                              
WORKCLT  DS    CL8                                                              
WORKCNM  DS    CL20                                                             
WORKPUB  DS    CL6                                                              
SVBKEY   DS    CL32                                                             
ESAVKEY  DS    CL32                                                             
ELCOD    DS    C                                                                
SORTCNT  DS    F                                                                
MAXSORT  DS    F                                                                
SORTOPT  DS    CL1              PROGPROF+4 OR QOPT1-3                           
*                                                                               
PROFKEY  DS    CL12                                                             
BSTART   DS    XL3                                                              
BEND     DS    XL3                                                              
ESTACT   DS    CL1                                                              
PRDACT   DS    CL1                                                              
CLTACT   DS    CL1                                                              
AGYACT   DS    CL1                                                              
CONACT   DS    CL1                                                              
*                                                                               
E18SW    DS    CL1                                                              
*                                                                               
PRTSW    DS    CL1                                                              
*                                                                               
MYCU     DS    CL3                 MY CU VALUE                                  
CULOOKUP DS    X                   Y=CU VALUE IS LOOKED UP                      
*                                                                               
HDSW     DS    CL1                                                              
TESTPASS DS    CL1                 USED BY QOPT5 TO CHK FOR                     
*                                  A BUY FOR THIS CONTRACT                      
SAVSYS   DS    CL1                                                              
OPENSES  DS    XL20                OPENED FILES (ROOM FOR 20)                   
*                                                                               
SADVDATA DS    0CL18               DATA SAVED FROM ADV CLIENT HEADER            
SVAOR    DS    CL2                                                              
SVADV    DS    CL3                                                              
SVADVC   DS    CL3                                                              
SVADVST  DS    XL3                                                              
SVADVED  DS    XL3                                                              
SVAORSE  DS    XL1                                                              
SVAORC   DS    XL3                CONTROL BYTES                                 
*                                                                               
SCONVDTE DS    CL3                 COL CONV DATE                                
SCONVFAC DS    F                   COL. CONV FACTOR                             
SCONVIND DS    CL1                 C'-' MEANS AFTER TO BEFORE                   
*                                  C'*' MEANS BEFORE TO AFTER                   
TCONVDTE DS    CL3                 COL CONV DATE                                
TCONVFAC DS    F                   COL. CONV FACTOR                             
TCONVIND DS    CL1                 C'-' MEANS AFTER TO BEFORE                   
*                                  C'*' MEANS BEFORE TO AFTER                   
SPACESW  DS    CL1                 SET TO X'01' IF SPACE DESC FOUND             
TOTSW    DS    CL1                                                              
PNETSW   DS    CL1                                                              
OAGYCNT  DS    PL2                 COUNT OF OTHER AGYS                          
LASTOAGY DS    CL3                                                              
*                                  FOUND - PRD STARTS WITH *                    
WKOPEN   DS    CL1                                                              
WKDASHES DS    CL25                                                             
*                                                                               
*                                                                               
W        DS    CL132               WORK SPACE TO BUILD PRINT LINE               
*                                                                               
SAVEP    DS    CL132                                                            
SAVEP2   DS    CL132                                                            
SAVESPAC DS    X                                                                
SAVMID1  DS    CL132                                                            
SAVMID2  DS    CL132                                                            
*                                                                               
*                                                                               
SVPROG   DS    CL80                                                             
SVOPT7   DS    CL1                                                              
SVPPUBL  DS    CL50                "PUBLISHER" + NUMBER + NAME                  
SVPNAME  DS    CL70                PUB NAME AND NUMBER                          
SVPZNAME DS    CL40                PUB ZONE NAME                                
SVPCON   DS    CL3                 CON NUMBER FOR INS CHG PAGES                 
SVRCON   DS    CL3                 SAVED REQUESTED CONTRACT NUMBER              
*                                                                               
TOTCNT   DS    PL4                 CONTRACT COUNTS FOR P14 + P18                
LOCKCNT  DS    PL4                                                              
CHACNT   DS    PL4                                                              
TOTPCNT  DS    PL4                                                              
*                                                                               
LNEED    DS    X                                                                
PUBSW    DS    X                                                                
RCHGSW   DS    X                                                                
*                                                                               
DOLTYPS  DS    CL4               3 PLUS X'FF'                                   
FULL2    DS    F                                                                
MYDUB    DS    D                                                                
STARS    DS    CL1                                                              
*                                                                               
PACKED14 DS    PL14                                                             
WORKP14  DS    PL14                                                             
SVPK14   DS    PL14                                                             
*                                                                               
*                                FROM FIRST CONTRACT RATE ELEM                  
PLIND    DS    CL1               PRIMARY LEVEL IND                              
PLEVEL   DS    PL5               PRIMARY LEVEL                                  
*                                                                               
SVGROSS  DS    3F                  GROSS,AC,CD                                  
SVCOST   DS    PL5                                                              
SVPRCOST DS    PL5                                                              
SVCD     DS    PL2                                                              
SVAC     DS    PL3                                                              
*                                                                               
ACCNUM   EQU   8                                                                
*                                                                               
PROTOTS  DS    0D                                                               
PROGRS   DS    D                   GROSS                                        
PROCD    DS    D                   CASH DISC                                    
PRONET   DS    D                   NET (GROSS-CD-AC)                            
PROINS   DS    D                   INSERTIONS/CONTRACT UNITS                    
PROLINES DS    D                                                                
PROINCH  DS    D                                                                
PROPREM  DS    D                   PRODUCT PREMIUM                              
PRONETP  DS    D                   PRODUCT NET PREMIUM                          
*                                                                               
*                                                                               
CLTTOTS  DS    0D                                                               
CLTGRS   DS    D                                                                
CLTCD    DS    D                                                                
CLTNET   DS    D                                                                
CLTINS   DS    D                                                                
CLTLINES DS    D                                                                
CLTINCH  DS    D                                                                
CLTPREM  DS    D                   CLIENT PREMIUM                               
CLTNETP  DS    D                   CLIENT NET PREMIUM                           
*                                                                               
AGYTOTS  DS    0D               FOR AU REPORT                                   
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
*                                                                               
CONTOTS  DS    0D                                                               
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
*                                                                               
REQTOTS  DS    0D         FOR ACROSS EDITION TOTALS                             
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
*                                                                               
SCONTOTS DS    0D      SAVE CONTRACT TOTALS FOR 19 ANALYSIS MESSAGE             
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
         DS    D                                                                
SCONPREM DS    D                                                                
SCONNETP DS    D                                                                
*                                                                               
         DS    0D                                                               
BUFREC   DS    0CL120                                                           
BUFKEY   DS    0CL28                                                            
BUFTYP   DS    CL1                                                              
*                                  X'01' = NON-OAGY DETAILS                     
*                                          BUFOAGY = X'00'                      
*                                  X'03' = OTHER AGY                            
*                                          ALL OTHER AGYS                       
*                                          BUFOAGY = X'FF'                      
*                                                                               
*                                  X'05' = CONTRACT TOTALS                      
*                                                                               
*                                  X'10' = REPORT TOTALS                        
*                                          USED FOR PUB=XXX,ALL REQS            
*                                  FOR ABOVE BREAKS BUFDESC =                   
*                                  X'FFFFFF' FOR TOTAL LINE                     
*                                                                               
*                                  IF OTHER AGY DATA FOUND                      
*                                  CONTRACT TOTALS WILL HAVE                    
*                                  SPACE BREAKOUT                               
*                                                                               
BUFOAGY  DS    CL3                 USED FOR OTHER AGY CODE                      
*                                  I.E. PRDS STARTING WITH *                    
BUFDESC  DS    CL20                                                             
*              FOR NEWS SPACE(8) THEN RATE IN BUFDESC+9                         
         DS    CL4                 SPARE                                        
BUFCOM   DS    CL20                USED FOR OTHER AGY NAME                      
*                                                                               
BUFLINES DS    D                   LINES - NEWS                                 
BUFINCH  DS    D                   INCHES- NEWS 2 DECIMALS                      
BUFINS   DS    D                   INSERTIONS                                   
BUFGRS   DS    D                   GROSS                                        
BUFACOM  DS    D                   AGY COM                                      
BUFCD    DS    D                   CASH DISC                                    
BUFNLNS  DS    D                   POST COL.CONV LINES - NEWS                   
BUFNINC  DS    D                   POST COL.CONV INCHES NEWS 2 DEC              
BUFNINS  DS    D                   POST COL CONV. INSERTIONS                    
*                                                                               
         DS    0H                                                               
WRKREC   DS    0CL72                                                            
WRKLEN   DS    H                                                                
         DS    H                                                                
WRKPFLE  DS    CL1                                                              
WRKID    DS    CL8                                                              
WRKAGY   DS    CL2                                                              
WRKMED   DS    CL1                                                              
WRKCLT   DS    CL3                                                              
WRKPUB   DS    CL11                                                             
WRKCON   DS    CL3                                                              
         DS    CL7                 SPARE                                        
*                                                                               
WRKOGRS  DS    D                                                                
WRKNGRS  DS    D                                                                
WRKCGRS  DS    D                                                                
WRKINS   DS    D                                                                
*                                                                               
ADRDATA  DS    CL(ADRLEN)          ADDRESS PRINT BLOCK                          
PADRDATA DS    CL(ADRLEN)          PRIOR ADDRESS PRINT BLOCK                    
ADRREC   DS    300C                                                             
*                                                                               
ADRTYP   DS    CL1          TYPE OF ADDRESS REC - CONTRACT, PAY, ETC.           
CLTDATA  DS    0CL7         USED TO PASS KEY INFO TO PPGETADR MODULE            
CLTAGY   DS    CL2                                                              
CLTMED   DS    CL1                                                              
CLTCODE  DS    CL3                                                              
CLTOFF   DS    CL1                                                              
*                                                                               
ADRCLT   DS    CL3                                                              
ADROFF   DS    CL1                                                              
ADRWORK  DS    CL4                                                              
ADRDUP   DS    CL1                 "X" MEANS "SAME ADDRESS"                     
*                           BELOW THREE FIELDS FROM RELEVANT PROFILE            
ADRSW    DS    CL1                 SHOW CONTRACT ADDRESS = Y                    
TFXSW    DS    CL1                 SHOW T=TELEPHONE, F=FAX, B=BOTH              
EMLSW    DS    CL1                 SHOW E-MAIL ADDRESS = Y                      
*                                                                               
MYFAX    DS    CL12                                                             
TOFAX    DS    CL16                FROM CONTROL FILE FAX RECORD                 
*                                                                               
PPBYOWRK DS    600C                                                             
*                                                                               
RTLKELS  CSECT                                                                  
         DS    4000X                                                            
*                                                                               
WRKRBUFF CSECT                                                                  
         DS    4096X                                                            
*                                                                               
ADVCTAB  CSECT                                                                  
         DS    6000C                                                            
ADVCTABX EQU   *-1                                                              
*                                                                               
SORTTAB  CSECT                                                                  
         DS    30015C          TABLE TO SORT BUYS                               
*                              ROOM FOR 2000 BUYS + ONE BYTE (X'FF')            
SORTTABX EQU   *                                                                
*                              15 BYTES PER BUY                                 
*                              INS DATE - 3                                     
*                              CREATED DATE - 3  (USED FOR AU)                  
*                              PRD - 3                                          
*                              EST - 2                                          
*                              DISKADDR - 4                                     
*                                                                               
MAXEQU   EQU   (SORTTABX-SORTTAB)/15-1                                          
*                                                                               
ADRDATAD DSECT                     TO COVER ADRDATA ABOVE                       
*                                                                               
ADRNAM   DS    CL30                                                             
ADRZNAM  DS    CL20                                                             
ADRLIN1  DS    CL30                                                             
ADRLIN2  DS    CL30                                                             
ADRATTN  DS    CL30                                                             
ADRTEL   DS    CL12                                                             
ADRFAX   DS    CL16                                                             
ADREML   DS    CL60                E-MAIL                                       
OVRSRCE  DS    CL1                 CONTRACT OVERRIDE INDICATOR                  
*                                  X'01'=ATTN, X'02'=TEL, X'04'=FAX             
ADRLEN   EQU   *-ADRNAM                                                         
*                                                                               
BUYLND   DSECT                                                                  
*                        THESE FIELDS ONLY FOR AU                               
ABAGY    DS    CL2                                                              
         DS    CL3                                                              
ABCLT    DS    CL3                                                              
         DS    CL2                                                              
ABPRD    DS    CL3                                                              
         DS    CL2                                                              
ABDATE   DS    CL11                                                             
         DS    CL2                                                              
ABNDATE  DS    CL8                                                              
         ORG   ABDATE                                                           
*                          THESE FIELDS ONLY FOR 19                             
BCLT     DS    CL3                                                              
         DS    CL2                                                              
BPRD     DS    CL3                                                              
         DS    CL2                                                              
BDATE    DS    CL11                                                             
         DS    CL2                                                              
*                          THESE FIELDS FOR BOTH                                
BEST     DS    CL3                                                              
         DS    CL2                                                              
BDESC    DS    0CL20                                                            
         DS    CL13                                                             
BLINES   DS    CL7                                                              
         DS    CL2                                                              
BORATE   DS    CL8                                                              
         DS    CL1                                                              
BOGROSS  DS    CL14                                                             
         DS    CL1                                                              
BCASHD   DS    CL14                                                             
         DS    CL1                                                              
BNET     DS    CL14                                                             
*                                                                               
         SPACE 2                                                                
       ++INCLUDE DDLOGOD                                                        
*                                                                               
         BUFF  LINES=300,ROWS=1,COLUMNS=9,FLAVOR=PACKED,COMMENT=20,    X        
               KEYLIST=(28,A)                                                   
*                                                                               
       ++INCLUDE DDBUFFALOD                                                     
*                                                                               
         EJECT                                                                  
PCATELD  DSECT                                                                  
       ++INCLUDE PCATELEM                                                       
PCTFELD  DSECT                                                                  
       ++INCLUDE PCTFELEM                                                       
PGETADRD DSECT                                                                  
       ++INCLUDE PPGETADRD                                                      
** CTGENFAX                                                                     
       ++INCLUDE CTGENFAX                                                       
         EJECT                                                                  
       ++INCLUDE GETADVCD                                                       
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE PPMODEQU                                                       
         EJECT                                                                  
       ++INCLUDE PPREPWORK                                                      
         EJECT                                                                  
       ++INCLUDE PPREPWORK2                                                     
         EJECT                                                                  
       ++INCLUDE PPNEWFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061PPREP1902 04/19/16'                                      
         END                                                                    
