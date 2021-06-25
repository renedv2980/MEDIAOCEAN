*          DATA SET PPREP7702  AT LEVEL 023 AS OF 12/17/07                      
*PHASE PP7702A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE CHOPPER                                                                
*INCLUDE PPGETADR                                                               
*INCLUDE PRNTOFC                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PP7702 - PRINTPAK TRAFFIC LIST'                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* boby  11/07   5 digit io numbers                                              
*                                                                               
* SMYE  03/06   ADD CODE FOR STEWARDSHIP BUYS                                   
*                                                                               
* SMYE  11/05   PRINT 2-CHARACTER CLIENT OFFICES                                
*                                                                               
* SMYE  08/05   USE CORE-RESIDENT OFFICER                                       
*                                                                               
* SMYE 05-12/05 ALWAYS REPLACE "AD NO" WITH AD-ID IF AD-ID USED                 
*                                                                               
* SMYE  01/05   CONDITIONALLY REPLACE "AD NO" WITH Ad ID                        
*                                                                               
* SMYE  11/04   ADD CODE FOR WEBIO INSERTION ORDERS                             
*                                                                               
* YKAP/ 05/03-  FIX BUG IN PRTCOM IN HANDLING OF COMMENT TABLE                  
* SMYE  06/04                                                                   
*                                                                               
* SMYE 08/01    ADD CODE (FLTPGRP) FOR PRODUCT GROUP REQUESTS                   
*                                                                               
* SMYE 07/01    DISPLAY LEGAL WARNING INFO IF PRESENT                           
*               AND CHANGE PP77WRK FOR                                          
*               ADDRESSABILITY PROBLEMS (INSTAB MOVED TO A CSECT)               
*               AND SKIP "NO TRAFFIC" (X'20' IN PBDSTAT) BUYS                   
*                                                                               
* KWAN 01/31/01 PRINT EXTENSION DATE INFO IF PRESENT                            
*                                                                               
* SMYE 05/00    CHANGES FOR LARGER PPNEWFILE                                    
*                                                                               
* KWAN 02/00    FIX QESTEND FILTER PROBLEM                                      
*                                                                               
* BPLA 10/99    READ AND CHECK P72A PROFILE TO SEE IF RATE                      
*               CHANGES SHOULD BE CHECKED                                       
*                                                                               
* SMYE 07/15/99 CHANGES IN RREP20 FOR "ONE-SIZE" PUB ADDRESS RECORD             
*                                                                               
* KWAN 06/99    SUB-ROUTINE FOR CHECKING ESTIMATE FILTERS                       
*                                                                               
* KWAN 03/99    ADD CODES FOR OFFICER, PUT TLIN INTO A CSECT                    
*                                                                               
* SMYE 11/97    DISPLAY SFH BUY INFO. (AT TL3201.)                              
*                                                                               
* SMYE 12/96    USE CALL TO PPGETADR TO GET ADDRESSES (IN READREP)              
*                                                                               
* BPLA 3/96     CHECK PUBAOVEL LENGTH BEFORE MOVING                             
*               PUBAOLN3                                                        
*                                                                               
* SMYE 2/96     DISPLAY REPEAT PUB FROM PIOELEM (X'70')                         
*               MOVED TLTOT ROUTINE TO THE "COMMON" CSECT                       
*                                                                               
* SMYE 12/13/95 CHANGED DTCNV TO DATCON WITH NEW PARAM'S                        
*                                                                               
* BPLA 6/95     DISPLAY SHIP DATE FROM PBSHPDEL (X'86')                         
*                                                                               
* LWEI 02/02/93 PRINT REF= ELEMENT (X'83')                                      
*                                                                               
* BPLA 5/4/92   DON'T CHOP LARGE IC= AND PI= FREE FORM COMMENTS                 
*               COMTAB LINES NOW 44 LONG INSTEAD OF 35                          
*                                                                               
* BPLA 4/14/92  CHANGES TO HANDLE LARGE IC= AND PI= COMMENTS                    
*                                                                               
* BPLA 11/18/91 OPTION TO SHOW SHIPPING ADDRESS INSTEAD OF TRAFFIC              
*               QOPT6='S'                                                       
*                                                                               
* BPLA 5/21/91 ALLOW FOR CLT/PRD/EST/JOB SORT WITHIN MARKET                     
*              MARKET SORT OPTION = 'F'                                         
* ROSA 2/8/91  SORT BY MARKET WHEN MEDIA IS O OR N AND OPTION                   
*              IS REQUESTED (OPT8)                                              
*                                                                               
* ROSA  8/13/90 COPY NUMBER NOT PRINTED WHEN OVERRIDDEN IN                      
*                COMMENT.                                                       
*                                                                               
* BPLA 7/23/90  BUG FIXED  - WASN'T PRINT REQUESTED STANDARD COMMENT            
*               IF NOT SORTING.                                                 
*               ALSO WAS ALWAYS SKIPPING TO NEW PAGE AFTER REQUESTED            
*               COMMENT AND LIST OF STANDARD COMMENTS                           
*                                                                               
* BPLA 7/16/90  BUG FIXED IN TLLAST CODE OF REQUESTED STANDARD COMM             
*               WAS NOT BEING DISPLAYED PROPERLY - REMOVED DISPLAY              
*                                                                               
* BPLA 7/13/90  STANDARD COMMENT MOVE FROM QRECORD+52 (QPAY) TO                 
*               QCOMM (COL 21 CARD 2)                                           
*                                                                               
* BPLA 12/5/89  GROUP REQUESTS                                                  
*                                                                               
* BPLA 8/5/88   WHEN DOING UNORDERED BUYS (QOPT1=N) DON'T                       
*               CHECK FOR IC= COMMENTS CHANGES                                  
*                                                                               
* ROSA 6/8/88   IF CLOSING OR MATERIAL SORT DATE REQUESTED AND                  
*               THESE DATES ARE BIN ZERO, PLUG IN INSERT DATE                   
*                                                                               
* ROSA 6/8/88   ADD PROFILE OPTION TO PRINT EITHER CLOSING OR MATERIAL          
*               CLOSING DATE-- REQUEST WILL OVERRIDE IF QBPDATE IS              
*               EITHER A C OR M.                                                
*                                                                               
* ROSA 5/9/88   PRINT STANDARD COMMENT IF ENTERED IN REQUEST                    
*                                                                               
* ROSA 5/4/88   CHANGE LOGIC FOR REP ADDRESSES TO HANDLE MULTIPLE               
*               CLIENTS WITH POSSIBLE REP OVERRIDES FOR THE SAME                
*               PUBLICATION.                                                    
*                                                                               
* ROSA 4/22/88  ADD NEW POSITION ORDER CANGES                                   
*                                                                               
* ROSA 4/17/88  USE PROFILE FOR PRINTING TRAFFIC REPS                           
*               QOPT6   PROF+4                                                  
*                                                                               
* ROSA 4/4/88   USE PROFILES IF PRESENT FOR                                     
*               1- PRINT OPTION (PROF+0)   QOPT2                                
*               2- ADFILE INFO (PROF+1)    QOPT4                                
*               3- DOUBLE SPACING (PROF+2) QOPT5                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*  PROFILE   OPTION   DESCRIPTION                                               
*    +0      QOPT2    $- PRINT DOLLARS                                          
*                                                                               
*    +1      QOPT4    Y- PRINT ADFILE DATA                                      
*                                                                               
*    +2      QOPT5    Y- DOUBLE SPACING                                         
*                                                                               
*    +3      NONE     Y- PRINT EXPANDED STANDARD COMMENTS                       
*                                                                               
*    +4      QOPT6    Y- PRINT TRAFFIC REP ADDRESSES                            
*                     S- PRINT SHIPPING ADDRESS IF FOUND, ELSE USE              
*                        TRAFFIC                                                
*                                                                               
*    +5      QBPDATE  D= INSERTION DATE                                         
*                     M= MATERIALS CLOSING                                      
*                     B= BILLING DATE                                           
*                     P= PAYABLE DATE                                           
*                     S= SALE DATE                                              
*                     I= INSERTION ORDER DATE                                   
*                     C= CLOSING DATE                                           
*                                                                               
*                                                                               
*    +6      QOPT1    A= ALL                                                    
*                     N= UNORDERED                                              
*                     O= ORDERED                                                
*                                                                               
*    +7      QOPT3    A= SORT PUB BY ALPHA CODE                                 
*                     N= NO SORT                                                
*                                                                               
*    +8               M=PRINT MATERIAL CLOSING DATE                             
*                     C= PRINT CLOSING DATE                                     
*                                                                               
*    +9      QQOPT8   Y=SORT BY MARKET                                          
*                     F=SORT MARKET HIGH (FIRST)                                
*                                                                               
*    ------->QOPT8    Y=MARKET SORT BEFORE PUB                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PP7702   CSECT                                                                  
         NMOD1 0,PP7702                                                         
         PRINT NOGEN                                                            
         SPACE 2                                                                
         LA    R7,1(RB)                                                         
         LA    R7,4095(R7)                                                      
         USING PP7702+4096,R7                                                   
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP77WRKD,R8                                                      
*                                                                               
         L     R1,=A(SINSTAB)                                                   
         ST    R1,ASINSTAB                                                      
*                                                                               
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         MVC   ACONIO1,ACONIO      (A)PCONREC                                   
         DROP  RF                                                               
         SPACE 2                                                                
         CLI   MODE,UNSRTREC                                                    
         BNE   EXIT                                                             
         MVC   SAVPARS,DMCB+4                                                   
         LM    R2,R3,SAVPARS                                                    
         B     TLBRTAB(R2)                                                      
*                                                                               
TLBRTAB  B     TLFIRST                                                          
         B     TLINPUT                                                          
         B     TLOUTPUT                                                         
         B     TLLAST                                                           
*                                                                               
         TITLE 'TLFIRST   -- INITIALIZE'                                        
*                                                                               
TLFIRST  DS    0H                                                               
         MVI    REPLINED,0         INITIALIZE                                   
         L      RF,PPWORK2C                                                     
         USING  PPWORK2D,RF                                                     
         MVC   QQOPT8,QOPT8                                                     
         MVC   QQOPT9,QOPT9        STEWARDSHIP OPTIONS                          
         DROP  RF                                                               
         XC    SAVMID,SAVMID                                                    
         XC    PREVREP,PREVREP                                                  
         MVC   TREPA1(180),SPACES                                               
         MVI   SAVMIDSW,0                                                       
         MVI   SAVMID,255                                                       
*                                                                               
         XC    DMCB(4),DMCB        NEED ADDRESS OF OFFICER                      
         MVC   DMCB+4(4),=X'D9000A38'                                           
         L     RF,VCOMFACS                                                      
         L     RF,(CCALLOV-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB                                                        
         MVC   VOFFICER,DMCB                                                    
*                                                                               
         L     RE,=V(PRNTOFC)                                                   
         ST    RE,VPRNTOFC         STORE PRNTOFC ADDRESS                        
*                                                                               
* MUST READ PROFILE MANUALLY                                                    
*                                                                               
         CLC   QCLIENT,SPACES                                                   
         BE    CLRWRK                                                           
         CLC   QCLIENT,=C'ALL'                                                  
         BE    CLRWRK                                                           
         CLI   QCLIENT,C'*'        OFFICE REQUEST                               
         BE    CLRWRK                                                           
         CLI   QCLIENT,C'&&'       GROUP REQUEST                                
         BE    CLRWRK                                                           
         MVC   PBUYKEY(3),QAGENCY                                               
         MVC   PBUYKEY+4(3),QCLIENT                                             
         BAS   RE,GETCLT                                                        
*                                                                               
CLRWRK   XC    WORK,WORK                                                        
         XC    PROGPROF,PROGPROF                                                
         MVI   STDCOMSW,0          EXPAND STANDART COMMENTS                     
         MVC   WORK(4),=C'PO77'    READ 77 PROFILE                              
         MVC   WORK+4(3),QAGENCY                                                
         CLC   QCLIENT,=C'ALL'                                                  
         BE    SET02               IF ALL OR BLANK                              
         CLC   QCLIENT,SPACES                                                   
         BE    SET02               IF ALL OR BLANK                              
*                                                                               
         MVC   WORK+7(3),QCLIENT                                                
         CLI   QCLIENT,C'*'        DO SPECIFIC OFFICE                           
         BE    SET02                                                            
         CLI   PCLTOFF,C' '        DOES CLIENT USE OFFICE                       
         BE    SET02                                                            
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
*                                                                               
SET02    GOTO1 GETPROF,DMCB,WORK,PROGPROF,DATAMGR                               
*                                                                               
         OC    PROGPROF,PROGPROF   ANY PROFILE OPTIONS                          
         BZ    NOPROFIL                                                         
         MVC   STDCOMSW,PROGPROF+3                                              
         CLI   QOPT2,X'40'         PRINT OPTION $ OR SPACE OVERRIDE             
         BH    *+10                                                             
         MVC   QOPT2,PROGPROF                                                   
*                                                                               
         CLI   QOPT4,X'40'         ADFILE OPTION                                
         BH    *+10                                                             
         MVC   QOPT4,PROGPROF+1                                                 
*                                                                               
         CLI   QOPT5,X'40'         DOUBLE SPACING                               
         BH    *+10                                                             
         MVC   QOPT5,PROGPROF+2                                                 
         CLI   QQOPT8,C'A'         REQUEST FOR MARKET SORT                      
         BH    *+10                                                             
         MVC   QQOPT8,PROGPROF+9                                                
*                                                                               
NOPROFIL DS    0H                                                               
         CLI   QQOPT8,C'A'                                                      
         BH    *+8                                                              
         MVI   QQOPT8,C'N'         SET DEFAULT TO "N"                           
*                                                                               
         CLI   QMEDIA,C'N'         SET MARKET SORT OFF, IF NOT                  
         BE    NOPROF5             NEWSPAPERS OR OUTDOOR                        
         CLI   QMEDIA,C'O'                                                      
         BE    NOPROF5                                                          
         MVI   QQOPT8,C'N'                                                      
NOPROF5  DS    0H                                                               
         CLI   QBPDATE,C' '                                                     
         BNE   BPDOK                                                            
         CLI   PROGPROF+5,0                                                     
         BE    *+14                                                             
         MVC   QBPDATE,PROGPROF+5  USE PROFILE                                  
         B     *+8                                                              
         MVI   QBPDATE,C'D'        USE DEFAULT IF PROF + OPT BLANK              
BPDOK    CLI   QOPT1,C' '                                                       
         BNE   ORDOK                                                            
         CLI   PROGPROF+6,0                                                     
         BE    *+14                                                             
         MVC   QOPT1,PROGPROF+6    USE PROFILE                                  
         B     *+8                                                              
         MVI   QOPT1,C'A'          USE DEFAULT IF PROF + OPT BLANK              
*                                                                               
ORDOK    DS    0H                                                               
*                                                                               
*    TEST FOR TRAFFIC ADDRESS REQUEST                                           
*                                                                               
         CLI   QOPT6,X'40'         NOT OVERRRIDEN IN REQ                        
         BNE   *+10                                                             
         MVC   QOPT6,PROGPROF+4                                                 
         MVI   FCGTTREP,C'N'                                                    
*                                                                               
         CLI   QOPT6,C'S'          SHIPPING ADDRESS                             
         BE    ORDOK5                                                           
         CLI   QOPT6,C'Y'          DO TRAFFIC REPS                              
         BNE   *+8                                                              
ORDOK5   MVI   FCGTTREP,C'Y'                                                    
         CLI   QOPT3,X'40'         NOT OVERRRIDEN IN REQ (SORT BY PUB)          
         BNE   *+10                                                             
         MVC   QOPT3,PROGPROF+7                                                 
*                                                                               
*  IF QOPT3 IS AN A CHECK PUB FOR BLANKS AND IF SO MOVE ALL IN PUB              
*                                                                               
         CLI   QOPT3,C'A'          SORT BY PUB NAME                             
         BNE   NOSORTP                                                          
         CLI   QPUB,C' '           IF BLANK SHOVE ALL IN QPUB                   
         BNE   *+10                                                             
         MVC   QPUB(3),=C'ALL'                                                  
NOSORTP  MVI   TREPAX,255          STOP  MOVING TO PRINT AREA                   
*                                                                               
         CLI   QBPDATE,C'M'        EXTRACT ON MATERIAL CLOSING DATE             
         BNE   LOOK4C              FORCE PROFILE TO PRINT MATERIAL CD           
         MVC   PROGPROF+8(1),QBPDATE         MOVE M OPTION                      
         B     OIDMINB                                                          
*                                                                               
LOOK4C   CLI   QBPDATE,C'C'        EXTRACT ON PUBLICAT CLOSING DATE             
         BNE   *+10                FORCE PROFILE TO PRINT CLOSING DATE          
         MVC   PROGPROF+8(1),QBPDATE         MOVE M OPTION                      
OIDMINB  DS    0H                                                               
         MVC   HEAD966(8),=C'        '                                          
         CLI   PROGPROF+8,C'M'     CHECK PROFILE                                
         BNE   *+10                                                             
         MVC   HEAD966(8),=C'MATERIAL'                                          
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
*                                                                               
         MVC   P,SPACES                                                         
         MVI   PBNAMX,C' '                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         XC    OLDKEY,OLDKEY                                                    
         XC    OLDJOB,OLDJOB                                                    
         XC    ESTTOTS(3*L'ESTTOTS),ESTTOTS                                     
*                                  IS SORT NECESSARY?                           
         L     R1,=A(SCOMTAB)                                                   
         ST    R1,ASCOMTAB                                                      
         SR    R0,R0               SET BSPARS FOR BINSRCH                       
         L     R1,ASCOMTAB                                                      
         SR    R2,R2                                                            
         LA    R3,7                                                             
         LA    R4,6                                                             
         LH    R5,=H'1000'                                                      
         STM   R0,R5,BSPARS                                                     
*                                                                               
         CLI   QMEDIA,C'N'                                                      
         BNE   TLF6                FOR MAG -YES, BECAUSE OF CLOSE DATE          
         CLC   QCLIENT,SPACES                                                   
         BE    TLF6                                                             
         CLC   QPRODUCT,SPACES                                                  
         BE    TLF6                                                             
         CLC   QJOB(5),=C'ALL  '                                                
         BE    TLF6                                                             
         CLC   QEST,=C'ALL'                                                     
         BE    TLF6                                                             
         CLC   QPUB,SPACES                                                      
         BE    TLF6                                                             
         CLC   QPUB(3),=C'ALL'                                                  
         BNE   TLF1                                                             
         CLI   QOPT3,C'A'          ALPHA SORT OF 'ALL' PUBS                     
         BE    TLF6                                                             
TLF1     DS    0H                                                               
         CLI  QQOPT8,C'Y'          MARKET SORT                                  
         BE    TLF6                                                             
         CLI  QQOPT8,C'F'          MARKET SORT                                  
         BE    TLF6                                                             
*                                  NO SORT                                      
         MVI   RUNSW,X'20'                                                      
         CLC   QSORT,=C'21'                                                     
         BNE   *+8                                                              
         MVI   RUNSW,X'21'                                                      
         XC    KEY,KEY                                                          
         XC    SAVPARS,SAVPARS                                                  
TLF2     DS    0H                                                               
         GOTO1 =A(TLIN)                                                         
         CLI   SAVPARS+3,8         END                                          
         BNE   TLF2A                                                            
         MVI   PBUYREC,X'FF'                                                    
         MVC   PBUYREC+1(24),PBUYREC                                            
TLF2A    DS    0H                                                               
         BAS   RE,TLOUT                                                         
         CLI   PBUYREC,X'FF'                                                    
         BNE   TLF2                                                             
         XC    SAVPARS,SAVPARS                                                  
         B     TLCOMM                                                           
*                                                                               
TLF6     DS    0H                                                               
         MVI   RUNSW,0                                                          
         MVC   SAVPARS+04(2),=C'81'   SORT REC LEN                              
         MVC   SAVPARS+06(2),=C'76'   SORT KEY LEN                              
         XC    KEY,KEY                                                          
         XC    PBUYREC(256),PBUYREC                                             
         XC    SRTLIN,SRTLIN                                                    
         B     EXIT                                                             
         SPACE 3                                                                
TLINPUT  DS    0H                                                               
         GOTO1 =A(TLIN)                                                         
         B     EXIT                                                             
         SPACE 3                                                                
TLOUTPUT DS    0H                                                               
         BAS   RE,TLOUT                                                         
         B     EXIT                                                             
         SPACE 3                                                                
TLLAST   DS    0H                                                               
*                                                                               
         MVI   PBUYREC,X'FF'                                                    
         MVC   PBUYREC+1(24),PBUYREC                                            
         BAS   RE,TLOUT                                                         
*                                                                               
TLCOMM   DS    0H                PRINT REQUESTED STANDARD COMMENT               
         MVI   FORCEHED,C'N'     MIGHT HAVE BEEN SET TO "Y" IN TLOUT            
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         CLC   QCOMM(6),SPACES     WAS STANDARD COMMENT REQSTD                  
         BNH   TLCOMMX                                                          
*                                                                               
*                                                                               
         BAS   RE,TLPRT            SKIP A LINE                                  
         L     RF,PPWORK2C                                                      
         LA    R4,P                                                             
         USING TLLIND,R4                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY                                                   
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),QCOMM      STD COMMENT RIGHT ALIGNED                    
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+8                                                              
         B     TRTC30              COMMENT NOT FOUND                            
         L     R0,ACONIO1          (A)PCONREC                                   
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         MVI   LINENEED,0                                                       
         LA    R4,0                                                             
         L     R2,ACONIO1          (A)PCONREC                                   
         LA    R2,33(R2)                                                        
         MVI   ELCOD,X'40'                                                      
TLOOP    BAS   RE,NEXTEL                                                        
         BNE   TRTC13                                                           
         LA    R4,1(R4)                                                         
         B     TLOOP                                                            
TRTC13   STC   R4,LINENEED                                                      
*                                                                               
         LA    R4,P              FOR PRINT LINE                                 
*                                                                               
         L     R2,ACONIO1          (A)PCONREC                                   
         LA    R2,33(R2)                                                        
         MVI   ELCOD,X'40'                                                      
         CLI   0(R2),X'40'                                                      
         BE    TRTC15                                                           
TRTC12   BAS   RE,NEXTEL                                                        
         BNE   TRTC30                                                           
TRTC15   ZIC   R5,1(R2)                                                         
         LA    R3,2(R2)                                                         
         CLI   2(R2),C'+'                                                       
         BNE   TRTC17                                                           
         MVC   SPACING,3(R2)                                                    
         NI    SPACING,X'0F'                                                    
         CLI   SPACING,3                                                        
         BNH   *+8                                                              
         MVI   SPACING,3                                                        
         CLI   SPACING,0                                                        
         BNE   *+8                                                              
         MVI   SPACING,1                                                        
         MVI   TLCLT,0                                                          
         BAS   RE,TLPRT                                                         
         SH    R5,=H'2'        FOR '+' AND NUMBER OF LINES                      
         LA    R3,4(R2)                                                         
*                                                                               
TRTC17   SH    R5,=H'3'        FOR ELEM CODE+LENGHT +1 FOR EXECUTE              
         BM    TRTC18          NOTHING TO PRINT SO PRINT BLANK LINE             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   TLCLT+10(0),0(R3)                                                
TRTC18   MVI   TLCLT,0                                                          
         BAS   RE,TLPRT                                                         
         B     TRTC12                                                           
*                                                                               
TRTC30   DS    0H                                                               
TLCOMMX  DS    0H                                                               
*                                                                               
***********************************************************************         
*                                                                               
EXITT    CLI   STDCOMSW,C'Y'        LIST STANDARD COMMENTS                      
         BNE   EXIT                                                             
         MVI   FORCEHED,C'Y'        TO A NEW PAGE                               
         GOTO1 =V(PRTCOM)                                                       
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
EXIT     DS    0H                                                               
         MVC   DMCB+4(8),SAVPARS                                                
XIT      DS    0H                                                               
         XIT1                                                                   
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'TLOUT   OUTPUT RECORD PROCESSING'                               
*                                                                               
TLOUT    NTR1                                                                   
         SPACE 2                                                                
         USING SORTRECD,R3                                                      
         MVC   SAVEMMM,SORTKEY                                                  
         CLI   RUNSW,0                                                          
         BNE   TLOUT2                                                           
*                                 IF CLIENT OR PRODUCT IS BLANK                 
*                                 CLEAR RECORDS AREAS - SO HEADLINES            
*                                 WON'T HAVE WRONG INFO                         
         CLI   QCLIENT,C' '                                                     
         BNE   *+10                                                             
         XC    PCLTREC(100),PCLTREC                                             
         CLI   QPRODUCT,C' '                                                    
         BNE   *+10                                                             
         XC    PPRDREC(100),PPRDREC                                             
*                                                                               
         CLI   PBUYREC,X'FF'                                                    
         BE    TLOUT2                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         MVC   KEY+27(4),SRECDA                                                 
         GOTO1 GETPRT                                                           
*        IF SHOWING SHIPPING OR TRAFFIC ADDRESS                                 
*        MUST READ CLIENT                                                       
*                                                                               
TLOUT1   DS    0H                                                               
*                                                                               
         B     TLOUT1B     NOW ALWAYS READ CLIENT                               
*                          SINCE I WILL NEED TO CHECK P72A PROFILE              
*****    CLI   QOPT6,C'S'         SHIPPING ADDRESS                              
*****    BE    TLOUT1B                                                          
*****    CLI   QOPT6,C'Y'         OR TRAFFIC ADDRESS                            
*****    BNE   TLOUT1F                                                          
TLOUT1B  BAS   RE,GETCLT           MUST READ CLIENT HERE TO GET PROPER          
*                                  ADDRESSES                                    
TLOUT1F  DS    0H                                                               
MCHGSW   MVC   CHGSW,SCHGSW                                                     
         DROP  R3                                                               
*                                                                               
TLOUT2   DS    0H                                                               
*---------->  PRINT A SPACE AFTER LAST LINE WHEN THERE IS A CHANGE              
*               IN MARKET                                                       
         CLI  QQOPT8,C'N'          MARKET SORT                                  
         BE    TLOUT2A2                                                         
DOMIDAX  L     R3,SAVPARS+4                                                     
         USING SORTRECD,R3                                                      
         CLC   SAVMID,SKMARK   HAS MID BEEN PRINTED AND NO CHANGE               
         BE    TLOUT2A2            IN MARKET                                    
         OC    SKMARK,SKMARK     NO MARKET                                      
         BZ    TLOUT2A2            NO SPACE                                     
         CLI   SAVMID,255          FIRST TIME THRU                              
         BE    TLOUT2A2            NO SPACE                                     
         CLC   QCLIENT,SPACES      CLIENTS TOGETHER                             
         BE    SPACEONE            YES                                          
         CLC   PBUYKCLT,OLDCLT                                                  
         BNE   TLOUT2A2            REPORT WILL SKIP TO NEXT PAGE                
SPACEONE MVI   P,C' '                                                           
         GOTO1 REPORT                                                           
*                                                                               
******** BAS   RE,DOMIDS                                                        
TLOUT2A2 CLI   QQOPT8,C'F'        SEE IF MARKET HIGH                            
         BE    TLOUT6             NO PAGE BREAKS                                
*                                                                               
         CLC   QCLIENT,SPACES                                                   
         BE    TLOUT4                                                           
         CLC   PBUYKCLT,OLDCLT                                                  
         BE    TLOUT4                                                           
         CLI   OLDCLT,0                                                         
         BE    TLOUT2B                                                          
         BAS   RE,CLRSMID                                                       
         BAS   RE,TLPRT                                                         
         BAS   RE,ENDEST                                                        
         BAS   RE,ENDPRD                                                        
         BAS   RE,ENDCLT                                                        
TLOUT2B  DS    0H                                                               
         BAS   RE,GETCLT                                                        
         MVI   SAVMID+1,0                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    OLDPRD,OLDPRD                                                    
         XC    OLDEST,OLDEST                                                    
         XC    OLDPUB,OLDPUB       FORCE PUB CHANGE                             
         MVC   OLDJOB,=6X'FF'      FORCE JOB CHANGE                             
*                                                                               
TLOUT4   DS    0H                                                               
         CLC   QPRODUCT,SPACES                                                  
         BE    TLOUT5                                                           
         CLI   RUNSW,X'21'         PUB/PRD SEQ                                  
         BE    TLOUT5                                                           
         CLC   PBUYKPRD,OLDPRD                                                  
         BE    TLOUT5                                                           
         CLI   OLDPRD,0                                                         
         BE    TLOUT4B                                                          
         BAS   RE,CLRSMID                                                       
         BAS   RE,TLPRT                                                         
         BAS   RE,ENDEST                                                        
         BAS   RE,ENDPRD                                                        
TLOUT4B  DS    0H                                                               
         MVI   SAVMID+1,0                                                       
         BAS   RE,GETPRD                                                        
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         XC    OLDEST,OLDEST                                                    
         XC    OLDPUB,OLDPUB       FORCE PUB CHANGE                             
         MVC   OLDJOB,=6X'FF'      FORCE JOB CHANGE                             
*                                                                               
TLOUT5   DS    0H                                                               
*                                                                               
         CLC   QEST,SPACES                                                      
         BE    TLOUT6                                                           
*                                                                               
         CLC   =C'ALL',QEST        CHECKING FOR EST FILTER                      
         BE    *+14                                                             
         CLC   QESTEND,SPACES      CHECKING FOR EST RANGE                       
         BNE   TLOUT6              YES, EST RANGE, DON'T FORCE PAGE             
*                                                                               
         CLC   PBUYKEST,OLDEST                                                  
         BE    TLOUT6                                                           
         OC    OLDEST,OLDEST                                                    
         BZ    TLOUT5B                                                          
         BAS   RE,CLRSMID                                                       
         BAS   RE,TLPRT            SKIP A LINE                                  
         BAS   RE,ENDEST                                                        
TLOUT5B  DS    0H                                                               
         BAS   RE,GGETEST                                                       
******   GOTO1 =A(GGETEST),DMCB,RR=Y                                            
*                                                                               
         MVI   SAVMID+1,0                                                       
         MVI   FORCEHED,C'Y'                                                    
         XC    OLDPUB,OLDPUB      FORCE PUB CHANGE                              
         MVC   OLDJOB,=6X'FF'     FORCE JOB CHANGE                              
*                                                                               
TLOUT6   DS    0H                                                               
         CLI   PBUYREC,X'FF'                                                    
         BNE   TLOUT7                                                           
         BAS   RE,CLRSMID                                                       
         B     TLOUTX                                                           
TLOUT7   DS    0H                                                               
*        CLC   QJOB,SPACES                                                      
*        BE    TLOUT8                                                           
*        CLC   PBDJOB,OLDJOB     NO DOUBLE SPACE WHEN CHANGE IN JOB             
*        BE    TLOUT8                                                           
*        MVI   SPACING,2                                                        
*        BAS   RE,TLPRT                                                         
*                                                                               
TLOUT8   CLC   QPUB,SPACES                                                      
         BE    TLOUT10                                                          
         OC    OLDPUB,OLDPUB      SEE IF FIRST TIME                             
         BZ    TLOUT10            DON'T SPACE                                   
         CLC   PBUYKPUB,OLDPUB                                                  
         BE    TLOUT10                                                          
         MVI   SAVMIDSW,255                                                     
         BAS   RE,TLPRT            SPACE BETWEEN PUBS                           
         MVI   SAVMIDSW,0                                                       
         B     TLOUT10                                                          
*                                                                               
TLOUT10  DS    0H                                                               
*                                  COUNT LINES NEEDED                           
         BAS   RE,SETLW            SET LEGAL WARNING "FIELDS"                   
         LA    R4,1                                                             
         LA    R2,PBUYREC+33                                                    
         MVI   ELCOD,X'70'                                                      
TLOUT12  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   TLOUT13C            NOW CHECK WEBIO ORDERS                       
         LA    R4,1(R4)                                                         
*                                                                               
         USING PIOELEM,R2                                                       
*                                                                               
         OC    PIOLWCD(2),PIOLWCD  ANY LEGAL WARNING ENTRIES                    
         BZ    TLOUT13             NO                                           
         CLC   PIOLWCD(2),SAVLWCD  LW USED=PRD OR BUY LW ?                      
         BE    TLOUT13             YES                                          
         LA    R4,1(R4)            ANOTHER LINE IF "LEGAL WARNING USED          
*****    B     TLOUT13             DIFFERS FROM DEFAULT OR OVERRIDE LW"         
TLOUT13  DS    0H                                                               
**** CHECK FOR "REPEAT" PUB HERE *****                                          
         CLI   1(R2),X'32'         PIOELEM GT 50?                               
         BNH   TLOUT12             NO                                           
*                                                                               
         OC    PIORPUB,PIORPUB     ANY DATA HERE?                               
         BZ    TLOUT12             NO                                           
         LA    R4,1(R4)            ANOTHER LINE FOR "REPEAT" PUB                
         B     TLOUT12                                                          
*                                                                               
         DROP  R2                                                               
TLOUT13C DS    0H                  NOW CHECK FOR WEBIO ORDERS                   
         LA    R2,PBUYREC+33       COUNT LINES NEEDED                           
         MVI   ELCOD,X'71'                                                      
         LA    R4,1(R4)                                                         
TLOUT13F DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   TLOUT14             GO BUILD PRINT LINE                          
         LA    R4,1(R4)                                                         
*                                                                               
         USING PWIOELEM,R2                                                      
*                                                                               
         OC    PWIOLWCD(2),PWIOLWCD  ANY LEGAL WARNING ENTRIES                  
         BZ    TLOUT13J              NO                                         
         CLC   PWIOLWCD(2),SAVLWCD   LW USED=PRD OR BUY LW ?                    
         BE    TLOUT13J              YES                                        
         LA    R4,1(R4)            ANOTHER LINE IF "LEGAL WARNING USED          
*****    B     TLOUT13J            DIFFERS FROM DEFAULT OR OVERRIDE LW"         
TLOUT13J DS    0H                                                               
**** CHECK FOR "REPEAT" PUB HERE *****                                          
*NOP*    OC    PWIORPUB,PWIORPUB   ANY DATA HERE?                               
*NOP*    BZ    TLOUT13F            NO                                           
*NOP*    LA    R4,1(R4)            ANOTHER LINE FOR "REPEAT" PUB                
         B     TLOUT13F                                                         
*                                                                               
         DROP  R2                                                               
TLOUT14  DS    0H                                                               
         STC   R4,LINENEED                                                      
*                                  BUILD PRINT LINE                             
         LA    R4,P                                                             
         USING TLLIND,R4                                                        
         MVC   TLCLT,PBUYKCLT                                                   
         MVC   TLPRD,PBUYKPRD                                                   
         MVC   TLJOB,PBDJOB                                                     
*                                                                               
         OC    PBDJOB,PBDJOB       JOBCODE IN BUY ?                             
         BZ    TLOUT14X            NO                                           
*                                                                               
*SMY*    CLI   PBDJOB,X'FF'        "AD-ID ONLY" JOB CODE ?                      
*SMY*    BE    TLOUT14J            YES                                          
*                                                                               
*SMY*    CLI   PROGPROF+10,C'Y'    REPLACE AD CODE WITH AD ID ?                 
*SMY*    BNE   TLOUT14X            NO                                           
*                                                                               
TLOUT14J DS    0H                                                               
         BAS   RE,GETJOBR          GET JOB RECORD                               
         CLI   PJOBADID,C' '       AD ID THERE ?                                
         BNH   TLOUT14X            NO                                           
*                                                                               
         MVC   TLJOB,=C'*ADID*'    AD ID WILL BE DISPLAYED "LATER"              
*                                                                               
TLOUT14X DS    0H                                                               
         IC    R3,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R3),PBUYKPUB),(C'S',TLPUB)                        
*                                                                               
* MAT                                                                           
*                                                                               
TLOUT15  CLI   PROGPROF+8,C'M'           MATERIALS CLOSING DATE                 
         BNE   TLOUT16                                                          
         OC    PBDMDATE,PBDMDATE                                                
         BZ    TLOUT18                                                          
         LA    RE,PBDMDATE                                                      
TOUT15A  GOTO1 DATCON,DMCB,(3,(RE)),(5,TLCDAT)                                  
         B     TLOUT18                                                          
*                                                                               
TLOUT16  DS    0H                       OTHERWISE SHOW CLOSING DATE             
         OC    PBDCDATE,PBDCDATE                                                
         BZ    TLOUT18                                                          
*        GOTO1 DATCON,DMCB,(3,PBDCDATE),(5,TLCDAT)                              
         LA    RE,PBDCDATE                                                      
         B     TOUT15A                                                          
*                                                                               
* MAT                                                                           
*                                                                               
TLOUT18  DS    0H                                                               
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TLEST,DUB                                                        
*                                                                               
         MVC   SPAC1(61),SPACES                                                 
         GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKEY+7                              
         TM    PBUYCNTL,X'80'                                                   
         BZ    *+10                                                             
         XC    GROSS,GROSS                                                      
         LA    R6,PPBYOWRK                                                      
         USING PPBYOUTD,R6                                                      
         LA    RF,PBUYREC                                                       
         ST    RF,PBYOINPT                                                      
         LA    RF,GROSS                                                         
         ST    RF,PBYOVALS                                                      
         MVC   PBYODTCN,DATCON                                                  
         MVI   PBYOCTL,X'24'       FOR ZZZ ALLOCATNS AND IO COMMENTS            
*                                                                               
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
*                                                                               
         MVC   SAVDAT2,PBYOMDY2    HOLD 2ND INS DATE                            
*                                                                               
*                                                                               
         CLI   PBDBFD,C'T'         TEST BUY ?                                   
         BNE   TLOUT18B            NO                                           
*                                                                               
         LA    R1,TLIDAT                                                        
         MVI   WORK,C'T'                                                        
         TM    PBDSTAT2,X'40'      STEWARD BUY ?                                
         BNO   TLODTE50            NO                                           
****  SHOW STEWARD BUYS WITH LINE NO. - TEST BUYS WITHOUT LINE NO.  ***         
         MVI   WORK,C'S'           REPLACE T WITH S                             
         LA    RF,10                                                            
         CLI   PBYOMDY+3,C'/'                                                   
         BNE   *+8                                                              
         LA    RF,8                                                             
         MVC   TLIDAT(1),WORK                                                   
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R1),PBYOMDY                                                  
         TM    PBUYCNTL,X'80'                                                   
         BZ    *+8                                                              
         MVI   BYTE,C'D'                                                        
*                                                                               
         LA    RF,TLIDAT+10                                                     
         CLI   0(RF),C' '                                                       
         BE    *+8                                                              
         LA    RF,TLIDAT+12                                                     
         MVC   0(1,RF),BYTE                                                     
         B     TLOUT18G                                                         
*                                                                               
TLODTE50 DS    0H       SHOW TEST BUY DATES (NON-STEWARD) W/O LINE NOS.         
         LA    RF,7                                                             
         CLI   PBYOMDY+3,C'/'                                                   
         BNE   *+8                                                              
         LA    RF,5                                                             
*SMY*    MVI   TLIDAT,C'T'                                                      
         MVC   TLIDAT(1),WORK                                                   
         EX    RF,*+8                                                           
         B     TLOUT18A                                                         
         MVC   1(0,R1),PBYOMDY                                                  
TLOUT18A TM    PBUYCNTL,X'80'                                                   
         BZ    *+8                                                              
         MVI   BYTE,C'D'                                                        
*                                                                               
         LA    RF,TLIDAT+9                                                      
         CLI   0(RF),C' '                                                       
         BE    *+8                                                              
         LA    RF,TLIDAT+11                                                     
         MVC   0(1,RF),BYTE                                                     
         B     TLOUT18G                                                         
*                                                                               
*                                                                               
*                                                                               
TLOUT18B MVC   TLIDAT(11),PBYOMDY                                               
         MVC   BYTE,PBDBFD                                                      
         CLI   BYTE,C'B'                                                        
         BE    TLOUT18C                                                         
         CLI   BYTE,C'W'                                                        
         BE    TLOUT18C                                                         
         MVI   BYTE,C' '                                                        
TLOUT18C TM    PBUYCNTL,X'80'                                                   
         BZ    *+8                                                              
         MVI   BYTE,C'D'                                                        
*                                                                               
TLOUT18F LA    RF,TLIDAT+8                                                      
         CLI   0(RF),C' '                                                       
         BE    *+8                                                              
         LA    RF,TLIDAT+11                                                     
         MVC   0(1,RF),BYTE                                                     
*                                                                               
TLOUT18G MVC   SPAC1(14),PBYOGRS                                                
         CLI   QOPT2,C'$'                                                       
         BNE   TLOUT19                                                          
*                                                                               
         CLI   SPAC1,C' '                                                       
         BNE   *+8                                                              
         MVI   SPAC1,X'00'        SO IT WILL PRINT                              
         B     TLOUT22                                                          
*                                                                               
TLOUT19  DS    0H                                                               
         MVC   SPAC1(40),PBYOSPC                                                
         CLI   QMEDIA,C'N'                                                      
         BNE   TLOUT22                                                          
*                                                                               
         CLI   PBYOSPC,C' '                                                     
         BH    *+10                                                             
         MVC   SPAC1(7),PBYOUNTS   UNITS                                        
         LA    RF,SPAC1+15                                                      
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(2,RF),PBYOPRM                                                  
*                                                                               
         CLI   PBYOLBC,C' '        LINES X COLS                                 
         BNH   *+10                                                             
         MVC   SPAC2(17),PBYOLBC                                                
*                                                                               
         DROP  R6                                                               
*                                                                               
TLOUT22  DS    0H                                                               
         CLI   QOPT5,C'Y'          DOUBLE SPACING ?                             
         BNE   TLOUT22A                                                         
         LA    RE,SPAC2                                                         
         CLI   0(RE),C' '  SEE IF USED                                          
         BE    *+8                                                              
         LA    RE,SPAC3                                                         
         MVI   0(RE),0      RE IS SET TO FIRST AVAILABLE SPACE LINE             
*                           SET TO X'00' SO IT WILL PRINT                       
*                                                                               
TLOUT22A DS    0H                                                               
         L     R3,ASINSTAB                                                      
         MVI   0(R3),C' '          CLEAR INSTAB                                 
         MVI   ORDSW,0                                                          
*                                                                               
         BAS   RE,SETLW            SET LEGAL WARNING "FIELDS"                   
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCOD,X'70'                                                      
TLOUT22B DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   TLOUT23             NOW CHECK WEBIO ORDERS                       
*                                                                               
         USING PIOELEM,R2                                                       
         OC    PIODATE,PIODATE                                                  
         BZ    TLOUT22B                                                         
         OI    ORDSW,1             SET ON ORDERED SWITCH                        
*                                                                               
         MVC   0(34,R3),SPACES                                                  
         GOTO1 DATCON,DMCB,(3,PIODATE),(5,0(R3))                                
*                                                                               
         OC    PIORPTDT,PIORPTDT                                                
         BZ    TLOUT22D                                                         
         GOTO1 DATCON,DMCB,(3,PIORPTDT),(5,26(R3))                              
*                                                                               
TLOUT22D DS    0H                                                               
         LA    R5,13(R3)                                                        
         MVC   0(1,R5),PAGYKMED                                                 
         MVI   1(R5),C'-'                                                       
         GOTO1 DATCON,DMCB,(3,PIODATE),(0,WORK)                                 
*                                                                               
         MVC   2(5,R5),WORK+1                                                   
         MVI   7(R5),C'-'                                                       
         MVC   HALF,PIONUM                                                      
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(4,R5),DUB                                                      
*                                                                               
         MVC   9(3,R3),=C'ORI'                                                  
         CLI   PIOTYP,C'N'                                                      
         BE    TLOUT22F                                                         
         MVC   9(3,R3),=C'CHG'                                                  
         CLI   PIOTYP,C'C'                                                      
         BE    TLOUT22F                                                         
         MVC   9(3,R3),=C'CAN'                                                  
*                                                                               
TLOUT22F DS    0H                                                               
         OC    PIOLWCD(2),PIOLWCD  ANY LEGAL WARNING ENTRIES                    
         BZ    TLOUT22G            NO                                           
         CLC   PIOLWCD(2),SAVLWCD  LW USED=PRD OR BUY LW ?                      
         BE    TLOUT22G            YES                                          
**** OUTPUT LEGAL WARNING PRINTED=XX HERE *****                                 
         LA    R3,34(R3)           NEXT ENTRY                                   
         XC    0(34,R3),0(R3)                                                   
         MVC   2(22,R3),=C'LEGAL WARNING PRINTED='                              
         MVC   25(2,R3),PIOLWCD                                                 
*****    B     TLOUT22G                                                         
TLOUT22G DS    0H                                                               
         CLI   1(R2),X'32'         PIOELEM GT 50?                               
         BNH   TLOUT22H            NO                                           
         OC    PIORPUB,PIORPUB     ANY DATA HERE?                               
         BZ    TLOUT22H            NO                                           
**** OUTPUT REPEAT PUB HERE ******                                              
         LA    R3,34(R3)           NEXT ENTRY                                   
         XC    0(34,R3),0(R3)                                                   
         MVC   0(11,R3),=C'REPEAT PUB='                                         
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),PIORPUB),(C'S',11(R3))                        
*                                                                               
TLOUT22H DS    0H                                                               
         LA    R3,34(R3)           NEXT ENTRY                                   
         MVI   0(R3),C' '                                                       
         B     TLOUT22B                                                         
*                                                                               
         DROP  R2                                                               
TLOUT23  DS    0H                  NOW DO WEBIO ORDERS                          
         CLI   QOPT5,C'Y'          DOUBLE SPACING ?                             
         BNE   TLOUT23A                                                         
         LA    RE,SPAC2                                                         
         CLI   0(RE),C' '  SEE IF USED                                          
         BE    *+8                                                              
         LA    RE,SPAC3                                                         
         MVI   0(RE),0      RE IS SET TO FIRST AVAILABLE SPACE LINE             
*                           SET TO X'00' SO IT WILL PRINT                       
TLOUT23A DS    0H                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCOD,X'71'                                                      
TLOUT23B DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   TLOUT24                                                          
*                                                                               
         USING PWIOELEM,R2                                                      
         OC    PWIODATE,PWIODATE                                                
         BZ    TLOUT23B                                                         
         OI    ORDSW,1             SET ON ORDERED SWITCH                        
*                                                                               
         MVC   0(34,R3),SPACES                                                  
         GOTO1 DATCON,DMCB,(3,PWIODATE),(5,0(R3))                               
*                                                                               
*NOP*    OC    PWIORPDT,PWIORPDT                                                
*NOP*    BZ    TLOUT23D                                                         
*NOP*    GOTO1 DATCON,DMCB,(3,PWIORPDT),(5,26(R3))                              
*                                       *********************                   
TLOUT23D DS    0H                                                               
         LA    R5,9(R3)                 START WEBIO NUMBER IN TYP FIELD         
         MVC   0(1,R5),PAGYKMED                                                 
         GOTO1 DATCON,DMCB,(3,PWIODATE),(0,WORK)                                
*                                                                               
         MVC   0(1,R5),PBUYKMED    SET MEDIA                                    
*                                                                               
         AHI   R5,1                BUMP TO NEXT POSITION                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PWIO#YER         GET IO# YEAR                                 
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(2,R5),DUB         SET YEAR                                     
*                                                                               
         AHI   R5,2                BUMP TO CLIENT PART                          
         MVC   0(3,R5),PBUYKCLT    SET CLIENT                                   
         AHI   R5,2                BUMP POINTER                                 
         CLI   0(R5),C' '          IF EMPTY                                     
         BH    *+8                                                              
         MVI   0(R5),C'-'             FILL IN WITH DASH                         
*                                                                               
         AHI   R5,1                 BUMP PAST LAST OF CODE                      
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,PWIO#SQ#       GET SEQUENCE NUMBER                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         LA    RF,4                DISPLAY 4 DIGITS                             
         CP    DUB,=P'9999'        IF OVER 9999                                 
         BNH   *+8                                                              
         LA    RF,5                   DISPLAY 5 DIGITS                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         SLL   RF,4                SHIFT TO LEFT NYBBLE                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  0(0,R5),DUB         SEQUENCE NUMBER                              
*                                                                               
         SRL   RF,4                RETURN LENGTH TO RIGHT NYBBLE                
*                                                                               
         LA    R5,1(RF,R5)         NEXT OUTPUT POSITION                         
*                                                                               
         OC    PWIO#REV,PWIO#REV   SKIP IF NO REVISION NUMBER                   
         BZ    TLOUT23F            DONE WITH WEBIO NUMBER                       
*                                                                               
         CP    DUB,=P'9999'        IF 5 DIGITS                                  
         BNH   *+18                                                             
         MVC   0(2,R5),=C'RV'         USE 'RV' FOR REVISION                     
         AHI   R5,2                   BUMP POINTER                              
         B     *+14                                                             
         MVC   0(3,R5),=C'REV'     ELSE REVISION INDICATOR                      
         AHI   R5,3                                                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,PWIO#REV       GET REVISION NUMBER                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(3,R5),DUB         SEQUENCE NUMBER                              
*                                                                               
TLO23DX  DS    0H                                                               
*                                                                               
TLOUT23F DS    0H                                                               
         OC    PWIOLWCD(2),PWIOLWCD  ANY LEGAL WARNING ENTRIES                  
         BZ    TLOUT23G              NO                                         
         CLC   PWIOLWCD(2),SAVLWCD   LW USED=PRD OR BUY LW ?                    
         BE    TLOUT23G              YES                                        
**** OUTPUT LEGAL WARNING PRINTED=XX HERE *****                                 
         LA    R3,34(R3)           NEXT ENTRY                                   
         XC    0(34,R3),0(R3)                                                   
         MVC   2(22,R3),=C'LEGAL WARNING PRINTED='                              
         MVC   25(2,R3),PWIOLWCD                                                
*****    B     TLOUT23G                                                         
TLOUT23G DS    0H                                                               
*NOP*    OC    PWIORPUB,PWIORPUB   ANY DATA HERE?                               
*NOP*    BZ    TLOUT23H            NO                                           
**** OUTPUT REPEAT PUB HERE ******                                              
*NOP*    LA    R3,34(R3)           NEXT ENTRY                                   
*NOP*    XC    0(34,R3),0(R3)                                                   
*NOP*    MVC   0(11,R3),=C'REPEAT PUB='                                         
*NOP*    IC    R0,PAGYPROF+12                                                   
*NOP*    GOTO1 PUBEDIT,DMCB,((R0),PWIORPUB),(C'S',11(R3))                       
*                                                                               
TLOUT23H DS    0H                                                               
         LA    R3,34(R3)           NEXT ENTRY                                   
         MVI   0(R3),C' '                                                       
         B     TLOUT23B                                                         
*                                                                               
         DROP  R2                                                               
TLOUT24  DS    0H                                                               
         CLI   QOPT5,C'Y'          SEE IF DOUBLE SPACING                        
         BNE   *+18                                                             
         XC    0(34,R3),0(R3)                                                   
         LA    R3,34(R3)                                                        
         MVI   0(R3),C' '                                                       
         CLI   QOPT2,C'$'          $ TOTALS                                     
         BNE   TLOUT24D                                                         
         SR    RF,RF                                                            
         CLI   ORDSW,1             SEE IF ORDERED                               
         BE    *+8                 YES - POST TO ORDERED ACCUM                  
         LA    RF,8                                                             
         LA    R3,ESTTOTS                                                       
         LA    R6,3                                                             
         AR    R3,RF                                                            
TLOUT24B DS    0H                                                               
         LM    R0,R2,0(R3)                                                      
         A     R0,GROSS                                                         
         LA    R1,1(R1)                                                         
         STM   R0,R2,0(R3)                                                      
         LA    R3,L'ESTTOTS(R3)                                                 
         BCT   R6,TLOUT24B                                                      
TLOUT24D DS    0H                                                               
         MVI  TREPA7,X'41'                                                      
TL26     DS    0H                  GET PUBNAME                                  
         MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),PBUYKPUB                                                
         MVC   KEY+7(2),PAGYKAGY                                                
         MVI   KEY+9,X'81'                                                      
         CLC   PUBKEY(07),KEY                                                   
         BE    TL28A               ALREADY HAVE RECORD                          
*                                                                               
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    TL28                                                             
         CLI   PAGYPROF+16,C'0'    TEST SRDS DEFAULT                            
         BNE   *+8                                                              
         BAS   RE,NOPUB                                                         
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+8                                                              
         BAS   RE,NOPUB                                                         
         MVC   KEY+7(2),=C'ZZ'                                                  
         MVI   KEY+9,X'81'                                                      
         CLC   KEY(25),KEYSAVE                                                  
         BE    TL28                HAVE SRDS                                    
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
NOPUB    DC    H'0'                PUB NOT FOUND                                
*                                                                               
TL28     DS    0H                                                               
         GOTO1 GETNAME                                                          
TL28A    DS    0H                                                               
         MVC   KEY(64),SAVKEYS                                                  
*                                                                               
* SEE IF READ FOR REP TRAFFIC ADDRESS REQUIRED                                  
*                                                                               
         CLI   QOPT6,C'S'      SHIPPING ADDRESS                                 
         BE    DORPRD                                                           
*                                                                               
         CLI   QOPT6,C'Y'                                                       
         BNE   TL28B                                                            
*                                                                               
DORPRD   MVI   FCGTTREP,C'Y'   TRAFFIC REP                                      
         GOTO1 =V(COMMON),DMCB,0                                                
*                                                                               
TL28B    DS    0H                                                               
         MVC   PBNAM1(81),SPACES                                                
         MVC   PBNAM1(40),PUBNAME       NAME & ZONE NAME                        
         CLI   PAGYKMED,C'N'                                                    
         BNE   TL30                                                             
*                                                                               
         LA    RF,PBNAM2                                                        
         CLI   PBNAM2,C' '                                                      
         BNH   *+8                                                              
         LA    RF,PBNAM3                                                        
         MVC   0(16,RF),PUBCITY                                                 
         LA    RF,16(RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C','                                                       
         MVC   3(2,RF),PUBSTATE                                                 
*                                                                               
TL30     DS    0H                                                               
         OC    PBNAM1(80),SPACES                                                
         CLI   QOPT5,C'Y'                                                       
         BNE   TL32                                                             
         LA    RF,PBNAM2                                                        
         CLI   0(RF),C' '                                                       
         BE    TL31                                                             
         LA    RF,PBNAM3                                                        
         CLI   0(RF),C' '                                                       
         BE    TL31                                                             
         LA    RF,PBNAM4                                                        
*                                                                               
TL31     MVI   0(RF),0                 WILL SKIP A LINE                         
*                                                                               
TL32     DS    0H                                                               
         LA    RF,1                                                             
         CLI   PBNAM2,C' '                                                      
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
         CLI   PBNAM3,C' '                                                      
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         CLC   BYTE,LINENEED                                                    
         BNH   *+10                                                             
         MVC   LINENEED,BYTE                                                    
*                                                                               
         MVI   COMTAB,C' '         INITIALIZE COMMENT TABLE                     
         LA    R3,COMTAB                                                        
         LA    RF,1                                                             
         CLI   SAVDAT2,C' '        TEST 2ND INS DATE                            
         BNH   TL3201                                                           
         LA    RF,1(RF)                                                         
         MVC   0(44,R3),SPACES                                                  
         MVI   0(R3),C'+'                                                       
         MVC   1(8,R3),SAVDAT2                                                  
         LA    R3,44(R3)                                                        
*                                                                               
TL3201   DS    0H                                                               
         LA    R2,PBUYREC+33       FIND REF ELEMENT                             
         MVI   ELCOD,X'83'                                                      
         BAS   RE,NEXTEL                                                        
         BNE   TLADIDC                                                          
         ZIC   R1,1(R2)                                                         
         SH    R1,=H'3'            ELCODE + LEN + 1 FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   2(0,R2),SPACES                                                   
         BNH   TLADIDC                                                          
         MVC   0(44,R3),SPACES                                                  
         MVC   0(4,R3),=C'REF='                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R3),2(R2)                                                    
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
*                                                                               
TLADIDC  DS    0H                                                               
*SMY*    CLI   PBDJOB,X'FF'        "AD-ID ONLY" JOB CODE ?                      
*SMY*    BE    TLADIDJ             YES                                          
*SMY*    CLI   PROGPROF+10,C'Y'    SEE IF REPLACING ADCODE WITH AD ID           
*SMY*    BNE   TL3201B             NO                                           
         OC    PBDJOB,PBDJOB       ANYTHING IN JOB CODE ?                       
         BZ    TL3201B             NO                                           
TLADIDJ  CLI   PJOBADID,C' '       ANYTHING IN AD ID ?                          
         BNH   TL3201B             NO                                           
         MVC   0(44,R3),SPACES                                                  
         MVC   0(6,R3),=C'Ad-ID='                                               
         MVC   6(L'PJOBADID,R3),PJOBADID                                        
TLADIDX  LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
*                                                                               
TL3201B  LA    R2,PBUYREC+33                                                    
         MVI   ELCOD,X'82'                                                      
         BAS   RE,NEXTEL                                                        
         BNE   TL3201C                                                          
         USING PBFSIELD,R2                                                      
         CP    PBFSI,=P'0'                                                      
         BE    TL3201C                                                          
         MVC   0(44,R3),SPACES                                                  
         MVC   0(4,R3),=C'FSI='                                                 
         EDIT  PBFSI,(8,4(R3)),0,ALIGN=LEFT                                     
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
*                                                                               
         DROP  R2                                                               
*                                                                               
TL3201C  LA    R2,PBUYREC+33                                                    
         MVI   ELCOD,X'86'                                                      
         BAS   RE,NEXTEL                                                        
         BNE   TL3201E                                                          
         USING PBSHPDEL,R2                                                      
         OC    PBSHDATE,PBSHDATE                                                
         BZ    TL3201E                                                          
         MVC   0(44,R3),SPACES                                                  
         MVC   0(10,R3),=C'SHIP DATE='                                          
         GOTO1 DATCON,DMCB,(3,PBSHDATE),(5,10(R3))                              
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         DROP  R2                                                               
*                                                                               
* 12/23/98 (DISPLAY EXTENSION DAYS=NNN)                                         
*                                                                               
TL3201E  LA    R2,PBUYREC+33                                                    
         MVI   ELCOD,X'89'                                                      
         BAS   RE,NEXTEL                                                        
         BNE   TL3201F                                                          
         USING PEXDAYEL,R2                                                      
         CP    PEXDAYS,=P'0'                                                    
         BE    TL3201F                                                          
         MVC   0(44,R3),SPACES                                                  
         MVC   0(15,R3),=C'EXTENSION DAYS='                                     
         EDIT  PEXDAYS,(3,15(R3)),0,ALIGN=LEFT                                  
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         DROP  R2                                                               
*                                                                               
* 01/31/01 (DISPLAY EXTENSION DATE=MMMDD/YY)                                    
*                                                                               
TL3201F  LA    R2,PBUYREC+33                                                    
         MVI   ELCOD,X'96'                                                      
         BAS   RE,NEXTEL                                                        
         BNE   TL3201M                                                          
         USING PEXDATEL,R2                                                      
         MVC   0(44,R3),SPACES                                                  
         MVC   0(15,R3),=C'EXTENSION DATE='                                     
         GOTO1 DATCON,DMCB,(3,PEXDATE),(5,15(R3))                               
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         DROP  R2                                                               
*                                                                               
TL3201M  DS    0H                  FUTURE USES                                  
*                                                                               
TL3201W  DS    0H                                                               
         TM    PBDSTAT,X'04'       SFH BUY ?                                    
         BNO   TL3201X             NO                                           
         MVC   0(44,R3),SPACES                                                  
         MVC   0(8,R3),=C'SFH=HOLD'                                             
         TM    PBDSTAT,X'08'       BUY HELD ?                                   
         BO    *+10                YES                                          
         MVC   4(7,R3),=C'RELEASE'   REPLACE "HOLD"                             
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
*                                                                               
TL3201X  DS    0H                                                               
         CLI   CHGSW,0                                                          
         BE    TL3205              NO CHGS SINCE LAST IO                        
         MVC   0(44,R3),SPACES                                                  
         LA    RE,0(R3)                                                         
         MVI   0(RE),C'*'                                                       
         LA    RE,2(RE)                                                         
         TM    CHGSW,X'40'         CHK FOR SPACE CHG                            
         BZ    TL3202              NO                                           
         MVC   0(6,RE),=C'SPACE,'                                               
         LA    RE,6(RE)                                                         
*                                                                               
TL3202   TM    CHGSW,X'20'         JOB CHG                                      
         BZ    TL3203              NO                                           
         MVC   0(4,RE),=C'JOB,'                                                 
         LA    RE,4(RE)                                                         
*                                                                               
TL3203   TM    CHGSW,X'10'         DATE CHG                                     
         BZ    TL3204                                                           
         MVC   0(5,RE),=C'DATE,'                                                
         LA    RE,6(RE)                                                         
*   CHANGE 4/22/88                                                              
TL3204   DS    0H                                                               
*                                                                               
TL3203C  TM    CHGSW,X'80'         IC COMMENT CHANGE                            
         BZ    TL3203F                                                          
         MVC   0(08,RE),=C'IC COMM,'                                            
         LA    RE,8(RE)                                                         
*                                                                               
TL3203F  TM    CHGSW,X'01'         POSITION INSTRUCTIONS CHANGE                 
         BZ    TL3203G                                                          
         MVC   0(09,RE),=C'POS. INS,'                                           
         LA    RE,9(RE)                                                         
*                                                                               
TL3203G  TM    CHGSW,X'02'         RATE CHANGE                                  
         BZ    TL3203BC                                                         
         MVC   0(05,RE),=C'RATE,'                                               
         LA    RE,5(RE)                                                         
TL3203BC BCTR  RE,0                                                             
         CLI   0(RE),C','                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '          BLANK LAST COMMA                             
         MVC   1(8,RE),=C'CHANGE *'                                             
         LA    R3,44(R3)                                                        
         LA    RF,1(RF)                                                         
*                                                                               
TL3205   DS    0H                                                               
         OC    PBDJOB,PBDJOB                                                    
         BZ    TL32A4              NO JOB NUMBER                                
         CLI   QOPT4,C'1'                                                       
         BL    TL32A4              NOT PRINTING COPY OR CAPTION                 
         MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),PAGYKAGY                                                  
         MVC   KEY+2(1),PAGYKMED                                                
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+4(3),PBUYKCLT                                                
         MVC   KEY+7(3),PBUYKPRD                                                
         MVC   KEY+10(6),PBDJOB                                                 
         CLC   PJOBREC(17),KEY                                                  
         BE    TL32A               HAVE JOB REC                                 
         STC   RF,RFSTCSV                                                       
         GOTO1 HIGH                                                             
         ZIC   RF,RFSTCSV                                                       
         CLC   KEY(17),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                JOB NOT ON FILE                              
         LA    R0,PJOBREC                                                       
         ST    R0,AREC                                                          
         STC   RF,RFSTCSV                                                       
         GOTO1 GETPRT                                                           
         ZIC   RF,RFSTCSV                                                       
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
TL32A    MVC   KEY(64),SAVKEYS        RESTORE KEYS                              
         STC   RF,RFSTCSV                                                       
         GOTO1 HIGH                RESTORE FOR SEQ READ                         
         ZIC   RF,RFSTCSV                                                       
*  CHECK TO SEE IF COPY IS OVERRIDDEN IN COMMENTS                               
         STC   RF,RFSTCSV                                                       
         GOTO1 COPYOVER,DMCB,(C'C',=C'COPY'),DMWORK                             
         ZIC   RF,RFSTCSV                                                       
         MVC   0(44,R3),SPACES                                                  
         MVI   36(R3),0                                                         
         MVC   0(6,R3),=C'COPY ='                                               
         MVC   6(17,R3),PJOBCPY                                                 
         CLC   DMWORK(17),SPACES                                                
         BNH   NCOPYW                                                           
         MVC   6(17,R3),DMWORK                                                  
NCOPYW   LA    R3,44(R3)                                                        
         CLI   QOPT4,C'2'                                                       
         BNE   TL32A4              NO CAPTIONS                                  
         STC   RF,RFSTCSV                                                       
         GOTO1 COPYOVER,DMCB,(C'T',=C'COPY'),DMWORK                             
         ZIC   RF,RFSTCSV                                                       
         MVC   0(44,R3),SPACES                                                  
         MVC   0(8,R3),=C'CAPTION='                                             
         MVC   8(25,R3),PJOBCAP1                                                
         CLC   DMWORK(17),SPACES                                                
         BNH   NCAPTA                                                           
         MVC   8(25,R3),DMWORK                                                  
NCAPTA   LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         MVC   0(44,R3),SPACES                                                  
         MVI   45(R3),0                                                         
         MVC   8(25,R3),PJOBCAP2                                                
         CLC   DMWORK+25(17),SPACES                                             
         BNH   NOCAPTB                                                          
         MVC   0(44,R3),SPACES                                                  
         MVI   45(R3),0                                                         
         MVC   8(25,R3),DMWORK+25                                               
NOCAPTB  CLC   0(44,R3),SPACES                                                  
         BNH   TL32A4                                                           
         MVC   0(8,R3),=C'CAPTION='                                             
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         MVI   0(R3),C' '                                                       
*                                                                               
TL32A4   DS    0H                                                               
         BAS   RE,SETLW            SET LEGAL WARNING "FIELDS"                   
*                                                                               
         OC    SAVLWCD(3),SAVLWCD  ANY LEGAL WARNING INFO ?                     
         BZ    LEGLW30             NO                                           
*                                                                               
         MVC   0(44,R3),SPACES                                                  
         MVC   0(14,R3),=C'LEGAL WARNING '                                      
         MVC   14(9,R3),=C'DEFAULT= '                                           
         CLI   SAVLWTYP,C'D'                                                    
         BE    *+10                                                             
         MVC   14(9,R3),=C'OVERRIDE='                                           
         MVC   23(2,R3),SAVLWCD    LW CODE AND QUARTERLY CODE                   
*                                                                               
         CLI   SAVLWCD,C'X'        LEGAL WARNING CODE IS X? (NONE)              
         BNE   LEGLW10                                                          
         MVC   23(4,R3),=C'NONE'   REPLACE LW AND QUARTERLY CODES               
         B     LEGLW20             DONE - BUMP TO NEXT LINE                     
*                                                                               
LEGLW10  CLI   SAVLWQC,C'X'        QUARTERLY CODE IS X?  (NONE)                 
         BNE   LEGLW20             DONE - BUMP TO NEXT LINE                     
         MVC   24(5,R3),=C' NONE'  REPLACE QUARTERLY CODE                       
*****    B     LEGLW20             DONE - BUMP TO NEXT LINE                     
LEGLW20  DS    0H                  BUMP TO NEXT "LINE"                          
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         MVI   0(R3),C' '                                                       
LEGLW30  DS    0H                                                               
*                                                                               
TL32A4D  LA    R6,PPBYOWRK                                                      
         USING PPBYOUTD,R6                                                      
         CLC   PBUYKPRD,=C'ZZZ'    SEE IF ZZZ BUY                               
         BNE   TL32A6                                                           
         OC    PBYOZZZ,SPACES                                                   
         CLI   PBYOZZZ,C' '                                                     
         BE    TL32A6              NO ALLOCATION                                
         CLC   PBYOZZZ+43(3),SPACES   SEE IT WILL FIT ON 1 LINE                 
         BNE   TL32A5                 NO                                        
         MVC   0(44,R3),PBYOZZZ                                                 
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         B     TL32A6                                                           
*                                                                               
TL32A5   LA    R1,42               SET FOR VARIABLE MOVE                        
         LA    R2,PBYOZZZ+43       SCAN BACKWARD FOR ,                          
TL32A5A  CLI   0(R2),C','                                                       
         BE    TL32A5C                                                          
         BCTR  R2,0                                                             
         BCTR  R1,0                                                             
         B     TL32A5A                                                          
*                                                                               
TL32A5C  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),PBYOZZZ     EXECUTED                                     
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         MVC   0(44,R3),SPACES                                                  
         LA    R1,1(R1)                                                         
         LA    R2,PBYOZZZ                                                       
         AR    R2,R1                                                            
         L     R5,=F'49'           49 BECAUSE OF EXECUTE                        
         SR    R5,R1                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),1(R2)       REST OF ALLOCATION LINE                      
*                                  1(R2) TO GET ME PAST ,                       
         LA    R3,44(R3)                                                        
         LA    RF,1(RF)                                                         
*                                                                               
* NOW DO IO COMMENTS                                                            
* NOTE THAT I/O COMMENTS WON'T EXCEED 44 CHARS                                  
* BECAUSE 'IC=' IS DROPPED                                                      
*                                                                               
TL32A6   LA    R2,PBYOCOMS                                                      
         LA    R5,5                FOR BCT                                      
TL32A8   DS    0H                                                               
         CLC   0(44,R2),SPACES                                                  
         BE    TL32A9                                                           
*                                                                               
         CLI   STDCOMSW,C'Y'      SEE IF LISTING STANDARD COMMENTS              
         BNE   TL32A8F              AT END                                      
         CLC   0(4,R2),=C'COM='    CHK FOR STANDARD COMMENT                     
         BNE   TL32A8F                                                          
         STC   RF,RFSTCSV     SAVE LINE COUNT//                                 
         GOTO1 BINSRCH,BSPARS,(1,4(R2))                                         
         ZIC   RF,RFSTCSV                                                       
*                                                                               
TL32A8F  DS    0H                                                               
         MVC   0(44,R3),0(R2)                                                   
         CLI   0(R3),C' '        PREVENT COMMENT FROM HAVING BLANK              
         BNE   *+8               IN FIRST POS// CONTROL PROBLEMS                
         MVI   0(R3),0                                                          
         LA    R3,44(R3)                                                        
         LA    RF,1(RF)                                                         
*                                                                               
TL32A8H  DS    0H                                                               
*                                                                               
TL32A9   LA    R2,47(R2)           NEXT IO COMMENT                              
         BCT   R5,TL32A8                                                        
         DROP  R6                                                               
         LA    R6,PPBYOWRK                                                      
         USING PPBYOUTD,R6                                                      
         LA    R1,PBUYREC                                                       
         ST    R1,PBYOINPT                                                      
         LA    R1,GROSS                                                         
         ST    R1,PBYOVALS                                                      
         MVC   PBYODTCN,DATCON                                                  
         MVI   PBYOCTL,X'00'       FOR ZZZ ALLOCATNS AND IO COMMENTS03          
         MVI   PBYOCLT2,X'80'      FOR POSITION INSTRUCTION                     
*                                                                               
         STC   RF,RFSTCSV                                                       
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
         ZIC   RF,RFSTCSV                                                       
*                                                                               
* NOTE THAT POSITION INSTRUCTIONS WON'T EXCEED 44 CHARS                         
* BECAUSE 'PI=' IS DROPPED                                                      
*                                                                               
         CLC   PBYOCOMS(94),SPACES                                              
         BE    TL32AX                                                           
         MVC   0(44,R3),=CL44'**** POSITION INSTRUCTIONS ****'                  
         LA    R3,44(R3)                                                        
         LA    R5,5                FOR BCT                                      
         LA    R2,PBYOCOMS         POINT TO FIRST POS INSTR                     
TL32B8   DS    0H                                                               
         CLC   0(47,R2),SPACES                                                  
         BE    TL32B9                                                           
         CLI   STDCOMSW,C'Y'       SEE IF LISTING STANDARD COMMENTS             
         BNE   TL32B8F             AT END                                       
         CLC   0(4,R2),=C'COM='    CHK FOR STANDARD COMMENT                     
         BNE   TL32B8F                                                          
         STC   RF,RFSTCSV                                                       
         GOTO1 BINSRCH,BSPARS,(1,4(R2))                                         
         ZIC   RF,RFSTCSV                                                       
*                                                                               
TL32B8F  DS    0H                                                               
         MVC   0(44,R3),0(R2)                                                   
*                                                                               
         CLI   0(R3),C' '          PREVENT COMMENT FROM HAVING BLANK            
         BNE   *+8                 IN FIRST POS// CONTROL PROBLEMS              
         MVI   0(R3),0                                                          
         LA    R3,44(R3)                                                        
         LA    RF,1(RF)                                                         
TL32B8H  DS    0H                                                               
*                                                                               
TL32B9   LA    R2,47(R2)           NEXT IO COMMENT                              
         BCT   R5,TL32B8                                                        
         DROP  R6                                                               
*                                                                               
*                                                                               
*                                                                               
TL32AX   MVI   0(R3),C' '          FORCE TO STOP  PRINTING                      
         CLI   QOPT5,C'Y'          SEE IF DOUBLE SPACING                        
         BNE   TL32AX5                                                          
         XC    0(44,R3),0(R3)                                                   
         LA    RF,1(RF)                                                         
         MVI   44(R3),C' '                                                      
*                                                                               
TL32AX5  DS 0H                                                                  
         STC   RF,BYTE                                                          
         CLC   BYTE,LINENEED                                                    
         BNH   *+10                                                             
         MVC   LINENEED,BYTE                                                    
*                                                                               
*   CHECK TO SEE IF DITTOS ARE NEEDED                                           
*                                                                               
         CLC   OLDPUB,PBUYKPUB                                                  
         BNE   NODITTO                                                          
         BAS   RE,TLCKHD                                                        
*        MVC   DUB,LINENEED                                                     
*        MVC   DUB+1(1),LINE                                                    
*        MVC   DUB+2(1),FORCEHED                                                
*        GOTO1 =V(PRNTBL),DMCB,(5,=C'LNEED'),DUB,C'DUMP',3,=C'1D'               
         CLI   FORCEHED,C'Y'                                                    
         BE    NODITTO                                                          
         MVC   PBNAM1(81),SPACES                                                
         MVC   PBNAM1+4(2),=C''''''          DITTOS                             
         MVI   PBNAM1,0                                                         
NODITTO  DS    0H                                                               
*                                                                               
         LA    R3,PBNAM1                                                        
         LA    R2,COMTAB                                                        
         LA    R6,SPAC1                                                         
         L     R5,ASINSTAB                                                      
         LA    RE,TREPA1-1         TRAFFIC REPS//-1 TO FORCE NE                 
*                                  FIRST TIME THRU LABEL TL33X                  
         CLI   1(RE),255                                                        
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
TL32B    DS    0H                                                               
         CLI   0(R3),C' '                                                       
         BE    TL33XX              NO MORE PUBNAME                              
*                                                                               
         MVC   TLPNAME,0(R3)                                                    
         LA    R3,20(R3)                                                        
         B     TL33                                                             
*                                                                               
TL33XX   DS    0H                                                               
         CLI   QOPT6,C'S'          SHIPPING ADDRESS                             
         BE    TL33XX5                                                          
         CLI   QOPT6,C'Y'                                                       
         BNE   TL33                                                             
TL33XX5  LA    RF,TREPA1           IF FIRST TIME THRU                           
         CR    RE,RF               THEN                                         
         BNL   TL33XXZ                                                          
         MVI   TLPUB,0             FORCE TO PRINT A BLANK LINE                  
         LA    RE,1(RE)                                                         
         B     TL33                                                             
*                                                                               
TL33XXZ  CLI   0(RE),255           STOP PRINTING                                
         BE    TL33                                                             
         CLC   0(20,RE),SPACES                                                  
         BH    *+12                                                             
         LA    RE,30(RE)           TO NEXT PRINTABLE ADDRESS                    
         B     TL33XXZ                                                          
         MVC   TLPUB(30),0(RE)                                                  
         LA    RE,30(RE)                                                        
*                                                                               
TL33     DS    0H                                                               
         CLI   0(R6),C' '          NO MORE SPACE DESC                           
         BE    TL33B                                                            
         MVC   TLSPACE,0(R6)                                                    
         LA    R6,20(R6)                                                        
         CLI   0(R2),X'40'                                                      
         BE    TL33F                                                            
         CLC   25(19,R2),SPACES    SEE IF COMMENT WILL FIT ON SAME LINE         
         BH    TL33F               NO                                           
         CLI   TLIDAT,C' '         IF NOTHING ALREADY THERE                     
         BNE   TL33F                                                            
         MVC   TLIDAT(25),0(R2)                                                 
         LA    R2,44(R2)                                                        
         B     TL33F                                                            
*                                                                               
*                                                                               
*                                                                               
TL33B    DS    0H                                                               
         CLI   0(R2),C' '                                                       
         BE    TL33F                                                            
         CLI   TLIDAT,C' '         SEE IF SOMETHING ALREADY THERE               
         BNE   TL33F               YES                                          
         MVC   TLIDAT(44),0(R2)                                                 
         LA    R2,44(R2)                                                        
*                                                                               
TL33F    DS    0H                                                               
         CLI   0(R5),C' '                                                       
         BE    TL34                NO MORE IO'S                                 
         MVC   TLIODAT(34),0(R5)                                                
         LA    R5,34(R5)                                                        
*                                                                               
TL34     DS    0H                                                               
         CLC   P,SPACES                                                         
         BE    TLOUTX              NO MORE TO PRINT                             
         ST    RE,SAVERE                                                        
*                                                                               
* INCLUDE NUMBER OF LINES NEEDED TO PRINT REP ADDRESS IN LINENEED               
*                                                                               
         CLC  REPLINED,LINENEED    IF REP IS LARGER THEN USE REPLINED           
         BNH  *+10                                                              
         MVC  LINENEED,REPLINED                                                 
         MVI  REPLINED,0           INITIALIZE                                   
*                                                                               
         BAS   RE,TLPRT                                                         
         L     RE,SAVERE                                                        
         B     TL32B                                                            
*                                                                               
TLOUTX   DS    0H                                                               
         MVC   OLDKEY,PBUYKEY                                                   
         MVC   OLDJOB,PBDJOB                                                    
         B     XIT                                                              
*                                                                               
*                                                                               
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCOD,0(R2)                                                      
         BER   RE                                                               
         B     NEXTEL+2                                                         
NEXTEL2  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
TLEDL    DS    0H                                                               
         EDIT  (P8,DUB),(6,(R5)),ALIGN=LEFT                                     
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
ENDEST   NTR1                                                                   
         CLC   QEST,SPACES                                                      
         BE    ENDESTX                                                          
         CLC   QESTEND,SPACES                                                   
         BNE   ENDESTX                                                          
         LA    R2,ESTTOTS                                                       
         LA    R3,=CL15'ESTIMATE TOTALS'                                        
         BAS   RE,DOTLTOT                **  TLTOT  **                          
ENDESTX  B     XIT                                                              
*                                                                               
***********************************************************************         
*                                                                               
ENDPRD   NTR1                                                                   
         CLC   QPRODUCT,SPACES                                                  
         BE    ENDPRDX                                                          
         LA    R2,PRDTOTS                                                       
         LA    R3,=CL15'PRODUCT TOTALS'                                         
         BAS   RE,DOTLTOT                **  TLTOT  **                          
ENDPRDX  B     XIT                                                              
*                                                                               
***********************************************************************         
*                                                                               
CLRSMID  XC    SAVMID,SAVMID                                                    
         MVI   SAVMID,255                                                       
         MVI   SAVMID+1,255                                                     
         MVI   FORCEMID,C'N'                                                    
         BR    RE                                                               
*                                                                               
***********************************************************************         
*                                                                               
ENDCLT   NTR1                                                                   
         LA    R2,CLTTOTS                                                       
         LA    R3,=CL15'CLIENT TOTALS'                                          
         BAS   RE,DOTLTOT                **  TLTOT  **                          
         B     XIT                                                              
*                                                                               
***********************************************************************         
*                                                                               
DOTLTOT  NTR1                                                                   
         GOTO1 =A(COMMON),DMCB,3      **  TLTOT  **                             
         BAS   RE,TLPRT                                                         
         B     XIT                                                              
*                                                                               
***********************************************************************         
*                                                                               
         DC    X'FF'                                                            
         ORG   *-1                                                              
SAVMIDSW DC    X'0'                                                             
QQOPT8   DC    X'0'                                                             
QQOPT9   DC    X'0'                                                             
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
DOMIDS   NTR1                                                                   
         GOTO1  =V(COMMON),DMCB,2     DOMIDS                                    
         B      XIT                                                             
         DROP  R3                                                               
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'TLPRT     PRINT RECORDS'                                        
         SPACE 2                                                                
*                                  PRINTING                                     
TLPRT    NTR1                                                                   
         SPACE 2                                                                
         BAS   RE,TLCKHD                                                        
         BAS   RE,DOMIDS                                                        
*                                                                               
TLPRT2   DS    0H                                                               
         CLI   FORCEHED,C'Y'                                                    
         BNE   TLPRT2A                                                          
         GOTO1  =V(COMMON),DMCB,1     TLHEAD                                    
TLPRT2A  MVC   HEAD9+66(8),HEAD966     MOVE TITLE                               
         GOTO1 REPORT                                                           
*                                                                               
         MVI   LINENEED,0                                                       
         B     XIT                                                              
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'TLHEAD PRINT HEADLINES'                                         
*                                                                               
TLCKHD   NTR1                                                                   
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         IC    R0,LINE                                                          
         IC    RF,LINENEED                                                      
         AR    R0,RF                                                            
         STC   R0,BYTE                                                          
******** GOTO1 =V(PRNTBL),DMCB,(5,=C'LNEED'),LINENEED,C'DUMP',1,=C'1D'          
         CLC   BYTE,MAXLINES                                                    
         BL    XIT                                                              
         MVI   FORCEHED,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* SET UP LEGAL WARNING DEFAULT AND OVERRIDE DATA IN WORKING STORAGE   *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
SETLW    NTR1                                                                   
         XC    SAVLWCD(3),SAVLWCD                                               
         LA    R2,PBUYREC+33       LOOK FOR LEGAL WARN OVERRIDE                 
         MVI   ELCOD,X'94'                                                      
         BAS   RE,NEXTEL           LEGAL WARNING ELEM FOUND IN BUY?             
         BNE   SETLW20             NO, CHECK IN PRODUCT RECORD                  
*                                                                               
         MVC   SAVLWCD(2),2(R2)    LW CODE AND QUARTERLY CODE                   
         MVI   SAVLWTYP,C'O'       LEGAL WARNING "OVERRIDE" FROM BUYREC         
         B     SETLWXT             DONE                                         
*                                                                               
*                                                                               
SETLW20  DS    0H                                                               
         BAS   RE,GETPRD                                                        
         LA    R2,PPRDREC+33                                                    
         MVI   ELCOD,X'40'         LEGAL WARNING ELEM CODE IN PRD               
         BAS   RE,NEXTEL                                                        
         BNE   SETLWXT             NO LW CODES FOUND IN THIS ROUTINE            
*                                                                               
         MVI   SAVLWTYP,C'D'       LEGAL WARNING "DEFAULT" FROM PRDREC          
*                                                                               
         CLI   PBUYKDAT+1,X'03'    MONTH IS GREATER THAN MARCH ?                
         BH    SETLW22             YES                                          
         MVC   SAVLWCD,2(R2)                                                    
         MVI   SAVLWQC,C'1'        QUARTER 1                                    
         B     SETLWXT             DONE                                         
SETLW22  CLI   PBUYKDAT+1,X'06'    MONTH IS GREATER THAN JUNE ?                 
         BH    SETLW24             YES                                          
         MVC   SAVLWCD,3(R2)                                                    
         MVI   SAVLWQC,C'2'        QUARTER 2                                    
         B     SETLWXT             DONE                                         
SETLW24  CLI   PBUYKDAT+1,X'09'    MONTH IS GREATER THAN SEPTEMBER ?            
         BH    SETLW26             YES                                          
         MVC   SAVLWCD,4(R2)                                                    
         MVI   SAVLWQC,C'3'        QUARTER 3                                    
         B     SETLWXT             DONE                                         
SETLW26  DS    0H                  MONTH IS GREATER THAN SEPTEMBER              
         MVC   SAVLWCD,5(R2)                                                    
         MVI   SAVLWQC,C'4'        QUARTER 4                                    
*****    B     SETLWXT             DONE                                         
SETLWXT  DS    0H                                                               
         LA    R0,PBUYREC          RESTORE AREC                                 
         ST    R0,AREC                                                          
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'GETCLT  GET CLIENT'                                             
GETCLT   NTR1                                                                   
         CLI   PBUYKEY,255                                                      
         BE    XIT                                                              
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(7),PBUYKEY                                                   
         MVI   KEY+3,2                                                          
*                                                                               
         CLC   PCLTREC(7),KEY      SEE IF CLIENT ALREADY THERE                  
         BNE   GETCLT5                                                          
         MVC   KEY(64),WORK                                                     
         B     XIT                                                              
*                                                                               
GETCLT5  GOTO1 READ                                                             
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                  GET P72A PROFILE                             
         XC    PFWORK,PFWORK                                                    
         MVC   PFWORK(4),=C'P72A'                                               
         NI    PFWORK,X'BF'        MAKE SYSTEM LOWER CASE                       
         MVC   PFWORK+4(2),PCLTKAGY                                             
         MVC   PFWORK+6(1),PCLTKMED                                             
         MVC   PFWORK+7(3),PCLTKCLT                                             
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   PFWORK+10,C'*'                                                   
         MVC   PFWORK+11(1),PCLTOFF                                             
         GOTO1 GETPROF,DMCB,PFWORK,P72APROF,DATAMGR                             
*                                                                               
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
         B     XIT                                                              
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'GETPRD  GET PRODUCT'                                            
GETPRD   NTR1                                                                   
         CLI   PBUYKEY,255                                                      
         BE    GETPRDX                                                          
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVI   KEY+3,6                                                          
         LA    R0,PPRDREC                                                       
         CLC   PPRDREC(10),KEY          SEE IF ALREADY THERE                    
         BNE   GETPRD5                                                          
         MVC   KEY(64),WORK                                                     
         B     GETPRDX                                                          
*                                                                               
GETPRD5  GOTO1 READ                                                             
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         CLI   RUNSW,0                                                          
         BE    GETPRDX                                                          
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
GETPRDX  XIT1                                                                   
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'GETEST  GET ESTIMATE'                                           
GGETEST  NTR1                                                                   
         CLI   PBUYKEY,255                                                      
         BE    GETESTX                                                          
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVC   KEY+10(2),PBUYKEST                                               
         MVI   KEY+3,7                                                          
         LA    R0,PESTREC                                                       
         CLC   PESTREC(12),KEY           SEE IF ALREADY THERE                   
         BNE   GETPRES             COMMON READ FOR PRD AND EST                  
         MVC   KEY(64),WORK                                                     
         B     GETESTX                                                          
*                                                                               
GETPRES  GOTO1 READ                                                             
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         CLI   RUNSW,0                                                          
         BE    GETESTX                                                          
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
GETESTX  XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'GETJOBR GET JOB REC'                                            
GETJOBR  NTR1                                                                   
         CLI   PBUYKEY,255                                                      
         BE    GETJOBRX                                                         
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVC   KEY+10(6),PBDJOB                                                 
         MVI   KEY+3,X'15'                                                      
         CLC   PJOBREC(17),KEY           SEE IF ALREADY THERE                   
         BNE   GETJOBRR                                                         
         MVC   KEY(64),WORK                                                     
         B     GETJOBRX                                                         
*                                                                               
GETJOBRR GOTO1 READ                                                             
         LA    R0,PJOBREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         CLI   RUNSW,0                                                          
         BE    GETJOBRX                                                         
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
GETJOBRX DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'TLIN    PROCESS INPUT'                                          
*                                                                               
TLIN     CSECT                                                                  
         NMOD1 0,TLIN                                                           
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP77WRKD,R8                                                      
*                                                                               
         OC    KEY,KEY                                                          
         BNZ   TLIN10                                                           
*                                  FIRST TIME                                   
         XC    BSTART,BSTART                                                    
         MVC   BEND,=3X'FF'                                                     
         CLC   QSTART(2),=C'ES'                                                 
         BNE   TLIN1                                                            
         MVC   QSTART(2),SPACES    RESET TO SPACES                              
         CLC   QEST,=C'ALL'                                                     
         BE    TLIN1                                                            
         CLC   QEST,SPACES                                                      
         BE    TLIN1                                                            
*                                  MUST TRY TO READ EST FOR DATES               
         MVC   KEY(3),RCSVAGY                                                   
         MVI   KEY+3,X'07'                                                      
         MVC   KEY+4(3),QCLIENT                                                 
         MVC   KEY+7(3),QPRODUCT                                                
         PACK  DUB,QEST                                                         
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
         MVC   KEY+10(2),HALF                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   TLIN1               EST NOT READABLE OR NOT FOUND                
*                                  LEAVE QSTART BLANK                           
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         MVC   QSTART(12),PESTST   GET DATES FROM ESTIMATE                      
TLIN1    CLI   QSTART,C' '                                                      
         BE    TLIN2                                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
*                                                                               
TLIN2    DS    0H                                                               
         CLI   QEND,C' '                                                        
         BE    TLIN4                                                            
         GOTO1 DATCON,DMCB,(0,QEND),(3,BEND)                                    
*                                                                               
TLIN4    DS    0H                                                               
*                                                                               
         XC    KEY,KEY             RECLEAR KEY - MAY HAVE EST                   
         MVC   KEY(3),RCSVAGY                                                   
         MVI   KEY+3,X'20'                                                      
         OC    KEY+3(1),RUNSW                                                   
         CLC   QCLIENT,SPACES                                                   
         BE    TLIN6                                                            
         CLC   QCLIENT,=C'ALL'                                                  
         BE    TLIN6                                                            
         CLI   QCLIENT,C'*'                                                     
         BE    TLIN6                                                            
         CLI   QCLIENT,C'&&'       GROUP REQUEST                                
         BE    TLIN6                                                            
         MVC   KEY+4(3),QCLIENT                                                 
*                                                                               
TLIN5    DS    0H                                                               
*                                                                               
         CLC   QPRODUCT,SPACES                                                  
         BE    TLIN6                                                            
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    TLIN6                                                            
         LA    R1,KEY+7                                                         
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+13                                                        
         MVC   0(3,R1),QPRODUCT                                                 
*                                                                               
TLIN6    DS    0H                                                               
*                                                                               
         XC    MYBPUB,MYBPUB                                                    
         CLI   QPUB,C'0'                                                        
         BL    TLIN8                                                            
         MVC   WORK(11),QPUB                                                    
         CLC   QPUB+8(3),=C'ZZZ'   SEE IF DOING ALL ZONES + EDTS                
         BNE   *+10                                                             
         MVC   WORK+8(3),SPACES                                                 
         GOTO1 PUBVAL,DMCB,(0,WORK),MYBPUB                                      
*                                                                               
         LA    R1,KEY+10                                                        
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+7                                                         
         MVC   0(6,R1),MYBPUB                                                   
*                                                                               
TLIN8    DS    0H                                                               
*                                                                               
         GOTO1 HIGH                                                             
         B     TLIN10A                                                          
*                                                                               
TLIN10   DS    0H                                                               
*                                                                               
         GOTO1 SEQ                                                              
*                                                                               
TLIN10A  DS    0H                                                               
*                                                                               
         CLC   KEY(4),KEYSAVE      A/M/R                                        
         BNE   TLIN90                                                           
*                               SEE IF PRODUCT GROUP REQUEST                    
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         CLI   Q2USER+24,X'C0'  REALLY SCHEME CODE IN 2ND CARD                  
         BNH   TLIN10AK                                                         
         DROP  RF                                                               
*                                                                               
         BAS   RE,FLTPGRP       FILTER ON GROUP                                 
         BNE   TLIN10           SKIP IF IN WRONG GROUP                          
*                                                                               
TLIN10AK DS    0H                                                               
*                                                                               
         CLC   QEST,=C'ALL'                                                     
         BE    *+14                                                             
         CLC   QEST,=C'   '                                                     
         BNE   TLIN10AP                                                         
         CLC   QESTEND(3),=C'   '                                               
         BE    TLIN10AP                                                         
*                                                                               
         LA    R0,PBUYREC          NEED TO GET PBUYREC FOR KEY INFO             
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         BAS   RE,CKESTFLT         CHECKING FOR ESTIMATE FILTER                 
         CLI   ESTFLTSW,C'N'                                                    
         BE    TLIN10                                                           
*                                                                               
TLIN10AP DS    0H                                                               
*                                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         CLI   QOPT7,C'Y'          INCLUDE TEST BUYS ?                          
         BE    TLIN10AR            YES - TO STEWARD TESTING                     
         CLI   PBDBFD,C'T'         TEST BUY ?                                   
         BE    TLIN10              YES - SKIP                                   
         TM    PBDSTAT,X'20'       "NO TRAFFIC" BUY ?                           
         BO    TLIN10              YES - SKIP                                   
         B     TLIN10B             CONTINUE                                     
*                                                                               
TLIN10AR DS    0H                  STEWARD TESTING                              
         CLI   QQOPT9,C' '         STEWARD REQUEST ?                            
         BH    TLIN10AT            YES                                          
         TM    PBDSTAT2,X'40'      NO - STEWARD BUY ?                           
         BO    TLIN10              YES - SKIP                                   
         B     TLIN10B             CONTINUE                                     
*                                                                               
TLIN10AT DS    0H                  STEWARD REQUEST                              
         CLI   QQOPT9,C'O'         STEWARD BUYS ONLY ?                          
         BE    TLIN10AX            YES                                          
*                                                                               
TLIN10AV DS    0H                  MUST BE LIVE PLUS STEWARD                    
         CLI   PBDBFD,C'T'         TEST BUY ?                                   
         BNE   TLIN10B             NO - CONTINUE                                
*                                                                               
TLIN10AX DS    0H                                                               
         TM    PBDSTAT2,X'40'      STEWARD BUY ?                                
         BNO   TLIN10              NO - SKIP                                    
*                                                                               
TLIN10B  CLI   KEY+25,X'FF'                                                     
         BE    TLIN10                                                           
         CLC   KEY(4),KEYSAVE      A/M/R                                        
         BNE   TLIN90                                                           
         CLC   QCLIENT,SPACES                                                   
         BE    TLIN12                                                           
         CLC   QCLIENT,=C'ALL'                                                  
         BE    TLIN12                                                           
         CLI   QCLIENT,C'*'        TEST OFFICE REQ                              
         BNE   TLIN10B5            CHK FOR GROUP REQUEST                        
         MVC   PBUYKEY,KEY                                                      
         BAS   RE,IGETCLT                                                       
         CLC   QCLIENT+1(1),PCLTOFF                                             
         BE    TLIN12                                                           
         B     TLIN10BX                                                         
*                                                                               
TLIN10B5 CLI   QCLIENT,C'&&'       TEST GROUP REQ                               
         BNE   TLIN10C             NO                                           
         MVC   PBUYKEY,KEY                                                      
         BAS   RE,IGETCLT                                                       
         CLC   QCLIENT+1(1),PCLTBLGP                                            
         BE    TLIN12                                                           
*                                                                               
TLIN10BX IC    RF,KEY+6            SKIP TO NEXT CLIENT                          
         LA    RF,1(RF)                                                         
         STC   RF,KEY+6                                                         
         XC    KEY+7(18),KEY+7                                                  
         B     TLIN5                                                            
*                                                                               
TLIN10C  DS    0H                                                               
*                                                                               
         CLI   QCLIENT,C'$'        TEST OFFILST LIST REQUEST                    
         BNE   TLIN10D             NO                                           
         MVC   PBUYKEY,KEY                                                      
         BAS   RE,IGETCLT                                                       
         CLI   PCLTOFF,C' '        MUST HAVE AN OFFICE                          
         BNH   TLIN10BX            SKIP TO NEXT CLIENT                          
*                                                                               
         XC    DUB,DUB                                                          
         MVI   DUB,C'P'            PRINT SYSTEM                                 
         MVC   DUB+2(2),QCLIENT    OFFICE GROUP                                 
         MVC   DUB+5(2),QAGENCY    ALPHA AGENCY                                 
         MVC   DUB+7(1),PCLTOFF    MEDIA OFFICE CODE                            
*                                                                               
         GOTO1 VOFFICER,DMCB,DUB,VCOMFACS                                       
*                                                                               
         CLI   0(R1),0                                                          
         BE    TLIN12                                                           
         B     TLIN10BX            SKIP TO NEXT CLIENT                          
*                                                                               
TLIN10D  DS    0H                                                               
*                                                                               
         CLC   QCLIENT,KEY+4                                                    
         BNE   TLIN90                                                           
*                                                                               
         CLC   QPRODUCT,SPACES                                                  
         BE    TLIN12                                                           
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    TLIN12                                                           
         LA    R1,KEY+7                                                         
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+13                                                        
         CLC   QPRODUCT,0(R1)                                                   
         BNE   TLIN90                                                           
*                                                                               
TLIN12   DS    0H                                                               
*                                                                               
         OC    KEY+21(3),KEY+21    PASSIVE POINTER                              
         BNZ   TLIN10                                                           
*                                                                               
         LA    R1,KEY+7                                                         
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+13                                                        
*                                                                               
         CLI   0(R1),C'*'           SKIP OTHER AGY DATA                         
         BE    TLIN10                                                           
*                                                                               
         OC    MYBPUB,MYBPUB                                                    
         BZ    TLIN14                                                           
         LA    R1,KEY+10                                                        
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+7                                                         
         LA    RF,5                FOR EXECUTE                                  
         CLC   QPUB+8(3),=C'ZZZ'   ALL ZONES AND EDTS                           
         BNE   *+8                                                              
         LA    RF,3                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   MYBPUB(0),0(R1)                                                  
         BNE   TLIN10                                                           
*                                                                               
TLIN14   DS    0H                                                               
*                                                                               
         CLC   QEST,=C'ALL'                                                     
         BE    TLIN14C                                                          
         CLC   QEST,SPACES                                                      
         BE    TLIN14C                                                          
         MVC   HALF,KEY+19                                                      
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(3),DUB                                                      
         CLC   WORK(3),QEST                                                     
         BE    TLIN14C                                                          
         BL    TLIN10                                                           
         CLC   QESTEND,SPACES                                                   
         BE    TLIN10                                                           
         CLC   WORK(3),QESTEND                                                  
         BH    TLIN10                                                           
*                                                                               
TLIN14C  DS    0H                                                               
*                                                                               
         B     TLIN15                  SKIP READING OF EST                      
*                                      TEST STATUS NOW IN PBUYREC               
TLIN15   DS    0H                                                               
*                                                                               
         CLI   QBPDATE,C'D'                                                     
         BNE   TLIN16                                                           
         CLC   BSTART,KEY+16                                                    
         BH    TLIN10                                                           
         CLC   BEND,KEY+16                                                      
         BL    TLIN10                                                           
         CLI   RUNSW,0                                                          
         BNE   TLIN16                                                           
         CLC   QJOB,SPACES                                                      
         BNE   TLIN16                                                           
         CLI   QOPT1,C'A'                                                       
         BNE   TLIN16                                                           
         TM    PBUYCNTL,X'80'                                                   
         BNZ   TLIN16                                                           
*                                  DONT NEED REC NOW                            
         B     TLIN20                                                           
*                                                                               
TLIN16   DS    0H                                                               
*                                                                               
         MVI   CHGSW,0                                                          
*                                                                               
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
         CLI   QOPT7,C'Y'          INCLUDE TEST BUYS ?                          
         BE    TLIN16D             YES - TO STEWARD TESTING                     
         CLI   PBDBFD,C'T'         TEST BUY ?                                   
         BE    TLIN10              YES - SKIP                                   
         TM    PBDSTAT,X'20'       "NO TRAFFIC" BUY ?                           
         BO    TLIN10              YES - SKIP                                   
         B     TLIN17              CONTINUE                                     
*                                                                               
TLIN16D  DS    0H                  STEWARD TESTING                              
         CLI   QQOPT9,C' '         STEWARD REQUEST ?                            
         BH    TLIN16H             YES                                          
         TM    PBDSTAT2,X'40'      NO - STEWARD BUY ?                           
         BO    TLIN10              YES - SKIP                                   
         B     TLIN17              CONTINUE                                     
*                                                                               
TLIN16H  DS    0H                  STEWARD REQUEST                              
         CLI   QQOPT9,C'O'         STEWARD BUYS ONLY ?                          
         BE    TLIN16P             YES                                          
*                                                                               
TLIN16L  DS    0H                  MUST BE LIVE PLUS STEWARD                    
         CLI   PBDBFD,C'T'         TEST BUY ?                                   
         BNE   TLIN17              NO - CONTINUE                                
*                                                                               
TLIN16P  DS    0H                  STEWARD BUYS ONLY REQUESTED                  
         TM    PBDSTAT2,X'40'      STEWARD BUY ?                                
         BNO   TLIN10              NO - SKIP                                    
*                                                                               
TLIN17   CLC   QJOB,SPACES                                                      
         BE    TLIN17C                                                          
         CLC   QJOB,=C'ALL   '                                                  
         BE    TLIN17C                                                          
         CLC   QJOB,PBDJOB                                                      
         BE    TLIN17C                                                          
         CLC   =C'NONE',QJOB                                                    
         BNE   TLIN10              SKIP                                         
         OC    PBDJOB,PBDJOB                                                    
         BNZ   TLIN10              SKIP                                         
*                                                                               
TLIN17C  DS    0H                  FIND MOST RECENT I/O (IF ANY)                
         LA    R2,PBUYREC+33                                                    
         MVI   ELCOD,X'70'                                                      
         MVI   IOELCOD,X'70'                                                    
         XC    LASTIODX,LASTIODX   CLEAR MOST RECENT I/O DATE                   
*                                                                               
TLIN17F  DS    0H                  LOOK FOR "REGULAR" I/O                       
         BAS   RE,INEXTEL                                                       
         BNE   TLIN17J                                                          
         USING PIOELEM,R2                                                       
         MVC   LASTIODX,PIODATE    SAVE DATE OF MOST RECENT I/O                 
         B     TLIN17F             LOOK FOR MORE                                
         DROP  R2                                                               
*                                                                               
TLIN17J  DS    0H                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCOD,X'71'                                                      
*                                                                               
TLIN17M  DS    0H                  LOOK FOR WEB I/O                             
         BAS   RE,INEXTEL                                                       
         BNE   TLIN17X                                                          
         USING PWIOELEM,R2                                                      
         CLC   PWIODATE,LASTIODX   PREVIOUS TO "REGULAR" I/O ?                  
         BL    TLIN17M             YES - IGNORE                                 
         MVI   IOELCOD,X'71'     USE WEB I/O ELEM FOR "CHANGE" TESTING          
         DROP  R2                    (SEE TLIN18A4 AND TLIN18B)                 
*                                                                               
TLIN17X  DS    0H                                                               
*                                                                               
TLIN18   DS    0H                                                               
*                                                                               
         CLI   QBPDATE,C'D'                                                     
         BE    TLIN18B                                                          
         LA    R2,PBDCDATE                                                      
         CLI   QBPDATE,C'C'                                                     
         BE    TLIN18A                                                          
*  MAT                                                                          
         LA    R2,PBDMDATE                  MATERIALS CLOSING DATE              
         CLI   QBPDATE,C'M'                                                     
         BE    TLIN18A                                                          
*  MAT                                                                          
         LA    R2,PBDBDATE                                                      
         CLI   QBPDATE,C'B'                                                     
         BE    TLIN18A                                                          
         LA    R2,PBDPDATE                                                      
         CLI   QBPDATE,C'P'                                                     
         BE    TLIN18A                                                          
         LA    R2,PBDSDATE                                                      
         CLI   QBPDATE,C'S'                                                     
         BE    TLIN18A                                                          
         CLI   QBPDATE,C'I'                                                     
         BE    TLIN18A4                                                         
         B     TLIN18A1                                                         
*                                                                               
TLIN18A  DS    0H                                                               
*                                                                               
         CLI   0(R2),0                                                          
         BNE   *+8                                                              
TLIN18A1 DS    0X                  AVOID POSSIBLE ALIGN PROBLEM                 
         LA    R2,PBUYKDAT         OR USE DATE                                  
         CLC   BSTART,0(R2)                                                     
         BH    TLIN10                                                           
         CLC   BEND,0(R2)                                                       
         BL    TLIN10                                                           
         B     TLIN18B                                                          
*                                                                               
TLIN18A4 DS    0H                  DATE OF INSERTION ORDER                      
*                                                                               
         LA    R2,PBUYREC+33                                                    
*NOP*    MVI   ELCOD,X'70'                                                      
         MVC   ELCOD,IOELCOD       IOELCOD SET IN TLIN17C TO TLIN17X            
*                                                                               
TLIN18A5 DS    0H                                                               
*                                                                               
         BAS   RE,INEXTEL                                                       
         BNE   TLIN10                                                           
         USING PIOELEM,R2                                                       
         CLI   PIODATE,0                                                        
         BE    TLIN18A5                                                         
         CLC   PIODATE,BSTART                                                   
         BL    TLIN18A5                                                         
         CLC   PIODATE,BEND                                                     
         BH    TLIN18A5                                                         
*                                                                               
TLIN18B  DS    0H                                                               
*                                                                               
         XC    SAVR2,SAVR2                                                      
         XC    LASTIOD,LASTIOD                                                  
         MVI   BYTE,C'N'                                                        
         LA    R2,PBUYREC+33                                                    
*NOP*    MVI   ELCOD,X'70'                                                      
         MVC   ELCOD,IOELCOD       IOELCOD SET IN TLIN17C TO TLIN17X            
*                                                                               
TLIN18D  DS    0H                                                               
*                                                                               
         BAS   RE,INEXTEL                                                       
         BNE   TLIN18E                                                          
         USING PIOELEM,R2                                                       
         OC    PIODATE,PIODATE                                                  
         BZ    TLIN18D                                                          
         MVI   BYTE,C'O'                                                        
         CLI   PIOTYP,C'D'                                                      
         BNE   *+8                                                              
         MVI   BYTE,C'C'           CANCELLED                                    
         ST    R2,SAVR2            SAVE A(LAST ELEM)                            
*                                                                               
*   CONVERT AND SAVE LASTIO DATE IN LASTIOD // THIS WILL BE USED TO             
*   DETERMINE THERE WAS AN INSERTION ORDER CHANGE                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,PIODATE),(2,LASTIOD)                              
         B     TLIN18D                                                          
*                                                                               
TLIN18E  DS    0H                                                               
*                                                                               
         MVC   FULL(1),QOPT1                                                    
         TM    PBUYCNTL,X'80'                                                   
         BNZ   TLIN18F                                                          
*                                  NOT DELETED                                  
         CLI   FULL,C'A'                                                        
*                                                                               
         BE    TLIN19                                                           
         CLC   BYTE,FULL                                                        
         BE    TLIN19                                                           
         CLI   FULL,C'N'           IF LISTING UNORDERED CHECK                   
         BNE   TLIN10              CHANGE STATUS                                
*                                                                               
         OC    LASTIOD,LASTIOD     CHK FOR LAST IO DATE                         
         BZ    TLIN18E3                                                         
         LA    R2,PBUYREC+33                                                    
         MVI   ELCOD,X'24'         SEE IF PI COMM CHANGED SINCE LASTIO          
*                                                                               
TLIN18E1 DS    0H                                                               
*                                                                               
         BAS   RE,INEXTEL                                                       
         BNE   TLIN18E3                                                         
         USING PCHGELEM,R2                                                      
         CLC   PCHGDAT,LASTIOD                                                  
         BNH   TLIN18E1            WAS BL                                       
         TM    PCHGIND1,X'01'      INSERTION ORDER COMMENT CHANGE               
         BZ    *+8                                                              
         OI    CHGSW,X'80'                                                      
         TM    PCHGIND3,X'01'      POSITION INSTRUCTIONS CHANGE                 
         BZ    *+8                                                              
         OI    CHGSW,X'01'                                                      
*                                                                               
         CLI   P72APROF+14,C'Y'    RATE CHG TRIGGER                             
         BNE   TLIN18E1            CONTINUE LOOKING                             
         TM    PCHGIND1,X'40'                                                   
         BZ    *+8                                                              
         OI    CHGSW,X'02'                                                      
         B     TLIN18E1           CONTINUE LOOKING                              
*                                                                               
TLIN18E3 DS    0H                                                               
*                                                                               
         DROP  R2                                                               
         USING PIOELEM,R2                                                       
*                                                                               
         L     R2,SAVR2            RESTPRE LAST ELEM                            
         CLC   PIOIDATE,PBUYKDAT   INS DATE                                     
         BE    *+8                                                              
         OI    CHGSW,X'10'         DATE CHG                                     
         CLC   PIOJOB,PBDJOB       JOB NO.                                      
         BE    *+8                                                              
         OI    CHGSW,X'20'         JOB CHANGE                                   
         CLI   QMEDIA,C'N'                                                      
         BE    TLIN18E8                                                         
*                                                                               
         CLI   PBDSPACE,X'FF'      OUTDOOR                                      
         BNE   TLIN18E4                                                         
         CLC   PIOSPACE,PBDSPACE                                                
         BE    *+8                                                              
         OI    CHGSW,X'40'         SPACE CHG                                    
         B     TLIN18EX                                                         
*                                                                               
TLIN18E4 DS    0H                  MAGE SPACE                                   
*                                                                               
         MVC   WORK(17),PBDSPACE                                                
         MVC   WORK+17(17),PIOSPACE                                             
         OC    WORK(34),SPACES                                                  
         CLC   WORK(17),WORK+17                                                 
         BE    *+8                                                              
         OI    CHGSW,X'40'         SPACE CHG                                    
         B     TLIN18EX                                                         
*                                                                               
TLIN18E8 DS    0H                  NEWSPAPERS                                   
*                                                                               
         MVC   WORK(6),PIOSAU                                                   
         MVC   WORK+6(6),PBDSPACE                                               
         OC    WORK(12),SPACES                                                  
         CLC   WORK(6),WORK+6                                                   
         BE    *+8                                                              
         OI    CHGSW,X'40'         SPACE CHG                                    
         CP    PIOUNITS,PBDUNITS                                                
         BE    *+8                                                              
         OI    CHGSW,X'40'         SPACE CHG                                    
         CP    PIOCLMS,PBDCLMS                                                  
         BE    *+8                                                              
         OI    CHGSW,X'40'         SPACE CHG                                    
         CLC   PIOUIND,PBDUIND                                                  
         BE    *+8                                                              
         OI    CHGSW,X'40'         SPACE CHG                                    
         CLC   PBDCL,PIOPRM                                                     
         BE    *+8                                                              
         OI    CHGSW,X'40'         SPACE CHG                                    
*                                                                               
TLIN18EX CLI   CHGSW,0                                                          
         BE    TLIN10              NO CHANGEES                                  
         CLI   CHGSW,X'80'         OR ONLY IC= COMMENT CHG                      
         BE    TLIN10              NO CHANGEES                                  
*                                                                               
TLIN18F  DS    0H                  DELETED BUYS                                 
*                                                                               
         CLI   BYTE,C'N'           BYPASS UNORDED                               
         BE    TLIN10                                                           
         CLI   FULL,C'N'           LIST 'UNCANCELLED'                           
         BNE   TLIN19                                                           
         CLI   BYTE,C'C'                                                        
         BE    TLIN10                                                           
         DROP  R2                                                               
*                                                                               
TLIN19   DS    0H                                                               
*                                                                               
TLIN20   DS    0H                                                               
*                                                                               
         CLI   RUNSW,0                                                          
         BNE   TLINX               NO SORT                                      
         L     R3,SAVPARS+4                                                     
         USING SORTRECD,R3                                                      
         XC    SORTREC,SORTREC                                                  
         MVC   SCHGSW,CHGSW        SET CHGSW IN SORT REC                        
         CLC   QCLIENT,SPACES                                                   
         BE    TLIN21N                                                          
*                                                                               
         CLI   QCLIENT,C'$'                OFFICE LIST REQUEST                  
         BNE   *+10                                                             
         MVC   SKOFF,PCLTOFF                                                    
*                                                                               
         MVC   SKCLT,KEY+4                                                      
*                                                                               
TLIN21N  CLC   QPRODUCT,SPACES                                                  
         BE    TLIN22                                                           
         LA    R1,KEY+7                                                         
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+13                                                        
         MVC   SKPRD,0(R1)                                                      
*                                                                               
TLIN22   DS    0H                                                               
*                                                                               
         CLC   QJOB,SPACES                                                      
         BE    *+10                                                             
         MVC   SKJOB,PBDJOB                                                     
*                                                                               
         CLC   QEST,=C'ALL'                                                     
         BNE   TLIN23                                                           
         MVC   SKEST,KEY+19                                                     
*                                                                               
TLIN23   DS    0H                                                               
*                                                                               
         CLC   QPUB,SPACES                                                      
         BE    TLIN24                                                           
         LA    R1,KEY+10                                                        
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,KEY+7                                                         
         MVC   SKPUB(6),0(R1)                                                   
         CLI   QOPT3,C'A'          PUB ALPHA                                    
         BNE   TLIN24                                                           
         CLC   OLDPUB,0(R1)                                                     
         BE    TLIN23H                                                          
         MVC   OLDPUB,0(R1)                                                     
         MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),OLDPUB                                                  
         MVC   KEY+7(2),PAGYKAGY                                                
         MVI   KEY+9,X'81'                                                      
         CLC   PUBKEY(7),KEY                                                    
         BE    TLIN23E                                                          
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    TLIN23D                                                          
         CLI   PAGYPROF+16,C'0'    TEST DEFAULT                                 
         BNE   *+8                                                              
         BAS   RE,INOPUB                                                        
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+8                                                              
         BAS   RE,INOPUB                                                        
         MVC   KEY+7(2),=C'ZZ'                                                  
         MVI   KEY+9,X'81'                                                      
         CLC   KEY(9),KEYSAVE                                                   
         BE    TLIN23D             HAVE SRDS                                    
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+8                                                              
         BAS   RE,INOPUB                                                        
*                                                                               
TLIN23D  DS    0H                                                               
*                                                                               
         GOTO1 GETNAME                                                          
*                                                                               
TLIN23E  DS    0H                                                               
*                                                                               
         MVC   KEY(64),SAVKEYS                                                  
*                                                                               
TLIN23H  DS    0H                                                               
*                                                                               
         MVC   SKPUB(13),PUBNAME                                                
         CLC   SKPUB(4),=C'THE '                                                
         BNE   *+10                                                             
         MVC   SKPUB(13),PUBNAME+4                                              
         MVC   SKPUB+13(6),PUBKPUB                                              
*                                                                               
TLIN24   DS    0H                                                               
*                                                                               
         CLI  QQOPT8,C'N'          MARKET SORT                                  
         BE    TLIN25                                                           
*                                                                               
* SEE IF PUB HAS BEEN READ IN                                                   
*                                                                               
TLIN24A  LA    R1,PBUYKPUB                                                      
         CLI   KEY+3,X'20'                                                      
         BE    *+8                                                              
         LA    R1,PBUYKEY+7                                                     
         CLC   PUBKPUB(6),0(R1)                                                 
         BE    TLIN24M             MOVE MARKET                                  
         MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+1(6),0(R1)                                                   
         MVC   KEY+7(2),PAGYKAGY                                                
         MVI   KEY+9,X'81'                                                      
         CLC   PUBKEY(7),KEY                                                    
         BE    TLIN24E                                                          
         GOTO1 HIGHPUB                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    TLIN24D                                                          
         CLI   PAGYPROF+16,C'0'    TEST DEFAULT                                 
         BNE   *+8                                                              
         BAS   RE,INOPUB                                                        
         CLC   KEY(7),KEYSAVE                                                   
         BE    *+8                                                              
         BAS   RE,INOPUB                                                        
         MVC   KEY+7(2),=C'ZZ'                                                  
         MVI   KEY+9,X'81'                                                      
         CLC   KEY(9),KEYSAVE                                                   
         BE    TLIN24D             HAVE SRDS                                    
         BAS   RE,HIGHPUB                                                       
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+8                                                              
         BAS   RE,INOPUB                                                        
*                                                                               
TLIN24D  DS    0H                                                               
*                                                                               
         GOTO1 GETNAME                                                          
*                                                                               
TLIN24E  DS    0H                                                               
*                                                                               
         MVC   KEY(64),SAVKEYS                                                  
TLIN24M  CLI   PBUYKMED,C'O'       OUTDOOR                                      
         BNE   TLIN24N                                                          
         MVC   SKMARK(20),PUBZNAME                                              
         B     TLIN24SS                                                         
*                                                                               
TLIN24N  MVC   SKMARK(2),PUBSTATE                                               
         MVC   SKMARK+2(L'PUBCITY),PUBCITY                                      
*                                                                               
TLIN24SS DS    0H                                                               
*                                                                               
         CLI   QQOPT8,C'F'                                                      
         BNE   TLIN24SX                                                         
         MVC   SKMCLT(14),SKCLT      MOVE CLT/PRD/EST/JOB                       
*                                    BEHIND MARKET IN SORT                      
         XC    SKCLT(14),SKCLT       CLEAR CLT/PRD/EST/JOB                      
*                                                                               
TLIN24SX DS    0H                                                               
*                                                                               
TLIN25   CLI   QBPDATE,C'C' CLOSING DATE REQUESTED                              
         BNE   TLIN26                                                           
         MVC   SKDAT1,PBDCDATE   CLOSING DATE INTO SORTKEY                      
*  MAT                                                                          
         B     TLIN28                                                           
TLIN26   CLI   QBPDATE,C'M'                  MATERIALS CLOSING DATE             
         BNE   *+10                                                             
         MVC   SKDAT1,PBDMDATE   MATERIALS CLOSING DATE INTO SORTKEY            
*  MAT                                                                          
*                                                                               
TLIN28   DS    0H                                                               
*                                                                               
         MVC   SKDAT2,KEY+16     MOVE SECONDARY SORT KEY (INSERT DATE)          
*                                                                               
         CLI   SKDAT1,0          ANY DATE IN PRIMARY SORT FIELD                 
         BNE   *+10                                                             
         MVC   SKDAT1,SKDAT2     OVERLAY WITH INSERT DATE                       
         LH    R1,SRTLIN                                                        
         LA    R1,1(R1)                                                         
         STH   R1,SRTLIN                                                        
         MVC   SKLIN,SRTLIN        PRESERVE ORIG SEQ                            
*                                                                               
         MVC   SRECDA,KEY+27                                                    
         B     TLINX                                                            
*                                                                               
TLIN90   DS    0H                                                               
*                                                                               
         XC    OLDPUB,OLDPUB                                                    
         MVI   SAVPARS+3,8                                                      
         B     TLINX                                                            
*                                                                               
TLINX    DS    0H                                                               
*                                                                               
******** GOTO1 =V(PRNTBL),DMCB,(5,=C'KEY--'),SORTKEY,C'DUMP',60,=C'1D'          
         XMOD1 1                                                                
*                                                                               
INOPUB   DC    H'0'                                                             
*                                                                               
ESTFLTSW DC    X'00'               ESTIMATE FILTER SWITCH                       
RSBUYRSW DC    X'00'               RESTORE PBUYREC SWITCH                       
*                                                                               
*                                                                               
*                                                                               
INEXTEL  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    INEXTEL2                                                         
         CLC   ELCOD,0(R2)                                                      
         BER   RE                                                               
         B     INEXTEL+2                                                        
INEXTEL2 LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
***********************************************************************         
*                                                                               
CKESTFLT NTR1                      CHECK FOR ESTIMATE FILTER                    
*                                                                               
         MVI   ESTFLTSW,C'N'       INITIALIZE ESTIMATE FILTER SWITCH            
         MVI   RSBUYRSW,C'N'       INITIALIZE RESTORE PBUYREC SWITCH            
*                                                                               
         CLI   PBUYKEY+25,X'FF'    SEE RECORD HAS BEEN DELETED                  
         BE    CKEFX                                                            
         MVC   WORK(64),KEY        SAVE PBUYKEY                                 
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVI   KEY+3,7             ESTIMATE RECORD CODE                         
         MVC   KEY+10(2),PBUYKEY+19                                             
*                                                                               
         CLC   PESTREC(12),KEY     SEE IF ESTIMATE ALREADY THERE                
         BE    CKEF50                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                ESTIMATE REC MUST EXIST                      
*                                                                               
         MVI   RSBUYRSW,C'Y'       NEED TO RESTORE PBUYREC LATER                
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
*                                                                               
CKEF50   DS    0H                                                               
         LA    R2,PESTREC+33                                                    
         CLI   0(R2),X'07'                                                      
         BE    *+6                                                              
         DC    H'0'                ELEMENT MUST EXIST                           
         USING PESTELEM,R2                                                      
*                                                                               
* FILTER ESTIMATE                                                               
*                                                                               
         LA    RE,QESTEND                                                       
         LA    RF,PESTGRPS                                                      
         LA    R0,3                                                             
*                                                                               
CKEF60   DS    0H                                                               
         CLI   0(RE),C'*'                                                       
         BE    CKEF80                                                           
         CLI   0(RE),C' '                                                       
         BE    CKEF80                                                           
*                                                                               
         TM    0(RE),X'40'         TEST NEGATIVE FILTER                         
         BZ    CKEF70              YES                                          
*                                                                               
         CLC   0(1,RE),0(RF)       POSITIVE FILTER                              
         BNE   CKEFNO                                                           
         B     CKEF80                                                           
*                                                                               
CKEF70   DS    0H                                                               
         MVC   DUB(1),0(RE)                                                     
         OI    DUB,X'40'                                                        
         CLC   DUB(1),0(RF)        NEGATIVE FILTER                              
         BE    CKEFNO                                                           
*                                                                               
CKEF80   DS    0H                                                               
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,CKEF60                                                        
*                                                                               
         DROP  R2                                                               
*                                                                               
CKEFYES  MVI   ESTFLTSW,C'Y'       SETTING ESTIMATE FILTER SWITCH               
         B     *+8                                                              
CKEFNO   MVI   ESTFLTSW,C'N'                                                    
*                                                                               
         MVC   KEY(64),WORK        RESTORE PBUYREC                              
         CLI   RSBUYRSW,C'Y'                                                    
         BNE   CKEFX                                                            
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    CKEFX                                                            
         DC    H'0'                MUST GET SAME RECORD BACK!                   
*                                                                               
CKEFX    DS    0H                                                               
IXIT     XIT1                                                                   
*                                                                               
***********************************************************************         
*                                                                               
IGETCLT  NTR1                                                                   
         CLI   PBUYKEY,255                                                      
         BE    IGETCLTX                                                         
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(7),PBUYKEY                                                   
         MVI   KEY+3,2                                                          
*                                                                               
         CLC   PCLTREC(7),KEY          SEE IF CLIENT ALREADY THERE              
         BNE   IGETCLT5                                                         
         MVC   KEY(64),WORK                                                     
         B     IGETCLTX                                                         
*                                                                               
IGETCLT5 GOTO1 READ                                                             
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
IGETCLTX B     IXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
*  SUBROUTINE TO CHECK IF PRODUCT IS IN THE REQUESTED GROUP                     
*                                                                               
**********************************************************************          
FLTPGRP  NTR1                                                                   
*                                                                               
         CLI   KEY+3,X'20'         BUY IN CLT/PRD SEQUENCE ?                    
         BNE   FLTP05              NO - CANT DETERMINE "LAST" PRODUCT           
         CLC   FLTPKEYS(10),KEY    SAME AGM/RC/CLT/PRD ?                        
         BE    FLTPXIT             YES - USE "RESULTS" OF PRIOR TEST            
FLTP05   DS    0H                                                               
         MVC   FLTPKEYS(64),KEY    SAVE KEY/KEYSAVE                             
         XC    KEY,KEY                                                          
         MVC   KEY(3),RCSVAGY                                                   
         MVI   KEY+3,X'3B'                                                      
         MVC   KEY+4(3),QCLIENT                                                 
         MVC   KEY+7(3),FLTPKEYS+7   PRODUCT                                    
         OC    KEY+7(6),SPACES       SPACE FILL                                 
*                                                                               
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         MVC   KEY+13(1),Q2USER+24   SCHEME CODE IN SECOND CARD                 
         DROP  RF                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(14),KEYSAVE       CHECK THRU SCHEME                          
         BNE   FLTPNO                                                           
         MVC   WORK(4),SPACES                                                   
         LA    R1,4                                                             
*                                                                               
         L     RF,PPWORK2C                                                      
         USING PPWORK2D,RF                                                      
         CLI   Q2USER+25,C' '   NO GROUP - HAS PASSED SCHEME CHECK              
         BNH   FLTPYES                                                          
         CLI   Q2USER+25,C'*'   NO GROUP - HAS PASSED SCHEME CHECK              
         BNH   FLTPYES                                                          
*                                                                               
         LA    R2,Q2USER+25        PRDGROUP IN SECOND CARD                      
         DROP  RF                                                               
*                                                                               
         LA    R3,WORK                                                          
         MVC   WORK(5),=C'00000'                                                
FLTP10   CLI   0(R2),C'0'          STOP SCANNING                                
         BL    FLTP20                                                           
         MVC   0(1,R3),0(R2)       MOVE DIGIT INTO WORK                         
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,FLTP10                                                        
*                                                                               
FLTP20   DS    0H                                                               
         LTR   R1,R1          MEANS I HAD 4 DIGITS                              
         BNZ   FLTP30                                                           
         PACK  DUB(3),WORK(5)      DO EXTRA BYTE FOR PWOS                       
         CLC   KEY+14(2),DUB       CHECK FOR MATCH                              
         BE    FLTPYES                                                          
         B     FLTPNO                                                           
*                                                                               
FLTP30   DS    0H                                                               
         CH    R1,=H'1'      MEANS I HAD 3 DIGITS                               
         BNE   FLTP40                                                           
         MVC   PCHECK(2),KEY+14                                                 
         NI    PCHECK+1,X'F0'     TURN OFF LAST HALF                            
         PACK  DUB(3),WORK(5)     ONE EXTRA FOR PWOS                            
         CLC   PCHECK(2),DUB                                                    
         BE    FLTPYES                                                          
         B     FLTPNO                                                           
*                                                                               
FLTP40   CH    R1,=H'2'    MEANS I HAD 2 DIGITS                                 
         BNE   FLTP50                                                           
         PACK  DUB(2),WORK(3)                                                   
         CLC   KEY+14(1),DUB                                                    
         BE    FLTPYES                                                          
         B     FLTPNO                                                           
*                                                                               
FLTP50   DS    0H                                                               
         CH    R1,=H'3'      MEANS I HAD 1 DIGIT                                
         BNE   FLTP60                                                           
         MVC   PCHECK(1),KEY+14                                                 
         NI    PCHECK,X'F0'    TURN OFF LAST HALF                               
         PACK  DUB(2),WORK(3)  ONE EXTRA FOR PWOS                               
         CLC   PCHECK(1),DUB                                                    
         BE    FLTPYES                                                          
         B     FLTPNO                                                           
*                                                                               
FLTP60   DS    0H               ONLY GROUP ID ENTERED                           
*                                                                               
FLTPYES  MVI   FLTPSW,C'Y'              PROPER GROUP                            
         B     *+8                                                              
*                                                                               
FLTPNO   MVI   FLTPSW,C'N'              NOT IN GROUP                            
*                                                                               
         MVC   KEY(32),FLTPKEYS                                                 
         GOTO1 HIGH                     RESTORE KEY                             
         MVC   KEYSAVE(32),FLTPKEYS+32  AND KEYSAVE TO ORIGINAL                 
*                                                                               
FLTPXIT  DS    0H                                                               
         CLI   FLTPSW,C'Y'     SETS CC CODE                                     
         XIT1                                                                   
*                                                                               
FLTPKEYS DS    CL64                                                             
FLTPSW   DS    CL1                                                              
PCHECK   DS    CL2                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'READ REP ADDRESS'                                               
COMMON   CSECT                                                                  
*                                                                               
*       ON CALL FCGTREP  C'Y'  GET PAY REP                                      
*               FCGTTREP C'Y'  TRAFFIC REP                                      
*               FCGTCREP C'Y'  CONTRACT REP                                     
*                                                                               
*       SAME DSECTS TO BE USED AS CALLING PROGRAM // DO NOT DESTROY             
*       THOSE REGISTERS ON CALL                                                 
*                                                                               
*                                                                               
*                                                                               
         NMOD1 0,REPREAD                                                        
         USING PPWORKD,RA                                                       
         USING PPFILED,RC,R9                                                    
         USING PP77WRKD,R8                                                      
         LR    RC,R9                                                            
         S     RC,=F'4096'      RESTORE RC                                      
         L     RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     READREP                                                          
         B     TTLHEAD                                                          
         B     DDOMIDS                                                          
         B     TLTOT                                                            
         EJECT                                                                  
DDOMIDS  CLI  QQOPT8,C'N'          MARKET SORT                                  
         BE    ENDMID                                                           
         CLI   SAVMIDSW,255        JUST A SPACE                                 
         BNE   DDOMIDDA                                                         
         CLI   FORCEHED,C'Y'       PRINTING A SPACE THAT FORCES HED             
         BNE   ENDMID                                                           
DDOMIDDA DS    0H                                                               
DOMIDAA  L     R3,SAVPARS+4                                                     
         USING SORTRECD,R3                                                      
         CLC   SAVMID,SKMARK   HAS MID BEEN PRINTED AND NO CHANGE               
         BNE   DOMIDAB             IN MARKET                                    
         CLI   FORCEHED,C'Y'                                                    
         BNE   ENDMID                                                           
         OI    CONTINU,255                                                      
         B     SPNOSPAC                                                         
DOMIDAB  CLI   SAVMID+1,255        BREAK IN CLI,PRD,EST,JOB NO                  
         BE    ENDMID                                                           
*                                                                               
         OC    SKMARK(20),SKMARK                                                
         BZ    ENDMID                                                           
SPNOSPAC DS    0H                                                               
         MVI   FORCEMID,C'Y'                                                    
         MVC   SAVMID,SKMARK                                                    
         MVC   MID1(20),SKMARK                                                  
         OC    SKMARK,SKMARK       IF ZERO GET OUT                              
         BZ    ENDMID                                                           
         CLI   KEY+2,C'N'                                                       
         BNE   DOSQUS                                                           
         MVC   MID1(2),SKMARK       STATE CODE                                  
         MVC   MID1+2(2),=C', '                                                 
         MVC   MID1+4(16),SKMARK+2  CITY                                        
DOSQUS   OC    MID1,SPACES                                                      
         GOTO1 =V(SQUASHER),DMCB,MID1,20                                        
         L     RF,4(R1)            SQUASHED LENGTH                              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MID2(0),=30C'-'                                                  
         CLI   CONTINU,255                                                      
         BNE   ENDMID                                                           
         LA    RE,MID1                                                          
         LA    RE,1(RE,RF)                                                      
         MVC   1(11,RE),=C'(CONTINUED)'                                         
         MVI   CONTINU,0                                                        
* MUST DETERMINE IF FORCE HEAD M/B TURNED ON                                    
         ZIC   R0,LINE                                                          
         LA    RF,4            NUMBER OF LINES FOR MIDS                         
         AR    R0,RF                                                            
         STC   R0,BYTE                                                          
         ZIC   RF,LINENEED                                                      
         LA    RF,4(RF)                                                         
         STC   RF,LINENEED                                                      
         CLC   BYTE,MAXLINES                                                    
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
******** GOTO1 =V(PRNTBL),DMCB,(5,=C'LNEED'),LINENEED,C'DUMP',2,=C'1D'          
ENDMID   XIT1                                                                   
CONTINU  DC    X'0'                                                             
         EJECT                                                                  
READREP  CLI   TREPA1,X'41'      WAS PRIOR REP EQUAL TO PREVIOUS                
         BE    *+10              DO NOT DESTROY PREVREP                         
         MVC   PREVREP,TREPA1    SAVE                                           
         SPACE 2                                                                
         MVI   DOUBLE,0    INITIALIZE                                           
         B     *+8                                                              
READREPI MVI   DOUBLE,1            READING REPS ON INPUT                        
         MVI   ADRTYP,C'P'         ASSUME PAY                                   
         CLI   FCGTREP,C'Y'                                                     
         BE    READREP0                                                         
*                                                                               
         MVI   ADRTYP,C'T'         TRAFFIC REP                                  
         CLI   FCGTTREP,C'Y'                                                    
         BE    READREP0                                                         
*                                                                               
         MVI   ADRTYP,C'C'         CONTRACT                                     
         CLI   FCGTCREP,C'Y'                                                    
         BNE   XXIT                                                             
         SPACE 2                                                                
READREP0 DS    0H                                                               
         SPACE 2                                                                
         CLI   QOPT6,C'S'          SEE IF DOING SHIPPING ADDRESS                
         BNE   *+8                                                              
         MVI   ADRTYP,C'S'         SHIPPING                                     
*                                    FILL IN 7 BYTES OF CLTDATA                 
         MVC   CLTDATA(3),PCLTKAGY   CLIENT AGY/MED                             
         MVC   CLTCODE,PCLTKCLT      CLIENT CODE                                
         MVC   CLTOFF,PCLTOFF        CLIENT OFFICE                              
*                                                                               
         GOTO1 =V(PPGETADR),DMCB,(ADRTYP,CLTDATA),PUBREC,DATAMGR,0              
*                                                                               
         CLI   0(R1),X'FF'         ERROR IN CALL ?                              
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         MVC   WORK(4),0(R1)       2-4 = ADDRESS LEVEL                          
*                                                                               
         L     R5,4(R1)            A(ADDRESS INFO FROM CALL)                    
         ST    R5,FULL             SAVE A(ADDRESS)                              
*                                                                               
         CLI   WORK,0              ADDRESS RECORD FOUND ?                       
         BE    RREP6               NO - TRY FOR A REP                           
         CLI   WORK,X'0B'          SHIPPING ADDRESS ?                           
         BE    RREP20              YES - ALWAYS USE IF FOUND                    
         CLI   WORK+1,X'FF'        IF HAVE CLIENT ADDR NO NEED                  
         BL    RREP20              TO LOOK FOR REP                              
*                                                                               
*                                  NOW TRY FOR A REP                            
RREP6    DS    0H                                                               
         LA    R2,PUBREC+33                                                     
         XC    DUB,DUB                                                          
         SR    R0,R0                                                            
RREP7    DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    RREP11                                                           
         CLI   0(R2),X'14'                                                      
         BE    RREP9                                                            
RREP8    DS    0H                                                               
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     RREP7                                                            
RREP9    DS    0H                                                               
         USING PUBREPEL,R2                                                      
         CLC   PUBRPOFF,=3X'FF'                                                 
         BE    RREP10                                                           
         CLC   PUBRPOFF,PCLTKCLT                                                
         BE    RREP10                                                           
         CLI   PUBRPOFF,X'FF'                                                   
         BNE   RREP8                                                            
         CLC   PUBRPOFF+1(1),PCLTOFF                                            
         BNE   RREP8                                                            
RREP10   DS    0H                                                               
         OC    PUBPAREP(12),PUBPAREP                                            
         BZ    RREP8               NO OVERIDES THIS ELEM                        
         LA    RF,PUBPAREP                                                      
         CLI   FCGTREP,C'Y'                                                     
         BE    RREP10B                                                          
         LA    RF,PUBTRREP                                                      
         CLI   FCGTTREP,C'Y'                                                    
         BE    RREP10B                                                          
         LA    RF,PUBCNREP                                                      
RREP10B  DS    0H                                                               
         MVC   DUB(4),0(RF)                                                     
         MVC   DUB+4(3),PUBRPOFF                                                
RREP11   DS    0H                                                               
         OC    WORK+1(3),WORK+1                                                 
         BZ    RREP12                                                           
         CLC   WORK+1(3),DUB+4     TEST 'LEVEL'                                 
         BL    RREP20              ADDR MORE SPECIFIC                           
*                                                                               
*  NOTE - THE ABOVE BL IS A BNH IN ALL PROGRAMS USING PUB ADDRESSES             
*         EXCEPT PPFILCON AND THIS ONE. THIS MEANS THAT IF THE 'LEVEL'          
*         IS EQUAL, A REP ADDRESS WOULD BE USED IN THIS PROGRAM AND             
*         IN PPFILCON AND A PUB ADDRESS OVERRIDE WOULD BE USED IN ALL           
*         OTHER PROGRAMS. THE MANUAL ("SPECIAL PUB ADDRESSES FOR                
*         CLIENTS/OFFICES") INDICATES THAT THE PUB ADDRESS IS THE ONE           
*         THAT SHOULD BE USED.                                                  
*                                                                               
RREP12   DS    0H                                                               
         OC    DUB(4),DUB                                                       
         BZ    RREP20              NO REP                                       
*                                                                               
         LR    R0,RE                                                            
         MVC   WORK,KEY                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(2),PUBKEY+7     AGENCY                                       
         MVC   KEY+2(1),PUBKEY     MEDIA                                        
         MVI   KEY+3,X'11'                                                      
         MVC   KEY+4(4),DUB        REP CODE                                     
         CLC   KEY(10),PREPKEY     DONT READ IF ALREADY THERE                   
         BE    RREP18                                                           
         XC    PREPKEY(159),PREPKEY                                             
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(10),KEY                                                  
         BNE   RREP18                                                           
         GOTO1 GETREP                                                           
         B     RREP19                                                           
*                                                                               
RREP18   DS    0H                                                               
*        DIDN'T READ ANYTHING SO RESTORE KEY AND EXIT                           
         MVC   KEY,WORK                                                         
         B     RREPX               REP ALREADY THERE                            
*                                                                               
RREP19   MVC   KEY,WORK            RESTORE KEY                                  
*        CLI   DOUBLE,1            REPS ON INPUT-RESTORE SEQ READ               
*        BE    RREP19C                                                          
*        CLI   SORTSW,C'N'         REPS ON OUTPUT-ONLY RESTORE READ             
*                                  IF NOT SORTING                               
*        BNE   RREPX                                                            
RREP19C  GOTO1 HIGH                                                             
         MVC   TREPA1(30),PREPNAME                                              
         MVC   TREPA2(30),PREPLIN1                                              
         MVC   TREPA3(30),PREPLIN2                                              
         MVC   TREPA4(120),SPACES                                               
         MVC   TREPA5(20),PREPATTN                                              
         MVC   TREPA6(12),PREPTEL                                               
         MVI   TREPA7,X'41'                                                     
         MVI   TREPAX,X'FF'    STOP                                             
         B     RREPX                                                            
         SPACE 3                                                                
RREP20   DS    0H                  ADDRESS FROM PUB ADDRESS OVERRIDE            
         XC    PREPREC(159),PREPREC                                             
         CLI   WORK+1,0            ADDRESS RECORD FOUND ?                       
         BE    RREPXX              NO - USE "DIRECT TO PUB"                     
         L     R2,FULL                                                          
         USING PGETADRD,R2                                                      
         MVC   PREPELEM(2),PGADELEM                                             
         MVC   TREPA1(L'PGADNAME),PGADNAME                                      
         MVC   TREPA2(L'PGADLIN1),PGADLIN1                                      
         MVC   TREPA3(L'PGADLIN2),PGADLIN2                                      
         MVC   TREPA4(120),SPACES                                               
         MVC   TREPA5(L'PGADATTN),PGADATTN                                      
         MVC   TREPA6(L'PGADTEL),PGADTEL                                        
         MVI   TREPA7,X'41'                                                     
         MVI   TREPAX,X'FF'    STOP                                             
*                                                                               
         OC    PGADLIN3,PGADLIN3   SEE IF ANYTHING HERE                         
         BZ    *+10                NO                                           
         MVC   TREPA4(L'PGADLIN3),PGADLIN3                                      
*                                                                               
RREP20F  MVC   PREPKEY(3),RCSVAGY                                               
*                                                                               
         DROP  R2                                                               
*                                                                               
RREPX    DS    0H                                                               
         CLC   PREVREP,TREPA1  SAME REP NAME                                    
         BNE   XXITT1                                                           
         MVC   TREPA1(180),SPACES                                               
         MVC   TREPA1+1(21),=C'ADDRESS SAME AS PRIOR'                           
         MVI   TREPA1,X'41'                                                     
         MVI   TREPA2,X'41'                                                     
         MVC   TREPA3(2),=X'FFFF'                                               
*                                                                               
XXITT1   DS    0H                                                               
*                                                                               
         LA    RE,TREPA1      BEGINING OF ADDRESSES                             
         LA    RF,0                                                             
CLIRE255 CLI   0(RE),255      END                                               
         BE    ENDXXITT                                                         
         LA    RF,1(RF)       NEED A LINE TO PRINT                              
         LA    RE,L'TREPA1(RE)                                                  
         B     CLIRE255                                                         
*                                                                               
ENDXXITT CLI   QOPT5,C'Y' DOUBLE SPACING                                        
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         STC   RF,REPLINED                                                      
*                                                                               
XXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
RREPXX   DS    0H                                                               
         MVC   TREPA1,SPACES                                                    
         MVC   TREPA1(17),=C'**DIRECT TO PUB**'                                 
         MVC   TREPA2,PUBLINE1                                                  
         MVC   TREPA3,PUBLINE2                                                  
         MVC   TREPA4(120),SPACES                                               
         MVI   TREPA7,X'41'                                                     
         MVI   TREPAX,255                                                       
*                                                                               
*                                                                               
         B     RREPX                                                            
         TITLE 'TLHEAD PRINT HEADLINES'                                         
TTLHEAD  DS    0H                                                               
         MVI   RCSUBPRG,0                                                       
         CLI   QQOPT8,C'F'         SEE IF MARKET FIRST                          
         BE    TLHEAD0D            YES - THEN LEAVE RCSUBPRG ALONE              
         CLC   QCLIENT,SPACES                                                   
         BE    *+8                                                              
         MVI   RCSUBPRG,1                                                       
         CLC   QPRODUCT,SPACES                                                  
         BE    TLHEAD0                                                          
         CLI   RUNSW,X'21'         PUB/PRD SEQ                                  
         BE    TLHEAD0                                                          
         MVI   RCSUBPRG,2                                                       
TLHEAD0  DS    0H                                                               
         CLC   QEST,SPACES                                                      
         BE    *+8                                                              
         MVI   RCSUBPRG,3                                                       
TLHEAD0D CLI   QOPT2,C'$'                                                       
         BNE   TLHEAD1                                                          
         SR    RF,RF                                                            
         IC    RF,RCSUBPRG                                                      
         LA    RF,10(RF)                                                        
         STC   RF,RCSUBPRG                                                      
TLHEAD1  DS    0H                                                               
         CLI   QOPT1,C'A'                                                       
         BE    TLHEAD4                                                          
         LA    R1,HEAD7+1                                                       
         CLI   QOPT1,C'N'                                                       
         BNE   TLHEAD2                                                          
         MVC   000(19,R1),=C'UNORDERED BUYS ONLY'                               
         MVC   132(19,R1),=C'-------------------'                               
         B     TLHEAD4                                                          
TLHEAD2  DS    0H                                                               
         MVC   000(17,R1),=C'ORDERED BUYS ONLY'                                 
         MVC   132(17,R1),=C'-----------------'                                 
TLHEAD4  DS    0H                                                               
         LA    RF,SPACES                                                        
         CLI   QBPDATE,C'D'                                                     
         BE    TLHEAD5                                                          
         LA    RF,BILHD                                                         
         CLI   QBPDATE,C'B'                                                     
         BE    TLHEAD5                                                          
         LA    RF,PAYHD                                                         
         CLI   QBPDATE,C'P'                                                     
         BE    TLHEAD5                                                          
         LA    RF,CLOSHD                                                        
         CLI   QBPDATE,C'C'                                                     
         BE    TLHEAD5                                                          
***MAT***                                                                       
         LA    RF,MCLOSHD                                                       
         CLI   QBPDATE,C'M'               MATERIALS CLOSING                     
         BE    TLHEAD5                                                          
***MAT***                                                                       
         LA    RF,OSDHD                                                         
         CLI   QBPDATE,C'S'                                                     
         BE    TLHEAD5                                                          
         LA    RF,IODHD                                                         
         CLI   QBPDATE,C'I'                                                     
TLHEAD5  DS    0H                                                               
         MVC   HEAD5+50(22),0(RF)                                               
         CLI   QCLIENT,C'*'                                                     
         BNE   TLHEAD6                                                          
         MVC   HEAD3+1(6),=C'OFFICE'                                            
*SMY*    MVC   HEAD3+10(1),QCLIENT+1                                            
*                                                                               
*        PRINT OFFICE CODE                                                      
*                                                                               
         GOTOR VPRNTOFC,DMCB,QCLIENT+1,(C'L',HEAD3+10),VOFFICER,       X        
               QAGENCY,VCOMFACS                                                 
*                                                                               
         B     TLHEAD7                                                          
*                                                                               
TLHEAD6  DS    0H                                                               
         CLI   QCLIENT,C'&&'               GROUP REQUEST                        
         BNE   TLHEAD6H                                                         
         MVC   HEAD3+1(5),=C'GROUP'                                             
         MVC   HEAD3+10(1),QCLIENT+1                                            
         B     TLHEAD7                                                          
*                                                                               
TLHEAD6H DS    0H                                                               
         CLI   QCLIENT,C'$'                OFFICE LIST REQUEST                  
         BNE   TLHEAD7                                                          
         MVC   HEAD3+01(11),=C'OFFICE LIST'                                     
         MVC   HEAD3+13(1),QCLIENT+1                                            
         MVI   HEAD3+15,C'('                                                    
         MVC   HEAD3+16(06),=C'OFFICE'                                          
*SMY*    MVC   HEAD3+23(1),PCLTOFF                                              
*                                                                               
*        PRINT OFFICE CODE                                                      
*                                                                               
         GOTOR VPRNTOFC,DMCB,PCLTOFF,HEAD3+23,VOFFICER,QAGENCY,VCOMFACS         
*                                                                               
         MVI   HEAD3+25,C')'                                                    
*                                                                               
TLHEAD7  DS    0H                                                               
         CLI   QOPT7,C'Y'                                                       
         BNE   TLHEADX                                                          
         MVI   HEAD4+115,C'T'                                                   
         MVC   HEAD7+97(34),=C'*INCLUDES ANY PROPOSED INSERTIONS*'              
         CLI   QMEDIA,C'O'                                                      
         BNE   TLHEADX                                                          
         MVC   HEAD7+120(11),=C'POSTINGS*  '                                    
TLHEADX  DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
TLTOT    DS    0H                                                               
         OC    0(L'ESTTOTS,R2),0(R2)                                            
         BZ    TLTXIT                                                           
         MVC   TOTWRK,0(R2)                                                     
         XC    0(L'ESTTOTS,R2),0(R2)                                            
         LM    R0,R1,TOTWRK                                                     
         A     R0,TOTWRK+8                                                      
         A     R1,TOTWRK+12                                                     
         STM   R0,R1,TOTTOT                                                     
*                                                                               
         MVC   P+1(15),0(R3)                                                    
         LA    RF,3                                                             
         LA    R5,ORDWRDS                                                       
         LA    R6,P+20                                                          
         LA    R2,TOTWRK                                                        
TLTOT2   DS    0H                                                               
         MVC   0(9,R6),0(R5)                                                    
         EDIT  (B4,0(R2)),(14,9(R6)),2,COMMAS=YES,MINUS=YES,FLOAT=$             
         CLI   25(R6),C' '                                                      
         BNE   *+6                                                              
         BCTR  R6,R0                                                            
         MVI   23(R6),C'/'                                                      
         EDIT  (B4,4(R2)),(5,24(R6)),ALIGN=LEFT                                 
         OI    24(R6),C'0'                                                      
*                                                                               
         LA    R2,8(R2)                                                         
         LA    R6,33(R6)                                                        
         LA    R5,9(R5)                                                         
         BCT   RF,TLTOT2                                                        
****     BAS   RE,TLPRT            ***************  SMYE                        
TLTXIT   DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
ORDWRDS  DC    CL9'  ORDERED'                                                   
         DC    CL9'UNORDERED'                                                   
         DC    CL9'    TOTAL'                                                   
*                                                                               
BILHD    DC    C' ** BILLING PERIOD ** '                                        
PAYHD    DC    C' ** PAYABLE DATES **  '                                        
CLOSHD   DC    C' ** CLOSING DATES **  '                                        
MCLOSHD  DC    C'**MAT. CLOSING DATES**'                                        
IODHD    DC    C'** INS. ORDER DATES **'                                        
OSDHD    DC    C' ** ON-SALE DATES **  '                                        
*                                                                               
         LTORG                                                                  
*          DATA SET PPREP7802  AT LEVEL 031 AS OF 04/14/88                      
         TITLE 'PRINT COMMENT TABLE AT END OF REQUEST'                          
PRTCOM   CSECT                                                                  
         NMOD1 0,PRTCOM                                                         
         L     RC,PPFILEC                                                       
         L     R6,ASCOMTAB                                                      
         L     RF,BSPAR3                 NUMBER OF COMMENTS                     
         OC    BSPAR3,BSPAR3                                                    
         BZ    PRTCX                                                            
         LA    R4,P              FOR PRINT LINE                                 
PRTC5    ST    RF,BCTCNT         SAVE BCTCNT GETS HERE AT PRTC30                
         CLI   0(R6),X'FF'                                                      
         BE    PRTCX                                                            
         BAS   RE,XLPRT                 SKIP A LINE                             
         LR    R3,R6                                                            
PRTC8    CLI   0(R3),C' '                                                       
         BNE   PRTC9                                                            
         LA    R3,1(R3)                                                         
         B     PRTC8                                                            
*                                                                               
PRTC9    CLI   0(R3),C' '                                                       
         BE    PRTC10                                                           
         LA    R3,1(R3)                                                         
         B     PRTC9                                                            
*                                                                               
PRTC10   SR    R3,R6                                                            
         CHI   R3,7                                                             
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK(7),SPACES                                                   
         LA    R1,WORK+6                                                        
         SR    R1,R3                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R6)         RIGHT ALIGN IN WORK                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),QAGENCY                                                   
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),WORK                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'              COMMENT NOT FOUND                              
         L     R0,ACONIO1          (A)PCONREC                                   
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         MVC   TLCLT+1(6),0(R6)                                                 
*                                                                               
         L     R2,ACONIO1          (A)PCONREC                                   
         LA    R2,33(R2)                                                        
         MVI   ELCOD,X'40'                                                      
         CLI   0(R2),X'40'                                                      
         BE    PRTC15                                                           
PRTC12   BAS   RE,PNEXTEL                                                       
         BNE   PRTC30                                                           
PRTC15   ZIC   R5,1(R2)                                                         
         LA    R3,2(R2)                                                         
         CLI   2(R2),C'+'                                                       
         BNE   PRTC17                                                           
         MVC   SPACING,3(R2)                                                    
         NI    SPACING,X'0F'                                                    
         CLI   SPACING,3                                                        
         BNH   *+8                                                              
         MVI   SPACING,3                                                        
         CLI   SPACING,0                                                        
         BNE   *+8                                                              
         MVI   SPACING,1                                                        
         MVI   TLCLT,0                                                          
         BAS   RE,XLPRT                                                         
         SH    R5,=H'2'        FOR '+' AND NUMBER OF LINES                      
         LA    R3,4(R2)                                                         
*                                                                               
PRTC17   SH    R5,=H'3'        FOR ELEM CODE+LENGHT +1 FOR EXECUTE              
         BM    PRTC18          NOTHING TO PRINT SO PRINT BLANK LINE             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   TLCLT+10(0),0(R3)                                                
PRTC18   MVI   TLCLT,0                                                          
         BAS   RE,XLPRT                                                         
         B     PRTC12                                                           
*                                                                               
PRTC30   LA    R6,7(R6)         GO DO NEXT STANDARD COMMENT                     
         L     RF,BCTCNT                                                        
         BCT   RF,PRTC5                                                         
         SPACE 2                                                                
PRTCX    XIT1                                                                   
BCTCNT   DS    F                                                                
*                                                                               
PNEXTEL  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    PNEXTEL2                                                         
         CLC   ELCOD,0(R2)                                                      
         BER   RE                                                               
         B     PNEXTEL+2                                                        
PNEXTEL2 LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
*                                  PRINTING                                     
XLPRT    NTR1                                                                   
         SPACE 2                                                                
         BAS   RE,XLCKHD                                                        
*                                                                               
XLPRT2   DS    0H                                                               
         CLI   FORCEHED,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,XLHEAD                                                        
         GOTO1 REPORT                                                           
*                                                                               
         MVI   LINENEED,0                                                       
         B     PRTCX                                                            
         TITLE 'TLHEAD PRINT HEADLINES'                                         
XLHEAD   DS    0H                                                               
         MVI   RCSUBPRG,20    FORCE TO NEW PAGEHEAD FOR COMMENTS                
*                                                                               
         BR    RE                                                               
         SPACE 2                                                                
BILHD1   DC    C' **   GLOSSARY     ** '                                        
PAYHD1   DC    C' ** PAYABLE DATES **  '                                        
CLOSHD1  DC    C' ** CLOSING DATES **  '                                        
MCLOSHD1 DC    C'**MAT. CLOSING DATES**'                                        
IODHD1   DC    C'** INS. ORDER DATES **'                                        
OSDHD1   DC    C' ** ON-SALE DATES **  '                                        
*                                                                               
         SPACE 3                                                                
XLCKHD   DS    0H                                                               
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         IC    R0,LINE                                                          
         IC    RF,LINENEED                                                      
         AR    R0,RF                                                            
         STC   R0,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BLR   RE                                                               
         MVI   FORCEHED,C'Y'                                                    
         BR    RE                                                               
         LTORG                                                                  
         DROP  RB,RC,RA,R9,R8                                                   
         EJECT                                                                  
COPYOVER CSECT                                                                  
         NMOD1 0,COPY                                                           
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP77WRKD,R8                                                      
         ST    R1,COPYSVE1         SAVE POINTER                                 
         MVC   DMWORK,SPACES                                                    
*                                                                               
*  DETERMINE IF THERE ARE CAPTION OR COPY OVERRIDES IN THE BUY PGM.             
*    IF SO, PASS THEM IN ADDRESS PROVIDED BY CALLING ROUTINE                    
*                                                                               
         MVC   FUNCTN,0(R1)      SAVE INDICATOR                                 
         MVC   COMPOPT,COPYEQ    ASSUME COPY MOVE EXECUTE LENGTH                
         CLI   FUNCTN,C'C'       MAX LENGTH OF MOVE AND CONSTANT                
         BE    *+10                                                             
         MVC   COMPOPT,CAPTEQ                                                   
*                                                                               
*  PREPARE TO READ COMMENT ELEMENTS                                             
*                                                                               
*========                                                                       
*========                                                                       
         LA    R2,PBUYREC+33                                                    
         MVI   ELCOD,X'66'       COMMENT ELEMENT ID                             
COMLOOP  BAS   RE,LOOPCOMM                                                      
         BNE   NOMORE66                                                         
         ZIC   RE,COMPOPT                                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   2(0,R2),COMPOPT+2                                                
         BNE   COMLOOP            HIT ON CAP= OR COPY=                          
         LR    R1,R2              SAVE ELEMENT ADDRESS                          
         LA    R1,3(RE,R1) PUSH PAST EL LEN,ID AND LEN OF COMPARE16             
         ZIC   RF,1(R2)      LENGTH OF ELEMENT                                  
         SH    RF,=H'4'      REDUCE BY EL ID, LEN AND FOR EXECUTE               
         SR    RF,RE                                                            
         ZIC   RE,COMPOPT+1  LIMIT ON MOVE                                      
         CR    RE,RF                                                            
         BH    *+6                                                              
         LR    RF,RE         USE MAX LIMIT                                      
         LTR   RF,RF         ENSURE POSITIVE                                    
         BNM   *+6           B ON NOT MINUS                                     
         LR    RF,RE                                                            
         L     RE,COPYSVE1   R1 POINTER                                         
         L     RE,4(RE)      POINT TO DMWORK                                    
         CLI   COMPOPT,3     CAN HAVE MORE THAN 1 CAPTION                       
         BNE   JUSTCOPY                                                         
         LA    R4,25(RE)     BUMP TO NEXT SECOND CAPTION SAVE                   
         L     R3,COPYSVE1                                                      
         ST    R4,4(R3)      NEW SAVE ADDRESS FOR NEXT GO AROUND                
JUSTCOPY EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(R1)                                                    
         B     COMLOOP        FORCE TO END OF COMMENTS                          
NOMORE66 DS    0H                                                               
         XMOD1 1                                                                
*                                                                               
COPYSVE1 DS    F                                                                
*                                                                               
COMPOPT  DS    CL7                                                              
*                                                                               
COPYEQ   DC    X'0416',C'COPY='  LEN OF COMPARE/ MOVE LIMIT                     
CAPTEQ   DC    X'0324',C'CAP= '  LEN OF COMPARE/ MOVE LIMIT                     
FF       EQU   X'FF'                                                            
FUNCTN   DS    CL1 CAPTION OR COMMENT                                           
         LTORG                                                                  
         SPACE 2                                                                
LOOPCOMM ZIC   R0,1(R2)     LOAD ELEMENT LENGTH                                 
         AR    R2,R0        GET TO NEXT ELEMENT                                 
         CLI   0(R2),0      END OF RECORD                                       
         BE    NXXTL                                                            
         CLC   ELCOD,0(R2)                                                      
         BER   RE                                                               
         B     LOOPCOMM     CONTINUE                                            
NXXTL    LTR   RE,RE        FORCE CONDITION CODE TO BNE                         
         BR    RE                                                               
*                                                                               
         TITLE 'DSECTS AND STORAGE'                                             
*                                                                               
SCOMTAB  CSECT                                                                  
         DS    7000C                  1000 X 6+1                                
*                                                                               
SINSTAB  CSECT                                                                  
         DS    1700C                  50 X 34                                   
*                                                                               
         SPACE 3                                                                
PP77WRKD DSECT                                                                  
RUNSW    DS    X                                                                
ESAVKEY  DS    CL32                                                             
CHGSW    DS    CL1                                                              
ORDSW    DS    CL1                                                              
BSTART   DS    XL3                                                              
BEND     DS    XL3                                                              
MYBPUB   DS    XL6                                                              
*                                                                               
P72APROF DS    CL16                                                             
PFWORK   DS    CL20                                                             
*                                                                               
OLDKEY   DS    0CL25                                                            
         DS    CL4                                                              
OLDCLT   DS    CL3                                                              
OLDPRD   DS    CL3                                                              
OLDPUB   DS    CL6                                                              
         DS    CL3                                                              
OLDEST   DS    CL2                                                              
         DS    CL4                                                              
OLDJOB   DS    CL6                                                              
*                                                                               
         DS    0F                                                               
SAVPARS  DS    0CL24                                                            
         DS    6F                                                               
SAVR2    DS    F                                                                
LASTIOD  DS    CL2                 PACKED DATE OF LAST IO                       
ELCOD    DS    X                                                                
LINENEED DS    X                                                                
SRTLIN   DS    H                                                                
ARECC    DS    A                                                                
SAVKEYS  DS    CL64                                                             
SAVDAT2  DS    CL8                                                              
PREVREP  DS    CL180                                                            
SAVLWCD  DS    CL1                 LEGAL WARNING CODE                           
SAVLWQC  DS    CL1                 LEGAL WARNING QUARTER                        
SAVLWTYP DS    CL1                 O = "OVERRIDE" (FROM BUYREC)                 
*                                  D = "DEFAULT"  (FROM PRDREC)                 
*                                                                               
IOELCOD  DS    XL1                 X'70' OR X'71'                               
LASTIODX DS    XL3                 MOST RECENT INSERTION ORDER DATE             
*                                                                               
         DS    0D                                                               
ESTTOTS  DS    CL16                                                             
PRDTOTS  DS    CL16                                                             
CLTTOTS  DS    CL16                                                             
*                                                                               
TOTWRK   DS    CL16                                                             
TOTTOT   DS    CL8                                                              
*                                                                               
PBNAM1   DS    CL20                                                             
PBNAM2   DS    CL20                                                             
PBNAM3   DS    CL20                                                             
PBNAM4   DS    CL20                                                             
PBNAMX   DS    CL1                                                              
SPAC1    DS    CL20                                                             
SPAC2    DS    CL20                                                             
SPAC3    DS    CL20                                                             
SPACX    DS    CL1                                                              
*                                                                               
TREPA    DS    0CL(30*6)                                                        
TREPA1   DS    CL30                                                             
TREPA2   DS    CL30                                                             
TREPA3   DS    CL30                                                             
TREPA4   DS    CL30                                                             
TREPA5   DS    CL30                                                             
TREPA6   DS    CL30                                                             
TREPA7   DS    CL30                                                             
TREPAX   DS    CL1                                                              
*                                                                               
SAVERE   DS    F                                                                
HEAD966  DS    CL8                                                              
*                                                                               
STDCOMSW DS    CL1                                                              
RFSTCSV  DS    CL1                                                              
REPLINED DS    C            NUMBER OF LINES NEEDED TO PRINT REPS                
ADRTYP   DS    CL1          TYPE OF ADDRESS REC - PAY, TRAFFIC, ETC.            
CLTDATA  DS    0CL7                                                             
CLTAGY   DS    CL2                                                              
CLTMED   DS    CL1                                                              
CLTCODE  DS    CL3                                                              
CLTOFF   DS    CL1                                                              
*                                                                               
ACONIO1  DS    A                   PCONREC ADDRESS FROM PPG                     
*                                                                               
ASCOMTAB DS    A                                                                
ANXTSCOM DS    A                                                                
         DS    A                                                                
APRTCOM  DS    A                                                                
ASINSTAB DS    A                                                                
ATLOUT   DS    A                                                                
*                                                                               
VOFFICER DS    V                                                                
VPRNTOFC DS    V                                                                
*                                                                               
BSPARS   DS    0F                                                               
BSPAR1   DS    F                                                                
BSPAR2   DS    F                                                                
BSPAR3   DS    F                                                                
BSPAR4   DS    F                                                                
BSPAR5   DS    F                                                                
BSPAR6   DS    F                                                                
*                                                                               
NOJOB    DS    CL1                                                              
MYCOPY   DS    CL17                                                             
MYCAP1   DS    CL25                                                             
MYCAP2   DS    CL25                                                             
MYCAPEND DS    CL1                                                              
*                                                                               
SAVMID   DS    CL20                                                             
SAVEMMM  DS    CL80                                                             
*                                                                               
PPBYOWRK DS    CL600                                                            
*                                                                               
COMTAB   DS    26CL44       COPY + 2 CAPTIONS + 2 ALLOS + 5 COMS                
*                           1 POSITION DESCRIPTION + 5 POS COMMENTS             
*                           PLUS 1 FOR DOUBLE SPACING +                         
*                           2ND DATE + FSI DATA + REF DATA +                    
*                           EXTENSION DAYS (12/23/98) OR                        
*                           EXTENSION DATE (01/31/01)                           
COMTABX  DS    CL1                                                              
         SPACE 3                                                                
SORTRECD DSECT                                                                  
SORTREC  DS    0CL81                                                            
SORTKEY  DS    0CL76                                                            
SKOFF    DS    CL1                                                              
SKCLT    DS    CL3                                                              
SKPRD    DS    CL3                                                              
SKEST    DS    XL2                                                              
SKJOB    DS    CL6                                                              
SKMARK   DS    CL20                                                             
SKMCLT   DS    CL3                                                              
SKMPRD   DS    CL3                                                              
SKMEST   DS    XL2                                                              
SKMJOB   DS    CL6                                                              
SKPUB    DS    CL19                                                             
SKDAT1   DS    CL3                                                              
SKDAT2   DS    CL3                                                              
SKLIN    DS    CL2                 TO PRESERV ORIG SEQ                          
SCHGSW   DS    CL1                                                              
SRECDA   DS    CL4                                                              
         SPACE 3                                                                
*                             LINE DSECT                                        
TLLIND   DSECT                                                                  
         DS    CL1                                                              
TLCLT    DS    CL3                                                              
         DS    CL1                                                              
TLPRD    DS    CL3                                                              
         DS    CL1                                                              
TLJOB    DS    CL6                                                              
         DS    CL1                                                              
TLPUB    DS    CL15                                                             
         DS    CL1                                                              
TLPNAME  DS    CL20                                                             
         DS    CL1                                                              
TLIDAT   DS    CL12                                                             
         DS    CL1                                                              
TLCDAT   DS    CL8                                                              
         DS    CL1                                                              
TLEST    DS    CL3                                                              
         DS    CL1                                                              
TLSPACE  DS    CL17                                                             
         DS    CL1                                                              
TLIODAT  DS    CL8                                                              
         DS    CL1                                                              
TLIOTYP  DS    CL3                                                              
         DS    CL1                                                              
TLREFNO  DS    CL12                                                             
         DS    CL1                                                              
TLRPTDT  DS    CL8                                                              
         SPACE 3                                                                
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPMODEQU                                                       
*                                                                               
       ++INCLUDE PPREPWORK                                                      
*                                                                               
         PRINT ON                                                               
PPWORKD  DSECT                                                                  
         ORG   QREGION                                                          
QJOB     DS    CL6                                                              
         ORG                                                                    
         PRINT ON                                                               
*                                                                               
       ++INCLUDE PPREPWORK2                                                     
         PRINT ON                                                               
         ORG   Q2USER                                                           
QCOMM    DS    CL6                                                              
         ORG                                                                    
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
*                                                                               
         PRINT ON                                                               
PGETADRD DSECT                                                                  
       ++INCLUDE PPGETADRD                                                      
         EJECT                                                                  
PCHGELD  DSECT                                                                  
       ++INCLUDE PCHGELEM                                                       
         EJECT                                                                  
PBFSIELD DSECT                                                                  
       ++INCLUDE PBFSIEL                                                        
PXDAYELD DSECT                                                                  
       ++INCLUDE PEXDAYEL                                                       
PXDATELD DSECT                                                                  
       ++INCLUDE PEXDATEL                                                       
         SPACE 3                                                                
PBSHPD   DSECT                                                                  
       ++INCLUDE PBSHPDEL                                                       
WORKIOD  DSECT                                                                  
       ++INCLUDE PPGENBYIO                                                      
*                                                                               
       ++INCLUDE DDCOMFACSD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023PPREP7702 12/17/07'                                      
         END                                                                    
