*          DATA SET SPREP4102  AT LEVEL 050 AS OF 04/13/20                      
*PHASE SP4102A                                                                  
*INCLUDE OFFOUT                                                                 
*INCLUDE DLFLD                                                                  
                                                                                
         TITLE 'SP41-02  CLT/PRD HEADER PRNT'                                   
*============================================================                   
* 21OCT19 MHER  LVL 52   MAKE REPORT OUTPUT DOWNLOADABLE                        
*============================================================                   
                                                                                
SP041    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SP41**                                                       
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SP041+4096,RC                                                    
*                                                                               
         CLI   MODE,RUNFRST                                                     
         JE    EXIT                                                             
         CLI   MODE,RUNLAST                                                     
         JE    EXIT                                                             
*                                                                               
         CLI   MODE,REQLAST                                                     
         JNE   PRC2                                                             
         CLI   DOWNLOAD,C'Y'       TEST DOWNLOAD REPORT OPEN                    
         JNE   EXIT                                                             
*                                                                               
D        USING DLCBD,DLCB                                                       
         MVI   D.DLCBACT,C'R'      SET E-O-R                                    
         GOTO1 =V(DLFLD),DLCB                                                   
         J     EXIT                                                             
*                                                                               
PRC2     CLI   MODE,REQFRST                                                     
         BNE   PRC6                                                             
*                                                                               
         MVI   DOWNLOAD,C'N'                                                    
         L     RE,VMASTC                                                        
         USING MASTD,RE                                                         
         L     RF,MCVREMOT         GET REMOTED ADDRESS                          
         USING REMOTED,RF                                                       
         TM    REMOTTYP,X'18'      TEST OUTPUT TYPE = DOWN OR SQL               
         BZ    *+8                                                              
         MVI   DOWNLOAD,C'Y'                                                    
         DROP  RE,RF                                                            
*                                                                               
         CLI   DOWNLOAD,C'Y'                                                    
         JNE   PRC4                                                             
*                                                                               
         XC    DLCB,DLCB                                                        
         MVI   D.DLCBACT,C'I'      START AND INITIALIZE REPORT                  
         MVC   D.DLCBAPR,=A(BLPRINT) PRINT ROUTINE ADDRESS                      
         LA    R0,P                                                             
         ST    R0,D.DLCBAPL        PRINT LINE ADDRESS                           
         OI    D.DLCBFLG1,DLCBFXTN                                              
         MVC   D.DLCXTND(7),MAXLINE                                             
*                                                                               
         GOTO1 =V(DLFLD),DLCB                                                   
*                                                                               
         MVC   DUB(8),=CL8'T00A'                                                
         MVI   BYTE,QEDITOR                                                     
         GOTO1 HEXOUT,DMCB,BYTE,DUB+4,1,0                                       
*                                                                               
         GOTO1 LOADER,DMCB,DUB,0                                                
         OC    4(4,R1),4(R1)                                                    
         JZ    *+2                                                              
         MVC   D.DLCBAED,4(R1)     DLFLD REQUIRES A(EDITOR)                     
*                                                                               
PRC4     XC    WORK(16),WORK       READ 00A PROFILE                             
         MVC   WORK(4),=C'S00A'                                                 
         NI    WORK,X'BF'          MAKE 'S' LOWERCASE                           
         MVC   WORK+4(2),AGY                                                    
         LA    R0,SV00APRF                                                      
         GOTO1 GETPROF,DMCB,WORK,(R0),DATAMGR                                   
*                                                                               
         LA    R1,MYHOOK                                                        
         ST    R1,HEADHOOK                                                      
         NI    DMINBTS,X'F7'       NO DELETES                                   
         CLI   QOPT3,C' '          SEE IF TURNAROUND                            
         BE    PRC6                                                             
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         B     PRC6                                                             
*                                                                               
PRC6     MVI   RQPRDAAA,C'Y'       INCLUDE PRODUCT AAA                          
         CLI   QOPT1,C'C'          * CLIENT                                     
         BNE   PRC10                                                            
         MVC   QPRD,=C'ALL'        SET UP REQ CARD FOR SPOOO3                   
         MVC   QEST,=C'ALL'        (NEEDS PRD,EST,ANY DATE)                     
         MVC   QSTART(12),=C'010180010181'                                      
         B     PRTCLT                                                           
*                                                                               
         SPACE                                                                  
PRC10    CLI   QOPT1,C'P'          * PRODUCT                                    
         BNE   PRC30                                                            
         MVC   QEST,=C'ALL'            SET UP REQ CARD FOR SP0003               
         MVC   QSTART,=C'010180010181'    (NEED EST,ANY DATE)                   
         CLI   MODE,REQFRST                                                     
         BNE   CHKLAST                                                          
         CLI   QOPT3,C' '              SEE IF TURNAROUND                        
         BNE   PRTCLT                                                           
*                                                                               
PRC10C   CLC   QPRD,=C'ALL'            REQUEST FOR ONE PRODUCT                  
         BNE   PRTCLT                                                           
         MVI   RQALLPOL,C'Y'           TO READ POL WITH PRDS                    
         MVI   FORCEHED,C'Y'                                                    
         XC    PRVAMC,PRVAMC                                                    
         B     EXIT                                                             
         SPACE                                                                  
CHKLAST  CLI   MODE,CLTLAST                                                     
         BNE   PRTCLT                                                           
         CLC   QPRD,=C'ALL'                                                     
         BNE   EXIT                                                             
         XC    PRVAMC,PRVAMC          SO TURNAROUNDS AND REQ FOR                
*                                     ONE PRD WILL BE ON NEXT PAGE              
PRC30    B     EXIT                                                             
         EJECT                                                                  
* PROCESS CLIENT HEADER *                                                       
*                                  CLT NAME & CD,OFFICE,ACTIVITY                
PRTCLT   CLI   QOPT1,C'C'          * CLIENT REPORT?                             
         BE    PRTC0               YES - GO TO IT                               
         CLI   MODE,CLTFRST        PRD REPORT --CLIENT MODE?                    
         BNE   PRTPRD              NO - TRY PRODUCT MODE                        
*                                                                               
         BRAS  RE,GETOFFC                                                       
*                                                                               
         L     R8,ADCLT                                                         
         USING CLTHDRD,R8                                                       
*                                                                               
         MVC   CUSER1,CPU1        SAVE USER DESCRIPTION FIELDS                  
         MVC   CUSER2,CPU2                                                      
         B     EXIT                                                             
         DROP  R8                                                               
*                                                                               
PRTC0    CLI   MODE,CLTFRST                                                     
         BNE   EXIT                                                             
*                                                                               
         MVI   RCSUBPRG,10                                                      
         MVI   FORCEHED,C'Y'                                                    
         L     R8,ADCLT                                                         
         USING CLTHDRD,R8                                                       
         CLI   QOPT3,C' '          SEE IF TURNAROUND                            
         BNE   PC0                YES                                           
         TM    CCNTRL,X'80'       SEE IF DELETED                                
         BO    EXIT               SKIP DELETES                                  
*                                                                               
PC0      L     R8,ADCLT                                                         
         USING CLTHDRD,R8                                                       
         MVC   P+1(8),=C'CLIENT -'                                              
         MVC   P+10(3),CLT                                                      
         MVC   P+14(20),CNAME                                                   
         MVC   DLCLT,CLT                                                        
         MVC   DLCNAME,CNAME                                                    
*                                                                               
         BRAS  RE,GETOFFC                                                       
*                                                                               
         MVC   P+40(8),=C'OFFICE -'                                             
         MVC   P+49(2),CLTOFF                                                   
*                                                                               
         CLI   TWOCHARS,C'Y'       TEST ON 2 CHAR OFFICES                       
         BE    PC10                YES - NO HEX PRINT EVER                      
*                                                                               
         LARL  RE,SV00APRF                                                      
         CLI   2(RE),C'Y'          TEST TO PRINT AS CHAR                        
         BE    PC10                                                             
         GOTO1 =V(OFFOUT),DMCB,COFFICE,HEXOUT,P+49                              
*                                                                               
PC10     MVC   DLCOFFC,P+49                   ACC OFFICE CODE                   
*                                                                               
         MVC   DLCACOFC,SPACES                                                  
         CLI   CACCOFC,0                      ACC OFFICE CODE                   
         BE    PC12                                                             
         MVC   P+52(12),=C'ACC OFFICE -'                                        
         MVC   P+65(2),CACCOFC                                                  
         MVC   DLCACOFC,P+65                                                    
*                                                                               
PC12     MVC   DLCINTFC,SPACES                                                  
         CLC   CCLTIFC,SPACES                 IS THERE INTERFACE NO.            
         BNH   PC20                                                             
         MVC   P+69(16),=C'INTERFACE NUMBER'                                    
         MVC   P+86(8),CCLTIFC                                                  
         MVC   DLCINTFC,P+86                                                    
*                                                                               
PC20     CLI   QOPT3,C'C'                                                       
         BNE   PC30                                                             
         MVC   P+125(7),=C'CHANGED'                                             
         B     PC40                                                             
PC30     CLI   QOPT3,C'A'                                                       
         BNE   PC40                                                             
         MVC   P+125(5),=C'ADDED'                                               
PC40     MVI   SPACING,2                                                        
         BRAS  RE,PRTIT                                                         
         TM    CCNTRL,X'80'             SEE IF DELETED                          
         BNO   PC42                                                             
         MVC   P+120(11),=C'**DELETED**'                                        
         BRAS  RE,PRTIT                                                         
PC42     DS    0H                                                               
*                                                                               
         MVC   P+4(14),=C'CLIENT OPTIONS'        * CLIENT OPTIONS               
         BRAS  RE,PRTIT                                                         
         MVC   P+4(14),=C'--------------'                                       
         MVI   SPACING,1                                                        
         BRAS  RE,PRTIT                                                         
*                                                                               
         LA    R2,CEXTRA            * EXTRA PROFILES                            
         CLI   CEXTRA+2,0                                                       
         BNE   *+8                                                              
         MVI   CEXTRA+2,C'N'       CHANGE 0 TO N ON DISPLAY                     
         CLI   CEXTRA+3,C'0'                                                    
         BNE   *+8                                                              
         MVI   CEXTRA+3,C'N'                                                    
         CLI   CEXTRA+5,C'0'                                                    
         BNE   *+8                                                              
         MVI   CEXTRA+5,C'N'       CHANGE 0 TO N ON DISPLAY                     
         CLI   CEXTRA+6,C'0'                                                    
         BNE   *+8                                                              
         MVI   CEXTRA+6,C'N'       CHANGE 0 TO N ON DISPLAY                     
         CLI   CEXTRA+7,C'0'                                                    
         BNE   *+8                                                              
         MVI   CEXTRA+7,C'N'       CHANGE 0 TO N ON DISPLAY                     
         CLI   CEXTRA+8,C'0'                                                    
         BNE   *+8                                                              
         MVI   CEXTRA+8,C'N'       CHANGE 0 TO N ON DISPLAY                     
*                                                                               
         CLI   CEXTRA+9,C'0'                                                    
         BE    *+12                                                             
         CLI   CEXTRA+9,C'N'                                                    
         BNE   PC44                                                             
         MVI   CEXTRA+9,C'U'                                                    
         L     R1,ADAGY                                                         
         CLI   AGYPROF+7-AGYHDR(R1),C'C'                                        
         BNE   PC44                                                             
         MVI   CEXTRA+9,C'C'                                                    
*                                                                               
PC44     CLI   CEXTRA+10,C'0'     OUT OF WEEK CLT                               
         BNE   PC46                                                             
         MVI   CEXTRA+10,C'N'     CHANGE 0 TO N ON DISPLAY                      
*                                                                               
PC46     CLI   CEXTRA+11,C'0'     GST                                           
         BNE   PC48                                                             
         MVI   CEXTRA+11,C'S'     CHANGE 0 TO S ON DISPLAY                      
*                                                                               
PC48     CLI   CEXTRA+12,C'0'     SPECIAL DEMO ADJ                              
         BNE   PC60                                                             
         MVI   CEXTRA+12,C'N'     CHANGE 0 TO N ON DISPLAY                      
         SPACE 2                                                                
PC60     LA    R2,ALLPROFS         R2 - CLT PROFILES                            
         MVC   0(15,R2),CPROF                                                   
         MVC   15(15,R2),CEXTRA                                                 
*                                                                               
         LARL  R3,COPTS            R3 - CLT OPTIONS                             
PC70     LA    R4,3                R4 - BCT LIMIT(3 PROFS PER P LINE)           
         LA    R5,P+4              R5 - P LINE                                  
         SPACE                                                                  
PC80     MVC   0(25,R5),0(R3)                                                   
         MVC   26(1,R5),0(R2)                                                   
         SPACE                                                                  
         LA    R2,1(R2)                                                         
         LA    R3,25(R3)                                                        
         CLI   0(R3),X'FF'         END OF OPTIONS                               
         BE    PC90                                                             
         LA    R5,35(R5)                                                        
         SPACE                                                                  
         BCT   R4,PC80                                                          
         BRAS  RE,PRTIT                                                         
         B     PC70                                                             
*                                                                               
PC90     MVC   35(3,R5),=C'PST'                                                 
         OC    CPST,CPST              ANY PST TO DISPLAY?                       
         BZ    PC92                   NO - SKIP PSTVAL CALL                     
         LA    R1,CPST                                                          
         BAS   RE,DISPPST                                                       
         MVC   39(29,R5),PSTOUT       OUTPUT - ONLY DISPLAY 6 PST'S             
*                                                                               
PC92     MVC   70(8,R5),=C'MAIN PST'  MAIN PST                                  
         CLI   CMPST,0                HAVE MAIN PST?                            
         BE    PC94                   NO                                        
         XC    WORK(10),WORK          BUILD 10 CHAR PST STRING IN WORK          
         LLC   RE,CMPST               INDEX INTO PST STRING +1                  
         BCTR  RE,0                   -1 FOR TRUE INDEX (1=0)                   
         LA    R1,WORK                POINT TO PST STRING FOR DISPPST           
         AR    RE,R1                  INDEX INTO WORK                           
         MVC   0(1,RE),CMPST+1        MOVE IN THE PST VALUE                     
         BAS   RE,DISPPST             CALL PSTVAL                               
         MVC   79(4,R5),PSTOUT        OUTPUT - ONLY DISPLAY 1 PST               
*                                                                               
PC94     MVI   SPACING,2                                                        
         BRAS  RE,PRTIT                                                         
*                                                                               
PC100    DS    0H                       * ID TITLE                              
         MVC   P+4(8),=C'ID TITLE'                                              
         BRAS  RE,PRTIT                                                         
         MVC   P+4(8),=C'--------'                                              
         BRAS  RE,PRTIT                                                         
         MVC   P+4(10),CTITLE                                                   
         MVI   SPACING,2                                                        
         BRAS  RE,PRTIT                                                         
         SPACE 3                                                                
*                                   * USER DEFINITION FIELDS                    
         BRAS  RE,PRTIT                                                         
         MVC   P+4(22),=C'USER DEFINITION FIELDS'                               
         BRAS  RE,PRTIT                                                         
         MVC   P+4(22),=C'----------------------'                               
         BRAS  RE,PRTIT                                                         
*                                                                               
         MVC   P+15(6),=C'DESC 1'                                               
         MVC   P+36(29),=C'REQ  EDIT  LEN  BILLS  A2  MX'                       
         MVC   P+70(6),=C'DESC 2'                                               
         MVC   P+91(29),=C'REQ  EDIT  LEN  BILLS  A2  MX'                       
         BRAS  RE,PRTIT                                                         
         MVC   P+15(6),=C'------'                                               
         MVC   P+36(29),=C'---  ----  ---  -----  --  --'                       
         MVC   P+70(6),=C'------'                                               
         MVC   P+91(29),=C'---  ----  ---  -----  --  --'                       
         BRAS  RE,PRTIT                                                         
         MVC   P+4(7),=C'PRODUCT'                                               
         LA    R3,CPU1                                                          
         BAS   RE,SETUSER                                                       
*                                                                               
         MVC   P+4(8),=C'ESTIMATE'                                              
         LA    R3,CEU1                                                          
         BAS   RE,SETUSER                                                       
         BRAS  RE,PRTIT                                                         
                                                                                
*                                   * CLIENT PRODUCTS                           
*                                     PRINT PRDS THAT HAVE PRDHDR               
         MVC   P+4(15),=C'CLIENT PRODUCTS'                                      
         BRAS  RE,PRTIT                                                         
         MVC   P+4(15),=C'---------------'                                      
         MVI   SPACING,1                                                        
         BRAS  RE,PRTIT                                                         
                                                                                
         MVC   KEY,CKEY            SET UP KEY FOR PRDHDR, THEN                  
         DROP  R8                                                               
*                                                                               
         L     R7,ADPRD              LOAD EC WITH NEW ADDRS.                    
         USING PRDHDRD,R7                                                       
NXTLIN   LA    R3,P+4                                                           
         LA    R4,5                                                             
NXTPRD   MVC   KEY+7(2),=X'FFFF'                                                
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      TEST SAME CLT                                
         BNE   PCEND                                                            
         GOTO1 GETPRD                                                           
         MVC   0(3,R3),PKEY+4                                                   
         MVI   3(R3),C'-'                                                       
         MVC   4(20,R3),PNAME                                                   
         LA    R3,26(R3)                                                        
         BCT   R4,NXTPRD                                                        
         BRAS  RE,PRTIT                                                         
         B     NXTLIN                                                           
PCEND    BRAS  RE,PRTIT                                                         
         SPACE                                                                  
         MVI   MODE,CLTLAST        TO GET NXT CLT                               
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
*        PRINT USER FIELDS                                                      
*                                                                               
SETUSER  NTR1                                                                   
         USING UDEFD,R3                                                         
         CLC   USER,SPACES                                                      
         BNH   SET10                                                            
         MVC   P+15(20),USER                                                    
         MVI   P+37,C'N'                                                        
         TM    FLAG1,CFLGREQQ                                                   
         BNO   *+8                                                              
         MVI   P+37,C'Y'                                                        
         MVC   P+42(1),TYPE                                                     
         EDIT  LEN,(2,P+47)                                                     
         MVI   P+54,C'N'                                                        
         TM    FLAG1,CFLGSPQ                                                    
         BNO   *+8                                                              
         MVI   P+54,C'Y'                                                        
*                                                                               
         MVI   P+59,C'N'                                                        
         TM    FLAG1,CFLGA2Q                                                    
         BNO   *+8                                                              
         MVI   P+59,C'Y'                                                        
*                                                                               
         MVI   P+63,C'N'                                                        
         TM    FLAG1,CFLGMXQ                                                    
         BNO   *+8                                                              
         MVI   P+63,C'Y'                                                        
*                                                                               
SET10    LA    R3,USERLNQ(R3)                                                   
         CLC   USER,SPACES                                                      
         BNH   SETX                                                             
         MVC   P+70(20),USER                                                    
         MVI   P+92,C'N'                                                        
         TM    FLAG1,CFLGREQQ                                                   
         BNO   *+8                                                              
         MVI   P+92,C'Y'                                                        
         MVC   P+97(1),TYPE                                                     
         EDIT  LEN,(2,P+102)                                                    
         MVI   P+109,C'N'                                                       
         TM    FLAG1,CFLGSPQ                                                    
         BNO   *+8                                                              
         MVI   P+109,C'Y'                                                       
*                                                                               
         MVI   P+114,C'N'                                                       
         TM    FLAG1,CFLGA2Q                                                    
         BNO   *+8                                                              
         MVI   P+114,C'Y'                                                       
*                                                                               
         MVI   P+118,C'N'                                                       
         TM    FLAG1,CFLGMXQ                                                    
         BNO   *+8                                                              
         MVI   P+118,C'Y'                                                       
*                                                                               
SETX     BRAS  RE,PRTIT                                                         
         XIT1                                                                   
         EJECT                                                                  
*==========================================================                     
* PRODUCT HEADERS                                                               
* NOTE DOWNLOAD FIELDS ARE NOT CLEARED BECAUSE THEY ARE                         
* EITHER ALL FILLED IN OR WILL BE CLEARED AS NEEDED                             
*==========================================================                     
                                                                                
PRTPRD   DS 0H                                                                  
         CLI   MODE,PRDFRST                                                     
         BNE   EXIT                                                             
* NEED TO FILL IN SOME CLIENT CODE FIELDS FOR DOWNLOAD                          
         L     R8,ADCLT                                                         
         USING CLTHDRD,R8                                                       
         MVC   DLMED,MED                                                        
         MVC   DLCLT,CLT                                                        
         MVC   DLCNAME,CNAME                                                    
         MVC   CUSER1,CPU1         SAVE USERDEF TITLES                          
         MVC   CUSER2,CPU2                                                      
         DROP  R8                                                               
*                                                                               
PP02     L     R8,ADPRD                                                         
         USING PRDHDRD,R8                                                       
         OC    PKEY+4(3),PKEY+4    SEE IF THIS IS A PRODUCT RECORD              
         BZ    EXIT           SINCE SP0003 WILL READ CLIENT HEADER              
*                             HERE IF THERE ARE NO PRODUCTS                     
*                                                                               
         CLI   QOPT3,C' '          SEE IF TURNAROUND                            
         BNE   PP04                                                             
         TM    PCNTRL,X'80'        SEE IF DELETED                               
         BO    EXIT                                                             
*                                                                               
PP04     MVI   RCSUBPRG,20         PROCESS PRODUCT HEADER                       
         MVI   ALLOWLIN,4                                                       
         CLC   PKEYAM(3),PRVAMC       CHK FOR SAME AGY/MED/CLT                  
         BE    PP05                                                             
         MVC   PRVAMC,PKEYAM                                                    
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PP05     OC    CLTOFF,CLTOFF                                                    
         BZ    PP06                                                             
         MVC   HEAD3+59(9),=C'OFFICE - '                                        
         MVC   HEAD3+68(2),CLTOFF                                               
*                                                                               
PP06     LA    R2,P                                                             
         USING PLINED,R2                                                        
*                                                                               
         MVC   PROD,PKEY+4         * HDR INFO TO P LINE                         
         MVC   DLPRD,PKEY+4                                                     
         MVC   NAME,PNAME                                                       
         MVC   DLPNAME,PNAME                                                    
         OC    DLPNAME,SPACES                                                   
         MVC   ACCT(4),PACCT                                                    
         CLI   PACCT,X'FF'                                                      
         BNE   *+10                                                             
         UNPK  ACCT(5),PACCT+1(3)                                               
         MVC   DLPACCT,ACCT                                                     
                                                                                
         MVC   DLPCLASS(2),SPACES                                               
         CLI   PCLASS,0            TEST ANY PRODUCT CLASS                       
         BE    PP08B               NO                                           
         MVC   CLASS(1),PCLASS                                                  
         CLI   PCLASS,X'99'        TEST 2 CLASSES                               
         BH    PP08A               NO                                           
         PACK  CLASS(1),PCLASS     FIRST CLASS                                  
         NI    CLASS,X'0F'                                                      
         OI    CLASS,X'C0'         MAKE A - I                                   
         MVC   CLASS+1(1),PCLASS   2ND CLASS                                    
         NI    CLASS+1,X'0F'                                                    
         OI    CLASS+1,X'C0'       MAKE A - I                                   
PP08A    MVC   DLPCLASS(2),CLASS   MOVE FORMATTED CLASS IDIOT!                  
*                                                                               
PP08B    MVC   DLBILLDT,SPACES               * BILL DATE                        
         OC    PBILLDT,PBILLDT                                                  
         BZ    PP09                                                             
         LARL  R1,MONTAB                                                        
         LLC   R0,PBILLDT+1                  EFF MON                            
         BCTR  R0,0                                                             
         MHI   R0,3                                                             
         AR    R1,R0                                                            
         MVC   BDATE(3),0(R1)                                                   
         MVI   BDATE+3,C'/'                                                     
         SR    R1,R1                                                            
         IC    R1,PBILLDT                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BDATE+4(2),DUB                                                   
         MVC   DLBILLDT,BDATE                                                   
*                                                                               
PP09     MVC   DLBILFRM,SPACES                                                  
         OC    PBILLBAS(5),PBILLBAS          * BILL FORMULA                     
         BZ    PP09E                                                            
         MVI   BFORM,C'G'                                                       
         TM    PBILLBAS,X'10'                                                   
         BZ    *+8                                                              
         MVI   BFORM,C'N'                                                       
         MVI   BFORM+1,C'+'                                                     
         L     R0,PBILLCOM                                                      
         LTR   R0,R0                                                            
         BNM   *+8                                                              
         MVI   BFORM+1,C'-'                                                     
         SPACE                                                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BFORM+2(7),DUB                                                   
         MVC   BFORM+2(2),BFORM+3                                               
         MVI   BFORM+4,C'.'                                                     
         LA    R1,BFORM+8                                                       
         LA    R0,3                                                             
PP09B    CLI   0(R1),C'0'          SUPPRESS TRAILING ZEROS                      
         BNE   PP09D                                                            
         MVI   0(R1),0                                                          
         BCTR  R1,0                                                             
         BCT   R0,PP09B                                                         
PP09D    MVI   1(R1),C'G'                                                       
         TM    PBILLBAS,X'01'                                                   
         BZ    *+8                                                              
         MVI   1(R1),C'N'                                                       
         MVC   DLBILFRM,BFORM                                                   
         OC    DLBILFRM,SPACES                                                  
*                                                                               
PP09E    MVC   DLADDCHG,SPACES                                                  
         CLI   QOPT3,C'C'                                                       
         BNE   PP10                                                             
         MVC   CHGED,=C'CHANGE'                                                 
         MVC   DLADDCHG,=C'CHG'                                                 
         B     PP12                                                             
*                                                                               
PP10     CLI   QOPT3,C'A'                                                       
         BNE   PP12                                                             
         MVC   CHGED(5),=C'ADDED'                                               
         MVC   DLADDCHG,=C'ADD'                                                 
*                                                                               
PP12     MVC   ADDR,PADDR1                                                      
*                                                                               
         MVC   DLADDR1,PADDR1                                                   
         MVC   DLADDR2,PADDR2                                                   
         MVC   DLADDR3,PADDR3                                                   
         MVC   DLADDR4,PADDR4                                                   
         LA    R1,DLADDR1                                                       
         LA    R0,4                                                             
         OC    0(30,R1),SPACES                                                  
         LA    R1,30(R1)                                                        
         BCT   R0,*-10                                                          
*                                                                               
         MVC   GST,PGSTCODE       MOVE IN GST CODE                              
         MVC   DLGST,PGSTCODE                                                   
         OC    DLGST,SPACES                                                     
*                                                                               
         MVC   DLUTTL1,SPACES                                                   
         MVC   DLUTTL2,SPACES                                                   
         MVC   DLUDTA1,SPACES                                                   
         MVC   DLUDTA2,SPACES                                                   
*                                                                               
         OC    CUSER1,CUSER1      IS THERE A DESCRIPTION                        
         BZ    PP12A                                                            
         MVC   USERDEF,PUSER1                                                   
         MVC   DLUTTL1,CUSER1                                                   
         OC    DLUTTL1,SPACES                                                   
         MVC   DLUDTA1,PUSER1                                                   
         OC    DLUDTA1,SPACES                                                   
*                                                                               
PP12A    OC    CUSER2,CUSER2      IS THERE A SECOND                             
         BZ    PP12B                                                            
         CLI   DOWNLOAD,C'Y'                                                    
         JNE   PP12B                                                            
         MVC   DLUTTL2,CUSER2                                                   
         OC    DLUTTL2,SPACES                                                   
         MVC   DLUDTA2,PUSER2                                                   
         OC    DLUDTA2,SPACES                                                   
*                                                                               
PP12B    LH    R5,PRDCNT                                                        
         LA    R5,1(R5)                                                         
         STH   R5,PRDCNT                                                        
         CH    R5,=H'8'                                                         
         BNH   PP13                                                             
         MVC   PRDCNT(2),=X'0001'                                               
         MVI   SPACING,2                                                        
         B     PP13+4                                                           
PP13     MVI   SPACING,1                                                        
         BRAS  RE,PRTIT                                                         
         TM    PCNTRL,X'80'             SEE IF DELETED                          
         BNO   PC13C                                                            
         MVC   CHGED,=C'DELETED'                                                
         BRAS  RE,PRTIT                                                         
PC13C    DS    0H                                                               
         CLC   PADDR2(30),=30C' '                                               
         BE    PP14                                                             
         MVC   ADDR,PADDR2                                                      
         BRAS  RE,PRTIT                                                         
*                                                                               
PP14     OC    CUSER2,CUSER2      IF THERE ARE 2 USER DEF FIELDS                
         BZ    PP14B                                                            
         OC    PUSER2,PUSER2      PUT 2ND ONE HERE                              
         BZ    PP14B                                                            
         MVC   USERDEF(16),CUSER2  TITLE FOR USER DATA 2                        
         MVI   USERDEF+16,C'='                                                  
         MVC   USERDEF+17(16),PUSER2                                            
*                                                                               
PP14B    CLC   PADDR3(30),=30C' '                                               
         BE    PP15                                                             
         MVC   ADDR,PADDR3                                                      
         BRAS  RE,PRTIT                                                         
         SPACE                                                                  
PP15     CLC   PADDR4(30),=30C' '                                               
         BE    PP16                                                             
         MVC   ADDR,PADDR4                                                      
         BRAS  RE,PRTIT                                                         
         SPACE                                                                  
PP16     MVC   DLPST,SPACES                                                     
         MVC   DLMAINPST,SPACES                                                 
         OC    PPST,PPST                                                        
         BZ    PP17                                                             
         LA    R1,PPST                                                          
         BAS   RE,DISPPST                                                       
         MVC   ADDR(3),=C'PST'                                                  
         MVC   ADDR+4(29),PSTOUT   OUTPUT                                       
         MVC   DLPST,PSTOUT                                                     
         OC    DLPST,SPACES                                                     
         BRAS  RE,PRTIT                                                         
*                                                                               
PP17     CLI   PMPST,0                SET THE MAIN PST STRING IN WORK           
         BE    PP18                   NO                                        
         XC    WORK(10),WORK          BUILD 10 CHAR PST STRING IN WORK          
         LLC   RE,PMPST               INDEX INTO PST STRING +1                  
         BCTR  RE,0                   -1 FOR TRUE INDEX (1=0)                   
         LA    R1,WORK                POINT TO PST STRING FOR DISPPST           
         AR    RE,R1                  INDEX INTO WORK                           
         MVC   0(1,RE),PMPST+1        MOVE IN THE PST VALUE                     
         BAS   RE,DISPPST             CALL PSTVAL                               
         MVC   ADDR(8),=C'MAIN PST'   MAIN PST                                  
         MVC   ADDR+9(4),PSTOUT       OUTPUT                                    
         MVC   DLMAINPST,PSTOUT                                                 
         OC    DLMAINPST,SPACES                                                 
         BRAS  RE,PRTIT                                                         
*                                                                               
PP18     BRAS  RE,PRTIT            SKIP LINE AFTER EACH PRDHDR                  
         MVI   MODE,PRDLAST                                                     
         BRAS  RE,OUTPUT                                                        
         B     EXIT                                                             
         EJECT                                                                  
*=============================================================                  
* CALL OFFICER FOR 2 CHAR OFFICE CODE IF AVAILABLE                              
*=============================================================                  
                                                                                
GETOFFC  NTR1                                                                   
         L     R8,ADCLT            YES GET CLIENT OFFICE AND GET OUT            
         USING CLTHDRD,R8                                                       
         MVC   CLTOFF(1),COFFICE   SAVE                                         
         MVI   CLTOFF+1,C' '                                                    
*                                                                               
         LA    R4,OFCWORK                                                       
         USING OFFICED,R4                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAGY,AGY                                                       
         MVC   OFCOFC,COFFICE                                                   
         L     RF,ADCONLST                                                      
         L     RF,VOFFICER-SPADCONS(RF)                                         
         GOTO1 (RF),DMCB,(C'2',OFFICED),ACOMFACS                                
         CLI   0(R1),0                                                          
         BNE   *+10                                                             
         MVC   CLTOFF,OFCOFC2      USE 2 CHAR OFFICE IF AVAILABLE               
*                                                                               
         MVI   TWOCHARS,C'Y'                                                    
         TM    OFCINDS,OFCINOLA    TEST ON 2-CHAR OFFICES                       
         BZ    *+8                                                              
         MVI   TWOCHARS,C'N'                                                    
         XIT1                                                                   
         DROP  R8                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* INTERCEPT CALLS TO REPORT TO SUPPRESS PRINTING ON DOWNLOADS                   
*===============================================================                
                                                                                
PRTIT    NTR1                                                                   
         CLI   DOWNLOAD,C'Y'       TEST DOWNLOADING                             
         BE    PRTIT2              YES                                          
         GOTO1 REPORT                                                           
         J     EXIT                                                             
*                                                                               
PRTIT2   MVC   P,SPACES            SUPPRESS PRINT LINES FOR NOW                 
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
*        DISPLAY PST CODES                                                      
*        R1 - A(PST CODES)                                                      
*===============================================================                
                                                                                
DISPPST  NTR1                                                                   
         LA    R2,PSTBLK                                                        
         USING PSTBLKD,R2                                                       
         XC    0(PSTLNQ,R2),0(R2)     CLEAR INTERFACE BLOCK                     
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
*                                                                               
         MVC   DUB,SPACES          GET PSTVAL                                   
         MVC   DUB(6),=C'T00A6B'                                                
         GOTO1 LOADER,DMCB,DUB,0                                                
*                                                                               
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,4(R1)                                                         
         GOTO1 (RF),DMCB,(R2)                                                   
*                                                                               
DPX      J     EXIT                                                             
         EJECT                                                                  
*                                                                               
MYHOOK   NTR1                                                                   
         CLI   DOWNLOAD,C'Y'                                                    
         JE    MYHOOK2                                                          
         CLI   RCSUBPRG,20         PROCESS PRODUCT HEADER                       
         JNE   EXIT                                                             
         BAS   RE,PUTHEAD          PUT OUT HEADLINE                             
         J     EXIT                                                             
*                                                                               
MYHOOK2  DS    0H                                                               
         LA    R0,14                                                            
         LA    R1,H1                                                            
         MVC   0(132,R1),SPACES                                                 
         AHI   R1,132                                                           
         JCT   R0,*-10                                                          
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        PUT OUT HEADLINES FOR PRODUCT REPORT                                   
*                                                                               
PUTHEAD  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,H6                                                            
         USING PLINED,R2                                                        
         MVC   PROD(7),=C'PRODUCT'                                              
         MVC   ADDR(21),=C'BILL NAME AND ADDRESS'                               
         MVC   CLASS(3),=C'PCL'                                                 
         MVC   ACCT(4),=C'ACCT'                                                 
         MVC   BDATE(7),=C'BILL DT'                                             
         MVC   BFORM(9),=C'BILL FORM'                                           
         LA    R1,GST                                                           
         BCTR  R1,0                                                             
         MVC   0(3,R1),=C'GST'                                                  
         OC    CUSER1,CUSER1      IS THERE A DESCRIPTION                        
         BZ    PH10                                                             
         MVC   USERDEF(20),CUSER1                                               
         B     PH20                                                             
*                                                                               
PH10     OC    CUSER2,CUSER2      IS THERE A DESCRIPTION                        
         BZ    PH30                                                             
         MVC   USERDEF(20),CUSER2                                               
*                                                                               
PH20     LA    R1,USERDEF+19                                                    
         LA    R5,20               LOOP - WILL HAVE N'USER CHARS                
*                                                                               
PH25     CLI   0(R1),C' '                                                       
         BH    PH30                                                             
         BCTR  R1,0                BACK UP A SPACE                              
         BCT   R5,PH25                                                          
*                                                                               
PH30     LTR   R5,R5                                                            
         BZ    *+6                                                              
         BCTR  R5,0                                                             
         LA    R2,H7                                                            
         MVC   PROD(7),=C'-------'                                              
         MVC   ADDR(21),=C'---------------------'                               
         MVC   CLASS(3),=C'---'                                                 
         MVC   ACCT(4),=C'----'                                                 
         MVC   BDATE(7),=C'-------'                                             
         MVC   BFORM(9),=C'---------'                                           
         LA    R1,GST                                                           
         BCTR  R1,0                                                             
         MVC   0(3,R1),=C'---'                                                  
         OC    CUSER1,CUSER1      IS THERE A DESCRIPTION                        
         BNZ   PH40                                                             
         OC    CUSER2,CUSER2                                                    
         BZ    PHX                                                              
*                                                                               
PH40     EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   USERDEF(0),=C'--------------------'                              
*                                                                               
PHX      J     EXIT                                                             
         EJECT                                                                  
*=============================================================*                 
* OUTPUT DOWNLOAD DATA TO PRTQUE REPORT                                         
*=============================================================*                 
                                                                                
OUTPUT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   DOWNLOAD,C'Y'                                                    
         JNE   EXIT                                                             
*                                                                               
         CLI   DOWNFRST,C'Y'       TEST FIRST TIME                              
         JNE   OUTPUT10                                                         
* SEND COLUMN HEADINGS                                                          
         LA    R4,RECTAB                                                        
         SR    RE,RE                                                            
*                                                                               
OUTPUT2  ICM   RE,7,9(R4)          COLUMN HEAD ADDR                             
         LLC   RF,8(R4)            GET DATA LEN                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   D.DLCBFLD(0),0(RE)                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    D.DLCBFLD(0),SPACES                                              
*                                                                               
         MVI   D.DLCBACT,DLCBPUT                                                
         MVI   D.DLCBTYP,C'T'                                                   
         L     R1,=A(DLCB)                                                      
         GOTO1 =V(DLFLD),(R1)                                                   
         MVI   D.DLCXDELC,C' '       ALWAYS RESTORE TERMINATOR                  
*                                                                               
         LA    R4,L'RECTAB(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   OUTPUT2                                                          
*                                                                               
         MVI   D.DLCBACT,DLCBEOL                                                
         L     R1,=A(DLCB)                                                      
         GOTO1 =V(DLFLD),(R1)                                                   
*                                                                               
         MVI   D.DLCBACT,DLCBEOL                                                
         GOTO1 =V(DLFLD),(R1)                                                   
*                                                                               
OUTPUT10 LA    R0,4                                                             
         LA    R1,P                                                             
         MVC   0(132,R1),SPACES                                                 
         AHI   R1,132                                                           
         JCT   R0,*-10                                                          
*                                                                               
         LA    R4,RECTAB                                                        
*                                                                               
OUTPUT12 SR    RE,RE                                                            
         ICM   RE,7,1(R4)          GET DATA ADDR                                
         LLC   RF,0(R4)            GET DATA LEN                                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   D.DLCBFLD(0),0(RE)                                               
*                                                                               
         CLI   5(R4),C'T'          TEST TEXT                                    
         BNE   OUTPUT14                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    D.DLCBFLD(0),SPACES                                              
         B     OUTPUT28                                                         
*                                                                               
OUTPUT14 CLI   5(R4),C'B'          TEST BINARY                                  
         BNE   OUTPUT24                                                         
         MVC   D.DLCBNDP,7(R4)     SET NUMBER OF DECIMAL PLACES                 
         MVC   D.DLCBLEN,4(R4)     SET DATE LENGTH                              
         B     OUTPUT30                                                         
*                                                                               
* FIXED LEN NUMERIC OUTPUT                                                      
*                                                                               
OUTPUT16 ICM   R0,15,0(RE)         GET VALUE IN R0                              
         MVI   D.DLCBTYP,C'N'      TYPE=NUMERIC                                 
*                                                                               
         CLI   7(R4),1             TEST 1 DECIMAL                               
         BNE   OUTPUT18                                                         
         MVC   WORK(11),=X'4021202020202020204B20'                              
         CVD   R0,DUB                                                           
         ED    WORK(11),DUB+3                                                   
         MVC   D.DLCBFLD(8),WORK+3                                              
         MVI   D.DLCBLEN,8         FIX OUTPUT LEN                               
         B     OUTPUT40                                                         
*                                                                               
OUTPUT18 CLI   7(R4),2             TEST 2 DECIMAL                               
         BNE   OUTPUT22                                                         
*                                                                               
         MVC   WORK(17),=X'40212020202020202020202020204B2020'                  
         LA    R1,DUB                                                           
         LTR   R0,R0                                                            
         BNM   OUTPUT20                                                         
         MVC   WORK(17),=X'404021202020202020202020204B202060'                  
         LA    R1,DUB+1                                                         
*                                                                               
OUTPUT20 CVD   R0,DUB                                                           
         ED    WORK(17),DUB                                                     
         MVC   D.DLCBFLD(13),WORK+4                                             
         MVI   D.DLCBLEN,13                                                     
         B     OUTPUT40                                                         
*                                                                               
OUTPUT22 DC    H'0'                                                             
*                                                                               
OUTPUT24 TM    7(R4),X'01'         TEST CVD REQUIRED                            
         BZ    OUTPUT26                                                         
         ICM   R0,15,0(RE)         GET DATA VALUE                               
         CVD   R0,DUB                                                           
         LTR   R0,R0                                                            
         BM    *+8                                                              
         OI    DUB+7,X'0F'                                                      
         SLL   RF,4                SET LEN TO UNPK TO                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  D.DLCBFLD(0),DUB                                                 
         B     OUTPUT30                                                         
*                                                                               
OUTPUT26 CLI   5(R4),C'X'          TEST HEX                                     
         BNE   OUTPUT28                                                         
*                                                                               
OUTPUT28 CLI   6(R4),0             TEST FIELD CAN END RECORD                    
         BE    OUTPUT30            NO                                           
         CLC   D.DLCBFLD(1),6(R4)  ELSE COMPARE                                 
         BNH   OUTPUT42            AND POSSIBLY END                             
*                                                                               
OUTPUT30 MVC   D.DLCBTYP(1),4(R4)                                               
         CLI   4(R4),C'X'          TEST HEX OUTPUT                              
         BNE   *+8                                                              
         MVI   D.DLCBTYP,C'T'      TELL DLFLD IT'S TEXT                         
*                                                                               
OUTPUT40 MVI   D.DLCBACT,DLCBPUT                                                
*                                                                               
         L     R1,=A(DLCB)                                                      
         GOTO1 =V(DLFLD),(R1)                                                   
         MVI   D.DLCXDELC,C' '       ALWAYS RESTORE TERMINATOR                  
*                                                                               
         LA    R4,L'RECTAB(R4)                                                  
         CLI   0(R4),X'FF'                                                      
         BNE   OUTPUT12                                                         
*                                                                               
OUTPUT42 MVI   D.DLCBACT,DLCBEOL                                                
         L     R1,=A(DLCB)                                                      
         GOTO1 =V(DLFLD),(R1)                                                   
*                                                                               
         MVI   D.DLCBACT,DLCBEOL                                                
         GOTO1 =V(DLFLD),(R1)                                                   
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* USER PRINT ROUTINE EXIT CALLED BY DLFLD                      *                
* ALL DATA PRINTED HERE GOES ON PAGE 2                         *                
*==============================================================*                
                                                                                
BLPRINT  NTR1  BASE=*,LABEL=*                                                   
         MVI   LINE,0              FORCE NO PAGE BREAK                          
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         CLI   DOWNFRST,C'Y'       TEST FIRST PRINT LINE                        
         JNE   BLPRINT2                                                         
         MVI   FORCEHED,C'Y'                                                    
         MVI   DOWNFRST,C'N'                                                    
*                                                                               
BLPRINT2 GOTO1 REPORT                                                           
         J     EXIT                                                             
*                                                                               
DOWNFRST DC    C'Y'                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
COPTS    DS    0D                  OPTIONS TABLE (25 BYTES EACH)                
         DC    C'BRAND/POL TRENDS         '                                     
         DC    C'LOCK BOX NUMBER          '                                     
         DC    C'MKT/STA TRENDS           '                                     
         DC    C'RATING SERVICE           '                                     
         DC    C'BILL FORMULA CNTRL       '                                     
         DC    C'BILL ESTIMATE CNTRL      '                                     
         DC    C'PRINT CLIENT CODE AS AAN '                                     
         DC    C'  ** NOT USED **         '                                     
         DC    C'GOALS CPP OVERRIDE       '                                     
         DC    C'PROGRAM ADJ CNTRL        '                                     
         DC    C'POL TIMESHEET DEMOS      '                                     
         DC    C'  ** NOT USED **         '                                     
         DC    C'  ** NOT USED **         '                                     
         DC    C'EXCLUSION GROUP CODE     '                                     
         DC    C'CLIENT RATE CNTRL        '                                     
CXOPTS   DC    C'CANADIAN DEMO OPTION     '         EXTRA OPTIONS               
         DC    C'CANADIAN NETWORK TAX     '                                     
         DC    C'BUY ID REQUIRED?         '                                     
         DC    C'ESTIMATE FILTERS REQ?    '                                     
         DC    C'CAMPAIGNS                '                                     
         DC    C'U.S. SPILL?              '                                     
         DC    C'''EST=NO'' EST NAME?       '                                   
         DC    C'MKGDS IN MISSED MTH?     '                                     
         DC    C'GOAL REQD FOR  BUY?      '                                     
         DC    C'COUNTRY                  '                                     
         DC    C'OUT-OF-WEEK CLIENT       '                                     
         DC    C'GST CODE                 '                                     
         DC    C'SPECIAL DEMO ADJ         '                                     
COPTEND  DC    X'FF'                                                            
         DS    0H                                                               
MONTAB   DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
*                                                                               
         DS    0D                                                               
SV00APRF DS    CL16                                                             
*                                                                               
*                                                                               
DLDATA   DS    0D                                                               
DLMED    DS    CL1                                                              
DLCLT    DS    CL3                                                              
DLCNAME  DS    CL20                                                             
DLCINTFC DS    CL8                                                              
DLCOFFC  DS    CL2                                                              
DLCACOFC DS    CL2                 ACC OFFICE                                   
DLPRD    DS    CL3                                                              
DLPNAME  DS    CL20                                                             
DLPCLASS DS    CL2                                                              
DLPACCT  DS    CL4                                                              
DLBILLDT DS    CL6                                                              
DLADDCHG DS    CL3                                                              
DLBILFRM DS    CL11                                                             
DLUTTL1  DS    CL20                                                             
DLUDTA1  DS    CL32                                                             
DLUTTL2  DS    CL20                                                             
DLUDTA2  DS    CL16                                                             
DLADDR1  DS    CL30                                                             
DLADDR2  DS    CL30                                                             
DLADDR3  DS    CL30                                                             
DLADDR4  DS    CL30                                                             
DLGST    DS    CL1                                                              
DLPST    DS    CL30                                                             
DLMAINPST DS   CL4                                                              
DLDATAX  EQU   *                                                                
*                                                                               
ALLPROFS DS    CL30                CPROF+CEXTRA                                 
*                                                                               
PRDCNT   DS    H                                                                
PRTSW    DS    CL1                                                              
DOWNLOAD DS    CL1                                                              
PRVAMC   DS    CL3              PREVIOUS AGY/MED/CLIENT                         
CLTOFF   DS    CL2              CURRENT CLIENT OFFICE                           
CUSER1   DS    CL20             USER DESCRIPTION FIELD                          
CUSER2   DS    CL20                                                             
PSTBLK   DS    CL(PSTLNQ)                                                       
PSTOUT   DS    CL64                                                             
TWOCHARS DS    C                                                                
OFCWORK  DS    XL64                                                             
         DS    0D                                                               
DLCB     DS    XL256                                                            
*                                                                               
MAXLINE  DC    H'132'              MAX LINE WIDTH                               
DELIM    DC    C' '                FIELD DELIMITER CHR                          
EOTCHR   DC    C'"'                END OF TEXT FIELD DELIMITER                  
EOTALT   DC    C''''               END OF TEXT CHR ALTERNATE                    
EOLCHR   DC    X'5E'               END OF LINE CHAR - SEMICOLON                 
EORCHR   DC    C':'                END OF REPORT CONTROL CHR                    
*                                                                               
         EJECT                                                                  
* ENTRIES ARE                                                                   
* AL1(L'DATA)           +0                                                      
* AL3(DATA)             +1                                                      
* CL1'TYPE'             +4                                                      
* XL3'00' NOT CURRENTLY DEFINED                                                 
* AL1(L'COLUMN HEADING) +8                                                      
* AL3(COLUMN HEADING)   +9                                                      
                                                                                
         DS    0D                                                               
RECTAB   DS    0XL12                                                            
         DC    AL1(L'DLMED),AL3(DLMED),C'T',3X'00'                              
         DC    AL1(L'TLMED),AL3(TLMED)                                          
         DC    AL1(L'DLCLT),AL3(DLCLT),C'T',3X'00'                              
         DC    AL1(L'TLCLT),AL3(TLCLT)                                          
         DC    AL1(L'DLCNAME),AL3(DLCNAME),C'T',3X'00'                          
         DC    AL1(L'TLCNAME),AL3(TLCNAME)                                      
**NOP    DC    AL1(L'DLCINTFC),AL3(DLCINTFC),C'T',3X'00'                        
**NOP    DC    AL1(L'TLCINTFC),AL3(TLCINTFC)                                    
**NOP    DC    AL1(L'DLCOFFC),AL3(DLCOFFC),C'T',3X'00'                          
**NOP    DC    AL1(L'TLCOFFC),AL3(TLCOFFC)                                      
         DC    AL1(L'DLPRD),AL3(DLPRD),C'T',3X'00'                              
         DC    AL1(L'TLPRD),AL3(TLPRD)                                          
         DC    AL1(L'DLPNAME),AL3(DLPNAME),C'T',3X'00'                          
         DC    AL1(L'TLPNAME),AL3(TLPNAME)                                      
         DC    AL1(L'DLPCLASS),AL3(DLPCLASS),C'T',3X'00'                        
         DC    AL1(L'TLPCLASS),AL3(TLPCLASS)                                    
         DC    AL1(L'DLPACCT),AL3(DLPACCT),C'T',3X'00'                          
         DC    AL1(L'TLPACCT),AL3(TLPACCT)                                      
         DC    AL1(L'DLBILLDT),AL3(DLBILLDT),C'T',3X'00'                        
         DC    AL1(L'TLBILLDT),AL3(TLBILLDT)                                    
         DC    AL1(L'DLBILFRM),AL3(DLBILFRM),C'T',3X'00'                        
         DC    AL1(L'TLBILFRM),AL3(TLBILFRM)                                    
         DC    AL1(L'DLUTTL1),AL3(DLUTTL1),C'T',3X'00'                          
         DC    AL1(L'TLUTTL1),AL3(TLUTTL1)                                      
         DC    AL1(L'DLUDTA1),AL3(DLUDTA1),C'T',3X'00'                          
         DC    AL1(L'TLUDTA1),AL3(TLUDTA1)                                      
         DC    AL1(L'DLUTTL2),AL3(DLUTTL2),C'T',3X'00'                          
         DC    AL1(L'TLUTTL2),AL3(TLUTTL2)                                      
         DC    AL1(L'DLUDTA2),AL3(DLUDTA2),C'T',3X'00'                          
         DC    AL1(L'TLUDTA2),AL3(TLUDTA2)                                      
         DC    AL1(L'DLADDR1),AL3(DLADDR1),C'T',3X'00'                          
         DC    AL1(L'TLADDR1),AL3(TLADDR1)                                      
         DC    AL1(L'DLADDR2),AL3(DLADDR2),C'T',3X'00'                          
         DC    AL1(L'TLADDR2),AL3(TLADDR2)                                      
         DC    AL1(L'DLADDR3),AL3(DLADDR3),C'T',3X'00'                          
         DC    AL1(L'TLADDR3),AL3(TLADDR3)                                      
         DC    AL1(L'DLADDR4),AL3(DLADDR4),C'T',3X'00'                          
         DC    AL1(L'TLADDR4),AL3(TLADDR4)                                      
         DC    AL1(L'DLGST),AL3(DLGST),C'T',3X'00'                              
         DC    AL1(L'TLGST),AL3(TLGST)                                          
         DC    AL1(L'DLPST),AL3(DLPST),C'T',3X'00'                              
         DC    AL1(L'TLPST),AL3(TLPST)                                          
         DC    AL1(L'DLMAINPST),AL3(DLMAINPST),C'T',3X'00'                      
         DC    AL1(L'TLMAINPST),AL3(TLMAINPST)                                  
         DC    X'FF'                                                            
*                                                                               
TLMED    DC    C'MED'                                                           
TLCLT    DC    C'CLT'                                                           
TLCNAME  DC    C'CLT NAME'                                                      
TLCINTFC DC    C'INTF CODE'                                                     
TLCOFFC  DC    C'CLT OFFC'                                                      
TLPRD    DC    C'PRD'                                                           
TLPNAME  DC    C'PRD NAME'                                                      
TLPCLASS DC    C'PCL'                                                           
TLPACCT  DC    C'ACCT'                                                          
TLBILLDT DC    C'BILL DATE'                                                     
TLBILFRM DC    C'BILL FORM'                                                     
TLUTTL1  DC    C'USER TITLE 1'                                                  
TLUDTA1  DC    C'USER DATA 1'                                                   
TLUTTL2  DC    C'USER TITLE 2'                                                  
TLUDTA2  DC    C'USER DATA 2'                                                   
TLADDR1  DC    C'ADDR LINE 1'                                                   
TLADDR2  DC    C'ADDR LINE 2'                                                   
TLADDR3  DC    C'ADDR LINE 3'                                                   
TLADDR4  DC    C'ADDR LINE 4'                                                   
TLGST    DC    C'GST CODE'                                                      
TLPST    DC    C'PST CODES'                                                     
TLMAINPST DC   C'MAIN PST'                                                      
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
UDEFD    DSECT                                                                  
USER     DS    CL20                                                             
TYPE     DS    CL1                                                              
LEN      DS    XL1                                                              
FLAG1    DS    XL1                                                              
FLAG2    DS    XL1                                                              
USERLNQ  EQU   *-USER                                                           
         SPACE 2                                                                
*                                                                               
PLINED   DSECT                                                                  
PROD     DS    CL3                                                              
         DS    CL1                                                              
NAME     DS    CL20                                                             
         DS    CL2                                                              
ADDR     DS    CL30                                                             
         DS    CL1                                                              
CLASS    DS    CL2                                                              
         DS    CL3                                                              
ACCT     DS    CL5                                                              
         DS    CL1                                                              
BDATE    DS    CL6                                                              
         DS    CL2                                                              
BFORM    DS    CL10                                                             
         DS    CL1                                                              
GST      DS    CL1                                                              
         DS    CL2                                                              
USERDEF  DS    CL32                                                             
         DS    CL1                                                              
CHGED    DS    CL7                                                              
         SPACE 2                                                                
*                                                                               
       ++INCLUDE DDDLCB                                                         
         PRINT OFF                                                              
       ++INCLUDE DDPSTBLK                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDCOREQUS                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPREPMODES                                                     
AGYRECD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050SPREP4102 04/13/20'                                      
         END                                                                    
